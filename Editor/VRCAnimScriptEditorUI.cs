// Editor/VRCAnimScriptEditorUI.cs

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEditor;
using UnityEditor.Animations;
using UnityEngine;
using VRC.SDK3.Avatars.Components;
using VRC.SDK3.Avatars.ScriptableObjects;
using VRC.SDKBase;

namespace VRCAnimScript {
    class KeyframeComparer : IComparer {
        int IComparer.Compare(object lhs, object rhs) {
            float x = ((Keyframe)lhs).time;
            float y = ((Keyframe)rhs).time;
            return x < y ? -1 : x > y ? 1 : 0;
        }
    }

    class Materializer {
        Ast.Root root;

        GameObject rootObject;

        List<string> materializedAssetPaths;

        public Materializer(Ast.Root root) {
            this.root = root;

            materializedAssetPaths = new List<string>();

            rootObject = GameObject.Find(root.rootObject);
            if (rootObject == null)
                throw new Exception("Couldn't find a game object named " + rootObject);
        }

        public static string DoubleQuote(string input) {
            return "\"" + input.Replace("\"", "\\\"") + "\"";
        }

        private static string GetBaseName(string path) {
            int lastSlashIndex = path.LastIndexOf('/');
            string fileNameWithExtension = (lastSlashIndex == -1) ? path : path.Substring(lastSlashIndex + 1);
            int lastDotIndex = fileNameWithExtension.LastIndexOf('.');
            return (lastDotIndex == -1) ? fileNameWithExtension : fileNameWithExtension.Substring(0, lastDotIndex);
        }

        private string PrepareToMaterializeAsset(string name, string extension = "asset") {
            // Move old asset to OS trash.
            string assetPath = "Assets/" + name + "." + extension;
            AssetDatabase.MoveAssetToTrash(assetPath);
            return assetPath;
        }

        private string MaterializeAsset(
                UnityEngine.Object asset,
                string name,
                string type,
                string extension = "asset") {
            string assetPath = PrepareToMaterializeAsset(name, extension);
            AssetDatabase.CreateAsset(asset, assetPath);
            materializedAssetPaths.Add(assetPath);
            return assetPath;
        }

        private string GenerateAssetName(string suffix) {
            return root.avatar + "Gen" + suffix;
        }

        public void Materialize() {
            string paramsAssetPath = MaterializeParams();
            string mainMenuAssetPath = MaterializeMenu();
            string actionControllerActionPath =
                MaterializeController(root.actionController, "Action");
            string fxControllerActionPath = MaterializeController(root.fxController, "FX");
            ApplyMaterializedAssetsToAvatar(
                paramsAssetPath,
                mainMenuAssetPath,
                actionControllerActionPath,
                fxControllerActionPath);
        }

        private string MaterializeParams() {
            Ast.Param[] allParams = root.allParams;
            var vrcParams = new VRCExpressionParameters();

            vrcParams.parameters = new VRCExpressionParameters.Parameter[allParams.Length];
            for (int i = 0; i < allParams.Length; i++) {
                Ast.Param param = allParams[i];
                var vrcParam = new VRCExpressionParameters.Parameter();
                vrcParam.defaultValue = param.initialValue ? 1.0f : 0.0f;
                vrcParam.name = param.name;
                vrcParam.saved = param.saved;
                vrcParam.valueType = VRCExpressionParameters.ValueType.Bool;
                vrcParams.parameters[i] = vrcParam;
            }

            return MaterializeAsset(vrcParams, GenerateAssetName("Parameters"), "parameters");
        }

        private string MaterializeMenu() {
            Ast.Menu mainMenu = root.mainMenu;
            Ast.MenuItem[] menuItems = mainMenu.items;

            var vrcMenu = new VRCExpressionsMenu();
            string menuName = GenerateAssetName("MainMenu");
            vrcMenu.name = menuName;

            for (int i = 0; i < menuItems.Length; i++) {
                Ast.MenuItem menuItem = menuItems[i];
                var vrcMenuItem = new VRCExpressionsMenu.Control();
                vrcMenuItem.name = menuItem.name;

                string[] texturePaths = AssetDatabase.FindAssets(
                    DoubleQuote(menuItem.icon) + " t:Texture2D");
                if (texturePaths.Length == 0) {
                    Debug.LogWarning("Couldn't find menu item texture " + vrcMenuItem.icon);
                } else {
                    vrcMenuItem.icon = AssetDatabase.LoadAssetAtPath<Texture2D>(
                        AssetDatabase.GUIDToAssetPath(texturePaths[0]));
                }

                vrcMenuItem.type = VRCExpressionsMenu.Control.ControlType.Toggle;
                vrcMenuItem.parameter = new VRCExpressionsMenu.Control.Parameter();
                vrcMenuItem.parameter.name = menuItem.toggle;

                vrcMenu.controls.Add(vrcMenuItem);
            }

            return MaterializeAsset(vrcMenu, menuName, "menu");
        }

        private string MaterializeController(
                Ast.ControllerSection controllerSection,
                string controllerName) {
            bool isAction = controllerName.Equals("Action");

            string assetName = GenerateAssetName(controllerName + "Controller");
            string assetPath = PrepareToMaterializeAsset(assetName, "controller");

            AnimatorController unityController =
                AnimatorController.CreateAnimatorControllerAtPath(assetPath);
            if (unityController == null)
                throw new Exception("Failed to create Unity controller " + assetName);

            HashSet<string> allParameters = new HashSet<string>();

            for (int layerIndex = 0; layerIndex < controllerSection.layers.Length; layerIndex++) {
                Ast.Layer layer = controllerSection.layers[layerIndex];

                // Create layer and state machine.
                unityController.AddLayer(layer.name);
                AnimatorControllerLayer[] unityLayers = unityController.layers;
                AnimatorControllerLayer unityLayer = unityLayers[unityLayers.Length - 1];
                AnimatorStateMachine unityStateMachine = unityLayer.stateMachine;

                unityLayer.defaultWeight = 1.0f;

                AnimatorState[] unityStates = new AnimatorState[layer.states.Length];
                var nameToUnityState = new Dictionary<string, AnimatorState>();

                // Materialize states.
                for (int stateIndex = 0; stateIndex < layer.states.Length; stateIndex++) {
                    Ast.State state = layer.states[stateIndex];
                    AnimatorState unityState = unityStateMachine.AddState(state.name);

                    // Turn action layer on and off as appropriate.
                    if (isAction) {
                        var vrcPlayableLayerControl =
                            unityState.AddStateMachineBehaviour<VRCPlayableLayerControl>();
                        vrcPlayableLayerControl.layer =
                            VRC_PlayableLayerControl.BlendableLayer.Action;
                        vrcPlayableLayerControl.goalWeight = state.animation == null ? 0.0f : 1.0f;

                        VRC_AnimatorTrackingControl.TrackingType trackingType;
                        if (state.animation == null)
                            trackingType = VRC_AnimatorTrackingControl.TrackingType.Tracking;
                        else
                            trackingType = VRC_AnimatorTrackingControl.TrackingType.Animation;

                        var vrcAnimatorTrackingControl =
                            unityState.AddStateMachineBehaviour<VRCAnimatorTrackingControl>();
                        vrcAnimatorTrackingControl.trackingHead = trackingType;
                        vrcAnimatorTrackingControl.trackingLeftHand = trackingType;
                        vrcAnimatorTrackingControl.trackingRightHand = trackingType;
                        vrcAnimatorTrackingControl.trackingHip = trackingType;
                        vrcAnimatorTrackingControl.trackingLeftFoot = trackingType;
                        vrcAnimatorTrackingControl.trackingRightFoot = trackingType;
                        vrcAnimatorTrackingControl.trackingLeftFingers = trackingType;
                        vrcAnimatorTrackingControl.trackingRightFingers = trackingType;
                        vrcAnimatorTrackingControl.trackingEyes = trackingType;
                        vrcAnimatorTrackingControl.trackingMouth = trackingType;
                    }

                    // Materialize animation.
                    if (state.animation != null) {
                        string animationPath = "";
                        if (state.animation is Ast.InlineAnimation) {
                            animationPath = MaterializeInlineAnimation(
                                (Ast.InlineAnimation)state.animation,
                                assetName,
                                layer.name,
                                state.name);
                        } else if (state.animation is Ast.ExternalAnimation) {
                            var external = (Ast.ExternalAnimation)state.animation;
                            string query = DoubleQuote(external.name) + " t:AnimationClip";

                            string[] animationClips = AssetDatabase.FindAssets(query);
                            foreach (string animationClip in animationClips) {
                                string thisPath = AssetDatabase.GUIDToAssetPath(animationClip);
                                if (GetBaseName(thisPath).Equals(external.name)) {
                                    animationPath = thisPath;
                                    break;
                                }
                            }

                            if (animationPath == null) {
                                string errorMessage = "Couldn't find external animation clip " +
                                    external;
                                if (animationClips.Length > 0) {
                                    errorMessage += " from candidates " +
                                        String.Join(", ", animationClips);
                                }
                                Debug.LogWarning(errorMessage);
                            }
                        } else {
                            throw new Exception("Unhandled animation type");
                        }

                        unityState.motion =
                            AssetDatabase.LoadAssetAtPath<AnimationClip>(animationPath);
                    }

                    unityStates[stateIndex] = unityState;
                    nameToUnityState.Add(state.name, unityState);
                }

                // Materialize transitions.
                for (int stateIndex = 0; stateIndex < layer.states.Length; stateIndex++) {
                    Ast.State state = layer.states[stateIndex];
                    AnimatorState unityState = unityStates[stateIndex];

                    if (state.initial)
                        unityStateMachine.AddEntryTransition(unityState);

                    for (int transitionIndex = 0;
                            transitionIndex < state.transitions.Length;
                            transitionIndex++) {
                        Ast.StateTransition transition = state.transitions[transitionIndex];
                        AnimatorState destination = nameToUnityState[transition.destination];
                        AnimatorStateTransition unityTransition =
                            unityState.AddTransition(destination);

                        // TODO: Make these customizable?
                        unityTransition.duration = 0.0f;
                        unityTransition.hasExitTime = false;

                        Ast.TransitionCondition condition = transition.condition;
                        if (condition is Ast.TimeTransitionCondition) {
                            var timeCondition = (Ast.TimeTransitionCondition)condition;
                            unityTransition.offset = (float)timeCondition.time;
                        } else if (condition is Ast.ParamTransitionCondition) {
                            var paramCondition = (Ast.ParamTransitionCondition)condition;
                            allParameters.Add(paramCondition.param);
                            AnimatorConditionMode conditionMode = paramCondition.value ?
                                AnimatorConditionMode.If :
                                AnimatorConditionMode.IfNot;
                            unityTransition.AddCondition(conditionMode, 0.0f, paramCondition.param);
                        } else {
                            throw new Exception("Unknown transition condition AST node");
                        }
                    }
                }

                // NB: Unity docs say we have to do this.
                unityController.layers = unityLayers;
            }

            // Set up parameters.
            foreach (string parameter in allParameters)
                unityController.AddParameter(parameter, AnimatorControllerParameterType.Bool);

            EditorUtility.SetDirty(unityController);
            AssetDatabase.SaveAssets();

            return assetPath;
        }

        private string GetRootObjectRelativePath(GameObject obj) {
            if (obj.transform.parent == null || obj.transform.parent.gameObject == rootObject)
                return obj.name;
            return GetRootObjectRelativePath(obj.transform.parent.gameObject) + "/" + obj.name;
        }

        private delegate void IterateBindings(EditorCurveBinding binding, float value);

        private void IterateBindingsForExpression(
                Ast.Expression expression,
                string path,
                string propertyName,
                Type type,
                IterateBindings iterator) {
            if (expression is Ast.IntExpression) {
                EditorCurveBinding binding = new EditorCurveBinding();
                binding.path = path;
                binding.type = type;
                binding.propertyName = propertyName;
                iterator(binding, (float)((Ast.IntExpression)expression).value);
                return;
            }

            if (expression is Ast.VectorLiteralExpression) {
                var vectorLiteralExpression = (Ast.VectorLiteralExpression)expression;
                for (int i = 0; i < vectorLiteralExpression.elements.Length; i++) {
                    EditorCurveBinding binding = new EditorCurveBinding();
                    binding.path = path;
                    binding.type = type;
                    binding.propertyName = propertyName + "." +
                        (new string[] { "x", "y", "z", "w" })[i];
                    iterator(binding, (float)vectorLiteralExpression.elements[i]);
                }
                return;
            }

            throw new Exception("Unhandled expression type");
        }

        private string MaterializeInlineAnimation(
                Ast.InlineAnimation animation,
                string controllerName,
                string layerName,
                string stateName) {
            AnimationClip unityAnimation = new AnimationClip();
            var unityCurves = new Dictionary<EditorCurveBinding, List<Keyframe>>();

            for (int frameIndex = 0; frameIndex < animation.frames.Length; frameIndex++) {
                Ast.Frame frame = animation.frames[frameIndex];
                for (int actionIndex = 0; actionIndex < frame.actions.Length; actionIndex++) {
                    Ast.FrameAction frameAction = frame.actions[actionIndex];

                    string query = frameAction.propertyPath[0];

                    GameObject gameObject = GameObject.Find(query);
                    if (gameObject == null) {
                        Debug.LogWarning("Failed to find game object " + query);
                        continue;
                    }

                    // Figure out which component we're targeting.
                    Type componentType = null;
                    if (frameAction.component.Equals("GameObject")) {
                        componentType = typeof(GameObject);
                    } else {
                        Component[] allComponents = gameObject.GetComponents<Component>();
                        foreach (Component component in allComponents) {
                            string fullName = component.GetType().FullName;
                            string baseName = fullName.Substring(fullName.LastIndexOf('.') + 1);
                            if (baseName.Equals(frameAction.component)) {
                                componentType = component.GetType();
                                break;
                            }
                        }
                    }
                    if (componentType == null) {
                        Debug.LogWarning("Failed to find component of type " + query);
                        continue;
                    }

                    IterateBindingsForExpression(
                        frameAction.expression,
                        GetRootObjectRelativePath(gameObject),
                        String.Join(".", frameAction.propertyName),
                        componentType,
                        (EditorCurveBinding binding, float value) => {
                            Keyframe keyframe = new Keyframe((float)frame.time, value);
                            if (!unityCurves.ContainsKey(binding))
                                unityCurves.Add(binding, new List<Keyframe>());
                            unityCurves[binding].Add(keyframe);
                        });
                }
            }

            foreach (var KVP in unityCurves) {
                Keyframe[] keyframes = KVP.Value.ToArray();
                Array.Sort(keyframes, new KeyframeComparer());
                AnimationCurve animationCurve = new AnimationCurve(keyframes);
                AnimationUtility.SetEditorCurve(unityAnimation, KVP.Key, animationCurve);
            }

            string animationName = controllerName + "_" + layerName + "_" + stateName;
            return MaterializeAsset(unityAnimation, animationName, "animation", "anim");
        }

        private void ApplyMaterializedAssetsToAvatar(
            string paramsAssetPath,
            string mainMenuAssetPath,
            string actionControllerAssetPath,
            string fxControllerAssetPath
        ) {
            VRCAvatarDescriptor descriptor = rootObject.GetComponent<VRCAvatarDescriptor>();
            if (descriptor == null) {
                Debug.LogWarning("Couldn't find a VRC avatar descriptor on the root object!");
                return;
            }

            descriptor.expressionParameters =
                AssetDatabase.LoadAssetAtPath<VRCExpressionParameters>(paramsAssetPath);
            descriptor.expressionsMenu =
                AssetDatabase.LoadAssetAtPath<VRCExpressionsMenu>(mainMenuAssetPath);

            ModifyCustomAnimLayer(ref descriptor.baseAnimationLayers[3], actionControllerAssetPath);
            ModifyCustomAnimLayer(ref descriptor.baseAnimationLayers[4], fxControllerAssetPath);
        }

        private void ModifyCustomAnimLayer(
                ref VRCAvatarDescriptor.CustomAnimLayer customAnimLayer,
                string assetPath) {
            if (assetPath == null) {
                customAnimLayer.isDefault = true;
                customAnimLayer.animatorController = null;
                return;
            }

            var controller = AssetDatabase.LoadAssetAtPath<RuntimeAnimatorController>(assetPath);
            customAnimLayer.animatorController = controller;
            customAnimLayer.isDefault = false;
        }

        public void LogAssetPaths() {
            Debug.Log("Created assets: " + String.Join(", ", materializedAssetPaths.ToArray()));
        }
    }

    public class EditorUI {
        [MenuItem("Tools/Run VRC Animation Script...")]
        static void RunAnimationScript() {
            string path = EditorUtility.OpenFilePanel(
                "Open Script",
                Application.dataPath,
                "vrcanimscript");
            using (var stream = new BufferedStream(File.Open(path, FileMode.Open))) {
                using (Parser parser = new Parser(stream)) {
                    Ast.Root root = parser.ParseRoot();
                    parser.EnsureFinished();

                    Materializer materializer = new Materializer(root);
                    materializer.Materialize();
                    materializer.LogAssetPaths();
                }
            }
        }

#if false
        [MenuItem("Tools/Dump Asset")]
        static void DumpAsset() {
            if (!(Selection.activeObject is AnimationClip)) {
                Debug.Log(Selection.activeObject.GetType());
                return;
            }

            var animationClip = (AnimationClip)Selection.activeObject;
            EditorCurveBinding[] curveBindings = AnimationUtility.GetCurveBindings(animationClip);
            for (int i = 0; i < curveBindings.Length; i++) {
                Debug.Log("path=" +
                    curveBindings[i].path +
                    " propertyName=" +
                    curveBindings[i].propertyName +
                    " type=" +
                    curveBindings[i].type);
                AnimationCurve curve = AnimationUtility.GetEditorCurve(
                    animationClip,
                    curveBindings[i]);
                for (int j = 0; j < curve.keys.Length; j++) {
                    Keyframe keyframe = curve[j];
                    Debug.Log("* keyframe " + j + ": inTangent=" + keyframe.inTangent +
                        " inWeight=" + keyframe.inWeight +
                        " outTangent=" + keyframe.outTangent +
                        " outWeight=" + keyframe.outWeight +
                        " time=" + keyframe.time +
                        " value=" + keyframe.value +
                        " mode=" + keyframe.weightedMode);
                }
            }
        }

        [MenuItem("Tools/Dump Component")]
        static void DumpComponent() {
            if (!(Selection.activeObject is GameObject)) {
                Debug.Log(Selection.activeObject.GetType());
                return;
            }

            var gameObject = (GameObject)Selection.activeObject;
            var avatarDescriptor = gameObject.GetComponent<VRCAvatarDescriptor>();

            for (int i = 0; i < avatarDescriptor.baseAnimationLayers.Length; i++) {
                string Desc = "" + i + ":";
                VRCAvatarDescriptor.CustomAnimLayer layer = avatarDescriptor.baseAnimationLayers[i];
                Desc += " enabled=" + layer.isEnabled;
                Desc += " type=" + layer.type;
                Desc += " animatorcontroller=" +
                    (layer.animatorController == null ? "null" : "nonnull");
                Desc += " mask=" + (layer.mask == null ? "null" : "nonnull");
                Desc += " isdefault=" + layer.isDefault;
                Debug.Log(Desc);
            }
        }
#endif
    }
}   // end namespace VRCAnimScript
