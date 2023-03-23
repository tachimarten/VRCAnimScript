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

    class ObjectReferenceKeyframeComparer : IComparer {
        int IComparer.Compare(object lhs, object rhs) {
            float x = ((ObjectReferenceKeyframe)lhs).time;
            float y = ((ObjectReferenceKeyframe)rhs).time;
            return x < y ? -1 : x > y ? 1 : 0;
        }
    }

    class Materializer {
        const double FLOAT_QUANTUM = 0.00001;

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

        private static string BaseNameOfType(Type type) {
            string fullName = type.FullName;
            return fullName.Substring(fullName.LastIndexOf('.') + 1);
        }

        private string GetAssetPath<T>(string name) {
            string query = DoubleQuote(name) + " t:" + BaseNameOfType(typeof(T));
            string[] guids = AssetDatabase.FindAssets(query);
            foreach (string guid in guids) {
                string assetPath = AssetDatabase.GUIDToAssetPath(guid);
                if (GetBaseName(assetPath).Equals(name))
                    return assetPath;
            }
            return null;
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
                vrcParam.name = param.name;
                vrcParam.saved = param.saved;

                if (param is Ast.BoolParam) {
                    var boolParam = (Ast.BoolParam)param;
                    vrcParam.valueType = VRCExpressionParameters.ValueType.Bool;
                    vrcParam.defaultValue = boolParam.initialValue ? 1.0f : 0.0f;
                } else if (param is Ast.FloatParam) {
                    var floatParam = (Ast.FloatParam)param;
                    vrcParam.valueType = VRCExpressionParameters.ValueType.Float;
                    vrcParam.defaultValue = (float)floatParam.initialValue;
                } else {
                    throw new Exception("Unknown parameter type");
                }

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

                string texturePath = GetAssetPath<Texture2D>(menuItem.icon);
                if (texturePath == null)
                    Debug.LogWarning("Couldn't find menu item texture " + vrcMenuItem.icon);
                else
                    vrcMenuItem.icon = AssetDatabase.LoadAssetAtPath<Texture2D>(texturePath);

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

            var allParameters = new Dictionary<string, AnimatorControllerParameterType>();

            for (int layerIndex = 0; layerIndex < controllerSection.layers.Length; layerIndex++) {
                Ast.Layer layer = controllerSection.layers[layerIndex];

                // Create layer and state machine.
                unityController.AddLayer(layer.name);
                AnimatorControllerLayer[] unityLayers = unityController.layers;
                AnimatorControllerLayer unityLayer = unityLayers[unityLayers.Length - 1];
                AnimatorStateMachine unityStateMachine = unityLayer.stateMachine;

                unityLayer.defaultWeight = 1.0f;
                if (layer.additive)
                    unityLayer.blendingMode = AnimatorLayerBlendingMode.Additive;
                else
                    unityLayer.blendingMode = AnimatorLayerBlendingMode.Override;

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
                            animationPath = GetAssetPath<AnimationClip>(external.name);

                            if (animationPath == null) {
                                string errorMessage = "Couldn't find external animation clip " +
                                    external;
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

                        Ast.TransitionCondition condition = transition.condition;
                        if (condition is Ast.TimeTransitionCondition) {
                            var timeCondition = (Ast.TimeTransitionCondition)condition;
                            AnimatorStateTransition unityTransition =
                                unityState.AddTransition(destination);
                            // TODO: Make these customizable?
                            unityTransition.duration = 0.0f;
                            unityTransition.hasExitTime = false;
                            unityTransition.offset = (float)timeCondition.time;
                        } else if (condition is Ast.ParamTransitionCondition) {
                            MaterializeParamTransition((Ast.ParamTransitionCondition)condition,
                                unityState, destination, allParameters);
                        } else {
                            throw new Exception("Unknown transition condition AST node");
                        }
                    }
                }

                // NB: Unity docs say we have to do this.
                unityController.layers = unityLayers;
            }

            // Set up parameters.
            foreach (KeyValuePair<string, AnimatorControllerParameterType> kvp in allParameters)
                unityController.AddParameter(kvp.Key, kvp.Value);

            EditorUtility.SetDirty(unityController);
            AssetDatabase.SaveAssets();

            return assetPath;
        }

        private void MaterializeParamTransition(
                Ast.ParamTransitionCondition paramCondition,
                AnimatorState source,
                AnimatorState destination,
                Dictionary<string, AnimatorControllerParameterType> allParameters) {
            Compiler.Disjunction disjunction =
                Compiler.Disjunction.FromAst(paramCondition.expression);

            foreach (Compiler.Conjunction conjunction in disjunction.conjunctions) {
                AnimatorStateTransition unityTransition = source.AddTransition(destination);

                // TODO: Make these customizable?
                unityTransition.duration = 0.0f;
                unityTransition.hasExitTime = false;

                foreach (Compiler.Literal literal in conjunction.literals) {
                    AnimatorConditionMode conditionMode = AnimatorConditionMode.If;
                    double testValue = 0.0;
                    var paramType = AnimatorControllerParameterType.Bool;

                    if (literal is Compiler.BoolParamLiteral) {
                        Compiler.BoolParamLiteral boolParamLiteral =
                            (Compiler.BoolParamLiteral)literal;
                        testValue = 0.0;
                        paramType = AnimatorControllerParameterType.Bool;

                        if (boolParamLiteral.negated)
                            conditionMode = AnimatorConditionMode.IfNot;
                        else
                            conditionMode = AnimatorConditionMode.If;
                    } else if (literal is Compiler.FloatParamLiteral) {
                        Compiler.FloatParamLiteral floatParamLiteral =
                            (Compiler.FloatParamLiteral)literal;
                        paramType = AnimatorControllerParameterType.Float;

                        switch (floatParamLiteral.op) {
                        case Compiler.FloatRelationalOperator.Less:
                            conditionMode = AnimatorConditionMode.Less;
                            testValue = floatParamLiteral.value;
                            break;
                        case Compiler.FloatRelationalOperator.Greater:
                            conditionMode = AnimatorConditionMode.Greater;
                            testValue = floatParamLiteral.value;
                            break;
                        case Compiler.FloatRelationalOperator.LessEqual:
                            conditionMode = AnimatorConditionMode.Less;
                            testValue = floatParamLiteral.value + FLOAT_QUANTUM;
                            break;
                        case Compiler.FloatRelationalOperator.GreaterEqual:
                            conditionMode = AnimatorConditionMode.Greater;
                            testValue = floatParamLiteral.value - FLOAT_QUANTUM;
                            break;
                        }
                    } else {
                        throw new Exception("Unknown param literal type");
                    }

                    unityTransition.AddCondition(conditionMode, (float)testValue,
                        literal.paramName);

                    if (allParameters.ContainsKey(literal.paramName)) {
                        AnimatorControllerParameterType expected =
                            allParameters[literal.paramName];
                        if (expected != paramType) {
                            throw new Exception("Mismatched parameter type: expected " + expected +
                                " but found " + paramType);
                        }
                    } else {
                        allParameters.Add(literal.paramName, paramType);
                    }
                }
            }
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

            if (expression is Ast.FloatExpression) {
                EditorCurveBinding binding = new EditorCurveBinding();
                binding.path = path;
                binding.type = type;
                binding.propertyName = propertyName;
                iterator(binding, (float)((Ast.FloatExpression)expression).value);
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

        private string PropertyToString(Ast.PropertyComponent[] propertyComponents) {
            string result = "";
            for (int i = 0; i < propertyComponents.Length; i++) {
                Ast.PropertyComponent propertyComponent = propertyComponents[i];
                if (propertyComponent is Ast.NumberPropertyComponent) {
                    // Automatically add `.Array.data`.
                    result += ".Array.data[" +
                        ((Ast.NumberPropertyComponent)propertyComponent).value + "]";
                } else if (propertyComponent is Ast.NamePropertyComponent) {
                    if (i > 0)
                        result += ".";
                    result += ((Ast.NamePropertyComponent)propertyComponent).name;
                } else {
                    throw new Exception("Unhandled property component type");
                }
            }
            return result;
        }

        private string MaterializeInlineAnimation(
                Ast.InlineAnimation animation,
                string controllerName,
                string layerName,
                string stateName) {
            AnimationClip unityAnimation = new AnimationClip();

            // Set loop time.
            AnimationClipSettings settings =
                AnimationUtility.GetAnimationClipSettings(unityAnimation);
            settings.loopTime = animation.loop;
            AnimationUtility.SetAnimationClipSettings(unityAnimation, settings);

            var unityFloatCurves = new Dictionary<EditorCurveBinding, List<Keyframe>>();
            var unityObjectReferenceCurves =
                new Dictionary<EditorCurveBinding, List<ObjectReferenceKeyframe>>();

            for (int frameIndex = 0; frameIndex < animation.frames.Length; frameIndex++) {
                Ast.Frame frame = animation.frames[frameIndex];
                for (int actionIndex = 0; actionIndex < frame.actions.Length; actionIndex++) {
                    Ast.FrameAction frameAction = frame.actions[actionIndex];

                    string query = frameAction.objectPath[0];

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
                            if (BaseNameOfType(component.GetType()).Equals(frameAction.component)) {
                                componentType = component.GetType();
                                break;
                            }
                        }
                    }
                    if (componentType == null) {
                        Debug.LogWarning("Failed to find component of type " +
                            frameAction.component);
                        continue;
                    }

                    if (frameAction.expression is Ast.AssetExpression) {
                        var assetExpression = (Ast.AssetExpression)frameAction.expression;

                        UnityEngine.Object asset = null;
                        string[] guids =
                            AssetDatabase.FindAssets(DoubleQuote(assetExpression.name));
                        foreach (string guid in guids) {
                            string assetPath = AssetDatabase.GUIDToAssetPath(guid);
                            if (!GetBaseName(assetPath).Equals(assetExpression.name))
                                continue;
                            // TODO: Support multiple assets in the same file.
                            Type assetType = AssetDatabase.GetMainAssetTypeAtPath(assetPath);
                            if (BaseNameOfType(assetType) != assetExpression.type)
                                continue;
                            asset = AssetDatabase.LoadAssetAtPath(assetPath, assetType);
                            break;
                        }

                        if (asset == null) {
                            Debug.LogWarning("Couldn't find an asset of type " +
                                assetExpression.type + " named " + assetExpression.name);
                        } else {
                            var binding = new EditorCurveBinding();
                            binding.path = GetRootObjectRelativePath(gameObject);
                            binding.propertyName = PropertyToString(frameAction.property);
                            binding.type = componentType;

                            ObjectReferenceKeyframe keyframe;
                            keyframe.time = (float)frame.time;
                            keyframe.value = asset;

                            if (!unityObjectReferenceCurves.ContainsKey(binding)) {
                                unityObjectReferenceCurves.Add(
                                    binding,
                                    new List<ObjectReferenceKeyframe>());
                            }
                            unityObjectReferenceCurves[binding].Add(keyframe);
                        }
                    } else {
                        // Float binding.
                        IterateBindingsForExpression(
                            frameAction.expression,
                            GetRootObjectRelativePath(gameObject),
                            PropertyToString(frameAction.property),
                            componentType,
                            (EditorCurveBinding binding, float value) => {
                                Keyframe keyframe = new Keyframe((float)frame.time, value);

                                if (frameAction.prevTangent != null) {
                                    keyframe.inTangent = (float)frameAction.prevTangent;
                                    if (frameAction.nextTangent != null)
                                        keyframe.outTangent = (float)frameAction.nextTangent;
                                    else
                                        keyframe.outTangent = keyframe.inTangent;
                                }

                                if (!unityFloatCurves.ContainsKey(binding))
                                    unityFloatCurves.Add(binding, new List<Keyframe>());
                                unityFloatCurves[binding].Add(keyframe);
                            });
                    }
                }
            }

            // Set float curves.
            foreach (var KVP in unityFloatCurves) {
                Keyframe[] keyframes = KVP.Value.ToArray();
                Array.Sort(keyframes, new KeyframeComparer());
                AnimationCurve animationCurve = new AnimationCurve(keyframes);
                AnimationUtility.SetEditorCurve(unityAnimation, KVP.Key, animationCurve);
            }

            // Set object reference curves.
            foreach (var KVP in unityObjectReferenceCurves) {
                ObjectReferenceKeyframe[] keyframes = KVP.Value.ToArray();
                Array.Sort(keyframes, new ObjectReferenceKeyframeComparer());
                AnimationUtility.SetObjectReferenceCurve(unityAnimation, KVP.Key, keyframes);
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

        [MenuItem("Tools/Create Scratchpad Animation")]
        static void CreateScratchpadAnimation() {
            string controllerPath = "Assets/VRCAnimScriptScratchpadController.controller";
            string clipPath = "Assets/VRCAnimScriptScratchpadAnimation.anim";

            AnimatorController controller = AnimatorController.CreateAnimatorControllerAtPath(
                controllerPath);
            if (controller == null)
                throw new Exception("Failed to create Unity controller");

            var clip = new AnimationClip();
            AssetDatabase.CreateAsset(clip, clipPath);

            AnimatorControllerLayer layer = controller.layers[0];
            AnimatorStateMachine stateMachine = layer.stateMachine;
            layer.defaultWeight = 1.0f;
            AnimatorState state = stateMachine.AddState("Temp");
            state.motion = clip;

            Debug.Log("Created " + controllerPath + " and " + clipPath);

            if (Selection.activeObject == null || !(Selection.activeObject is GameObject))
                return;
            GameObject gameObject = (GameObject)Selection.activeObject;
            Animator animator = gameObject.GetComponent<Animator>();
            if (animator == null)
                return;
            animator.runtimeAnimatorController = controller;
        }

        [MenuItem("Tools/Dump Animation Properties")]
        static void DumpAnimationProperties() {
            if (Selection.activeObject == null || !(Selection.activeObject is AnimationClip)) {
                Debug.LogError("Select an animation clip asset first.");
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

#if false
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

    namespace Compiler {

        // Disjunctive normal form
        class Disjunction {
            public Conjunction[] conjunctions;

            public static Disjunction FromAst(Ast.BooleanExpression expression) {
                // Special desugaring case for == and !=.
                //
                // This will always terminate. Proof: conversion of == or != to >=/<= is a one-
                // way operation and nothing can produce a == or != that wasn't already there in
                // the source.
                if (expression is Ast.RelationalBooleanExpression) {
                    var relationalExpression = (Ast.RelationalBooleanExpression)expression;
                    if (relationalExpression.op == Ast.RelationalOperator.Equal ||
                            relationalExpression.op == Ast.RelationalOperator.NotEqual) {
                        var newAndExpression = new Ast.AndBooleanExpression();
                        var newLHSExpression = new Ast.RelationalBooleanExpression();
                        var newRHSExpression = new Ast.RelationalBooleanExpression();
                        newAndExpression.lhs = newLHSExpression;
                        newAndExpression.rhs = newRHSExpression;

                        newLHSExpression.paramName = newRHSExpression.paramName =
                            relationalExpression.paramName;
                        newLHSExpression.value = newRHSExpression.value =
                            relationalExpression.value;
                        newLHSExpression.op = Ast.RelationalOperator.GreaterEqual;
                        newRHSExpression.op = Ast.RelationalOperator.LessEqual;

                        if (relationalExpression.op == Ast.RelationalOperator.Equal)
                            return Disjunction.FromAst(newAndExpression);

                        // NotEqual case:
                        var newNotExpression = new Ast.NotBooleanExpression();
                        newNotExpression.expression = newAndExpression;
                        return Disjunction.FromAst(newNotExpression);
                    }

                }

                // Literals
                if (expression is Ast.SingleTestBooleanExpression) {
                    Literal literal = null;
                    
                    if (expression is Ast.ParamBooleanExpression) {
                        literal = new BoolParamLiteral();
                    } else if (expression is Ast.RelationalBooleanExpression) {
                        var relationalExpression = (Ast.RelationalBooleanExpression)expression;
                        FloatParamLiteral floatLiteral = new FloatParamLiteral();
                        floatLiteral.value = relationalExpression.value;

                        switch (relationalExpression.op) {
                        case Ast.RelationalOperator.Greater:
                            floatLiteral.op = FloatRelationalOperator.Greater;
                            break;
                        case Ast.RelationalOperator.Less:
                            floatLiteral.op = FloatRelationalOperator.Less;
                            break;
                        case Ast.RelationalOperator.GreaterEqual:
                            floatLiteral.op = FloatRelationalOperator.GreaterEqual;
                            break;
                        case Ast.RelationalOperator.LessEqual:
                            floatLiteral.op = FloatRelationalOperator.LessEqual;
                            break;
                        }

                        literal = floatLiteral;
                    }

                    literal.paramName = ((Ast.SingleTestBooleanExpression)expression).paramName;

                    Conjunction conjunction = new Conjunction();
                    conjunction.literals = new Literal[1];
                    conjunction.literals[0] = literal;

                    Disjunction disjunction = new Disjunction();
                    disjunction.conjunctions = new Conjunction[1];
                    disjunction.conjunctions[0] = conjunction;
                    return disjunction;
                }

                if (expression is Ast.OrBooleanExpression) {
                    Ast.OrBooleanExpression orExpression = (Ast.OrBooleanExpression)expression;
                    Disjunction lhs = Disjunction.FromAst(orExpression.lhs);
                    Disjunction rhs = Disjunction.FromAst(orExpression.rhs);

                    var conjunctions = new List<Conjunction>();
                    conjunctions.AddRange(lhs.conjunctions);
                    conjunctions.AddRange(rhs.conjunctions);
                    Disjunction disjunction = new Disjunction();
                    disjunction.conjunctions = conjunctions.ToArray();
                    return disjunction;
                }

                if (expression is Ast.AndBooleanExpression) {
                    Ast.AndBooleanExpression andExpression = (Ast.AndBooleanExpression)expression;
                    Disjunction lhs = Disjunction.FromAst(andExpression.lhs);
                    Disjunction rhs = Disjunction.FromAst(andExpression.rhs);

                    var conjunctions = new List<Conjunction>();
                    for (int lhsIndex = 0; lhsIndex < lhs.conjunctions.Length; lhsIndex++) {
                        for (int rhsIndex = 0; rhsIndex < rhs.conjunctions.Length; rhsIndex++) {
                            var literals = new List<Literal>();
                            literals.AddRange(lhs.conjunctions[lhsIndex].literals);
                            literals.AddRange(rhs.conjunctions[rhsIndex].literals);

                            Conjunction conjunction = new Conjunction();
                            conjunction.literals = literals.ToArray();
                            conjunctions.Add(conjunction);
                        }
                    }

                    Disjunction disjunction = new Disjunction();
                    disjunction.conjunctions = conjunctions.ToArray();
                    return disjunction;
                }

                if (expression is Ast.NotBooleanExpression) {
                    var notExpression = ((Ast.NotBooleanExpression)expression).expression;
                    if (notExpression is Ast.ParamBooleanExpression) {
                        Disjunction disjunction = Disjunction.FromAst(notExpression);
                        var boolParamLiteral = (Compiler.BoolParamLiteral)
                            disjunction.conjunctions[0].literals[0];
                        boolParamLiteral.negated = true;
                        return disjunction;
                    }

                    if (notExpression is Ast.RelationalBooleanExpression) {
                        var origExpression = (Ast.RelationalBooleanExpression)notExpression;
                        var negatedExpression = new Ast.RelationalBooleanExpression();
                        negatedExpression.value = origExpression.value;
                        negatedExpression.paramName = origExpression.paramName;

                        switch (origExpression.op) {
                        case Ast.RelationalOperator.Equal:
                            negatedExpression.op = Ast.RelationalOperator.NotEqual;
                            break;
                        case Ast.RelationalOperator.NotEqual:
                            negatedExpression.op = Ast.RelationalOperator.Equal;
                            break;
                        case Ast.RelationalOperator.Less:
                            negatedExpression.op = Ast.RelationalOperator.GreaterEqual;
                            break;
                        case Ast.RelationalOperator.Greater:
                            negatedExpression.op = Ast.RelationalOperator.LessEqual;
                            break;
                        case Ast.RelationalOperator.LessEqual:
                            negatedExpression.op = Ast.RelationalOperator.Greater;
                            break;
                        case Ast.RelationalOperator.GreaterEqual:
                            negatedExpression.op = Ast.RelationalOperator.Less;
                            break;
                        }

                        return Disjunction.FromAst(negatedExpression);
                    }

                    // Double negation
                    if (notExpression is Ast.NotBooleanExpression) {
                        return Disjunction.FromAst(
                            ((Ast.NotBooleanExpression)notExpression).expression);
                    }

                    // De Morgan's laws

                    if (notExpression is Ast.OrBooleanExpression) {
                        Ast.OrBooleanExpression orExpression =
                            (Ast.OrBooleanExpression)notExpression;

                        Ast.AndBooleanExpression andExpression = new Ast.AndBooleanExpression();
                        andExpression.lhs = new Ast.NotBooleanExpression();
                        andExpression.rhs = new Ast.NotBooleanExpression();
                        ((Ast.NotBooleanExpression)andExpression.lhs).expression =
                            orExpression.lhs;
                        ((Ast.NotBooleanExpression)andExpression.rhs).expression =
                            orExpression.rhs;

                        return Disjunction.FromAst(andExpression);
                    }

                    if (notExpression is Ast.AndBooleanExpression) {
                        Ast.AndBooleanExpression andExpression =
                            (Ast.AndBooleanExpression)notExpression;

                        Ast.OrBooleanExpression orExpression = new Ast.OrBooleanExpression();
                        orExpression.lhs = new Ast.NotBooleanExpression();
                        orExpression.rhs = new Ast.NotBooleanExpression();
                        ((Ast.NotBooleanExpression)orExpression.lhs).expression =
                            andExpression.lhs;
                        ((Ast.NotBooleanExpression)orExpression.rhs).expression =
                            andExpression.rhs;

                        return Disjunction.FromAst(orExpression);
                    }

                    throw new Exception("Unknown Boolean expression type behind not");
                }

                throw new Exception("Unknown Boolean expression type");
            }
        }

        class Conjunction {
            public Literal[] literals;
        }

        abstract class Literal {
            public string paramName;
        }

        class BoolParamLiteral : Literal {
            public bool negated;
        }

        class FloatParamLiteral : Literal {
            public FloatRelationalOperator op;
            public double value;
        }

        enum FloatRelationalOperator {
            Greater,
            Less,
            GreaterEqual,
            LessEqual
        }

    } // end namespace VRCAnimScript

}   // end namespace VRCAnimScript
