// Editor/VRCAnimScriptCompiler.cs

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
    class Materializer {
        const double FLOAT_QUANTUM = 0.00001;

        Ast.Root root;

        GameObject rootObject;

        List<string> materializedAssetPaths;

        private static Dictionary<string, PropertyAbbreviation> mPropertyAbbreviations;

        private static Dictionary<string, PropertyAbbreviation> PropertyAbbreviations {
            get {
                if (mPropertyAbbreviations == null) {
                    mPropertyAbbreviations = new Dictionary<string, PropertyAbbreviation>();
                    mPropertyAbbreviations.Add("active",
                        new PropertyAbbreviation("GameObject", "m_IsActive"));
                    mPropertyAbbreviations.Add("rotation",
                        new PropertyAbbreviation("Transform", "localEulerAnglesRaw"));
                    mPropertyAbbreviations.Add("scale",
                        new PropertyAbbreviation("Transform", "m_LocalScale"));
                    mPropertyAbbreviations.Add("materials",
                        new PropertyAbbreviation("SkinnedMeshRenderer", "m_Materials"));
                    mPropertyAbbreviations.Add("blendshapes",
                        new PropertyAbbreviation("SkinnedMeshRenderer", "blendShape"));
                }
                return mPropertyAbbreviations;
            }
        }

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

        private VRC_AnimatorTrackingControl.TrackingType GetTrackingControl(
                CFG.Node cfgNode,
                int partIndex) {
            if ((cfgNode.trackingMask & (1 << partIndex)) == 0)
                return VRC_AnimatorTrackingControl.TrackingType.NoChange;
            if ((cfgNode.tracking & (1 << partIndex)) == 0)
                return VRC_AnimatorTrackingControl.TrackingType.Animation;
            return VRC_AnimatorTrackingControl.TrackingType.Tracking;
        }

        public void Materialize() {
            CompiledParams compiledParams = MaterializeParams();
            string mainMenuAssetPath = MaterializeMenu(compiledParams);
            string actionControllerActionPath =
                MaterializeController(root.actionController, "Action");
            string fxControllerActionPath = MaterializeController(root.fxController, "FX");

            ApplyMaterializedAssetsToAvatar(
                compiledParams.assetPath,
                mainMenuAssetPath,
                actionControllerActionPath,
                fxControllerActionPath);
        }

        private CompiledParams MaterializeParams() {
            Ast.Param[] allParams = root.allParams;

            var vrcParams = new VRCExpressionParameters();
            var paramTypes = new Dictionary<string, VRCExpressionParameters.ValueType>();

            vrcParams.parameters = new VRCExpressionParameters.Parameter[allParams.Length];
            for (int i = 0; i < allParams.Length; i++) {
                Ast.Param param = allParams[i];

                VRCExpressionParameters.ValueType valueType;
                float defaultValue;
                if (param is Ast.BoolParam) {
                    var boolParam = (Ast.BoolParam)param;
                    valueType = VRCExpressionParameters.ValueType.Bool;
                    defaultValue = boolParam.initialValue ? 1.0f : 0.0f;
                } else if (param is Ast.FloatParam) {
                    var floatParam = (Ast.FloatParam)param;
                    valueType = VRCExpressionParameters.ValueType.Float;
                    defaultValue = (float)floatParam.initialValue;
                } else {
                    throw new Exception("Unknown parameter type");
                }

                if (paramTypes.ContainsKey(param.name))
                    throw new Exception("Param " + param.name + " defined multiple times");
                paramTypes.Add(param.name, valueType);

                var vrcParam = new VRCExpressionParameters.Parameter();
                vrcParam.name = param.name;
                vrcParam.saved = param.saved;
                vrcParam.valueType = valueType;
                vrcParam.defaultValue = defaultValue;
                vrcParams.parameters[i] = vrcParam;
            }

            string assetPath = MaterializeAsset(
                vrcParams,
                GenerateAssetName("Parameters"),
                "parameters");

            return new CompiledParams(paramTypes, assetPath);
        }

        private string MaterializeMenu(CompiledParams compiledParams) {
            Ast.Menu mainMenu = root.mainMenu;
            Ast.MenuItem[] menuItems = mainMenu.items;

            var vrcMenu = new VRCExpressionsMenu();
            string menuName = GenerateAssetName("MainMenu");
            vrcMenu.name = menuName;

            for (int i = 0; i < menuItems.Length; i++) {
                Ast.MenuItem menuItem = menuItems[i];
                var vrcMenuItem = new VRCExpressionsMenu.Control();
                vrcMenuItem.name = menuItem.name;

                if (menuItem.icon != null) {
                    string texturePath = GetAssetPath<Texture2D>(menuItem.icon);
                    if (texturePath == null)
                        Debug.LogWarning("Couldn't find menu item texture " + vrcMenuItem.icon);
                    else
                        vrcMenuItem.icon = AssetDatabase.LoadAssetAtPath<Texture2D>(texturePath);
                }

                vrcMenuItem.parameter = new VRCExpressionsMenu.Control.Parameter();
                vrcMenuItem.parameter.name = menuItem.param;

                if (!compiledParams.paramTypes.ContainsKey(menuItem.param))
                    throw new Exception("Menu item controls unknown parameter " + menuItem.param);

                switch (compiledParams.paramTypes[menuItem.param]) {
                    case VRCExpressionParameters.ValueType.Bool:
                        vrcMenuItem.type = VRCExpressionsMenu.Control.ControlType.Toggle;
                        break;
                    case VRCExpressionParameters.ValueType.Float:
                        vrcMenuItem.type = VRCExpressionsMenu.Control.ControlType.RadialPuppet;
                        break;
                    default:
                        throw new Exception("Unknown parameter value type");
                }

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

                int unityLayerIndex = unityLayers.Length - 1;
                AnimatorControllerLayer unityLayer = unityLayers[unityLayerIndex];
                AnimatorStateMachine unityStateMachine = unityLayer.stateMachine;

                unityLayer.defaultWeight = 1.0f;
                if (layer.additive)
                    unityLayer.blendingMode = AnimatorLayerBlendingMode.Additive;
                else
                    unityLayer.blendingMode = AnimatorLayerBlendingMode.Override;

                // Create the CFG. Compute tracking.
                CFG.Graph cfg = new CFG.Graph(layer, isAction);
                cfg.ComputeTrackingMasks();

                AnimatorState[] unityStates = new AnimatorState[layer.states.Length];
                var nameToUnityState = new Dictionary<string, AnimatorState>();

                // Materialize states.
                for (int stateIndex = 0; stateIndex < layer.states.Length; stateIndex++) {
                    AnimatorState unityState = MaterializeState(
                        layer.states[stateIndex],
                        unityStateMachine,
                        unityController,
                        assetName,
                        unityLayerIndex,
                        layer.name,
                        cfg,
                        isAction);
                    unityStates[stateIndex] = unityState;
                    nameToUnityState.Add(unityStates[stateIndex].name, unityState);
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

        private AnimatorState MaterializeState(
                Ast.State state,
                AnimatorStateMachine unityStateMachine,
                AnimatorController controller,
                string controllerAssetName,
                int unityLayerIndex,
                string layerName,
                CFG.Graph cfg,
                bool isAction) {
            AnimatorState unityState;
            BlendTree blendTree = null;
            if (state is Ast.RegularState) {
                unityState = unityStateMachine.AddState(state.name);
            } else if (state is Ast.BlendState) {
                unityState = controller.CreateBlendTreeInController(
                    state.name, out blendTree, unityLayerIndex);
            } else
                throw new Exception("Unknown state type");

            // Turn action layer on and off as appropriate.
            if (isAction) {
                var vrcPlayableLayerControl =
                    unityState.AddStateMachineBehaviour<VRCPlayableLayerControl>();
                vrcPlayableLayerControl.layer = VRC_PlayableLayerControl.BlendableLayer.Action;

                if (CFG.StateUtils.StateHasAnimations(state))
                    vrcPlayableLayerControl.goalWeight = 1.0f;
                else
                    vrcPlayableLayerControl.goalWeight = 0.0f;
            }

            // Create playable layer control if necessary.
            CFG.Node cfgNode = cfg.nodes[cfg.nodeNames[state.name]];
            if (cfgNode.trackingMask != 0) {
                var vrcAnimatorTrackingControl =
                    unityState.AddStateMachineBehaviour<VRCAnimatorTrackingControl>();
                vrcAnimatorTrackingControl.trackingHead = GetTrackingControl(cfgNode, 0);
                vrcAnimatorTrackingControl.trackingLeftHand = GetTrackingControl(cfgNode, 1);
                vrcAnimatorTrackingControl.trackingRightHand = GetTrackingControl(cfgNode, 2);
                vrcAnimatorTrackingControl.trackingHip = GetTrackingControl(cfgNode, 3);
                vrcAnimatorTrackingControl.trackingLeftFoot = GetTrackingControl(cfgNode, 4);
                vrcAnimatorTrackingControl.trackingRightFoot = GetTrackingControl(cfgNode, 5);
                vrcAnimatorTrackingControl.trackingLeftFingers = GetTrackingControl(cfgNode, 6);
                vrcAnimatorTrackingControl.trackingRightFingers = GetTrackingControl(cfgNode, 7);
                vrcAnimatorTrackingControl.trackingEyes = GetTrackingControl(cfgNode, 8);
                vrcAnimatorTrackingControl.trackingMouth = GetTrackingControl(cfgNode, 9);
            }

            // Materialize animation.

            if (state is Ast.RegularState) {
                Ast.RegularState regularState = (Ast.RegularState)state;
                if (regularState.animation != null) {
                    string animationName = CreateInlineAnimationName(
                        controllerAssetName,
                        layerName,
                        state.name,
                        null);
                    unityState.motion = MaterializeAnimation(
                        regularState.animation, animationName);
                }
                return unityState;
            }

            if (state is Ast.BlendState) {
                Ast.BlendState blendState = (Ast.BlendState)state;
                foreach (Ast.BlendStateKeyframe keyframe in blendState.keyframes) {
                    string animationName = CreateInlineAnimationName(
                        controllerAssetName,
                        layerName,
                        state.name,
                        keyframe.time);
                    Motion motion = MaterializeAnimation(keyframe.animation, animationName);
                    blendTree.AddChild(motion, (float)keyframe.time);
                }
                return unityState;
            }

            throw new Exception("Unknown state type");
        }

        private Motion MaterializeAnimation(Ast.Animation animation, string inlineAnimationName) {
            string animationPath = "";
            if (animation is Ast.InlineAnimation) {
                animationPath = MaterializeInlineAnimation(
                    (Ast.InlineAnimation)animation,
                    inlineAnimationName);
            } else if (animation is Ast.ExternalAnimation) {
                var external = (Ast.ExternalAnimation)animation;
                animationPath = GetAssetPath<AnimationClip>(external.name);

                if (animationPath == null) {
                    string errorMessage = "Couldn't find external animation clip " + external;
                    Debug.LogWarning(errorMessage);
                }
            } else {
                throw new Exception("Unhandled animation type");
            }

            return AssetDatabase.LoadAssetAtPath<AnimationClip>(animationPath);
        }

        private void MaterializeParamTransition(
                Ast.ParamTransitionCondition paramCondition,
                AnimatorState source,
                AnimatorState destination,
                Dictionary<string, AnimatorControllerParameterType> allParameters) {
            DNF.Disjunction disjunction =
                DNF.Disjunction.FromAst(paramCondition.expression);

            foreach (DNF.Conjunction conjunction in disjunction.conjunctions) {
                AnimatorStateTransition unityTransition = source.AddTransition(destination);

                // TODO: Make these customizable?
                unityTransition.duration = 0.0f;
                unityTransition.hasExitTime = false;

                foreach (DNF.Literal literal in conjunction.literals) {
                    AnimatorConditionMode conditionMode = AnimatorConditionMode.If;
                    double testValue = 0.0;
                    var paramType = AnimatorControllerParameterType.Bool;

                    if (literal is DNF.BoolParamLiteral) {
                        DNF.BoolParamLiteral boolParamLiteral =
                            (DNF.BoolParamLiteral)literal;
                        testValue = 0.0;
                        paramType = AnimatorControllerParameterType.Bool;

                        if (boolParamLiteral.negated)
                            conditionMode = AnimatorConditionMode.IfNot;
                        else
                            conditionMode = AnimatorConditionMode.If;
                    } else if (literal is DNF.FloatParamLiteral) {
                        DNF.FloatParamLiteral floatParamLiteral =
                            (DNF.FloatParamLiteral)literal;
                        paramType = AnimatorControllerParameterType.Float;

                        switch (floatParamLiteral.op) {
                            case DNF.FloatRelationalOperator.Less:
                                conditionMode = AnimatorConditionMode.Less;
                                testValue = floatParamLiteral.value;
                                break;
                            case DNF.FloatRelationalOperator.Greater:
                                conditionMode = AnimatorConditionMode.Greater;
                                testValue = floatParamLiteral.value;
                                break;
                            case DNF.FloatRelationalOperator.LessEqual:
                                conditionMode = AnimatorConditionMode.Less;
                                testValue = floatParamLiteral.value + FLOAT_QUANTUM;
                                break;
                            case DNF.FloatRelationalOperator.GreaterEqual:
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

            if (expression is Ast.BoolExpression) {
                EditorCurveBinding binding = new EditorCurveBinding();
                binding.path = path;
                binding.type = type;
                binding.propertyName = propertyName;
                iterator(binding, ((Ast.BoolExpression)expression).value ? 1.0f : 0.0f);
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
                string animationName) {
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

                    string componentName;
                    string propertyPath = "";
                    var firstPropertyComponent = (Ast.NamePropertyComponent)
                        frameAction.property[0];
                    if (PropertyAbbreviations.ContainsKey(firstPropertyComponent.name)) {
                        PropertyAbbreviation abbreviation =
                            PropertyAbbreviations[firstPropertyComponent.name];
                        componentName = abbreviation.component;
                        propertyPath = abbreviation.property;
                    } else {
                        componentName = firstPropertyComponent.name;
                    }

                    for (int componentIndex = 1;
                            componentIndex < frameAction.property.Length;
                            componentIndex++) {
                        Ast.PropertyComponent propertyComponent =
                            frameAction.property[componentIndex];
                        if (propertyComponent is Ast.NumberPropertyComponent) {
                            // Automatically add `.Array.data`.
                            propertyPath += ".Array.data[" +
                                ((Ast.NumberPropertyComponent)propertyComponent).value + "]";
                        } else if (propertyComponent is Ast.NamePropertyComponent) {
                            if (propertyPath.Length > 0)
                                propertyPath += ".";
                            propertyPath += ((Ast.NamePropertyComponent)propertyComponent).name;
                        } else {
                            throw new Exception("Unhandled property component type");
                        }
                    }

                    // Figure out which component we're targeting.
                    Type componentType = null;
                    if (componentName.Equals("GameObject")) {
                        componentType = typeof(GameObject);
                    } else {
                        Component[] allComponents = gameObject.GetComponents<Component>();
                        foreach (Component component in allComponents) {
                            if (BaseNameOfType(component.GetType()).Equals(componentName)) {
                                componentType = component.GetType();
                                break;
                            }
                        }
                    }
                    if (componentType == null) {
                        Debug.LogWarning("Failed to find component of type " + componentName);
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
                            binding.propertyName = propertyPath;
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
                            propertyPath,
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

            return MaterializeAsset(unityAnimation, animationName, "animation", "anim");
        }

        private static string CreateInlineAnimationName(
                string controllerName,
                string layerName,
                string stateName,
                double? blendTime) {
            string animationName = controllerName + "_" + layerName + "_" + stateName;
            if (blendTime != null) {
                string blendString = blendTime.ToString();
                foreach (char ch in blendString) {
                    if (Char.IsDigit(ch))
                        animationName += ch;
                    else
                        animationName += '_';
                }
            }
            return animationName;
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
}
