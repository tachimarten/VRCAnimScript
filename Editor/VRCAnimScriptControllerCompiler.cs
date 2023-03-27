// Editor/VRCAnimScriptControllerCompiler.cs

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
    class ControllerCompiler {
        const double FLOAT_QUANTUM = 0.00001;

        private delegate void IterateBindings(EditorCurveBinding binding, float value);

        private Ast.Root root;
        private GameObject rootObject;
        private TrackingAnalysis.TrackingAnalysis trackingAnalysis;
        private AssetCompiler assetCompiler;

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


        public ControllerCompiler(
                Ast.Root root,
                GameObject rootObject,
                TrackingAnalysis.TrackingAnalysis trackingAnalysis,
                AssetCompiler assetCompiler) {
            this.root = root;
            this.rootObject = rootObject;
            this.trackingAnalysis = trackingAnalysis;
            this.assetCompiler = assetCompiler;
        }

        public string MaterializeController(
                Ast.ControllerSection controllerSection,
                string controllerName,
                TrackingAnalysis.LayerGraph[] trackingGraphs) {
            bool isAction = controllerName.Equals("Action");

            string assetName = Materializer.GenerateAssetName(controllerName + "Controller", root);
            string assetPath = AssetCompiler.PrepareToMaterializeAsset(assetName, "controller");

            AnimatorController unityController;
            if (controllerSection.extends == null) {
                unityController = AnimatorController.CreateAnimatorControllerAtPath(assetPath);
            } else {
                string[] srcAssetPaths = Materializer.GetAssetPathsByName<AnimatorController>(
                    controllerSection.extends);
                if (srcAssetPaths.Length == 0) {
                    throw new Exception("Couldn't find controller to extend named \"" +
                        controllerSection.extends + "\"");
                } else if (srcAssetPaths.Length > 1) {
                    Debug.LogWarning("Ambiguous controller to extend \"" +
                        controllerSection.extends + "\"");
                }
                string srcAssetPath = srcAssetPaths[0];

                if (!AssetDatabase.CopyAsset(srcAssetPath, assetPath)) {
                    throw new Exception("Failed to initialize controller extending \"" +
                        controllerSection.extends + "\"");
                }
                unityController = AssetDatabase.LoadAssetAtPath<AnimatorController>(assetPath);
            }

            if (unityController == null)
                throw new Exception("Failed to create Unity controller " + assetName);

            var allParameters = new Dictionary<string, AnimatorControllerParameterType>();

            // Compile layers.
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

                // Create the CFG.
                AnimatorState[] unityStates = new AnimatorState[layer.states.Length];
                var nameToUnityState = new Dictionary<string, AnimatorState>();

                TrackingAnalysis.LayerGraph layerGraph = trackingGraphs[layerIndex];

                // Materialize states.
                for (int stateIndex = 0; stateIndex < layer.states.Length; stateIndex++) {
                    AnimatorState unityState = MaterializeState(
                        layer.states[stateIndex],
                        unityStateMachine,
                        unityController,
                        controllerName,
                        assetName,
                        unityLayerIndex,
                        layer.name,
                        layerGraph,
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

            // Compile user parameters.
            foreach (KeyValuePair<string, AnimatorControllerParameterType> kvp in allParameters)
                unityController.AddParameter(kvp.Key, kvp.Value);

            // Compile transition parameters.

            if (isAction) {
                // Action controllers need all parameters to be declared.
                foreach (List<string> trackingParameters in trackingAnalysis.trackingParameters) {
                    foreach (string trackingParameter in trackingParameters) {
                        unityController.AddParameter(
                            trackingParameter, AnimatorControllerParameterType.Bool);
                    }
                }
            } else {
                // Otherwise, only declare the parameters for this layer.
                foreach (TrackingAnalysis.LayerGraph layerGraph in trackingGraphs) {
                    uint usedParts = layerGraph.UntrackedParts;
                    for (int partIndex = 0;
                            partIndex < TrackingAnalysis.Node.Parts.Length;
                            partIndex++) {
                        if ((usedParts & (1 << partIndex)) == 0)
                            continue;
                        string paramName = TrackingAnalysis.TrackingAnalysis.GetTrackingParamName(
                            controllerName,
                            layerGraph.name,
                            partIndex);
                        unityController.AddParameter(paramName, AnimatorControllerParameterType.Bool);
                    }
                }
            }

            if (isAction)
                MaterializeTrackingLayers(unityController);

            EditorUtility.SetDirty(unityController);
            AssetDatabase.SaveAssets();

            return assetPath;
        }

        private AnimatorState MaterializeState(
                Ast.State state,
                AnimatorStateMachine unityStateMachine,
                AnimatorController controller,
                string controllerType,
                string controllerAssetName,
                int unityLayerIndex,
                string layerName,
                TrackingAnalysis.LayerGraph layerGraph,
                bool isAction) {
            AnimatorState unityState;
            BlendTree blendTree = null;
            if (state is Ast.RegularState) {
                unityState = unityStateMachine.AddState(state.name);
            } else if (state is Ast.BlendState) {
                var blendState = (Ast.BlendState)state;
                unityState = controller.CreateBlendTreeInController(
                    state.name, out blendTree, unityLayerIndex);
                blendTree.blendParameter = blendState.param;
            } else
                throw new Exception("Unknown state type");

            // Turn action layer on and off as appropriate.
            if (isAction) {
                var vrcPlayableLayerControl =
                    unityState.AddStateMachineBehaviour<VRCPlayableLayerControl>();
                vrcPlayableLayerControl.layer = VRC_PlayableLayerControl.BlendableLayer.Action;

                if (TrackingAnalysis.StateUtils.StateHasAnimations(state))
                    vrcPlayableLayerControl.goalWeight = 1.0f;
                else
                    vrcPlayableLayerControl.goalWeight = 0.0f;
            }

            // Create parameter modifications if necessary.
            TrackingAnalysis.Node cfgNode = layerGraph.nodes[layerGraph.nodeNames[state.name]];
            if (cfgNode.trackingMask != 0) {
                for (int partIndex = 0;
                        partIndex < TrackingAnalysis.Node.Parts.Length;
                        partIndex++) {
                    if ((cfgNode.trackingMask & (1 << partIndex)) == 0)
                        continue;

                    var parameter = new VRC_AvatarParameterDriver.Parameter();
                    parameter.type = VRC_AvatarParameterDriver.ChangeType.Set;
                    if ((cfgNode.tracking & (1 << partIndex)) != 0)
                        parameter.value = 1.0f;
                    else
                        parameter.value = 0.0f;
                    parameter.name = TrackingAnalysis.TrackingAnalysis.GetTrackingParamName(
                        controllerType,
                        layerName,
                        partIndex);

                    var driver = unityState.AddStateMachineBehaviour<VRCAvatarParameterDriver>();
                    driver.localOnly = true;
                    driver.parameters.Add(parameter);
                }
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

        private void MaterializeTrackingLayers(AnimatorController controller) {
            for (int partIndex = 0; partIndex < TrackingAnalysis.Node.Parts.Length; partIndex++) {
                string layerName = "VRCAnimScriptTracking" +
                    TrackingAnalysis.Node.Parts[partIndex];
                controller.AddLayer(layerName);

                AnimatorControllerLayer[] layers = controller.layers;
                AnimatorControllerLayer layer = layers[layers.Length - 1];
                AnimatorStateMachine stateMachine = layer.stateMachine;

                layer.defaultWeight = 1.0f;
                layer.blendingMode = AnimatorLayerBlendingMode.Override;

                AnimatorState trackedState = stateMachine.AddState("Tracked");
                AnimatorState untrackedState = stateMachine.AddState("Untracked");
                CreateTrackingControlBehavior(trackedState, partIndex, true);
                CreateTrackingControlBehavior(untrackedState, partIndex, false);

                stateMachine.AddEntryTransition(trackedState);

                AnimatorStateTransition untrackedToTracked =
                    untrackedState.AddTransition(trackedState);
                untrackedToTracked.duration = 0.0f;
                untrackedToTracked.hasExitTime = false;

                foreach (string parameterName in trackingAnalysis.trackingParameters[partIndex]) {
                    AnimatorStateTransition trackedToUntracked =
                        trackedState.AddTransition(untrackedState);
                    trackedToUntracked.duration = 0.0f;
                    trackedToUntracked.hasExitTime = false;

                    trackedToUntracked.AddCondition(
                        AnimatorConditionMode.IfNot,
                        0.0f,
                        parameterName);
                    untrackedToTracked.AddCondition(AnimatorConditionMode.If, 0.0f, parameterName);
                }
            }
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

        private Motion MaterializeAnimation(Ast.Animation animation, string inlineAnimationName) {
            if (animation is Ast.InlineAnimation) {
                string animationPath = MaterializeInlineAnimation(
                    (Ast.InlineAnimation)animation,
                    inlineAnimationName);
                return AssetDatabase.LoadAssetAtPath<AnimationClip>(animationPath);
            }
            
            if (animation is Ast.ExternalAnimation) {
                var external = (Ast.ExternalAnimation)animation;
                AnimationClip animationClip =
                    Materializer.LoadAssetByName<AnimationClip>(external.name);

                if (animationClip == null) {
                    string errorMessage = "Couldn't find external animation clip " + external.name;
                    Debug.LogWarning(errorMessage);
                }

                if (!external.noBlendShapes)
                    return animationClip;

                string newAnimationPath = DuplicateAndRemoveBlendShapes(
                    animationClip, inlineAnimationName);
                return AssetDatabase.LoadAssetAtPath<AnimationClip>(newAnimationPath);
            }

            throw new Exception("Unhandled animation type");
        }

        private void CreateTrackingControlBehavior(
                AnimatorState animatorState, int partIndex, bool tracked) {
            var vrcATC = animatorState.AddStateMachineBehaviour<VRCAnimatorTrackingControl>();
            vrcATC.trackingHead = GetTrackingControl(0, partIndex, tracked);
            vrcATC.trackingLeftHand = GetTrackingControl(1, partIndex, tracked);
            vrcATC.trackingRightHand = GetTrackingControl(2, partIndex, tracked);
            vrcATC.trackingHip = GetTrackingControl(3, partIndex, tracked);
            vrcATC.trackingLeftFoot = GetTrackingControl(4, partIndex, tracked);
            vrcATC.trackingRightFoot = GetTrackingControl(5, partIndex, tracked);
            vrcATC.trackingLeftFingers = GetTrackingControl(6, partIndex, tracked);
            vrcATC.trackingRightFingers = GetTrackingControl(7, partIndex, tracked);
            vrcATC.trackingEyes = GetTrackingControl(8, partIndex, tracked);
            vrcATC.trackingMouth = GetTrackingControl(9, partIndex, tracked);
        }

        private VRC_AnimatorTrackingControl.TrackingType GetTrackingControl(
                int partIndex,
                int currentPartIndex,
                bool tracked) {
            if (partIndex != currentPartIndex)
                return VRC_AnimatorTrackingControl.TrackingType.NoChange;
            if (tracked)
                return VRC_AnimatorTrackingControl.TrackingType.Tracking;
            return VRC_AnimatorTrackingControl.TrackingType.Animation;
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
                            if (Materializer.BaseNameOfType(component.GetType()).Equals(
                                    componentName)) {
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
                        string[] guids = AssetDatabase.FindAssets(Materializer.DoubleQuote(
                            assetExpression.name));
                        foreach (string guid in guids) {
                            string assetPath = AssetDatabase.GUIDToAssetPath(guid);
                            if (!Materializer.GetBaseName(assetPath).Equals(assetExpression.name))
                                continue;
                            // TODO: Support multiple assets in the same file.
                            Type assetType = AssetDatabase.GetMainAssetTypeAtPath(assetPath);
                            if (Materializer.BaseNameOfType(assetType) != assetExpression.type)
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

            return assetCompiler.MaterializeAsset(
                unityAnimation,
                animationName,
                "animation",
                "anim");
        }

        private string DuplicateAndRemoveBlendShapes(
                AnimationClip originalAnimation,
                string newAnimationName) {
            AnimationClip newAnimation = new AnimationClip();

            // Copy over float curves, except for blendshapes.
            foreach (EditorCurveBinding curveBinding in
                    AnimationUtility.GetCurveBindings(originalAnimation)) {
                if (!curveBinding.propertyName.StartsWith("blendShape.")) {
                    AnimationCurve curve =
                        AnimationUtility.GetEditorCurve(originalAnimation, curveBinding);
                    AnimationUtility.SetEditorCurve(newAnimation, curveBinding, curve);
                }
            }

            // Copy over object reference curves.
            foreach (EditorCurveBinding curveBinding in
                    AnimationUtility.GetObjectReferenceCurveBindings(originalAnimation)) {
                ObjectReferenceKeyframe[] curve =
                    AnimationUtility.GetObjectReferenceCurve(originalAnimation, curveBinding);
                AnimationUtility.SetObjectReferenceCurve(newAnimation, curveBinding, curve);
            }

            // Copy over events.
            AnimationEvent[] events = AnimationUtility.GetAnimationEvents(originalAnimation);
            AnimationUtility.SetAnimationEvents(newAnimation, events);

            return assetCompiler.MaterializeAsset(
                newAnimation, newAnimationName, "animation", "anim");
        }

        private string GetRootObjectRelativePath(GameObject obj) {
            if (obj.transform.parent == null || obj.transform.parent.gameObject == rootObject)
                return obj.name;
            return GetRootObjectRelativePath(obj.transform.parent.gameObject) + "/" + obj.name;
        }

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
    }
} // end namespace VRCAnimScript
