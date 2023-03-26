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
    }

}   // end namespace VRCAnimScript
