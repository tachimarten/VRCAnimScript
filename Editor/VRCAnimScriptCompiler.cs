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
        private Ast.Root root;

        private GameObject rootObject;

        private TrackingAnalysis.TrackingAnalysis trackingAnalysis;

        private AssetCompiler assetCompiler;

        private static string[] mControllerNames;

        public static string[] ControllerNames {
            get {
                if (mControllerNames == null) {
                    mControllerNames =
                        new string[5] {"Base", "Additive", "Gesture", "Action", "FX"};
                }
                return mControllerNames;
            }
        }

        public Materializer(Ast.Root root) {
            this.root = root;

            rootObject = GameObject.Find(root.rootObject);
            if (rootObject == null)
                throw new Exception("Couldn't find a game object named " + rootObject);

            trackingAnalysis = new TrackingAnalysis.TrackingAnalysis();

            assetCompiler = new AssetCompiler();
        }

        public static string DoubleQuote(string input) {
            return "\"" + input.Replace("\"", "\\\"") + "\"";
        }

        public static string GetBaseName(string path) {
            int lastSlashIndex = path.LastIndexOf('/');
            string fileNameWithExtension = (lastSlashIndex == -1) ? path : path.Substring(lastSlashIndex + 1);
            int lastDotIndex = fileNameWithExtension.LastIndexOf('.');
            return (lastDotIndex == -1) ? fileNameWithExtension : fileNameWithExtension.Substring(0, lastDotIndex);
        }

        public static string GenerateAssetName(string suffix, Ast.Root root) {
            return root.avatar + "Gen" + suffix;
        }

        public static string BaseNameOfType(Type type) {
            string fullName = type.FullName;
            return fullName.Substring(fullName.LastIndexOf('.') + 1);
        }

        public static string[] GetAssetPathsByName<T>(string name) {
            string query = DoubleQuote(name) + " t:" + BaseNameOfType(typeof(T));
            string[] guids = AssetDatabase.FindAssets(query);

            List<string> assetPaths = new List<string>();
            foreach (string guid in guids) {
                string assetPath = AssetDatabase.GUIDToAssetPath(guid);
                assetPaths.Add(assetPath);
            }
            return assetPaths.ToArray();
        }

        public static T LoadAssetByName<T>(string name) where T: UnityEngine.Object {
            string[] assetPaths = GetAssetPathsByName<T>(name);
            foreach (string assetPath in assetPaths) {
                UnityEngine.Object[] assets = AssetDatabase.LoadAllAssetsAtPath(assetPath);
                foreach (UnityEngine.Object asset in assets) {
                    if (asset is T && asset.name.Equals(name))
                        return (T)asset;
                }
            }
            return null;
        }

        public void Materialize() {
            // Analysis
            Dictionary<string, VRCExpressionParameters.ValueType> paramTypes = TypecheckParams();

            TrackingAnalysis.LayerGraph[][] trackingGraphs =
                new TrackingAnalysis.LayerGraph[root.controllerSections.Length][];
            for (int controllerIndex = 0;
                    controllerIndex < root.controllerSections.Length;
                    controllerIndex++) {
                Ast.ControllerSection controllerSection = root.controllerSections[controllerIndex];
                if (controllerSection != null) {
                    trackingGraphs[controllerIndex] = AnalyzeTracking(
                        root.controllerSections[controllerIndex],
                        ControllerNames[controllerIndex]);
                }
            }

            // Compilation
            string paramsAssetPath = MaterializeParams(paramTypes);
            string mainMenuAssetPath = MaterializeMenuAsset(root.mainMenu, paramTypes);

            string[] controllerAssetPaths = new string[root.controllerSections.Length];
            for (int controllerIndex = 0;
                    controllerIndex < root.controllerSections.Length;
                    controllerIndex++) {
                Ast.ControllerSection controllerSection = root.controllerSections[controllerIndex];
                if (controllerSection != null) {
                    ControllerCompiler controllerCompiler =
                        new ControllerCompiler(root, rootObject, trackingAnalysis, assetCompiler);
                    controllerAssetPaths[controllerIndex] =
                        controllerCompiler.MaterializeController(
                            controllerSection,
                            ControllerNames[controllerIndex],
                            trackingGraphs[controllerIndex]);
                }
            }

            ApplyMaterializedAssetsToAvatar(
                paramsAssetPath,
                mainMenuAssetPath,
                controllerAssetPaths);
        }

        private Dictionary<string, VRCExpressionParameters.ValueType> TypecheckParams() {
            Ast.Param[] allParams = root.allParams;
            var paramTypes = new Dictionary<string, VRCExpressionParameters.ValueType>();

            for (int i = 0; i < allParams.Length; i++) {
                Ast.Param param = allParams[i];

                VRCExpressionParameters.ValueType valueType;
                if (param is Ast.BoolParam)
                    valueType = VRCExpressionParameters.ValueType.Bool;
                else if (param is Ast.FloatParam)
                    valueType = VRCExpressionParameters.ValueType.Float;
                else
                    throw new Exception("Unknown parameter type");

                if (paramTypes.ContainsKey(param.name))
                    throw new Exception("Param " + param.name + " defined multiple times");
                paramTypes.Add(param.name, valueType);
            }

            return paramTypes;
        }

        private TrackingAnalysis.LayerGraph[] AnalyzeTracking(
                Ast.ControllerSection controllerSection, string controllerName) {
            bool isAction = controllerName.Equals("Action");
            TrackingAnalysis.LayerGraph[] layerGraphs =
                new TrackingAnalysis.LayerGraph[controllerSection.layers.Length];

            for (int layerIndex = 0; layerIndex < controllerSection.layers.Length; layerIndex++) {
                Ast.Layer layer = controllerSection.layers[layerIndex];
                TrackingAnalysis.LayerGraph layerGraph = new TrackingAnalysis.LayerGraph();

                layerGraph.PopulateGraph(layer, isAction);
                layerGraph.ComputeTrackingMasks();
                trackingAnalysis.ProcessLayerGraph(controllerName, layerGraph);
                layerGraphs[layerIndex] = layerGraph;
            }

            return layerGraphs;
        }

        private string MaterializeParams(
                Dictionary<string, VRCExpressionParameters.ValueType> paramTypes) {
            Ast.Param[] allParams = root.allParams;

            var parameters = new List<VRCExpressionParameters.Parameter>();

            for (int i = 0; i < allParams.Length; i++) {
                Ast.Param param = allParams[i];

                VRCExpressionParameters.ValueType valueType = paramTypes[param.name];
                float defaultValue;
                if (param is Ast.BoolParam) {
                    var boolParam = (Ast.BoolParam)param;
                    defaultValue = boolParam.initialValue ? 1.0f : 0.0f;
                } else if (param is Ast.FloatParam) {
                    var floatParam = (Ast.FloatParam)param;
                    defaultValue = (float)floatParam.initialValue;
                } else {
                    throw new Exception("Unknown parameter type");
                }

                var vrcParam = new VRCExpressionParameters.Parameter();
                vrcParam.name = param.name;
                vrcParam.saved = param.saved;
                vrcParam.valueType = valueType;
                vrcParam.defaultValue = defaultValue;
                parameters.Add(vrcParam);
            }

            // Create tracking parameters.
            foreach (List<string> trackingParameters in trackingAnalysis.trackingParameters) {
                foreach (string parameterName in trackingParameters) {
                    var vrcParam = new VRCExpressionParameters.Parameter();
                    vrcParam.name = parameterName;
                    vrcParam.saved = false;
                    vrcParam.valueType = VRCExpressionParameters.ValueType.Bool;
                    vrcParam.defaultValue = 1.0f;
                    parameters.Add(vrcParam);
                }
            }

            var vrcParams = new VRCExpressionParameters();
            vrcParams.parameters = parameters.ToArray();

            string assetPath = assetCompiler.MaterializeAsset(
                vrcParams,
                GenerateAssetName("Parameters", root),
                "parameters");

            return assetPath;
        }

        private string MaterializeMenuAsset(
                Ast.MenuItem[] menuItems,
                Dictionary<string, VRCExpressionParameters.ValueType> paramTypes) {
            string assetName = GenerateAssetName("MainMenu", root);
            VRCExpressionsMenu vrcMenu = MaterializeMenu(menuItems, paramTypes, assetName);
            return assetCompiler.MaterializeAsset(vrcMenu, assetName, "menu");
        }

        private VRCExpressionsMenu MaterializeMenu(
                Ast.MenuItem[] menuItems,
                Dictionary<string, VRCExpressionParameters.ValueType> paramTypes,
                string menuName) {
            var vrcMenu = new VRCExpressionsMenu();
            vrcMenu.name = menuName;

            for (int i = 0; i < menuItems.Length; i++) {
                Ast.MenuItem menuItem = menuItems[i];
                var vrcMenuItem = new VRCExpressionsMenu.Control();
                vrcMenuItem.name = menuItem.name;

                if (menuItem.icon != null) {
                    Texture2D texture = LoadAssetByName<Texture2D>(menuItem.icon);
                    if (texture == null)
                        Debug.LogWarning("Couldn't find menu item texture " + menuItem.icon);
                    else
                        vrcMenuItem.icon = texture;
                }

                if (menuItem is Ast.ParamMenuItem) {
                    var paramMenuItem = (Ast.ParamMenuItem)menuItem;

                    if (!paramTypes.ContainsKey(paramMenuItem.param)) {
                        throw new Exception("Menu item controls unknown parameter " +
                            paramMenuItem.param);
                    }

                    switch (paramTypes[paramMenuItem.param]) {
                        case VRCExpressionParameters.ValueType.Bool:
                            vrcMenuItem.parameter = new VRCExpressionsMenu.Control.Parameter();
                            vrcMenuItem.parameter.name = paramMenuItem.param;
                            vrcMenuItem.type = VRCExpressionsMenu.Control.ControlType.Toggle;
                            break;
                        case VRCExpressionParameters.ValueType.Float:
                            vrcMenuItem.subParameters = new VRCExpressionsMenu.Control.Parameter[1] {
                                new VRCExpressionsMenu.Control.Parameter()
                            };
                            vrcMenuItem.subParameters[0].name = paramMenuItem.param;
                            vrcMenuItem.type = VRCExpressionsMenu.Control.ControlType.RadialPuppet;
                            break;
                        default:
                            throw new Exception("Unknown parameter value type");
                    }
                } else if (menuItem is Ast.SubmenuItem) {
                    var submenuItem = (Ast.SubmenuItem)menuItem;
                    string submenuName = menuName + menuItem.name;
                    VRCExpressionsMenu vrcSubmenu = MaterializeMenu(
                        submenuItem.items, paramTypes, submenuName);
                    assetCompiler.MaterializeAsset(vrcSubmenu, submenuName, "menu");
                    vrcMenuItem.subMenu = vrcSubmenu;
                    vrcMenuItem.type = VRCExpressionsMenu.Control.ControlType.SubMenu;
                } else {
                    throw new Exception("Unknown menu item type");
                }

                vrcMenu.controls.Add(vrcMenuItem);
            }

            return vrcMenu;
        }

        private void ApplyMaterializedAssetsToAvatar(
            string paramsAssetPath,
            string mainMenuAssetPath,
            string[] controllerAssetPaths
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

            for (int controllerIndex = 0;
                    controllerIndex < controllerAssetPaths.Length;
                    controllerIndex++) {
                if (controllerAssetPaths[controllerIndex] != null) {
                    ModifyCustomAnimLayer(
                        ref descriptor.baseAnimationLayers[controllerIndex],
                        controllerAssetPaths[controllerIndex]);
                }
            }
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
            assetCompiler.LogAssetPaths();
        }
    }

    class AssetCompiler {
        public List<string> materializedAssetPaths;

        public AssetCompiler() {
            materializedAssetPaths = new List<string>();
        }

        public static string PrepareToMaterializeAsset(string name, string extension = "asset") {
            // Move old asset to OS trash.
            string assetPath = "Assets/" + name + "." + extension;
            AssetDatabase.MoveAssetToTrash(assetPath);
            return assetPath;
        }

        public string MaterializeAsset(
                UnityEngine.Object asset,
                string name,
                string type,
                string extension = "asset") {
            string assetPath = PrepareToMaterializeAsset(name, extension);
            AssetDatabase.CreateAsset(asset, assetPath);
            materializedAssetPaths.Add(assetPath);
            return assetPath;
        }

        public void LogAssetPaths() {
            Debug.Log("Created assets: " + String.Join(", ", materializedAssetPaths.ToArray()));
        }
    }
} // end namespace VRCAnimScript
