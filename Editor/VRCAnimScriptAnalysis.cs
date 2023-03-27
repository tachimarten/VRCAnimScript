// Editor/VRCAnimScriptAnalysis.cs

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

    class CompiledParams {
        public readonly Dictionary<string, VRCExpressionParameters.ValueType> paramTypes;
        public readonly string assetPath;

        public CompiledParams(
                Dictionary<string, VRCExpressionParameters.ValueType> paramTypes,
                string assetPath) {
            this.paramTypes = paramTypes;
            this.assetPath = assetPath;
        }
    }

    class PropertyAbbreviation {
        public string component;
        public string property;

        public PropertyAbbreviation(string component, string property) {
            this.component = component;
            this.property = property;
        }
    }

    namespace DNF {

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
                        var boolParamLiteral = (DNF.BoolParamLiteral)
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

    } // end namespace DNF

    namespace TrackingAnalysis {

        public static class StateUtils {
            public static bool StateHasAnimations(Ast.State state) {
                if (state is Ast.RegularState) {
                    var regularState = (Ast.RegularState)state;
                    return regularState.animation != null;
                }
                if (state is Ast.BlendState) {
                    var blendState = (Ast.BlendState)state;
                    foreach (Ast.BlendStateKeyframe keyframe in blendState.keyframes) {
                        if (keyframe.animation != null)
                            return true;
                    }
                    return false;
                }
                throw new Exception("Unknown state type");
            }
        }

        public class TrackingAnalysis {
            public readonly List<string>[] trackingParameters;

            public TrackingAnalysis() {
                trackingParameters = new List<string>[Node.Parts.Length];
                for (int i = 0; i < Node.Parts.Length; i++)
                    trackingParameters[i] = new List<string>();
            }

            public void ProcessLayerGraph(string controllerType, LayerGraph layerGraph) {
                uint untrackedParts = layerGraph.UntrackedParts;
                for (int partIndex = 0; partIndex < trackingParameters.Length; partIndex++) {
                    if ((untrackedParts & (1 << partIndex)) != 0) {
                        trackingParameters[partIndex].Add(GetTrackingParamName(
                            controllerType,
                            layerGraph.name,
                            partIndex));
                    }
                }
            }

            public static string GetTrackingParamName(
                    string controllerType,
                    string layerName,
                    int partIndex) {
                return "VRCAnimScriptTracking" + controllerType + Node.Parts[partIndex] +
                    layerName;
            }
        }

        public class LayerGraph {
            public string name;
            public readonly List<Node> nodes;
            public readonly Dictionary<string, int> nodeNames;
            public readonly List<Edge> edges;

            public LayerGraph() {
                name = null;
                nodes = new List<Node>();
                nodeNames = new Dictionary<string, int>();
                edges = new List<Edge>();
            }

            public void PopulateGraph(Ast.Layer layer, bool isAction) {
                name = layer.name;

                // Create nodes.
                for (int stateIndex = 0; stateIndex < layer.states.Length; stateIndex++) {
                    Ast.State state = layer.states[stateIndex];

                    var node = new Node();
                    node.id = stateIndex;

                    if (isAction && StateUtils.StateHasAnimations(state))
                        node.tracking = 0;
                    else
                        node.tracking = (uint)((1 << Node.Parts.Length) - 1);

                    foreach (string part in state.untracked) {
                        bool found = false;
                        for (int partIndex = 0; partIndex < Node.Parts.Length; partIndex++) {
                            if (part == Node.Parts[partIndex]) {
                                node.tracking &= ~(uint)(1 << partIndex);
                                found = true;
                                break;
                            }
                        }

                        if (!found)
                            throw new Exception("No such untracked part \"" + part + "\"");
                    }

                    nodes.Add(node);
                    nodeNames.Add(state.name, stateIndex);
                }

                // Create edges.
                for (int stateIndex = 0; stateIndex < layer.states.Length; stateIndex++) {
                    Ast.State state = layer.states[stateIndex];
                    foreach (Ast.StateTransition transition in state.transitions) {
                        if (!nodeNames.ContainsKey(transition.destination)) {
                            throw new Exception("No such transition destination state \"" +
                                transition.destination + "\"");
                        }

                        var edge = new Edge();
                        edge.from = stateIndex;
                        edge.to = nodeNames[transition.destination];
                        edges.Add(edge);
                    }
                }
            }

            public void ComputeTrackingMasks() {
                foreach (Edge edge in edges) {
                    Node from = nodes[edge.from], to = nodes[edge.to];
                    to.trackingMask |= from.tracking ^ to.tracking;
                }
            }

            public uint UntrackedParts {
                get {
                    uint untrackedParts = 0;
                    foreach (Node node in nodes)
                        untrackedParts |= (uint)(~node.tracking & ((1 << Node.Parts.Length) - 1));
                    return untrackedParts;
                }
            }
        }

        public class Node {
            public int id;
            public uint tracking;
            public uint trackingMask;

            static string[] gParts;

            public static string[] Parts {
                get {
                    if (gParts == null) {
                        gParts = new string[] {
                            "Head",
                            "LeftHand",
                            "RightHand",
                            "Hip",
                            "LeftFoot",
                            "RightFoot",
                            "LeftFingers",
                            "RightFingers",
                            "Eyes",
                            "Mouth",
                        };
                    }
                    return gParts;
                }
            }
        }

        public class Edge {
            public int from;
            public int to;
        }

    } // end namespace TrackingAnalysis

} // end namespace VRCAnimScript