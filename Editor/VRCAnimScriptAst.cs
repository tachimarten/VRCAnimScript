// Editor/VRCAnimScriptAst.cs

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace VRCAnimScript.Ast {

    public class Root {
        public string avatar;
        public string rootObject;
        public Param[] allParams;
        public MenuItem[] mainMenu;
        public ControllerSection[] controllerSections;
    }

    public abstract class Param {
        public bool saved;
        public string name;
    }

    public class BoolParam : Param {
        public bool initialValue;
    }

    public class FloatParam : Param {
        public double initialValue;
    }

    public abstract class MenuItem {
        public string name;
        public string icon;
    }

    public class ParamMenuItem : MenuItem {
        public string param;
    }

    public class SubmenuItem : MenuItem {
        public MenuItem[] items;
    }

    public class ControllerSection {
        public string extends;
        public Layer[] layers;
    }

    public class Layer {
        public string name;
        public State[] states;
        public bool additive;
    }

    public abstract class State {
        public string name;
        public bool initial;
        public StateTransition[] transitions;
        public string[] untracked;
    }

    public class RegularState : State {
        public Animation animation;
    }

    public class BlendState : State {
        public string param;
        public BlendStateKeyframe[] keyframes;
    }

    public class BlendStateKeyframe {
        public double time;
        public Animation animation;
    }

    public abstract class Animation { }

    public class ExternalAnimation : Animation {
        public string name;
        public bool noBlendShapes;
    }

    public class InlineAnimation : Animation {
        public Frame[] frames;
        public bool loop;
    }

    public class Frame {
        public double time;
        public FrameAction[] actions;
    }

    public class FrameAction {
        public string[] objectPath;
        public PropertyComponent[] property;
        public Expression expression;
        public double? prevTangent;
        public double? nextTangent;
    }

    public abstract class PropertyComponent { }

    public class NamePropertyComponent : PropertyComponent {
        public string name;
    }

    public class NumberPropertyComponent : PropertyComponent {
        public int value;
    }

    public abstract class Expression { }

    public class BoolExpression : Expression {
        public bool value;
    }

    public class IntExpression : Expression {
        public int value;
    }

    public class FloatExpression : Expression {
        public double value;
    }

    public class VectorLiteralExpression : Expression {
        public double[] elements;
    }

    public class AssetExpression : Expression {
        public string type;
        public string name;
    }

    public class StateTransition {
        public string destination;
        public TransitionCondition condition;
    }

    public abstract class TransitionCondition { }

    public class ParamTransitionCondition : TransitionCondition {
        public BooleanExpression expression;
    }

    public class TimeTransitionCondition : TransitionCondition {
        public double time;
    }

    public abstract class BooleanExpression { }

    public abstract class SingleTestBooleanExpression : BooleanExpression {
        public string paramName;
    }

    public class ParamBooleanExpression : SingleTestBooleanExpression { }

    public class RelationalBooleanExpression : SingleTestBooleanExpression {
        public RelationalOperator op;
        public double value;
    }

    public class AndBooleanExpression : BooleanExpression {
        public BooleanExpression lhs;
        public BooleanExpression rhs;
    }

    public class OrBooleanExpression : BooleanExpression {
        public BooleanExpression lhs;
        public BooleanExpression rhs;
    }

    public class NotBooleanExpression : BooleanExpression {
        public BooleanExpression expression;
    }

    public enum RelationalOperator {
        Equal,
        NotEqual,
        Less,
        Greater,
        LessEqual,
        GreaterEqual,
    }

} // end namespace VRCAnimScript.Ast
