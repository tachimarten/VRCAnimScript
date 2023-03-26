// Editor/VRCAnimScriptParser.cs

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using VRCAnimScript.Ast;

namespace VRCAnimScript {

    class Parser : IDisposable {
        Tokenizer tokenizer;
        Token nextToken;
        Token lookaheadToken;

        public Parser(Stream stream) {
            tokenizer = new Tokenizer(stream);
            nextToken = tokenizer.NextToken();
            lookaheadToken = tokenizer.NextToken();
        }

        void IDisposable.Dispose() {
            tokenizer.Dispose();
        }

        private Token GetToken() {
            Token token = nextToken;
            nextToken = lookaheadToken;
            lookaheadToken = tokenizer.NextToken();
            return token;
        }

        public Root ParseRoot() {
            string avatarName = null;
            string rootObject = null;
            var menuItems = new List<MenuItem>();
            var allParams = new List<Param>();
            ControllerSection actionController = null;
            ControllerSection fxController = null;

            while (true) {
                if (ConsumeKeyword("avatar")) {
                    avatarName = ExpectMaybeQuotedString();
                    continue;
                }
                if (ConsumeKeyword("rootobject")) {
                    rootObject = ExpectMaybeQuotedString();
                    continue;
                }

                Param param;
                if ((param = ParseParam()) != null) {
                    allParams.Add(param);
                    continue;
                }
                MenuItem menuItem;
                if ((menuItem = ParseMenuItem()) != null) {
                    menuItems.Add(menuItem);
                    continue;
                }

                if (ConsumeKeyword("controller")) {
                    if (ConsumeKeyword("fx"))
                        fxController = ParseControllerSection();
                    else if (ConsumeKeyword("action"))
                        actionController = ParseControllerSection();
                    else {
                        throw new ParseException(
                            "Expected \"fx\" or \"action\" after \"controller\"", tokenizer);
                    }
                    continue;
                }
                break;
            }

            Menu mainMenu = new Menu();
            mainMenu.items = menuItems.ToArray();

            Root root = new Root();
            root.avatar = avatarName;
            root.rootObject = rootObject;
            root.mainMenu = mainMenu;
            root.allParams = allParams.ToArray();
            root.fxController = fxController;
            root.actionController = actionController;
            return root;
        }

        public void EnsureFinished() {
            if (nextToken != null)
                throw new ParseException("Expected end of input but found " + nextToken, tokenizer);
        }

        Param ParseParam() {
            bool saved = ConsumeKeyword("saved");
            if (!ConsumeKeyword("param"))
                return null;

            string name = ExpectMaybeQuotedString();
            ExpectPunctuation(":");
            if (ConsumeKeyword("bool")) {
                ExpectPunctuation("=");

                bool initialValue;
                if (ConsumeKeyword("false")) {
                    initialValue = false;
                } else if (ConsumeKeyword("true")) {
                    initialValue = true;
                } else {
                    throw new ParseException(
                        "Expected \"true\" or \"false\" for parameter initializer",
                        tokenizer);
                }

                BoolParam param = new BoolParam();
                param.name = name;
                param.saved = saved;
                param.initialValue = initialValue;
                return param;
            } else if (ConsumeKeyword("float")) {
                ExpectPunctuation("=");

                double initialValue = ExpectIntOrFloatLiteral();

                FloatParam param = new FloatParam();
                param.name = name;
                param.saved = saved;
                param.initialValue = initialValue;
                return param;
            }

            throw new ParseException("Expected \"bool\" or \"float\" for parameter type",
                tokenizer);
        }

        MenuItem ParseMenuItem() {
            if (!ConsumeKeyword("menuitem"))
                return null;

            MenuItem item = new MenuItem();
            item.name = ExpectMaybeQuotedString();

            while (true) {
                if (ConsumeKeyword("icon")) {
                    item.icon = ExpectMaybeQuotedString();
                    continue;
                }
                if (ConsumeKeyword("controls")) {
                    item.param = ExpectMaybeQuotedString();
                    continue;
                }
                break;
            }

            return item;
        }

        ControllerSection ParseControllerSection() {
            ControllerSection section = new ControllerSection();

            var layers = new List<Layer>();
            Layer layer;
            while ((layer = ParseLayer()) != null)
                layers.Add(layer);
            section.layers = layers.ToArray();

            return section;
        }

        Layer ParseLayer() {
            bool additive = false;
            if (ConsumeKeyword("additive")) {
                additive = true;
                ExpectKeyword("layer");
            } else if (!ConsumeKeyword("layer")) {
                return null;
            }

            Layer layer = new Layer();
            layer.name = ExpectMaybeQuotedString();
            layer.additive = additive;

            var states = new List<State>();
            State state;
            while ((state = ParseState()) != null)
                states.Add(state);
            layer.states = states.ToArray();

            return layer;
        }

        State ParseState() {
            bool initial = ConsumeKeyword("initial");

            State state;
            if ((state = ParseRegularState(initial)) != null)
                return state;
            if ((state = ParseBlendState(initial)) != null)
                return state;

            if (initial) {
                throw new ParseException("Expected \"state\" or \"blend\" but found " + nextToken,
                    tokenizer);
            }
            return null;
        }

        RegularState ParseRegularState(bool initial) {
            if (!ConsumeKeyword("state"))
                return null;

            RegularState state = new RegularState();
            state.initial = initial;
            state.name = ExpectMaybeQuotedString();

            var stateTransitions = new List<StateTransition>();
            var untrackeds = new List<string>();
            while (true) {
                Animation animation;
                StateTransition stateTransition;
                string untracked;
                if ((animation = ParseAnimation()) != null) {
                    if (state.animation != null)
                        throw new Exception("State has multiple animations");
                    state.animation = animation;
                    continue;
                }
                if ((stateTransition = ParseStateTransition()) != null) {
                    stateTransitions.Add(stateTransition);
                    continue;
                }
                if ((untracked = ParseUntracked()) != null) {
                    untrackeds.Add(untracked);
                    continue;
                }

                break;
            }

            state.transitions = stateTransitions.ToArray();
            state.untracked = untrackeds.ToArray();
            return state;
        }

        BlendState ParseBlendState(bool initial) {
            if (!ConsumeKeyword("blend"))
                return null;
            ExpectKeyword("state");

            string name = ConsumeMaybeQuotedString();

            ExpectPunctuation("(");
            string param = ConsumeMaybeQuotedString();
            ExpectPunctuation(")");

            var keyframes = new List<BlendStateKeyframe>();
            var transitions = new List<StateTransition>();
            var untrackeds = new List<string>();

            while (true) {
                BlendStateKeyframe keyframe;
                StateTransition transition;
                string untracked;
                if ((keyframe = ParseBlendStateKeyframe()) != null) {
                    keyframes.Add(keyframe);
                    continue;
                }
                if ((transition = ParseStateTransition()) != null) {
                    transitions.Add(transition);
                    continue;
                }
                if ((untracked = ParseUntracked()) != null) {
                    untrackeds.Add(untracked);
                    continue;
                }

                break;
            }

            BlendState blendState = new BlendState();
            blendState.initial = initial;
            blendState.name = name;
            blendState.param = param;
            blendState.keyframes = keyframes.ToArray();
            blendState.transitions = transitions.ToArray();
            blendState.untracked = untrackeds.ToArray();
            return blendState;
        }

        BlendStateKeyframe ParseBlendStateKeyframe() {
            if (!ConsumeKeyword("at"))
                return null;
            ExpectKeyword("value");

            BlendStateKeyframe keyframe = new BlendStateKeyframe();
            keyframe.time = ExpectIntOrFloatLiteral();;
            keyframe.animation = ParseAnimation();
            return keyframe;
        }

        string ParseUntracked() {
            return ConsumeKeyword("untracked") ? ExpectMaybeQuotedString() : null;
        }

        Animation ParseAnimation() {
            Animation animation;
            if ((animation = ParseExternalAnimation()) != null)
                return animation;
            return ParseInlineAnimation();
        }

        ExternalAnimation ParseExternalAnimation() {
            if (!ConsumeKeyword("play"))
                return null;

            ExternalAnimation animation = new ExternalAnimation();
            animation.name = ExpectMaybeQuotedString();
            return animation;
        }

        InlineAnimation ParseInlineAnimation() {
            bool loop = false;
            if (ConsumeKeyword("loop")) {
                loop = true;
                ExpectKeyword("do");
            } else if (!ConsumeKeyword("do")) {
                return null;
            }

            bool singleFrame = FrameActionIsNext();

            InlineAnimation animation = new InlineAnimation();
            animation.loop = loop;

            var frames = new List<Frame>();
            if (singleFrame) {
                Frame frame = new Frame();
                frame.time = 0;
                frame.actions = ParseFrameActions();
                frames.Add(frame);
            } else {
                Frame frame;
                while ((frame = ParseFrame()) != null)
                    frames.Add(frame);
            }
            animation.frames = frames.ToArray();

            return animation;
        }

        Frame ParseFrame() {
            if (!ConsumeKeyword("at"))
                return null;
            ExpectKeyword("time");

            Frame frame = new Frame();
            frame.time = ExpectIntOrFloatLiteral();
            frame.actions = ParseFrameActions();
            return frame;
        }

        FrameAction[] ParseFrameActions() {
            var actions = new List<FrameAction>();
            FrameAction action;
            while ((action = ParseAction()) != null)
                actions.Add(action);
            return actions.ToArray();
        }

        FrameAction ParseAction() {
            if (!FrameActionIsNext())
                return null;

            FrameAction action = new FrameAction();

            List<string> doubleColonDelimited = new List<string>();
            while (IsMaybeQuotedString() && DoubleColonIsNext()) {
                doubleColonDelimited.Add(ExpectMaybeQuotedString());
                ExpectPunctuation("::");
            }

            var propertyComponents = new List<PropertyComponent>();
            PropertyComponent propertyComponent = ParseNamePropertyComponent();
            if (propertyComponent == null)
                throw new ParseException("Expected property component", tokenizer);
            propertyComponents.Add(propertyComponent);

            while ((propertyComponent = ParsePropertyComponent()) != null)
                propertyComponents.Add(propertyComponent);

            action.objectPath = doubleColonDelimited.ToArray();
            action.property = propertyComponents.ToArray();

            ExpectPunctuation("=");

            action.expression = ParseExpression();

            if (ConsumeKeyword("curve")) {
                ExpectPunctuation("(");
                action.prevTangent = (double)ExpectIntOrFloatLiteral();
                if (ConsumePunctuation(","))
                    action.nextTangent = (double)ExpectIntOrFloatLiteral();
                ExpectPunctuation(")");
            }

            return action;
        }

        PropertyComponent ParsePropertyComponent() {
            PropertyComponent propertyComponent;
            if (ConsumePunctuation("."))
                return ParseNamePropertyComponent();
            if (ConsumePunctuation("["))
                return ParseNumberPropertyComponent();
            return null;
        }

        NamePropertyComponent ParseNamePropertyComponent() {
            var namePropertyComponent = new NamePropertyComponent();
            namePropertyComponent.name = ExpectMaybeQuotedString();
            return namePropertyComponent;
        }

        NumberPropertyComponent ParseNumberPropertyComponent() {
            var numberPropertyComponent = new NumberPropertyComponent();
            numberPropertyComponent.value = ExpectIntLiteral();
            ExpectPunctuation("]");
            return numberPropertyComponent;
        }

        Expression ParseExpression() {
            Expression expression;
            if ((expression = ParseBoolExpression()) != null)
                return expression;
            if ((expression = ParseIntExpression()) != null)
                return expression;
            if ((expression = ParseFloatExpression()) != null)
                return expression;
            if ((expression = ParseVectorLiteralExpression()) != null)
                return expression;
            if ((expression = ParseAssetExpression()) != null)
                return expression;
            return null;
        }

        BoolExpression ParseBoolExpression() {
            if (ConsumeKeyword("true")) {
                BoolExpression boolExpression = new BoolExpression();
                boolExpression.value = true;
                return boolExpression;
            }

            if (ConsumeKeyword("false")) {
                BoolExpression boolExpression = new BoolExpression();
                boolExpression.value = false;
                return boolExpression;
            }

            return null;
        }

        IntExpression ParseIntExpression() {
            int value;
            if (!ConsumeIntLiteral(out value))
                return null;

            IntExpression intExpression = new IntExpression();
            intExpression.value = value;
            return intExpression;
        }

        FloatExpression ParseFloatExpression() {
            double value;
            if (!ConsumeFloatLiteral(out value))
                return null;

            FloatExpression floatExpression = new FloatExpression();
            floatExpression.value = value;
            return floatExpression;
        }

        VectorLiteralExpression ParseVectorLiteralExpression() {
            double[] values;
            if (ConsumeKeyword("vec3"))
                values = new double[3];
            else if (ConsumeKeyword("vec4"))
                values = new double[4];
            else
                return null;

            ExpectPunctuation("(");
            for (int index = 0; index < values.Length; index++) {
                values[index] = ExpectIntOrFloatLiteral();
                ConsumePunctuation(",");
            }
            ExpectPunctuation(")");

            VectorLiteralExpression expression = new VectorLiteralExpression();
            expression.elements = values;
            return expression;
        }

        AssetExpression ParseAssetExpression() {
            if (!PunctuationIsNext("("))
                return null;
            if (!(lookaheadToken is KeywordToken) && !(lookaheadToken is StringLiteralToken))
                return null;

            var assetExpression = new AssetExpression();
            ExpectPunctuation("(");
            assetExpression.type = ExpectMaybeQuotedString();
            ExpectPunctuation(")");
            assetExpression.name = ExpectMaybeQuotedString();
            return assetExpression;
        }

        StateTransition ParseStateTransition() {
            if (!ConsumeKeyword("goto"))
                return null;

            StateTransition transition = new StateTransition();
            transition.destination = ExpectMaybeQuotedString();
            transition.condition = ParseTransitionCondition();
            return transition;
        }

        TransitionCondition ParseTransitionCondition() {
            TransitionCondition condition;
            if ((condition = ParseParamTransitionCondition()) != null)
                return condition;
            if ((condition = ParseTimeTransitionCondition()) != null)
                return condition;
            return null;
        }

        ParamTransitionCondition ParseParamTransitionCondition() {
            if (!ConsumeKeyword("if"))
                return null;

            ParamTransitionCondition condition = new ParamTransitionCondition();
            BooleanExpression expression = ParseBooleanDisjunction();
            if (expression == null)
                throw new ParseException("Expected Boolean expression", tokenizer);
            condition.expression = expression;
            return condition;
        }

        TimeTransitionCondition ParseTimeTransitionCondition() {
            if (!ConsumeKeyword("at"))
                return null;
            ExpectKeyword("time");

            TimeTransitionCondition condition = new TimeTransitionCondition();
            condition.time = ExpectIntOrFloatLiteral();
            return condition;
        }

        BooleanExpression ParseBooleanDisjunction() {
            BooleanExpression lhs = ParseBooleanConjunction();
            if (lhs == null)
                return null;

            if (ConsumePunctuation("||")) {
                BooleanExpression rhs = ParseBooleanDisjunction();
                if (rhs == null)
                    throw new ParseException("Expected Boolean expression", tokenizer);

                OrBooleanExpression expression = new OrBooleanExpression();
                expression.lhs = lhs;
                expression.rhs = rhs;
                return expression;
            }
            return lhs;
        }

        BooleanExpression ParseBooleanConjunction() {
            BooleanExpression lhs = ParseBooleanComparison();
            if (lhs == null)
                return null;

            if (ConsumePunctuation("&&")) {
                BooleanExpression rhs = ParseBooleanConjunction();
                if (rhs == null)
                    throw new ParseException("Expected Boolean expression", tokenizer);

                AndBooleanExpression expression = new AndBooleanExpression();
                expression.lhs = lhs;
                expression.rhs = rhs;
                return expression;
            }
            return lhs;
        }

        BooleanExpression ParseBooleanComparison() {
            BooleanExpression lhs = ParseHighPrecedenceBooleanExpression();
            if (lhs == null)
                return null;

            if (!(nextToken is PunctuationToken))
                return lhs;

            RelationalOperator op;
            switch (((PunctuationToken)nextToken).Value) {
                case "==":
                    op = RelationalOperator.Equal;
                    break;
                case "!=":
                    op = RelationalOperator.NotEqual;
                    break;
                case "<":
                    op = RelationalOperator.Less;
                    break;
                case ">":
                    op = RelationalOperator.Greater;
                    break;
                case "<=":
                    op = RelationalOperator.LessEqual;
                    break;
                case ">=":
                    op = RelationalOperator.GreaterEqual;
                    break;
                default:
                    return lhs;
            }
            GetToken();

            if (!(lhs is ParamBooleanExpression))
                throw new ParseException("Expected parameter name", tokenizer);

            RelationalBooleanExpression expression = new RelationalBooleanExpression();
            expression.paramName = ((ParamBooleanExpression)lhs).paramName;
            expression.op = op;
            expression.value = ExpectIntOrFloatLiteral();
            return expression;
        }

        BooleanExpression ParseHighPrecedenceBooleanExpression() {
            if (ConsumePunctuation("!")) {
                var notExpression = new NotBooleanExpression();
                BooleanExpression subexpression = ParseHighPrecedenceBooleanExpression();
                if (subexpression == null)
                    throw new ParseException("Expected Boolean expression", tokenizer);
                notExpression.expression = subexpression;
                return notExpression;
            }

            if (ConsumePunctuation("(")) {
                BooleanExpression expression = ParseBooleanDisjunction();
                ExpectPunctuation(")");
                return expression;
            }

            return ParseBooleanLiteral();
        }

        BooleanExpression ParseBooleanLiteral() {
            string paramName = ExpectMaybeQuotedString();
            var paramBooleanExpression = new ParamBooleanExpression();
            paramBooleanExpression.paramName = paramName;
            return paramBooleanExpression;
        }

        private bool ConsumeKeyword(string keyword) {
            if (nextToken is KeywordToken && ((KeywordToken)nextToken).Value == keyword) {
                GetToken();
                return true;
            }
            return false;
        }

        private bool IsMaybeQuotedString() {
            return (nextToken is StringLiteralToken) || (nextToken is KeywordToken);
        }

        private string ConsumeMaybeQuotedString() {
            if (nextToken is StringLiteralToken)
                return ((StringLiteralToken)GetToken()).Value;
            if (nextToken is KeywordToken)
                return ((KeywordToken)GetToken()).Value;
            return null;
        }

        private bool ConsumeIntLiteral(out int outValue) {
            if (nextToken is IntegerLiteralToken) {
                outValue = ((IntegerLiteralToken)GetToken()).Value;
                return true;
            }
            outValue = 0;
            return false;
        }

        private bool ConsumeFloatLiteral(out double outValue) {
            if (nextToken is FloatLiteralToken) {
                outValue = ((FloatLiteralToken)GetToken()).Value;
                return true;
            }
            outValue = 0.0;
            return false;
        }

        private bool ConsumeIntOrFloatLiteral(out double outValue) {
            if (nextToken is IntegerLiteralToken) {
                outValue = (double)((IntegerLiteralToken)GetToken()).Value;
                return true;
            }
            if (nextToken is FloatLiteralToken) {
                outValue = ((FloatLiteralToken)GetToken()).Value;
                return true;
            }
            outValue = 0.0;
            return false;
        }

        private bool PunctuationIsNext(string punct) {
            return nextToken is PunctuationToken && ((PunctuationToken)nextToken).Value == punct;
        }

        private bool ConsumePunctuation(string punct) {
            if (PunctuationIsNext(punct)) {
                GetToken();
                return true;
            }
            return false;
        }

        private void ExpectPunctuation(string punct) {
            if (!ConsumePunctuation(punct))
                throw new ParseException("Expected " + punct, tokenizer);
        }

        private void ExpectKeyword(string keyword) {
            if (!ConsumeKeyword(keyword))
                throw new ParseException("Expected " + keyword, tokenizer);
        }

        private string ExpectMaybeQuotedString() {
            string quotedString = ConsumeMaybeQuotedString();
            if (quotedString == null) {
                throw new ParseException(
                    "Expected quoted or unquoted string but found " + nextToken, tokenizer);
            }
            return quotedString;
        }

        private int ExpectIntLiteral() {
            int value;
            if (!ConsumeIntLiteral(out value))
                throw new Exception("Expected integer literal");
            return value;
        }

        private double ExpectIntOrFloatLiteral() {
            double value;
            if (!ConsumeIntOrFloatLiteral(out value))
                throw new Exception("Expected integer or float literal");
            return value;
        }

        private bool FrameActionIsNext() {
            if (!(lookaheadToken is PunctuationToken))
                return false;
            var punctuation = (PunctuationToken)lookaheadToken;
            return punctuation.Value == "." || punctuation.Value == "::" ||
                punctuation.Value == "[";
        }

        private bool DoubleColonIsNext() {
            if (!(lookaheadToken is PunctuationToken))
                return false;
            return ((PunctuationToken)lookaheadToken).Value == "::";
        }
    }

    namespace Ast {

        public class Root {
            public string avatar;
            public string rootObject;
            public Param[] allParams;
            public Menu mainMenu;
            public ControllerSection actionController;
            public ControllerSection fxController;
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

        public class Menu {
            public MenuItem[] items;

            public Menu() {
                items = null;
            }
        }

        public class MenuItem {
            public string name;
            public string icon;
            public string param;
        }

        public class ControllerSection {
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

    }   // end namespace Ast

}   // end namespace VRCAnimScript
