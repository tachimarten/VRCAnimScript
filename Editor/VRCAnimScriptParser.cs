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
                    else
                        throw new Exception("Expected \"fx\" or \"action\" after \"controller\"");
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
                throw new Exception("Expected end of input but found " + nextToken);
        }

        Param ParseParam() {
            Param param = new Param();
            param.saved = ConsumeKeyword("saved");
            if (!ConsumeKeyword("param"))
                return null;

            param.name = ExpectMaybeQuotedString();
            ExpectPunctuation(":");
            ExpectKeyword("bool");  // TODO: support ints
            ExpectPunctuation("=");

            if (ConsumeKeyword("false"))
                param.initialValue = false;
            else if (ConsumeKeyword("true"))
                param.initialValue = true;
            else
                throw new Exception("Expected \"true\" or \"false\" for parameter initializer");

            return param;
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
                if (ConsumeKeyword("toggle")) {
                    item.toggle = ExpectMaybeQuotedString();
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
            if (!ConsumeKeyword("layer"))
                return null;

            Layer layer = new Layer();
            layer.name = ExpectMaybeQuotedString();

            var states = new List<State>();
            State state;
            while ((state = ParseState()) != null)
                states.Add(state);
            layer.states = states.ToArray();

            return layer;
        }

        State ParseState() {
            bool initial = ConsumeKeyword("initial");
            if (!ConsumeKeyword("state"))
                return null;

            State state = new State();
            state.initial = initial;
            state.name = ExpectMaybeQuotedString();

            var stateTransitions = new List<StateTransition>();
            while (true) {
                Animation animation;
                if ((animation = ParseAnimation()) != null) {
                    state.animation = animation;
                    continue;
                }

                StateTransition stateTransition;
                if ((stateTransition = ParseStateTransition()) != null) {
                    stateTransitions.Add(stateTransition);
                    continue;
                }

                break;
            }

            state.transitions = stateTransitions.ToArray();
            return state;
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
            if (!ConsumeKeyword("do"))
                return null;

            bool singleFrame = LookaheadIsDotOrDoubleColon();

            InlineAnimation animation = new InlineAnimation();
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
            if (!LookaheadIsDotOrDoubleColon())
                return null;

            FrameAction action = new FrameAction();

            List<string> doubleColonDelimited = new List<string>();
            doubleColonDelimited.Add(ExpectMaybeQuotedString());
            while (ConsumePunctuation("::"))
                doubleColonDelimited.Add(ExpectMaybeQuotedString());
            string component = doubleColonDelimited[doubleColonDelimited.Count - 1];
            doubleColonDelimited.RemoveAt(doubleColonDelimited.Count - 1);

            var propertyName = new List<string>();
            while (ConsumePunctuation("."))
                propertyName.Add(ExpectMaybeQuotedString());

            action.propertyName = propertyName.ToArray();
            action.propertyPath = doubleColonDelimited.ToArray();
            action.component = component;

            ExpectPunctuation("=");

            action.expression = ParseExpression();
            return action;
        }

        Expression ParseExpression() {
            Expression expression;
            if ((expression = ParseIntExpression()) != null)
                return expression;
            if ((expression = ParseVectorLiteralExpression()) != null)
                return expression;
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
                bool negative = ConsumePunctuation("-");
                values[index] = (negative ? -1 : 1) * ExpectIntOrFloatLiteral();
                ConsumePunctuation(",");
            }
            ExpectPunctuation(")");

            VectorLiteralExpression expression = new VectorLiteralExpression();
            expression.elements = values;
            return expression;
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
            condition.value = !ConsumePunctuation("!");
            condition.param = ExpectMaybeQuotedString();
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

        private bool ConsumeKeyword(string keyword) {
            if (nextToken is KeywordToken && ((KeywordToken)nextToken).Value == keyword) {
                GetToken();
                return true;
            }
            return false;
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

        private bool ConsumePunctuation(string punct) {
            if (nextToken is PunctuationToken && ((PunctuationToken)nextToken).Value == punct) {
                GetToken();
                return true;
            }
            return false;
        }

        private void ExpectPunctuation(string punct) {
            if (!ConsumePunctuation(punct))
                throw new Exception("Expected " + punct);
        }

        private void ExpectKeyword(string keyword) {
            if (!ConsumeKeyword(keyword))
                throw new Exception("Expected " + keyword);
        }

        private string ExpectMaybeQuotedString() {
            string quotedString = ConsumeMaybeQuotedString();
            if (quotedString == null)
                throw new Exception("Expected quoted or unquoted string");
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

        private bool LookaheadIsDotOrDoubleColon() {
            if (!(lookaheadToken is PunctuationToken))
                return false;
            var punctuation = (PunctuationToken)lookaheadToken;
            return punctuation.Value == "." || punctuation.Value == "::";
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

        public class Param {
            public bool saved;
            public string name;
            public bool initialValue;

            public Param() {
                saved = false;
                name = null;
                initialValue = false;
            }
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
            public string toggle;
        }

        public class ControllerSection {
            public Layer[] layers;
        }

        public class Layer {
            public string name;
            public State[] states;
        }

        public class State {
            public string name;
            public bool initial;
            public Animation animation;
            public StateTransition[] transitions;
        }

        public abstract class Animation { }

        public class ExternalAnimation : Animation {
            public string name;
        }

        public class InlineAnimation : Animation {
            public Frame[] frames;
        }

        public class Frame {
            public double time;
            public FrameAction[] actions;
        }

        public class FrameAction {
            public string[] propertyPath;
            public string component;
            public string[] propertyName;
            public Expression expression;
        }

        public abstract class Expression { }

        public class IntExpression : Expression {
            public int value;
        }

        public class VectorLiteralExpression : Expression {
            public double[] elements;
        }

        public class StateTransition {
            public string destination;
            public TransitionCondition condition;
        }

        public abstract class TransitionCondition { }

        public class ParamTransitionCondition : TransitionCondition {
            public string param;
            public bool value;
        }

        public class TimeTransitionCondition : TransitionCondition {
            public double time;
        }

    }   // end namespace Ast

    // A simple tokenizer that splits a stream into tokens
    class Tokenizer : IDisposable {
        private StreamReader reader;

        public Tokenizer(Stream stream) {
            this.reader = new StreamReader(stream);
        }

        public void Dispose() {
            this.reader.Dispose();
        }

        public Token NextToken() {
            string token = "";
            char c;

            while ((c = (char)reader.Peek()) != 0xffff) {
                // Check for comments
                if (c == '#') {
                    reader.ReadLine();
                    continue;
                }

                // Check for whitespace
                if (Char.IsWhiteSpace(c)) {
                    reader.Read();
                    continue;
                }

                // Check for integer or floating point literals
                if (Char.IsDigit(c)) {
                    bool isFloatingPoint = false;

                    while ((c = (char)reader.Peek()) != -1 && (Char.IsDigit(c) || (!isFloatingPoint && c == '.'))) {
                        if (c == '.')
                            isFloatingPoint = true;

                        token += (char)reader.Read();
                    }

                    if (isFloatingPoint)
                        return new FloatLiteralToken(Double.Parse(token));
                    return new IntegerLiteralToken(Int32.Parse(token));
                }

                // Check for quoted strings
                if (c == '"' || c == '\'') {
                    char quoteChar = c;
                    token += (char)reader.Read();

                    while ((c = (char)reader.Peek()) != -1 && c != quoteChar) {
                        if (c == '\\') {
                            token += (char)reader.Read();

                            if ((c = (char)reader.Peek()) != -1)
                                token += (char)reader.Read();
                        } else {
                            token += (char)reader.Read();
                        }
                    }

                    if (c == quoteChar)
                        token += (char)reader.Read();

                    return new StringLiteralToken(ParseQuotedString(token.Substring(1, token.Length - 2)));
                }

                // Check for keywords
                if (Char.IsLetter(c) || c == '_') {
                    while ((c = (char)reader.Peek()) != -1 && (Char.IsLetterOrDigit(c) || c == '_')) {
                        token += (char)reader.Read();
                    }

                    return new KeywordToken(token);
                }

                if (c == ':') {
                    token += c;
                    reader.Read();
                    if (reader.Peek() == ':') {
                        reader.Read();
                        token += ':';
                    }
                    return new PunctuationToken(token);
                }

                if ("=!*,-.()".Contains("" + c)) {
                    token += c;
                    reader.Read();
                    return new PunctuationToken(token);
                }

                // If we get here, we have an invalid token
                throw new Exception("Syntax error: invalid token: " + (int)c);
            }

            // End of input
            return null;
        }

        public string ParseQuotedString(string input) {
            StringBuilder output = new StringBuilder();
            bool escape = false;

            foreach (char c in input) {
                if (escape) {
                    switch (c) {
                        case 'n':
                            output.Append('\n');
                            break;
                        case 'r':
                            output.Append('\r');
                            break;
                        case 't':
                            output.Append('\t');
                            break;
                        case '\\':
                            output.Append('\\');
                            break;
                        case '\'':
                            output.Append('\'');
                            break;
                        case '\"':
                            output.Append('\"');
                            break;
                        default:
                            throw new Exception($"Invalid escape sequence: \\{c}");
                    }

                    escape = false;
                } else if (c == '\\') {
                    escape = true;
                } else {
                    output.Append(c);
                }
            }

            if (escape)
                throw new Exception("Invalid escape sequence: \\");

            return output.ToString();
        }


    }

    abstract class Token {
        public abstract override string ToString();
    }

    class IntegerLiteralToken : Token {
        public int Value { get; }

        public IntegerLiteralToken(int value) {
            Value = value;
        }

        public override string ToString() {
            return $"IntegerLiteralToken({Value})";
        }
    }

    class FloatLiteralToken : Token {
        public double Value { get; }

        public FloatLiteralToken(double value) {
            Value = value;
        }

        public override string ToString() {
            return $"FloatLiteralToken({Value})";
        }
    }

    class StringLiteralToken : Token {
        public string Value { get; }

        public StringLiteralToken(string value) {
            Value = value;
        }

        public override string ToString() {
            return $"StringLiteralToken(\"{Value}\")";
        }
    }

    class KeywordToken : Token {
        public string Value { get; }

        public KeywordToken(string value) {
            Value = value;
        }

        public override string ToString() {
            return $"KeywordToken({Value})";
        }
    }

    class PunctuationToken : Token {
        public string Value { get; }

        public PunctuationToken(string value) {
            Value = value;
        }

        public override string ToString() {
            return $"PunctuationToken('{Value}')";
        }
    }

}   // end namespace VRCAnimScript
