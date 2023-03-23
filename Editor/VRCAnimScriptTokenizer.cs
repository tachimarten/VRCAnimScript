// Editor/VRCAnimScriptTokenizer.cs

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace VRCAnimScript {

    // A simple tokenizer that splits a stream into tokens
    class Tokenizer : IDisposable {
        private StreamReader reader;

        public int lineNumber;

        public Tokenizer(Stream stream) {
            this.reader = new StreamReader(stream);
            lineNumber = 1;
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
                    lineNumber++;
                    continue;
                }

                // Check for whitespace
                if (c == '\t' || c == ' ' || c == '\r') {
                    reader.Read();
                    continue;
                }
                if (c == '\n') {
                    reader.Read();
                    lineNumber++;
                    continue;
                }

                // Check for integer or floating point literals
                if (c == '-' || Char.IsDigit(c)) {
                    bool isFloatingPoint = false;

                    if (c == '-')
                        token += (char)reader.Read();

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

                // Possibly doubled characters
                if (":&|".Contains("" + c)) {
                    token += c;
                    reader.Read();
                    if (reader.Peek() == c) {
                        reader.Read();
                        token += c;
                    }
                    return new PunctuationToken(token);
                }

                if ("=!*,-.()[]<>".Contains("" + c)) {
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

    class ParseException : Exception {
        private Tokenizer tokenizer;

        public ParseException(string message, Tokenizer tokenizer) : base(message) {
            this.tokenizer = tokenizer;
        }

        public override String Message {
            get {
                return base.Message + " at line " + tokenizer.lineNumber;
            }
        }
    }

}

