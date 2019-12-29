using System;
using Xunit;

namespace InterpreterImpl
{
    public class InterpreterTests
    {
        [Fact]
        public void MinusTest()
        {
            Assert.Equal(1, Expr("2-1"));
            Assert.Equal(11, Expr("12 - 1"));
            Assert.Equal(11, Expr("14 - 2 - 1"));
        }

        [Fact]
        public void PlusTest()
        {
            Assert.Equal(1, Expr("0+1"));
            Assert.Equal(11, Expr("10 + 1"));
            Assert.Equal(13, Expr("11 + 1 + 1"));
        }

        [Fact]
        public void MixedTest()
        {
            Assert.Equal(1, Expr("0+2-1"));
            Assert.Equal(11, Expr("20 - 10 + 1"));
            Assert.Equal(0 + 1 * 2 - 3 / 1 + 4, Expr("0 + 1 * 2 - 3 / 1 + 4"));
        }

        [Fact]
        public void MulTest()
        {
            Assert.Equal(0, Expr("0*1"));
            Assert.Equal(2, Expr("2 * 1"));
            Assert.Equal(6, Expr("1 * 3 * 2"));
        }

        [Fact]
        public void DivTest()
        {
            Assert.Equal(0, Expr("0/1"));
            Assert.Equal(2, Expr("2 / 1"));
            Assert.Equal(3, Expr("12 / 2 / 2"));
        }

        [Fact]
        public void TestName()
        {
            Assert.Equal(7 + 3 * (10 / (12 / (3 + 1) - 1)), Expr("7 + 3 * (10 / (12 / (3 + 1) - 1))"));
        }

        private int Expr(string text)
        {
            return new Interpreter(new Lexer(text)).Expr();
        }
    }

    internal class Lexer
    {
        private const char EOF = '\0';
        private char currentChar;
        private int pos = 0;
        private readonly string text;

        public Lexer(string text)
        {
            this.text = text;
            this.currentChar = text[pos];
        }

        internal Token GetNextToken()
        {
            while (Char.IsWhiteSpace(currentChar))
            {
                Advance();
            }

            if (Char.IsDigit(currentChar))
            {
                return new Token(Operation.Integer, Int());
            }

            if (currentChar == '+')
            {
                Advance();
                return new Token(Operation.Plus, '+');
            }

            if (currentChar == '-')
            {
                Advance();
                return new Token(Operation.Minus, '-');
            }

            if (currentChar == '*')
            {
                Advance();
                return new Token(Operation.Mul, '*');
            }

            if (currentChar == '/')
            {
                Advance();
                return new Token(Operation.Div, '/');
            }

            if (currentChar == '(')
            {
                Advance();
                return new Token(Operation.Lparen, '(');
            }

            if (currentChar == ')')
            {
                Advance();
                return new Token(Operation.Rparen, ')');
            }

            if (currentChar == EOF)
            {
                return new Token(Operation.EOF, EOF);
            }

            throw new InvalidOperationException();
        }

        private int Int()
        {
            string s = string.Empty;
            while (Char.IsDigit(currentChar))
            {
                s += currentChar.ToString();
                Advance();
            }
            return int.Parse(s);
        }

        private void Advance()
        {
            pos++;
            if (pos > this.text.Length - 1)
                currentChar = EOF;
            else
                currentChar = text[pos];
        }
    }

    internal class Interpreter
    {
        private readonly Lexer lexer;
        private Token currentToken;

        public Interpreter(Lexer lexer)
        {
            this.lexer = lexer;
            this.currentToken = lexer.GetNextToken();
        }

        internal int Factor()
        {
            if (currentToken.Type == Operation.Lparen)
            {
                Eat(Operation.Lparen);
                var res = Expr();
                Eat(Operation.Rparen);
                return res;
            }

            var result = currentToken.Value;
            Eat(Operation.Integer);
            return result;
        }

        internal int Term()
        {
            int result = Factor();
            while (currentToken.Type == Operation.Mul || currentToken.Type == Operation.Div)
            {
                if (currentToken.Type == Operation.Mul)
                {
                    Eat(Operation.Mul);
                    result *= Factor();
                }
                else if (currentToken.Type == Operation.Div)
                {
                    Eat(Operation.Div);
                    result /= Factor();
                }
            }
            return result;
        }

        internal int Expr()
        {
            var result = Term();

            while (currentToken.Type == Operation.Plus || currentToken.Type == Operation.Minus)
            {
                if (currentToken.Type == Operation.Plus)
                {
                    Eat(Operation.Plus);
                    result += Term();
                }
                else if (currentToken.Type == Operation.Minus)
                {
                    Eat(Operation.Minus);
                    result -= Term();
                }
                else
                {
                    throw new InvalidOperationException();
                }
            }

            return result;
        }

        private void Eat(Operation type)
        {
            if (currentToken.Type == type)
            {
                currentToken = lexer.GetNextToken();
            }
            else
            {
                throw new InvalidOperationException();
            }
        }
    }

    internal enum Operation
    {
        None,
        EOF,
        Integer,
        Plus,
        Minus,
        Mul,
        Div,
        Lparen,
        Rparen
    }

    internal class Token
    {
        internal Operation Type { get; }
        internal int Value { get; }

        public Token(Operation type, int value)
        {
            Type = type;
            Value = value;
        }

        public override string ToString()
        {
            return $"Type: {Type}; Value: {Value}";
        }
    }
}
