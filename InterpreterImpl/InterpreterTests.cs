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

        [Fact]
        public void RPNTest()
        {
            Assert.Equal("5 3 + 12 * 3 /", RPN("(5 + 3) * 12 / 3"));
        }

        [Fact]
        public void LISPTest()
        {
            Assert.Equal("(+ 2 3)", LISP("2 + 3"));
            Assert.Equal("(+ 2 (* 3 5))", LISP("(2 + 3 * 5)"));
        }

        private string RPN(string text)
        {
            return new Interpreter(new Parser(new Lexer(text))).RPN();
        }

        private string LISP(string text)
        {
            return new Interpreter(new Parser(new Lexer(text))).LISP();
        }

        private int Expr(string text)
        {
            return new Interpreter(new Parser(new Lexer(text))).Expr();
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

    internal class Parser
    {
        private readonly Lexer lexer;
        private Token currentToken;

        public Parser(Lexer lexer)
        {
            this.lexer = lexer;
            this.currentToken = lexer.GetNextToken();
        }

        internal AST Factor()
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
            return new Num(result);
        }

        internal AST Term()
        {
            var result = Factor();
            while (currentToken.Type == Operation.Mul || currentToken.Type == Operation.Div)
            {
                if (currentToken.Type == Operation.Mul)
                {
                    Eat(Operation.Mul);
                    result = new BinOp(result, Operation.Mul, Factor());
                }
                else if (currentToken.Type == Operation.Div)
                {
                    Eat(Operation.Div);
                    result = new BinOp(result, Operation.Div, Factor());
                }
            }
            return result;
        }

        internal AST Expr()
        {
            var result = Term();

            while (currentToken.Type == Operation.Plus || currentToken.Type == Operation.Minus)
            {
                if (currentToken.Type == Operation.Plus)
                {
                    Eat(Operation.Plus);
                    result = new BinOp(result, Operation.Plus, Term());
                }
                else if (currentToken.Type == Operation.Minus)
                {
                    Eat(Operation.Minus);
                    result = new BinOp(result, Operation.Minus, Term());
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

    internal abstract class AST
    {
        internal abstract string RPN();
        internal abstract string LISP();
    }

    internal class BinOp : AST
    {
        public BinOp(AST left, Operation op, AST right)
        {
            this.Left = left;
            this.Operation = op;
            this.Right = right;
        }

        public Operation Operation { get; }
        internal AST Left { get; }
        internal AST Right { get; }

        public override string ToString()
        {
            return $"{this.Left} {this.Operation} {this.Right}";
        }

        internal override string RPN()
        {
            return $"{this.Left.RPN()} {this.Right.RPN()} {ConvertToString(this.Operation)}";
        }

        internal override string LISP()
        {
            return $"({ConvertToString(this.Operation)} {this.Left.LISP()} {this.Right.LISP()})";
        }

        string ConvertToString(Operation operation)
        {
            switch (operation)
            {
                case Operation.Plus: return "+";
                case Operation.Minus: return "-";
                case Operation.Div: return "/";
                case Operation.Mul: return "*";
                default: throw new InvalidOperationException();
            }
        }
    }

    internal class Num : AST
    {
        public int Value { get; }

        public Num(int value)
        {
            this.Value = value;
        }

        public override string ToString()
        {
            return $"{this.Value}";
        }

        internal override string RPN()
        {
            return this.ToString();
        }

        internal override string LISP()
        {
            return this.ToString();
        }
    }

    internal class NodeVisitor
    {
    }

    internal class Interpreter : NodeVisitor
    {
        private readonly Parser parser;

        public Interpreter(Parser parser)
        {
            this.parser = parser;
        }

        protected int Visit(AST node)
        {
            if (node is Num num)
            {
                return VisitNum(num);
            }
            if (node is BinOp binOp)
            {
                return VisitBinOp(binOp);
            }
            throw new NotImplementedException();
        }

        internal int VisitNum(Num node)
        {
            return node.Value;
        }

        internal int VisitBinOp(BinOp node)
        {
            switch (node.Operation)
            {
                case Operation.Plus:
                    return Visit(node.Left) + Visit(node.Right);
                case Operation.Minus:
                    return Visit(node.Left) - Visit(node.Right);
                case Operation.Mul:
                    return Visit(node.Left) * Visit(node.Right);
                case Operation.Div:
                    return Visit(node.Left) / Visit(node.Right);
                default: throw new NotImplementedException();
            }
        }

        internal int Expr()
        {
            return Visit(parser.Expr());
        }

        internal string RPN()
        {
            return parser.Expr().RPN();
        }

        internal string LISP()
        {
            return parser.Expr().LISP();
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
