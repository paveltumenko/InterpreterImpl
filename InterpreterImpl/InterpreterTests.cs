using System;
using System.Collections.Generic;
using System.Linq;
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
        public void UnaryTest()
        {
            Assert.Equal(-3, Expr("- 3"));
            Assert.Equal(3, Expr("+ 3"));
            Assert.Equal(5 - - - + - 3, Expr("5 - - - + - 3"));
            Assert.Equal(5 - - - + - (3 + 4) - +2, Expr("5 - - - + - (3 + 4) - +2"));
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

        [Fact]
        public void GlobalScopeTest()
        {
            Assert.Equal("[ number : 2, a : 2, b : 25, c : 27, x : 11 ]", GlobalScope(Program));
        }

        private static string GlobalScope(string text)
        {
            Interpreter interpreter = new Interpreter(new Parser(new Lexer(text)));
            interpreter.Interpret();
            return interpreter.PrintGlobalScope();
        }

        private static string Program => ""
            + "BEGIN\n"
            + "\n"
            + "    BEGIN\n"
            + "        number := 2;\n"
            + "        a := number;\n"
            + "        b := 10 * a + 10 * number / 4;\n"
            + "        c := a - - b\n"
            + "    END;\n"
            + "\n"
            + "    x := 11;\n"
            + "END.\n";

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

        Dictionary<string, Token> Keywords = new Dictionary<string, Token>
        {
            { "BEGIN", new Token(Operation.Begin, "BEGIN") },
            { "END", new Token(Operation.End, "END") },
        };

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

            if (Char.IsLetter(currentChar))
            {
                return ID();
            }

            if (currentChar == ':' && this.Peek() == '=')
            {
                Advance();
                Advance();
                return new Token(Operation.Assign, '=');
            }

            if (currentChar == '.')
            {
                Advance();
                return new Token(Operation.Dot, '.');
            }

            if (currentChar == ';')
            {
                Advance();
                return new Token(Operation.Semi, ';');
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

        private Token ID()
        {
            string result = string.Empty;
            while (Char.IsLetter(currentChar))
            {
                result += currentChar.ToString();
                Advance();
            }

            if (Keywords.ContainsKey(result))
            {
                return Keywords[result];
            }
            return new Token(Operation.ID, result);
        }

        private char Peek()
        {
            var peek_pos = pos + 1;
            return (peek_pos > this.text.Length - 1) ? EOF : this.text[peek_pos];
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

        internal AST Program()
        {
            AST node = CompoundStatement();
            Eat(Operation.Dot);
            return node;
        }

        private AST CompoundStatement()
        {
            Eat(Operation.Begin);
            List<AST> nodes = StatementList();
            Eat(Operation.End);
            var root = new Compound();
            foreach (var node in nodes)
            {
                root.AddNode(node);
            }
            return root;
        }

        private List<AST> StatementList()
        {
            var result = new List<AST> { Statement() };
            while (currentToken.Type == Operation.Semi)
            {
                Eat(Operation.Semi);
                result.Add(Statement());
            }
            if (currentToken.Type == Operation.ID)
            {
                throw new InvalidOperationException();
            }
            return result;
        }

        private AST Statement()
        {
            if (currentToken.Type == Operation.Begin)
            {
                return CompoundStatement();
            }
            if (currentToken.Type == Operation.ID)
            {
                return AssignmentStatement();
            }
            return Empty();
        }

        private AST AssignmentStatement()
        {
            Var left = Variable();
            var token = currentToken;
            Eat(Operation.Assign);
            AST right = Expr();

            return new Assign(left, token, right);
        }

        private Var Variable()
        {
            var node = new Var(currentToken);
            Eat(Operation.ID);
            return node;
        }

        private static NoOp Empty()
        {
            return new NoOp();
        }

        internal AST Factor()
        {
            if (currentToken.Type == Operation.Plus)
            {
                Eat(Operation.Plus);
                return new UnaryOp(Operation.Plus, Factor());
            }

            if (currentToken.Type == Operation.Minus)
            {
                Eat(Operation.Minus);
                return new UnaryOp(Operation.Minus, Factor());
            }

            if (currentToken.Type == Operation.Lparen)
            {
                Eat(Operation.Lparen);
                var res = Expr();
                Eat(Operation.Rparen);
                return res;
            }

            if (currentToken.Type == Operation.Integer)
            {
                var result = currentToken.Value;
                Eat(Operation.Integer);
                return new Num(result);
            }

            return Variable();
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

        internal AST Parse()
        {
            var node = Program();

            if (currentToken.Type != Operation.EOF)
            {
                throw new InvalidOperationException();
            }

            return node;
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

    internal class Compound : AST
    {
        internal List<AST> Children { get; } = new List<AST>();

        internal void AddNode(AST node)
        {
            Children.Add(node);
        }

        internal override string LISP()
        {
            throw new NotImplementedException();
        }

        internal override string RPN()
        {
            throw new NotImplementedException();
        }
    }

    internal class Assign : AST
    {
        internal Var Left { get; }
        internal Operation Operation { get; }
        internal AST Right { get; }

        public Assign(Var left, Token op, AST right)
        {
            Left = left;
            Operation = op.Type;
            Right = right;
        }

        internal override string LISP()
        {
            throw new NotImplementedException();
        }

        internal override string RPN()
        {
            throw new NotImplementedException();
        }
    }

    internal class Var : AST
    {
        public Var(Token token)
        {
            Token = token;
            Value = token.Value;
        }

        public Token Token { get; }
        public dynamic Value { get; }

        internal override string LISP()
        {
            throw new NotImplementedException();
        }

        internal override string RPN()
        {
            throw new NotImplementedException();
        }
    }

    internal class NoOp : AST
    {
        internal override string LISP()
        {
            return string.Empty;
        }

        internal override string RPN()
        {
            return string.Empty;
        }
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

    internal class UnaryOp : AST
    {
        public UnaryOp(Operation operation, AST expr)
        {
            Operation = operation;
            Expr = expr;
        }

        public Operation Operation { get; set; }
        public AST Expr { get; set; }

        public override string ToString()
        {
            return $"{this.Operation}{this.Expr}";
        }

        internal override string LISP()
        {
            return this.ToString();
        }

        internal override string RPN()
        {
            return this.ToString();
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
        Dictionary<string, dynamic> GlobalScope = new Dictionary<string, dynamic>();

        private readonly Parser parser;

        public Interpreter(Parser parser)
        {
            this.parser = parser;
        }

        protected int Visit(AST node)
        {
            if (node is Compound compound)
            {
                return VisitCompound(compound);
            }
            if (node is Assign assign)
            {
                return VisitAssign(assign);
            }
            if (node is NoOp noOp)
            {
                return VisitNoOp(noOp);
            }
            if (node is Var var)
            {
                return VisitVar(var);
            }
            if (node is Num num)
            {
                return VisitNum(num);
            }
            if (node is BinOp binOp)
            {
                return VisitBinOp(binOp);
            }
            if (node is UnaryOp unaryOp)
            {
                return VisitUnaryOp(unaryOp);
            }
            throw new NotImplementedException();
        }

        private dynamic VisitVar(Var var)
        {
            if (GlobalScope.ContainsKey(var.Value))
            {
                return GlobalScope[var.Value];
            }
            throw new InvalidOperationException();
        }

        private int VisitNoOp(NoOp noOp)
        {
            return 0;
        }

        private int VisitAssign(Assign assign)
        {
            GlobalScope[assign.Left.Value] = Visit(assign.Right);
            return 0;
        }

        private int VisitCompound(Compound compound)
        {
            foreach (var item in compound.Children)
            {
                Visit(item);
            }
            return 0;
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

        internal int VisitUnaryOp(UnaryOp node)
        {
            switch (node.Operation)
            {
                case Operation.Plus:
                    return +Visit(node.Expr);
                case Operation.Minus:
                    return -Visit(node.Expr);
                default: throw new NotImplementedException();
            }
        }

        internal int Interpret()
        {
            var tree = parser.Parse();
            return Visit(tree);
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

        internal string PrintGlobalScope()
        {
            return $"[ {string.Join(", ", GlobalScope.Select(item => $"{item.Key} : {item.Value}"))} ]";
        }
    }

    internal enum Operation
    {
        None,
        EOF,
        Begin,
        End,
        Dot,
        ID,
        Assign,
        Semi,
        Integer,
        Plus,
        Minus,
        Mul,
        Div,
        Lparen,
        Rparen
    }

    internal struct Token
    {
        internal Operation Type { get; }
        internal dynamic Value { get; }

        public Token(Operation type, dynamic value)
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
