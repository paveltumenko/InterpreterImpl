using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;
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
            Assert.Equal(0 + 1 * 2 - 3 / 1 + 4, Expr("0 + 1 * 2 - 3 DIV 1 + 4"));
        }

        [Fact]
        public void MulTest()
        {
            Assert.Equal(0, Expr("0*1"));
            Assert.Equal(2, Expr("2 * 1"));
            Assert.Equal(6, Expr("1 * 3 * 2"));
        }

        [Fact]
        public void IntDivTest()
        {
            Assert.Equal(0, Expr("0DIV1"));
            Assert.Equal(2, Expr("2 DIV 1"));
            Assert.Equal(1, Expr("12 DIV 2 DIV 5"));
        }

        [Fact]
        public void FloatDivTest()
        {
            Assert.Equal(0f, Expr("0.0/1.0"));
            Assert.Equal(2f / 1f, Expr("2 / 1"));
            Assert.Equal(12f / 2f / 5f, Expr("12 / 2 / 5"), 5);
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
        public void GlobalScopeTest()
        {
            Assert.Equal($"[ number : 2, a : 2, b : 25, c : 27, x : 11, y : {20f / 7f + 3.14f} ]", GlobalScope(Program));
        }

        private static string GlobalScope(string text)
        {
            Interpreter interpreter = new Interpreter(new Parser(new Lexer(text)));
            interpreter.Interpret();
            return interpreter.PrintGlobalScope();
        }

        private static string Program => ""
            + "PROGRAM Part10;\n"
            + "VAR\n"
            + "    number     : INTEGER;\n"
            + "    a, b, c, x : INTEGER;\n"
            + "    y          : REAL;\n"
            + "\n"
            + "BEGIN {Part10}\n"
            + "    BEGIN\n"
            + "        number := 2;\n"
            + "        a := number;\n"
            + "        b := 10 * a + 10 * number DIV 4;\n"
            + "        c := a - - b\n"
            + "    END;\n"
            + "\n"
            + "    x := 11;\n"
            + "    y := 20 / 7 + 3.14;\n"
            + "    { writeln('a = ', a); }\n"
            + "    { writeln('b = ', b); }\n"
            + "    { writeln('c = ', c); }\n"
            + "    { writeln('number = ', number); }\n"
            + "    { writeln('x = ', x); }\n"
            + "    { writeln('y = ', y); }\n"
            + "END.  {Part10}\n";

        private dynamic Expr(string text)
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
            { "PROGRAM", new Token(Operation.Program, "PROGRAM") },
            { "VAR", new Token(Operation.Var, "VAR") },
            { "INTEGER", new Token(Operation.Integer, "INTEGER") },
            { "REAL", new Token(Operation.Real, "REAL") },
            { "DIV", new Token(Operation.IntegerDiv, "INTEGER_DIV") },
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
            while (this.currentChar != EOF)
            {
                if (Char.IsWhiteSpace(currentChar))
                {
                    while (Char.IsWhiteSpace(currentChar))
                        Advance();
                    continue;
                }

                if (currentChar == '{')
                {
                    while (currentChar != '}')
                        Advance();
                    Advance();
                    continue;
                }

                if (Char.IsDigit(currentChar))
                {
                    return Number();
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

                switch (this.currentChar)
                {
                    case '.':
                        Advance();
                        return new Token(Operation.Dot, '.');
                    case ';':
                        Advance();
                        return new Token(Operation.Semi, ';');
                    case ':':
                        Advance();
                        return new Token(Operation.Colon, ':');
                    case ',':
                        Advance();
                        return new Token(Operation.Comma, ',');
                    case '+':
                        Advance();
                        return new Token(Operation.Plus, '+');
                    case '-':
                        Advance();
                        return new Token(Operation.Minus, '-');
                    case '*':
                        Advance();
                        return new Token(Operation.Mul, '*');
                    case '/':
                        Advance();
                        return new Token(Operation.FloatDiv, '/');
                    case '(':
                        Advance();
                        return new Token(Operation.Lparen, '(');
                    case ')':
                        Advance();
                        return new Token(Operation.Rparen, ')');
                    default:
                        throw new InvalidOperationException("Error parsing input");
                }
            }

            return new Token(Operation.EOF, EOF);
        }

        private Token ID()
        {
            string result = string.Empty;
            while (Char.IsLetterOrDigit(currentChar))
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

        private Token Number()
        {
            string s = string.Empty;
            while (Char.IsDigit(currentChar))
            {
                s += currentChar.ToString();
                Advance();
            }
            if (currentChar == '.')
            {
                s += currentChar.ToString();
                Advance();
                while (Char.IsDigit(currentChar))
                {
                    s += currentChar.ToString();
                    Advance();
                }
                return new Token(Operation.RealConst, float.Parse(s));
            }

            return new Token(Operation.IntegerConst, int.Parse(s));
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

        internal Program Program()
        {
            Eat(Operation.Program);
            var variable = Variable();
            Eat(Operation.Semi);
            Block block = Block();
            Eat(Operation.Dot);
            return new Program(variable.Value, block);
        }

        private Block Block()
        {
            return new Block(Declarations(), CompoundStatement());
        }

        private VarDecl[] Declarations()
        {
            var result = new List<VarDecl>();
            if (currentToken.Type == Operation.Var)
            {
                Eat(Operation.Var);
                while (currentToken.Type == Operation.ID)
                {
                    result.AddRange(VariableDeclaration());
                    Eat(Operation.Semi);
                }
            }

            return result.ToArray();
        }

        private IEnumerable<VarDecl> VariableDeclaration()
        {
            var varNodes = new List<Var>();
            varNodes.Add(new Var(currentToken));
            Eat(Operation.ID);
            while (currentToken.Type == Operation.Comma)
            {
                Eat(Operation.Comma);
                varNodes.Add(new Var(currentToken));
                Eat(Operation.ID);
            }

            Eat(Operation.Colon);

            var type = Type();

            foreach (var item in varNodes)
            {
                yield return new VarDecl(item, type);
            }
        }

        private Compound CompoundStatement()
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
                throw new UnexpectedTokenTypeException(Operation.ID, currentToken.Type);
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

        private Assign AssignmentStatement()
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

        private Type Type()
        {
            var node = new Type(currentToken);
            if (currentToken.Type == Operation.Integer)
            {
                Eat(Operation.Integer);
            }
            else
            {
                Eat(Operation.Real);
            }
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

            if (currentToken.Type == Operation.IntegerConst)
            {
                var result = currentToken.Value;
                Eat(Operation.IntegerConst);
                return new Num(result);
            }

            if (currentToken.Type == Operation.RealConst)
            {
                var result = currentToken.Value;
                Eat(Operation.RealConst);
                return new Num(result);
            }

            return Variable();
        }

        internal AST Term()
        {
            var result = Factor();
            while (currentToken.Type == Operation.Mul || currentToken.Type == Operation.FloatDiv || currentToken.Type == Operation.IntegerDiv)
            {
                if (currentToken.Type == Operation.Mul)
                {
                    Eat(Operation.Mul);
                    result = new BinOp(result, Operation.Mul, Factor());
                }
                else if (currentToken.Type == Operation.FloatDiv)
                {
                    Eat(Operation.FloatDiv);
                    result = new BinOp(result, Operation.FloatDiv, Factor());
                }
                else if (currentToken.Type == Operation.IntegerDiv)
                {
                    Eat(Operation.IntegerDiv);
                    result = new BinOp(result, Operation.IntegerDiv, Factor());
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
                throw new UnexpectedTokenTypeException(Operation.EOF, currentToken.Type);
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
                throw new UnexpectedTokenTypeException(type, currentToken.Type);
            }
        }

        [Serializable]
        private class UnexpectedTokenTypeException : Exception
        {
            public UnexpectedTokenTypeException()
            {
            }

            public UnexpectedTokenTypeException(Operation expected, Operation actual) : this($"Expected: {expected}, Actual: {actual}")
            {
            }

            public UnexpectedTokenTypeException(string message) : base(message)
            {
            }

            public UnexpectedTokenTypeException(string message, Exception innerException) : base(message, innerException)
            {
            }

            protected UnexpectedTokenTypeException(SerializationInfo info, StreamingContext context) : base(info, context)
            {
            }
        }
    }

    internal abstract class AST
    {
    }

    internal class Program : AST
    {
        public Program(string name, Block block)
        {
            Name = name;
            Block = block;
        }

        public string Name { get; }
        public Block Block { get; }
    }

    internal class Block : AST
    {
        public Block(VarDecl[] declarations, Compound compoundStatement)
        {
            Declarations = declarations;
            CompoundStatement = compoundStatement;
        }

        public VarDecl[] Declarations { get; }
        public Compound CompoundStatement { get; }
    }

    internal class VarDecl : AST
    {
        public VarDecl(Var name, Type type)
        {
            VarNode = name;
            TypeNode = type;
        }

        public Var VarNode { get; }
        public Type TypeNode { get; }
    }

    internal class Type : AST
    {
        public Type(Token token)
        {
            Token = token;
            Value = token.Value;
        }

        public Token Token { get; }
        public dynamic Value { get; }
    }

    internal class Compound : AST
    {
        internal List<AST> Children { get; } = new List<AST>();

        internal void AddNode(AST node)
        {
            Children.Add(node);
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
    }

    internal class NoOp : AST
    {
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
    }

    internal class Num : AST
    {
        public dynamic Value { get; }

        public Num(dynamic value)
        {
            this.Value = value;
        }

        public override string ToString()
        {
            return $"{this.Value}";
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

        protected dynamic Visit(AST node)
        {
            return node switch
            {
                Program program => VisitProgram(program),
                Block block => VisitBlock(block),
                VarDecl decl => VisitDecl(decl),
                Compound compound => VisitCompound(compound),
                Assign assign => VisitAssign(assign),
                NoOp noOp => VisitNoOp(noOp),
                Var var => VisitVar(var),
                Num num => VisitNum(num),
                BinOp binOp => VisitBinOp(binOp),
                UnaryOp unaryOp => VisitUnaryOp(unaryOp),
                _ => throw new NotImplementedException()
            };
        }

        private dynamic VisitDecl(VarDecl decl)
        {
            return 0;
        }

        private dynamic VisitBlock(Block block)
        {
            foreach (var item in block.Declarations)
            {
                Visit(item);
            }
            return Visit(block.CompoundStatement);
        }

        private dynamic VisitProgram(Program program)
        {
            return Visit(program.Block);
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

        internal dynamic VisitNum(Num node)
        {
            return node.Value;
        }

        internal dynamic VisitBinOp(BinOp node)
        {
            switch (node.Operation)
            {
                case Operation.Plus:
                    return Visit(node.Left) + Visit(node.Right);
                case Operation.Minus:
                    return Visit(node.Left) - Visit(node.Right);
                case Operation.Mul:
                    return Visit(node.Left) * Visit(node.Right);
                case Operation.IntegerDiv:
                    return Visit(node.Left) / Visit(node.Right);
                case Operation.FloatDiv:
                    return Convert.ToSingle(Visit(node.Left)) / Convert.ToSingle(Visit(node.Right));
                default: throw new NotImplementedException();
            }
        }

        internal dynamic VisitUnaryOp(UnaryOp node)
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

        internal dynamic Interpret()
        {
            var tree = parser.Parse();
            return Visit(tree);
        }

        internal dynamic Expr()
        {
            return Visit(parser.Expr());
        }

        internal string PrintGlobalScope()
        {
            return $"[ {string.Join(", ", GlobalScope.Select(item => $"{item.Key} : {item.Value}"))} ]";
        }
    }

    internal enum Operation
    {
        None,
        Program,
        Var,
        ID,
        Integer,
        Real,
        Begin,
        End,
        Dot,
        Assign,
        IntegerConst,
        RealConst,
        Semi,
        Colon,
        Comma,
        Plus,
        Minus,
        Mul,
        IntegerDiv,
        FloatDiv,
        Lparen,
        Rparen,
        EOF,
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
