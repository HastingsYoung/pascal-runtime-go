package parser

import (
	"errors"
	"github.com/pascal-runtime-go/intermediate"
	"github.com/pascal-runtime-go/message"
	. "github.com/pascal-runtime-go/scanner"
	. "github.com/pascal-runtime-go/token"
	"strings"
)

type OpSubset map[TokenName]TokenName

func (subset *OpSubset) Contains(name TokenName) bool {
	if _, ok := (*subset)[name]; !ok {
		return false
	}

	return true
}

// Updates: delete [Parse() error] function
type IParser interface {
	GetScanner() Scanner
	GetICode() intermediate.ICode
	GetSymTabStack() *intermediate.SymTabStack
	GetErrorCount() int
	CurrentToken() *Token
	NextToken() *Token
}

type IDeclarationParser interface {
	IParser
	Parse(token *Token, parentId *SymTabEntry) *SymTabEntry
}

type IBlockParser interface {
	IParser
	Parse(token *Token, routineId *SymtabEntry) ICodeNode
}

type IStatementParser interface {
	IParser
	Parse(token *Token) intermediate.ICodeNode
	ParseList(
		token *Token,
		parentNode intermediate.ICodeNode,
		terminator TokenName,
	)
}

type ITypeSpecificationParser interface {
	IParser
	Parse(token *Token) *TypeSpec
}

type PascalParser struct {
	scanner Scanner
	iCode   intermediate.ICode
	stack   *intermediate.SymTabStack
}

func NewPascalParser(scanner Scanner) *PascalParser {
	return &PascalParser{
		scanner: scanner,
		stack:   intermediate.NewSymTabStack(),
	}
}

func (parser *PascalParser) GetScanner() Scanner {
	return parser.scanner
}

func (parser *PascalParser) GetICode() intermediate.ICode {
	return parser.iCode
}

func (parser *PascalParser) GetSymTabStack() *intermediate.SymTabStack {
	return parser.stack
}

func (parser *PascalParser) Parse() error {

	iCode := intermediate.NewICode()

	token := parser.NextToken()

	var rootNode intermediate.ICodeNode

	if token.GetName() == BEGIN {
		statementParser := NewStatementParser(parser)
		rootNode = statementParser.ParseInLocal(token)
		token = parser.CurrentToken()
	} else {
		message.Error("Unexpected Token", token)
	}

	if token.GetName() != DOT {
		message.Error("Missing Period", token)
	}

	token = parser.CurrentToken()

	if rootNode != nil {
		iCode.SetRoot(rootNode)
	}

	return nil
}

func (parser *PascalParser) GetErrorCount() int {
	return 0
}

func (parser *PascalParser) CurrentToken() *Token {
	return parser.scanner.CurrentToken()
}

func (parser *PascalParser) NextToken() *Token {
	return parser.scanner.NextToken()
}

func (parser *PascalParser) Synchronize(set OpSubset) *Token {
	token := parser.CurrentToken()

	if set.Contains(token.GetName()) {
		panic(errors.New("Unexpected Token"))
	}

	token = parser.NextToken()

	for (token.GetType() != EOFToken) && (set.Contains(name)) {
		token = parser.NextToken()
	}
	return token
}

type DeclarationsParser struct {
	IParser
}

func NewDeclarationsParser(parent IParser) *DeclarationsParser {
	return &DeclarationsParser{
		parent,
	}
}

var DECLARATION_START_SET = OpSubset{
	CONST:     CONST,
	TYPE:      TYPE,
	VAR:       VAR,
	PROCEDURE: PROCEDURE,
	FUNCTION:  FUNCTION,
	BEGIN:     BEGIN,
}

var TYPE_START_SET = OpSubset{
	TYPE:      TYPE,
	VAR:       VAR,
	PROCEDURE: PROCEDURE,
	FUNCTION:  FUNCTION,
	BEGIN:     BEGIN,
}

var VAR_START_SET = OpSubset{
	VAR:       VAR,
	PROCEDURE: PROCEDURE,
	FUNCTION:  FUNCTION,
	BEGIN:     BEGIN,
}

var ROUTINE_START_SET = OpSubset{
	PROCEDURE: PROCEDURE,
	FUNCTION:  FUNCTION,
	BEGIN:     BEGIN,
}

// ParseInLocal may bear a different signature to other struct inherited from *PascalParser
func (parser *DeclarationsParser) Parse(token *Token, parentId *SymTabEntry) *SymTabEntry {
	token = parser.Synchronize(DECLARATION_START_SET)

	// if token.GetName() == CONST {
	// 	token = parser.NextToken()
	// 	constantDefinitionParser
	// }
}

type DeclaredRoutineParser struct {
	IDeclarationParser
}

func NewDeclaredroutineParser(parent IDeclarationParser) *DeclaredRoutineParser {
	return &DeclaredRoutineParser{
		parent,
	}
}

type ConstantDefinitionsParser struct {
	IDeclarationParser
}

var (
	IDENTIFIER_SET = OpSubset{
		TYPE:       TYPE,
		VAR:        VAR,
		PROCEDURE:  PROCEDURE,
		FUNCTION:   FUNCTION,
		BEGIN:      BEGIN,
		IDENTIFIER: IDENTIFIER,
	}
	CONSTANT_START_SET = OpSubset{
		IDENTIFIER: IDENTIFIER,
		INTEGER:    INTEGER,
		REAL:       REAL,
		PLUS:       PLUS,
		MINUS:      MINUS,
		STRING:     STRING,
		SEMICOLON:  SEMICOLON,
	}
	EQUALS_SET = OpSubset{
		IDENTIFIER: IDENTIFIER,
		INTEGER:    INTEGER,
		REAL:       REAL,
		PLUS:       PLUS,
		MINUS:      MINUS,
		STRING:     STRING,
		SEMICOLON:  SEMICOLON,
		EQUALS:     EQUALS,
		SEMICOLON:  SEMICOLON,
	}
	NEXT_START_SET = OpSubset{
		TYPE:       TYPE,
		VAR:        VAR,
		PROCEDURE:  PROCEDURE,
		FUNCTION:   FUNCTION,
		BEGIN:      BEGIN,
		SEMICOLON:  SEMICOLON,
		IDENTIFIER: IDENTIFIER,
	}
)

func NewConstantDefinitionsParser(parent IDeclarationParser) *ConstantDefinitionsParser {
	return &ConstantDefinitionsParser{
		parent,
	}
}

func (parser *ConstantDefinitionsParser) Parse(
	token *Token,
	parentId *intermediate.SymTabEntry,
) *intermediate.SymTabEntry {

}

func (parser *ConstantDefinitionsParser) ParseConstant(token *Token) interface{} {

}

func (parser *ConstantDefinitionsParser) ParseIdentifierConstant(token *Token, name TokenName) interface{} {

}

func (parser *ConstantDefinitionsParser) GetConstantType(val interface{}) *intermediate.TypeSpec {

}

func (parser *ConstantDefinitionsParser) GetConstantTypeFromToken(token *Token) *intermediate.TypeSpec {

}

type StatementParser struct {
	IParser
}

func NewStatementParser(parent IParser) *StatementParser {
	return &StatementParser{
		parent,
	}
}

func (parser *StatementParser) Parse(token *Token) intermediate.ICodeNode {
	var statementNode intermediate.ICodeNode
	switch token.GetName() {
	case BEGIN:
		statementNode = NewCompoundStatementParser(parser).Parse(token)
	case IDENTIFIER:
		statementNode = NewAssignmentStatementParser(parser).Parse(token)
	default:
		statementNode = intermediate.NewICodeNodeImpl("NO_OP")
	}

	parser.SetLineNumber(statementNode, token)
	return statementNode
}

func (parser *StatementParser) SetLineNumber(node intermediate.ICodeNode, token *Token) {
	if node != nil {
		node.SetAttribute("LINE", token.GetLineNum())
	}
}

func (parser *StatementParser) ParseList(
	token *Token,
	parentNode intermediate.ICodeNode,
	terminator TokenName,
) {
	for token.Type != EOFToken && token.GetName() != terminator {
		statementNode := parser.Parse(token)
		parentNode.AddChild(statementNode)

		token = parser.CurrentToken()
		tokenName := token.GetName()
		if tokenName == SEMICOLON {
			token = parser.NextToken()
		} else if tokenName == IDENTIFIER {
			panic(errors.New("Mission Semicolon"))
		} else if tokenName != terminator {
			panic(errors.New("Unexpected Token"))
			token = parser.NextToken()
		}
	}

	if token.GetName() == terminator {
		token = parser.NextToken()
		return
	}

	panic("Unknown Token")
	return
}

type ExpressionParser struct {
	IStatementParser
}

func NewExpressionParser(parent IStatementParser) *ExpressionParser {
	return &ExpressionParser{
		parent,
	}
}

var REL_OPS = OpSubset{
	EQUALS:         EQUALS,
	NOT_EQUALS:     NOT_EQUALS,
	LESS_THAN:      LESS_THAN,
	LESS_EQUALS:    LESS_EQUALS,
	GREATER_THAN:   GREATER_THAN,
	GREATER_EQUALS: GREATER_EQUALS,
}

var ADD_OPS = OpSubset{
	PLUS:  PLUS,
	MINUS: MINUS,
	OR:    OR,
}

var MULT_OPS = OpSubset{
	STAR:  STAR,
	SLASH: SLASH,
	DIV:   DIV,
	MOD:   MOD,
	AND:   AND,
}

var TOKEN_NAMES_TO_NODE = map[TokenName]intermediate.NodeType{
	// REL_OPS
	EQUALS:         intermediate.EQ,
	NOT_EQUALS:     intermediate.NE,
	LESS_THAN:      intermediate.LT,
	LESS_EQUALS:    intermediate.LE,
	GREATER_THAN:   intermediate.GT,
	GREATER_EQUALS: intermediate.GE,

	// ADD_OPS
	PLUS:  intermediate.ADD,
	MINUS: intermediate.SUBTRACT,
	OR:    intermediate.OR,

	// MULT_OPS
	STAR:  intermediate.MULTIPLY,
	SLASH: intermediate.FLOAT_DIVIDE,
	DIV:   intermediate.INTEGER_DIVIDE,
	MOD:   intermediate.MOD,
	AND:   intermediate.AND,
}

func (parser *ExpressionParser) ParseExpression(token *Token) intermediate.ICodeNode {
	rootNode := parser.ParseSimpleExpression(token)
	token = parser.CurrentToken()
	tokenName := token.GetName()

	if REL_OPS.Contains(tokenName) {
		opNode := intermediate.NewICodeNodeImpl(TOKEN_NAMES_TO_NODE[tokenName])
		opNode.AddChild(rootNode)

		token = parser.NextToken()

		opNode.AddChild(parser.ParseSimpleExpression(token))
		rootNode = opNode
	}

	return rootNode
}

func (parser *ExpressionParser) ParseSimpleExpression(token *Token) intermediate.ICodeNode {
	var (
		leadingSignName TokenName
		tokenName       = token.GetName()
	)

	if tokenName == PLUS || tokenName == MINUS {
		leadingSignName = tokenName
		token = parser.NextToken()
	}

	rootNode := parser.ParseTerm(token)

	if leadingSignName == MINUS {
		negateNode := intermediate.NewICodeNodeImpl(intermediate.NEGATE)
		negateNode.AddChild(rootNode)
		rootNode = negateNode
	}

	token = parser.CurrentToken()
	tokenName = token.GetName()

	for ADD_OPS.Contains(tokenName) {
		opNode := intermediate.NewICodeNodeImpl(TOKEN_NAMES_TO_NODE[tokenName])
		opNode.AddChild(rootNode)
		token = parser.NextToken()

		opNode.AddChild(parser.ParseTerm(token))
		rootNode = opNode
		token = parser.CurrentToken()
		tokenName = token.GetName()
	}

	return rootNode
}

func (parser *ExpressionParser) ParseTerm(token *Token) intermediate.ICodeNode {
	var (
		rootNode intermediate.ICodeNode = parser.ParseFactor(token)
	)

	token = parser.CurrentToken()

	for name := token.GetName(); MULT_OPS.Contains(name); {
		opNode := intermediate.NewICodeNodeImpl(TOKEN_NAMES_TO_NODE[name])

		opNode.AddChild(rootNode)

		token = parser.NextToken()

		opNode.AddChild(parser.ParseFactor(token))

		rootNode = opNode

		token = parser.CurrentToken()

		name = token.GetName()
	}

	return rootNode
}

func (parser *ExpressionParser) ParseFactor(token *Token) intermediate.ICodeNode {
	var (
		rootNode  intermediate.ICodeNode
		tokenName = token.GetName()
	)

	switch tokenName {

	case IDENTIFIER:
		name := strings.ToLower(token.GetText())
		id := parser.GetSymTabStack().LookUpLocal(name)

		if id == nil {
			panic(errors.New("Identifier Undefined"))
			id = parser.GetSymTabStack().EnterLocal(name)
		}

		rootNode = intermediate.NewICodeNodeImpl(intermediate.VARIABLE)
		rootNode.SetAttribute("ID", id)
		id.AppendLineNum(token.GetLineNum())

		token = parser.NextToken()
	case INTEGER:
		rootNode = intermediate.NewICodeNodeImpl(intermediate.INTEGER_CONSTANT)
		rootNode.SetAttribute("VALUE", token.GetValue())

		token = parser.NextToken()
	case REAL:
		rootNode = intermediate.NewICodeNodeImpl(intermediate.REAL_CONSTANT)
		rootNode.SetAttribute("VALUE", token.GetValue())

		token = parser.NextToken()
	case STRING:
		rootNode = intermediate.NewICodeNodeImpl(intermediate.STRING_CONSTANT)
		rootNode.SetAttribute("VALUE", token.GetValue())

		token = parser.NextToken()
	case NOT:
		token = parser.NextToken()

		rootNode = intermediate.NewICodeNodeImpl(intermediate.NOT)
		rootNode.AddChild(parser.ParseFactor(token))
	case LEFT_PAREN:
		token = parser.NextToken()

		rootNode = parser.ParseExpression(token)

		token = parser.CurrentToken()

		if token.GetName() == RIGHT_PAREN {
			token = parser.NextToken()
		} else {
			panic(errors.New("Missing Right Paren"))
		}

	default:
		panic(errors.New("Unexpected Token"))
	}

	return rootNode
}

type CompoundStatementParser struct {
	IStatementParser
}

func NewCompoundStatementParser(parent IStatementParser) *CompoundStatementParser {
	return &CompoundStatementParser{
		parent,
	}
}

func (parser *CompoundStatementParser) Parse(token *Token) intermediate.ICodeNode {
	token = parser.NextToken()
	compoundNode := intermediate.NewICodeNodeImpl(intermediate.COMPOUND)
	parser.ParseList(token, compoundNode, END)
	return compoundNode
}

type AssignmentStatementParser struct {
	IStatementParser
}

func NewAssignmentStatementParser(parent IStatementParser) *AssignmentStatementParser {
	return &AssignmentStatementParser{
		parent,
	}
}

func (parser *AssignmentStatementParser) Parse(token *Token) intermediate.ICodeNode {
	assignNode := intermediate.NewICodeNodeImpl(intermediate.ASSIGN)
	targetName := strings.ToLower(token.GetText())
	targetId := parser.GetSymTabStack().LookUpLocal(targetName)
	if targetId == nil {
		targetId = parser.GetSymTabStack().EnterLocal(targetName)
	}
	targetId.AppendLineNum(token.GetLineNum())

	token = parser.NextToken()

	variableNode := intermediate.NewICodeNodeImpl("VARIABLE")
	variableNode.SetAttribute("ID", targetId)
	assignNode.AddChild(variableNode)

	if token.GetName() == COLON_EQUALS {
		token = parser.NextToken()
	} else {
		panic(errors.New("Missing Colon EQUALS"))
	}

	expressionParser := NewExpressionParser(parser)
	assignNode.AddChild(expressionParser.Parse(token))
	return assignNode
}
