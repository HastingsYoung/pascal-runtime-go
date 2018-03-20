package parser

import (
	"errors"
	"github.com/pascal-runtime-go/intermediate"
	"github.com/pascal-runtime-go/intermediate/definition"
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
	Synchronize(set OpSubset) *Token
}

type IDeclarationParser interface {
	IParser
	Parse(token *Token, parentId *intermediate.SymTabEntry) *intermediate.SymTabEntry
}

type IBlockParser interface {
	IParser
	Parse(token *Token, routineId *intermediate.SymTabEntry) intermediate.ICodeNode
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
	Parse(token *Token) *intermediate.TypeSpec
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
		rootNode = statementParser.Parse(token)
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

	for (token.GetType() != EOFToken) && (set.Contains(token.GetName())) {
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

func (parser *DeclarationsParser) Parse(
	token *Token,
	parentId *intermediate.SymTabEntry,
) *intermediate.SymTabEntry {
	token = parser.Synchronize(DECLARATION_START_SET)

	if token.GetName() == CONST {
		token = parser.NextToken()
		constantDefinitionParser := NewConstantDefinitionsParser(parser)
		constantDefinitionParser.Parse(token, nil)
	}

	token = parser.Synchronize(TYPE_START_SET)

	if token.GetName() == TYPE {
		token = parser.NextToken()
		typeDefinitionsParser := NewTypeDefinitionsParser(parser)
		typeDefinitionsParser.Parse(token, nil)
	}

	token = parser.Synchronize(VAR_START_SET)

	if token.GetName() == VAR {
		token = parser.NextToken()
		variableDeclarationsParser := NewVariableDeclarationsParser(parser)
		variableDeclarationsParser.Parse(token, nil)
	}

	token = parser.Synchronize(ROUTINE_START_SET)
	name := token.GetName()

	for name == PROCEDURE || name == FUNCTION {
		routineParser := NewDeclaredroutineParser(parser)
		routineParser.Parse(token, parentId)

		token = parser.CurrentToken()

		if token.GetName() == SEMICOLON {
			for token.GetName() == SEMICOLON {
				token = parser.NextToken()
			}
		}

		token = parser.Synchronize(ROUTINE_START_SET)
		name = token.GetName()
	}
	return nil
}

type DeclaredRoutineParser struct {
	IDeclarationParser
}

func NewDeclaredroutineParser(parent IDeclarationParser) *DeclaredRoutineParser {
	return &DeclaredRoutineParser{
		parent,
	}
}

// todo: DeclaredRoutineParser.Parse

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

func NewConstantDefinitionsParserFromScanner(scan Scanner) *ConstantDefinitionsParser {
	return &ConstantDefinitionsParser{
		NewDeclarationsParser(NewPascalParser(scan)),
	}
}

func (parser *ConstantDefinitionsParser) Parse(
	token *Token,
	parentId *intermediate.SymTabEntry,
) *intermediate.SymTabEntry {
	token = parser.Synchronize(IDENTIFIER_SET)
	for token.GetName() == IDENTIFIER {
		text := strings.ToLower(token.GetText())
		constantId := parser.GetSymTabStack().LookUpLocal(text)

		if constantId == nil {
			constantId = parser.GetSymTabStack().EnterLocal(text)
			constantId.AppendLineNum(token.GetLineNum())
		} else {
			panic(errors.New("Identifier Redefined"))
		}

		token = parser.NextToken()

		token = parser.Synchronize(EQUALS_SET)
		if token.GetName() == EQUALS {
			token = parser.NextToken()
		} else {
			panic(errors.New("Missing Equals"))
		}

		constantToken := *token
		value := parser.ParseConstant(&constantToken)

		if constantId == nil {
			constantId.SetDefinition(definition.CONSTANT)
			constantId.SetAttribute("CONSTANT_VALUE", value)

			var constantType *intermediate.TypeSpec
			if constantToken.GetName() == IDENTIFIER {
				constantType = parser.GetConstantTypeFromToken(&constantToken)
			} else {
				constantType = parser.GetConstantType(value)
			}
			constantId.SetTypeSpec(constantType)
		}

		token = parser.CurrentToken()
		name := token.GetName()

		if name == SEMICOLON {
			for token.GetName() == SEMICOLON {
				token = parser.NextToken()
			}
		} else if NEXT_START_SET.Contains(name) {
			panic(errors.New("Missing Semicolon"))
		}

		token = parser.Synchronize(IDENTIFIER_SET)
	}
	return nil
}

func (parser *ConstantDefinitionsParser) ParseConstant(token *Token) interface{} {
	var sign TokenName

	token = parser.Synchronize(CONSTANT_START_SET)
	name := token.GetName()

	if name == PLUS || name == MINUS {
		sign = name
		token = parser.NextToken()
	}

	switch token.GetName() {
	case IDENTIFIER:
		return parser.ParseIdentifierConstant(token, sign)
	case INTEGER:
		value := (token.GetValue()).(int)
		parser.NextToken()
		if sign == MINUS {
			return -value
		}
		return value
	case REAL:
		value := (token.GetValue()).(float64)
		parser.NextToken()
		if sign == MINUS {
			return -value
		}
		return value
	case STRING:
		parser.NextToken()
		return (token.GetValue()).(string)
	default:
		return nil
	}
}

func (parser *ConstantDefinitionsParser) ParseIdentifierConstant(token *Token, sign TokenName) interface{} {
	name := strings.ToLower(token.GetText())
	id := parser.GetSymTabStack().LookUpLocal(name)

	parser.NextToken()

	if id == nil {
		panic(errors.New("Identifier Undefined"))
		return nil
	}

	def := id.GetDefinition()

	if def == definition.CONSTANT {
		value := id.GetAttribute("CONSTANT_VALUE")
		id.AppendLineNum(token.GetLineNum())

		switch value.(type) {
		case int:
			if sign == MINUS {
				return -(value.(int))
			}
			return (value.(int))
		case float64:
			if sign == MINUS {
				return -(value.(float64))
			}
			return (value.(float64))
		case string:
			return value
		default:
			return nil
		}
	} else if def == definition.ENUMERATION_CONSTANT {
		value := id.GetAttribute("CONSTANT_VALUE")
		id.AppendLineNum(token.GetLineNum())
		return value
	}
	return nil
}

func (parser *ConstantDefinitionsParser) GetConstantType(val interface{}) *intermediate.TypeSpec {
	switch val.(type) {
	case int:
		return intermediate.IntegerType
	case float64:
		return intermediate.RealType
	case string:
		if s := val.(string); len(s) == 1 {
			return intermediate.CharType
		} else {
			return intermediate.NewTypeSpecImplFromString(s)
		}
	default:
	}
	return nil
}

func (parser *ConstantDefinitionsParser) GetConstantTypeFromToken(token *Token) *intermediate.TypeSpec {
	name := strings.ToLower(token.GetText())
	id := parser.GetSymTabStack().LookUpLocal(name)

	if id == nil {
		return nil
	}

	def := id.GetDefinition()

	if def == definition.CONSTANT || def == definition.ENUMERATION_CONSTANT {
		return id.GetTypeSpec()
	}

	return nil
}

type TypeDefinitionsParser struct {
	IDeclarationParser
}

func NewTypeDefinitionsParser(parent IDeclarationParser) *TypeDefinitionsParser {
	return &TypeDefinitionsParser{
		parent,
	}
}

var FOLLOW_SET = OpSubset{SEMICOLON: SEMICOLON}

func (parser *TypeDefinitionsParser) Parse(
	token *Token,
	parentId *intermediate.SymTabEntry,
) *intermediate.SymTabEntry {
	token = parser.Synchronize(IDENTIFIER_SET)
	for token.GetName() == IDENTIFIER {
		text := strings.ToLower(token.GetText())
		typeId := parser.GetSymTabStack().LookUpLocal(text)

		if typeId == nil {
			typeId = parser.GetSymTabStack().EnterLocal(text)
			typeId.AppendLineNum(token.GetLineNum())
		} else {
			panic(errors.New("Identifier Redefined"))
		}

		token = parser.NextToken()

		token = parser.Synchronize(EQUALS_SET)
		if token.GetName() == EQUALS {
			token = parser.NextToken()
		} else {
			panic(errors.New("Missing Equals"))
		}

		typeSpecDefinitionParser := NewTypeSpecificationParser(parser)
		spec := typeSpecDefinitionParser.Parse(token)

		if spec != nil && typeId != nil {
			if spec.GetIdentifier() == nil {
				spec.SetIdentifier(typeId)
			}
			typeId.SetTypeSpec(spec)
		} else {
			token = parser.Synchronize(FOLLOW_SET)
		}

		token = parser.CurrentToken()
		name := token.GetName()

		if name == SEMICOLON {
			for token.GetName() == SEMICOLON {
				parser.NextToken()
			}
		} else if NEXT_START_SET.Contains(name) {
			panic(errors.New("Missing Semicolon"))
		}

		token = parser.Synchronize(IDENTIFIER_SET)
	}

	return nil
}

type TypeSpecificationParser struct {
	IParser
}

func NewTypeSpecificationParser(parent IParser) *TypeSpecificationParser {
	return &TypeSpecificationParser{
		parent,
	}
}

var SIMPLE_TYPE_START_SET = OpSubset{
	IDENTIFIER: IDENTIFIER,
	INTEGER:    INTEGER,
	REAL:       REAL,
	PLUS:       PLUS,
	MINUS:      MINUS,
	STRING:     STRING,
	SEMICOLON:  SEMICOLON,
	COMMA:      COMMA,
	LEFT_PAREN: LEFT_PAREN,
}

func (parser *TypeSpecificationParser) Parse(token *Token) *intermediate.TypeSpec {
	token = parser.Synchronize(TYPE_START_SET)

	switch token.GetName() {
	case ARRAY:
		arrayTypeParser := NewArrayTypeParser(parser)
		return arrayTypeParser.Parse(token)
	case RECORD:
		recordTypeParser := NewRecordTypeParser(parser)
		return recordTypeParser.Parse(token)
	default:
		simpleTypeParser := NewSimpleTypeParser(parser)
		return simpleTypeParser.Parse(token)
	}
}

type ArrayTypeParser struct {
	ITypeSpecificationParser
}

func NewArrayTypeParser(parent ITypeSpecificationParser) *ArrayTypeParser {
	return &ArrayTypeParser{
		parent,
	}
}

var (
	LEFT_BRACKET_SET = OpSubset{
		ARRAY:         ARRAY,
		RECORD:        RECORD,
		SEMICOLON:     SEMICOLON,
		LEFT_BRACKET:  LEFT_BRACKET,
		RIGHT_BRACKET: RIGHT_BRACKET,
	}

	RIGHT_BRACKET_SET = OpSubset{
		RIGHT_BRACKET: RIGHT_BRACKET,
		OF:            OF,
		SEMICOLON:     SEMICOLON,
	}

	OF_SET = OpSubset{
		ARRAY:     ARRAY,
		RECORD:    RECORD,
		SEMICOLON: SEMICOLON,
		OF:        OF,
	}
	INDEX_START_SET = OpSubset{
		ARRAY:     ARRAY,
		RECORD:    RECORD,
		SEMICOLON: SEMICOLON,
		COMMA:     COMMA,
	}
	INDEX_END_SET = OpSubset{
		RIGHT_BRACKET: RIGHT_BRACKET,
		OF:            OF,
		SEMICOLON:     SEMICOLON,
	}
	INDEX_FOLLOW_SET = OpSubset{
		ARRAY:         ARRAY,
		RECORD:        RECORD,
		COMMA:         COMMA,
		RIGHT_BRACKET: RIGHT_BRACKET,
		OF:            OF,
		SEMICOLON:     SEMICOLON,
	}
)

func (parser *ArrayTypeParser) Parse(token *Token) *intermediate.TypeSpec {
	arrayType := intermediate.NewTypeSpecImpl(intermediate.ARRAY)
	token = parser.NextToken()

	token = parser.Synchronize(LEFT_BRACKET_SET)
	if token.GetName() != LEFT_BRACKET {
		panic(errors.New("Missing Left Bracket"))
	}

	elemType := parser.ParseIndexTypeList(token, arrayType)

	token = parser.Synchronize(RIGHT_BRACKET_SET)
	if token.GetName() == RIGHT_BRACKET {
		token = parser.NextToken()
	} else {
		panic(errors.New("Missing Right Bracket"))
	}

	token = parser.Synchronize(OF_SET)
	if token.GetName() == OF {
		token = parser.NextToken()
	} else {
		panic(errors.New("Missing Of"))
	}

	elemType.SetAttribute(intermediate.ARRAY_ELEMENT_TYPE, parser.ParseElementType(token))
	return arrayType
}

func (parser *ArrayTypeParser) ParseIndexTypeList(
	token *Token,
	arrayType *intermediate.TypeSpec,
) *intermediate.TypeSpec {
	elemType := arrayType
	anotherIndex := false
	token = parser.NextToken()

	token = parser.Synchronize(INDEX_START_SET)
	parser.ParseIndexType(token, elemType)

	token = parser.Synchronize(INDEX_FOLLOW_SET)
	name := token.GetName()

	if name != COMMA && name != RIGHT_BRACKET {
		if INDEX_START_SET.Contains(name) {
			anotherIndex = true
			panic(errors.New("Missing Comma"))
		}
	} else if name == COMMA {
		newElementType := intermediate.NewTypeSpecImpl(intermediate.ARRAY)
		elemType.SetAttribute(intermediate.ARRAY_ELEMENT_TYPE, newElementType)
		elemType = newElementType
		token = parser.NextToken()
		anotherIndex = true
	}

	for anotherIndex {
		anotherIndex = false
		token = parser.Synchronize(INDEX_START_SET)
		parser.ParseIndexType(token, elemType)

		token = parser.Synchronize(INDEX_FOLLOW_SET)
		name := token.GetName()

		if name != COMMA && name != RIGHT_BRACKET {
			if INDEX_START_SET.Contains(name) {
				anotherIndex = true
				panic(errors.New("Missing Comma"))
			}
		} else if name == COMMA {
			newElementType := intermediate.NewTypeSpecImpl(intermediate.ARRAY)
			elemType.SetAttribute(intermediate.ARRAY_ELEMENT_TYPE, newElementType)
			elemType = newElementType
			token = parser.NextToken()
			anotherIndex = true
		}
	}

	return elemType
}

func (parser *ArrayTypeParser) ParseIndexType(token *Token, arrayType *intermediate.TypeSpec) {
	simpleTypeParser := NewSimpleTypeParser(parser)
	indexType := simpleTypeParser.Parse(token)

	arrayType.SetAttribute(intermediate.ARRAY_INDEX_TYPE, indexType)

	if indexType == nil {
		return
	}

	form := indexType.GetForm()
	count := 0

	if form == intermediate.SUBRANGE {
		minValue := (indexType.GetAttribute(intermediate.SUBRANGE_MIN_VALUE)).(int)
		maxValue := (indexType.GetAttribute(intermediate.SUBRANGE_MAX_VALUE)).(int)
		count = maxValue - minValue + 1
	} else if form == intermediate.ENUMERATION {
		constants := (indexType.GetAttribute(intermediate.ENUMERATION_CONSTANTS)).([]*intermediate.SymTabEntry)
		count = len(constants)
	} else {
		panic(errors.New("Invalid Index Type"))
	}

	arrayType.SetAttribute(intermediate.ARRAY_ELEMENT_COUNT, count)
	return
}

func (parser *ArrayTypeParser) ParseElementType(token *Token) *intermediate.TypeSpec {
	typeSpecificationParser := NewTypeSpecificationParser(parser)
	return typeSpecificationParser.Parse(token)
}

type RecordTypeParser struct {
	ITypeSpecificationParser
}

func NewRecordTypeParser(parent ITypeSpecificationParser) *RecordTypeParser {
	return &RecordTypeParser{
		parent,
	}
}

var END_SET = OpSubset{
	VAR:       VAR,
	PROCEDURE: PROCEDURE,
	FUNCTION:  FUNCTION,
	BEGIN:     BEGIN,
	END:       END,
	SEMICOLON: SEMICOLON,
}

func (parser *RecordTypeParser) Parse(token *Token) *intermediate.TypeSpec {
	recordType := intermediate.NewTypeSpecImpl(intermediate.RECORD)
	token = parser.NextToken()

	recordType.SetAttribute(intermediate.RECORD_SYMTAB, parser.GetSymTabStack().Push())
	variableDeclarationsParser := NewVariableDeclarationsParserFromScanner(parser.GetScanner())
	variableDeclarationsParser.SetDefinition(definition.FIELD)
	variableDeclarationsParser.Parse(token, nil)

	parser.GetSymTabStack().Pop()
	token = parser.Synchronize(END_SET)
	if token.GetName() == END {
		token = parser.NextToken()
	} else {
		panic(errors.New("Missing End"))
	}

	return recordType
}

type SubrangeTypeParser struct {
	ITypeSpecificationParser
}

func NewSubrangeTypeParser(parent ITypeSpecificationParser) *SubrangeTypeParser {
	return &SubrangeTypeParser{
		parent,
	}
}

func (parser *SubrangeTypeParser) Parse(token *Token) *intermediate.TypeSpec {
	subrangeType := intermediate.NewTypeSpecImpl(intermediate.SUBRANGE)
	constantToken := &(*token)
	constantParser := NewConstantDefinitionsParserFromScanner(parser.GetScanner())
	var (
		minValue interface{}
		maxValue interface{}
		minType  *intermediate.TypeSpec
		maxType  *intermediate.TypeSpec
	)

	minValue = constantParser.ParseConstant(token)

	if constantToken.GetName() == IDENTIFIER {
		constantParser.GetConstantTypeFromToken(constantToken)
	} else {
		constantParser.GetConstantType(minValue)
	}

	minValue = parser.CheckValueType(constantToken, minValue, minType)
	token = parser.CurrentToken()

	sawDotDot := false

	if token.GetName() == DOT_DOT {
		token = parser.NextToken()
		sawDotDot = true
	}

	name := token.GetName()
	if CONSTANT_START_SET.Contains(name) {
		if !sawDotDot {
			panic(errors.New("Missing Dot Dot"))
		}

		token = parser.Synchronize(CONSTANT_START_SET)
		constantToken = token
		maxValue = constantParser.ParseConstant(token)
		if constantToken.GetName() == IDENTIFIER {
			maxType = constantParser.GetConstantTypeFromToken(constantToken)
		} else {
			maxType = constantParser.GetConstantType(maxValue)
		}
		maxValue = parser.CheckValueType(constantToken, maxValue, maxType)

		if minValue == nil || maxValue == nil {
			panic(errors.New("Incompatible Types"))
		} else if minType != maxType {
			panic(errors.New("Invalid Subrange Type"))
		} else if minValue != nil && maxValue != nil {
			if min, max := minValue.(int), maxValue.(int); min >= max {
				panic(errors.New("Min Greater Than Max"))
			}
		}
	} else {
		panic(errors.New("Invalid Subrange Type"))
	}

	subrangeType.SetAttribute(intermediate.SUBRANGE_BASE_TYPE, minType)
	subrangeType.SetAttribute(intermediate.SUBRANGE_MIN_VALUE, minValue)
	subrangeType.SetAttribute(intermediate.SUBRANGE_MAX_VALUE, maxValue)

	return subrangeType
}

func (parser *SubrangeTypeParser) CheckValueType(
	token *Token,
	value interface{},
	t *intermediate.TypeSpec,
) interface{} {
	if t == nil {
		return value
	}
	if t == intermediate.IntegerType {
		return value
	} else if t == intermediate.CharType {
		return int(value.(rune)) - int('0')
	} else if t.GetForm() == intermediate.ENUMERATION {
		return value
	} else {
		panic(errors.New("Invalid Subrange Type"))
	}
}

type EnumerationTypeParser struct {
	ITypeSpecificationParser
}

func NewEnumerationTypeParser(parent ITypeSpecificationParser) *EnumerationTypeParser {
	return &EnumerationTypeParser{
		parent,
	}
}

type SimpleTypeParser struct {
	ITypeSpecificationParser
}

func NewSimpleTypeParser(parent ITypeSpecificationParser) *SimpleTypeParser {
	return &SimpleTypeParser{
		parent,
	}
}

func (parser *SimpleTypeParser) Parse(token *Token) *intermediate.TypeSpec {
	token = parser.Synchronize(SIMPLE_TYPE_START_SET)

	switch token.GetName() {
	case IDENTIFIER:
		text := strings.ToLower(token.GetText())
		id := parser.GetSymTabStack().LookUpLocal(text)
		if id != nil {
			def := id.GetDefinition()
			if def == definition.TYPE {
				id.AppendLineNum(token.GetLineNum())
				token = parser.NextToken()
				return id.GetTypeSpec()
			} else if def != definition.CONSTANT && def != definition.ENUMERATION_CONSTANT {
				panic(errors.New("Not Type Identifier"))
				token = parser.NextToken()
			} else {
				subrangeTypeParser := NewSubrangeTypeParser(parser)
				return subrangeTypeParser.Parse(token)
			}
		} else {
			panic(errors.New("Identifier Undefined"))
			token = parser.NextToken()
			return nil
		}
	case LEFT_PAREN:
		enumerationTypeParser := NewEnumerationTypeParser(parser)
		return enumerationTypeParser.Parse(token)
	case COMMA:
		panic(errors.New("Invalid Type"))
	case SEMICOLON:
		panic(errors.New("Invalid Type"))
	default:
		subrangeTypeParser := NewSubrangeTypeParser(parser)
		return subrangeTypeParser.Parse(token)
	}

	return nil
}

type VariableDeclarationsParser struct {
	IDeclarationParser
	def definition.Definition
}

func NewVariableDeclarationsParser(parent IDeclarationParser) *VariableDeclarationsParser {
	return &VariableDeclarationsParser{
		IDeclarationParser: parent,
	}
}

func NewVariableDeclarationsParserFromScanner(scan Scanner) *VariableDeclarationsParser {
	return &VariableDeclarationsParser{
		IDeclarationParser: NewDeclarationsParser(NewPascalParser(scan)),
	}
}

func (parser *VariableDeclarationsParser) SetDefinition(def definition.Definition) {
	parser.def = def
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
