package parser

import (
	"errors"
	"github.com/pascal-runtime-go/intermediate"
	"github.com/pascal-runtime-go/intermediate/definition"
	"github.com/pascal-runtime-go/intermediate/routinecode"
	"github.com/pascal-runtime-go/message"
	. "github.com/pascal-runtime-go/scanner"
	. "github.com/pascal-runtime-go/token"
	"strconv"
	"strings"
)

type OpSubset map[TokenName]TokenName

func (subset OpSubset) Contains(name TokenName) bool {
	if _, ok := subset[name]; !ok {
		return false
	}

	return true
}

func (subset OpSubset) Copy() OpSubset {
	s := subset
	return s
}

func (subset OpSubset) Add(name TokenName) OpSubset {
	subset[name] = name
	s := subset
	return s
}

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

	iCode := intermediate.NewICodeImpl()

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
		routineParser := NewDeclaredRoutineParser(parser)
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
	counter int
}

func NewDeclaredRoutineParser(parent IDeclarationParser) *DeclaredRoutineParser {
	return &DeclaredRoutineParser{
		parent,
		0,
	}
}

var (
	PARAMETER_SET = OpSubset{
		CONST:       CONST,
		TYPE:        TYPE,
		VAR:         VAR,
		PROCEDURE:   PROCEDURE,
		FUNCTION:    FUNCTION,
		BEGIN:       BEGIN,
		IDENTIFIER:  IDENTIFIER,
		RIGHT_PAREN: RIGHT_PAREN,
	}

	LEFT_PAREN_SET = OpSubset{
		CONST:      CONST,
		TYPE:       TYPE,
		VAR:        VAR,
		PROCEDURE:  PROCEDURE,
		FUNCTION:   FUNCTION,
		BEGIN:      BEGIN,
		LEFT_PAREN: LEFT_PAREN,
		SEMICOLON:  SEMICOLON,
		COLON:      COLON,
	}

	RIGHT_PAREN_SET = OpSubset{
		CONST:       CONST,
		TYPE:        TYPE,
		VAR:         VAR,
		PROCEDURE:   PROCEDURE,
		FUNCTION:    FUNCTION,
		BEGIN:       BEGIN,
		RIGHT_PAREN: RIGHT_PAREN,
		SEMICOLON:   SEMICOLON,
		COLON:       COLON,
	}

	PARAMETER_FOLLOW_SET = OpSubset{
		CONST:       CONST,
		TYPE:        TYPE,
		VAR:         VAR,
		PROCEDURE:   PROCEDURE,
		FUNCTION:    FUNCTION,
		BEGIN:       BEGIN,
		COLON:       COLON,
		RIGHT_PAREN: RIGHT_PAREN,
		SEMICOLON:   SEMICOLON,
	}

	COMMA_SET = OpSubset{
		CONST:       CONST,
		TYPE:        TYPE,
		VAR:         VAR,
		PROCEDURE:   PROCEDURE,
		FUNCTION:    FUNCTION,
		BEGIN:       BEGIN,
		COMMA:       COMMA,
		COLON:       COLON,
		IDENTIFIER:  IDENTIFIER,
		RIGHT_PAREN: RIGHT_PAREN,
		SEMICOLON:   SEMICOLON,
	}
)

func (parser *DeclaredRoutineParser) Parse(
	token *Token,
	parentId *intermediate.SymTabEntry,
) *intermediate.SymTabEntry {
	var (
		routineDef definition.Definition
		dummyName  string
		routineId  *intermediate.SymTabEntry
		tokenName  = token.GetName()
	)

	switch tokenName {
	case PROGRAM:
		token = parser.NextToken()
		routineDef = definition.PROGRAM
		dummyName = strings.ToLower("DummyProgramName")
	case PROCEDURE:
		token = parser.NextToken()
		routineDef = definition.PROCEDURE
		dummyName = strings.ToLower("DummyProcedureName_" + strconv.Itoa(parser.counter))
		parser.counter++
	case FUNCTION:
		token = parser.NextToken()
		routineDef = definition.FUNCTION
		dummyName = strings.ToLower("DummyFunctionName_" + strconv.Itoa(parser.counter))
		parser.counter++
	default:
		routineDef = definition.PROGRAM
		dummyName = strings.ToLower("DummyProgramName")
	}

	routineId = parser.ParseRoutineName(token, dummyName)
	routineId.SetDefinition(routineDef)

	token = parser.CurrentToken()

	iCode := intermediate.NewICodeImpl()
	routineId.SetAttribute("ROUTINE_ICODE", iCode)
	routineId.SetAttribute("ROUTINE_ROUTINES", []*intermediate.SymTabEntry{})

	if routineId.GetAttribute("ROUTINE_CODE") == routinecode.FORWARD {
		symTab := (routineId.GetAttribute("ROUTINE_SYMTAB")).(*intermediate.SymTab)
		parser.GetSymTabStack().Add(symTab)
	} else {
		routineId.SetAttribute("ROUTINE_SYMTAB", parser.GetSymTabStack().Push())
	}

	if routineDef == definition.PROGRAM {
		parser.GetSymTabStack().SetProgramId(routineId)
	} else if routineId.GetAttribute("ROUTINE_CODE") == routinecode.FORWARD {
		if token.GetName() != SEMICOLON {
			panic(errors.New("Already Forwarded"))
		}
	} else {
		parser.ParseHeader(token, routineId)
	}

	token = parser.CurrentToken()

	if token.GetName() == SEMICOLON {
		for ; token.GetName() == SEMICOLON; token = parser.NextToken() {
		}
	} else {
		panic(errors.New("Missing Semicolon"))
	}

	if token.GetName() == IDENTIFIER && strings.ToLower(token.GetText()) == "forward" {
		token = parser.NextToken()
		routineId.SetAttribute("ROUTINE_CODE", routinecode.FORWARD)
	} else {
		routineId.SetAttribute("ROUTINE_CODE", routinecode.DECLARED)
		blockParser := NewBlockParser(parser)
		rootNode := blockParser.Parse(token, routineId)
		iCode.SetRoot(rootNode)
	}

	parser.GetSymTabStack().Pop()

	return routineId
}

func (parser *DeclaredRoutineParser) ParseRoutineName(
	token *Token,
	dummyName string,
) *intermediate.SymTabEntry {
	var routineId *intermediate.SymTabEntry

	if token.GetName() == IDENTIFIER {
		routineName := strings.ToLower(token.GetText())
		routineId = parser.GetSymTabStack().LookUpLocal(routineName)

		if routineId == nil {
			routineId = parser.GetSymTabStack().EnterLocal(routineName)
		} else if routineId.GetAttribute("ROUTINE_CODE") != routinecode.FORWARD {
			routineId = nil
			panic(errors.New("Identifier Redefined"))
		}

		token = parser.NextToken()
	} else {
		panic(errors.New("Missing Identifier"))
	}

	if routineId == nil {
		routineId = parser.GetSymTabStack().EnterLocal(dummyName)
	}

	return routineId
}

func (parser *DeclaredRoutineParser) ParseHeader(
	token *Token,
	routineId *intermediate.SymTabEntry,
) {
	parser.ParseFormalParameters(token, routineId)
	token = parser.CurrentToken()

	if routineId.GetDefinition() == definition.FUNCTION {
		variableDeclarationsParser := NewVariableDeclarationsParser(parser)
		variableDeclarationsParser.SetDefinition(definition.FUNCTION)
		spec := variableDeclarationsParser.ParseTypeSpec(token)

		token = parser.CurrentToken()

		if spec != nil {
			form := spec.GetForm()
			if form == intermediate.ARRAY || form == intermediate.RECORD {
				panic(errors.New("Invalid Type"))
			}
		} else {
			spec = intermediate.UndefinedType
		}

		routineId.SetTypeSpec(spec)
		token = parser.CurrentToken()
	}
}

func (parser *DeclaredRoutineParser) ParseFormalParameters(
	token *Token,
	routineId *intermediate.SymTabEntry,
) {
	token = parser.Synchronize(LEFT_PAREN_SET)

	if token.GetName() == LEFT_PAREN {
		token = parser.NextToken()
		params := []*intermediate.SymTabEntry{}
		token = parser.Synchronize(PARAMETER_SET)

		for name := token.GetName(); name == IDENTIFIER || name == VAR; name = token.GetName() {
			params = append(params, parser.ParseParmSublist(token, routineId)...)
			token = parser.CurrentToken()
		}

		if token.GetName() == RIGHT_PAREN {
			token = parser.NextToken()
		} else {
			panic(errors.New("Missing Right Paren"))
		}

		routineId.SetAttribute("ROUTINE_PARMS", params)
	}
}

func (parser *DeclaredRoutineParser) ParseParmSublist(
	token *Token,
	routineId *intermediate.SymTabEntry,
) []*intermediate.SymTabEntry {

	var (
		isProgram = routineId.GetDefinition() == definition.PROGRAM
		parmDefn  definition.Definition
		name      = token.GetName()
	)

	if isProgram {
		parmDefn = definition.PROGRAM_PARM
	}

	if name == VAR {
		if !isProgram {
			parmDefn = definition.VAR_PARM
		} else {
			panic(errors.New("Invalid Var Parameter"))
		}

		token = parser.NextToken()
	} else if !isProgram {
		parmDefn = definition.VALUE_PARM
	}

	variableDeclarationsParser := NewVariableDeclarationsParser(parser)
	variableDeclarationsParser.SetDefinition(parmDefn)
	sublist := variableDeclarationsParser.ParseIdentifierSublist(
		token,
		PARAMETER_FOLLOW_SET,
		COMMA_SET,
	)

	token = parser.CurrentToken()
	name = token.GetName()

	if !isProgram {
		if name == SEMICOLON {
			for token.GetName() == SEMICOLON {
				token = parser.NextToken()
			}
		} else if NEXT_START_SET.Contains(name) {
			panic(errors.New("Missing Semicolon"))
		}

		token = parser.Synchronize(PARAMETER_SET)
	}

	return sublist
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

type BlockParser struct {
	IParser
}

func NewBlockParser(parent IParser) *BlockParser {
	return &BlockParser{
		parent,
	}
}

func (parser *BlockParser) Parse(token *Token, routineId *intermediate.SymTabEntry) intermediate.ICodeNode {
	declarationsParser := NewDeclarationsParser(parser)
	statementParser := NewStatementParser(parser)
	declarationsParser.Parse(token, routineId)
	token = parser.Synchronize(STMT_START_SET)

	name := token.GetName()
	var rootNode intermediate.ICodeNode

	if name == BEGIN {
		rootNode = statementParser.Parse(token)
	} else {
		panic(errors.New("Missing Begin"))

		if STMT_START_SET.Contains(name) {
			rootNode = intermediate.NewICodeNodeImpl(intermediate.COMPOUND)
			statementParser.ParseList(token, rootNode, END)
		}
	}

	return rootNode
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

var (
	COLON_SET = OpSubset{
		COLON:     COLON,
		SEMICOLON: SEMICOLON,
	}
	IDENTIFIER_START_SET = OpSubset{
		IDENTIFIER: IDENTIFIER,
		COMMA:      COMMA,
	}
	IDENTIFIER_FOLLOW_SET = OpSubset{
		COLON:     COLON,
		SEMICOLON: SEMICOLON,
		VAR:       VAR,
		PROCEDURE: PROCEDURE,
		FUNCTION:  FUNCTION,
		BEGIN:     BEGIN,
	}
	COMMA_SET_VDP = OpSubset{
		COMMA:      COMMA,
		COLON:      COLON,
		IDENTIFIER: IDENTIFIER,
		SEMICOLON:  SEMICOLON,
	}
	NEXT_START_SET_VDP = OpSubset{
		IDENTIFIER: IDENTIFIER,
		SEMICOLON:  SEMICOLON,
		PROCEDURE:  PROCEDURE,
		FUNCTION:   FUNCTION,
		BEGIN:      BEGIN,
	}
)

func (parser *VariableDeclarationsParser) SetDefinition(def definition.Definition) {
	parser.def = def
}

func (parser *VariableDeclarationsParser) ParseTypeSpec(token *Token) *intermediate.TypeSpec {
	token = parser.Synchronize(COLON_SET)

	if token.GetName() == COLON {
		token = parser.NextToken()
	} else {
		panic(errors.New("Missing Colon"))
	}

	typeSpecificationParser := NewTypeSpecificationParser(parser)
	spec := typeSpecificationParser.Parse(token)

	if parser.def != definition.VARIABLE && parser.def != definition.FIELD && spec != nil && spec.GetIdentifier() == nil {
		panic(errors.New("Invalid Type"))
	}

	return spec
}

func (parser *VariableDeclarationsParser) ParseIdentifierSublist(
	token *Token,
	followSet,
	commaSet OpSubset,
) []*intermediate.SymTabEntry {
	sublist := []*intermediate.SymTabEntry{}
	token = parser.Synchronize(IDENTIFIER_START_SET)
	do := true

	for do || !followSet.Contains(token.GetName()) {
		do = false
		id := parser.ParseIdentifier(token)
		if id != nil {
			sublist = append(sublist, id)
		}

		token = parser.Synchronize(commaSet)
		if token.GetName() == COMMA {
			token = parser.NextToken()
			if followSet.Contains(token.GetName()) {
				panic(errors.New("Missing Identifier"))
			}
		} else if IDENTIFIER_START_SET.Contains(token.GetName()) {
			panic(errors.New("Missing Comma"))
		}
	}

	if parser.def != definition.PROGRAM_PARM {
		spec := parser.ParseTypeSpec(token)
		for _, v := range sublist {
			v.SetTypeSpec(spec)
		}
	}
	return sublist
}

func (parser *VariableDeclarationsParser) ParseIdentifier(token *Token) *intermediate.SymTabEntry {
	var id *intermediate.SymTabEntry

	if token.GetName() == IDENTIFIER {
		text := strings.ToLower(token.GetText())
		id = parser.GetSymTabStack().LookUpLocal(text)

		if id == nil {
			id = parser.GetSymTabStack().EnterLocal(text)
			id.SetDefinition(parser.def)
			id.AppendLineNum(token.GetLineNum())
		} else {
			panic(errors.New("Identifier Redefined"))
		}

		token = parser.NextToken()
	} else {
		panic(errors.New("Missing Identifier"))
	}

	return id
}

type StatementParser struct {
	IParser
}

func NewStatementParser(parent IParser) *StatementParser {
	return &StatementParser{
		parent,
	}
}

var STMT_START_SET = OpSubset{
	BEGIN:      BEGIN,
	CASE:       CASE,
	FOR:        FOR,
	IF:         IF,
	REPEAT:     REPEAT,
	WHILE:      WHILE,
	IDENTIFIER: IDENTIFIER,
	SEMICOLON:  SEMICOLON,
}

var STMT_FOLLOW_SET = OpSubset{
	SEMICOLON: SEMICOLON,
	END:       END,
	ELSE:      ELSE,
	UNTIL:     UNTIL,
	DOT:       DOT,
}

func (parser *StatementParser) Parse(token *Token) intermediate.ICodeNode {
	var statementNode intermediate.ICodeNode
	switch token.GetName() {
	case BEGIN:
		statementNode = NewCompoundStatementParser(parser).Parse(token)
	case IDENTIFIER:
		var idDef definition.Definition
		text := strings.ToLower(token.GetText())
		id := parser.GetSymTabStack().LookUpLocal(text)

		if id != nil {
			idDef = id.GetDefinition()
		} else {
			idDef = definition.UNDEFINED
		}

		switch idDef {
		case definition.VARIABLE:
			statementNode = NewAssignmentStatementParser(parser).Parse(token)
		case definition.VALUE_PARM:
			statementNode = NewAssignmentStatementParser(parser).Parse(token)
		case definition.VAR_PARM:
			statementNode = NewAssignmentStatementParser(parser).Parse(token)
		case definition.UNDEFINED:
			statementNode = NewAssignmentStatementParser(parser).Parse(token)
		case definition.FUNCTION:
			statementNode = NewAssignmentStatementParser(parser).ParseFunctionNameAssignment(token)
		case definition.PROCEDURE:
			statementNode = NewCallParser(parser).Parse(token)
		default:
			panic(errors.New("Unexpected Token"))
		}
	case REPEAT:
		statementNode = NewRepeatStatementParser(parser).Parse(token)
	case WHILE:
		statementNode = NewWhileStatementParser(parser).Parse(token)
	case FOR:
		statementNode = NewForStatementParser(parser).Parse(token)
	case IF:
		statementNode = NewIfStatementParser(parser).Parse(token)
	case CASE:
		statementNode = NewCaseStatementParser(parser).Parse(token)
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
	terminatorSet := STMT_START_SET.
		Copy().
		Add(terminator)

	for token.Type != EOFToken && token.GetName() != terminator {
		statementNode := parser.Parse(token)
		parentNode.AddChild(statementNode)

		token = parser.CurrentToken()
		tokenName := token.GetName()
		if tokenName == SEMICOLON {
			token = parser.NextToken()
		} else if STMT_START_SET.Contains(tokenName) {
			panic(errors.New("Mission Semicolon"))
		}
		token = parser.Synchronize(terminatorSet)
	}

	if token.GetName() == terminator {
		token = parser.NextToken()
		return
	}

	panic("Unknown Token")
	return
}

type CallParser struct {
	IStatementParser
}

func NewCallParser(parent IStatementParser) *CallParser {
	return &CallParser{
		parent,
	}
}

type CallStandardParser struct {
	IStatementParser
}

func NewCallStandardParser(parent IStatementParser) *CallStandardParser {
	return &CallStandardParser{
		parent,
	}
}

type IfStatementParser struct {
	IStatementParser
}

func NewIfStatementParser(parent IStatementParser) *IfStatementParser {
	return &IfStatementParser{
		parent,
	}
}

type WhileStatementParser struct {
	IStatementParser
}

func NewWhileStatementParser(parent IStatementParser) *WhileStatementParser {
	return &WhileStatementParser{
		parent,
	}
}

type RepeatStatementParser struct {
	IStatementParser
}

func NewRepeatStatementParser(parent IStatementParser) *RepeatStatementParser {
	return &RepeatStatementParser{
		parent,
	}
}

type ForStatementParser struct {
	IStatementParser
}

func NewForStatementParser(parent IStatementParser) *ForStatementParser {
	return &ForStatementParser{
		parent,
	}
}

type CaseStatementParser struct {
	IStatementParser
}

func NewCaseStatementParser(parent IStatementParser) *CaseStatementParser {
	return &CaseStatementParser{
		parent,
	}
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
	isFunctionTarget bool
}

func NewAssignmentStatementParser(parent IStatementParser) *AssignmentStatementParser {
	return &AssignmentStatementParser{
		parent,
		false,
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

func (parser *AssignmentStatementParser) ParseFunctionNameAssignment(token *Token) intermediate.ICodeNode {
	parser.isFunctionTarget = true
	return parser.Parse(token)
}
