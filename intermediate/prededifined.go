package intermediate

import (
	"github.com/pascal-runtime-go/intermediate/definition"
	"github.com/pascal-runtime-go/intermediate/routinecode"
)

var (
	// predefined types
	IntegerType,
	RealType,
	BooleanType,
	CharType,
	UndefinedType *TypeSpec

	// predefined identifiers
	IntegerId,
	RealId,
	BooleanId,
	CharId,
	FalseId,
	TrueId,
	ReadId,
	ReadlnId,
	WriteId,
	WritelnId,
	AbsId,
	ArctanId,
	ChrId,
	CosId,
	EofId,
	EolnId,
	ExpId,
	LnId,
	OddId,
	OrdId,
	PredId,
	RoundId,
	SinId,
	SqrId,
	SqrtId,
	SuccId,
	TruncId *SymTabEntry
)

func Initialize(stack *SymTabStack) {
	initializeTypes(stack)
	initializeConstants(stack)
	initializeStandardRoutines(stack)
}

func initializeTypes(stack *SymTabStack) {
	IntegerId = stack.EnterLocal("integer")
	IntegerType = NewTypeSpecImpl(SCALAR)
	IntegerType.SetIdentifier(IntegerId)
	IntegerId.SetDefinition(definition.TYPE)
	IntegerId.SetTypeSpec(IntegerType)

	RealId = stack.EnterLocal("real")
	RealType = NewTypeSpecImpl(SCALAR)
	RealType.SetIdentifier(RealId)
	RealId.SetDefinition(definition.TYPE)
	RealId.SetTypeSpec(RealType)

	BooleanId = stack.EnterLocal("boolean")
	BooleanType = NewTypeSpecImpl(SCALAR)
	BooleanType.SetIdentifier(BooleanId)
	BooleanId.SetDefinition(definition.TYPE)
	BooleanId.SetTypeSpec(BooleanType)

	CharId = stack.EnterLocal("char")
	CharType = NewTypeSpecImpl(SCALAR)
	CharType.SetIdentifier(CharId)
	CharId.SetDefinition(definition.TYPE)
	CharId.SetTypeSpec(CharType)

	UndefinedType = NewTypeSpecImpl(SCALAR)
}

func initializeConstants(stack *SymTabStack) {
	FalseId = stack.EnterLocal("false")
	FalseId.SetDefinition(definition.ENUMERATION_CONSTANT)
	FalseId.SetTypeSpec(BooleanType)
	FalseId.SetAttribute("CONSTANT_VALUE", 0)

	TrueId = stack.EnterLocal("true")
	TrueId.SetDefinition(definition.ENUMERATION_CONSTANT)
	TrueId.SetTypeSpec(BooleanType)
	TrueId.SetAttribute("CONSTANT_VALUE", 1)

	constants := []*SymTabEntry{FalseId, TrueId}
	BooleanType.SetAttribute(ENUMERATION_CONSTANTS, constants)
}

func initializeStandardRoutines(stack *SymTabStack) {
	ReadId = enterStandard(stack, definition.PROCEDURE, "read", routinecode.READ)
	ReadlnId = enterStandard(stack, definition.PROCEDURE, "readln", routinecode.READLN)
	WriteId = enterStandard(stack, definition.PROCEDURE, "write", routinecode.WRITE)
	WritelnId = enterStandard(stack, definition.PROCEDURE, "writeln", routinecode.WRITELN)

	AbsId = enterStandard(stack, definition.FUNCTION, "abs", routinecode.ABS)
	ArctanId = enterStandard(stack, definition.FUNCTION, "arctan", routinecode.ARCTAN)
	ChrId = enterStandard(stack, definition.FUNCTION, "chr", routinecode.CHR)
	CosId = enterStandard(stack, definition.FUNCTION, "cos", routinecode.COS)
	EofId = enterStandard(stack, definition.FUNCTION, "eof", routinecode.EOF)
	EolnId = enterStandard(stack, definition.FUNCTION, "eoln", routinecode.EOLN)
	ExpId = enterStandard(stack, definition.FUNCTION, "exp", routinecode.EXP)
	LnId = enterStandard(stack, definition.FUNCTION, "ln", routinecode.LN)
	OddId = enterStandard(stack, definition.FUNCTION, "odd", routinecode.ODD)
	OrdId = enterStandard(stack, definition.FUNCTION, "ord", routinecode.ORD)
	PredId = enterStandard(stack, definition.FUNCTION, "pred", routinecode.PRED)
	RoundId = enterStandard(stack, definition.FUNCTION, "round", routinecode.ROUND)
	SinId = enterStandard(stack, definition.FUNCTION, "sin", routinecode.SIN)
	SqrId = enterStandard(stack, definition.FUNCTION, "sqr", routinecode.SQR)
	SqrtId = enterStandard(stack, definition.FUNCTION, "sqrt", routinecode.SQRT)
	SuccId = enterStandard(stack, definition.FUNCTION, "succ", routinecode.SUCC)
	TruncId = enterStandard(stack, definition.FUNCTION, "trunc", routinecode.TRUNC)
}

func enterStandard(
	stack *SymTabStack,
	defn definition.Definition,
	name string,
	routineCode routinecode.RoutineCode,
) *SymTabEntry {
	entry := stack.EnterLocal(name)
	entry.SetDefinition(defn)
	entry.SetAttribute("ROUTINE_CODE", routineCode)
	return entry
}
