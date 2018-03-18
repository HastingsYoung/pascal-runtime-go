package intermediate

import (
	. "github.com/pascal-runtime-go/intermediate/definition"
	"github.com/pascal-runtime-go/intermediate/routinecode"
)

var (
	// predefined types
	IntegerType,
	RealType,
	BooleanType,
	CharType,
	UndefinedType TypeSpec

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
	IntegerId.SetDefinition(TYPE)
	IntegerId.SetTypeSpec(IntegerType)

	RealId = stack.EnterLocal("real")
	RealType = NewTypeSpecImpl(SCALAR)
	RealType.SetIdentifier(RealId)
	RealId.SetDefinition(TYPE)
	RealId.SetTypeSpec(RealType)

	BooleanId = stack.EnterLocal("boolean")
	BooleanType = NewTypeSpecImpl(SCALAR)
	BooleanType.SetIdentifier(BooleanId)
	BooleanId.SetDefinition(TYPE)
	BooleanId.SetTypeSpec(BooleanType)

	CharId = stack.EnterLocal("char")
	CharType = NewTypeSpecImpl(SCALAR)
	CharType.SetIdentifier(CharId)
	CharId.SetDefinition(TYPE)
	CharId.SetTypeSpec(CharType)

	UndefinedType = NewTypeSpecImpl(SCALAR)
}

func initializeConstants(stack *SymTabStack) {
	FalseId = stack.EnterLocal("false")
	FalseId.SetDefinition(ENUMERATION_CONSTANT)
	FalseId.SetTypeSpec(BooleanType)
	FalseId.SetAttribute(CONSTANT_VALUE, 0)

	TrueId = stack.EnterLocal("true")
	TrueId.SetDefinition(ENUMERATION_CONSTANT)
	TrueId.SetTypeSpec(BooleanType)
	TrueId.SetAttribute(CONSTANT_VALUE, 1)

	constants := []*SymTabEntry{FalseId, TrueId}
	BooleanType.SetAttribute(ENUMERATION_CONSTANTS, constants)
}

func initializeStandardRoutines(stack *SymTabStack) {
	ReadId = enterStandard(stack, PROCEDURE, "read", routinecode.READ)
	ReadlnId = enterStandard(stack, PROCEDURE, "readln", routinecode.READLN)
	WriteId = enterStandard(stack, PROCEDURE, "write", routinecode.WRITE)
	WritelnId = enterStandard(stack, PROCEDURE, "writeln", routinecode.WRITELN)

	AbsId = enterStandard(stack, FUNCTION, "abs", routinecode.ABS)
	ArctanId = enterStandard(stack, FUNCTION, "arctan", routinecode.ARCTAN)
	ChrId = enterStandard(stack, FUNCTION, "chr", routinecode.CHR)
	CosId = enterStandard(stack, FUNCTION, "cos", routinecode.COS)
	EofId = enterStandard(stack, FUNCTION, "eof", routinecode.EOF)
	EolnId = enterStandard(stack, FUNCTION, "eoln", routinecode.EOLN)
	ExpId = enterStandard(stack, FUNCTION, "exp", routinecode.EXP)
	LnId = enterStandard(stack, FUNCTION, "ln", routinecode.LN)
	OddId = enterStandard(stack, FUNCTION, "odd", routinecode.ODD)
	OrdId = enterStandard(stack, FUNCTION, "ord", routinecode.ORD)
	PredId = enterStandard(stack, FUNCTION, "pred", routinecode.PRED)
	RoundId = enterStandard(stack, FUNCTION, "round", routinecode.ROUND)
	SinId = enterStandard(stack, FUNCTION, "sin", routinecode.SIN)
	SqrId = enterStandard(stack, FUNCTION, "sqr", routinecode.SQR)
	SqrtId = enterStandard(stack, FUNCTION, "sqrt", routinecode.SQRT)
	SuccId = enterStandard(stack, FUNCTION, "succ", routinecode.SUCC)
	TruncId = enterStandard(stack, FUNCTION, "trunc", routinecode.TRUNC)
}

func enterStandard(
	stack *SymTabStack,
	defn Definition,
	name string,
	routineCode routinecode.RoutineCode,
) *SymTabEntry {
	entry := stack.EnterLocal(name)
	entry.SetDefinition(defn)
	entry.SetAttribute(ROUTINE_CODE, routineCode)
	return entry
}
