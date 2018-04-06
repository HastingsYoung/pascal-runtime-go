package typechecker

import (
	"github.com/pascal-runtime-go/intermediate"
)

func IsInteger(spec *intermediate.TypeSpec) bool {
	return spec != nil && spec.BaseType() == intermediate.IntegerType
}

func AreBothInteger(spec1, spec2 *intermediate.TypeSpec) bool {
	return IsInteger(spec1) && IsInteger(spec2)
}

func IsReal(spec *intermediate.TypeSpec) bool {
	return spec != nil && spec.BaseType() == intermediate.RealType
}

func IsIntegerOrReal(spec *intermediate.TypeSpec) bool {
	return IsInteger(spec) || IsReal(spec)
}

func IsAtLeastOneReal(spec1, spec2 *intermediate.TypeSpec) bool {
	return IsReal(spec1) && IsReal(spec2) ||
		IsReal(spec1) && IsInteger(spec2) ||
		IsInteger(spec1) && IsReal(spec2)
}

func IsBoolean(spec *intermediate.TypeSpec) bool {
	return spec != nil && spec.BaseType() == intermediate.BooleanType
}

func AreBothBoolean(spec1, spec2 *intermediate.TypeSpec) bool {
	return IsBoolean(spec1) && IsBoolean(spec2)
}

func IsChar(spec *intermediate.TypeSpec) bool {
	return spec != nil && spec.BaseType() == intermediate.CharType
}

func AreAssignmentCompatible(targetSpec, valueSpec *intermediate.TypeSpec) bool {
	if targetSpec == nil || valueSpec == nil {
		return false
	}

	targetSpec = targetSpec.BaseType()
	valueSpec = valueSpec.BaseType()

	var compatible bool = false
	if targetSpec == valueSpec {
		compatible = true
	} else if IsReal(targetSpec) && IsInteger(valueSpec) {
		compatible = true
	} else {
		compatible = targetSpec.IsPascalString() && valueSpec.IsPascalString()
	}

	return compatible
}

func AreComparisonCompatible(spec1, spec2 *intermediate.TypeSpec) bool {
	if spec1 == nil || spec2 == nil {
		return false
	}

	spec1 = spec1.BaseType()
	spec2 = spec2.BaseType()

	form := spec1.GetForm()
	var compatible bool = false
	if spec1 == spec2 &&
		(form == intermediate.SCALAR || form == intermediate.ENUMERATION) {
		compatible = true
	} else if IsAtLeastOneReal(spec1, spec2) {
		compatible = true
	} else {
		compatible = spec1.IsPascalString() && spec2.IsPascalString()
	}

	return compatible
}
