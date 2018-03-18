package intermediate

type TypeKey int

const (
	ENUMERATION_CONSTANTS TypeKey = iota
	SUBRANGE_BASE_TYPE
	SUBRANGE_MIN_VALUE
	SUBRANGE_MAX_VALUE
	ARRAY_INDEX_TYPE
	ARRAY_ELEMENT_TYPE
	ARRAY_ELEMENT_COUNT
	RECORD_SYMTAB
)

type TypeForm string

const (
	SCALAR      TypeForm = "SCALAR"
	ENUMERATION TypeForm = "ENUMERATION"
	SUBRANGE    TypeForm = "SUBRANGE"
	ARRAY       TypeForm = "ARRAY"
	RECORD      TypeForm = "RECORD"
)

type TypeSpec struct {
	maps       map[TypeKey]interface{}
	form       TypeForm
	identifier *SymTabEntry
}

func NewTypeSpecImpl(form TypeForm) *TypeSpec {
	return &TypeSpec{
		maps: map[TypeKey]interface{}{},
		form: form,
	}
}

func NewTypeSpecImplFromString(val string) *TypeSpec {

	this := &TypeSpec{
		maps: map[TypeKey]interface{}{},
		form: ARRAY,
	}

	indexType := NewTypeSpecImpl(SUBRANGE)
	indexType.SetAttribute(SUBRANGE_BASE_TYPE, IntegerType)
	indexType.SetAttribute(SUBRANGE_MIN_VALUE, 1)
	indexType.SetAttribute(SUBRANGE_MAX_VALUE, len(val))

	this.SetAttribute(ARRAY_INDEX_TYPE, indexType)
	this.SetAttribute(ARRAY_ELEMENT_TYPE, CharType)
	this.SetAttribute(ARRAY_ELEMENT_COUNT, len(val))

	return this
}

func (spec *TypeSpec) GetForm() TypeForm {
	return spec.form
}

func (spec *TypeSpec) SetIdentifier(idf *SymTabEntry) {
	spec.identifier = idf
}

func (spec *TypeSpec) GetIdentifier() *SymTabEntry {
	return spec.identifier
}

func (spec *TypeSpec) SetAttribute(key TypeKey, obj interface{}) {
	spec.maps[key] = obj
}

func (spec *TypeSpec) GetAttribute(key TypeKey) interface{} {
	return spec.maps[key]
}

func (spec *TypeSpec) IsPascalString() bool {
	if spec.form == ARRAY {
		elmtType := spec.GetAttribute(ARRAY_ELEMENT_TYPE).(*TypeSpec)
		indexType := spec.GetAttribute(ARRAY_INDEX_TYPE).(*TypeSpec)

		return elmtType.BaseType() == CharType && indexType.BaseType() == indexType
	}
	return false
}

func (spec *TypeSpec) BaseType() *TypeSpec {
	if spec.form == SUBRANGE {
		return spec.GetAttribute(SUBRANGE_BASE_TYPE).(*TypeSpec)
	}
	return spec
}
