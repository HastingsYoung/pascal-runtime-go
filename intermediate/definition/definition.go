package definition

type Definition string

const (
	CONSTANT             Definition = "CONSTANT"
	ENUMERATION_CONSTANT Definition = "ENUMERATION_CONSTANT"
	TYPE                 Definition = "TYPE"
	VARIABLE             Definition = "VARIABLE"
	FIELD                Definition = "FIELD"
	VALUE_PARM           Definition = "VALUE_PARM"
	VAR_PARM             Definition = "VAR_PARM"
	PROGRAM_PARM         Definition = "PROGRAM_PARM"
	PROGRAM              Definition = "PROGRAM"
	PROCEDURE            Definition = "PROCEDURE"
	FUNCTION             Definition = "FUNCTION"
	UNDEFINED            Definition = "UNDEFINED"
)
