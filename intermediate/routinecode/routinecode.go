package routinecode

type RoutineCode int

const (
	DECLARED RoutineCode = iota
	FORWARD
	READ
	READLN
	WRITE
	WRITELN
	ABS
	ARCTAN
	CHR
	COS
	EOF
	EOLN
	EXP
	LN
	ODD
	ORD
	PRED
	ROUND
	SIN
	SQR
	SQRT
	SUCC
	TRUNC
)

var ROUTINE_CODES = map[RoutineCode]string{
	DECLARED: "DECLARED",
	FORWARD:  "FORWARD",
	READ:     "READ",
	READLN:   "READLN",
	WRITE:    "WRITE",
	WRITELN:  "WRITELN",
	ABS:      "ABS",
	ARCTAN:   "ARCTAN",
	CHR:      "CHR",
	COS:      "COS",
	EOF:      "EOF",
	EOLN:     "EOLN",
	EXP:      "EXP",
	LN:       "LN",
	ODD:      "ODD",
	ORD:      "ORD",
	PRED:     "PRED",
	ROUND:    "ROUND",
	SIN:      "SIN",
	SQR:      "SQR",
	SQRT:     "SQRT",
	SUCC:     "SUCC",
	TRUNC:    "TRUNC",
}
