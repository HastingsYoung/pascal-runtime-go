package token

type TokenName int

type TokenNamesTable map[string]TokenName

func (table *TokenNamesTable) Contains(name string) bool {
	if _, ok := (*table)[name]; ok {
		return true
	}
	return false
}

func (table *TokenNamesTable) GetName(key string) TokenName {
	if table.Contains(key) {
		return (*table)[key]
	}
	return ERROR
}

const (
	// Reserved words.
	AND TokenName = iota
	ARRAY
	BEGIN
	CASE
	CONST
	DIV
	DO
	DOWNTO
	ELSE
	END
	FILE
	FOR
	FUNCTION
	GOTO
	IF
	IN
	LABEL
	MOD
	NIL
	NOT
	OF
	OR
	PACKED
	PROCEDURE
	PROGRAM
	RECORD
	REPEAT
	SET
	THEN
	TO
	TYPE
	UNTIL
	VAR
	WHILE
	WITH

	// Special symbols
	PLUS
	MINUS
	STAR
	SLASH
	COLON_EQUALS
	DOT
	COMMA
	SEMICOLON
	COLON
	QUOTE
	EQUALS
	NOT_EQUALS
	LESS_THAN
	LESS_EQUALS
	GREATER_EQUALS
	GREATER_THAN
	LEFT_PAREN
	RIGHT_PAREN
	LEFT_BRACKET
	RIGHT_BRACKET
	LEFT_BRACE
	RIGHT_BRACE
	UP_ARROW
	DOT_DOT

	// Others
	IDENTIFIER
	INTEGER
	REAL
	STRING
	ERROR
	END_OF_FILE

	// Routine reserved
	WRITELN
	WRITE
	READLN
	READ
)

var TOKEN_NAMES = map[TokenName]string{
	// Reserved words.
	AND:       "AND",
	ARRAY:     "ARRAY",
	BEGIN:     "BEGIN",
	CASE:      "CASE",
	CONST:     "CONST",
	DIV:       "DIV",
	DO:        "DO",
	DOWNTO:    "DOWNTO",
	ELSE:      "ELSE",
	END:       "END",
	FILE:      "FILE",
	FOR:       "FOR",
	FUNCTION:  "FUNCTION",
	GOTO:      "GOTO",
	IF:        "IF",
	IN:        "IN",
	LABEL:     "LABEL",
	MOD:       "MOD",
	NIL:       "NIL",
	NOT:       "NOT",
	OF:        "OF",
	OR:        "OR",
	PACKED:    "PACKED",
	PROCEDURE: "PROCEDURE",
	PROGRAM:   "PROGRAM",
	RECORD:    "RECORD",
	REPEAT:    "REPEAT",
	SET:       "SET",
	THEN:      "THEN",
	TO:        "TO",
	TYPE:      "TYPE",
	UNTIL:     "UNTIL",
	VAR:       "VAR",
	WHILE:     "WHILE",
	WITH:      "WITH",

	// Special symbols
	PLUS:           "+",
	MINUS:          "-",
	STAR:           "*",
	SLASH:          "/",
	COLON_EQUALS:   ":=",
	DOT:            ".",
	COMMA:          ",",
	SEMICOLON:      ";",
	COLON:          ":",
	QUOTE:          "'",
	EQUALS:         "=",
	NOT_EQUALS:     "<>",
	LESS_THAN:      "<",
	LESS_EQUALS:    "<=",
	GREATER_EQUALS: ">=",
	GREATER_THAN:   ">",
	LEFT_PAREN:     "(",
	RIGHT_PAREN:    ")",
	LEFT_BRACKET:   "[",
	RIGHT_BRACKET:  "]",
	LEFT_BRACE:     "{",
	RIGHT_BRACE:    "}",
	UP_ARROW:       "^",
	DOT_DOT:        "..",

	// Others
	IDENTIFIER:  "IDENTIFIER",
	INTEGER:     "INTEGER",
	REAL:        "REAL",
	STRING:      "STRING",
	ERROR:       "ERROR",
	END_OF_FILE: "END_OF_FILE",
}

var TOKEN_NAMES_RESERVED = TokenNamesTable{
	"AND":       AND,
	"ARRAY":     ARRAY,
	"BEGIN":     BEGIN,
	"CASE":      CASE,
	"CONST":     CONST,
	"DIV":       DIV,
	"DO":        DO,
	"DOWNTO":    DOWNTO,
	"ELSE":      ELSE,
	"END":       END,
	"FILE":      FILE,
	"FOR":       FOR,
	"FUNCTION":  FUNCTION,
	"GOTO":      GOTO,
	"IF":        IF,
	"IN":        IN,
	"LABEL":     LABEL,
	"MOD":       MOD,
	"NIL":       NIL,
	"NOT":       NOT,
	"OF":        OF,
	"OR":        OR,
	"PACKED":    PACKED,
	"PROCEDURE": PROCEDURE,
	"PROGRAM":   PROGRAM,
	"RECORD":    RECORD,
	"REPEAT":    REPEAT,
	"SET":       SET,
	"THEN":      THEN,
	"TO":        TO,
	"TYPE":      TYPE,
	"UNTIL":     UNTIL,
	"VAR":       VAR,
	"WHILE":     WHILE,
	"WITH":      WITH,
}

var TOKEN_NAMES_SPECIAL_SYMBOLS = TokenNamesTable{
	"+":  PLUS,
	"-":  MINUS,
	"*":  STAR,
	"/":  SLASH,
	":=": COLON_EQUALS,
	".":  DOT,
	",":  COMMA,
	";":  SEMICOLON,
	":":  COLON,
	"'":  QUOTE,
	"=":  EQUALS,
	"<>": NOT_EQUALS,
	"<":  LESS_THAN,
	"<=": LESS_EQUALS,
	">=": GREATER_EQUALS,
	">":  GREATER_THAN,
	"(":  LEFT_PAREN,
	")":  RIGHT_PAREN,
	"[":  LEFT_BRACKET,
	"]":  RIGHT_BRACKET,
	"{":  LEFT_BRACE,
	"}":  RIGHT_BRACE,
	"^":  UP_ARROW,
	"..": DOT_DOT,
}

var TOKEN_NAMES_OTHERS = TokenNamesTable{
	"IDENTIFIER":  IDENTIFIER,
	"INTEGER":     INTEGER,
	"REAL":        REAL,
	"STRING":      STRING,
	"ERROR":       ERROR,
	"END_OF_FILE": END_OF_FILE,
}

var ROUTINE_KEYWORDS = TokenNamesTable{
	// Routine reserved
	"WRITELN": WRITELN,
	"WRITE":   WRITE,
	"READLN":  READLN,
	"READ":    READ,
}
