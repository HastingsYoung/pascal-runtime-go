package parser

import (
	. "github.com/pascal-runtime-go/intermediate"
	"github.com/pascal-runtime-go/message"
	. "github.com/pascal-runtime-go/scanner"
	. "github.com/pascal-runtime-go/token"
)

type Parser interface {
	Scanner() Scanner
	ICode() ICode
	SymTab() *SymTab
	Parse() error
	GetErrorCount() int
	CurrentToken() *Token
	NextToken() *Token
}

type PascalParser struct {
	scanner Scanner
	iCode   ICode
	symTab  *SymTab
}

func NewPascalParser(scanner Scanner) *PascalParser {
	return &PascalParser{
		scanner: scanner,
	}
}

func (parser *PascalParser) Scanner() Scanner {
	return parser.scanner
}

func (parser *PascalParser) ICode() ICode {
	return parser.iCode
}

func (parser *PascalParser) SymTab() *SymTab {
	return parser.symTab
}

func (parser *PascalParser) Parse() error {
	for token := parser.NextToken(); token.Type != EOFToken && token.Type != ErrorToken; token = parser.NextToken() {
		message.Log("Token:", *token)
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
