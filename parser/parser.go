package parser

import (
	"fmt"
	. "github.com/lexer-example/intermediate"
	. "github.com/lexer-example/scanner"
	. "github.com/lexer-example/token"
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
		fmt.Println("TOKEN:", *token)
	}
	tk := *parser.CurrentToken()
	fmt.Println(tk.Value, tk.Type, tk.Name)

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
