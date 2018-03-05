package main

import (
	"io"
	// . "github.com/lexer-example/executor"
	. "github.com/pascal-runtime-go/intermediate"
	. "github.com/pascal-runtime-go/parser"
	. "github.com/pascal-runtime-go/scanner"
	. "github.com/pascal-runtime-go/source"
	"strings"
)

type Pascal struct {
	parser Parser
	source *Source
	iCode  ICode
	symTab *SymTab
	// executor Executor
}

func NewPascal(src io.Reader) *Pascal {
	source := NewSource(src)
	parser := NewPascalParser(NewPascalScanner(source))

	parser.Parse()

	return &Pascal{
		parser: parser,
		source: source,
		iCode:  parser.ICode(),
		symTab: parser.SymTab(),
	}
}

func main() {
	NewPascal(strings.NewReader(
		`PROGRAM Hello;

		VAR       
		    Num1, Num2, Sum : Integer;

		BEGIN
			Write('Hello World. Prepare to learn PASCAL!!');
		END`))
}
