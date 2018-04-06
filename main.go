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
	parser IParser
	source *Source
	iCode  ICode
	stack  *SymTabStack
	// executor Executor
}

func NewPascal(src io.Reader) *Pascal {
	source := NewSource(src)
	parser := NewPascalParser(NewPascalScanner(source))

	parser.Parse()

	return &Pascal{
		parser: parser,
		source: source,
		iCode:  parser.GetICode(),
		stack:  parser.GetSymTabStack(),
	}
}

func main() {
	NewPascal(strings.NewReader(
		`PROGRAM HelloWorld;

		VAR       
		    Num1, Num2, Sum : Integer;

		BEGIN
			writeln('Hello World. Prepare to learn PASCAL!!');
		END.`))
}
