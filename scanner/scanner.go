package scanner

import (
	. "github.com/lexer-example/source"
	. "github.com/lexer-example/token"
)

type Scanner interface {
	CurrentToken() *Token
	NextToken() *Token
	ExtractToken() *Token
	CurrentChar() (rune, error)
	NextChar() (rune, error)
}

type PascalScanner struct {
	source *Source
	token  *Token
}

func NewPascalScanner(source *Source) *PascalScanner {
	return &PascalScanner{
		source: source,
	}
}

func (scanner *PascalScanner) ExtractToken() *Token {
	var (
		currChar rune
	)

	scanner.skipWhiteSpace()

	currChar, _ = scanner.CurrentChar()

	scanner.token = NewToken(scanner.source, currChar)
	return scanner.token
}

func (scanner *PascalScanner) CurrentChar() (rune, error) {
	return scanner.source.CurrentChar()
}

func (scanner *PascalScanner) NextChar() (rune, error) {
	return scanner.source.NextChar()
}

func (scanner *PascalScanner) CurrentToken() *Token {
	return scanner.token
}

func (scanner *PascalScanner) NextToken() *Token {
	scanner.token = scanner.ExtractToken()
	return scanner.CurrentToken()
}

func (scanner *PascalScanner) skipWhiteSpace() {
	r, _ := scanner.CurrentChar()

	for r == 32 || r == '{' || r == '\n' {
		// start of a comment
		if r == '{' {
			// consume comment content
			for r, _ = scanner.NextChar(); r != '}' && r != EOF; r, _ = scanner.NextChar() {
			}

			if r == '}' {
				r, _ = scanner.NextChar()
			}
		} else {
			r, _ = scanner.NextChar()
		}
	}
}
