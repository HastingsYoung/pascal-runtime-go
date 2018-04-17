package scanner

import (
	. "github.com/pascal-runtime-go/message"
	. "github.com/pascal-runtime-go/source"
	. "github.com/pascal-runtime-go/token"
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

	currChar, _ = scanner.CurrentChar()

	scanner.token = NewToken(scanner.source, currChar)
	return scanner.token
}

func (scanner *PascalScanner) CurrentChar() (rune, error) {
	scanner.skipWhiteSpace()
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
	Log("token:", scanner.token, TOKEN_NAMES[scanner.token.Name])
	return scanner.CurrentToken()
}

func (scanner *PascalScanner) skipWhiteSpace() {
	r, _ := scanner.source.CurrentChar()

	for r == 32 || r == '{' || r == '\n' || r == '\t' {
		// start of a comment
		if r == '{' {
			// consume comment content
			for r, _ = scanner.source.NextChar(); r != '}' && r != EOF; r, _ = scanner.source.NextChar() {
			}

			if r == '}' {
				r, _ = scanner.source.NextChar()
			}
		} else {
			r, _ = scanner.source.NextChar()
		}
	}
}

func (scanner *PascalScanner) AtEOF() bool {
	return scanner.source.AtEOF()
}

func (scanner *PascalScanner) AtEOL() bool {
	return scanner.source.AtEOL()
}

func (scanner *PascalScanner) LineNum() int {
	return scanner.source.LineNum()
}
