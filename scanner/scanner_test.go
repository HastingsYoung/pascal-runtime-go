package scanner

import (
	. "github.com/pascal-runtime-go/source"
	. "github.com/pascal-runtime-go/token"
	"github.com/stretchr/testify/assert"
	"strings"
	"testing"
)

func TestScanner(t *testing.T) {
	scanner := NewPascalScanner(
		NewSource(strings.NewReader(`PROGRAM HelloOnce;
		BEGIN
		    writeln('Hello, world.')
		END.`)),
	)
	var (
		NIL_TOKEN       *Token = nil
		LAST_LINE_POS_3        = 3
		tk              *Token
	)
	assert.Equal(t, scanner.CurrentToken(), NIL_TOKEN, "initialize with nil token")

	for _, err := scanner.NextChar(); err == nil &&
		!scanner.AtEOL(); _, err = scanner.NextChar() {
	}

	for _, err := scanner.NextChar(); err == nil &&
		!scanner.AtEOF(); _, err = scanner.NextChar() {
	}

	assert.Equal(t, scanner.LineNum(), LAST_LINE_POS_3, "proceed to the end of file")

	scanner = NewPascalScanner(
		NewSource(strings.NewReader(`PROGRAM HelloOnce;
		BEGIN
		    writeln('Hello, world.')
		END.`)),
	)

	for tk = scanner.NextToken(); tk != nil &&
		!scanner.AtEOF(); tk = scanner.NextToken() {
	}
	assert.Equal(t, tk.GetType(), EOFToken, ". token")
}
