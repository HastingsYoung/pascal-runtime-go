package source

import (
	"github.com/stretchr/testify/assert"
	"strings"
	"testing"
)

func TestSource(t *testing.T) {
	source := NewSource(strings.NewReader(`PROGRAM HelloOnce;
		BEGIN
		    writeln('Hello, world.')
		END.`))
	var (
		LINESTRING_NIL   *string = nil
		LINENUM_NEG_1            = -1
		CURRENTPOS_NEG_3         = -3
		FIRST_LINE_POS_0         = 0
		LAST_LINE_POS_3          = 3
	)
	assert.Equal(t, source.LineNum(), LINENUM_NEG_1, "start at line -1")
	assert.Equal(t, source.CurrentPos(), CURRENTPOS_NEG_3, "start at position -3")
	assert.Equal(t, source.LineString(), LINESTRING_NIL, "line string is nil")

	firstChar, err_1 := source.CurrentChar()
	firstPeek, err_2 := source.PeekChar()
	nextChar, err_3 := source.NextChar()

	assert.Nil(t, err_1)
	assert.Nil(t, err_2)
	assert.Nil(t, err_3)

	assert.Equal(t, firstChar, 'P', "first char read")
	assert.Equal(t, firstPeek, nextChar, "first char peek")

	for _, err := source.NextChar(); err == nil &&
		!source.AtEOL(); _, err = source.NextChar() {
	}

	assert.Equal(t, source.LineNum(), FIRST_LINE_POS_0, "proceed to the end of first line")

	for _, err := source.NextChar(); err == nil &&
		!source.AtEOF(); _, err = source.NextChar() {
	}

	assert.Equal(t, source.LineNum(), LAST_LINE_POS_3, "proceed to the end of file")
}
