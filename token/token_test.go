package token

import (
	"github.com/pascal-runtime-go/source"
	"github.com/stretchr/testify/assert"
	"strings"
	"testing"
)

func TestToken(t *testing.T) {
	source_1 := source.NewSource(strings.NewReader(`PROGRAM`))
	c1, _ := source_1.CurrentChar()
	token_1 := NewToken(source_1, c1)
	assert.Equal(t, token_1.GetName(), PROGRAM, "reserved word")
	assert.Equal(t, token_1.GetType(), WordToken)
	assert.Equal(t,
		TOKEN_NAMES_RESERVED.Contains(
			strings.ToUpper(token_1.GetText()),
		),
		true,
	)

	source_2 := source.NewSource(strings.NewReader(`+`))
	c2, _ := source_2.CurrentChar()
	token_2 := NewToken(source_2, c2)
	assert.Equal(t, token_2.GetName(), PLUS, "special symbols")
	assert.Equal(t, token_2.GetType(), SpecialSymbolToken)

	source_3 := source.NewSource(strings.NewReader(`'test string'`))
	c3, _ := source_3.CurrentChar()
	token_3 := NewToken(source_3, c3)
	assert.Equal(t, token_3.GetName(), STRING, "string")
	assert.Equal(t, token_3.GetType(), StringToken)

	source_4 := source.NewSource(strings.NewReader(`
		`))
	c4, _ := source_4.CurrentChar()
	token_4 := NewToken(source_4, c4)
	assert.Equal(t, token_4.GetType(), EOLToken)
}
