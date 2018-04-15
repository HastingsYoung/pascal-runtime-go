package token

import (
	"errors"
	"github.com/pascal-runtime-go/message"
	. "github.com/pascal-runtime-go/source"
	"math"
	"strings"
	"unicode"
)

type TokenType int

const (
	EOFToken TokenType = iota
	EOLToken
	WordToken
	NumberToken
	StringToken
	SpecialSymbolToken
	ErrorToken
)

const MAX_EXPONENT = 37

var TOKEN_TYPES = map[TokenType]string{
	EOFToken:           "EOF",
	EOLToken:           "EOL",
	WordToken:          "WORD",
	NumberToken:        "NUMBER",
	StringToken:        "STRING",
	SpecialSymbolToken: "SPECIAL_SYMBOL",
	ErrorToken:         "INVALID",
}

type Token struct {
	Text     string
	Value    interface{}
	Type     TokenType
	Name     TokenName
	Source   *Source
	lineNum  int
	position int
}

func NewToken(source *Source, char ...rune) *Token {
	if len(char) > 0 {
		currChar := char[0]

		token := &Token{
			Source:   source,
			lineNum:  source.LineNum(),
			position: source.CurrentPos(),
		}

		if currChar == EOF {
			token.Type = EOFToken
		} else if currChar == EOL {
			token.Type = EOLToken
		} else if unicode.IsLetter(currChar) {
			token.Type = WordToken
		} else if unicode.IsNumber(currChar) {
			token.Type = NumberToken
		} else if currChar == '\'' {
			token.Type = StringToken
		} else if TOKEN_NAMES_SPECIAL_SYMBOLS.Contains(string(currChar)) {
			token.Type = SpecialSymbolToken
		} else {
			token.Type = ErrorToken
		}

		return token.Extract()
	}

	return (&Token{
		Source:   source,
		lineNum:  source.LineNum(),
		position: source.CurrentPos(),
	}).Extract()
}

// Each type of token has its own way of extraction
// TokenName will be set during the extraction period
func (tk *Token) Extract() *Token {
	switch tk.Type {
	case EOFToken:
		return tk
	case EOLToken:
		return tk
	case WordToken:
		var chars []rune

		for char, _ := tk.CurrentChar(); ; {
			if unicode.IsLetter(char) || unicode.IsDigit(char) {
				chars = append(chars, char)
				char, _ = tk.NextChar()
			} else {
				break
			}
		}

		tk.Text = runeToString(chars)
		key := strings.ToUpper(tk.GetText())

		if TOKEN_NAMES_RESERVED.Contains(key) {
			tk.Name = TOKEN_NAMES_RESERVED.GetName(key)
		} else {
			tk.Name = IDENTIFIER
		}
		return tk
	case NumberToken:
		var (
			wholeDigits,
			fractionDigits,
			exponentDigits,
			textBuffer []rune
			exponentSign rune = '+'
			sawDotDot    bool = false
			currentChar  rune
			err          error
		)

		tk.Name = INTEGER

		wholeDigits, err = tk.unsignedIntegerDigits()
		textBuffer = append(textBuffer, wholeDigits...)

		if err != nil {
			tk.Type = ErrorToken
			tk.Value = err
			return tk
		}

		currentChar, _ = tk.CurrentChar()

		if currentChar == '.' {
			if peek, _ := tk.PeekChar(); peek == '.' {
				sawDotDot = true
			} else {
				tk.Name = REAL
				textBuffer = append(textBuffer, currentChar)
				currentChar, _ = tk.NextChar()

				fractionDigits, err = tk.unsignedIntegerDigits()
				textBuffer = append(textBuffer, fractionDigits...)

				if err != nil {
					tk.Type = ErrorToken
					tk.Value = err
					return tk
				}
			}
		}

		currentChar, _ = tk.CurrentChar()

		if !sawDotDot && (currentChar == 'E' || currentChar == 'e') {
			tk.Name = REAL
			textBuffer = append(textBuffer, currentChar)
			currentChar, _ = tk.NextChar()

			if currentChar == '+' || currentChar == '-' {
				textBuffer = append(textBuffer, currentChar)
				exponentSign = currentChar
				currentChar, _ = tk.NextChar()
			}

			exponentDigits, err = tk.unsignedIntegerDigits()
			textBuffer = append(textBuffer, exponentDigits...)

			if err != nil {
				tk.Type = ErrorToken
				tk.Value = err
				return tk
			}
		}

		switch tk.Name {
		case INTEGER:
			var integerValue int
			integerValue, err = computeIntegerValue(wholeDigits)

			tk.Value = integerValue

			if err != nil {
				tk.Type = ErrorToken
				tk.Value = err
			}
		case REAL:
			var floatValue float64
			floatValue, err = computeFloatValue(
				wholeDigits,
				fractionDigits,
				exponentDigits,
				exponentSign,
			)
			tk.Value = floatValue

			if err != nil {
				tk.Value = err
				tk.Type = ErrorToken
			}
		default:
			tk.Type = ErrorToken
			tk.Value = errors.New("Not A Number Error")
		}
		return tk
	case StringToken:
		var (
			textBuffer,
			valueBuffer []rune
			currentChar rune
		)

		// left '\''
		textBuffer = append(textBuffer, '\'')
		currentChar, _ = tk.NextChar()

		if currentChar != '\'' && currentChar != EOF {
			textBuffer = append(textBuffer, currentChar)
			valueBuffer = append(textBuffer, currentChar)
			currentChar, _ = tk.NextChar()
		}

		if currentChar == '\'' {
			nextChar, _ := tk.PeekChar()

			for currentChar == '\'' && nextChar == '\'' {
				nextChar, _ = tk.PeekChar()
				textBuffer = append(textBuffer, '\'', '\'')
				valueBuffer = append(textBuffer, currentChar)

				// consume pair quotes
				currentChar, _ = tk.NextChar()
				currentChar, _ = tk.NextChar()
			}
		}

		for currentChar != '\'' && currentChar != EOF {
			if unicode.IsSpace(currentChar) {
				currentChar = ' '
			}

			if currentChar != '\'' && currentChar != EOF {
				textBuffer = append(textBuffer, currentChar)
				valueBuffer = append(textBuffer, currentChar)
				currentChar, _ = tk.NextChar()
			}

			if currentChar == '\'' {
				nextChar, _ := tk.PeekChar()

				for currentChar == '\'' && nextChar == '\'' {
					nextChar, _ = tk.PeekChar()
					textBuffer = append(textBuffer, '\'', '\'')
					valueBuffer = append(textBuffer, currentChar)

					// consume pair quotes
					currentChar, _ = tk.NextChar()
					currentChar, _ = tk.NextChar()
				}
			}
		}

		if currentChar == '\'' {
			tk.NextChar()
			textBuffer = append(textBuffer, '\'')
			tk.Name = STRING
			tk.Value = runeToString(valueBuffer)
		} else {
			tk.Type = ErrorToken
			tk.Value = errors.New("Unexpected End of File")
			return tk
		}

		tk.Text = runeToString(textBuffer)
		return tk
	case SpecialSymbolToken:
		var (
			runes       = []rune{}
			currentChar rune
		)
		currentChar, _ = tk.CurrentChar()

		switch currentChar {
		case '+':
			tk.NextChar()
			tk.Name = PLUS
			break
		case '-':
			tk.NextChar()
			tk.Name = MINUS
			break
		case '*':
			tk.NextChar()
			tk.Name = STAR
			break
		case '/':
			tk.NextChar()
			tk.Name = SLASH
			break
		case ',':
			tk.NextChar()
			tk.Name = COMMA
			break
		case ';':
			tk.NextChar()
			tk.Name = SEMICOLON
			break
		case '\'':
			tk.NextChar()
			tk.Name = QUOTE
			break
		case '=':
			tk.NextChar()
			tk.Name = EQUALS
			break
		case '(':
			tk.NextChar()
			tk.Name = LEFT_PAREN
			break
		case ')':
			tk.NextChar()
			tk.Name = RIGHT_PAREN
			break
		case '[':
			tk.NextChar()
			tk.Name = LEFT_BRACKET
			break
		case ']':
			tk.NextChar()
			tk.Name = RIGHT_BRACKET
			break
		case '{':
			tk.NextChar()
			tk.Name = LEFT_BRACE
			break
		case '}':
			tk.NextChar()
			tk.Name = RIGHT_BRACE
			break
		case '^':
			tk.NextChar()
			tk.Name = UP_ARROW
			break
		case ':':
			currentChar, _ = tk.NextChar()
			if currentChar == '=' {
				runes = append(runes, currentChar)
				tk.NextChar()
				tk.Name = COLON_EQUALS
			} else {
				tk.Name = COLON
			}
			break
		case '<':
			currentChar, _ = tk.NextChar()
			if currentChar == '=' {
				runes = append(runes, currentChar)
				tk.NextChar()
				tk.Name = LESS_EQUALS
			} else if currentChar == '>' {
				runes = append(runes, currentChar)
				tk.NextChar()
				tk.Name = NOT_EQUALS
			} else {
				tk.Name = LESS_THAN
			}
		case '>':
			currentChar, _ = tk.NextChar()

			if currentChar == '=' {
				runes = append(runes, currentChar)
				tk.NextChar()
				tk.Name = GREATER_EQUALS
			} else {
				tk.Name = GREATER_THAN
			}
			break
		case '.':
			currentChar, _ = tk.NextChar()

			if currentChar == '.' {
				runes = append(runes, currentChar)
				tk.NextChar()
				tk.Name = DOT_DOT
			} else {
				tk.Name = DOT
			}
			break
		default:
			tk.Type = ErrorToken
			tk.Value = errors.New("Invalid Character")
			tk.Name = ERROR
			return tk
		}

		if tk.Type != ErrorToken {
			tk.Value = currentChar
		}

		return tk
	default:
		message.Error("Unknown Token Type")
		// ErrorToken
		tk.Type = ErrorToken
		tk.Value = errors.New("Invalid Character")
		tk.NextChar()
	}

	return tk
}

func (tk *Token) GetName() TokenName {
	return tk.Name
}

func (tk *Token) GetType() TokenType {
	return tk.Type
}

func (tk *Token) GetText() string {
	return tk.Text
}

func (tk *Token) GetLineNum() int {
	return tk.Source.LineNum()
}

func (tk *Token) GetValue() interface{} {
	return tk.Value
}

func (tk *Token) CurrentChar() (rune, error) {
	return tk.Source.CurrentChar()
}

func (tk *Token) NextChar() (rune, error) {
	return tk.Source.NextChar()
}

func (tk *Token) PeekChar() (rune, error) {
	return tk.Source.PeekChar()
}

func (tk *Token) unsignedIntegerDigits() (chars []rune, err error) {

	for char, _ := tk.CurrentChar(); ; {
		if unicode.IsDigit(char) {
			char, _ = tk.NextChar()
		} else {
			return chars, errors.New("Invalid Number")
		}
		chars = append(chars, char)
	}

	return chars, nil
}

func computeIntegerValue(digits []rune) (int, error) {
	if len(digits) < 1 {
		return 0, nil
	}

	integerValue := 0
	preValue := -1
	index := 0

	for (index < len(digits)) && (integerValue >= preValue) {
		preValue = integerValue
		index++
		integerValue = 10*integerValue + int(digits[index])
	}

	if integerValue >= preValue {
		return integerValue, nil
	}

	return integerValue, errors.New("Integer Out of Range Error")
}

func computeFloatValue(
	wholeDigits,
	fractionDigits,
	exponentDigits []rune,
	exponentSign rune,
) (float64, error) {
	var (
		floatValue       float64 = 0.0
		exponentValue, _         = computeIntegerValue(exponentDigits)
	)

	if exponentSign == '-' {
		exponentValue = -exponentValue
	}

	if frac := len(fractionDigits); frac > 0 {
		exponentValue -= frac
		wholeDigits = append(wholeDigits, fractionDigits...)
	}

	if ran := exponentValue + len(wholeDigits); ran > MAX_EXPONENT || -ran < MAX_EXPONENT {
		return floatValue, errors.New("Real Number Out of Range Error")
	}

	for index := 0; index < len(wholeDigits); {
		index++
		floatValue = 10*floatValue + float64(wholeDigits[index])
	}

	if exponentValue != 0 {
		floatValue *= math.Pow(10, float64(exponentValue))
	}

	return floatValue, nil
}

func runeToString(runes []rune) string {
	return string(runes)
}
