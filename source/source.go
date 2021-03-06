package source

import (
	"bufio"
	"github.com/pascal-runtime-go/message"
	"io"
	"unicode/utf8"
)

const (
	EOF = '0'
	EOL = '\n'
)

type Source struct {
	reader  *bufio.Scanner
	currPos int     // current line position
	line    *string // current line
	lineNum int     // current line number
}

func NewSource(reader io.Reader) *Source {
	s := bufio.NewScanner(reader)
	s.Split(bufio.ScanLines)
	return &Source{
		reader:  s,
		currPos: -3,
		line:    nil,
		lineNum: -1,
	}
}

func (src *Source) CurrentChar() (rune, error) {
	if src.currPos <= -2 {
		// first time
		src.readLine()
		return src.NextChar()
	} else if src.line == nil {
		return EOF, nil
	} else if src.CurrentPos() == -1 || src.CurrentPos() == len(*src.line) {
		// end of line
		return EOL, nil
	} else if src.CurrentPos() > len(*src.line) {
		src.readLine()
		return src.NextChar()
	} else {
		r, _ := utf8.DecodeRune([]byte{(*src.line)[src.CurrentPos()]})
		return r, nil
	}
}

func (src *Source) NextChar() (rune, error) {
	src.currPos++
	r, err := src.CurrentChar()
	if src.line != nil {
		message.Log("line:", src.LineNum(), "position:", src.CurrentPos(), "rune:", r)
	}
	return r, err
}

func (src *Source) PeekChar() (rune, error) {
	if src.CurrentPos()+1 >= len(*src.line) {
		return EOL, nil
	}

	r, _ := utf8.DecodeRune([]byte{(*src.line)[src.CurrentPos()+1]})
	return r, nil
}

func (src *Source) CurrentPos() int {
	return src.currPos
}

func (src *Source) LineNum() int {
	return src.lineNum
}

func (src *Source) LineString() *string {
	return src.line
}

func (src *Source) AtEOF() bool {
	if src.currPos <= -2 {
		src.readLine()
	}
	return src.line == nil
}

func (src *Source) AtEOL() bool {
	return src.line != nil && src.currPos == len(*src.line)
}

func (src *Source) readLine() {
	if src.reader.Scan() {
		src.lineNum++
		line := src.reader.Text()
		src.line = &line
		src.currPos = -1
		return
	}
	src.line = nil
	return
}
