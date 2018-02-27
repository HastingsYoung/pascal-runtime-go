package source

import (
	"fmt"
	"io"
	"text/scanner"
	"unicode/utf8"
)

const (
	EOF = '0'
	EOL = '\n'
)

type Source struct {
	reader  scanner.Scanner
	currPos int     // current line position
	line    *string // current line
}

func NewSource(reader io.Reader) *Source {
	var s scanner.Scanner
	s.Init(reader)
	s.Filename = "source"
	return &Source{
		reader:  s,
		currPos: -3,
		line:    nil,
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
		r, _ := utf8.DecodeRune([]byte{(*src.line)[src.currPos]})
		return r, nil
	}
}

func (src *Source) NextChar() (rune, error) {
	src.currPos++
	r, err := src.CurrentChar()
	if src.line != nil {
		fmt.Println("LOG:", *src.line, src.currPos, r)
	}
	return r, err
}

func (src *Source) PeekChar() (rune, error) {
	char := src.reader.Peek()

	if char != scanner.EOF {
		return char, nil
	}

	return EOF, nil
}

func (src *Source) LineNum() int {
	return src.reader.Pos().Line
}

func (src *Source) CurrentPos() int {
	return src.currPos
}

func (src *Source) readLine() {
	char := src.reader.Scan()
	src.currPos = -1
	if char != -1 {
		line := src.reader.TokenText()
		src.line = &line
		return
	}
	src.line = nil
	return
}
