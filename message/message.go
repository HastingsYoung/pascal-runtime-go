package message

import (
	"errors"
	"fmt"
	"strings"
)

type Message struct {
	Type string
	Text string
}

func Log(args ...interface{}) {
	fmt.Print("[LOG] >> ")
	fmt.Println(args...)
}

func Error(args ...interface{}) error {
	fmt.Print("[ERROR] >> ")
	var template = []string{}
	for i := 0; i < len(args); i++ {
		template = append(template, "%+v")
	}
	text := fmt.Sprintf(strings.Join(template, "%s"), args...)
	fmt.Println(text)
	return errors.New(text)
}
