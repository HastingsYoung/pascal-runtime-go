package message

import (
	"fmt"
)

type Message struct {
	Type string
	Text string
}

func Log(args ...interface{}) {
	fmt.Print("[LOG] >> ")
	fmt.Println(args...)
}

func Error(args ...interface{}) {
	fmt.Print("[ERROR] >> ")
	fmt.Println(args...)
}
