package message

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestMessage(t *testing.T) {
	Log("Test Message")
	Warn("Test Message")
	Debug("Test Message")
	assert.Error(t, Error("Test Message"), "Return an error object after log")
}
