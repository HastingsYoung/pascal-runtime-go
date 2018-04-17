package intermediate

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestICode(t *testing.T) {
	iCodeImpl := NewICodeImpl()
	node := NewICodeNodeImpl(PROGRAM)
	assert.Implements(t, (*ICode)(nil), iCodeImpl, "ICode interface implementation")
	iCodeImpl.SetRoot(node)
	assert.Equal(t, node, iCodeImpl.GetRoot(), "getter and setter of node")
}
