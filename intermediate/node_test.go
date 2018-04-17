package intermediate

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestNode(t *testing.T) {
	var (
		rootNode     = NewICodeNodeImpl(PROGRAM)
		childNode    = NewICodeNodeImpl(FUNCTION)
		spec         = NewTypeSpecImpl(SUBRANGE)
		CHILDREN_LEN = 1
		DUMMY_VAL    = "dummy"
	)
	rootNode.AddChild(childNode)
	childNode.SetAttribute(ID, DUMMY_VAL)
	childNode.SetTypeSpec(spec)

	assert.Implements(t, (*ICodeNode)(nil), rootNode, "node implementation")
	assert.Equal(t, rootNode.GetType(), PROGRAM)
	assert.Equal(t, rootNode.Copy().GetType(), PROGRAM)
	assert.Equal(t, len(rootNode.GetChildren()), CHILDREN_LEN)
	assert.Equal(t, childNode.GetParent(), rootNode)
	assert.Equal(t, childNode.GetAttribute(ID), DUMMY_VAL)
	assert.Equal(t, childNode.GetTypeSpec(), spec)
	assert.Equal(t, childNode.GetTypeSpec().GetForm(), SUBRANGE)
}
