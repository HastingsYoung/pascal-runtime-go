package intermediate

type NodeType string

// Define the types of node in AST tree
const (
	// Program structure
	PROGRAM   NodeType = "PROGRAM"
	PROCEDURE NodeType = "PROCEDURE"
	FUNCTION  NodeType = "FUNCTION"

	// Statements
	COMPOUND         NodeType = "COMPOUND"
	ASSIGN           NodeType = "ASSIGN"
	LOOP             NodeType = "LOOP"
	TEST             NodeType = "TEST"
	CALL             NodeType = "CALL"
	PARAMETERS       NodeType = "PARAMETERS"
	IF               NodeType = "IF"
	SELECT           NodeType = "SELECT"
	SELECT_BRANCH    NodeType = "SELECT_BRANCH"
	SELECT_CONSTANTS NodeType = "SELECT_CONSTANTS"
	NO_OP            NodeType = "NO_OP"

	// Relational operators
	EQ  NodeType = "EQ"
	NE  NodeType = "NE"
	LT  NodeType = "LT"
	LE  NodeType = "LE"
	GT  NodeType = "GT"
	GE  NodeType = "GE"
	NOT NodeType = "NOT"

	// Additive operators
	ADD      NodeType = "ADD"
	SUBTRACT NodeType = "SUBTRACT"
	OR       NodeType = "OR"
	NEGATE   NodeType = "NEGATE"

	// Multiplicative operators
	MULTIPLY       NodeType = "MULTIPLY"
	INTEGER_DIVIDE NodeType = "INTEGER_DIVIDE"
	FLOAT_DIVIDE   NodeType = "FLOAT_DIVIDE"
	MOD            NodeType = "MOD"
	AND            NodeType = "AND"

	// Operands
	VARIABLE         NodeType = "VARIABLE"
	SUBSCRIPTS       NodeType = "SUBSCRIPTS"
	FIELD            NodeType = "FIELD"
	INTEGER_CONSTANT NodeType = "INTEGER_CONSTANT"
	REAL_CONSTANT    NodeType = "REAL_CONSTANT"
	STRING_CONSTANT  NodeType = "STRING_CONSTANT"
	BOOLEAN_CONSTANT NodeType = "BOOLEAN_CONSTANT"

	// WRITE parameter
	WRITE_PARM NodeType = "WRITE_PARM"
)

type ICodeNode interface {
	GetType() NodeType
	GetParent() ICodeNode
	SetParent(n ICodeNode) ICodeNode
	AddChild(node ICodeNode) ICodeNode
	GetChildren() []ICodeNode
	SetAttribute(key ICodeKey, val interface{})
	GetAttribute(key ICodeKey) interface{}
	SetTypeSpec(spec *TypeSpec)
	GetTypeSpec() *TypeSpec
	Copy() ICodeNode
}

type ICodeNodeImpl struct {
	nodeType   NodeType
	parent     ICodeNode
	children   []ICodeNode
	attributes map[ICodeKey]interface{}
	typeSpec   *TypeSpec
}

func NewICodeNodeImpl(nt NodeType) *ICodeNodeImpl {
	return &ICodeNodeImpl{
		nodeType:   nt,
		parent:     nil,
		children:   []ICodeNode{},
		attributes: map[ICodeKey]interface{}{},
	}
}

func (node *ICodeNodeImpl) GetType() NodeType {
	return node.nodeType
}

func (node *ICodeNodeImpl) GetParent() ICodeNode {
	return node.parent
}

func (node *ICodeNodeImpl) SetParent(n ICodeNode) ICodeNode {
	node.parent = n
	return node.parent
}

func (node *ICodeNodeImpl) AddChild(n ICodeNode) ICodeNode {
	if n != nil {
		node.children = append(node.children, n)
		n.SetParent(node)
	}
	return n
}

func (node *ICodeNodeImpl) GetChildren() []ICodeNode {
	return node.children
}

func (node *ICodeNodeImpl) SetAttribute(key ICodeKey, val interface{}) {
	node.attributes[key] = val
}

func (node *ICodeNodeImpl) GetAttribute(key ICodeKey) interface{} {
	return node.attributes[key]
}

func (node *ICodeNodeImpl) SetTypeSpec(spec *TypeSpec) {
	node.typeSpec = spec
}

func (node *ICodeNodeImpl) GetTypeSpec() *TypeSpec {
	return node.typeSpec
}

func (node *ICodeNodeImpl) Copy() ICodeNode {
	cp := NewICodeNodeImpl(node.GetType())
	for k, v := range node.attributes {
		cp.SetAttribute(k, v)
	}
	return cp
}
