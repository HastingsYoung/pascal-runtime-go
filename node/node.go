package node

type NodeType string

// Define the types of node in AST tree
const (
	NodeTypeValString      NodeType = "String"
	NodeTypeValInteger     NodeType = "Integer"
	NodeTypeValFloat       NodeType = "Float"
	NodeTypeOprAssignment  NodeType = "Assignment"
	NodeTypeOprEqual       NodeType = "Equal"
	NodeTypeOprGreaterThan NodeType = "GreaterThan"
	NodeTypeOprLessThan    NodeType = "LessThan"
	NodeTypeOprPlus        NodeType = "Plus"
	NodeTypeOprMinus       NodeType = "Minus"
	NodeTypeOprMultiply    NodeType = "Multiply"
	NodeTypeOprDivide      NodeType = "Divide"
	NodeTypeOprMod         NodeType = "Mod"
	NodeTypeExpression     NodeType = "Expression"
	NodeTypeOprAnd         NodeType = "And"
	NodeTypeOprOr          NodeType = "Or"
)

type INode interface {
	HasChildren() bool
	GetValue() (interface{}, error)
	GetType() NodeType
	GetName() string
	GetChild(pos int) (*Node, error)
	GetChildren() ([]*Node, error)
}
