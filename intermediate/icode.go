package intermediate

type ICodeKey string

const (
	LINE  ICodeKey = "LINE"
	ID    ICodeKey = "ID"
	VALUE ICodeKey = "VALUE"
)

type ICode interface {
	SetRoot(node ICodeNode) ICodeNode
	GetRoot() ICodeNode
}

type ICodeImpl struct {
	root ICodeNode
}

func (code *ICodeImpl) SetRoot(node ICodeNode) ICodeNode {
	code.root = node
	return node
}

func (code *ICodeImpl) GetRoot() ICodeNode {
	return code.root
}

func NewICode() ICode {
	return new(ICodeImpl)
}
