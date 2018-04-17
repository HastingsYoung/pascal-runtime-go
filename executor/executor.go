package executor

// import (
// 	"errors"
// 	. "github.com/pascal-runtime-go/intermediate"
// 	"math"
// )

// type ExpressionExecutor struct {
// }

// Evaluate() will return the evaluated result of the current node
// func (ee *ExpressionExecutor) Evaluate() interface{} {

// }

// func (ee *ExpressionExecutor) executeBinaryOperator(node INode, nType NodeType) (interface{}, error) {
// 	if node.HasChildren() {

// 		oprNode1, _ := node.GetChild(0)
// 		oprNode2, _ := node.GetChild(1)

// 		if nType == NodeTypeOprAnd || nType == NodeTypeOprOr {
// 			opr1, bolok1 := (ee.Evaluate(oprNode1)).(bool)
// 			opr2, bolok2 := (ee.Evaluate(oprNode1)).(bool)
// 			switch nType {
// 			case NodeTypeOprAnd:
// 				return opr1 && opr2, nil
// 			case NodeTypeOprOr:
// 				return opr1 || opr2, nil
// 			}
// 		}

// 		opr1, intok1 := (ee.Evaluate(oprNode1)).(int)
// 		opr2, intok2 := (ee.Evaluate(oprNode2)).(int)

// 		if intok1 && intok2 {
// 			switch nType {
// 			case NodeTypeOprPlus:
// 				return opr1 + opr2, nil
// 			case NodeTypeOprMinus:
// 				return opr1 - opr2, nil
// 			case NodeTypeOprMultiply:
// 				return opr1 * opr2, nil
// 			case NodeTypeOprDivide:
// 				if opr2 == 0 {
// 					return 0, errors.New("Division by zero")
// 				}
// 				return opr1 / opr2, nil
// 			case NodeTypeOprMod:
// 				if orp2 == 0 {
// 					return 0, errors.New("Division by zero")
// 				}
// 				return opr1 % opr2, nil
// 			}
// 		}

// 		opr3, floatok3 := (ee.Evaluate(oprNode1)).(float64)
// 		opr4, floatok4 := (ee.Evaluate(oprNode2)).(float64)

// 		if floatok3 && floatok4 {
// 			switch nType {
// 			case NodeTypeOprPlus:
// 				return opr3 + opr4, nil
// 			case NodeTypeOprMinus:
// 				return opr3 - opr4, nil
// 			case NodeTypeOprMultiply:
// 				return opr3 * opr4, nil
// 			case NodeTypeOprDivide:
// 				if opr4 == 0.00 {
// 					return 0, errors.New("Division by zero")
// 				}
// 				return opr3 / opr4, nil
// 			case NodeTypeOprMod:
// 				if opr4 == 0.00 {
// 					return 0, errors.New("Division by zero")
// 				}
// 				return opr3 % opr4, nil
// 			}
// 		}

// 		return nil, errors.New("Node type unidentifiable")
// 	}
// }
