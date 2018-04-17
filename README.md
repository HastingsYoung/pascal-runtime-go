# Pascal-Runtime-Go
## Intro
A Pascal runtime written in Golang.

The project is highly inspired by and slightly varied from the book **[Writing Compilers and Interpreters (3rd edition): A Software Engineering Approach](http://www.apropos-logic.com/wci/)** and **[Pascalts](https://github.com/hsiaosiyuan0/Pascalts)**.

## Quick Start
```sh
// Check "Hello world" example by running
go run main.go
```

## Todos
| Feature  | Done |
|---|---|
| Source | [✓] |
| Token | [✓] |
| Scanner | [✓] |
| Parser | [✓] |
| Node | [✓] |
| AST | [✓] |
| Messages | [✓] |
| Executor | [x] |
| Debugger | [x] |

## Tests
| Package  | Done | Coverage |
|---|---|---|
| executor | [x] | |
| intermediate | [x] | |
| intermediate/definition | [x] | |
| intermediate/routinecode | [x] | |
| intermediate/typechecker | [x] | |
| message | [✓]	| [100.0%] | 
| parser | [x] | |
| scanner | [x] | |
| source | [✓] | [94.9%] |
| token | [✓] | [27.4%] |