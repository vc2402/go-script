# GoScript
***
Go-like scripting language for embedding into Go programs
## General Info
GoScript is Go-like scripting language with access to Go objects
## Current status
### Implemented:
* functions definitions
* var declaration and initialization (embedded types are int, float, string, bool, any, map[string]..., slices, error)
* arithmetic and logic operations
* if-then-else
* access to preregistered objects and functions
* type conversions (incl. implicit)

### Next to implement:
* for loop with range
* type assertion
* build-in functions (len, cap, make, append)

### Restrictions:
* there is no possibility to create structs and type aliases
* there is no pointers
## To try
```
$ git clone https://github.com/vc2402/go-script
$ cd go-script/cli
$ go mod tidy
$ go run .
> compile test.gos
> run setG
```
Registering of accessible objects you can find in function initRegistry of main.go