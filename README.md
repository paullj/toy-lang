# toy

This project is a simple toy programming language designed for learning and exploration.

Things to do:

 * Parser

Longer Term:

 * Type checker
 * Online playground - WASM?
 * LSP
 * VM
 * Imports
 * Testing like pytest
 * Debug Adapter Protocol
 * JIT
 * CLI ala cargo
    * "Building" ie embed the interpreter and byte code into a binary
    * Package management
    * Support multiple versions of the language
 * Automatic documentation from docstring

Language Features

```
// A person is 
struct Person {
    pub name: string
    age: int = 0

    fn greet(other: string) -> string {
        return "Hello {other}!"
    }
}
```