# toy

This project is a simple toy programming language designed for learning and exploration.

Things to do:

 * Parser
    * Good error handling
 * VM
    * Get basic features in. ie finish crafting interpreters

Longer Term:

 * Type checker
 * LSP
 * Nice lang features
    * f strings
    * lists + dicts 
    * enums
    * traits? or equiv
    * option
    * result
 * Automatic documentation from docstring
 * Imports
 * Async? go-like
 * JIT
 * Online playground - WASM?
 * Debug Adapter Protocol
 * Testing like pytest
 * CLI ala cargo
    * "Building" ie embed the interpreter and byte code into a binary
    * Package management
    * Support multiple versions of the language

Optimisations
* https://docs.rs/tinyvec/latest/tinyvec/enum.TinyVec.html


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