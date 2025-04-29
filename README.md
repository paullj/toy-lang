# toy

This project is a simple toy programming language designed for learning and exploration.

## Things to do:

 * Parser
    * Good error handling
 * VM
    * Get basic features in. ie finish crafting interpreters
    * functions
    * structs

## Longer Term:

 * Type checker
 * Nice lang features
    * lists + dicts 
    * for loops with iterators
    * option
    * result
    * enums
    * f strings
    * traits? or equiv
 * LSP
 * Imports
 * Automatic documentation from docstring
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

## Lang Ideas

```
-- 
struct Dog {
    pub name: Str
    age: Int

    fn new(name: Str) -> Self {
        return Dog {
            name: name,
            age: 0
        }
    }

    fn bark(self) {
        return "Woof!"
    }
}

let dog: Dog = Dog::new("Sally")

echo dog.bark()
```