## Brick

### PRETTY-FUCKIN-PRINT

HAVE TO HAVE IT. Seriously...

### REPL-like environment: websocket-based repl

### Compile-time-parsing

Basic parser for expression mini-languages that can be compiled to native code at compile time. 

Examples:

- SQL-like statements for describing data extraction and transformation for writing DSL code by non-experts
- mini-expression language for GUI sizing and positioning constrains



### Marked curry

Curry is good. But it should be visible.

```
(defns

    (add3 (:: int -> int -> int -> int)
        [a b c]
        (+ a b)
        )
        
    (plus5 :: int -> int -> int
        (add2 5 ..)
    
    (plus5-all :: (slice int) -> (slice int)
        [is] ((slice/foldl int) plus5 is) 
    
    
    
(defn plus10 : int -> int
    [a] (+ a 10))


```


## From MPS




## From mbeddr


### Cleaned Up C99

A cleaned up version of C99 helps avoid low-level bugs. For example, the preprocessor is not supported and all its legitimate uses are supported by first-class concepts such as a robust module system. mbeddr also supports a native boolean type, enforces the use of the size-aware integral types and also has a cleaned up syntax for function types, function pointers and lambdas.


### Physical Units

Many embedded systems deal with quantities from the real world, and mbeddr comes with an extension for phyiscal units to help represent such quantities. Types as well as literals can be annotated with units. The seven SI units are predefined, and users can define there own derived or convertible units. The type system computes with these units and reports errors in the IDE. There is zero runtime overhead, since the generated code has no representation of the units.

### State Machines
    
State-based behavior is ubiquitous in embedded software. mbeddr natively supports state machines that have events, variables, states and transitions with guards. States have entry, exit and do actions, transitions can have actions as well. State machines can be hierarchical and use epsilon transitions. State machines can be triggered from C code and themselves interact with C code in actions. State machines can be edited as text and tables, and can be visualized. Verification is available via model checking.


