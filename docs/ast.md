MLC AST
-------

THe goal here is to have

- isomorphism: invoking a builtin and invoking a user function should look and behave the same
- homogeneity:

A single SSA assignment should use the same type as a complicated composite expression.

Expression
==========

Represents a tree of SSA expression blocks that always return a single value, so they can be assigned to a symbol.

Expressions have a singular top-level symbol table.


- parameters: a list of symbols that the caller needs to provide
- returnType: the type of the return value of this expression

- head: the label of the entry point in the tree
- tails: the labels of the exit points in the tree

- locals : a list of symbols that are used in the expressions local symbol tree




Flow
====

- Assignments group into blocks

- blocks flow 1-to-1 until branching or merging

- calling another expression suspends the current block and transfers flow to the called block.

- upon calling, the arguments are passed to the block in order and are put into the symbol table of the called block

- the called block executes its tree using the regular flow

- when the called expression returns the returned value is put into the symbol table of the calling expression

- the flow of the calling block continues






```
"main"
```

```
constants
    __0 U64 "0"
    __1 U64 "1"


params
    :s   Const:Ptr:U8
    :c   U8

returns Maybe:U64
    from "loop.done"

entry
    "main"

symbols
    :l U64
    :i U64
    :cmp U1
    :ch U8
    :isOk U8
    :next U64
    :retval Maybe:U64

calls
    U64 U64/alias U64
    U1 U64/lt U64 U64
    U8 U8/at U64 Const:Ptr:U8
    U64 U64/+ U64 U64
    U1 U8/eq U8 U8

    Maybe:U64 Maybe:U64/nothing
    Maybe:U64 Maybe:U64/just U64



label "main"

    :l U64 = C/strlen :s

    jump "loop.pre"



label "loop.pre"

    from "main"
        :i = U64/alias __0

    from "loop.iter"
        :i = U64/alias :next

    :cmp = U64/lt i l

    branch :cmp
        "loop.iter"
        "loop.done"



label "loop.iter"

    :ch = U8/at :i :s
    :next = U64/+ :i  __1
    :isOk = U8/eq :ch :c

    branch :isOk
        "loop.done"
        "loop.pre"



label "loop.done"

    from "loop.iter"
        :retval = Maybe:U64/nothing

    from "loop.pre"
        :retval = Maybe:U64/just :i

    return :retval





```