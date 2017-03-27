MLC AST
-------



BlockTree
=========

Reprensents a block of operation.


- symbols:
    - imports: the symbols imported by the current block
    - exports: the symbols that can escape the current block

- entry: describes how control may enter this node
    - single: one local entry
    - merge: a phi-node which has a value based on where the control is coming from

- body: a list of SSA instructions

- exit: the way control leaves this block
    - Single next: control is transfered to the next block
    - Branch left right: control transfer is dependent on the given symbol



Instructions
============

Reprsents SSA expresssions.


- symbol: the symbol to be defined
    - name: the name of the symbol in the local symbol table
    - type: the underlying type of the symbol

- expression: the SSA expression to use for combining the args
    - name: the name of the expression
    - type: the return type of the expression

- args: a list of symbols to pass as arguments

```elm





```


Expression
==========


Represents a tree of expressions that always return a single value, so they can be assigned
to a symbol.


- parameters: a list of symbols that the caller needs to provide
- returnType: the type of the return value of this expression

- head: the label of the entry point in the tree
- tails: the labels of the exit points in the tree


ExpressionBlock
===============

