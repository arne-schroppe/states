# States tool

## Purpose
A tool that helps me think about the various states a system can be in

## What does it do
I write down information about the possible states in a dsl
(This notation should already help me think about the various states)
The tool can then print out all the possible states in a long list

## Notation
The syntax is based on algebraic data types. The basic type is a symbol
(maybe also strings?). The higher order types are the usual product and
sum types. 

All of: `a, b`
Alternative: `a | b`
Hierarchy/specialisation: `a b`

Parentheses for structure: `a (b | d)`, `((a, b), c)`

- What's the difference between `a b1 b2` and `a (b1, b2)`?
- Should we disallow the first form?


## Syntax examples
```



```
