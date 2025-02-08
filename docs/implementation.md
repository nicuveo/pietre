# Code organization

The code is segmented in several stages, that resemble the stages of a real compiler. Each stage has a corresponding file in `Lang/Pietre/Stages`, and each representation has a corresponding file in `Lang/Pietre/Representations`.

```mermaid
stateDiagram-v2
    s: source code
    t: tokens
    a: AST
    i: IR
    b: bytecode
    r: image
    e: error

    state lexing <<choice>>
    state parsing <<choice>>
    state analysis <<choice>>

    s --> lexing: lexing
    lexing --> t
    lexing --> e: lexical error

    t --> parsing: parsing
    parsing --> a
    parsing --> e: syntax error

    a --> analysis: analysis
    analysis --> i
    analysis --> e: type error
    analysis --> e: name error

    i --> i: optimization
    i --> b: code generation
    b --> r: assembly
```

# Lexing and parsing

