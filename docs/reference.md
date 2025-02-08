# Piètre

**VERSION: draft**

### Overview

This document provides a reference for the language as well as details of the implementation.

### Versioning

Until a first version of the language is published, this document will be considered a draft, subject to breaking changes without any notice. Proper versioning will start once a "1.0.0" version of the project is released.

### Acknowledgments

The structure of this document is heavily inspired by the [Rust reference](https://doc.rust-lang.org/reference/introduction.html).

## Terminology

### Undefined

This reference uses the term "UNDEFINED" to describe any part of either the compiler or the generated program that falls outside of the scope of this document. Any implementation is free to choose whichever behaviour it deems correct in such cases.

## Lexical structure

### Input

All input is assumed to be valid UTF-8. Failure to decode a UTF-8 codepoint will result in an error, and the input will be rejected.

### Whitespace

Whitespace is not significant beyond the separation of tokens. Replacing all occurrences of one or more whitespace characters in a program by a different non-empty sequence of whitespace characters does not change the meaning of the program.

### Tokens

The input file is broken down by the lexer into a series of tokens. Each token can be:

- a literal
- a keyword
- an identifier
- an operator
- a delimiter

Comments are removed as part of this phase.

### Comments

After `//`, the rest of the line is ignored. Longer code blocks use the `/* ... */` syntax.

### Literals

Valid literal tokens are:

- integer literals
- ~~float literals~~
- character literals
- string literals

#### Integers

Integer literals come in two forms: decimal and hexadecimal. Decimal literals are a non-empty sequence of digits from `0` to `9`. Hexadecimal literals are a non-empty sequence of digits from `0` to `F` prefixed by `0x`.

> UNDEFINED!
> The meaning of an integer literal that does not fit in a signed 64bit integer (i.e. a literal strictly greater than 9223372036854775807) is UNDEFINED. The compiler is free to reject it, truncate it, replace it by another value, or any other behaviour it sees fit.

#### Characters

A character literal is one UTF-8 codepoint, enclosed between two single quotes. The usual escapes are supported:

- `\xNN`: ASCII character
- `\uNNNN`: unicode code point
- `\n`: newline
- `\r`: carriage return
- `\t`: tab
- `\\`: backslash
- `\0`: null
- `\'`: single quote
- `\"`: double quote

Any unrecognized escape sequence will be rejected with an error.

#### Strings

A string literal is a sequence of characters enclosed between two double quotes. The same escaping rules apply as for single characters, but with an additional one: if a `\` character is followed by a newline, then all following whitespace, including said newline, is skipped until either another `\` or the end of the string is encountered. This allows for multi-line strings, like so:

```
"First line \
    \and second line"
```

This is equivalent to the string `"First line and second line"`.

After escaping a newline with a backslash, encountering any other character than whitespace, a closing backslash, or the end of the string is a parse error.

Raw string literals are not supported.

### Keywords

Keywords are special "identifiers" to which the language ascribes special meanings, and that consequently cannot be used as identifiers. They are:

- `and`
- `as`
- `break`
- `const`
- `continue`
- `elif`
- `else`
- `enum`
- `false`
- `for`
- `fun`
- `if`
- `import`
- `in`
- `let`
- `not`
- `or`
- `record`
- `return`
- `struct`
- `true`
- `type`
- `while`
- `xor`

### Identifiers

An identifier is any non-empty combination of letters (as per the Unicode definition) and the underscore character. Numbers are allowed in identifiers except as the first character.

- `foo` is a valid identifier
- `_` is a valid identifier
- `_2x` is a valid identifier
- `名前` is a valid identifier
- `2_` isn't
- `x'` isn't
- `>>=` isn't

### Operators

There isn't a fixed list of operators in the language. Any combination of operator characters is a valid operator, as far as the syntax of the language is concerned. The long-term goal is to support user-defined operators, even if this is not yet part of the language.

Valid operator characters are:

- `+`
- `-`
- `*`
- `/`
- `=`
- `-`
- `%`
- `^`
- `>`
- `<`
- `.`
- `;`
- `:`

For instance, `/=`, `+=`, and `>=>` are all valid operators, while they might not necessarily be defined and meaningful.

### Delimiters

Delimiters are special operators that define a "bracket". The opening bracket must always match a closing bracket. The valid pairs of brackets are:

- `( )`
- `[ ]`
- `{ }`

## Grammar

A piètre file is made of a combination of *statements*. A statement is one of the following:

- an import statement
- a type declaration
- a function declaration

### Import

An import statement brings into 
