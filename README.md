# Mvanda programming language

Mvanda is a stack-based programming language, the majority of which was built
in about six hours. The name comes from a conlang and was generated using
[phonk](https://github.com/elyatai/phonk/). All values on the stack are
arbitrary-precision rational (Haskell's `Rational`).

## Syntax

All instructions are either a single punctuation character (one of
``!#$%&'()*+,-./:;<=>?@\^_`{|}~``), or a series of lowercase letters (`a-z`).
Spaces are only required between two instructions with alphabetical names.

String literals are enclosed with double quotes and support backslash
escapes: `\"` for a literal double quote, `\\` for a literal backslash, and
`\n` for a newline character. Other escapes, such as `\e`, are not yet
implemented, but can be upon request.

Integer literals are simply a sequence of digits (`0-9`). There is no syntax
for non-integer literals.

"Code blocks" (wrapped in `[]`) are treated as one instruction, but when
executed are merely stepped into and its contents run. The `.` instruction
jumps back and starts executing the innermost block, returning back after the
block has finished running. The `;` instruction will "return" from a block
immediately.

## Instruction set

All instructions can be found in `src/Instructions.hs` (no wiki page yet).

## Known bugs

Because of Haskell's pattern matching syntax and the way the function that
executes instructions is designed, if there aren't enough items on the stack
for an instruction to execute, it (mostly) will incorrectly error saying that
the instruction doesn't exist.