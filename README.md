# Mvanda programming language

Mvanda is a stack-based programming language, the majority of which was built
in about six hours. The name comes from a conlang and was generated using
[phonk](https://github.com/elyatai/phonk/). All values on the stack are
arbitrary-precision rational (Haskell's `Rational`).

## Dependencies

- Parsec 3

## Inspirations

- [><>](https://esolangs.org/wiki/Fish) - some instruction names
- [Incalculate](https://github.com/ry00001/incalculate) - paradigm ideas

## Syntax

All instructions are either a single punctuation character (one of
``!#$%&'()*+,-./:;<=>?@\^_`{|}~``), or a series of lowercase letters (`a-z`).
Spaces are only required between two instructions with alphabetical names.

String literals are enclosed with double quotes and support some backslash
escapes.

Integer literals are simply a sequence of digits (`0-9`). There is no syntax
for non-integer literals.

"Code blocks" (wrapped in `[]`) are treated as one instruction, but when
executed are merely stepped into and its contents run. The `.` instruction
jumps back and starts executing the innermost block, returning back after the
block has finished running. The `;` instruction will "return" from a block
immediately.

## Instruction set

All instructions can be found in `src/Instructions.hs` (no wiki page yet).