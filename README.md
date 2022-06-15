# hlispgen

I created this repo to test out an idea for writing the parser in [hlisp](https://github.com/static-clouds/hlisp).

Interesting files:

- `src/HLispGen/Grammar.hs` contains generic functions for defining a language specification (BNF grammar) for a language and generating QuickCheck test data (text <-> parse tree).
- `src/HLispGen/Language.hs` contains an implementation of a very simple language and parser. There is code for generating a parse tree (which can be autonmatically tested) and an abstract syntax tree (can this be automatically tested?).
- `test/ParseSpec.hs` contains a test that asserts that given a string that is valid according to the language specification in `Language.hs` it can be parsed into its corresponding parse tree.
