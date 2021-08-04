Tasks for pattern matching:
- [x] Extend for associative structures
- [x] Allow functions of x to match just x (identity function)
- [x] Allow functions of x to match constants in some cases, as with distribution.
- [ ] Improve associative matching so that it doesn't appear to be recursive.
- [ ] Improve associative matching of dissimilar structures.\*
- [ ] Match Commutative structures even when pattern and math are reversed.
- [ ] Prioritize more specific patterns.\*\*

\* Whenever I match a constant and a function `5x^2` I want it to match in one go
even if the constant is spread over multiple operands in the multiplication. For
example `5sin(3)x^2` should immediately detect `5sin(3)` as the whole constant,
without recursion. This will require the pattern matching to try every combination,
or else I'll have to build it to intelligently catagorize the operands so that they
may be matched to the correct component of the pattern.

\*\* The patter that matches a scaler and a function of `x`, `cf(x)`, could
technically match any two constants, `c\sin(b)`, as a function of `x` can match any
arbitrary constant. The pattern matcher needs to prioritize simpler matches over
more complex ones, especially ones that don't "appear" to match, like when a
function of `x` matches just `x` or a constant.

Simplifications Engine:
- [ ] Roll Simplifications into rules engine.
- [ ] Simplify fraction subtraction.
- [ ] Simplify mutliplication of numbers.
- [ ] Simplify multiplication of fractions.
- [ ] Reduce fractions.

Math structures:
- [X] Subtraction.\*
- [ ] Negation.
- [ ] Parenthesis.

\* Only for pretty-printing and latex.

Pretty Printing
- [x] Make pretty printing recursive on addition.

Derivation:
- [ ] Chain Rule.
- [ ] Quotient Rule.

Development:
- [ ] Add logging using timbre.

Other:
- [ ] Write latex dsl.
