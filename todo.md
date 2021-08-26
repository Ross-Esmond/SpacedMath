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

Math Unifier:
- [ ] Support recursive matching
- [ ] Maintain order of variable pattern matchers: [\f \x] and \x
- [ ] Support partial matches: `u-u` matching `a+b-b`

Simplifications Engine:
- [ ] Roll Simplifications into rules engine.
- [x] Make it performant.
- [ ] Definitions
  - [ ] a + a = 2a
  - [ ] a\*a = a^2
- [ ] Cancelation
  - [ ] a - a = 0
  - [ ] a/a = 1
- [ ] Calculations
  - [x] 2 + 2 = 4
  - [x] 2\*2 = 4
  - [x] 2^2 = 4
  - [x] 4/6 = 2/3
  - [ ] x^(2.4-1) = x^(1.4)
- [x] Fraction Arithmetic
  - [x] 1/2 + 1 = 3/2
  - [x] 2/3\*4/3 = 8/9
  - [x] \(2/3)^2 = 4/9
  - [x] \(2/3)/\(5/7) = 14/15
- [ ] Combinations
  - [ ] \(a+b)+c = a+b+c
  - [ ] \(a+b)+c+d = a+b+c+d\*
  - [ ] \(ab)c = abc
  - [ ] \(a^b)^c = a^(bc)
- [ ] Normalization
  - [ ] \(-1)/2 = -(1/2)
  - [ ] a + (-b) = a - b
  - [ ] a\*(1/b) = a/b
- [x] Don't move
  - [x] x5 = x5
  - [x] 1+x = 1+x
- [ ] Numeric forms
  - [x] fractions: 1/3
  - [ ] decimal: 0.25
  - [ ] irrational: 0.33…
- [ ] Timing
  - [ ] instantanious: before the user even sees it x^1=x
  - [ ] ohio case: with one equality before moving on (x^2/2)'=2x/2=x
  - [ ] belated: saved till the end in case the user doesn't do it 28/49=4/7

\* The rule (a+b)+c=a+b+c causes a bug when there are two outside operators. The
matching code will always group the operators into a nested addition, which will
trigger the rule a second time to revert the change, entering an endless loop.

Math structures:
- [X] Subtraction.\*
- [ ] Negation.
- [ ] Parenthesis.
- [ ] Roots with nested expressions: root(5t, 2)
- [ ] Theta

\* Only for pretty-printing and latex.

Pretty Printing
- [x] Make pretty printing recursive on addition.
- [ ] Don't show number 2 on root 2.

Derivation:
- [ ] Chain Rule.
- [x] Quotient Rule.
- [ ] Don't detect subtraction as scaler skill.
- [ ] Derive y=1/(x+1)

Bugs:
- [ ] h(r)=a\*e^r/b leaves 0

Development:
- [ ] Add logging using timbre.

Adding Problems:
- [x] implement simple math expression parser
- [x] add mafs guide to page
- [x] extend mafs
  - [x] add sqrt
  - [x] add greek letters\*
- [x] create tool to quickly add new equations to system
  - [x] display equation in latex
  - [x] display solution in latex
  - [x] display skills involved
  - [ ] verify problem solution using open source library like sympy
  - [x] enter problems into database
  - [x] serve those problems and display them in main app
  - [x] link to eq.html
  - [ ] add visual indictation of successful problem addition
  - [ ] check if equation is valid before submitting

\* Just pi for now, but the feature is working.

Application:
- [x] Get google login working on site.
- [x] Get relational database running.
- [ ] Setup permission tracking.

Varible Detection:
- [ ] Prioritize some variables: x, y and z over A, c, etc.

Latex:
- [ ] print derivation of variable--d/dx(y)--as one fraction: dy/dx.
- [ ] Write latex dsl.
- [ ] Function calls like sin(x) shouldn't use left-right parens.
