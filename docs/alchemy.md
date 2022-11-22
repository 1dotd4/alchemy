# Alchemy

## Overview and rationale

Alchemy is an extensible computer algebra library written in [portable R7RS Scheme](https://docs.scheme.org/guide/portable-r7rs/)<sup>2</sup>. Its aim is to provide a meaningful way to express and use mathematical objects. A notable goal it tries to achieve is breaking cryptography without fighting the library as it should be as easy as parsing the data and calling a function, not harder. It tries to be readable and portable. Due to Scheme nature, it is easy to extend.

The rationale behind this are [building my own birdfeeder](https://raw.githubusercontent.com/angea/pocorgtfo/master/contents/articles/02-02.pdf) and I do not really like Python, Sagemath, MAGMA, et al.'s approach. By other means, luck is when preparation meets opportunity. Then, I hope this code publication is one of [Type II](http://www.loper-os.org/?p=4012#selection-106.0-106.2). Moreover, this project is meant to be a continuous endeavour based on my knowledge.

Scheme seems to be balanced enough as I can write later a macro or other syntax sugar to get my code written faster. To prevent implementation error, tests are written to check edge cases. It usually takes some effort and will power to write a sketch idea and later [rewrite](http://verisimilitudes.net/2020-12-27) it, but the result is satifying.

The name was chosen because I wanted to play with Algebra, but not being too much rigorous at first, I chose to name it _Alchemy_. The metaphor is playing with mathematical objects, just like one would do with spells in a cauldron. For clarity, this methaphor is not used to explain the mathematical objects. Because I try to combine functions, combinators are implemented to help reusing code. Some topics of interests are the following.

- Number Theory
- Linear Algebra
- Abstract Algebra
- Cryptography
- Compilers

Books and articles have been used to write this code and are listed below.

- _R7RS Scheme_
- Iverson, _Notation as a Tool of Thought_
- Backus, _Can Programming Be Liberated from the von Neumann Style?_
- Cohen, _A Course in Computational Algebraic Number Theory_
- Sussman, _Software Design for Flexibility_
- [_Patterns_](//unpx.net/d4/#patterns)

<p><small><ol>
    <li value="2">It is implemented starting from CHICKEN Scheme because it is the most portable, complete, and simple Scheme I could find.</li>
</ol></small></p>

## Installation guide

[CHICKEN Scheme](https://www.call-cc.org/) is mainly used. We list the SRFI used and then we provide the line to install all packages for CHICKEN.

<table>
  <tr><th>Task</th><th>SRFI import</th></tr>
  <tr><td>List Library</td><td>1</td></tr>
  <tr><td>Homogeneous numeric vector datatypes</td><td>4</td></tr>
  <tr><td>String Library</td><td>13</td></tr>
  <tr><td>Source of Random Bits</td><td>27</td></tr>
  <tr><td>Basic Format Strings</td><td>28</td></tr>
  <tr><td>Sorting and Merging</td><td>95</td></tr>
  <tr><td>Bitwise Operations</td><td>151</td></tr>
  <tr><td>String Library (reduced)</td><td>152</td></tr>
  <tr><td>String-notated bytevectors</td><td>207</td></tr>
  <tr><td>Flexible curried procedures</td><td>232</td></tr>
</table>

```
chicken-install r7rs srfi-1 srfi-4 srfi-13 srfi-27 srfi-95 srfi-151 srfi-152 srfi-207 srfi-232
```

### Startup

- Test library with `csi -R r7rs src/alchemy.scm -s src/.../library.test.scm`
- Run with `csi -R r7rs src/alchemy.scm`


## Design principles

- _Design by proofs to enable flexibility and metaprogramming_. Like Haskell, we provide the proof, the type handler, to manipulate simple data structures (numbers, lists, vectors). In this way, we can have a generic function, for example `GCD`. In this example, `GCD` requires a unique factorization domain to work, and just uses some simple operation defined on that ringspecial ring.
- _Combinators to enable the reuse of code_. For example, `applify` enable to write function composition almost like in APL.
- _Divide and conquer to enable parallelization_.

Notable portions of code are:

- `alchemy.scm` - main configuration file
- `docs/alchemy.md` - this document
- `math/algebra.scm` - algebraic system
- `cauldron/cauldron.scm` - test system
- `shared/language.scm` - macros and combinators

## Enhancement booklet

### 0.1b1

Basic functions, algebraic structures, and combinators. See [quick reference below](#quick-ref) for now.

### Planned

- LLL
- Gröbner basis
- Berlekamp's algorithm
- Berlekamp–Massey algorithm
- ECM integer factorization
- Distributed computation

## Quick reference
<span id="quick-ref"></span>


Usage: `(import (alchemy LIBRARY))`

<table>
  <tr><th>Library</th><th>Exports</th></tr>
  <tr><td>`algebra`</td><td>`range` `cartesian-product` `make-set` `s:member?` `s:cardinality` `make-monoid` `make-group` `g:identity` `g:compose` `g:inverse` `g:identity?` `` `g:got-inverse?` `` `make-ring` `r:zero` `r:add` `r:subtract` `r:negate` `r:one` `r:multiply` `r:zero?` `r:one?` `` `r:quotient` `r:modulo` `make-field` `f:inverse` `ring->multiplicative-monoid` `field->multiplicative-group` `integer-ring` `Z` `make-integer-ring-modulo` `ZZn` `real-field` `R`</td></tr>
  <tr><td>`cauldron`</td><td>`steer` `steer-taste` `steer-observe</td></tr>
  <tr><td>`encoding`</td><td>`*asciiPrintable*` `encode-grid` `pretty-print-grid` `encode-hex` `decode-hex` `encode-base64` `decode-base64` `u8vector->string` `u8vector->safe-string`</td></tr>
  <tr><td>`cipher`</td><td>`xor` `xor-byte` `xor-key`</td></tr>
  <tr><td>`graph`</td><td>`graph->dot`</td></tr>
  <tr><td>`language`</td><td>`define-curried` `applify` `compose`</td></tr>
  <tr><td>`linear-algebra`</td><td>`ma-swap-col!` `ma-pp` `v-pp` `square-linear-system` `rho` `matrix-identity` `matrix-inverse` `matrix-multiplication` `matrix-determinat` `matrix-kernel` `transpose`</td></tr>
  <tr><td>`number-theory`</td><td>`double-and-add` `square-multiply` `gcd` `xgcd` `chinese-remainder-theorem` `sum-of-two-squares?` `prime?` `kronecker` `legendreSymbol` `tonelli` `phi` `order-of-element` `modexpt` `factors` `integer-square-root` `square-test` `prime-power-test` `make-unit-group` `ZZn*`</td></tr>
  <tr><td>`polynomials`</td><td>`make-poly` `poly->string` `poly-degree` `poly+` `poly-` `poly-by-scalar` `poly*` `evaluate-polynomial` `poly-euclidean-division` `poly-gcd` `make-polynomial-ring-over-field`</td></tr>
</table>

<!-- <h2>Data dictionaries</h2> Don't want to compile this, read algebra.scm -->
