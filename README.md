# Alchemy

Alchemy is an extensible computer algebra library written in [portable R7RS Scheme](https://docs.scheme.org/guide/portable-r7rs/)<sup>2</sup>. Its aim is to provide a meaningful way to express and use mathematical objects. A notable goal it tries to achieve is breaking cryptography without fighting the library as it should be as easy as parsing the data and calling a function, not harder. It tries to be readable and portable. Due to Scheme nature, it is easy to extend.

The rationale behind this are [building my own birdfeeder](https://raw.githubusercontent.com/angea/pocorgtfo/master/contents/articles/02-02.pdf) and I do not really like Python, Sagemath, MAGMA, et al.'s approach. By other means, luck is when preparation meets opportunity. Then, I hope this code publication is one of [Type II](http://www.loper-os.org/?p=4012#selection-106.0-106.2). Moreover, this project is meant to be a continuous endeavour based on my knowledge.

_Read more in the documentation._

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
chicken-install r7rs srfi-1 srfi-4 srfi-13 srfi-27 srfi-95 srfi-151 srfi-152 srfi-207 srfi-232 linenoise trace
```

It is also suggested to create a config file `~/.csirc`.

```
(import linenoise)
(current-input-port (make-linenoise-port))
```

### Startup

- Test library with `csi -R r7rs src/alchemy.scm -s src/.../library.test.scm`
- Run with `csi -R r7rs src/alchemy.scm`

## Enhancement booklet

### 0.1b1

Basic functions, algebraic structures, and combinators. See documentation.

### Planned

- Berlekamp's algorithm for polynomial factorization
- Berlekamp–Massey algorithm
- Distributed computation

