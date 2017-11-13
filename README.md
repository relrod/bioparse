# bioparse

[![Build Status](https://travis-ci.org/relrod/bioparse.svg?branch=master)](https://travis-ci.org/relrod/bioparse)

**NOTE: Don't rely on this for anything critical!**

This is a collection of parsers built on modern tooling (`parsers`, `lens`) for
dealing with biological sequencing formats such as FASTA, FastQ, and phd.

Silly benchmarks here: https://relrod.github.io/bioparse/benchmarks.html

Because we use `parsers`, bioparse will work with a variety of parser combinator
libraries without any code changes. The choice of parser combinator library is
left up to the user/call-site. `attoparsec` should be somewhat fast, but have
less useful errors than, say, something like `trifecta`.

## License

`biocore` is released under the BSD-2 license. See `LICENSE` for details.

Note that we provide instances for `biocore` classes in
[bioparse-biocore-instances](https://github.com/relrod/bioparse-biocore-instances).
`biocore` is released under the LGPL, as is `bioparse-biocore-instances`.
