# Megaparsec tests

Megaparsec's test suite as a standalone package. The reason for the
separation is that we can avoid circular dependency on `hspec-megaparsec`
and thus avoid keeping copies of its source files in our test suite, as we
had to do before. Another benefit is that we can export some auxiliary
functions in `megaparsec-tests` which can be used by other test suites, for
example in the `parser-combinators-tests` package.

Version of `megaparsec-tests` will be kept in sync with versions of
`megaparsec` from now on.

## License

Copyright © 2015–2019 Megaparsec contributors

Distributed under FreeBSD license.
