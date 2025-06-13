# fooheads stdlib

fooheads stdlib is a collection of utility functions and macros for clojure and clojurescript projects. It provides helpers for sequence manipulation, string templating, numeral conversion and more. The library is published under the coordinates `com.fooheads/stdlib`.

## Installation

Add the dependency to your `deps.edn`:

```clojure
{:deps {com.fooheads/stdlib {:mvn/version "0.4.7"}}}
```

## Usage

Refer to the namespaces under `fooheads.stdlib` and `fooheads.numeral` for the available functions. A few highlights include:

- `render-template` – simple string templating using `{key}` placeholders.
- `digits` and `digit-string` – convert numbers to and from arbitrary bases.
- Sequence helpers such as `mapt`, `filtert`, `partition-using` and more.

See the [tests](./test/fooheads) directory for additional examples.

## Development

Run the test suite with:

```bash
clojure -M:test
```

All tests should pass with exit code `0`.

## License

Distributed under the [Eclipse Public License v2.0](LICENSE).
