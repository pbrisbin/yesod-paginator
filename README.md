# yesod-paginator

Handle a database query and/or array-math to paginate a list and produce a page
of items along with a pagination widget for navigating them.

## Usage

See the top-level [module documentation][docs].

[docs]: http://hackage.haskell.org/package/yesod-paginator/docs/Yesod-Paginator.html

## Examples

See the [example](./example/Main.hs). Run it with

```console
stack build --flag yesod-paginator:examples
stack exec yesod-paginator-example
```

## Development & Tests

```console
stack setup
stack build --pedantic --test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
