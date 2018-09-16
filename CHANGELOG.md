## [*Unreleased*](https://github.com/pbrisbin/yesod-paginator/compare/v1.1.0.1...master)

None

## [v1.1.0.1](https://github.com/pbrisbin/yesod-paginator/compare/v1.1.0.0...v1.1.0.1)

- Export `getCurrentPage`
- Simplify some constraints with `MonadHandler`

## [v1.1.0.0](https://github.com/pbrisbin/yesod-paginator/compare/v0.11.0...v1.1.0.0)

- Major rewrite

  Interfaces for `paginate` and `selectPaginated` now return a `Pages a` type,
  free of any visuals-related concerns. This value provides the paginated items
  and can be passed to functions from the `Widgets` model for rendering
  navigation HTML.

  The `ellipsed` widget is most like previous behavior with `simple` being a
  new, simpler alternative. These are (for the most part) not configurable, we
  should instead strive to create separate widgets if we need different
  behavior.

  Everything is now very type-safe: we use `Natural` everywhere, with `newtype`s
  to differentiate things like `PerPage`, `PageNumber`s, etc.

- Officially drop support for GHC < 7.10

## [v0.11.0](https://github.com/pbrisbin/yesod-paginator/compare/v0.10.1...v0.11.0)

- Add `simplePaginationWidget`

## [v0.10.1](https://github.com/pbrisbin/yesod-paginator/compare/v0.10.0...v0.10.1)

- Support persistent-2.5

## [v0.10.0](https://github.com/pbrisbin/yesod-paginator/compare/v0.9.1...v0.10.0)

- Require yesod-1.4 & persistent-2.0

## [v0.9.1](https://github.com/pbrisbin/yesod-paginator/compare/v0.9...v0.9.1)

- Allow text-2.0
- Require persistent-1.3

## [v0.9](https://github.com/pbrisbin/yesod-paginator/compare/v0.4.1...v0.9)

*1.0 Release Candidate*

## [v0.4.1](https://github.com/pbrisbin/yesod-paginator/compare/v0.4.0...v0.4.1)

*No changes*

## [v0.4.0](https://github.com/pbrisbin/yesod-paginator/compare/v0.3.3...v0.4.0)

- Require yesod-1.2 & persistent-1.2

## [v0.3.3](https://github.com/pbrisbin/yesod-paginator/compare/v0.3.2...v0.3.3)

- Require yesod-1.1 & persistent-1.1

## [v0.3.2](https://github.com/pbrisbin/yesod-paginator/compare/v0.3...v0.3.2)

- Relax lower bounds on yesod & persistent back to 0.10 / 0.8

## [v0.3](https://github.com/pbrisbin/yesod-paginator/compare/v0.2.3...v0.3)

- Support custom pagination widgets

## [v0.2.3](https://github.com/pbrisbin/yesod-paginator/compare/v0.2.2.1...v0.2.3)

- Require yesod-1.0 & persistent-0.9

## [v0.2.2.1](https://github.com/pbrisbin/yesod-paginator/compare/v0.2...v0.2.2.1)

- Add example

## [v0.2](https://github.com/pbrisbin/yesod-paginator/compare/v0.1.1...v0.2)

- `selectPaginated` needs to be given to `runDB` by you
- Fix disabling of next/prev links when appropriate

## [v0.1.1](https://github.com/pbrisbin/yesod-paginator/tree/v0.1.1)

First released version.
