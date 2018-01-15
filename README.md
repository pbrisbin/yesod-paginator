# Yesod paginator

Handle a database query and/or array-math to paginate a list and produce a
pagination widget suitable for [Bootstrap][].

[bootstrap]: http://getbootstrap.com/components/#pagination

## Usage

### DB Entities

```hs
getPageR :: Handler Html
getPageR = do
    (things, widget) <- runDB $ selectPaginated 10 [] []

    defaultLayout $ do
        [whamlet|
            $forall thing <- things
                ^{showThing $ snd thing}

            ^{widget}
            |]
```

### Pure List

```hs
getPageR :: Handler Html
getPageR = do
    things' <- getAllThings

    (things, widget) <- paginate 10 things'

    defaultLayout $ do
        [whamlet|
            $forall thing <- things
                ^{showThing thing}

            ^{widget}
            |]
```

### Pre-paginated

```hs
getPageR :: Handler Html
getPageR = do
    cur <- getCurrentPage

    let limit = 10

    (items, total) <- runSphinxSearch "query" limit

    defaultLayout $ do
        [whamlet|
          $forall thing <- things
              ^{showThing thing}

          ^{defaultWidget cur limit total}
          |]
```

### Customization

```hs
getPageR :: Handler Html
getPageR = do
    (things, widget) <- selectPaginatedWith myWidget 10 [] []

    defaultLayout $ do
        -- ...

    where
        myWidget :: PageWidget App
        myWidget = paginationWidget $ PageWidgetConfig
            { prevText     = "Newer"
            , nextText     = "Older"
            , pageCount    = 7
            , ascending    = False
            , showEllipsis = False
            , listClasses  = ["pagination", "pagination-centered"]
            }
```

## Development & Tests

```console
stack setup
stack build --pedantic --test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
