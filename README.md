# Yesod paginator

Handle a database query and/or array-math to paginate a list and produce
a pagination widget suitable for [Bootstrap][].

[bootstrap]: http://getbootstrap.com/components/#pagination

## Usage

Paginate directly out of the database:

```haskell
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

Or an existing list in memory:

```haskell
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

Just provide the pagination widget:

```haskell
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

## Customization

```haskell
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

## Example and Tests

Run a local example:

```bash
stack build --flag yesod-paginator:example
stack exec yesod-paginator-example
```

Then open [http://localhost:3000](http://localhost:3000/) in the browser.

To run the tests, execute `stack test`
