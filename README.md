# Yesod paginator

~~~ { .haskell }
import Yesod.Paginator
~~~

Do you have some `[Thing]` that you've already selected out of your DB, 
maybe composed from multiple tables, and you just want to paginate this 
big list?

~~~ { .haskell }
getPageR :: Handler RepHtml
getPageR = do
    things' <- getAllThings

    -- note: things will be the same type as things'
    (things, widget) <- paginate 10 things'

    defaultLayout $ do
        [whamlet|
            $forall thing <- things
                ^{showThing thing}

            ^{widget}
            |]
~~~

Do you have a single table of records and you want to paginate them, 
selecting only the records needed to display the current page?

~~~ { .haskell }
getPageR :: Handler RepHtml
getPageR = do
    (things, widget) <- runDB $ selectPaginated 10 [] []

    defaultLayout $ do
        [whamlet|
            $forall thing <- things
                ^{showThing $ snd thing}

            ^{widget}
            |]
~~~

### Notes:

`selectPaginated` can be thought of a direct drop-in for `selectList`. 
This means it can be thrown inside any existing `runDB` block. Example 
[here][tags]

[tags]: https://github.com/pbrisbin/devsite/blob/master/Handler/Tags.hs#L17

`paginationWidget` is also available if you've already got more 
complicated database code doing the record pagination but you just want 
the pretty links. Example [here][widget].

[widget]: https://github.com/pbrisbin/renters-reality/blob/master/Helpers/Search.hs#L54

### Testing

~~~
$ cabal install
$ runhaskell Test.hs
$ $BROWSER http://localhost:3000
~~~
