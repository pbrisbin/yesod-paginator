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
    -- note: things is [Entity a] just like selectList returns
    (things, widget) <- selectPaginated 10 [] []

    defaultLayout $ do
        [whamlet|
            $forall thing <- things
                ^{showThing $ snd thing}

            ^{widget}
            |]
~~~
