{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}

module View.Initial where

import qualified Data.Map as M
import           Prelude hiding (div)

import           Shpadoinkle
import           Shpadoinkle.Html
import           Shpadoinkle.Router

import           Types


default (ClassList)

initialView :: ZettelController m => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
initialView model = div [class' "s11k-view-initial"] [
  reloadWidget,
  addCategoryWidget model,
  categoryList model ]


reloadWidget :: ZettelController m => HtmlM m (Zettel, InitialV)
reloadWidget = div [ class' "s11k-reload btn btn-link", onClickE reload ] [ "Reload" ]


addCategoryWidget :: ZettelController m => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
addCategoryWidget model@(_,i) = div [class' "s11k-add-category form-group row"] [
  input [ class' "form-control col-sm-9", ("type", "text"), onSubmitE handleNewCategory
        , onInput (setNewCategoryTitle model)
        , ("value", textProp (newCategoryTitle i)) ] [],
  button [ class' "form-control col btn btn-primary", onClickE handleNewCategory ] [ text "New Category" ] ]


categoryList :: ZettelController m => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
categoryList model = div [class' "s11k-category-list"]
                     $ categorySummary model <$> M.elems (categories (fst model))


categorySummary :: ZettelController m => (Zettel, InitialV) -> Category -> HtmlM m (Zettel, InitialV)
categorySummary model cat = div [class' "s11k-category-summary mb-3"] [
  h2 [class' "s11k-category-title"] [ text (categoryTitle cat) ],
  addThreadWidget model cat,
  div [class' "s11k-thread-summaries row"] (threadSummary model <$> categoryThreads (fst model) cat) ]


addThreadWidget :: ZettelController m => (Zettel, InitialV) -> Category -> HtmlM m (Zettel, InitialV)
addThreadWidget model cat = div [class' "s11k-add-thread row form-group"] [
  input [ class' "form-control col-sm-9", ("type", "text"), onSubmitE (handleNewThread cat)
        , onInput (setNewThreadTitle model cat)
        , ("value", textProp (getNewThreadTitle model cat)) ] [],
  button [ class' "form-control col-sm-3 btn btn-primary", onClickE (handleNewThread cat) ] [ text "New Thread" ] ]


threadSummary :: ZettelController m => (Zettel, InitialV) -> Thread -> HtmlM m (Zettel, InitialV)
threadSummary _ t = div [ class' "s11k-thread-summary col-sm-4 col-md-3 col-lg-2 mb-2"
                        , onClickM_ $ navigate @SPA (ThreadRoute (threadId t)) ]
                        [ text (threadTitle t) ]

