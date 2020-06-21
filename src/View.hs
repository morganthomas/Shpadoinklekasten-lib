{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module View where


import           Control.Monad.IO.Class
import           Control.PseudoInverseCategory (piiso)
import           Control.ShpadoinkleContinuation (runContinuation)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Text
import           Shpadoinkle
import           Shpadoinkle.Html
import           Shpadoinkle.Router
import           UnliftIO
import           Prelude hiding (div)

import           Types


template :: Monad m => Text -> HtmlM m a -> HtmlM m a
template js v = html_
  [ head_
    [ link' [ rel "stylesheet", href "https://cdn.usebootstrap.com/bootstrap/4.3.1/css/bootstrap.min.css" ]
    , meta [ charset "ISO-8859-1" ] []
    , script [ type' "text/javascript" ] [ text js ] ]
  , body_ [ v ] ]


viewRouter :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
           => Route -> IO (HtmlM m Model)
viewRouter r = let model = initialModel r (Zettel mempty mempty)
  in view . ($ model) <$> runContinuation (router r) model


view :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
     => Model -> HtmlM m Model
view = viewContainer . pimap coproductIsoModel . viewCases . modelToCoproduct


viewContainer :: Monad m =>  HtmlM m a -> HtmlM m a
viewContainer v =
  div [class' "container-fluid s11k-app"] [
      h1_ [ "Shpadoinklekasten" ],
      div [class' "view"] [ v ] ]


viewCases :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
          => ModelCoproduct -> HtmlM m ModelCoproduct
viewCases = initialView `eitherH` threadView


initialView :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
            => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
initialView model = div [class' "s11k-view-initial"] [
  reloadWidget,
  addCategoryWidget model,
  categoryList model ]


threadView :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
           => (Zettel, ThreadV) -> HtmlM m (Zettel, ThreadV)
threadView model@(z, v) =
  let t = viewedThread v
  in div [class' "s11k-view-thread"] $ [
    backToInitial,
    div [class' "s11k-thread-title"] [ text (threadTitle t) ],
    div [class' "s11k-categories row"] ( text "Categories: "
           : ( div [class' "s11k-category col"] . (:[]) . text
               <$> catMaybes (categoryIdTitle z <$> categorization t) ) ),
    div [class' "s11k-links row"] ( text "Links: " :  (linkView <$> links t) ),
    addCommentWidget model,
    div [class' "s11k-comments"] ( text "Comments: " :  (commentView <$> comments t) ) ]


-- initialView pieces


reloadWidget :: Monad m => ZettelEditor m => HtmlM m (Zettel, InitialV)
reloadWidget = div [ class' "s11k-reload", onClickE reload ] [ "Reload" ]


addCategoryWidget :: MonadUnliftIO m => ZettelEditor m
                  => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
addCategoryWidget model@(_,i) = div [class' "s11k-add-category form-group row"] [
  input [ class' "form-control col-sm-9", ("type", "text"), onSubmitE addCategory
        , onInput (setNewCategoryTitle model)
        , ("value", textProp (newCategoryTitle i)) ] [],
  button [ class' "form-control col button btn-primary", onClickE addCategory ] [ text "New Category" ] ]


categoryList :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
             => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
categoryList model = div [class' "s11k-category-list"]
                     $ categorySummary model <$> M.elems (categories (fst model))


categorySummary :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
                => (Zettel, InitialV) -> Category -> HtmlM m (Zettel, InitialV)
categorySummary model cat = div [class' "s11k-category-summary"] [
  div [class' "s11k-category-title"] [ text (categoryTitle cat) ],
  addThreadWidget model cat,
  div [class' "s11k-thread-summaries row"] (threadSummary model <$> categoryThreads (fst model) cat) ]


addThreadWidget :: MonadUnliftIO m => ZettelEditor m
                => (Zettel, InitialV) -> Category -> HtmlM m (Zettel, InitialV)
addThreadWidget model cat = div [class' "s11k-add-thread form-group"] [
  input [ class' "form-control col-sm-9", ("type", "text"), onSubmitE (addThread cat)
        , onInput (setNewThreadTitle model cat)
        , ("value", textProp (getNewThreadTitle model cat)) ] [],
  button [ class' "form-control col button btn-primary", onClickE (addThread cat) ] [ text "New Thread" ] ]


threadSummary :: MonadJSM m => (Zettel, InitialV) -> Thread -> HtmlM m (Zettel, InitialV)
threadSummary _ t = div [ class' "s11k-thread-summary col"
                        , onClickM_ $ navigate @SPA (ThreadRoute (threadId t)) ]
                        [ text (threadTitle t) ]


-- threadView pieces


backToInitial :: MonadJSM m => HtmlM m (Zettel, ThreadV)
backToInitial = div [ class' "s11k-back", onClickM_ (navigate @SPA InitialRoute) ] [ text "Back" ]


addCommentWidget :: MonadUnliftIO m => ZettelEditor m
                 => (Zettel, ThreadV) -> HtmlM m (Zettel, ThreadV)
addCommentWidget model@(_,v) = div [class' "s11k-add-comment form-group"] [
  textarea' [ class' "form-control", ("rows", "4"), ("cols", "70"), onSubmitE addComment
            , onInput (setNewComment model), ("value", textProp (newComment v)) ],
  button [ class' "form-control button btn-primary", onClickE addComment ] [ text "Add Comment" ] ]


linkView :: Monad m => Link -> HtmlM m (Zettel, ThreadV)
linkView l = div [class' "col s11k-link"] [ text (linkDescription l) ]


commentView :: Monad m => Text -> HtmlM m (Zettel, ThreadV)
commentView = div [class' "s11k-comment"] . (:[]) . text
