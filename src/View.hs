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


viewRouter :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
           => Route -> IO (HtmlM m Model)
viewRouter r = let model = initialModel r (Zettel mempty mempty)
  in view . ($ model) <$> runContinuation (router r) model


view :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
     => Model -> HtmlM m Model
view = pimap coproductIsoModel . viewCases . modelToCoproduct


viewCases :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
          => ModelCoproduct -> HtmlM m ModelCoproduct
viewCases = initialView `eitherH` threadView


initialView :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
            => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
initialView model = div_ [
  reloadWidget,
  addCategoryWidget model,
  categoryList model ]


threadView :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
           => (Zettel, ThreadV) -> HtmlM m (Zettel, ThreadV)
threadView model@(z, v) =
  let t = viewedThread v
  in div_ $ [
    backToInitial,
    div_ [ text (threadTitle t) ],
    div_ ( text "Categories: "
           : ( div_ . (:[]) . text <$> catMaybes (categoryIdTitle z <$> categorization t) ) ),
    div_ ( text "Links: " :  (linkView <$> links t) ),
    addCommentWidget model,
    div_ ( text "Comments: " :  (commentView <$> comments t) ) ]


-- initialView pieces


reloadWidget :: Monad m => ZettelEditor m => HtmlM m (Zettel, InitialV)
reloadWidget = div [ onClickE reload ] [ "Reload" ]


addCategoryWidget :: MonadUnliftIO m => ZettelEditor m
                  => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
addCategoryWidget model@(_,i) = span_ [
  input [ ("type", "text"), onSubmitE addCategory
        , onInput (setNewCategoryTitle model)
        , ("value", textProp (newCategoryTitle i)) ] [],
  button [ onClickE addCategory ] [ text "New Category" ] ]


categoryList :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
             => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
categoryList model = div_ $ categorySummary model <$> M.elems (categories (fst model))


categorySummary :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
                => (Zettel, InitialV) -> Category -> HtmlM m (Zettel, InitialV)
categorySummary model cat = div_ (
  div_ [ text (categoryTitle cat) ] :
  addThreadWidget model cat :
  (threadSummary model <$> categoryThreads (fst model) cat) )


addThreadWidget :: MonadUnliftIO m => ZettelEditor m
                => (Zettel, InitialV) -> Category -> HtmlM m (Zettel, InitialV)
addThreadWidget model cat = span_ [
  input [ ("type", "text"), onSubmitE (addThread cat)
        , onInput (setNewThreadTitle model cat)
        , ("value", textProp (getNewThreadTitle model cat)) ] [],
  button [ onClickE (addThread cat) ] [ text "New Thread" ] ]


threadSummary :: MonadJSM m => (Zettel, InitialV) -> Thread -> HtmlM m (Zettel, InitialV)
threadSummary _ t = div [ onClickM_ $ navigate @SPA (ThreadRoute (threadId t)) ]
                        [ text (threadTitle t) ]


linkView :: Monad m => Link -> HtmlM m (Zettel, ThreadV)
linkView l = div_ [ text (linkDescription l) ]


-- threadView pieces


backToInitial :: MonadJSM m => HtmlM m (Zettel, ThreadV)
backToInitial = div [ onClickM_ (navigate @SPA InitialRoute) ] [ text "Back" ]


addCommentWidget :: MonadUnliftIO m => ZettelEditor m
                 => (Zettel, ThreadV) -> HtmlM m (Zettel, ThreadV)
addCommentWidget model@(_,v) = span_ [
  input' [ ("type", "text"), onSubmitE addComment
         , onInput (setNewComment model), ("value", textProp (newComment v)) ],
  button [ onClickE addComment ] [ text "Add Comment" ] ]


commentView :: Monad m => Text -> HtmlM m (Zettel, ThreadV)
commentView = div_ . (:[]) . text


-- template


template :: Monad m => Text -> HtmlM m a -> HtmlM m a
template js v = html_
  [ head_
    [ link' [ rel "stylesheet", href "https://cdn.usebootstrap.com/bootstrap/4.3.1/css/bootstrap.min.css" ]
    , meta [ charset "ISO-8859-1" ] []
    , script [ type' "text/javascript" ] [ text js ] ]
  , body_ [ v ] ]
