{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module View where


import           Control.Monad.IO.Class
import           Control.PseudoInverseCategory (piiso)
import           Control.ShpadoinkleContinuation (runContinuation)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Text
import           Data.Time.Calendar
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


viewRouter :: HasZettelHandlers m => Route -> IO (HtmlM m Model)
viewRouter r = let model = initialModel r emptyZettel
  in view . ($ model) <$> runContinuation (router r) model


view :: HasZettelHandlers m => Model -> HtmlM m Model
view model = viewContainer model . pimap coproductIsoModel . viewCases $ modelToCoproduct model


viewContainer :: Monad m => Model -> HtmlM m a -> HtmlM m a
viewContainer model v =
  div [class' "container-fluid s11k-app"] [
      h1_ [ "Shpadoinklekasten" ],
      div [class' "s11k-login-status"] [ text $ maybe "Not logged in" (("Logged in as " <>) . unUserId) (whoAmI (fst model)) ],
      div [class' "s11k-view"] [ v ] ]


viewCases :: HasZettelHandlers m => ModelCoproduct -> HtmlM m ModelCoproduct
viewCases = initialView `eitherH` threadView `eitherH` loginView


initialView :: HasZettelHandlers m => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
initialView model = div [class' "s11k-view-initial"] [
  reloadWidget,
  addCategoryWidget model,
  categoryList model ]


threadView :: HasZettelHandlers m => (Zettel, ThreadV) -> HtmlM m (Zettel, ThreadV)
threadView model@(z, v) =
  let t = viewedThread v
  in div [class' "s11k-view-thread"] $ [
    backToInitial,
    div [class' "s11k-categories row"]
    ( h2 [class' "s11k-category col"] . (:[]) . text
      <$> catMaybes (categoryIdTitle z <$> categorization t) ),
    h3 [class' "s11k-thread-title"] [ text (threadTitle t) ],
    div [class' "s11k-thread-author"] [ text (unUserId (threadAuthor t)) ],
    div [class' "s11k-thread-created"] [ text (dateView (threadCreated t)) ],
    --div [class' "s11k-links row"] (linkView <$> links t),
    addCommentWidget model,
    div [class' "s11k-comments"] (commentView <$> threadComments z t) ]


loginView :: HasZettelHandlers m => (Zettel, LoginV) -> HtmlM m (Zettel, LoginV)
loginView model@(z,l) =
  div [class' "s11k-login form-group"] [
    input' [ class' "form-control", type' "text", placeholder "User ID", onInput (setUserId model) ],
    input' [ class' "form-control", type' "password", placeholder "Password", onInput (setPassword model) ],
    button [ class' "btn btn-primary", onClickE handleLogin ] [ text "Login" ] ]


-- initialView pieces


reloadWidget :: HasZettelHandlers m => HtmlM m (Zettel, InitialV)
reloadWidget = div [ class' "s11k-reload btn btn-link", onClickE reload ] [ "Reload" ]


addCategoryWidget :: HasZettelHandlers m => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
addCategoryWidget model@(_,i) = div [class' "s11k-add-category form-group row"] [
  input [ class' "form-control col-sm-9", ("type", "text"), onSubmitE handleNewCategory
        , onInput (setNewCategoryTitle model)
        , ("value", textProp (newCategoryTitle i)) ] [],
  button [ class' "form-control col btn btn-primary", onClickE handleNewCategory ] [ text "New Category" ] ]


categoryList :: HasZettelHandlers m => (Zettel, InitialV) -> HtmlM m (Zettel, InitialV)
categoryList model = div [class' "s11k-category-list"]
                     $ categorySummary model <$> M.elems (categories (fst model))


categorySummary :: HasZettelHandlers m => (Zettel, InitialV) -> Category -> HtmlM m (Zettel, InitialV)
categorySummary model cat = div [class' "s11k-category-summary mb-3"] [
  h2 [class' "s11k-category-title"] [ text (categoryTitle cat) ],
  addThreadWidget model cat,
  div [class' "s11k-thread-summaries row"] (threadSummary model <$> categoryThreads (fst model) cat) ]


addThreadWidget :: HasZettelHandlers m => (Zettel, InitialV) -> Category -> HtmlM m (Zettel, InitialV)
addThreadWidget model cat = div [class' "s11k-add-thread row form-group"] [
  input [ class' "form-control col-sm-9", ("type", "text"), onSubmitE (handleNewThread cat)
        , onInput (setNewThreadTitle model cat)
        , ("value", textProp (getNewThreadTitle model cat)) ] [],
  button [ class' "form-control col-sm-3 btn btn-primary", onClickE (handleNewThread cat) ] [ text "New Thread" ] ]


threadSummary :: HasZettelHandlers m => (Zettel, InitialV) -> Thread -> HtmlM m (Zettel, InitialV)
threadSummary _ t = div [ class' "s11k-thread-summary col-sm-4 col-md-3 col-lg-2 mb-2"
                        , onClickM_ $ navigate @SPA (ThreadRoute (threadId t)) ]
                        [ text (threadTitle t) ]


-- threadView pieces


backToInitial :: MonadJSM m => HtmlM m (Zettel, ThreadV)
backToInitial = div [ class' "s11k-back btn btn-link"
                    , onClickM_ (navigate @SPA InitialRoute) ] [ text "Back" ]


addCommentWidget :: HasZettelHandlers m => (Zettel, ThreadV) -> HtmlM m (Zettel, ThreadV)
addCommentWidget model@(_,v) = div [class' "s11k-add-comment form-group"] [
  textarea' [ class' "form-control", ("rows", "4"), ("cols", "70"), onSubmitE handleNewComment
            , onInput (setCommentField model), ("value", textProp (commentField v)) ],
  button [ class' "form-control btn btn-primary", onClickE handleNewComment ] [ text "Add Comment" ] ]


{-linkView :: MonadJSM m => Link -> HtmlM m (Zettel, ThreadV)
linkView l = div [class' "col s11k-link"]
             [ a [class' "btn btn-link", onClickM_ $ navigate @SPA (ThreadRoute (linkTo l))]
               [text (linkDescription l)] ]-}


commentView :: Monad m => Comment -> HtmlM m (Zettel, ThreadV)
commentView c = div [class' "s11k-comment mb-2"] [
  div [class' "s11k-comment-text mb-1"] [ text (commentText c) ],
  div [class' "s11k-comment-metadata"]
    [ strong [] . (:[]) . text $ "- " <> unUserId (commentAuthor c)
      <> ", " <> dateView (commentCreated c) ] ]


-- Shared pieces
dateView :: Day -> Text
dateView = pack . (\(y, m, d) -> show y <> "-" <> show m <> "-" <> show d) . toGregorian
