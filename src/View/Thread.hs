{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}


module View.Thread where

import qualified Data.Map as M
import           Prelude hiding (div)
import           Data.Maybe (catMaybes)
import           Data.Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)

import           Shpadoinkle
import           Shpadoinkle.Html
import           Shpadoinkle.Router

import           Types

default (ClassList)


threadView :: ZettelController m => (Zettel, ThreadV) -> HtmlM m (Zettel, ThreadV)
threadView model@(z, v) =
  case M.lookup (viewedThread v) (threads z) of
    Just t ->
      div [class' "s11k-view-thread"] $ [
        backToInitial,
          div [class' "s11k-categories row"]
        ( h2 [class' "s11k-category col"] . (:[]) . text
          <$> catMaybes (categoryIdTitle z <$> categorization t) ),
        h3 [class' "s11k-thread-title"] [ text (threadTitle t) ],
        div [class' "s11k-thread-author"] [ text (unUserId (threadAuthor t)) ],
        div [class' "s11k-thread-created"] [ text (dateView (threadCreated t)) ],
        --div [class' "s11k-links row"] (linkView <$> links t),
        addCommentWidget model,
        div [class' "s11k-comments"] (commentDispatch model <$> threadComments z t) ]


backToInitial :: MonadJSM m => HtmlM m (Zettel, ThreadV)
backToInitial = div [ class' "s11k-back btn btn-link"
                    , onClickM_ (navigate @SPA InitialRoute) ] [ text "Back" ]


addCommentWidget :: ZettelController m => (Zettel, ThreadV) -> HtmlM m (Zettel, ThreadV)
addCommentWidget model@(_,v) = div [class' "s11k-add-comment form-group"] [
  textarea' [ class' "form-control", ("rows", "4"), ("cols", "70"), onSubmitE handleNewComment
            , onInput (setCommentField model), ("value", textProp (commentField v)) ],
  button [ class' "form-control btn btn-primary", onClickE handleNewComment ] [ text "Add Comment" ] ]


commentDispatch :: ZettelController m => (Zettel, ThreadV) -> Comment -> HtmlM m (Zettel, ThreadV)
commentDispatch model@(z, t) c = 
  case (editCommentField t) of
    (Just (i, t)) -> if (commentId c == i) then commentEditor model i t c else commentView c   
    Nothing       -> commentView c

commentEditor :: ZettelController m => (Zettel, ThreadV) 
                                    -> CommentId
                                    -> Text
                                    -> Comment 
                                    -> HtmlM m (Zettel, ThreadV)
commentEditor model i t c = div [class' "s11k-add-comment form-group" ] [ 
    textarea' [ class' "form-control", ("rows", "4"), ("cols", "70"), onSubmitE handleSaveEdit
              , onInput (setEditCommentField model), ("value", textProp t) ],
    div [class' "s11k-comment-metadata"]
      [ strong [] . (:[]) . text $ "- " <> unUserId (commentAuthor c)
        <> ", " <> dateView (commentCreated c) ],
    button [ class' "form-control btn", onClickE handleSaveEdit ] [ text "Save Comment" ] ]

commentView :: ZettelController m => Comment -> HtmlM m (Zettel, ThreadV)
commentView c = div [class' "s11k-comment mb-2"] [
  div [class' "s11k-comment-text mb-1"] [ text (commentText c) ],
  div [class' "s11k-comment-metadata"]
    [ strong [] . (:[]) . text $ "- " <> unUserId (commentAuthor c)
      <> ", " <> dateView (commentCreated c)
    , button [ class' "form-control btn", onClickE (handleOpenEdit $ commentId c)  ] [ text "Edit Comment" ] ] ]

dateView :: UTCTime -> Text
dateView = pack . (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))) 
