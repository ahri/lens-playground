{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, RankNTypes, TypeApplications, TypeFamilies, InstanceSigs #-}

import Control.Lens
    ( (&),
      _head,
      has,
      (^.),
      makePrisms,
      lens,
      only,
      outside,
      prism',
      (%~),
      (.~),
      Lens',
      Prism' )
import Control.Applicative ()
import Data.Char ()
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "Hello world!"

type Path = [String]
type Body = String
data Request
    = Post Path Body
    | Get Path
    | Delete Path
    deriving Show
makePrisms ''Request

path :: Lens' Request Path
path = lens getter setter
  where
    getter (Post p body)   = p
    getter (Get p)         = p
    getter (Delete p)      = p
    setter (Post _ body) p = Post p body
    setter (Get _) p       = Get p
    setter (Delete _) p    = Delete p

-- Default server handler:
serveRequest :: Request -> String
serveRequest _ = "404 Not Found"

_PathPrefix :: String -> Prism' Request Request
_PathPrefix prefix = prism' embed match
  where
    -- Add the prefix to the path
    embed :: Request -> Request
    embed req = req & path %~ (prefix :)

    -- Check if the prefix matches the path
    match :: Request -> Maybe Request
    match req
        | has (path . _head . only prefix) req = Just (req & path %~ drop 1)
    match _ = Nothing

userHandler :: Request -> String
userHandler req = "User handler! Remaining path: " <> L.intercalate "/" (req ^. path)

postsHandler :: Request -> String
postsHandler = const "404 Not Found"
    & outside _Post
        .~ (\(path', body) -> "Created post with body: " <> body)
    & outside _Get
        .~ (\path' -> "Fetching post at path: " <> L.intercalate "/" path')
    & outside _Delete
        .~ (\path' -> "Deleting post at path: " <> L.intercalate "/" path')

server :: Request -> String
server = serveRequest
    & outside (_PathPrefix "users") .~ userHandler
    & outside (_PathPrefix "posts") .~ const "Posts Handler!"
    & outside (_PathPrefix "posts") .~ postsHandler