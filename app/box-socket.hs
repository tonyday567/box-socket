{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-

It's a box. It's a socket.

-}

module Main where

import Box
import Box.Socket
import Web.Page
import Lucid hiding (with)
import qualified Lucid as L
import Control.Lens hiding (Wrapped, Unwrapped)
import Data.Generics.Labels ()
import NumHask.Prelude hiding (STM, bracket)
import Options.Generic
import Text.InterpolatedString.Perl6
import qualified Data.Text as Text
import Data.HashMap.Strict as HashMap
import Web.Page.Examples
import qualified Data.Attoparsec.Text as A
import Control.Monad.Conc.Class (MonadConc)
import Control.Concurrent.Classy.Async as C

testServe :: Box IO Text Text -> IO ()
testServe b =
  serveBox defaultSocketConfig testPage b

testC :: Cont IO (Committer IO Text)
testC = pure toStdout

testE :: Cont IO (Emitter IO Text)
testE =
  fmap code <$>
   fromListE initInputs <>
   pure (mapE (pure . Just . either Eval id) (readE fromStdin))

testB :: Cont IO (Box IO Text Text)
testB = Box <$> testC <*> testE

-- TODO: convert to shared version
sharedServe' :: IO ()
sharedServe' =
  serveBox defaultSocketConfig testPage <$.>
  (Box <$> c <*> e)
  where
    c = pure toStdout
    e = fmap code <$>
      fromListE initCode <>
      pure (mapE (pure . Just . either Eval id) (readE fromStdin))
    initCode =
      [ Console [q|"sharedRep test!"|],
        Replace' "input" $ toText $ toHtml rangeTest <> toHtml textTest
      ]

loop :: (MonadConc m, MonadIO m) => SharedRep m a -> (a -> Text) -> Box m [Code] (Text, Text) -> m ()
loop sr out (Box c e) = flip evalStateT (0, HashMap.empty) $ do
  -- you only want to run unrep once for a SharedRep
  (Rep h fa) <- unrep sr
  b <- lift $ commit c (inputCode h)
  s <- get
  lift $ putStrLn $ "pre outputCode entry" <> (show s :: Text)
  o <- outputCode fa out
  b' <- lift $ commit c o
  when (b && b') (go fa)
  where
    go fa = do
      incoming <- lift $ emit e
      putStrLn $ "incoming " <> (show incoming :: Text)
      preUpdate <- get
      putStrLn $ "preUpdate " <> (show preUpdate :: Text)
      modify (updateS incoming)
      postUpdate <- get
      putStrLn $ "postUpdate " <> (show postUpdate :: Text)
      o <- outputCode fa out
      putStrLn $ "output" <> (show o :: Text)
      b <- lift $ commit c o
      when b (go fa)

inputCode :: Html () -> [Code]
inputCode h = [Append' "input" (toText h)]

outputCode :: (MonadIO m, MonadConc m) => (HashMap Text Text -> (HashMap Text Text, Either Text a)) -> (a -> Text) -> StateT (Int, HashMap Text Text) m [Code]
outputCode fa out = do
  s <- get
  sleep 1
  putStrLn ("preOutputCode" <> (show s :: Text))
  let (m', ea) = fa (snd s)
  modify (second (const m'))
  s <- get
  putStrLn ("postOutputCode" <> (show s :: Text))
  pure $
    [ case ea of
        Left err -> Append' "debug" err
        Right a -> Replace' "output" (out a)
    ]

loop' :: (MonadIO m, MonadConc m) => SharedRep m a -> (a -> Text) -> Box m Text Text -> m ()
loop' sr out b = loop sr out (wrangle b)

-- | connect up two box actions via two queues
fuseActions :: (MonadConc m) => (Box m a b -> m r) -> (Box m b a -> m r') -> m r'
fuseActions abm bam = do
  (Box ca ea, _) <- toBoxM Unbounded
  (Box cb eb, _) <- toBoxM Unbounded
  concurrentlyRight (abm (Box ca eb)) (bam (Box cb ea))

-- | turn a box action into a box continuation
fromAction :: (MonadConc m) => (Box m a b -> m r) -> Cont m (Box m b a)
fromAction baction = Cont $ fuseActions baction

shareTest :: IO ()
shareTest = testServe <$.> fromAction (loop' repExamples show)

-- | {"event":{"element":"textid","value":"abcdees"}}
parserJ :: A.Parser (Text,Text)
parserJ = do
  _ <- A.string [q|{"event":{"element":"|]
  e <- A.takeTill (=='"')
  _ <- A.string [q|","value":"|]
  v <- A.takeTill (=='"')
  _ <- A.string [q|"}}|]
  pure (e,v)

wrangle :: Monad m => Box m Text Text -> Box m [Code] (Text,Text)
wrangle (Box c e) = Box c' e'
  where
    c' = listC $ contramap code c
    e' = mapE (pure . either (const Nothing) Just) (parseE parserJ e)

listC :: (Monad m) => Committer m a -> Committer m [a]
listC c = Committer $ \as ->
  any id <$> (sequence $ commit c <$> as)

updateS :: (Maybe (Text,Text)) -> (Int, HashMap Text Text) -> (Int, HashMap Text Text)
updateS Nothing s = s
updateS (Just (k,v)) s = second (insert k v) s


-- * page elements
testPage :: Page
testPage =
  bootstrapPage &
  #htmlBody
      .~ divClass_
        "container"
        ( mconcat
            [ divClass_ "row" (h1_ "box bridge"),
              divClass_ "row" $ mconcat $ (\(t, h) -> divClass_ "col" (h2_ (toHtml t) <> L.with div_ [id_ t] h)) <$> sections
            ]
        ) &
   #jsOnLoad .~ webSocket' <> PageJsText oldjsb <> runScriptJs <> refreshJsbJs
  where
    sections = 
      [ ("input", mempty),
        ("output", mempty)
      ]

-- | bridge testing without the SharedRep method
rangeTest :: Input Int
rangeTest =
  Input
    3
    (Just "range example")
    "rangeid"
    ( Slider
        [ style_ "max-width:15rem;",
          min_ "0",
          max_ "5",
          step_ "1"
        ]
    )

textTest :: Input Text
textTest =
  Input
    "abc"
    (Just "label")
    "textid"
    TextBox

-- * code messaging
data Code =
  Replace' Text Text |
  Append' Text Text |
  Console Text |
  Eval Text |
  Val Text
  deriving (Eq, Show, Generic, Read)

code :: Code -> Text
code (Replace' i t) = replace' i t
code (Append' i t) = append' i t
code (Console t) = console t
code (Eval t) = t
code (Val t) = val t

initInputs :: [Code]
initInputs =
  [ Console [q|"box is woke!"|],
    Replace' "input" $ toText $ toHtml rangeTest <> toHtml textTest
  ]

console :: Text -> Text
console t = [qc| console.log({t}) |]

val :: Text -> Text
val t = [qc| jsb.ws.send({t}) |]

alert :: Text -> Text
alert a = [qc| alert("{a}")|]

-- | replace a container and run any embedded scripts
replace' :: Text -> Text -> Text
replace' d t =
      [qc|
     var $container = document.getElementById('{d}');
     $container.innerHTML = '{clean t}';
     runScripts($container);
     refreshJsb();
     |]

-- | append to a container and run any embedded scripts
append' :: Text -> Text -> Text
append' d t =
      [qc|
     var $container = document.getElementById('{d}');
     $container.innerHTML += '{clean t}';
     runScripts($container);
     refreshJsb();
     |]

clean :: Text -> Text
clean =
  Text.intercalate "\\'" . Text.split (== '\'')
    . Text.intercalate "\\n"
    . Text.lines

-- * initial javascript
-- | create a web socket for event data
webSocket' :: PageJs
webSocket' =
  PageJsText
    [q|
window.jsb = {ws: new WebSocket('ws://' + location.host + '/')};
jsb.ws.onmessage = function (evt) {
  console.log(evt.data);
  eval(evt.data);
  };
|]

oldjsb :: Text
oldjsb = [q|
jsb.event =  function(ev) {
         if (jsb.debug) { console.log('event',{event: ev}); }
         jsb.ws.send(JSON.stringify({event: ev}));
   };
jsb.error = function(n,err) {
         if (jsb.debug) { console.log('send',{id: n, error: err}); }
         jsb.ws.send(JSON.stringify({id: n, error: err}));
         throw(err);
   };
jsb.reply = function(n,obj) {
       Promise.all(obj).then(function(obj){
         if (jsb.debug) { console.log('reply',{id:n, result:obj}); }
         jsb.ws.send(JSON.stringify({id: n, result: obj}));
       }).catch(function(err){
         jsb.error(n,err);
       });
   };
jsb.ws.onmessage = function(evt){ 
   if (jsb.debug) { console.log('eval',evt.data); }
   eval('(function(){' + evt.data + '})()');
};
jsb.rs = [];

|]

-- old shit

data SocketType = Serve | Client | Responder | TestRun deriving (Eq, Read, Show, Generic)

instance ParseField SocketType

instance ParseRecord SocketType

instance ParseFields SocketType

data Opts w
  = Opts
      { apptype :: w ::: SocketType <?> "type of websocket app"
      }
  deriving (Generic)

instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "example websocket apps"
  r :: Text <- case apptype o of
    Client -> show <$> clientIO
    Responder -> show <$> q' serverIO
    TestRun -> show <$> testRun
    Serve -> show <$> (testServe <$.> testB)
  putStrLn r

serverIO :: IO ()
serverIO = runServer defaultSocketConfig
  (responderApp (\x -> bool (Right $ "echo:" <> x) (Left "quit") (x=="q")))

clientIO :: IO ()
clientIO =
  (runClient defaultSocketConfig . clientApp)
  (Box (contramap show toStdout) fromStdin)

q' :: IO a -> IO (Either () a)
q' f = C.race (cancelQ fromStdin) f

cancelQ :: Emitter IO Text -> IO ()
cancelQ e = do
  e' <- emit e
  case e' of
    Just "q" -> pure ()
    _ -> do
      putStrLn ("nothing happens" :: Text)
      cancelQ e

-- | test of clientApp via a cRef committer and a canned list of Text
tClient :: [Text] -> IO [Either Text Text]
tClient xs = do
  (c,r) <- cRef
  runClient defaultSocketConfig
    (\conn ->
       (\b -> clientApp b conn) <$.>
       (Box <$>
        pure c <*>
        fromListE (xs <> ["q"])))
  r

tClientIO :: [Text] -> IO ()
tClientIO xs =
  (runClient defaultSocketConfig . clientApp) <$.>
  (Box (contramap show toStdout) <$> (fromListE (xs <> ["q"])))

-- | main test run of client-server functionality
-- the code starts a server in a thread, starts the client in the main thread, and cancels the server on completion.
-- >>> testRun
-- [Left "receiver: received: echo:1",Right "echo:1",Left "receiver: received: echo:2",Right "echo:2",Left "receiver: received: echo:3",Right "echo:3",Left "receiver: received: close: 1000 \"received close signal: responder closed.\""]
testRun :: IO [Either Text Text]
testRun = do
  a <- C.async (runServer defaultSocketConfig (responderApp (\x -> bool (Right $ "echo:" <> x) (Left "quit") (x=="q"))))
  sleep 0.1
  r <- tClient (show <$> [1..3::Int])
  C.cancel a
  pure r

