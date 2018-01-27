{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language DisambiguateRecordFields #-}
{-# language TypeOperators #-}
module Main where

import Language.Haskell.LSP.Constant as LSP
import Language.Haskell.LSP.Control as LSP
import Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Diagnostics as LSP
import Language.Haskell.LSP.Messages as LSP
import Language.Haskell.LSP.TH.ClientCapabilities as LSP
import Language.Haskell.LSP.TH.Constants as LSP
import Language.Haskell.LSP.TH.DataTypesJSON as LSP hiding (change)
import Language.Haskell.LSP.Utility as LSP
import Language.Haskell.LSP.VFS as LSP

import Language.Haskell.Ghcid

import Control.Concurrent.STM as STM
import Control.Concurrent

import Data.Default (def)

import Data.Aeson (ToJSON)

import qualified System.IO as IO

import Data.Char as Char

import qualified Yi.Rope as Yi
import Data.Text (Text)
import qualified Data.Text as T

type params ~> response = LSP.LspFuncs () -> params -> IO (Maybe response)
type Notified params = LSP.LspFuncs () -> params -> IO ()

lflog lf _ s =
  LSP.sendFunc lf
    (LSP.NotificationMessage "2.0" LSP.WindowLogMessage
      (LSP.LogMessageParams LSP.MtInfo (T.pack s)))

fileContents :: LSP.LspFuncs () -> LSP.Uri -> IO String
fileContents lf uri = do
  -- NB: if this is not just we should return what is on disk instead
  mvf <- LSP.getVirtualFileFunc lf uri
  case mvf of
    Just (VirtualFile _ rope) -> return (Yi.toString rope)
    Nothing ->
      case uriToFilePath uri of
        Just fp -> IO.readFile fp
        Nothing -> return ""


takeWhileRev :: (a -> Bool) -> [a] -> [a]
takeWhileRev p = reverse . takeWhile p . reverse

idAt :: String -> (Int, Int) -> String
idAt s (y, x) = takeWhileRev p a ++ takeWhile p z
  where
    p c = Char.isAlpha c || c `elem` ("._" :: [Char])
    line = lines s !! y
    (a, z) = splitAt x line

hover :: TVar (Maybe Ghci) -> TextDocumentPositionParams ~> Hover
hover ghci_ref lf (TextDocumentPositionParams (TextDocumentIdentifier uri) (Position line char)) = do
  Just ghci <- readTVarIO ghci_ref
  contents <- fileContents lf uri
  (fp, h) <- IO.openTempFile "/tmp" "fileXXXXXXXX.hs"
  IO.hClose h
  IO.writeFile fp contents
  execStream ghci (":l " ++ fp) (lflog lf)
  res <- exec ghci (":i " ++ contents `idAt` (line, char))
  reload ghci
  return $ Just Hover {
    _contents=LSP.List [ LSP.PlainString (T.pack r) | r <- res ],
    _range=Nothing
  }

{-
change :: Notified DidChangeTextDocumentParams
change lf (LSP.DidChangeTextDocumentParams (LSP.VersionedTextDocumentIdentifier uri version) _) = do
  contents <- fileContents lf uri
  LSP.publishDiagnosticsFunc lf 10 uri (Just version) (LSP.partitionBySource [
    LSP.Diagnostic {
      _range=Range (Position 2 1) (Position 2 2),
      _severity=Nothing,
      _code=Nothing,
      _source=Nothing,
      _message=contents
    }])
    -}

main :: IO ()
main = do
  lsp_funcs_ref <- STM.newTVarIO (Nothing :: Maybe (LSP.LspFuncs ()))

  let handle
        :: ToJSON response => params ~> response
        -> Maybe (LSP.Handler (LSP.RequestMessage method params response))
      handle h = Just $ \ (LSP.RequestMessage jsonrpc req_id _method params) -> do
        Just lf <- STM.readTVarIO lsp_funcs_ref
        mresponse <- h lf params
        case mresponse of
          Just response -> do
            LSP.sendFunc lf (LSP.ResponseMessage jsonrpc (LSP.responseId req_id) (Just response) Nothing)
          Nothing -> return ()

  let notified
        :: Notified params
        -> Maybe (LSP.Handler (LSP.NotificationMessage method params))
      notified h = Just $ \ (LSP.NotificationMessage jsonrpc _method params) -> do
        Just lf <- STM.readTVarIO lsp_funcs_ref
        h lf params

  ghci_ref <- STM.newTVarIO (Nothing :: Maybe Ghci)

  forkIO $ do
    lf <- atomically $ do
      mlf <- readTVar lsp_funcs_ref
      maybe STM.retry return mlf
    -- let notify = do
    --       lf
    (ghci, load) <- startGhci "stack repl --ghc-options -fno-code" (Just "/home/dan/code/sixserver") (lflog lf)
    atomically $ writeTVar ghci_ref (Just ghci)

  LSP.run
    (\ _ -> Right (), \ lf -> STM.atomically (STM.writeTVar lsp_funcs_ref (Just lf)) >> return Nothing)
    (def {
      hoverHandler=handle (hover ghci_ref)
--    , didChangeTextDocumentNotificationHandler=notified change
    })
    def
  return ()


