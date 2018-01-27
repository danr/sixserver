{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language DisambiguateRecordFields #-}
{-# language ExplicitForAll #-}
{-# language TypeOperators #-}
module Main where

import Language.Haskell.LSP.Constant as LSP
import Language.Haskell.LSP.Control as LSP
import Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Diagnostics as LSP
import Language.Haskell.LSP.Messages as LSP
import Language.Haskell.LSP.TH.ClientCapabilities as LSP
import Language.Haskell.LSP.TH.Constants as LSP
import Language.Haskell.LSP.TH.DataTypesJSON as LSP
import Language.Haskell.LSP.Utility as LSP
import Language.Haskell.LSP.VFS as LSP

import Control.Concurrent.STM as STM

import Data.Default (def)

import Data.Aeson (ToJSON)

type params ~> response = params -> IO (Maybe response)

hover :: TextDocumentPositionParams ~> Hover
hover params = do
  print params
  return $ Just Hover {
    _contents=LSP.List [LSP.PlainString "Hover!"],
    _range=Nothing
  }

main :: IO ()
main = do
  lsp_funcs_ref <- STM.newTVarIO (Nothing :: Maybe (LSP.LspFuncs ()))

  let handle
        :: ToJSON response => params ~> response
        -> Maybe (LSP.Handler (LSP.RequestMessage method params response))
      handle h = Just $ \ (LSP.RequestMessage jsonrpc req_id _method params) -> do
        mresponse <- h params
        case mresponse of
          Just response -> do
            Just lf <- STM.readTVarIO lsp_funcs_ref
            LSP.sendFunc lf (LSP.ResponseMessage jsonrpc (LSP.responseId req_id) (Just response) Nothing)
          Nothing -> return ()

  LSP.run
    (\ _ -> Right (), \ lf -> STM.atomically (STM.writeTVar lsp_funcs_ref (Just lf)) >> return Nothing)
    (def {hoverHandler=handle hover})
    def
  return ()


