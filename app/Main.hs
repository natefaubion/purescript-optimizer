module Main where

import Prelude
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar
import Control.Monad (join, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Foldable (for_, foldl')
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as D
import qualified Data.Text.Prettyprint.Doc.Render.Text as D
import qualified Data.Text.IO as IO
import Data.Traversable (for)
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Errors as Errs
import qualified Language.PureScript.Make as Make
import qualified Language.PureScript.Names as Names
import qualified Language.PureScript.Optimizer.CoreAnf as O
import qualified Language.PureScript.Optimizer.Types as O
import qualified Language.PureScript.Optimizer as O
import Language.PureScript.Options (defaultOptions)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.FilePath.Glob (glob)
import System.IO

actions :: MVar (M.Map Names.ModuleName O.OptimizerResult) -> Make.MakeActions Make.Make
actions var = Make.MakeActions
  { getInputTimestamp = const (pure (Left Make.RebuildAlways))
  , getOutputTimestamp = const (pure Nothing)
  , readExterns = error "readExterns not implemented"
  , codegen = \m _ _ -> lift . liftIO $ do
      env <- takeMVar var
      let
        res  = O.optimize env m
        env' = M.insert (O.modName . O.optModule $ res) res env
        doc = D.renderStrict . D.layoutPretty (D.LayoutOptions D.Unbounded) . O.ppModule . O.optModule $ res
        path = "./output/" <> T.unpack (Names.runModuleName . O.modName . O.optModule $ res)
      IO.writeFile path doc
      putMVar var env'
  , ffiCodegen = \_ -> pure ()
  , progress = \_ -> pure ()
  }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  filePaths <- case filter ((/= '-') . head) args of
    [] -> die "File glob required."
    gs ->
      fmap join $ for gs $ \g -> do
        paths <- glob g
        when (null paths) $ die "Glob did not match any files."
        pure paths

  mbModules <- forConcurrently filePaths $ \path -> do
    src <- IO.readFile path
    pure . (path, src,) $ CST.parseModuleFromFile path src

  let
    modules = foldl' go id mbModules $ Right []
    go k = \case
      (path, src, Left errs) -> do
        let errs' = (path, src,) <$> errs
        \case
          Left errs'' -> k $ Left (errs' <> errs'')
          Right _ -> k $ Left errs'
      (_, _, Right md) ->
        \case
          Left errs'' -> k $ Left errs''
          Right mds -> k $ Right (md : mds)

  case modules of
    Left errs ->
      for_ errs $ \(path, _, err) ->
        putStrLn $ ex <> " " <> path <> " " <> CST.prettyPrintError err
    Right ms -> do
      var <- newMVar mempty
      res <- Make.runMake defaultOptions $ Make.make (actions var) ms
      case res of
        (Left errs, _) -> do
          hPutStrLn stderr (Errs.prettyPrintMultipleErrors Errs.defaultPPEOptions errs)
          exitFailure
        (Right _, _) -> do
          putStrLn $ check <> " OK"

check :: String
check = "\x1b[32m✓\x1b[0m"

ex :: String
ex = "\x1b[31m✗\x1b[0m"
