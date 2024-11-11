{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Symbol
  ( Symbol(..)
  , SymbolTable(..)
  , toSymbol
  , lookupSymbol
  , insertSymbol
  , newSymbolTable
  , HasSymbolTable(..)
  )
where

import Data.Hashable qualified as Hashable
import Data.HashMap.Strict qualified as HM
import UnliftIO (MonadUnliftIO)


class HasSymbolTable ctx where
  getSymbolTableRef :: ctx -> IORef SymbolTable


newtype SymbolTable = SymbolTable {getSymbolTable :: HashMap Text Int}


data Symbol = Symbol
  { text :: Text
  , hash :: Int
  }
  deriving (Show, Read)


instance Eq Symbol where
  a == b = a.hash == b.hash


instance Ord Symbol where
  compare a b = compare a.hash b.hash


newSymbolTable :: SymbolTable
newSymbolTable = SymbolTable HM.empty


toSymbol :: (HasSymbolTable ctx, MonadReader ctx m, MonadUnliftIO m) => Text -> m Symbol
toSymbol text = do
  symbolTable <- ask >>= readIORef . getSymbolTableRef
  case lookupSymbol text symbolTable of
    Just symbol -> pure symbol
    Nothing -> do
      let n = Hashable.hash text
      ref <- getSymbolTableRef <$> ask
      ref `writeIORef` insertSymbol text n symbolTable
      pure (Symbol text n)


lookupSymbol :: Text -> SymbolTable -> Maybe Symbol
lookupSymbol text symbolTable =
  case HM.lookup text symbolTable.getSymbolTable of
    Just n -> Just (Symbol text n)
    Nothing -> Nothing


insertSymbol :: Text -> Int -> SymbolTable -> SymbolTable
insertSymbol text hash = SymbolTable . HM.insert text hash . (.getSymbolTable)
