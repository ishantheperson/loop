{-# LANGUAGE LambdaCase #-}
module Eval where

import Parser 

import Control.Monad.State.Strict 
import qualified Data.Map.Strict as Map 

import Data.Maybe (fromJust)
import Data.Foldable (for_)

type EvalState = Map.Map String Integer 

run :: Program -> EvalState 
run p = snd $ runState (traverse evalS p) Map.empty 

evalS :: Statement -> State EvalState ()
evalS = \case 
  Increment v -> modify (Map.adjust succ v) -- silently fails if v is not in the map 
  Assign v e -> do value <- evalE e 
                   modify (Map.insert v value)

  Do v p -> do numTimes <- evalE v 
               for_ [1..numTimes] (const $ traverse evalS p)

evalE :: Expression -> State EvalState Integer 
evalE = \case 
  Variable s -> gets (fromJust . Map.lookup s) -- crashes if s is not in the map 
  Constant c -> return c 