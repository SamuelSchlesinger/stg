{-# LANGUAGE LambdaCase #-}
module STG.Semantics.StateMachine where

import STG.Syntax
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word)

data Value addr lit = Addr addr | Lit lit

type ArgumentStack addr lit = [Value addr lit]

data Closure addr lit prim constr var = Closure
  { lambda :: Lambda lit prim constr var
  , values :: [Value addr lit]
  }

type Heap addr lit prim constr var = Map addr (Closure addr lit prim constr var)

type GlobalEnvironment var addr = Map var addr

type LocalEnvironment addr lit var = Map var (Value addr lit)

data Code addr lit prim constr var =
    Eval (Expression lit prim constr var) (LocalEnvironment addr lit var)
  | Enter addr
  | ReturnCon constr [Value addr lit]
  | ReturnLit lit

val :: Ord var => LocalEnvironment addr lit var -> GlobalEnvironment var addr -> Atom lit var -> Maybe (Value addr lit)
val localEnv globalEnv = \case
  LiteralAtom lit -> Just (Lit lit)
  VarAtom var ->
    case Map.lookup var localEnv of
      Just v -> Just v
      Nothing -> Addr <$> Map.lookup var globalEnv
