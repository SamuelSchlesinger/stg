module STG.Syntax where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Bindings lit prim constr var = Map var (Lambda lit prim constr var)

data Program lit prim constr var = Program
  { bindings :: Bindings lit prim constr var
  }
  deriving (Eq, Ord, Show, Read)

-- | Invariant: forall (l :: Lambda). nub (arguments l) == arguments l
data Lambda lit prim constr var = Lambda
  { free :: Set var
  , updateFlag :: UpdateFlag
  , arguments :: [var]
  , expression :: Expression lit prim constr var
  }
  deriving (Eq, Ord, Show, Read)

data UpdateFlag = Updatable | NotUpdatable
  deriving (Eq, Ord, Show, Read)

data Expression lit prim constr var =
    Let (Bindings lit prim constr var) (Expression lit prim constr var)
  | LetRec (Bindings lit prim constr var) (Expression lit prim constr var)
  | Case (Expression lit prim constr var) (Alternatives lit prim constr var)
  | Application var (Atoms lit var)
  | Constructor constr (Atoms lit var) 
  | Primitive lit prim (Atoms lit var)
  | Literal lit
  deriving (Eq, Ord, Show, Read)

data Alternatives lit prim constr var =
    AlgebraicAlternatives [AlgebraicAlternative lit prim constr var]
  | PrimitiveAlternatives [PrimitiveAlternative lit prim constr var]
  deriving (Eq, Ord, Show, Read)

data AlgebraicAlternative lit prim constr var = AlgebraicAlternative
  { algAltTag :: constr
  , algAltVars :: [var] 
  , algAltExpression :: Expression lit prim constr var
  }
  deriving (Eq, Ord, Show, Read)

data PrimitiveAlternative lit prim constr var = PrimitiveAlternative
  { primAltLit :: lit
  , primAltExpression :: Expression lit prim constr var
  }
  deriving (Eq, Ord, Show, Read)

data Atom lit var =
    LiteralAtom lit
  | VarAtom var
  deriving (Eq, Ord, Show, Read)

type Atoms lit var = [Atom lit var]
