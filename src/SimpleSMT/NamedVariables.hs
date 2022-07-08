{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module SimpleSMT.NamedVariables ( Name (..)
                                , Id (..)
                                , Ctx
                                , eqName
                                , indexEq
                                ) where

import GHC.TypeLits
import Data.Proxy
import Data.Kind
import Data.Type.Equality

data Index (g :: [k]) (s :: k) where
  Here  :: Index (s ': g) s
  There :: Index g s -> Index (t ': g) s

deriving instance Show (Index g s)

-- toInt :: Index g s -> Int
-- toInt Here = 0
-- toInt (There idx) = 1 + toInt idx

indexEq :: Index g s -> Index g t -> Maybe (s :~: t)
indexEq idx idx'
  = case (idx, idx') of
      (Here , Here) -> Just Refl
      (There idx1, There idx1') -> indexEq idx1 idx1'
      _                         -> Nothing

type family If (b :: Bool)(t :: k)(e :: k) :: k where
  If 'True t _  = t
  If 'False _ e = e

type family Lookup (g :: [(Symbol, Type)]) (s :: Symbol) :: Maybe Type where
  Lookup '[]             _ = 'Nothing
  Lookup ('(t , a) ': g) s = If (s == t) ('Just a) (Lookup g s)


data Id (s :: Symbol) = KnownSymbol s => Id

instance Show (Id s) where
  show Id = symbolVal (Proxy :: Proxy s)

-- reification of variables in scope

type Ctx = [(Symbol, Type)]

type family AtHead (g :: [k]) (s :: k) :: Bool where
  AtHead '[]      _ = 'False
  AtHead (t ': _) s = s == t

class Lookup g s ~ 'Just t =>
      Reify (g :: Ctx) (s :: Symbol) (t :: Type) (b :: Bool) where
  reify :: Proxy b -> Id s -> Index g '(s, t)

instance (s == s) ~ 'True => Reify ('(s , a) ': g) s a 'True where
  reify _ _ = Here

instance ((s == t) ~ 'False, Reify g s a (AtHead g '(s, a))) =>
         Reify ('(t , b) ': g) s a 'False where
  reify _ n = There (reify' n)

reify' :: forall g s a. Reify g s a (AtHead g '(s, a)) => Id s -> Index g '(s, a)
reify' nm = reify (Proxy :: Proxy (AtHead g '(s, a))) nm

reify'' :: forall g s a. Name s g a -> Index g '(s, a)
reify'' (The nm) = reify (Proxy :: Proxy (AtHead g '(s, a))) nm

-- definition of names

data Name (s :: Symbol)(g :: Ctx)(a :: Type)
  = Reify g s a (AtHead g '(s, a)) => The (Id s)

instance Show (Name s g a) where
  show v@(The n) = concat [ show n, " (= " , show (reify'' v),  ")" ]

eqName :: Name s g a -> Name s' g b -> Maybe (s :~: s' , a :~: b)
eqName n m = case indexEq (reify'' n) (reify'' m) of
               Just Refl -> Just (Refl , Refl)
               Nothing   -> Nothing
