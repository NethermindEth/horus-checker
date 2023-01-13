module Horus.CallStack
  ( singleton
  , push
  , pop
  , top
  , stackTrace
  , CallEntry
  , CallStack
  , callerOfRoot
  , callerPcOfCallEntry
  , calledFOfCallEntry
  , digestOfCallStack
  , digestOfStackTrace
  , initialWithFunc
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
  ( NonEmpty
  , fromList
  , map
  , reverse
  , singleton
  , toList
  )
import Data.Map (Map, (!))
import Data.Text (Text)

import Horus.Label (Label (Label, unLabel))
import Horus.SW.ScopedName (ScopedName (ScopedName))
import Horus.Util (tShow)

newtype NonEmptyStack a = NonEmptyStack {unStack :: NonEmpty.NonEmpty a}
  deriving (Show, Eq, Ord, Functor)

type CallEntry = (Label, Label)
type CallStack = NonEmptyStack CallEntry

callerPcOfCallEntry :: CallEntry -> Label
callerPcOfCallEntry = fst

calledFOfCallEntry :: CallEntry -> Label
calledFOfCallEntry = snd

singleton :: CallEntry -> CallStack
singleton = NonEmptyStack . NonEmpty.singleton

push :: CallEntry -> CallStack -> CallStack
push x (NonEmptyStack xs) = NonEmptyStack (x :| NonEmpty.toList xs)

pop :: CallStack -> (CallEntry, CallStack)
pop st@(NonEmptyStack (x :| [])) = (x, st)
pop (NonEmptyStack (x :| xs)) = (x, NonEmptyStack (NonEmpty.fromList xs))

top :: CallStack -> CallEntry
top (NonEmptyStack (x :| _)) = x

initialWithFunc :: Label -> CallStack
initialWithFunc = singleton . (callerOfRoot,)

descriptiveStackTrace :: CallStack -> NonEmpty CallEntry
descriptiveStackTrace = NonEmpty.reverse . unStack

stackTrace :: CallStack -> NonEmpty Label
stackTrace = NonEmpty.map fst . descriptiveStackTrace

callerOfRoot :: Label
callerOfRoot = Label (-1)

digestOfStackTrace :: Map Label ScopedName -> NonEmpty CallEntry -> Text
digestOfStackTrace names = wrap . foldr (flip (<>) . tShowCaller) ""
 where
  tShowCaller (Label pc, calledF) =
    if pc == unLabel callerOfRoot
      then "root"
      else tShow pc <> "=" <> fName (names ! calledF) <> "/"
  wrap trace = "<" <> trace <> ">"
  fName (ScopedName name) = last name

digestOfCallStack :: Map Label ScopedName -> CallStack -> Text
digestOfCallStack names = digestOfStackTrace names . descriptiveStackTrace
