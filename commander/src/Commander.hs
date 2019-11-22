{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Commander where

import Control.Applicative (Alternative(..))
import Control.Monad (ap, void)
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import Data.HashSet as HashSet
import Data.HashMap.Strict as HashMap
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, unpack)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import System.Environment (getArgs)
import Data.Text.Read (decimal, signed)
import qualified Control.Monad.Stack.Trans as Stack

data Arg :: Symbol -> * -> *

data Opt :: Symbol -> Symbol -> Symbol -> * -> *

data Named :: Symbol -> *

data (...) :: k -> k' -> *
infixr 4 ...

data Raw :: *

data Flag :: Symbol -> *

data a + b
infixr 2 +

class Unrender t where
  unrender :: Text -> Maybe t

instance Unrender String where
  unrender = Just . unpack

instance Unrender Text where
  unrender = Just

instance Unrender Bool where
  unrender = flip Prelude.lookup [("True", True), ("False", False)]

instance Unrender Integer where
  unrender = either (const Nothing) h . signed decimal where
    h (n, "") = Just n
    h _ = Nothing

instance Unrender Int where
  unrender = either (const Nothing) h . signed decimal where
    h (n, "") = Just n
    h _ = Nothing

instance Unrender Word where
  unrender = either (const Nothing) h . decimal where
    h (n, "") = Just n
    h _ = Nothing

data CommanderT summary state m a
  = Action (state -> m (CommanderT summary state m a, state))
  | Defeat summary
  | Victory summary a
  deriving Functor

summaryAction :: (Monoid summary, Monad m) => summary -> CommanderT summary state m a -> CommanderT summary state m a
summaryAction summary (Defeat summary') = Defeat (summary <> summary')
summaryAction summary (Victory summary' a) = Victory (summary <> summary') a
summaryAction summary (Action action) = Action \state -> action state >>= \case
  (action', state') -> return (summaryAction summary action', state')

runCommanderT :: (Monoid summary, Monad m) => CommanderT summary state m a -> state -> m (summary, Maybe a)
runCommanderT (Action action) state = do
  (action', state') <- action state
  (summary, m) <- runCommanderT action' state'
  return (summary, m)
runCommanderT (Defeat summary) _ = return (summary, Nothing)
runCommanderT (Victory summary a) _ = return (summary, Just a)

instance (Monad m, Monoid summary) => Applicative (CommanderT summary state m) where
  (<*>) = ap
  pure = Victory mempty

instance Monoid summary => Stack.MonadTrans (CommanderT summary state) where
  lift ma = Action \state -> do
    a <- ma
    return (pure a, state)

instance Monoid summary => MonadTrans (CommanderT summary state) where
  lift ma = Action \state -> do
    a <- ma
    return (pure a, state)

instance (Monoid summary, MonadIO m) => MonadIO (CommanderT summary state m) where
  liftIO ma = Action \state -> do
    a <- liftIO ma
    return (pure a, state)

instance (Monad m, Monoid summary) => Monad (CommanderT summary state m) where
  Defeat summary >>= _ = Defeat summary
  Victory summary a >>= f = case f a of
    Action action -> Action \state -> (\(action', state') -> (action', state')) <$> action state
    Defeat summary' -> Defeat (summary <> summary')
    Victory summary' b -> Victory (summary <> summary') b
  Action action >>= f = Action \state -> do
    (action', state') <- action state
    return (action' >>= f, state')

instance (Monad m, Monoid summary) => Alternative (CommanderT summary state m) where
  empty = Defeat mempty 
  Defeat summary <|> a  = summaryAction summary a -- we must remember out defeats
  v@(Victory _ _) <|> _ = v -- if we have succeeded, we don't need to try another strategy
  Action action <|> p = Action \state -> do
    (action', state') <- action state 
    return (action' <|> p, state')  -- go back to this branch if you don't find victory

data State = State 
  { arguments :: [Text]
  , options :: HashMap Text Text
  , flags :: HashSet Text }

data Event 
  = BadOption Text Text Text
  | GoodOption Text
  | BadArgument Text Text
  | GoodArgument Text
  | TryingBranch Text
  | WrongBranch Text
  | Success
  deriving (Show, Eq, Ord)

class HasProgram p where
  data ProgramT p (m :: * -> *)
  run :: ProgramT p IO -> CommanderT [Event] State IO ()
  hoist :: (forall x. m x -> n x) -> ProgramT p m -> ProgramT p n 
  invocations :: [Text]

instance (Unrender t, KnownSymbol name, HasProgram p) => HasProgram (Arg name t ... p) where
  newtype ProgramT (Arg name t ... p) m = ArgProgramT { unArgProgramT :: t -> ProgramT p m }
  run f = Action $ \State{..} -> do
    case arguments of
      (x : xs) -> 
        case unrender x of
          Just t -> return (summaryAction [GoodArgument $ pack $ symbolVal (Proxy @name)] $ run (unArgProgramT f t), State{ arguments = xs, .. })  
          Nothing -> return (Defeat [BadArgument x (pack $ symbolVal (Proxy @name))], State{..})
      [] -> return (Defeat mempty, State{..})
  hoist n (ArgProgramT f) = ArgProgramT (hoist n . f)
  invocations = [(("<" <> pack (symbolVal (Proxy @name)) <> "> ") <>)] <*> invocations @p

instance (HasProgram x, HasProgram y) => HasProgram (x + y) where
  data ProgramT (x + y) m = ProgramT x m :+: ProgramT y m
  run (f :+: g) = run f <|> run g
  hoist n (f :+: g) = hoist n f :+: hoist n g
  invocations = invocations @x <> invocations @y

infixr 2 :+:

instance HasProgram Raw where
  newtype ProgramT Raw m = RawProgramT { unRawProgramT :: m () }
  run = summaryAction [Success] . liftIO . unRawProgramT
  hoist n (RawProgramT m) = RawProgramT (n m)
  invocations = [mempty]

instance (KnownSymbol name, KnownSymbol long, KnownSymbol short, HasProgram p, Unrender (Maybe t)) => HasProgram (Opt name long short t ... p) where
  newtype ProgramT (Opt name long short t ... p) m = OptProgramT { unOptProgramT :: Maybe t -> ProgramT p m }
  run f = Action $ \State{..} -> do
    case HashMap.lookup (pack $ symbolVal (Proxy @long)) options <|> HashMap.lookup (pack $ symbolVal (Proxy @short)) options of
      Just opt' -> 
        case unrender opt' of
          Just t -> return (run (unOptProgramT f t), State{..})
          Nothing -> return (Defeat [BadOption opt' (pack (symbolVal $ Proxy @short)) (pack (symbolVal $ Proxy @long))], State{..})
      Nothing  -> return (run (unOptProgramT f Nothing), State{..})
  hoist n (OptProgramT f) = OptProgramT (hoist n . f)
  invocations = [ (("-" <> (pack $ symbolVal (Proxy @short)) <> " <" <> (pack $ symbolVal (Proxy @name)) <> "> ") <>)
                , (("--" <> (pack $ symbolVal (Proxy @long)) <> " <" <> (pack $ symbolVal (Proxy @name)) <> "> ") <>)  ] <*> invocations @p

instance (KnownSymbol flag, HasProgram p) => HasProgram (Flag flag ... p) where
  newtype ProgramT (Flag flag ... p) m = FlagProgramT { unFlagProgramT :: Bool -> ProgramT p m }
  run f = Action $ \State{..} -> do
    let presence = HashSet.member (pack (symbolVal (Proxy @flag))) flags
    return (run (unFlagProgramT f presence), State{..})
  hoist n = FlagProgramT . fmap (hoist n) . unFlagProgramT
  invocations = [(("~" <> (pack $ symbolVal (Proxy @flag)) <> " ") <>)] <*> invocations @p

instance (KnownSymbol name, HasProgram p) => HasProgram (Named name ... p) where
  newtype ProgramT (Named name ...p) m = NamedProgramT { unNamedProgramT :: ProgramT p m }
  run = run . unNamedProgramT 
  hoist n = NamedProgramT . hoist n . unNamedProgramT
  invocations = [((pack (symbolVal (Proxy @name)) <> " ") <>)] <*> invocations @p

instance (KnownSymbol seg, HasProgram p) => HasProgram (seg ... p) where
  newtype ProgramT (seg ... p) m = SegProgramT { unSegProgramT :: ProgramT p m }
  run s = Action $ \State{..} -> do 
    case arguments of
      (x : xs) -> if x == pack (symbolVal $ Proxy @seg) then return (summaryAction [TryingBranch (pack . symbolVal $ Proxy @seg)] $ run $ unSegProgramT s, State{arguments = xs, ..})
                                                        else return (Defeat $ [WrongBranch . pack . symbolVal $ Proxy @seg], State{..})
      [] -> return (Defeat $ [WrongBranch . pack . symbolVal $ Proxy @seg], State{..})
  hoist n = SegProgramT . hoist n . unSegProgramT
  invocations = [((pack $ symbolVal (Proxy @seg) <> " ") <> )] <*> invocations @p
 
initialState :: IO State
initialState = do
  args <- getArgs
  let (opts, args', flags) = takeOptions args
  return $ State args' (HashMap.fromList opts) (HashSet.fromList flags) 
    where
      takeOptions :: [String] -> ([(Text, Text)], [Text], [Text])
      takeOptions = go [] [] [] where
        go opts args flags (('~':x') : z) = go opts args (pack x' : flags) z
        go opts args flags (('-':'-':x) : y : z) = go ((pack x, pack y) : opts) args flags z 
        go opts args flags (('-':x) : y : z) = go ((pack x, pack y) : opts) args flags z
        go opts args flags (x : y) = go opts (pack x : args) flags y
        go opts args flags [] = (opts, reverse args, flags)

commander_ :: HasProgram p => ProgramT p IO -> IO ()
commander_ prog = void $ initialState >>= runCommanderT (run prog)

commander :: HasProgram p => ProgramT p IO -> IO [Event]
commander prog = fmap fst $ initialState >>= runCommanderT (run prog)

arg :: KnownSymbol name => (x -> ProgramT p m) -> ProgramT (Arg name x ... p) m 
arg = ArgProgramT

opt :: (KnownSymbol name, KnownSymbol long, KnownSymbol short) => (Maybe x -> ProgramT p m) -> ProgramT (Opt name long short x ... p) m
opt = OptProgramT

raw :: m () -> ProgramT Raw m
raw = RawProgramT

sub :: KnownSymbol s => ProgramT p m -> ProgramT (s ... p) m
sub = SegProgramT

named :: KnownSymbol s => ProgramT p m -> ProgramT (Named s  ... p) m
named = NamedProgramT

flag :: KnownSymbol f => (Bool -> ProgramT p m) -> ProgramT (Flag f ... p) m
flag = FlagProgramT

(<+>) :: ProgramT p m -> ProgramT q m -> ProgramT (p + q) m
(<+>) = (:+:)

infixr 2 <+>
