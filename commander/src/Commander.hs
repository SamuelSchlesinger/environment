{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Commander where

import Control.Applicative (Alternative(..))
import Control.Monad (ap, void)
import Control.Monad.Trans (MonadIO(..))
import Data.HashSet as HashSet
import Data.HashMap.Strict as HashMap
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, unpack)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import System.Environment (getArgs)
import Data.Text.Read (decimal, signed)

data Arg :: Symbol -> * -> *

data Opt :: Symbol -> Symbol -> * -> *

data Doc :: Symbol -> *

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
    return (action' <|> p, state')  -- the state monad with backtracking!

data State = State 
  { arguments :: [Text]
  , options :: HashMap Text Text
  , flags :: HashSet Text }

data Event 
  = BadOption Text Text
  | BadArgument Text
  | TryingBranch Text
  | WrongBranch Text
  | Success
  deriving (Show, Eq, Ord)

class HasProgram p where
  data ProgramT p (m :: * -> *)
  run :: ProgramT p IO -> CommanderT [Event] State IO ()
  hoist :: (forall x. m x -> n x) -> ProgramT p m -> ProgramT p n 

instance (Unrender t, KnownSymbol name, HasProgram p) => HasProgram (Arg name t ... p) where
  newtype ProgramT (Arg name t ... p) m = ArgProgramT { unArgProgramT :: t -> ProgramT p m }
  run f = Action $ \State{..} -> do
    case arguments of
      (x : xs) -> 
        case unrender x of
          Just t -> return (run (unArgProgramT f t), State{ arguments = xs, .. })  
          Nothing -> return (Defeat [BadArgument (pack $ symbolVal (Proxy @name))], State{..})
      [] -> return (Defeat mempty, State{..})
  hoist n (ArgProgramT f) = ArgProgramT (hoist n . f)

instance (HasProgram x, HasProgram y) => HasProgram (x + y) where
  data ProgramT (x + y) m = ProgramT x m :+: ProgramT y m
  run (f :+: g) = run f <|> run g
  hoist n (f :+: g) = hoist n f :+: hoist n g

infixr 2 :+:

instance HasProgram Raw where
  newtype ProgramT Raw m = RawProgramT { unRawProgramT :: m () }
  run = summaryAction [Success] . liftIO . unRawProgramT
  hoist n (RawProgramT m) = RawProgramT (n m)

instance (KnownSymbol long, KnownSymbol short, HasProgram p, Unrender (Maybe t)) => HasProgram (Opt long short t ... p) where
  newtype ProgramT (Opt long short t ... p) m = OptProgramT { unOptProgramT :: Maybe t -> ProgramT p m }
  run f = Action $ \State{..} -> do
    case HashMap.lookup (pack $ symbolVal (Proxy @long)) options <|> HashMap.lookup (pack $ symbolVal (Proxy @short)) options of
      Just opt' -> 
        case unrender opt' of
          Just t -> return (run (unOptProgramT f t), State{..})
          Nothing -> return (Defeat [BadOption (pack (symbolVal $ Proxy @short)) (pack (symbolVal $ Proxy @long))], State{..})
      Nothing  -> return (run (unOptProgramT f Nothing), State{..})
  hoist n (OptProgramT f) = OptProgramT (hoist n . f)

instance (KnownSymbol flag, HasProgram p) => HasProgram (Flag flag ... p) where
  newtype ProgramT (Flag flag ... p) m = FlagProgramT { unFlagProgramT :: Bool -> ProgramT p m }
  run f = Action $ \State{..} -> do
    let presence = HashSet.member (pack (symbolVal (Proxy @flag))) flags
    return (run (unFlagProgramT f presence), State{..})
  hoist n = FlagProgramT . fmap (hoist n) . unFlagProgramT

instance HasProgram p => HasProgram (Doc doc ... p) where
  newtype ProgramT (Doc doc ...p) m = DocProgramT { unDocProgramT :: ProgramT p m }
  run = run . unDocProgramT 
  hoist n = DocProgramT . hoist n . unDocProgramT

instance (KnownSymbol seg, HasProgram p) => HasProgram (seg ... p) where
  newtype ProgramT (seg ... p) m = SegProgramT { unSegProgramT :: ProgramT p m }
  run s = Action $ \State{..} -> do 
    case arguments of
      (x : xs) -> if x == pack (symbolVal $ Proxy @seg) then return (summaryAction [TryingBranch (pack . symbolVal $ Proxy @seg)] $ run $ unSegProgramT s, State{arguments = xs, ..})
                                                        else return (Defeat $ [WrongBranch . pack . symbolVal $ Proxy @seg], State{..})
      [] -> return (Defeat $ [WrongBranch . pack . symbolVal $ Proxy @seg], State{..})
  hoist n = SegProgramT . hoist n . unSegProgramT
 
initialState :: IO State
initialState = do
  args <- getArgs
  let (opts, args', flags) = takeOptions args
  return $ State args' (HashMap.fromList opts) (HashSet.fromList flags) 
    where
      takeOptions :: [String] -> ([(Text, Text)], [Text], [Text])
      takeOptions = go [] [] [] where
        go opts args flags (('`':x') : z) = go opts args (pack x' : flags) z
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

opt :: (KnownSymbol long, KnownSymbol short) => (Maybe x -> ProgramT p m) -> ProgramT (Opt long short x ... p) m
opt = OptProgramT

doc :: KnownSymbol doc => ProgramT p m -> ProgramT (Doc doc ... p) m
doc = DocProgramT

raw :: m () -> ProgramT Raw m
raw = RawProgramT

sub :: KnownSymbol s => ProgramT p m -> ProgramT (s ... p) m
sub = SegProgramT

flag :: KnownSymbol f => (Bool -> ProgramT p m) -> ProgramT (Flag f ... p) m
flag = FlagProgramT

(<+>) :: ProgramT p m -> ProgramT q m -> ProgramT (p + q) m
(<+>) = (:+:)

infixr 2 <+>
