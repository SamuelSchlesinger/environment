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
import Data.HashSet (HashSet)
import Data.HashMap.Strict as HashMap
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, unpack)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import System.Environment (getArgs)

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

data CommanderT summary state m a
  = Action (state -> m (CommanderT summary state m a, state, summary))
  | Defeat summary
  | Victory summary a
  deriving Functor

summaryAction :: (Monoid summary, Monad m) => summary -> CommanderT summary state m a -> CommanderT summary state m a
summaryAction summary (Defeat summary') = Defeat (summary <> summary')
summaryAction summary (Victory summary' a) = Victory (summary <> summary') a
summaryAction summary (Action action) = Action \state -> action state >>= \case
  (action', state', summary') -> return (summaryAction summary action', state', summary')

runCommanderT :: (Monoid summary, Monad m) => CommanderT summary state m a -> state -> m (summary, Maybe a)
runCommanderT (Action action) state = do
  (action', state', summary) <- action state
  (summary', m) <- runCommanderT action' state'
  return (summary <> summary', m)
runCommanderT (Defeat summary) _ = return (summary, Nothing)
runCommanderT (Victory summary a) _ = return (summary, Just a)

instance (Monad m, Monoid summary) => Applicative (CommanderT summary state m) where
  (<*>) = ap
  pure = Victory mempty

instance (Monoid summary, MonadIO m) => MonadIO (CommanderT summary state m) where
  liftIO ma = Action \state -> do
    a <- liftIO ma
    return (pure a, state, mempty)

instance (Monad m, Monoid summary) => Monad (CommanderT summary state m) where
  Defeat summary >>= _ = Defeat summary
  Victory summary a >>= f = case f a of
    Action action -> Action \state -> (\(action', state', summary') -> (action', state', summary <> summary')) <$> action state
    Defeat summary' -> Defeat (summary <> summary')
    Victory summary' b -> Victory (summary <> summary') b
  Action action >>= f = Action \state -> do
    (action', state', summary) <- action state
    case action' >>= f of
      Action action'' -> return (Action action'', state', summary)
      Defeat summary' -> return (Defeat summary', state', summary <> summary')
      Victory summary' b -> return (Victory summary' b, state', summary <> summary')

instance (Monad m, Monoid summary) => Alternative (CommanderT summary state m) where
  empty = Defeat mempty -- the empty commander action is a defeat with no information
  Defeat summary <|> Defeat _ = Defeat summary -- we don't need to get defeated again!
  Defeat summary <|> a  = summaryAction summary a -- retain the knowledge from our defeat...
  Victory summary a <|> _ = Victory summary a  -- we've already won, why do anything else?
  Action action <|> p = Action \state -> do -- here, we descend down the left action one level
    (action', state', summary) <- action state
    return (action' <|> p, state', summary)     

data State = State
  { arguments :: [Text]
  , options :: HashMap Text Text
  , flags :: HashSet Text }

data Event 
  = BadOption Text Text
  | BadArgument Text
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
          Just t -> return (run (unArgProgramT f t), State{ arguments = xs, .. }, mempty)  
          Nothing -> return (Defeat [BadArgument (pack $ symbolVal (Proxy @name))], State{..}, mempty)
      [] -> return (Defeat mempty, State{..}, mempty)
  hoist n (ArgProgramT f) = ArgProgramT (hoist n . f)

instance (HasProgram x, HasProgram y) => HasProgram (x + y) where
  data ProgramT (x + y) m = ProgramT x m :+: ProgramT y m
  run (f :+: g) = run f <|> run g
  hoist n (f :+: g) = hoist n f :+: hoist n g

infixr 2 :+:

instance HasProgram Raw where
  newtype ProgramT Raw m = RawProgramT { unRawProgramT :: m () }
  run m = liftIO $ unRawProgramT m
  hoist n (RawProgramT m) = RawProgramT (n m)

instance (KnownSymbol long, KnownSymbol short, HasProgram p, Unrender (Maybe t)) => HasProgram (Opt long short t ... p) where
  newtype ProgramT (Opt long short t ... p) m = OptProgramT { unOptProgramT :: Maybe t -> ProgramT p m }
  run f = Action $ \State{..} -> do
    case HashMap.lookup (pack $ symbolVal (Proxy @long)) options <|> HashMap.lookup (pack $ symbolVal (Proxy @short)) options of
      Just opt' -> 
        case unrender opt' of
          Just t -> return (run (unOptProgramT f t), State{..}, mempty)
          Nothing -> return (Defeat [BadOption (pack (symbolVal $ Proxy @short)) (pack (symbolVal $ Proxy @long))], State{..}, mempty)
      Nothing  -> return (run (unOptProgramT f Nothing), State{..}, mempty)
  hoist n (OptProgramT f) = OptProgramT (hoist n . f)

instance HasProgram p => HasProgram (Doc doc ... p) where
  newtype ProgramT (Doc doc ...p) m = DocProgramT { unDocProgramT :: ProgramT p m }
  run = run . unDocProgramT 
  hoist n = DocProgramT . hoist n . unDocProgramT

instance (KnownSymbol seg, HasProgram p) => HasProgram (seg ... p) where
  newtype ProgramT (seg ... p) m = SegProgramT { unSegProgramT :: ProgramT p m }
  run s = Action $ \State{..} -> do 
    case arguments of
      (x : xs) -> if x == pack (symbolVal $ Proxy @seg) then return (run $ unSegProgramT s, State{arguments = xs, ..}, mempty)
                                                        else return (Defeat mempty, State{..}, [WrongBranch $ pack $ symbolVal $ Proxy @seg])
      [] -> return (Defeat mempty, State{..}, [WrongBranch $ pack $ symbolVal $ Proxy @seg])
  hoist n = SegProgramT . hoist n . unSegProgramT
 
initialState :: IO State
initialState = do
  args <- getArgs
  let (opts, args') = takeOptions args
  return $ State args' (HashMap.fromList opts) mempty 
    where
      takeOptions :: [String] -> ([(Text, Text)], [Text])
      takeOptions = go [] [] where
        go :: [(Text, Text)] -> [Text] -> [String] -> ([(Text, Text)], [Text])
        go opts args (x'@('-':_) : y : z) = go ((pack x', pack y) : opts) args z 
        go opts args (x : y) = go opts (pack x : args) y
        go opts args [] = (opts, reverse args)

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

(<+>) :: ProgramT p m -> ProgramT q m -> ProgramT (p + q) m
(<+>) = (:+:)

infixr 2 <+>
