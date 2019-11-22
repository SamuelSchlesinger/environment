{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Commander where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.HashSet (HashSet)
import Data.HashMap.Strict as HashMap
import Data.Proxy
import Data.Text (Text, pack, unpack)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import System.Environment

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
  unrender :: Text -> t

data CommanderT summary state m a
  = Action (state -> m (CommanderT summary state m a, state, summary))
  | Defeat summary
  | Victory summary a
  deriving Functor

summaryAction :: (Monoid summary, Monad m) => summary -> CommanderT summary state m a -> CommanderT summary state m a
summaryAction summary (Defeat summary') = Defeat (summary <> summary')
summaryAction summary (Victory summary' a) = Victory (summary <> summary') a
summaryAction summary (Action action) = Action \state -> action state >>= \case
  (action', state', summary') -> return (action', state', summary <> summary')

runCommanderT :: (Monoid summary, Monad m) => state -> CommanderT summary state m a -> m (summary, Maybe a)
runCommanderT state (Action action) = do
  (action', state', summary) <- action state
  runCommanderT state' (summaryAction summary action')
runCommanderT _ (Defeat summary) = return (summary, Nothing)
runCommanderT _ (Victory summary a) = return (summary, Just a)

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

class HasProgram p where
  data ProgramT p (m :: * -> *)
  run :: ProgramT p IO -> CommanderT [Text] State IO ()

instance (Unrender t, KnownSymbol name, HasProgram p) => HasProgram (Arg name t ... p) where
  newtype ProgramT (Arg name t ... p) m = ArgProgramT { unArgProgramT :: t -> ProgramT p m }
  run f = Action $ \State{..} -> do
    case arguments of
      (x : xs) -> do
        return (run (unArgProgramT f $ unrender x), State{ arguments = xs, .. }, mempty)  
      [] -> return (Defeat mempty, State{..}, mempty)

instance (HasProgram x, HasProgram y) => HasProgram (x + y) where
  data ProgramT (x + y) m = ProgramT x m :+: ProgramT y m
  run (f :+: g) = run f <|> run g

infixr 2 :+:

instance HasProgram Raw where
  newtype ProgramT Raw m = RawProgramT { unRawProgramT :: m () }
  run m = liftIO $ unRawProgramT m

instance (KnownSymbol long, KnownSymbol short, HasProgram p, Unrender (Maybe t)) => HasProgram (Opt long short t ... p) where
  newtype ProgramT (Opt long short t ... p) m = OptProgramT { unOptProgramT :: Maybe t -> ProgramT p m }
  run f = Action $ \State{..} -> do
    case HashMap.lookup (pack $ symbolVal (Proxy @long)) options <|> HashMap.lookup (pack $ symbolVal (Proxy @short)) options of
      Just opt -> return (run (unOptProgramT f . unrender $ opt), State{..}, mempty)
      Nothing  -> return (run (unOptProgramT f Nothing), State{..}, mempty)

instance HasProgram p => HasProgram (Doc doc ... p) where
  newtype ProgramT (Doc doc ...p) m = DocProgramT { unDocProgramT :: ProgramT p m }
  run = run . unDocProgramT 

instance (KnownSymbol seg, HasProgram p) => HasProgram (seg ... p) where
  newtype ProgramT (seg ... p) m = SegProgramT { unSegProgramT :: ProgramT p m }
  run s = Action $ \State{..} -> do 
    case arguments of
      (x : xs) -> if x == pack (symbolVal $ Proxy @seg) then return (run $ unSegProgramT s, State{arguments = xs, ..}, mempty)
                                                        else return (Defeat mempty, State{..}, [pack $ symbolVal $ Proxy @seg])
      [] -> return (Defeat mempty, State{..}, [pack $ symbolVal $ Proxy @seg])

type Interface = "booyah" ... Opt "--poop" "-p" Bool ... Raw
               + "hooray" ... Arg "plankton"  String ... Raw
               + Raw
    
example :: ProgramT Interface IO
example = example1 :+: example0 :+: help
  where
    example0 = SegProgramT . ArgProgramT $ \t -> RawProgramT $ case t of
           "Boooop" -> putStrLn "Boooop"
           "Yeeeah-dawg" -> putStrLn "Heyo"
           _ -> putStrLn "Wut?"
    example1 = SegProgramT . OptProgramT $ \t -> RawProgramT $ case t of
      Just True -> putStrLn "CheesyBoyz"
      Just False -> putStrLn "REEEEE"
      Nothing -> putStrLn "..."
    help = RawProgramT $ putStrLn "YOU DON'T GET NO HELP"

getInitialState :: IO State
getInitialState = do
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

instance Unrender String where
  unrender = unpack

instance Unrender (Maybe Bool) where
  unrender "True" = Just True
  unrender "False" = Just False
  unrender _ = Nothing  

main :: IO ()
main = void $ liftIO getInitialState >>= flip runCommanderT (run example)
