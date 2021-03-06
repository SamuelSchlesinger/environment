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

data Arg :: Symbol -> * -> *

data Opt :: Symbol -> Symbol -> * -> *

data Named :: Symbol -> *

data Usage :: * -> *

data (&) :: k -> * -> *
infixr 4 &

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

data CommanderT state m a
  = Action (state -> m (CommanderT state m a, state))
  | Defeat
  | Victory a
  deriving Functor

data Documentation
  = DocSub String String [(String, Documentation)]
  | DocFlag String (Maybe String)
  | DocOpt String String String (Maybe String) Documentation
  | DocArg String (Maybe String) Documentation
  | DocNamed String (Maybe String) Documentation
  deriving (Eq, Show, Read)

runCommanderT :: Monad m 
              => CommanderT state m a 
              -> state 
              -> m (Maybe a)
runCommanderT (Action action) state = do
  (action', state') <- action state
  m <- runCommanderT action' state'
  return m
runCommanderT Defeat _ = return Nothing
runCommanderT (Victory a) _ = return (Just a)

instance (Monad m) => Applicative (CommanderT state m) where
  (<*>) = ap
  pure = Victory

instance MonadTrans (CommanderT state) where
  lift ma = Action \state -> do
    a <- ma
    return (pure a, state)

instance MonadIO m => MonadIO (CommanderT state m) where
  liftIO ma = Action \state -> do
    a <- liftIO ma
    return (pure a, state)

-- Return laws:
-- Goal: return a >>= k = k a
-- Proof: return a >>= k 
--      = Victory a >>= k 
--      = k a 
--      = k a
-- Goal: m >>= return = m
-- Proof:
--   Case 1: Defeat >>= return = Defeat
--   Case 2: Victory a >>= return 
--         = Victory a
--   Case 3: Action action >>= return
--         = Action \state -> do
--             (action', state') <- action state
--             return (action' >>= return, state')
--
-- Case 3 serves as an inductive proof only if action' is a strictly smaller action
-- than action!
--
--  Bind laws:
--  Goal: m >>= (\x -> k x >>= h) = (m >>= k) >>= h
--  Proof: 
--    Case 1: Defeat >>= _ = Defeat
--    Case 2: Victory a >>= (\x -> k x >>= f)
--          = k a >>= f
--          = (Victory a >>= k) >>= f
--    Case 3: Action action >>= (\x -> k x >>= h)
--          = Action \state -> do
--              (action', state') <- action state
--              return (action' >>= (\x -> k x >>= h), state')
--          = Action \state -> do
--              (action', state') <- action state
--              return ((action' >>= k) >>= h, state') -- by IH
--    On the other hand,
--            (Action action >>= k) >>= h
--          = Action (\state -> do
--              (action', state') <- action state
--              return (action' >>= k, state') >>= h
--          = Action \state -> do
--              (action', state') <- action state
--              return ((action' >>= k) >>= h, state')
--               
--   This completes our proof for the case when these are finite.
--   Basically, we require that the stream an action produces is strictly
--   smaller than any other streams, for all state inputs. The ways that we
--   use this monad transformer satisify this constraint. If this
--   constraint is not met, many of our functions will return bottom.
--
--   We can certainly have functions that operate on these things and
--   change them safely, without violating this constraint. All of the
--   functions that we define on CommanderT programs preserve this
--   property.
--
--   An example of a violating term might be:
--
--   violator :: CommanderT state m
--   violator = Action (\state -> return (violator, state))
--
--   The principled way to include this type would be to parameterize it by
--   a natural number and have that natural number decrease over time, but
--   to enforce that in Haskell we couldn't have the monad instance
--   anyways. This is the way to go for now, despite the type violating the
--   monad laws potentially for infinite inputs. 
instance Monad m => Monad (CommanderT state m) where
  Defeat >>= _ = Defeat
  Victory a >>= f = f a
  Action action >>= f = Action \state -> do
    (action', state') <- action state
    return (action' >>= f, state')

instance Monad m => Alternative (CommanderT state m) where
  empty = Defeat 
  Defeat <|> a = a 
  v@(Victory _) <|> _ = v
  Action action <|> p = Action \state -> do
    (action', state') <- action state 
    return (action' <|> p, state')

data State = State 
  { arguments :: [Text]
  , options :: HashMap Text Text
  , flags :: HashSet Text }

class HasProgram p where
  data ProgramT p (m :: * -> *) a
  run :: ProgramT p IO a -> CommanderT State IO a
  hoist :: (forall x. m x -> n x) -> ProgramT p m a -> ProgramT p n a
  invocations :: [Text]

instance (Unrender t, KnownSymbol name, HasProgram p) => HasProgram (Arg name t & p) where
  newtype ProgramT (Arg name t & p) m a = ArgProgramT { unArgProgramT :: t -> ProgramT p m a }
  run f = Action $ \State{..} -> do
    case arguments of
      (x : xs) -> 
        case unrender x of
          Just t -> return (run (unArgProgramT f t), State{ arguments = xs, .. })  
          Nothing -> return (Defeat, State{..})
      [] -> return (Defeat, State{..})
  hoist n (ArgProgramT f) = ArgProgramT (hoist n . f)
  invocations = [(("<" <> pack (symbolVal (Proxy @name)) <> "> ") <>)] <*> invocations @p

instance (HasProgram x, HasProgram y) => HasProgram (x + y) where
  data ProgramT (x + y) m a = ProgramT x m a :+: ProgramT y m a
  run (f :+: g) = run f <|> run g
  hoist n (f :+: g) = hoist n f :+: hoist n g
  invocations = invocations @x <> invocations @y

infixr 2 :+:

instance HasProgram Raw where
  newtype ProgramT Raw m a = RawProgramT { unRawProgramT :: m a }
  run = liftIO . unRawProgramT
  hoist n (RawProgramT m) = RawProgramT (n m)
  invocations = [mempty]

instance HasProgram p => HasProgram (Usage p) where
  data ProgramT (Usage p) m a = UsageProgramT
  run _ = Action \s -> do
    liftIO $ do
      putStrLn "usage:"
      void . traverse (putStrLn . unpack) $ invocations @p
    return (Defeat, s)
  hoist _ _ = UsageProgramT
  invocations = [mempty]

instance (KnownSymbol name, KnownSymbol option, HasProgram p, Unrender t) => HasProgram (Opt option name t & p) where
  newtype ProgramT (Opt option name t & p) m a = OptProgramT { unOptProgramT :: Maybe t -> ProgramT p m a }
  run f = Action $ \State{..} -> do
    case HashMap.lookup (pack $ symbolVal (Proxy @option)) options of
      Just opt' -> 
        case unrender opt' of
          Just t -> return (run (unOptProgramT f (Just t)), State{..})
          Nothing -> return (Defeat, State{..})
      Nothing  -> return (run (unOptProgramT f Nothing), State{..})
  hoist n (OptProgramT f) = OptProgramT (hoist n . f)
  invocations = [(("-" <> (pack $ symbolVal (Proxy @option)) <> " <" <> (pack $ symbolVal (Proxy @name)) <> "> ") <>)  ] <*> invocations @p

instance (KnownSymbol flag, HasProgram p) => HasProgram (Flag flag & p) where
  newtype ProgramT (Flag flag & p) m a = FlagProgramT { unFlagProgramT :: Bool -> ProgramT p m a }
  run f = Action $ \State{..} -> do
    let presence = HashSet.member (pack (symbolVal (Proxy @flag))) flags
    return (run (unFlagProgramT f presence), State{..})
  hoist n = FlagProgramT . fmap (hoist n) . unFlagProgramT
  invocations = [(("~" <> (pack $ symbolVal (Proxy @flag)) <> " ") <>)] <*> invocations @p

instance (KnownSymbol name, HasProgram p) => HasProgram (Named name & p) where
  newtype ProgramT (Named name &p) m a = NamedProgramT { unNamedProgramT :: ProgramT p m a }
  run = run . unNamedProgramT 
  hoist n = NamedProgramT . hoist n . unNamedProgramT
  invocations = [((pack (symbolVal (Proxy @name)) <> " ") <>)] <*> invocations @p

instance (KnownSymbol seg, HasProgram p) => HasProgram (seg & p) where
  newtype ProgramT (seg & p) m a = SegProgramT { unSegProgramT :: ProgramT p m a }
  run s = Action $ \State{..} -> do 
    case arguments of
      (x : xs) -> 
        if x == pack (symbolVal $ Proxy @seg) 
          then return (run $ unSegProgramT s, State{arguments = xs, ..})
          else return (Defeat, State{..})
      [] -> return (Defeat, State{..})
  hoist n = SegProgramT . hoist n . unSegProgramT
  invocations = [((pack $ symbolVal (Proxy @seg) <> " ") <> )] 
            <*> invocations @p
 
initialState :: IO State
initialState = do
  args <- getArgs
  let (opts, args', flags) = takeOptions args
  return $ State args' (HashMap.fromList opts) (HashSet.fromList flags) 
    where
      takeOptions :: [String] -> ([(Text, Text)], [Text], [Text])
      takeOptions = go [] [] [] where
        go opts args flags (('~':x') : z) = go opts args (pack x' : flags) z
        go opts args flags (('-':x) : y : z) = go ((pack x, pack y) : opts) args flags z
        go opts args flags (x : y) = go opts (pack x : args) flags y
        go opts args flags [] = (opts, reverse args, flags)

command_ :: HasProgram p 
         => ProgramT p IO a 
         -> IO ()
command_ prog = void $ initialState >>= runCommanderT (run prog)

command :: HasProgram p 
        => ProgramT p IO a 
        -> IO (Maybe a)
command prog = initialState >>= runCommanderT (run prog)

arg :: KnownSymbol name
    => (x -> ProgramT p m a) 
    -> ProgramT (Arg name x & p) m a 
arg = ArgProgramT

-- | Option combinator, taking a name for the argument, a long option
-- name and a short option name.
opt :: (KnownSymbol option, KnownSymbol name)
    => (Maybe x -> ProgramT p m a) 
    -> ProgramT (Opt option name x & p) m a
opt = OptProgramT

-- | Raw monadic combinator
raw :: m a 
    -> ProgramT Raw m a
raw = RawProgramT

-- | Subcommand combinator
sub :: KnownSymbol s 
    => ProgramT p m a 
    -> ProgramT (s & p) m a
sub = SegProgramT

-- | Named command combinator, should only really be used at the top level.
named :: KnownSymbol s 
      => ProgramT p m a 
      -> ProgramT (Named s & p) m a
named = NamedProgramT

-- | Boolean flag combinator
flag :: KnownSymbol f 
     => (Bool -> ProgramT p m a) 
     -> ProgramT (Flag f & p) m a
flag = FlagProgramT

-- | A convenience combinator that constructs the program I often want
-- to run out of a program I want to write.
toplevel :: forall s p m a. (HasProgram p, KnownSymbol s, MonadIO m) 
         => ProgramT p m a 
         -> ProgramT (Named s & ("help" & Usage (Named s & p) + p)) m a
toplevel p = named (sub usage :+: p) where

-- | A meta-combinator that takes a type-level description of a command 
-- line program and produces a simple usage program.
usage :: HasProgram p => ProgramT (Usage p) m a
usage = UsageProgramT
