module Utils (stateExe, stateExeT) where

import Control.Monad.State.Lazy

stateExe :: s -> State s a -> a
stateExe st s = evalState s st

stateExeT :: Monad m => s -> StateT s m a -> m a
stateExeT st s = evalStateT s st
