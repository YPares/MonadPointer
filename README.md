MonadPointer
============

In the pure spirit of transformers, MonadPointer empowers the user to

- write functions that target a specific monad transformer of some stack without using mtl typeclasses;
- run those functions against a stack without having to count the number of lifts to do. Just call mpoint and let the swathe of GHC type extensions work for you.

It may require a bit of extra type hints, though.


```haskell
test :: (StateT Int (ReaderT Double (ReaderT Double IO))) String
test = do x <- mpoint $ helper 42
          liftIO $ print x
          return (show (x::Double))  -- type necessary, or else GHC wouldn't know which istance of show to call
          

helper :: (Num t) => t -> At (ReaderT t) t  -- At is just an alias to save some Constraint typing
helper x = (*x) <$> ask


--| If you run see you can see the helper is ran against the _first_ ReaderT Double of the stack.
x = test <:: flip runStateT 3 <:: flip runReaderT 10 <:: flip runReaderT 1
{- >> x
   420
   ("420",1) -}
```

(The (<::) operator is just a reversed application operator, so the order in which the transformers are ran may be read from left to right)
