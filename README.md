MonadPointer
============

MonadPointer aims at helping you

- write functions that target a specific monad transformer of some stack without using mtl typeclasses, and without specifying the whole transformer stack;
- run those functions against a stack without having to count the number of lifts to do. Just call mpoint and let the swathe of GHC type extensions work for you.

Note that it's a bit rough for now and will require extra type hinting.

The idea is that functions like
```haskell
readerAct :: Int -> Int -> Reader Int String
readerAct x y = do z <- ask
                   return (show (x+y+z))
```

Are difficult to reuse, because they force you to run in Reader. MTL came with the solution:

```haskell
readerAct :: (MonadReader Int m) => Int -> Int -> m String
readerAct x y = do z <- ask
                   return (show (x+y+z))
```

It allows readerAct to be use with a monad transformer, but it needs to introduce a new class per Monad, and to write an instance of each class for each Monad. So if I have 4 monads (Reader, Writer, State, Maybe), I will need to write 16 instances (that's what MTL does).

My idea was to stay with the defined types of the transformers package, and to ensure composability, _always_ use the transformer versions. This goes:

```haskell
readerAct :: (MA m) => Int -> Int -> ReaderT Int m String
readerAct x y = do z <- ask
                return (show (x+y+z))
```

where the type is simply an alias for:

```haskell
readerAct :: (Monad m, Applicative m) => Int -> Int -> ReaderT Int m String
```

So you are explicitely saying that your function needs a ReaderT Int behavior but can run whatever the monad beneath the ReaderT Int. (close to what you would do with mtl with MonadReader).

However, what if your final monad stack looks like:

```haskell
fn :: StateT MyWorld (ReaderT Configuration (WriterT String (ReaderT Int IO))) ()
```

Then your ReaderT Int is pushed deep down the stack, and you have to insert 3 lifts to execute actions in it:

```haskell
fn = do count <- lift (lift (lift readerAct))
        lift (lift (tell count))
```

It is a bit ugly, and everytime you want to execute one action in your stack, you have to look at it to count the number of lifts you should insert. Cumbersome. MonadPointer allows you to just replace whatever amount of lifts by:

```haskell
fn = do count <- mpoint readerAct
        mpoint (tell count)
```

If now you want to make an action the requires _some_ monad
transformers to be reachable, yet without being tied to a specific stack,
you can use MTSet:

```haskell
test :: (MTSet '[StateT Int, ReaderT Double] m, MonadIO m) => m String
test = do x <- mpoint $ helper 42
          y <- mpoint get
          liftIO $ print x
          return (show $ (x::Double) + fromIntegral (y::Int))
```

(yep, GitHub markdown doesn't like the type list)

Here you say that m will have to contain a StateT Int and a State
Double, in whatever order and possibly with other transformers between
them. I'll try to add a MTList equivalent that will enforce an order between
the transformers.

As you can see, you cannot really do without type hints. But I still find it clearer than explicit lift (lift (lift ...))). However, note that the test explicit type can be removed if you use NoMonomorphismRestriction (provided you leave the type hints at the last line of test in place).


Note that conversely to MTL, MonadPointer does not rely on
OverlappingInstances: it requires and proves statically that your
monad stack won't for instance contain two StateT Int for instance, so
there are no ambiguities in which transformer mpoint should address!



**BEWARE:** The text beneath needs some rewriting!

And if you _really_ can't stand the boilerplate introduced by mpoint, you may simply declare polymorphic variants of your stack-accessing functions:

```haskell
ask' :: (PointableIn m (ReaderT r)) => m r
ask' = mpoint ask

put' :: (PointableIn m (StateT s)) => s -> m ()
put' x = mpoint (put x)

tell' :: (PointableIn m (WriterT w)) => w -> m ()
tell' x = mpoint (tell x)
```

And then you get fully polymorphic accessors without having to write a single class (PointableIn, the class behind mpoint, is generic enough). And you can then rewrite the code as:

```haskell
readerAct x y = do z <- ask'
                   return (show (x+y+z))

fn = do count <- readerAct
        tell' count
```
