MonadPointer
============

MonadPointer aims at helping you

- write functions that target a specific monad transformer of some stack without using mtl typeclasses;
- run those functions against a stack without having to count the number of lifts to do. Just call mpoint and let the swathe of GHC type extensions work for you.

It may require a bit of extra type hints, though.

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
readerAct :: Int -> Int -> At (ReaderT Int) String
readerAct x y = do z <- ask
                return (show (x+y+z))
```

where the type is simply an alias for:

```haskell
readerAct :: (Monad m) => Int -> Int -> ReaderT Int m String
```

So you are explicitely saying that your function needs a ReaderT Int behavior but can run in any monad stack provided it contains a ReaderT Int somewhere. (You actually say the same than with MTL, only without the need to introduce).

However, what if your final monad stack looks like:

```haskell
fn :: StateT MyWorld (ReaderT Configuration (WriterT String (ReaderT Int IO))) ()
```

Then your ReaderT Int is pushed deep down the stack, and you have to insert 3 lifts to execute actions in it:

```haskell
fn = do count <- lift (lift (lift readerAct))
        lift (lift (tell count)) -- we log the number logged so far.
```

It is a bit ugly, and everytime you want to execute one action in your stack, you have to look at it to count the number of lifts you should insert. Cumbersome. MonadPointer allows you do just replace whatever amount of lifts by:

```haskell
fn = do count <- mpoint readerAct
        mpoint (tell count) -- we log the number logged so far.
```

And if you _really_ can't stand the boilerplate introduced by mpoint, you may simply declare polymorphic variants of your stack-accessing functions:

```haskell
get' = mpoint get
put' = mpoint . put
ask' = mpoint ask
tell' = mpoint . tell
```

And then you get fully polymorphic accessors without having to write a single class (PointableIn, the class behind mpoint, is generic enough). And you can then rewrite the code as:

```haskell
readerAct x y = do z <- ask'
                   return (show (x+y+z))

fn = do count <- readerAct
        tell' count
```

The gotcha that remains is anyway already present in MTL: if you have twice the same monad transformer (e.g. if you have two ```ReaderT Double``` in the stack), then mpoint will address the uppermost. But I wouldn't consider good practise to have twice the same transformer, as it's not clear what each Reader is meant to be used for (you have newtypes for clarifying this).
