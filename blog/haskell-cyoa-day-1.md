---
mastohost: hachyderm.io
mastouser: piturnah
mastotoot: 113454204285111953
---

# Building a Choose-Your-Own-Adventure Style Text Game Using Day 1 Haskell

-# 2024-11-09 #-

I have been writing a lot of Haskell lately, and enjoying it a great deal. This has come to be due to my final year university project in [PL theory](https://en.wikipedia.org/wiki/Programming_language_theory). Haskell is a very versatile language with an extremely expressive type system that can be a joy to write, but beginners often struggle to get a foothold on it. Coming from the world of imperative languages, there's a lot of new abstractions to become familiar with (such as the dreaded Monad), as we are ripped from our world of writing precedures to one where we must get by with function composition alone.

I was chatting with a good friend of mine who is currently taking a one-semester module on functional programming using Haskell, and like many people is finding it hard to get anything to "click".

> I think my issue is [that] unlike other languages where I could almost immediately see something I could make with it [...] I just don't see that in Haskell. Like I was trying to make cyoa games in Python from day 1.

The person who said this is a very competent game developer who has been writing code for a long time and has experience in multiple languages. This stuck in my head, and I began considering how *I* might write a [CYOA](https://en.wikipedia.org/wiki/Choose_Your_Own_Adventure) style game using only very simple Haskell, and I came to the conclusion that this is a very achievable goal.

I empathise a lot with the position that Haskell is obscure for new users, and I thought that perhaps some people may benefit from seeing this simple example of a "real" program a beginner may come up with, using only the kind of Haskell you would see in the very first stages of learning the language.

## Modelling our Game

In a choose-your-own-adventure game, the player must make a series of choices which come to form a story. We can model this quite simply as a kind of single-player turn-based game, where each turn is a choice the player must make. Then a choice consists of a prompt, and a list of options to pick from which will determine which choice we will present next.

Haskell is a language of types and functions on types. When we go to model our game in Haskell, we have to translate each part of our model into one of those two things.

```haskell
data Choice = Choice {
  prompt :: String,
  opts :: [(String, Int)]
}

allChoices :: [Choice]
allChoices = undefined
```

Here we define the type of choices, with a single record constructor. A `Choice` has two fields. `prompt` is the text that will be shown to the player, and `opts` is a list of options that the player may take. Each option is a pair of a string (*the text to go along with the option*), and an `Int`, which will tell us the index into `allChoices` that leads to the corresponding choice.

Let's talk a bit about the data that falls under our `Choice` type. We gave one constructer, `Choice`, with two fields. Then to create a value of type `Choice`, we use the keyword `Choice` followed by the data of each field:

```haskell
exampleChoice = Choice "Here's the prompt." [("Here's a list of one option.", 0)]
```

Haskell then automatically provides the following functions:

```haskell
prompt :: Choice -> String
opts :: Choice -> [(String, Int)]
```

We can use these functions to access our data.

```console
ghci> prompt exampleChoice
"Here's the prompt."
```

## The Event Loop

A common paradigm for creating games is to run an event loop. In our case, every iteration of the loop should be a choice for the player to make.

> *But wait!* I thought we couldn't write loops in Haskell?

I hear you, but the answer is simple: *recursion*. When we get to the end of the loop, we can just call it again. Haskell implements a common type of optimisation, called [tail-call elimination](https://en.wikipedia.org/wiki/Tail_call), to make this kind of thing efficient.

Our loop will have the following type:

```haskell
loop :: Int -> IO ()
```

As established, we are looking up choices by indexing into `allChoices`. Therefore, each iteration of the loop is passed an `Int` representing the index of the choice for that iteration. The return type of our loop is `IO ()`, which is Haskell's [IO monad](https://www.haskell.org/tutorial/io.html). I already mentioned that monads are a stumbling block for new Haskellers, and this is the only one we will have to use.

If you don't know what a monad is yet, we can just think of our loop as returning an *IO action*. Monads are a powerful tool for working with *effectful computations* in a purely functional way. Haskell then provides us some lovely syntactic sugar so we can write our monadic function as though we were back in imperative land.

Let's write an initial implementation for our event loop, calling some functions along the way that we haven't actually defined yet.

```haskell
-- Given a choice, let's display the player's potential options.
displayOptions :: Choice -> IO ()
displayOptions = undefined

-- Given a choice & an input string from the player, let's return the index of
-- the `Choice` corresponding to the option they took.
determineNextChoice :: Choice -> String -> Int
determineNextChoice = undefined

loop :: Int -> IO ()
loop idx = do
  -- First, we use the provided index to lookup the choice.
  let choice = allChoices !! idx
  -- We can use the `putStrLn` function to display the prompt.
  putStrLn (prompt choice)
  -- Next, let's use this undefined function to show the possible options.
  displayOptions choice
  -- Thanks to IO-monad magic, we can get some input from the player.
  input <- getLine
  -- Onto the next iteration!
  loop (determineNextChoice choice input)
```

## Adding Some Choices

Now that we've got the basic infrastructure set up, it's time to think about what our adventure will be. Let's get meta and write a CYOA game about creating a CYOA game.

```haskell
allChoices =
  [ Choice "You have embarked on a journey to create a CYOA game. How will you start?" [("Choose a programming language", 1), ("Give up and go home", 2)]
  , Choice "There are many programming languages to pick from. Which will you choose?" [("Python", 3), ("Haskell", 2)]
  , Choice "Unfortunately, you failed. Maybe next time." []
  , Choice "Congratulations! You've made a Steam #1 bestseller!" []
  ]

-- Now that we have populated `allChoices`, we can also define our entrypoint.
main = loop 0
```

Now all we have to do is implement our two undefined functions from earlier. We'll start with `determineNextChoice` as it's the easier of the two.

`determineNextChoice` takes a `Choice` and an input string and determines the global index of the choice corresponding to the option the player wants to take. In order for this to make sense, we have to define what kind of input we're expecting.

Let's say the user will see something like this:

```console
You have to pick from the following options.
 (0) Option A
 (1) Option B
```

This makes life easier for us as we can expect the user to input a number (0 or 1 in the above example) and we can use that for indexing. The prelude function `read :: Read a => String -> a` can be used to parse a string into a type that is an instance of (that is, a type that *implements*) `Read`. We can use GHCi to check if we do have `Read`.

```console
ghci> :info Int
instance Read Int -- Defined in ‘GHC.Read’
```

Great!

```haskell
determineNextChoice :: Choice -> String -> Int
determineNextChoice choice input = read input
```

The above compiles, but is it correct? If the numbers that the user gives index the options available for a given choice, then we need to turn that into a global index for `allChoices`. We included that information in the second part of the tuples of `opts`. Here's what we really want:

```haskell
determineNextChoice choice input = snd (opts choice !! read input)
```

`snd :: (a, b) -> b` extracts the second part of the tuples returned by `opts choice`.

Implementing `displayOptions` will be a bit trickier.
```haskell
displayOptions :: Choice -> IO ()
displayOptions choice = undefined
```

Let's work backwards. It's straightforward to extract the options from our choice.

```haskell
(opts choice) :: [(String, Int)]
```

If we only had one option, we could print it by extracting the first element of the pair and using `putStrLn`. I.e., if `opt :: (String, Int)` then we can print it with:

```haskell
(putStrLn . fst) opt
```

You may have heard of the very useful function `map :: (a -> b) -> [a] -> [b]`. `map` takes a function, and applies it to every element of the list. So, are we done?

```haskell
displayOptions :: Choice -> IO ()
displayOptions choice = map (putStrLn . fst) (opts choice)
```

If you try the above code, you'll see it fails to compile. What's going wrong is we're mapping a function of type `a -> IO ()` to a list `[a]`, which gives us something of type `[IO ()]`. What we need is some way to take a bunch of IO actions and turn them into one IO action which performs each in sequence.

When you're in a spot like this, [Hoogle](https://hoogle.haskell.org/) is your friend. You can use it to search for a function based on the *type* of function that you need. In this case, we worked out that we need something of type `[IO ()] -> IO ()`. Typing this into Hoogle, we see that the prelude provides the following function.

```haskell
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
```

What happened here is that Hoogle worked out that we can use a much more general function than the one we asked for, because `[]` is an instance of `Foldable` and `IO` is an instance of `Monad`. Perfect! Let's write our (nearly) finished function.

```haskell
displayOptions :: Choice -> IO ()
displayOptions choice = sequence_ $ map (putStrLn . fst) (opts choice)
```

We could call ourselves done now. But if we are to run our [AAA](https://en.wikipedia.org/wiki/AAA_(video_game_industry%29) game, we'll see something like the following:

```console
You have embarked on a journey to create a CYOA game. How will you start?
Choose a programming language
Give up and go home
```

Hmmm. It would be much nicer if we had something more like the example output we showed earlier. In order to do this, we need to get information about the index of each option when we map.

What we want is something like Python's `enumerate`, a function that takes a list of type `[a]` and turns it into a list of type `[(Int, a)]`, where the first value in the tuple gives us the index of the item.

Haskell provides `zip :: [a] -> [b] -> [(a, b)]`. Using the power of lazily evaluated lists, we can get `enumerate` by `zip`-ing a list with `[0..]`.

```haskell
displayOptions :: Choice -> IO ()
displayOptions choice = sequence_ $ map (putStrLn . showOpt) (zip [0..] $ opts choice)
  where showOpt (idx, (text, _)) = " (" ++ show idx ++ ") " ++ text
```

And now, we are done.

## Making Some Improvements

Our game is finished, and we could leave it there. That being said, there are several places where our implementation could easily be improved.

1. We don't *really* really handle the case where there are no options to take.
2. It is a lot of mental load on the programmer to make sure the indexes are accurate.

We'll focus on problem 2, as it is a lot more interesting.

### Solution 1. Using a string-ident lookup

One of the ways we could solve this problem is by associating each possible choice by a unique identifying string. For example:

```haskell
allChoices :: [(String, Choice)]
allChoices = [
  ("start", Choice "You have embarked on a journey to create a CYOA game. How will you start?" [("Choose a programming language", "choose-language"), ("Give up and go home", "go-home")])
  , -- and so on...
  ]
```

Then instead of indexing, we could provide this string and use Haskell's handy `lookup :: Eq a => a -> [(a, b)] -> Maybe b` function to pull choices out.

This is definitely a big improvement. It's a lot easier to keep track of meaningful names than it is indexes. Further, if the order of choices in the list changes, or one is removed, then all our work is not lost. However, there are still some problems with this approach.

The main one is that it is totally possible to accidentally mistype or misremember an identifier. The onus is on the programmer to go through and check every possible path to make sure this hasn't happened. Haskell has such a powerful type system, is there no way we could just get the compiler to do that work for us?

### Solution 2. Getting the compiler to do that work for us

If you are familiar with similar problems in other languages, you may have already thought of the idea of using enumerations: we can encode all the possible identifiers in the type system.

An enum in Haskell is just a datatype with several constructors:

```haskell
data ChoiceIdent
  = Start
  | ChooseLanguage
  | LoseGame
  | WinGame
  -- We'll derive an `Eq` instance so we can still use `lookup`.
  deriving Eq

allChoices :: [(ChoiceIdent, Choice)]
allChoices = [
  (Start, Choice "You have embarked on a journey to create a CYOA game. How will you start?" [("Choose a programming language", ChooseLanguage), ("Give up and go home", LoseGame)])
  , -- and so on...
  ]
```

This is getting pretty good, but there's still one glaring issue. How do we ensure exhaustiveness? For every possible `ChoiceIdent`, how do we know that we remembered to define the corresponding `Choice` in `allChoices`? Similarly, how do we know that we didn't accidentally use the same `ChoiceIdent` for more than one `Choice`?

Let's do away with all this nonsense and implement `allChoices` directly as what it really is: a map `ChoiceIdent -> Choice`.

```haskell
allChoices :: ChoiceIdent -> Choice
allChoices ident = case ident of
  Start          -> Choice "You have embarked on a journey to create a CYOA game. How will you start?" [("Choose a programming language", ChooseLanguage), ("Give up and go home", LoseGame)]
  ChooseLanguage -> Choice "There are many programming languages to pick from. Which will you choose?" [("Python", WinGame), ("Haskell", LoseGame)]
  LoseGame       -> Choice "Unfortunately, you failed. Maybe next time." []
  WinGame        -> Choice "Congratulations! You’ve made a Steam #1 bestseller!" []
```

From here, we just have to follow the compilation errors to work out where we need to provide a `ChoiceIdent` instead of an index. There are only a couple, I believe in you!

*Hang on a minute...* by improving the maintainability of our code and making it easier to see the paths, we've discovered an obvious bug! Choosing Haskell to make our CYOA game maps to `LoseGame`, but we've just shown how simple it really is!

## Bonus Chapter: Adding an Inventory System

Here is the whole code so far.

```haskell
data ChoiceIdent
  = Start
  | ChooseLanguage
  | LoseGame
  | WinGame

data Choice = Choice {
  prompt :: String,
  opts :: [(String, ChoiceIdent)]
}

allChoices :: ChoiceIdent -> Choice
allChoices ident = case ident of
  Start          -> Choice "You have embarked on a journey to create a CYOA game. How will you start?"
                    [("Choose a programming language", ChooseLanguage), ("Give up and go home", LoseGame)]
  ChooseLanguage -> Choice "There are many programming languages to pick from. Which will you choose?"
                    [("Python", WinGame), ("Haskell", LoseGame)]
  LoseGame       -> Choice "Unfortunately, you failed. Maybe next time." []
  WinGame        -> Choice "Congratulations! You’ve made a Steam #1 bestseller!" []

determineNextChoice :: Choice -> String -> ChoiceIdent
determineNextChoice choice input = snd (opts choice !! read input)

displayOptions :: Choice -> IO ()
displayOptions choice = sequence_ $ map (putStrLn . showOpt) (zip [0..] $ opts choice)
  where showOpt (idx, (text, _)) = " (" ++ show idx ++ ") " ++ text

loop :: ChoiceIdent -> IO ()
loop choiceIdent = do
  let choice = allChoices choiceIdent
  putStrLn (prompt choice)
  displayOptions choice
  input <- getLine
  loop (determineNextChoice choice input)

main = loop Start
```

Other than just creating a more in-depth story to explore, there are many ways we could improve our game to make it more fun. The world is your oyster. One such way, which we will briefly discuss, could be by adding an inventory system.

We already discovered the "bug" earlier where picking Haskell as our language results in us losing the game. One way we could fix that is by changing the story: either language will work, but only if we've installed the necessary compiler!

Let's add the relevant types.

```haskell
data Item
  = GHC
  | CPython
  deriving Eq -- We'll need `Eq` to check if items are present in the inventory.

type Inventory = [Item]
```

Clearly, the event loop is going to need to take the player's inventory into account.

```haskell
loop :: ChoiceIdent -> Inventory -> IO ()
```

To make life simpler for ourselves, we're going to assume that taking an option could add items to the player's inventory, but that there is no way to remove items. Implementing the ability to consume items is left as an exercise.

Okay, so each option now needs to know what items it can add to the inventory. We can do this by amending its type from `(String, ChoiceIdent)` to `(String, ChoiceIdent, [Item])`.

We also need a way to determine the next choice to go to based on what we have in our inventory. In that case, perhaps we shouldn't just provide a `ChoiceIdent` in each option but rather a function `Inventory -> ChoiceIdent`. Depending on what the player has, we may want to send them to different parts of the story. In the case where we don't care what is in the inventory, we can restore the old behaviour by using `const :: a -> b -> a`, where

```haskell
const a = \_ -> a
```

Let's see an example.

```haskell
data ChoiceIdent
  = Start
  | ChooseLanguage
  | LoseGame
  | WinGame
  | InstallCompiler

-- Let's extract this into a new type as it's getting a bit convoluted.
type Option = (String, Inventory -> ChoiceIdent, [Item])

data Choice = Choice {
  prompt :: String,
  opts :: [Option]
}

allChoices :: ChoiceIdent -> Choice
allChoices ident = case ident of
  Start           -> Choice "You have embarked on a journey to create a CYOA game. How will you start?"
                     [ ("Choose a programming language", const ChooseLanguage, [])
                     , ("Install compilers", const InstallCompiler, [])
                     , ("Give up and go home", const LoseGame, [])
                     ]
  ChooseLanguage  -> Choice "There are many programming languages to pick from. Which will you choose?"
                     [ ("Python"
                       -- We can use `elem` to check if an item is present in a list.
                       , \inv -> if elem CPython inv
                                 then WinGame
                                 else LoseGame
                       , [])
                     , ("Haskell"
                       , \inv -> if elem GHC inv
                                 then WinGame
                                 else LoseGame
                       , [])
                     ]
  InstallCompiler -> Choice "Here are the compilers available on your distro:"
                     [ ("CPython", const Start, [CPython])
                     , ("GHC", const Start, [GHC])
                     ]
  LoseGame        -> Choice "Unfortunately, you failed. Maybe next time." []
  WinGame         -> Choice "Congratulations! You’ve made a Steam #1 bestseller!" []
```

Now we need to update `determineNextChoice` so that it returns not only the next `ChoiceIdent`, but also any new items we may have acquired. We can do this with Haskell's pattern matching.

```haskell
determineNextChoice :: Choice -> String -> (ChoiceIdent, [Item])
determineNextChoice choice input = (ident, items)
  where (_, ident, items) = opts choice !! read input
```

> But wait! This code doesn't compile, as `ident` is now of type `Inventory -> ChoiceIdent`!

You're right. We can fix this by just passing in the player's inventory.

```haskell
determineNextChoice :: Choice -> Inventory -> String -> (ChoiceIdent, [Item])
determineNextChoice choice inventory input = (getIdent inventory, items)
  where (_, getIdent, items) = opts choice !! read input
```

We'll also update `displayOptions` so that `showOpt` matches against a triple rather than a pair.

```haskell
displayOptions :: Choice -> IO ()
displayOptions choice = sequence_ $ map (putStrLn . showOpt) (zip [0..] $ opts choice)
  where showOpt (idx, (text, _, _)) = " (" ++ show idx ++ ") " ++ text
```

Finally, we update the event loop to take these changes into account.

```haskell
loop :: ChoiceIdent -> Inventory -> IO ()
loop choiceIdent inventory = do
  let choice = allChoices choiceIdent
  putStrLn (prompt choice)
  displayOptions choice
  input <- getLine
  -- The next iteration of the loop will take the old inventory concatenated
  -- with the items we just unlocked.
  let (nextIdent, newItems) = determineNextChoice choice inventory input
  loop nextIdent (inventory ++ newItems)

main = loop Start []
```

And we're done! That's how easy it was to add a simple inventory system to our game!

## The End

I hope you found this tutorial useful. If you did, you can let me know by contacting me on fedi [(links on the homepage)](/). If you thought it was terrible, or have constructive feedback, then you can also let me know in the same places!

If you found it *really* useful, consider [buying me a coffee](https://ko-fi.com/piturnah).

I have put the whole code up on [Codeberg](https://codeberg.org/Piturnah/haskell-cyoa-day-1) under the MIT license. If you have a creative way to extend or improve it, feel free to submit a PR!

P.S. If you made it this far: *I know that CPython isn't a compiler.* ;)
