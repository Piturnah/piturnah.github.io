---
mastohost: mathstodon.xyz
mastouser: piturnah
mastotoot: 114162019239955994

title: The Unreasonable Effectfulness of Haskell
published: 2025-03-14
description: Haskell is effectful.
---

One of the first things you hear when learning about functional programming is the idea that functional programs do not have side-effects. The author may then proceed to give some examples of side-effects, perhaps including:

- Interactions with state
- Input/Output
- Control flow, like loops

And from this you may walk away thinking something like

> What? These programming languages are clearly useless. How can I write code without I/O or even control flow?

But we've missed something. Haskell is just as effectful as any other programming language. We are claiming an omission of *side*-effects, not of effects in general. You can [fire the missiles](https://www.youtube.com/watch?v=iSmkqocn0oQ&t=130) after all.

## When is an effect not a side-effect?

You may be aware that functional programming finds its origins in what's known as the [λ-calculus](https://en.wikipedia.org/wiki/Lambda_calculus). The λ-calculus is particularly interesting to PL researchers and logicians because typed variants are easy to give a formal semantics to which can be mathematically reasoned about --- we find that they correspond with [categories](https://en.wikipedia.org/wiki/Category_theory); where the objects of the category correspond with the types of the language. Category theory taught us that effects may be modelled by *monads*.[^1]

As a programmer, you don't need to care about categories. Concretely what this means is that we can expect to find a *type* corresponding to computations which produce a given effect. A lack of side-effects doesn't mean our language lacks effects entirely ---  on the contrary, it means that effects are first-class. We can reason about them on the level of types!

### Example 1

In Python, we can freely loop over some integers and print them:

```python
for i in range(5):
    print(i)
```

In Haskell, we can do the same!

```haskell
forM_ [0..4] $ \i ->
    print(i)
```

What's the type of `for` in Python? It seems like a bizarre question to ask. `for` is control flow, why should it be typed? On the other hand, Haskell readily has an answer for this question:

```haskell
forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
```

The type tells us "given a list of `a`s, and a function which goes from `a -> b` producing some effect of type `m`, `forM_` will also give an effect of type `m`." In particular the final effect is just the sequence of the ones we get from applying the given function to all of our `a`s.

In our case `m` was `IO`, the type of Input/Output.

### Example 2


In Python, we can interact with a stateful value:

```python
def foo():
    x = 41
    x += 1
```

In Haskell, we can do the same!

```haskell
foo = do
    put 41
    modify (+ 1)
```

What's the type of the statement `x = 41`? Again, it's a weird question to be asking in Python-land. But why is it a weird question? Haskell knows: The type of `put 41` is just

```haskell
State Int ()
```

It's a thing which gives no value (an element of the unit type, `()`), and does some kind of stateful operation on a store of `Int`.

## Why should you care?

As programmers rather than computer scientists, the main reason we care about types in our languages is that it allows us to reason about notions of correctness. If my `add` function doesn't return a number, my compiler can tell me that. There seems to be a blind spot when it comes to effects, which is surprising given that doing arbitrary effectful things all over the shop is a sure-fire way to invalidate some invariants.

Just as I want to know if my `add` function isn't returning numbers, I also want to know if it's printing to the screen, or modifying program state! But, that doesn't mean I want to, or have to, give up those abilities.

## Sneaky Endnote

I must confess that I told a bit of a lie about control-flow. Haskell programs are declarative, and so *technically speaking* we don't have control-flow. But it doesn't matter --- we can produce the same effects regardless.

[^1]: Eugenio Moggi (1991). [*Notions of computation and monads*](https://www.cs.cmu.edu/~crary/819-f09/Moggi91.pdf).
