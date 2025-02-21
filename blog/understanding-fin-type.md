# Understanding the Fin type

-# 2025-02-21 #-

I have been taking an interest in Edwin Brady's [Idris](https://www.idris-lang.org/) lately, and was reading its introductory documentation when I suddenly came face-to-face withe following data definition.

```hs
data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)
```

The `Fin` type. I had seen this type before on my university's Agda module, which I am currently an undergraduate TA for. In Agda, it looked like this:

```hs
data Fin : ℕ → Type where
 zero : {n : ℕ} → Fin (suc n)
 suc  : {n : ℕ} → Fin n → Fin (suc n)
```

I never quite understood this type when I was taking the module. Actually, the second test happened to rely quite heavily on it, and my grade suffered as a result. So when I saw it as one of the *very first* examples of a dependent type in the Idris documentation, I was once again left feeling a bit stumped and a bit stupid. I had no problem with the rest of the module, so why couldn't I understand this one type which was apparently so simple it should be an introductory example?

What confused me so much was not *what* it was, but *how* it was. We are told `Fin n` is just a type with exactly `n` elements [^1], but do we we believe that?

## The complicated way (via ℕ)

We can start by examining each constructor in turn, where I will bring back the implicit `∀` as I believe it makes the understanding a lot easier.

```hs
FZ : ∀ k . Fin (S k)
```

This tells us that `FZ` is an element of every `Fin n` where `n != Z`. I.e, \[\forall n \geq 1,\; \text{FZ} \in \text{Fin } n.\]

```hs
FS : ∀ k . Fin k -> Fin (S k)
```

This tells us that whenever we have an element of `Fin k`, we have an element of `Fin (k + 1)`. This seems to make sense. It looks like we have \[\forall n < n',\; k \in \text{Fin } n \implies k \in \text{Fin } n'.\]

For a long time, this is where my understanding stopped, and I was left quite confused. However, we are taught to reason about elements of `Fin n` as natural numbers in the below sense:

```hs
finToNat : Fin n -> Nat
finToNat FZ     = Z
finToNat (FS k) = S (finToNat k)
```

So really, the rule tells us \[\forall n \leq n',\; k \in \text{Fin } n \implies k + 1 \in \text{Fin } n',\] where we now understand that \(\text{Fin }n \subset \mathbb{N}\) up to isomorphism [^2]. This comes directly from the equality `finToNat (k + 1) == (finToNat k) + 1`. We *could* have done this induction directly, but I think it is easier to see via the natural numbers. Starting with `FZ : Fin (S k)`, we can follow this chain to see that
\[\begin{aligned} &0 \in \text{Fin } (m + 1) \\\implies&1 \in \text{Fin } (m + 2) \\\implies &2 \in \text{Fin } (m + 3) \\\implies &3 \in \text{Fin } (m + 4) \\ &\vdots \\\implies &n \in \text{Fin } (m + n + 1)\end{aligned}\] up to isomorphism for all \(n,\, m \in \mathbb{N} \).

## The simpler way (via elements)

If this still doesn't make sense, the thing that finally made it click *intuitively* for me was examining the elements of `Fin n` in more detail. Every element `fin` of type `Fin n` is of the following form: 
```hs
fin = (FS . ... . FS) FZ
--    ^^^^^^^^^^^^^^^ m times
```

Recalling that `FZ : ∀ k . Fin (S k)`, and `FS : ∀ k . Fin k -> Fin (S k)`, this means that 
```hs
fin : ∀ n . Fin (m + S n)
```
Since `m` may take any value of type `Nat`, we may use our `finToNat` injection to once again conclude that
\[\forall n,\, m \in \mathbb{N},\; n \in \text{Fin } (m + n + 1)\] up to isomorphism.

## One loose-end

Lots of you reading will have noticed that we have actually only shown that `Fin n` contains *at least* `n` elements, rather than exactly `n`. This can be shown by observing that the function we defined, `finToNat`, is not only injective but actually also surjective when the codomain is restricted to \(\mathbb{N}_{<n}\).

### Footnotes

[^1]: Specifically with \(n \leq n' \iff \text{Fin } n \subseteq \text{Fin } n'\).

[^2]: Strictly speaking we only showed \(\subseteq\), but we should be very suprised if there is any \(n\) with \(\text{Fin } n \cong \mathbb{N}\). 😛
