# Understanding the Fin type

-# 2025-02-21 #-

I have been taking an interest in Edwin Brady's [Idris](https://www.idris-lang.org/) lately, and was reading its introductory documentation when I suddenly came face-to-face with the following data definition.

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

What confused me so much was not *what* it was, but *how* it was. We are told `Fin n` is just a type with exactly `n` elements [^1], but do we believe that?

## What elements are we actually dealing with?

We can answer this question by examining each constructor in turn, where I will bring back the implicit `∀` as I believe it makes the understanding a lot easier. I'm going to be going back and forth between reasoning in \(\mathbf{Set}\) and \(\mathbf{Type}\) depending on what feels the most natural. Also, we take \(0 \in \mathbb{N}\) as convention.

### Rule 1

```hs
FZ : ∀ k . Fin (S k)
```

This tells us that `FZ` is an element of every `Fin n` where `n != Z`. I.e., \[\forall k \in \mathbb{N},\; \text{FZ} \in \text{Fin } (k + 1).\]

### Rule 2

```hs
FS : ∀ k . Fin k -> Fin (S k)
```

This tells us that whenever we have an element `a` of type `Fin k`, we have an element of type `Fin (k + 1)`, namely `FS a`. I.e.,
\[\forall k \in \mathbb{N},\; a \in \text{Fin } k \implies \text{FS}\, a \in \text{Fin } (k + 1).\] Starting with \(\text{FZ} \in \text{Fin} \, (k + 1)\), we can follow this chain to see that
\[\begin{aligned} \text{FZ} &\in \text{Fin } (k + 1) \\\implies\text{FS FZ} &\in \text{Fin } (k + 2) \\\implies (\text{FS} \circ \text{FS})\, \text{FZ} &\in \text{Fin } (k + 3) \\\implies (\text{FS} \circ \text{FS} \circ \text{FS})\, \text{FZ} &\in \text{Fin } (k + 4) \\ &\vdots \\\implies \underset{n \text{ times}}{\left(\text{FS} \circ \cdots \circ \text{FS}\right)}\, \text{FZ} &\in \text{Fin } (k + n + 1)\end{aligned}\] for all \(n,\, k \in \mathbb{N} \).

## Seems pretty natural

There is a kind of obvious analogy with these constructors and those given for the type `Nat`. Namely, we had that every element of type `Fin k` takes the form
\[\underset{n \text{ times}}{\left(\text{FS} \circ \cdots \circ \text{FS}\right)}\, \text{FZ}\] for some \(n \in \mathbb{N}\), and we see similarly that every element of type `Nat` takes the form
\[\underset{n \text{ times}}{\left(\text{S} \circ \cdots \circ \text{S}\right)}\, \text{Z}\] for some \(n \in \mathbb{N}\),
and we can use this to define the following function:

```hs
finToNat : Fin n -> Nat
finToNat FZ     = Z
finToNat (FS k) = S (finToNat k)
```

This function is obviously injective, so we have an upper bound for the size of our `Fin` type (namely, it is at most countably infinite...). What I claim is that this function is also surjective, when we restrict the codomain to exactly \(\mathbb{N}_{<n}\). If this were true, we'd have our desired result that \(\left|\text{Fin}\,n\right| = n\), and the convenient isomorphism \(\text{Fin}\,n \cong \{0,1,2,\ldots,n-1\}\)!

Consider \(m = \underset{m \text{ times}}{\left(\text{S} \circ \cdots \circ \text{S}\right)}\, \text{Z} \in \mathbb{N}_{<n}\). Then clearly \[a := \underset{m \text{ times}}{\left(\text{FS} \circ \cdots \circ \text{FS}\right)}\, \text{FZ}\] is mapped to \(m\) by `finToNat`, so all we need to show is that \(a \in \text{Fin}\,n\).

We can directly apply our result from the last section to show that for all \(k \in \mathbb{N}\),
\[a = \underset{m \text{ times}}{\left(\text{FS} \circ \cdots \circ \text{FS}\right)}\, \text{FZ} \in \text{Fin} (k + m + 1),\] so this is equivalent to finding some \(k \in \mathbb{N}\) such that \(k + m + 1 = n\). In other words, we need \[\mathbb{N} \ni k = n - m -1.\]
But \(m < n\) by assumption, as \(m \in \mathbb{N}_{<n}\). So
\[\begin{aligned}&n - m \geq 1\\\therefore\quad & n - m - 1 \in \mathbb{N},\end{aligned}\] but \(m \in \mathbb{N}_{<n}\) was chosen arbitrarily, and so we're done.

## The end

In the end, it wasn't that complicated. So why was this type so confusing, at least for me? In hindsight, I think it was largely because most of the introductions I've seen present the property as matter-of-fact, so I thought it should be immediately obvious from the definition (without considering an isomorphism), and felt confused when it wasn't. But the proof was not long, so maybe it *is* immediately obvious to you! Aside from that, as a [friend of mine](https://anna-maths.xyz/) put it:

> It's not the most intuitive thing because you aren't [directly] defining the elements of one set, but defining which sets each possible element belongs to[.]

I hope you found this post useful. If you did, you can let me know by leaving a comment via fedi (link below). If you thought it was terrible, or have constructive feedback, then you can also let me know in the same place! I'm sure I've made a mistake somewhere or missed something completely obvious.

### Footnotes

[^1]: Specifically with \(n \leq n' \iff \text{Fin } n \subseteq \text{Fin } n'\).
