---
mastohost: mathstodon.xyz
mastouser: piturnah
mastotoot: 114152300811331994
---

# Rewriting my site in Haskell (using hakyll)

-# 2025-03-13 #-

The other day I spent a couple of hours learning about, and rewriting this website in, [Hakyll](https://jaspervdj.be/hakyll/) --- a site generator library for Haskell. In this post my plan is to write a little about why I did this and what the process was like, and whether it may be a good idea for your site, too!

## Why?

When I originally made this website back in 2019, it was just some HTML and CSS. It had one page, the root, with some links at the top to my GitHub[^1] and &c. Simple and it worked. As I started adding other stuff, like embedding web builds of some of my games, I got by with nothing more than a little bit of [jQuery](https://en.wikipedia.org/wiki/JQuery) to make stuff like reusing components more convenient.

A couple of years later, after having spent some of the intervening time doing freelance web development,[^2] I had the bright idea to rewrite everything using [Svelte](https://svelte.dev/), specifically as a static site generator. I quickly realised this was far too much infrastructure for a tiny personal homepage and reverted back to my original setup.

Still though, I wanted *more* flexibility. I wanted to do fun things at build time. But the other, less enterprise-y, solutions like [Jekyll](https://jekyllrb.com/) or [Hugo](https://gohugo.io/) also seemed too restrictive in the way of tying you to a specific framework. I wanted something site-agnostic. Not a lot more than "turn my markdown into html! let me put my documents in a layout template! let me do arbitrary transformations on the text!", but without making me do things *The Jeykll Way*, or whatever it may be.

### Shovel

All this led to me eventually writing my own site generator: [Shovel](https://codeberg.org/Piturnah/shovel-ssg). Shovel was written in Rust, and with the goal of being as agnostic as possible about how you want to lay out your stuff. Perhaps unsurprisingly, my naïve approach to this only resulted in *yet another site generator framework*. Worse still, it wasn't nearly as capable as the existing solutions, as whenever I needed a new feature I'd be stuck for weeks on how to implement it in a site-agnostic way.

### Discovering Hakyll

Eventually I made a great realisation. *I* know how I want to transform the documents in my site's build process and I just need a convenient tool to express that. We already have such tools: programming languages. We already have high-level programming languages which are turing complete. The perfect site generator isn't some CLI tool with a restrictive framework and mountains of awkward configuration, it's just a library in an ergonomic high-level language. [My friend Abi](https://www.akpain.net/) already understood this; her site being built by a bespoke Python script.

I posted a toot about this thought and within minutes I got [a reply](https://mathstodon.xyz/@jj@types.pl/114141092743989492) recommending that I check out Hakyll. Hakyll immediately seemed like what I was looking for. I could have pivoted Shovel into being a more bespoke thing more explicitly just for my site, but the problem with Rust is that it *sucks* if you don't need the low-level access and speed. Haskell, on the other hand, is a beautifully ergonomic language which is a joy to write for those tasks where you don't need Rust's main offerings.

## How?

Hakyll's API is centered primarily around a couple of monads it exposes. The first is the `Rules` monad, which you can find the docs for [here](https://hackage.haskell.org/package/hakyll-4.16.6.0/docs/Hakyll-Core-Rules.html). Hakyll's entry point is a function

```haskell
hakyll :: Rules a -> IO ()
```

which takes some rules and "does IO", i.e., generates your site based on them. In a rule you usually quantify over some source files --- which you can retrieve via a glob --- and specify things such as what route they should be mapped to and how they should be compiled. For example, here is the rule which turns most of my `.md` files into rendered pages:

```haskell
mdRule = match "**.md" $ route mdRoute >> compileWith pandocCompiler
```

where `mdRoute` and `compileWith` are also defined by me, not as part of the library. I'm basically just saying, "for files ending in `.md`, render them with [pandoc](https://pandoc.org/) to this specific path." For those wondering, `mdRoute` just transforms the original filepath by moving it into a subdirectory and changing the name to `index.html`. This is super readable and convenient in Haskell.

```haskell
mdRoute :: Routes
mdRoute = customRoute $ (</> "index.html") . dropExtension . toFilePath
```

For comparison, here was the corresponding Rust code from Shovel:

```rust
/// Give the path of an input file and it will create the relevant directory tree in the output
/// directory, returning a `PathBuf` to where the output file will go.
fn get_output_path(&self, build_path: &Path, file_path: &Path, dir: bool) -> Result<PathBuf> {
    let relative_path = file_path.strip_prefix(&self.root)?;
    let mut output_path = PathBuf::new();
    output_path.push(build_path);
    output_path.push(relative_path);
    let parent = if dir {
        output_path.set_extension("");
        output_path.clone()
    } else {
        output_path
            .parent()
            .context("failed to get file parent")?
            .to_path_buf()
    };
    fs::create_dir_all(parent).context("failed to create parent directory")?;
    Ok(output_path)
}

fn build(&self, path: &str, use_ws: bool) -> Result<()> {
    // ...
    let mut output_path =
        self.get_output_path(Path::new(path), file.path(), true)?;
    output_path.push("index.html");
    let mut buf =
        File::create(output_path).context("failed to open file for writing")?;
    // ...
}
```
To be a bit fair, the Rust code is also doing filesystem stuff whereas the Haskell one is only the bit concerned with transforming the path. But it's clear that even the path transformation was a *pain* before and gross to read[^3] --- this is one of the clear benefits to using a higher-level language.

You specify how a rule should compile (e.g., what layouts to apply, what compiler to use, &c.) using the `Compiler` monad ([docs](https://hackage.haskell.org/package/hakyll-4.16.6.0/docs/Hakyll-Core-Compiler.html)). For example, here's the bit of my site config which extends a given compiler with the layout from the given `Item`'s metadata (glossing over the details of `Item`s and metadata for now).

```haskell
compileWith :: Compiler (Item String) -> Rules ()
compileWith compiler = compile $ do
    identifier <- getUnderlying
    layout <- getMetadataField identifier "layout"
    compiler >>= case layout of
        Just "none" -> pure
        Just path -> loadAndApplyTemplate (fromFilePath path) defaultContext
        Nothing -> loadAndApplyTemplate "_templates/default.html" defaultContext
```

### Contexts

Compilation is done in a certain `Context` ([docs](https://hackage.haskell.org/package/hakyll-4.16.6.0/docs/Hakyll-Web-Template-Context.html)), which basically consists of a bunch of key-value pairs. It's implemented as a `Monoid` so it's super convenient to work with.

One of the places you can access the values from the context is in templating. I will showcase a place I have used this on the site: the blog! My blog layout is very similar to my default layout ([example](/reading-list)), but with extra stuff such as the [Mastodon comments](https://github.com/dpecos/mastodon-comments) section. The context system allows me to use the same template for both, where for example I can specify that I want to include certain header stuff only when the `"blog"` key is present:

```html
<head>
$if(blog)$
  <script
    type="module"
    src="https://cdn.jsdelivr.net/gh/dpecos/mastodon-comments@6fe36e7e308412cc02f26ae1477987a75d32e58f/mastodon-comments.js"
  />
$endif$
</head>
```

which I can make true by just appending it to the `defaultContext` in the relevant compiler:

```haskell
main :: IO ()
main = hakyll $ do
    -- ...
    match "blog/**.md" $ do
        route mdRoute
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate
                    "_templates/default.html"
                    (defaultContext <> emptyField "blog")
    -- ...
```

### The downsides

Not everything is perfect about Hakyll, believe it or not. The first thing that frustrated me was that it doesn't really seem to be possible to do templating in a regular file and compile that, you can only do it via a layout template. This resulted in me having to do a little janky solution to render my `index.html`:

```haskell
main :: IO ()
main = hakyll $ do
    match "_templates/*" . compile $ templateCompiler
    create ["index.html"] $ do
        route idRoute
        compile . loadAndApplyTemplate "_templates/index.html" defaultContext $
            Item "index.html" ""
    -- ...
```

what I'm doing here is basically compiling `_templates/index.html` as a template, and then creating a new empty item called "index.html" which is not associated to any file and rendering it into the aforementioned template. This isn't the end of the world, and, the biggest benefit of using Hakyll is that we're working in a turing-complete high-level language. I can just make a special extension for files I want to render this way, and write some code to handle them this way automatically. For now I only need it for the root, and so this solution is fine.

Still, it seems a little weird that this apparently basic want is not included in the library. Maybe it is, and I just couldn't find it?

The next downside, and this is a major one, is the new CI compile time. On CI, my current solution is to compile all the dependencies on the server and build the site. With shovel this took a couple of minutes, but pandoc is *big*, and so my new CI takes about 20 minutes in total. Normally this would be fine, but since I have to go and make a second edit after every blog post to include the meta for the mastodon comments section, it means that for the first 20 minutes the post is live there will be no comments. Which is a little bit annoying. This may be something I will find a better solution for in the future.

## Conclusions

Hakyll is fantastic and I'm so glad I switched to using it. I would highly recommend for anyone with a similar use-case to me: small websites where you care a lot about build flexibility. It only took a couple of hours to port my site, and the code is *tiny*! In fact, it's less than 50 lines. Here is the entire `site.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import System.FilePath
import Text.Pandoc.Options

emptyField :: String -> Context String
emptyField name = field name (const . pure $ "")

mdRoute :: Routes
mdRoute = customRoute $ (</> "index.html") . dropExtension . toFilePath

compileWith :: Compiler (Item String) -> Rules ()
compileWith compiler = compile $ do
    identifier <- getUnderlying
    layout <- getMetadataField identifier "layout"
    compiler >>= case layout of
        Just "none" -> pure
        Just path -> loadAndApplyTemplate (fromFilePath path) defaultContext
        Nothing -> loadAndApplyTemplate "_templates/default.html" defaultContext

main :: IO ()
main = hakyll $ do
    match "_templates/*" . compile $ templateCompiler

    create ["index.html"] $ do
        route idRoute
        compile . loadAndApplyTemplate "_templates/index.html" defaultContext $
            Item "index.html" ""

    match "blog/**.md" $ do
        route mdRoute
        compile $
            pandocCompilerWith
                defaultHakyllReaderOptions
                defaultHakyllWriterOptions{writerHTMLMathMethod = MathJax ""}
                >>= loadAndApplyTemplate
                    "_templates/default.html"
                    (defaultContext <> emptyField "blog")

    match "**.md" $ route mdRoute >> compileWith pandocCompiler
    match "**.html" $ route idRoute >> compileWith getResourceBody
    match "**" $ route idRoute >> compile copyFileCompiler
```

I'm looking forward to leveraging it to do more cool stuff, like add an Atom feed for this blog.

I hope you enjoyed reading. If you did, feel free to leave a comment. If you didn't, also leave a comment! Bye for now.

[^1]: Still using GitHub? [Give it up](https://sfconservancy.org/GiveUpGitHub/) today.
[^2]: Those of you who know me even slightly may find this fact hilariously incongruent.
[^3]: There are some nightly-only features which do make it a *little* better.
