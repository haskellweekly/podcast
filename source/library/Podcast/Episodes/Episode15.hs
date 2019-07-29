{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode15
  ( episode15
  )
where

import qualified Podcast.Quasi as Quasi
import qualified Podcast.Type.Article as Article
import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Date as Date
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Guid as Guid
import qualified Podcast.Type.Media as Media
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Title as Title
import qualified Podcast.Type.Transcript as Transcript

episode15 :: Either String Episode.Episode
episode15 =
  Episode.Episode
    <$> Article.fromString "https://treszkai.github.io/2019/07/13/haskell-eval"
    <*> Date.fromGregorian 2019 7 23
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about how function calls are \
          \evaluated in Haskell with regards to non-strictness."
    <*> Seconds.fromTimestamp 18 13
    <*> Guid.fromString "a76ba20a-49f7-4a5f-a40d-bffb34417b2d"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-07-23-episode-15.mp3"
    <*> Number.fromNatural 15
    <*> Right (Bytes.fromNatural 26208359)
    <*> Title.fromString "Lazy Sharing"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hello, and welcome to the Haskell Weekly Podcast. As you might have guessed, this show is about Haskell, which is a purely functional programming language. I'm your host Taylor Fausak. I'm the lead engineer at ITPro.TV.
>> Hey, Taylor. I'm Cameron, I'm also an engineer here, but I'm not the lead engineer.

I'm just excited to be here today. I'm glad we get to do a show again, it's been a little while.
>> Yeah, it has been a little while.
>> How are you doing, what are we talking about today?
>> I'm doing well, and I'm really looking forward to talking about this post.

It's titled Evaluation of Function Calls in Haskell, which just sounds riveting. But very exciting for me, and it's by Laszlo Tretski. Hopefully, I'm pronouncing that name right, I'm almost sure I'm not. And we're pulling it from issue 168 of Haskell Weekly. And I'm still flabbergasted that we're up into the hundreds for issue numbers, it's crazy.

>> That's a lot of articles, which is really cool, that we'll able to kinda group all this stuff together.
>> Yeah.
>> But I'm really excited about that article, it sounds really fascinating. Reading over it, it was a little heavy cuz it talks a lot, it's a lot nitty-gritty.

And so, I think we should really start high level kind of-
>> It's a good place to start.
>> Good overview, right? I could go really high level and be like what is a function?
>> I think we can assume everybody knows.
>> Yeah, that's fair. But in this article, he talks a lot about, kinda the gist of it is coming from a chapter in Haskell Programming form First Principles by Christopher Allen and Julie Moronuki.

>> Yeah.
>> I nailed that.
>> I was curious and this is something they're kind of talking about preventing sharing on purpose. So it's sharing.
>> Yeah, sharing.
>> Sharing.
>> Yeah, that's kind of a weird concept cuz it's not something we think about usually. So much so that it's a term a lot of people probably haven't heard of.

I'm sure many people are familiar with the fact that Haskell is a lazy language, or non-strict. And what that means is that, when you write a function call, it doesn't evaluate that immediately. It can wait until you actually need that thing before it evaluates it. And what sharing means is that if you have the same thing used in two places, maybe it will only actually be computed once, and then it'll use the result in both of those places.

Whereas in a strict language, you would be forced to compute at both times because you're using it twice. So that's a real quick introduction of what sharing is, hopefully I didn't miss anything too bad-
>> And sharing is always caring, so that's good.
>> Exactly.
>> Well, I think we use sharing well.

But this article presents a way to not share, to create a function that isn't shareable.
>> Right, yeah, they have kinda two motivating examples, right? They have this this lambda, this function that takes an argument and completely ignores it and then returns something. And they use that as an example of something that doesn't share.

It keeps all of its toys for itself. And then, they have another example that functionally is exactly the same, the end result is the same except that it allows sharing. And it does that by using the const helper function that's in the prelude to not have a lambda there.

And that allows GHC to analyze this a little more thoroughly and do the sharing optimization.
>> Yeah, which is really cool. Which the way they use the const operator, or the function, it's not an operator. Using the const function, it makes the function point free, right?
>> Yeah.

>> As opposed to point full.
>> Right.
>> So could you kinda talk about point free verse point full a little bit?
>> Sure, point free programming is definitely something that is, when people look at Haskell, they think about that a lot. All these really dense expressions with a lot of function composition.

But really, what it boils down to is that with point free programming, you don't talk about your argument names. You don't actually list them out. So what we're used to in most programming languages is that when you write a function, you have to explicitly list all of the arguments that you take in.

And in Haskell, you can do that. And when you write a lambda or a top-level function declaration, that's the common way to do things. But if your function is just a series of other functions composed together, and it takes its argument, it can be a little annoying to say f of x equals this function, open parentheses, this other function, open parentheses, this other function, x.

And so, point free lets you kinda rewrite that without mentioning the x at all, and just say f equals f1 composed with f2 composed with f3.
>> Right.
>> Which is really nice for kind of understandability and looking at it and seeing that it's just a pipeline of functions.

>> Right.
>> But specifically, with regards to this blog post, point free is interesting because it changes the semantics of how it actually runs. And we've talked about sharing already, so it should come as no surprise that it influences sharing.
>> Right, and yeah, it's behind the scenes that things are different, right?

Cuz if you look at the lambda verse the const compositionally, it doesn't look that different in Haskell. But they kind of touch on something called core, right?
>> Yeah, and core is something that hopefully, or maybe not hopefully, that's the wrong way to put it. Generally speaking, day-to-day, if you're writing Haskell code, you're not gonna have reason to go look at the core that it produces.

But you can think of core as the first step in the compilation pipeline. And core could be, it isn't this, but essentially, you can think of it as a very minified version of Haskell that doesn't have any syntactic sugar. And because of that, it ends up having a lot of lambdas and a lot of case statements.

>> So it's not as sweet as Haskell, is that what you're telling me?
>> Yeah, it's a little sour maybe, or bitter, I'm not sure which flavor it would be.
>> Nonetheless, it's still part of Haskell and the compilation process.
>> Right, and the reason that core kinda comes into this discussion is that when we talk about sharing, it's not apparent when you look at Haskell source code if something will be shared or not.

There are some kind of heuristics that this blog post talks about, especially at the end, that tell you when you can expect something to be shared or not. But the only way to be sure is to look at the core. And you can tell GHC to output of the core so that you can look at it.

And as I said, it doesn't have any syntactic sugar, so it ends up being really verbose a lot of the time. Which means it's not something you typically want to be looking at all the time.
|]
