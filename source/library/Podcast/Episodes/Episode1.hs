{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode1
  ( episode1
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

episode1 :: Either String Episode.Episode
episode1 =
  Episode.Episode
    <$> Article.fromString "https://markkarpov.com/tutorial/exceptions.html"
    <*> Date.fromGregorian 2019 3 11
    <*> Description.fromString
          "Cody Goodman and Taylor Fausak talk about handling errors in \
          \Haskell by using exceptions."
    <*> Seconds.fromTimestamp 9 43
    <*> Guid.fromString "6fe12dba-e0c3-4af5-b9fc-844bc2396ae7"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-03-11-episode-1.mp3"
    <*> Number.fromNatural 1
    <*> Right (Bytes.fromNatural 13999481)
    <*> Title.fromString "Handling exceptions"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hello, and welcome to the Haskell weekly podcast. I'm your host Taylor Fausak. I'm the lead developer at ITProTV. And with me in the studio today is Cody Goodman. Thanks for being with us, Cody.
>> Thanks, Taylor, I hear we're gonna talk about some exceptions.
>> We are, before we get to that, just a quick overview.

Haskell is a safe, purely functional programming language with a fast concurrent runtime. And as Cody mentioned, we're gonna be talking about exceptions in Haskell today. In particular, we're looking at this article posted by Mark Karpov called exceptions tutorial. It's from the yet to be published intermediate Haskell book.

Really we're just going to jump right in. So Cody, why do we even bother with exceptions? Why not have pure computations everywhere?
>> Well, we're trying to model the real world. The real world isn't perfect, so we have to have some way of capturing that. Exceptions are an imperfect but time-tested, proven way of doing that.

We know them from other languages that we've worked with, other imperative languages. But we get to Haskell, we start learning it. We see these different paradigms, these different ways of modeling the world around us. Why can't we just stuff exceptions into that model, and get all the benefits as well?

And it turns out, after you try that, you'll see that coming up with the exception hierarchies, making models that make sense is really difficult. Whenever you want to add another exception, for instance, you have to type that out, account for it somewhere else. It becomes quite becomes quite laborious.


>> Yeah, and let's back up a minute and make this concrete. You mentioned that the real world, unfortunately, has imperfections, unlike our pure, crystalline world of Haskell. What are some of those things that we could talk about, just to have a concrete example to play with?
>> If you just take integer division, for instance, something that could go wrong is a user passing a zero and a divide by zero happening.


>> Yeah and then the whole app blows up, which may be what we want, maybe not. Let's say that somebody divides by zero, one way that we could model that is with an exception. I think a lot of languages, like Python, do that. Haskell may even do that.

But what are some other options we have for how to model that other than an exception.
>> One other option, if you've been learning Haskell or using it much, is the maybe or either monad. Now in this case, the either monad wouldn't make a lot of sense. Because there's really only that one case that could go wrong with integer division.

Maybe I'll listen to this later and see I was wrong here. But-
>> Yeah, there might be lots of ways that an integer division could go wrong.
>> I'm not a mathematician, I couldn't tell you.
>> Indeed. So using a maybe, we could model that division by zero as a nothing.

Or if we got the result and it wasn't a zero, just that value.
>> And since we only have one kind of exception case to deal with here, it's pretty easy to decide. Yeah, we should probably use maybe instead of either. But if we had a lot of cases, wouldn't we wanna use either rather than maybe?


>> Yeah, typically you'd wanna use either. When you when you first learn about that, say from real world Haskell or some more updated materials, you'll use either string some value. And you will use that to essentially plan out what that string value is. But that's remarkably similar to just using exceptions.


>> Yeah, the only difference really is that we've captured it in the type and we can work with it as a value. Either string something isn't super useful cuz the only thing you can do with that string is maybe print it out. Or maybe poke out it if you're really wanna try to determine, does it have zero in there somewhere?


>> Right.
>> Which isn't very satisfying. But let's say, we had integer division by zero, which returns a maybe. We have some more complicated thing that returns a neither. Isn't it kinda difficult to smoosh those values together and work with both of them in one context?
>> How do you mean exactly?


>> So if we have integer division that returns a maybe integer. And so, we have to deal with just some value or nothing. And then we have some other computation, I can't think of one off the top my head. But it returns in either, let's say, string integer.

And we wanna, if the maybe is a just integer, and the either is a right integer, add those things together. Don't we have to do a lot of pattern matching or something?
>> Yeah, we have to do pattern matching. There's no good way to use any F mapping or short-circuiting between maybes and eithers, cuz they have a different number of parameters.


>> Yeah, and they're just different types. Whereas, if everything was a maybe or everything was an either, even with the same error type, then we could work with those values really easily.
>> Right.
>> And right, I guess a problem there would be that that error type, if we were using either, would start to get really big, right?


>> Yeah, so let's say you replace that string with your own my custom error type. You make it a sum type. You enumerate out all the things that could go wrong. Sure, with simple problems that would not be too bad, and it may even be the preferred option in simple cases.

But if you have something relatively complex or something that gets very complex on you, even though you thought it would be simple. It becomes really laborious and the value proposition isn't so much in favor of those huge hierarchies.
>> Yeah, and you look at the exception hierarchy even for a language like Haskell, but first stuff like Rubi or Python, there's a lot of different types of errors in there.

And stuffing all those into one error type, I don't wanna deal with all those exception cases all the time, cuz most of the time they're not gonna happen.
>> Right, if 90% of your case matches that you're doing control flow off of do the exact same thing, and they print out the error.

What value are you getting specifically from not using exceptions and instead using either in a custom error hierarchy?
>> Yeah, and furthermore, if I had a, let's say I had this integer division function. And it returned either some big error type integer. If I wanted to handle the result of that, really, I know, based on, let's say the documentation or just my gut, that the only exception it's actually going to return is the division by zero one.

But I also have to handle file does exist error and control c and all this other stuff, even though I know in my heart of hearts that it's never gonna happen.
>> Right and that's something that just doesn't make sense. And it's something that your definitely not gonna have time for.

I think that might be a reason that, for instance, programmers who don't know as much about Haskell or the benefits if it, will see it as not a real-world language. They don't wanna be caught up in all of those concerns. But the good news is, they don't have to be.


>> Yeah exactly, it is a real-word language. And it can be used the same way as every other language, because you can just throw an exception. And in a way, during an exception is a lot like you're in this either some giant error case. Except that you don't have to explicitly handle all these cases that you know aren't gonna happen.

You can only catch or deal with the exceptions that your documentation says that thing is gonna throw.
>> Right, and that's okay. I think we should encourage that. Because it's also an effective way of learning Haskell is only handling the cases that you think you're gonna have to care about.

And then update and encode incrementally as you go along. There's, at first, a feeling of, I have to learn everything up front.
>> Yeah, you don't need to learn everything all at once. You can learn stuff as you go. We've been talking about integer division. Obviously, that's a pure operation.

It doesn't need to read from a file or talk to the network or anything like that. But we're also talking about throwing exceptions. Isn't it weird that exceptions can be thrown from pure code?
>> It does seem weird, why does this happen? Why can't we just depend on those pure operations?

And the reasoning for that is because the complications of the real world again. Maybe our computer runs out of memory, for instance, while we're trying two add numbers.
>> Yeah, and what do we do there?
>> Exactly.
>> So exceptions have to be able to be thrown from pure code.

Because the real world is gonna butt in and say, well, I know that's a pure mathematical function you're dealing with, but sorry, I ran out of power.
>> Yeah, maybe we got too close to the Sun.
>> There was a flare or something.
>> Yeah, something big got flipped on our hard drive.

What are we gonna do? But if we can throw exceptions from pure code, wouldn't it make sense that we could catch them from pure code, too?
>> While it would, and that's where you get into sort of one of the gotchas in Haskell, at least at first, only IO is in order.

Pure code doesn't necessarily have a guarantee of the order, for instance, addition.
>> Yeah so, what do you mean by addition? If I'm adding two and three, it seems pretty clear what's gonna happen.
>> Right, but if we have two errors that are being added together. We have error string of line and error string of two.

We don't necessarily know which one of those will be evaluated first.
>> Right, so it could be we'd run that and we'd get one printed out to our console as the error. Or we'd get two, and there's just no way to know ahead of time which one it's gonna be.


>> Exactly.
>> So since that's the case, we can only catch errors in IO, is that right?
>> Correct.
>> That means that we can throw errors anywhere we want, and then at the top level of our program, which has to be IO, right?
>> Mm-hm.
>> That's where we handle everything and do the typical IO stuff of just log it out and crash the program.


>> Correct, correct.
>> Thanks for being on the show, Cody.
>> It's been great to be here. I look forward to talking to you soon.
>> To learn more information about Haskell, go to our website, haskellweekly.news. From all of us here at Haskell Weekly and ITProTV, I'm your host Taylor Fausak.

We'll see you next week.
|]
