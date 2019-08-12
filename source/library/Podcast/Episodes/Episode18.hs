{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode18
  ( episode18
  )
where

import qualified Podcast.Quasi as Quasi
import qualified Podcast.Type.Articles as Articles
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

episode18 :: Either String Episode.Episode
episode18 =
  Episode.Episode
    <$> Articles.fromStrings
          ["https://blog.kabir.sh/posts/inventing-monads.html"]
    <*> Date.fromGregorian 2019 8 13
    <*> Description.fromString
          "Cody Goodman and Taylor Fausak walk through inventing monads from \
          \scratch in JavaScript."
    <*> Seconds.fromTimestamp 16 41
    <*> Guid.fromString "153162fd-b6f5-40f7-8b05-fe20b91b702b"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-08-13-episode-18.mp3"
    <*> Number.fromNatural 18
    <*> Right (Bytes.fromNatural 24062492)
    <*> Title.fromString "Inventing Monads"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hello and welcome to the Haskell weekly podcast. As you might have guessed this show is about Haskell, which is a purely functional programming language. I'm your host Tyler Fossick. I'm the lead engineer at ITPro.TV.
>> I'm Cody Goodman. I'm a senior engineer at ITProTV.
>> Thanks for joining me today, Cody.

What are we going to be talking about?
>> We are going to be following a blog post called inventing monad and talking through it cool.
>> I feel like I've been programming Haskell for a while. So I'm familiar with monads, but this is more of a beginner's perspective, right?

>> Right, it tries to actually go through and start using or uses JavaScript throughout all of those examples to make it a little more approachable since that's kind of the lingua Franca.
>> Yeah, for sure. JavaScript use just a little bit more than Haskell in industry, and it's nice to see that the concept of my monads can be applied in other languages.

It's not something that is specific to Haskell.
>> Right.
>> So you mentioned that he does this in JavaScript. So how does he start off, because I know that Haskell and JavaScript represent things a lot differently. So it's kind of weird to see that as a starting point.

>> Right, he just starts with a normal kind of JavaScript block of a function of how you would get a middle name. You have a function and two constants in it. A function call and two constants in it. Then he does this sort of pointless looking transformation where he introduces an apply function and he uses that apply function to get the middle name, and then he calls the get user function on that, and then the apply above that as well.

And finally, calling get ID with all of those things and the subsequent plies on it.
>> Got you, so he has this kind of straightforward JavaScript function and he transforms it. Throwing a seemingly unnecessary amount of abstraction in there, but I have to assume it'll come in handy later.

>> Right, he calls it a dense representation. It totally looks pointless and it's like God, why would you do this? Yeah, well, hopefully he'll tell us. And it's interesting here that JavaScript and Haskell again are such different languages, but he manages to make a function called syntax that ends up looking pretty close to Haskell.

So if you kind of squint your eyes a little bit, you could imagine this JavaScript is actually Haskell which is nice for me.
>> Yeah, great.
>> So now that we've thrown all this extra syntax and boilerplate in front of ourselves, what do we actually get out of it?

What's the first thing that he does with this stuff? Well, the first thing is he poses the question. What if I ID, middle name, user if any of those things could be null?
>> Which is super common, right?
>> Right, right, anything fails, you'll get a null, especially in JavaScript.

He gives a code snippet where the get ID and get user functions are now checking for null, same for get middle name.
>> Right, so I showing you kind of the more typical JavaScript way of approaching this problem, right?
>> Correct and showing how all of them have to handle the null values and the possibility of returning it.


>> Right, every single place.
>> And then he pivots and says, what if we check for it automatically?
>> Right and how do we how can we do that automatically? Does it use that apply function we had earlier.
>> Right, so the secret to all of this is in that you change how these functions are interpreted instead of changing those functions themselves.


>> That's pretty cool. So earlier, you said that apply, it takes an argument in a function and then it applies that function to that argument exactly like you might expect, but what does it do now that null has entered the picture?
>> That's actually where he gets to updating the applied function where if x is equal to null, so given apply xf.

If x is equal to null, you just return null. Otherwise, you apply f2x.
>> That's pretty cool. I like to think of that as short circuiting where once you encounter a null, everything stops happening and you just get null from that point out. And I see that happening a lot in JavaScript functions where you say, right at the top of some handler, you say, if some condition is met, then return null or error or whatever it is and then carry on.

So this is pretty similar to that in my mind.
>> Yeah, there's just a lot of times we're within a function, you'd have to do a lot of nested if checks or something where you could have used short circuiting.
>> Yeah, it cleans things up a lot, I think.

So now that he's got this new apply implementation that supports null checking, what does he do with it?
>> He uses it to show how he can get the middle name function exactly the same way as he did in the previous iterations.
>> And that's pretty cool, right, that the same code can behave differently, depending on if you need to handle nulls or not.


>> Right.
>> It's starting to look a little bit like something, but I don't know. We don't wanna give it a name yet. I don't wanna scare anybody off.
>> So after he shows this example of dealing with null, he kind of pivots and walks back to the original example and then adds another, I'll call it an effect, but another thing.

So instead of checking for null, he does something else which is kind of logging things as they happen, right? So that if you were needing to debug this function, cuz you unexpectedly got a middle name of popcorn and that doesn't make sense and you wanna figure out how you got there.

So these this logging would help you figure that out, right?
>> Right, not being able to log and functions like this if we tell people coming to Haskell like yeah, you can't log. That's not going to work right kind of eased.
>> You you need to be practical, you got to debug stuff.

And so how do they approach logging, given that they're trying to implement these things as quote, unquote, pure functions?
>> So the first thing you do is in functional languages avoid global variables to keep track of all those messages. So the first pass that, that is modifying utility functions to handle arrays.


>> And when you say arrays, what do you mean? What's in those arrays?
>> The result, as well as the log message.
>> Right, so you get back an array where the first thing in it is the answer and everything else is kind of stuff that happened along the way that you might be interested in and you might throw away.


>> Right, the the trouble is that then your functions have to know how to handle that.
>> Yeah, every single function you call would need to like d structure or lookup or whatever. But fortunately, we have functions that we've thrown in our own way here, right, that we can change yet again to do what we need here.


>> Right, that pointless seeming function from it first apply which made things look all horrible.
>> Now it's like we can just go back and modify this now to distructure that and show us what the logs, the results of.
>> Yeah, previously, we were using the supply function to check for null.

But now we're saying, grab that first thing out of it and append all can cat together all the other stuff. So we're accumulating these log messages as we go through, but still producing the result that we wanted in the first place.
>> Yeah and that's that's really cool to be able to just modify that apply again to handle all our needs.


>> Exactly, pretty nice. Again, you end up with the same shape at the top level. It still looks exactly the same. But underneath the covers, the implement, excuse me, the implementation details are kind of switched out from underneath you. So instead of handling nulls, now we're doing logging.


>> And that's why this blog post does a really good job of motivating why word we're not saying is useful.
>> Maybe at this point we can start talking about it is in the title after. So-
>> Yeah. So far, these have been two examples of monads and this is showing you that yeah, they are useful for real life situations that you're gonna come across as you're writing programs.

It's not just advanced math Ivory Tower stuff.
>> For sure, there's a lot of it a lot of real world advantages that you get from monads and he's worse good examples already.
>> And they're going to give a few more, so let's kind of get through those. They talk about global environment which when I read that makes me shudder a little bit, because I don't like global environments.

I like things to be as local as possible, but I think they don't really mean global. They mean something else, don't they? It's kind of like scoped to the function or something like that.
>> Right, they mean for a given function, you can provide an environment that can be modified within that scope.


>> Right, so the way that I like to think about this and I think this actually came up in our code base recently here at work where we had a series of functions that all needed some config and we were passing that config from function A to function B, B2 C, C to D.

And that's a lot of boilerplate, cuz you have to thread that thing all the way through and I think we came out of that not having to thread it through. Can you tell us how we got there?
>> Right, so we started using a monad transformer which the important thing is that it had a reader monad in it.


>> And that reader monad is what?
>> That just means that you can simply use a function called ask and whatever you initiated that reader on Ed with whatever value it was initiated with, you'll get that back. In our case, it was convicted. Got you, so we had all these functions that were operating in this monad.

And instead of passing an argument through to everything in the single place where we needed it, all we had to do was ask for it and it was suddenly available.
>> Right and actually at one point, we had both the new monad and we were still passing in fig.


>> It headed twice just to make sure get it from one place, get it from the other and make sure they're the same and then move on.
>> Right.
>> So in this blog posts, he shows this global environment example and kind of walked you through the reader monad is what it would be in Haskell and provides motivation for this example that we just ran into.

Passing all these arguments is annoying. And if you can put it in your context and then ask for it when you need it, it's pretty nice. And once again, as you might be able to guess, you change apply and your actual business logic ends up looking the same and all the stuff underneath the covers kind of changes out from underneath which sounds scary, but promise isn't as long as you follow a few rules exactly.

They're important rules as we call them laws, but I don't think you get arrested if you don't follow them. It's just very, very frowned upon. So now that we've gone through dealing with nulls accumulating a bunch of state in this case logs as we go through stuff and having a kind of read-only environment that's available to our functions.

We land on the final example which is state which is something that gets passed to every function, but also can be updated along the way. Can you tell us a little more about this example and kind of how it differs from the previous ones?
>> It's really similar like you're saying, I think to the reader mpnad.

The only difference is that that initial state that was passed in. You can update it and you can you can get it again do whatever you want with it.
>> And I think the example they use as kind of innovation has to deal with random numbers. Because if you wanna generate a bunch of random numbers in a pure fashion without reading from like slash dev random or something like that.

Typically, you're gonna need a random number seed and those are deterministic. So if you give something the same seed, you get back the same answer which works fine if you only need one random number. You could throw that in your reader context and pull out the seed, and generate a random number.

But if you wanna generate to, you would generate the same number both times, because you couldn't change that reader context. So you need something like the state where you can ask for to get it and then do whatever operation you want with, and then put something back into the state change it.

I actually remember I remember having a bug with random numbers before where I might have been using a reader monads just threading that same seed through. And after I figured it out, it just reminded me of the xkcd comic, it's like turn 42.
>> Yeah, guaranteed random. We rolled some dice and figured out this is a very random number.

We're just gonna keep giving it to you. Yeah, so we don't want to implement random number generators like that and you don't have to. The monad abstraction is powerful enough to give you this ability to consistently take something in into your environment and use it, and modify it, and then return that back out along with your answer.

So once again, we end up with changing the apply function and using the same business logic over and over and over again. So that gets us through all of the motivating examples that this guy gave for monads and he managed to make it through this post without using monads we didn't quite think we made it halfway through.


>> But this is something that happens a lot in the Haskell community, right, beginners kind of finally grokking monads. Have you seen this come up before?
>> Yeah a ton. I personally started writing Haskell just by making functions of the five ten fifteen arguments. Yeah, to pass the state through and then I finally got annoyed and I was like I'm going to learn what monads are about and I came across a billion tutorials.


>> Monads are burritos. What else are they?
>> They're all kinds of things. I keep thinking the straws, but I think that's lenses are straws. Monads are burritos. Their boxes, their containers. I don't know, any number of things.
>> That's why I really like tutorials like this which focus on some real examples, because you're not gonna get very far with metaphors as much as you will with using these things and seeing why those laws that are seemingly pointless.

Why there is applied functions through seemingly pointless help you.
>> Yeah, I agree. I much prefer seeing a motivating example rather than here's the type signature and here's the laws and it's important and good luck.
>> Walking through stuff like this really helps me or would have helped me when I was learning Haskell see why do I care about this?

What do I get out of it?
>> Exactly.
>> And like you, I've seen so many of these tutorials through the years that it's feels a little bit like a rite of passage for new Haskellers. Like yeah, you got to learn what mountains are and then you got to write blog posts that says you what they are.


>> And that's exactly what's happened. Yeah, somehow I think neither you, nor I have written this blog post, but maybe that just means we haven't truly understood monads yet.
>> Well, I hope we do. We're making a podcast about them.
>> Yeah, it does this count as a blog post probably.

Anyway, I think that about wraps it up for us today. Thanks for being with me on the show today, Cody.
>> Thanks for having me, Tyler. It was a lot of fun.
>> It was a lot of fun. It's always good and good having you on here, and thank you for listening to the Haskell weekly podcast.

If you like what you heard, please find out more at our website, haskellweekly.news. And if you did like our show, again, please go rate and review us on iTunes. It helps a lot. Haskell weekly is brought to you every week by ITPro.TV, the tech skills development platform for IT professionals who also happens to be our employer.

So please send your sysadmins and network admins to www.itpro.tv for other learning needs and they can let you know what they think of the people who develop it which is us. So thanks again, Cody and we'll see you all next week.
|]
