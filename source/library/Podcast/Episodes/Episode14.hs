{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode14
  ( episode14
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

episode14 :: Either String Episode.Episode
episode14 =
  Episode.Episode
    <$> Article.fromString
          "https://danieljharvey.github.io/posts/2019-07-05-refined-types.html"
    <*> Date.fromGregorian 2019 7 16
    <*> Description.fromString
          "Andres Schmois and Cody Goodman talk about using the Refined \
          \library to turn runtime checks into types."
    <*> Seconds.fromTimestamp 15 18
    <*> Guid.fromString "5ec19b21-9399-474b-be54-beadd37894f9"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-07-16-episode-14.mp3"
    <*> Number.fromNatural 14
    <*> Right (Bytes.fromNatural 22040576)
    <*> Title.fromString "Refinement Types"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hello and welcome to the Haskell weekly podcast. I'm your host Cody Goodman. I'm a senior engineer at ITProTV. With me today is Andreas one of the engineers on my team. Thanks for joining me today, Andreas.
>> Yeah, thanks for having me, Cody. I'm excited to talk about some fun stuff with you.

>> Cool, this shows about Haskell, a purely function program language. Today we're gonna be talking about Refined which is a way of moving a lot of runtime errors into the types system. There is an interesting blog post recently in Haskell Weekly that explored using Refined to solve a few common cases of runtime errors, such as division by zero, or making sure a string is uppercase.

Things that you'd usually have to worry about runtime. Andreas, what were your thoughts on it?
>> The first thing that came to mind was it's very interesting to think of type checking at the runtime level. And one of the things that first stood out to me was what is the use?

High level seems very useful. After diving into it a little bit, seems like maybe some of these features should be implemented more directly into the compiler level, or even just as part of the actual Haskell language. But we can talk more about that in a bit.
>> Great.

Definitely want to talk more about that in a bit. Haskell is actually moving towards supporting dependent typing I hear in the next year, so we'll probably have something pretty useful and workable. In terms of Refined specifically though, and things that we can use today, what do you think are the trade-offs in deciding to use something like that?

Is there a lot of ceremony involved with using something like Refined? So runtime errors are expensive but is it more expensive to implement something with Refined?
>> I think the major point that came to me first is how much extra coding work do I need to actually do to be able to use this?

And I think that's probably the major downside to using something like Refined. We've gotten to a point where we are trying to fix, or not really fix but more solve a problem that isn't already solved by using type checking and runtime checking and all of these things. And the first thing that sticks out is lots of different conversion functions.

So there's a lot of extra boilerplate required around using Refined and using it in such a way that is helpful to the code. And that's my first takeaway, and there's some extra stuff in there as well that is very nice. It's just that my first question is, how much extra coding do I have to do?

And it's actually a little bit more than I'm comfortable with.
>> Okay. And like you mentioned, part of the reason for that is because it's an external library and not something integrated with a compiler, so it's exciting to see if that helps reduce that ceremony and boilerplate.
>> Yeah, definitely something that I think would be better benefited to be directly inside the language than as a library.

But it would be nice to know, or at least to be able to use it in practice, to be able to see if is it actually a lot of work? Or if it's just something that I get from the representation of it in the blog post and the documentation.

>> Right.
>> What did you think, as a first glance, what did you first think that this was gonna do?
>> I think that Refined is really interesting. I know there's a lot of ceremony with setting up things at first. I do wonder, though, if you go from the approach of wrecked by construction, and you eventually build up a lot of these refined helpers.

If it pretty much gets rid of the ceremony, once you've integrated into your normal workflow of programming and start thinking in terms of runtime errors. If you think in terms of using Refined, and does this value Refined, before I pass it to the next function? If you start thinking that way and just implementing those things as a lever you could reuse, I think it could be a lot more convenient.

It's harder to make that argument though without having a flushed out example.
>> Yeah, I agree. I think once this is actually in use, and we're able to see more of how it is used, then we can make that call. It's just one of my major downsides that I can see just off the bat.

If the extra work required to wrapping it and unwrapping it, better than just doing that type checking by hand. Or, is that actually worth it?
>> Right, and I think that when you look at some of the examples, by nature they're trying to teach something, so they're gonna be a little contrived.

You have to simplify things to teach the concept first, but it would be good if we had somewhere to go from there, layers on the different things that we've learned. Maybe having Refined check at compile time, that a string is uppercase is invaluable. But if you're verifying social security numbers, maybe verifying that they are again eight, nine digits, then that's something that could be useful.

>> Yeah, I agree. However, I do see verification is usually user input variables and they're not available at build time. So I can see that being a major issue, so let's say that you have a Refined type of positive number. What, how will the compiler know that a user is gonna input two minus one if you don't actually go through the entire refining it and unrefining it in the process?

Which, when you refine the two minus one it's gonna throw an error, or however you decide to actually implement it, and that is the same as if you just did an If statement for example. So I'm a bit hard to see the difference of actually type checking in code, and type checking in the types itself.

>> Right, that's a great point. I think the special sauce, so to speak, lies in that. It's really not about verifying that value. It is in the exhaustiveness checking that we get by defining that and how it forces programmers. If you are throwing an error on exhaustiveness checks, it forces the programmers to handle that case upfront.

Whereas you could have missed that case at a dynamic language.
>> Yeah, I agree. And that's obviously where we see the benefits of this library. The question I had was, is it worth it? Do we want to add all this extra complexity? Just to be more safe for the developer to write this code.

And the upsides I think are still not better than the downsides.
>> Okay. What do you think about the possibility of senior engineers using Refined to write out the types and stub out implementations? And then having the rest of the engineers implementing the functionality for those things?
>> If most of these types are gonna be used in a compilation manner, so we are mainly using Haskell in a in a web environment.

So most of our types that need type checking are gonna be user input, and I think that's where it falls short. If it is going to be used in such a way that this type checking is gonna help in the code writing, then I think that's a good thing.

So Refined is a great library to make sure that your types that are more dynamic than other types, such as numbers, lists, things like that. Stay inside the range they're supposed to be, then Refined is a great library for that. Regardless whether it's a senior engineer doing the structuring or it's a junior one, I don't see a difference in that scenario.

>> Okay. Did you see if it was possible in the docs, I'm scrolling through them now, to add Refined values to have composite Refined values?
>> You mean doing, for example, one plus one on a Refined value?
>> Sort of. I mean saying that to refine this value needs to both be uppercase and both be a length of ten.

>> I'm actually not sure exactly how it's used and that's the idea behind looking into this library was to see what my first thoughts are in just as a skimming sort of view. So, is it going to appeal enough for me to actually sit down and start using it?

I could go in there and start writing it and see if it feels natural. If it just feels very unnatural, or just completely different than what we're used to. And I can't actually answer the question that you're asking there.
>> Right.
>> If it were the case that you can do that then that's an extra plus.

>> Right, and actually that's a little bit of a warning sign to me, and something I see too commonly in Haskell libraries is if you can't communicate the really valuable pieces like that. So if you can't show how to move past the trivial examples to something more complicated but also more useful, that's a marketing problem as well.

>> Yeah, I agree. I had the same thought, I said I'm probably gonna have a different mindset, when I actually go in there and play around with it. But I thought it was more valuable to have that viewpoint where I actually haven't used the code. I'm just going by what the author has told me about it, and so far I'm not 100% agreed on it.

>> Mm-hm.
>> And I think having a little more examples that could help you understand what the library is meant to fix and how it fixes it would benefit the adoption of it.
>> Right, and I think it's really valuable for you to look at this from the mindset of somebody who's just thinking about looking at Haskell, thinking about adopting Haskell for some of these benefits they heard about.

Or maybe is a Haskell user who's thinking about putting more type safety into their code base. That's pretty similar to the mindset they're going to be in. And I think you found a valuable opportunity for anyone who's writing blog posts about Refined or any documentation, and that is to give more examples.

And this is true through Haskell documentation in general, but especially in here you wanna motivate not only the simple cases but the intermediately advanced.
>> Yes, and I actually have a comment on that one as well. The last part of the blog post where they show the examples of using Refined inside of a JSON type.

As a used case to use Refined to make sure that your JSON parsing matches want you want it to be, that seems like what Refined was meant to do, at least in my eyesight. As a server developer for Haskell, JSON parsing is pretty much a lot of what we do so we wanna make sure that the input that comes in is what we want it to be.

And I think Refined attempted to use liaison library to help type these JSON values a little better.
>> Right.
>> However, that seemed to me like it was an afterthought, it was okay. We also did this. They never went into what you can do with it or what the code can do what?

When you're writing it, if it can make the parsing less verbose or maybe more concise, things like that, is what I want to see when I'm reading a blog post. It is a fine line though, right?
>> Mm-hm, so what they left is a footnote about JSON there.

You would have maybe like to see them leave with that and then expound upon that example.
>> Yeah, I mean, everyone's thoughts on what Refined is good for is obviously different. I just wanted to get a little more information upfront about all the different ways you can use Refined, not just simple strings and then by the way, we also do JSON.

>> So, look at their example for a second. Looks like they find a type called the alcohol user, who has a name and an age. They have a Refined type to make sure that there's at least one character in the string. Where would you have usually verified something like that without Refined?

>> That is a good question. That would probably depend on how simple the parsing is. Most likely I would write it either directly inside the ASON instance, or as a conversion of the type. So if I had a type that was an alcohol username, I would have a string to alcohol username that would type check there and make sure you can only convert a string to that type if it is at least one character.

Otherwise you fail.
>> So the difference that would get us is kind of a more declarative way of listing out how to parse and validate that JSON.
>> Right, and if it would also remove a little bit of the boilerplate code required to do so.
>> Right, yeah for building a syntax for building the data and stuff like that.

>> Yeah.
>> Thanks for being on the show with me today, Andreas.
>> Yeah, thank you very much. It was fun doing this. And something that I actually forgot to mention at the beginning, I have just started doing Haskell a couple months ago, so most of this stuff is brand new to me.

And it helps a lot to read about these libraries that people want to implement, just to understand what people's thoughts are in terms of Haskell's strict type system and where it needs to be improved in.
>> Thanks for that very valuable input. And thank you for listening to the Haskell Weekly podcast.

If you liked what you heard, find out more at our website HaskellWeekly.news. Also, please rate and review us on iTunes. It helps a lot. Thanks again for listening. We'll see you again next week.
|]
