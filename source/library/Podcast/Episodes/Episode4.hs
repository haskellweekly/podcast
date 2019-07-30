{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode4
  ( episode4
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

episode4 :: Either String Episode.Episode
episode4 =
  Episode.Episode
    <$> Articles.fromStrings
          [ "https://runtimeverification.com/blog/code-smell-boolean-blindness/"
          ]
    <*> Date.fromGregorian 2019 4 1
    <*> Description.fromString
          "Dustin Segers and Taylor Fausak talk about avoiding boolean \
          \blindness by using custom types."
    <*> Seconds.fromTimestamp 15 57
    <*> Guid.fromString "aea8101c-b126-4cb5-be14-00200d3f6c27"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-01-episode-4.mp3"
    <*> Number.fromNatural 4
    <*> Right (Bytes.fromNatural 23002958)
    <*> Title.fromString "Boolean Blindness"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hi, welcome to the Haskell weekly podcast. This show is about Haskell, a purely functional programming language. I'm Dustin Segers, a software engineer here at ITProTV. And I'm with the lead engineer here, and he is the host for today.
>> Hey, Dustin. It's good to be here with you in studio.

>> It's good to be here with you too, man.
>> Thanks for joining me. My name's Taylor, as Dustin mentioned, I am the lead developer at ITPro, I also publish the Haskell weekly podcast. And today we're gonna be talking about a story that was in one of the older issues about Boolean Blindness.

>> Mm-hm.
>> Dustin, could you explain to me what Boolean Blindness is?
>> So Boolean Blindness is basically, where you are using a bool in place of maybe, something that could be expressed a little bit. More or less generically?
>> So, with a bool we have, it's either true or it's false.

>> Yes.
>> And that doesn't encode any interesting information there. Lots of things could be represented as bools.
>> Correct.
>> A light switch could be a bool, but Boolean blindness is saying that, maybe we're better off encoding that as Light Switch, is a special data type that could either be on or off instead of true or false.

Is that what you're saying?
>> Yes, pretty much, yeah.
>> So, what are the benefits of modeling one of these things as a data type rather than reusing bools.
>> It's more expressive. So, let's use your light switch for example. On or off is better than just saying true or false.

>> Right, cuz what does true mean?
>> Exactly. So we could have a data type that would be light switch, and we could pass it in to a function. And, it could be either on or off versus true or false, which gives a bit more meaning to what you have.

>> Exactly. That's really nice when you're calling the function because instead of saying some function true, It'll say, some function light switch on or light switch off.
>> Correct.
>> That dream of self-documenting code.
>> Yes, yes.
>> And addition to the code being self-documenting when you call it, the function, the documentation that is generated for that function or, if you're just reading through the code, that's gonna be much nicer as well.

Because it's not gonna say, some function takes five Booleans and returns something. It'll take a light switch state and a door state, and all these different things that it needs. One function that we use very frequently in our code bases here is filter. And filter says, give me some predicate, some function that returns true or false.

And give me a list and I'll give you back a smaller list that fulfills that predicate. But for me, every time I use filter, I have to think, is this function the thing that keeps things in or the thing that keeps things out? And I can never remember, and the type signature doesn't really help me.

>> Right.
>> Could you tell me how Boolean blindness might help there, or not suffering from Boolean blindness might help?
>> You could say, so how much a filter this list of users based on whether or not they are part of a team, and your data type if you will, could be on team or off team, instead of just a regular old Boolean.

And you could use your filter to say, I want to filter only these users who are off the team, and then in that case, your filter will only take the users who are not on the team and you can do whatever you want to there.
>> Right, so that's a lot clearer than saying, where team membership is true or team membership is false, it's self-documenting just on the name of the value.

>> Correct.
>> But that filter function itself is still a little confusing because, we have to get the concept we're interested in, which is are they on the team or aren't they into a Boolean? So we have to say, some equality check their, wouldn't it be nice if we could have a filter where instead of the filter function itself taking a Boolean, it would take some other thing, like keep this thing, or discard this thing.

>> Right, yes.
>> Instead of converting from this user is on the team, or this user is off the team into a Boolean, we would convert it into a, they should be kept as part of this filter operation, or they should be removed. Doesn't that clean things up a little bit?

>> Yes.
>> Would you prefer to use a function like that, or would you prefer to keep using the original filter that just has bools all over the place?
>> I'd probably prefer, yeah, the one that does not use bools all over the place.
>> Why is that?

What makes it better?
>> Well, it's more intuitive, it's more expressive, so it's easier to understand. The ax murderer who comes in later, six months down the road, and it's like, who wrote this code? Won't come to find me.
>> It happens all the time.
>> Yes, exactly.

He'll be a bit more docile I would think, so yeah.
>> He might put his ax away and take out something else instead like a feather duster.
>> Yeah, or a burger. Yeah.
>> Burger.
>> As a gift for the person, the earth is a wonderful code.
>> Yes,
>> That's one way we might clean up this filter function and avoid Boolean blindness.

There are other ways too and this blog post mentions a few of them. One example they give is changing the name of the function. Because, for me like I said the problem I have with filter is that, I can never remember does it filter things in or does it filter them out?

I don't know which way it goes?
>> Mm-hm.
>> So maybe a better name would be better? Is there some other name we could use for filter that might be good?
>> Filtering in or filter out would be nice.
>> It's a step in the right direction for sure.

Let's say that we did change the name of the filter to. Filter in but it still took a Boolean.
>> Okay.
>> Is there any, like what's the upshot of doing it that way versus the way we were talking about before about using a different type.
>> So, filter in that would take me maybe a keep.

>> Mm-hm, okay.
>> The upshot there is when you're not confused about what's going on, and the self documenting code dealio. And yeah, it's just more expressive again, and it seems like it would be better to upkeep, well to help with upkeep in the future as well, so.

>> Yeah, cuz imagine that, bringing it back to our light switch example.
>> Mm-hm.
>> We currently have a very simple light switch that is either on or off. But what if we upgrade our lights and suddenly we have dimmers. And we want to filter out for all the lights that are on.

Well, how do we decide if a light is on or not, to keep it in this filter or to rule it out? Maybe we wanna say well, at 50% brightness, or once the dimmer switch hits halfway up, then it counts as on. And in coding that I think, is a lot clearer, because we would have a function that takes a light switch state and gives us back a, do you keep it or do you rule it out?

Versus something that just returns a Boolean, right? And then you're thinking, well what is this Boolean used for later on?
>> Right.
>> Yeah, and it can be really challenging as you said, six months down the line, when that ax wielding psychopath looks at your code. They're thinking, well I can sort of understand the dis function returns true, when the light switch if more than 50% on, but what does that mean?

>> Right.
>> Yes.
>> So we've talked about two methods for changing filter to make it more intention-revealing. Either, changing the type of the function that it takes to return some better thing, like keep or discard instead of a Boolean. We've also talked about renaming it. So instead of just filter, you have filter in or filter out.

>> Mm-hm.
>> The third option presented in this blog post is to make the type, return the thing that you wanna end up with. So instead of saying, I'm only selecting things to keep or discard, instead you're kind of mapping as you're going along and producing new values.

But you're wrapping those values in maybe, and you're only keeping the ones that are just some value and discarding all the ones that are nothings. That feels to me like it's very clear about which ones to keep. Because obviously, if you have a nothing, there isn't anything to keep there and you can go ahead and throw that one out.

>> Right.
>> And when there's a jest, you can hang on to that thing. But, it's unfortunate I think, that the two operations are getting balled together, where you have mapping and like changing the values in this collection, at the same time as mapping over them and filtering some of them out?

>> Yeah.
>> What do you think about that? Is that a good trade off or is that weird?
>> So, my gut feeling says, I don't like it, and mainly because, it's been drilled into my head that mutation is evil.
>> So it-
>> It looks a little like mutation.

>> Yeah, yeah, it does. Even if it's not explicitly mutation, it does feel like we're changing, Some essential piece of this. It seems like side effects are happening here. So, I know we need those, but just initially, I don't like that much?
>> Don't like it?
>> Yeah.

>> Yeah, I don't like it either, and for the reason you mentioned, but also when I look at the type like so. If we had this weird filter function that returned a maybe instead of returning either yes or no, or keep it or don't keep it, or Boolean, whatever.

It's not clear from the type signature if the thing you're interested in doing is changing the shape of these values in their collection. Or removing things from the collection.
>> Right.
>> And when we have the two separate functions, map and filter, that does each of those things very specifically.

It can be clear to me as a reader that this function, it's gonna keep all the elements in there. They may change a little bit, and hopefully they do otherwise we're not doing anything interesting.
>> Right.
>> Or, when if I see a filter I can think okay, this isn't gonna change any of them.

It's only gonna remove some of them.
>> And I like having that assurance because, when I'm trying to step through code and think of all the states it could be in, it's very useful for me to say. Well, I know this operation is gonna keep all the elements that I'm dealing with or this one is not gonna change them.

>> Right, yes.
>> That makes debugging maintenance a lot easier, so that's one of the reasons why I don't like the other method of doing this filter functions.
>> Gotcha, yep I have to concur with those.
>> Taking it a step further, another suggestion this blog post has for a way to implement the same thing in yet another way.

It seems like there is an endless number of ways to implement this really simple function.
>> Yes.
>> What they suggest is, instead of returning a maybe, so either you have something, or you don't. You could return a list that says, either you have nothing or you have exactly one thing, or you have two or three or thousand or infinity of them.

Which is starting to look to me not at all like a filter operation,
>> Right
>> And I'm having a hard time thinking of a filter where, like, okay, I want to filter out light switches that are on or off. But, every time I look at a light switch, I produce two light switches.

>> Yeah, yeah.
>> That doesn't make any sense.
>> They're multiplying.
>> I just wanted to find all the light switches are on and now I have twice as many light switches in here.
>> If only I could do that with money that'd be nice.
>> I think that's coming in the next version of Haskell.

>> Nice, print money, yes.
>> Okay so, I think we've covered a lot of interesting ways to tackle this one very simple problem of how do we filter things out of collections, is there anything else you wanted to say about this topic?
>> No not really, other than that, I feel like we encountered this a lot in ELM.

>> Yeah.
>> And I see that we are using it, but we're trying to avoid Boolean blindness by implementing types that, convey what true or false may mean specific to that function, so.
>> Yeah, and I'm trying to think of a concrete example that we've had recently, can you think of one from our own front end?

>> It's been a bit, but I think it is this user assigned or not?
>> Mm-hm.
>> That might have been one of them.
>> I know that we have a lot of, I think they're called accordions in our UI.
>> Yes, yeah.
>> Where, we have three steps, but only one step can be open at a time.

>> Mm-hm.
>> And I think, it's kind of natural to reach for a Boolean in that case and say, is this one open or not? True or false? And I think we try to represent that as an actual type that says-
>> Yeah.
>> This is accordion open or accordion closed.

>> Yeah, collapsed and uncollapsed, I believe we did.
>> And does that make the code easier to read? Easier to maintain, is it,
>> Yeah, I would say definitely, even if it's just like small things, you get the cognitive overhead. it's reduced just because you don't have to think much about it.

Like I said, it's just expressive.
>> Yeah..
>> It's nice.
>> You don't have to think that's true mean this or that.
>> Yeah,
>> And I know I stumble all the time when we have a value or a function that itself is negated, so it says like not turned on.

>> Mm-hm.
>> Or turned off. And turned off equal true and I think well okay, so that means that it's off, which means, turned on would be false. And with this specific type, you don't have to jump through those hoops every time you are analyzing one of these things.

>> Right, and even the, let's say I remember in that article he had a function of the type signature that had a bunch of bools. The bools were then replaced with more meaningful, types, you can just look at the function and determine a lot more just from that alone as well.

>> Exactly, do you feel like writing functions like that, or writing code that uses those functions is harder than writing the similar versions that use Boolean's?
>> I don't feel like they're harder. I feel like they're a lot easier to use.
>> Mm-hm.
>> So there may be a little bit of, okay, I just need to suss out what true and false means here, for this particular thing.

And then name it which is one of the hardest things to do. But, after I get done with that, yeah, I think it's really nice to have.
>> So you're talking about the kind of mechanics of refactoring a function to use a specific type rather than Boolean, right?

>> Right, yeah, I'm sorry.
>> No that's okay, that's good insight. I meant to ask about, just day to day using one when you have to write a new function and you got to use one of these that needs a specific type, is it annoying to have to import that type and wonder why didn't they just use a Boolean here?

>> I think after you realize how useful it is to have an expressive type like this. Yeah, I think you get over that pretty quickly if you have any sorts of doubts or yeah, negative feelings towards it.
>> I agree.
>> I've grown to dislike Boolean so much, when I see one in the code I think there's a type lurking here.

I need to write it down somewhere.
>> We've been talking about Boolean Blindness, and how you can avoid it by using more expressive types. With me today, has been Dustin, once of the engineers here at ITPro. Thanks, Dustin for joining me.
>> And thank you for having me.

>> And thank you, for listening to the Haskell weekly podcast. This has been episode four. If you liked our show, find out more at our website HaskellWeekly.news. Thanks again for listening. I've been your host, Taylor Fausak, and we'll see you next week.
|]
