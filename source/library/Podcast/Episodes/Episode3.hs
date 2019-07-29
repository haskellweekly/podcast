{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode3
  ( episode3
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

episode3 :: Either String Episode.Episode
episode3 =
  Episode.Episode
    <$> Article.fromString
          "https://www.parsonsmatt.org/2015/10/03/elm_vs_purescript.html"
    <*> Date.fromGregorian 2019 3 25
    <*> Description.fromString
          "Jason Fry and Taylor Fausak compare frontend and backend \
          \languages, including PureScript and Elm."
    <*> Seconds.fromTimestamp 23 47
    <*> Guid.fromString "069964f7-2457-479f-8bab-9cb4f3abec9c"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-03-25-episode-3.mp3"
    <*> Number.fromNatural 3
    <*> Right (Bytes.fromNatural 34265398)
    <*> Title.fromString "Frontend Languages"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hi, and welcome to the Haskell weekly podcast. This show is about Haskell, a purely functional programming language. He's your host, Taylor Fausak.
>> Hello.
>> And he's the lead engineer at ITProTV. With him today is me, Jason Fry. I'm one of the engineers here like a pro as well, thanks for me joining you today.

>> Thanks for joining me Jason and for introducing yourself.
>> Yeah, we think it's fun for the host not to introduce himself, so what are we talking about today?
>> Well, I know that last week you were asking me a little about the languages that we use everyday here at ITPro and kind of why we chose them.

For context the languages that we use on the back end is mostly Haskell, and on the front end is mostly Elm. And there's a lot of choices, especially on the front end of which language you want to use and you were wondering why do we use Elm?
>> Yeah, I've heard you talk about PureScript, talk about it well and I am interested in learning more about PureScript.

So I guess the question I asked you was, if we were to make this decision again today knowing what we know and experience we've gained over the last year and a half. How would the decision change? Would we use PureScript on the front and backend for simplicity's sake?

Would we still use Haskell and Elm? Would we use Haskell and PureScript or just go back to JavaScript.
>> I'm pretty sure we wouldn't go back to JavaScript. Everybody seems to like working with Haskell and Elm around here, so I don't wanna subject them to the horrors of JavaScript.

>> It's true, it's true.
>> If we made the decision again today, I think we would end up with the same choices, Haskell on the back-end and Elm on the front-end. But I wasn't here when we made the decision in the first place. So I'm not entirely sure if we were still using JavaScript and we had to reintroduce some new languages.

Would these ones gain traction or would we end up using something else like TypeScript on the front end and go on the back end? I don't know. Obviously I like Haskell, so I would really campaign for that one on the back end. But on the front end, I feel like there's not a way to use Haskell on the front end or at least not a way that I would want to subject everybody on the team to.

I know that there are tools like GHCJS, which let you compile Haskell into JavaScript and use it on the front end, but they seem to be not quite production ready. I'm a little hesitant to say that, because I know that people are using them for real and using them in production.

I am not ready to make that leap.
>> So what is not quite there for GHCJS in your opinion? Because I know very little about it but I thought it's been around for a little while and it theoretically should be usable, but you're like, no, we won't do that.

So what is lacking there in your opinion? Yeah, before I get into what's lacking, I just wanna do a brief recap or a summary of what ghcjs is. As the name implies, it is like GHC, except it emits JavaScript rather than static binaries or binaries. And that's a nice thing because you can use the exact same code on the back end and the front end and it'll work in both places.

One of the project goals for GHCJS is to be able to compile all of hackage that the Haskell package repository, and have it just work on JavaScript. That's in comparison to things like Haste which are a subset of Haskell that you have to specifically target. So it is kind of like Haskell and you can reuse a lot of the same stuff, but not everything.

So I mentioned that because that's kind of the upshot of GHCJS. And if that sounds interesting to anybody listening to this, then I encourage you to go check it out. But for me, one of the reasons that I'm not excited about using GHCJS in production, is that, well, it comes down to do two things for me.

One is that it produces very large build artifacts. So the amount of JavaScript that it produces is very high and the reason for that is that it has to emulate the entire GHC run time system in JavaScript. All of the laziness, all of the multi threaded abilities that GHC has, it has to pretend to be able to do those things or actually do those things, in JavaScript, which doesn't support them by default.

That means that a really small application, the classic example of something that prints hello world, ends up being very large in GHCJS. Versus if you wrote it in plain JavaScript, it's literally one line of code.
>> Okay, that makes sense. My understanding though, is that Elm produces a lot of lines of code in JavaScript.

So when you're comparing these, is it just a lot more with GHCJS even though Elm is kind of a lot itself?
>> It is. I wish I had some specific numbers to point to, but my kind of gut level understanding is that GHCJS will produce JavaScript on the order of like 1-10 megabytes.

Whereas ELM I think typically is under one megabyte pretty comfortably.
>> Yeah, wow, that is a lot, okay.
>> And the number in my head for Elm is for a small app you're talking about kilobytes.
>> Yeah.
>> So that is a pretty big difference. Okay, that makes sense.

>> And to touch on that for a second, one of the reasons that Elm produces a somewhat large JavaScript bundle is that it has its own run time to handle all the message passing and serializing to and from JSON and all that stuff. But it tries really hard to match the semantics of JavaScript whenever possible, so it's strict and it's single threaded.

So it doesn't have to deal with multi threading or laziness or anything like that. So its run time can be a lot smaller.
>> So a little more about GHCJS, is there anything else that's sort of lacking there? Or is that just sort of, that's basically it?
>> That's the main one for me, but kinda writing on the coattails of that is that performance can be a problem with GHCJS.

The GHC runtime, the one that you get by default just compiling a normal Haskell program, has been tuned pretty well over the years. Haskell is a surprisingly old language and GHC is a surprisingly mature compiler. And the stuff that you produce out of it will perform really well.

You don't have to worry like, this is tight loop is not gonna perform well. You can, if you're interested in it, really optimize the performance of stuff, but it's a discussion for another time. With GAHCJS it is largely the work of one person and it's seems to me I don't know if this is true or not, but it seems to be a more of a hobby type project for that person.

It's not the thing they're working on full-time. So they don't have as many opportunities to get these optimizations in there to make the program that it produces really, really fast. However, even if they did have the time to do that, it still has the problem of basically running the Haskell run time inside of JavaScript.

So you've got a price to pay just for doing that. So that performance makes me kind of shy away from it. Again, I wish I had some specific numbers for how Elm performs versus GHCJS versus vanilla JavaScript. And it's also my understanding that GHCJS has improve quite a bit recently and gotten a lot faster.

But I think it's still slower than all of its contemporaries.
>> Okay, so let's compare those two then. If the comparison is no longer with Haskell, like we're just always using Haskell on the back end. Then on the front end PeerScript versus Elm pros cons, we went with Elm.

We really like Elm, but I do sometimes wonder, because I'm just curious about PeerScript if we were just going to make the decision differently a year and a half ago. But you answered that what would we do today we probably still use Haskell and Elm. So lets just compare them, PureScript is pretty great you talk highly of it.

Sure, I really like using Purescript. I used it kind of recreationally for a while.
>> I've never used Purescript in production, but I know plenty of people who have, so I have confidence in it in that regard. Backing up a bit. You mentioned that it was around a year and a half ago that we started using Elm.

And it was kind of the entry way for us into Haskell. Before that the team was largely using JavaScript with a smattering of other languages, like maybe Go or Rust something to that effect, right?
>> Yeah, I don't think we were using rust. We weren't that cool back then.

But yeah, Angular one point, something or other and Go and I feel like we have a TypeScript file in there somewhere.
>> Elm is nice because it is very simple, especially in comparison to Haskell. And because of that simplicity, it has the ability to produce very helpful and illuminating error messages.

When you do something wrong in Haskell, not you specifically, but anybody on the team, often the error message that it spits out is inscrutable.
>> Yes.
>> And I have frequently seen team members completely stumped by an error message and call me over, and I have to say, well, what it's really telling you is this, and here's how you may want to fix it.

Versus when something goes wrong in Elm that's almost literally what the Elm compiler tells you. It says, hey, I saw you did this. I was expecting this. Here's how you might fix it. And that's super useful when so many other things are changing, because Elm forces you into this weird architecture, weird from the perspective of an angular developer, it's completely foreign.

And it's a strange syntax, and it's a different compiler. All of these things changed, it's really nice to have that helpful compiler there at your side.
>> Absolutely. It's one of the things that we really like, and with Elm 19, it's an even friendlier compiler. Took me out to dinner last night.

>> Complete gentlemen. But for when we started Elm a year and a half ago, it was really helpful to get those that sort of helpful feedback. But we still were pretty new and even with it being a friendly compiler, we still were stumped much more often. But now we have some more people in our company that are coming in and learning Elm.

More of our design team is starting to learn Elm and that highlights it even more for us, is that they have repeatedly said my goodness this compiler is so helpful. So that is really cool to hear them say that and for us to be reminded. Cuz we begin to take it for granted and especially because we compare it to the Haskell compiler.

Which just today had some issue that I thought when we compiled it was going to complain because a parens was in the wrong spot and it gave some really gnarly output. I was like I don't know what you were saying, put a parens there and my kinda gut just tells that's what is is, sure enough that's what it was.

>> Yeah.
>> But if I had not already suspected I was gonna have a parens issue I don't know long I would've looked at that output not knowing what to do.
>> Yeah, I often find that the GHT compiler is more useful at telling me that there is an error rather than what exactly the error is.

Usually I'm like, well, I probably know what it is already.
>> Yeah.
>> Thanks for letting me know Haskell.
>> Yeah, so we love Elm's compiler. Purescripts' compiler just isn't quite as friendly, it sort of as a couple tattoos, smokes or something.
>> Not nearly as friendly. In fact, when you compare Elm to Haskell, and you see how much better Elam is, I feel that when you compare Haskell to Purescript, you see how much better Haskell is.

>> So that bad? Yeah, granted, my perspective of the Purescript compiler is woefully out of date. I used it maybe two or three years ago, and I I am confident that in the meantime things have gotten better. But it feels a lot like it technically tells you what the problem is.

But it drowns you in a bunch of contexts and weird variable names and other things and weird terms. From my point of view, I wouldn't be super comfortable throwing a junior developer in front of the Purescript compiler and having confidence that they could figure things out. Or not a junior developer, but our front end team.

If they were working on a PureScript application, I would expect to be called over there frequently, having to puzzle out some output from the PureScript compiler.
>> Right, so one of the draws of JavaScript on the front end and back end is the simplicity. Elm is not yet usable on the back end, I don't believe.

Haskell, as we've discussed for us, we're not going to use it on the front end. So is there still value though in going with Purescript because you can use it on the front end and the back end? Maybe not for us because we can do Haskell and we think Haskell, as good as PureScript is, Haskell is even better.

But if Haskell wasn't an option for us for some reason, does that make more sense to go PureScript front and back, or Elm front end, Purescript back end, or something different?
>> Given the choice and having to exclude Haskell, I probably would choose Purescript on the back end.

I may end up choosing something even more niche. Actually, I don't know that would be more niche but something like OCaml or ReasonML, all of which can compile to JavaScript. So for instance, if I had a mandate that said you have to run Node JS on the back end, I would say, okay, I'll do Purescript.

>> I would say why, why do I have this mandate.
>> Who's making these mandates. In those situations I would choose something like Purescript or OCaml or ReasonML over GFCJS. There's plenty of languages that compile to JavaScript. I think that Purescript in a lot of ways is better than Haskell.

It addresses a lot of the problems with Haskell that come from it being a very old language that has sort of ossified around these strange original design decisions. For instance, not having anonymous records, so every time you wanna build a new data type, you have to explicitly say these are all the names of the fields and their types.

Versus in Purescript you can just create one as easily as you create a tuple or a list or whatever.
>> Nice. Similarly, strings in Haskell are notoriously a linked list of characters, which is one of the worst representations you could come up with.
>> I feel like I could come up with a worst one, but sure.

>> Whereas in Purescript, they're basically the same as JavaScript strings. Which, given how ubiquitous JavaScript is, we have to assume are at least somewhat acceptable.
>> Yeah, yeah. Okay, so Purescript does the anonymous records, which reduces a lot of boiler plate. So if you have a user record with a name and an age and an occupation, and blood type or whatever, sure, and you want a subset of that record for some other purpose, so you have your master record.

And then you have well here, I just want these three fields, but there, I want these five fields. That's what you're talking about with anonymous records?
>> Exactly. As an example from our code base, we have the fully fledged user type that has those fields you were talking about.

Not blood type, we don't capture that about our users.
>> Yet.
>> But we also have a minimal user, which is useful when we're writing tests, or when we want to add a new user, and we don't have all of their information yet. And we don't care, we don't have their address or their billing information, or any of that stuff.

>> Their shirt size.
>> Their shirt size, exactly. With Haskell we have to explicitly make a user, a minimal user, a slightly more minimal user, a slightly less minimal user, user with a bunch of other fields tacked on. And in Purescript you can make all of those a little more implicitly, a little more off the cuff.

And as we frequently experienc, one of the hardest things in day-to-day software engineering is naming things.
>> Yes.
>> And when you have to make new Explicit types of everything, you have to make names for them. And this sounds kind of like a petty concern, but it's honestly hard to do.

Because if you have a user and you have a user with two different fields, they're both users. So which one, you have to call one user and one something else. It's frustrating.
>> User prime.
>> User prime prime.
>> Please don't do that.
>> User prime prime prime.

>> We don't allow primes here.
>> No primes.
>> No primes. Yeah, so I like that, I can certainly hear someone who prefers the Haskell way of doing it, though. Because arguably, those are different things, and different things who have different names.
>> Very true.
>> But in the day-to-day, it is just kind of inconvenient.

So is there anything else that comes to mind, Purescript being better, maybe than Haskell in some way, or some other benefits to it? I think that it has a lot of well thought out type level features. This isn't something we typically work with in our work at ITPro.

We try to stay in the value level as much as possible. But Haskell is moving in this direction where it's getting closer and closer to having dependent types where your type signatures and your type computations can depend on the values themselves. And the classic example of this is a list where you know how many elements are in that list.

And so you could group, for instance, with a map operation, that you don't change the number of elements. I don't really wanna get into dependent types. But suffice to say that I think Purescript has a has a lot more niceties in the type system that, to get them done in Haskell, you have to turn on a bunch of language extensions and jump through some weird hoops and do some things that aren't very comfortable.

So to bring it back home, I think you original question was, if we weren't using Haskell on the back end, what would we be using on the back end?
>> Yeah, yeah. Let's talk about that.
>> So ultimately I think I would choose Purescript. The reasons that I prefer Haskell over Purescript right now are that it produces executables, binary executables which are a lot easier to deploy.

In our instance we stuff the executable into an empty docker container, throw it up in AWS and run it. With Purescript we have to stuff all of our code in the container, along with all of its NPM dependencies and all of the power dependencies, and no JS. And then throw that up on AWS and it's a much bigger container, there's a lot more points of failure and lots of security, well things that could go wrong.

Not to say that nothing could go wrong with the Haskell thing, but it's conceptually simpler to deal with. Also Haskell has multi-threading built in. So if I want to say, send this email off on a separate thread, I can do that. And it's built right in. Versus doing that in JavaScript or Purescript you have to handle that with asynchronous and, sorry.

Asychrony and call backs. So you have to say, well, do this. But while that thing is executing, nothing else can actually happen. So if you have some CPU intensive stuff that you want to do on a separate thread, you can't, you have to rely on a much more heavyweight abstraction like a queue.

So you publish work to the queue and then have something else, some entirely separate process pull it off the queue and do that work. So those kind of operational concerns are what makes me lean toward Haskell versus Purescript right now.
>> Okay, that multithreading constraint sounds pretty significant.

Because a lot of what a back end is going to do at some point, maybe not a lot of what it does, but frequently you're going to be doing something like that, where you're wanting to handle something asynchronously send off an email.
>> Yeah, almost all the time.

>> Yeah. Always sending emails. So that makes a lot of sense. Purescript sounds great. But if you can do, if you can use Haskell, it sounds like you should.
>> You should.
>> Of course, that's our opinion because that's literally what we're doing. But I start to see that a little bit better now.

Thank you.
>> So we're choosing Haskell on the back end over Purescript because Haskell just has more tooling like asynchronicity, whereas PureScript is a little more complicated. It's easier to deploy because Haskell just has the binary that you deploy that. Whereas PureScript, a lot more points of failure there.

Again these things tend to be solved problems but there's always cracks in that so just nicer, simpler to think about. Simpler to handle. So that's Haskell versus Purescript on the back end just quickly. On the-
>> And to jump in for just a second, I would love to use Elm on the back end, but it's not currently an option.

And I feel like Elm is doing very well at targeting the front end and its particular niche that it wants to dominate. I'm very happy that they're doing that. If I could use it on the back end, I would love to, but-
>> Really? That's the first I've heard of this.

Elm on the back end, Haskell on the front end.
>> You heard it here first.
>> Precisely. So, Elm over Purescript, primarily because of the compiler. Purescript has a lot more complexity, a lot more robustness in some areas. But you don't need those strictly. They're really quite helpful in some cases, so maybe your use case, you really do need those things.

But for most people of of the time, Elm totally targets single page apps. They are focused on that, and they're doing it quite well. The compiler makes it really easy to pick up this new paradigm of functional programming, a new syntax. It has, we didn't talk about this, but something that we like a lot here at ITPro is, it has this architecture built right in to the language itself.

And it's nice to not have to think about that or worry about that, it's just there.
>> Right.
>> The tooling's really good. It's an opinionated formatter, so you don't have formatting wars or arguments. So, those are some of the reasons that you mentioned and then I just threw on top of that of Elm over Purescript.

It's like Purescript seems really good, but Haskell might just be a little better than it in a lot of cases on the back end and Elm might be just a little better than it In a lot of the cases in the front end. But I just need to use it recreational like you used to.

>> Is what I need to do.
>> Thanks for being on the show with me today, Jason.
>> Thank you.
>> It was great to have you here talking about Haskell versus Elm versus Purescript. A fight for the ages.
>> Versus a bunch of other things.
>> We don't talk about them too much.

And thank you for listening to the Haskell weekly podcast. This has been episode three. If you liked our show, find out more at our website, haskellweekly.news. Thanks again for listening. I've been your host Taylor Fausak, and we'll see you again next week.
>> Bye.
|]
