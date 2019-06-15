{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode10
  ( episode10
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

episode10 :: Either String Episode.Episode
episode10 =
  Episode.Episode
    <$> Article.fromString
          "https://blog.ploeh.dk/2016/03/18/functional-architecture-is-ports-and-adapters/"
    <*> Date.fromGregorian 2019 5 20
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about how Haskell encourages \
          \you to use the ports and adapters architecture."
    <*> Seconds.fromTimestamp 16 37
    <*> Guid.fromString "32fd3459-b349-4c99-9150-5073fedab6bf"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-05-20-episode-10.mp3"
    <*> Number.fromNatural 10
    <*> Right (Bytes.fromNatural 23942886)
    <*> Title.fromString "Functional architecture"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hello and welcome to the Haskell Weekly Podcast, a podcast about Haskell. Haskell is a functional programming language that we use here at ITProTV, and we're just super excited about this episode. I'm your host Cameron, and with me today we have Taylor Fausak, our lead engineer here at ITProTV.

How are you doing Taylor?
>> I'm doing well, thanks. How are you Cam?
>> Can't complain. It's going well, I would say.
>> Full of fries?
>> I just ate so many fries, ugh. It was so good. But today, I just wanna kind of talk about an architecture that is a good functional architecture.

It is Ports-And-Adapters. And what are Ports-And-Adapters? So I'll kind of talk more about Ports because that's really what I wanna do. So Ports are kind of responsible for interacting with the outside world and that could be a web app, it could be the database layer, a way to communicate with the outside world
>> It's the thing that's making a call to the database or to some external service that you're using or even writing to the console.

>> Exactly, yeah. And adapters are kind of everything inside, kind of handling all the business logic.
>> Okay.
>> Being able to kind of going hey, this is what this is.
>> So adapters take their input from the the ports and then do some other stuff. And then what talk to another port talk to another adapter.

>> Just depends on what the flow you know of your-
>> So it could be either one.
>> Right if you have an internal structure that needs to talk to another internal structure you'd be using adapters. But if you needed to call out to the database, you'd be using a port.

>> Okay.
>> Kind of, it kind of keeps things together, not so much, not so jumbled. It kind of keeps it clear, the boundaries between pure logic and input-output logic, right?
>> Right, and so you mean, the pure logic is what a lot of people sometimes call business logic.

The interesting stuff that your business is doing, or your application is doing.
>> Right.
>> And the ports handle the, maybe not so interesting stuff have actually performing an HTTP call, or something like that.
>> Exactly.
>> Okay, so now that we have kinda the vocabulary of this architecture, could you explain in more detail what the architecture is?

>> Kind of the Ports-And-Adapters architecture allows us some niceties around it. So it kind of allows us to separate that input-output stream from the internal business logic. And it keeps our the ability to test internal business logic easier, cuz you're not always interacting with some input-output that could mess with the result.

>> Yeah, it's a lot easier to test a pure function because if you have to write a test that, let's say interacts with the database. Then that means to run your test suite, you have to have your database up and running and your schema has to be up to date and you may have to have fake data populated in there.

But if you're testing pure code, you just have to have a bunch of values in memory which are a lot easier to make. Also, you can test things in parallel if they don't have to talk to a database. Cuz maybe you have two tests for the same piece of business logic in your code and they would both be writing to the same database table.

You're gonna have a bad time unless you really do a good job of separating those things out. Whereas in pure code, you can just run two things at the same time, no problem.
>> Right, and at some point, some level, you want to test the ports. You want to make sure that given a set of input, you're getting a reliable source of output.

Because that's what you expect from an API. You're saying the same thing to an API you would expect, it's returning to the same values.
>> At some point you want that integration test to prove that your API actually works.
>> Right, but it does-
>> But the unit tests on the inside, those are still really valuable.

>> Right and the integrations are still a little more boilerplate kinda set up generally. And they're a little more, I won't say in bases, but they're definitely a little more meaty per se. So it's nice to be able to have that separation and have guarantee that the pure functions, they're gonna do what they need to do and those individual units are gonna perform as intended.

But we also can create this test for the ports as well.
>> Yeah.
>> To allow that kinda testing.
>> So Cam, we're talking about this architecture today because of this blog post that Mark Seaman wrote called Functional Architecture is Ports-And-Adapters. So pretty straightforward. But he makes a point in there that you can do this architecture in other languages.

He uses F+ as an example. But in those languages, it can be really hard to have the discipline to force yourself into this architecture constantly. And if you ever mess up, then suddenly you have something that is both a port and an adapter, and it becomes hard to tease those apart.

What is it about Haskell that helps us write programs that are in this architecture and get these benefits from them? Yeah, the ability of purity, in types. Types systems enforces a lot of need to have type A, cross the boundary is type A still. Like it shouldn't be able to change across the boundery.

>> Yeah, and we've mentioned purity a couple of times, and I just wanna be clear about what we're talking about specifically with regards to ASCII. It's a function that doesn't operate in I/O for our purposes at least at ITPro. That's typically what we mean by pure or not in our like application level handler.

>> Right.
>> Something that you give it a bunch of inputs and it gives you an output. End of story. Whereas impure code is something that has to be executed in some context, either with a database connection or with I/O or whatever. So when we're talking about pure functions, that's what we mean.

>> Right, right, right. You know, we read through this article I'm a little bit in. It's about Ports-And-Adapters in school and stuff like that. But kind of talking about is a lot different than implementing it because in reality, Haskell kind of forces out hand this like And we don't have to really think about what the architecture really is in the grand scheme of things.

>> Right, it forces you to do this architecture without having to be consciously aware, thinking, I'm implementing Ports-And-Adapters here.
>> Right.
>> Instead, you say, ih, this function suddenly needs I/O, so I have to put it in the type. And it propagates out everywhere. And then that encourages you to try to constrain the places where that's used to say, no let's only I/O over here at the boundary.

And keep everything else pure.
>> Yeah, and on our code base here, we're very adamant about that. If we see that, we're passing the monastic context through all of these functions, we don't necessarily always need the database connection every single one of those functions. We should take a step back and say, why don't we just find the data we need and then pass it to a pure function that allows us to have a little more certainty?

And not have to worry too much about passing this working in-
>> Instead of passing this giant implicit context around them, basically the whole real world. You identify the little pieces of that that you do need and turn them into regular function arguments.
>> Right.
>> And in the course of doing that refactoring, sometimes your function's signature can look more complicated, because it's getting more arguments.

Instead of just being an I/O, or app, or whatever. But conceptually it becomes a lot easier to reason about. Because just looking at that type declaration, you know those are the only things that has access to use that's all it's gonna do.
>> Right.
>> This is especially nice for a long running, a long lived application where many different developers are working on it.

And people are maintaining it, fixing bugs. And you get that confidence on that pure code when you repack or something, you haven't accidentally broken some other part of the system that relied on some internal state that that thing fiddled with.
>> Right, right, right. Yeah, and I highly value that with Haskell.

It really makes, and there's a few of our podcasts we talked refactoring in general. And how easy Haskell makes that. But the the type features just, it makes it easy to understand. Okay, we're going to pass all of these things in, and we're going to adapt them to the type we want them to be, right?

Okay, adapter, right? Crazy, guys.
>> That's where the name came from.
>> Wow. But that that's the nice aspect of Haskell is being able to read and know, okay I take these arguments and I transform them and I adapt them to be the type I'm returning. Whereas a lot of other languages that's not always the case.

And it's not easy to understand, wait, why am I trying to keep these boundaries separate? There's other languages, just not as clear.
>> Right, it can be hard to tell looking at a function, if it's pure or not. In Haskell, it's very clear if that's the case. And it's interesting because this blog post by Mark reminded me of an earlier screen cast by Gary Bernhardt who did that famous JavaScript talk where he shows up all these kinda weird cases of JavaScript behavior.

But he has a screencast where he talks about this concept of functional core imperative shell. Which is very similar where all of your business logic on the quote and quote inside of your application is pure.
>> Mm-hm.
>> And then you have a very small layer on the outside that's responsible for collecting stuff from the outside world, passing it off to your application and then taking that result and sending it back to the outside world.

>> Right.
>> So it's funny how this concept of either functional core imperative shell or Ports-And-Adapters keep showing up in a guy who works with Ruby full time or Mark, I think he does a lot of F sharp, C sharp kind of the .NET world. Or even like you mentioned in computer science curriculum, which is often Java or Python or something like that.

>> Right, very object oriented.
>> All these language ecosystems have recognized that this is a powerful, useful architecture to set up your applications. But in Haskell It's just the way that you do things. There's it's almost harder to not follow this architecture.
>> Right.
>> So I see a lot of benefit and using a language that pushes you in the right direction like that.

>> Yeah, cuz obviously if all these people are using it, it's not wrong, like it's-
>> Probably not a bad idea.
>> Right, like there's probably other options out there. But the fact that it allows you to protect your internal business logic from the outside world, that's important.

>> Yeah.
>> You don't want to be able to just come in and immediately from the outside world somehow modify some of the internal business logic and Haskell doesn't really allow it to happen.
>> Yeah and it can be even more benign than leaking details about your business logic.

One example I've seen before is, in Ruby, there is a templating language which is basically a wrapper around Ruby. And there had been times where you can have a template, that inside the template, it makes a call to the database. And if you put that inside in your template cuz you're like listing all of your users and you wanna get some associated object with them.

You could be making a thousand queries in your template of all places. And templates feel like they should be pure functions.
>> Right.
>> Like don't talk to the database, you should already have the information you need.
>> Right.
>> So, this architecture helps you avoid problems like that in addition to a bunch of others.

>> Right, and it's not to say that like, like you said, it's just harder to make Haskell not be Ports-And-Adapters. I can go through code we've written when we were first starting Haskell and just, we would pass this giant contacts all around. And we're gonna find data here, here and here and always be talking to the outside world.

And kind of over time you kind of realize, this just doesn't feel right. And so Haskell allows you to refactor that out too. Like we can make you make this one piece.
>> Yeah, and that feeling of this isn't quite right. That's Haskell nudging you in the quote unquote right direction for this architecture and where a lot of other languages kind of push you away from this architecture.

Haskell pulls you in, so it's, yes, keep doing that. That's the right thing.
>> You can do it.
>> Right, I think there's, a nice, Ports-And-Adapters allows you to kind of see that outside, you've got various aspects that are that are calling your API, your code base whatever that may be.

>> Yeah, we talked about API's a lot cuz we do web programming. So that's kinda our bread and butter.
>> Right.
>> But this applies to other applications as well.
>> Right, and allows you to see okay, what has the possibility to talk to. What does it have the possibility to talk to?

And then inside it allows you to kind of keep all of those functions that allow you to be agile and quick and reuse and have these validation functions. Or having like auth checks or stuff like that, depending on what your object case is. That's calling out and finding a user and understanding what app permissions they have.

That could probably involve more of a port than you're wanting to use in that case. But it just kind of allows you to keep all that logic just so separate. And not feel like, I don't know, I feel like there's been times, and there's frameworks that do kind of force your hand in this and other languages, right.

Cuz there's MVC, right. MVC kind of, in my mind can translate into Ports-And-Adapters in some regards.
>> Yeah, cuz your controller ends up usually being a port where it's collecting information from the outside world. And then your view is kind of a port in that it's presenting that information back out.

And then your model is typically an adaptor in that scenario.
>> Right, right and we, using sales when we're in JavaScript land, that kind of kept that stuff separate. They get hairy at times, like, we can find ourselves like in a service pretty much doing all the time controller action.

When the idea of a service is really, it should be more than an adapter. And say hey take these outputs and I give you this output.
>> Right.
>> Rather than like we're gonna call out to this service and that service and we're gonna build this giant thing for you.

And the controller's just going to say, I'll listen here and I accept request here.
>> Yeah.
>> And then I enter to service, that's a big thing. And that's just a super hard to go back into and understand like it doesn't like yeah, MVC could be like Ports-And-Adapters but it's not, it's not.

It's easy to not do that.
>> And nothing is pushing you in the right direction.
>> Right.
>> You said that shoving all this stuff into the service didn't feel right but that was just kind of a gut feeling. There was nothing in the language or the framework telling you, maybe don't do that.

>> Right.
>> Whereas in Haskell, you do get, you know, I have to propagate I/O through all these functions or I'm passing in a hundred arguments. This is very clearly telling me something is wrong.
>> Mm-hm, know for sure.
>> So I think I've said just about everything I know about this functional architecture being Ports-And-Adapters.

Do you have anything else to add?
>> I don't think so. I'm a little all over the place today. Just kind of coming out of the food coma a little bit and yeah. Yeah it's a little warm in here but that is what it is.
>> It's Florida I mean.

>> It's Florida but I also have my fair issues of sweating.
>> At least we're not recording outside.
>> I'm always here for you guys.
>> Always representing. But anywho we, this was a great article it was cool kind of go blast from the past kind of was all over the place.

But I think because this Haskell has this identity that pushes you towards Ports-And-Adapters. It makes it really easy to just keep that logic separate from input and outputs and all the internal business logic those things. It's very important to keep those separately. It allows employees who are coming in or when we revisit a code later, like understanding like this is just business logic.

This is the inputs, the outputs, if you need to change something later, like Haskell is functional and it's compiler is amazing.
>> Sure is.
>> And the type system is incredible. So that really just kind of all meshes nicely together.
>> Yeah, well said. So thanks for chatting with me about Ports-And-Adapters, Cameron.

>> Of course, thanks for having me.
>> It's always great to have you on the show. And thank you for listening to the Haskell Weekly podcast. If you liked what you heard today, and want to know more about us, please check out our website at haskellweekly.news. This has been episode ten, and we'll see you next time.

>> Adios.
|]
