{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode13
  ( episode13
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

episode13 :: Either String Episode.Episode
episode13 =
  Episode.Episode
    <$> Article.fromString
          "https://github.com/github/semantic/blob/eaf13783838861fe5eb6cd46d59354774a8eb88d/docs/why-haskell.md"
    <*> Date.fromGregorian 2019 6 10
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about why the Semantic team \
          \at GitHub decided to use Haskell."
    <*> Seconds.fromTimestamp 25 8
    <*> Guid.fromString "fb192c3c-02a5-4413-ab53-1346677940ec"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-06-10-episode-13.mp3"
    <*> Number.fromNatural 13
    <*> Right (Bytes.fromNatural 26111814)
    <*> Title.fromString "Why Haskell?"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hello, and welcome to the Haskell weekly podcast. I'm your host, Taylor Fausak. I'm the lead engineer at ITProTV. With me today is Cameron Gera, one of the engineers on my team. Thanks for joining me today, Cam.
>> Thanks for having me, Taylor.
>> It's good to have you on the show.

And on this show, we talk about Haskell, no surprise there. It's a purely functional programming language. But what specifically are we going to be talking about today, Cam?
>> We're gonna be talking about why Haskell?
>> Why Haskell? What do you mean by that?
>> Well, there's a blog post by the semantic team at GitHub, called Haskell.

And why they chose Haskell for that project, kind of led by Timothy Clem, so.
>> Okay, so GitHub is using Haskell in production, that's awesome.
>> Yeah, it's a big secret.
>> We're not the only company out there with Haskell code.
>> No, there's some big time. But before we kinda get into their article, why don't we kinda start with what is Semantic?

And why maybe this Haskell good for them?
>> Yeah, so Semantic is I guess a package or program by the GitHub team, and it's what they use to analyze and compare source code on their servers. So when you submit a portal request to GitHub, you may notice that in addition to being able to break it down by file.

Jump to this file in the diff, you can also jump to specific functions and say, show me the changes on this function alone, and Semantic is a tool on their backend that allows that to happen. So they can parse the source code of JavaScript, or Python, or whatever, and figure out, this is a function, and something within that function changed.

>> That's awesome. Why is this announcement big news?
>> This is a big deal because GitHub, obviously, is a giant force in the development community. A lot of people use GitHub to manage their source code, and they're just a huge company, so they've got lots of people working for them.

And this is a big deal because Haskell is a pretty small community, especially in relation to all of GitHub, there aren't too many companies out there using Haskell. A couple of the big ones that I can think of off the top of my head are Facebook and Nike, and us of course.

But ITProTV is not as big as them yet.
>> We're coming for you, Facebook.
>> ITProTV, the next Facebook in IT world?
>> But yeah, this is just a big deal, cause GitHub is a big company, and Haskell is a small language, and it is really interesting to a lot of people.

>> Yeah, it's good to see the community growing, and-
>> Yeah.
>> The neighboring organizations picking up this language allows for the community to obviously grow. I mean, there's so many employees at GitHub, and obviously just the Semantic team right now is using Haskell. But I'm sure it's spreading through their organization.

And it's gonna allow our community to continue to grow, and have more experts in the field.
>> Absolutely.
>> It's really cool.
>> And even if you're at a smaller company, and you're interested in using Haskell, this can be really useful, because you can point to get up and say look, they're using it, and it's working for them.

It can work for us too, right?
>> And a lot of the articles out there about why people choose Haskell are pretty normal. They're pretty standard like this this and this, but why do you think you know GitHub kinda chose this different path?
>> This different path to using Haskell?

>> Right.
>> Yeah.
>> So for a lot of the blog posts that I've read in the past about why companies chose Haskell, you sort of see the same reasons trotted out. Haskell is a pure language, so you don't have to worry about side effects in weird parts of your code.

Or it has lazy evaluation, so you don't have to worry about things being evaluated, even if you don't end up using them. Or it let's you express complicated ideas in a small amount of code. And the first time you see that set of arguments, it might be compelling.

But the 5th, or 10th, or 100th time you see them, you're like, okay, I get it. I know Haskell has those things, what else can I do?
>> No, that's awesome, yeah. And I think it's really cool that they kinda broke it down. They had some different reasons for choosing Haskell.

So do you kind of want to kick off, what reasons and why they chose Haskell?
>> Sure, as I mentioned, there's kind of the bog standard reasons for why people typically choose Haskell. But one thing that I like about this article is they didn't dwell on that too much.

They said yeah, these are all characteristics of Haskell. These are reasons why you might wanna choose it. But they ended up focusing more on what things that are only possible, because of those kind of fundamental characteristics, but more compelling reasons for their particular business case. And I think the one that they start with is control flow, right?

>> Correct, yep.
>> And so by control flow, what they mean is that in a typical programming language, something like Java or C#, or go the control flow is built into the language itself. So if a statement is above another one, you know that it's going to get executed first.

So from top to bottom left to right, basically. And in Haskell, that's not necessarily the case. You can write code that looks the same, but one version of it might be synchronous, and another version of it might be asynchronous. So this comes up in JavaScript all the time where the synchronous function is really easy to write, but you block the main thread for a long time.

If you're doing something with a web UI, then your UI stalls while that synchronous thing is happening. So you wanna write stuff in this a sync fashion. But then you have to do like nested call backs. So you do that a sync await, or something like that, right?

So the syntax, you have to change the way that you write your code, in order to change how the control flow works. But with Haskell, the code that you write looks the same, but you can change how it is executed.
>> Right, right, right. So pretty much higher up in the program is where is kind of the deciding factor of, does this sync, is this asynchronous or synchronous?

>> Exactly.
>> And the further you get in to the code, it just looks the same. You don't have to be worried of, am I writing this the right way?
>> Mm-hm.
>> Because in the end, I want this result. The code you write can be executed however you want it to be executed.

You just tell that once you start to write the executable, and make that code come true.
>> Yeah, and that's especially useful for the Semantic team, because what they're doing is parsing and analyzing source code. So they wanna be able to parse, let's say, a Python file and analyze it as if it was executing, but not actually execute it.

And so that becomes really powerful to have all these tools that Haskell provides to be able to manage the control flow in the same way that you manage regular data in other languages.
>> Right, right, no, I think that's awesome. So the other thing they kinda talk about and the reason they chose it was runtime correctness.

And they gave a lot of really great things we've kinda experienced, but what were some of the things you took away from that aspect to the article?
>> I really liked that aspect to the article, and I'm glad that they brought it up. Because when you read about, let's say Elm, another language that's in the same vein.

They bring that front and center all the time, it's on their home page. They say you can write programs with confidence, and not have to worry about runtime errors. And I wish Haskell marketed more in that direction, because it is really powerful to not have to worry about those runtime errors.

And trust that, when you write the program and it compiles, it will generally do what you want, or what you intended for it to do.
>> Right.
>> And you don't have to worry, crap, well, I forgot that this value could be null. So then you get a null pointer exception in production, or you forgot I changed the name of that method last week, and I forgot to update it here at this call site.

So now I'm getting a method not found error, right? It saves you from those really tedious kind of rote problems. And so that you can focus on developing new features, and shipping those out to your users, rather than fixing some Bug that popped up in your issue tracker for the hundredth time.

>> Yeah, one thing I wish Haskell was magical. And it can be we just aren't necessarily using it right now. I like to put TV is that kind of a way to get away from accidentally like misaligning like a value from, JSON or something like that. Because we've kind of, some of our parsing, for our JSON parsing we use the applicative stuff and that kind of ensures that like, hey, this could end up being different because you misalign the keys or whatever.

So I think as a group of developers I would like for us to kind of lead lean more into, okay, let's not use a plug it in just so we can make sure we keep our sanity. Because I mean, a few months ago, I was just banging my head against the wall trying to figure out like, wait a second, what's happening here and sure enough, it was just a miss of flip-flopped key.

It was like
>> That brings up a good point that just because Haskell gets rid of a large portion of runtime errors, it doesn't mean that it gets rid of all of them. And in fact, it's not even desirable to get rid of all of them because you'd have to pay so much upfront in order to do that.

But there are somethings like you mentioned with the applicative syntax where they allow you to sort of accidentally introduce problems that are very hard to detect and there are easy ways around that. In our case writing those pressures in a more monastic style makes it clearer which things are going where.

So you can look at a line and say no, we're pulling this field it's named price but we're pulling it from object where the key we're looking for his name, and that obviously doesn't line up.
>> Right, right, right, yeah, so I mean I do think a lot of the things that Haskell provides for runtime correctness is are great.

So I'm really glad that they kind of echoed that because like you said, Elm, that's something they boast all about.
>> Yeah.
>> But housing you don't really hear about that until you're in it. And you're like, this isn't gonna let me change a function name over here without updating all the colors, that's not possible.

>> Yep.
>> Because that compiler will not be happy with you.
>> That's awesome. The other thing they kinda touched on was kinda the research. So academia is kinda leaning more towards Haskell as a language that, these new research methods are kinda written in these algorithms and all this various stuff.

What is that useful for GitHub?
>> You touched on it a little already in that a lot of computer science papers are written using Haskell or a language that's very close to Haskell. But that kind of typical computer science paper that you read with the font that everybody knows and the to column layout and all that stuff.

The code that you look at in those papers is usually Haskell. And if you can take that code from an established research paper and more or less copy paste it into your library, and start using it right away. That's a huge win versus, if you were using basically any other language you would have to think.

Okay, how can I put this behavior that I want that they're talking about in this paper into the language that I have? And you have to change the semantics, you have to change the actual shape of the code itself, you have to make sure everything works the way you expected.

So it's just more difficult to do that. Now, that's not a common problem for work-all-day programmers like us, we're writing web apps.
>> Right.
>> We're not looking into research papers to figure out, well, how do we parse JSON? It's kind of a solved problem at this point.

>> Correct, you're right, which is nice, but it is also helpful if you're trying to be on the cutting edge of technology and the growth of technology. Research papers have Haskell, that's where the code is.
>> Yeah.
>> That's what it is. Those papers are still coming out, programming language theory is not a solved field.

And the Semantic team at GitHub Is on the cutting edge of this, they are analyzing source code for millions of software projects across many different programming languages. And they wanna do it quickly, effectively, safely and the research is kinda showing them how to do that and they're able to crib from it effectively.

>> And I think that's really awesome. So I definitely applaud them for-
>> Yeah, me too.
>> This article. They also kinda talked about the things I didn't like, which is a helpful thing when you're analyzing, okay, do I jump in and go with Haskell? Or do I lean towards the Java or C#, or something a little more mainstream?

>> Yeah, I always like it when I see an article like this. And they include the stuff they didn't like. Otherwise, it's really easy to look at it and discount it offhand, and think well, they just really like this and it's their pet language, and they want it to succeed.

So they're not gonna say anything bad about it.
>> Is it their cat language?
>> Or their dog language?
>> A cat or dog, right? Are they cat or dog people, do you think?
>> Maybe fish, they could be fish people.
>> Fish people. Lizard people.
>> The Aquaman.

>> All right, I think it does add credibility to this document or blog post.
>> Yeah, this article.
>> Very unofficial article.
>> Yeah.
>> Articles are a very good term, I should use that more often. But they talk about kind of the weak tooling aspect. This is something we've come to deal with.

>> Yeah, it's a problem we've definitely run into.
>> Right, and so do you wanna kinda explain what we do to kinda counteract this and-
>> Sure.
>> Acknowledge it?
>> First, just kinda for a statement of terms. I think that when they talk about tooling, they're talking about like an ID integration, something like IntelliJ or Visual Studio.

Where when you're working on code, you can mouse over a certain part and it'll show you perhaps the documentation for that thing or what parameters it expects or where in your code base it's used, stuff that isn't earth shattering on it's own. But a bunch of small niceties that add up to a really positive experience.

And they're right to say that Haskell is kinda weak in this regards, there are certainly tools that help. The one that we use most often is GHCID.
>> Another one I was just thinking about offhand, so I didn't catch you off on this but Intero.
>> Yeah.
>> Intero offers, the awesome IDE experience.

But it takes over people's machines and like, crashes them.
>> Yeah and we've run into problems with Haskell IDE engine where the promise of it is excellent but when you run it for more than a couple hours it starts heating up all the RAM it can find. So-

>> It's hungry.
>> I think that Haskell is moving in the right direction and they seem to have landed on the same solutions as us. Which is you have your editor up maybe the left hand side of your screen and your compiler output up on the right hand and you kind of stitch things together yourself.

>> Obviously, not ideal but definitely workable. And just to kinda color that a bit, from my point of view, I think this comes down mostly to lack of resources because developing all of that tooling just takes a lot of work. And for these other languages like Java or JavaScript, you have millions of people working in these languages, and that's not true for Haskell.

So I'm excited to see GitHub talk about this because if they get people interested and excited about Haskell and working in it, then the tooling is naturally gonna improve as more people get in with the community.
>> No, I think that's a very good point. So way to go, GitHub.

>> Awesome, well, they also kind of talk about I think that's a very good point. So way to go, github.
>> No dependent types. Why do you think this is an issue for them?
>> So we already talked a little how they kinda borrowed from current programming language research and apply it, to their problem domain.

So they're already in a kind of unusual situation as far as that goes. And I think that's why they feel this pain from the lack of dependent typing, because they're pushing, Haskell's type system to its limits and Haskell, while it's well-revered for having a strong useful type system, it does not have dependent types.

And there's lot of things you can do to try to pretend like you have them or give you some of the same benefits of dependent types, but at the end of the day, it doesn't happen. And I can't speak personally to this cause I haven't run into this particular limitation in the type of development that we do.

And I think that most developers would not run into this problem, they wouldn't think, well, I wish I had dependent types here, it doesn't come up that often. But for them it does, I don't wanna discount their experience here, obviously they wish they had this and they don't.

So the you know the only languages that do have this they're they're few and far between stuff like Agda and Idris. They have this but they're they're a little less production ready than Haskell is they're not used quite as widely.
>> Right, that's interesting, yeah, I don't think we've really thought of dependent types has never really been a thing.

>> I think that it's a great idea and I'm really interested in it but day to day there's not too much where I'm like, this would be so much easier if I had dependent types, drats.
>> Awesome, well, they also kind of talk about kind of the lack of infra glue.

>> Yeah, I like that terminology they used, infra glue, it sounds like something you buy from a TV channel or something.
>> Right, here is your infra glu, glue, wow, I can't even talk.
>> But yeah, so they mentioned that for the actual meat and potatoes of their program, Haskell does a great job.

But for the stuff where they have to worry about deploying or making containers or stuff like that, It's a little more difficult. And that's mostly because it's similar to the tooling problem where just not as many people have been using it. So there's not a canned solution but I go I want to deploy this to AWS or I want to make a container out of this.

You can definitely do those things but you're probably going to have to do them yourself rather than just picking something off the shelf that says, okay, yeah, I'll play this Python program to look at whatever.
>> It's a little more boilerplate.
>> Yeah, and again not a show stopper and not terribly difficult to get around, but just annoying that it's not there.

>> Yeah, that's fair, but one of the more interesting ones that I feel like we've encountered a lot is, the issues with lazy evaluation. I think for us we've had various scripts and you know things as you know our code is trying to figure out what it's gonna do.

Until it actually needs to execute something it just builds up this giant thunk that hangs out all over the place. And you're like, everything slows down and you're like what's going on?
>> And then you put one exclamation point in one place and everything works again, ta-da!
>> Yeah and they kinda talked about their language extension, strict data?

>> Strict data, I was like, yeah.
>> Static data, no?
>> Strict data, yeah, and so that's kind of how they interacted it, but why do you think for them that's kind of a big issue?
>> I think that lazy evaluation is a double edged sword in Haskell for sure.

And the main reason for that is that Haskell is pretty much the only programming language that I can think of off down my head that is lazy by default. Every other programming language is strict by default. So as programmers, we're not used to identifying and solving, problems that stem from laziness, because we rarely encounter them.

In other languages, you have to explicitly opt in. So like in Python, you can make a generator, or in JavaScript you do all the acing stuff to pretend like you have laziness, whereas in Haskell, it's everywhere. So people just aren't used to looking for those types of problems and so they're bound to crop up every now and then.

That being said, they're usually not terribly hard to diagnose and fix once you have a couple tricks in your bag. But the first time that you run into them, they can be completely mystifying because you can look at code and it looks totally reasonable. But for whatever reason, the way that you wrote it either generates too many thunks, like you mentioned, and you don't evaluate them at the right time.

Or just something like that, I mean, can be really difficult to figure out what's going on. And again, this comes back to the lack of tooling, if there was better tooling to analyze the laziness of your program this could be an easier problem to solve.
>> So maybe with time, maybe with time, maybe with time.

The last one, this one hits home for us is they kinda of said, it's gotta a notorious interpretation of being too hard to learn. As bunch of junior developers who just minus you and Cody, learning Haskell in the last few years it had it's a little bit of growing pains in the beginning.

But once you understand the concept and the paradigm of functional programming, it becomes a lot more attainable to really conquer the language and have a good understanding of it. Without you we're not the deepest understanding but you can have a general idea what's going on, so what did they have to say about that.

>> I think they say something very similar to what we think and hope I've said but if we haven't yet we'll say now that Haskell does have this reputation of being difficult to learn. But it's not that difficult and it's especially not that difficult to get to a working knowledge, where maybe you don't have a super duper understanding of everything that's going on.

But you can get worked on and you can feel effective and be effective, you don't have to learn category theory, or really understand what is a monad in order to program in Haskell day to day. And they say, especially with some type of, mentor or tutor on the team, you can get people up to speed really quickly.

And we've seen that we hired somebody a month ago and they got up to speed basically within the first week. Now, granted they obviously don't understand all of Haskell, everything, I mean, it's a huge language, there's a lot to know.
>> Right.
>> But they know enough to be effective, and write code that works, and submit it for PR and get it into production within a week or two of being hired.

>> Right.
>> It's not insurmountable.
>> Yeah.
>> And I think another attribute for us, attribute to our ability to learn Haskell, has been the idea of promiscuous pairing, which for those listeners, you guys check it out, there's a cool paper on it. So we've been doing it for a little while now and we find it very effective, t allows it knowledge transfer really well.

So I did a shameless plug for Jason, he's all he's all about it.
>> Yeah, he's all about it and it really ties into what this semantic team at GitHub said, of having a tutor or somebody who knows the loops to show the new person promiscuous pairing, just forces that every time around.

>> Everyone must be a learner.
>> A teacher, I'm whispering into a microphone.
>> That's kind of weird, all right, and obviously, this program is pretty large semantic. Like as far as the depth of what it can do, but it's only 20,000 lines of code. Yeah and that's amazing because as they point out several times in their article if they had written this in, let's say Java.

They may have written 20,000 lines of just boilerplate, whereas the entire Haskell program would fit into that same space. So yeah, it's phenomenal that they can get so much done with so little code.
>> Yeah, I think it's just incredible, so I do think this is a really great article and I would definitely encourage all of our listeners to check it out.

>> For sure.
>> But do you have anything else you'd like to add on it?
>> I don't think so, I just wanna reiterate that it's great to see another big company like GitHub using Haskell in production, and generally coming away with a positive experience. Obviously, they had some things that they didn't like too much, but overall, they were really positive about it.

And it seems like if they had to choose it again, they would.
>> Yeah, we'll see the octocat soon with some sweet Haskell tattoos.
>> Yeah, the purely functional octocat.
>> Yeah.
>> Well, awesome, well Taylor it's been really fun talking.
>> It's been great talking with you Cameron, thanks for being on the show with me.

>> Of course, always, any time.
>> And thank you for listening to the Haskell weekly podcast, this has been episode number 13.
>> If you liked what you heard find out more at our website haskellweekly.news. Also please take a minute to read and review us on iTunes it helps us out a lot.

I think right now Kelvin and I are the only people that have reviewed us.
>> Out there reviewed.
>> This episode like all the other ones we've done so far was actually recorded in our own office, the ITProTV studios. So I just wanna give a shout out to ITPro for recording this whole thing, whoo-hoo.

>> And at ITPro what we do is provide IT training that's both effective and entertaining. So if that sound interesting to you please go find out more at ITPro.TV. Thanks again for listening and we'll see you next week, bye.
>> Adios.
|]
