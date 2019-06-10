{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode6
  ( episode6
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

episode6 :: Either String Episode.Episode
episode6 =
  Episode.Episode
    <$> Article.fromString
          "https://functor.tokyo/blog/2019-04-07-ghcid-for-web-app-dev"
    <*> Date.fromGregorian 2019 4 15
    <*> Description.fromString
          "Jason Fry and Taylor Fausak talk about getting fast feedback when \
          \developing Haskell by using ghcid."
    <*> Seconds.fromTimestamp 18 38
    <*> Guid.fromString "7ed15199-bcd3-461e-af62-d504ae8a4a01"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-15-episode-6.mp3"
    <*> Number.fromNatural 6
    <*> Right (Bytes.fromNatural 26845627)
    <*> Title.fromString "Fast feedback"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Welcome to the Haskell Weekly Podcast. This show is about Haskell, a purely functional programming language. I'm your guest, Jason Fry, I'm a software engineer here at ITProTV. With me as always today is your host, Taylor Fausak, he's the lead engineer here at ITProTV. Thank you for joining me today, Taylor.


>> Hey Jason, thanks for hosting me.
>> So what are we getting into today?
>> Today we're gonna be getting into a post in the most recent issue of Haskell Weekly that talks about using GHCID for web development. And using it to have a quick feedback cycle so that you're not waiting for the compiler all day long.


>> Yeah, yeah we use GHCID here.
>> We do.
>> It's very speedy. I would love to talk a little more about this. Cool so where are we starting, what are we doing?
>> So we'll start kind of at the dark ages, I guess.
>> Ooh.
>> Far away from GHCID to talk about how bad it is and then as we kind of improve things, we'll get closer and closer to GHCID and the beauty that it gives you.


>> Okay. So when do the dark ages start?
>> A few years ago?
>> Time-wise I'm not sure but tool wise they start with just GHC the compiler itself. This blog post talks about web development. And if you're producing a website with Haskell, you may run GHC which will produce an executable for you and then you'll run the executable to make your website.

That's in comparison to something like Ruby where it's just a script and running the script produces a website, there's no intermediate compilation step.
>> Okay, so that intermediate compilation step that makes the process for web development a lot slower cuz you have to re-render it in your browser and all that.


>> Right, you have to compile it and then either generate your files or start a new web server or whatever, and then go look in your browser for the changes, pretty slow.
>> Okay, yeah, that is pretty slow. So what is the next step from that? What happens next?


>> The next step and this is the one that we were most familiar with here at ITPro in recent memory is using some type of file watcher to do that thing for you, in our case we were using Stack. There are many tools that do this Stack is the most convenient for us because it does a lot of other things.

But Stack has an option dash dash file watch to say when any of the files in your project change go ahead and rebuild the whole thing, so that handles the compilation side of things. It also has an extra option called exec that says once the build has finished do this other thing.

So you can say watch all the files, rebuild it when any of them change and then once that build has succeeded, rerun the site generator or the server or whatever it is.
>> Okay, I actually use that Stack Build whenever I'm writing a script to do something to interact with the database to fix some customer data or something.

So I always use Stack Build fast, why would I ever not, I'm not sure? And by-
>> And to be clear, fast means build it quickly, not the thing you're producing will be fast. In fact, the thing that you're producing will be really slow because what fast does is turn off optimizations.


>> It says, I don't actually need the code you're generating to be fast I just wanna know if it type checks or whatever.
>> Okay, I always was curious about that. I knew that it was compiling it faster but, okay, that makes sense. It turns off the optimization so it really is more geared towards fast feedback cycle, writing the code but not production stuff?


>> Yeah.
>> Okay, that makes sense.
>> That's exactly right.
>> So we'll do the fast flag, as well as the file watch flag but it doesn't actually pick up on my changes. I still have to go over to the terminal and press Enter to redo that. Is that always happened like that?


>> Not always. I think that's a side effect of our local development environment where we use Docker and we're running Stack inside of Docker on Mac OS. And there's a bug in how Mac OS implements these file notifications and Stack doesn't pick up on the change even though it should probably should.


>> That's right so really quite separate from GHCID-
>> Yes.
>> And Stack and all that?
>> I think I know where you're going with that though, because with GHCID it does pick up the changes.
>> Mm-hm.
>> And there's no secret sauce difference between those two things.

It's just that GHCID is using a newer version of some library behind the scenes that works around this bug and the version of Stack that we're on and in fact, I think the next version to be released will have that same version but the one we're on doesn't have it.


>> Ooh to be released, do we have an insight as when that will be Taylor?
>> No, no idea the Stack release process is a mystery to me.
>> Yes, I'm sure it is to everyone. So that's where we are with GHC, Stack, file watch but and you can use exec to run something arbitrary, like start your server again or something.

But that's still kind of a slow feedback loop, especially compared to JavaScript, as I commented last week.
>> Yeah.
>> It's so fast, that it doesn't have to compile anything.
>> It's just so quick. So what comes next?
>> Well, it's funny you mention not compiling anything, because that's really the only way to speed things up here.

One of the slowest parts of the Stack file watch feedback loop is that it has to link your executable and rebuild it and get all these things lined up that just take a while. There's another way of doing this feedback loop before we get to GHCID which is using GHCI directly.

And instead of rerunning GHC and then rerunning your executable you can have a ripple prompt up with your whole project loaded. And then when you make a change, you go over to the ripple and you type, colon reload or just colon r and then hit Enter and it'll rebuild everything.

So it has a manual step involved, but since it's in GHCI, it's going to be a lot faster. The I in GHCI stands for interactive or interpreted, and that means that it's not exactly compiling things behind the scenes and it also isn't doing any optimizations. So you're getting that quick compilation speed that the dash dash fast flag gives you and you're not wasting any time actually producing an executable and linking it and all that stuff.


>> So, I have so many questions-
>> But we're gonna get into some nuts and bolts of that and so I will punt on that for another time. So yeah, we actually began to flirt with that workflow, I don't know, a year ago or something. It was just too much of a burden.


>> To type colon r.
>> To press, yeah colon r, who's got the time?
>> Who's got the time? And, I'm maybe a particularly lazy developer so I want my feedback loop, I mean, to be in the past if I can get it just-
>> Faster than instant.

So I kept complaining and kept complaining.
>> And heard about this GHCID thing and so what happens next in the iteration of this stuff?
>> So the last hopefully or maybe not last the iteration we're on right now is GHCID. And the way I like to think of GHCID is it takes that workflow I just described of having GHCI open it and pressing colon r whenever something changes, and automates it.

And-
>> Brilliant.
>> Yeah, brilliant.
>> Million dollar idea. At its core, that's basically what it's doing. It's using a file watcher like the Stack Build did but instead of creating an executable, it's running everything in GHCI. And it'll pick up your change, it'll reload everything, and then it'll show you the errors if there were any, and if there weren't, it'll run whatever you tell it to run.


>> Mm-hm. It's really cool, we really enjoy it here. There's not my understanding of the editor integrations that are available they don't really show you the errors in the code, it's still in a side panel or something.
>> Mm-hm.
>> Are there maybe editors I'm not aware of that are doing this better or is that just the limitation of GHCID or?


>> There are probably editors that do it better, I am not aware of any or I haven't explored this enough. For me I feel comfortable enough and productive enough having basically my editor up and the terminal up at the same time and just kind of looking between them.

I definitely would prefer, a red squiggly underline in the editor, but it hasn't been worth it yet for me to figure out how to get that lined up. Yeah, I know that you have some experience setting that up with Haskell IDE engine, right?
>> Yeah, I was just gonna say I've tried to make that work, because that kind of is the ideal situation where you have nearly instant feedback as you're typing almost it's that quick.

But the problem is there's some memory leak in the GHC version that it uses or something and so after about a half hour or an hour or two your entire computer crashes.
>> It's not always but it's bad.
>> It doesn't sound good.
>> No it's not good.

So, can't really use HIE that well, one of our coworkers has a script that will just kill HIE every time it hits-
>> 2 gigs of memory usage or more-
>> Problem solved.
>> Memory leak solved. But it noticeably slows down the machine, noticeably slows down the editor.

I'm running Vim with a very lightweight vimrc, and that was still crashing at least once a day because of HIE, I commented out that, and only that, no crashes in the last week.
>> Hard to get more lightweight than that set up.
>> Right. So HIE has great promise, I can't wait for it to get stable in that sense, but like you, I've just kind of gotten used to having GHCID up, I have my editor up, I save, I see the issues, I go fix them, hopefully I go fix them.


>> For some reason that feels awkward but I've been doing it for enough months now that I'm kind of used to it.
>> Right.
>> It's not what I want.
>> You're desensitized to it?
>> Yeah, I'm desensitized to it. I want the feedback loop to be as quick as possible.

We have all our stuff containerized in the Docker stuff we use Mac OS so it's got that issue with file watch doesn't do it properly. But even if it did the feedback loop was still-
>> It's still slow.
>> Multiple minutes-
>> Yeah.
>> At one point before you started rejiggering things.


>> Yeah, I think we're much better these days but it's still a lot slower to use the Stack Build than the GHCl.
>> Yeah and at this point GHCID, it's really quick, it's pretty easy to set up like containerizing it wasn't very difficult or just running it on the metal is very straightforward.

So we've only been using GHCID for six months-
>> Something like that.
>> Eight months maybe.
>> It hasn't been long.
>> Why having we been doing it from the get go?
>> That's a great question. I don't know why we didn't use it from the outset. I think that I ran into some problems getting it set up.

They weren't insurmountable, obviously, we're using it now. But the way our project was structured, we had one kind of top level project that had lots of smaller packages in it. And it was hard to get GHCID to run all of those packages simultaneously. And part of that is our fault because we had multiple or within each package we had module names that were repeated.

So package a had module x and package b also had module x and they defined different things. And with GHCID it has to be able to pull everything into scope at once. So it says, well, you had module x over here and module x over there I don't know which one you want I'm gonna crash.


>> I remember this yes. So we had to restructure our app essentially, our code base to use GHCID.
>> More or less yeah.
>> So don't do it that way.
>> Start with GHCID.
>> Yeah start with GHCID and it'll go a lot faster. So, what are some of the neat features of GHCID?


>> I'm glad you asked, I was gonna loop back to bring up something about HIE. You said that you want the feedback cycle to be as short as possible and with HIE I think that's as good as it can get.
>> Yeah.
>> It's integrated tightly with the editor and it gives you very quick feedback about the code you're writing.

However often you want feedback beyond just syntax errors or type errors or stuff like that. You wanna run your test suite, or you wanna launch a server and poke around in your browser, or you wanna run an executable against a test file you have locally. You can't exactly do that with HIE.


>> Right.
>> Because its whole purpose is to integrate with your editor and your editor hopefully isn't doing those things.
>> I don't know yours might be with VScode.
>> I knew that was gonna be a shot at VScode-
>> I knew it.
>> Should've been a shot of Emacs.


>> Yeah, there you go.
>> I can run VScode in Emacs.
>> There you go. Probably the other way around too.
>> Yeah.
>> I bring that up because one of the nice things about GHCID is that you can run arbitrary things after your code builds. And since it is essentially just a GHCI ripple, you can type whatever you could type into the ripple into your command line options for GHCID and have them run.

So in our case, we often run like do our build and then run our web server, or run our test suite. And you'll get your actual test suite output right there as fast as possible once your build has completed.
>> Yeah that's really nice for us because at this point you have set it up so that it compiles and then if it compiles it runs the test suite.


>> Mm-hm.
>> And it's really nice because sometimes you don't want to run the whole test suite when you know you're in the middle of something. You don't really need it to run, it's just sort of running silently in the background, you have the information you need it compiles or not.


>> Mm-hm.
>> That's what you need right now.
>> Yeah.
>> And then you make sure you run the test suite multiple times throughout you while you're developing but just not every single save, you don't need that.
>> Mm-hm.
>> So by having it silently run in the background at the end I find that so valuable.

I don't have to worry about it, if I don't care about it in the moment, it's really helpful.
>> Another nice thing that GHCID does, in relation to test suites, is that if your test suite is running, and then you make another change in your editor and save it, it'll stop that run of your test suite, and rebuild it, and rerun it.

So you don't have to sit there, like you do with the Stack Build, and wait for the tests to succeed or fail in order to start another build for the change you just made.
>> Yeah, that´s nice. So, you mentioned you can run some arbitrary something with the exec command.

Is that what it is, you use exec command?
>> Or I think in GHC idea it's called test.
>> Yes.
>> Which is a little confusing-
>> Yes.
>> Because you can run non-tests.
>> Yes, it's a test flag but you can target whatever you want.
>> Yeah.


>> Which when you showed me this a little while ago, confused me as it should.
>> But it was really helpful because I was writing a script and I had it set up so that when I ran it it gave me meaningful output, and then I just needed to change the innards to change the output.

So you said well you can you use the test flag, target this script and every single time you save it will run that and show you the output instead of, saving, seeing if it compiles, and then manually running the script.
>> Right, how I made it.
>> My gosh, I just saved five seconds.

Thank you so much.
>> But really, it was really a little bit, I mean, really cool for me because it makes the feedback loop just that much faster.
>> Mm-hm.
>> And once I finally realized the test flag meant whatever. Yeah, you can use a target whatever.
>> Yeah, it's confusing because it says test and you can use it for test and you probably should but really what it means is run this after the build succeeds.


>> Right, so that is really helpful. So there's a lot of things that we've done to speed up our feedback loop going from GHC, flirted with GHCI a little bit then GHCID, we got it containerized, we're trying to make HIE work for us. I think we got that containerized at one point or we nearly did test suite running, restructured our app to make GHCID work better for us.

But I would say it's all worth it because at this point, our workflow as long as we're using GHCID is very quick, very easy.
>> Mm-hm.
>> We don't have to worry about the issue in Mac OS Docker where it doesn't actually pick up the file changes correctly, we don't have to worry about Stack.

As fast as it is, I mean really, it is not slow but really when you think of what you'll get.
>> It's on the order of a couple seconds which can kind of kill your momentum if you're looking for that fast feedback.
>> Yes, yes and that's something you were saying earlier about how GHCID is so fast, you just throw away save, you don't really care, right?


>> Mm-hm.
>> So how does that affect your development cycle or how ever you do it?
>> The way that I used to develop with Stack Build would be I would try to make the code as perfect as I knew how, and then hit Save, and have the compiler check me and then if I messed something up I'd go fix it.

And with GHCID I feel like my workflow has changed to where I write something that I know is gonna fail, and I get GHCID to tell me exactly what's wrong with it, and I make these real small incremental steps.
>> Yeah.
>> One bit at a time, all the way through.


>> Yeah, not literally one B-Y-T-E.
>> Not writing one character. When I can get into that flow, and go at that clip, where I'm writing one line, saving, one line saving, that is so much faster for me.
>> Agreed.
>> That's because GHCID, I mean, it feels like it's a less than a second for it to recompile.

And we have like 600-
>> Yeah our app is not small.
>> It's not tiny, it's probably not huge, I don't know what huge is in this regards, but it feels large.
>> Mm-hm.
>> And it takes minutes for the Docker container to come up, build everything, do everything.

Once you have that up and going, running GHCID in that container, it's so quick.
>> It's a game-changer.
>> Yeah, it really is, it really has improved my personal dev practice. I think the whole team, I mean, I know the whole team likes it, but I don't know how much has revolutionized what they do.


>> But I know it's really improved mine, and everybody's very happy with it.
>> Thanks for being on the show with me today, Jason.
>> My pleasure, thank you.
>> And thank you for listening to the Haskell Weekly Podcast. This has been episode six. If you liked our show, find out more at our website, haskellweekly.news.

Thanks again for listening. I've been your host, Taylor Fausak. We'll see you again next week.
>> Bye everybody!
|]
