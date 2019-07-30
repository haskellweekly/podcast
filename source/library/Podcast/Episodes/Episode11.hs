{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode11
  ( episode11
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

episode11 :: Either String Episode.Episode
episode11 =
  Episode.Episode
    <$> Articles.fromStrings ["https://blog.jez.io/profiling-in-haskell/"]
    <*> Date.fromGregorian 2019 5 27
    <*> Description.fromString
          "Sara Lichtenstein and Taylor Fausak talk about improving the \
          \performance of Haskell programs by profiling them."
    <*> Seconds.fromTimestamp 19 12
    <*> Guid.fromString "3ec1dc79-7a9c-46c3-b919-61471e876708"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-05-27-episode-11.mp3"
    <*> Number.fromNatural 11
    <*> Right (Bytes.fromNatural 27690623)
    <*> Title.fromString "Profiling Performance"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hello and welcome to the Haskell weekly podcast. This is a podcast about Haskell a purely functional programming language. I'm your host Taylor Fausak, I'm the lead engineer here at ITProTV. And with me today is Sara Lichtenstein, one of the engineers on my team. Thanks for joining me, Sara.

>> Thanks for having me, Taylor.
>> It's good to have you here and today we're gonna be talking about profiling in Haskell. And in particular, this article by Jake Zimmerman, where he talks about speeding up one of his programs by ten times. So pretty pretty significant improvement.
>> Yeah definitely, I think anything ten times improvement is pretty great.

>> Yeah, its hard to argue with that, can't beat ten times better. So Jake explains the problem he was trying to solve in this article and we will kinda recap for our listeners here. He was at a carnival and playing this carnival game where he had a six by six grid of shapes or symbols and you turned over some pieces and tried to see if you got a bingo like if you lined up a whole column or a row or a diagonal or something like that.

And he was interested in finding out if or rather what their probability was of winning this carnivore of a game. So how good should he feel when he wins? Is it like you win 50% of the time or 1% of the time? And he figured that he could write some type of combinatorics solution to get the exact answer like you're gonna win exactly 123 out of 767 times.

But he thought it would be more fun to write one that just generated these boards and shuffled all the pieces together and then solve at one or not.
>> If a task takes more than five minutes, why not program it?
>> Exactly, yeah. Why sit there all day making bingo boards if you could write a Haskell program to do that for you.

So he wrote this program and it seemed to work. He didn't really talk too much about if he got the implementation right or not. So we're assuming he got the implementation right, but his first time around was way too slow for him. And his threshold for too slow is apparently very different than mine because his first attempt took the order of like one second.

And this I think was to generate and solve like 100,000 boards. So for me that seems fast enough for this problem, but I definitely understand wanting to really dig in to something and find out how fast can it be and he definitely went on that red hole. So he talks about some of the changes he made to his attempts and the first one was to use like a single large integer as the whole game board.

Because with, I think yeah with 36 slots on the board, you could represent that in one large integer rather than having some data structure with a bunch of different fields in it.
>> Right.
>> And his assumption was that if you can use one integer then it will probably be faster cuz you're not gonna have as many things floating around in memory.

But it's also I think, somewhat of a strange choice. So Sara for the pro or for the programs that we right here at work, we don't do a lot of this really cramming, getting the most performance possible out of stuff. So I was wondering, did this look weird to you or you're like okay, yeah, I kind of get what's going on here.

>> The idea in general just using one inter just seems very odd
>> Yeah
>> To represent this whole thing. But he did make a point about how simple this was to implement for him.So I guess you know if it's easy to implement and it works why not.

>> That's a great point. Yeah, why make things more complicated just in looking for Purity or something like that.
>> Exactly.
>> But yeah, he said he was able to pretty quickly knock out this Fisher Yates Shuffle to generate the random boards and. This used a random number generator, the one kind of from the standard library in Haskell for doing this stuff which is of course just called random.

And so he kind of-
>> What else was he called?
>> Why I come up with a more imaginative name, random works fine. But as we'll discover, there's kind of a problem with that. And what he did was he passed in this random number generator and did one step of the shuffle and gave back the slightly shuffled board and they are like next random number generators.

So just keeps getting passed along to successive calls. Which is of interesting the way they do things and makes sense for Haskell because it's pure. So you have to have something that's sort of implements that randomness, but it's still kind of strange even to me after programming it for a while to see that explicitly passed in as an argument.

So then he gets into kind of the performance characteristics, and this is where he talks about how slow it is. And again, it's crazy to me that's under a second, and he's like it's just too slow.
>> Yeah, I found that very hilarious, because I started reading it and he was like, yeah 738 milliseconds and Im like, what in the world?

How is that slow?
>> Yeah, I would think, okay we're done here you know?
>> Yeah, absolutely.
>> Fast enough, but I am guessing based on later in this article he talks about porting some C code over to Haskell, maybe he is a C programmer or RASP programmer or something like where he has kind of different definition of what it means to be performant and fast enough.

>> That would make sense.
>> Yeah, I'm just guessing, I don't know. Hopefully Jake can can tell us or we could probably read and find out who knows.
>> So then it gets into really the meat of what we're doing here. And I guess Sara, do you wanna kinda explain the process that he goes through to profile this code and figure out what's going on with it.

>> So profiling and Haskell is actually extremely simple because you can literally just run stack build dash dash profile to build it with that and then add a dash p to exec And that's it, it prints you you out this like really nice profile, tells you everything you need to know.

It's got all your time allocations, all that good stuff. So it's super useful. It doesn't really take any overhead to implement or anything like that. So, definitely great tool.
>> Yeah, it is surprisingly easy to use, I don't know what to think there if it's stack, doing a bunch of lifting for you.

Or somewhere deeper in the stack like COBOL or GHC or something else. But yeah, you just throw this dash dash profile option onto your build and then pass another option to the program when you run it. And boom, you get this output that tells you yeah, your program spent, in this guy's case 70, no more than that 85% of your time, coming up with random integer values, which is crazy,

>> Right.
>> And it's it's also really useful to look at this in terms of percentage time because already I've lost track of the fact that we're only taking a second. I see 85% and that's way too high, we gotta make that number lower.
>> So yeah, I mentioned earlier that the de facto standard random number library in Haskel being called random is a bit of a problem because it has, it's really slow.

Which is what this guy figures out in his relatively small program where he's generating all these boards and checking to see if they're valid. None of that business logic is the slow part, it's the random number generation.
>> Right, which seems crazy.
>> Yeah.
>> You always think that the logic would take more time.

>> Right.
>> Which is kind of what he explains is that his like assumption is, it's got to be the logic. And then once he starts using profiling, he's like, it's not the logic at all.
>> Yeah which is one of the huge upsides of profiling is you write your program and don't care about what part is fast.

And then if it's too slow, which isn't even a given, sometimes you can write just atrociously, it's really slow looking code, but then it runs fast enough to because your inputs not big enough or whatever. And as soon as you run into a performance problem throw the profiler out and you might be surprised at what you find.

>> Exactly.
>> Cuz I wouldn't have guessed at the beginning of this article that the random number generation would be the slow part. I can flip coins pretty fast, I can roll dice pretty fast, that's not gonna be the slow part.
>> Yeah, exactly and I think as programmers we might be a little predisposed to think, my logic must be wrong, rather than the library being the slow thing.

>> Right, because so often, especially in the type of code that we do, the library is almost never at fault. And the language-
>> Exactly.
>> Is never at fault. It's always us, but that's more so for bugs than performance. And we rarely have reason to look at performance.

Generally, things are fast enough for us with Haskell. But when you do, it can be surprising to find, because I know as a library author and this probably applies to many library authors, I don't run benchmarks against my library code. I just kind of assume that it's fast enough and if I'm using it for a particular problem, I'll definitely make sure it's fast enough for that.

But I may not have envisioned usage like this if I wrote this library in the first place.
>> Right.
>> So the fact that it can be used in all these different contexts and that authors aren't typically focused on that means that yeah library code can be the slow part and profiling is the thing to tell you that as fast as possible.

So then he gets into talking about how he after he identified this random number generation as the slow part of his program, how he fixed it. And we don't wanna get too far off into the weeds of that, but it's impressive, to me at least, that he imported this C program into Haskell.

It's been a long time since I've had reason to look at C code and I think that's true for you too right Sara?
>> Yes, absolutely. Not since I think maybe junior year of college.
>> Yeah, so it's been a while. I don't think I'd do as well as him, but it looks like it paid off.

I mean, he speed his program up by like six times by ditching the standard library and using his own.
>> Right, which is already a very impressive speed up.
>> And then he goes on to ask, I think a very important question when you're looking at performance improvements.

Because well, what he asks is what did we have to give up in order to get this performance improvement? And I feel like that's an important question because sometimes when you're focusing on sort of a sub problem, in this case performance, you can lose sight of the bigger picture.

And so you can make something that's really, really fast, but then it's a huge pain in the butt to use.
>> Right and that would kinda just defeat the purpose of having it at all. If it's not simple to use then why use it?
>> Right, yeah, ideally, we could keep the same interface and swap out the internals and make it faster.

That's the best case scenario. So I don't think he quite achieved that here. But it is surprising how little he had to change. If you look at the, obviously our listeners can't actually look at the code, but the original code that he wrote with the slow one and the new code that he wrote with the fast one.

And they're almost the same, like if you squint they look the same.
>> Definitely.
>> Which I think is a great metric to have in mind when you're profiling code in Haskell or really in any language is keep the call site looking almost the same and try to update the internals without break in the API.

>> Right, if one of our listeners wanted to look at this code, is this article found in Haskell Weekly?
>> It is, as per usual. It will also be in the show notes for this episode, we'll have a link to it.
>> Perfect.
>> And so moving on, this guy, Jake, he again runs into this problem that I would not run into of saying that 126 milliseconds, the new run time is just too slow.

He can't abide by that,
>> 126 how slow?
>> I mean, if it was 126 seconds, maybe I'd agree but milliseconds,
>> Yeah.
>> I'm gonna hit the return key and it'll be done almost instantaneously. But yeah, he talks about how he went on to improve it and again, the actual mechanics of what he did to speed it up aren't super interesting.

It's that he continued to use profiling to identify the hot spots in his program. So instead of saying, well, I've swapped out the random number generator so I can stop worrying about that part and move on to something else that I think is the problem. He ran the profiler again and discovered, no, the random number generation is still the slow part.

Even though it's been sped up so much, it still takes a big chunk of the time. Which again, is really surprising, you mentioned before about how libraries aren't often the problem with bugs, but can be with performance. And with bugs, when you fix them, they're gone.
>> Right.

>> But with performance, it can't really be fixed. It can just get faster, or I guess it could get slower if you did something bad.
>> But, isn't it interesting that you can spend a bunch of effort, he wrote a whole new random number generator and-
>> And it's still slow.

>> And it's still the slow part,
>> Right.
>> So yeah, that to me is just the huge positive benefit that profiling has versus kind of staring at code and hoping you can identify which part will probably be slow. As it will tell you, no, even though you've already put a ton of work into that it's still the slow part.

>> Yeah, absolutely, I mean, I definitely think it's helpful to have that kind of information. Especially if you don't have to do a ton of set-up or anything. It's such a useful tool. There's no reason really not to use it, but I think it's also good to keep in mind like we had brought up the kinda of exchange that goes on with making something more performing versus what you're using for the program.

>> Right, you don't wanna loose track of that bigger picture.
>> Exactly.
>> Yeah, and you mentioned that it's really easy to do this which it is, and it's still interesting though cuz even here at work, I don't think we've ever profiled our code base. Actually you said we did that once actually when I wasn't here, right?

>> Right, when the boss is away the mice will play but-
>> Boss isn't here quick profile of the code.
>> Cody and I were working on a problem and the code base is just like a bug that we had found. And he suggested using profiling and I'd never used that before, I was like what kind of profile are you trying to make here.

What does that even mean. And so he kind of explained it to me and we tried to use it and while we didn't use it for very long, it was still a very interesting experience to have something tell us what was going on.
>> Yeah, and I think you mentioned that you didn't end up using this profiling report to actually solve the problem.

So something else popped up, but the profiling at the very least told you that there wasn't one thing that's taking in this case like 85% of the time.
>> Right
>> It was a bunch of little things. Which can be a lot less satisfying. If you run the profiler and it says, well, you spent at most 5% of your time in any given part of your program.

You might have to do some harder thinking to figure out how to speed that up.
>> Right, we were hoping for more of a straightforward answer, but I guess it's also good to know that nothing we're using is overtly wrong.
>> Yeah, that's very encouraging. That's a good way to look at it.

>> Yeah.
>> So yeah, that kinda covers this blog post talking about profiling Haskell programs to speed them up in maybe the best case by ten times but in the, typical case, probably quite a bit. Is there anything else you wanted to mention about profiling in Haskell?
>> I think we pretty much covered it honestly.

>> Yeah, I think the only thing I had to add was that I profiled some programs. In my high school career, none of them have been as great a success story as this. So it's not like, throw profiling at your problem and it'll magically get ten times faster.

But right as we discussed, it can still be validating to see that there isn't one huge hot spot where it's slowing everything else down and you weren't even thinking about it.
>> Right?
>> And I just feel the need to reiterate again, this is such a good way to think about performance problems and to me that means don't think about them at all until it becomes a problem.

And then when they do, throw it to at it that tells you exactly where the problem is rather than trying to guess.
>> I think that's the best way to honestly look at performance problems because you should mostly optimize just making the code as the best as it can be, and then after that if it becomes an issue-

>> Yeah.
>> To try and solve it.
>> Yeah, and we touched on this already, but if you're writing the code without an itoid performance, and you were focusing on making a nice expressive API or something that's easy to use. Then when you have the need to make it more performant, and you maintain that API, you're meeting both goals.

Versus if you right out of the gate start jumping through some hoops just to right code that you think will be faster, you might end up with a much worse API for almost no benefit.
>> Right?
>> So I agree. Write the nice API and then if you needed to be faster, just go ahead and make it faster.

>> Then maybe not like less than a second is too short.
>> Yeah, that's still where we disagree. Maybe if this guys was at Google Scale or writing some web service that needed to turn these requests around real fast, people really wanted to solve this carnival game, I could see it.

But it's definitely a good practice and that's something I've noticed in my personal programming with Haskell is that, you get the ability to sort of look into things that may not be worth your while to do at your day job, but are still fun and interesting. And then put another tool in your toolbox, that you can use later on.

>> Yeah, definitely.
>> So I bet, you mentioned Cody doing or using profiling for a problem here at work. And I bet that's not the first time he used it. So he was hacking away at something at home, and found that profiling was a neat tool, and figured out how to use it and then boom, applied it at worked.

>> Cowboy Cody, I would guess that wasn't the first time you used it,
>> Good old Cowboy Cody. Yeah, and I guess the final thing I wanted to mention here about profiling in Haskell performance is that it's very possible to write Haskell that is really, really fast. This guy ended up with a program that simulated 100,000 board states and checked them to see if they were winners or losers, and did it in 70 milliseconds?

>> Right.
>> And when you look at the performance of other languages that are as expressive as Haskell, it's hard to find one that can get to be that quick. So the fact that Haskell gives us the best of both worlds when we need it is awesome, I love that about it.

>> Is there anything you don't love about Haskell though?
>> Good question, no, Haskell is the perfect language. All right, well, thanks for joining me today Sara to talk about profiling and Haskell. It's nice to have you on the show again.
>> Absolutely, it was nice to be here.

>> And thank you for listening to the Haskell Weekly podcast. If you liked what you heard here today please go find out more at haskellweekly.news. This has been episode 11, and please be sure to join in next week, see you.
|]
