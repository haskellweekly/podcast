{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode12
  ( episode12
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

episode12 :: Either String Episode.Episode
episode12 =
  Episode.Episode
    <$> Article.fromString "https://www.tweag.io/posts/2019-05-27-ormolu.html"
    <*> Date.fromGregorian 2019 6 3
    <*> Description.fromString
          "Dustin Segers and Cody Goodman talk about formatting Haskell \
          \source code with automated tools like Ormolu."
    <*> Seconds.fromTimestamp 16 37
    <*> Guid.fromString "f166f89f-1a16-49f1-915a-d54505c301a0"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-06-03-episode-12.mp3"
    <*> Number.fromNatural 12
    <*> Right (Bytes.fromNatural 23912963)
    <*> Title.fromString "Formatting code"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Welcome to the Haskell weekly podcast, this show is about Haskell, a purely functional programming language. I'm your guest, Justin Seger, I'm an engineer at ITProTV. With me today is your host, Cody Goodman, he is a senior engineer here at ITProTV, thanks for joining me today.
>> No problem, Justin, I'm glad to be here and talk about some Haskell.


>> So today I think we are going to talk about Ormolu? Looks like a format, Haskell code like never before stylizing dealio. At least that's the tagline at the top.
>> Right, yeah, I actually Googled that, I was like, how do you pronounce that? It's apparently Ormolu.
>> Nice.


>> An 18th century guilding technique for applying finely ground, high-carat gold-mercury amalgam.
>> Wow,
>> That's a mouthful.
>> Yep, yeah, I had no idea, so I figured I'd probably butcher the name, but thanks for letting me know. Yeah, so the author, Mark Karpov, excuse me if I mispronounced the last name, has got a pretty good train of thought here.

Did you happen to see anything interesting, Cody, as you read over this article?
>> Yeah, I think kind of building on the name Ormolu, they kinda start out talking about the principals, which is something you'll find common with high schoolers. And I think that's not just because of some of the kind of academic roots, but also because Haskell's type system is more amenable to allowing you to kind of, at a high-level state with the principles of your program should be, and limit the space of what makes sense.


>> All right, so any of these particular principles that, I guess, Ormolu is laying out for us, any of them catch your eye in particular?
>> Yeah, I like they how they kinda start out by motivating what it's for, what exactly is a code formatter, what does it do?

Do we want there to be multiple styles? Do we want there to be a single style? I'm very happy they ended up on a single style, cuz as a programmer I just want a formatting tool to kind of get out of my way
>> Yes, I can can agree with that.


>> Now there are some cases where it, it does something that looks a little insane and can affect readability. I see they also address that though, with a rule of, if you make a new line, it will line based on that new one. Now, that allows for some differences and it kinda goes against the uniformity, so I'm a little worried about the implementation of that, but I could see it being a positive.


>> Yeah,I also see he mentions Hindent and Brittany, both of which we have used here at ITProTV, Hindent I think the lesser of the two.
>> No, I wasn't here when you guys were using Hindent, how was that?
>> Well, at the time we were using Hindent I was just a little Haskell novice, which I'm still not much more than that now, but.

I would say it was nice coming from things like ESLint and stuff like that, it was just nice to have something, help us format our code. And we were in dire need of it, especially whenever we were just starting out learning Haskell, and we didn't really know Ralph the bad, how I should format certain things.

So it was a good kinda entry into formatting Haskell
>> You know, that's a good point. I didn't really think about how that could lower the barrier of entry for Haskell, something to automatically format code.
>> Mm-hm, yeah, but I do see that they also mention Brittany as well, which we still use.

I'm enjoying Brittany, although I'm probably the least opinionated whenever it comes to stylizing stuff. Mainly because I just like it to look good, and if it looks decent enough, I'm okay with it. But I know that some people are extremely particular whenever it comes to formatting. Which of those camps do you fall into?

Are you?
>> I used to be a little more opinionated, things like align everything based on comma, well, I guess I still do that. I think your opinion sums it up pretty good for me. Just as long as it looks all right, as long as it's not inscrutable, I'm okay with it.

Because I used to only use EMAx alone, and they had this, God. The Haskell mode for that had three different tab cycling modes, one that was intelligent, one that just cycled through them. I wasted so much time with that, where I could have been writing code.
>> And that's actually a good a segue into another thing that Mike is talking about here with the, how could formatters also good to take away some of the tedium of writing code in the first place.

And if your formatter's getting in the way of that, or actually doing the opposite of what you're saying, then yeah, it's probably time to either pick up something new or maybe the tool just isn't doing its job correctly.
>> Yeah, I totally agree with taking away the tedium, it's not just because we're lazy, but also because we don't wanna get knocked out of that flow state.

We wanna just be able to focus on the problem at hand.
>> Correct.
>> And have all those things, ideally have imports taking care of automatically, specially since we use qualified imports, sort them if you wish. That would be great.
>> All right, well, looking at an example of Ormolu's approach.

How do you feel about the formatted code that is displayed here?
>> Yeah, their example, I think is pretty good, it seems like a pretty sane decision, they're not doing anything too complicated, just going by line, with the looks like and. I am concerned about the possible exponential blowups they talk about when formatting deeply nested expressions.

On the one hand, that shouldn't happen too often, but it would be pretty annoying for your formatter to freeze your editor, if you're unlucky enough to have a synchronous editor like EMAx.
>> Yes. So one of the things I'm noticing is that, it does look like, it looks very similar to how our code is formatted right now using Brittany, and I like that.

I mean, it's nothing too crazy, I see that thing later on, or maybe a little bit before, I may have missed it. I thought I read that Ormolu is supposed to allow you to have a little bit of an opinion, but still retain a consistent style throughout everything.

Yes, and actually I see that now, it's a little bit below the formatted code example here, but. But yes, I like how it's nothing too different and too insane from what I'm used to myself. So, I'm kinda biased in that regard, that I already like what I see because it's similar to what I use everyday.

How do you feel about that? Feel about the same way?
>> Yeah, I mean, the first saying when I saw this announcement was, why another formatter? Why isn't Brittany good enough? I think that the simplicity of implementation looks like one reason. Maybe they think long term it would be easier to use this architecture and maintain it, and then also maybe that sort of ease overriding, things by new line instead of having config files.


>> Yeah.
>> And I think it's just trying to get closer to something like GOFormat where, not the biggest fan of GO, but one great thing from GO, and is never having to worry about formatting, and then always being able to read things in the same format.
>> Yes, that's definitely very handy.

Continuing on, other features he says are worth mentioning are, formatting already formatted code is a nola.
>> Yeah, I think if that wasn't the case, then that sort of defeats the whole purpose of having a formatter, it wouldn't be consistent, so we gotta have that.
>> Says, project aims to implement one true formatting style.

I think we just touched on this a little bit.
>> Right, basically, just avoiding the need for a configuration, lowering that barrier of entry so that everyone can pick it up and use it.
>> Yep, everybody is using, this styler will be on the same page, which should be very nice.

Then it looks like they're aiming to reduce the diff sizes, which is.
>> Right.
>> Which is super awesome to us, because we have just single commits just dedicated to running Brittany and formatting things accordingly, and then pushing those up and then seeing those in Git later. The diffs are just quite large, and they can get a little confusing, especially if you're new and you don't know exactly what to look for in the beginning.


>> Yeah, I agree with that. Maybe we'll have to do a test and take all of our old diffs of Brittany, something and compare it to Ormolu.
>> I actually like that, yeah, I think that'd be pretty cool.
>> So they use the THC coarser like Brittany does, which means that they don't have to worry about incompatibilities.

I believe it's type THC dev tools that provides the alternative AST that's easier to use than the GHTAPI. As nice as that is to use, you've gotta worry about supporting all these language extensions, and it's more of a headache than it's worth.
>> Looks like the code of the actual formatter itself is written, so it's easy to modify and maintain.


>> That'll be really important. They go on to mention that only parts of the GHC AST, abstract syntax tree for anyone that doesn't know it, is implemented, but there's a lot left. So having something that's easy to extend with, when you're trying to have as many different behaviors defined as you'll need for the GHC AST is really important.


>> Yeah and I see, final thing is, they're saying that the codebase is hacking friendly, so that's pretty cool too.
>> Yeah, if we can get the community involved in something, then they're gonna be more likely to use it. If everyone can kinda standardize on one of these, that'll be great.

I'm personally undecided, we've been using Brittany, and it's been working out pretty well for us.
>> Right.
>> Let me see. We had a few things missing, I think quasi-quotes aren't formatted, typeclass instances were not formatted, I think that got added recently though.
>> I believe you're correct, if not recently, then sometime, yeah.


>> So it'll be interesting to see if Ormolu has some sort of challenges implementing those things, or if the development will kinda play out the same.
>> Looks like they have a testing scheme in place, I mean, it's less bugs, which is always good.
>> Mm-hm, I guess the pressing question for me is, is this going to let us remove our Brittany config file for instance?


>> Yeah.
>> That, I guess the greatest, when in complexity, is being able to remove that config file, but still be able to preserve a same style. Of course as they say, right now this is vaporware, so we can't see if that's true yet.
>> Yeah, no, I agree, and especially only having seen the config file recently for Brittany.

You know, if you were trying to use this in the workplace that you, let's say you just got hired for a Haskell job and they use the Brittany config file, and you just happen to be hired as a noob. It'd be pretty nice to not have to worry about that down the road, say if you're given some project or something that may have required you to tinker with it.

Which I think that tinkering is great for learning and stuff, but maybe not in that point in time. Something's needed, right, if it's pressing, there's urgency to the matter or something.
>> Funnily enough, my first job title was actually noob.
>> Not really, I just really wanted to make that joke.


>> Yeah, and it looks like they're saying that the project is open through the fork. Actively maintains, those are all pluses. Twig is maintaining it, yeah.
>> And that gives some confidence too, cuz they came out with quite a few really good code bases, and Haskell community like in line JS, Lori, if anyone's familiar with Nick's OS.

And I believe they also have Cassandra bondings off somewhere. So some good code basis coming out there.
>> Yeah, that's actually pretty good to hear. I'm already all for it, but like he said, and like this article said, it's vaporware, but looks pretty cool and seems exciting. If it delivers on the features and principles, and stuff like that, then it could very well be a replacement for us here, although we're perfectly happy with Brittany at the moment.

It's really, you know.
>> I guess one of the, every once in a while with Brittany we'll think a line is too long or something and, man, I wish a format like this with that. So if they can give us the best of both worlds, I think we could probably switch, but only time will tell.


>> Thanks for being on the show with me today, Cody, and thank you for listening to the Haskell weekly podcast. This has been Episode 12. If you liked our show, find out more at our website Haskellweekly.news. Thanks again for listening. I've been your guest, Justin Seger.
>> I've been your host, Cody Goodman.


>> And we'll see you again next week.
|]
