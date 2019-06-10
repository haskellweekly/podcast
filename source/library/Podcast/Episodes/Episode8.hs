{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode8
  ( episode8
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

episode8 :: Either String Episode.Episode
episode8 =
  Episode.Episode
    <$> Article.fromString
          "https://medium.com/co-star-engineering/continuous-improvement-with-hlint-code-smells-e490886558a1"
    <*> Date.fromGregorian 2019 4 29
    <*> Description.fromString
          "Cameron Gera and Cody Goodman talk about enforcing best practices \
          \with HLint and refactoring."
    <*> Seconds.fromTimestamp 14 20
    <*> Guid.fromString "53bbcaeb-6e6f-4e1f-9806-f24032ac7a9f"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-29-episode-8.mp3"
    <*> Number.fromNatural 8
    <*> Right (Bytes.fromNatural 20714874)
    <*> Title.fromString "Best practices"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Welcome to the Haskell Weekly podcast. This show is about Haskell, a purely functional programming language. I'm your guest, Cameron Gera, and I'm an engineer here at ITProTV. And with me today is your host, Cody Goodman. He's one of our senior engineers here at ITProTV. That just means he's really really old, right Cody?

Yep, I got the gray hairs to prove it. Programming can do that to you though.
>> Well, I'm glad to have you on the show. I wanted to take some time today and kind of talk about refactoring Haskell. I stumbled upon article in Haskell Weekly this week, that was about continuous improvement with hlint code smells.

So we use hlint here at ITProTV to kind of enhance and kind of unify our code. What are some things that you have experienced with hlint? And maybe some of the, I think right now, we're just gonna talk about hlint and then maybe dive more into refactoring a little bit later.

How's that sound?
>> Yeah, sounds good. So at ITPro, we've been using hlint ever since I've been here. Something that is kinda annoying about hlint is it'll bug you about things like, use parentheses here instead of dollar signs. And that may conflict with your personal style or what you think looks more aesthetically pleasing, but it also goes and it does more complex recommendations too.

Like you can do an eta reduce here, you can use point-free here. It doesn't go to the extent of using composition, with composition, where you have three dots next to each other. But it provides sort of some common sense recommendations. It leads you to start refactoring code in a uniform way.

And that's something really valuable across a code base.
>> Yeah, I know. I feel like I have really enjoyed having hlint. Sometimes it was like, yeah, I was supposed to take away those parens that I accidentally left. Those are some of the smaller things it can do, but I also know we use map maybe and and it's like there's a cat.

Maybe there's a function that does the same thing or I don't know. It's a cat maybe. Something like that that does a very similar thing so it just says, hey, use this function instead of this function. And it kinda cleans up and minimizes the code, which is nice to have that going into each file, and having some sense of pattern, and kind of just cleanliness around the code base, right?

>> Yeah, you could have your Haskell looking like lists, but you have hlint, then you have some assurance that it's gonna be a lot more clear, and you can focus on what the code's doing, instead of all that noise.
>> Right, and I think it just kind of gets you back to coding efficiently, which is really nice.

So okay, so we've got hlint, we use hlint. We've kind of experienced hlint, and this article kind of talks about four differently code smells that hlint has helped them with. One being along functions, another one being functions with many arguments. We have long type list and then we have lots of imports.

So something that, those things seem to kind of maybe clean up and minimize this expansive language that Haskell can be. Kind of minimizes it a little bit to make maybe beginners and intermediate people kind of be able to come in and understand what's going on. Have you experienced any of that?

I don't think we've really come across any of those four in our code base that I've seen.
>> So a lot of imports, especially when you're just trying to hack something together, you can end up with that. So you leave a debug.trace or something. Hlint's gonna catch that and you can remove it.

And then, like you said, for newcomers who are newer to Haskell, they might, if you're not using qualified imports, they'll be saying, where's this function to find that? If you have a ton of imports you're not using, it's going to be really hard for them to track it down.

>> Yeah, I agree with you, Cody. I think it does help intermediate, only being in Haskell myself for a little over a year now, hlint really just kind of helped me shape my styles when I'm writing Haskell. I know you've written Haskell a lot longer than I have.

So you probably have a little more frustrations with hlint, because it's like, I want you to do this when you really prefer to do it maybe another way. But I think overall, it helps all of us, any of the engineers here at ITProTV come into our code base and understand, this is what we do, this is the style we have, this is set across all our files and we know what's what.

And I think that's really, really valuable.
>> Right, and I think something actually useful when you're learning Haskell is to use hlint, follow all the suggestions and not only follow them but ask why. Why is it useful to transform this like that, because there's a lot of experienced developers or at least one who came up with these different rewrite rules.

And they were there for a reason and they were born out of the experience that developer had.
>> Hm, yeah, which I think is really cool. One thing that I kind of wish hlint did, which is probably just expecting too much, is kind of being able to take a function and kind of maybe analyze the way in which it's being performed, taking, okay what are the inputs, what's the output?

Okay, let's see how they wrote the code to make this work. I wish it would be able to maybe look at it and say, hey, you should probably break this up or you should kind of shift things around because it's too conglomerate. There's too many-
>> Too complex.

>> Too complex, right. Sorry, conglomerate doesn't really make sense there. But yeah, too complex.
>> Those functions are starting a business.
>> Jeez. But say we have anonymous functions, instead of anonymous functions like being, hey, let's pull this out and those kind of things may be nice. Because I know this week, you and I both had come across some code in our code base that was, it worked but it was very thick.

It's hard to fully comprehend.
>> Right, and it actually wasn't even that it was that large either. It was kind of the, I think the fact that in one place we took advantage of partial application, where you have to read from right to left to understand it, and then we had a lambda inside of there which is left to right.

I think that was sort of the thing that made it hard to follow. And then when you add or reduce that, you get the advantage of it all being right to left typically, and that's really valuable. Yeah, so if hlint could have maybe identified that, we had conflicting styles, right.

And it could have been, hey, let's reevaluate this. If you want to stick to point free, okay, stick to point free, but don't be jumping all these things in here. Don't mix partial application with lambdas, that doesn't make for easy readability. So I think, for us, that would kinda be maybe a nice thing to have.

But nice thing is Haskell makes re-factoring pretty easy. Cuz the problem we had was kind of complex. We had a document that had a list of, in our case, it was users. And we wanted to kind of create a map that had one of those users tied to another value on that document.

And so, say we had ten users, we wanted to have a map with ten entries. And then we were actually doing that over a list of those documents. So it was kind of just a lot going on and so we have some helper functions that we used to kind of group things, but that didn't really fall into, this category wasn't there.

And so to begin with, I just was like, hey let's, I was working with Jason. We're like, hey, let's just figure out how to make this work. We know what the type signature looks like. Let's just let the compiler happen, but obviously, we put that out for PR.

We're kind of like, this seems rough, there was code smells, right? There could be something better here. So I wish, I mean, that would have been kinda cool, if hlint could have done something like that for us, say, hey, reevaluate.
>> Yeah, and I think it would definitely be possible, although difficult, for hlint to say, your code is from right to left because of usage of composition here.

Then in this lambda, it is left to right. I think that's something that could be added. Then you were kind of hinting at complexity. And there is an existing calculation to figure that out, which is called cyclomatic complexity. And that's pretty interesting.
>> Does hlint have that?
>> I don't think so, but it'd be interesting to start an issue about that and have a discussion.

>> Yeah, it really would. Maybe we'll see if that could be put into place.
>> Sure.
>> No, but I think that is really awesome. Don, you were gonna say something.
>> Yeah, yeah, I was gonna say that our specific problem, what helped make sense of it to me, is that we had that set of users and then we could easily turn that into a map.

But from that map, we needed to, the map's values had another list of users and we needed to basically expand the map out. And that was what kind of made it difficult. And you might reach for a fold in that situation but something that's often times easier is making a lot of maps and unioning them together, which is where we ended up.

>> Good old union with this. Also the map had, the key was a user ID, but the value within that was a list. And so unioning that with semi group allowed us to get exactly the result we were expecting. And it cleaned it up because, and we felt like when Jason and I wrote it we felt like, there could be a better way to do this.

We knew that there was some sort of, just kind of something wrong. We felt like we could have, we changed things from set to a list because we deal with a set. And then we felt like we could have left that as a set and done the operation that we needed to, and which was what we ended up getting to, which was really wrong.

But in this case hlint didn't necessarily save the day. But in a lot of other cases it has, I'm very thankful for hlint.
>> I think you're getting at a sort of unique selling proposition of Haskell, which is that you can not only sort of write the code that just works quickly.

The compiler will help you do it, and then once you write it, even if you're not proud of it, you know that when you wanna go refactor it, you have a lot of guarantees. It's way easier to actually hack the code, get it out there real quick, and then have the confidence you can come back later without breaking things and improve upon it.

>> No, and I really value that aspect of Haskell. I think it's, for us coming from a JavaScript background, most of us, we kind of dealt with, don't touch that code, you don't know what the side effects will be.
>> It's a ball of marbles, it's a ball of marbles.

But being able to say, okay, we're in a compiled language that's strongly typed. That value is just tremendous, because any time you need to come back and re-factor, you have confidence. I think that's something that everyone should get on board with, and join and love Haskell, because it really gives you that confidence and that peace.

I remember, a year, a year and a half ago, when I was writing Java Script, and then I'd go reopen some old controller or model or action that I wanted to mess with. And I would just have this sense of stress, of I'm touching something. Is this gonna work?

Is our test gonna catch this? Cuz that's all you have in Javascript, you don't necessarily, in user testing. But that's all you got. Haskell, we have strongly typed languages which makes that kind of refactor nice and then we have tooling like hlint and Brittany. Things that kind of keep us within lines, within guards.

Whereas like JavaScript, it's the wild, wild West. You have, eslint and stuff like that, but I think, hlint is just leaps and bounds above eslint or any other. Well, in my opinion, I could be wrong, and I'm sure somebody on the internet may want to tell me I'm wrong.

And that's okay, but in my opinion, hlint, for me in Haskell is great.
>> It leads you to, hlint, those different rewrites that it does, how you can glean understanding of why that does happen. Just the act of refactoring something can lead you to understand the nature of the problem you're working on as a whole.

That's an economic factor of deciding to rewrite something that's a code smell that I think a lot of people don't take into account.
>> Yeah, that's fair. Well, Cody, I think we've had quite the show here, quite the talk on hlint and even refactoring a little bit in Haskell and others.

Could be even more to talk about, but we wanna respect our listener's time and sign off. So I'll shoot it over to you.
>> All right, thanks, Cameron. Thanks for being on the show with me today. Thanks for listening to the Haskell Weekly podcast. This has been episode eight.

If you like our show, find out more at our website, HaskellWeekly.news. Thanks again for listening. I've been your host, Cody Goodman. We'll see you again next week.
>> Adios.
|]
