{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode17
  ( episode17
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

episode17 :: Either String Episode.Episode
episode17 =
  Episode.Episode
    <$> Articles.fromStrings
          ["https://typeclasses.com/news/2019-07-phrasebook"]
    <*> Date.fromGregorian 2019 8 6
    <*> Description.fromString
          "Sara Lichtenstein and Andres Schmois discuss quickly learning \
          \Haskell by studying annotated examples."
    <*> Seconds.fromTimestamp 13 56
    <*> Guid.fromString "df526ec2-5d4e-4c1a-b4b5-eca8b6918731"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-08-06-episode-17.mp3"
    <*> Number.fromNatural 17
    <*> Right (Bytes.fromNatural 20120892)
    <*> Title.fromString "Haskell Phrasebook"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hello and welcome to the Haskell Weekly podcast. As you might have guessed this show is about Haskell, which is a purely functional programming language. I'm your host, Sarah Lichtenstein. I'm an engineer here at ITProTV. With me Today is Andres Smoss, one of the engineers. Thanks for joining me today, Andres.

>> Thank you for having me, Sarah. I'm excited to be here. I think today we're gonna be talking about the Haskell phrasebook, which is one of the Haskell Weekly topics. And since I'm just starting out here, I thought it would be a pretty good experience to talk about.

Next to you, I'm the next newest so, we have the two most junior Haskell learners here.
>> Yep. I'm actually very excited to continue my learning experience as well as, maybe even potentially get to a point where I can start teaching things about Haskell, since it's very interesting topic for me.

>> Absolutely, and I'm really excited to hear more in depth about your learning experience. So why don't you tell me a little bit about what you did to first learn the language?
>> The first thing that I started doing was reading a book that probably everyone has heard of which is, Learn You.

Learn You a Haskell for Great Good. It's something that kind of put me a little bit uneasy about the whole learning experience with Haskell. Because while it's a great learning resource and it's freely available online, and all the great things that come with all that. There is one downside to it, and it was a little daunting at first, and that's the same thing with every language out there.

But one thing that first came to mind was, there's a lot of syntax here that I am gonna have to learn which is gonna be extremely difficult.
>> Yeah, absolutely. I think I had that same experience when I first came into shadow before I started working here. When they had Haskell up on the screens, and it was just so unlike anything I'd seen before, so it can be really scary at first.

>> Yeah, definitely. One thing that was brought up to us this week was in the, Haskell Weekly was the Haskell phrasebook. And it caught my attention because it's similar resource to Learn You a Haskell for Great Good. But it's a lot more quick and succinct, and I do say similar in that they're both beginner pieces.

Not that they match one-to-one in what they actually teach.
>> Right, absolutely. Is there any takeaways from the phrasebook that you particularly like or that you didn't like?
>> One thing I really, really loved about reading through that was how quick and simple each example was. And it starts you off slow with the hello world, just like every other resource out there.

Yeah, and, now I very much liked that it was quick, succinct. And from the last three months that I've been doing this Haskell learning experience, most of the topics that were touched upon their, I have been needing to use.
>> Absolutely. I really like that they introduced ghcid almost immediately.

Because I don't think we started using that until a little bit after Taylor got here, and it's such a useful tool. I mean, we use it everyday.
>> Yeah, definitely.
>> So in talking about the phrasebook, we can't help but highlight a couple really important sections. So one of the ones that I wanted to talk about was continual checking because they use ghcid almost immediately.

It's the second section in the phrasebook, and it's such a useful tool. And I don't think we started using it until Taylor, our lead engineer got hired here. And it's great to see it represented so early because we use this everyday.
>> Yeah, definitely, ghcid was kind of weird to me at first when I when I got started because we already had a compilation tool that was already doing everything for us.

And then, my coworkers started saying, hey, we also use ghcid if you want a much faster experience with the compilation and all these things. And I was surprised at first that the compilation side and the ghcid side was so decoupled from each other. It was really nice to see all of these errors instantly happen not having to wait for the compiler to pick up the changes and be a little slow about it.

So I very much like that ghcid was instantly brought up.
>> Yeah, the instant feedback loop that it provides is so invaluable. It's amazing.
>> Definitely.
>> Are there any sections that you particularly thought were interesting or would be useful to somebody trying to switch to Haskell from let's say, an imperative language?

>> The one thing that came to mind when I first read through this phrasebook was the fact that at the end of it, they brought up mutable variables and threading.
>> Mm-hm.
>> That's not something that you see as often in beginner guide books and under like, the Learn You a Haskell for Great Good, did not touch up on mutable variables, did not do threading.

Maybe mentioned it, I can't really remember off the top of my head, but definitely wasn't anything that stuck out with me.
>> Yeah, those are definitely more complex subject. So for it to be in a beginner's book is really interesting.
>> Yeah, and I think this is more a beginner Haskell that rather than a beginner programming phrasebook.

Now, it could be seen either way regardless because you can just pick this up and start coding in Haskell and be perfectly fine. But one thing that I have from my background of coming from an imperative language. My background comes from Java, Android, on the front end, and then I'd done some pretty extensive node frameworking, and all of that in the back end.

So, I come mostly from imperative and I've been doing it for quite a while now. So, seeing all this here is a good thing to see because if I would have started off with this I would have been like, okay. Well, I'm thinking in imperative and I'm gonna switch over to Haskell, but I'm gonna leave the functional things a little bit behind until I start getting used to the syntax and all of the other weird things that come with it.

>> Right, I definitely can understand that. What was the hardest part for you from switching from an imperative language to something so functional like Haskell?
>> The hardest part was the syntax weirdly enough. I took a very long time to realize some of the different things that Haskell does to the syntax like not needing to say which monitor your in explicitly.

So you could just use it as if it was already there. A lot of the the small syntax things really took me out of my comfort zone. And so, once I got through that, learning all of the different things Haskell does in functional and all that. It wasn't as difficult, especially because JavaScript and all of those languages could technically be used in a functional manner.

They're usually not because they have a bunch of state saving and all that that you can do. But I very much enjoyed, once I got through the syntax, I enjoyed being able to just apply all the knowledge I had into this great functional language that doesn't usually allow state saving.

>> Absolutely, that definitely makes sense. Are there any other parts of the phrasebook that you'd like to highlight or talk about here?
>> Well, you mentioned saying, coming from imperative language, what other things stood up to me. The if, then and for loops, from an imperative language, they're used pretty extensively, I assume you agree as well.

>> Yes, for the experience I had in imperative languages. We definitely did a lot of those loops.
>> Yeah, and one thing that I've learned over the past three months working on a Haskell server-side coding is that if statements and for loops have been used very sparingly. I think for loops, I have not seen one yet, which is a crazy thing to think about.

And if statements, we use very rarely because most of our coding is parsing different types, and making sure everything is typed safe, and our responses. And if statements don't really come into any of that. So it's interesting to see that once coming back after three months and reading through this phrasebook that if statements are a thing in the programming world.

I mean, obviously. But it is nice to be able to come from an imperative language and have the same syntax that we're used to. But then once you start actually working purely in a Haskell manner, you stop using the things that you're used to from imperative languages.
>> Yeah, definitely.

Hilariously enough up until I read this phrasebook, I didn't even know that Haskell had for loops.
>> Yeah. That's actually a pretty good point. I mean, you've been doing this for a year and a half. How was your first three months of your learning experience?
>> My beginning learning experience, I like to call my brain melt.

Because everyday I would come to work and I would pair with the other engineers. And I would feel like my brain was melting out of my ears because it was just so much new information, and it was just so vastly different from anything else that I've ever learned.

But as affectionately as I called it the brain melt, I like to say that it also reformed into a bigger better brain. So, it was definitely a challenge for me.
>> I agree completely there. I'm going through that brain melt now, and going through the first few weeks was difficult to say the least.

It wasn't more that I couldn't understand what was happening. It was more that trying to get your thoughts into a functional manner became a lot more difficult when at the same time I had to learn the syntax for Haskell. So, I could totally see that brain melt being a thing for Haskell learning because you're learning so many things at once.

Not unlike learning programming for the first time. So maybe.
>> Absolutely.
>> Yeah, maybe it was like that when I first started. I did start at a slightly young age, so I had a lot more time available in my hand. So, it could be that a slower pace into Haskell is much less demanding.

But you and me, we both sort of jumped in there, and had to learn it as quick as possible. And I think it's a pretty cool skill to have, to be able to just jump in and brain melt for a week or two, and just keep on learning.

So, yeah, hopefully one day, in that year-and-a-half, I'll look back and think the same thing that you're saying.
>> Absolutely, what really I think kind of helps me solidify my knowledge is being able to like work with our coworkers and some of our new newer coworkers. Like I distinctly remember pairing with you on one of your first day as an, explaining some concept.

I've been like, wow, I know stuff now. So I'm sure you're gonna have a similar experience with our new intern, which will be fun for you.
>> Yeah, definitely. I've already started seeing that happen, and it's a good feeling to be able to explain something after you very recently just learned it, so.

>> Absolutely.
>> Totally agree there.
>> Okay. Well, is there anything else, maybe about the phrasebook or about learning Haskell, any tips or tricks?
>> Well, one thing that I thought of when I first started learning Haskell was, Learn You a Haskell for Great Good is a great resource.

>> Right.
>> And when I first started reading it, it was a daunting thing. It was too much at once, and I think it kind of gave me a false sense of, this is gonna be extremely difficult and possibly not possible. So while it's a great resource, I think it shouldn't be the first thing you read.

I think the first thing you read could be this phrasebook. It's kind of hard to say at the moment, but just thinking back after these three months. I think that phrasebook is a lot more easier thing to start with than Learn You a Haskell for Great Good. Now, that's not to say that Learn You a Haskell for Great Good is not something you shouldn't read.

I think it's a great read and I think it will solidify a lot of the things that you've been slowly learning while doing test programs and things like that. So I enjoyed reading both and I will probably read them again after a month or so after getting a little more stable with my Haskell writing and reading, and certain things like that.

>> Absolutely. I definitely think that this phrasebook would be a good first introduction before the Learn You a Haskell book. Just because it's so succinct and so clear, and it does make a lot of good bridges from imperative to functional.
>> I agree.
>> Thanks for being on the show with me today, Andres.

>> Thank you for having me. I enjoyed talking about my first experiences about Haskell.
>> Absolutely, me too. And thank you for listening to the Haskell Weekly podcast. If you like what you heard, find out more at our website, haskellweekly.news. Also, please rate and view us on iTunes.

It helps a lot. Haskell Weekly is brought to you by ITProTV, the tech skills development platform for IT professionals.
>> And also, our employer.
>> Yes, that too. Send anyone who needs their IT news to www.itprotv for all of their learning needs. Thanks again for listening. We'll see you next week.
|]
