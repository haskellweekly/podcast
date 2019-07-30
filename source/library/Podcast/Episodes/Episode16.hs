{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode16
  ( episode16
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

episode16 :: Either String Episode.Episode
episode16 =
  Episode.Episode
    <$> Articles.fromStrings
          ["https://treszkai.github.io/2019/07/13/haskell-eval"]
    <*> Date.fromGregorian 2019 7 30
    <*> Description.fromString
          "Dustin Segers and Cody Goodman talk about developing video games \
          \using Haskell."
    <*> Seconds.fromTimestamp 12 54
    <*> Guid.fromString "0bbb483d-d256-4311-b2bc-2a98a0c0eaad"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-07-30-episode-16.mp3"
    <*> Number.fromNatural 16
    <*> Right (Bytes.fromNatural 18561769)
    <*> Title.fromString "Game Development"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hello and welcome to the Haskell weekly podcast as you might have guessed this show is about Haskell, which is a purely functional programming language. I'm your host Dustin Segers and I'm an engineer here at IT Pro TV.
>> Hi, I'm Cody Goodman. I'm a senior engineer here at IT Pro TV.

What are we talking about today Dustin?
>> We're talking about game development and Haskell. I thought it would be cool to kind of bring together my love of game development and video games in general and Haskell, which is what I use daily here at IT Pro TV. I actually have a few questions for you and I figured it would be cool to kind of just look into the Haskell Game Dev community and space and see what all was out there.

>> Yeah. I think a lot of people are in your position, you know, if you browse the Haskell subreddit, you'll see occasionally questions about people want to make games and people frustrated trying to make games. So if you don't take the right path, it can be pretty frustrating.

One recommendation I've seen over the years and tried a little myself is to start out with a 2D library called gloss.
>> Gotcha. Have you ever heard of it?
>> Yeah actually are our lead engineer. I think he did a few just very basic games using gloss. I heard it was super intuitive and quick to get something up on the screen.

>> Right, right. Yeah. That's that's one of the reasons I recommend it and I think a lot of other people recommend it is you can you can draw window and draw a circle and a single line and it's all and you know plain English words that you might be able to guess yourself.

>> Nice.
>> You know gloss is really aimed at simplicity. There's a ton of tutorials available, not just like basic tutorials, but tutorials of you know, how do you make pong?
>> Yeah.
>> Right. It's more of an end to end thing which has Haskells sorely lacking in other areas, but when you're just getting started, it's a new paradigm, a new language.

You you need you know a little bit of hand-holding. You just get all the way from point A to point B at least exactly least once.
>> That's it. So gloss seems really attractive to me in that aspect mainly because I'm not super proficient at Haskell. I've only been doing it for a little while, but I do enjoy it and I think I have more experience in game development mainly as a hobby right versus you know programming in Haskell.

So I would like to merge both of them and gloss seems like a pretty good pretty good way to do that.
>> Yeah, and for people trying to merge their game development experience or interest in their love Haskell, you'll probably run into a functional reactive programming which you have few are really in proficient in a school.

It can be a bit of a mind Bender wrap your head around that.
>> Gotcha, yeah. I actually just doing a little bit of research before this podcast. We stumbled upon FRP functional reactive programming and then also just your classic mean Iterating over your main game Loop. And do you have any like, I guess a concise way, if you could explain functional reactive programming to those who don't know?

>> I didn't come up with the best answer myself, but I looked around and hope I'd say the name right here T cange Elvis as they talk on answering that that question, you know, you have to have a whole talk to answer it yet. He says if I had to summarize it in one sentence, I would say that functional reactive programming is programming with time varying values.

Now, that's not the most satisfying answer person one who might already be overwhelmed.
>> Yeah. Yeah, but basically that's for lack of anything better to use at this moment. It's state right like so I represent State.
>> Yeah. It's a lot like state. The so it's still encodes the information that state would encode the difference would be that the way that these things composed together or different we won't get too deep into that.

And that's why the reasons we recommend gloss to start out with. Yeah, so you can just you know gloss over FRP I guess.
>> Yeah, see what you did there. Okay, cool. So do we have any like games that may have used FRP and examples of that or up top your head?

Or any packages that might implement or use this?
>> Yeah recent a recent really cool game with an interesting premise that we might recognize the roots of is people mon.
>> Yes. I did see that believe is by Alex Stewart like wrecked.
>> Right, right. And so the premise is that you you catch people and battle with them and style of Pokémon.

>> I got you. That's pretty funny. Yeah, I did. I got a chance to at least watch the little trailer for it and go to the site but I have not yet played it I plan to play it here. It's pretty short, but looks pretty entertaining at the very least.

>> And I recommend at least check out their homepage is some pretty hilarious images on got you. Let's see. So people mine. Was it based on it was yamp, I think okay, which is an FRP Library, you'll notice that the more complex examples of Haskell games use FRP.

I think we had an exception which used a classic game Loop, which we'll talk about in a second, but just to answer why these libraries might prefer FRP as as complexity skills up. FRP seems to have some advantages over the classic game Loop much in the way that Haskell has advantages over imperative languages.

>> Yeah, that actually reminds me Cody. We will be linking the package Yabba as well as people Mon and other things mentioned in this podcast on our Haskell weekly podcast page. So if you're, you know interested in, you know finding these not really having to Google them even though you totally can we'll have those links available at the Haskell weekly podcast page what the mention of people Mon and then also Pokemon, which is a mainstream game.

I found out just a little while ago that there was actually a Haskell game that was greenlit on Steam now, they've retired the whole green light thing on Steam, but so this is a little bit older, but it was still pretty cool to see that I think it was even back in 2012.

Right people were using Haskell to create games and they actually, you know, we're getting somewhere with it.
>> Trail Blazers for sure. That was Nikki in the robots. And I remember watching on excitedly in their development blog and tracking their progress trying to get everything building myself is exciting and then to see that they got on Steam Greenlight.

It was like wow. So in 2012, it's possible to write a Haskell game if you put in enough effort and get it greenlit on Steam.
>> It's actually really cool. Yeah and notice that I went to their page and I did a little a little snooping around and I saw their Tech stack consisted of hipmunk, which is the physics engine, right?

Yeah. It's a Haskell take on chipmunk. Hmm, which is pretty cool. It looks like the package is now deprecated. So it might be a little old, but it was still nice and cool to see that, you know the community is out there and they're doing things with it.

>> You know, I rewatch that Nikki and the robots video after after looking around for some examples and it just made me think you know, what would it take to revive Nikki in the robots and make it something compiling something that people for instance in this podcast you heard about this could go and download the source code and try it out.

How much work is it to update that there was an older FPS game made in Haskell. I can't recall the name of it. But somebody did the work to like bring it current. Well, it's really cool.
>> Nice.
>> Maybe somebody maybe Me Maybe you or someone hearing this can try to revive Nikki in the robot.

>> Right? And it's also just it would be good to I guess have something else to do to merge these I guess these hobbies and this love of Haskell together and which would help progress. You know, my Gillan has school as well. So I guess that's like another right other bonus and doing this on the topic of that.

>> On the topic of that, there's also Dino Rush, you know if you're wanting to merge your hobbies with Haskell and game development, but you're kind of new, you might want to use like a classic game Loop and Dino Rush actually went that route and they used a classic game Loop.

So highly recommend anyone who wants to do that to check out their blog posts that I think you're probably going to put the resources to yes download the source code get it building poke around in there.
>> Nice. Yeah, I'm definitely going to do that actually went to the page earlier and checked it out and it's pretty cool.

I like the pixel art for the game. It looks pretty.
>> Right and in for context their game Diner Rush, it's you know, when you lose internet connection on Google Chrome. Yes, there's a little diner. Jumping over stuff.
>> I think we have a high score board somewhere around in the office.

Yep.
>> Yep. I remember that day.
>> Those examples are pretty inspiring Cody, but if I get lost, how could I get support?
>> Right, you know, we're talking about Nikki in the robots and their Trail Blazers there wasn't there weren't really a lot of people trying to do have school Game Dev back then so they had to figure out all these things yourself, but then at some point past that they started a for someone started a subreddit called are / Haskell Game Dev and they made a Wiki of how to get started.

Now they're getting started I think is more fitted to complex games and I would still recommend using gloss to get started since it'll won't get you up and running the fastest but some really good resources there. They also have a Haskell Game Dev Channel on freenode if you're familiar with IRC chat and you can usually get pretty quick help in the Haskell chat room.

I'm guessing the Haskell Game Dev is similar, then there's always stack Overflow. I've noticed quite a few Game Dev questions and ask will get answered on stack Overflow.
>> Gotcha. Nice. Yeah, I'll definitely give those a shot it just to clarify somebody coming from let's say unity3d developing their there's nothing like that currently for the high school Community right like is basically C sharp for the most part in right Unity, right?

>> Yeah. Nothing is complete is Unity. There is a game engine that I think attempted to be something like Unity called Helm that you might want to take a look at I think it's still compiles latest GHC and is is working couldn't find any good game examples of Though got you.

>> Okay, cool. Thanks for being on the show with me today Cody.
>> No problem. It's been a lot of fun and thank you for listening to the Haskell weekly podcast.
>> And thank you for listening to the Haskell Weekly Podcast. If you liked what you heard find out more at our website Haskell Weekly News.

Also, please rate and review us on iTunes. It helps a lot. Ask a weekly is brought to you by it pro TV the tech skills development platform for it professionals and also our employer. Yeah that too. It's a senior sis admins and network admins to www.1800fairoffer.com needs. Thanks again for listening.

We'll see you again next week.
|]
