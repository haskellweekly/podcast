{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode9
  ( episode9
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

episode9 :: Either String Episode.Episode
episode9 =
  Episode.Episode
    <$> Article.fromString
          "https://medium.com/daml-driven/four-tweaks-to-improve-haskell-b1de9c87f816"
    <*> Date.fromGregorian 2019 5 6
    <*> Description.fromString
          "Jason Fry and Cameron Gera talk about four small ways to improve \
          \Haskell as a language."
    <*> Seconds.fromTimestamp 21 52
    <*> Guid.fromString "de704aad-e6a1-41a6-976f-bd3f2ef34ad2"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-05-06-episode-9.mp3"
    <*> Number.fromNatural 9
    <*> Right (Bytes.fromNatural 31507647)
    <*> Title.fromString "Improving Haskell"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Welcome to the Haskell weekly podcast. This show is about Haskell, a purely functional programming language. I'm your guest, Jason Fry. I'm an engineer here at ITPROTV. With me today is your host Cameron Gera. He is one of our software engineers at ITProTV. Thanks for joining me today, Cameron.

>> How's it going, Jason. I'm glad to be here.
>> It's going really well, thank you.
>> Yeah, well, what are we doing here? I mean I come just to hang out with you and you're like, hey, let's do a podcast. So what are we doing a podcast about?

>> Yeah, that's how that worked. We're doing a podcast about Haskell, Cameron.
>> Okay. There's a lot to talk about Haskell, I don't think we have all that time.
>> Not really.
>> We're trying to keep this to 15 minutes.
>> Haskell's pretty simple.
>> Our viewers are very busy, busy, busy people.

>> Not much to do here.
>> No, okay. Hopefully everyone will learn Haskell.
>> Well, so we were looking through Haskell Weekly, as we do most weeks, and found an article that you and I can talk about and maybe springboard off of called Four Tweaks to Improve Haskell.

>> Mm-hm, yeah.
>> So let's talk through that.
>> Yeah, it seems pretty interesting. Reading through it, looks like Digital Assets is the one who wrote this. They seem to be pretty big in that Haskell community, and also kind of in the open source world, which is kind of cool.

And they kind of seem to have some sort of idea that there's tweaks in Haskell, which every programming language has enhancements. And I think today we should kind of talk about what's good about these tweaks, maybe some of our frustrations with these tweaks. But the first one they mentioned is pretty interesting, something I'm on board with because I'm a lover of Elm.

They call it the new colon convention. Which is quite funny because as you know, type declarations for functions in Haskell are double colon. And they make the joke that Haskell would be a much faster language if we just took out that double colon, made it a single colon.

>> Yeah.
>> Like our language of Elm.
>> Yeah, you mention Elm and we love Elm. But what is annoying about Elm is that it does the opposite of Haskell here. So this convention is just saying, no, do what Elm does. So when you have the type signature, you use a single colon instead of double colon.

And then I guess cons would be two colons. And you use cons probably far less than you use the type signature, the declaration there. So yeah, they say Hackage would be one megabyte smaller.
>> Which is pretty substantial considering.
>> That feels like a lot.
>> Yeah, I mean, that's pretty cool.

We are Elm users here at ITPro, so we are very familiar with the syntax, so it would make it pretty easy for us to jump right into Haskell. Cuz I can't tell you how many times I end up typing double colon when I'm in Elm and then single colon when I'm in Haskell.

We do promiscuous pairing here and generally my pair is like, are you trying to do Haskell or Elm? What are you doing here? So it would make my life a little simpler if we'd just had one convention that went across Elm and Haskell.
>> Yes, but one thing I will say against this is that the idea with cons is that, I'm gonna mess something up here.

But you're visualizing the spine of the list with these single colon, which makes a lot of sense theoretically. But I think what Evan does with Elm, I've heard people say that he does okay breaking with convention if it makes it more sensible inside the language. And I think that's why he's done it that way with two colons being cons instead of the one colon.

So while a double colon cons and a single colon type declaration would mean less typing, less colons, I can see like why Haskell did it that way. I doubt it will ever change in Haskell, but overall I'm on board too with this change.
>> Yeah, and one other thing before we move on to the next one is, they tend to use the list constructor as the type instead of putting brackets around their type for a list, which is kind of a very Elm-based thing as well.

So I literally, when I saw this article I was like, did they write Elm or did they write Haskell? Cuz it literally looks like something I'd find in our Elm right now if I went into there.
>> Yeah, that's valid Elm.
>> Yeah, for sure. No, I think that's cool, and I'm on board with that one as well.

I think they're onto something, but like you said, I think Haskell's not gonna change yet, and that's okay. I know there's been conversations preparing for this, kinda looking through what was what. They're was definitely some issues about it and there was good conversation about it, but the bottom line was nobody else wants it, especially if they're not an Elm user.

They're used to Haskell this way, so they're not going to change, which is a totally, totally valid reason. But the next one they kind of talk about is record with syntax, which, this kind of confused me a lot. And that's probably contradicting myself cuz I'm used to Haskell, and so it looks weird because it looks more like something in Python or something you would use in one of the scripting languages, it just it seems a little wonky.

What do you think, Jason? Well, I agree with you, however, with a big caveat that the authors of this post are smarter than me, Shane Fletcher and Neil Mitchell. Neil Mitchell is probably well known for being the author of Google and Hlint and GHCID, which we use all three of those here.

So he's smarter than me. So either they didn't make a good enough case for this, or they made a great case and I'm a bit slow, or it's just not good. What I don't like about the with syntax is that it's five characters versus two. And with only has four characters in it, but there's an extra space you have to put in there.

So it's longer, and to me it doesn't enclose the record. I like the curly brackets enclosing the record. But this is one of those things that people can have holy wars over what is essentially completely unimportant. But those are my two opinions. It's longer to type and it doesn't visually enclose the record, which I've come to appreciate.

>> Right, and they have some examples of little functions that, like setter functions, that doesn't really seem to help. I can see their reasoning for it, because they have a simple function that says set this value to two. And so the function calls blah with 2 equals 2 and says, okay, set this record, its field through to 2.

But it just does not, it just doesn't feel right. I feel like I would rather say, at this point I would be able to just put that R that's there and then use record syntax and put in the attribute we're trying to update.
>> Yeah, and that's the thing they say is that its impact on readability is profound.

And I don't know if it's profound, but that little function that you say, this function is called set foo two. So with the record syntax, that means r bracket foo equals 2 bracket, right. That these, it's awkward to say those words, it's awkward to describe that. But with the with syntax, you would say set foo two r equals r with foo equals two.

That's easy to say in English, I'm just reading it off the page. It does make sense with that. You're saying, okay, this new r is gonna be the old r with foo equals 2 now. And I get how that readability is a little better. But I'm not a big fan of this, especially when you're using it when creating the data type.

I don't like the with there, and I don't know how it would look if you're updating multiple things at once. So is that r with foo equals two comma bar equals three. I'm not sure what that would look like, but I'm just not a fan.
>> Yeah, and they brought semicolons back.

I try to escape JavaScript. Like, their inline data declaration for t is like separated by colons, and I'm like, eh, semicolons, I don't really want to deal with that. But I get the readability wise, like yeah, like, it's very like, okay this with this equals this. Which is nice, but it also seems like they probably, their record extensions.

Or not their record extensions, just their overall Haskell extensions, kind of allowed this to be more useful than we're used to. We have minimal language extensions here at ITPRO. Overloaded strings, come with the basics, nothing crazy. And so they're talking about, you know, they use record puns and record wild cards and.

That may be helpful for these records if they use syntax.
>> Yeah.
>> I just don't know enough about that to say one way versus the other.
>> Yeah, and I'm gonna go ahead and assume that that is where the power of this thing lies. Because you're right, we don't use those extensions.

I'm gonna have to assume that, again, they're smarter than us, so that's why they like that.
>> Yeah, no, I would say that too. Well, yeah, there's another pretty cool one that I'm a fan of. I'm on board with that. I was very confused the first time I saw it, but they call it inline functions.

Inline function return type annotation. So pretty much what this is doing is giving a function a type declaration the same time you declare the function. So it reduces line costs, which is nice. But it allows you to say, okay, we have a function, it's gonna take a variable with colon the type.

And then colon the type that the function returns. Which seems kind of nice. Like it doesn't seem terrible, especially for like quick functions that you're using to, just one time use. Makes it pretty easy to say, hey, okay, let's put this, we know what this type needs to be, and it tells Haskell's compiler like, hey, this is what type this is, without needing necessarily its own line to say, this is what this type is.

>> And I'm certain that there are languages that actually have this syntax very similar to this, and I'm trying to remember which one or ones. I'm wanting to say PureScript, TypeScript, something like that uses this syntax. And at first, yeah, I can get on board with this and it's not bad.

But at first when I thought about this, yeah, I was confused. And then when I and grokked what was going on there, I thought, well, what if you have several parameters to this function, three or four parameters? And the parameter name, because you wanna be very clear with what this parameter is, it's something that's more than just one character.

So what I'm getting at is that you end up with this declaration that's very long, and is going to break over multiple lines anyway. So that's going to not, it's not going to help if your intent is to take the function definition and signature and roll them into one line.

But I think there's more to it than that, but I just want to point that out.
>> No, yeah, I would agree with that. It could definitely make, it could get a little hairy if you have more one or two arguments to the function. So I'm definitely on board with that.

Looked like, this was a kind of interesting fact that they had pointed out, was GHC had always parsed its signatures but would just throw an error in a later phase. Which is kind of interesting. Like, they're like, it's a good sign that they're a logical construct in Haskell.

You know, like allowing that to be something that maybe not super difficult for Haskell to implement one day. But, yeah, I'm still definitely on the fence about this one. But I definitely feel better about it today than I did about yesterday. You know, cuz I was like, hey, like what's this thing?

And me, you, and Dustin were like, I have no idea.
>> Yeah, yeah. It took us a moment to grok it, for sure. And I think that may be telling is that yesterday we weren't quite as happy with it, but today we're a little bit better with it.

So like most people, we don't like that change. We don't like change. I'm over here Googling, trying to figure out which language it is that has a similar syntax. And I thought it was PureScript, but it looks like that is not it. But I can't find it, but anyway.

Yeah, this is interesting because it lets you, it just puts together the type with the named parameter closer so that. I mean, I don't think it's much of a problem when they're separate, but it's just. I don't know. Maybe a little nicer.
>> No, I would agree. I mean, maybe it could be C++ that they're cribbing from over here.

Yeah, cuz C++ has inline functions that tell you that like, inline, and then the return type, and then the function. But I don't know if that's really. I don't know. I was trying to do some research too just to kind of to see if we could figure out what it was.

>> Yeah.
>> But, I mean, they have a reason for it.
>> It's more like, here's TypeScript. So you have value colon string padding colon any. And so that's. I think it's that, that it looks a lot like. But you're right, C++, Java, they do have a way of telling you what the type is in line.

But I meant the particular syntax of parameter, colon, type.
>> Right.
>> Parameter, colon, type.
>> Yeah, it does look a lot like TypeScript. That's true. Should've thought about that. We do have one repository here at ITPRO that is TypeScript for one of our internal applications. That I wish we understood the concept of functional programming when we wrote that, cuz at this point it looks like a JavaScript.

>> It's just JavaScript. Everything is any, you know?
>> Yeah, yeah. Every time I open it up in VS Code, VS Code will say hey, I have something to help you with this TypeScript file. And I'm confused, because I'm like, this is a JS file and I don't see any TypeScript syntax in here.

>> Right, yeah.
>> So there's like one thing in there or something that tells that it's a TypeScript and otherwise just JavaScript.
>> Yeah, pretty much. Which you know, that is what it is. We live and learn, but yeah. So obviously that's three of the four we've talked about here today.

And the final one is, most of these are all stylistic. Obviously, there's no real functional changes here. But the last one is the module qualified syntax. So-
>> I like it.
>> I do too, to an extent. And this is probably my just lack of knowledge is that I just wish it could be like Elm, where you could just import blah, and then you could use it as blah dot whatever, instead of you importing blah, and it just imports everything that blah exports.

Which I understand Elm's is different than Haskell. But, I do kind of like that rather than like always exposing everything that a function has, you know, Elm says, you have to tell me what you want to expose if you're going to expose everything. Or you can access it through the qualified name, which is kind of, I prefer that when I go back and read it.

But this is an improvement. I do agree it helps with, like the tab issues. You know, where, you know, you've got one important statement and then you've got a non-imported statement, and they just look wonky. Especially if you use like, not Brittany, indent, indent looks really wonky. Which it's like, it looks clean, but at the same time, you're like, this could just be better.

So their proposed solution was just instead of import qualified M, you would use import M qualified. Which reads better too, because like, okay I'm importing M as qualified rather than like, import qualified M. Like, it doesn't necessarily read well. And it seems a lot like they're very much on board with just kind of making Haskell easier to read for the human, which isn't bad.

I think it's a good, I think it's a noble cause. But yeah, I would be on board with this one as well if they could just do like, not export everything and just implicitly have to expose whatever you wanna use if you wanna expose everything in a module.

>> Yeah, and I'm more okay with Haskell's way of doing things than you are. I know it's not a big deal for either one of us. But the reason I really like this Is I mean, most of these come down to the way we use the language versus somebody else.

So the way we use Haskell is we import everything qualified. So Data.Aeson, for instance, when working with JSON stuff, we import qualified Data.Aeson as Aeson. And then we use it, Aeson.ToJSON or whatever. But there are some weird infix operators in there that always look really ugly whenever you qualify them.

So you have Aeson dot, what is, dot colon, or colon equals or whatever, that just looks really weird. So we will explicitly import the infix operators that we're going to use. So we have import qualified Data.Aeson as Aeson, and then import Data.Aeson parens, and then we then we list all the infix operators.

So no big deal, but in addition, we sort our import list alphabetically. When you have it that way, the import qualified comes differently in the order than import Data.Aeson infix operators. So what I like about this is that you have import and then the library name and then other stuff.

So you have import Data.Aeson qualified as a Aeson. That will come right next to import Data.Aeson infix operators. So that's why I like it, because right now I have to sort the import list, I have to sort them automatically. And then manually re-sort them to get those end fix operator imports in the right spot.

And there's some question of, and this is silly, but software engineers, we're silly bunch. Do you do the qualified Data.Aeson first or second? And where are the infix operators? I just don't want to have to think about these things. I just want to you know select this list, hit colon sort on them, and boom it's done.

That's what I want. And this would make that possible. But yeah, it's also more readable, it's closer to English, at least I should say. There's a lot of people writing Haskell that don't speak English very much or at all. So I don't know what it's like for them in their language but, certainly better for English.

>> I would agree, yeah. There's definitely a few perks in this document, especially in this bullet point of the imports. One being readability, the other being sorting, which is a little more minor, that's not necessarily all that that big. But I do really appreciate these two authors writing this article, I think it was good food for thought.

It really allowed us to kind of understand what maybe some of our opinions and solidify some of our opinions about Haskell and why we liked Haskell. And challenge some of our opinions. I mean like okay, why do you like that? I think that was really important as we an engineering team are getting more and more familiar and comfortable with Haskell.

That this allowed us to really form an opinion, and have a little pride about how like what we liked about Haskell. You know what I mean?
>> Yeah, yeah, yeah, I like this article. And again, the authors Shane Fletcher and Neil Mitchell are smarter than I, or-
>> Agreed.

>> Or Cam, I can speak for Cam on this one.
>> Ouch, but I agree, I do agree.
>> So yeah, I said before, that their opinions are righter than mine, generally speaking. So anytime I differ with these folks, I want to give that caveat that they're certainly smarter than me.

But, like I said, it comes down to, I think, largely how do you use the language? And we just use it differently than they do because they have different extensions on or just or whatever. And I think that's the main difference.
>> Right, and I think they're trying to, like most Haskell shops around town, you're trying to find someone to come in and learn Haskell.

You wanna remove as many barriers as possible. And so I think they were trying to kind of bring Haskell more to a point of a TypeScript. And cleaning up some of the clutter that can kind of trip up people in Haskell. Obviously, it just takes a couple of times to fully understand and grok what's going on.

But if you can remove those barriers before they happen, that's good. So I definitely do applaud them for that, and I do appreciate them taking the time to express their feelings on this subject. Especially cuz they both have different backgrounds. Shane very much C++, OCaml kind of style.

So I'm sure curious what maybe what OCaml influenced on this article and on this. This kind of sounds like this is their style of Haskell for digital asset. They kind of have that internal format kinda that they write Haskell in. So I think this is probably more of an internal thing that they decided to share externally, which is greatly appreciated.

I would say I learned something from this.
>> Yeah and that's a good point. Shane's experiences in C++, which i know a little bit about, and OCaml, which I know nothing about. And Neil Mitchell, I'm sure he has a lot of experience in a lot of things. But certainly Haskell is the main one from this description, it seems like.

So yeah, that's a good point. Maybe OCaml, there's some similarities here with OCaml stuff or something.
>> Yeah, no, Neil Mitchell, he seems like he's a really cool guy. He's got a PhD, definitely smarter than I am.
>> Yeah, we use HLint and ghcid all the time in Google.

>> Yeah, so obviously, the guy knows what he's doing. Well, Jason, I just wanna thank you for being on the show today with me.
>> It's my pleasure.
>> And thank you guys for listening to Haskell Weekly Podcast, this has been episode nine. If you liked our show, find out more at our website, haskellweekly.news.

Thanks again for listening, I've been your host, Cameron Gera. We'll see you again next week, bye.
|]
