{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode7
  ( episode7
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

episode7 :: Either String Episode.Episode
episode7 =
  Episode.Episode
    <$> Article.fromString
          "https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html"
    <*> Date.fromGregorian 2019 4 22
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about how regular expressions \
          \compare to parser combinators in Haskell."
    <*> Seconds.fromTimestamp 17 29
    <*> Guid.fromString "287a197e-e9fd-47b6-9506-2f39be002af7"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-22-episode-7.mp3"
    <*> Number.fromNatural 7
    <*> Right (Bytes.fromNatural 25296111)
    <*> Title.fromString "Parser Combinators"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Welcome to the Haskell Weekly podcast. This show is about Haskell, a purely functional programming language. I'm your guest Cameron Gera. I'm an engineer here at ITProTV. And with me today is your host Taylor Fausak. He's the lead engineer here at ITPro. Thanks for joining me, Taylor.
>> Thanks for having me, Cam.

It's good to be back.
>> How you doing? Yeah, right. I always loved being in here. It's always fun. They have this really scary picture behind where Taylor sits. It's quite intimidating.
>> Too bad we can't pick it up on audio. Yeah, I can't remember I don't. What's its name again?

>> I don't know. I thought you would know.
>> I don't know. Well, any who we're here to talk about Haskell, not that guy in the picture.
>> Which we'll get to that later, maybe. But today I had some questions about parsing in Haskell. I know Haskell's really powerful for that.

I was reading an article that a guy just kinda put out about kind of a regex cheat sheet. So I know regex is pretty popular for parsing, and there's a lot of power behind it, but it can get confusing and hazy. This guy was nice enough to make a cheat sheet for it, which is really cool.

But what good is regex compared to maybe some of the other stuff in Haskell? I've heard some things like parsing combinators and stuff like that. What's the difference?
>> So the world for parsing in Haskell is vast. We could fill way more than 15 minutes talking about it.

However, you mentioned regular expressions and parser combinators, which are two things I think we can focus in on. And Haskell has these really powerful utilities for parsing and that makes it an excellent language for prototyping other languages in. A lot of languages have their first implementation written in Haskell or a similar language like OCaml, or something like that.

Before they transfer over to a quote unquote real implementation written in C or C++ or Rust. And the reason for that is that there are so many tools in Haskell for writing powerful parsers that turn text into something useful that you can then do something with. Not everybody implements programming languages.

In fact, very few people do. So why is parsing so useful? Why is Haskell so good at it? One answer to that is that parsing comes up everywhere. We were talking over lunch about parsing. And we started rattling off all the places in our application that use it, and it's basically everywhere.

An HTTP call uses parsing, dealing with JSON uses parsing, talking to the database uses parsing. Parsing some bespoke game replay file format, that's parsing too. It's everywhere, you can't escape it.
>> Yeah, and we don't even necessarily see it, because of kind of the things that Haskell has already built and all these packages that are already built that handle all this parsing behind the scenes that are just done.

But my curiosity is kind of, what's under that? Like, obviously they've built their parsing with something. What did they build it with?
>> That's a good question. Just to back up for one second. Parsing, it is everywhere and it is hidden from us. And that's not just in Haskell, that's in JavaScript too, in Ruby, C, every language, there'll be a parser under the covers and they will expose some interface that is a lot nicer to work with.

So like with JSON,
>> Hopefully, it's an appropriate interface, just saying.
>> Yeah.
>> Under the sheets, you never know what could be under there, it could be bad. But we're gonna move on.
>> We are, moving right along. For something like JSON, it'd expose an interface where you'd deal with objects, and arrays, and numbers, and strings rather than parsed tokens like a curly bracket and a comma and a quote.

So Haskell's really good at providing those things, but it's not the only language that can do that.
>> Hm, and so-
>> And I'm sorry, I forgot your question.
>> It's okay, that was a little more intense than. My question was a little more intense than we wanted to go.

But I'd like to touch on the JSON, or the JavaScript side a little bit, being more background in JavaScript, becoming more 50/50 now Haskell or JavaScript as far as knowledge is concerned. In JavaScript, we use regex a lot to parse, and match, and see if something's what we expect it to be.

>> Yeah, like on a credit card form.
>> Right, and like validation, all that kind of stuff. Kind of make sure it's all numbers or whatever. And we tend to just use regex, cuz the library's pretty easy to use. It's very well resourced, there's a lot of stuff out there for regex, they're everywhere.

>> It confuses the crap out of me still, and I have to reference it from time to time. But it is still, there's a lot out there and I know Haskell, we can use regex in Haskell for parsing, and it can do that. But it's probably a little bit better at just kinda the simple stuff, right.

Whereas maybe something like, like a programming language there's a little more intensive, right. There's a little more parsing involved and a little bit more robustness, I would say that we need to have, besides just what regex provides.
>> Yeah, when you look at the spectrum of things that you can parse, on the one hand you have something really simple, like a credit card number, which, let's say for the sake of the argument, is 16 digits.

That's pretty easy to write a regular expression for. Most people would probably understand what that means. But if you wanted to write a regular expression that parses, say, a Python program, that's not gonna work. You're not gonna be able to do it or if you can, it's gonna be some giant monstrosity that nobody wants to look at.

>> Hey, but regex is strong enough, it can do it, okay?
>> If it sets its mind to it, it might be able to. You were talking about, maybe, more powerful abstractions we could use to parse something that's a little richer than a credit card number.
>> Right.

>> And in Haskell, the thing that we reach for is parser combinators. It's not the only thing that'll do this, but it's the one that I think we're most interested in talking about today.
>> That's the one I heard recently and I would like to talk more about for sure.

Cuz it's definitely pretty interesting. The idea of a combinator in my mind sounds like something that we would be combining these little functions or something together.
>> Exactly that kind of do this action and say, hey, I'll take this kind of file or this kind of text, and I can give you out this kind of result.

>> Mm-hm, that's a great explanation of what combinator is. And when you plug it together with something like parser and you end up with parser combinator, what that means is that you can take small parsers that are all pretty simple by themselves. Let's say you have a parser that parses a string of numbers and another one that parses a comma and another one that parses a bunch of white space.

You could combine all of those using parser combinators to come up with an expression that says something like, parse a number followed by a comma followed by another number. Or parse any number of numbers, each separated by a comma. Those types of expressions are really easy to do with parser combinators, but surprisingly difficult to do with regular expressions.

>> Mm-hm.
>> Usually in a language like JavaScript, you will use regular expressions as part of otherwise imperative code that parses some stream. So you're like, chew up, you know, a bunch of digits and then change your parser state to say, okay, now I'm looking for a comma, and then you go look for a comma.

And then you switch back into the I'm looking for a numbers thing. Whereas in a language like Haskell with parser combinators, you can express that more declaratively and say, I want a bunch of numbers. And then a comma, and then some more numbers and it reads very well.

>> That's really neat, that's nice that it gives us that ability. So we want to parse some big weather data for fun. Let's see what some weather trends are and let's write a little app for it. And we would be able to take whatever weather format data it is and if we have these simple little parsers that parse each little piece, we can kind of combine those all together just to make it all work.

And there's type safety, right? That's also a big nicety with regex, you could mess up a regex expression and never really realize that, right?
>> Yeah.
>> That's something that's dangerous. Because that could end up in production easy if you just forget to test this one case, and then you have some sort of leak or something that somebody could input an invalid credit card.

And they're not having access to your service, or whatever
>> Or they get it for free or something. Yeah, regex are pretty, or maybe even surprisingly easy to mess up. You miss one closing parenthesis, and your whole regex crashes. Or you put a dot in the wrong place and it means something drastically different.

You obviously still can screw things up with parser combinators.
>> Right, but you can screw up anything with anything.
>> Right, if you try anything you can screw anything up. But it's gonna be harder with parser combinators because you are still working in the hosts language in Haskell, you're just writing Haskell code calling Haskell functions works the same as everything else.

You can code review it without having to look over to your cheat sheet and figure out what the heck that new rejects feature is mean. What that means, whereas regex is this own very compact little language that sits inside of another language like JavaScript or Ruby or what have you.

And so that makes it really powerful because you can apply all the same tooling that you do for static analysis or just your gut feel for that code looks right in that code doesn't look right. You can apply that to code written with parser combinators versus stuff wouldn't written with regular expressions.

>> No, that makes sense and I was I don't like this guys cheat cheats. Pretty cool as far as like just little bits of Haskell regex, a little shortcuts and things that. But it just doesn't seem like it's all that robust. And obviously, it's hard when it's a tutorial to really go deep into maybe some of the powerful sides of regex, but I just I just see it getting confusing.

If we were trying to go any further than the simple examples.
>> Yeah, it can get confusing really fast. And as someone who's spent a lot of my career working with regular expressions. I can say that they feel really good to write because you think like, I'm so clever and I got this really complicated expression down to this really small expression of it.

But then, six months later when you come to look at it you think, I don't know what this means anymore.
>> I hope it researched and figured out which is which is where and there's obviously rejects.
>> It's useful, it's got a purpose. There's plenty of people out there using it.

>> Yeah. Is cool.
>> And earlier you mentioned something about type safety which I wanted to come back to because Haskell is a strongly typed language, not every language is. But even aside from that, when you parse something with regular expressions, the only thing you can get out of it, use strings, you match strings and you get them out as groups of strings.

And then you have to do something else with that group of strings. So in a language like JavaScript, if you wanted to pull a number out of a CSV file, you could do that with the regex. But then, once you have the number, it's actually a string, so you have to convert it into a number via some other means.

In Haskell, when you're using parser combinators, you can have a parser combinator that produces something other than a string. In fact almost all of them produce things other than strings. They'll produce a number directly or even a custom data type. So if you have like a user in your system and you wanna write it out to some custom, serialized format and then read that back.

You can write a parser that will read it directly into a user, there's no intermediate step involved.
>> Right, which that's nice and that guarantees that you're gonna get the tech you expect.
>> Exactly, there's no chance-
>> And the compiler will also tell you that.
>> There's no chance in between parsing the thing and turning it into the value that you actually want.

For anything to go wrong cuz they happen at the same time.
>> Right, that's really neat I think that value in itself is obviously a testament to the strongly typed Haskell language. And I think that leans more towards this parser combinator idea rather than regex, even in Haskell because regex deals with strings.

That's its nature. Yeah.
>> I will say indefensive regexes. One thing they do very well that pressure combinators aren't necessarily as good at is substituting something in a string of text. So if you have some big run and you want to replace every letter t with the letter g, For whatever reason.

>> Maybe you're doing DNA sequencing and you're doing some weird stuff. I don't know.
>> Maybe YG.
>> Something like that.
>> Whatever it is.
>> Doing that replacement is super easy with regular expressions because you can match on something and then take an action based on that match.

Granted, this isn't a feature of regular expressions themselves, like computer science concept. But in every language that has regular expressions, there's a mechanism for doing these substitutions, right? Which is helpful and with JavaScript like It's one of the best ways to be able to, somewhat safely, guaranteed substitution.

If you're looking through this set of data, and you say, hey, every time you see this word I want you to replace is with this word, that substitute function is useful for that.
>> It's so easy to use.
>> Right, and that's the great thing about regex, and I don't I think it's really good for the little stuff but it's just really cool that Haskell offers us the parser combinator that allow us to get a little.

A little, more intense, without losing understanding and also some type safety like that's I that is important to writing good robust code that doesn't break every other week. It's only about finding a bug. This forum is validating right anymore which I mean I doubt we would use pressure commentators on a form validation because.

>> But you, right.
>> Yeah, there's no reason not to. Yeah, swear. So I know we're kind of running out on time maybe but I just kind of wanted to. Have more of a curiosity, is there much overhead with understanding parser combinators? If you have a good level understanding of Haskell, what's the level of difficulty of parser combinators?

>> Parser combinators definitely don't come for free. It's not like if you already understand Haskell, you can jump right into a parser and work it out just fine. You'll probably be able to stumble around and figure stuff out and go look at documentation or read something, but it won't necessarily be immediately obvious.

That being said, as I said earlier, you can still lean on your ASCII knowledge, you can look at something and say that's a function call, that's an operator. That's taken this argument or that argument, and you can kind of piece it together. Many of the parser libraries for Haskell share a lot of common kind of vocabulary.

They use the same words for stuff which makes it really easy to jump between them. So if you use parser and one of your libraries and you use mega parts like in the other or there's a bunch cool. You'll be able to jump between those without too much trouble.

So your knowledge is transferable between them. There's still the downside of as you mentioned at the top of the show. Regex is are everywhere, every language basically has them everybody. Most working programmers know what they mean and parser commentators are not like that at all. They're their own little thing.

>> Right. So there's a little bit of mental complexity there but.
>> Mm-hm.
>> The payoff of safety and.
>> I think it's worth it.
>> Right, it's worth it in the end. I mean that's kind of how a lot of things are, I mean ours was. Great, but there are some confusing aspects.

Conduits and lenses, all these different topics, and parser combinators are probably in that list.
>> Yeah, it's actually, in my mind, very similar to lenses, in that it's a very powerful abstraction that gives you something that's hard to get In other languages, and it does it in an expressive way that looks kind of impenetrable at first..

But then as you come to understand it, you realize, no, this actually makes a lot of sense, it's just weirder, it's just different than what I'm used to. And with lenses and parser combinators both, they have ties into other parts of Haskell. So as you understand them better, you sort of get a different of deeper understanding of other parts that you've used before, that you now see in a different light.

So with parser combinators, they frequently use this weird operator that's like an asterisk and then a greater than sign or the other way around. And you can view this in a parser as, do this, but throw away the result and then do this other thing. And it turns out that that's super useful and it shows up in lots of other places in Haskell anything like that's the same concept that I was using over here for parsers is applicable over here where I'm running servers.

And it's just kind of strange to see that same thing in these two wildly different places.
>> That's really cool and I think really that's been a great conversation on parsing and Haskell. Some of the differences between, the regex and parser combinators and some of the more verbosity.

I don't know if that's the right word.
>> I think verbosity's a word.
>> I keep trying to use these fancy words and probably just shouldn't. I should just let it be. But bottom line is it's powerful, right? It gives us.
>> It is. Haskell is great for parsing.

It is a language that a lot of people use to parse, and that's really cool.
>> I've never had just time, hey, let me write this parser for this random thing. But I know another one of co-workers here, Justin. He was writing a parser in Rust. And he asked me questions cuz he doesn't deal with the functional paradigm that much.

But he was kinda like, what is going on here? And I'm like, I can kind of see what's going on. Like obviously just from the knowledge, you're like the functional programming paradigm.
>> Yeah.
>> And so I think that's just cool that Haskell is more commonly the one you choose to write partner and just a prototype that parser.

Just because it's quick and it works the way you expect it to work and there's type safety. So it's a great language for you.
>> Yeah.
>> I think it's awesome.
>> Yeah, it's awesome.
>> Thanks for talking to me about pursing in Haskell this week Cameron
>> No, thank you for having me too.

It's been a lot of fun. It was really cool to hear your background and your knowledge and understanding of parsing the Haskell
>> I hope I've helped you understand parsing in Haskell a little better and I hope I've helped our listeners understand parsing and how it compares with regular expressions other languages like JavaScript.

>> Yeah, I'm excited about topics to come to I think our listeners got some cool stuff coming.
>> Thank you for listening to the Haskell Weekly podcast. This has been episode seven. If you liked our show, find out more at our website, haskellweekly.news. Thanks again for listening. I've been your host Taylor Fausek.

See you again next week. Adios.
|]
