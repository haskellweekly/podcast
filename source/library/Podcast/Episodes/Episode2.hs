{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode2
  ( episode2
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

episode2 :: Either String Episode.Episode
episode2 =
  Episode.Episode
    <$> Article.fromString
          "https://engineering.itpro.tv/2019/03/01/upgrading-elm-to-v19/"
    <*> Date.fromGregorian 2019 3 18
    <*> Description.fromString
          "Sara Lichtenstein and Taylor Fausak talk about the good and bad \
          \of upgrading from Elm 0.18 to 0.19."
    <*> Seconds.fromTimestamp 14 59
    <*> Guid.fromString "00900298-5aa6-4301-a207-619d38cdc81a"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-03-18-episode-2.mp3"
    <*> Number.fromNatural 2
    <*> Right (Bytes.fromNatural 21580339)
    <*> Title.fromString "Upgrading Elm"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hi, welcome to the Haskell Weekly podcast. This is a show about Haskell, a purely functional programming language. I am your guest, Sara Lichtenstein, and I'm an engineer at ITProTV. And this your host, Taylor Fausak.
>> Hi, I'm the lead developer at ITProTV. Today, we're going to be talking about Elm.

In particular, upgrading Elm from 0.18 to 0.19 which we recently did here at ITPro. Sara was a big part of that upgrade process, and she wrote a blog post about it. And so we're gonna be kinda digging into that blog post and talking more about it. Sara, do you wanna tell us a little about Elm and get us started on the upgrade path?

>> Absolutely, so here at ITProTV, as you know, as our lead engineer, we use a lot of Haskell. And luckily hand-in-hand with that goes Elm, since it's written in Haskell. So we use Elm for our front-end, as I'm sure you're aware. And we had quite an interesting time upgrading from 0.18 to 0.19.

There was a couple things that didn't go great. But overall we've really enjoyed the upgrade and using the upgrade.
>> And using Elm altogether.
>> Absolutely.
>> It's been great.
>> Yes.
>> Yeah, this upgrade didn't really make us want to ditch Elm entirely. We're still very much into Elm.

But what are some of the things that didn't go well with the upgrade?
>> So what we noticed were there was a couple breaking changes, for example, the change from the core date into time POSIX. It didn't really have the same functionality and it's not in core anymore, which is kind of a lot.

>> That's weird.
>> Yeah.
>> If it's not in core, where did it go?
>> It went to a library called time from Elm, so it's not in the Elm core part, but it's in a time of it's own.
>> Ha ha.
>> A land before time, maybe.

They've moved it off into their own official Elm-
>> Time library.
>> Yeah, okay, so it's still official, it's just not included by default.
>> Right.
>> Got you. And they changed a bunch of stuff when they moved, it right?
>> Mm-hm.
>> So it's a little annoying they did both of those things at the same time.

>> Yeah, the functionality is just totally different and switching from date to POSIX is not very seamless. It's a little bit testy.
>> Testy, that's a good word. We wrote a lot of tests around this, didn't we?
>> Yes, we did.
>> Okay.
>> Actually, the only tests in our database are about time, so.

>> Yeah. You gotta be sure that you get time right, I mean.
>> Mm-hm. It's pretty important.
>> Yeah.
>> This weird construct that we've created. It's pretty important to get it right.
>> Very true, so fortunately we were able to get time right and move on. But there were still some other things that didn't go well.

What were they?
>> Well with the rest of our breaking changes, we had some issues with the navigation since now you have to have this key concept in.
>> And what's that key concept?
>> So you pass a key in your navigation through, so instead of using, I think it's browser.

>> Yeah, I think that's what it was before.
>> Yeah, I think was browser before. So instead of using that, you have to pass this key along. Which is kind of helpful, actually, once you get down to it. But making that change was a little bit more difficult because it's a different concept than using browser was.

>> Right, so instead of saying, go to this particular URL or this particular route, you have to pass along a key that says. I don't know, what does it say?
>> Like this is your route, I guess?
>> Okay.
>> But it makes more sense in my mind to pass it around for the URL changes and how to link it back and forth.

But making that change was still a little bit difficult.
>> Got you.
>> So-
>> So you like where we ended up but the path was maybe not so nice.
>> Yeah, exactly. We had some changes with the HTTP library as well, which again were great changes. But the package felt like it had more breaking changes than any other package for us, which was obviously not great.

No one likes a breaking change.
>> Yeah and we do a lot of HTTP calls. So maybe it's not so surprising, but it's still annoying.
>> Mm-hm, exactly. And then, just some function signature changes that didn't quite make sense and maybe could have come along with an explanation that would have made them make more sense to us.

>> Yeah.
>> So.
>> Yeah, it's kind of frustrating. I don't remember the scope of these changes, but it's on the order of, the arguments change positions or something like that.
>> Yeah.
>> Well sure, I could see how it might be better but is it really worth it?

>> Yeah, it would have been better if that had come with an explanation as to why it was changed and what was the purpose. Versus just making the change and not really saying anything about it.
>> Yeah, well, I don't want to keep harping on these breaking changes.

I know other stuff was involved in the upgrade. So what else went wrong?
>> So with all the breaking changes came a broken compiler. Which obviously, being familiar with using Haskell and its great compiler, and Elm and its even better compiler. Cuz it's so friendly and nice.
>> And great with the errors.

Not having a compiler made it very difficult to figure out what exactly was going wrong and how to fix it.
>> Yeah, and when you say broken compiler, you don't mean that the Elm compiler itself was broken. You mean that it didn't work with our project for a little while?

>> Right, when you're upgrading, you have to upgrade the compiler simultaneously, which makes it a lot more difficult. Because if the compiler doesn't work with what you're using, I don't know how to phrase this.
>> Yeah so, as part of the upgrade, they changed some syntax.
>> Yeah.

>> And we couldn't write something that worked both with the old compiler and the new compiler.
>> Yep, yep.
>> So,
>> Those are the words I was looking for.
>> When we upgraded, Elm 19 just said, nope, I can't deal with this file. And that's a lot less useful than it saying, the problem is right here, like it normally does with the errors.

It points it straight to it and it even says, hey, maybe this is the solution you're looking for.
>> So politely, is this what you meant?
>> Yeah, it's a very nice compiler.
>> Yes, Elm, thank you.
>> Why can't it just make the changes for us?
>> I know, that'd be too easy.

>> That maybe will be in Elm 20, who knows?
>> Evan are you listening?
>> Elm 20.
>> So some of these problems sound like they would have been solved or at least made a little better with better documentation or explanation around the changes. But I know we had problems with the documentation itself.

Can you talk a little about that?
>> Yeah, so that's another big issue is that once Elm 19 came out, that 18 documentation just went away.
>> That's not great. Luckily, we were able to find it, but it took a lot of Internet digging, which good thing we're engineers familiar with the Internet.

>> That's 90% of our job.
>> Yeah, we were able to find it eventually. It currently lives on this GitHub page which, hopefully it'll stay there so that there's some sort of record of it. But it would be really nice if on the Elm website where the packages actually is, it let you go back and forth between the packages.

So you could see,
>> The old one.
>> This is what was in 18, this is what's in 19, this is what's in whatever the next one is.
>> Yeah, the one that's coming up to be prepared for the change.
>> Yeah, absolutely.
>> And you mentioned that this old documentation was hosted on some GitHub page.

It's not an official thing?
>> Is it an official thing?
>> I don't know.
>> I don't think so.
>> I don't remember where it lives.
>> I just remember one of the other engineers was like, this is where it is. And I was like, well great, I'm glad it's somewhere.

>> Gotta bookmark that, because I'll be coming back to it a lot.
>> Yep.
>> Yeah, it would be nice if it was an official thing because then you could count on it sticking around. But if it's just something that some enthusiast put together, maybe he'll get tired of it and it'll go away.

>> Exactly.
>> Fortunately, we're done with the upgrade now. But I'm sure people still haven't upgraded and it would be nice for them to use that same resource.
>> Yeah, we were pretty late to the game cuz I think it came out last August.
>> Yeah.
>> And we didn't do ours until what, February?

>> Yeah, we waited quite a while.
>> Yeah.
>> And one of the reasons we waited was so that the packages in the ecosystem would have an opportunity to upgrade. But I remember being kinda surprised because some of them weren't upgraded, right? Didn't we run into that?
>> I think so, yeah.

Yeah, cuz some of the community packages were just no longer supported.
>> Yeah, they're like, sorry, they don't support dates anymore, good luck.
>> Okay, which is absurd cuz we specifically chose those packages for like, Elm Community supports this. They're totally gonna keep this in the next one.

>> And then it was just like,
>> Psych.
>> Where'd it go?
>> Yeah.
>> So.
>> It feels kinda like we got bait and switched.
>> Little bit.
>> They're like use the community packages cuz they're not entirely official, but more official than depending on some random GitHub user/their Elm package.

>> And then they just became nonexistent.
>> Yeah, they just went away entirely.
>> For the GitHub users whose packages we do use, we use one for Elm Bootstrap and for Elm Dropdown. We had to fork those ourselves, just to update them to 19 to work for us.

>> Yeah, and were we able to contribute those changes back to them or are we still relying on our own forks?
>> Just relying on our own forks. But they work and that's great and obviously that's something that happens. If it's not a community-supported package, you have to kind of be aware that you may have to fork it and update it on your own.

So it's not a big deal, but it was still just a small annoyance.
>> Yeah, yeah and especially annoying because in Elm 19, you can't depend directly on a fork, right?
>> Right.
>> You have to publish it to the Elm package thing, so.
>> Yeah.
>> We have these weird packages that are exactly the same as an existing one.

They just support a little different version of the Elm compiler.
>> If you need the Elm 19 packages for Elm Bootstrap or Elm Dropdown, EdutainmentLIVE/ Elm Bootsrap, or Elm Dropdown.
>> Shameless plug for our packages.
>> There we go.
>> Hit us up on our Patreon page if you want this.

>> This is sponsored by stamps.com.
>> So, we had some small annoyances, obviously, and we definitely have some suggestions that we've talked about. Maybe splitting up the update to update the compiler first, or let it be backwards compatible a little bit. Maybe have a bridge in between, like a 18.5, to kind of bridge the gap.

>> Yeah, so that we can do this upgrade kind of in pieces.
>> Steps.
>> Rather than upgrading all the packages and the compiler and everything all at once.
>> Yeah, cuz it was a humongous change to our code base to upgrade an entire project, so.
>> Yeah, and as you mentioned, it was really annoying to have it broken in the middle.

>> Absolutely.
>> Where you're like, well, we had it working on 18, and it was broken for a week. And then it's working on 19, hopefully.
>> Yeah, maybe. We'll find out.
>> Just kidding, it's totally working. But yeah, obviously those would be really great changes to make for the next upgrade.

And maybe making that documentation super easy to find for everyone to just have record, and have that available.
>> Mm-hm.
>> Thoughts, keep your documentation available.
>> Please.
>> Really important.
>> We actually do use it.
>> We need it.
>> Yeah, it's especially frustrating because Elm has such good documentation.

>> Absolutely.
>> And then they just blow it all away when they release a new version.
>> Exactly, which is crazy. We want to keep it around, please.
>> Your users want it.
>> Yes, we your users.
>> We, your users, want your documentation.
>> So I don't want this to be completely a gripe session about the Elm upgrade.

I think that a lot of it went really well. And even though we have ideas about things that we would change in the future. Let's talk a little about the things that went well and the stuff we like about Elm 19.
>> Absolutely, because we really do love Elm 19 now that it's all upgraded and it's usable.

And we've doing a lot of stories in our Elm repository, and it's been going very well. There's a lot more Elm-supported packages, so Elm community became just Elm. And they have a lot more packages that they support, like browser, JSON, SVG, time and URL, which is great. So hopefully those actually translate into the next upgrade as well.

>> Yeah, and what makes it so nice to have those in the Elm namespace?
>> Just having those be supported by the language itself just makes it, I lost my words.
>> More reliable.
>> Yeah.
>> Reliable, yeah.
>> Yeah, and as we noted, we depended on that Elm Community package and it went away.

And it seems like maybe they weren't willing to commit to pull it into like their core namespace. But now that these ones are in the core namespace, we can be pretty sure that they're not gonna go away. Knock on wood, because this is a gamble we made with Elm Community.

>> Not gonna actually knock on wood.
>> But yeah, other than that, they removed the polymorphic toString function and replaced it by something like Int.toString, I believe. Which is a lot safer, and as we love type safety, this is a great change.
>> Yeah.
>> Absolutely loving it.

>> So instead of having a function that takes literally anything and converts it to a string. You have to be very specific and say, no I wanna convert this integer to a string.
>> This is not JavaScript.
>> So we are very specific about what we want to become strings.

>> Thankfully, it is not JavaScript.
>> Bless.
>> The documentation is actually even better in 19, which is great. Their documentation just keeps improving, just like the language, which is fantastic.
>> Yeah, and it's really nice to see Elm have this focus on documentation. Where it seems like for them the package isn't complete until it's documented and documented well.

Versus looking at some Haskell packages, sometimes you land on their documentation and it's literally just the type signatures. And you think, well great, this doesn't help at all.
>> What do I do with this? Yeah.
>> Yeah.
>> Yeah, and then the HTTP package, as we mentioned before, has changed and it is much better.

So they have functions like riskyRequest and riskyTask.
>> RiskyTask. So you know that you're using risky security policies. I have pronunciation.
>> And the function signatures for posting get got even better, so.
>> When you're reading the code that makes an HTTP call, it's clear when something uses riskyRequest or riskyTask that you need to be extra careful and look at this and understand what it's doing.

Versus before, it would just say request and you'd really have to dig in to the details to figure out if it's doing something weird
>> Yeah, absolutely.
>> Or risky, you might say.
>> Such risk.
>> Now we're not the only ones that have gone through this upgrade process.

We were really excited to see on the Elm subreddit yesterday that another company posted their experience report and mirrored a lot of the same comments that we had. So we're not alone in this process. And I'm hopeful that Evan and the rest of the Elm community will look at these posts and think about how the upgrade process can be better in the future.

>> Yeah, absolutely. I think it's great that we're not alone in this and that other people are experiencing the same sort of issues. And other people are as invested as we are in Elm that they wanna help make it better as well. And so they're making these kinds of postings and kind of trying to help contribute to this community.

>> Absolutely. Sara, any final thoughts?
>> As much as a headache as we've made this sound, it really wasn't too terrible. And we've learned so much along the way, and we're really loving Elm 19. And I highly recommend to anybody who's interested in it to definitely get in there and try it.

It's a really great language and it's very well documented, and it's definitely worth your time.
>> Sara, you gave us some really good information today. Thanks for being on the show with me.
>> Thank you for having me.
>> And thank you for listening to the Haskell weekly podcast.

This has been episode two. If you liked our show, find out more at our website haskellweekly.news. Thanks again for listening, I've been your host, Taylor Fausek, and we'll see you next week.
|]
