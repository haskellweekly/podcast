{-# LANGUAGE QuasiQuotes #-}

module Podcast.Episodes.Episode5
  ( episode5
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

episode5 :: Either String Episode.Episode
episode5 =
  Episode.Episode
    <$> Articles.fromStrings
          ["https://sakshamsharma.com/2018/03/haskell-proj-struct/"]
    <*> Date.fromGregorian 2019 4 8
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about build tools in Haskell, \
          \including Stack and Cabal."
    <*> Seconds.fromTimestamp 15 15
    <*> Guid.fromString "25b43cdb-e278-42da-97dc-3c6d353ec8c8"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-08-episode-5.mp3"
    <*> Number.fromNatural 5
    <*> Right (Bytes.fromNatural 21977225)
    <*> Title.fromString "Build Tools"
    <*> Right (Just transcript)

transcript :: Transcript.Transcript
transcript = Transcript.fromString [Quasi.string|
>> Hi, welcome to the Haskell weekly podcast. This show is about Haskell, a purely functional programming language. I'm your host, Cameron Gera, and with me today we have our wizard expert here at ITProTV for Haskell, Taylor How are you doing Taylor?
>> I'm doing well Cam. How are you?

I'm doing pretty good. I'm pretty excited about today.
>> Me too. I would be more excited if I knew, what are we talking about?
>> Well, I was just gonna let you figure that out.
>> But I will bring it up because I think a very important subject in the Haskell world is stack vs ball.

So I just wanna talk about kind of stack a little bit more, and maybe some of the build tools and how that's helpful for us. I was reading an article about kind of how stack can give you this project structure out of the box, and how that's helpful for keeping all Haskell projects in the same format.

>> We use Stack here at ITProTV to manage our Haskell projects. We only have one big project, and it's got a lot of smaller projects tucked inside of it. And Stack does a great job of keeping those things all in sync, along with all the dependencies that we use.

Cabal is kind of the other contender in this area and we don't have any experience as a team here using Cabal directly. But Zach uses Cabal for a lot of things behind the scenes and in a lot of ways you could think of it as a smooth veneer on top of the foundation that Cabal provides.

>> So it's like a shiny little outer coating.
>> Exactly.
>> Underneath is the real meat of it, which is Cabal.
>> Yeah, I don't know why you have a shiny coating on top of your meat, but whatever floats your boat.
>> I was gonna go with like a piece of candy.

Kinda like the M&M, like the shiny green outer and then Cabal is like the chocolate center.
>> That's delicious.
>> The chocolate center of Haskell project development.
>> Yeah, well, I've heard a lot of things, being relatively new to Haskell. Stack made it kind of easy to understand, this is how I make something new.

I've worked with Stack, trying to make a Hakyll blog and other stuff like that. And it seems to be fairly useful, when I'm trying to figure out what I need to use, what dependencies I have, I just kinda tell it hey, install this. Then it's there. There's no jumping through any hoops.

And why is that? I know we have this thing called Stackage. Kinda how does that help us? So Stackage is a set of Haskell packages that are known to all work together. So you mentioned Hakyll. Hakyll as part of its, one of Hakyll's dependencies is Pan Doc which is a thing that lets you convert document formats from one to the other.

So if you have a markdown file and you want to produce HTML, which is extremely common when writing a blog. Pan Doc is a tool that will let you do that and about a thousand other things. Pan Doc requires a lot of dependencies and it can be hard to find versions of all of those things that work well together.

So if you want to start a blog and you want to use Hakyll to produce it, it can be challenging to find all the dependencies you need and all the versions of those dependencies that all work togethe. Stackage has done all that work for you. So when you tell stack hey install Hakyll stack already knows, yeah, I can install these dependencies together and they're just going to work.

That's in comparison to something like Cabal, at least older versions of Cabal. I don't know how things have changed recently. But Cabal works by resolving dependencies. So each dependency has a version constraint that says I work with this broad range of packages. And Cabal tries to pick a specific version of each one that the package claims it will work with.

This is basically working on the honor system. So there's no guarantee that package A will actually work with package B even though it says it will. With Stackage that work is done ahead of time and the maintainers are notified when something doesn't actually work. But with Cabal, it's possible to get into a situation where you try to install something that says it should work and it doesn't.

And you're left trying to figure out what went wrong. And this is kind of historically, in the Haskell community, called Cabal hell and this is something that stack helps you avoid.
>> I see, yeah, so having a background in JavaScript to a little extent, we use some forms of NPM.

There was kind of this hell of you have this one package you're specifying but it's installed a newer version of this other package. And you have all this issue they've got package lock now on a lot of stuff. And I haven't necessarily spent enough time in JavaScript lately to really understand or appreciate that stuff, because we've been working in Haskell.

In Haskell, I've had stack, so I haven't had to experience Cabal hell, but I have heard fun stories. And that's nothing against, well, it's just part of how they do things. And I appreciate that stack allowed us to kind of not have to worry about that kind of coming into Haskell and being a beginner, right.

Like stack I feel like makes the beginner's life a lot simpler.
>> I definitely agree with that. It's one less thing to worry about when you're getting started with Haskell. You don't have to wonder will these packages build together. You know that they will. It's also nice, you mentioned NPM.

One of the things that NPM does that Haskell can't really do is that if you have two packages that depend on different versions of some sub package, they can just install both versions of that sub package. And say sure, whatever, package A will get this one version, package B will get this other version.

In Haskell you can't do that because all of your packages have to agree with each other about every dependency that they're using. So you can't say Hackyll gonna use version one of text and pan doc is going to use version two and won't work because the types won't line up.

It's also worth mentioning that another thing Stack does, that NPM also does, is that it kinda quarantines your local project's dependencies from your global installation. So anyone who's worked with a JavaScript project is familiar with the node modules folder.
>> Ouch.
>> Which is a giant folder with hundreds of folders in there, one for each dependency.

Do we have an ouch noise. So known models is a giant folder with hundreds of dependencies in there and it's separate from your kind of global installation. And a lot of times, people will install things globally like Bower, or ESLint, some kind of tool that's just convenient to have on the command line.

In the old days with Cabal, it installed most of your Haskell packages globally. So if you were working on two projects and they needed different versions of a dependency, too bad, you can't really do that. Eventually they introduced this concept of a sand box, which is a lot like the node modules thing, the folder.

But the sand box augmented your global packages, it was in addition to all of the global stuff. So if you accidentally installed some global package that wasn't the one you wanted in your project, again, you're stuck. You have to go remove that global package and install it locally instead

>> That thing is rough, because then that could cause mismatches in people's local environments to what's happening in production, too. I feel like that's super dangerous.
>> Very dangerous and annoying.
>> Right, and I feel like we've kinda circumvented that because we dockerize things. So in our development locally, we dockerize everything.

So all of our packages, if they were installed globally even would be books on our container that we're working in. So we hopefully would be able to avoid that said issue.
>> Exactly, I don't think we would have run into this particular problem, even if we were using Cabal instead of Stack, because we're inside a Docker container.

Like you said, a global package inside a Docker container is still kind of local to that Docker container.
>> Right.
>> Newer versions of Cabal have added this new way of building stuff, where it completely ignores your global package database, and only uses one that's sandboxed for the project.

They call this NYX-style local builds. Which is taking a page out of the NYX handbook, which does everything for your entire system like this. Cabal focuses just on Haskell packages.
>> Interesting, yeah, our co-worker Cody, he told me that I should make a Nix joke, so I'm glad you brought that into the picture.

>> Can we nix that NYX joke?
>> No, I can't nix it now. I have to, well, let's just move on, anyways.
>> I tried, I'm not as funny as some of our fellow peers here at ITProTV. But I do what I can, to try and stay with it.

Stack in Cabal kind of heard, kind of the underlying side of Cabal, which has been nice to kind of understand more of like, what happened and why global dependencies and stuff like that were kind of causing issues. If you're going to give advice to anyone kinda starting in Haskell, whether they have programming experience or not.

What would be the best thing about Stack for them to look into? I dropped a bomb on you, now-
>> It's hard for me to pick one thing. I feel like there are at least two and the first one is that stack is going to manage your compiler installation along with all of your dependencies.

So Cabal doesn't install GHC for you. You have to do that yourself and then you can point Cabal at the GHC that you want to use.
>> What's the nicety? This is really cool that Stack did this for me.
>> Right, it's something that's gonna be invisible in that you're not going to end up in a situation where you're stuck because some dependencies don't work out.

And that's a hard thing to sell too hard on because it's the lack of a problem. But the problem itself is really annoying and very difficult for a beginner to solve. Because they don't really have the tools or the expertise to figure out what happened, and how to fix it.

And Stack, for the most part, just sidesteps the whole thing. Occasionally, you get into a situation where you want a package that isn't on Stackage. And that's when things get complicated, from the Stack side. With Cabal, pretty much any package, well really any package is gonna be installable with Cabal eventually.

Maybe you need to work out some dependencies but-
>> Anything on hackage pretty much you could find use.
>> Exactly, mm hm. And that's also true with Stack, but if something isn't on Stackage, then you don't get that nicety of it just working automatically. It may work really well without any extra work, but it might not work at all.

And then you have to figure that out and that can be hard for a beginner to do.
>> Got you, so we have a little bit of that in our code base based on our latest resolver that we have. We don't have some of our dependencies, so we had to bring in-

>> Yeah, like Happstack.
>> Happstack, right. And so that was kind of a, it kinda uses these extra dependencies and text kinda thing in the stack.yaml. And for a beginner, what would be just something to be aware of when trying to figure out, what do I do with this extra dependency, what does that look like?

>> I think that Stack's documentation does a good job of explaining what these extra dependencies are, why you might need them, and how to put them into your stack.yaml file. Also, when you make a new project with Stack, it includes a Stack.yaml file that has a bunch of comments in it that say, if you need to include an extra depth, this is what it would look like, and this is why you might need it.

There's also a lot of good blog posts on this topic. This is something that a lot of people have run into where they're working with Stack and they're enjoying it and then they run into a package that they need to install that's not on Stackage and they tell you how to do that.

So I would say turn to Google.
>> Good ol Google, that's an engineer and developer's best friend.
>> Sure is.
>> Stack Overflow, any blog posting platform is great. They do a really good job. I know for me that's been something that's really really been helpful. It's a big topic.

Volver stack in the Haskell world. We want everyone to know we love both. We just use Stack so we're more familiar with it. Why do you think necessarily this is such a hot topic?
>> Me personally I don't feel too strongly one way or the other and we've been posing this as a Cabal versus Stack-

>> Discussion.
>> Flame war, discussion that we're hinting at here, but really there are other entrants in this battle. There's NYX which we've mentioned because Cabal kind of borrowed this concept of NYX style local builds. But you could use NYX to build Haskell packages and a lot of people do.

You can also use some more Niche tools, I know that there's one called Mafia. You could also use GHC directly, which works great. If you don't have a lot of dependencies GHC has a -- make flag that makes it behave more like a build tool than a compiler.

And I'm sure there are many other build tools that I'm not aware of, or that I'm failing to mention right now. That being said, Stack and Cabal are kinda the big players. And I think Stack has done a great job of making Haskell development accessible to a wider audience, which was its goal from the get go.

In a previous job I was very gung ho about using Haskell, but I worked on a team that primarily worked with Ruby code. And I didn't feel comfortable suggesting to them that we use Cabal as our build tool. Because I ran into so many of its sharp edges with failing to install a dependency, or polluting the global package database, any of those things.

When Stack came out I thought, you know what, I would be okay subjecting the rest of the team to this tool.
>> Right.
>> Cuz it generally works pretty well. As to why it's such a contentious topic in the community, I think that a lot of people have invested time into learning Cabal and its quirks and mastering i.

And when there's a new tool that doesn't require all of that expertise, it can make you feel like you either you've wasted some of your time or that people need to learn the same things that you learned in order to get onto your level. And so this kind of undercutting their expertise However, I'm not a psychologist.

I couldn't tell you. I'm sure there are many more reasons why.
>> Well, I appreciate your time, too. I appreciate you being able to take the time to just talk to us about Atack and about Cabal and hear some of the quirks of both even and how we can, use that in our day to day even before expert or we are beginning.

Because there's some niceties with Stack and if you use Cabal that's great and there's nothing no hard feelings.
>> Yeah.
>> All right, thanks for being with us Taylor and thank you all for listening to the Haskell weekly podcasts. This has been episode five and I've been your host Cameron Gara.

If you liked our show find out more at our website haskellweekly.news. Thanks again for listening and see you next week.
|]
