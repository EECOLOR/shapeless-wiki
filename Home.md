# shapeless: generic programming for Scala

**shapeless** is a type class and dependent type based generic programming library for Scala. It had its origins in
several talks by Miles Sabin ([@milessabin][milessabin]), given over the course of 2011, on implementing [scrap your
boilerplate][syb] and [higher rank polymorphism][higherrank] in Scala. Since then it has evolved from being a resolutely
experimental project into library which, while still testing the limits of what's possible in Scala, is being used
widely in production systems wherever there are arities to be abstracted over and boilerplate to be scrapped. 

## Contents
+ [Finding out more about the project](#finding-out-more-about-the-project)
+ [Using shapeless](#using-shapeless)
  + [shapeless-2.0.0](#shapeless-200)
  + [shapeless-2.1.0-SNAPSHOT](#shapeless-210-snapshot)
  + [shapeless-1.2.4](#shapeless-124)
+ [Building shapeless](#building-shapeless)
+ [Contributors](#contributors)

## Finding out more about the project

A feature overview of shapeless-2.0.0 can be found [[here|Feature-overview:-shapeless-2.0.0]]. If you are upgrading from
shapeless-1.2.4 you will find the [[release notes|Release-notes:-shapeless-2.0.0-M1]] and
[[migration guide|Migration-guide:-shapeless-1.2.4-to-2.0.0]] useful.

shapeless is part of the [typelevel][] family of projects along with [Scalaz][scalaz] and [Spire][spire]. It is an Open
Source project under the Apache License v2, hosted on [github][source]. Binary artefacts are published to the [Sonatype
OSS Repository Hosting service][sonatype] and synced to Maven Central.

The project is currently at the first milestone release of shapeless-2.0.0 and if you are starting to investigate
shapeless it is recommended that you start there: a final shapeless-2.0.0 release is expected before the end of 2013.
shapeless-2.0.0 takes advantage of the availability of implicit macros in Scala 2.10.2 to reduce, and in many cases
completely eliminate, the already minimal boilerplate that remained in earlier releases.

There is a [mailing list][group] for discussion around generic programming in Scala in general and shapeless in
particular. You will also find many of the main shapeless contributors on IRC in the #shapeless channel on
[freenode][irc]. Questions about shapeless are often asked and answered under the [shapeless tag on StackOverflow][so].
Some articles on the implementation techniques can be found on [Miles's blog][blog], and Olivera, Moors and Odersky,
[Type Classes as Object and Implicits][tcoi] is useful background material.

Support for Scala 2.9.x is still available via the shapeless-1.2.4 release (feature overview
[[here|Feature-overview:-shapeless-1.2.4]]). It isn't straightforward to bring the latest shapeless features to Scala
versions which don't support implicit macros, and this release should be treated as a stopgap until you are able to move
your project to Scala 2.10. It might, however, be feasible to backport some of the updates via a compiler plugin for
Scala 2.9.x, and anyone interested in contributing or sponsoring such work should [get in
touch](mailto:miles@milessabin.com).

[milessabin]: https://twitter.com/milessabin
[syb]: http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/
[higherrank]: http://www.cs.rutgers.edu/~ccshan/cs252/usage.pdf
[typelevel]: http://typelevel.org/
[scalaz]: https://github.com/scalaz/scalaz
[spire]: https://github.com/non/spire
[tcoi]: http://ropas.snu.ac.kr/~bruno/papers/TypeClasses.pdf
[source]: https://github.com/milessabin/shapeless
[sonatype]: https://oss.sonatype.org/index.html#nexus-search;quick~shapeless
[wiki]: https://github.com/milessabin/shapeless/wiki
[group]: https://groups.google.com/group/shapeless-dev
[so]: http://stackoverflow.com/questions/tagged/shapeless
[irc]: http://freenode.net/
[blog]: http://www.chuusai.com/blog

## Using shapeless

Binary release artefacts are published to the [Sonatype OSS Repository Hosting service][sonatype] and synced to Maven
Central. Snapshots of the master and scala-2.11.x branches are built using [Travis CI][ci] and automatically published
to the Sonatype OSS Snapshot repository. To include the Sonatype repositories in your SBT build you should add,

```scala
resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)
```

[ci]: https://travis-ci.org/milessabin/shapeless

### shapeless-2.0.0

Builds are available for Scala 2.10.2 and later and for Scala 2.11.0-RC4. Note that you must specify a Scala version
of at least 2.10.2, and that for Scala 2.10.x and non-final Scala 2.11.0 releases you must add either
`cross CrossVersion.full` or provide an explicit Scala version suffix to your shapeless dependency,

```scala
// For Scala 2.10.x >= 2.10.2
scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless_2.10.4" % "2.0.0"
  // "com.chuusai" % "shapeless" % "2.0.0" cross CrossVersion.full  // Alternatively ...
)
```

Note that Scala 2.10.x releases are compatible with each other starting from 2.10.2, so a mismatch in minor versions
above would be fine.

```scala
// For Scala 2.11.0-RC4
scalaVersion := "2.11.0-RC4"

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless_2.11.0-RC4" % "2.0.0"
  // "com.chuusai" % "shapeless" % "2.0.0" cross CrossVersion.full  // Alternatively ...
)
```

### shapeless-2.1.0-SNAPSHOT

Builds will be available for Scala 2.10.4 and Scala 2.11.0 shortly after the final release of Scala 2.11.0.
The main line of development for shapeless 2.1.0 will be Scala 2.11.0 with Scala 2.10.x supported via the macro
paradise compiler plugin.

```scala
scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.1.0-SNAPSHOT" changing()
)
```

Note that for Scala 2.10.4 you must provide an explicit Scala version suffix to your shapeless dependency,

```scala
scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless_2.10.4" % "2.1.0-SNAPSHOT" changing()
)
```

### shapeless-1.2.4

Builds are available for Scala 2.9, 2.10 and 2.11.0-RC4. If you are working with Scala 2.10.2 or later you
should use shapeless-2.0.0 instead.

If your project is built with Scala 2.9.3 or earlier, then you will need to specify the `-Ydependent-method-types`
compiler flag,

```scala
scalaVersion := "2.9.3"

scalacOptions += "-Ydependent-method-types"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "1.2.4"
)
```

This option isn't necessary or supported in Scala 2.10, so you should omit it if you are building with Scala 2.10.2 or
later,

```scala
scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "1.2.4"
)
```

If you want to be able to support building relative to both 2.9.3 and 2.10.x then you should use the 2.10.4
configuration above and add the following,
 
```scala
scalacOptions <++= scalaVersion map { version =>
  val Some((major, minor)) = CrossVersion.partialVersion(version)
  if (major < 2 || (major == 2 && minor < 10)) 
    Seq("-Ydependent-method-types")
  else Nil
}
```

which will set the `-Ydependent-method-types` compiler flag conditionally on the actual Scala version in use.

## Building shapeless

shapeless is built with SBT 0.13.1. The master branch is built with Scala 2.10.4 by default. To build with Scala 2.11.0
you should check out the scala-2.11.x branch. As a general rule all new features and bugfixes are made against master
and Scala 2.10.4 and merged into the scala-2.11.x branch with only the minimal changes needed for forwards
compatibility.

## Contributors

+ Alois Cochard <alois.cochard@gmail.com> [@aloiscochard](https://twitter.com/aloiscochard)
+ Ben Hutchison <brhutchison@gmail.com> [@ben_hutchison](https://twitter.com/ben_hutchison)
+ Ben James <ben.james@guardian.co.uk> [@bmjames](https://twitter.com/bmjames)
+ Brian McKenna <brian@brianmckenna.org> [@puffnfresh](https://twitter.com/puffnfresh)
+ Cody Allen <ceedubs@gmail.com> [@fourierstrick](https://twitter.com/FouriersTrick)
+ Dario Rexin <dario.rexin@r3-tech.de> [@evonox](https://twitter.com/evonox)
+ George Leontiev <folone@gmail.com> [@folone](https://twitter.com/folone)
+ Huw Giddens <hgiddens@gmail.com>
+ Jason Zaugg <jzaugg@gmail.com> [@retronym](https://twitter.com/retronym)
+ Johannes Rudolph <johannes.rudolph@gmail.com> [@virtualvoid](https://twitter.com/virtualvoid)
+ Joni Freeman <joni.freeman@ri.fi> [@jonifreeman](https://twitter.com/jonifreeman)
+ Julien Tournay <jto boudhevil@gmail.com> [@skaalf](https://twitter.com/skaalf)
+ Kevin Wright <kev.lee.wright@gmail.com> [@thecoda](https://twitter.com/thecoda)
+ Lars Hupel <lars.hupel@mytum.de> [@larsr_h](https://twitter.com/larsr_h)
+ Mathias Doenitz <mathias@spray.io> [@sirthias](https://twitter.com/sirthias)
+ Michael Donaghy <md401@srcf.ucam.org>
+ Michael Pilquist <mpilquist@gmail.com> [@mpilquist](https://twitter.com/mpilquist)
+ Miles Sabin <miles@milessabin.com> [@milessabin](https://twitter.com/milessabin)
+ Nikolas Evangelopoulos <nikolas@jkl.gr> 
+ Paolo G. Giarrusso <p.giarrusso@gmail.com> [@blaisorblade](https://twitter.com/blaisorblade)
+ Stacy Curl <stacy.curl@gmail.com>
+ Stephen Compall <scompall@nocandysw.com> [@S11001001](https://twitter.com/S11001001)
+ Tom Switzer <thomas.switzer@gmail.com> [@tixxit](https://twitter.com/tixxit)
+ Travis Brown <travisrobertbrown@gmail.com> [@travisbrown](https://twitter.com/travisbrown)
