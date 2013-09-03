<!-- --- title: Feature overview: shapeless-2.0.0 -->

## Contents
+ [Polymorphic function values](./Feature-overview:-shapeless-2.0.0#polymorphic-function-values)
+ [oeterogenous lists](./Feature-overview:-shapeless-2.0.0#heterogenous-lists)
+ [HList-style operations on standard Scala tuples](./Feature-overview:-shapeless-2.0.0#hlist-style-operations-on-standard-scala-tuples)
+ [Facilities for abstracting over arity](./Feature-overview:-shapeless-2.0.0#facilities-for-abstracting-over-arity)
+ [Heterogenous maps](./Feature-overview:-shapeless-2.0.0#heterogenous-maps)
+ [Singleton-typed literals](./Feature-overview:-shapeless-2.0.0#singleton-typed-literals)
+ [Extensible records](./Feature-overview:-shapeless-2.0.0#extensible-records)
+ [Coproducts](./Feature-overview:-shapeless-2.0.0#coproducts)
+ [Generic representation of (sealed families of) case classes](./Feature-overview:-shapeless-2.0.0#generic-representation-of-sealed-families-of-case-classes)
+ [Boilerplate-free lenses for arbitrary case classes](./Feature-overview:-shapeless-2.0.0#boilerplate-free-lenses-for-arbitrary-case-classes)
+ [Automatic type class instance derivation](./Feature-overview:-shapeless-2.0.0#automatic-type-class-instance-derivation)
+ [Collections with statically know sizes](./Feature-overview:-shapeless-2.0.0#collections-with-statically-know-sizes)
+ [Type safe cast](./Feature-overview:-shapeless-2.0.0#type-safe-cast)
+ [Testing for non-compilation](./Feature-overview:-shapeless-2.0.0#testing-for-non-compilation)

All the examples below assume you have previously imported shapeless.\_

### Polymorphic function values

Ordinary [Scala function values are monomorphic][polyblog1]. shapeless, however, provides an encoding of polymorphic
function values. It supports [natural transformations][nattrans], which are familiar from libraries like Scalaz,

```scala
import poly._

// choose is a function from Sets to Options with no type specific cases
object choose extends (Set ~> Option) {
  def apply[T](s : Set[T]) = s.headOption
}

scala> choose(Set(1, 2, 3))
res0: Option[Int] = Some(1)

scala> choose(Set('a', 'b', 'c'))
res1: Option[Char] = Some(a)
```

Being polymorphic, they may be passed as arguments to functions or methods and then applied to values of different types
within those functions,

```scala
scala> def pairApply(f: Set ~> Option) = (f(Set(1, 2, 3)), f(Set('a', 'b', 'c')))
pairApply: (f: shapeless.poly.~>[Set,Option])(Option[Int], Option[Char])

scala> pairApply(choose)
res2: (Option[Int], Option[Char]) = (Some(1),Some(a))
```

They are nevertheless interoperable with ordinary monomorphic function values,

```scala
// choose is convertible to an ordinary monomorphic function value and can be
// mapped across an ordinary Scala List

scala> List(Set(1, 3, 5), Set(2, 4, 6)) map choose
res3: List[Option[Int]] = List(Some(1), Some(2))
```

However, they are [more general than natural transformations][polyblog2] and are able to capture type-specific cases
which, as we'll see below, makes them ideal for generic programming,

```scala
// size is a function from Ints or Strings or pairs to a 'size' defined
// by type specific cases

object size extends Poly1 {
  implicit def caseInt = at[Int](x => 1)
  implicit def caseString = at[String](_.length)
  implicit def caseTuple[T, U]
    (implicit st : Case.Aux[T, Int], su : Case.Aux[U, Int]) =
      at[(T, U)](t => size(t._1)+size(t._2))
}

scala> size(23)
res4: Int = 1

scala> size("foo")
res5: Int = 3

scala> size((23, "foo"))
res6: Int = 4

scala> size(((23, "foo"), 13))
res7: Int = 5
```

[polyblog1]: http://www.chuusai.com/2012/04/27/shapeless-polymorphic-function-values-1/
[nattrans]: http://en.wikipedia.org/wiki/Natural_transformation
[polyblog2]: http://www.chuusai.com/2012/05/10/shapeless-polymorphic-function-values-2/

### Heterogenous lists

shapeless provides a comprehensive Scala `HList` which has many features not shared by other HList implementations.

It has a `map` operation, applying a polymorphic function value across its elements. This means that it subsumes both
typical `HList`'s and also `KList`'s (`HList`'s whose elements share a common outer type constructor).
      
```scala
import poly._

// The same definition of choose as above
object choose extends (Set ~> Option) {
  def apply[T](s : Set[T]) = s.headOption
}

scala> val sets = Set(1) :: Set("foo") :: HNil
sets: Set[Int] :: Set[String] :: HNil = Set(1) :: Set(foo) :: HNil

scala> val opts = sets map choose   // map selects cases of choose for each HList element
opts: Option[Int] :: Option[String] :: HNil = Some(1) :: Some(foo) :: HNil
```

It also has a `flatMap` operation,

```scala
import poly.identity

scala> val l = (23 :: "foo" :: HNil) :: HNil :: (true :: HNil) :: HNil
l: ((Int :: String :: HNil) :: HNil :: (Boolean :: HNil) :: HNil
   = (23 :: foo :: HNil) :: HNil :: (true :: HNil) :: HNil

scala> l flatMap identity
res0: Int :: String :: Boolean :: HNil = 23 :: foo :: true :: HNil
```

It has a set of fully polymorphic fold operations which take a polymorphic binary function value. The fold is sensitive
to the static types of all of the elements of the `HList`. Given the earlier definition of size,
  
```scala
object addSize extends Poly2 {
  implicit  def default[T](implicit st: size.Case.Aux[T, Int]) =
    at[Int, T]{ (acc, t) => acc+size(t) }
}

scala> val l = 23 :: "foo" :: (13, "wibble") :: HNil
l: Int :: String :: (Int, String) :: HNil = 23 :: foo :: (13,wibble) :: HNil

scala> l.foldLeft(0)(addSize)
res1: Int = 11
```

It also has a zipper for traversal and persistent update,
    
```scala
import Zipper._

scala> val l = 1 :: "foo" :: 3.0 :: HNil
l: Int :: String :: Double :: HNil = 1 :: foo :: 3.0 :: HNil

scala> l.toZipper.right.put(("wibble", 45)).reify
res0: Int :: (String, Int) :: Double :: HNil = 1 :: (wibble,45) :: 3.0 :: HNil

scala> l.toZipper.right.delete.reify
res1: Int :: Double :: HNil = 1 :: 3.0 :: HNil

scala> l.toZipper.last.left.insert("bar").reify
res2: Int :: String :: String :: Double :: HNil = 1 :: foo :: bar :: 3.0 :: HNil
```

It is is covariant,

```scala
trait Fruit
case class Apple() extends Fruit
case class Pear() extends Fruit

type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
type APAP = Apple :: Pear :: Apple :: Pear :: HNil

val a : Apple = Apple()
val p : Pear = Pear()

val apap : APAP = a :: p :: a :: p :: HNil
val ffff : FFFF = apap  // APAP <: FFFF 
```

And it has a `unify` operation which converts it to an `HList` of elements of the least upper bound of the original
types,
      
```scala
scala> apap.unify
res0: Fruit :: Fruit :: Fruit :: Fruit :: HNil = Apple() :: Pear() :: Apple() :: Pear() :: HNil
```

It supports conversion to an ordinary Scala `List` of elements of the least upper bound of the original types,
      
```scala
scala> apap.toList
res0: List[Fruit] = List(Apple(), Pear(), Apple(), Pear())
```
      
And it has a `Typeable` type class instance (see below), allowing, eg. vanilla `List[Any]`'s or `HList`'s with elements
of type `Any` to be safely cast to precisely typed `HList`'s.

```scala
import syntax.typeable._

scala> val ffff : FFFF = apap.unify
ffff: FFFF = Apple() :: Pear() :: Apple() :: Pear() :: HNil

scala> val precise: Option[APAP] = ffff.cast[APAP]
precise: Option[APAP] = Some(Apple() :: Pear() :: Apple() :: Pear() :: HNil)
```
      
These last three features make this `HList` dramatically more practically useful than `HList`'s are typically thought to
be: normally the full type information required to work with them is too fragile to cross subtyping or I/O boundaries.
This implementation supports the discarding of precise information where necessary (eg. to serialize a precisely typed
record after construction), and its later reconstruction (eg. a weakly typed deserialized record with a known schema can
have it's precise typing reestabilished).

### HList-style operations on standard Scala tuples

shapeless allows standard Scala tuples to be manipulated in exactly the same ways as `HList`s,

```scala
import syntax.std.tuple._

// head, tail, take, drop, split
scala> (23, "foo", true).head
res0: Int = 23

scala> (23, "foo", true).tail
res1: (String, Boolean) = (foo,true)

scala> (23, "foo", true).drop(2)
res2: (Boolean,) = (true,)

scala> (23, "foo", true).take(2)
res3: (Int, String) = (23,foo)

scala> (23, "foo", true).split(1)
res4: ((Int,), (String, Boolean)) = ((23,),(foo,true))

// prepend, append, concatenate
scala> 23 +: ("foo", true)
res5: (Int, String, Boolean) = (23,foo,true)

scala> (23, "foo") :+ true
res6: (Int, String, Boolean) = (23,foo,true)

scala> (23, "foo") ++ (true, 2.0)
res7: (Int, String, Boolean, Double) = (23,foo,true,2.0)

// map, flatMap
import poly._

object option extends (Id ~> Option) {
  def apply[T](t: T) = Option(t)
}

scala> (23, "foo", true) map option
res8: (Option[Int], Option[String], Option[Boolean]) = (Some(23),Some(foo),Some(true))

scala> ((23, "foo"), (), (true, 2.0)) flatMap identity
res9: (Int, String, Boolean, Double) = (23,foo,true,2.0)

// fold (using previous definition of addSize
scala> (23, "foo", (13, "wibble")).foldLeft(0)(addSize)
res10: Int = 11

// conversion to `HList`s and ordinary Scala `List`s
scala> (23, "foo", true).productElements
res11: Int :: String :: Boolean :: HNil = 23 :: foo :: true :: HNil

scala> (23, "foo", true).toList
res12: List[Any] = List(23, foo, true)

// zipper
import Zipper._

scala> (23, ("foo", true), 2.0).toZipper.right.down.put("bar").root.reify
res13: (Int, (String, Boolean), Double) = (23,(bar,true),2.0)
```

### Facilities for abstracting over arity

Conversions between tuples and `HList`'s, and between ordinary Scala functions of arbitrary arity and functions which
take a single corresponding `HList` argument allow higher order functions to abstract over the arity of the functions
and values they are passed,

```scala
import syntax.std.function._
import ops.function._

def applyProduct[P <: Product, F, L <: HList, R](p: P)(f: F)
  (implicit gen: Generic.Aux[P, L], fp: FnToProduct.Aux[F, L => R]) =
    f.toProduct(gen.to(p))

scala> applyProduct(1, 2)((_: Int)+(_: Int))
res0: Int = 3

scala> applyProduct(1, 2, 3)((_: Int)*(_: Int)*(_: Int))
res1: Int = 6
```    

### Heterogenous maps

Shapeless provides a heterogenous map which supports an arbitrary relation between the key type and the corresponding
value type,
  
```scala
// Key/value relation to be enforced: Strings map to Ints and vice versa
class BiMapIS[K, V]
implicit val intToString = new BiMapIS[Int, String]
implicit val stringToInt = new BiMapIS[String, Int]

val hm = HMap[BiMapIS](23 -> "foo", "bar" -> 13)
//val hm2 = HMap[BiMapIS](23 -> "foo", 23 -> 13)   // Does not compile

scala> hm.get(23)
res0: Option[String] = Some(foo)

scala> hm.get("bar")
res1: Option[Int] = Some(13)
```

And in much the same way that an ordinary monomorphic Scala map can be viewed as a monomorphic function value, so too
can a heterogenous shapeless map be viewed as a polymorphic function value,

```scala
scala> import hm._
import hm._

scala> val l = 23 :: "bar" :: HNil
l: Int :: String :: HNil = 23 :: bar :: HNil

scala> l map hm
res2: String :: Int :: HNil = foo :: 13 :: HNil
```

### Singleton-typed literals

Although Scala's typechecker has always represented singleton types for literal values internally, there has not
previously been syntax available to express them, other than by [modifying the compiler][literaltype]. shapeless adds
support for singleton-typed literals via implicit macros.

Singleton types bridge the gap between the value level and the type level and hence allow the exploration in Scala of
techniques which would typically only be available in languages with support for full-spectrum dependent types. The
latest iteration of shapeless records (see next bullet) makes a start on that. Another simpler application is the use of
`Int` literals to index into `HList`s and tuples,

```scala
import syntax.std.tuple._

scala> val l = 23 :: "foo" :: true :: HNil
l: Int :: String :: Boolean :: HNil = 23 :: foo :: true :: HNil

scala> l(1)
res0: String = foo

scala> val t = (23, "foo", true)
t: (Int, String, Boolean) = (23,foo,true)

scala> t(1)
res1: String = foo
```

The [examples in the tests][singletons] and the following illustrate other possibilities,

```scala
scala> import shapeless._, syntax.singleton._
import shapeless._
import syntax.singleton._

scala> 23.narrow
res0: Int(23) = 23

scala> "foo".narrow
res1: String("foo") = foo

scala> val (wTrue, wFalse) = (Witness(true), Witness(false))
wTrue: shapeless.Witness{type T = Boolean(true)} = $1$$1@212b9eca
wFalse: shapeless.Witness{type T = Boolean(false)} = $2$$1@36c5f0c9

scala> type True = wTrue.T
defined type alias True

scala> type False = wFalse.T
defined type alias False

scala> trait Select[B] { type Out }
defined trait Select

scala> implicit val selInt = new Select[True] { type Out = Int }
selInt: Select[True]{type Out = Int} = $anon$1@2c7b5e2a

scala> implicit val selString = new Select[False] { type Out = String }
selString: Select[False]{type Out = String} = $anon$2@57632e36

scala> def select[T](b: WitnessWith[Select])(t: b.Out) = t
select: [T](b: shapeless.WitnessWith[Select])(t: b.Out)b.Out

scala> select(true)(23)
res2: Int = 23

scala> select(true)("foo")
<console>:18: error: type mismatch;
 found   : String("foo")
 required: Int
              select(true)("foo")
                           ^

scala> select(false)(23)
<console>:18: error: type mismatch;
 found   : Int(23)
 required: String
              select(false)(23)
                            ^

scala> select(false)("foo")
res5: String = foo
```

[literaltype]: http://existentialtype.net/2008/07/21/literally-dependent-types/
[singletons]: https://github.com/milessabin/shapeless/blob/master/core/src/test/scala/shapeless/singletons.scala


### Extensible records

shapeless provides an implementation of extensible records modelled as `HLists` of values tagged with the singleton
types of their keys. This means that there is no concrete representation needed at all for the keys. Amongst other
things this will allow subsequent work on `Generic` to map case classes directly to records with their member names
encoded in their element types.

```scala
import shapeless._ ; import syntax.singleton._ ; import record._

val book =
  ("author" ->> "Benjamin Pierce") ::
  ("title"  ->> "Types and Programming Languages") ::
  ("id"     ->>  262162091) ::
  ("price"  ->>  44.11) ::
  HNil

scala> book("author")  // Note result type ...
res0: String = Benjamin Pierce

scala> book("title")   // Note result type ...
res1: String = Types and Programming Languages

scala> book("id")      // Note result type ...
res2: Int = 262162091

scala> book("price")   // Note result type ...
res3: Double = 44.11

scala> book.keys       // Keys are materialized from singleton types encoded in value type
res4: String("author") :: String("title") :: String("id") :: String("price") :: HNil =
  author :: title :: id :: price :: HNil

scala> book.values
res5: String :: String :: Int :: Double :: HNil =
  Benjamin Pierce :: Types and Programming Languages :: 262162091 :: 44.11 :: HNil

scala> val newPrice = book("price")+2.0
newPrice: Double = 46.11

scala> val updated = book +("price" ->> newPrice)  // Update an existing field
updated: ... complex type elided ... =
  Benjamin Pierce :: Types and Programming Languages :: 262162091 :: 46.11 :: HNil

scala> updated("price")
res6: Double = 46.11

scala> val extended = updated + ("inPrint" ->> true)  // Add a new field
extended: ... complex type elided ... =
  Benjamin Pierce :: Types and Programming Languages :: 262162091 :: 46.11 :: true :: HNil

scala> val noId = extended - "id"  // Removed a field
noId: ... complex type elided ... =
  Benjamin Pierce :: Types and Programming Languages :: 46.11 :: true :: HNil

scala> noId("id")  // Attempting to access a missing field is a compile time error
<console>:25: error: could not find implicit value for parameter selector ...
              noId("id")
                  ^
```

Joni Freeman's ([@jonifreeman][jonifreeman]) [sqltyped][sqltyped] library [makes extensive use][sqltypedeg] of shapeless
records.

[recordgist]: https://gist.github.com/milessabin/6185537
[jonifreeman]: https://twitter.com/jonifreeman
[sqltyped]: https://github.com/jonifreeman/sqltyped 
[sqltypedeg]: https://github.com/jonifreeman/sqltyped/blob/shapeless_records/core/src/test/scala/examples.scala

### Coproducts

shapeless has a Coproduct type, a generalization of Scala's `Either` to an arbitrary number of choices. Currently it
exists primarily to support `Generic` (see the next section), but will be expanded analogously to `HList` in later
releases. Currently `Coproduct` supports mapping, selection and unification,

```scala
scala> type ISB = Int :+: String :+: Boolean :+: CNil
defined type alias ISB

scala> val isb = Coproduct[ISB]("foo")
isb: ISB = foo

scala> isb.select[Int]
res0: Option[Int] = None

scala> isb.select[String]
res1: Option[String] = Some(foo)

object size extends Poly1 {
  implicit def caseInt = at[Int](i => (i, i))
  implicit def caseString = at[String](s => (s, s.length))
  implicit def caseBoolean = at[Boolean](b => (b, 1))
}

scala> isb map size
res2: (Int, Int) :+: (String, Int) :+: (Boolean, Int) :+: CNil = (foo,3)

scala> res4.select[(String, Int)]
res3: Option[(String, Int)] = Some((foo,3))
```

### Generic representation of (sealed families of) case classes

The `Iso`s of earlier shapeless releases have been completely reworked as the new `Generic` type, which closely
resembles the [generic programming capabilities introduced to GHC 7.2][ghcgeneric].

`Generic[T]`, where `T` is a case class or an abstract type at the root of a case class hierarchy, maps between values
of `T` and a generic sum of products representation (`HList`s and `Coproduct`s),

```scala
scala> case class Foo(i: Int, s: String, b: Boolean)
defined class Foo

scala> val fooGen = Generic[Foo]
fooGen: shapeless.Generic[Foo]{ type Repr = Int :: String :: Boolean :: HNil } = $1$$1@724d2dfe

scala> val foo = Foo(23, "foo", true)
foo: Foo = Foo(23,foo,true)

scala> fooGen.to(foo)
res0: fooGen.Repr = 23 :: foo :: true :: HNil

scala> 13 :: res0.tail
res1: Int :: String :: Boolean :: HNil = 13 :: foo :: true :: HNil

scala> fooGen.from(res1)
res2: Foo = Foo(13,foo,true)
```

Typically values of `Generic` for a given case class are materialized using an implicit macro, allowing a wide variety
of [structural programming problems][deepsearch] to be solved with no or minimal boilerplate.  In particular the
existing [lens][lenses], [Scrap Your Boilerplate][sybclass] and [generic zipper][zipper] implementations are now
available for any case class family (recursive families included, as illustrated below) without any additional
boilerplate being required,

```scala
// Simple recursive case class family
sealed trait Tree[T]
case class Leaf[T](t: T) extends Tree[T]
case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]

// Polymorphic function which adds 1 to any Int and is the identity
// on all other values
object inc extends ->((i: Int) => i+1)

val tree: Tree[Int] =
  Node(
    Node(
      Node(
        Leaf(1),
        Node(
          Leaf(2),
          Leaf(3)
        )
      ),
      Leaf(4)
    ),
    Node(
      Leaf(5),
      Leaf(6)
    )
  )

// Transform tree by applying inc everywhere
everywhere(inc)(tree)

// result:
//   Node(
//     Node(
//       Node(
//         Leaf(2),
//         Node(
//           Leaf(3),
//           Leaf(4)
//         )
//       ),
//       Leaf(5)
//     ),
//     Node(
//       Leaf(6),
//       Leaf(7)
//     )
//   )
```

[ghcgeneric]: http://www.haskell.org/haskellwiki/GHC.Generics
[sybclass]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/sybclass.scala
[zipper]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/zipper.scala
[lenses]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/lenses.scala
[deepsearch]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/deepsearch.scala

### Boilerplate-free lenses for arbitrary case classes

A combination of `Generic` and singleton-typed `Int` literals supports boilerplate-free lens creation for arbitrary case
classes,

```scala
import shapeless._

// A pair of ordinary case classes ...
case class Address(street : String, city : String, postcode : String)
case class Person(name : String, age : Int, address : Address)

// Some lenses over Person/Address ...
val nameLens     = Lens[Person] >> 0
val ageLens      = Lens[Person] >> 1
val addressLens  = Lens[Person] >> 2
val streetLens   = Lens[Person] >> 2 >> 0
val cityLens     = Lens[Person] >> 2 >> 1
val postcodeLens = Lens[Person] >> 2 >> 2

scala> val person = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))
person: Person = Person(Joe Grey,37,Address(Southover Street,Brighton,BN2 9UA))

scala> val age1 = ageLens.get(person)               // Read field, note inferred type
age1: Int = 37

scala> val person2 = ageLens.set(person)(38)        // Update field
person2: Person = Person(Joe Grey,38,Address(Southover Street,Brighton,BN2 9UA))

scala> val person3 = ageLens.modify(person2)(_ + 1) // Transform field
person3: Person = Person(Joe Grey,39,Address(Southover Street,Brighton,BN2 9UA))

scala> val street = streetLens.get(person3)         // Read nested field
street: String = Southover Street

scala> val person4 = streetLens.set(person3)("Montpelier Road")  // Update nested field
person4: Person = Person(Joe Grey,39,Address(Montpelier Road,Brighton,BN2 9UA))
```

### Automatic type class instance derivation

Based on and extending `Generic`, Lars Hupel ([@larsr_h][larsh])has contributed the `TypeClass` type class, which provides automatic type
class derivation facilities roughly equivalent to those available with GHC as described in ["A Generic Deriving
Mechanism for Haskell"][genericderiving].  There is a description of the Scala mechanism [here][larshderiving], and
examples of its use deriving `Show` and `Monoid` instances [here][show] and [here][monoid]. In the `Monoid` case, once
the general deriving infrastructure for monoids is in place, instances are automatically available for arbitrary case
classes without any additional boilerplate,

```scala
import MonoidSyntax._
import Monoid.auto._

// A pair of arbitrary case classes
case class Foo(i : Int, s : String)
case class Bar(b : Boolean, s : String, d : Double)

scala> Foo(13, "foo") |+| Foo(23, "bar")
res0: Foo = Foo(36,foobar)

scala> Bar(true, "foo", 1.0) |+| Bar(false, "bar", 3.0)
res1: Bar = Bar(true,foobar,4.0)
```

The [shapeless-contrib][contrib] project also contains automatically derived type class instances for
[Scalaz][tcscalaz], [Spire][tcspire] and [Scalacheck][tcscalacheck].

  [larsh]: https://twitter.com/larsr_h
  [genericderiving]: http://dreixel.net/research/pdf/gdmh.pdf
  [larshderiving]: http://typelevel.org/blog/2013/06/24/deriving-instances-1.html
  [show]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/shows.scala
  [monoid]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/monoids.scala
  [contrib]: https://github.com/typelevel/shapeless-contrib
  [tcscalaz]: https://github.com/typelevel/shapeless-contrib/blob/master/scalaz/main/scala/typeclass.scala
  [tcspire]: https://github.com/typelevel/shapeless-contrib/blob/master/spire/main/scala/typeclass.scala
  [tcscalacheck]: https://github.com/typelevel/shapeless-contrib/blob/master/scalacheck/main/scala/package.scala

### Collections with statically know sizes

shapeless provides collection types with statically known sizes. These can prevent runtime errors such as those that
would result from attempting to take the head of an empty list, and can also verify more complex relationships. 

In the example below we define a method `csv` whose signature guarantees at compile time that there are exactly as many
column headers provided as colums,

```scala
def row(cols : Seq[String]) = cols.mkString("\"", "\", \"", "\"")
def csv[N <: Nat]
  (hdrs : Sized[Seq[String], N],
   rows : List[Sized[Seq[String], N]]) = row(hdrs) :: rows.map(row(_))

val hdrs = Sized("Title", "Author")
val rows = List(
  Sized("Types and Programming Languages", "Benjamin Pierce"),
  Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones")
)

// hdrs and rows statically known to have the name number of columns
val formatted = csv(hdrs, rows)                        // Compiles

// extendedHdrs has the wrong number of columns for rows
val extendedHdrs = Sized("Title", "Author", "ISBN")
val badFormatted = csv(extendedHdrs, rows)             // Does not compile
```

### Type safe cast

shapeless provides a `Typeable` type class which provides a type safe cast operation. `cast` returns an `Option` of the
target type rather than throwing an exception if the value is of the incorrect type, as can happen with separete
`isInstanceOf` and `asInstanceOf` operations. `Typeable` handles primitive values correctly and will recover erased
types in many circumstances,

```scala
import syntax.typeable._

val l: Any = List(Vector("foo", "bar", "baz"), Vector("wibble"))
l: Any = List(Vector(foo, bar, baz), Vector(wibble))

scala> l.cast[List[Vector[String]]]
res0: Option[List[Vector[String]]] = Some(List(Vector(foo, bar, baz), Vector(wibble)))

scala> l.cast[List[Vector[Int]]]
res1: Option[List[Vector[Int]]] = None

scala> l.cast[List[List[String]]]
res2: Option[List[List[String]]] = None
```

### Testing for non-compilation

Libraries like shapeless which make extensive use of type-level computation and implicit resolution often need to
provide guarantees that certain expressions _don't_ typecheck. Testing these guarantees is supported in shapeless via
the `illTyped` macro,

```scala
import shapeless.test.illTyped

scala> illTyped { """1+1 : Boolean""" }

scala> illTyped { """1+1 : Int""" }
<console>:19: error: Type-checking succeeded unexpectedly.
Expected some error.
       illTyped { """1+1 : Int""" }
                ^
```
