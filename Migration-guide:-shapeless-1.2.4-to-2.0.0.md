<!-- --- title: Migration guide: shapeless 1.2.4 to 2.0.0 -->

Details on the changes required to update your code follow. You might also find the complete set of differences in
shapeless's examples and tests between 1.2.4 and 2.0.0 a useful guide to what's involved in updating. They and can be
found in [this commit][delta].

[delta]: https://github.com/milessabin/shapeless/commit/d33938d65acb2aafae7b89df9dd52a955a0c9c8c

### 1. Required Scala version is now 2.10.2 or later

The advantages offered by the availability of implicit macros in Scala 2.10.2 and Scala 2.11.0 have proved to be so
significant that all current and future shapeless development has committed to them.

Note that releases prior to 2.10.2 in the 2.10.x series don't include the relevant [implicit macro bugfix][macrofix], so
relying on major release binary compatibility isn't sufficient: you will need to specify Scala version 2.10.2 and
depend on shapeless using the full cross version,

```scala
scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full
  // "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1" // Alternatively ...
)
```

Regrettably this means that shapeless-1.2.4 will very likely be the last release for Scala 2.9.x. It might be possible
to backport some of the more cosmetic changes to Scala 2.9.x, but before going down that route I'd like to gauge the
level of interest in continued support for older Scala releases. It might also be feasible to backport some, possibly
all, of the major shapeless-2.0.0 updates via a compiler plugin for Scala 2.9.x. Anyone interested in contributing or
sponsoring such work should [get in touch with me](mailto:miles@milessabin.com).

[macrofix]: https://issues.scala-lang.org/browse/SI-5923

### 2. Iso is now Generic

The `Iso` type class from shapeless-1.2.4 has evolved significantly in shapeless-2.0.0. Previously it purported to
represent an arbitrary isomorphism between a pair of types. In practice however, its almost exclusive use was to map
case classes onto a generic product type (ie. `HList`)representation and back again. In shapeless-2.0.0 this slightly
less general use than arbitrary isomorphism has been hightlighted by the type class being renamed to `Generic`. If you
need a type class representing arbitrary isomorphism you should use Scalaz's `Iso` instead.

`Generic` has also been generalized in that as well as providing a generic representation of individual case classes it
can also represent sealed families of case classes via a sum of products representation (ie. a `Coproduct` of `HList`s)
meaning that it now closely resembles the [generic programming capabilities introduced to GHC 7.2][ghcgeneric]. Values
of `Generic` for a given type are materialized using an implicit macro, eliminating all the already minimal boilerplate
associated with the earlier `Iso` type class.

In most cases migrating from `Iso` to `Generic` involves,

* Replacing `Iso` with `Generic` throughout.

* Removing all implicit publications of `Iso` instances, ie.,

```scala
import shapeless._

case class Foo(i: Int, s: String)
val foo = Foo(23, "bar")

// shapeless 1.2.4

def hd[T, L <: HList](t: T)(implicit iso: Iso[T, L], ihc: IsHCons[L]) = iso.to(t).head

implict val fooIso = Iso.hlist(Foo.apply _, Foo.unapply)

hd(foo) // 23

// shapeless 2.0.0

def hd[T, L <: HList](t: T)(implicit gen: Generic.Aux[T, L], ihc: IsHCons[L]) = iso.to(t).head

hd(foo) // 23
```

Note that due to limitations with implicit macros in Scala 2.10.2 the representation type must be treated only as an
output and cannot be used to contrain the mapping. This means that constructs like the following, which were valid with
shapeless 1.2.4,

```scala
case class Foo(i: Int, s: String)

implicit val fooIso = Iso.hlist(Foo.apply _, Foo.unapply _)

def makeFoo[L <: HList](l: L)(implicit fooIso: Iso[Foo, L]): Foo = fooIso.from(l)

makeFoo(23 :: "bar" :: HNil) // Foo(23, "bar")
```

will fail if updated naively for shapeless 2.0.0,

```scala
def makeFoo[L <: HList](l: L)(implicit fooGen: Generic.Aux[Foo, L]): Foo = fooGen.from(l)

makeFoo(23 :: "bar" :: HNil)
// error: could not find implicit value for
//   parameter fooGen: Generic.Aux[Foo, Int :: String :: HNil]
```

because here the representation type `L` is acting as a constraint on the implicit resolution of `fooGen` as well as
being an output of that resolution. This limitation will go away in Scala 2.11 as a result of the fix for [SI-7470][],
and in the meantime can be worked around by expressing the constraint via an explicit type equality proof,

```scala
def makeFoo[L <: HList, M <: HList](l: L)
  (implicit fooGen: Generic.Aux[Foo, M], eq: L =:= M): Foo = fooGen.from(l)

makeFoo(23 :: "bar" :: HNil) // Foo(23, "bar")
```

Here the representation type `M` is used only as an output of the resolution of the `Generic` implicit and the equality
between `M` and the argument type `L` is provided by the subsequent implicit witness of `L =:= M`.

[ghcgeneric]: http://www.haskell.org/haskellwiki/GHC.Generics
[SI-7470]: https://issues.scala-lang.org/browse/SI-7470

### 3. Type class Aux suffix convention change

The previous shapeless convention of providing two variants of all type classes, one with result types as members and
one (with an `Aux` suffix) with result types as additional type parameters, has been refined. Now only the definition
with type members is provided as a first-class trait or class, and the additional type parameter variant is defined via
a type alias in the former's companion object.

Hence the pattern has changed as follows,

| shapeless-1.2.4                          | shapeless-2.0.0                           |
|:-----------------------------------------|:------------------------------------------|
| <code><i>TypeClass</i>[T, ...]</code>    | <code><i>TypeClass</i>[T, ...]</code>     |
| <code><i>TypeClass</i>Aux[T, ...]</code> | <code><i>TypeClass</i>.Aux[T, ...]</code> | 

This results in a significantly smaller number of class files and also simplifies implicit resolution in some cases.

```scala
def unzip[L <: HList, OutM <: HList, OutT <: HList](l : L)
  (implicit
    mapper     : Mapper.Aux[productElements.type, L, OutM], // was 'MapperAux'
    transposer : Transposer.Aux[OutM, OutT],                // was 'TransposerAux'
    tupler     : Tupler[OutT]) =
  l.map(productElements).transpose.tupled
```

### 4. Import HList type classes from ops.hlist

Witnesses for `HList` operations are no longer direct members of the `shapeless` package and must be imported
explicitly,

```scala
import shapeless._
import ops.hlist.Prepend // New import

def usePrepend[L <: HList, M <: HList](l: L, m: M)(implicit prepend: Prepend[L, M]) = l ++ m
```

### 5. productElements is the new name for hlisted

The extension method to convert a tuple or a case class to an `HList` is now `productElements` instead of `hlisted` and
the syntax object must be imported,

```scala
import shapeless._
import syntax.std.product._  // New import

scala> (23, "foo", true).productElements // was '.hlisted'
res0: Int :: String :: HNil = 23 :: foo :: true :: HNil

scala> case class Foo(i: Int, s: String)
defined class Foo

scala> val foo = Foo(23, "bar")
foo: Foo = Foo(23,bar)

scala> foo.productElements
res1: Int :: String :: HNil = 23 :: bar :: HNil
```

### 6. Conversions between functions and HList functions

The type classes and extension methods for converting between functions of multiple arguments and functions with a
single `HList` argument have been renamed and the syntax and/or ops object must be imported from.

The renamings are as follows,

| shapeless-1.2.4  | shapeless-2.0.0   | &nbsp; 
|:-----------------|:------------------|--------------------
| `hlisted`        | `toProduct`       | _Extension methods_
| `unhlisted`      | `fromProduct`     |
| `FnHLister`      | `ToProduct`       | _Type classes_
| `FnHListerAux`   | `ToProduct.Aux`   |
| `FnUnHLister`    | `FromProduct`     |
| `FnUnHListerAux` | `FromProduct.Aux` |

```scala
import shapeless._
import syntax.std._ // New import

scala> def sum3(a: Int, b: Int, c: Int) = a+b+c
sum3: (a: Int, b: Int, c: Int)Int

scala> (sum3 _).toProduct  // was '.hlisted'
res0: (Int :: Int :: Int :: HNil) => Int = <function1>

import ops.function._ // New import

scala> def applyL[L <: HList, F, R](l: L, f: F)
     |   (implicit f2p: FnToProduct.Aux[F, L => R]) = f.toProduct(l) // was 'FnHListerAux'
applyL: [L <: HList, F, R](l: L, f: F)(implicit f2p: FnToProduct.Aux[F,L => R])R

scala> applyL(2 :: 3 :: 5 :: HNil, sum3 _)
res1: Int = 10
```

### 7. HList zipped/unzipped now zip/unzip

The introduction of `HList`-like operations directly on tuples revealed an unfortunate name clash between the `zipped`
operation on tuples, which zips multiple collections, and the shapeless operation, which zips the tuples themselves.
Consequently the shapeless operation has been named `zip`, it's converse `unzip` and the `HList` operations have been
renamed accordingly for consistency,

```scala
scala> import shapeless._
import shapeless._

scala> val l1 = 1 :: 2 :: 3 :: HNil
l1: Int :: Int :: Int :: HNil = 1 :: 2 :: 3 :: HNil

scala> val l2 = 23 :: "foo" :: true :: HNil
l2: Int :: String :: Boolean :: HNil = 23 :: foo :: true :: HNil

scala> l1 zip l2  // was 'zipped'
res0: (Int, Int) :: (Int, String) :: (Int, Boolean) :: HNil = (1,23) :: (2,foo) :: (3,true) :: HNil
```
### 8. Poly and Case naming convention change

The `Poly` and `Case` name conventions have been aligned with the new shapeless type class `Aux` suffix convention.

The new pattern is as follows (where `L <: HList`, `f: Poly`, <code>f<i>n</i>: Poly<i>N</i></code>),

| shapeless-1.2.4                                  | shapeless-2.0.0                                  | &nbsp;
|:-------------------------------------------------|:-------------------------------------------------|---------------------------
| <code>CaseAux[f.type, L]</code>                  | <code>Case[f.type, L]                            | _Standalone cases_ 
| <code>Pullback[f.type, L, R]</code>              | <code>Case.Aux[f.type, L, R]</code>              |
| <code>Case<i>N</i>Aux[f.type, T, ...]</code>     | <code>Case<i>N</i>[f.type, T, ...]</code>        |
| <code>Pullback<i>N</i>[f.type, T, ..., R]</code> | <code>Case<i>N</i>.Aux[f.type, T, ..., R]</code> |
| <code>f.Case[L]</code>                           | <code>f.ProductCase[L]</code>                    | _Instance dependent cases_
| <code>f.Pullback[T, L, R]</code>                 | <code>f.ProductCase.Aux[L, R]</code>             | 
| <code>f.Case<i>N</i>[T, ...]</code>              | <code>f<i>n</i>.Case[T, ...]</code>              |
| <code>f.Pullback<i>N</i>[T, ..., R]</code>       | <code>f<i>n</i>.Case.Aux[T, ..., R]</code>       |

This is more consistent with other uses of the `Aux` suffix in shapeless, results in a significantly smaller number of
class files, simplifies implicit resolution in some cases, and is slightly terser in typical cases.

```scala
// shapeless-1.2.4
def pairApply(f: Poly)(implicit ci : f.Case1[Int], cs : f.Case1[String]) = (f(23), f("bar"))

// shapeless-2.0.0
def pairApply(f: Poly1)(implicit ci : f.Case[Int], cs : f.Case[String]) = (f(23), f("bar"))
```

### 9. Import Cases from poly, miscellaneous Polys moved.

Standalone cases of polymorphic functions are no longer direct members of the `shapeless` package and must be imported
explicitly,

```scala
import shapeless._
import poly._ // New import

object inc extends Poly1 {
  implicit def caseInt = at[Int](_+1)
}

def usePoly[T](t: T)(implicit cse: Case1[inc.type, T]) = cse(t)
```

With the exception of `identity` the miscellaneous collection of example Polys in *poly.scala* should never have been
included in the main shapeless namespace. `identity` has now been moved to `poly` and hence must be imported explicitly.
The other Polys have been moved to tests or examples.

```scala
scala> import shapeless._, syntax.std.tuple._
import shapeless._
import syntax.std.tuple._

scala> import poly._  // New import
import poly._

scala> ((23, "foo"), (), (true, 2.0)) flatMap identity
res0: (Int, String, Boolean, Double) = (23,foo,true,2.0)
```

### 10. Record changes

`Field[T]` has been renamed to `FieldOf[T]` and fields are now constructed using the `->>` operator which is made
available on values by importing from `shapeless.syntax.singleton`,

```scala
scala> import shapeless._, record._, singleton._
import shapeless._
import record._
import singleton._

scala> object foo extends FieldOf[Int]     // was 'Field'
defined module foo

scala> object bar extends FieldOf[String]  // was 'Field'
defined module bar

scala> val r = (foo ->> 23) :: (bar ->> "baz") :: HNil  // was '->'
r: FieldType[foo.type,Int] :: FieldType[bar.type,String] :: HNil = 23 :: baz :: HNil
```

Also note that the `FieldEntry` type alias has been renamed to `FieldType` and requires an explicit value type to be
provided as a second type argument. Whilst this makes sense for the newly supported singleton typed literal keys which
don't inherently encode their value types, this is perhaps less suitable for `FieldOf` keys which do. This change might
be reconsidered for M2 ... feedback welcome.

### 11. TypeOperators object removed, most used members moved to shapeless package object

The most used members of `TypeOperators`, eg. `Id` and `Const`, have been moved to the shapeless package object and are
now available without prefix whenever the shapeless package has been imported from. All imports from `TypeOperators`
should be removed.

### 12. Tagged types and NewType have moved

Tagged types and `NewType` are now accessible via the `tag` and `newtype` objects respectively,

```scala
scala> import shapeless._
import shapeless._

scala> import tag._
import tag._

scala> trait Foo
defined trait Foo

scala> tag[Foo](23)
res0: shapeless.tag.@@[Int,Foo] = 23

scala> import newtype._
import newtype._

scala> implicit class MyStringOps(s: String) { def mySize = s.size }
defined class MyStringOps

scala> type MyString = Newtype[String, MyStringOps]
defined type alias MyString

scala> def MyString(s : String) : MyString = newtype(s)
MyString: (s: String)MyString

scala> val ms = MyString("Hello world!")
ms: MyString = Hello world!

scala> ms.mySize
res1: Int = 12

scala> ms.size
<console>:21: error: value size is not a member of MyString
              ms.size
                 ^
```

It should be noted that, whilst there are still uses for tagged types, most, if not all, uses of newtype can be replaced
by Scala value classes.

### 13. Nat types and constants now imported from nat, type classes from ops.nat

The `Nat` types and constants `_0`, `_1`, ... etc. were previously imported from the `Nat` object. This is now named 
`nat`. The `Nat` type classes and instances have been moved from the shapeless package to `shapeless.ops.nat`,

```scala
scala> import shapeless._
import shapeless._

scala> import nat._      // Was 'Nat'
import nat._

scala> import ops.nat._  // New import
import ops.nat._

scala> implicitly[Sum.Aux[_2, _3, _5]]
res0: Sum.Aux[_2, _3, _5] = shapeless.ops.nat$Sum$$anon$3@e037c82
```

### 14. Sized type and extension methods moved

The `Sized` type is now defined directly in the shapeless namespace and the extension methods for Scala collection types
have been moved to `syntax.sized`.

```scala
scala> import shapeless._
import shapeless._

scala> import syntax.sized._  // New import
import syntax.sized._

scala> val Some(l) = List(1, 2, 3).sized(3)
l: Sized[List[Int],_3] = Sized@17191095

scala> l.splitAt(1)
res0: (Sized[List[Int],_1], Sized[List[Int],_2]) = (Sized@73f49b57,Sized@88453718)

scala> l.drop(1)
res1: Sized[List[Int],_2]] = Sized@88453718

scala> l.take(4)  // Doesn't compile
<console>:15: error: ...
                                  
```

### 15. The Typeable extension methods have moved.

The `Typeable` extension methods have moved to `syntax.typeable`,

```scala
scala> import shapeless._
import shapeless._

scala> import syntax.typeable._  // New import
import syntax.typeable._

scala> ("Hello world!" : Any).cast[String]
res0: Option[String] = Some(Hello world!)
```

