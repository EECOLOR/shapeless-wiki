Aux
---

Exposing type members as type parameters.

``` scala
trait Last[L <: HList] { 
  type Out
  def apply(l:L):Out
}

object Last {
  type Aux[L <: HList, Out0] = Last[L] { type Out = Out0 }
}
```

This is useful when you need access to the result of an implicit within the same argument list.

``` scala
implicit def lastToString[L <: HList, O](l:L)(
  implicit last:Last.Aux[L, O], toString:ToString[O]):String = toString(last(l))
```
Commonly used for query style classes where you supply a type and the result is encoded as a type parameter.

Type factory
------------

Combining explicit and implicit type parameters to construct an instance.

``` scala
object Coproduct {
  class MkCoproduct[C <: Coproduct] {
    def apply[T](t: T)(implicit inj: Inject[C, T]): C = inj(t)
  }
 
  def apply[C <: Coproduct] = new MkCoproduct[C]
}
```

This is useful if not all of the types of your method can be inferred from the arguments.

```scala
Coproduct[String :+: CNil](42)
```

Constructor
-----------

Providing an easy way to get an instance.

```scala
object Typeable {
  def apply[T](implicit castT:Typeable[T]):Typeable[T] = castT
}
```

Saves the user some characters and improves the style of your library. 

``` scala
val t = implicitly[Typeable[String]]
// vs
val t = Typeable[String]
```

Providing type and value level versions
---------------------------------------

Help the user by providing a type and a value level version.

``` scala
sealed trait HList
final case class ::[+H, +T <: HList](head : H, tail : T) extends HList
sealed trait HNil extends HList {
  def ::[H](h : H) = shapeless.::(h, this)
}
case object HNil extends HNil
```

This allows the user to mirror types and values

```scala
val string = "test"
val l: String :: HNil = string :: HNil
```

Note that in the above case both `HNil` and `::` exist on both type and value level.

Type constants
--------------

Useful to define defaults

``` scala
trait Converter[T] {
  def convert(in: String): String
}
object Converter {
  // will only exist at type level, no instance can be created
  sealed trait UpperCase
  sealed trait LowerCase

  implicit object UpperCase extends Converter[UpperCase] {
    def convert(in: String) = in.toUpperCase
  }
  implicit object LowerCase extends Converter[LowerCase] {
    def convert(in: String) = in.toLowerCase
  }
}
```

Note that in Scala 2.11 you can also use type members

``` scala
object Converter {
  type UpperCase
  type LowerCase
}
```

Default type parameters
-----------------------

Allows type parameters to be optional providing a default value

``` scala
trait ConverterType[T]
object ConverterType {
  implicit def anyConverterType[T]: ConverterType[T] = null
  implicit object defaultConverterType extends ConverterType[Converter.UpperCase]
}

// ConverterType[T] acts as a magnet for type T
class UniversalConverter[T](implicit outputType: ConverterType[T], converter: Converter[T]) {
  def convert(in:String) = converter convert in
}
```

Gives the user of your library the chance to provide an alternative type to your sensible default

``` scala
val withUpperCaseConverter = new UniversalConverter
val withLowerCaseConverter = new UniversalConverter[Converter.LowerCase]

println(withUpperCaseConverter convert "TeSt") // TEST
println(withLowerCaseConverter convert "TeSt") // test
```

Infix notation
--------------

Can improve readability

```scala
type -[L <: HList, X] = Remove[L, X]

trait Remove[L <: HList, X] {
  type Out <: HList
}
```

In some cases the `[`, `,` and `]` clutter important stuff.

```scala
implicitly[L - String]
implicitly[L Remove String]
```


Type level recursion
--------------------

TODO

``` scala
object Select {
  implicit def atHead[X, T <: HList]:Select[X :: T, X] = null
    
  implicit def inTail[X, H, T <: HList](selectFromTail:Select[T, X]):Select[H :: T, X] = null
}
```

Note how the types lock it into place
