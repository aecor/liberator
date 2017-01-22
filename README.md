# Liberator - sent to make you Free

[![Build Status](https://img.shields.io/travis/aecor/liberator/master.svg)](https://travis-ci.org/aecor/liberator)
[![Maven Central](https://img.shields.io/maven-central/v/io.aecor/liberator_2.11.svg)](https://github.com/aecor/liberator)
[![Join the chat at https://gitter.im/aecor/liberator](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/aecor/liberator)

The goal of this library is to generate everything you need to create programs using Free monad, without boilerplate.

It is built using [scala.meta](http://scalameta.org), [Cats](https://github.com/typelevel/cats) and a bit of [Shapeless](https://github.com/milessabin/shapeless).

### Using Liberator

Liberator supports only Scala 2.11 due to missing support of 2.12 from scala.meta paradise (subject to change very soon)

To start using Liberator add the following to your `build.sbt` file:

```scala
scalaOrganization := "org.typelevel"
libraryDependencies += "io.aecor" %% "liberator" % "0.2.0"
scalacOptions += "-Ypartial-unification"
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-beta4" cross CrossVersion.full)
```

### Usage example

```scala
import io.aecor.liberator.macros.free

@free
trait KeyValueStore[F[_]] {
  def setValue(key: String, value: String): F[Unit]

  def getValue(key: String): F[Option[String]]
}
```

The code above will be expanded at compile time to this (desanitized for brevity):
```scala
trait KeyValueStore[F[_]] {
  def setValue(key: String, value: String): F[Unit]
  def getValue(key: String): F[Option[String]]
}
object KeyValueStore {
  // A helper method to get an instance of KeyValueStore[F]
  def apply[F[_]](implicit instance: KeyValueStore[F]): KeyValueStore[F] = instance
  
  
  // A free AST
  sealed abstract class KeyValueStoreFree[A] extends Product with Serializable
  object KeyValueStoreFree {
    final case class SetValue(key: String, value: String) extends KeyValueStoreFree[Unit]
    final case class GetValue(key: String) extends KeyValueStoreFree[Option[String]]
  }
  
  // A function to convert a natural transformation to your trait
  def fromFunctionK[F[_]](f: KeyValueStoreFree ~> F]): KeyValueStore[F] = 
    new KeyValueStore[F] {
      def setValue(key: String, value: String): F[Unit] = 
        f(KeyValueStoreFree.SetValue(key, value))
        
      def getValue(key: String): F[Option[String]] = 
        f(KeyValueStoreFree.GetValue(key))
    }
    
    
  // A function to create a natural tranformation from your trait
  def toFunctionK[F[_]](ops: KeyValueStore[F]): KeyValueStoreFree ~> F = 
    new (KeyValueStoreFree ~> F) {
      def apply[A](op: KeyValueStoreFree[A]): F[A] = op match {
        case KeyValueStoreFree.SetValue(key, value) => ops.setValue(key, value)
        case KeyValueStoreFree.GetValue(key) => ops.getValue(key)
      }
    }
    
  type FreeHelper1[F[_]] = { type Out[A] = Free[F, A] } //workaround of a bug in scala.meta
  implicit def freeInstance[F[_]](implicit inject: Inject[KeyValueStoreFree, F]): KeyValueStore[FreeHelper1[F]#Out] = 
    fromFunctionK(new (KeyValueStoreFree ~> FreeHelper1[F]#Out) { 
      def apply[A](op: KeyValueStoreFree[A]): Free[F, A] = Free.inject(op) 
    })
    
  implicit val freeAlgebra: FreeAlgebra.Aux[KeyValueStore, KeyValueStoreFree] = 
    new FreeAlgebra[KeyValueStore] {
      type Out[A] = KeyValueStoreFree[A]
      override def apply[F[_]](of: KeyValueStore[F]): KeyValueStoreFree ~> F = 
        KeyValueStore.toFunctionK(of)
    }
}

```

Given all above you can write your programs like this

```scala
import io.aecor.liberator.macros.free
import io.aecor.liberator.{ FreeAlgebra, ProductKK }

@free
trait Logging[F[_]] {
  def debug(s: String): F[Unit]
}

def program[F[_]: Monad: KeyValueStore: Logging](key: String): F[String] =
    for {
      value <- KeyValueStore[F].getValue(key)
      _ <- Logging[F].debug(s"Got value $value")
      newValue = UUID.randomUUID().toString
      _ <- KeyValueStore[F].setValue(key, newValue)
      _ <- Logging[F].debug(s"Update value to $newValue")
    } yield newValue    

val freeAlgebra = FreeAlgebra[ProductKK[KeyValueStore, Logging, ?[_]]]

// Notice that you don't have to know anything about presence of AST

val freeProgram = program[Free[freeAlgebra.Out, ?]]("key")

val taskKeyValueStore: KeyValueStore[Task] = ???

val taskLogging: Logging[Task] = ???

val task = freeProgram.foldMap(freeAlgebra(ProductKK(taskKeyValueStore, taskLogging)))

task.runAsync // the only side-effecting call
```

### Known issues
- There is a possibility of type name collision if base trait contains abstract type named `F` and it is not last unary type constructor, e.g. `trait Foo[F, A, B[_]]`
