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

The `?` syntax for type lambdas is provided by [kind-projector](https://github.com/non/kind-projector) compiler plugin.

```scala
import io.aecor.liberator.macros.free

@free
@algebra
trait KeyValueStore[F[_]] {
  def setValue(key: String, value: String): F[Unit]

  def getValue(key: String): F[Option[String]]
}

/*
 * We need an empty companion object as a temporary workaround
 * for https://github.com/scalameta/paradise/issues/176
 */
object KeyValueStore
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
  sealed abstract class KeyValueStoreOp[A] extends Product with Serializable
  object KeyValueStoreOp {
    final case class SetValue(key: String, value: String) extends KeyValueStoreOp[Unit]
    final case class GetValue(key: String) extends KeyValueStoreOp[Option[String]]
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

  implicit def freeInstance[F[_]](implicit inject: Inject[KeyValueStoreOp, F]): KeyValueStore[Free[F, A]] =
    fromFunctionK(new (KeyValueStoreFree ~> Free[F, A]) {
      def apply[A](op: KeyValueStoreFree[A]): Free[F, A] = Free.inject(op) 
    })
    
  implicit val freeAlgebra: Algebra.Aux[KeyValueStore, KeyValueStoreOp] =
    new FreeAlgebra[KeyValueStore] {
      type Out[A] = KeyValueStoreOp[A]
      override def apply[F[_]](of: KeyValueStore[F]): KeyValueStoreOp ~> F =
        KeyValueStore.toFunctionK(of)
    }
}

```

Given all above you can write your programs like this

```scala
import io.aecor.liberator.macros.free
import io.aecor.liberator.data.ProductKK
import io.aecor.liberator.Algebra

@free
@algebra
trait Logging[F[_]] {
  def debug(s: String): F[Unit]
}

object Logging

def program[F[_]: Monad: KeyValueStore: Logging](key: String): F[String] =
    for {
      value <- KeyValueStore[F].getValue(key)
      _ <- Logging[F].debug(s"Got value $value")
      newValue = UUID.randomUUID().toString
      _ <- KeyValueStore[F].setValue(key, newValue)
      _ <- Logging[F].debug(s"Update value to $newValue")
    } yield newValue    

val algebra = Algebra[ProductKK[KeyValueStore, Logging, ?[_]]]

// Notice that you don't have to know anything about presence of AST

val freeProgram = program[Free[algebra.Out, ?]]("key")

val taskKeyValueStore: KeyValueStore[Task] = ???

val taskLogging: Logging[Task] = ???

val task = freeProgram.foldMap(freeAlgebra(ProductKK(taskKeyValueStore, taskLogging)))

task.runAsync // the only side-effecting call
```

### Known issues
- There is a possibility of type name collision if base trait contains abstract type named `F` and it is not last unary type constructor, e.g. `trait Foo[F, A, B[_]]`
