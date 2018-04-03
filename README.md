# Liberator - sent to make you Free

[![Build Status](https://img.shields.io/travis/aecor/liberator/master.svg)](https://travis-ci.org/aecor/liberator)
[![Maven Central](https://img.shields.io/maven-central/v/io.aecor/liberator_2.12.svg)](https://github.com/aecor/liberator)
[![Join the chat at https://gitter.im/aecor/liberator](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/aecor/liberator)

The goal of this library is to generate everything you need to create programs using Free monad or tagless algebras, without boilerplate.

It is built using [scala.meta](http://scalameta.org), [Cats](https://github.com/typelevel/cats) and a bit of [Shapeless](https://github.com/milessabin/shapeless).

### Using Liberator

Liberator is built for Scala 2.11 and 2.12.

To start using Liberator add the following to your `build.sbt` file:

```scala
scalaVersion := "2.11.11"
libraryDependencies += "io.aecor" %% "liberator" % "0.8.0"
scalacOptions += "-Ypartial-unification"
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full)
```

or

```scala
scalaVersion := "2.12.4"
libraryDependencies += "io.aecor" %% "liberator" % "0.8.0"
scalacOptions += "-Ypartial-unification"
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full)
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
  def fromFunctionK[F[_]](f: KeyValueStoreFree ~> F): KeyValueStore[F] = 
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

  implicit def freeInstance[F[_]](implicit inject: InjectK[KeyValueStoreOp, F]): KeyValueStore[Free[F, A]] =
    fromFunctionK(new (KeyValueStoreFree ~> Free[F, A]) {
      def apply[A](op: KeyValueStoreFree[A]): Free[F, A] = Free.inject(op) 
    })
    
  implicit val freeAlgebra: Algebra.Aux[KeyValueStore, KeyValueStoreOp] =
    new Algebra[KeyValueStore] {
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

### FunctorK

Liberator provides `@functorK` annotation.  
This macros generates [`FunctorK`](https://github.com/aecor/liberator/blob/master/macros/src/main/scala/io/aecor/liberator/FunctorK.scala) instance.  
Use case:
```scala
import io.aecor.liberator.macros.functorK
import io.aecor.liberator.syntax._

@functorK
trait Logging[F[_]] {
  def debug(s: String): F[Unit]
}

val fLogging: Logging[F] = ...
val f2g: F ~> G = ...
val gLogging: Logging[G] = fLogging.mapK(f2g) 

```

### ReifiedInvocations

Liberator provides `@reifyInvocations` annotation.
This macros generates [`ReifiedInvocations`](https://github.com/aecor/liberator/blob/master/macros/src/main/scala/io/aecor/liberator/ReifiedInvocations.scala) instance.
Use case:
```scala
import io.aecor.liberator.macros.reifyInvocations
import io.aecor.liberator.syntax._
import monix.eval.Task._

@reifyInvocations
trait KVS[F[_]] {
  def set(k: String, v: String): F[Unit]
  def get(k: String): F[Option[Unit]]
}

type Halt[F[_], A] = F[Unit]

object LoggingKVS extends KVS[Halt[Task, ?]] {
  def set(k: String, v: String): Task[Unit] = Task(println(s"Set $k to $v"))
  def get(k: String): Task[Unit] = Task(println(s"Get $k to $v"))
}

val realKVS: KVS[Task] = ...

val introspected = ReifiedInvocations[KVS].mapK {
  new (Invocation[KVS, ?] ~> Task) {
    def apply[A](invocation: Invocation[KVS, A]): Task[A] =
      invocation.invoke(LoggingKVS).flatMap(_ => invocation.invoke(realKVS))
  }
}

```

### Known issues
- There is a possibility of type name collision if base trait contains abstract type named `F` and it is not last unary type constructor, e.g. `trait Foo[F, A, B[_]]`
