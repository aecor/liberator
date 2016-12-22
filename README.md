Liberator - sent to make you Free
---

The goal of this library is to generate everything you need to create programs using Free monad, without boilerplate

Example
```scala
@free
trait KeyValueStore[F[_]] {
  def setValue(key: String, value: String): F[Unit]

  def getValue(key: String): F[Option[String]]
}
```

this will be expanded at compile time to this (desanitized for brevity) 
```scala
trait KeyValueStore[F[_]] {
  def setValue(key: String, value: String): F[Unit]
  def getValue(key: String): F[Option[String]]
}
object KeyValueStore {
  def apply[F[_]](implicit instance: KeyValueStore[F]): KeyValueStore[F] = instance
  sealed abstract class KeyValueStoreFree[A] extends Product with Serializable
  object KeyValueStoreFree {
    final case class SetValue(key: String, value: String) extends KeyValueStoreFree[Unit]
    final case class GetValue(key: String) extends KeyValueStoreFree[Option[String]]
  }
  def fromFunctionK[F[_]](f: KeyValueStoreFree ~> F]): KeyValueStore[F] = new KeyValueStore[F] {
    def setValue(key: String, value: String): F[Unit] = f(KeyValueStoreFree.SetValue(key, value))
    def getValue(key: String): F[Option[String]] = f(KeyValueStoreFree.GetValue(key))
  }
  def toFunctionK[F[_]](ops: KeyValueStore[F]): KeyValueStoreFree ~> F = new (KeyValueStoreFree ~> F) {
    def apply[A](op: KeyValueStoreFree[A]): F[A] = op match {
      case KeyValueStoreFree.SetValue(key, value) => ops.setValue(key, value)
      case KeyValueStoreFree.GetValue(key) => ops.getValue(key)
    }
  }
  type FreeHelper1[F[_]] = { type Out[A] = _root_.cats.free.Free[F, A] } //workaround of a bug in scala.meta
  implicit def freeInstance[F[_]](implicit inject: Inject[KeyValueStoreFree, F]): KeyValueStore[FreeHelper1[F]#Out] = 
    fromFunctionK(new (KeyValueStoreFree ~> FreeHelper1[F]#Out) { 
      def apply[A](op: KeyValueStoreFree[A]): Free[F, A] = Free.inject(op) 
    })
  implicit val freeAlgebra: FreeAlgebra.Aux[KeyValueStore, KeyValueStoreFree] = 
    new FreeAlgebra[KeyValueStore] {
      type Out[A] = KeyValueStoreFree[A]
      override def apply[F[_]](of: KeyValueStore[F]): KeyValueStoreFree ~> F = KeyValueStore.toFunctionK(of)
    }
}

```