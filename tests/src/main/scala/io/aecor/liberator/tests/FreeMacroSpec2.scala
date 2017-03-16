package io.aecor.liberator.tests

import io.aecor.liberator.macros.FreeMacro
import org.scalatest.{ FlatSpec, Matchers }

import scala.meta._

class FreeMacroSpec2 extends FlatSpec with Matchers {

  "Free macro" should "use higher order type name from trait definition to avoid name collision" in {

    val defn =
      source"""
        trait KVS[K, V, Higher[_]] {
            def getValue(key: K): Higher[Option[V]]
          }
      """.collect {
        case t: Defn.Trait => t
      }.head

    val block = Term.Block(
      """
        |  trait KVS[K, V, Higher[_]] {
        |    def getValue(key: K): Higher[Option[V]]
        |  }
        |  object KVS {
        |    def apply[K, V, Higher[_]](implicit instance: KVS[K, V, Higher]): KVS[K, V, Higher] = instance
        |    sealed abstract class KVSFree[K, V, A] extends Product with Serializable
        |    object KVSFree { final case class GetValue[K, V](key: K) extends KVSFree[K, V, Option[V]] }
        |    type AppliedKVSFree2[K, V] = { type Out[A] = KVSFree[K, V, A] }
        |    def fromFunctionK[K, V, Higher[_]](f: _root_.cats.arrow.FunctionK[AppliedKVSFree2[K, V]#Out, Higher]): KVS[K, V, Higher] = new KVS[K, V, Higher] { def getValue(key: K): Higher[Option[V]] = f(KVSFree.GetValue(key)) }
        |    def toFunctionK[K, V, Higher[_]](ops: KVS[K, V, Higher]): _root_.cats.arrow.FunctionK[AppliedKVSFree2[K, V]#Out, Higher] = new _root_.cats.arrow.FunctionK[AppliedKVSFree2[K, V]#Out, Higher] {
        |      def apply[A](op: KVSFree[K, V, A]): Higher[A] = op match {
        |        case KVSFree.GetValue(key) => ops.getValue(key)
        |      }
        |    }
        |    type FreeHelper1[Higher[_]] = { type Out[A] = _root_.cats.free.Free[Higher, A] }
        |    implicit def freeInstance[K, V, Higher[_]](implicit inject: _root_.cats.free.Inject[AppliedKVSFree2[K, V]#Out, Higher]): KVS[K, V, FreeHelper1[Higher]#Out] = fromFunctionK(new _root_.cats.arrow.FunctionK[AppliedKVSFree2[K, V]#Out, FreeHelper1[Higher]#Out] { def apply[A](op: KVSFree[K, V, A]): _root_.cats.free.Free[Higher, A] = _root_.cats.free.Free.inject(op) })
        |    type AppliedKVS3[K, V] = { type Out[Higher[_]] = KVS[K, V, Higher] }
        |    implicit def liberatorFreeAlgebra[K, V]: io.aecor.liberator.FreeAlgebra.Aux[AppliedKVS3[K, V]#Out, AppliedKVSFree2[K, V]#Out] = new io.aecor.liberator.FreeAlgebra[AppliedKVS3[K, V]#Out] {
        |      type Out[A] = KVSFree[K, V, A]
        |      override def apply[Higher[_]](of: KVS[K, V, Higher]): _root_.cats.arrow.FunctionK[AppliedKVSFree2[K, V]#Out, Higher] = KVS.toFunctionK(of)
        |    }
        |  }
      """.stripMargin.parse[Source].get.stats
    )

    FreeMacro(defn, None).toString() shouldEqual block.toString()
  }

}
