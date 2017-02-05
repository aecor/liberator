package io.aecor.liberator.tests

import cats.implicits._
import cats.{ Functor, ~> }
import shapeless.{ :+:, CNil, Generic }

object Server {

  type ConstT[F[_], A, B] = F[A]

  trait Encoder[A, B] {
    def apply(a: A): B
  }

  trait Decoder[F[_], A, B] { outer =>
    def decode(a: A): F[B]
    def map[C](f: B => C)(implicit F: Functor[F]): Decoder[F, A, C] = new Decoder[F, A, C] {
      override def decode(a: A): F[C] = F.map(outer.decode(a))(f)
    }
  }

  class Server[In, Out, Decoded[_]] {
    def mkServer[F[_], Op[_]](impl: Op ~> F)(
      implicit decoder: Decoder[Decoded, In, Op[_]],
      encoderK: EncoderK[Op, F, Out]
    ): In => Decoded[F[Out]] = {
      val encoder = encoderK(impl)
      decoder.map(op => encoder(op)).decodeJson
    }
  }

  trait ProjectOp[C[_], F[_], T, Out] {
    def apply(cf: C ~> F): T => F[Out]
  }

  object ProjectOp {
    implicit def handlerCnil[C[_], F[_], Out]: ProjectOp[C, F, CNil, Out] =
      new ProjectOp[C, F, CNil, Out] {
        override def apply(cf: ~>[C, F]) = _.impossible
      }

    implicit def handlerCCons[CA, C[_], A, F[_], T <: shapeless.Coproduct, Out](
      implicit unapply: CA <:< C[A],
      encoder: Encoder[A, Out],
      F: Functor[F],
      tailComponentEncoder: ProjectOp[C, F, T, Out]
    ): ProjectOp[C, F, CA :+: T, Out] =
      new ProjectOp[C, F, CA :+: T, Out] {
        override def apply(cf: ~>[C, F]) =
          _.eliminate(ca => cf(unapply(ca)).map(encoder(_)), tailComponentEncoder(cf))
      }
  }

  trait EncoderK[C[_], F[_], Out] {
    def apply(cf: C ~> F): C ~> ConstT[F, Out, ?]
  }

  object EncoderK {
    implicit def generic[C[_], F[_], Repr, Out](
      implicit gen: Generic.Aux[C[_], Repr],
      applyOp: ProjectOp[C, F, Repr, Out]
    ): EncoderK[C, F, Out] =
      new EncoderK[C, F, Out] {
        override def apply(cf: ~>[C, F]): ~>[C, ConstT[F, Out, ?]] =
          new (C ~> ConstT[F, Out, ?]) {
            override def apply[A](fa: C[A]): ConstT[F, Out, A] =
              applyOp(cf)(gen.to(fa))
          }
      }
  }

}
