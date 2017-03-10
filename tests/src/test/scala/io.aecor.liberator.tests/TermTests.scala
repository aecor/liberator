package io.aecor.liberator.tests

import cats.kernel.laws.GroupLaws
import cats.laws.discipline.{ MonadTests, SerializableTests }
import cats.{ Eq, Eval, Id, Monad }
import io.aecor.liberator.Term
import org.scalacheck.{ Arbitrary, Cogen, Gen }

class TermTests extends LiberatorSuite with TermInstances {
  trait M[F[_]]
  implicit def mf: M[Id] = new M[Id] {}
  checkAll("Term[M, Int]", GroupLaws[Term[M, Int]].monoid)
  checkAll("Free[Option, ?]", MonadTests[Term[M, ?]].monad[Int, Int, Int])
  checkAll("Monad[Free[Option, ?]]", SerializableTests.serializable(Monad[Term[M, ?]]))
}

sealed trait TermInstances {
  private def freeGen[M[_[_]], A](maxDepth: Int)(implicit A: Arbitrary[A]): Gen[Term[M, A]] = {
    val noFlatMapped =
      Gen.oneOf(A.arbitrary.map(Term.pure[M, A]), A.arbitrary.map(Term.pure[M, A]))

    val nextDepth = Gen.chooseNum(1, math.max(1, maxDepth - 1))

    def withFlatMapped =
      for {
        fDepth <- nextDepth
        freeDepth <- nextDepth
        f <- Arbitrary
              .arbFunction1[A, Term[M, A]](
                Arbitrary(freeGen[M, A](fDepth)),
                Cogen[Unit].contramap(_ => ())
              )
              .arbitrary
        freeFA <- freeGen[M, A](freeDepth)
      } yield freeFA.flatMap(f)

    if (maxDepth <= 1) noFlatMapped
    else Gen.oneOf(noFlatMapped, withFlatMapped)
  }

  implicit def freeArbitrary[M[_[_]], A](implicit A: Arbitrary[A]): Arbitrary[Term[M, A]] =
    Arbitrary(freeGen[M, A](4))

  implicit def freeEq[M[_[_]], A, F[_]](implicit mf: M[F],
                                        eqA: Eq[F[A]],
                                        F: Monad[F]): Eq[Term[M, A]] =
    new Eq[Term[M, A]] {
      override def eqv(x: Term[M, A], y: Term[M, A]): Boolean =
        eqA.eqv(x(mf), y(mf))
    }
}
