package io.aecor.liberator.macros

import scala.collection.immutable.Seq
import scala.meta._

class term extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Term.Block(Seq(t: Defn.Trait, companion: Defn.Object)) =>
        TermMacro(t, Some(companion))
      case t: Defn.Trait =>
        TermMacro(t, None)
    }
  }
}

object TermMacro {
  def apply(base: Defn.Trait, companion: Option[Defn.Object]): Term.Block = {
    val baseName = base.name
    val abstractParams  = base.tparams.dropRight(1)
    val abstractTypes = abstractParams.map(_.name.value).map(Type.Name(_))

    val appliedBase =
      if (abstractTypes.isEmpty) {
        base.name
      } else {
        t"({ type Out[F[_]] = $baseName[..$abstractTypes, F] })#Out"
      }

    val applyWithImplicit = {
      val types = base.tparams.map(_.name.value).map(Type.Name(_))
      q"def apply[..${base.tparams}](implicit instance: $baseName[..$types]): $baseName[..$types] = instance"
    }

    val hasDefinedApplyWithImplicit = companion.flatMap(_.templ.stats).exists(_.exists {
      case q"def apply[..$_](implicit instance: $_[..$_]): $_[..$_] = instance" => true
      case _ => false
    })

    val applyWithImplicitStats = if (hasDefinedApplyWithImplicit) Seq.empty else Seq(applyWithImplicit)

    val companionStats: Seq[Stat] = applyWithImplicitStats ++ Seq(
      q"""
         implicit def termInstance[..$abstractParams, Alg[_], M[_[_]]](
           implicit algebra: io.aecor.liberator.Algebra.Aux[$appliedBase, Alg],
           extract: _root_.io.aecor.liberator.Extract[M, $appliedBase]
         ): $baseName[..$abstractTypes, ({type Out[A] = _root_.io.aecor.liberator.Term[M, A]})#Out] =
          algebra.fromFunctionK(new _root_.cats.arrow.FunctionK[Alg, ({type Out[A] = _root_.io.aecor.liberator.Term[M, A]})#Out] {
            def apply[A](op: Alg[A]): _root_.io.aecor.liberator.Term[M, A] =
              _root_.io.aecor.liberator.Term.lift(new _root_.io.aecor.liberator.Term.Invocation[M, A] {
                override def apply[F[_]](mf: M[F]): F[A] =
                  algebra.toFunctionK(extract(mf))(op)
              })
          })
        """
    )

    val newCompanion = companion match {
      case Some(c) =>
        val existingStats = c.templ.stats.getOrElse(Nil)
        c.copy(templ = c.templ.copy(stats = Some(companionStats ++ existingStats)))
      case None =>
        q"object ${Term.Name(baseName.value)} { ..$companionStats }"

    }

    Term.Block(Seq(base, newCompanion))
  }
}
