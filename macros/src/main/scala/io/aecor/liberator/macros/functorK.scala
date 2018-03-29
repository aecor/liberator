package io.aecor.liberator.macros

import scala.collection.immutable.Seq
import scala.meta._

class functorK(commonFields: scala.Symbol*) extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val commonFields = this match {
      case q"new $_(..$xs)" => xs.map { case ctor"$_(${Lit(x: String)})" => x }.toList
      case _ => Nil
    }
    defn match {
      case Term.Block(Seq(t: Defn.Trait, companion: Defn.Object)) =>
        FunctorKMacro(commonFields, t, Some(companion))
      case t: Defn.Trait =>
        FunctorKMacro(commonFields, t, None)
      case other =>
        defn
    }
  }
}

object FunctorKMacro {

  def apply(commonFields: List[String], base: Defn.Trait, companion: Option[Defn.Object]): Term.Block = {
    val typeName = base.name
    val traitStats = base.templ.stats.get
    val (theF, abstractParams) = (base.tparams.last.name, base.tparams.dropRight(1))
    val abstractTypes = abstractParams.map(_.name.value).map(Type.Name(_))

    val unifiedBase =
      if (abstractTypes.isEmpty) {
        base.name
      } else {
        t"({type X[F[_]] = $typeName[..$abstractTypes, F]})#X"
      }

    val companionStats: Seq[Stat] = Seq(
      q"""
        implicit def liberatorFunctorKInstance[..$abstractParams]: io.aecor.liberator.FunctorK[$unifiedBase] =
          new io.aecor.liberator.FunctorK[$unifiedBase] {
            final def mapK[F[_], G[_]](mf: $typeName[..$abstractTypes, F], fg: _root_.cats.arrow.FunctionK[F, G]): $typeName[..$abstractTypes, G] =
              new ${Ctor.Name(typeName.value)}[..$abstractTypes, G] {
                ..${
                  traitStats.map {
                    case q"def $name[..$tps](..$params): ${someF: Type.Name}[$out]" if someF.value == theF.value =>
                      q"final def $name[..$tps](..$params): G[$out] = fg(mf.$name(..${params.map(_.name.value).map(Term.Name(_))}))"
                    case q"def $name: ${someF: Type.Name}[$out]" if someF.value == theF.value =>
                      q"final def $name: G[$out] = fg(mf.$name)"
                    case other =>
                      abort(s"Illegal method [$other]")
                  }
                }
              }
          }
    """
    )

    val newCompanion = companion match {
      case Some(c) =>
        val oldTemplStats = c.templ.stats.getOrElse(Nil)
        c.copy(templ = c.templ.copy(stats = Some(companionStats ++ oldTemplStats)))
      case None =>
        q"object ${Term.Name(typeName.value)} { ..$companionStats }"

    }

    Term.Block(Seq(base, newCompanion))
  }
}

