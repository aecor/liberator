package io.aecor.liberator.macros

import scala.collection.immutable.Seq
import scala.meta._

class free extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Term.Block(Seq(t: Defn.Trait, companion: Defn.Object)) =>
        FreeMacro(t, Some(companion))
      case t: Defn.Trait =>
        FreeMacro(t, None)
    }
  }
}

object FreeMacro {
  def apply(t: Defn.Trait, companion: Option[Defn.Object]): Term.Block = {
    val typeName = t.name
    val freeName = s"${typeName.value}Free"
    val freeTypeName = Type.Name(freeName)

    val traitStats = t.templ.stats.get

    val cases = traitStats.map {
      case q"def $name[..$tps](..$params): F[$out]" =>
        q"final case class ${Type.Name(name.value.capitalize)}[..$tps](..$params) extends ${Ctor.Name(freeName)}[$out]"
    }

    def caseName(name: Term.Name) = s"$freeName.${name.value.capitalize}"

    val methods = traitStats.map {
      case q"def $name[..$tps](..$params): F[$out]" =>
        val ctor = Ctor.Name(caseName(name))
        val args = params.map(_.name.value).map(Term.Name(_))
        q"def $name[..$tps](..$params): F[$out] = f($ctor(..$args))"
    }

    val patMatCases = traitStats.map {
      case q"def $methodName[..$tps](..$params): F[$out]" =>
        val args = params.map(_.name.value).map(Term.Name(_))
        val exractArgs = args.map(Pat.Var.Term(_))
        val ctor = Term.Name(caseName(methodName))
        p"case $ctor(..$exractArgs) => ops.$methodName(..$args)"
    }

    val helperName = Type.fresh("FreeHelper")

    val companionStats: Seq[Stat] = Seq(
      q"def apply[F[_]](implicit instance: $typeName[F]): $typeName[F] = instance",
      q"sealed abstract class $freeTypeName[A] extends Product with Serializable",
      q"""object ${Term.Name(freeName)} {
        ..$cases
      }
      """
      ,q"""def fromFunctionK[F[_]](f: _root_.cats.arrow.FunctionK[$freeTypeName, F]): $typeName[F] =
         new ${Ctor.Name(typeName.value)}[F] {
          ..$methods
          }
       """
    ,
      q"""def toFunctionK[F[_]](ops: $typeName[F]): _root_.cats.arrow.FunctionK[$freeTypeName, F] =
         new _root_.cats.arrow.FunctionK[$freeTypeName, F] {
          def apply[A](op: $freeTypeName[A]): F[A] =
            op match { ..case $patMatCases }
         }
       """
      ,
        q"""
           type $helperName[F[_]] = {
             type Out[A] = _root_.cats.free.Free[F, A]
           }
         """
    ,
    q"""
       implicit def freeInstance[F[_]](implicit inject: _root_.cats.free.Inject[$freeTypeName, F]): $typeName[$helperName[F]#Out] =
         fromFunctionK(new _root_.cats.arrow.FunctionK[$freeTypeName, $helperName[F]#Out] {
          def apply[A](op: $freeTypeName[A]): _root_.cats.free.Free[F, A] = _root_.cats.free.Free.inject(op)
         })
     """,
    q"""
        implicit val freeAlgebra: io.aecor.liberator.FreeAlgebra.Aux[$typeName, $freeTypeName] =
          new io.aecor.liberator.FreeAlgebra[$typeName] {
            type Out[A] = $freeTypeName[A]
            override def apply[F[_]](of: $typeName[F]): _root_.cats.arrow.FunctionK[$freeTypeName, F] = ${Term.Name(typeName.value)}.toFunctionK(of)
          }
    """)


    val newCompanion = companion match {
      case Some(c) =>
        val oldTemplStats = c.templ.stats.getOrElse(Nil)
          c.copy(templ = c.templ.copy(stats = Some(companionStats ++ oldTemplStats)))
      case None =>
        q"object ${Term.Name(t.name.value)} { ..$companionStats }"

    }

    Term.Block(Seq(t, newCompanion))
  }
}


