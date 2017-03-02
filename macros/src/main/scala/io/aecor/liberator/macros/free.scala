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

  def findLastHigherOrderTypeParam(params: Seq[Type.Param]): (Type.Param, Seq[Type.Param]) = {
    val lastHigherOrderType = params.filter(_.tparams.length == 1).last
    lastHigherOrderType -> params.filterNot(_ == lastHigherOrderType)
  }

  def apply(base: Defn.Trait, companion: Option[Defn.Object]): Term.Block = {
    val typeName = base.name
    val freeName = s"${typeName.value}Free"
    val freeTypeName = Type.Name(freeName)
    val traitStats = base.templ.stats.get
    val (theFType, abstractParams) = findLastHigherOrderTypeParam(base.tparams)
    val theFName = theFType.name.value
    val typeNameAsTheF = Type.Name(theFName)
    val abstractTypes = abstractParams.map(_.name.value).map(Type.Name(_))

    def caseName(name: Term.Name) = s"$freeName.${name.value.capitalize}"

    val freeHelperName = Type.fresh("FreeHelper")
    val appliedFreeName = Type.fresh(s"Applied$freeTypeName")
    val appliedBaseName = Type.fresh(s"Applied${typeName.value}")

    val appliedFreeNameOut =
      if (abstractTypes.isEmpty) {
        Type.Name(freeName)
      } else {
        t"$appliedFreeName[..$abstractTypes]#Out"
      }

    val appliedBaseNameOut =
      if (abstractTypes.isEmpty) {
        base.name
      } else {
        t"$appliedBaseName[..$abstractTypes]#Out"
      }



    val abstractMethods = traitStats.map {
      case m @ q"def $name[..$tps](..$params): ${someF: Type.Name}[$out]" if someF.value == theFName =>
        m
    }

    val companionStats: Seq[Stat] = Seq(
      {
        val types = base.tparams.map(x => Type.Name(x.name.value))
        q"def apply[..${base.tparams}](implicit instance: $typeName[..$types]): $typeName[..$types] = instance"
      },
      q"sealed abstract class $freeTypeName[..$abstractParams, A] extends Product with Serializable", {
        val freeAdtLeafs = abstractMethods.map {
          case q"def $name[..$tps](..$params): $_[$out]" =>
            q"""final case class ${Type.Name(name.value.capitalize)}[..${abstractParams ++ tps}](..$params)
              extends ${Ctor.Name(freeName)}[..$abstractTypes, $out]"""
        }
        q"""object ${Term.Name(freeName)} {
        ..$freeAdtLeafs
        }"""
      },
      q"""
         type $appliedFreeName[..$abstractParams] = {
            type Out[A] = $freeTypeName[..$abstractTypes, A]
         }
       """, {
        val methods = abstractMethods.map {
          case q"def $name[..$tps](..$params): $_[$out]" =>
            val ctor = Ctor.Name(caseName(name))
            val args = params.map(_.name.value).map(Term.Name(_))
            q"def $name[..$tps](..$params): $typeNameAsTheF[$out] = f($ctor(..$args))"
        }
        q"""def fromFunctionK[..$abstractParams, $theFType](f: _root_.cats.arrow.FunctionK[$appliedFreeNameOut, $typeNameAsTheF]): $typeName[..$abstractTypes, $typeNameAsTheF] =
         new ${Ctor.Name(typeName.value)}[..$abstractTypes, $typeNameAsTheF] {
          ..$methods
          }
       """
      }, {
        val cases = abstractMethods.map {
          case q"def $methodName[..$tps](..$params): $theF[$out]" =>
            val args = params.map(_.name.value).map(Term.Name(_))
            val exractArgs = args.map(Pat.Var.Term(_))
            val ctor = Term.Name(caseName(methodName))
            p"case $ctor(..$exractArgs) => ops.$methodName(..$args)"
        }
        q"""def toFunctionK[..$abstractParams, $theFType](ops: $typeName[..$abstractTypes, $typeNameAsTheF]): _root_.cats.arrow.FunctionK[$appliedFreeNameOut, $typeNameAsTheF] =
         new _root_.cats.arrow.FunctionK[$appliedFreeNameOut, $typeNameAsTheF] {
          def apply[A](op: $freeTypeName[..$abstractTypes, A]): $typeNameAsTheF[A] =
            op match { ..case $cases }
         }
       """
      },
      q"""
           type $freeHelperName[$theFType] = {
             type Out[A] = _root_.cats.free.Free[$typeNameAsTheF, A]
           }
         """,
      q"""
       implicit def freeInstance[..$abstractParams, $theFType](implicit inject: _root_.cats.free.Inject[$appliedFreeNameOut, $typeNameAsTheF]): $typeName[..$abstractTypes, $freeHelperName[$typeNameAsTheF]#Out] =
         fromFunctionK(new _root_.cats.arrow.FunctionK[$appliedFreeNameOut, $freeHelperName[$typeNameAsTheF]#Out] {
          def apply[A](op: $freeTypeName[..$abstractTypes, A]): _root_.cats.free.Free[$typeNameAsTheF, A] = _root_.cats.free.Free.inject(op)
         })
     """,
      q"""
           type $appliedBaseName[..$abstractParams] = {
             type Out[$theFType] = $typeName[..$abstractTypes, $typeNameAsTheF]
           }
         """,
      q"""
        implicit def liberatorFreeAlgebra[..$abstractParams]: io.aecor.liberator.FreeAlgebra.Aux[$appliedBaseNameOut, $appliedFreeNameOut] =
          new io.aecor.liberator.FreeAlgebra[$appliedBaseNameOut] {
            type Out[A] = $freeTypeName[..$abstractTypes, A]
            override def apply[$theFType](of: $typeName[..$abstractTypes, $typeNameAsTheF]): _root_.cats.arrow.FunctionK[$appliedFreeNameOut, $typeNameAsTheF] = ${Term
        .Name(typeName.value)}.toFunctionK(of)
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

    println(newCompanion)

    Term.Block(Seq(base, newCompanion))
  }
}
