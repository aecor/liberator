package io.aecor.liberator.macros

import scala.collection.immutable.Seq
import scala.meta._

class algebra extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Term.Block(Seq(t: Defn.Trait, companion: Defn.Object)) =>
        AlgebraMacro(t, Some(companion))
      case t: Defn.Trait =>
        AlgebraMacro(t, None)
    }
  }
}

object AlgebraMacro {
  def apply(base: Defn.Trait, companion: Option[Defn.Object]): Term.Block = {
    val typeName = base.name
    val opName = s"${typeName.value}Op"
    val opTypeName = Type.Name(opName)
    val traitStats = base.templ.stats.get
    val (theF, abstractParams) = (base.tparams.last.name, base.tparams.dropRight(1))
    val abstractTypes = abstractParams.map(_.name.value).map(Type.Name(_))

    def opCaseName(name: Term.Name) = s"$opName.${name.value.capitalize}"

    val unifiedOp =
      if (abstractTypes.isEmpty) {
        Type.Name(opName)
      } else {
        t"({type X[A] = $opTypeName[..$abstractTypes, A]})#X"
      }

    val unifiedBase =
      if (abstractTypes.isEmpty) {
        base.name
      } else {
        t"({type X[F[_]] = $typeName[..$abstractTypes, F]})#X"
      }

    val abstractMethods = traitStats.map {
      case m @ q"def $name[..$tps](..$params): ${someF: Type.Name}[$out]" if someF.value == theF.value =>
        m
      case m @ q"def $name: ${someF: Type.Name}[$out]" =>
        m
    }

    val companionStats: Seq[Stat] = Seq(
      {
        val types = base.tparams.map(x => Type.Name(x.name.value))
        q"def apply[..${base.tparams}](implicit instance: $typeName[..$types]): $typeName[..$types] = instance"
      },
      q"sealed abstract class $opTypeName[..$abstractParams, A] extends Product with Serializable", {
        val freeAdtLeafs = abstractMethods.map {
          case q"def $name[..$tps](..$params): $_[$out]" =>
            q"""final case class ${Type.Name(name.value.capitalize)}[..${abstractParams ++ tps}](..$params)
              extends ${Ctor.Name(opName)}[..$abstractTypes, $out]"""
          case m @ q"def $name: ${someF: Type.Name}[$out]" =>
            q"""final case object ${Term.Name(name.value.capitalize)} extends ${Ctor.Name(opName)}[..$abstractTypes, $out]"""
        }
        q"""object ${Term.Name(opName)} {
        ..$freeAdtLeafs
        }"""
      },
       {
        val methods = abstractMethods.map {
          case q"def $name[..$tps](..$params): $_[$out]" =>
            val ctor = Ctor.Name(opCaseName(name))
            val args = params.map(_.name.value).map(Term.Name(_))
            q"def $name[..$tps](..$params): F[$out] = f($ctor(..$args))"
          case m @ q"def $name: ${someF: Type.Name}[$out]" =>
            val objectName = Term.Name(opCaseName(name))
            q"def $name: F[$out] = f($objectName)"
        }
        q"""def fromFunctionK[..$abstractParams, F[_]](f: _root_.cats.arrow.FunctionK[$unifiedOp, F]): $typeName[..$abstractTypes, F] =
         new ${Ctor.Name(typeName.value)}[..$abstractTypes, F] {
          ..$methods
          }
       """
      }, {
        val cases = abstractMethods.map {
          case q"def $methodName[..$tps](..$params): $theF[$out]" =>
            val args = params.map(_.name.value).map(Term.Name(_))
            val exractArgs = args.map(Pat.Var.Term(_))
            val ctor = Term.Name(opCaseName(methodName))
            p"case $ctor(..$exractArgs) => ops.$methodName(..$args)"
          case m @ q"def $methodName: ${someF: Type.Name}[$out]" =>
            val objectName = Term.Name(opCaseName(methodName))
            p"case $objectName => ops.$methodName"
        }
        q"""def toFunctionK[..$abstractParams, F[_]](ops: $typeName[..$abstractTypes, F]): _root_.cats.arrow.FunctionK[$unifiedOp, F] =
         new _root_.cats.arrow.FunctionK[$unifiedOp, F] {
          def apply[A](op: $opTypeName[..$abstractTypes, A]): F[A] =
            op match { ..case $cases }
         }
       """
      },
      q"""
        implicit def liberatorAlgebraInstance[..$abstractParams]: io.aecor.liberator.Algebra.Aux[$unifiedBase, $unifiedOp] =
          new io.aecor.liberator.Algebra[$unifiedBase] {
            type Out[A] = $opTypeName[..$abstractTypes, A]
            override def toFunctionK[F[_]](of: $typeName[..$abstractTypes, F]): _root_.cats.arrow.FunctionK[$unifiedOp, F] =
              ${Term.Name(typeName.value)}.toFunctionK(of)
            override def fromFunctionK[F[_]](f: _root_.cats.arrow.FunctionK[$unifiedOp, F]):  $typeName[..$abstractTypes, F] =
              ${Term.Name(typeName.value)}.fromFunctionK(f)
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
