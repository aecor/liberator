package io.aecor.liberator.macros

import scala.collection.immutable.Seq
import scala.meta._

class algebra(commonFields: scala.Symbol*) extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val commonFields = this match {
      case q"new $_(..$xs)" => xs.map { case ctor"$_(${Lit(x: String)})" => x }.toList
      case _ => Nil
    }
    defn match {
      case Term.Block(Seq(t: Defn.Trait, companion: Defn.Object)) =>
        AlgebraMacro(commonFields, t, Some(companion))
      case t: Defn.Trait =>
        AlgebraMacro(commonFields, t, None)
      case other =>
        defn
    }
  }
}

object AlgebraMacro {
  def apply(commonFields: List[String], base: Defn.Trait, companion: Option[Defn.Object]): Term.Block = {
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

    val abstractMethods = traitStats.collect {
      case m @ q"def $name[..$tps](..$params): ${someF: Type.Name}[$out]" if someF.value == theF.value =>
        m
      case m @ q"def $name: ${someF: Type.Name}[$out]" =>
        m
    }


    val commonFieldsStat =
      if (commonFields.nonEmpty) {
        abstractMethods.collectFirst {
          case q"def $name[..$tps](..$params): $_[$out]"  => params
        }.map { params =>
          params.collect {
            case param"..$mods $paramname: ${Some(tpe: Type.Arg)} = $expropt" if commonFields.contains(paramname.value) =>
              q"def ${Term.Name(paramname.value)}: ${Type.Name(tpe.toString)}"
          }
        }.getOrElse(Seq.empty)
      } else {
        Seq.empty
      }

    val companionStats: Seq[Stat] = Seq(
      q"""sealed abstract class $opTypeName[..$abstractParams, A] extends _root_.io.aecor.liberator.Term.Invocation[$unifiedBase, A] with Product with Serializable {
          ..$commonFieldsStat
         }
       """, {
        val freeAdtLeafs = abstractMethods.map {
          case q"def $name[..$tps](..$params): $_[$out]" =>
            q"""final case class ${Type.Name(name.value.capitalize)}[..${abstractParams ++ tps}](..$params)
              extends ${Ctor.Name(opName)}[..$abstractTypes, $out] {
                def invoke[F[_]](mf: $typeName[..$abstractTypes, F]): F[$out] = mf.$name(..${params.map(_.name.value).map(Term.Name(_))})
                }
              """
          case m @ q"def $name: ${someF: Type.Name}[$out]" =>
            q"""final case object ${Term.Name(name.value.capitalize)} extends ${Ctor.Name(opName)}[..$abstractTypes, $out] {
                def invoke[F[_]](mf: $typeName[..$abstractTypes, F]): F[$out] = mf.$name
               }
             """
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
      },
      {
        q"""def toFunctionK[..$abstractParams, F[_]](ops: $typeName[..$abstractTypes, F]): _root_.cats.arrow.FunctionK[$unifiedOp, F] =
         new _root_.cats.arrow.FunctionK[$unifiedOp, F] {
          def apply[A](invocation: $opTypeName[..$abstractTypes, A]): F[A] =
            invocation.invoke(ops)
         }
       """
      },
      q"""
        implicit def liberatorAlgebraInstance[..$abstractParams]: _root_.io.aecor.liberator.Algebra.Aux[$unifiedBase, $unifiedOp] =
          new _root_.io.aecor.liberator.Algebra[$unifiedBase] {
            type Out[A] = $opTypeName[..$abstractTypes, A]
            final override def toFunctionK[F[_]](of: $typeName[..$abstractTypes, F]): _root_.cats.arrow.FunctionK[$unifiedOp, F] =
              ${Term.Name(typeName.value)}.toFunctionK(of)
            final override def fromFunctionK[F[_]](f: _root_.cats.arrow.FunctionK[$unifiedOp, F]):  $typeName[..$abstractTypes, F] =
              ${Term.Name(typeName.value)}.fromFunctionK(f)

            final override def invoke[F[_], A](mf: $unifiedBase[F], f: Out[A]): F[A] = f.invoke(mf)

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
