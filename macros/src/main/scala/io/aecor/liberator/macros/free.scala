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
  def apply(base: Defn.Trait, companion: Option[Defn.Object]): Term.Block = {
    val typeName = base.name
    val freeName = s"${typeName.value}Op"
    val freeTypeName = Type.Name(freeName)
    val traitStats = base.templ.stats.get
    val (theF, abstractParams) = (base.tparams.last.name, base.tparams.dropRight(1))
    val abstractTypes = abstractParams.map(_.name.value).map(Type.Name(_))

    def caseName(name: Term.Name) = s"$freeName.${name.value.capitalize}"

    val freeHelperName = Type.fresh("FreeHelper")
    val termHelperName = Type.fresh("TermHelper")
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
      q"sealed abstract class $freeTypeName[..$abstractParams, A] extends Product with Serializable", {
        val freeAdtLeafs = abstractMethods.map {
          case q"def $name[..$tps](..$params): $_[$out]" =>
            q"""final case class ${Type.Name(name.value.capitalize)}[..${abstractParams ++ tps}](..$params)
              extends ${Ctor.Name(freeName)}[..$abstractTypes, $out]"""
          case m @ q"def $name: ${someF: Type.Name}[$out]" =>
            q"""final case object ${Term.Name(name.value.capitalize)} extends ${Ctor.Name(freeName)}[..$abstractTypes, $out]"""
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
            q"def $name[..$tps](..$params): F[$out] = f($ctor(..$args))"
          case m @ q"def $name: ${someF: Type.Name}[$out]" =>
            val objectName = Term.Name(caseName(name))
            q"def $name: F[$out] = f($objectName)"
        }
        q"""def fromFunctionK[..$abstractParams, F[_]](f: _root_.cats.arrow.FunctionK[$appliedFreeNameOut, F]): $typeName[..$abstractTypes, F] =
         new ${Ctor.Name(typeName.value)}[..$abstractTypes, F] {
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
          case m @ q"def $methodName: ${someF: Type.Name}[$out]" =>
            val objectName = Term.Name(caseName(methodName))
            p"case $objectName => ops.$methodName"
        }
        q"""def toFunctionK[..$abstractParams, F[_]](ops: $typeName[..$abstractTypes, F]): _root_.cats.arrow.FunctionK[$appliedFreeNameOut, F] =
         new _root_.cats.arrow.FunctionK[$appliedFreeNameOut, F] {
          def apply[A](op: $freeTypeName[..$abstractTypes, A]): F[A] =
            op match { ..case $cases }
         }
       """
      },
      q"""
           type $freeHelperName[F[_]] = {
             type Out[A] = _root_.cats.free.Free[F, A]
           }
         """,
      q"""
       implicit def freeInstance[..$abstractParams, F[_]](implicit inject: _root_.cats.free.Inject[$appliedFreeNameOut, F]): $typeName[..$abstractTypes, $freeHelperName[F]#Out] =
         fromFunctionK(new _root_.cats.arrow.FunctionK[$appliedFreeNameOut, $freeHelperName[F]#Out] {
          def apply[A](op: $freeTypeName[..$abstractTypes, A]): _root_.cats.free.Free[F, A] = _root_.cats.free.Free.inject(op)
         })
     """,
      q"""
           type $termHelperName[M[_[_]]] = {
             type Out[A] = _root_.io.aecor.liberator.Term[M, A]
           }
         """,
      q"""
         implicit def termInstance[..$abstractParams, M[_[_]]](implicit extract: _root_.io.aecor.liberator.Extract[M, $appliedBaseNameOut]): $typeName[..$abstractTypes, $termHelperName[M]#Out] =
                  fromFunctionK(new _root_.cats.arrow.FunctionK[$appliedFreeNameOut, $termHelperName[M]#Out] {
                   def apply[A](op: $freeTypeName[..$abstractTypes, A]): _root_.io.aecor.liberator.Term[M, A] =
                     new Term[M, A] {
                      override def apply[F[_]](alg: M[F])(implicit F: Monad[F]): F[A] =
                        toFunctionK(extract(alg))(op)
                      }
                  })
        """
      ,
      q"""
           type $appliedBaseName[..$abstractParams] = {
             type Out[F[_]] = $typeName[..$abstractTypes, F]
           }
         """,
      q"""
        implicit def liberatorOps[..$abstractParams]: io.aecor.liberator.Ops.Aux[$appliedBaseNameOut, $appliedFreeNameOut] =
          new io.aecor.liberator.Ops[$appliedBaseNameOut] {
            type Out[A] = $freeTypeName[..$abstractTypes, A]
            override def toFunctionK[F[_]](of: $typeName[..$abstractTypes, F]): _root_.cats.arrow.FunctionK[$appliedFreeNameOut, F] =
              ${Term.Name(typeName.value)}.toFunctionK(of)
            override def fromFunctionK[F[_]](f: _root_.cats.arrow.FunctionK[$appliedFreeNameOut, F]):  $typeName[..$abstractTypes, F] =
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
