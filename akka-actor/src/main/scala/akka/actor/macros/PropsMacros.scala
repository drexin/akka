package akka.actor.macros

import scala.reflect.macros.Context
import akka.actor.{ Props, Actor }
import scala.language.experimental.macros

object SafeProps {
  def apply[A <: Actor](args: Any*): Props = macro checkedProps[A]

  def checkedProps[A <: Actor](c: Context)(args: c.Expr[Any]*)(implicit actorType: c.WeakTypeTag[A]): c.Expr[Props] = {
    import c.universe._

    val paramsTypes = args.map(x ⇒ x.actualType.normalize)

    val hasMatchingConstructor = actorType.tpe.declaration(nme.CONSTRUCTOR).asTerm.alternatives.exists {
      case m: MethodSymbol ⇒
        m.paramss.headOption.exists { symbols ⇒
          symbols.map(_.asTerm.typeSignature.normalize).zip(paramsTypes).forall {
            case (t1, t2) ⇒ t2 <:< t1
          }
        }

      case _ ⇒ false
    }

    if (hasMatchingConstructor) {
      val propsType = typeOf[Props]
      val propsCompanion = propsType.typeSymbol.companionSymbol

      c.Expr[Props](
        Apply(
          Select(c.universe.Ident(propsCompanion), newTermName("apply")),
          (c.reifyRuntimeClass(actorType.tpe) +: args.map(_.tree)).toList))
    } else {
      c.abort(c.enclosingPosition, s"No matching constructor found on ${actorType.tpe.typeSymbol} for types ${paramsTypes.mkString("(", ",", ")")}")
    }
  }
}
