package rescala.lms.syntaxops

import scala.language.implicitConversions
import scala.reflect.SourceContext

// avoid name clashes with object factory methods
import rescala.{DepHolder, SignalSynt => RESignalSynt, Signal => RESignal}
import scala.virtualization.lms.common.{Base, BaseExp, FunctionsExp, ScalaGenFunctions, EffectExp}

trait SignalSyntax extends Base {
  object Signal {
    def apply[A:Manifest](
      deps: Rep[DepHolder]*
    )(
      expr: Rep[RESignalSynt[A]] => Rep[A]
    ) = sig_ops_newSignal(deps, expr)
  }

  def sig_ops_newSignal[A:Manifest](deps: Seq[Rep[DepHolder]],
    expr: Rep[RESignalSynt[A]] => Rep[A]): Rep[RESignalSynt[A]]

  object StaticSignal {
    def apply[A:Manifest](deps: Rep[DepHolder]*)(expr: => Rep[A]) =
      sig_ops_newStaticSignal(deps, expr)
  }

  def sig_ops_newStaticSignal[A:Manifest](
    deps: Seq[Rep[DepHolder]],
    expr: => Rep[A]): Rep[RESignal[A]]

  implicit def toSignalOps[A:Manifest](s: Rep[RESignal[A]]) = SignalOps(s)

  case class SignalOps[A:Manifest](sig: Rep[RESignal[A]]) {
    def get: Rep[A] = sig_ops_get(sig)
    def apply(): Rep[A] = sig_ops_apply(sig)
    def apply(depSig: Rep[RESignalSynt[_]]): Rep[A] = sig_ops_apply_dep(sig,depSig)
    def map[B:Manifest](f: Rep[A] => Rep[B]): Rep[RESignal[B]] = sig_ops_map(sig,f)
  }

  def sig_ops_get[A:Manifest](s: Rep[RESignal[A]]): Rep[A]
  def sig_ops_apply[A:Manifest](s: Rep[RESignal[A]]): Rep[A]
  def sig_ops_apply_dep[A:Manifest](sig: Rep[RESignal[A]], depSig: Rep[RESignalSynt[_]]): Rep[A]
  def sig_ops_map[A:Manifest, B:Manifest](sig: Rep[RESignal[A]], f: Rep[A] => Rep[B]): Rep[RESignal[B]]
}

trait SignalOps extends FunctionsExp with EffectExp {
  this: SignalSyntax with BaseExp =>

  override def sig_ops_newSignal[A:Manifest](deps: Seq[Exp[DepHolder]],
    expr: Exp[RESignalSynt[A]] => Exp[A]): Exp[RESignalSynt[A]] = deps match {
    case Seq(d) if d.isInstanceOf[Exp[RESignal[A]]] => SingleDepSignalCreation(d.asInstanceOf[Exp[RESignal[A]]], expr)
    case _ => SignalCreation(deps,fun(expr))
  }

  case class SignalCreation[A:Manifest](deps: Seq[Exp[DepHolder]],
    expr: Exp[RESignalSynt[A] => A]) extends Def[RESignalSynt[A]] {
    val t = manifest[A]
  }

  // Used for transforming signal expressions that depend only on one
  // Signal into map calls for fusion
  case class SingleDepSignalCreation[A:Manifest](
    dep: Exp[RESignal[A]],
    expr: Exp[RESignalSynt[A] => A]
  ) extends Def[RESignalSynt[A]] {
    val t = manifest[A]
  }

  override def sig_ops_newStaticSignal[A:Manifest](
    deps: Seq[Exp[DepHolder]],
    expr: => Exp[A]): Exp[RESignal[A]] = StaticSignalCreation(deps, reifyEffects(expr))

  case class StaticSignalCreation[A](
    deps: Seq[Exp[DepHolder]],
    expr: Block[A])(implicit val t: Manifest[A]) extends Def[RESignal[A]]

  override def sig_ops_get[A:Manifest](s: Exp[RESignal[A]]): Exp[A] =
    reflectMutable(SigGetValue(s))
  case class SigGetValue[A:Manifest](s: Exp[RESignal[A]]) extends Def[A] {
    val t = manifest[A]
  }

  override def sig_ops_apply[A](s: Exp[RESignal[A]])(implicit m: Manifest[A]): Exp[A] =
    reflectMutable(SigApply(s))
  case class SigApply[A:Manifest](s: Exp[RESignal[A]]) extends Def[A] {
    val t = manifest[A]
  }

  override def sig_ops_apply_dep[A](sig: Exp[RESignal[A]], depSig: Exp[RESignalSynt[_]])(implicit m: Manifest[A]): Exp[A] =
    reflectMutable(SigApplyDep(sig,depSig))
  case class SigApplyDep[A:Manifest](sig: Exp[RESignal[A]], depSig: Exp[RESignalSynt[_]]) extends Def[A] {
    val t = manifest[A]
  }

  override def sig_ops_map[A:Manifest, B:Manifest](sig: Exp[RESignal[A]], f: Exp[A] => Exp[B]): Exp[RESignal[B]] =
    MappedSignal(sig, doLambda(f))

  case class MappedSignal[A:Manifest,B:Manifest](
    sig: Exp[RESignal[A]],
    f: Rep[A => B]
  ) extends Def[RESignal[B]] {
    val t = (manifest[A],manifest[B])
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case SignalCreation(dhs,body) => effectSyms(body)
    case StaticSignalCreation(dhs,body) => effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def mirror[A:Manifest](
    e: Def[A],
    f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
      case SigApplyDep(a,b) => sig_ops_apply_dep(f(a),f(b))
      case Reflect(SigApplyDep(a,b), u, es) =>
        reflectMirrored(Reflect(SigApplyDep(f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(SigApply(a), u, es) =>
        reflectMirrored(Reflect(SigApply(f(a)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case a@SignalCreation(ds,l@Def(Lambda(func,arg,res))) => toAtom(SignalCreation(f(ds),f(l))(a.t))
      case a@SingleDepSignalCreation(d,l@Def(Lambda(func,arg,res))) => toAtom(SingleDepSignalCreation(f(d),f(l))(a.t))
      case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenSignals extends ScalaGenReactiveBase with ScalaGenFunctions {
  val IR: SignalOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case StaticSignalCreation(deps,expr) => emitValDef(sym,
      rescalaPkg + "StaticSignal(List(" + deps.map(quote).mkString(",") + ")) {")
      emitBlock(expr)
      stream.println(quote(getBlockResult(expr)) + "\n")
      stream.println("}")
    case s@SignalCreation(deps,expr) => emitSignalCreation(s,sym)
    case s@SingleDepSignalCreation(dep,expr) =>
      val str = "SignalSynt[" + s.t + "]" +
              "(List(" + quote(dep) + "))" +
             "{" + quote(expr) + "}"
      emitValDef(sym, rescalaPkg + str)
    case SigGetValue(s) => emitValDef(sym, quote(s) + ".get")
    case SigApply(s) =>
      emitValDef(sym, quote(s) + "()")
    case SigApplyDep(s,dep) => emitValDef(sym, quote(s) + "(" + quote(dep) + ")")
    case MappedSignal(s,f) => emitValDef(sym, quote(s) + ".map(" + quote(f) + ")" )
    case _ => super.emitNode(sym,node)
  }

  def emitSignalCreation[A:Manifest](sigNode: SignalCreation[A], sym: Sym[Any]): Unit = {
    val SignalCreation(deps,expr) = sigNode

    val className = "SignalSynt[" + sigNode.t + "]" // use manifest for type ascription
    val quotedDeps = "(List(" + deps.map(quote).mkString(",") + "))"
    val quotedExpr = "{" + quote(expr) + "}"

    emitValDef(sym, rescalaPkg + className + quotedDeps + quotedExpr )
  }

}
