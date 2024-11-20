package hrf.base
//
//
//
//
import hrf.colmat._
import hrf.logger._
//
//
//
//

import hrf.elem._

import scala.util.control.NonFatal


@scalajs.reflect.annotation.EnableReflectiveInstantiation
trait Record extends Product with GoodMatch

trait BasePlayer

trait Gaming extends Timelines {
    type Record = hrf.base.Record
    type BasePlayer = hrf.base.BasePlayer

    type F <: BasePlayer
    type G <: BaseGame

    val version = hrf.BuildInfo.version

    trait Signer
    case object Randomness extends Signer
    case object Revelation extends Signer
    case class FactionSigner(faction : F) extends Signer

    trait Die[T] {
        def valid(t : T) : Boolean
        def roll() : T
    }

    case class CustomDie[T](values : $[T]) extends Die[T] {
        def valid(v : T) = values.has(v)
        def roll() = values.shuffle(0)
    }

    case class StraitDie(low : Int, high : Int) extends Die[Int] {
        def valid(v : Int) = low <= v && v <= high
        def roll() = random(high + 1 - low) + low
    }

    object Die {
        def custom[T](values : Seq[T]) = new CustomDie(values.$)
        def from[T](values : T*) = new CustomDie(values.$)
        def range(r : (Int, Int)) = new StraitDie(r._1, r._2)
    }

    trait ConvertMode
    case object ModeDefault extends ConvertMode
    case object ModeLog extends ConvertMode

    trait GameModeElementary {
        def elem(g : G, mode : ConvertMode) : Elem
    }

    trait GameElementary extends GoodMatch {
        def elem(implicit g : G) : Elem
    }

    trait Elementary extends GoodMatch {
        def elem : Elem
    }

    implicit class ElementaryList(val l : $[Elementary]) {
        private def ll = l./(_.elem)
        def spaced : $[Elem] = ll./~(e => $(Space, e)).drop(1)
        def comma : $[Elem] = ll./~(e => $(Comma, e)).drop(1)
        def commaAnd : $[Elem] = ll.dropRight(1).some.?(_.comma :+ Text("and")) ++ ll.lastOption
        def commaOr : $[Elem] = ll.dropRight(1).some.?(_.comma :+ Text("or")) ++ ll.lastOption
        def spaceComma : Elem = ll./~(e => $(Comma, Text(" "), e)).drop(2).merge
        def spaceCommaOr : Elem = ll.dropRight(1).some./(_.spaceComma ~ Text(" or ")) ~ ll.lastOption
    }

    trait Message extends Record with GameElementary {
        def ~(m : Message) = ConcatMessage(this, m)
    }

    trait ElementaryMessage extends Message with Elementary {
        def elem(implicit g : G) : Elem = elem
    }

    case class JustElem(e : Elementary) extends Message {
        def elem(implicit g : G) = e.elem
    }

    case object NoMessage extends Message {
        def elem(implicit g : G) = Empty
    }

    case class ConcatMessage(a : Message, b : Message) extends Message with GameModeElementary {
        def elem(implicit g : G) = a.elem(g) ~ b.elem(g)
        def elem(g : G, mode : ConvertMode) = convertDesc(a)(styles, mode)(g) ~ convertDesc(b)(styles, mode)(g)
    }

    case class NotInLog(m : Message) extends Message {
        def elem(implicit game : G) = m.elem
    }

    case class AltInLog(m : Message, alt : Message) extends Message {
        def elem(implicit game : G) = m.elem
    }




    trait Named extends ImageIdPart {
        def name : String
        def elem : Elem = this @@ {
            case s : Styling => Span(Text(name), styles.get(s))
            case _ => Text(name)
        }
    }

    trait NamedToString extends Named {
        def name = toString
    }

    implicit def actionToForce(fa : ForcedAction) = Force(fa)


    trait ActionClass[A <: Action]


    sealed trait Action extends Record with ExpandCheck { self : ActionClass[_] =>
        def unwrap : Action = this @@ {
            case a : WrappedAction => a.then.unwrap
            case a : ForceAction => a.then.unwrap
            case a : Cancel => CancelAction
            case a => a
        }
    }

    trait ExpandCheck { self : Action =>
        def canExpandTo(a : Action) : Boolean = this @@ {
            case a : WrappedAction => a.then.unwrap.canExpandTo(a)
            case a : ForceAction => a.then.unwrap.canExpandTo(a)
            case a => this == a
        }
    }


    trait ForcedAction extends Action with ActionClass[ForcedAction] {
        def wrap = DoAction(this)
        def as(q : (G => Elem)*) = WrapAction(this)(q : _*)
        def view[T](obj : T)(view : T => Any) = WrapViewAction(obj, this)(view)
    }

    trait ExternalAction extends Action { self : ActionClass[_] => }

    trait SelfValidate { self : Action =>
        def validate(a : Action) : Boolean
    }

    trait HalfValidate { self : Action =>
        def validate(a : Action) : Option[Boolean]
    }

    trait SkipValidate { self : Action => }

    case class ForceInvalidAction(action : ExternalAction) extends ExternalAction with ActionClass[ForceInvalidAction] with SkipValidate

    case class CantParseAction(action : String) extends ExternalAction with ActionClass[CantParseAction] with SkipValidate

    case class CommentAction(comment : String) extends ExternalAction with ActionClass[CommentAction] with SkipValidate


    trait OracleAction extends ExternalAction with ActionClass[OracleAction]

    trait StartGameAction extends ExternalAction with ActionClass[StartGameAction]

    trait GameVersion {
        def version : String
    }

    case class UndoAction(initiators : $[F], count : Int, target : Int) extends ExternalAction with ActionClass[UndoAction]


    case object WaitAction extends ForcedAction

    trait ShuffledAction[T] extends OracleAction {
        def shuffled : $[T]
    }

    trait Shuffled2Action[T, U] extends OracleAction {
        def shuffled1 : $[T]
        def shuffled2 : $[U]
    }

    trait Shuffled3Action[T, U, V] extends OracleAction {
        def shuffled1 : $[T]
        def shuffled2 : $[U]
        def shuffled3 : $[V]
    }

    trait RolledAction[T] extends OracleAction {
        def rolled : $[T]
    }

    trait Rolled2Action[T, U] extends OracleAction {
        def rolled1 : $[T]
        def rolled2 : $[U]
    }

    trait Rolled3Action[T, U, V] extends OracleAction {
        def rolled1 : $[T]
        def rolled2 : $[U]
        def rolled3 : $[V]
    }

    trait RandomAction[T] extends OracleAction {
        def random : T
    }

    trait SelfPerform { self : Action =>
        def perform(soft : Void)(implicit g : G) : Continue
    }

    trait WrappedAction extends SelfPerform with SoftOptional { self : Action =>
        def then : ForcedAction
        def soft = then.isSoft
        def perform(soft : Void)(implicit g : G) = then
    }

    trait SideEffectOnly extends ForcedAction {
        def then : ForcedAction
    }

    trait UserActionClass[A <: UserAction]

    trait UserAction extends ExternalAction with ActionClass[UserAction] { self : UserActionClass[_] =>
        def question(implicit g : G) : Elem
        def option(implicit g : G) : Elem
        def unary_+(implicit builder : ActionCollector) = builder.add(this)
    }

    trait Info extends UserAction with UserActionClass[Info] {
        override def canExpandTo(a : Action) : Boolean = false
    }

    object Info {
        def apply(o : (G => Elem)*) = InfoAction(o : _*)
    }

    trait Unavailable extends Info {
        def action : Choice
    }

    trait Back extends UserAction with UserActionClass[Back] with Soft

    trait Cancel extends UserAction with UserActionClass[Cancel] with Soft

    object Cancel {
        def as(q : (G => Elem)*) = WrapCancelAction(q : _*)
    }

    trait Choice extends UserAction with UserActionClass[Choice]

    case class DoAction(then : ForcedAction) extends Choice with WrappedAction {
        def question(implicit g : G) = Empty
        def option(implicit g : G) = then.toString
    }

    case class ForceAction(then : Choice) extends ForcedAction with SelfPerform with SoftOptional {
        def soft = then.isSoft
        def perform(soft : Void)(implicit g : G) = Force(then)
    }

    trait Soft { self : Action =>
        override def canExpandTo(a : Action) : Boolean = true
    }

    trait Only[T <: Action] extends ExpandCheck { self : Action with Soft =>
        implicit def tag : ClassTag[T]
        override def canExpandTo(a : Action) : Boolean = a.is[T] || (a @@ {
            case a : ExpandThen => canExpandTo(a.then)
            case _ => false
        })
    }

    trait ExpandThen { self : Action =>
        def then : ForcedAction
    }

    trait SoftOptional { self : Action =>
        def soft : Boolean
    }

    trait SoftMark

    trait SoftMarked extends SoftOptional { self : Action with Product =>
        def soft : Boolean = productIterator.exists(_.is[SoftMark])
    }

    implicit class SoftExAction(val a : Action) {
        def isSoft : Boolean = a @@ {
            case a : Soft => true
            case a : SoftOptional if a.soft => true
            case _ => false
        }
    }

    trait SelfExplode extends Soft { self : Choice with Soft =>
        def explode(withSoft : Boolean) : $[UserAction]
        def verifyX(a : UserAction) = a @@ {
            case a : Choice if a.isSoft => explode(true).has(a)
            case _ => explode(false).has(a)
        }
    }

    trait HalfExplode extends Soft { self : Choice with Soft =>
        def expand(target : |[Action]) : $[UserAction]
    }

    trait NoExplode extends SelfExplode { self : Choice with Soft =>
        def explode(withSoft : Boolean) = withSoft.$(self)
    }

    trait OutOfTurn { self : Action => }

    trait Retry { self : Action => }

    trait DontRecord { self : Choice => }

    trait Extra[T] extends Choice {
        def fromAny(s : Any) : |[T]
        def update(v : T) : Choice
        def validate(v : T) : Boolean
    }

    trait LimitedExtra[T] extends Extra[T] {
        def values : $[T]
        def validate(v : T) = values.contains(v)
    }

    trait ExtraInt extends Extra[Int] {
        def fromAny(s : Any) = s @@ {
            case n : Int => Some(n)
            case _ => None
        }
    }

    trait ActionCollector {
        def add(action : UserAction) : Unit
        def list : $[UserAction]
        def apply(f : F) : Ask = Ask(f).add(list)
    }

    private class UserActionListBuilder(private var actions : $[UserAction]) extends ActionCollector {
        def add(action : UserAction) {
            if (actions == null)
                throw new Error("adding to the drained action collector")

            actions = actions :+ action
        }

        def list = {
            val result = actions
            actions = null
            result
        }
    }

    def builder : ActionCollector = new UserActionListBuilder($)


    trait Continue extends GoodMatch {
        def unwrap = this
    }

    case object StartContinue extends Continue

    case class NoAsk(faction : F) {
        def apply(then : ForcedAction) : Continue = Ask(faction).add(then.as("Hobson's Choice"))
    }

    case class Ask(faction : F, actions : $[UserAction] = $) extends Continue {
        def group(l : (G => Elem)*) = add(InfoGroupAction(l : _*)())
        def available = actions.%{
            case i : Info => false
            case i : Hidden => false
            case _ => true
        }.any
        def choice = actions.%{
            case i : Info => false
            case _ => true
        }.num > 1
        def prepend(l : $[UserAction]) = copy(actions = l ++ actions)
        def prepend(a : UserAction) = copy(actions = a +: actions)
        def add(l : $[UserAction]) = copy(actions = actions ++ l)
        def add(a : UserAction) = copy(actions = actions :+ a)
        def when(cond : Boolean)(a : => UserAction) = if (cond) copy(actions = actions :+ a) else this
        def add(a : |[UserAction]) = copy(actions = actions ++ a)
        def +(l : $[UserAction]) = add(l)
        def +(a : UserAction) = add(a)
        def +(a : |[UserAction]) = add(a)
        def apply(l : $[UserAction]) = add(l)
        def apply(a : UserAction) = add(a)
        def apply(a : |[UserAction]) = add(a)
        def each[T](l : $[T])(f : T => UserAction) = add(l./(f))
        def each[T, U](l : $[(T, U)])(f : (T, U) => UserAction) = add(l./(f))
        def some[T](l : $[T])(f : T => IterableOnce[UserAction]) = add(l./~(f))
        def needOk = add(HiddenOkAction)
        def needOkIf(b : Boolean) = b.?(needOk).|(this)
        def cancel = add(CancelAction)
        def ocancel = add(choice.?(CancelAction))
        def cancelIf(cond : Boolean) = cond.?(cancel).|(this)
        def bail(then : => ForcedAction) = available.?(this).|(add(then.as("Cherry Bail Bonds")))
        def bailHard(then : => Action) = available.?(this).|(Force(then))
        def bailw(then : => Action)(before : => Unit) = available.?(this).|{ before ; Force(then) }
        def bailout(a : => UserAction) = available.?(this).|(add(a))
        def done(then : ForcedAction) = add(DoneAction(then))
        def done(then : |[ForcedAction]) = add(then./(x => DoneAction(x)))
        def doneIf(cond : Boolean)(then : ForcedAction) = cond.?(done(then)).|(this)
        def refuse(then : ForcedAction) = add(RefuseAction(then))
        def back(then : ForcedAction) = add(BackAction(then))
        def skip(then : ForcedAction) = add(SkipAction(then))

        val exploded = scala.collection.mutable.Map[G, $[Action]]()
    }

    case class MultiAsk(asks : $[Ask]) extends Continue

    case class Force(action : Action) extends Continue

    case class Roll[T](dice : $[Die[T]], roll : $[T] => RolledAction[T], tag : Any = None) extends Continue
    case class Roll2[T, U](dice1 : $[Die[T]], dice2 : $[Die[U]], roll : ($[T], $[U]) => Rolled2Action[T, U], tag : Any = None) extends Continue
    case class Roll3[T, U, V](dice1 : $[Die[T]], dice2 : $[Die[U]], dice3 : $[Die[V]], roll : ($[T], $[U], $[V]) => Rolled3Action[T, U, V], tag : Any = None) extends Continue
    case class Shuffle[T](list : $[T], shuffle : $[T] => ShuffledAction[T], tag : Any = None) extends Continue
    case class ShuffleUntil[T](list : $[T], shuffle : $[T] => ShuffledAction[T], condition : $[T] => Boolean, tag : Any = None) extends Continue
    case class Shuffle2[T, U](l1 : $[T], l2 : $[U], shuffle : ($[T], $[U]) => Shuffled2Action[T, U], tag : Any = None) extends Continue
    case class Shuffle3[T, U, V](l1 : $[T], l2 : $[U], l3 : $[V], shuffle : ($[T], $[U], $[V]) => Shuffled3Action[T, U, V], tag : Any = None) extends Continue
    case class Random[T](values : $[T], random : T => RandomAction[T], tag : Any = None) extends Continue

    case class Log(message : Elem, kind : LogKind, continue : Continue) extends Continue {
        override def unwrap = continue.unwrap
    }

    trait LogKind

    object LogKind {
        case object Normal extends LogKind
        case object Temp extends LogKind
    }

    case class Milestone(message : String, then : ForcedAction) extends Continue

    object Milestone {
        def apply(then : ForcedAction) : Milestone = Milestone("", then)
    }

    case class DelayedContinue(delay : Int, continue : Continue) extends Continue {
        override def unwrap = continue.unwrap
    }

    case class GameOver(winners : $[F], message : Elem, ask : $[UserAction] = $) extends Continue

    case object UnknownContinue extends Continue

    case object TryAgain extends Continue

    case class ErrorContinue(exception : Throwable, message : |[String]) extends Continue {
        error(message)
    }


    case class Notification(factions : $[F], infos : $[Info])

    trait UIEffect

    case class PrintLog(e : Elem) extends UIEffect
    case class PrintTempLog(e : Elem) extends UIEffect
    case class PrintWarning(e : Elem) extends UIEffect
    case class PrintError(e : Elem) extends UIEffect
    case class Delay(ms : Int) extends UIEffect

    case class PerformResult(continue : Continue, effects : $[UIEffect]) extends GoodMatch {
        def nest = effects.foldRight(continue)((e, c) => e @@ {
            case PrintLog(e) => Log(e, LogKind.Normal, c)
            case PrintTempLog(e) => Log(e, LogKind.Temp, c)
            case _ => +++("ignoring nest", e); c
        })
    }

    trait BaseGame {
        def continue : Continue
        def performVoid(a : ExternalAction) : Unit
        def performContinue(old : |[Continue], a : Action, validating : Boolean) : PerformResult
        def explode(actions : $[UserAction], withSoft : Boolean, target : |[Action]) : $[UserAction]
        def validate(continue : Continue, a : Action) : Boolean
        def info(waiting : $[F], self : |[F], actions : $[UserAction]) : $[Info]
        def isOver : Boolean

        def desc(s : Any*)(implicit mode : ConvertMode) : Elem
    }

    implicit val styles : StyleMapping

    implicit val mode : ConvertMode = ModeDefault

    def convertDesc(o : Any)(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = (g : G) => o @@ {
        case _ if o == null => throw new Error("DEADBEEF")

        case e : Elem => e
        case e : GameModeElementary => e.elem(g, mode)
        case e : GameElementary => e.elem(g)
        case e : Elementary => e.elem

        case ns : Named with Styling => Span(Text(ns.name), styles.get(ns))

        case s : String => Text(s)
        case n : Int => Text(n.toString)

        case None => Empty
        case Some(v) => convertDesc(v)(styles, mode)(g)

        case List(v) => convertDesc(v)(styles, mode)(g)

        case l : List[_] => List.unfold[$[Elem], $[Elem]](l.%(_ != "").%(_ != None).%(_ != Nil).%(_ != Empty)./(convertDesc)./(_(g))) {
            case Nil => None
            case Break :: tail => Some(tail.span(_ != Break))
            case l => Some(l.span(_ != Break))
        }./(_./~($(_, Join)).sliding(2).flatMap[Elem] {
            case Join :: (_ : Postfix) :: _ => None
            case a :: _ :: _ => Some(a)
        }.$.merge).join(Break)

        case t : Product if t.productIterator.isEmpty => throw new Error("empty product, maybe object " + o.getClass.getName + " " + o.toString)

        case t : Product => convertDesc(t.productIterator.$)(styles, mode)(g)

        case _ => ("[[unelementable undescribable " + o.getClass.getName + " " + o.toString + "]]").hl
    }

    object BaseGame {
        implicit def anyToDescE(o : Any)(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = convertDesc(o)
        implicit def listToDescE(o : List[_])(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = convertDesc(o)
        implicit def functionToDescErrorA(o : Function1[_, _])(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = ???
        implicit def functionToDescErrorB(o : Function1[_, _])(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = ???
    }

    trait LoggedGame extends BaseGame { self : G =>
        var logging : Boolean = true

        private case class LogElem(elem : Elem, kind : LogKind)

        private var logs : $[LogElem] = $

        private def writeLog(s : => LogElem) {
            logs = s +: logs
        }

        def desc(s : Any*)(implicit mode : ConvertMode) = convertDesc(s.$)(styles, mode)(this)

        def log(s : Any*) {
            if (logging)
                writeLog(LogElem(desc(s : _*)(ModeLog), LogKind.Normal))
        }

        def logtemp(s : Any*) {
            if (logging)
                writeLog(LogElem(desc(s : _*)(ModeLog), LogKind.Temp))
        }

        def handleError(e : Throwable, msg : |[String] = None) : PerformResult

        val voiding = new scala.util.control.Breaks

        def performRaw(action : Action, void : Boolean) : PerformResult = {
            try {
                val a = action.unwrap

                var c : Continue = UnknownContinue

                voiding.breakable {
                    val break = new CallbackBreakVoid(void, () => voiding.break(), () => {
                        if (a.isSoft) {
                            +++("soft-cut on soft action " + a)
                            throw new Error("soft-cut on soft action " + a)
                        }
                    })

                    c = loggedPerform(a, break)
                }

                val ll = logs.reverse
                logs = $

                PerformResult(c, ll./{
                    case LogElem(e, LogKind.Normal) => PrintLog(e)
                    case LogElem(e, LogKind.Temp) => PrintTempLog(e)
                })
            } catch {
                case NonFatal(e) if false => throw e
            }
        }

        def loggedPerform(action : Action, soft : Void) : Continue
    }

    trait ContinueGame extends BaseGame { self : G =>
        var continue : Continue = StartContinue

        var notifications : $[Notification] = $

        def notify(f : $[F], l : $[Info]) {
            notifications = Notification(f, l) :: notifications
        }

        def notify(f : F, l : $[Info]) {
            notifications = Notification($(f), l) :: notifications
        }

        def handleError(e : Throwable, msg : |[String] = None) : PerformResult = {
            continue = ErrorContinue(e, msg)
            PerformResult(continue, $(PrintError(msg.|("no message").txt)))
        }

        def performRaw(a : Action, void : Boolean) : PerformResult

        def mapForceLog(c : Continue) : Unit = c @@ {
            case e : ErrorContinue => continue = e
            case Force(a) => mapForceLog(performRaw(a, true).continue)
            case Log(l, k, c) => mapForceLog(c)
            case DelayedContinue(_, Force(then)) => mapForceLog(performRaw(then, true).continue)
            case _ =>
        }

        def performVoid(a : ExternalAction) {
            mapForceLog(a @@ {
                case _ if continue.is[ErrorContinue] => continue
                case a : CommentAction => UnknownContinue
                case ForceInvalidAction(a) => performRaw(a, true).continue
                case a : ForcedAction => performRaw(a, true).continue
                case a : ExternalAction => performRaw(a, true).continue
                case a => throw new Error("non-soft non-forced non-external action " + a)
            })
        }

        def performContinue(old : |[Continue], a : Action, validating : Boolean) : PerformResult = {
            try {
                old @@ {
                    case Some(ErrorContinue(e, m)) => return PerformResult(MultiAsk($), $(PrintLog(a.unwrap.toString.takeWhile(_ != '(').flatMap(c => c.isUpper.?(" " + c).|("o0oOo0oOo".toList.shuffle.take(2).mkString)))))
                    case _ =>
                }

                // VALIDATE
                if (validating && old.any) {
                    val valid = a.unwrap @@ {
                        case _ : OutOfTurn => true
                        case _ : SkipValidate => true
                        case _ if hrf.HRF.flag("valid-skip") || hrf.HRF.flag("skip-valid") => true
                        case _ => validate(old.get, a)
                    }

                    if (valid.not) {
                        if (hrf.HRF.flag("valid-ignore").not) {
                            val e = "perform validation failed\n" + a + "\n on \n" + continue @@ {
                                case Ask(f, l) => "Ask(" + f + "\n  " + l.mkString("\n  ") + "\n)\nExplode:\n  " + explode(l, true, None).mkString("\n  ")
                                case Roll(l, x, _) => a @@ {
                                    case a : RolledAction[_] => "expecting " + x(a.rolled)
                                    case _ => "(not a roll)"
                                }
                                case Random(l, x, _) => a @@ {
                                    case a : RandomAction[_] => "expecting " + x(a.random)
                                    case _ => "(not a random)"
                                }
                                case Shuffle(l, x, _) => a @@ {
                                    case a : ShuffledAction[_] => "expecting " + x(a.shuffled)
                                    case _ => "(not a shuffle)"
                                }
                                case c => ""
                            } + "\n" + continue.toString

                            return handleError(new Error(e), Some("perform validation failed"))
                        }
                        else
                        this @@ {
                            case g : LoggedGame =>
                                g.log("perform validation failed:")
                                g.log(a.toString)
                            case _ =>
                                error("perform validation failed:")
                                error(a)
                        }
                    }
                }

                // CANCEL BAIL
                a @@ {
                    case a : Cancel => return PerformResult(continue.unwrap, $)
                    case _ =>
                }

                def isAsk(c : Continue) : Boolean = c @@ {
                    case _ : Ask => true
                    case _ : MultiAsk => true
                    case Log(_, _, c) => isAsk(c)
                    case _ => false
                }

                // SOFT EXTERNAL DON'T MODIFY CONTINUE
                a @@ {
                    case a : ExternalAction if a.isSoft && isAsk(continue) =>
                        def mapForceSoft(c : Continue) : Continue = c @@ {
                            case Force(a) if a.isSoft => mapForceSoft(performRaw(a, false).nest)
                            case Force(a) => throw new Error("map force non-soft from soft external " + a)
                            case c => c
                        }

                        mapForceSoft(Force(a)) @@ {
                            case Log(l, k, c) => throw new Error("log on soft " + l)
                            case c => return PerformResult(c, $)
                        }

                    case a : ExternalAction if a.isSoft =>
                        warn("soft external on non-ask")
                        warn("action: " + a)
                        warn("continue: " + continue)

                    case _ =>
                }

                // PERFORM
                continue = a @@ {
                    case CommentAction(m) => continue.unwrap
                    case ForceInvalidAction(a) =>
                      warn("invalid action forced", a)

                      Log("Warning: ".spn ~ "invalid action forced".spn(xstyles.warning), LogKind.Normal, performRaw(a, false).nest)

                    case a : ForcedAction => performRaw(a, false).nest
                    case a : ExternalAction => performRaw(a, false).nest
                    case a => throw new Error("non-soft non-forced non-external action " + a)
                }

                // CANCEL ERROR CHECK -- maybe back as well??
                a @@ {
                    case a : ExternalAction => continue @@ {
                        case Ask(_, l) => l.foreach {
                            case b : Cancel => throw new Error("cancel on non-soft external action " + a)
                            case _ =>
                        }
                        case _ =>
                    }
                    case _ =>
                }

                // RECURSIVELY FORCE FORCEABLE
                def mapForceLog(c : Continue) : Continue = c @@ {
                    case Force(a) => performContinue(|(continue), a, validating).nest
                    case Log(l, k, c) => Log(l, k, mapForceLog(c))
                    case c => c
                }


                val result = mapForceLog(continue)

                // WARN IF NO LOG
                a @@ {
                    case a : ExternalAction => result @@ {
                        case Log(_, _, _) =>
                        case _ =>
                            // warn("no log on external action", a)
                    }
                    case _ =>
                }

                PerformResult(result, $)
            } catch {
                case NonFatal(e) => handleError(e)
            }
        }

        def explode(actions : $[UserAction], withSoft : Boolean, target : |[Action]) : $[UserAction] = {
            def performRepeat(action : Action) : $[UserAction] = {
                def desc = " " + action + "\n    while\n" + actions.mkString("\n")

                if (action.isSoft.not)
                    throw new Error("perform repeat non-soft action" + desc)

                val l = performRaw(action, false).continue @@ {
                    case ErrorContinue(e, m) => throw e
                    case Force(a) => performRepeat(a)
                    case DelayedContinue(n, Force(a)) => performRepeat(a)
                    case Log(_, _, _) => throw new Error("log on explode from" + desc)
                    case Ask(_, Nil) => throw new Error("empty ask fro" + desc)
                    case Ask(_, l) => l
                    case MultiAsk(aa) => aa./~(_.actions)
                    case _ => throw new Error("unknown continue in explode from" + desc)
                }

                l
            }

            def process(actions : $[UserAction], filtered : $[UserAction]) : $[UserAction] = {
                actions./~{
                    case a : Cancel => withSoft.$(a)
                    case a : Back => withSoft.$(a)
                    case a : SelfExplode => $(a)
                    case a : HalfExplode => $(a)
                    case a : SelfValidate => $(a)
                    case a : LimitedExtra[_] => a.values./(a.update)
                    case a : Extra[_] => throw new Error("explode unlimited extra")
                    case a : Info => $
                    case a : Hidden => $
                    case a => $(a)
                }.distinct.diff(filtered)./~{
                    case a : Cancel => withSoft.$(a)
                    case a : Back => withSoft.$(a)
                    case a : SelfExplode => a.explode(withSoft)
                    case a : HalfExplode => withSoft.$(a) ++ process(a.expand(target), a +: filtered)
                    case a : SelfValidate => $(a)
                    case a : Choice if a.isSoft => withSoft.$(a) ++ process(performRepeat(a), a +: filtered)
                    case a : UserAction => $(a)
                }
            }

            val result = process(actions, $)./~{
                case a if !withSoft && a.isSoft =>
                    None
                case a =>
                    Some(a)
            }

            if (result.none)
                throw new Error("empty explode of\n" + actions.mkString("\n"))

            result
        }

        def validate(continue : Continue, a : Action) : Boolean = {
            val action = a.unwrap

            if (action.isSoft)
                return validate(continue, a, true, true)

            if (validate(continue, a, true, false))
                return true

            // 0.8.106 --> 0.8.108
            a.unwrap.as[arcs.MoveListAction].foreach { a =>
                val b = a.copy(cascade = a.cascade.not).asInstanceOf[Action]

                if (validate(continue, b, true, false))
                    return true
            }

            // 0.8.106 --> 0.8.108
            a.unwrap.as[arcs.ReorderResourcesAction].foreach { a =>
                val b = a.copy(then = arcs.ContinueMultiAdjustResourcesAction(arcs.CheckWinAction)).asInstanceOf[Action]

                if (validate(continue, b, true, false))
                    return true
            }

            if (validate(continue, a, true, true).not)
                return false

            warn("validate failed with expansion filtering")
            +++("  action: " + a)
            +++("  continue: " + continue)

            true
        }

        protected def validate(c : Continue, action : Action, exploding : true, expandAll : Boolean) : Boolean = c @@ {
            case StartContinue if action.is[StartGameAction] => true
            case _ if action.unwrap.is[SkipValidate] => true
            case _ if action.unwrap.is[OutOfTurn] => true
            case Force(x) if action == x => true
            case Milestone(_, x) if action == x || action.unwrap == x.unwrap => true
            case DelayedContinue(_, c) => validate(c, action, exploding, expandAll)
            case Log(_, _, c) => validate(c, action, exploding, expandAll)
            case MultiAsk(l) => l.exists(ask => validate(ask, action, exploding, expandAll))
            case Ask(f, l) if l.contains(action) => true
            case Ask(f, l) if {
                val aa = action.unwrap
                l.exists(_.unwrap == aa)
            } => true
            case ask @ Ask(f, l : $[UserAction]) if exploding && {
                val aa = action.unwrap
                val ll = expandAll.?(l).|(l.%(_.canExpandTo(aa)))

                if (expandAll.not && ll.none)
                    return false

                val lll =
                    if (expandAll)
                        ask.exploded.getOrElseUpdate(this, explode(ll, true, None))
                    else
                        explode(ll, true, |(action))

                lll.contains(action) || lll.exists(_.unwrap == aa) || lll./(_.unwrap).of[SelfValidate].exists(_.validate(aa))
            } => true
            case Shuffle(l, x, _) => action @@ {
                case a : ShuffledAction[_] => action == x(a.shuffled) && a.shuffled.toSet == l.toSet
                case _ => false
            }
            case ShuffleUntil(l, x, p, _) => action @@ {
                case a : ShuffledAction[_] => action == x(a.shuffled) && a.shuffled.toSet == l.toSet && p(a.shuffled)
                case _ => false
            }
            case Shuffle2(l1, l2, x, _) => action @@ {
                case a : Shuffled2Action[_, _] => action == x(a.shuffled1, a.shuffled2) && a.shuffled1.toSet == l1.toSet && a.shuffled2.toSet == l2.toSet
                case _ => false
            }
            case Shuffle3(l1, l2, l3, x, _) => action @@ {
                case a : Shuffled3Action[_, _, _] => action == x(a.shuffled1, a.shuffled2, a.shuffled3) && a.shuffled1.toSet == l1.toSet && a.shuffled2.toSet == l2.toSet && a.shuffled3.toSet == l3.toSet
                case _ => false
            }
            case Roll(l, x, _) => action @@ {
                case a : RolledAction[_] => action == x(a.rolled) && l.lazyZip(a.rolled).forall((die, r) => die.valid(r))
                case _ => false
            }
            case Roll2(l1, l2, x, _) => action @@ {
                case a : Rolled2Action[_, _] => action == x(a.rolled1, a.rolled2) && l1.lazyZip(a.rolled1).forall((die, r) => die.valid(r)) && l2.lazyZip(a.rolled2).forall((die, r) => die.valid(r))
                case _ => false
            }
            case Roll3(l1, l2, l3, x, _) => action @@ {
                case a : Rolled3Action[_, _, _] => action == x(a.rolled1, a.rolled2, a.rolled3) && l1.lazyZip(a.rolled1).forall((die, r) => die.valid(r)) && l2.lazyZip(a.rolled2).forall((die, r) => die.valid(r)) && l3.lazyZip(a.rolled3).forall((die, r) => die.valid(r))
                case _ => false
            }
            case Random(l, x, _) => action @@ {
                case a : RandomAction[_] => action == x(a.random) && l.contains(a.random)
                case _ => false
            }
            case _ => false
        }
    }

    abstract class ElemAction(q : Elem)(o : Elem) { self : UserAction =>
        def question(implicit g : G) = q
        def option(implicit g : G) = o
    }

    abstract class HelperAction(s : String) extends ElemAction(Empty)(s) with WrappedAction { self : UserAction => }

    case class WrapAction(then : ForcedAction)(o : (G => Elem)*) extends BaseUserAction()(o : _*) with WrappedAction with Choice with SoftOptional {
        def apply(q : (G => Elem)*) = WrapQAction(then)(q : _*)(o : _*)
        def noClear = WrapQNoClearAction(then)()(o : _*)
    }

    case class WrapViewAction[T](obj : T, then : ForcedAction)(view : T => Any) extends BaseUserAction()(view(obj)) with WrappedAction with Choice with SoftOptional with ViewObject[T]

    case class WrapQAction(then : ForcedAction)(q : (G => Elem)*)(o : (G => Elem)*) extends BaseUserAction(q : _*)(o : _*) with WrappedAction with Choice {
        def noClear = WrapQNoClearAction(then)(q : _*)(o : _*)
    }

    case class WrapQNoClearAction(then : ForcedAction)(q : (G => Elem)*)(o : (G => Elem)*) extends BaseUserAction(q : _*)(o : _*) with WrappedAction with Choice with NoClear

    case class WrapCancelAction(o : (G => Elem)*) extends BaseUserAction()(o : _*) with Cancel {
        def apply(q : (G => Elem)*) = WrapCancelQAction(q : _*)(o : _*)
    }

    case class WrapCancelQAction(q : (G => Elem)*)(o : (G => Elem)*) extends BaseUserAction(q : _*)(o : _*) with Cancel


    case class InfoAction(o : (G => Elem)*) extends BaseUserAction()(o : _*) with Info {
        def apply(q : (G => Elem)*) = InfoGroupAction(q : _*)(o : _*)
    }

    case class InfoGroupAction(q : (G => Elem)*)(o : (G => Elem)*) extends BaseUserAction(q : _*)(o : _*) with Info

    case object CancelAction extends ElemAction(Empty)("Cancel") with Cancel
    case class BackAction(then : ForcedAction) extends HelperAction("Back") with Back
    case class RefuseAction(then : ForcedAction) extends HelperAction("Refuse") with Choice
    case class SkipAction(then : ForcedAction) extends HelperAction("Skip") with Choice
    case class OkAction(then : ForcedAction) extends HelperAction("Ok") with Choice
    case class DoneAction(then : ForcedAction) extends HelperAction("Done") with Choice

    trait Hidden { self : UserAction =>
        def question(implicit g : G) = throw new Error("hidden info question " + this)
        def option(implicit g : G) = throw new Error("hidden info option " + this)
    }

    abstract class HiddenInfo extends Info with Hidden

    abstract class HiddenChoice extends Choice with Hidden

    case object HiddenOkAction extends HiddenChoice

    trait FactionAction {
        def self : F
    }

    abstract class BaseUserAction(q : (G => Elem)*)(o : (G => Elem)*) {
        def question(implicit g : G) = convertDesc(q.$./(_(g)))(styles, ModeDefault)(g)
        def option(implicit g : G) = convertDesc(o.$./(_(g)))(styles, ModeDefault)(g)
    }

    abstract class BaseAction(q : (G => Elem)*)(o : (G => Elem)*) extends BaseUserAction(q : _*)(o : _*) with Choice with FactionAction
    abstract class BaseInfo(q : (G => Elem)*)(o : (G => Elem)*) extends BaseUserAction(q : _*)(o : _*) with Info

    abstract class OptionUserAction(o : (G => Elem)*) { self : UserAction =>
        def option(implicit g : G) = convertDesc(o.$./(_(g)))(styles, ModeDefault)(g)
    }

    abstract class OptionAction(o : (G => Elem)*) extends OptionUserAction(o : _*) with Choice
    abstract class OptionInfo(o : (G => Elem)*) extends OptionUserAction(o : _*) with Info


    implicit class FactionActionEx(a : UserAction) {
        def x(b : Boolean, reason : String = "") = a @@ {
            case a : Choice => b.not.?(a).|(UnavailableReasonAction(a, reason))
            case _ => a
        }

        def !(b : Boolean, reason : String = "") = a @@ {
            case a : Choice => b.not.?(a).|(UnavailableReasonAction(a, reason))
            case _ => a
        }
    }

    case class UnavailableReasonAction(action : Choice, reason : String) extends Unavailable {
        def question(implicit g : G) : Elem = action.question(g)
        def option(implicit g : G) : Elem = action.option(g)
    }

    abstract class BaseStubAction extends FactionAction { self : UserAction =>
        def question = (g : G) => "TO DO"
        def option = (g : G) => toString
    }

    trait AskResult
    case object AskHuman extends AskResult
    case object WaitRemote extends AskResult
    case class AskBot(action : $[UserAction] => UserAction) extends AskResult

    trait GameUI {
        var currentGame : G = _
        var overrideGame : Option[G] = None
        implicit def game : G = overrideGame.||(currentGame.?).|(throw new Error("no current no override game"))
        def factionElem(f : F) : Elem
        def start() : Unit
        def updateStatus() : Unit
        def updateHighlight(a : |[UserAction]) : Unit = {}
        def onClick(a : Any) : Unit
        def wait(self : $[F], factions : $[F])
        def ask(faction : |[F], actions : $[UserAction], then : UserAction => Unit)
        def shadowAsk(faction : F, actions : $[UserAction]) : |[UserAction] = None
        def alog(e : Elem, n : Int, onClick : Any => Unit, delayed : Boolean = false) : hrf.ui.LazyBlock
        def blog() : hrf.html.Container
        def showNotifications(self : $[F]) : Unit = {}
        def describeActionForUndo(a : ExternalAction, self : |[F]) : $[UndoDescriptor]
        def rewind(n : Int, actions : $[ExternalAction], speed : |[Int], latest : () => Unit, undoTo : (Int, Int) => Unit, playback : Int => Unit) : Unit = {}
        def stopRewind() : Unit = {}
    }

    trait UndoDescriptor extends GoodMatch {
        def elem(count : Int) : Elem
    }

    trait Selectable { self : UserAction =>
        def selected : Boolean
    }

    trait Selected extends Selectable { self : UserAction =>
        final def selected = true
    }

    implicit class AnyActionSelected(val self : UserAction) {
        def selected = self @@ {
            case a : Selectable => a.selected
            case _ => false
        }
    }

    trait NoClear { self : UserAction => }

    trait OnClickInfo extends NoClear { self : UserAction =>
        def param : Any
    }

    trait ViewObject[T] { self : UserAction =>
        def obj : T
    }

    trait JumpTo { self : Action =>
        def desc(l : $[Action]) : Elem
    }

    trait ElemWrap { self : UserAction =>
        def wrap(g : G)(e : Elem) : Elem
    }

    case class RawElemInfo(q : Elem)(o : Elem) extends ElemAction(q)(o) with Info with ElemWrap {
        def wrap(g : G)(e : Elem) = o
    }

    case class RawElemGroup(q : Elem) extends ElemAction(q)(Empty) with Info with ElemWrap {
        def wrap(g : G)(e : Elem) = Empty
    }

    case object BreakAction extends ElemAction(Empty)(HorizontalBreak) with Info with ElemWrap {
        def wrap(g : G)(e : Elem) = HorizontalBreak
    }

}
