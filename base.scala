package hrf.base
//
//
//
//
import logger._, colmat._
//
//
//
//

import hrf.elem._

@scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
trait Record extends Product

trait BasePlayer

sealed trait Difficulty

case object Off extends Difficulty
case object Recorded extends Difficulty
case object Human extends Difficulty
case class BotDebug(name : String) extends Difficulty
case class Bot(name : String) extends Difficulty
case object AllVsHuman extends Difficulty

sealed trait DefaultChoice

object DefaultChoice {
    case object None extends DefaultChoice
    case class First(timeout : Double) extends DefaultChoice
    case class Last(timeout : Double) extends DefaultChoice
}


trait Gaming {
    type F <: BasePlayer
    type G <: BaseGame

    type Record = hrf.base.Record
    type BasePlayer = hrf.base.BasePlayer

    trait ConvertMode
    case object ModeDefault extends ConvertMode
    case object ModeLog extends ConvertMode
    
    trait GameModeElementary {
        def elem(g : G, mode : ConvertMode) : Elem
    }
    
    trait GameElementary {
        def elem(g : G) : Elem
    }
    
    trait Elementary {
        def elem : Elem
    }
    
    trait Message extends Record with GameElementary {
        def ~(m : Message) = CompoundMessage(this, m)
    }

    trait ElementaryMessage extends Message with Elementary {
        def elem(g : G) : Elem = elem
    }
    
    case object NoMessage extends Message {
        def elem(g : G) = Empty
    }
    
    case class CompoundMessage(a : Message, b : Message) extends Message with GameModeElementary {
        def elem(g : G) = a.elem(g) ~ b.elem(g)
        def elem(g : G, mode : ConvertMode) = convertDesc(a)(styles, mode)(g) ~ convertDesc(b)(styles, mode)(g)
    }

    trait Named {
        def name : String
        def elem : Elem = this match {
            case s : Styling => Span(Text(name), styles.get(s))
            case _ => Text(name)
        }
    }

    implicit def named2elem(n : Named) = ElemContainer(n.elem)
    
    implicit def action2force(fa : ForcedAction) = Force(fa)
    

    
    trait ActionClass[A <: Action]
    
    sealed trait Action extends Record { self : ActionClass[_] => 
        def unwrap : Action = this match {
            case a : ThenAction => a.then.unwrap
            case a : Cancel => CancelAction
            case a => a
        }
    }

    trait ForcedAction extends Action with ActionClass[ForcedAction] {
        def as(q : (G => Elem)*) = WrapAction(this)(q : _*)
    }
    
    trait ExternalAction extends Action { self : ActionClass[_] => }
    
    trait SkipValidate { self : Action => }

    case class ForceInvalidAction(action : ExternalAction) extends ExternalAction with ActionClass[ForceInvalidAction] with SkipValidate
    
    trait OracleAction extends ExternalAction with ActionClass[OracleAction]

    trait StartGameAction extends ExternalAction with ActionClass[StartGameAction]

    case object WaitAction extends ForcedAction

    trait ShuffledAction[T] extends OracleAction {
        def shuffled : List[T]
    }

    trait RolledAction[T] extends OracleAction {
        def rolled : List[T]
    }

    trait RandomAction[T] extends OracleAction {
        def random : T
    }

    trait SelfPerform { self : Action => 
        def perform(g : G) : Continue
    }
    
    trait ThenAction extends SelfPerform with SoftOptional { self : Action =>
        def then : ForcedAction
        def soft = then.isSoft
        def perform(g : G) = then
    }
  
    case class MilestoneAction(then : ForcedAction) extends OracleAction with SelfPerform {
        def perform(g : G) = then
    }

    trait SideEffectOnly extends ForcedAction {
        def then : ForcedAction
    }
    
    trait UserActionClass[A <: UserAction] 
 
    trait UserAction extends ExternalAction with ActionClass[UserAction] { self : UserActionClass[_] =>
        def question(g : G) : Elem
        def option(g : G) : Elem
    }

    trait NeverExplode { self : UserAction => } 
    
    trait Info extends UserAction with UserActionClass[Info] with NeverExplode

    object Info {
        def apply(q : (G => Elem)*) = InfoAction(q : _*)
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
    
    case class DoAction(then : ForcedAction) extends Choice with ThenAction {
        def question(g : G) = Empty
        def option(g : G) = then.toString
    }
    
    case class ForceAction(then : Choice) extends ForcedAction with SelfPerform with SoftOptional {
        def soft = then.isSoft
        def perform(g : G) = Force(then)
    }
    
    trait Soft { self : Action => }

    trait SoftOptional { self : Action => 
        def soft : Boolean
    }

    trait SoftMark

    trait SoftMarked extends SoftOptional { self : Action with Product => 
        def soft : Boolean = productIterator.exists(_.isInstanceOf[SoftMark])
    }

    implicit class SoftExAction(val a : Action) {
        def isSoft : Boolean = a match {
            case a : Soft => true
            case a : SoftOptional if a.soft => true
            case _ => false
        }
    }

    trait SelfExplode extends Soft { self : Choice with Soft => 
        def explode(withSoft : Boolean) : List[UserAction]
        def verifyX(a : UserAction) = a match {
            case a : Choice if a.isSoft => explode(true).has(a)
            case _ => explode(false).has(a)
        }
    }
    
    trait HalfExplode extends Soft { self : Choice with Soft => 
        def expand(withBack : Boolean) : List[UserAction]
    }
    
    trait NoExplode extends SelfExplode with NeverExplode { self : Choice with Soft =>
        def explode(withSoft : Boolean) = withSoft.??($(self))
    }

    trait OutOfTurn { self : Choice => }

    trait Retry { self : Choice => }

    trait DontRecord { self : Choice => }
    
    trait SpecialAction { self : Choice => }
    
    trait Extra[T] extends Choice {
        def fromAny(s : Any) : Option[T]
        def update(v : T) : Choice
        def validate(v : T) : Boolean
    }
    
    trait LimitedExtra[T] extends Extra[T] {
        def values : List[T]
        def validate(v : T) = values.contains(v)
    }
    
    trait ExtraInt extends Extra[Int] {
        def fromAny(s : Any) = s match {
            case n : Int => Some(n)
            case _ => None
        }
    }
    
    object NewEQ {
        trait SideEffect
        case class LogEffect(message : Elem) extends SideEffect
        case class TempLogEffect(message : Elem) extends SideEffect

        trait ExternalQuery
        case object Start extends ExternalQuery
        case class Ask(faction : F, actions : List[UserAction], default : DefaultChoice = DefaultChoice.None) extends ExternalQuery
        case class Roll[T](xquestion : Any, dice : List[Die[T]], roll : List[T] => RolledAction[T]) extends ExternalQuery
        case class Shuffle[T](xquestion : Any, cards : List[T], shuffle : List[T] => ShuffledAction[T]) extends ExternalQuery
        case class ShuffleUntil[T](xquestion : Any, cards : List[T], shuffle : List[T] => ShuffledAction[T], condition : List[T] => Boolean) extends ExternalQuery
        case class Random[T](xquestion : Any, values : List[T], random : T => RandomAction[T]) extends ExternalQuery
        case class MultiAsk(asks : List[Ask]) extends ExternalQuery
    }

    trait Continue
    case class Ask(faction : F, actions : List[UserAction] = Nil, default : DefaultChoice = DefaultChoice.None) extends Continue {
        def available = actions.%{
            case i : Info => false
            case i : Hidden => false
            case _ => true
        }.any
        def choice = actions.%{
            case i : Info => false
            case i : Hidden => false
            case _ => true
        }.num > 1
        def apply(l : List[UserAction]) = copy(actions = actions ++ l)
        def apply(a : UserAction) = copy(actions = actions :+ a)
        def apply(a : Option[UserAction]) = copy(actions = actions ++ a)
        def needOk = apply(HiddenOkAction)
        def cancel = apply(CancelAction)
        def ocancel = apply(choice.?(CancelAction))
        def refuse(then : ForcedAction) = apply(RefuseAction(then))
        def bail(then : Action) = available.?(this).|(Force(then))
        def bailw(then : Action)(before : => Unit) = available.?(this).|{ before; Force(then) }

        val exploded = scala.collection.mutable.Map[G, List[Action]]()
    }
    case class Force(action : Action) extends Continue
    case class DelayedContinue(delay : Int, continue : Continue) extends Continue

    case class Roll[T](dice : List[Die[T]], roll : List[T] => RolledAction[T], tag : Any = None) extends Continue
    case class Shuffle[T](cards : List[T], shuffle : List[T] => ShuffledAction[T], tag : Any = None) extends Continue
    case class ShuffleUntil[T](cards : List[T], shuffle : List[T] => ShuffledAction[T], condition : List[T] => Boolean, tag : Any = None) extends Continue
    case class Random[T](values : List[T], random : T => RandomAction[T], tag : Any = None) extends Continue

    case class Milestone(then : ForcedAction) extends Continue

    case class Notify(factions : List[F], infos : List[Info], then : ForcedAction) extends Continue
    case class GameOver(winners : List[F], message : Elem, asks : List[Ask]) extends Continue
    case object UnknownContinue extends Continue
    case class ErrorContinue(message : String) extends Continue {
        ===(message)
    }
    case object StartContinue extends Continue
    case class Log(message : Elem, kind : LogKind, then : Continue) extends Continue 
    case class MultiAsk(asks : List[Ask]) extends Continue
    

    trait LogKind

    object LogKind {
        case object Normal extends LogKind
        case object Temp extends LogKind
    }

    implicit class ActionsEx(val l : List[UserAction]) {
        def xavailable = l.%{
            case i : Info => false
            case _ => true
        }.any
        
        def choice = l.%{
            case i : Info => false
            case _ => true
        }.num != 1

        def done(then : ForcedAction) : List[UserAction] = l :+ DoneAction(then)
        def done(then : Option[ForcedAction]) : List[UserAction] = l ++ then./(x => DoneAction(x))
        def ok(then : ForcedAction) : List[UserAction] = l :+ OkAction(then)
        def refuse(then : ForcedAction) : List[UserAction] = l :+ RefuseAction(then)
        def skip(then : ForcedAction) : List[UserAction] = l :+ SkipAction(then)
        def cancel : List[UserAction] = l :+ CancelAction
        def cancelIf(b : Boolean) : List[UserAction] = l ++ b.?(CancelAction)
        def ocancel : List[UserAction] = l ++ l.choice.?(CancelAction)
        def back(then : ForcedAction) : List[UserAction] = l :+ BackAction(then)
        def okNeeded = l ++ l.choice.not.?(HiddenOkAction)
        def apply(a : UserAction) : List[UserAction] = l :+ a
        def apply(a : Option[UserAction]) : List[UserAction] = l ++ a
    }
    
    trait BaseGame {
        def continue : Continue
        def perform(a : Action) : Continue
        def performContinue(a : Action) : Continue
        def explode(actions : List[UserAction], withSoft : Boolean = false) : List[UserAction]
        def validate(a : Action) : Boolean
        def info(waiting : List[F], self : Option[F], actions : List[UserAction]) : List[UserAction with Info]
    }
    
    implicit val styles : StyleMapping
    
    implicit val mode : ConvertMode = ModeDefault

    def convertDesc(o : Any)(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = (g : G) => o match {
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
        
        case l : List[_] => List.unfold[List[Elem], List[Elem]](l.%(_ != "").%(_ != None).%(_ != Nil).%(_ != Empty)./(convertDesc)./(_(g))) {
            case Nil => None
            case Break :: tail => Some(tail.span(_ != Break))
            case l => Some(l.span(_ != Break))
        }./(_.join(" ")).join(Break)

        case t : Product if t.productIterator.isEmpty => throw new Error("empty product, maybe object " + o.getClass.getName + " " + o.toString)
        
        case t : Product => convertDesc(t.productIterator.toList)(styles, mode)(g)
        
        case _ => throw new Error("unelementable undescribable " + o.getClass.getName + " " + o.toString)
    }
    
    object BaseGame {
        implicit def any2descE(o : Any)(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = convertDesc(o)
        implicit def list2descE(o : List[_])(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = convertDesc(o)
        implicit def function2descErrorA(o : Function1[_, _])(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = ???
        implicit def function2descErrorB(o : Function1[_, _])(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = ???
        implicit def option2descE(o : G => Option[Any])(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = (g => o(g)./(convertDesc)./(_(g)).|(Empty))
        implicit def option2descEX(o : G => Option[Any])(implicit styles : StyleMapping, mode : ConvertMode) : G => Elem = (g => o(g)./(convertDesc)./(_(g)).|(Empty))
    }
    
    trait LoggedGame extends BaseGame { self : G =>
        def logging : Boolean
        
        private case class LogElem(elem : Elem, kind : LogKind)

        private var logs : List[LogElem] = Nil
        
        private def writeLog(s : => LogElem) {
            logs = s +: logs
        }
    
        def desc(s : Any*)(implicit mode : ConvertMode) = convertDesc(s.toList)(styles, mode)(this)
        
        def log(s : Any*) {
            if (logging)
                writeLog(LogElem(desc(s : _*)(ModeLog), LogKind.Normal))
        }

        def logtemp(s : Any*) {
            if (logging)
                writeLog(LogElem(desc(s : _*)(ModeLog), LogKind.Temp))
        }
        
        
        def perform(a : Action) : Continue = {
            var c = loggedPerform(a)
                          
            logs.foreach { l => c = Log(l.elem, l.kind, c) }
            logs = Nil
    
            c
        }
        
        def loggedPerform(a : Action) : Continue
    }

    trait ContinueGame extends BaseGame { self : G =>
        var continue : Continue = StartContinue
        
        def performContinue(a : Action) : Continue = {
            // VALIDATE
            if (hrf.HRF.flag("skipvalid").not && !validate(a)) {
                if (hrf.HRF.flag("unvalid").not)
                    return ErrorContinue("perform validation failed\n" + a + "\n on \n" + (continue match {
                        case Ask(f, l, _) => "Ask(" + f + "\n  " + l.mkString("\n  ") + "\n)\nExplode:\n  " + explode(l, true).mkString("\n  ")
                        case Roll(l, x, _) => a match {
                            case a : RolledAction[_] => "expecting " + x(a.rolled)
                            case _ => "(not a roll)"
                        }
                        case Random(l, x, _) => a match {
                            case a : RandomAction[_] => "expecting " + x(a.random)
                            case _ => "(not a random)"
                        }
                        case c => ""
                    }) + "\n" + continue.toString)
                else 
                this match {
                    case g : LoggedGame =>
                        g.log("perform validation failed:")
                        g.log(a.toString)
                    case _ =>
                        ===("perform validation failed:")
                        ===(a.toString)
                }
            }
                
            // CANCEL BAIL
            a match {
                case a : Cancel => return continue
                case _ => 
            }

            // SOFT EXTERNAL DON'T MODIFY CONTINUE
            a match {
                case a : ExternalAction if a.isSoft && (continue.isInstanceOf[Ask] || continue.isInstanceOf[MultiAsk]) => 
                    def mapForceSoft(c : Continue) : Continue = c match {
                        case Force(a) if a.isSoft => mapForceSoft(perform(a))
                        case Force(a) => throw new Error("map force non-soft from soft external " + a)
                        case c => c
                    }
                
                    mapForceSoft(Force(a)) match {
                        case Log(l, k, c) => throw new Error("log on soft " + l)
                        case c => return c
                    }

                case a : ExternalAction if a.isSoft => 
                    println("whoops")
                    println("a: " + a)
                    println("continue: " + continue)

                case _ =>
            }

            // PERFORM
            continue = a match {
                case ForceInvalidAction(a) => ===("invalid action forced"); ===(a); Log(Text("warning: invalid action forced"), LogKind.Normal, perform(a))
                case a : ForcedAction => perform(a)
                case a : ExternalAction => perform(a)
                case a => throw new Error("non-soft non-forced non-external action " + a)
            }
            
            // CANCEL ERROR CHECK -- MAYBE BACK AS WELL??
            a match {
                case a : ExternalAction => continue match {
                    case Ask(_, l, _) => l.foreach {
                        case b : Cancel => throw new Error("cancel on non-soft external action " + a)
                        case _ =>
                    }
                    case _ =>
                }
                case _ =>
            }

            // RECURSIVELY FORCE FORCEABLE 
            def mapForceLog(c : Continue) : Continue = c match {
                case Force(a) => performContinue(a)
                case Log(l, k, c) => Log(l, k, mapForceLog(c))
                case c => c
            }
    
            val result = mapForceLog(continue)

            // WARN IF NO LOG
            a match {
                case a : ExternalAction => result match {
                    case Log(_, _, _) =>
                    case _ => ===("no log on external action " + a)
                }
                case _ => 
            }

            result
        }

        def performContinueNonRecursiveObsolete(aa : Action) : Continue = {
            var a = aa
            var ll : List[(Elem, LogKind)] = Nil

            while (true) {
                if (!validate(a))
                    throw new Error("perform validation failed\n" + a + "\n on \n" + (continue match {
                        case Ask(f, l, _) => "Ask(" + f + "\n  " + l.mkString("\n  ") + "\n)"
                        case Roll(l, x, _) => a match {
                            case a : RolledAction[_] => "expecting " + x(a.rolled)
                            case _ => "(not a roll)"
                        }
                        case Random(l, x, _) => a match {
                            case a : RandomAction[_] => "expecting " + x(a.random)
                            case _ => "(not a random)"
                        }
                        case c => ""
                    }) + "\n" + continue.toString)
            
                continue = a match {
                    case ForceInvalidAction(a) => ===("invalid action forced"); ===(a); Log(Text("warning: invalid action forced"), LogKind.Normal, perform(a))
                    case a if a.isSoft => perform(a)
                    case a : ForcedAction => perform(a)
                    case a : ExternalAction => perform(a)
                    case a => throw new Error("non-soft non-forced non-external action " + a)
                }
                
                a match {
                    case a if a.isSoft => 
                    case a : ForcedAction => 
                    case a : ExternalAction => continue match {
                        case Ask(_, l, _) => l.foreach {
                            case b : Cancel => throw new Error("cancel on non-soft external action " + a)
                            case _ =>
                        }
                        case _ =>
                    }
                }
            
                var c = continue

                while (c match {
                    case Log(l, k, cc) => 
                        ll +:= (l, k)
                        c = cc
                        true
                    case _ =>
                        false
                }) {}

                c match {
                    case Force(aaa) => 
                        a = aaa
                    case _ => 
                        ll.foreach { case (l, k) => c = Log(l, k, c) }
                        return c
                }
            }

            null
        }
        
        def explode(actions : List[UserAction], withSoft : Boolean = false) : List[UserAction] = {
            def performRepeat(action : Action) : List[UserAction] = {
                if (action.isSoft.not)
                    throw new Error("performRepeat non-soft action " + action + "\n    while\n" + actions.mkString("\n"))
                    
                val l = perform(action) match {
                    case Force(a) => performRepeat(a)
                    case DelayedContinue(n, Force(a)) => performRepeat(a)
                    case Log(_, _, _) => throw new Error("log on explode from " + action + "\n    while\n" + actions.mkString("\n"))
                    case Ask(_, Nil, _) => throw new Error("empty ask from " + action + "\n    while\n" + actions.mkString("\n"))
                    case Ask(_, l, _) => l
                    case MultiAsk(aa) => aa./~(_.actions)
                    case _ => throw new Error("unknown continue in explode from " + action + "\n    while\n" + actions.mkString("\n"))
                }

                l
            }
            
            def process(actions : List[UserAction], filtered : List[UserAction]) : List[UserAction] = {
                actions./~{
                    case a : Cancel => withSoft.??($(a))
                    case a : Back => withSoft.??($(a))
                    case a : SelfExplode => $(a)
                    case a : HalfExplode => $(a)
                    case a : LimitedExtra[_] => a.values./(a.update)
                    case a : Extra[_] => throw new Error("explode unlimited extra")
                    case a : Info => Nil
                    case a : Hidden => Nil
                    case a => List(a)
                }.distinct.diff(filtered)./~{
                    case a : Cancel => withSoft.??($(a))
                    case a : Back => withSoft.??($(a))
                    case a : SelfExplode => a.explode(withSoft)
                    case a : HalfExplode => withSoft.??($(a)) ++ process(a.expand(withSoft), a +: filtered)
                    case a : Choice if a.isSoft => withSoft.??($(a)) ++ process(performRepeat(a), a +: filtered)
                    case a : UserAction => $(a)
                }
            }
            
            val result = process(actions, Nil)./~{
                case a if !withSoft && a.isSoft => { println("soft action from non-soft explode:\n    " + a + "\nfrom" + actions./("\n    " + _)); None }
                case a => Some(a)
            }
    
            if (result.none)
                throw new Error("empty explode of\n" + actions.mkString("\n"))
                
            result
        }
        
        def validate(a : Action) : Boolean = validate(continue, a)
    
        protected def validate(c : Continue, a : Action) : Boolean = c match {
            case _ if a.isInstanceOf[OutOfTurn] => true
            case _ if a.isInstanceOf[SkipValidate] => true
            case StartContinue if a.isInstanceOf[StartGameAction] => true
            case Force(x) => a == x
            case DelayedContinue(_, c) => validate(c, a)
            case Milestone(x) => a == MilestoneAction(x) || a == x || a.unwrap == x.unwrap
            case Log(_, _, c) => validate(c, a)
            case Ask(_, l, _) if l.contains(a) => true
            case ask @ Ask(_, l : List[UserAction], _) if { 
                val ll = ask.exploded.getOrElseUpdate(this, explode(l, true))
                val dt = a.unwrap

                ll.contains(a) || ll.exists(w => w.unwrap == dt)
            } => true
            case MultiAsk(l) => l.%(validate(_, a)).any
            case Shuffle(l, x, _) => a match {
                case a : ShuffledAction[_] => a == x(a.shuffled) && a.shuffled.toSet == l.toSet
                case _ => false
            }
            case ShuffleUntil(l, x, p, _) => a match {
                case a : ShuffledAction[_] => a == x(a.shuffled) && a.shuffled.toSet == l.toSet && p(a.shuffled)
                case _ => false
            }
            case Roll(l, x, _) => a match {
                case a : RolledAction[_] => a == x(a.rolled) && l.lazyZip(a.rolled).forall((die, r) => die.values.has(r))
                case _ => false
            }
            case Random(l, x, _) => a match {
                case a : RandomAction[_] => a == x(a.random) && l.contains(a.random)
                case _ => false
            }
            case Notify(_, _, x) => x == a
            case _ => false
        }
    }
    
    case class Die[T](values : List[T])
    
    abstract class ElemAction(q : Elem)(o : Elem) { self : UserAction =>
        def question(g : G) = q
        def option(g : G) = o
    }
    
    abstract class HelperAction(s : String) extends ElemAction(Empty)(s) with ThenAction { self : UserAction => }
    
    case class WrapAction(then : ForcedAction)(o : (G => Elem)*) extends BaseUserAction()(o : _*) with ThenAction with Choice {
        def apply(q : (G => Elem)*) = WrapQAction(then)(q : _*)(o : _*)
    }
    case class WrapQAction(then : ForcedAction)(q : (G => Elem)*)(o : (G => Elem)*) extends BaseUserAction(q : _*)(o : _*) with ThenAction with Choice

    case class WrapCancelAction(o : (G => Elem)*) extends BaseUserAction()(o : _*) with Cancel {
        def apply(q : (G => Elem)*) = WrapCancelQAction(q : _*)(o : _*)
    }
    case class WrapCancelQAction(q : (G => Elem)*)(o : (G => Elem)*) extends BaseUserAction(q : _*)(o : _*) with Cancel


    case class InfoAction(q : (G => Elem)*) extends BaseUserAction()(q : _*) with Info

    case object CancelAction extends ElemAction(Empty)("Cancel") with Cancel
    case class BackAction(then : ForcedAction) extends HelperAction("Back") with Back
    case class RefuseAction(then : ForcedAction) extends HelperAction("Refuse") with Choice
    case class SkipAction(then : ForcedAction) extends HelperAction("Skip") with Choice
    case class OkAction(then : ForcedAction) extends HelperAction("Ok") with Choice
    case class DoneAction(then : ForcedAction) extends HelperAction("Done") with Choice
    
    trait Hidden { self : UserAction =>
        def question(g : G) = throw new Error("Hidden info Q")
        def option(g : G) = throw new Error("Hidden info O")
    }

    abstract class HiddenInfo extends Info with Hidden
    
    abstract class HiddenChoice extends Choice with Hidden
    
    case object HiddenOkAction extends HiddenChoice

    trait FactionAction {
        def self : F
    }
    
    abstract class BaseUserAction(q : (G => Elem)*)(o : (G => Elem)*) {
        def question(g : G) = convertDesc(q.toList./(_(g)))(styles, ModeDefault)(g)
        def option(g : G) = convertDesc(o.toList./(_(g)))(styles, ModeDefault)(g)
    }
 
    abstract class BaseAction(q : (G => Elem)*)(o : (G => Elem)*) extends BaseUserAction(q : _*)(o : _*) with Choice with FactionAction
    abstract class BaseInfo(q : (G => Elem)*)(o : (G => Elem)*) extends BaseUserAction(q : _*)(o : _*) with Info with FactionAction
    
    abstract class OptionUserAction(o : (G => Elem)*) { self : UserAction => 
        def option(g : G) = convertDesc(o.toList./(_(g)))(styles, ModeDefault)(g)
    }
 
    abstract class OptionAction(o : (G => Elem)*) extends OptionUserAction(o : _*) with Choice
    abstract class OptionInfo(o : (G => Elem)*) extends OptionUserAction(o : _*) with Info
 
    
    implicit class FactionActionEx(a : UserAction) {
        def x(b : Boolean, reason : String = "") = a match {
            case a : Choice => b.not.?(a).|(UnavailableReasonAction(a, reason))
            case _ => a
        }
    }
    
    case class UnavailableReasonAction(action : Choice, reason : String) extends Unavailable {
        def question(g : G) : Elem = action.question(g)
        def option(g : G) : Elem = action.option(g)
    }
    
    abstract class BaseStubAction extends FactionAction { self : UserAction => 
        def question = (g : G) => "TO DO"
        def option = (g : G) => toString
    }
    
    trait AskResult
    case object AskHuman extends AskResult
    case object WaitRemote extends AskResult
    case class AskBot(action : List[UserAction] => UserAction) extends AskResult

    trait GameUI {
        var overrideGame : G = _
        def updateStatus() : Unit
        def updateHighlight(a : Option[UserAction]) : Unit = {}
        def alog(e : Elem, n : Int, onClick : Any => Unit) : hrf.html.HtmlBlock
        def notify(factions : List[F], infos : List[UserAction], then : => Unit)
        def wait(self : List[F], factions : List[F], notifies : List[Notify])
        def ask(faction : F, actions : List[UserAction], waiting : List[F], notifies : List[Notify], then : UserAction => Unit)
        def start() : Unit
    }
    
    trait Selectable { self : UserAction => 
        def selected : Boolean
    }

    trait Selected extends Selectable { self : UserAction => 
        final def selected = true
    }

    implicit class AnyActionSelected(val self : UserAction) {
        def selected = self match {
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

    trait ElemWrap { self : UserAction =>
        def wrap(g : G)(e : Elem) : Elem
    }
    
    case class RawElemInfo(q : Elem)(o : Elem) extends ElemAction(q)(o) with Info with ElemWrap {
        def wrap(g : G)(e : Elem) = o
    }

    case object BreakAction extends ElemAction(Empty)(HorizontalBreak) with Info with ElemWrap {
        def wrap(g : G)(e : Elem) = HorizontalBreak
    }


}
