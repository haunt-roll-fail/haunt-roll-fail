package suok

import colmat._

import hrf.tracker._

import hrf.elem._

trait Side extends Record with Named with Styling {
    def name = toString
}

case object Loyal extends Side
case object Rebel extends Side
case object Neutral extends Side

case object Effect extends Styling

abstract class Role(val name : String, val image : String, val side : Side) extends Record with Elementary {
    def elem = name.styled(side)
    def short = name.take(1).styled(side)
    def long = Image(image, styles.rcard)
}

case object Sultan extends Role("Sultan", "sultan", Loyal)
case object Guard extends Role("Guard", "guard", Loyal)

case object Assassin extends Role("Assassin", "assassin", Rebel)
case object Slave extends Role("Slave", "slave", Rebel) {
    override def short = "X".styled(Rebel)
}

case object Dancer extends Role("Dancer", "dancer", Neutral)
case object Vizier extends Role("Vizier", "vizier", Neutral)
case object Hunter extends Role("Hunter", "hunter", Neutral)
case object Oracle extends Role("Oracle", "oracle", Neutral)

object Roles {
    def neutrals = Dancer :: Vizier :: Hunter :: Oracle
}

trait Faction extends BasePlayer with Named with Styling {
    def name = toString
    def short = name.take(1)
    def style = name.toLowerCase
    def ss = name.take(2).styled(this)
}

case object Red extends Faction
case object Green extends Faction
case object Blue extends Faction
case object Yellow extends Faction
case object White extends Faction
case object Violet extends Faction
case object Pink extends Faction
case object Teal extends Faction
case object Orange extends Faction
case object Cyan extends Faction

case object HiddenRole extends Faction with Elementary {
    override def elem = Text("hidden role")
}

trait ViewCard { self : UserAction => }

case object StartAction extends StartGameAction
case class ShuffledNeutralsAction(shuffled : List[Role]) extends ShuffledAction[Role]
case class ShuffledRolesAction(shuffled : List[Role]) extends ShuffledAction[Role]

case object TurnStartAction extends ForcedAction
case class MainAction(f : Faction) extends ForcedAction
case object TurnEndAction extends ForcedAction

case class ViewRoleInfoAction(self : Faction, f : Faction, role : Role) extends BaseInfo("")(f.elem ~ " " ~ role.long) with ViewCard

case class ViewRolesAction(self : Faction, l : List[Faction]) extends BaseAction("View Roles")(l)
case class ViewingRolesAction(self : Faction, l : List[Faction]) extends ForcedAction


trait MainQuestion extends FactionAction {
    def question(g : Game) = self.elem
}

case class PeekMainAction(self : Faction) extends OptionAction("Peek") with MainQuestion with Soft
case class PeekAction(self : Faction, f : Faction) extends BaseAction("Peek")(f)
case class PeekViewAction(self : Faction, f : Faction) extends ForcedAction

case class HideMainAction(self : Faction, exchange : Boolean) extends OptionAction("Hide") with MainQuestion

case class OpenExchangeMainAction(self : Faction) extends OptionAction("Exchange") with MainQuestion with Soft
case class OpenExchangeAction(self : Faction, f : Faction) extends BaseAction("Exchange")(f)
case class OpenExchangeHiddenAction(self : Faction) extends BaseAction("Exchange")("Hidden Role")

case class SecretExchangeAction(self : Faction, f : Faction) extends BaseAction("Exchange")(f)
case class SecretExchangeHiddenAction(self : Faction) extends BaseAction("Exchange")("Hidden Role")
case class SecretExchangeNoExchangeAction(self : Faction) extends BaseAction("Exchange")("Done")

case class RoyalClaimAction(self : Faction, then : ForcedAction) extends OptionAction("Royal Claim".styled(Loyal)) with MainQuestion

case class ContinueRevoltAction(then : ForcedAction) extends ForcedAction
case class SupportRevoltAction(self : Faction, then : ForcedAction) extends BaseAction("Slave Revolt".styled(Rebel))("Support".styled(Rebel)) with Retry
case class IgnoreRevoltAction(self : Faction, then : ForcedAction) extends BaseAction("Slave Revolt".styled(Rebel))("Ignore") with Retry
case class CallRevoltAction(self : Faction, then : ForcedAction) extends OptionAction("Revolt".styled(Rebel)) with MainQuestion

case class AssassinateMainAction(self : Faction) extends OptionAction("Assassinate".styled(Rebel)) with MainQuestion with Soft
case class AssassinateAction(self : Faction, e : Faction) extends BaseAction("Assassinate".styled(Rebel))(e)
case class ContinueAssassinateAction(self : Faction, e : Faction) extends ForcedAction
case class InterceptAction(self : Faction, a : Faction, e : Faction) extends BaseAction("Intercept", a, "assassinating", e)("Intercept".styled(Loyal))
case class StandByAction(self : Faction, a : Faction, e : Faction) extends BaseAction("Intercept", a, "assassinating", e)("Stand By")

case class ExecuteMainAction(self : Faction) extends OptionAction("Execute".styled(Loyal)) with MainQuestion with Soft
case class ExecuteAction(self : Faction, e : Faction) extends BaseAction("Execute".styled(Loyal))(e)

case class ManipulateMainAction(self : Faction) extends OptionAction("Manipulate".styled(Neutral)) with MainQuestion with Soft
case class ManipulateRevealAction(self : Faction) extends OptionAction("Manipulate".styled(Neutral)) with MainQuestion with Soft
case class ManipulateForcedAction(self : Faction) extends ForcedAction
case class ManipulateAction(self : Faction, e : Faction) extends BaseAction("Manipulate".styled(Neutral))(e)

case class OracleMainAction(self : Faction) extends OptionAction("Foretell".styled(Neutral)) with MainQuestion with Soft

case class DancerMainAction(self : Faction) extends OptionAction("Dance".styled(Neutral)) with MainQuestion

case class HunterMainAction(self : Faction) extends OptionAction("Hunt".styled(Neutral)) with MainQuestion with Soft
case class HunterAction(self : Faction, e : Faction) extends BaseAction("Hunt".styled(Neutral))(e)

case class GameOverAction(side : Side, m : Elem) extends ForcedAction

case class ArrestMainAction(self : Faction) extends OptionAction("Arrest".styled(Loyal)) with MainQuestion with Soft
case class ArrestAction(self : Faction, e : Faction) extends BaseAction("Arrest".styled(Loyal))(e)
case class ContinueArrestAction(self : Faction, e : Faction) extends ForcedAction
case class AvoidArrestAction(self : Faction) extends BaseAction("Arrest".styled(Loyal))("Avoid Arrest".styled(Loyal))
case class GoToJailAction(self : Faction) extends BaseAction("Arrest".styled(Loyal))("Go to Jail")

case class ChooseSideAction(self : Faction, side : Side, then : ForcedAction) extends BaseAction("Choose Side")(side)

class Player(val faction : Faction, var role : Role, val roles : List[Role], val nn : Int) {
    def flip() = math.random() > 0.999 
    var alive : Boolean = true && !flip()
    var open : Boolean = false || !alive || flip()
    var coronation : Boolean = false || flip()
    var tired : Boolean = false || flip()
    var choice : Side = Neutral
    var jailed : Boolean = false || flip()
    var chained : Boolean = false || flip()
    var marked : Boolean = false
    var blocked : List[Faction] = Nil
    
    def side = role match {
        case Vizier => open.?(choice).|(Neutral)
        case Oracle => open.?(choice).|(Neutral)
        case Dancer => open.?(Rebel).|(Loyal)
        case Hunter => open.?(Loyal).|(Rebel)
        case r => r.side
    }
 
    val xknowledge = new XKnowledge(roles, nn, null)
    val expectation = new XKnowledge(roles, nn, xknowledge)
}

class Knowledge {
    var full : List[Faction] = Nil
    var notSlave : List[Faction] = Nil
    var roles : List[Role] = Nil
}

class XKnowledge(val roles : List[Role], val nn : Int, val parent : XKnowledge) {
    var children : List[XKnowledge] = Nil

    if (parent != null)
        parent.children :+= this

    var states : List[Map[Faction, List[Role]]] = Nil
    var neutrals : List[Role] = Nil

    def is(e : Faction, r : Role) {
        states = states./~(m => m(e).contains(r).?(m + (e -> $(r)))).distinct
        
        if (r.side == Neutral)
            if (neutrals.has(r).not) {
                neutrals :+= r
                
                if (neutrals.num == nn) {
                    val out = Roles.neutrals.diff(neutrals)
     
                    states = states./(_.view.mapValues(l => l.diff(out)).toMap)
                }
            }

        optimize()

        children.foreach(_.is(e, r))
    }
    
    def isnt(e : Faction, r : Role) {
        states = states./~(m => m(e).but(r).any.?(m + (e -> m(e).but(r))))

        optimize()

        children.foreach(_.isnt(e, r))
    }

    def exchange(f : Faction, e : Faction) {
        states = states./(m => m + (f -> m(e)) + (e -> m(f)))

        optimize()

        children.foreach(_.exchange(f, e))
    }
    
    def secretExchange(f : Faction, l : List[Faction]) {
        states = states./~(m => l./(t => m + (f -> m(t)) + (t -> m(f))))

        optimize()

        children.foreach(_.secretExchange(f, l))
    }
 
    def optimize() {
        var s = states

        var p = s
        p = null

        while (p != s) {
            p = s

            s = s./{ m =>
                val claimed = m.values./~(_.distinct.single).toList

                val filled = claimed.distinct.%(r => claimed.count(r) == roles.count(r))

                m.view.mapValues(l => l.distinct.single./($(_)).|(l.diff(filled))).toMap
            }./~(m => m.values.%(_.none).none.?(m))
        }

        states = s.distinct

        if (states.none) {
            if (parent != null) {
                println("conflict, switching to parent")

                states = parent.states
                neutrals = parent.neutrals
            }
            else 
                throw new Error("empty state and no parent")
        }
    }
}

class Game(val setup : List[Faction], val logging : Boolean) extends BaseGame with ContinueGame with LoggedGame {
    type F = Faction

    var players = Map[Faction, Player]()
    var roles : List[Role] = Nil
    var hidden : Role = null

    val factions = setup
    var current : Faction = factions(0)
    var highlight : List[Faction] = Nil

    var uroles : List[Role] = Nil
    
    implicit def faction2player(f : Faction) = players(f)
    
    def neighbours(f : Faction) = {
        val alive = factions.%(_.alive)
        val next = (alive ++ alive).dropWhile(_ != f).drop(1).take(1)
        val prev = (alive ++ alive).reverse.dropWhile(_ != f).drop(1).take(1)
        (next ++ prev).but(f).distinct
    }

    def next(f : Faction) = {
        val alive = factions.%(_.alive)
        (alive ++ alive).dropWhile(_ != f)(1)
    }

    def prev(f : Faction) = {
        val alive = factions.%(_.alive)
        (alive ++ alive).reverse.dropWhile(_ != f)(1)
    }

    var over = false

    var turn : Int = 0
   
    def loggedPerform(action : Action) : Continue = {
        println("> " + action)
        
        val c = performZ(action)
        
        c match {
            case Ask(_, Nil, _) =>
                println("")
                println("")
                println("")
                println("WTF!!!")
                println("Empty Ask as a result of " + action)
            case _ =>
        }

        println("    < " + c)

        c
    }

    implicit class ActionEx(action : Action) {
        def vr(f : Faction) : List[UserAction] = action match {
            case fa : ForcedAction =>
                Nil 
            case _ => log("Why " + action + " isn't forced?"); Nil
        }
    
        def vrl(self : Faction, then : ForcedAction) = $(ViewRoleInfoAction(self, self, self.role))
    }

    implicit class ActionsExExEx(l : List[UserAction])(implicit a : Action) {
        def vr(f : Faction) : List[UserAction] = l ++ a.vr(f)
    }
    
    def preinfo(waiting : List[Faction], self : Option[Faction], actions : List[UserAction]) : List[UserAction with Info] = Nil

    def info(waiting : List[Faction], self : Option[Faction], actions : List[UserAction]) : List[UserAction with Info] = {
        self.%(players.contains)./~(WaitAction.vrl(_, WaitAction))
    }
    
    def performZ(a : Action) : Continue = {
        implicit val action = a
        
        def withClaim(c : Continue) : Continue = {
            a match {
                case then : ForcedAction =>
                    val hs = (players.num == factions.num).?~(factions.%(_.alive).%(_.open.not).%(_.role == Sultan).single)
            
                    hs./(s => c match {
                        case c if current == s && current.marked.not => c
                        case Ask(f, ll, x) if f == s => Ask(f, ll :+ RoyalClaimAction(f, then), x)
                        case a @ Ask(f, ll, x) if f != s => MultiAsk(a :: Ask(f, RoyalClaimAction(s, then) :: HiddenOkAction))
                        case MultiAsk(aa) if aa.%(_.faction == s).any => MultiAsk(aa./(a => a match {
                            case Ask(f, ll, x) if f == s => Ask(f, ll :+ RoyalClaimAction(s, then), x)
                            case a => a
                        }))
                        case MultiAsk(aa) if aa.%(_.faction == s).none => MultiAsk(aa :+ Ask(s, RoyalClaimAction(s, then) :: HiddenOkAction))
                        case c => c
                    }).|(c)
                    
                case _ => throw new Error("claim on non-forced action")
            }
        }

        def reveal(f : Faction) {
            if (f.open.not) {
                f.open = true

                factions.foreach(_.xknowledge.is(f, f.role))

                log(f, "revealed", f.role)

                if (f.role == Sultan)
                    current.coronation = true
            }
        }
        
        def kill(f : Faction) {
            f.open = true
            f.alive = false
            f.jailed = false
            f.chained = false
            f.tired = false

            factions.foreach(_.xknowledge.is(f, f.role))
            
            if (f.role == Hunter)
                factions.foreach(_.chained = false)
        }
        
        action match {
            // HELPERS
            case BackAction(then) =>
                then
    
            case DoneAction(then) =>
                then
                    
            // INIT
            case StartAction =>
                Shuffle(Roles.neutrals, ll => ShuffledNeutralsAction(ll), "Shuffle neutrals")
                
            case ShuffledNeutralsAction(neutrals) =>
                roles = factions.num match {
                    case  5 => Sultan * 1 ++ Guard * 1 ++ Assassin * 1 ++ Slave * 3
                    case  6 => Sultan * 1 ++ Guard * 1 ++ Assassin * 1 ++ Slave * 3 ++ neutrals.take(1)
                    case  7 => Sultan * 1 ++ Guard * 1 ++ Assassin * 1 ++ Slave * 3 ++ neutrals.take(2)
                    case  8 => Sultan * 1 ++ Guard * 2 ++ Assassin * 2 ++ Slave * 3 ++ neutrals.take(1)
                    case  9 => Sultan * 1 ++ Guard * 2 ++ Assassin * 2 ++ Slave * 3 ++ neutrals.take(2)
                    case 10 => Sultan * 1 ++ Guard * 2 ++ Assassin * 2 ++ Slave * 3 ++ neutrals.take(3)
                    case 11 => Sultan * 1 ++ Guard * 2 ++ Assassin * 2 ++ Slave * 4 ++ neutrals.take(3)
                    case 12 => Sultan * 1 ++ Guard * 3 ++ Assassin * 3 ++ Slave * 4 ++ neutrals.take(2)
                    case 13 => Sultan * 1 ++ Guard * 3 ++ Assassin * 3 ++ Slave * 4 ++ neutrals.take(3)
                    case 14 => Sultan * 1 ++ Guard * 3 ++ Assassin * 3 ++ Slave * 4 ++ neutrals.take(4)
                    case 15 => Sultan * 1 ++ Guard * 3 ++ Assassin * 3 ++ Slave * 5 ++ neutrals.take(4)
                }
                
                Shuffle(roles, rr => ShuffledRolesAction(rr), "Shuffle roles")

            case ShuffledRolesAction(roles) =>
                log("Shuffled roles")
                
                uroles = roles.%(_.side != Neutral) ++ Roles.neutrals

                factions.lazyZip(roles).foreach { (f, r) =>
                    players += f -> new Player(f, r, uroles, roles.%(_.side == Neutral).num)
                }

                hidden = roles.last

                
                factions.foreach { f =>
                    f.xknowledge.states = $((factions :+ HiddenRole)./(e => e -> uroles.distinct).toMap)
                }

                factions.foreach { f =>
                    f.xknowledge.is(f, f.role)
                }

                TurnStartAction
            
            // ROLE
            case RoyalClaimAction(f, then) =>
                reveal(f)

                then
                
            case CallRevoltAction(f, then) =>
                reveal(f)

                log(f, "called for revolt")

                ContinueRevoltAction(then)
                
            case ContinueRevoltAction(then) =>
                if (factions.%(_.open).%(_.role == Slave).num == roles.%(_ == Slave).num)
                    then
                else {
                    val l = factions.%(_.alive).%(_.open.not).%(_.jailed.not).%(_.marked.not)
        
                    if (l.any)
                        withClaim(MultiAsk(l./(f => Ask(f, (f.role == Slave).?(SupportRevoltAction(f, then) :: IgnoreRevoltAction(f, then)).|(IgnoreRevoltAction(f, then) :: HiddenOkAction)))))
                    else
                        then
                }
            
            case SupportRevoltAction(f, then) =>
                reveal(f)

                factions.foreach(_.xknowledge.is(f, f.role))
                
                highlight :+= f

                log(f, "supported revolt")

                ContinueRevoltAction(then)

            case IgnoreRevoltAction(f, then) =>
                f.marked = true
                
                val p = prev(f)
                val pp = prev(p)

                val n = next(f)
                val nn = next(n)

                def r(e : Faction) = e.role == Slave && e.open && e.jailed.not && e.chained.not

                if ((r(p) && r(pp)) || (r(p) && r(n)) || (r(n) && r(nn)))
                    factions.but(f).foreach(_.expectation.isnt(f, Slave))

                ContinueRevoltAction(then)
                
            case AssassinateMainAction(f) =>
                Ask(f, factions.but(f).%(_.alive)./(e => AssassinateAction(f, e)).cancel)
                
            case AssassinateAction(f, e) =>
                reveal(f)
                
                log(f, "assassinated", e)

                if (e.role.side != Rebel) {
                    val gg = (neighbours(f) ++ neighbours(e)).but(f).but(e).%(_.open.not).%(_.jailed.not).%(g => neighbours(g).%(d => d.open && d.jailed.not && d.role == Dancer).none)
                    
                    factions.foreach { o => 
                        gg.but(o).foreach { g =>
                            o.expectation.isnt(g, Guard) 
                        }    
                    }
                }

                highlight :+= e

                ContinueAssassinateAction(f, e)

            case ContinueAssassinateAction(f, e) =>
                val h = factions.%(_.open).%(_.role == Guard).num < roles.%(_ == Guard).num

                val gg = (neighbours(f) ++ neighbours(e)).but(f).but(e).%(g => g.role == Guard || (g.open.not && h)).%(_.jailed.not).%(_.marked.not).%(g => neighbours(g).%(d => d.open && d.jailed.not && d.role == Dancer).none)
                
                if (gg.any) 
                    withClaim(MultiAsk(gg./(g => Ask(g, (g.role == Guard).?(InterceptAction(g, f, e) :: StandByAction(g, f, e)).|(StandByAction(g, f, e) :: HiddenOkAction)))))
                else {
                    log(e, "was killed")
                 
                    kill(e)

                    TurnEndAction
                }
                
            case InterceptAction(f, a, e) =>
                reveal(f)

                kill(a)
                
                highlight :+= f
                
                log(f, "intercepted", a)

                TurnEndAction

            case StandByAction(f, a, e) =>
                f.marked = true
                
                ContinueAssassinateAction(a, e)
                                                                          
            case ExecuteMainAction(f) =>
                Ask(f, factions.but(f).%(_.alive).%(_.open).%(_.role.side == Rebel)./(e => ExecuteAction(f, e)).cancel)
                
            case ExecuteAction(f, e) =>
                reveal(f)

                kill(e)

                highlight :+= f

                log(f, "executed", e)
                
                TurnEndAction

            case ArrestMainAction(f) =>
                Ask(f, factions.but(f).%(_.alive).%(_.jailed.not).%(e => e.open.not || e.role.side != Loyal)./(e => ArrestAction(f, e)).cancel)
                
            case ArrestAction(f, e) =>
                reveal(f)

                highlight :+= e

                e.jailed = true
 
                log(f, "arrested", e)

                if (e.open)
                    TurnEndAction
                else
                    ContinueArrestAction(f, e)
                
            case ContinueArrestAction(f, e) =>
                if (e.role == Sultan)
                    Ask(e, AvoidArrestAction(e) :: GoToJailAction(e))
                else
                    withClaim(
                        if (e.role.side == Loyal)
                            Ask(e, AvoidArrestAction(e) :: GoToJailAction(e))
                        else
                            Ask(e, GoToJailAction(e) :: HiddenOkAction)
                    )

            case AvoidArrestAction(f) =>
                reveal(f)

                f.jailed = false

                log(f, "avoided arrest")

                TurnEndAction

            case GoToJailAction(e) =>
                factions.but(e).foreach(_.expectation.isnt(e, Sultan))
                factions.but(e).foreach(_.expectation.isnt(e, Guard))

                TurnEndAction

            case ChooseSideAction(f, side, then) =>
                reveal(f)

                f.choice = side

                log(f, "chose", side)
                
                then
                
            case ManipulateRevealAction(f) =>
                Ask(f, (ChooseSideAction(f, Loyal, ManipulateForcedAction(f)) :: ChooseSideAction(f, Rebel, ManipulateForcedAction(f))).cancel)
                
            case ManipulateForcedAction(f) =>
                f.marked = true

                Force(ManipulateMainAction(f))

            case ManipulateMainAction(f) =>
                Ask(f, factions.but(f).%(_.alive).%(_.open.not)./(e => ManipulateAction(f, e)) ++ f.marked.not.?(CancelAction))
                
            case ManipulateAction(f, e) =>
                log(f, "manipulated", e)
                
                e.tired = true

                e.marked = true

                reveal(e)

                Force(MainAction(e))
                
            case OracleMainAction(f) =>
                val l = factions.but(f).filter(_.open.not)
                
                if (l.num <= 3)
                    Ask(f, $(ViewRolesAction(f, l)))
                else
                    Ask(f, l.combinations(3).toList./(ViewRolesAction(f, _)))
                
            case ViewRolesAction(f, l) =>
                reveal(f)

                log(f, "viewed roles of", l)

                l.foreach(e => f.xknowledge.is(e, e.role))
                
                ViewingRolesAction(f, l)

            case ViewingRolesAction(f, l) =>
                withClaim(Ask(f, l./(e => ViewRoleInfoAction(f, e, e.role)) :+ ChooseSideAction(f, Loyal, TurnEndAction) :+ ChooseSideAction(f, Rebel, TurnEndAction)))
                
            case HunterMainAction(f) =>
                Ask(f, factions.but(f).%(_.alive).%(_.chained.not).%(e => e.open.not || e.role == Slave)./(e => HunterAction(f, e)).cancel)
                
            case HunterAction(f, e) if e.open && e.role == Slave =>
                e.chained = true

                log(f, "chained", e)
                
                TurnEndAction
                
            case HunterAction(f, e) if e.open.not =>
                log(f, "investigated", e)

                if (e.role == Slave) {
                    reveal(e)

                    e.chained = true
                    
                    log(f, "captured", e)

                    MainAction(f)
                }
                else {
                    factions.foreach(_.xknowledge.isnt(e, Slave))

                    TurnEndAction
                }
                
            case DancerMainAction(f) =>
                reveal(f)

                TurnEndAction
                    
            // COMMON
            case PeekMainAction(f) =>
                Ask(f, factions.but(f).%(_.alive).%(_.open.not)./(e => PeekAction(f, e)).cancel)
                
            case PeekAction(f, e) =>
                log(f, "peeked at", e)

                f.xknowledge.is(e, e.role)
                
                highlight :+= e

                PeekViewAction(f, e)
                
            case PeekViewAction(f, e) =>
                withClaim(Ask(f, List(ViewRoleInfoAction(f, e, e.role)).done(TurnEndAction)))
                
            case OpenExchangeMainAction(f) =>
                Ask(f, (factions.but(f).%(_.alive).%(_.open.not).%(_.jailed.not).diff(f.blocked)./(e => OpenExchangeAction(f, e)) :+ OpenExchangeHiddenAction(f)).cancel)
                
            case OpenExchangeAction(f, e) =>
                val r = f.role
                f.role = e.role
                e.role = r

                e.blocked :+= f

                highlight :+= e
                
                factions.foreach { o =>
                    o.xknowledge.exchange(f, e)
                }
                
                f.xknowledge.is(f, f.role)
                e.xknowledge.is(e, e.role)

                log(f, "exchanged with", e)

                TurnEndAction
                
            case OpenExchangeHiddenAction(f) =>
                val r = f.role
                f.role = hidden
                hidden = r

                factions.foreach { o =>
                    o.xknowledge.exchange(f, HiddenRole)
                }

                f.xknowledge.is(f, f.role)

                log(f, "exchanged with hidden role")
            
                TurnEndAction
                
            case HideMainAction(f, exchange) =>
                f.open = false
                
                if (f.role == Sultan)
                    factions.foreach(_.coronation = false)

                if (f.role == Hunter)
                    factions.foreach(_.chained = false)

                if (exchange) {
                    log(f, "went into hiding and secretly exchanged role")
                    
                    Ask(f, (factions.but(f).%(_.alive).%(_.open.not).%(_.jailed.not).diff(f.blocked)./(e => SecretExchangeAction(f, e)) :+ SecretExchangeHiddenAction(f) :+ SecretExchangeNoExchangeAction(f)))
                }                                                                                                                                                                                          
                else {
                    log(f, "went into hiding") 

                    TurnEndAction
                }

            case SecretExchangeAction(f, e) =>
                val r = f.role
                f.role = e.role
                e.role = r

                e.blocked :+= f

                var hh = factions.%(_.open.not) :+ HiddenRole
                
                factions.but(f).foreach { o =>
                    o.xknowledge.secretExchange(f, hh)
                }

                f.xknowledge.exchange(f, e)

                f.xknowledge.is(f, f.role)

                hh.but(HiddenRole).foreach { o =>
                    o.xknowledge.is(o, o.role)
                }
                
                TurnEndAction
                
            case SecretExchangeHiddenAction(f) =>
                val r = f.role
                f.role = hidden
                hidden = r

                var hh = factions.%(_.open.not) :+ HiddenRole
                
                factions.but(f).foreach { o =>
                    o.xknowledge.secretExchange(f, hh)
                }

                f.xknowledge.exchange(f, HiddenRole)

                f.xknowledge.is(f, f.role)

                hh.but(HiddenRole).foreach { o =>
                    o.xknowledge.is(o, o.role)
                }
                
                TurnEndAction
                
            case SecretExchangeNoExchangeAction(f) =>
                var hh = factions.%(_.open.not) :+ HiddenRole
                
                factions.but(f).foreach { o =>
                    o.xknowledge.secretExchange(f, hh)
                }

                f.xknowledge.is(f, f.role)

                hh.but(HiddenRole).foreach { o =>
                    o.xknowledge.is(o, o.role)
                }

                TurnEndAction

            // TURN
            case TurnStartAction =>
                val f = current

                if (factions.%(_.alive.not).%(_.role == Sultan).any)
                    GameOverAction(Rebel, "Sultan Assassinated".styled(Rebel))
                else
                if ((factions.%(_.alive) ++ factions.%(_.alive))./(f => f.open && f.jailed.not && f.chained.not && f.role == Slave).containsSlice(true :: true :: true))
                    GameOverAction(Rebel, "Slave Revolt".styled(Rebel))
                else
                if (factions.%(_.alive).%(_.open.not).none && factions.%(_.alive).%(_.side != Rebel).none)
                    GameOverAction(Rebel, "All Rebel".styled(Rebel))
                else
                if (factions.%(_.alive).%(_.open.not).none && factions.%(_.alive).%(_.side != Loyal).none)
                    GameOverAction(Loyal, "All Loyal".styled(Loyal))
                else
                if (roles.diff(factions.%(_.alive.not)./(_.role)).has(Assassin).not && roles.diff(factions.%(_.alive.not)./(_.role)).diff(factions.%(_.alive).%(_.open).%(f => f.jailed || f.chained)./(_.role)).%(_ == Slave).num < 3)
                    GameOverAction(Loyal, "Loyal Domination".styled(Loyal))
                else
                if (f.coronation)
                    GameOverAction(Loyal, "Successful Coronation".styled(Loyal))
                else
                if (f.alive.not)
                    TurnEndAction
                else {
                    log(DoubleLine)
                    turn += 1
                    log("Turn", ("#" + turn).hl, f)
                    
                    if (f.jailed)
                        TurnEndAction
                    else
                    if (f.chained)
                        TurnEndAction
                    else
                    if (f.open && f.role == Oracle)
                        Force(HideMainAction(f, false))
                    else
                        MainAction(f)
                }

            case MainAction(f) =>
                var actions = List[UserAction]()
                
                if (f.marked.not) {
                    if (factions.but(f).%(_.alive).%(_.open.not).any)
                        actions :+= PeekMainAction(f)
                        
                    if (f.open.not)
                        actions :+= OpenExchangeMainAction(f)
                    else
                        actions :+= HideMainAction(f, f.tired.not)
                }
 
                if (f.marked || f.tired.not) {
                    if (f.role == Slave)
                        actions :+= CallRevoltAction(f, TurnEndAction)
                     
                    if (f.role == Assassin)
                        actions :+= AssassinateMainAction(f)
                    
                    if (f.role == Sultan) {
                        if (factions.but(f).%(_.alive).%(_.open).%(_.role.side == Rebel).any)
                            actions :+= ExecuteMainAction(f)

                        if (f.open.not)
                            actions :+= RoyalClaimAction(f, MainAction(f))
                    }
                            
                    if (f.role == Guard)
                        if (neighbours(f).%(_.open).%(_.jailed.not).%(_.role == Dancer).none)
                            actions :+= ArrestMainAction(f)

                    if (f.role == Vizier)
                        if (f.open)
                            actions :+= ManipulateMainAction(f)
                        else
                            actions :+= ManipulateRevealAction(f)

                    if (f.role == Oracle)
                        actions :+= OracleMainAction(f)

                    if (f.role == Hunter)
                        actions :+= HunterMainAction(f)

                    if (f.role == Dancer)
                        if (f.open.not)
                            actions :+= DancerMainAction(f)
                }

                if (actions.none || f.tired.not)
                    actions :+= DoneAction(TurnEndAction)

                withClaim(Ask(f, actions))
                
            case TurnEndAction =>
                factions.foreach(_.marked = false)
                
                current.tired = false

                current.blocked = Nil

                if (current.jailed) {
                    log(current, "goes out of jail")
                    current.jailed = false
                }

                highlight = Nil

                var order = factions

                while (order.last != current)
                    order = order.drop(1) ++ order.take(1)
                    
                current = order(0)
                
                TurnStartAction

            case GameOverAction(s, m) =>
                over = true
                log(m)
                log((s.toString + "s").styled(s), "won")
                val winners2 = factions.%(_.alive).%(_.open).%(_.side == s)
                val winners1 = factions.%(_.alive).%(_.open.not).%(f => f.side == s || (f.role == Vizier && neighbours(f).intersect(winners2).any))
                GameOver(winners2 ++ winners1, "Game Over" ~ Break ~ 
                winners2./(f => f.elem ~ " won " ~ 2.hl ~ " points").join(Break) ~ Break ~
                winners1./(f => f.elem ~ " won " ~ 1.hl ~ " point").join(Break), Nil)

        }
    }
}