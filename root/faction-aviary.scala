package root

import root.gaming._

import colmat._

import hrf.elem._
import root.elem._

import scala.collection.immutable.SortedMap

trait Aviary extends WarriorFaction {
    val expansion = AviaryExpansion
    
    val warrior = Hawk
    
    val abilities = List(RuleIfTied, DisdainForTrade, DaylightCraft)
    
    val pieces = Hawk ** 20 ++ Roost ** 7
}

case object Roost extends Building
case object Hawk extends Warrior

sealed trait Decree extends Record with Elementary {
    def name : String
    def elem = Text(name)
}

object Decree {
    case object Recruit extends Decree {
        val name = "Recruit"
    }
    case object Move extends Decree {
        val name = "Move"
    }
    case object Battle extends Decree {
        val name = "Battle"
    }
    case object Build extends Decree {
        val name = "Build"
    }

    val all = List[Decree](Recruit, Move, Battle, Build)
    val empty : Map[Decree, List[Card]] = all./(_ -> Nil).toMap
    val clean : Map[Decree, List[Suit]] = all./(_ -> Nil).toMap

    def of(f : Aviary) = "Decree".styled(f)
}

case object DisdainForTrade extends FactionEffect {
    override def name = "Disdain for Trade"
}

case object RuleIfTied extends FactionEffect {
    override def name = "Rule If Tied"
}

sealed trait Leader extends FactionEffect with Record {
    def starting : List[Decree]
    def img = Image(name.toLowerCase, styles.card)
    def desc : Elem
}

case object Builder extends Leader {
    val starting = List(Decree.Recruit, Decree.Move)
    val desc = "Full " ~ dt.VP ~ " crafting"
}

case object Charismatic extends Leader {
    val starting = List(Decree.Recruit, Decree.Battle)
    val desc = "Double recruting"
}

case object Commander extends Leader with BattleEffect {
    val starting = List(Decree.Move, Decree.Battle)
    val desc = "Extra " ~ 1.hit ~ " attacking"
}

case object Despot extends Leader with BattleEffect {
    val starting = List(Decree.Move, Decree.Build)
    val desc = "Extra " ~ dt.VP ~ " destroying"
}

case object LoyalVizier extends Card with Record {
    def suit = Bird
    def name = "Loyal Vizier"
    def img = Image("loyal-vizier", styles.card)
}

class AviaryPlayer(val game : Game, val faction : Faction) extends PlayerState {
    var contenders : List[Leader] = List(Builder, Charismatic, Commander, Despot)
    var leader : Leader = null
    var retired : List[Leader] = Nil

    override def abilities = Option(leader).toList

    val viziers = game.cards.another[Card]("viziers", List(LoyalVizier, LoyalVizier), _ == LoyalVizier)
    val decree = Decree.all./(d => d -> game.cards.another[Card]("decree-" + d)).toMap
    var done = Decree.clean
    
    def desc(d : Decree) = {
        val t = todo(d)
        val o = done(d)
        val r = t.diff(o).diff(o.diff(t)./(_ => Bird))
        val c = t.diff(r)
        c.us ~ r.ss
    }
    
    def todo = decree.view.mapValues(_./(_.suit)).toMap
    
    def craft = all(Roost)./(game.mapping)
}

case object ED extends Aviary {
    val name = "Eyrie Dynasties"
    override def funName = NameReference(name, this) ~ " Dynasties"
    val short = "ED"
    val style = "ed"
    val priority = "B"

    def advertising = Decree.all./(c => Image(style + "-" + c.name + "-bird", styles.piece)).merge
    def motto = "Expand".styled(this)
}

trait EDDaylightQuestion extends FactionAction {
    override def self : Aviary
    def question(g : Game) = self.elem ~ " " ~ Decree.of(self)
}

case class NewRoostAction(self : Aviary, c : Clearing) extends BaseAction("Start a new", Roost.of(self), Roost.imgd(self))(c)



case class DecreeMainAction(f : Aviary, h : List[DeckCard], m : SortedMap[Int, Decree]) extends ForcedAction with Soft
case class DecreeCardAction(self : Aviary, h : List[DeckCard], m : SortedMap[Int, Decree], l : Leader, n : Int) extends BaseAction("Add to", Decree.of(self))(h(n).img) with Soft with ViewCard with Selectable with NoClear with NoExplode with LimitedExtra[Decree] with ElemWrap {
    def d = h(n)

    def wrap(g : Game)(e : Elem) = Div(values./ { c =>
        val x = m.contains(n) || (m.num < 2 && (h(n).suit != Bird || m.keys./(h.apply)./(_.suit).%(_ == Bird).none))
        val s = m.get(n) == Some(c)
        val p = self.style + "-" + (c == Decree.Recruit && l == Charismatic).??("double-") + c.name + "-" + h(n).suit.name + s.not.??("-done")
        x.?(OnClick(c, s.?(Image(p, styles.action, styles.selected, xstyles.pointer)).|(Image(p, styles.action, xstyles.pointer)))).|(Image(p, styles.action))
    }.merge ~ Break ~ m.get(n).any.?(OnClick(m(n), e.styled(xstyles.pointer))).|(e), styles.inline, styles.margined)
    
    def selected = m.contains(n)
    
    def fromAny(s : Any) = s match {
        case c : Decree => Some(c)
        case _ => None
    }

    def values = Decree.all

    def update(c : Decree) = this.copy(m = (m.get(n) == Some(c)).?(m - n).|(m + (n -> c)))
}
    
case class DecreeExplodeAction(self : Aviary, h : List[DeckCard], l : Leader) extends HiddenChoice with SelfExplode {
    def explode(withSoft : Boolean) = {
        val iii = 0.to(2)./~(h.indices.combinations).flatMap { l =>
            l.foldLeft(List(SortedMap[Int, Decree]()))((mm, n) => mm./~(m => Decree.all./(c => m + (n -> c))))
        }
        
        val ii = iii.%(m => h.indices.%(m.contains).%(n => h(n).suit == Bird).num <= 1).%(_.any)
        
        ii./(m => DecreeAction(self, h, m.keys.toList, m.values.toList)) ++ withSoft.??(ii./~(m => h.indices./(DecreeCardAction(self, h, m, l, _))))
    }
}

case class DecreeAction(self : Aviary, h : List[DeckCard], l : List[Int], d : List[Decree]) extends BaseAction(None)("Done")



case class NewLeaderAction(f : Aviary, l : Option[Leader], then : ForcedAction) extends ForcedAction with Soft

abstract class ShowLeaderAction(self : Aviary, l : Leader, then : ForcedAction) extends BaseUserAction(self, "get a new leader")(l.starting./{ c =>
    val id = self.style + "-" + (c == Decree.Recruit && l == Charismatic).??("double-") + c.name
    val s = Bird
    Image(id + "-" + s.name, styles.action)
}.merge ~ Image(self.style + "-" + l.name, styles.card) ~ l.name.hl ~ Break ~ Span(l.desc, xstyles.smaller85)) with ViewLeader with NoClear { self : UserAction => }

case class SelectLeaderAction(self : Aviary, l : Leader, then : ForcedAction) extends ShowLeaderAction(self, l, then) with Choice with Soft
case class InfoLeaderAction(self : Aviary, l : Leader, then : ForcedAction) extends ShowLeaderAction(self, l, then) with Info with Selected with OnClickInfo { def param = l }

case class AssignLeaderAction(self : Aviary, l : Leader, then : ForcedAction) extends BaseAction(None)("Ok")


case class HawkMainAction(self : Aviary) extends ForcedAction

case class RecruitHawkMainAction(self : Aviary, from : List[Clearing]) extends OptionAction(Decree.Recruit, g => "[" ~ g.ed(self).desc(Decree.Recruit) ~ "]") with EDDaylightQuestion with Soft
case class RecruitHawkAction(self : Aviary, c : Clearing) extends BaseAction(Decree.Recruit, "in", g => "[" ~ g.ed(self).desc(Decree.Recruit) ~ "]")(c)
case class MoveHawkMainAction(self : Aviary, from : List[Clearing]) extends OptionAction(Decree.Move, g => "[" ~ g.ed(self).desc(Decree.Move) ~ "]") with EDDaylightQuestion with Soft
case class BattleHawkMainAction(self : Aviary, from : List[Clearing]) extends OptionAction(Decree.Battle, g => "[" ~ g.ed(self).desc(Decree.Battle) ~ "]") with EDDaylightQuestion with Soft
case class BuildRoostMainAction(self : Aviary, from : List[Clearing]) extends OptionAction(Decree.Build, g => "[" ~ g.ed(self).desc(Decree.Build) ~ "]") with EDDaylightQuestion with Soft
case class BuildRoostAction(self : Aviary, c : Clearing) extends BaseAction(Decree.Build, "in", g => "[" ~ g.ed(self).desc(Decree.Build) ~ "]")(c)
case class TurmoilMainAction(self : Aviary, d : Decree, x : List[Suit]) extends OptionAction("Turmoil".styled(self)) with EDDaylightQuestion
case class TurmoilAction(self : Aviary) extends ForcedAction
case class DecreeDoneAction(self : Aviary, s : Suit, then : ForcedAction) extends ForcedAction


object AviaryExpansion extends Expansion {
    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            // SETUP
            case CreatePlayerAction(f : Aviary) =>
                pstates += f -> new AviaryPlayer(game, f)
                FactionInitAction(f)
                
            case FactionSetupAction(f : Aviary) if options.has(SetupTypeHomelands) =>
                val h = homelands
                val hh = h./~(board.connected)
                val hhh = hh./~(board.connected)

                val l = board.clearings.diff(h)
                val ll = l.diff(board.inner).some.|(l)
                val lll = ll.diff(hh).diff(hhh).some.||(ll.diff(hh).some).|(ll)

                Ask(f)(lll./(c => StartingClearingAction(f, c).as(c)(f, "starts in"))).needOk

            case FactionSetupAction(f : Aviary) =>
                StartingCornerAction(f)
                  
            case StartingClearingAction(f : Aviary, r) =>
                homelands :+= r

                f.pool.one(Roost) --> r
                f.pool.sub(6, Hawk) --> r
                
                f.log("started in", r)
    
                NewLeaderAction(f, None, SetupNextAction)
                
            case NewLeaderAction(f, i, then) =>
                Ask(f, f.contenders./(l => (i == Some(l)).?(InfoLeaderAction(f, l, then)).|(SelectLeaderAction(f, l, then))) :+ AssignLeaderAction(f, i.|(null), then).x(i.none))
            
            case SelectLeaderAction(f, l, then) =>
                NewLeaderAction(f, Some(l), then)

            case AssignLeaderAction(f, l, then) =>
                f.leader = l
                f.contenders = f.contenders.but(l)
                l.starting.foreach { d => f.viziers --> LoyalVizier --> f.decree(d) }
                f.done = Decree.clean
                
                f.log("started with", l.name.hl, "leader")
                
                then
                
            // HELPER
            case MoveListAction(f : Aviary, t, m, from : Clearing, to : Clearing, l, DecreeDoneAction(ff, Bird, then)) =>
                DecreeDoneAction(ff, from.suit, ForceAction(MoveListAction(f, t, m, from, to, l, then)))
            
            case BattleStartAction(f, a, m, c, o, i, DecreeDoneAction(ff, Bird, then)) =>
                DecreeDoneAction(ff, c.suit, ForceAction(BattleStartAction(f, a, m, c, o, i, then)))

            case BattlePostHitInAction(b, e, f : Aviary, Hawk, then) =>
                e.log("banished", Hawk.of(f))
                then

            case BattlePostHitInAction(b, e, f : Aviary, Roost, then) =>
                e.log("desolated", Roost.of(f))
                then

            case BattlePostHitOutAction(b, _, _, _, ForcedRemoveAction(e : Aviary, c, f, p : Scoring, x, m, then)) if e.can(Despot) =>
                e.used :+= Despot
                ForcedRemoveAction(e, c, f, p, x + 1, AsDespot(m), then)
            
            case BattlePostHitOutAction(b, e : Aviary, f, p : Scoring, then) =>
                if (e.can(Despot)) {
                    e.used :+= Despot
                    e.oscore(1)("for destruction as", Despot)
                }
                then

            case BattleCleanupAttackerAction(b, f : Aviary) =>
                BattleForcedRemoveAction(b.then match {
                    case DecreeDoneAction(f, Bird, then) => b.copy(then = DecreeDoneAction(f, b.clearing.suit, then))
                    case _ => b
                })
                
            case CraftScoreAction(f : Aviary, d, n, m, then) if n > 1 && f.has(Builder).not =>
                CraftScoreAction(f, d, 1, m, then)

            // TURN
            case BirdsongNAction(30, f : Aviary) =>
                if (f.hand.none) {
                    f.log("issued", "Emergency Orders".styled(f))
                    DrawCardsAction(f, 1, NoMessage, AddCardsAction(f, Next))
                }
                else
                    Next

            case BirdsongNAction(40, f : Aviary) =>
                if (f.all(Roost).none) {
                    val b = clearings.%(canPlace(f)).%(canBuild(f))./(c => c -> (factions.but(f)./(_.at(c).warrior.num).sum))
                    val m = b./(_._2).min
                    val l = b.%(_._2 == m)./(_._1)
                    if (l.any)
                        Ask(f, l./(NewRoostAction(f, _)))
                    else
                        Next
                }
                else
                    Next
                    
            case NewRoostAction(f, c) =>
                if (canBuild(f)(c))
                    f.pool.one(Roost) --> c
    
                if (canPlace(f)(c))
                    f.pool.sub(3, Hawk) --> c
    
                f.log("started a new", Roost.of(f), "in", c)
                
                Next
            
            case BirdsongNAction(50, f : Aviary) =>
                if (f.hand.any)
                    DecreeMainAction(f, f.hand.get, SortedMap())
                else {
                    f.log("had no cards for", Decree.of(f))
                    
                    Next
                }
    
            case DecreeMainAction(f, h, m) =>
                Ask(f, (0.until(h.num)./(n => DecreeCardAction(f, h, m, f.leader, n)) :+ DecreeAction(f, h, m.keys.toList, m.values.toList).x(m.none) :+ DecreeExplodeAction(f, h, f.leader)).birdsong(f).noHand)
                
            case DecreeCardAction(f, h, m, _, _) =>
                DecreeMainAction(f, h, m)

            case DecreeAction(f, h, nn, cc) =>
                nn.lazyZip(cc).foreach { case (n, c) =>
                    f.hand --> h(n) --> f.decree(c)

                    f.log("added", h(n), "to", c.name.hl)
                }

                Next

            case DaylightNAction(40, f : Aviary) =>
                XCraftMainAction(f)
            
            case DaylightNAction(50, f : Aviary) =>
                HawkMainAction(f)

            case HawkMainAction(f : Aviary) => 
                var dd = Decree.all
                while (dd.any && f.done(dd.head).num == f.todo(dd.head).num)
                    dd = dd.drop(1)
    
                var actions : List[UserAction] = Nil
                
                if (dd.any) {
                    val d = dd.head
                    val t = f.todo(d)
                    val o = f.done(d)
                    val r = t.diff(o).diff(o.diff(t)./(_ => Bird))
                    val x = FoxRabbitMouse.%(s => r.contains(s) || r.contains(Bird))
                    val m = (c : Clearing) => x.contains(c.suit)
                    
                    d match {
                        case Decree.Recruit =>
                            val l = f.all(Roost).%(canPlace(f)).%(m)
                            if (l.any)
                                if (f.pooled(Hawk) > f.has(Charismatic).??(1) || f.totalWar)
                                    actions :+= RecruitHawkMainAction(f, l)
                                
                        case Decree.Move =>
                            val l = moveFrom(f).%(m)
                            if (l.any)
                                actions :+= MoveHawkMainAction(f, l)
                                
                        case Decree.Battle =>
                            val l = clearings.%(c => attack(f)(c).any).%(m)
                            if (l.any)
                                actions :+= BattleHawkMainAction(f, l)
                            
                        case Decree.Build =>
                            val l = clearings.%(rule(f)).%(canBuild(f)).diff(f.all(Roost)).%(canPlace(f)).%(m)
                            if (l.any)
                                if (f.pooled(Roost) > 0)
                                    actions :+= BuildRoostMainAction(f, l)
    
                    }
    
                    if (actions.none) {
                        f.logtemp("could not", d.name.hl, (x.num < 3).??("in " ~ x./(_.elem).join(" or ") ~ " clearing"))
                        actions :+= TurmoilMainAction(f, d, x)
                        actions :+= HiddenOkAction
                    }
    
                    Ask(f, actions.odaylight(f))
                }
                else
                    Ask(f, Nil.done(EveningStartAction(f)).daylight(f))
                
            case RecruitHawkMainAction(f, l) =>
                Ask(f, f.all(Roost)./(c => RecruitHawkAction(f, c).x(l.has(c).not)).okNeeded.daylight(f))
     
            case RecruitHawkAction(f, c) =>
                highlights :+= PlaceHighlight(List(c))

                val n = 1 + f.has(Charismatic).??(1)
                val m = min(f.pooled(Hawk), n)

                if (m > 0) {
                    val l = f.pool.sub(m, Hawk)

                    l --> c
    
                    f.log("recruited", l./(_ => Hawk.of(f)).comma, "in", c)
                }

                if (n > m && f.totalWar)
                    f.oscore(n - m)("recruiting")
                    
                DecreeDoneAction(f, c.suit, Repeat)

     
            case MoveHawkMainAction(f, l) =>
                MoveInitAction(f, Nil, NoMessage, l, movable(f), f.daylight, DecreeDoneAction(f, Bird, Repeat))
                
            case BattleHawkMainAction(f, l) =>
                BattleInitAction(f, NoMessage, l, f.daylight ++ $(HiddenCancel), DecreeDoneAction(f, Bird, Repeat))
                
            case BuildRoostMainAction(f, l) =>
                Ask(f, l./(BuildRoostAction(f, _)).cancel)
                
            case BuildRoostAction(f, c) =>
                highlights :+= PlaceHighlight(List(c))

                f.pool.one(Roost) --> c
    
                f.log("built", Roost.of(f), "in", c)
                
                DecreeDoneAction(f, c.suit, Repeat)
    
            case DecreeDoneAction(f, s, then) =>
                var dd = Decree.all
                while (dd.any && f.done(dd.head).num == f.todo(dd.head).num)
                    dd = dd.drop(1)

                f.done += ((dd.head) -> (f.done(dd.head) :+ s))
                
                then
    
            case TurmoilMainAction(f, d, x) =>
                f.log("could not", d.name.hl, (x.num < 3).??("in " ~ x./(_.elem).join(" or ") ~ " clearing"), "and fell into", "Turmoil".hl)

                DelayedContinue(50, Force(TurmoilAction(f)))
                
            case TurmoilAction(f) =>
                val n = Decree.all./~(f.todo).%(_ == Bird).num

                f.oscore(-n)("due to", "Turmoil".hl)
                
                val l = Decree.all./~(d => f.decree(d)).but(LoyalVizier)

                Decree.all.foreach { d =>
                    f.decree(d).foreach {
                        case LoyalVizier => f.decree(d) --> LoyalVizier --> f.viziers
                        case o : DeckCard => f.decree(d) --> o --> discard(f).quiet
                    }
                }
    
                f.log("discarded", l, "from", Decree.of(f))
    
                f.log("retired", f.leader)

                f.retired :+= f.leader
                
                f.leader = null
                if (f.contenders.none) {
                    f.contenders = f.retired
                    f.retired = Nil
                }
    
                NewLeaderAction(f, None, EveningStartAction(f))
                
            case EveningNAction(90, f : Aviary) =>
                val r = List(5, 4, 4, 3, 2, 1, 0, 0)(f.pooled(Roost))
                f.oscore(r)("for", f.all(Roost).num.hl, Roost.plural.styled(f))
                Next
                
            case NightStartAction(f : Aviary) =>
                val n = 1 + (f.pooled(Roost) < 2).??(1) + (f.pooled(Roost) < 5).??(1) 
                EveningDrawAction(f, n)
                
            case FactionCleanUpAction(f : Aviary) =>
                f.done = Decree.clean
                EndPlayerTurnAction(f)

            case _ => UnknownContinue
        }
    }
}

