package root
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
import root.elem._

import scala.collection.immutable.SortedMap

trait Aviary extends WarriorFaction {
    val clashKey = ED

    val warrior = Hawk

    def abilities(options : $[Meta.O]) = $(RuleIfTied, DisdainForTrade, DaylightCraft) ++ options.has(OneTrueEmpire).$(TurmoilTolerance)

    def pieces(options : $[Meta.O]) = Hawk *** 20 ++ Roost *** 7

    override def initialDamagePriorities(options : $[Meta.O]) = $(Hawk, Roost)
}

case object Roost extends Building
case object Hawk extends Warrior

sealed trait Decree extends Record with Elementary {
    val name : String
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

    val all = $[Decree](Recruit, Move, Battle, Build)
    val empty : Map[Decree, $[Card]] = all./(_ -> $()).toMap
    val clean : Map[Decree, $[Suit]] = all./(_ -> $()).toMap

    def of(f : Aviary) = "Decree".styled(f)
}

case object DisdainForTrade extends FactionEffect {
    override val name = "Disdain for Trade"
}

case object RuleIfTied extends FactionEffect {
    override val name = "Rule If Tied"
}

case object TurmoilTolerance extends FactionEffect {
    override val name = "Turmoil Tolerance"
}

sealed trait Leader extends DisplayEffect with Record {
    def starting : $[Decree]
    def img = Image(name, styles.card)
    def desc : Elem
}

case object Builder extends Leader {
    val name = "Builder"
    val starting = $(Decree.Recruit, Decree.Move)
    val desc = "Full " ~ dt.VP ~ " crafting"
}

case object Charismatic extends Leader {
    val name = "Charismatic"
    val starting = $(Decree.Recruit, Decree.Battle)
    val desc = "Double recruting"
}

case object Commander extends Leader with BattleEffect {
    val name = "Commander"
    val starting = $(Decree.Move, Decree.Battle)
    val desc = "Extra " ~ 1.hit ~ " attacking"
}

case object Despot extends Leader with BattleEffect {
    val name = "Despot"
    val starting = $(Decree.Move, Decree.Build)
    val desc = "Extra " ~ dt.VP ~ " destroying"
}

case object LoyalVizier extends Card with Record {
    val suit = Bird
    val name = "Loyal Vizier"
    val img = Image("loyal-vizier", styles.card)
}

class AviaryPlayer(val faction : Aviary)(implicit val game : Game) extends FactionState {
    var contenders : $[Leader] = $(Builder, Charismatic, Commander, Despot)
    var leader : |[Leader] = None
    var retired : $[Leader] = $

    var tolerance : Int = 0
    var skipped : Int = 0

    override def transient = leader.$

    val viziers = game.cards.another[Card]("viziers", $(LoyalVizier, LoyalVizier), _ == LoyalVizier)
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

    def craft = all(Roost)./(_.asset)
}


case class AsDespot(m : Message) extends Message {
    def elem(implicit game : Game) = "as " ~ Despot.elem ~ " " ~ m.elem
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

case object PE extends Aviary {
    val name = "Perching Emirates"
    override def funName = NameReference(name, this) ~ " Emirates"
    val short = "PE"
    val style = "pe"
    val priority = "B'"

    def advertising = Decree.all./(c => Image(style + "-" + c.name + "-bird", styles.piece)).merge
    def motto = "Expand".styled(this)
}

trait EDDaylightQuestion extends FactionAction {
    override def self : Aviary
    def question(implicit game : Game) = self.elem ~ " " ~ Decree.of(self)
}

case class NewRoostAction(self : Aviary, c : Clearing) extends BaseAction("Start a new", Roost.of(self), Roost.imgd(self))(c)

case class DecreeMainAction(f : Aviary, h : $[DeckCard], m : SortedMap[Int, Decree]) extends ForcedAction with Soft
case class DecreeCardAction(self : Aviary, h : $[DeckCard], m : SortedMap[Int, Decree], l : Leader, n : Int) extends BaseAction("Add to", Decree.of(self))(h(n).img) with Soft with ViewCard with Selectable with NoClear with NoExplode with LimitedExtra[Decree] with ElemWrap {
    val d = h(n)

    def wrap(g : Game)(e : Elem) = Div(values./ { c =>
        val x = m.contains(n) || (m.num < 2 && (h(n).suit != Bird || m.keys./(h.apply)./(_.suit).%(_ == Bird).none))
        val s = m.get(n) == Some(c)
        val p = self.style + "-" + (c == Decree.Recruit && l == Charismatic).??("double-") + c.name + "-" + h(n).suit.name + s.not.??("-done")
        x.?(OnClick(c, s.?(Image(p, styles.action, styles.selected, xlo.pointer)).|(Image(p, styles.action, xlo.pointer)))).|(Image(p, styles.action))
    }.merge ~ Break ~ m.get(n).any.?(OnClick(m(n), e.styled(xlo.pointer))).|(e), styles.inline, styles.margined)

    def selected = m.contains(n)

    def fromAny(s : Any) = s match {
        case c : Decree => Some(c)
        case _ => None
    }

    def values = Decree.all

    def update(c : Decree) = this.copy(m = (m.get(n) == Some(c)).?(m - n).|(m + (n -> c)))
}

case class DecreeExplodeAction(self : Aviary, h : $[DeckCard], l : Leader) extends HiddenChoice with SelfExplode {
    def explode(withSoft : Boolean) = {
        val iii = 0.to(2)./~(h.indices.combinations).flatMap { l =>
            l.foldLeft($(SortedMap[Int, Decree]()))((mm, n) => mm./~(m => Decree.all./(c => m + (n -> c))))
        }

        val ii = iii.%(m => h.indices.%(m.contains).%(n => h(n).suit == Bird).num <= 1).%(_.any)

        ii./(m => DecreeAction(self, h, m.keys.$, m.values.$)) ++ withSoft.??(ii./~(m => h.indices./(DecreeCardAction(self, h, m, l, _))))
    }
}

case class DecreeAction(self : Aviary, h : $[DeckCard], l : $[Int], d : $[Decree]) extends BaseAction(None)("Done")

case class NewLeaderAction(f : Aviary, s : Option[Leader], then : ForcedAction) extends ForcedAction with Soft

abstract class ShowLeaderAction(self : Aviary, l : Leader) extends BaseUserAction(self, "get a new leader")(l.starting./{ c =>
    val id = self.style + "-" + (c == Decree.Recruit && l == Charismatic).??("double-") + c.name
    val s = Bird
    Image(id + "-" + s.name, styles.action)
}.merge ~ Image(self.style + "-" + l.name, styles.card) ~ l.name.hl ~ Break ~ Span(l.desc, xstyles.smaller85)) with ViewLeader with NoClear { self : UserAction => }

case class SelectLeaderAction(self : Aviary, l : Leader, then : ForcedAction) extends ShowLeaderAction(self, l) with Choice with Soft
case class InfoLeaderAction(self : Aviary, l : Leader) extends ShowLeaderAction(self, l) with Info with Selected with OnClickInfo { def param = l }

case class AssignLeaderAction(self : Aviary, l : Option[Leader], then : ForcedAction) extends BaseAction(None)("Ok")

case class AviaryMainAction(self : Aviary) extends ForcedAction
case class AviaryMainDecreeAction(self : Aviary, d : Decree, x : $[Suit]) extends ForcedAction
case class TurmoilLoomingAction(self : Aviary, d : Decree, x : $[Suit]) extends ForcedAction

case class AviaryRecruitMainAction(self : Aviary, from : $[Clearing], suits : $[Suit]) extends OptionAction(Decree.Recruit, implicit g => "[" ~ self.desc(Decree.Recruit) ~ "]") with EDDaylightQuestion with Soft with Only[AviaryRecruitAction] { def tag = implicitly }
case class AviaryRecruitAction(self : Aviary, c : Clearing, s : Suit, alts : $[Suit]) extends BaseAction(Decree.Recruit, "in", implicit g => "[" ~ self.desc(Decree.Recruit) ~ "]")(c, (alts.num > 1).?("(" ~ s.elem ~ ")"))
case class AviaryMoveMainAction(self : Aviary, from : $[Clearing], suits : $[Suit]) extends OptionAction(Decree.Move, implicit g => "[" ~ self.desc(Decree.Move) ~ "]") with EDDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class AviaryBattleMainAction(self : Aviary, from : $[Clearing], suits : $[Suit]) extends OptionAction(Decree.Battle, implicit g => "[" ~ self.desc(Decree.Battle) ~ "]") with EDDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class BuildRoostMainAction(self : Aviary, from : $[Clearing], suits : $[Suit]) extends OptionAction(Decree.Build, implicit g => "[" ~ self.desc(Decree.Build) ~ "]") with EDDaylightQuestion with Soft with Only[BuildRoostAction] { def tag = implicitly }
case class BuildRoostAction(self : Aviary, c : Clearing, s : Suit, alts : $[Suit]) extends BaseAction(Decree.Build, "in", implicit g => "[" ~ self.desc(Decree.Build) ~ "]")(c, (alts.num > 1).?("(" ~ s.elem ~ ")"))
case class TurmoilMainAction(self : Aviary, d : Decree, x : $[Suit]) extends OptionAction("Turmoil".styled(self)) with EDDaylightQuestion
case class TurmoilAvoidAction(self : Aviary, d : Decree, x : $[Suit]) extends OptionAction("Avoid Turmoil".styled(self)) with EDDaylightQuestion
case class TurmoilAction(self : Aviary) extends ForcedAction
case class DecreeDoneAction(self : Aviary, cost : SuitCost, suits : $[Suit], then : ForcedAction) extends ForcedAction


object AviaryExpansion extends FactionExpansion[Aviary] {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Aviary) =>
            game.states += f -> new AviaryPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Aviary) if options.has(SetupTypeHomelands) && soft =>
            val h = game.homelands
            val hh = h./~(game.connected)
            val hhh = hh./~(game.connected)

            val l = clearings.diff(h)
            val ll = l.diff(board.inner).some.|(l)
            val lll = ll.diff(hh).diff(hhh).some.||(ll.diff(hh).some).|(ll)

            Ask(f)(lll./(c => StartingClearingAction(f, c).as(c)(f, "starts in"))).needOk

        case FactionSetupAction(f : Aviary) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : Aviary, r) =>
            game.homelands :+= r

            f.reserve --> Roost --> r
            f.reserve --> 6.times(Hawk) --> r

            f.log("placed", Roost.of(f), "and six", Hawk.sof(f), "in", r)

            NewLeaderAction(f, None, SetupFactionsAction)

        case NewLeaderAction(f, s, then) =>
            Ask(f)
              .each(f.contenders)(l => s.has(l).?(InfoLeaderAction(f, l)).|(SelectLeaderAction(f, l, then)))
              .add(AssignLeaderAction(f, s, then).!(s.none))
              .needOk

        case SelectLeaderAction(f, l, then) =>
            NewLeaderAction(f, Some(l), then)

        case AssignLeaderAction(f, Some(l), then) =>
            f.leader = Some(l)
            f.contenders = f.contenders.but(l)
            l.starting.foreach { d => f.viziers --> LoyalVizier --> f.decree(d) }
            f.done = Decree.clean

            f.log("started with", l.name.hl, "leader")

            then

        // HELPER
        case MoveListAction(self, f : Aviary, t, m, from : Clearing, to : Clearing, l, DecreeDoneAction(ff, NoSuit, all, then)) =>
            DecreeDoneAction(ff, from.cost, all, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case BattleStartAction(self, f, a, m, c, o, i, DecreeDoneAction(ff, NoSuit, all, then)) =>
            DecreeDoneAction(ff, c.cost, all, BattleStartAction(self, f, a, m, c, o, i, then))

        case LegalAAAConveneAction(f, h, c, e, DecreeDoneAction(ff, NoSuit, all, then)) =>
            DecreeDoneAction(ff, c.cost, all, LegalAAAConveneAction(f, h, c, e, then))

        case BattlePostHitInAction(b, e, f : Aviary, Hawk, then) =>
            e.log("banished", Hawk.of(f))
            then

        case BattlePostHitInAction(b, e, f : Aviary, Roost, then) =>
            e.log("desolated", Roost.of(f))
            then

        case BattlePostHitOutAction(b, _, _, _, TryForcedRemoveAction(e : Aviary, c, f, p : Scoring, x, m, then, fail)) if e.can(Despot) && b.instigator.none =>
            e.used :+= Despot

            TryForcedRemoveAction(e, c, f, p, x + 1, AsDespot(m), then, fail)

        // case BattlePostHitOutAction(b, e : Aviary, f, p : Scoring, then) if b.instigator.none =>
        //     println(action)
        //     ???
        //     if (e.can(Despot)) {
        //         e.used :+= Despot
        //         e.oscore(1)("for destruction as", Despot)
        //     }
        //     then


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
                val b = clearings.%(f.canPlace).%(f.canBuild)./(c => c -> (factions.but(f)./(_.at(c).of[Warrior].num).sum))
                val m = b.rights.minOr(0)
                val l = b.%((_, b) => b == m).lefts
                Ask(f).each(l)(NewRoostAction(f, _)).needOk.bailw(Next) { f.log("could not find a place for a new", Roost.of(f)) }
            }
            else
                Next

        case NewRoostAction(f, c) =>
            if (f.canBuild(c))
                f.reserve --> Roost --> c

            if (f.canPlace(c))
                f.reserve --?> 3.times(Hawk) --> c

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
            Ask(f)(NoHand)
              .each(0.until(h.num).$)(n => DecreeCardAction(f, h, m, f.leader.get, n))
              .add(DecreeAction(f, h, m.keys.$, m.values.$).x(m.none))
              .add(DecreeExplodeAction(f, h, f.leader.get))
              .birdsong(f)

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
            AviaryMainAction(f)

        case AviaryMainAction(f : Aviary) if soft =>
            // +++(" \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ ")

            var dd = Decree.all
            while (dd.any && f.done(dd.head).num == f.todo(dd.head).num)
                dd = dd.drop(1)

            if (dd.any) {
                val d = dd.head
                val t = f.todo(d)
                val o = f.done(d)
                val r = t.diff(o).diff(o.diff(t)./(_ => Bird))

                AviaryMainDecreeAction(f, d, r)
            }
            else
                Ask(f).done(EveningStartAction(f)).daylight(f)

        case AviaryMainDecreeAction(f, d @ Decree.Recruit, r) =>
            val l = f.all(Roost).%(f.canPlace).%(c => r.exists(c.cost.matched))

            if (l.any && (f.pooled(Hawk) > f.has(Charismatic).??(1) || f.totalWar))
                Ask(f)(AviaryRecruitMainAction(f, l, r)).daylight(f)
            else
                TurmoilLoomingAction(f, d, r)

        case AviaryMainDecreeAction(f, d @ Decree.Move, r) =>
            val l = f.moveFrom.of[Clearing].%(c => r.exists(c.cost.matched))

            if (l.any)
                Ask(f)(AviaryMoveMainAction(f, l, r)).daylight(f)
            else
                TurmoilLoomingAction(f, d, r)

        case AviaryMainDecreeAction(f, d @ Decree.Battle, r) =>
            val l = clearings.%(f.canAttackIn).%(c => r.exists(c.cost.matched))

            if (l.any)
                Ask(f)(AviaryBattleMainAction(f, l, r)).daylight(f)
            else
                TurmoilLoomingAction(f, d, r)

        case AviaryMainDecreeAction(f, d @ Decree.Build, r) =>
            val l = clearings.%(f.rules).%(f.canBuild).diff(f.all(Roost)).%(f.canPlace).%(c => r.exists(c.cost.matched))

            if (l.any && f.pool(Roost))
                Ask(f)(BuildRoostMainAction(f, l, r)).daylight(f)
            else
                TurmoilLoomingAction(f, d, r)

        case AviaryRecruitMainAction(f, l, r) =>
            Ask(f)
              .some(f.all(Roost)) { c =>
                  if (l.has(c)) {
                      val x = r.distinct.%(c.cost.matched)
                      x./(s => AviaryRecruitAction(f, c, s, x))
                  }
                  else
                      $(AviaryRecruitAction(f, c, Bird, $).!(true))
              }
              .needOk
              .daylight(f)

        case AviaryRecruitAction(f, c, s, r) =>
            game.highlights :+= PlaceHighlight($(c))

            val n = 1 + f.has(Charismatic).??(1)
            val m = min(f.pooled(Hawk), n)

            if (m > 0) {
                f.reserve --> m.times(Hawk) --> c

                f.log("recruited", m.times(Hawk.of(f)).comma, "in", c)
            }

            if (n > m && f.totalWar)
                f.oscore(n - m)("recruiting")

            DecreeDoneAction(f, s, r, Repeat)


        case AviaryMoveMainAction(f, l, r) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, f.daylight, DecreeDoneAction(f, NoSuit, r.distinct, Repeat))

        case AviaryBattleMainAction(f, l, r) =>
            BattleInitAction(f, f, NoMessage, l, f.daylight ++ $(HiddenCancel), DecreeDoneAction(f, NoSuit, r.distinct, Repeat))

        case BuildRoostMainAction(f, l, r) =>
            Ask(f)
              .some(l) { c =>
                  if (l.has(c)) {
                      val x = r.distinct.%(_.matches(c.cost))
                      x./(s => BuildRoostAction(f, c, s, x))
                  }
                  else
                      $(BuildRoostAction(f, c, Bird, $).!(true))
              }
              .needOk
              .daylight(f)

        case BuildRoostAction(f, c, s, r) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> Roost --> c

            f.log("built", Roost.of(f), "in", c)

            DecreeDoneAction(f, s, r, Repeat)

        case DecreeDoneAction(f, s, r, then) =>
            val x = r.%(_ == s).some.|(r.%(_.matches(s)))

            if (x.none) {
                warn(DecreeDoneAction(f, s, r, then))

                throw new Error("decree on unknown suit")
            }

            var dd = Decree.all
            while (dd.any && f.done(dd.head).num == f.todo(dd.head).num)
                dd = dd.drop(1)

            val current = dd.head

            if (x.num > 1)
                Ask(f).each(x)(s => DecreeDoneAction(f, s, r, then).as("Execute", current.name.hl, "as", s)("Decree"))
            else {
                f.done += current -> (f.done(current) :+ x.only)

                then
            }

        case TurmoilLoomingAction(f, d, r) =>
            f.logtemp("could not", d.name.hl, (r.distinct.num < 3).$("in", r.distinct./(_.elem).commaOr, "clearing"))

            if (f.tolerance >= f.skipped + r.num)
                Ask(f)(TurmoilAvoidAction(f, d, r)).needOk.daylight(f)
            else
                Ask(f)(TurmoilMainAction(f, d, r)).needOk.daylight(f)

        case TurmoilMainAction(f, d, r) =>
            f.log("could not", d.name.hl, (r.distinct.num < 3).$("in", r.distinct./(_.elem).commaOr, " clearing"), "and fell into", "Turmoil".hl)

            DelayedContinue(50, Force(TurmoilAction(f)))

        case TurmoilAvoidAction(f, d, x) =>
            f.log("avoided", "Turmoil".hl, "with", TurmoilTolerance.of(f))

            f.skipped += x.num

            var dd = Decree.all
            while (dd.any && f.done(dd.head).num == f.todo(dd.head).num)
                dd = dd.drop(1)

            val current = dd.head

            f.done += current -> (f.done(current) ++ x)

            Repeat

        case TurmoilAction(f) =>
            val n = Decree.all./~(f.todo).%(_ == Bird).num

            f.oscore(-n)("due to", "Turmoil".hl)

            val o = factions.but(f).of[Aviary].sortBy(_.vp).lastOption.%(_ => options.has(OneTrueEmpire))

            val l = Decree.all./~(d => f.decree(d)).but(LoyalVizier)
            var s = $[DeckCard]()
            var t = $[DeckCard]()

            Decree.all.foreach { x =>
                f.decree(x).foreach {
                    case LoyalVizier =>
                        f.decree(x) --> LoyalVizier --> f.viziers

                    case d : DeckCard if o.any && d.suit == Bird =>
                        f.decree(x) --> d --> o.get.decree(x)
                        s :+= d

                    case d : DeckCard =>
                        f.decree(x) --> d --> discard.quiet
                        t :+= d
                }
            }

            if (s.any)
                o.get.log("took", s, "from", Decree.of(f), "of", f)

            if (t.any)
                f.log("discarded", t, "from", Decree.of(f))

            f.log("retired", f.leader.get)

            f.retired ++= f.leader

            f.leader = None

            if (f.has(TurmoilTolerance)) {
                f.tolerance += 1
                f.skipped = 0

                f.log("gained more", TurmoilTolerance.of(f))
            }

            if (f.contenders.none) {
                f.contenders = f.retired
                f.retired = $
            }

            NewLeaderAction(f, None, EveningStartAction(f))

        case EveningNAction(90, f : Aviary) =>
            val r = $(5, 4, 4, 3, 2, 1, 0, 0)(f.pooled(Roost))
            f.oscore(r)("for", f.all(Roost).num.hl, Roost.plural.styled(f))
            Next

        case NightStartAction(f : Aviary) =>
            val n = 1 + (f.pooled(Roost) < 2).??(1) + (f.pooled(Roost) < 5).??(1)
            EveningDrawAction(f, n)

        case FactionCleanUpAction(f : Aviary) =>
            f.done = Decree.clean
            // f.skipped = 0

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
