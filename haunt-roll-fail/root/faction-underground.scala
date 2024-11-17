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

trait Underground extends WarriorFaction {
    val clashKey = UD

    val warrior = Mole

    def abilities(options : $[Meta.O]) = $(EveningCraft)

    def pieces(options : $[Meta.O]) = Mole *** 20 ++ Tunnel *** 3 ++ Citadel *** 3 ++ Market *** 3

    val ministers : $[Minister] = $(Foremole, Captain, Marshal, Brigadier, Banker, Mayor, Duchess, Baron, Earl)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Mole, Tunnel, Market, Citadel)
}

case object Mole extends Warrior
case object Citadel extends Building
case object Market extends Building
case object Tunnel extends Token

case class Burrow(faction : Underground) extends ExtraRegion with Rulable with Record {
    override def name = "Burrow"
    def elem(implicit game : Game) = name.styled(faction)
}

case object Rubble extends SpecialRegion

abstract class Minister(val rank : Int) extends Record {
    def name = toString
    def short = toString
    def full = name
    def of(f : Faction) = full.styled(f)
    def desc(f : Faction) : Elem
    def long(f : Faction)(implicit game : Game) : Elem
}

case object Foremole extends Minister(2) {
    def desc(f : Faction) = dt.AnyCard ~ dt.ArrowWide ~ Image(f.style + "-building", styles.piece)
    def long(f : Faction)(implicit game : Game) = "Build".hh ~ " revealing " ~ "any".hh ~ " card"
}

case object Captain extends Minister(2) {
    def desc(f : Faction) = dt.Battle
    def long(f : Faction)(implicit game : Game) = "Battle".hh
}

case object Marshal extends Minister(2) {
    def desc(f : Faction) = dt.Move
    def long(f : Faction)(implicit game : Game) = "Move".hh
}

case object Brigadier extends Minister(3) {
    def desc(f : Faction) = 2.times(dt.Move) ~ " | " ~ 2.times(dt.Battle)
    def long(f : Faction)(implicit game : Game) = "Move".hh ~ " then " ~ "Move".hh ~ Break ~ "~".hl ~ " or " ~ "~".hl ~ Break ~ "Battle".hh ~ " then " ~ "Battle".hh
}

case object Banker extends Minister(3) {
    def desc(f : Faction) = dt.AnyCard ~ dt.ArrowWide ~ Span("?" ~ Span(Span(" ", xstyles.smaller75) ~ "VP", xstyles.smaller75), styles.vp)
    def long(f : Faction)(implicit game : Game) = "Discard " ~ "cards" ~ " of any " ~ "one".hh ~ " " ~ "suit".hh ~ "," ~ Break ~ "gain " ~ 1.vp ~ " for each card"
}

case object Mayor extends Minister(3) {
    def desc(f : Faction) = dt.Swap
    def long(f : Faction)(implicit game : Game) =
        options.has(MayorAssemblyAction).?(
            "Take any " ~ "Assembly".styled(f) ~ " action"
        ).|(
            "Take the action of any swayed " ~ "Noble".styled(f) ~ " or " ~ "Squire".styled(f)
        )
}

case object Duchess extends Minister(4) {
    override val short = "duchess"
    override val full = "Duchess of Mud"
    def desc(f : Faction) = 3.times(Tunnel.imgd(f)) ~ dt.ArrowWide ~ 2.vp
    def long(f : Faction)(implicit game : Game) = "Gain " ~ 2.vp ~ " if all " ~ Tunnel.sof(f) ~ " are on the map"
}

case object Baron extends Minister(4) {
    override val short = "baron"
    override val full = "Baron of Dirt"
    def desc(f : Faction) = "each " ~ Market.imgd(f) ~ dt.ArrowWide ~ 1.vp
    def long(f : Faction)(implicit game : Game) = "Gain " ~ 1.vp ~ " for each " ~ Market.of(f) ~ " on the map"
}

case object Earl extends Minister(4) {
    override val short = "earl"
    override val full = "Earl of Stone"
    def desc(f : Faction) = "each " ~ Citadel.imgd(f) ~ dt.ArrowWide ~ 1.vp
    def long(f : Faction)(implicit game : Game) = "Gain " ~ 1.vp ~ " for each " ~ Citadel.of(f) ~ " on the map"
}

case object UD extends Underground {
    val name = "Underground Duchy"
    override def funName = "Underground " ~ NameReference(name, this)
    val short = "UD"
    val style = "ud"
    val priority = "H"

    def advertising = 3.times(Tunnel.img(this)).merge
    def motto = "Sway".styled(this)
}

case object DR extends Underground {
    val name = "Deep Realm"
    override def funName = NameReference(name, this) ~ " Realm"
    val short = "DR"
    val style = "dr"
    val priority = "H'"

    def advertising = 3.times(Tunnel.img(this)).merge
    def motto = "Sway".styled(this)
}

class UndergroundPlayer(val faction : Underground)(implicit val game : Game) extends FactionState {
    var acted = 0

    val revealed = cards("revealed")

    val (_, burrow) = location(Burrow(faction), u => u.faction == faction && u.piece == faction.warrior)

    val rubble = location(Rubble, u => u.faction == faction && u.piece.in(Market, Citadel))

    var swayed : $[Minister] = $

    var worked : $[Minister] = $

    var retired : $[Minister] = $

    def craft = all(Citadel)./(_.asset) ++ all(Market)./(_.asset)

    def punchIn(m : Minister, mayor : Boolean) {
        if (mayor) {
            worked :+= Mayor
            game.log(Mayor.of(faction), "worked as", m.of(faction))
        }
        else {
            worked :+= m
            game.log(m.of(faction), "worked")
        }
    }

    override def presence = burrow +: super.presence
}


case class Digs(f : Underground, c : Clearing) extends Message {
    def elem(implicit game : Game) = " digs " ~ Tunnel.of(f) ~ " to " ~ c.elem
}


trait UndergroundDaylightQuestion extends FactionAction {
    override def self : Underground
    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            1.to(2)./(_ => Image("action-black", styles.action)).take(self.acted) ~
            1.to(2)./(_ => Image(self.style + "-action", styles.action)).drop(self.acted),
        styles.margined)
}

trait UndergroundDaylightWorkQuestion extends FactionAction {
    override def self : Underground
    def isMayor = this match {
        case WorkAction(_, _, true) => true
        case _ => false
    }
    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~ isMayor.?(Mayor.of(self)).|("Ministers".styled(self))
}

case class UndergroundAttackAction(self : Underground, l : $[Clearing], then : ForcedAction) extends OptionAction("Battle".styled(self), dt.Battle) with UndergroundDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class UndergroundMoveAction(self : Underground, l : $[Region], then : ForcedAction) extends OptionAction("Move".styled(self), dt.Move) with UndergroundDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class UndergroundRecruitAction(self : Underground, then : ForcedAction) extends OptionAction("Recruit".styled(self), Image(self.style + "-young", styles.fund)) with UndergroundDaylightQuestion

case class UndergroundBuildMainAction(self : Underground, l : $[Clearing], p : Building, then : ForcedAction) extends OptionAction("Build", p.of(self), dt.AnyCard, dt.Arrow, p.imgd(self)) with UndergroundDaylightQuestion with Soft with Only[UndergroundBuildAction] { def tag = implicitly }
case class UndergroundBuildClearingAction(self : Underground, c : Clearing, p : Building, then : ForcedAction) extends BaseAction("Build", p.of(self))(c) with Soft
case class UndergroundBuildAction(self : Underground, c : Clearing, p : Building, then : ForcedAction) extends ForcedAction

case class UndergroundDigMainAction(self : Underground, l : $[Clearing], then : ForcedAction) extends OptionAction("Dig", Tunnel.of(self), dt.AnyCard, dt.Arrow, Tunnel.imgd(self)) with UndergroundDaylightQuestion with Soft with Only[UndergroundDigAction] { def tag = implicitly }
case class UndergroundDigClearingAction(self : Underground, c : Clearing, then : ForcedAction) extends BaseAction("Dig", Tunnel.of(self))(c) with Soft
case class UndergroundDigFromClearingAction(self : Underground, c : Clearing, from : Clearing, then : ForcedAction) extends BaseAction("Reroute", Tunnel.of(self), "to", c, "from")(from) with Soft
case class UndergroundDigAction(self : Underground, c : Clearing, from : Option[Clearing], then : ForcedAction) extends ForcedAction

case class UndergroundDigPartyAction(self : Underground, c : Clearing, then : ForcedAction) extends ForcedAction
case class UndergroundDigMoveAction(self : Underground, c : Clearing, l : $[Piece], then : ForcedAction) extends BaseAction("Move to", c, "from", Burrow(self))(l./(_.img(self)).merge)

case class UndergroundRevealCardMainAction(self : Underground, s : SuitCost, then : ForcedAction) extends ForcedAction with Soft
case class UndergroundRevealCardAction(self : Underground, s : SuitCost, d : DeckCard, then : ForcedAction) extends BaseAction("Reveal", s.elem, "card")(d.img) with ViewCard with ExpandThen

case class AssemblyMainAction(self : Underground, mayor : Boolean) extends ForcedAction with Soft
case class AssemblyDoneAction(self : Underground, then : ForcedAction) extends ForcedAction

case class WorkMainAction(self : Underground, mayor : Boolean) extends ForcedAction with Soft

case class WorkAction(self : Underground, m : Minister, mayor : Boolean) extends OptionAction(m.of(self), m.desc(self)) with UndergroundDaylightWorkQuestion with Soft
case class WorkVPAction(self : Underground, m : Minister, p : Piece, n : Int) extends OptionAction(m.of(self), (n > 0).?("(" ~ n.vp ~ ")")) with UndergroundDaylightWorkQuestion
case class WorkDoneAction(self : Underground, m : Minister, mayor : Boolean) extends ForcedAction

case class ForemoleBuildClearingAction(self : Underground, c : Clearing, p : Building, mayor : Boolean) extends BaseAction("Build", p.of(self))(c) with Soft with Only[ForemoleBuildAction] { def tag = implicitly }
case class ForemoleBuildAction(self : Underground, c : Clearing, p : Building, mayor : Boolean) extends ForcedAction

case class BrigadierWageWarAction(self : Underground, mayor : Boolean) extends BaseAction(Brigadier.of(self))("Wage War".styled(self), 2.times(dt.Battle).merge) with Soft with Only[BattleStartAction] { def tag = implicitly }
case class BrigadierMarchAction(self : Underground, mayor : Boolean) extends BaseAction(Brigadier.of(self))("March".styled(self), 2.times(dt.Move).merge) with Soft with Only[MoveListAction] { def tag = implicitly }

case class BankerScoreAction(self : Underground, l : $[DeckCard], mayor : Boolean) extends ForcedAction

case class SwayMinisterMainAction(self : Underground) extends ForcedAction with Soft
case class SwayMinisterSelectAction(self : Underground, l : $[SuitCost], m : Minister, n : Int, vp : Int) extends BaseAction("Sway", "Minister".styled(self), "with", n.times(dt.AnyCard).merge, "for", vp.vp)(m.of(self), m.desc(self)) with Soft
case class SwayMinisterAction(self : Underground, l : $[DeckCard], m : Minister, vp : Int) extends ForcedAction

case class DiscardMinisterAction(self : Underground, m : Minister, then : ForcedAction) extends BaseAction("Dismiss".styled(styles.hit), "Minister".styled(self))(m.of(self))


object UndergroundExpansion extends FactionExpansion[Underground] {
    override def extraMoveFrom(f : Faction)(implicit game : Game) = f @@ {
        case f : Underground => $(f.burrow)
        case _ => $()
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Underground) =>
            game.states += f -> new UndergroundPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Underground) if options.has(SetupTypeHomelands) =>
            val l = clearings.diff(game.homelands)
            val ll = l.diff(game.homelands./~(game.connected)).some.|(l)

            Ask(f)(ll./(c => StartingClearingAction(f, c).as(c)(f, "starts in"))).needOk

        case StartingClearingAction(f : Underground, c) if options.has(SetupTypeHomelands) =>
            game.homelands :+= c

            f.reserve --> Tunnel --> c
            f.reserve --> 2.times(Mole) --> c

            f.log("placed", Tunnel.of(f), "and", Mole.sof(f), "in", c)

            val n = game.connected(c).%(f.canPlace)

            var l = n
            while (l.num < 5)
                l ++= n

            val ll = l.combinations(5).$.%(x => l.diff(x).distinct.num == l.diff(x).num)

            Ask(f).each(ll)(l => PlacePieceClearingsAction(f, l, f.warrior, SetupFactionsAction))

        case FactionSetupAction(f : Underground) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : Underground, c) =>
            f.reserve --> Tunnel --> c
            f.reserve --> 2.times(Mole) --> c

            f.log("placed", Tunnel.of(f), "and", Mole.sof(f), "in", c)

            game.connected(c).foreach { x =>
                f.reserve --> 2.times(Mole) --> x
                f.log("placed", Mole.sof(f), "in", x)
            }

            SetupFactionsAction

        // HELPER
        case MoveListAction(self, f, t, m, from, to, l, AssemblyDoneAction(ff, then)) =>
            AssemblyDoneAction(ff, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case BattleStartAction(self, f, a, m, c, o, i, AssemblyDoneAction(ff, then)) =>
            AssemblyDoneAction(ff, BattleStartAction(self, f, a, m, c, o, i, then))

        case LegalAAAConveneAction(f, h, c, e, AssemblyDoneAction(ff, then)) =>
            AssemblyDoneAction(ff, LegalAAAConveneAction(f, h, c, e, then))

        case MoveListAction(self, f, t, m, from, to, l, WorkDoneAction(ff, minister, mayor)) =>
            ff.punchIn(minister, mayor)

            Force(MoveListAction(self, f, t, m, from, to, l, Repeat))

        case BattleStartAction(self, f, a, m, c, o, i, WorkDoneAction(ff, minister, mayor)) =>
            ff.punchIn(minister, mayor)

            Force(BattleStartAction(self, f, a, m, c, o, i, Repeat))

        case BattlePostHitInAction(b, e, f : Underground, Mole, then) =>
            e.log("whacked", Mole.of(f))

            then

        case BattlePostHitInAction(b, e, f : Underground, Citadel, then) =>
            e.log("overcame", Citadel.of(f))

            then

        case BattlePostHitInAction(b, e, f : Underground, Market, then) =>
            e.log("ransacked", Market.of(f))

            then

        case BattlePostHitInAction(b, e, f : Underground, Tunnel, then) =>
            e.log("collapsed", Tunnel.of(f))

            then

        case UndergroundRevealCardMainAction(f, s, then) =>
            Ask(f).each(f.hand)(d => UndergroundRevealCardAction(f, s, d, then).!(d.matches(s).not)).cancel

        case UndergroundRevealCardAction(f, _, d, then) =>
            f.hand --> d --> f.revealed

            f.log("revealed", d)

            then

        case ForcedRemoveProcessAction(f : Underground, then) =>
            var g = 0

            board.clearings.foreach { c =>
                val l = f.limbo(c).$.%(_.piece.is[Building])

                g += l.num
            }

            if (g > 0) {
                val q = DiscardRandomCardAction(f, then)

                if (f.swayed.any) {
                    val r = f.swayed./(_.rank).max
                    Ask(f).each(f.swayed)(m => DiscardMinisterAction(f, m, q).!(m.rank < r))
                }
                else
                    q
            }
            else
                then

        case DiscardMinisterAction(f, m, then) =>
            f.swayed :-= m
            f.retired :+= m
            f.log("dismissed", m.of(f))

            then

        // TURN
        case BirdsongNAction(50, f : Underground) =>
            val n = $(1, 2, 4, 6)(f.all(Citadel).num)
            val m = min(f.pooled(Mole), n)

            if (m > 0) {
                f.reserve --> m.times(Mole) --> f.burrow

                f.log("recruited", m.times(Mole.of(f)).comma, "in", f.burrow)
            }

            if (n > m && f.totalWar)
                f.oscore(n - m)("recruiting")

            Next

        case BirdsongNAction(60, f : Underground) =>
            soft()

            Ask(f)(f.birdsong).done(Next)

        case DaylightNAction(30, f : Underground) =>
            log("Assembly".styled(f))

            Next

        case DaylightNAction(31, f : Underground) =>
            AssemblyMainAction(f, false)

        case AssemblyMainAction(f : Underground, false) if f.acted >= 2 =>
            Ask(f)(Next.as("End".hl, "Assembly".styled(f))).daylight(f)

        case AssemblyMainAction(f : Underground, mayor) =>
            implicit val ask = builder

            val then : ForcedAction = mayor.?(WorkDoneAction(f, Mayor, false)).|(AssemblyDoneAction(f, Repeat))

            val att = clearings.%(f.canAttackIn)
            + UndergroundAttackAction(f, att, then)
                .!(att.none)

            val mvv = f.moveFrom
            + UndergroundMoveAction(f, mvv, then)
                .!(mvv.none)

            + UndergroundRecruitAction(f, then)
                .!(f.pool(Mole).not && f.totalWar.not, "maximum")

            val bll = clearings.%(f.rules).%(f.canBuild).%(c => f.hand.%(_.matches(c.cost)).any)
            + UndergroundBuildMainAction(f, bll, Citadel, then)
                .!(f.pool(Citadel).not, "maximum")
                .!(bll.none)
            + UndergroundBuildMainAction(f, bll, Market, then)
                .!(f.pool(Market).not, "maximum")
                .!(bll.none)

            val tll = clearings.diff(f.all(Tunnel)).%(f.canPlace).%(c => f.hand.%(_.matches(c.cost)).any)
            + UndergroundDigMainAction(f, tll, then)
                .!(tll.none)

            if (mayor.not) {
                + EndTurnSoftAction(f, "Daylight".styled(styles.phase), ForfeitActions(2 - f.acted))

                ask(f).daylight(f)
            }
            else
                ask(f).cancel

        case UndergroundAttackAction(f, l, then) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), then)

        case UndergroundMoveAction(f, l, then) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), then)

        case UndergroundRecruitAction(f, then) =>
            if (f.pool(Mole)) {
                f.reserve --> Mole --> f.burrow

                f.log("recruited", Mole.of(f), "in", f.burrow)
            }
            else
                f.oscore(1)("recruiting")

            then

        case UndergroundBuildMainAction(f, l, b, then) =>
            Ask(f).each(l)(UndergroundBuildClearingAction(f, _, b, then)).cancel

        case UndergroundBuildClearingAction(f, c, b, then) =>
            UndergroundRevealCardMainAction(f, c.cost, UndergroundBuildAction(f, c, b, then))

        case UndergroundBuildAction(f, c, b, then) =>
            f.drawn --> f.revealed
            f.reserve --> b --> c
            f.log("built", b.of(f), "in", c)
            then

        case UndergroundDigMainAction(f, l, then) =>
            Ask(f).each(l)(UndergroundDigClearingAction(f, _, then)).cancel

        case UndergroundDigClearingAction(f, c, then) =>
            if (f.pool(Tunnel))
                OptionalDiscardCardAction(f, Digs(f, c), c.cost, UndergroundDigAction(f, c, None, then))
            else
                Ask(f).each(f.all(Tunnel))(UndergroundDigFromClearingAction(f, c, _, then)).cancel

        case UndergroundDigFromClearingAction(f, c, s, then) =>
            OptionalDiscardCardAction(f, Digs(f, c), c.cost, UndergroundDigAction(f, c, Some(s), then))

        case UndergroundDigAction(f, c, from, then) =>
            f.drawn --> discard(f)

            from./(f.from).|(f.reserve) --> Tunnel --> c

            if (from.any)
                f.log("rerouted", Tunnel.of(f), "to", c, "from", from.get)
            else
                f.log("built", Tunnel.of(f), "in", c)

            UndergroundDigPartyAction(f, c, then)

        case UndergroundDigPartyAction(f, c, then) =>
            Ask(f)
              .each(1.to(min(4, f.at(f.burrow).num))./~(n => f.at(f.burrow).combinations(n)))(UndergroundDigMoveAction(f, c, _, then))
              .done(then)

        case UndergroundDigMoveAction(f, c, l, then) =>
            f.from(f.burrow) --> l --> c

            f.log("moved", l./(_.of(f)).comma, "from", f.burrow, "to", c)

            MoveCompleteAction(f, f, f.burrow, c, l, $, then)

        case AssemblyDoneAction(f, then) =>
            f.acted += 1

            then

        case DaylightNAction(49, f : Underground) =>
            log("Parliament".styled(f))
            Next

        case DaylightNAction(50, f : Underground) =>
            WorkMainAction(f, false)

        case WorkMainAction(f, mayor) =>
            var actions : $[UserAction] = $

            val mm = mayor.?(f.swayed.%(_.rank <= 3).but(Mayor)).|(f.swayed)
            val ww = mayor.not.??(f.worked)

            if (mm.has(Foremole)) {
                val bll = clearings.%(f.rules).%(f.canBuild)
                actions :+= WorkAction(f, Foremole, mayor).!(ww.has(Foremole)).!(f.pool(Citadel).not && f.pool(Market).not, "maximum").!(f.hand.none, "no cards").!(bll.none, "no place")
            }

            val att = clearings.%(f.canAttackIn)
            val mvv = f.moveFrom

            if (mm.has(Captain))
                actions :+= WorkAction(f, Captain, mayor).!(ww.has(Captain)).!(att.none)

            if (mm.has(Marshal))
                actions :+= WorkAction(f, Marshal, mayor).!(ww.has(Marshal)).!(mvv.none)

            if (mm.has(Brigadier))
                actions :+= WorkAction(f, Brigadier, mayor).!(ww.has(Brigadier)).!(att.none && mvv.none)

            if (mm.has(Banker))
                actions :+= WorkAction(f, Banker, mayor).!(ww.has(Banker)).!(f.hand.none, "no cards")

            if (mm.has(Mayor)) {
                if (options.has(MayorAssemblyAction))
                    actions :+= WorkAction(f, Mayor, mayor).!(ww.has(Mayor))
                else
                    actions :+= WorkAction(f, Mayor, mayor).!(ww.has(Mayor)).!(f.swayed.%(_.rank <= 3).but(Mayor).none, "no minister")
            }

            if (mm.has(Duchess))
                actions :+= WorkVPAction(f, Duchess, Tunnel, (f.pool(Tunnel).not).??(2)).!(ww.has(Duchess)).!(f.pool(Tunnel), "not all tunnels")

            if (mm.has(Baron))
                actions :+= WorkVPAction(f, Baron, Market, f.all(Market).num).!(ww.has(Baron)).!(f.all(Market).none, "no markets")

            if (mm.has(Earl))
                actions :+= WorkVPAction(f, Earl, Citadel, f.all(Citadel).num).!(ww.has(Earl)).!(f.all(Citadel).none, "no citadels")

            if (mayor)
                Ask(f)(actions).refuse(Repeat)
            else
                Ask(f)(actions).done(Next).daylight(f)

        case WorkVPAction(f, m, p, n) =>
            f.punchIn(m, false)
            f.oscore(n)("for", p.sof(f))
            Repeat

        case WorkAction(f, Foremole, mayor) =>
            val l = clearings.%(f.rules).%(f.canBuild)
            val bb = $(Citadel, Market).%(f.pool(_))
            Ask(f)(bb./~(b => l./(ForemoleBuildClearingAction(f, _, b, mayor)))).cancel

        case ForemoleBuildClearingAction(f, c, b, mayor) =>
            UndergroundRevealCardMainAction(f, AnySuit, ForemoleBuildAction(f, c, b, mayor))

        case ForemoleBuildAction(f, c, b, mayor) =>
            f.drawn --> f.revealed
            f.punchIn(Foremole, mayor)
            f.reserve --> b --> c
            f.log("built", b.of(f), "in", c)
            Repeat

        case WorkAction(f, Captain, mayor) =>
            val l = clearings.%(f.canAttackIn)
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), WorkDoneAction(f, Captain, mayor))

        case WorkAction(f, Marshal, mayor) =>
            MoveInitAction(f, f, $, NoMessage, f.moveFrom, f.movable, $(CancelAction), WorkDoneAction(f, Marshal, mayor))

        case WorkDoneAction(f, m, mayor) =>
            f.punchIn(m, mayor)

            Repeat

        case WorkAction(f, Brigadier, mayor) =>
            val cc = f.moveFrom
            val aa = BrigadierWageWarAction(f, mayor).!(clearings.%(f.canAttackIn).none)
            val mm = BrigadierMarchAction(f, mayor).!(f.moveFrom.none)

            Ask(f)(aa)(mm).cancel

        case BrigadierWageWarAction(f, mayor) =>
            WageWarAction(f, 1, 2, WorkDoneAction(f, Brigadier, mayor))

        case BrigadierMarchAction(f, mayor) =>
            MarchAction(f, 1, 2, WorkDoneAction(f, Brigadier, mayor))

        case WorkAction(f, Banker, mayor) =>
            XXSelectObjectsAction(f, f.hand)
                .withGroupDyn(l => game.desc(Banker.of(f), "scores", max(1, l.num).vp, (l.num > 1).?(("with", l.num.hlb)).|("per"), FoxRabbitMouse.%(s => l.all(d => d.matches(s))).single, (l.num > 1).?("cards").|("card")))
                .withRule(_.all(l => FoxRabbitMouse.exists(s => l.all(d => d.matches(s)))))
                .withThen(BankerScoreAction(f, _, mayor))(l => game.desc("Score", l.num.vp))
                .withExtra($(CancelAction, NoHand))

        case BankerScoreAction(f, l, mayor) =>
            f.punchIn(Banker, mayor)
            f.hand --> l --> discard.quiet
            f.nscore(l.num)("banking")(Banker.of(f), ScoredVP, "with", l)
            Repeat

        case WorkAction(f, Mayor, _) if options.has(MayorAssemblyAction) =>
            AssemblyMainAction(f, true)

        case WorkAction(f, Mayor, _) =>
            WorkMainAction(f, true)

        case DaylightNAction(70, f : Underground) =>
            SwayMinisterMainAction(f)

        case SwayMinisterMainAction(f) =>
            def cost(m : Minister) = m.rank + options.has(OldBoys).??(factions.but(f).of[Underground].exists(_.swayed.has(m)).??(1))
            def vps(m : Minister) = m.rank - 1 - options.has(OldBoys).??(factions.but(f).of[Underground].exists(_.swayed.has(m)).??(m.rank - 1))

            val c = clearings.%(f.present)./(_.cost)
            val h = f.hand./(_.suit)
            val s = h

            val iii = f.ministers.diff(f.swayed).sortBy(cost)
            val ii = iii.%(m => cost(m) <= s.num)
            val i = ii.%(m => (f.swayed ++ f.retired).%(_.rank == m.rank).num < 3)

            Ask(f)
              .each(iii)(m => SwayMinisterSelectAction(f, c, m, cost(m), vps(m))
                .!(cost(m) > f.hand.num, "no cards")
                .!(ii.has(m).not, "no clearings")
                .!(i.has(m).not, "no crown")
              )
              .done(Next).daylight(f).needOk

        case SwayMinisterSelectAction(f, s, m, n, vp) =>
            XXSelectObjectsAction(f, f.hand)
                .withGroupDyn(l => game.desc(f, "sways", m.of(f), "with", n.hlb, "of", s, "cards"))
                .withRule(_.num(n).all(l => {
                    val q : $[Suit] = l./(_.suit)
                    val ss : $[SuitCost] = s.diff(q)
                    val qq : $[Suit] = q.diff(s)

                    qq.permutations.exists { pp =>
                        var sss = ss
                        var ppp = pp
                        var result = true

                        while (ppp.any && result) {
                            sss.find(ppp(0).matches) match {
                                case Some(sp) =>
                                    sss :-= sp
                                    ppp = ppp.drop(1)
                                case None => result = false
                            }
                        }

                        result
                    }
                }))
                .withThenElem(SwayMinisterAction(f, _, m, vp))(game.desc("Sway", m.of(f), "for", (m.rank - 1).vp))
                .withExtra($(CancelAction, NoHand))

        case SwayMinisterAction(f, l, m, vp) =>
            f.hand --> l --> f.revealed

            val o = options.has(OldBoys).??(factions.but(f).of[Underground].%(_.swayed.has(m)).single)

            f.swayed :+= m

            o.foreach {
                _.swayed :-= m
            }

            f.log("swayed", m.of(f), "with", l, o.any.$("from", o))

            f.oscore(vp)("swaying", m.of(f))

            Next

        case EveningNAction(10, f : Underground) =>
            val b = f.revealed.%(_.suit == Bird)

            if (b.any)
                f.revealed --> b --> discard(f)

            if (f.revealed.any) {
                f.log("got back", f.revealed.get)
                f.revealed --> f.hand
            }

            Next

        case EveningNAction(50, f : Underground) =>
            XCraftMainAction(f)

        case EveningNAction(60, f : Underground) =>
            soft()

            Ask(f).evening(f).done(Next)

        case NightStartAction(f : Underground) =>
            EveningDrawAction(f, 1 + f.all(Market).num)

        case FactionCleanUpAction(f : Underground) =>
            f.acted = 0
            f.worked = $

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
