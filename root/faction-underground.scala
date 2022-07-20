package root

import root.gaming._

import colmat._

import hrf.elem._
import root.elem._

trait Underground extends WarriorFaction {
    val expansion = UndergroundExpansion
    
    val warrior = Mole

    val abilities = $(EveningCraft)

    val pieces = Mole ** 20 ++ Tunnel ** 3 ++ Citadel ** 3 ++ Market ** 3

    val ministers : $[Minister] = $(Foremole, Captain, Marshal, Brigadier, Banker, Mayor, Queen, Baron, Earl)
}

case object Mole extends Warrior
case object Citadel extends Building
case object Market extends Building
case object Tunnel extends Token

case class Burrow(faction : Underground) extends ExtraRegion with Record {
    def name = "Burrow"
    def elem(g : Game) = name.styled(faction)
}

abstract class Minister(val rank : Int) extends Record {
    def name = toString
    def short = toString 
    def full = name
    def of(f : Faction) = full.styled(f)
    def desc(f : Faction) : Elem
}

case object Foremole extends Minister(2) {
    def desc(f : Faction) = dt.AnyCard ~ dt.ArrowWide ~ Image(f.short + "-building", styles.piece)
}
case object Captain extends Minister(2) {
    def desc(f : Faction) = dt.Battle
}
case object Marshal extends Minister(2) {
    def desc(f : Faction) = dt.Move
}
case object Brigadier extends Minister(3) {
    def desc(f : Faction) = dt.Move.repeat(2) ~ " | " ~ dt.Battle.repeat(2)
}
case object Banker extends Minister(3) {
    def desc(f : Faction) = dt.AnyCard ~ dt.ArrowWide ~ Span("?" ~ Span(Span(" ", xstyles.smaller75) ~ "VP", xstyles.smaller75), styles.vp)
}
case object Mayor extends Minister(3) {
    def desc(f : Faction) = dt.Swap
}
case object Queen extends Minister(4) {
    override val short = "Duchess"
    override val full = "Duchess of Mud"
    def desc(f : Faction) = Tunnel.imgd(f).repeat(3) ~ dt.ArrowWide ~ 2.vp
}
case object Baron extends Minister(4) {
    override val full = "Baron of Dirt"
    def desc(f : Faction) = "each " ~ Market.imgd(f) ~ dt.ArrowWide ~ 1.vp
}
case object Earl extends Minister(4) {
    override val full = "Earl of Stone"
    def desc(f : Faction) = "each " ~ Citadel.imgd(f) ~ dt.ArrowWide ~ 1.vp
}

case object UD extends Underground {
    val name = "Underground Duchy"
    override def funName = "Underground " ~ NameReference(name, this)
    val short = "UD"
    val style = "ud"
    val priority = "H"

    def advertising = Tunnel.img(this).repeat(3).merge
    def motto = "Sway".styled(this)
}

class UndergroundPlayer(val game : Game, val faction : Underground) extends PlayerState {
    var acted = 0

    val revealed = cards("revealed")

    val xburrow = figures("burrow", Nil, _.piece == Mole)
    val burrow = Burrow(faction)
    
    val rubble = figures("rubble", Nil, f => f.piece == Citadel || f.piece == Market)

    var swayed : $[Minister] = Nil

    var worked : $[Minister] = Nil

    var retired : $[Minister] = Nil
    
    def craft = (all(Citadel) ++ all(Market))./(game.mapping)

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
}


case class Digs(f : Underground, c : Clearing) extends Message {
    def elem(g : Game) = " digs " ~ Tunnel.of(f) ~ " to " ~ c.elem(g)
}


trait UndergroundDaylightQuestion extends FactionAction {
    override def self : Underground
    def question(g : Game) = self.elem ~ " (" ~ "Daylight".styled(styles.phase) ~ ")" ~ Break ~ 
        Div(
            1.to(2)./(_ => Image("action-black", styles.action)).take(g.ud(self).acted) ~
            1.to(2)./(_ => Image(self.style + "-action", styles.action)).drop(g.ud(self).acted),
        styles.margined)
}

trait UndergroundDaylightWorkQuestion extends FactionAction {
    override def self : Underground
    def isMayor = this match {
        case WorkAction(_, _, true) => true
        case _ => false
    }
    def question(g : Game) = self.elem ~ " (" ~ "Daylight".styled(styles.phase) ~ ")" ~ Break ~ isMayor.?(Mayor.of(self)).|("Ministers".styled(self))
}

case class MoleAttackAction(self : Underground, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with UndergroundDaylightQuestion with Soft
case class MoleMoveAction(self : Underground, l : $[Region]) extends OptionAction("Move".styled(self), dt.Move) with UndergroundDaylightQuestion with Soft
case class MoleRecruitAction(self : Underground) extends OptionAction("Recruit".styled(self), Image(self.style + "-young", styles.fund)) with UndergroundDaylightQuestion

case class MoleBuildMainAction(self : Underground, l : $[Clearing], p : Building) extends OptionAction("Build", p.of(self), dt.AnyCard, dt.Arrow, p.imgd(self)) with UndergroundDaylightQuestion with Soft
case class MoleBuildClearingAction(self : Underground, c : Clearing, p : Building) extends BaseAction("Build", p.of(self))(c) with Soft
case class MoleBuildAction(self : Underground, c : Clearing, p : Building) extends ForcedAction

case class MoleDigMainAction(self : Underground, l : $[Clearing]) extends OptionAction("Dig", Tunnel.of(self), dt.AnyCard, dt.Arrow, Tunnel.imgd(self)) with UndergroundDaylightQuestion with Soft
case class MoleDigClearingAction(self : Underground, c : Clearing) extends BaseAction("Dig", Tunnel.of(self))(c) with Soft
case class MoleDigFromClearingAction(self : Underground, c : Clearing, from : Clearing) extends BaseAction("Reroute", Tunnel.of(self), "to", c, "from")(from) with Soft
case class MoleDigAction(self : Underground, c : Clearing, from : Option[Clearing]) extends ForcedAction

case class MoleDigPartyAction(self : Underground, c : Clearing) extends ForcedAction
case class MoleDigMoveAction(self : Underground, c : Clearing, l : $[Piece]) extends BaseAction("Move to", c, "from", Burrow(self))(l./(_.img(self)).merge)


case class MoleRevealCardMainAction(self : Underground, s : DiscardCost, then : ForcedAction) extends ForcedAction with Soft
case class MoleRevealCardAction(self : Underground, s : DiscardCost, d : DeckCard, then : ForcedAction) extends BaseAction("Reveal", s.elem, "card")(d.img) with ViewCard

case class MoleMainAction(self : Underground) extends ForcedAction
case class MoleDoneAction(self : Underground, then : ForcedAction) extends ForcedAction

case class WorkMainAction(self : Underground, mayor : Boolean) extends ForcedAction with Soft

case class WorkAction(self : Underground, m : Minister, mayor : Boolean) extends OptionAction(m.of(self), m.desc(self)) with UndergroundDaylightWorkQuestion with Soft
case class WorkVPAction(self : Underground, m : Minister, p : Piece, n : Int) extends OptionAction(m.of(self), (n > 0).??("(" ~ n.vp ~ ")")) with UndergroundDaylightWorkQuestion
case class WorkDoneAction(self : Underground, m : Minister, mayor : Boolean) extends ForcedAction

case class ForemoleBuildClearingAction(self : Underground, c : Clearing, p : Building, mayor : Boolean) extends BaseAction("Build", p.of(self))(c) with Soft
case class ForemoleBuildAction(self : Underground, c : Clearing, p : Building, mayor : Boolean) extends ForcedAction

case class BrigadierWageWarAction(self : Underground, mayor : Boolean) extends BaseAction(Brigadier.of(self))("Wage War".styled(self), dt.Battle.repeat(2).merge) with Soft
case class BrigadierMarchAction(self : Underground, mayor : Boolean) extends BaseAction(Brigadier.of(self))("March".styled(self), dt.Move.repeat(2).merge) with Soft

case class BankerScoreAction(self : Underground, l : $[DeckCard], mayor : Boolean) extends ForcedAction

case class SwayMinisterMainAction(self : Underground) extends ForcedAction
case class SwayMinisterSelectAction(self : Underground, l : $[Suit], m : Minister) extends BaseAction("Sway", "Minister".styled(self), "with", dt.AnyCard.repeat(m.rank).merge, "for", (m.rank - 1).vp)(m.of(self), m.desc(self)) with Soft
case class SwayMinisterAction(self : Underground, l : $[DeckCard], m : Minister) extends ForcedAction

case class DiscardMinisterAction(self : Underground, m : Minister, then : ForcedAction) extends BaseAction("Dismiss".styled(styles.hit), "Minister".styled(self))(m.of(self))

object UndergroundExpansion extends Expansion {
    override def extraMoveFrom(game : Game, faction : Faction) = faction match {
        case f : Underground => $(game.ud(f).burrow)
        case _ => Nil
    }

    override def extraConnected(game : Game, faction : Faction, from : Region) = faction match {
        case f : Underground =>
            val b = game.ud(f).burrow
            val t = game.ud(f).all(Tunnel)
            if (from == b)
                t
            else
            if (t.contains(from))
                $(b)
            else
                Nil
        case _ => Nil
    }

    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            // SETUP
            case CreatePlayerAction(f : Underground) =>
                pstates += f -> new UndergroundPlayer(game, f)
                xregions += f.burrow -> f.xburrow
                
                FactionInitAction(f)

            case FactionSetupAction(f : Underground) if options.has(SetupTypeHomelands) =>
                val l = board.clearings.diff(homelands)
                val ll = l.diff(homelands./~(board.connected)).some.|(l)
                Ask(f)(ll./(c => StartingClearingAction(f, c).as(c)(f, "starts in"))).needOk

            case StartingClearingAction(f : Underground, c) if options.has(SetupTypeHomelands) =>
                homelands :+= c

                f.pool.one(Tunnel) --> c
                f.pool.sub(2, Mole) --> c

                f.log("started in", c)

                val n = board.connected(c).%(canPlace(f))

                var l = n
                while (l.num < 5)
                    l ++= n
                    
                val ll = l.combinations(5).toList.%(x => l.diff(x).distinct.num == l.diff(x).num)

                Ask(f)(ll./(l => PlaceStartingWarriorsAction(f, l, f.warrior, SetupNextAction)))

            case FactionSetupAction(f : Underground) =>
                StartingCornerAction(f)

            case StartingClearingAction(f : Underground, c) =>
                f.pool.one(Tunnel) --> c
                f.pool.sub(2, Mole) --> c

                board.connected(c).foreach { x =>
                    f.pool.sub(2, Mole) --> x
                }
                
                SetupNextAction

            // HELPER
            case MoveListAction(f, t, m, from, to, l, MoleDoneAction(ff, then)) =>
                MoleDoneAction(ff, ForceAction(MoveListAction(f, t, m, from, to, l, then)))
            
            case BattleStartAction(f, a, m, c, o, i, MoleDoneAction(ff, then)) =>
                MoleDoneAction(ff, ForceAction(BattleStartAction(f, a, m, c, o, i, then)))

            case MoveListAction(f, t, m, from, to, l, WorkDoneAction(ff, minister, mayor)) =>
                ff.punchIn(minister, mayor)
                Force(MoveListAction(f, t, m, from, to, l, Repeat))
            
            case BattleStartAction(f, a, m, c, o, i, WorkDoneAction(ff, minister, mayor)) =>
                ff.punchIn(minister, mayor)
                Force(BattleStartAction(f, a, m, c, o, i, Repeat))
                
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
                
            case MoleRevealCardMainAction(f, s, t) =>
                Ask(f, f.hand./(d => MoleRevealCardAction(f, s, d, t).x(d.suit.m(s).not)).cancel)

            case MoleRevealCardAction(f, _, d, t) =>
                f.hand --> d --> f.revealed
                f.log("revealed", d)
                t
                
            case ForcedRemoveEffectAction(e, c, f : Underground, p : Building, then) =>
                f.limbo.one(p) --> f.rubble
                then

            case ForcedRemoveCleanupAction(f : Underground, then) =>
                coffins(f.limbo.get)

                if (f.rubble.num > 0) {
                    f.rubble.get --> f.pool
                    val q = DiscardRandomCardAction(f, then)
                    if (f.swayed.any) {
                        val r = f.swayed./(_.rank).max
                        Ask(f, f.swayed.%(_.rank == r)./(DiscardMinisterAction(f, _, q)))
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
                    f.pool.sub(n, Mole) --> f.burrow

                    f.log("recruited", List.fill(n)(Mole.of(f)).comma, "in", f.burrow)
                }

                if (n > m && f.totalWar)
                    f.oscore(n - m)("recruiting")
                
                Next
                
            case BirdsongNAction(60, f : Underground) =>
                Ask(f, Nil.birdsong(f).done(Next))

            case DaylightNAction(30, f : Underground) =>
                log("Assembly".styled(f))

                Next

            case DaylightNAction(31, f : Underground) =>
                MoleMainAction(f)
            
            case MoleMainAction(f : Underground) =>
                var actions : $[UserAction] = Nil
                
                val att = clearings.%(c => attack(f)(c).any)
                actions :+= MoleAttackAction(f, att).x(att.none)
                    
                val mvv = moveFromR(f)
                actions :+= MoleMoveAction(f, mvv).x(mvv.none)
    
                actions :+= MoleRecruitAction(f).x(f.pooled(Mole) == 0 && f.totalWar.not, "maximum")
    
                val bll = clearings.%(rule(f)).%(canBuild(f)).%(c => f.hand.%(_.suit.m(c.suit)).any)
                actions :+= MoleBuildMainAction(f, bll, Citadel).x(f.pooled(Citadel) == 0, "maximum").x(bll.none)
                actions :+= MoleBuildMainAction(f, bll, Market).x(f.pooled(Market) == 0, "maximum").x(bll.none)

                val tll = clearings.diff(f.all(Tunnel)).%(canPlace(f)).%(c => f.hand.%(_.suit.m(c.suit)).any)
                actions :+= MoleDigMainAction(f, tll).x(tll.none)

                if (f.acted >= 2) {
                    actions = Nil
    
                    actions :+= EndTurnAction(f)
                }
                else
                    actions :+= EndTurnSoftAction(f, ForfeitActions(2 - f.acted))

                Ask(f, actions.odaylight(f))
                
            case MoleAttackAction(f, l) =>
                BattleInitAction(f, NoMessage, l, $(CancelAction), MoleDoneAction(f, Repeat))
                
            case MoleMoveAction(f, l) =>
                MoveInitAction(f, Nil, NoMessage, l, movable(f), $(CancelAction), MoleDoneAction(f, Repeat))
                
            case MoleRecruitAction(f) =>
                if (f.pooled(Mole) > 0) {
                    f.pool.one(Mole) --> f.burrow

                    f.log("recruited", Mole.of(f), "in", f.burrow)
                }
                else
                    f.oscore(1)("recruiting")
                
                MoleDoneAction(f, Repeat)
                
            case MoleBuildMainAction(f, l, b) =>
                Ask(f, l./(MoleBuildClearingAction(f, _, b)).cancel)
                
            case MoleBuildClearingAction(f, c, b) =>
                MoleRevealCardMainAction(f, c.suit, MoleBuildAction(f, c, b))
                
            case MoleBuildAction(f, c, b) =>
                f.drawn --> f.revealed
                f.pool.one(b) --> c
                f.log("built", b.of(f), "in", c)
                MoleDoneAction(f, Repeat)
                
            case MoleDigMainAction(f, l) =>
                Ask(f, l./(MoleDigClearingAction(f, _)).cancel)
                
            case MoleDigClearingAction(f, c) =>
                if (f.pooled(Tunnel) > 0)
                    OptionalDiscardCardAction(f, Digs(f, c), c.suit, MoleDigAction(f, c, None))
                else
                    Ask(f, f.all(Tunnel)./(MoleDigFromClearingAction(f, c, _)).cancel)
                
            case MoleDigFromClearingAction(f, c, s) =>
                OptionalDiscardCardAction(f, Digs(f, c), c.suit, MoleDigAction(f, c, Some(s)))
                
            case MoleDigAction(f, c, from) =>
                f.drawn --> discard(f)

                from.foreach(s => f.at(s).one(Tunnel) --> f.pool)
                f.pool.one(Tunnel) --> c

                if (from.any)
                    f.log("rerouted", Tunnel.of(f), "to", c, "from", from.get)
                else
                    f.log("built", Tunnel.of(f), "in", c)

                MoleDoneAction(f, MoleDigPartyAction(f, c))

            case MoleDigPartyAction(f, c) =>
                Ask(f, 1.to(min(4, f.at(f.burrow).num))./~(n => f.at(f.burrow)./(_.piece).combinations(n))./(MoleDigMoveAction(f, c, _)).done(Repeat))

            case MoleDigMoveAction(f, c, l) =>
                f.at(f.burrow).sublist(l) --> c
                f.log("moved", l./(_.of(f)).comma, "from", f.burrow, "to", c)
                onMoveTo(f, c, Repeat)
 
            case MoleDoneAction(f, then) =>
                f.acted += 1 

                then

            case DaylightNAction(49, f : Underground) =>
                log("Parliament".styled(f))
                Next

            case DaylightNAction(50, f : Underground) =>
                WorkMainAction(f, false)

            case WorkMainAction(f, mayor) =>
                var actions : $[UserAction] = Nil
                
                val mm = mayor.?(f.swayed.%(_.rank <= 3).but(Mayor)).|(f.swayed)
                val ww = mayor.?(Nil).|(f.worked)
                
                if (mm.has(Foremole)) {
                    val bll = clearings.%(rule(f)).%(canBuild(f))
                    actions :+= WorkAction(f, Foremole, mayor).x(ww.has(Foremole)).x(f.pooled(Citadel) + f.pooled(Market) == 0, "maximum").x(f.hand.none, "no cards").x(bll.none, "no place")
                }
                
                val att = clearings.%(c => attack(f)(c).any)
                val mvv = moveFromR(f)

                if (mm.has(Captain))
                    actions :+= WorkAction(f, Captain, mayor).x(ww.has(Captain)).x(att.none)

                if (mm.has(Marshal))
                    actions :+= WorkAction(f, Marshal, mayor).x(ww.has(Marshal)).x(mvv.none)

                if (mm.has(Brigadier))
                    actions :+= WorkAction(f, Brigadier, mayor).x(ww.has(Brigadier)).x(att.none && mvv.none)

                if (mm.has(Banker))
                    actions :+= WorkAction(f, Banker, mayor).x(ww.has(Banker)).x(f.hand.none, "no cards")
      
                if (mm.has(Mayor))
                    actions :+= WorkAction(f, Mayor, mayor).x(ww.has(Mayor)).x(f.swayed.%(_.rank <= 3).but(Mayor).none, "no minister")

                if (mm.has(Queen))
                    actions :+= WorkVPAction(f, Queen, Tunnel, (f.pooled(Tunnel) == 0).??(2)).x(ww.has(Queen)).x(f.pooled(Tunnel) > 0, "not all tunnels")

                if (mm.has(Baron))
                    actions :+= WorkVPAction(f, Baron, Market, f.all(Market).num).x(ww.has(Baron)).x(f.all(Market).none, "no markets")
                
                if (mm.has(Earl))
                    actions :+= WorkVPAction(f, Earl, Citadel, f.all(Citadel).num).x(ww.has(Earl)).x(f.all(Citadel).none, "no citadels")

                if (mayor)
                    Ask(f, actions.refuse(Repeat))
                else
                    Ask(f, actions.done(Next).odaylight(f))
            
            case WorkVPAction(f, m, p, n) =>
                f.punchIn(m, false)
                f.oscore(n)("for", p.sof(f))
                Repeat

            case WorkAction(f, Foremole, mayor) =>
                val l = clearings.%(rule(f)).%(canBuild(f))
                val bb = $(Citadel, Market).%(f.pooled(_) > 0)
                Ask(f, bb./~(b => l./(ForemoleBuildClearingAction(f, _, b, mayor))).cancel)

            case ForemoleBuildClearingAction(f, c, b, mayor) =>
                MoleRevealCardMainAction(f, AnySuit, ForemoleBuildAction(f, c, b, mayor))
                
            case ForemoleBuildAction(f, c, b, mayor) =>
                f.drawn --> f.revealed
                f.punchIn(Foremole, mayor)
                f.pool.one(b) --> c
                f.log("built", b.of(f), "in", c)
                Repeat
                
            case WorkAction(f, Captain, mayor) =>
                val l = clearings.%(c => attack(f)(c).any)
                BattleInitAction(f, NoMessage, l, $(CancelAction), WorkDoneAction(f, Captain, mayor))

            case WorkAction(f, Marshal, mayor) =>
                MoveInitAction(f, Nil, NoMessage, moveFromR(f), movable(f), $(CancelAction), WorkDoneAction(f, Marshal, mayor))
                
            case WorkDoneAction(f, m, mayor) =>
                f.punchIn(m, mayor)

                Repeat

            case WorkAction(f, Brigadier, mayor) =>
                val cc = moveFromR(f)
                val aa = BrigadierWageWarAction(f, mayor).x(clearings.%(c => attack(f)(c).any).none)
                val mm = BrigadierMarchAction(f, mayor).x(moveFromR(f).none)

                Ask(f, $(aa, mm).cancel)

            case BrigadierWageWarAction(f, mayor) =>
                WageWarAction(f, 1, 2, WorkDoneAction(f, Brigadier, mayor))

            case BrigadierMarchAction(f, mayor) =>
                MarchAction(f, 1, 2, WorkDoneAction(f, Brigadier, mayor))

            case WorkAction(f, Banker, mayor) =>
                XXSelectObjectsAction(f, f.hand)
                    .withGroupDyn(l => desc(Banker.of(f), "scores", max(1, l.num).vp, (l.num > 1).?(("with", l.num.hlb)).|("per"), FoxRabbitMouse.%(s => l.all(d => d.suit.m(s))).single, (l.num > 1).?("cards").|("card")))
                    .withRule(_.all(l => FoxRabbitMouse.exists(s => l.all(d => d.suit.m(s)))))
                    .withThen(BankerScoreAction(f, _, mayor))(l => desc("Score", l.num.vp))
                    .withExtra($(CancelAction, NoHand))
                
            case BankerScoreAction(f, l, mayor) =>
                f.punchIn(Banker, mayor)
                f.hand --> l --> discard(f)
                f.nscore(l.num)("banking")(Banker.of(f), ScoredVP, "with", l)
                Repeat
                
            case WorkAction(f, Mayor, _) =>
                WorkMainAction(f, true)

            case DaylightNAction(70, f : Underground) =>
                SwayMinisterMainAction(f)
                
            case SwayMinisterMainAction(f) =>
                val c = clearings.%(c => f.at(c).any)./(_.suit)
                val h = f.hand./(_.suit)
                val s = h.intersect(c) ++ h.intersect(c.diff(h)./(_ => Bird))
                val iiii = f.ministers
                val iii = iiii.diff(f.swayed)
                val ii = iii.%(_.rank <= s.num)
                val i = ii.%(m => (f.swayed ++ f.retired).%(_.rank == m.rank).num < 3)
 
                Ask(f, iii./(m => SwayMinisterSelectAction(f, c, m).x(m.rank > f.hand.num, "no cards").x(ii.has(m).not, "no clearings").x(i.has(m).not, "no crown")).done(Next).daylight(f))
            
            case SwayMinisterSelectAction(f, s, m) =>
                XXSelectObjectsAction(f, f.hand)
                    .withGroupDyn(l => desc(f, "sways", m.of(f), "with", m.rank.hlb, "of", s, "cards"))
                    .withRule(_.num(m.rank).all(l => { 
                        val q : $[Suit] = l./(_.suit)
                        val ss : $[DiscardCost] = s.diff(q)
                        val qq : $[Suit] = q.diff(s)
                    
                        qq.permutations.exists { pp =>
                            var sss = ss
                            var ppp = pp
                            var result = true
                    
                            while (ppp.any && result) {
                                sss.find(ppp(0).m(_)) match {
                                    case Some(sp) => 
                                        sss :-= sp
                                        ppp = ppp.drop(1)
                                    case None => result = false
                                }
                            }
                    
                            result
                        }
                    }))
                    .withThenElem(SwayMinisterAction(f, _, m))(desc("Sway", m.of(f), "for", (m.rank - 1).vp))
                    .withExtra($(CancelAction, NoHand))
                
            case SwayMinisterAction(f, l, m) =>
                f.hand --> l --> f.revealed
    
                f.swayed :+= m
                f.log("swayed", m.of(f), "with", l)

                f.oscore(m.rank - 1)("swaying", m.of(f))
                
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

            case NightStartAction(f : Underground) =>
                EveningDrawAction(f, 1 + f.all(Market).num)
                
            case FactionCleanUpAction(f : Underground) =>
                f.acted = 0
                f.worked = Nil
                EndPlayerTurnAction(f)

            case _ => UnknownContinue
        }
    }
}
