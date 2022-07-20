package root

import root.gaming._

import colmat._

import hrf.elem._
import root.elem._

trait Mischief extends WarriorFaction {
    val expansion = MischiefExpansion

    val warrior = Raven

    val abilities = List(Nimble, MorningCraft, EmbeddedAgents)
    
    override val transports : List[List[Transport]] = $($(FreeMove))

    val pieces = Raven ** 15 ++ Bomb ** 2 ++ Snare ** 2 ++ Extortion ** 2 ++ Raid ** 2

    lazy val kinds = pieces./~{
        case p : Plot => Some(p)
        case _ => None
    }.distinct

    def advertising = Plot.guessable./(_.realImg(this)).merge
    def motto = "Trick".styled(this)
}

trait Plot extends Token {
    override def of(f : Faction) = Plot.of(f)
    def realOf(f : Faction) = super.of(f)
    override def img(f : Faction) = Plot.img(f)
    def realImg(f : Faction) = super.img(f)
}

case object Bomb extends Plot
case object Snare extends Plot
case object Extortion extends Plot
case object Raid extends Plot
case object Diversion extends Plot

case object Diversions extends FactionEffect

case object Plot extends Token {
    val all = $[Plot](Bomb, Snare, Extortion, Raid, Diversion)
    val guessable = $[Plot](Bomb, Snare, Extortion, Raid)
    val flippable = $[Plot](Bomb, Snare, Extortion, Raid)
}

case object Raven extends Warrior

case object EmbeddedAgents extends BattleEffect

case object CC extends Mischief {
    val name = "Corvid Conspiracy"
    override def funName = NameReference(name, this) ~ " Conspiracy"
    val short = "CC"
    val style = "cc"
    val priority = "I"
}

case object RI extends Mischief {
    val name = "Raven Innocence"
    override def funName = NameReference(name, this) ~ " Innocence"
    val short = "RI"
    val style = "ri"
    val priority = "I'"
    
    override val pieces = CC.pieces ++ Diversion ** 2
}

class MischiefPlayer(val game : Game, val faction : Mischief) extends PlayerState {
    var acted = 0
    var placed = 0
    var exert = false
    var hidden = List[Clearing]()
    
    def plots = all(Bomb) ++ all(Snare) ++ all(Extortion) ++ all(Raid) ++ all(Diversion)

    def craft = plots./(game.mapping)
}

// CC
trait CCDaylightQuestion extends FactionAction {
    override def self : Mischief
    def question(g : Game) = self.elem ~ g.cc(self).exert.?(
            " (" ~ "Evening".styled(styles.phase) ~ ")" ~ 
            Div(
                1.to(3)./(_ => Image("action-black", styles.action)) ~
                Image("action-bird", styles.action), 
            styles.margined)
        ).|(
            " (" ~ "Daylight".styled(styles.phase) ~ ")" ~
            Div(
                1.to(3)./(_ => Image("action-black", styles.action)).take(g.cc(self).acted) ~
                1.to(3)./(_ => Image(self.style + "-action", styles.action)).drop(g.cc(self).acted) ~
                Image("action-bird", styles.action),
            styles.margined)
        )
}

case class ToExpose(e : Mischief, p : Plot, c : Clearing) extends Message {
    def elem(g : Game) = "to expose " ~ p.realOf(e) ~ " in " ~ c.elem(g)
}

case class RecruitsIn(l : List[Clearing]) extends Message {
    def elem(g : Game) = " to recruit in " ~ l./(_.elem(g)).join(" ")
}


case class FlipMainAction(self : Mischief) extends ForcedAction
case class FlipAction(self : Mischief, p : Plot, c : Clearing) extends BaseAction("Flip plots")(p.realOf(self), p.img(self), "in", c)
case class RavenRecruitSuitAction(self : Mischief, s : Suit, l : List[Clearing], n : Int) extends BaseAction("Recruit")(s, self.warrior.imgd(self).repeat(min(n, l.num)).merge) with Soft
case class RavenRecruitListAction(self : Mischief, s : Suit, l : List[Clearing], r : List[Clearing]) extends BaseAction("Recruit", self.warrior.img(self).repeat(l.num).merge, "in")(l) with Soft
case class RavenRecruitAction(self : Mischief, s : Suit, l : List[Clearing], r : List[Clearing]) extends ForcedAction

case class RavenDoneAction(f : Mischief, then : ForcedAction) extends ForcedAction
case class RavenMainAction(f : Mischief) extends ForcedAction


case class RavenAttackAction(self : Mischief, l : List[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with CCDaylightQuestion with Soft

case class RavenMoveAction(self : Mischief, l : List[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with CCDaylightQuestion with Soft

case class RavenPlotSetupAction(self : Mischief, p : Plot, l : List[Clearing]) extends BaseAction(self, "places plot")("Plot", p.realImg(self), p.realOf(self)) with Soft

case class RavenPlotMainAction(self : Mischief, p : Plot, l : List[Clearing], n : Int) extends OptionAction("Plot", p.realOf(self), self.warrior.imgd(self).repeat(n).merge, dt.Arrow, p.realImg(self)) with CCDaylightQuestion with Soft
case class RavenPlotAction(self : Mischief, p : Plot, n : Int, c : Clearing) extends BaseAction("Plot", p.realOf(self), p.realImg(self), "for", (n > 0).?(self.warrior.imgd(self).repeat(n).merge).|("free"), "in")(c)

case class RavenTrickMainAction(self : Mischief, h : List[Clearing], r : List[Clearing]) extends OptionAction("Trick".styled(self), dt.Swap) with CCDaylightQuestion with Soft
case class RavenTrickAction(self : Mischief, a : Clearing, b : Clearing, ap : Plot, bp : Plot) extends BaseAction("Trick")(ap.realOf(self), ap.realImg(self), "in", a, "with", bp.realOf(self), bp.realImg(self), "in", b)

case class RavenRaidMainAction(self : Mischief, c : Clearing, then : ForcedAction) extends ForcedAction
case class RavenRaidAction(self : Mischief, c : Clearing, l : List[Clearing], then : ForcedAction) extends BaseAction("Raid".styled(self), "from", c)(l)

case class PlotRemovedAction(self : Mischief, c : Clearing, then : ForcedAction) extends ForcedAction

case class PlotInfoAction(self : Mischief, p : Plot, c : Clearing) extends BaseInfo(Break ~ "Hidden Plots")(p.realOf(self), p.realImg(self), "in", c) with Info

case class ExposeMainAction(self : Faction, f : Mischief, l : List[Clearing], then : ForcedAction) extends BaseAction("\nExpose")("Expose", f) with Soft
case class ExposeClearingAction(self : Faction, f : Mischief, c : Clearing, then : ForcedAction) extends BaseAction("Expose", f, "in")(c) with Soft
case class ExposePlotAction(self : Faction, f : Mischief, c : Clearing, p : Plot, then : ForcedAction) extends BaseAction("Expose", f, "in", c)(p.realImg(f), p.realOf(f)) with Soft
case class ExposeAction(self : Faction, f : Mischief, c : Clearing, p : Plot, then : ForcedAction) extends ForcedAction

case class NoExertAction(self : Mischief, n : Int) extends BaseAction(None)("Draw Cards", dt.CardBack.repeat(n).merge)

case class ClearBombAction(self : Mischief, c : Clearing) extends ForcedAction

object MischiefExpansion extends Expansion {
    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            // SETUP
            case CreatePlayerAction(f : Mischief) =>
                pstates += f -> new MischiefPlayer(game, f)
                FactionInitAction(f)
                
            case FactionSetupAction(f : Mischief) =>
                if (f.plots.none && game.options.has(AdSetBuffOn)) {
                    f.placed = -1
    
                    val ptt = clearings.diff(homelands).%(canPlace(f)).%(c => factions.but(f)./~(_.at(c)).scoring.none)

                    Ask(f)(f.kinds./(p => RavenPlotSetupAction(f, p, ptt).x(f.pooled(p) == 0, "maximum").x(ptt.none)))
                }
                else
                if (f.all(Raven).num - f.hidden.num < 3) {
                    val s = f.all(Raven).diff(f.hidden)./(_.suit)
                    Ask(f, clearings.%(canPlace(f))./(c => PlaceStartingWarriorAction(f, c, Raven).x(c.suit.in(s))))
                }
                else 
                    SetupNextAction
                
            case PlaceStartingWarriorAction(f : Mischief, r, p) =>
                f.pool.one(p) --> r
    
                f.log("placed", p.of(f), "in", r)
                
                FactionSetupAction(f)

            // HELPER
            case MoveListAction(f, t, m, from, to, l, RavenDoneAction(ff, then)) =>
                RavenDoneAction(ff, ForceAction(MoveListAction(f, t, m, from, to, l, then)))
            
            case BattleStartAction(f, a, m, c, o, i, RavenDoneAction(ff, then)) =>
                RavenDoneAction(ff, ForceAction(BattleStartAction(f, a, m, c, o, i, then)))

            case BattlePostHitInAction(b, e, f : Mischief, Raven, then) =>
                e.log("scared", Raven.of(f))
                then

            case BattlePostHitInAction(b, e, f : Mischief, Raid, then) =>
                e.log("brought on", Raid.realOf(f))
                then
 
            case BattlePostHitInAction(b, e, f : Mischief, p : Plot, then) =>
                e.log("diffused", Plot.of(f))
                then

            case ForcedRemoveEffectAction(e, c, f : Mischief, Raid, then) =>
                RavenRaidMainAction(f, c, then)
            
            case ForcedRemoveEffectAction(e, c, f : Mischief, p : Token, then) =>
                PlotRemovedAction(f, c, then)
            
            case ExposeMainAction(f, e, l, then) =>
                Ask(f, l./(c => ExposeClearingAction(f, e, c, then).x(f.hand.%(_.suit.m(c.suit)).none, "no matching cards")).cancel)
                
            case ExposeClearingAction(f, e, c, then) =>
                Ask(f, Plot.guessable./(ExposePlotAction(f, e, c, _, then)).cancel)

            case ExposePlotAction(f, e, c, p, then) =>
                OptionalDiscardCardAction(f, ToExpose(e, p, c), c.suit, ExposeAction(f, e, c, p, then))
                
            case ExposeAction(f, e, c, p, then) =>
                if (e.at(c).plot.has(p)) {
                    f.oscore(1)("exposing", p.realOf(e), "in", c, "with", f.drawn.get)
                    f.drawn --> f.hand
                    e.hidden :-= c
                    e.at(c).one(p) --> e.pool
                }
                else {
                    f.log("failed to expose", p.realOf(e), "in", c, " and gave ", f.drawn.get, "to", e)
                    f.drawn --> e.hand
                }
                
                then
                
            // TURN
            case BirdsongNAction(40, f : Mischief) =>
                XCraftMainAction(f)
    
            case BirdsongNAction(50, f : Mischief) =>
                Ask(f, f.plots.%(_.in(f.hidden)).%(c => f.at(c).warrior.any)./(c => FlipAction(f, f.at(c).plot.get, c).x(Plot.flippable.has(f.at(c).plot.get).not)).done(Next).birdsong(f).noPlots)
                
            case FlipAction(f, p, c) =>
                highlights :+= PlaceHighlight(List(c))

                f.hidden :-= c

                val n = f.plots.diff(f.hidden).num

                p match {
                    case Bomb =>
                        f.nscore(n)("flipping", p.realOf(f))(f, "bombed", c, ForVP)
                        NukeAction(f, factions.%(f.friends), List(c), ClearSector, ClearBombAction(f, c))
                    case Snare =>
                        f.nscore(n)("flipping", p.realOf(f))(f, "ensnared", c, ForVP)
                        Repeat
                    case Raid =>
                        f.nscore(n)("flipping", p.realOf(f))(f, "planned raid from", c, ForVP)
                        Repeat
                    case Extortion =>
                        f.nscore(n)("flipping", p.realOf(f))(f, "shook down", c, ForVP)
                        MassStealCardAction(f, factions.but(f).%(_.at(c).any), Repeat)
                }
                
            case ClearBombAction(f, c) =>
                f.at(c).one(Bomb) --> f.pool

                Repeat
                
            case BirdsongNAction(60, f : Mischief) =>
                if (f.pooled(Raven) > 0 || f.totalWar) {
                    val l = FoxRabbitMouse./(s => s -> f.hand.%(_.suit.m(s)).any.??(clearings.%(_.suit == s).%(canPlace(f)))).toMap
                    Ask(f, FoxRabbitMouse./(s => RavenRecruitSuitAction(f, s, l(s), f.pooled(Raven)).x(l(s).none)).skip(Next).birdsong(f))
                }
                else
                    Next

            case RavenRecruitSuitAction(f, s, l, n) =>
                if (f.pooled(Raven) >= l.num)
                    Force(RavenRecruitListAction(f, s, l, l))
                else
                    Ask(f, l.combinations(f.pooled(Raven)).toList./(RavenRecruitListAction(f, s, _, l)).cancel)

            case RavenRecruitListAction(f, s, l, r) =>
                OptionalDiscardCardAction(f, RecruitsIn(l), s, RavenRecruitAction(f, s, l, r))

            case RavenRecruitAction(f, s, l, r) =>
                highlights :+= PlaceHighlight(l)

                f.drawn --> discard(f).quiet

                l.foreach(f.pool.one(Raven) --> _)
                                             
                if (l.any)
                    f.log("recruited", "in", l./(_.elem(game)).comma, "with", f.drawn.get)

                if (r.num > l.num && f.totalWar)
                    f.oscore(r.num - l.num)("recruiting") 

                Next
                
            case RavenRaidMainAction(f, c, then) =>
                f.hidden :-= c

                f.limbo.one(Raid) --> c

                val l = connected(f)(c).of[Clearing].%(canPlace(f))

                while (f.pooled(Raven) < l.num && f.limbo.got(Raven))
                    f.limbo.one(Raven) --> f.pool

                if (f.pooled(Raven) >= l.num)
                    Force(RavenRaidAction(f, c, l, then))
                else
                    Ask(f, l.combinations(f.pooled(Raven))./(RavenRaidAction(f, c, _, then)))

            case RavenRaidAction(f, c, l, then) =>
                l.foreach(f.pool.one(Raven) --> _)

                f.at(c).one(Raid) --> f.limbo

                f.log("raided", l)

                then
    
            case PlotRemovedAction(f, c, then) =>
                f.hidden :-= c
  
                then
 
            case DaylightNAction(50, f : Mischief) =>
                RavenMainAction(f)

            case EveningNAction(40, f : Mischief) =>
                f.exert = true
                Next

            case EveningNAction(50, f : Mischief) =>
                if (f.exert)
                    RavenMainAction(f)
                else
                    Next
                
            case RavenMainAction(f) =>
                var actions : List[UserAction] = Nil
                
                val att = clearings.%(c => attack(f)(c).any)
                actions :+= RavenAttackAction(f, att).x(att.none)
                    
                val mvv = moveFrom(f)
                actions :+= RavenMoveAction(f, mvv).x(mvv.none)
    
                val ptt = clearings.diff(f.plots).%(canPlace(f)).%(c => f.at(c).warrior.num > f.placed)
                
                f.kinds.foreach { p =>
                    actions :+= RavenPlotMainAction(f, p, ptt, f.placed + 1).x(f.pooled(p) == 0, "maximum").x(ptt.none)
                }

                val thh = (f.hidden.num > 1).??(f.hidden)
                val trr = (f.plots.diff(f.hidden)./(c => f.at(c).plot.get).distinct.num > 1).??(f.plots.diff(f.hidden))

                actions :+= RavenTrickMainAction(f, thh, trr).x(thh.none && trr.none)

                if (f.exert) {
                    Ask(f, (actions :+ NoExertAction(f, 1 + f.all(Extortion).diff(f.hidden).num)).evening(f))
                }
                else {
                    if (f.acted >= 3) {
                        actions = Nil

                        actions :+= EndTurnAction(f)
                    }
                    else
                        actions :+= EndTurnSoftAction(f, ForfeitActions(3 - f.acted))
                     
                    Ask(f, actions.daylight(f))
                }

            case NoExertAction(f, _) =>
                Next

            case RavenAttackAction(f, l) =>
                BattleInitAction(f, NoMessage, l, $(CancelAction), RavenDoneAction(f, Repeat))
                
            case RavenMoveAction(f, l) =>
                MoveInitAction(f, Nil, NoMessage, l, movable(f), $(CancelAction), RavenDoneAction(f, Repeat))
                
            case RavenPlotMainAction(f, p, l, n) =>
                Ask(f, l./(RavenPlotAction(f, p, n, _)).cancel)

            case RavenPlotSetupAction(f, p, l) =>
                Ask(f, l./(RavenPlotAction(f, p, -1, _)).cancel)

            case RavenPlotAction(f, p, n, c) =>
                highlights :+= PlaceHighlight(List(c))

                if (n == -1) {
                    f.pool.one(Raven) --> c
                    if (options.has(SetupTypeHomelands)) {
                        homelands :+= c

                        f.log("started in", c)
                    }
                }
                else
                    f.at(c).sub(n, Raven) --> f.pool

                f.pool.one(p) --> c

                f.placed += 1

                f.hidden :+= c

                f.log("started a", Plot.of(f), "in", c, "with", (n > 0).??(Raven.of(f).repeat(n)))
                
                if (f.placed == 0)
                    FactionSetupAction(f)
                else
                    RavenDoneAction(f, Repeat)
                
            case RavenTrickMainAction(f, h, r) =>
                Ask(f, (h.combinations(2) ++ r.combinations(2)).toList./(ab => RavenTrickAction(f, ab(0), ab(1), f.at(ab(0)).plot.get, f.at(ab(1)).plot.get)).cancel)

            case RavenTrickAction(f, a, b, _, _) =>
                highlights :+= PlaceHighlight(List(a, b))

                val pa = f.at(a).one(f.at(a).plot.get)
                val pb = f.at(b).one(f.at(b).plot.get)
                
                pa --> b
                pb --> a

                f.log("swapped plots in", a, "and", b)
                
                RavenDoneAction(f, Repeat)

            case RavenDoneAction(f, then) =>
                if (f.exert)
                    f.exert = false
                else
                    f.acted += 1 

                then
    
            case NightStartAction(f : Mischief) =>
                if (f.exert.not)
                    EveningHandLimitAction(f)
                else
                    EveningDrawAction(f, 1 + f.all(Extortion).diff(f.hidden).num)
                
            case FactionCleanUpAction(f : Mischief) =>
                f.acted = 0
                f.placed = 0
                f.exert = false
                EndPlayerTurnAction(f)
                
            case _ => UnknownContinue
        }
    }
}

