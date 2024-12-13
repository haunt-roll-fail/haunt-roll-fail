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

trait Mischief extends WarriorFaction { self =>
    val clashKey = CC

    val warrior = Raven

    def abilities(options : $[Meta.O]) = $(Nimble, MorningCraft, EmbeddedAgents)

    override val transports : $[$[Transport]] = $($(FreeMove))

    def pieces(options : $[Meta.O]) = {
        val n = 2 + options.has(ThreeOfEachPlot).??(1)
        Raven     *** 15 ++
        Bomb      *** n ++
        Snare     *** n ++
        Extortion *** n ++
        Raid      *** n ++
        Diversion *** options.has(DiversionPlot).??(n) ++
        HiddenPlot      *** (n * (4 + options.has(DiversionPlot).??(1)))
    }

    def kinds(implicit game : Game) = pieces(game.options).of[RealPlot].distinct ++ game.options.has(BrutalHonesty).$(OpenBomb)

    def advertising = Plot.guessable./(_.img(self)).merge
    def motto = "Trick".styled(self)
}

trait Plot extends Token

trait RealPlot extends Plot {
    def real : RealPlot = this
}

case object OpenBomb extends RealPlot {
    override def id = "bomb"
    override def name = "Revealed Bomb"
    override def of(f : Faction) = "Revealed".hh ~ " " ~ "Bomb".styled(f)
    override def real = Bomb
}
case object Bomb extends RealPlot
case object Snare extends RealPlot
case object Extortion extends RealPlot
case object Raid extends RealPlot
case object Diversion extends RealPlot

case object Diversions extends Effect

case object HiddenPlot extends Plot {
    override def id = "Plot"
}

case object Plot {
    val all = $[RealPlot](Bomb, Snare, Extortion, Raid, Diversion)
    val guessable = $[RealPlot](Bomb, Snare, Extortion, Raid)
    val flippable = $[RealPlot](Bomb, Snare, Extortion, Raid)
}

case object Raven extends Warrior

case object EmbeddedAgents extends BattleEffect with FactionEffect {
    val name = "Embedded Agents"
}

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
}

class MischiefPlayer(val faction : Mischief)(implicit val game : Game) extends FactionState {
    var acted = 0
    var placed = 0
    var exert = false
    var secret = Map[Clearing, RealPlot]()
    def hidden = secret.keys.$

    def plots = all(HiddenPlot) ++ all(Bomb) ++ all(Snare) ++ all(Extortion) ++ all(Raid) ++ all(Diversion)

    def craft = plots./(_.asset)
}

trait CCDaylightQuestion extends FactionAction {
    override def self : Mischief
    def question(implicit game : Game) = self.elem ~ self.exert.?(
            SpacedDash ~ Evening.elem ~
            Div(
                1.to(3)./(_ => Image("action-black", styles.action)) ~
                Image("action-bird", styles.action),
            styles.margined)
        ).|(
            SpacedDash ~ Daylight.elem ~
            Div(
                1.to(3)./(_ => Image("action-black", styles.action)).take(self.acted) ~
                1.to(3)./(_ => Image(self.style + "-action", styles.action)).drop(self.acted) ~
                Image("action-bird", styles.action),
            styles.margined)
        )
}

case class ToExpose(e : Mischief, p : RealPlot, c : Clearing) extends Message {
    def elem(implicit game : Game) = "to expose " ~ p.of(e) ~ " in " ~ c.elem
}

case class RecruitsIn(l : $[Clearing]) extends Message {
    def elem(implicit game : Game) = " to recruit in " ~ l./(_.elem).comma
}

case class FlipMainAction(self : Mischief) extends ForcedAction
case class FlipAction(self : Mischief, p : RealPlot, c : Clearing) extends BaseAction("Flip plots")(p.of(self), p.img(self), "in", c)
case class MischiefRecruitSuitAction(self : Mischief, s : Suit, l : $[Clearing], n : Int) extends BaseAction("Recruit")(s, min(n, l.num).times(self.warrior.imgd(self)).merge) with Soft
case class MischiefRecruitListAction(self : Mischief, s : Suit, l : $[Clearing], r : $[Clearing]) extends BaseAction("Recruit", l.num.times(self.warrior.img(self)).merge, "in")(l.comma) with Soft
case class MischiefRecruitAction(self : Mischief, s : Suit, l : $[Clearing], r : $[Clearing]) extends ForcedAction

case class MischiefDoneAction(f : Mischief, then : ForcedAction) extends ForcedAction
case class MischiefMainAction(f : Mischief) extends ForcedAction with Soft

case class MischiefAttackAction(self : Mischief, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with CCDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }

case class MischiefMoveAction(self : Mischief, l : $[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with CCDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }

case class MischiefPlotSetupAction(self : Mischief, p : RealPlot, l : $[Clearing]) extends BaseAction(self, "places plot")("Plot", p.img(self), p.of(self)) with Soft

case class MischiefPlotMainAction(self : Mischief, p : RealPlot, l : $[Clearing], n : Int) extends OptionAction("Plot", p.of(self), n.times(self.warrior.imgd(self)).merge, dt.Arrow, p.img(self)) with CCDaylightQuestion with Soft with Only[MischiefPlotAction] { def tag = implicitly }
case class MischiefPlotAction(self : Mischief, p : RealPlot, n : Int, c : Clearing) extends BaseAction("Plot", p.of(self), p.img(self), "for", (n > 0).?(n.times(self.warrior.imgd(self)).merge).|("free"), "in")(c)

case class MischiefTrickMainAction(self : Mischief, h : $[Clearing], r : $[Clearing]) extends OptionAction("Trick".styled(self), dt.Swap) with CCDaylightQuestion with Soft with Only[MischiefTrickAction] { def tag = implicitly }
case class MischiefTrickAction(self : Mischief, a : Clearing, b : Clearing, ap : RealPlot, bp : RealPlot) extends BaseAction("Trick")(ap.of(self), ap.img(self), "in", a, "with", bp.of(self), bp.img(self), "in", b)

case class MischiefRaidMainAction(self : Mischief, c : Clearing, then : ForcedAction) extends ForcedAction
case class MischiefRaidAction(self : Mischief, c : Clearing, l : $[Region], then : ForcedAction) extends BaseAction("Raid".styled(self), "from", c)(l.comma)

case class PlotRemovedAction(self : Mischief, c : Clearing, then : ForcedAction) extends ForcedAction

case class PlotInfoAction(self : Mischief, p : RealPlot, c : Clearing) extends BaseInfo(Break ~ "Hidden Plots")(p.of(self), p.img(self), "in", c) with Info

case class ExposeMainAction(self : Faction, f : Mischief, l : $[Clearing], then : ForcedAction) extends BaseAction("Expose")("Expose", f) with Soft with Only[PrepareDiscardCardsAction] { def tag = implicitly }
case class ExposeClearingAction(self : Faction, f : Mischief, c : Clearing, then : ForcedAction) extends BaseAction("Expose", f, "in")(c) with Soft
case class ExposePlotAction(self : Faction, f : Mischief, c : Clearing, p : RealPlot, then : ForcedAction) extends BaseAction("Expose", f, "in", c)(p.img(f), p.of(f)) with Soft
case class ExposeAction(self : Faction, f : Mischief, c : Clearing, p : RealPlot, then : ForcedAction) extends ForcedAction

case class NoExertAction(self : Mischief, n : Int) extends OptionAction("Draw Cards", n.times(dt.CardBack).merge) with CCDaylightQuestion

case class ClearBombAction(self : Mischief, c : Clearing) extends ForcedAction


object MischiefExpansion extends FactionExpansion[Mischief] {
    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        expose(f)
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        expose(f)
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        expose(f)
    }

    def expose(f : Faction)(implicit game : Game, ask : ActionCollector) {
        factions.but(f).of[Mischief].foreach { e =>
            e.hidden.%(f.present).some.foreach { l =>
                + ExposeMainAction(f, e, l, Repeat)
                    .!(l.%(c => f.hand.exists(_.matches(c.cost))).none, "no matching cards")
            }
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Mischief) =>
            game.states += f -> new MischiefPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Mischief) =>
            if (f.plots.none && game.options.has(AdSetBuffOn)) {
                f.placed = -1

                val ptt = clearings.diff(game.homelands).%(f.canPlace).%(c => factions.but(f)./~(_.at(c)).of[Scoring].none)

                Ask(f)(f.kinds./(p => MischiefPlotSetupAction(f, p, ptt).!(f.pooled(p.real) == f.secret.values.$.count(p), "maximum").!(ptt.none)))
            }
            else
            if (f.all(Raven).num - f.plots.num < 3) {
                val costs = f.all(Raven).diff(f.plots)./(_.cost)

                Ask(f)(clearings.%(f.canPlace)./(c => PlacePieceAction(f, c, Raven, FactionSetupAction(f)).!(
                    FoxRabbitMouse.permutations.exists(_.lazyZip(c.cost +: costs).forall((a, c) => a.matches(c))).not
                )))
            }
            else
                SetupFactionsAction

        // HELPER
        case MoveListAction(self, f, t, m, from, to, l, MischiefDoneAction(ff, then)) =>
            MischiefDoneAction(ff, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case BattleStartAction(self, f, a, m, c, o, i, MischiefDoneAction(ff, then)) =>
            MischiefDoneAction(ff, BattleStartAction(self, f, a, m, c, o, i, then))

        case LegalAAAConveneAction(f, h, c, e, MischiefDoneAction(ff, then)) =>
            MischiefDoneAction(ff, LegalAAAConveneAction(f, h, c, e, then))


        case BattlePostHitInAction(b, e, f : Mischief, Raven, then) =>
            e.log("scared", Raven.of(f))
            then

        case BattlePostHitInAction(b, e, f : Mischief, Raid, then) =>
            e.log("brought on", Raid.of(f))
            then

        case BattlePostHitInAction(b, e, f : Mischief, HiddenPlot, then) if f.secret.get(b.clearing).has(Raid) =>
            e.log("brought on", Raid.of(f))
            then

        case BattlePostHitInAction(b, e, f : Mischief, p : Plot, then) =>
            e.log("diffused", p.of(f))
            then

        case ForcedRemoveTargetEffectAction(e, c, f : Mischief, Raid, then) =>
            MischiefRaidMainAction(f, c, then)

        case ForcedRemoveTargetEffectAction(e, c, f : Mischief, HiddenPlot, then) if f.secret.get(c).has(Raid) =>
            PlotRemovedAction(f, c, MischiefRaidMainAction(f, c, then))

        case ForcedRemoveTargetEffectAction(e, c, f : Mischief, HiddenPlot, then) =>
            PlotRemovedAction(f, c, then)

        case ExposeMainAction(f, e, l, then) =>
            Ask(f)
                .each(l)(c =>
                    ExposeClearingAction(f, e, c, then)
                        .!(f.hand.%(_.matches(c.cost)).none, "no matching cards")
                        .!(f.canRemove(c)(e).not, "can't remove")
                )
                .cancel

        case ExposeClearingAction(f, e, c, then) =>
            Ask(f)(Plot.guessable./(ExposePlotAction(f, e, c, _, then))).cancel

        case ExposePlotAction(f, e, c, p, then) =>
            OptionalDiscardCardAction(f, ToExpose(e, p, c), c.cost, ExposeAction(f, e, c, p, then))

        case ExposeAction(f, e, c, p, then) =>
            if (e.secret.get(c).has(p)) {
                e.notify(f.drawn./(d => ViewCardInfoAction(f, f.elem ~ " exposed " ~ p.of(e) ~ " in " ~ c.elem ~ " with", d)))
                f.oscore(1)("exposing", p.of(e), "in", c)
                f.drawn --> f.hand
                e.secret -= c
                e.from(c) --> HiddenPlot --> e.reserve
            }
            else {
                e.notify(f.drawn./(d => ViewCardInfoAction(f, f.elem ~ " failed to expose " ~ p.of(e) ~ " in " ~ c.elem ~ " with", d)))
                f.log("failed to expose", p.of(e), "in", c, "and gave the card to", e)
                f.drawn --> e.hand
            }

            then

        // TURN
        case BirdsongNAction(40, f : Mischief) =>
            XCraftMainAction(f)

        case BirdsongNAction(50, f : Mischief) =>
            Ask(f)(NoPlots)
              .each(f.plots.%(_.in(f.hidden)).%(c => f.at(c).of[Warrior].any))(c => FlipAction(f, f.secret(c), c).!(Plot.flippable.has(f.secret(c)).not))
              .done(Next)
              .birdsong(f)

        case FlipAction(f, p, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.from(c) --> HiddenPlot --> f.reserve

            f.reserve --> p --> c

            f.secret -= c

            val n = f.plots.diff(f.hidden).num

            p match {
                case Bomb =>
                    f.nscore(n)("flipping", p.of(f))(f, "bombed", c, ForVP)
                    NukeAction(f, f.enemies.%(f.canRemove(c)), $(c), NukeType.ClearSector, ClearBombAction(f, c))
                case Snare =>
                    f.nscore(n)("flipping", p.of(f))(f, "ensnared", c, ForVP)
                    Repeat
                case Raid =>
                    f.nscore(n)("flipping", p.of(f))(f, "planned raid from", c, ForVP)
                    Repeat
                case Extortion =>
                    f.nscore(n)("flipping", p.of(f))(f, "shook down", c, ForVP)
                    MassStealCardAction(f, factions.but(f).%(_.present(c)), Repeat)
            }

        case ClearBombAction(f, c) =>
            f.from(c) --> Bomb --> f.reserve

            Repeat

        case BirdsongNAction(60, f : Mischief) =>
            if (f.pool(Raven) || f.totalWar) {
                Ask(f)
                  .each(FoxRabbitMouse ++ factions.of[CommonInvasive].any.$(Frog))(s => {
                    val d = f.hand.%(_.matches(s)).any
                    val l = d.??(clearings.%(_.cost.matched(s)).%(f.canPlace))
                    MischiefRecruitSuitAction(f, s, l, f.pooled(Raven)).!(d.not, "no cards").!(l.none, "can't place")
                  })
                  .skip(Next)
                  .birdsong(f)
            }
            else
                Next

        case MischiefRecruitSuitAction(f, s, l, n) =>
            if (f.pooled(Raven) >= l.num)
                Force(MischiefRecruitListAction(f, s, l, l))
            else
                Ask(f)(l.combinations(f.pooled(Raven)).$./(MischiefRecruitListAction(f, s, _, l))).cancel

        case MischiefRecruitListAction(f, s, l, r) =>
            OptionalDiscardCardAction(f, RecruitsIn(l), s, MischiefRecruitAction(f, s, l, r))

        case MischiefRecruitAction(f, s, l, r) =>
            game.highlights :+= PlaceHighlight(l)

            l.foreach(c => f.reserve --> Raven --> c)

            if (l.any)
                f.log("recruited", "in", l./(_.elem(game)).comma, f.drawn.get.any.?("with"), f.drawn.get)

            f.drawn --> discard.quiet

            if (r.num > l.num && f.totalWar)
                f.oscore(r.num - l.num)("recruiting")

            Next

        case MischiefRaidMainAction(f, c, then) =>
            val after = f.connectedFor(c).%(f.canPlace)

            if (f.limbo(c).has(HiddenPlot)) {
                f.limbo(c) --> HiddenPlot --> f.reserve
                f.reserve --> Raid --> f.limbo(c)
            }

            f.limbo(c) --> Raid --> c

            val before = f.connectedFor(c).%(f.canPlace)

            val l = options.has(TunnelsIgnoreRaid).?(after).|(before)

            while (f.pooled(Raven) < l.num && f.limbo(c).$.pieces.has(Raven))
                f.limbo(c) --> Raven --> game.recycle

            if (f.pooled(Raven) >= l.num)
                Force(MischiefRaidAction(f, c, l, then))
            else
                Ask(f)(l.combinations(f.pooled(Raven))./(MischiefRaidAction(f, c, _, then)))

        case MischiefRaidAction(f, c, l, then) =>
            l.foreach(c => f.reserve --> Raven --> c)

            f.from(c) --> Raid --> f.limbo(c)

            f.log("raided", l./(_.elem).comma)

            then

        case PlotRemovedAction(f, c, then) =>
            f.secret -= c

            then

        case DaylightNAction(50, f : Mischief) =>
            MischiefMainAction(f)

        case EveningNAction(40, f : Mischief) =>
            f.exert = true
            Next

        case EveningNAction(50, f : Mischief) =>
            if (f.exert)
                MischiefMainAction(f)
            else
                Next

        case MischiefMainAction(f) =>
            var actions : $[UserAction] = $

            val att = clearings.%(f.canAttackIn)
            actions :+= MischiefAttackAction(f, att).!(att.none)

            val mvv = f.moveFrom.of[Clearing]
            actions :+= MischiefMoveAction(f, mvv).!(mvv.none)

            val ptt = clearings.diff(f.plots).%(f.canPlace).%(c => f.at(c).of[Warrior].num > f.placed)

            f.kinds.foreach { p =>
                actions :+= MischiefPlotMainAction(f, p, ptt, f.placed + 1).!(f.pooled(p.real) == f.secret.values.$.count(p), "maximum").!(ptt.none)
            }

            val thh = (f.hidden.num > 1).??(f.hidden)
            val trr = (f.plots.diff(f.hidden)./(c => f.at(c).of[RealPlot].only).distinct.num > 1).??(f.plots.diff(f.hidden))

            actions :+= MischiefTrickMainAction(f, thh, trr).!(thh.none && trr.none)

            if (f.exert) {
                Ask(f)(NoExertAction(f, 1 + f.all(Extortion).diff(f.hidden).num))(actions).evening(f)
            }
            else {
                if (f.acted >= 3) {
                    actions = $

                    actions :+= Next.as("End".hl, "Daylight".styled(styles.phase))
                }
                else
                    actions :+= EndTurnSoftAction(f, "Daylight".styled(styles.phase), ForfeitActions(3 - f.acted))

                Ask(f)(actions).daylight(f)
            }

        case NoExertAction(f, _) =>
            Next

        case MischiefAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), MischiefDoneAction(f, Repeat))

        case MischiefMoveAction(f, l) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), MischiefDoneAction(f, Repeat))

        case MischiefPlotMainAction(f, p, l, n) =>
            Ask(f)(l./(MischiefPlotAction(f, p, n, _))).cancel

        case MischiefPlotSetupAction(f, p, l) =>
            Ask(f)(l./(MischiefPlotAction(f, p, -1, _))).cancel

        case MischiefPlotAction(f, p, n, c) =>
            game.highlights :+= PlaceHighlight($(c))

            if (n == -1) {
                f.reserve --> Raven --> c

                if (game.options.has(SetupTypeHomelands)) {
                    game.homelands :+= c

                    f.log("started in", c)
                }
            }
            else
                f.from(c) --> n.times(Raven) --> game.recycle

            f.placed += 1

            if (p == OpenBomb) {
                f.reserve --> Bomb --> c

                f.log("placed a", p.of(f), "in", c, (n > 0).??("with"), n.times(Raven.of(f)).comma)
            }
            else {
                f.reserve --> HiddenPlot --> c

                f.secret += (c -> p)

                f.log("started a", HiddenPlot.of(f), "in", c, (n > 0).??("with"), n.times(Raven.of(f)).comma)
            }

            if (f.placed == 0)
                FactionSetupAction(f)
            else
                MischiefDoneAction(f, Repeat)

        case MischiefTrickMainAction(f, h, r) =>
            Ask(f)
                .each(h.combinations(2).$)(ab => MischiefTrickAction(f, ab(0), ab(1), f.secret(ab(0)), f.secret(ab(1))))
                .each(r.combinations(2).$)(ab => MischiefTrickAction(f, ab(0), ab(1), f.at(ab(0)).of[RealPlot].only, f.at(ab(1)).of[RealPlot].only))
                .cancel

        case MischiefTrickAction(f, a, b, _, _) =>
            game.highlights :+= PlaceHighlight($(a, b))

            if (f.hidden.has(a) && f.hidden.has(b)) {
                val pa = f.secret(a)
                val pb = f.secret(b)

                f.secret += (a -> pb)
                f.secret += (b -> pa)
            }
            else {
                val pa = f.from(a) --> f.at(a).of[RealPlot].only
                val pb = f.from(b) --> f.at(b).of[RealPlot].only

                pa --> b
                pb --> a
            }

            f.log("swapped plots in", a, "and", b)

            MischiefDoneAction(f, Repeat)

        case MischiefDoneAction(f, then) =>
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

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
