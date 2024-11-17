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


case class CheckDominanceAction(f : Faction, then : ForcedAction) extends ForcedAction with JumpTo {
    def desc(l : $[Action]) = f.elem ~ " turn " ~ "#".hh ~ l.of[CheckDominanceAction].num.hl ~ " (round " ~ (l.of[CheckDominanceAction].%(_.f == f).num).hh ~ ")"
}

case class TakeDominanceMainAction(self : Faction, l : $[Dominance], then : ForcedAction) extends BaseAction("Dominance")("Take a", "Dominance".hh, "from discard") with Soft
case class TakeDominanceAction(self : Faction, d : Dominance, then : ForcedAction) extends BaseAction("Take a", "Dominance".hh, "from discard")(d.img) with ViewCard with Soft
case class TakeDominanceTakeAction(self : Faction, d : Dominance, then : ForcedAction) extends ForcedAction

case class ActivateDominanceMainAction(self : Faction, l : $[Dominance], then : ForcedAction) extends BaseAction("Dominance")("Activate", "Dominance".hh) with Soft
case class ActivateDominanceAction(self : Faction, d : Dominance, then : ForcedAction) extends BaseAction("Activate", "Dominance".hh)(d.img) with ViewCard

case class FormCoalitionMainAction(self : Hero, cc: $[Faction], l : $[Dominance], then : ForcedAction) extends BaseAction("Dominance")("Form", "Coalition".hh) with Soft
case class FormCoalitionFactionAction(self : Hero, x : Faction, l : $[Dominance], then : ForcedAction) extends BaseAction(implicit g => (self.vp < 10).?("Warning: " ~ "rules".hl ~ " might not allow to form a coalition with less than " ~ 10.vp ~ "," ~ Break ~ "check with other players before you continue" ~ Break)./(_.styled(xstyles.error)), "Form Coalition with")(x) with Soft
case class FormCoalitionAction(self : Hero, x : Faction, d : Dominance, then : ForcedAction) extends BaseAction("Form", "Coalition".hh, "with", x, " using")(d.img) with ViewCard

case class TotalWarAction(f : WarriorFaction, then : TopLevelAction) extends ForcedAction
case class TotalWarBattleAction(f : WarriorFaction, then : TopLevelAction) extends ForcedAction
case class TotalWarRecruitAction(self : WarriorFaction, c : Region, n : Int, then : TopLevelAction) extends BaseAction("Total War".hl, hrf.elem.MDash, n.vp, hrf.elem.MDash, "Recruit in")(c)
case class TotalWarDoneAction(f : WarriorFaction, then : TopLevelAction) extends ForcedAction

case class BattleTotalWar(n : Int) extends Message {
    def elem(implicit game : Game) = " with " ~ "Total War".hl ~ " " ~ n.vp
}


object DominanceExpansion extends MandatoryExpansion {
    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (f.dominance.none) {
            f @@ {
                case f : Hero =>
                    if (factions.num >= 4)
                        f.hand.$.of[Dominance].some.foreach { l =>
                            factions.notOf[Hero].%(_.dominance.none).some.foreach { ff =>
                                + FormCoalitionMainAction(f, ff.%(_.vp == ff./(_.vp).min), l, Repeat)
                            }
                        }
                case _ =>
                    if (f.dominance.none) {
                        val l =
                            (f.vp >= 10).??(f.hand.$.of[BaseSuitDominance]) ++
                            (f.vp >= 10).??(f.hand.$.of[CornersDominance]) ++
                            (clearings.%(_.asset.use(a => a.matches(Frog) && FoxRabbitMouse.exists(a.matches).not)).num * 2 >= clearings.num).??(f.hand.$.of[FrogDominance.type])


                        if (l.any)
                            + ActivateDominanceMainAction(f, l, Repeat)
                    }
            }
        }

        if (game.dominances.any) {
            game.dominances.%(d => f.hand.%(_.matches(d.suit)).any).some.foreach { l =>
                + TakeDominanceMainAction(f, l, Repeat)
            }
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // CHECK
        case CheckDominanceAction(f, then) =>
            f.dominance @@ {
                case Some(CornersDominance(_)) if board.diagonals.%((a, b) => f.rules(a) && f.rules(b)).any =>
                    f.vp = 999
                    f.log("achieved", "Bird Dominance".styled(Bird))
                case Some(BaseSuitDominance(s)) if clearings.%(_.asset.matches(s)).%(f.rules).num >= 3 =>
                    f.vp = 999
                    f.log("achieved", (s.name + " Dominance").styled(s))
                case Some(FrogDominance) if clearings.%(_.asset.matches(Frog)).use(l => l.%(f.rules).num > f.enemies.notOf[Hireling]./(e => l.%(e.rules).num).max) =>
                    f.vp = 999
                    f.log("achieved", ("Frog Dominance").styled(Frog))
                case _ =>
            }

            then

        // TOTAL WAR
        case a : TopLevelAction if options.has(TotalWarDominance) =>
            val twf = factions.of[WarriorFaction].%(_.dominance.any).%(_.vp > 0)

            if (twf.any)
                TotalWarAction(twf(0), a)
            else
                UnknownContinue

        case TotalWarAction(f, then) if f.vp <= 0 =>
            then

        case TotalWarAction(f, then) if f.pool(f.warrior).not =>
            TotalWarBattleAction(f, then)

        case TotalWarAction(f, then) if f.presence.exists(f.canPlace).not =>
            TotalWarBattleAction(f, then)

        case TotalWarAction(f, then) =>
            Ask(f).each(f.presence.%(f.canPlace))(TotalWarRecruitAction(f, _, f.vp, then)).needOk

        case TotalWarBattleAction(f, then) =>
            val l = clearings.%(f.canAttackIn)

            if (l.any)
                BattleInitAction(f, f, BattleTotalWar(f.vp), l, $, TotalWarDoneAction(f, then))
            else {
                f.vp = 0

                then
            }

        case TotalWarRecruitAction(f, c, _, then) =>
            f.reserve --> f.warrior --> c
            f.log("recruited", f.warrior.of(f), "in", c, "with", "Total War".hl)
            TotalWarDoneAction(f, then)

        case TotalWarDoneAction(f, then) =>
            f.vp -= 1
            TotalWarAction(f, then)

        // ACTIVATION
        case ActivateDominanceMainAction(f, l, then) =>
            Ask(f).each(l)(ActivateDominanceAction(f, _, then)).cancel

        case ActivateDominanceAction(f, d, then) =>
            f.hand --> d --> f.stuck
            f.dominance = Some(d)
            f.vp = 0
            f.log("activated", d)
            then

        case FormCoalitionMainAction(f, cc, l, then) =>
            Ask(f).each(cc)(FormCoalitionFactionAction(f, _, l, then)).cancel

        case FormCoalitionFactionAction(f, x, l, then) =>
            Ask(f).each(l)(FormCoalitionAction(f, x, _, then)).cancel

        case FormCoalitionAction(f, x, d, then) =>
            f.hand --> d --> f.stuck
            f.dominance = Some(d)
            f.vp = -999
            f.coalition = Some(x)
            f.log("formed", "Coalition".hh, "with", x, "using", d)
            if (f.hostile(x)) {
                f.attitude += x -> Indifferent
                x.log("became", Indifferent, "to", f)
            }
            then

        // DOMINANCE CARDS
        case TakeDominanceMainAction(f, l, then) =>
            Ask(f)(l./(TakeDominanceAction(f, _, then))).cancel

        case TakeDominanceAction(f, d, then) =>
            OptionalDiscardCardAction(f, TakeCard(d), d.suit, TakeDominanceTakeAction(f, d, then))

        case TakeDominanceTakeAction(f, d, then) =>
            f.drawn --> discard(f, "to take", d)
            game.dominances --> d --> f.hand
            then

        case _ => UnknownContinue
    }

}
