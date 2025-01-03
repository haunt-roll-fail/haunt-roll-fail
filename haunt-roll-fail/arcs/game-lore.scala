package arcs
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

import hrf.tracker4._
import hrf.tracker4.implicits._
import hrf.elem._

import arcs.elem._


abstract class Lore(val id : String, val name : String) extends Record with Effect with Elementary {
    def img = Image(id, styles.card)
    def elem = name.styled(styles.title).hl
}

case object ToolPriests       extends Lore("lore01", "Tool Priests")
case object SprinterDrives    extends Lore("lore03", "Sprinter Drives")
case object MirrorPlating     extends Lore("lore04", "Mirror Plating")
case object HiddenHarbors     extends Lore("lore05", "Hidden Harbors")
case object SignalBreaker     extends Lore("lore06", "Signal Breaker")
case object RepairDrones      extends Lore("lore07", "Repair Drones")
case object GatePorts         extends Lore("lore08", "Gate Ports")
case object CloudCities       extends Lore("lore09", "Cloud Cities")
case object LivingStructures  extends Lore("lore10", "Living Structures")
case object GateStations      extends Lore("lore11", "Gate Stations")
case object RailgunArrays     extends Lore("lore12", "Railgun Arrays")
case object SeekerTorpedoes   extends Lore("lore14", "Seeker Torpedoes")
case object AncientHoldings   extends Lore("lore13", "Ancient Holdings")
case object SurvivalOverrides extends Lore("lore18", "Survival Overrides")
case object EmpathsVision     extends Lore("lore19", "Empath's Vision")
case object KeepersTrust      extends Lore("lore21", "Keeper's Trust")
case object WarlordsCruelty   extends Lore("lore23", "Warlord's Cruelty")
case object TyrantsEgo        extends Lore("lore25", "Tyrant's Ego")
case object TycoonsCharm      extends Lore("lore28", "Tycoon's Charm")

object Lores {
    def all = $(
        MirrorPlating     ,
        HiddenHarbors     ,
        SignalBreaker     ,
        RepairDrones      ,
        LivingStructures  ,
        RailgunArrays     ,
        SeekerTorpedoes   ,
        AncientHoldings   ,
        SurvivalOverrides ,
        EmpathsVision     ,
        KeepersTrust      ,
        WarlordsCruelty   ,
        TyrantsEgo        ,
        TycoonsCharm      ,
    )

    def preset1 = $(MirrorPlating, HiddenHarbors, WarlordsCruelty, AncientHoldings, SignalBreaker)
    def preset2 = $(LivingStructures, SurvivalOverrides, GatePorts, SprinterDrives, ToolPriests)
    def preset3 = $(RepairDrones, RailgunArrays, SeekerTorpedoes, CloudCities, GateStations)
    def preset4 = all.diff(preset1).diff(preset2).diff(preset3)
}

case class DiscardLoreCardAction(self : Faction, c : Lore, then : ForcedAction) extends ForcedAction

case class NurtureMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft

case class PruneMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class PruneCityAction(self : Faction, cost : Cost, s : System, u : Figure, then : ForcedAction) extends ForcedAction
case class PruneStarportAction(self : Faction, cost : Cost, s : System, u : Figure, then : ForcedAction) extends ForcedAction

case class MartyrMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class MartyrAction(self : Faction, cost : Cost, s : System, u : Figure, t : Figure, then : ForcedAction) extends ForcedAction


object LoreExpansion extends Expansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        case DiscardLoreCardAction(f, c, then) =>
            f.lores :-= c

            f.log("discarded", c)

            then


        // SURVIVAL OVERRIDES
        case MartyrMainAction(f, x, then) =>
            def convert(u : Figure) = game.showFigure(u, u.damaged.??(1))

            Ask(f).group("Martyr".hl, x)
                .some(systems./~(s => f.at(s).ships.fresh.take(1)./(s -> _))) { case (s, u) =>
                    f.others./~(_.at(s).ships./(t => MartyrAction(f, x, s, u, t, then).as(convert(t))("Martyr".hh, "in", s)))
                }
                .cancel

        case MartyrAction(f, x, s, u, t, then) =>
            f.pay(x)

            t --> f.trophies

            u --> f.reserve

            f.log("martyred", t, "in", s, "with", u)

            then

        // LIVING STRUCTURES
        case NurtureMainAction(f, x, then) =>
            TaxMainAction(f, x, |(LivingStructures), then)

        case PruneMainAction(f, x, then) =>
            val prefix = f.short + "-"
            def suffix(s : System) = f.rivals.exists(_.rules(s)).??("-damaged")

            Ask(f)
                .group("Prune".hl)
                .some(systems)(s => f.pool(Starport).??(f.at(s).cities)./(u => PruneCityAction(f, x, s, u, then).as(City.of(f), Image(prefix + "city" + f.damaged.has(u).??("-damaged"), styles.qbuilding), "in", s)))
                .some(systems)(s => f.pool(City).??(f.at(s).starports)./(u => PruneStarportAction(f, x, s, u, then).as(Starport.of(f), Image(prefix + "starport" + f.damaged.has(u).??("-damaged"), styles.qbuilding), "in", s)))
                .cancel

        case PruneCityAction(f, x, s, u, then) =>
            f.pay(x)

            val u2 = f.reserve --> Starport.of(f)

            if (f.damaged.has(u)) {
                f.damaged :+= u2
                f.damaged :-= u
            }

            if (game.unslotted.has(u)) {
                game.unslotted :+= u2
                game.unslotted :-= u
            }

            u --> f.reserve

            u2 --> s

            f.log("pruned", u, "in", s, x)

            if (f.pooled(City) > 2)
                f.adjust = true

            AdjustResourcesAction(then)

        case PruneStarportAction(f, x, s, u, then) =>
            f.pay(x)

            val u2 = f.reserve --> City.of(f)

            if (f.damaged.has(u)) {
                f.damaged :+= u2
                f.damaged :-= u
            }

            if (game.unslotted.has(u)) {
                game.unslotted :+= u2
                game.unslotted :-= u
            }

            u --> f.reserve

            u2 --> s

            f.log("pruned", u, "in", s, x)

            then

        case _ => UnknownContinue
    }
}
