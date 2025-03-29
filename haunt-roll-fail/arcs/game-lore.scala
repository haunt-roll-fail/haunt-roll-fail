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


abstract class Lore(val id : String, val name : String) extends CourtCard with Record with Effect with Elementary {
}

case object ToolPriests       extends Lore("lore01", "Tool Priests")
case object GalacticRifles    extends Lore("lore02", "Galactic Rifles")
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
case object AncientHoldings   extends Lore("lore13", "Ancient Holdings")
case object SeekerTorpedoes   extends Lore("lore14", "Seeker Torpedoes")
case object PredictiveSensors extends Lore("lore15", "Predictive Sensors")
case object ForceBeams        extends Lore("lore16", "Force Beams")
case object RaiderExosuits    extends Lore("lore17", "Raider Exosuits")
case object SurvivalOverrides extends Lore("lore18", "Survival Overrides")
case object EmpathsVision     extends Lore("lore19", "Empath's Vision")
case object EmpathsBond       extends Lore("lore20", "Empath's Bond")
case object KeepersTrust      extends Lore("lore21", "Keeper's Trust")
case object KeepersSolidarity extends Lore("lore22", "Keeper's Solidarity")
case object WarlordsCruelty   extends Lore("lore23", "Warlord's Cruelty")
case object WarlordsTerror    extends Lore("lore24", "Warlord's Terror")
case object TyrantsEgo        extends Lore("lore25", "Tyrant's Ego")
case object TyrantsAuthority  extends Lore("lore26", "Tyrant's Authority")
case object TycoonsAmbition   extends Lore("lore27", "Tycoon's Ambition")
case object TycoonsCharm      extends Lore("lore28", "Tycoon's Charm")
case object GuildLoyaltyLL      extends Lore("lore29", "Guild Loyalty")
case object CatapultOverdriveLL extends Lore("lore30", "Catapult Overdrive")

object Lores {
    def all = $(
        ToolPriests,
        GalacticRifles,
        SprinterDrives,
        MirrorPlating,
        HiddenHarbors,
        SignalBreaker,
        RepairDrones,
        GatePorts,
        CloudCities,
        LivingStructures,
        GateStations,
        RailgunArrays,
        AncientHoldings,
        SeekerTorpedoes,
        PredictiveSensors,
        ForceBeams,
        RaiderExosuits,
        SurvivalOverrides,
        EmpathsVision,
        EmpathsBond,
        KeepersTrust,
        KeepersSolidarity,
        WarlordsCruelty,
        WarlordsTerror,
        TyrantsEgo,
        TyrantsAuthority,
        TycoonsAmbition,
        TycoonsCharm,
        GuildLoyaltyLL,
        CatapultOverdriveLL,
    )

    def done = $(
        ToolPriests,
        GalacticRifles,
        SprinterDrives,
        MirrorPlating,
        HiddenHarbors,
        SignalBreaker,
        RepairDrones,
        GatePorts,
        CloudCities,
        LivingStructures,
        GateStations,
        RailgunArrays,
        AncientHoldings,
        SeekerTorpedoes,
        PredictiveSensors,
        ForceBeams,
        RaiderExosuits,
        SurvivalOverrides,
        WarlordsCruelty,
        GuildLoyaltyLL,
        CatapultOverdriveLL
    )

    def preset1 = $(SprinterDrives, PredictiveSensors, ForceBeams, CatapultOverdriveLL, SurvivalOverrides) // Movement
    def preset2 = $(LivingStructures, CloudCities, GateStations, GatePorts, ToolPriests) // Buildings
    def preset3 = $(SignalBreaker, RailgunArrays, SeekerTorpedoes, MirrorPlating, GalacticRifles) // Battle
    def preset4 = $(GuildLoyaltyLL, AncientHoldings, HiddenHarbors, RaiderExosuits, RepairDrones) // Archivist
    def preset5 = $(WarlordsTerror) // Ambitions
    def preset6 = $(WarlordsCruelty) // Clear Outrage
}

case class DiscardLoreCardAction(self : Faction, c : Lore, then : ForcedAction) extends ForcedAction

case class NurtureMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft

case class PruneMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class PruneCityAction(self : Faction, cost : Cost, s : System, u : Figure, then : ForcedAction) extends ForcedAction
case class PruneStarportAction(self : Faction, cost : Cost, s : System, u : Figure, then : ForcedAction) extends ForcedAction

case class MartyrMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class MartyrAction(self : Faction, cost : Cost, s : System, u : Figure, t : Figure, then : ForcedAction) extends ForcedAction

case class FireRiflesMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class FireRiflesFromAction(self : Faction, cost : Cost, s : System, then : ForcedAction) extends ForcedAction with Soft
case class FireRiflesAction(self : Faction, cost : Cost, s : System, e : Faction, t : System, then : ForcedAction) extends ForcedAction
case class FireRiflesRolledAction(self : Faction, e : Faction, t : System, rolled : Rolled, then : ForcedAction) extends RolledAction[$[BattleResult]]

case class PredictiveSensorsAction(self : Faction, s : System, u : Figure, t : System, then : ForcedAction) extends ForcedAction

case class GuideMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class GuideFromAction(self : Faction, cost : Cost, s : System, l : $[System], then : ForcedAction) extends ForcedAction with Soft
case class GuideToAction(self : Faction, cost : Cost, s : System, l : $[System], then : ForcedAction) extends ForcedAction with Soft
case class GuidePathAction(self : Faction, cost : Cost, s : System, t : System, cancel : Boolean, then : ForcedAction) extends ForcedAction with Soft
case class GuideAction(self : Faction, cost : Cost, s : System, t : System, l : $[Figure], then : ForcedAction) extends ForcedAction

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
                    f.others./~(_.at(s).ships./(t =>
                        MartyrAction(f, x, s, u, t, then).as(convert(t))("Martyr".hh, "in", s)
                            .!(f.regent && t.faction.regent && Empire.at(s).any, "truce")
                    ))
                }
                .cancel

        case MartyrAction(f, x, s, u, t, then) =>
            f.pay(x)

            t --> f.trophies

            t.faction.damaged :-= t

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

        // GALACTIC RIFLES
        case FireRiflesMainAction(f, x, then) =>
            Ask(f).group("Fire Rifles".hl, x, "from")
                .each(systems.%(s => f.at(s).ships.fresh.any)) { s =>
                    FireRiflesFromAction(f, x, s, then).as(s)
                }
                .cancel

        case FireRiflesFromAction(f, x, s, then) =>
            Ask(f).group("Fire Rifles".hl, x, "from")
                .some(board.connected(s))(t => factions.but(f).%(_.at(t).any)./(e => FireRiflesAction(f, x, s, e, t, then).as(e, "in", t)))
                .cancel

        case FireRiflesAction(f, x, s, e, t, then) =>
            f.pay(x)

            f.log("fired rifles from", s, "at", e, "in", t)

            Roll[$[BattleResult]](f.at(s).ships.fresh.num.times(Skirmish.die), l => FireRiflesRolledAction(f, e, t, l, then))

        case FireRiflesRolledAction(f, e, t, l, then) =>
            f.log("rolled", l./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)))

            AssignHitsAction(f, t, f, e, e.at(t), l.flatten.count(HitShip), 0, 0, |(GalacticRifles), $, then)

        // PREDICTIVE SENSORS
        case PredictiveSensorsAction(f, s, u, t, then) =>
            u --> t

            f.log("moved", u, "from", s, "to", t, "with", PredictiveSensors)

            then

        // FORCE BEAMS
        case GuideMainAction(f, x, then) =>
            val l = systems.%(s => f.at(s).starports.fresh.any)

            Ask(f)
                .group("Guide from").each(l)(s => GuideFromAction(f, x, s, board.connected(s), then).as(s).!!!)
                .group("Guide to").each(l)(s => GuideToAction(f, x, s, board.connected(s), then).as(s).!!!)
                .cancel

        case GuideFromAction(f, x, s, l, then) =>
            Ask(f).group("Guide from", s, "to")
                .each(l)(t => GuidePathAction(f, x, s, t, true, then).as(t).!!!)
                .cancel

        case GuideToAction(f, x, t, l, then) =>
            Ask(f).group("Guide to", t, "from")
                .each(l)(s => GuidePathAction(f, x, s, t, true, then).as(s).!!!)
                .cancel

        case GuidePathAction(f, x, s, t, cancel, then) =>
            val l = factions./~(_.at(s).ships)

            val combinations = l.groupBy(_.faction).$./~{ (f, l) =>
                val n = l.num
                val (damaged, fresh) = l.partition(f.damaged.has)
                val combinations = 0.to(n)./~(k => max(0, k - fresh.num).to(min(k, damaged.num))./(i => fresh.take(k - i) ++ damaged.take(i)))
                combinations.%(_.any)
            }

            implicit def convert(u : Figure) = game.showFigure(u, u.faction.damaged.has(u).??(1))

            Ask(f).group("Move from", s, "to", t)
                .each(combinations)(l => GuideAction(f, x, s, t, l, then).as(l./(u => convert(u))))
                .cancelIf(cancel)
                .doneIf(cancel.not)(then)

        case GuideAction(f, x, s, t, l, then) =>
            f.pay(x)

            f.log("guided", l.comma, "from", s, "to", t, x)

            l --> t

            GuidePathAction(f, AlreadyPaid, s, t, false, then)

        case _ => UnknownContinue
    }
}
