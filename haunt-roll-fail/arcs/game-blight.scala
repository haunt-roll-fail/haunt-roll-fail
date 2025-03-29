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


trait Tough { self : Piece => }

case object Blight extends Piece with Tough

case object Blights extends Color
case object Empire extends Color
case object Free extends Color


abstract class ImperialCard(val id : String, val name : String) extends CourtCard
case object ImperialCouncilInSession extends ImperialCard("aid01a", "Imperial Council")
case object ImperialCouncilDecided extends ImperialCard("aid01b", "Imperial Council Decided")


abstract class Fate(val name : String, val id : String, val act : Int) extends Record with Elementary {
    def img = Image(id, styles.fateCard)
    def elem = (id == "no-fate").?(name.styled(styles.title).hh.spn(styles.notDoneYet)).|(name.styled(styles.title).hl)
}

case object Steward       extends Fate("Steward",            "fate01", 1)
case object Founder       extends Fate("Founder",            "fate02", 1)
case object Magnate       extends Fate("Magnate",            "fate03", 1)
case object Advocate      extends Fate("Advocate",           "fate04", 1)
case object Caretaker     extends Fate("Caretaker",          "fate05", 1)
case object Partisan      extends Fate("Partisan",           "fate06", 1)
case object Admiral       extends Fate("Admiral",            "fate07", 1)
case object Believer      extends Fate("Believer",           "fate08", 1)

case object Pathfinder    extends Fate("Pathfinder",         "fate09", 2)
case object Hegemon       extends Fate("Hegemon",            "fate10", 2)
case object PlanetBreaker extends Fate("Planet Breaker",     "fate11", 2)
case object Pirate        extends Fate("Pirate",             "fate12", 2)
case object BlightSpeaker extends Fate("Blight Speaker",     "fate13", 2)
case object Pacifist      extends Fate("Pacifist",           "fate14", 2)
case object Peacekeeper   extends Fate("Peacekeeper",        "fate15", 2)
case object Warden        extends Fate("Warden",             "fate16", 2)

case object Overlord      extends Fate("Overlord",           "no-fate", 3)
case object Survivalist   extends Fate("Survivalist",        "no-fate", 3)
case object Redeemer      extends Fate("Redeemer",           "no-fate", 3)
case object Guardian      extends Fate("Guardian",           "no-fate", 3)
case object Naturalist    extends Fate("Naturalist",         "no-fate", 3)
case object GateWraith    extends Fate("Gate Wraith",        "no-fate", 3)
case object Conspirator   extends Fate("Conspirator",        "no-fate", 3)
case object Judge         extends Fate("Judge",              "no-fate", 3)

object Fates {
    val act1 = $(
        Steward       ,
        Founder       ,
        Magnate       ,
        Advocate      ,
        Caretaker     ,
        Partisan      ,
        Admiral       ,
        Believer      ,
    )

    val act2 = $(
        Pathfinder    ,
        Hegemon       ,
        PlanetBreaker ,
        Pirate        ,
        BlightSpeaker ,
        Pacifist      ,
        Peacekeeper   ,
        Warden        ,
    )

    val act3 = $(
        Overlord      ,
        Survivalist   ,
        Redeemer      ,
        Guardian      ,
        Naturalist    ,
        GateWraith    ,
        Conspirator   ,
        Judge         ,
    )
}


abstract class Edict(val id : String, val name : String) extends Record with Effect with Elementary {
    def img = Image(id, styles.card)
    def elem = name.styled(styles.title).hl
}

abstract class Objective(val id : String, val name : String) extends Record with Effect with Elementary {
    def img = Image(id, styles.card)
    def elem = name.styled(styles.title).hl
}

abstract class Ability(val id : String, val name : String) extends Record with Effect with Elementary {
    def img = Image(id, styles.card)
    def elem = name.styled(styles.title).hl
}

abstract class Law(val id : String, val name : String) extends Record with Effect with Elementary {
    def img = Image(id, styles.card)
    def elem = name.styled(styles.title).hl
}



case object PolicyOfPeace extends Edict("aid04", "A Policy of Peace")
case object PolicyOfEscalation extends Edict("aid05", "A Policy of Escalation")
case object PolicyOfWar extends Edict("aid03", "A Policy of War")

case object CouncilIntrigue   extends VoxEffect("Council Intrigue")
case object DimplomaticFiasco extends VoxEffect("Dimplomatic Fiasco")
case object BlightLooms       extends VoxEffect("Blight Looms")


case object ConsolidateImperialPower extends Objective("f01-01b", "Consolidate Imperial Power")
case object ImperialAuthority extends Lore("f01-02", "Imperial Authority")
case object Dealmakers extends GuildEffect("Dealmakers", Psionic, 999)

case object InspireConfidence extends Objective("f02-01b", "Inspire Confidence")
case object ParadeFleets extends GuildEffect("Parade Fleets", Fuel, 999)

trait GolemType extends Resource
case object WarriorGolem extends GolemType
case object ProtectorGolem extends GolemType
case object SeekerGolem extends GolemType
case object HarvesterGolem extends GolemType

case object FindGolems extends Objective("f05-01b", "Find Golems")
case object GolemBeacons extends Lore("f05-02", "Golem Beacons")
case object GolemHearth extends Lore("f05-03", "Golem Hearth")
case object StoneSpeakers extends GuildEffect("Stone-Speakers", Material, 999)

case object ProveYourself extends Objective("f07-01b", "Prove Yourself")
case object UseImperialFoundries extends Edict("f07-03", "Use Imperial Foundries")
case object ImperialOfficers extends GuildEffect("Imperial Officers", Weapon, 999)


object BlightCards {
    def court = $(
        GuildCard("cc01", MiningInterest    ),
        GuildCard("cc02", ConstructionUnion ),
        GuildCard("cc03", Gatekeepers       ),
        GuildCard("cc04", ShippingInterest  ),
        GuildCard("cc05", PrisonWardens     ),
        GuildCard("cc06", ArmsUnion         ),
        GuildCard("cc07", LatticeSpies      ),
        GuildCard("cc08", SilverTongues     ),
        GuildCard("cc09", SwornGuardians    ),
        GuildCard("cc10", ElderBroker       ),
        VoxCard("cc11", PopulistDemands   ),
        VoxCard("cc12", CouncilIntrigue   ),
        VoxCard("cc13", DimplomaticFiasco ),
        VoxCard("cc14", SongOfFreedom     ),
        VoxCard("cc15", BlightLooms       ),
    )

    def sidedeck = $(
        ImperialCouncilInSession,
        ImperialCouncilDecided,
    )

    def fates : Map[Fate, $[CourtCard]] = Map(
        Steward   -> $(
            GuildCard("f01-03", Dealmakers),
        ),
        Founder   -> $(
            GuildCard("f02-02", ParadeFleets),
        ),
        Magnate   -> $(),
        Advocate  -> $(),
        Caretaker -> $(
            GolemBeacons,
            GolemHearth,
            GuildCard("f05-04", StoneSpeakers),
        ),
        Partisan  -> $(),
        Admiral   -> $(
            GuildCard("f07-02", ImperialOfficers),
        ),
        Believer  -> $(),
    )
}


class NegotiationState(implicit val game : Game) {
    val factions : Map[Faction, NegotiationFactionState] = game.factions./(f => f -> new NegotiationFactionState(f)).toMap
    val market : $[CourtCard] = game.market.$

    def listItems(_from : Faction, _to : Faction) : $[NegotiationItem] = {
        implicit val ungame1 : Game = null
        implicit val ungame2 : Game = null

        val result = new collection.mutable.ListBuffer[NegotiationItem]

        val from = factions(_from)
        val to = factions(_to)

        if (from.primus && to.regent.not)
            result += NegotiationEmpireInvitation(_from, _to)

        val lastShip = from.systems.values./(s => s.shipsFresh + s.shipsDamaged).sum <= 1
        val lastCity = from.systems.values./(s => s.citiesFresh + s.citiesDamaged).sum <= 1
        val lastStarport = from.systems.values./(s => s.starportsFresh + s.starportsDamaged).sum <= 1

        val fullShip = to.reserve.ships == 0
        val fullCity = to.reserve.cities == 0
        val fullStarport = to.reserve.starports == 0

        if (to.reserve.ships > 0 && from.systems.values./(s => s.shipsFresh + s.shipsDamaged).sum > 1) {
            from.systems.foreach { case (s, t) =>
                if (t.shipsFresh > 0)
                    result += NegotiationPiece(_from, _to, s, Ship, false)

                if (t.shipsDamaged > 0)
                    result += NegotiationPiece(_from, _to, s, Ship, true)
            }
        }

        if (to.reserve.cities > 0 && from.systems.values./(s => s.citiesFresh + s.citiesDamaged).sum > 1) {
            from.systems.foreach { case (s, t) =>
                if (t.shipsFresh == 0 && t.shipsDamaged == 0) {
                    if (t.citiesFresh > 0)
                        result += NegotiationPiece(_from, _to, s, City, false)

                    if (t.citiesDamaged > 0)
                        result += NegotiationPiece(_from, _to, s, City, true)
                }
            }
        }

        if (to.reserve.starports > 0 && from.systems.values./(s => s.starportsFresh + s.starportsDamaged).sum > 1) {
            from.systems.foreach { case (s, t) =>
                if (t.shipsFresh == 0 && t.shipsDamaged == 0) {
                    if (t.starportsFresh > 0)
                        result += NegotiationPiece(_from, _to, s, Starport, false)

                    if (t.starportsDamaged > 0)
                        result += NegotiationPiece(_from, _to, s, Starport, true)
                }
            }
        }

        if (to.reserve.agents > 0) {
            market.foreach { c =>
                if (from.market(c).agents > 0)
                    result += NegotiationAgent(_from, _to, c)
            }
        }

        if (from.reserve.agents > 0)
            result += NegotiationFavor(_from, _to, _from)

        from.favors.foreach { case (e, t) =>
            if (t.favors > 0)
                result += NegotiationFavor(_from, _to, e)
        }

        from.captives.foreach { case (e, t) =>
            if (t.captives > 0)
                result += NegotiationCaptive(_from, _to, e)
        }

        from.trophies.foreach { case (c, t) =>
            if (t.ships > 0)
                result += NegotiationTrophy(_from, _to, c, Ship)

            if (t.cities > 0)
                result += NegotiationTrophy(_from, _to, c, City)

            if (t.starports > 0)
                result += NegotiationTrophy(_from, _to, c, Starport)

            if (t.agents > 0)
                result += NegotiationTrophy(_from, _to, c, Agent)

            if (t.blights > 0)
                result += NegotiationTrophy(_from, _to, c, Blight)

        }

        from.resources.distinct.foreach { case (r, i) =>
            result += NegotiationResource(_from, _to, r, |(i))
        }

        result.$
    }

    def applyItem(n : NegotiationItem) {
        n @@ {
            case NegotiationPiece(from, to, s, Ship, false) =>
                factions(from).systems(s).shipsFresh -= 1
                factions(to  ).systems(s).shipsFresh += 1

            case NegotiationPiece(from, to, s, Ship, true) =>
                factions(from).systems(s).shipsDamaged -= 1
                factions(to  ).systems(s).shipsDamaged += 1

            case NegotiationPiece(from, to, s, City, false) =>
                factions(from).systems(s).citiesFresh -= 1
                factions(to  ).systems(s).citiesFresh += 1

            case NegotiationPiece(from, to, s, City, true) =>
                factions(from).systems(s).citiesDamaged -= 1
                factions(to  ).systems(s).citiesDamaged += 1

            case NegotiationPiece(from, to, s, Starport, false) =>
                factions(from).systems(s).starportsFresh -= 1
                factions(to  ).systems(s).starportsFresh += 1

            case NegotiationPiece(from, to, s, Starport, true) =>
                factions(from).systems(s).starportsDamaged -= 1
                factions(to  ).systems(s).starportsDamaged += 1

            case NegotiationAgent(from, to, c) =>
                factions(from).market(c).agents -= 1
                factions(to  ).market(c).agents += 1

            case NegotiationFavor(from, to, faction) =>
                if (faction == from)
                    factions(from).reserve.agents -= 1
                else
                    factions(from).favors(faction).favors -= 1

                if (faction == to)
                    factions(to  ).reserve.agents += 1
                else
                    factions(to  ).favors(faction).favors += 1

            case NegotiationCaptive(from, to, faction) =>
                factions(from).captives(faction).captives -= 1

                if (faction == to)
                    factions(to  ).reserve.agents += 1
                else
                    factions(to  ).captives(faction).captives += 1

            case NegotiationTrophy(from, to, faction, Ship) =>
                factions(from).trophies(faction).ships -= 1

                if (faction == to)
                    factions(to  ).reserve.ships += 1
                else
                    factions(to  ).trophies(faction).ships += 1

            case NegotiationTrophy(from, to, faction, City) =>
                factions(from).trophies(faction).cities -= 1

                if (faction == to)
                    factions(to  ).reserve.cities += 1
                else
                    factions(to  ).trophies(faction).cities += 1

            case NegotiationTrophy(from, to, faction, Starport) =>
                factions(from).trophies(faction).starports -= 1

                if (faction == to)
                    factions(to  ).reserve.starports += 1
                else
                    factions(to  ).trophies(faction).starports += 1

            case NegotiationTrophy(from, to, faction, Agent) =>
                factions(from).trophies(faction).agents -= 1

                if (faction == to)
                    factions(to  ).reserve.agents += 1
                else
                    factions(to  ).trophies(faction).agents += 1

            case NegotiationTrophy(from, to, faction, Blight) =>
                factions(from).trophies(faction).blights -= 1
                factions(to  ).trophies(faction).blights += 1

            case NegotiationResource(from, to, resource, lock) =>
                factions(from).resources :-= resource -> lock.|(0)

                factions(to  ).resources :+= resource -> lock.|(0)

            case NegotiationEmpireInvitation(from, to) =>
                factions(to  ).regent = true
        }
    }
}

class NegotiationFactionState(self : Faction)(implicit val game : Game) {
    var regent : Boolean = self.regent
    var primus : Boolean = self.primus
    val reserve : NegotiationFactionReserveState = new NegotiationFactionReserveState(self.pooled(Ship), self.pooled(City), self.pooled(Starport), self.pooled(Agent))
    val systems : Map[System, NegotiationFactionSystemState] = board.systems./(s => s -> self.at(s).use(l =>
        new NegotiationFactionSystemState(l.ships.fresh.num, l.ships.damaged.num, l.cities.fresh.num, l.cities.damaged.num, l.starports.fresh.num, l.starports.damaged.num))).toMap
    val market : Map[CourtCard, NegotiationFactionCourtState] = game.market./(c => c -> new NegotiationFactionCourtState(Influence(c).$.ofc(self).num)).toMap
    val favors : Map[Faction, NegotiationFactionFavorsState] = factions.but(self)./(f => f -> new NegotiationFactionFavorsState(Favors(self).$.ofc(f).num)).toMap
    val captives : Map[Faction, NegotiationFactionCaptivesState] = factions.but(self)./(f => f -> new NegotiationFactionCaptivesState(Captives(self).$.ofc(f).num)).toMap
    val trophies : Map[Color, NegotiationFactionTrophiesState] = colors.but(self)./(f => f -> Trophies(self).$.ofc(f).use(l =>
        new NegotiationFactionTrophiesState(l.ships.num, l.cities.num, l.starports.num, l.agents.num, l.blights.num))).toMap
    var resources : $[(Resource, Int)] = self.resKeys
}

class NegotiationFactionSystemState(var shipsFresh : Int, var shipsDamaged : Int, var citiesFresh : Int, var citiesDamaged : Int, var starportsFresh : Int, var starportsDamaged : Int)
class NegotiationFactionCourtState(var agents : Int)
class NegotiationFactionFavorsState(var favors : Int)
class NegotiationFactionCaptivesState(var captives : Int)
class NegotiationFactionReserveState(var ships : Int, var cities : Int, var starports : Int, var agents : Int)
class NegotiationFactionTrophiesState(var ships : Int, var cities : Int, var starports : Int, var agents : Int, var blights : Int)


trait NegotiationItem extends Record with GameElementary {
    def from : Faction
    def to : Faction
    def cost : Int = 1
    def elem(implicit game : Game) : Elem
}

case class NegotiationPiece(from : Faction, to : Faction, s : System, p : Piece, damaged : Boolean) extends NegotiationItem { def elem(implicit game : Game) = game.desc(damaged.?("Damaged"), p.of(from), "in", s) }
case class NegotiationAgent(from : Faction, to : Faction, c : CourtCard) extends NegotiationItem { def elem(implicit game : Game) = game.desc(Agent.of(from), "on", c) }
case class NegotiationFavor(from : Faction, to : Faction, faction : Faction) extends NegotiationItem { def elem(implicit game : Game) = game.desc("Favor from", faction) }
case class NegotiationCaptive(from : Faction, to : Faction, faction : Faction) extends NegotiationItem { def elem(implicit game : Game) = game.desc("Captive of", faction) }
case class NegotiationTrophy(from : Faction, to : Faction, faction : Color, p : Piece) extends NegotiationItem { def elem(implicit game : Game) = game.desc("Trophy", p.of(faction), "of", faction) }
case class NegotiationResource(from : Faction, to : Faction, resource : Resource, lock : |[Int]) extends NegotiationItem { override def cost = lock.|(1) ; def elem(implicit game : Game) = game.desc("Resource", ResourceRef(resource, lock)) }
case class NegotiationEmpireInvitation(from : Faction, to : Faction) extends NegotiationItem { def elem(implicit game : Game) = game.desc("Invitation to the", Empire) }

case class NegotiationDraft(initator : Faction, index : Int, parties : $[Faction], approved : $[Faction], items : $[NegotiationItem])


case class DealFatesAction(then : ForcedAction) extends ForcedAction
case class ChooseFatesAction(then : ForcedAction) extends ForcedAction
case class ChooseFateAction(self : Faction, x : Fate, then : ForcedAction) extends ForcedAction
case class FatesShuffledAction(shuffled : $[Fate], then : ForcedAction) extends ShuffledAction[Fate]
case object AddLoreToCourtAction extends ForcedAction
case class LoreToCourtShuffledAction(shuffled : $[Lore]) extends ShuffledAction[Lore]
case object SetupEmpireAction extends ForcedAction
case class EmpireClustersRandomAction(random : $[Int]) extends RandomAction[$[Int]]
case class FreeCitiesRandomAction(random : Symbol) extends RandomAction[Symbol]
case class GovernEdictRandomAction(random : Resource) extends RandomAction[Resource]

case object BuildingsShipsSetupAction extends ForcedAction
case object FatesChosenAction extends ForcedAction
case class FatesSetupAction(l : $[Faction]) extends ForcedAction
case class FateSetupAction(f : Faction, then : ForcedAction) extends ForcedAction
case class FateInitAction(f : Faction, fate : Fate, act : Int, then : ForcedAction) extends ForcedAction
case class PlaceCityAndShipsAction(self : Faction, s : System) extends ForcedAction
case class PlaceStarportAndShipsAction(self : Faction, s : System) extends ForcedAction
case object FillFreeCitiesSetupAction extends ForcedAction

case class ResolveEventAction(self : Faction) extends ForcedAction
case class ResolveCouncilAction(self : Faction) extends ForcedAction

case class StartSummitAction(self : Faction, then : ForcedAction) extends ForcedAction

case class CallToOrderAction(self : Faction, then : ForcedAction) extends ForcedAction
case class ReturnFavorsMainAction(self : Faction, then : ForcedAction) extends ForcedAction
case class PetitionCouncilAction(self : Faction, then : ForcedAction) extends ForcedAction
case class LeaveEmpireAction(self : Faction, then : ForcedAction) extends ForcedAction
case class FirstRegentAction(self : Faction, then : ForcedAction) extends ForcedAction
case class ReviveEmpireAction(self : Faction, then : ForcedAction) extends ForcedAction

case class NegotiationsAction(self : Faction, then : ForcedAction) extends ForcedAction
case class ContinueNegotiationsAction(l : $[Faction], then : ForcedAction) extends ForcedAction
case class AbandonNegotiationsAction(self : Faction, then : ForcedAction) extends ForcedAction
case class NegotiateMainAction(self : Faction, l : $[Faction], then : ForcedAction) extends ForcedAction with Soft
case class NegotiateComposeAction(self : Faction, l : $[Faction], items : $[NegotiationItem], then : ForcedAction) extends ForcedAction with Soft with SelfExplode with SelfValidate {
    def validate(target : Action) = target @@ {
        case NegotiateComposeAction(f, ll, _, _) => self == f && ll.toSet.equals(l.toSet)
        case NegotiateSubmitAction(f, _, _) => self == f && true // TODO
        case _ => false
    }

    def explode(withSoft : Boolean) = {
        $
    }
}
case class NegotiateApproveAction(self : Faction, draft : Int, then : ForcedAction) extends ForcedAction
case class NegotiateReconsiderAction(self : Faction, draft : Int, then : ForcedAction) extends ForcedAction
case class NegotiateRejectAction(self : Faction, draft : Int, then : ForcedAction) extends ForcedAction

case class NegotiateCommitAction(draft : Int, then : ForcedAction) extends ForcedAction

case class NegotiateSubmitAction(self : Faction, items : $[NegotiationItem], then : ForcedAction) extends ForcedAction

case class CouncilBonusAction(self : Faction) extends ForcedAction
case class ChooseEventAction(self : Faction) extends ForcedAction
case class ChooseCrisesAction(self : Faction) extends ForcedAction
case class ChooseEdictsAction(self : Faction) extends ForcedAction

case class RollForEventAction(self : Faction) extends ForcedAction

case class EventRolledAction(random1 : Boolean, random2 : Int, random3 : Symbol) extends Random3Action[Boolean, Int, Symbol]
case object ResolveEventAction extends ForcedAction
case object ResolveEdictsAction extends ForcedAction

case class BlightLoomsSecuredAction(self : Faction, cluster : Int, then : ForcedAction) extends ForcedAction


object BlightExpansion extends Expansion {
    def executeNegotiationItem(i : NegotiationItem)(implicit game : Game) = i @@ {
        case NegotiationPiece(from, to, s, Ship, damaged) =>
            val o = from.at(s).ships.fresh.first
            o --> from.reserve
            val n = to.reserve.$.ships.first
            n --> s

            if (damaged) {
                from.damaged :-= o
                to.damaged :-= n
            }

        case NegotiationPiece(from, to, s, City, damaged) =>
            val o = from.at(s).cities.fresh.first
            o --> from.reserve
            val n = to.reserve.$.cities.first
            n --> s

            if (damaged) {
                from.damaged :-= o
                to.damaged :-= n
            }

        case NegotiationPiece(from, to, s, Starport, damaged) =>
            val o = from.at(s).starports.fresh.first
            o --> from.reserve
            val n = to.reserve.$.starports.first
            n --> s

            if (damaged) {
                from.damaged :-= o
                to.damaged :-= n
            }

        case NegotiationAgent(from, to, c) =>
            Influence(c) --> Agent.of(from) --> to.reserve
            to.reserve --> Agent.of(to) --> Influence(c)

        case NegotiationFavor(from, to, faction) =>
            val u = if (faction == from)
                from.reserve --> Agent.of(from)
            else
                from.favors --> Agent.of(faction)

            u --> (if (faction == to)
                from.reserve
            else
                from.favors)

        case NegotiationCaptive(from, to, faction) =>
            val u = from.captives --> Agent.of(faction)

            u --> (if (faction == to)
                from.reserve
            else
                from.captives)

        case NegotiationTrophy(from, to, faction, unit) =>
            val u = from.trophies --> unit.of(faction)

            u --> (if (faction == to)
                from.reserve
            else
                from.trophies)

        case NegotiationResource(from, to, resource, lock) =>
            from.pay(PayResource(resource, lock))
            to.gain(resource)

        case NegotiationEmpireInvitation(from, to) =>
            to.regent = true
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        case StartSetupAction =>
            factions.foreach { f =>
                f.regent = true
            }

            SetupEmpireAction

        case SetupEmpireAction =>
            Random[$[Int]]($($(1, 2), $(2, 3), $(3, 4), $(4, 5), $(5, 6), $(6, 1)), EmpireClustersRandomAction(_))

        case EmpireClustersRandomAction(l) =>
            systems.foreach { s =>
                if (s.cluster.in(l)) {
                    Empire.reserve --> Ship --> s
                }
                else {
                    val u = Blights.reserve --> Blight
                    u --> s
                    Blights.damaged :+= u
                }
            }

            Empire.log("controlled sectors", l(0).hlb, "and", l(1).hlb)

            Blights.log("spread through the rest of the map")

            Random[Symbol]($(Arrow, Crescent, Hex), FreeCitiesRandomAction(_))

        case FreeCitiesRandomAction(symbol) =>
            systems.foreach { s =>
                if (s.symbol == symbol && Empire.at(s).none) {
                    val u = Free.reserve --> City

                    u --> s

                    log(u, "remained in", s)
                }
            }

            Random[Resource]($(Weapon, Fuel, Relic), GovernEdictRandomAction(_))

        case GovernEdictRandomAction(r) =>
            game.edicts :+= r @@ {
                case Weapon => PolicyOfWar
                case Fuel => PolicyOfEscalation
                case Relic => PolicyOfPeace
            }

            log("Initial policy was", game.edicts)

            game.sidedeck --> ImperialCouncilInSession --> game.market

            ShuffleCourtDiscardAction(ReplenishMarketAction(AddLoreToCourtAction))

        case AddLoreToCourtAction =>
            ShuffleTake[Lore](Lores.done, factions.num, LoreToCourtShuffledAction(_))

        case LoreToCourtShuffledAction(l) =>
            game.sidedeck --> l --> game.court

            log("Added", l, "to court")

            ShuffleCourtDiscardAction(FactionsSetupAction)

        case FactionsSetupAction =>
            DealFatesAction(FatesChosenAction)

        case DealFatesAction(then) =>
            game.act += 1

            Shuffle[Fate](Fates.act1, FatesShuffledAction(_, then))

        case FatesShuffledAction(l, then) =>
            var rest = l

            factions.foreach { f =>
                if (f.fates.none) {
                    f.fates ++= rest.take(1)
                    rest = rest.drop(1)
                }

                f.fates ++= rest.take(1)
                rest = rest.drop(1)
            }

            log("Fates were shuffled and dealt")

            ChooseFatesAction(then)

        case ChooseFatesAction(then) =>
            val l = factions.%(_.fates.num > 1)

            implicit val convert = (x : Fate) => x.img

            if (l.none)
                Milestone(then)
            else
                MultiAsk(l./(f =>
                    YYSelectObjectsAction(f, f.fates)
                        .withGroup("Choose Fate")
                        .withThen(x => ChooseFateAction(f, x, ChooseFatesAction(then)))(x => x)("~~~")
                        .ask
                ))

        case ChooseFateAction(f, x, then) =>
            f.fates = $(x)

            if (f.past.has(x).not)
                f.past :+= x

            then

        case FatesChosenAction =>
            factions.foreach { f =>
                f.log("chose", f.fates)
            }

            FatesSetupAction(factions)

        case FatesSetupAction(Nil) =>
            BuildingsShipsSetupAction

        case FatesSetupAction(f :: rest) =>
            FateSetupAction(f, FatesSetupAction(rest))

        case FateSetupAction(f, then) =>
            val fate = f.fates.only

            f.log("setup", fate)

            if (fate.act == game.act)
                f.fatedeck = game.courtiers.register(FateDeck(fate), content = BlightCards.fates(fate))

            FateInitAction(f, fate, game.act, then)

        case FateInitAction(f, Steward, 1, then) =>
            f.objective = |(ConsolidateImperialPower)

            f.progress = game.factions.num @@ {
                case 2 => 20
                case 3 => 17
                case 4 => 14
            }

            f.log("objective was set to", f.progress.hlb)

            f.primus = true

            f.log("became the", "First Regent".styled(Empire))

            f.lores :+= ImperialAuthority

            f.log("got", f.lores.last)

            f.fatedeck.%(_.id == "f01-03") --> f.loyal

            f.log("took", f.loyal.last)

            then

        case FateInitAction(f, Founder, 1, then) =>
            f.objective = |(InspireConfidence)

            f.progress = game.factions.num @@ {
                case 2 => 18
                case 3 => 16
                case 4 => 14
            }

            f.log("objective was set to", f.progress.hlb)

            f.fatedeck.%(_.id == "f02-02") --> f.loyal

            f.log("took", f.loyal.last)

            then

        case FateInitAction(f, Caretaker, 1, then) =>
            f.objective = |(FindGolems)

            f.progress = 18

            f.log("objective was set to", f.progress.hlb)

            f.lores :+= GolemBeacons

            f.log("got", f.lores.last)

            f.lores :+= GolemHearth

            f.log("got", f.lores.last)

            f.fatedeck.%(_.id == "f05-04") --> f.loyal

            f.log("took", f.loyal.last)

            then

        case FateInitAction(f, Admiral, 1, then) =>
            f.objective = |(ProveYourself)

            f.progress = 13

            f.log("objective was set to", f.progress.hlb)

            if (factions.%(_.fates.has(Steward)).none) {
                f.primus = true

                f.log("became", "the First Regent".styled(Empire))
            }

            game.edicts :+= UseImperialFoundries

            f.log("added", UseImperialFoundries)

            f.fatedeck.%(_.id == "f07-02") --> f.loyal

            f.log("took", f.loyal.last)

            then

        case FateInitAction(f, fate, act, then) =>
            log(fate, "not initialized")

            then

        case BuildingsShipsSetupAction =>
            val next = factions.%(f => systems./(f.at(_).buildings.num).sum == 0).starting ||
               factions.reverse.%(f => systems./(f.at(_).buildings.num).sum == 1).starting

            next./{ f =>
                val prefix = f.short + "-"

                Ask(f)
                    .some(systems.%(Empire.at(_).any).%(game.freeSlots(_) > 0)) { s =>
                        PlaceCityAndShipsAction(f, s).as(City.of(f), Image(prefix + "city", styles.qbuilding), "and ships")("Place in", s) ::
                        PlaceStarportAndShipsAction(f, s).as(Starport.of(f), Image(prefix + "starport", styles.qbuilding), "and ships")("Place in", s)
                    }
            }.|(Milestone(FillFreeCitiesSetupAction))

        case PlaceCityAndShipsAction(f, s) =>
            val u = f.reserve --> City.of(f)
            u --> s

            val s1 = f.reserve --> Ship.of(f)
            s1 --> s

            val s2 = f.reserve --> Ship.of(f)
            s2 --> s

            val s3 = f.reserve --> Ship.of(f)
            s3 --> s

            f.log("placed", u, "and", s1, Comma, s2, Comma, s3, "in", s)

            val r = game.resources(s).only

            f.gain(r)

            f.log("gained", r)

            BuildingsShipsSetupAction

        case PlaceStarportAndShipsAction(f, s) =>
            val u = f.reserve --> Starport.of(f)
            u --> s

            val s1 = f.reserve --> Ship.of(f)
            s1 --> s

            val s2 = f.reserve --> Ship.of(f)
            s2 --> s

            val s3 = f.reserve --> Ship.of(f)
            s3 --> s

            f.log("placed", u, "and", s1, Comma, s2, Comma, s3, "in", s)

            val r = game.resources(s).only

            f.gain(r)

            f.log("gained", r)

            BuildingsShipsSetupAction

        case FillFreeCitiesSetupAction =>
            factions.foreach { f =>
                f.adjust = false
            }

            if (factions.%(_.primus).none) {
                factions.first.primus = true

                factions.first.log("became the", "First Regent".styled(Empire))
            }

            systems.%(Empire.at(_).any).foreach { s =>
                game.freeSlots(s).timesDo { () =>
                    val u = Free.reserve --> City.of(Free)

                    u --> s

                    Free.log("placed", u, "in", s)
                }
            }

            StartChapterAction

        case ResolveEventAction(f) =>
            log("RESOLVING EVENT CARD")

            Ask(f)
                .add(StartSummitAction(f, RollForEventAction(f)).as("Start Summit".hlb)("Event Card".hlb))
                .skip(RollForEventAction(f))

        case StartSummitAction(f, then) =>
            log("STARTING SUMMIT")

            f.log("started a", "Summit".hl)

            CallToOrderAction(f, then)

        case CallToOrderAction(f, then) =>
            log("CALL TO ORDER")

            Ask(f).group("Call to Order".hlb)
                .add(ReturnFavorsMainAction(f, then).as("Return Favors").!(f.favors.none))
                .add(PetitionCouncilAction(f, then).as("Petition the Council").!(game.market.has(ImperialCouncilDecided).not))
                .add(LeaveEmpireAction(f, then).as("Leave Empire").!(f.regent.not))
                .add(ReviveEmpireAction(f, then).as("Revive Empire").!(factions.exists(_.regent)))
                .done(NegotiationsAction(f, then))
                .needOk

        case ReturnFavorsMainAction(f, then) =>
            val state = new NegotiationState

            Ask(f).group("Return Favors".hlb)
                .some(factions.but(f)) { e =>
                    val n = f.favors.$.ofc(e).num
                    state.listItems(e, f)./(i => NegotiationsAction(f, then)
                        .as(i.elem, "for", i.cost.hlb, "Favor" + (i.cost != 1).??("s"))("From", i.from, "to", i.to, "(" ~ n.hlb, "Favor" + (n != 1).??("s") + ")")
                        .!(i.cost > n)
                    )
                }
                .group(" ")
                .cancel

        case LeaveEmpireAction(f, then) =>
            f.regent = false

            f.log("left the", "Empire".styled(Empire))

            if (f.primus) {
                f.primus = false

                val l = factions.%(_.regent)

                if (l.any)
                    Ask(f).group("First Regent".hlb)
                        .each(l.%(_.power == l./(_.power).max))(e => FirstRegentAction(e, CallToOrderAction(f, then)).as(e))
                else {
                    log("EMPIRE DISSOLVED")

                    f.log("TODO: LOSE POWER FOR RESOURCES IN TRUST")

                    then
                }
            }
            else
                CallToOrderAction(f, then)

        case FirstRegentAction(f, then) =>
            f.primus = true

            f.log("became the", "First Regent".styled(Empire))

            then

        case NegotiationsAction(f, then) =>
            log("Negotiations started")

            game.negotiators = factions

            ContinueNegotiationsAction(factions, then)

        case ContinueNegotiationsAction(l, then) if l.num < 2 =>
            log("Negotiations completed")

            game.drafts = $
            game.draftsCount = 0
            game.negotiators = $

            then

        case ContinueNegotiationsAction(l, then) =>
            val next = ContinueNegotiationsAction(l, then)

            MultiAsk(l./(f =>
                Ask(f)
                    .some(game.drafts) { d =>
                        val g = $[Any]("Deal Draft", "#".hl ~ d.index.hlb, d.approved.some./(l => $(Comma, "approved by", d.approved.commaAnd)))
                        d.items./(i => Info(i.elem, "to", i.to)(g)) ++
                        d.parties.has(f).$(
                            d.approved.has(f).?(NegotiateReconsiderAction(f, d.index, next).as("Reconsider".styled(xstyles.warning))(g)).|(NegotiateApproveAction(f, d.index, next).as("Approve".styled(Blights))(g)),
                            NegotiateRejectAction(f, d.index, next).as("Reject".styled(styles.hit))(g)
                        )
                    }
                    .group("Negotiations")
                    .each(l.but(f))(e => NegotiateMainAction(f, $(f, e), next).as("Negotiate with", e))
                    .add((l.num > 2).?(NegotiateMainAction(f, l, next).as("Negotiate with multiple parties")))
                    .add(AbandonNegotiationsAction(f, ContinueNegotiationsAction(l.but(f), then)).as("Withdraw from Negotiations".styled(xstyles.error))(" "))
                    .needOk
            ))

        case AbandonNegotiationsAction(f, then @ ContinueNegotiationsAction(l, _)) =>
            f.log("withdrew from the negotiations")

            if (l.num >= 2) {
                val (remaining, invalid) = game.drafts.partition(_.parties.has(f).not)

                invalid.foreach { o =>
                    log("Proposal", "#".hl ~ o.index.hlb, "was no longer valid")
                }

                game.drafts = remaining
            }

            then

        case NegotiateMainAction(f, l, then) =>
            NegotiateComposeAction(f, l, $, then)

        case NegotiateComposeAction(f, l, items, then) if items.num > 12 =>
            Ask(f).done(then).needOk

        case NegotiateComposeAction(f, l, items, then) =>
            val state = new NegotiationState

            items.foreach(state.applyItem)

            var ask = Ask(f)

            if (items.any) {
                ask = ask
                    .each(items)(i => Info(i.elem, "to", i.to)("Compose Deal"))
                    .add(NegotiateSubmitAction(f, items, then).as("Submit".hlb))
            }

            l.combinations(2).$./~(c => $(c(0) -> c(1), c(1) -> c(0)))./{ (a, b) =>
                ask = ask
                    .group("From", a, "to", b)
                    .each(state.listItems(a, b))(i => NegotiateComposeAction(f, l, items :+ i, then).as(i.elem))
            }

            ask.group(" ").cancel

        case NegotiateSubmitAction(f, items, then) =>
            game.draftsCount += 1

            game.drafts :+= NegotiationDraft(f, game.draftsCount, (items./(_.to) ++ items./(_.from)).distinct, $(f), items)

            f.log("drafted proposal", "#".hl ~ game.draftsCount.hlb)

            items.foreach { i => log("#".hl ~ game.draftsCount.hlb, ">", i.elem, "to", i.to) }

            then

        case NegotiateReconsiderAction(f, n, then) =>
            game.drafts = game.drafts./{ d =>
                if (d.index == n)
                    d.copy(approved = d.approved.but(f))
                else
                    d
            }

            f.log("considered proposal", "#".hl ~ n.hlb)

            then

        case NegotiateApproveAction(f, n, then) =>
            game.drafts = game.drafts./{ d =>
                if (d.index == n)
                    d.copy(approved = d.approved :+ f)
                else
                    d
            }

            f.log("approved proposal", "#".hl ~ n.hlb)

            val result = game.drafts.%(_.index == n).%(d => d.parties.diff(d.approved).none).single

            if (result.any)
                NegotiateCommitAction(n, then)
            else
                then

        case NegotiateCommitAction(n, then) =>
            val d = game.drafts.%(_.index == n).only

            game.drafts = game.drafts.%(_.index != n)

            log(d.approved.commaAnd, "agreed on proposal", "#".hl ~ n.hlb)

            log("Executed agreement", "#".hl ~ n.hlb)

            d.items.foreach { i => log(i.elem, "to", i.to) }

            d.items.foreach { executeNegotiationItem }

            val (remaining, invalid) = game.drafts.partition { o =>
                val combination = o.parties.combinations(2).$./~(c => $(c(0) -> c(1), c(1) -> c(0)))

                val state = new NegotiationState
                var ok = true

                o.items.foreach { i =>
                    if (ok)
                        if (combination.exists { case (a, b) => state.listItems(a, b).has(i) })
                            state.applyItem(i)
                        else
                            ok = false
                }

                ok
            }

            invalid.foreach { o =>
                log("Proposal", "#".hl ~ o.index.hlb, "was no longer valid")
            }

            game.drafts = remaining./(_.copy(approved = $))

            then

        case ResolveCouncilAction(f) =>
            log("RESOLVING COUNCIL DECIDED")

            Ask(f)
                .add(StartSummitAction(f, CouncilBonusAction(f)).as("Start Summit".hlb))
                .skip(CouncilBonusAction(f))

        case CouncilBonusAction(f) =>
            log("COUNCIL BONUS")

            ChooseEventAction(f)

        case ChooseEventAction(f) =>
            Ask(f).group("Choose")
                .add(ChooseCrisesAction(f).as("Crises".hlb))
                .add(ChooseEdictsAction(f).as("Edicts".hlb))

        case ChooseCrisesAction(f) =>
            Random3[Boolean, Int, Symbol]($(false), $(1, 2, 3, 4, 5, 6), $(Arrow, Crescent, Hex), (e, i, s) => EventRolledAction(e, i, s))

        case ChooseEdictsAction(f) =>
            Random3[Boolean, Int, Symbol]($(true), $(0), $(Gate), (e, i, s) => EventRolledAction(e, i, s))

        case RollForEventAction(f) =>
            Random3[Boolean, Int, Symbol]($(false, true), $(1, 2, 3, 4, 5, 6), $(Arrow, Crescent, Hex), (e, i, s) => EventRolledAction(e, i, s))

        case EventRolledAction(e, i, s) =>
            if (e)
                log("EXECUTING EDICTS")
            else
                log("PERFORMING CRISES " + i + " " + s)

            ContinueRoundsAction


        case GainCourtCardAction(f, ImperialCouncilInSession, _, then) =>
            f.loyal --> ImperialCouncilInSession --> game.sidedeck

            val l = game.market.$

            game.sidedeck --> ImperialCouncilDecided --> game.market

            game.market --> l --> game.market

            game.decided = |(f)

            then

        case GainCourtCardAction(f, c @ VoxCard(_, BlightLooms), e, then) =>
            val next = BuryCourtCardAction(f, c, then)

            val l1 = systems.%(f.present)
            val l2 = systems.%(Blights.at(_).damaged.any)

            val cc = l1./(_.cluster).distinct `intersect` l2./(_.cluster).distinct

            Ask(f).group(c, "destroys all damaged Blight in")
                .each(cc)(i => BlightLoomsSecuredAction(f, i, next).as("Cluster".hl, i.hlb))
                .skip(next)
                .needOk

        case BlightLoomsSecuredAction(f, i, then) =>
            systems.%(_.cluster == i).foreach { s =>
                Blights.at(s).damaged.foreach { u =>
                    f.log("destroyed", u, "in", s)

                    u --> Blights.reserve

                    Blights.damaged :-= u
                }
            }

            then

        case _ => UnknownContinue
    }
}
