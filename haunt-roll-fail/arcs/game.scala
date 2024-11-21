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


trait Color extends Styling with Record {
    def name = toString
    def short = name.take(1)
    def style = short.toLowerCase
}

trait Faction extends BasePlayer with Color with Elementary {
    def ss = name.take(1).styled(this)
    def elem : Elem = name.styled(this).styled(styles.condensed)
}

case object Red extends Faction
case object White extends Faction
case object Blue extends Faction
case object Yellow extends Faction


trait Resource extends NamedToString with Styling with Record {
    override def elem = name.styled(this)
}

case object Material extends Resource
case object Fuel extends Resource
case object Weapon extends Resource
case object Relic extends Resource
case object Psionic extends Resource
case object Nothingness extends Resource

object Resources {
    def all = $[Resource](Material, Fuel, Weapon, Relic, Psionic)
}

trait BattleResult extends Record
case object OwnDamage extends BattleResult
case object Intersept extends BattleResult
case object HitShip extends BattleResult
case object HitBuilding extends BattleResult
case object RaidKey extends BattleResult

trait BattleDie extends Record {
    val die : CustomDie[$[BattleResult]]
}

case object Skirmish extends BattleDie {
    val die = Die.from($, $, $, $(HitShip), $(HitShip), $(HitShip))
}
case object Assault extends BattleDie {
    val die = Die.from($, $(OwnDamage, HitShip), $(Intersept, HitShip), $(OwnDamage, HitShip, HitShip), $(HitShip, HitShip), $(OwnDamage, HitShip))
}
case object Raid extends BattleDie {
    val die = Die.from($(Intersept, RaidKey, RaidKey), $(OwnDamage, RaidKey), $(Intersept), $(OwnDamage, HitBuilding), $(HitBuilding, RaidKey), $(OwnDamage, HitBuilding))
}


case class AmbitionMarker(high : Int, low : Int)


trait Piece extends Record {
    def name = toString
    def plural = name + "s"
    def of(f : Faction) = SomePieceOf(f, this)
    def sof(f : Faction) : Elem = (f.name + " " + plural).styled(f)
}

trait PieceOf {
    def faction : Color
    def piece : Piece
}


trait Building extends Piece
case object City extends Building
case object Starport extends Building
case object Ship extends Piece
case object Agent extends Piece

case class SomePieceOf(faction : Color, piece : Piece) extends PieceOf with Elementary {
    def elem = (faction.name + " " + piece.name).styled(faction)
}

case class Figure(faction : Color, piece : Piece, index : Int) extends GameElementary {
    override def toString = "" + faction + "/" + piece + "/" + index
    def sp = SomePieceOf(faction, piece)
    def elem(implicit game : Game) = (faction.as[Faction]./~(_.damaged.has(this).?("Damaged ")).?? + faction.name + " " + piece.name).styled(faction)
}


trait Ambition extends NamedToString with Elementary with Record {
    override def elem = this.toString.hl.styled(styles.title)
}

case object Tycoon extends Ambition
case object Tyrant extends Ambition
case object Warlord extends Ambition
case object Keeper extends Ambition
case object Empath extends Ambition

trait Suit extends NamedToString with Styling with Record

case object Administration extends Suit
case object Aggression extends Suit
case object Construction extends Suit
case object Mobilization extends Suit

trait StandardAction extends Record
case object Tax extends StandardAction
case object Build extends StandardAction
case object Move extends StandardAction
case object Repair extends StandardAction
case object Influence extends StandardAction
case object Secure extends StandardAction
case object Battle extends StandardAction


trait Cost extends Elementary with Record {
    def elemLog : Elem = elem
}

case object Pip extends Cost {
    def elem = "with card action"
}

case object NoCost extends Cost {
    def elem = Empty
}

case object AlreadyPaid extends Cost {
    def elem = Empty
}

case class PayResource(resource : Resource, lock : |[Int]) extends Cost {
    def elem = "with " ~ ResourceRef(resource, lock).elem
    override def elemLog = "with " ~ resource.elem
}

case class ResourceRef(resource : Resource, lock : |[Int]) extends Elementary {
    def elem = resource.elem ~ Image(resource.name, styles.token) ~ lock./(n => Image("keys-" + n, styles.token))
}


trait DeckCardLocation
case class Hand(faction : Faction) extends DeckCardLocation
case class Played(faction : Faction) extends DeckCardLocation
case class Blind(faction : Faction) extends DeckCardLocation
case object Deck extends DeckCardLocation

trait CourtLocation
case object CourtDeck extends CourtLocation
case object CourtMarket extends CourtLocation
case object CourtDiscard extends CourtLocation
case class DiscardAfterRound(faction : Faction) extends CourtLocation
case class Loyal(faction : Faction) extends CourtLocation


case class DeckCard(suit : Suit, strength : Int, pips : Int) extends Elementary with Record {
    def imgid = suit + "-" + strength
    def img = Image(imgid, styles.card)
    def elem = (" " + suit + " " + strength + " ").pre.spn(xstyles.outlined).styled(suit)
}

object DeckCards {
    def deck = $(
        DeckCard(Administration, 1, 4),
        DeckCard(Administration, 2, 4),
        DeckCard(Administration, 3, 3),
        DeckCard(Administration, 4, 3),
        DeckCard(Administration, 5, 3),
        DeckCard(Administration, 6, 2),
        DeckCard(Administration, 7, 1),
        DeckCard(Aggression, 1, 3),
        DeckCard(Aggression, 2, 3),
        DeckCard(Aggression, 3, 2),
        DeckCard(Aggression, 4, 2),
        DeckCard(Aggression, 5, 2),
        DeckCard(Aggression, 6, 2),
        DeckCard(Aggression, 7, 1),
        DeckCard(Construction, 1, 4),
        DeckCard(Construction, 2, 4),
        DeckCard(Construction, 3, 3),
        DeckCard(Construction, 4, 3),
        DeckCard(Construction, 5, 2),
        DeckCard(Construction, 6, 2),
        DeckCard(Construction, 7, 1),
        DeckCard(Mobilization, 1, 4),
        DeckCard(Mobilization, 2, 4),
        DeckCard(Mobilization, 3, 3),
        DeckCard(Mobilization, 4, 3),
        DeckCard(Mobilization, 5, 2),
        DeckCard(Mobilization, 6, 2),
        DeckCard(Mobilization, 7, 1),
    )
}


trait Effect extends Record

trait CourtCard extends Effect with Record with Elementary {
    def id : String
    def name : String
    def elem = name.styled(styles.title).hl
}

abstract class GuildCard(val id : String, val name : String, val suit : Resource, val keys : Int) extends CourtCard
abstract class VoxCard(val id : String, val name : String) extends CourtCard

trait LoyalGuild { self : GuildCard => }

case object LoyalEngineers    extends GuildCard("bc01", "Loyal Engineers",    Material, 3) with LoyalGuild
case object MiningInterest    extends GuildCard("bc02", "Mining Interest",    Material, 2)
case object MaterialCartel    extends GuildCard("bc03", "Material Cartel",    Material, 2)
case object AdminUnion        extends GuildCard("bc04", "Admin Union",        Material, 2)
case object ConstructionUnion extends GuildCard("bc05", "Construction Union", Material, 2)
case object FuelCartel        extends GuildCard("bc06", "Fuel Cartel",        Fuel,     2)
case object LoyalPilots       extends GuildCard("bc07", "Loyal Pilots",       Fuel,     3) with LoyalGuild
case object Gatekeepers       extends GuildCard("bc08", "Gatekeepers",        Fuel,     2)
case object ShippingInterest  extends GuildCard("bc09", "Shipping Interest",  Fuel,     2)
case object SpacingUnion      extends GuildCard("bc10", "Spacing Union",      Fuel,     2)
case object ArmsUnion         extends GuildCard("bc11", "Arms Union",         Weapon,   2)
case object PrisonWardens     extends GuildCard("bc12", "Prison Wardens",     Weapon,   2)
case object Skirmishers       extends GuildCard("bc13", "Skirmishers",        Weapon,   2)
case object CourtEnforcers    extends GuildCard("bc14", "Court Enforcers",    Weapon,   2)
case object LoyalMarines      extends GuildCard("bc15", "Loyal Marines",      Weapon,   3) with LoyalGuild
case object LatticeSpies      extends GuildCard("bc16", "Lattice Spies",      Psionic,  2)
case object Farseers          extends GuildCard("bc17", "Farseers",           Psionic,  2)
case object SecretOrder       extends GuildCard("bc18", "Secret Order",       Psionic,  2)
case object LoyalEmpaths      extends GuildCard("bc19", "Loyal Empaths",      Psionic,  3) with LoyalGuild
case object SilverTongues     extends GuildCard("bc20", "Silver Tongues",     Psionic,  2)
case object LoyalKeepers      extends GuildCard("bc21", "Loyal Keepers",      Relic,    3) with LoyalGuild
case object SwornGuardians    extends GuildCard("bc22", "Sworn Guardians",    Relic,    1)
case object ElderBroker       extends GuildCard("bc23", "Elder Broker",       Relic,    2)
case object RelicFence        extends GuildCard("bc24", "Relic Fence",        Relic,    2)
case object GalacticBards     extends GuildCard("bc25", "Galactic Bards",     Relic,    1)
case object MassUprising      extends VoxCard  ("bc26", "Mass Uprising")
case object PopulistDemands   extends VoxCard  ("bc27", "Populist Demands")
case object OutrageSpreads    extends VoxCard  ("bc28", "Outrage Spreads")
case object SongOfFreedom     extends VoxCard  ("bc29", "Song of Freedom")
case object GuildStruggle     extends VoxCard  ("bc30", "Guild Struggle")
case object CallToAction      extends VoxCard  ("bc31", "Call to Action")

object CourtCards {
    def deck = $(
        LoyalEngineers    ,
        MiningInterest    ,
        MaterialCartel    ,
        AdminUnion        ,
        ConstructionUnion ,
        FuelCartel        ,
        LoyalPilots       ,
        Gatekeepers       ,
        ShippingInterest  ,
        SpacingUnion      ,
        ArmsUnion         ,
        PrisonWardens     ,
        Skirmishers       ,
        CourtEnforcers    ,
        LoyalMarines      ,
        LatticeSpies      ,
        Farseers          ,
        SecretOrder       ,
        LoyalEmpaths      ,
        SilverTongues     ,
        LoyalKeepers      ,
        SwornGuardians    ,
        ElderBroker       ,
        RelicFence        ,
        GalacticBards     ,
        MassUprising      ,
        PopulistDemands   ,
        OutrageSpreads    ,
        SongOfFreedom     ,
        GuildStruggle     ,
        CallToAction      ,
    )
}


trait LeaderEffect extends Effect with Elementary {
    def name = toString
    def elem = name.styled(styles.title).hl
}

case object Attuned extends LeaderEffect
case object Cryptic extends LeaderEffect
case object Bold extends LeaderEffect
case object Paranoid extends LeaderEffect

abstract class Leader(val id : String, val name : String, val effects : $[Effect], val resources : $[Resource], val setupA : $[Piece], val setupB : $[Piece], val setupC : $[Piece]) extends Record with Elementary {
    def img = Image(id, styles.leaderCard)
    def elem = name.styled(styles.title).hl
}

case object Elder         extends Leader("leader01", "Elder",         $, $, $, $, $)
case object Mystic        extends Leader("leader02", "Mystic",        $(Attuned, Cryptic), $(Psionic, Relic), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object FuelDrinker   extends Leader("leader03", "Fuel-Drinker",  $(), $(Fuel, Fuel), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object Upstart       extends Leader("leader04", "Upstart",       $(), $(Psionic, Material), $(City, Ship, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object Rebel         extends Leader("leader05", "Rebel",         $(), $(Material, Weapon), $(Starport, Ship, Ship, Ship, Ship), $(Ship, Ship, Ship, Ship), $(Ship, Ship))
case object Warrior       extends Leader("leader06", "Warrior",       $, $, $, $, $)
case object Feastbringer  extends Leader("leader07", "Feastbringer",  $, $, $, $, $)
case object Demagogue     extends Leader("leader08", "Demagogue",     $(Bold, Paranoid), $(Psionic, Weapon), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object Archivist     extends Leader("leader09", "Archivist",     $, $, $, $, $)
case object Overseer      extends Leader("leader10", "Overseer",      $, $, $, $, $)
case object Corsair       extends Leader("leader11", "Corsair",       $, $, $, $, $)
case object Noble         extends Leader("leader12", "Noble",         $(), $(Psionic, Psionic), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object Anarchist     extends Leader("leader13", "Anarchist",     $, $, $, $, $)
case object Shaper        extends Leader("leader14", "Shaper",        $, $, $, $, $)
case object Agitator      extends Leader("leader15", "Agitator",      $, $, $, $, $)
case object Quartermaster extends Leader("leader16", "Quartermaster", $, $, $, $, $)

object Leaders {
    def all = $(
        Elder         ,
        Mystic        ,
        FuelDrinker   ,
        Upstart       ,
        Rebel         ,
        Warrior       ,
        Feastbringer  ,
        Demagogue     ,
        Archivist     ,
        Overseer      ,
        Corsair       ,
        Noble         ,
        Anarchist     ,
        Shaper        ,
        Agitator      ,
        Quartermaster ,
    )

    def preset1 = $(Mystic, FuelDrinker, Upstart, Rebel, Noble, Demagogue)
}


abstract class Lore(val id : String, val name : String) extends Record with Effect with Elementary {
    def img = Image(id, styles.card)
    def elem = name.styled(styles.title).hl
}

case object MirrorPlating     extends Lore("lore04", "Mirror Plating")
case object HiddenHarbors     extends Lore("lore05", "Hidden Harbors")
case object LivingStructures  extends Lore("lore10", "Living Structures")
case object SurvivalOverrides extends Lore("lore18", "Survival Overrides")
case object WarlordsCruelty   extends Lore("lore23", "Warlords Cruelty")

object Lores {
    def all = $(
        MirrorPlating    ,
        HiddenHarbors    ,
        LivingStructures ,
        SurvivalOverrides,
        WarlordsCruelty  ,
    )

    def preset1 = $(MirrorPlating, HiddenHarbors, LivingStructures, SurvivalOverrides, WarlordsCruelty)
}


trait Region {
    def -->(p : Piece)(implicit tracker : IdentityTracker[Region, Figure]) = tracker.get(this).%(_.piece == p).take(1).single.|!(p.name + " not found in " + this)
    def -->(p : PieceOf)(implicit tracker : IdentityTracker[Region, Figure]) = tracker.get(this).%(u => u.piece == p.piece && u.faction == u.faction).take(1).single.|!(p.piece.name + " of " + p.faction.name + " not found in " + this)
}

trait SpecialRegion extends Region
case class Reserve(f : Faction) extends SpecialRegion
case class Outrage(f : Faction) extends SpecialRegion
case class Trophies(f : Faction) extends SpecialRegion
case class Captives(f : Faction) extends SpecialRegion
case class Influence(c : CourtCard) extends SpecialRegion


trait Symbol extends NamedToString with Record
case object Gate extends Symbol
case object Arrow extends Symbol
case object Crescent extends Symbol
case object Hex extends Symbol

case class System(cluster : Int, symbol : Symbol) extends Region with Elementary with Record {
    def name = (symbol.name + " " + cluster + "" + symbol @@ {
        case Gate => 0x2726.toChar.toString
        case Arrow => 0x2B9D.toChar.toString
        case Crescent => 0x263E.toChar.toString
        case Hex => 0x2B22.toChar.toString
    })

    def elem = name.hlb
}


trait Board {
    val name : String
    def systems : $[System]

    def starting : $[(System, System, $[System])]

    def connected(c : System) : $[System]

    lazy val distance = systems./(a => a -> systems./(b => b -> {
        var n = 0
        var l = $(a)
        while (!l.contains(b)) {
            n += 1
            l = l./~(connected)
        }
        n
    }).toMap).toMap
}

trait BaseBoard extends Board with Record {
    val clusters : $[Int]

    def nextCluster(g : Int) : Int = {
        var n = ((g + 1) - 1) % 6 + 1
        if (n.in(clusters).not)
            nextCluster(n)
        else
            n
    }

    def prevCluster(g : Int) : Int = {
        var n = ((g - 1) + 5) % 6 + 1
        if (n.in(clusters).not)
            prevCluster(n)
        else
            n
    }

    def slots(s : System): Int = s @@ {
        case System(_, Gate) => 0
        case System(1, Arrow) => 2
        case System(1, Crescent) => 1
        case System(1, Hex) => 2
        case System(2, Arrow) => 1
        case System(2, Crescent) => 1
        case System(2, Hex) => 2
        case System(3, Arrow) => 1
        case System(3, Crescent) => 1
        case System(3, Hex) => 2
        case System(4, Arrow) => 2
        case System(4, Crescent) => 2
        case System(4, Hex) => 1
        case System(5, Arrow) => 1
        case System(5, Crescent) => 1
        case System(5, Hex) => 2
        case System(6, Arrow) => 1
        case System(6, Crescent) => 2
        case System(6, Hex) => 1
    }

    def resource(s : System) : Resource = s @@ {
        case System(1, Arrow) => Weapon
        case System(1, Crescent) => Fuel
        case System(1, Hex) => Material
        case System(2, Arrow) => Psionic
        case System(2, Crescent) => Weapon
        case System(2, Hex) => Relic
        case System(3, Arrow) => Material
        case System(3, Crescent) => Fuel
        case System(3, Hex) => Weapon
        case System(4, Arrow) => Relic
        case System(4, Crescent) => Fuel
        case System(4, Hex) => Material
        case System(5, Arrow) => Weapon
        case System(5, Crescent) => Relic
        case System(5, Hex) => Psionic
        case System(6, Arrow) => Material
        case System(6, Crescent) => Fuel
        case System(6, Hex) => Psionic
    }

    lazy val systems : $[System] = clusters./~(i => $(System(i, Gate), System(i, Arrow), System(i, Crescent), System(i, Hex)))

    def connected(s : System) = s @@ {
        case System(i, Gate) => $(System(nextCluster(i), Gate), System(prevCluster(i), Gate), System(i, Arrow), System(i, Crescent), System(i, Hex))
        case System(i, Crescent) => $(System(i, Gate), System(i, Arrow), System(i, Hex))
        case System(i, Arrow) => $(System(i, Gate), System(i, Crescent)) ++ (i == 6 && clusters.has(5)).$(System(5, Hex)) ++ (i == 3 && clusters.has(2)).$(System(2, Hex))
        case System(i, Hex) => $(System(i, Gate), System(i, Crescent)) ++ (i == 5 && clusters.has(6)).$(System(6, Arrow)) ++ (i == 2 && clusters.has(3)).$(System(3, Arrow))
    }
}

case object Board4Mixup1 extends BaseBoard {
    val name = "4 Players / Mix Up 1"
    val clusters = $(1, 2, 4, 5, 6)

    val starting : $[(System, System, $[System])] = $(
        (System(4, Arrow), System(6, Hex), $(System(1, Gate))),
        (System(4, Hex), System(5, Hex), $(System(6, Gate))),
        (System(5, Arrow), System(1, Hex), $(System(4, Gate))),
        (System(6, Arrow), System(1, Arrow), $(System(5, Gate))),
    )

}

case object Board3Frontiers extends BaseBoard {
    val name = "3 Players / Frontiers"
    val clusters = $(1, 4, 5, 6)

    val starting : $[(System, System, $[System])] = $(
        (System(1, Hex), System(4, Hex), $(System(6, Gate))),
        (System(5, Hex), System(1, Arrow), $(System(5, Gate))),
        (System(4, Crescent), System(6, Arrow), $(System(1, Gate))),
    )

}


object Piece {
    implicit class PieceCast(val p : Piece) {
        def building = p match {
            case b : Building => |(b)
            case _ => None
        }
    }
}

//[[ PINKER
object Figure {
    implicit class FiguresEx[T](val t : T)(implicit val conv : T => $[Figure]) {
        def l = conv(t)
        def of(f : Color) = l.%(_.faction == f)
        def piece(p : Piece) = l.%(_.piece == p)
        def one(p : Piece) = l.%(_.piece == p).head
        def colors = l./(_.faction).distinct
        def count(p : Piece) = l.%(u => u.piece == p).num
        def hasA(p : Piece) = l.exists(u => u.piece == p)
        def hasBuilding = l.exists(u => u.piece.is[Building])
        def sublist(s : $[Piece]) = {
            var rest = s
            l./~{ u =>
                if (rest.contains(u.piece)) {
                    rest = rest :- (u.piece)
                    |(u)
                }
                else
                    None
            }
        }
        def sub(n : Int, p : Piece) = sublist(n.times(p))

        def buildings = l.%(u => u.piece.building.any)
        def ships = l.%(u => u.piece == Ship)
        def cities = l.%(u => u.piece == City)
        def starports = l.%(u => u.piece == Starport)

        def comma : $[Any] = l./~(e => $(Comma, e)).drop(1)
    }

}
//]]


trait ViewCard extends ViewObject[DeckCard] { self : UserAction =>
    def d : DeckCard
    def obj = d
}


trait Key

/*
trait BuildKey extends Key {
    val color : Color
    val area : Area
}
*/

trait SoftKeys

//[[ BLUER
case class StartAction(version : String) extends StartGameAction with GameVersion
case class StartingInitiativeAction(random : Faction) extends RandomAction[Faction]
case object CourtSetupAction extends ForcedAction
case class ShuffleCourtDiscardAction(then : ForcedAction) extends ForcedAction
case class ShuffleCourtDeckAction(then : ForcedAction) extends ForcedAction
case class ShuffledCourtDeckAction(shuffled : $[CourtCard], then : ForcedAction) extends ShuffledAction[CourtCard]
case class ReplenishMarketAction(then : ForcedAction) extends ForcedAction
case object FactionsSetupAction extends ForcedAction
case class LeadersLoresShuffledAction(shuffled1 : $[Leader], shuffled2 : $[Lore]) extends Shuffled2Action[Leader, Lore]
case class DraftNextAction(f : Faction) extends ForcedAction
case class AssignLeaderAction(f : Faction, l : Leader, then : ForcedAction) extends ForcedAction
case class AssignLoreAction(f : Faction, l : Lore, then : ForcedAction) extends ForcedAction
case object LeadersFactionsSetupAction extends ForcedAction
case object BaseFactionsSetupAction extends ForcedAction
case object StartChapterAction extends ForcedAction
case object CheckWinAction extends ForcedAction
case object DealCardsAction extends ForcedAction
case class ShuffleDeckCardsAction(shuffled : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]
case object StartRoundAction extends ForcedAction
case class LeadMainAction(self : Faction) extends ForcedAction with Soft
case class LeadAction(self : Faction, d : DeckCard) extends ForcedAction
case class PassAction(self : Faction) extends ForcedAction
case class CheckAmbitionAction(self : Faction, d : DeckCard) extends ForcedAction with Soft
case class FollowAction(self : Faction) extends ForcedAction with Soft
case class SurpassAction(self : Faction, d : DeckCard) extends ForcedAction
case class CopyAction(self : Faction, d : DeckCard) extends ForcedAction
case class PivotAction(self : Faction, d : DeckCard) extends ForcedAction
case class CheckSeizeAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class SeizeAction(self : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class DeclareAmbitionAction(self : Faction, ambition : Ambition, zero : Boolean, then : ForcedAction) extends ForcedAction
case class AmbitionDeclaredAction(self : Faction, ambition : Ambition, used : $[Effect], then : ForcedAction) extends ForcedAction with Soft
case class PreludeActionAction(self : Faction, suit : Suit, pips : Int) extends ForcedAction

case class AdjustResourcesAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class MultiAdjustResourcesAction(l : $[Faction],  then : ForcedAction) extends ForcedAction
case class ContinueMultiAdjustResourcesAction(then : ForcedAction) extends ForcedAction with Soft

case class AdjustingResourcesAction(self : Faction, l : $[Resource], then : ForcedAction) extends HiddenChoice with Soft with NoExplode
case class ExplodeReorderResourcesAction(self : Faction, slots : Int, l : $[Resource], then : ForcedAction) extends HiddenChoice with SelfExplode with SelfValidate {
    def validate(target : Action) = target @@ {
        case AdjustingResourcesAction(f, m, t) => self == f && then == t && m.toSet.equals(l.toSet)
        case ReorderResourcesAction(f, m, t) => self == f && then == t && m.toSet.equals(l.toSet)
        case _ => false
    }

    def explode(withSoft : Boolean) = {
        val c = l.permutations.$
        val cc = c.%(l => l.drop(slots).but(Nothingness).none || l.take(slots).has(Nothingness).not)

        cc./(ReorderResourcesAction(self, _, then).as("Done")) ++ withSoft.??(c./(AdjustingResourcesAction(self, _, then)))
    }
}
case class ReorderResourcesAction(self : Faction, l : $[Resource], then : ForcedAction) extends ForcedAction


case class TaxMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class TaxLoyalAction(self : Faction, cost : Cost, r : System, c : Figure, then : ForcedAction) extends ForcedAction
case class TaxRivalAction(self : Faction, cost : Cost, r : System, e : Faction, c : Figure, then : ForcedAction) extends ForcedAction

case class MoveMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class MoveFromAction(self : Faction, r : System, l : $[Figure], cascade : Boolean, x : Cost, alt : UserAction, then : ForcedAction) extends ForcedAction with Soft
case class MoveToAction(self : Faction, r : System, d : System, l : $[Figure], cascade : Boolean, x : Cost, then : ForcedAction) extends ForcedAction with Soft
case class MoveListAction(self : Faction, r : System, d : System, l : $[Figure], cascade : Boolean, x : Cost, then : ForcedAction) extends ForcedAction

case class BattleMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class BattleSystemAction(self : Faction, cost : Cost, r : System, then : ForcedAction) extends ForcedAction with Soft
case class BattleFactionAction(self : Faction, cost : Cost, r : System, e : Faction, then : ForcedAction) extends ForcedAction with Soft
case class BattleDiceAction(self : Faction, cost : Cost, r : System, e : Faction, skirmish : Int, assault : Int, raid : Int, then : ForcedAction) extends ForcedAction
case class BattleRolledAction(self : Faction, r : System, e : Faction, rolled1 : $[$[BattleResult]], rolled2 : $[$[BattleResult]], rolled3 : $[$[BattleResult]], then : ForcedAction) extends Rolled3Action[$[BattleResult], $[BattleResult], $[BattleResult]]
case class SkirmishersAction(self : Faction, r : System, e : Faction, results : $[BattleResult], reroll : $[$[BattleResult]], then : ForcedAction) extends ForcedAction
case class SkirmishersRolledAction(self : Faction, r : System, e : Faction, results : $[BattleResult], old : $[$[BattleResult]], rolled : $[$[BattleResult]], then : ForcedAction) extends RolledAction[$[BattleResult]]
case class BattleProcessAction(self : Faction, r : System, e : Faction, results : $[BattleResult], then : ForcedAction) extends ForcedAction
case class BattleRaidAction(self : Faction, r : System, e : Faction, raid : Int, then : ForcedAction) extends ForcedAction with Soft
case class BattleRaidResourceAction(self : Faction, e : Faction, r : Resource, keys : Int, then : ForcedAction) extends ForcedAction
case class BattleRaidCourtCardAction(self : Faction, e : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction


case class AssignHitsAction(self : Faction, r : System, f : Faction, e : Faction, l : $[Figure], hits : Int, bombardments : Int, raid : Int, then : ForcedAction) extends ForcedAction
case class DealHitsAction(self : Faction, r : System, f : Faction, e : Faction, l : $[Figure], raid : Int, then : ForcedAction) extends ForcedAction
case class OutrageAction(self : Faction, r : Resource, then : ForcedAction) extends ForcedAction
case class RansackMainAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction with Soft
case class RansackAction(self : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction

case class BuildMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class BuildCityAction(self : Faction, cost : Cost, r : System, then : ForcedAction) extends ForcedAction
case class BuildStarportAction(self : Faction, cost : Cost, r : System, then : ForcedAction) extends ForcedAction
case class BuildShipAction(self : Faction, cost : Cost, r : System, then : ForcedAction) extends ForcedAction

case class RepairMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class RepairAction(self : Faction, cost : Cost, r : System, u : Figure, then : ForcedAction) extends ForcedAction

case class InfluenceMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class InfluenceAction(self : Faction, cost : Cost, c : CourtCard, then : ForcedAction) extends ForcedAction

case class SecureMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class SecureAction(self : Faction, cost : Cost, c : CourtCard, then : ForcedAction) extends ForcedAction

case class AddBattleOptionAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction

case class ManufactureMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction
case class SynthesizeMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction

case class ReserveCardMainAction(self : Faction, c : GuildCard, l : $[DeckCard], then : ForcedAction) extends ForcedAction with Soft
case class ReserveCardAction(self : Faction, c : GuildCard, d : DeckCard, then : ForcedAction) extends ForcedAction

case class FillSlotsMainAction(self : Faction, x : Resource, then : ForcedAction) extends ForcedAction
case class StealResourceMainAction(self : Faction, x : |[Resource], extra : $[UserAction], then : ForcedAction) extends ForcedAction with Soft
case class StealResourceAction(self : Faction, e : Faction, x : Resource, k : Int, then : ForcedAction) extends ForcedAction

case class StealGuildCardMainAction(self : Faction, alt : $[UserAction], then : ForcedAction) extends ForcedAction with Soft
case class StealGuildCardAction(self : Faction, e : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction

case class ShipAtEachGateMainAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class ShipsInSystemsAction(self : Faction, l : $[System], then : ForcedAction) extends ForcedAction

case class ShipsInSystemMainAction(self : Faction, l : $[System], then : ForcedAction) extends ForcedAction with Soft
case class ShipsInSystemAction(self : Faction, r : System, then : ForcedAction) extends ForcedAction

case class PressgangMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class PressganAction(self : Faction, u : Figure, r : Resource, cost : Cost, then : ForcedAction) extends ForcedAction

case class ExecuteMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class ExecuteAction(self : Faction, l : $[Figure], cost : Cost, then : ForcedAction) extends ForcedAction

case class AbductMainAction(self : Faction, l : $[CourtCard], cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class AbductAction(self : Faction, c : CourtCard, cost : Cost, then : ForcedAction) extends ForcedAction

case class FarseersMainAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction with Soft
case class FarseersAction(self : Faction, e : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class FarseersBackAction(self : Faction, e : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction

case class FarseersRedrawMainAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class FarseersRedrawAction(self : Faction, l : $[DeckCard], then : ForcedAction) extends ForcedAction

case class FenceResourceAction(self : Faction, r : Resource, cost : Cost, then : ForcedAction) extends ForcedAction
case class GainResourcesAction(self : Faction, r : $[Resource], then : ForcedAction) extends ForcedAction

case class TradeMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class TakeResourceAction(self : Faction, e : Faction, x : Resource, k : Int, then : ForcedAction) extends ForcedAction
case class GiveBackResourceMainAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction with Soft
case class GiveBackResourceAction(self : Faction, e : Faction, r : Resource, then : ForcedAction) extends ForcedAction


case class LatticeSeizeAction(self : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction

case class FreeCityAction(self : Faction, r : System, u : Figure, then : ForcedAction) extends ForcedAction
case class FreeCitySeazeAskAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class FreeCitySeazeAction(self : Faction, then : ForcedAction) extends ForcedAction

case class OutrageSpreadsAction(self : Faction, r : Resource, then : ForcedAction) extends ForcedAction

case class BoldMainAction(self : Faction, influenced : $[CourtCard], then : ForcedAction) extends ForcedAction with Soft

case class GainCourtCardAction(self : Faction, c : CourtCard, from : |[Faction], then : ForcedAction) extends ForcedAction
case class DiscardCourtCardAction(self : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction
case class BuryCourtCardAction(self : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction

case class UsedEffectCardAction(self : Faction, c : Effect, then : ForcedAction) extends ForcedAction

case class EndPreludeAction(self : Faction, suit : Suit, done : Int, total : Int) extends ForcedAction
case class MainTurnAction(self : Faction, suit : Suit, done : Int, total : Int) extends ForcedAction with Soft
case class EndTurnAction(self : Faction) extends ForcedAction
case object EndRoundAction extends ForcedAction
case object TransferRoundAction extends ForcedAction
case object EndChapterAction extends ForcedAction
case object CleanUpChapterAction extends ForcedAction

case class ViewCardInfoAction(self : Faction, d : DeckCard) extends BaseInfo(Break ~ "Hand")(d.img) with ViewCard with OnClickInfo { def param = d }


case class GameOverAction(winner : Faction) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won", "(" ~ NameReference(f.name, f).hl ~ ")")

case object NoHand extends HiddenInfo
//]]

//[[ BLACKER
class Player(val faction : Faction)(implicit game : Game) {
    import game._

    val reserve = game.figures.register(Reserve(faction), _.faction == faction,
        1.to(5)./(Figure(faction, City, _)) ++
        1.to(5)./(Figure(faction, Starport, _)) ++
        1.to(15)./(Figure(faction, Ship, _)) ++
        1.to(10)./(Figure(faction, Agent, _))
    )

    var outraged : $[Resource] = $

    val trophies = game.figures.register(Trophies(faction))

    val captives = game.figures.register(Captives(faction))

    var leader : |[Leader] = None
    var lores : $[Lore] = $

    var power = 0

    val keys : $[Int] = $(3, 1, 1, 2, 1, 3)
    def resourceSlots = $(6, 6, 6, 4, 3, 2)(pooled(City))

    var resources : $[Resource] = $
    var spent : $[Resource] = $

    var anyBattle : Boolean = false

    var hand = game.cards.register(Hand(faction))
    var played = game.cards.register(Played(faction))
    var blind = game.cards.register(Blind(faction))
    var taking : $[DeckCard] = $

    var loyal = game.courtiers.register(Loyal(faction))
    val discardAfterRound = courtiers.register(DiscardAfterRound(faction))

    var damaged : $[Figure] = $

    var taxed : $[Figure] = $
    var built : $[Figure] = $
    var used : $[Effect] = $

    var lead : Boolean = false
    var surpass : Boolean = false
    var copy : Boolean = false
    var pivot : Boolean = false

    var adjust : Boolean = false

    def can(e : Effect) = (loyal.contains(e) || lores.contains(e) || leader.exists(_.effects.has(e))) && used.has(e).not

    def ruleValue(r : System) : Int = faction.at(r).diff(damaged).count(Ship)

    def rules(r : System) = faction.ruleValue(r) > game.factions.but(faction)./(_.ruleValue(r)).max

    def at(r : System) = game.figures.get(r).%(_.faction == faction)

    def present(r : System) = game.figures.get(r).exists(_.faction == faction)

    def pooled(p : Piece) = reserve.$./(_.piece).count(p)

    def pool(p : Piece) = reserve.$.exists(_.piece == p)

    def remove(x : ResourceRef) {
        val i = 0.until(resources.num).reverse.%(i => resources(i) == x.resource && x.lock./(_ == keys(i)).|(true)).$.first

        if (i.none)
            throw new Error("resource ref " + x + " not found")

        resources = resources.updated(i.get, Nothingness)
    }

    def add(x : Resource) : Boolean = {
        if (game.available(x)) {
            if (resources.has(Nothingness).not)
                resources :+= Nothingness

            resources = resources.updated(resources.indexOf(Nothingness), x)

            true
        }
        else
            false
    }

    def stealable(x : Resource) : Boolean = {
        resources.has(x) && loyal.has(SwornGuardians).not
    }

    def pay(cost : Cost) {
        cost @@ {
            case PayResource(r, Some(lock)) =>
                val i = 0.until(resources.num).reverse.%(i => resources(i) == r && keys(i) == lock).$.first

                if (i.none)
                    throw new Error("payment " + cost + " not found")

                resources = resources.updated(i.get, Nothingness)
                spent :+= r

            case _ =>
                // println("skipping payment " + cost)
        }
    }
}
//]]

class Game(val setup : $[Faction], val options : $[Meta.O]) extends BaseGame with ContinueGame with LoggedGame {
    private implicit val game = this

    var isOver = false

    var factions : $[Faction] = setup
    var states = Map[Faction, Player]()

    var leaders : $[Leader] = $
    var lores : $[Lore] = $

    implicit val cards = new IdentityTracker[DeckCardLocation, DeckCard]
    implicit val courtiers = new IdentityTracker[CourtLocation, CourtCard]
    implicit val figures = new IdentityTracker[Region, Figure]

    val deck = cards.register(Deck, content = DeckCards.deck.%(d => factions.num == 4 || (d.strength > 1 && d.strength < 7)))
    val court = courtiers.register(CourtDeck, content = CourtCards.deck)
    val market = courtiers.register(CourtMarket)
    val discourt = courtiers.register(CourtDiscard)

    var turn : Int = 0
    var chapter : Int = 0
    var passed : Int = 0

    var lead : |[DeckCard] = None
    var zeroed : Boolean = false
    var seized : |[Faction] = None


    val markers : $[AmbitionMarker] = $(AmbitionMarker(2, 0), AmbitionMarker(3, 2), AmbitionMarker(5, 3), AmbitionMarker(4, 2), AmbitionMarker(6, 3), AmbitionMarker(9, 4), AmbitionMarker(4, 2))

    var ambitionable : $[AmbitionMarker] = $
    var declared : Map[Ambition, $[AmbitionMarker]] = Map()

    val board : BaseBoard = factions.num @@ {
        case 4 => Board4Mixup1
        case 3 => Board3Frontiers
    }

    val systems = board.systems

    def availableNum(r : Resource) = 5 - factions./(_.resources.count(r)).sum - factions./(_.spent.count(r)).sum

    def available(r : Resource) = availableNum(r) > 0

    board.systems.foreach(r => figures.register(r))

    court.$./(c => figures.register(Influence(c)))

    implicit class FactionEx(f : Color) {
        def log(s : Any*) { if (logging) game.log((f +: s.$) : _*) }
    }

    def at(c : System) = figures.get(c)

    def freeSlots(c : System) = board.slots(c) - figures.get(c).%(_.piece.is[Building]).num

    implicit def descCard(g : Game, d : DeckCard) = d.img

    var current : Faction = null

    var highlightFaction : $[Faction] = $

    def viewHand(f : Faction) = f.hand./(ViewCardInfoAction(f, _))

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = {
        self.%(states.contains)./~( f =>
            actions.has(NoHand).not.??(viewHand(f))
        )
    }

    def convertForLog(s : $[Any]) : $[Any] = s./~{
        case Empty => None
        case NotInLog(_) => None
        case AltInLog(_, m) => |(m)
        case f : Faction => |(f.elem)
        case d : DeckCard => |(OnClick(d, d.elem.spn(xlo.pointer)))
        case c : CourtCard => |(OnClick(c, c.elem.spn(xlo.pointer)))
        case l : $[Any] => convertForLog(l)
        case x => |(x)
    }

    override def log(s : Any*) {
        super.log(convertForLog(s.$) : _*)
    }

    def build(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + BuildMainAction(f, x, repeat).as("Build".styled(f), x)(group)
            .!{
                val ll = systems.%(f.present)
                val bb = ll.%(c => freeSlots(c) > 0)
                val ss = ll.%(f.at(_).hasA(Starport)).%(s => f.at(s).exists(u => u.piece == Starport && u.faction == f && f.built.has(u).not))
                (bb.none || (f.pool(City).not && f.pool(Starport).not)) && ss.none
            }

        if (f.loyal.has(MiningInterest)) {
            + ManufactureMainAction(f, x, repeat).as("Manufacture".styled(f), x)(group)
        }

        if (f.loyal.has(ShippingInterest)) {
            + SynthesizeMainAction(f, x, repeat).as("Synthesize".styled(f), x)(group)
        }

        if (f.loyal.has(PrisonWardens) && f.captives.any) {
            + PressgangMainAction(f, x, repeat).as("Press Gang".styled(f), x)(group)
        }
    }

    def repair(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + RepairMainAction(f, x, repeat).as("Repair".styled(f), x)(group).!(f.damaged.none)
    }

    def move(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + MoveMainAction(f, x, repeat).as("Move".styled(f), x)(group)
    }

    def battle(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + BattleMainAction(f, x, repeat).as("Battle".styled(f), x)(group)

        val limit = f.resources.count(Weapon) + f.loyal.of[GuildCard].count(_.suit == Weapon)

        if (f.loyal.has(CourtEnforcers)) {
            val l = market.%(c => Influence(c).%(_.faction != f).use(l => l.any && l.num < limit))

            + AbductMainAction(f, l, x, repeat).as("Abduct".styled(f), x)(group).!(l.none)
        }
    }

    def secure(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + SecureMainAction(f, x, repeat).as("Secure".styled(f), x)(group).!(market.exists(c => Influence(c).$.use(l => l.%(_.faction == f).num > factions.but(f)./(e => l.%(_.faction == e).num).max)).not)
    }

    def influence(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + InfluenceMainAction(f, x, repeat).as("Influence".styled(f), x)(group).!(f.pooled(Agent) <= f.outraged.num, "no agents")

        if (f.loyal.has(PrisonWardens) && f.captives.any) {
            + ExecuteMainAction(f, x, repeat).as("Execute".styled(f), x)(group)
        }
    }

    def tax(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + TaxMainAction(f, x, repeat).as("Tax".styled(f), x)(group)

        if (f.loyal.has(ElderBroker) && systems.exists(r => f.rules(r) && factions.but(f).exists(e => e.at(r).cities.any))) {
            + TradeMainAction(f, x, repeat).as("Trade".styled(f), x)(group)
        }
    }

    def loggedPerform(action : Action, soft : Void) : Continue = {
        // println("> " + action)

        val c = performInternal(action, soft)

        highlightFaction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a) => a./(_.faction)
            case _ => Nil
        }

        // println("< " + c)

        c
    }

    def performInternal(a : Action, soft : Void) : Continue = {
        implicit val action = a

        action match {
            // INIT
            case StartAction(version) =>
                log("HRF".hl, "version", gaming.version.hlb)
                log("Arcs: Conflict and Collapse in the Reach".hlb.styled(styles.title))

                if (version != gaming.version)
                    log("Saved game version", version.hlb)

                options.foreach { o =>
                    log(o.group, o.valueOn)
                }

                setup.foreach { f => states += f -> new Player(f) }

                Random[Faction](setup, StartingInitiativeAction(_))

            case StartingInitiativeAction(f) =>
                current = f

                factions = factions.dropWhile(_ != current) ++ factions.takeWhile(_ != current)

                f.log("randomly took initiative")

                log("Play order", factions.comma)

                CourtSetupAction

            case CourtSetupAction =>
                ShuffleCourtDiscardAction(ReplenishMarketAction(FactionsSetupAction))

            case ShuffleCourtDiscardAction(then) =>
                discourt --> court

                if (chapter > 0)
                    log("The court discard was added to the court deck")

                ShuffleCourtDeckAction(then)

            case ShuffleCourtDeckAction(then) =>
                Shuffle[CourtCard](court, ShuffledCourtDeckAction(_, then))

            case ShuffledCourtDeckAction(l, then) =>
                court --> l --> court

                log("The court deck was shuffled")

                then

            case ReplenishMarketAction(then) =>
                while (market.num < 4 && court.any) {
                    court.take(1) --> market

                    if (chapter > 0)
                        log("A card was added to the market")
                }

                if (chapter == 0)
                    log("Cards were added to the market")

                then

            case FactionsSetupAction =>
                if (options.has(LeadersAndLorePreset1))
                    Shuffle2[Leader, Lore](Leaders.preset1, Lores.preset1, (l1, l2) => LeadersLoresShuffledAction(l1, l2))
                else
                    BaseFactionsSetupAction

            case LeadersLoresShuffledAction(l1, l2) =>
                leaders = l1.take(factions.num + 1)
                lores = l2.take(factions.num + 1)

                log("Leaders".hh, "and", "Lores".hh, "were shuffled")

                leaders.foreach { l =>
                    log("Drew", l)
                }

                lores.foreach { l =>
                    log("Drew", l)
                }

                DraftNextAction(factions.last)

            case DraftNextAction(f) =>
                if (leaders.num <= 1 && lores.num <= 1)
                    Milestone(LeadersFactionsSetupAction)
                else {
                    val next = DraftNextAction((factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f)).last)

                    implicit val convert = (l : Either[Leader, Lore]) => l @@ {
                        case Left(l) => l.img
                        case Right(l) => l.img
                    }

                    YYSelectObjectsAction(f, leaders./(Left(_)) ++ lores./(Right(_)))
                        .withGroup("Leaders and Lores".hl)
                        .withSplit($(leaders.num))
                        .withRule({
                            case Left(l) => f.leader.none
                            case Right(l) => f.lores.none
                        })
                        .withThen({
                            case Left(l) => AssignLeaderAction(f, l, next)
                            case Right(l) => AssignLoreAction(f, l, next)
                        })({
                            case Left(l) => "Take " ~ l.elem
                            case Right(l) => "Take " ~ l.elem
                        })("Take")
                }

            case AssignLeaderAction(f, l, then) =>
                leaders :-= l

                f.leader = |(l)

                f.log("took", l)

                then

            case AssignLoreAction(f, l, then) =>
                lores :-= l

                f.lores :+= l

                f.log("took", l)

                then

            case LeadersFactionsSetupAction =>
                factions.lazyZip(board.starting).foreach { case (f, (a, b, cc)) =>
                    val leader = f.leader.get

                    leader.setupA.foreach { p =>
                        f.reserve --> p --> a
                    }

                    f.log("placed", leader.setupA./(_.of(f)).comma, "in", a)

                    leader.setupB.foreach { p =>
                        f.reserve --> p --> b
                    }

                    f.log("placed", leader.setupB./(_.of(f)).comma, "in", b)

                    cc.foreach { c =>
                        leader.setupC.foreach { p =>
                            f.reserve --> p --> c
                        }

                        f.log("placed", leader.setupC./(_.of(f)).comma, "in", c)
                    }

                    f.resources = leader.resources

                    f.log("took", leader.resources.lift(0), "and", leader.resources.lift(1))
                }

                StartChapterAction

            case BaseFactionsSetupAction =>
                factions.lazyZip(board.starting).foreach { case (f, (city, port, fleets)) =>
                    f.reserve --> City.of(f) --> city
                    f.reserve --> Ship.of(f) --> city
                    f.reserve --> Ship.of(f) --> city
                    f.reserve --> Ship.of(f) --> city
                    f.log("placed", City.of(f), "and", Ship.sof(f), "in", city)

                    f.reserve --> Starport.of(f) --> port
                    f.reserve --> Ship.of(f) --> port
                    f.reserve --> Ship.of(f) --> port
                    f.reserve --> Ship.of(f) --> port
                    f.log("placed", Starport.of(f), "and", Ship.sof(f), "in", port)

                    fleets.foreach { fleet =>
                        f.reserve --> Ship.of(f) --> fleet
                        f.reserve --> Ship.of(f) --> fleet
                        f.log("placed", Ship.sof(f), "in", fleet)
                    }

                    f.resources :+= board.resource(city)
                    f.resources :+= board.resource(port)

                    f.log("took", board.resource(city), "and", board.resource(port))
                }

                StartChapterAction

            // ADJUST
            case AdjustResourcesAction(f, then) =>
                Force(AdjustingResourcesAction(f, f.resources, then))

            case MultiAdjustResourcesAction(Nil, then) =>
                then

            case MultiAdjustResourcesAction(l, then) =>
                l.foreach { _.adjust = true }

                ContinueMultiAdjustResourcesAction(then)

            case ContinueMultiAdjustResourcesAction(then) =>
                if (factions.exists(_.adjust))
                    MultiAsk(factions.%(_.adjust)./~(f => game.performInternal(AdjustingResourcesAction(f, f.resources, ContinueMultiAdjustResourcesAction(then)), NoVoid).as[Ask]))
                else
                    Milestone(then)

            case AdjustingResourcesAction(f, ll, then) =>
                val kk = f.resourceSlots

                val keys = f.keys.take(kk)./(n => Image("keys-" + n, styles.token3x).spn(styles.card0)(styles.circle)) ~ (f.resources.num - kk).times(Image("discard-resource", styles.token3x).spn(styles.card0)(styles.circle))

                val l = ll ++ (kk - ll.num).times(Nothingness)

                implicit val convert = (r : Resource) => Image(r.name, styles.token3x)

                if (l.but(Nothingness).any)
                    XXSelectObjectsAction(f, l)
                        .withGroup("Adjust resources" ~ Break ~ keys)
                        .withSplit($(l.num))
                        .withBreak({
                            case 0 => Empty
                            case _ => HorizontalBreak
                        })
                        .withRule(rule => rule.num(2).all(ss => ss.num < 2 || ss(0) != ss(1)))
                        .withAutoIndex(ii => AdjustingResourcesAction(f, 0.until(l.num)./{
                            case i if i == ii(0) => l(ii(1))
                            case i if i == ii(1) => l(ii(0))
                            case i => l(i)
                        }, then))
                        .withExtra($(ReorderResourcesAction(f, l, then).as("Done").!(l.drop(kk).but(Nothingness).any && l.take(kk).has(Nothingness)), ExplodeReorderResourcesAction(f, kk, l, then)))
                        .ask
                else
                    NoAsk(f)(ReorderResourcesAction(f, f.resources, then))

            case ReorderResourcesAction(f, l, then) =>
                if (l != f.resources) {
                    f.resources = l

                    f.log("reordered resources")
                }

                f.resources.drop(f.resourceSlots).but(Nothingness).foreach { r =>
                    f.log("discarded", r)
                }

                f.resources = f.resources.take(f.resourceSlots)

                f.adjust = false

                then

//[[ GREENER
            // COURT
            case FillSlotsMainAction(f, r, then) =>
                if (f.resources.has(Nothingness)) {
                    if (f.add(r))
                        FillSlotsMainAction(f, r, then)
                    else
                    if (factions.but(f).exists(_.stealable(r)))
                        StealResourceMainAction(f, |(r), $(), FillSlotsMainAction(f, r, then))
                    else
                        then
                }
                else
                    then

            case StealResourceMainAction(f, x, extra, then) =>
                Ask(f).group("Steal", x.|("Resource"))
                    .some(factions.but(f).%(e => x./(e.resources.has).|(e.resources.but(Nothingness).any))) { e =>
                        e.resources.lazyZip(e.keys).toList.%<(_ != Nothingness).%<(r => x.but(r).none)./ { case (x, k) =>
                            StealResourceAction(f, e, x, k, then).as(ResourceRef(x, |(k)))
                                .!(e.loyal.has(SwornGuardians))
                        }
                    }
                    .needOk
                    .add(extra)

            case StealResourceAction(f, e, x, k, then) =>
                e.remove(ResourceRef(x, |(k)))

                if (f.add(x)) {
                    f.log("stole", ResourceRef(x, |(k)))

                    AdjustResourcesAction(f, then)
                }
                else
                    then

            case StealGuildCardMainAction(f, alt, then) =>
                Ask(f).group("Steal".hl, "a", "Guild Card".hh)
                    .some(factions.but(f))(e => e.loyal.of[GuildCard]./ { c =>
                        StealGuildCardAction(f, e, c, then).as(c, "from", e)
                            .!(e.loyal.has(SwornGuardians) && c != SwornGuardians)
                    })
                    .add(alt)

            case StealGuildCardAction(f, e, c, then) =>
                e.loyal --> c --> f.loyal

                f.log("stole", c)

                GainCourtCardAction(f, c, |(e), then)

            case ReserveCardMainAction(f, c, l, then) =>
                Ask(f).group("Take played card")
                    .each(l)(d => ReserveCardAction(f, c, d, then).view(d)(_.img))
                    .cancel

            case ReserveCardAction(f, c, d, then) =>
                f.log("reserved", d, "with", c)

                f.loyal --> c --> f.discardAfterRound

                f.taking :+= d

                then

            case ShipAtEachGateMainAction(f, then) =>
                val ss = systems.%(_.symbol == Gate)
                val pp = min(ss.num, f.pooled(Ship))

                Ask(f).group("Place", Ship.of(f), "at gates")
                    .each(ss.combinations(pp).$)(l => ShipsInSystemsAction(f, l, then).as(l.comma))
                    .needOk
                    .cancel

            case ShipsInSystemsAction(f, l, then) =>
                l.foreach { r =>
                    f.reserve --> Ship --> r

                    f.log("placed", Ship.of(f), "in", r)
                }

                then

            case ShipsInSystemMainAction(f, l, then) =>
                Ask(f).group("Place", min(3, f.pooled(Ship)).hlb, Ship.sof(f), "in")
                    .each(l)(r => ShipsInSystemAction(f, r, then).as(r))
                    .needOk
                    .cancel

            case ShipsInSystemAction(f, r, then) =>
                val l = f.reserve.$.ships.take(3)

                l --> r

                f.log("placed", l, "in", r)

                then

            case PressgangMainAction(f, x, then) =>
                val done = (x == AlreadyPaid).?(AdjustResourcesAction(f, then).as("Done")).|(CancelAction)

                implicit val convert = (u : Figure, selected : Boolean) => {
                    val prefix = selected.not.??(u.faction.?./(_.short.toLowerCase + "-").|(""))
                    val suffix = selected.??("-empty")

                    Image(prefix + "agent" + suffix, styles.qship)
                }

                if (f.captives.any)
                    YYSelectObjectsAction(f, f.captives)
                        .withGroup("Pressgang captives", x)
                        .withThens(u => Resources.all.%(available)./(r => PressganAction(f, u, r, x, then).as(ResourceRef(r, None))))
                        .withExtras(done)
                else
                    Ask(f)(done)

            case PressganAction(f, u, r, x, then) =>
                f.pay(x)

                f.add(r)

                u --> u.faction.reserve

                f.log("released", u, "and took", r, x)

                PressgangMainAction(f, AlreadyPaid, then)

            case ExecuteMainAction(f, x, then) =>
                implicit val convert = (u : Figure, selected : Boolean) => {
                    val prefix = selected.not.??(u.faction.?./(_.short.toLowerCase + "-").|(""))
                    val suffix = selected.??("-empty")

                    Image(prefix + "agent" + suffix, styles.qship)
                }

                XXSelectObjectsAction(f, f.captives)
                    .withGroup("Execute captives", x)
                    .withThen(l => ExecuteAction(f, l, x, then))(l => ("Execute", l))
                    .withExtras(CancelAction)

            case ExecuteAction(f, l, x, then) =>
                f.pay(x)

                f.captives --> l --> f.trophies

                f.log("executed", l, x)

                then

            case AbductMainAction(f, l, x, then) =>
                Ask(f).group("Abduct".hl, "Agents", x)
                    .each(market)(c => AbductAction(f, c, x, then).as(c).!(l.has(c).not))
                    .cancel

            case AbductAction(f, c, x, then) =>
                f.pay(x)

                val l = Influence(c).%(_.faction != f)

                l --> f.captives

                f.log("abducted", l, x)

                then

            case FarseersMainAction(f, e, then) =>
                YYSelectObjectsAction(f, e.hand)
                    .withGroup(Farseers, "look at", e, "cards")
                    .withThen(d => FarseersAction(f, e, d, then))(d => ("Take", d, "from", e))("Take a card from", e)
                    .withExtras(then.as("Skip"))

            case FarseersAction(f, e, d, then) =>
                e.hand --> d --> f.hand

                f.log("took a card from", e, "with", Farseers)

                YYSelectObjectsAction(f, f.hand)
                    .withGroup(Farseers, "give back", e, "a card")
                    .withThen(d => FarseersBackAction(f, e, d, then))(d => ("Give", d, "to", e))("Give a card back to", e)
                    .withExtras(NoHand)

            case FarseersBackAction(f, e, d, then) =>
                f.hand --> d --> e.hand

                f.log("gave back a card to", e)

                then

            case FarseersRedrawMainAction(f, then) =>
                XXSelectObjectsAction(f, f.hand)
                    .withGroup(Farseers, "discard cards to redraw")
                    .withThen(l => FarseersRedrawAction(f, l, then))(l => ("Discard to draw", (l.num + 1).cards))
                    .withExtras(NoHand, FarseersRedrawAction(f, $, then).as("Discard", Farseers, "to draw", 1.cards), CancelAction)

            case FarseersRedrawAction(f, l, then) =>
                f.hand --> l --> deck

                deck.take(l.num + 1) --> f.hand

                f.log("discarded", l.num.cards, "and drew", (l.num + 1).cards)

                then

            case FenceResourceAction(f, r, x, then) =>
                f.pay(x)

                if (f.add(r)) {
                    f.log("fenced", Relic, x)

                    AdjustResourcesAction(f, then)
                }
                else
                    then

            case GainResourcesAction(f, l, then) =>
                var any = false

                l.foreach { r =>
                    if (f.add(r)) {
                        any = true

                        f.log("gained", r)
                    }
                }

                if (any)
                    AdjustResourcesAction(f, then)
                else
                    then
//]]

//[[ PINKER
            // ON SECURE
            case GainCourtCardAction(f, c @ CallToAction, e, then) =>
                deck.take(1) --> f.hand

                f.log("drew", 1.cards)

                DiscardCourtCardAction(f, c, then)

            case GainCourtCardAction(f, c @ PopulistDemands, e, then) =>
                val next = DiscardCourtCardAction(f, c, then)

                if (ambitionable.any)
                    Ask(f).group("Declare Ambition".hl, "with", c)
                        .add(DeclareAmbitionAction(f, Tycoon,  false, next).use(a => a.as(a.ambition)))
                        .add(DeclareAmbitionAction(f, Tyrant,  false, next).use(a => a.as(a.ambition)))
                        .add(DeclareAmbitionAction(f, Warlord, false, next).use(a => a.as(a.ambition)))
                        .add(DeclareAmbitionAction(f, Keeper,  false, next).use(a => a.as(a.ambition)))
                        .add(DeclareAmbitionAction(f, Empath,  false, next).use(a => a.as(a.ambition)))
                        .add(next.as("Skip"))
                else
                    Ask(f).group("Declare Ambition".hl, "with", c).add(next.as("No Ambition Markers")).needOk

            case GainCourtCardAction(f, c @ MassUprising, e, then) =>
                val next = DiscardCourtCardAction(f, c, then)

                val nn = systems./(_.cluster).distinct
                val pp = min(4, f.pooled(Ship))

                if (pp > 0)
                    Ask(f).group("Place", Ship.sof(f), "in a cluster")
                        .some(nn)(n => systems.%(_.cluster == n).combinations(pp)./(l => ShipsInSystemsAction(f, l, next).as(l.comma)))
                        .needOk
                        .cancel
                else
                    NoAsk(f)(next)

            case GainCourtCardAction(f, c @ GuildStruggle, e, then) =>
                val next = ShuffleCourtDiscardAction(DiscardCourtCardAction(f, c, then))

                StealGuildCardMainAction(f, $(next.as("Skip")), next)

            case GainCourtCardAction(f, c @ SongOfFreedom, e, then) =>
                val next = BuryCourtCardAction(f, c, ShuffleCourtDeckAction(then))

                val l = systems.%(f.rules)

                Ask(f).group(SongOfFreedom, "frees a", "City".hl)
                    .some(l)(r => factions./~(_.at(r).cities)./(u => FreeCityAction(f, r, u, next).as(u, "in", r)))
                    .skip(next)

            case FreeCityAction(f, r, u, then) =>
                u --> u.faction.reserve

                f.log("freed", u, "in", r)

                AdjustResourcesAction(u.faction.as[Faction].get, FreeCitySeazeAskAction(f, then))

            case FreeCitySeazeAskAction(f, then) =>
                if (seized.none && factions(0) != f)
                    Ask(f).group("Initiative")
                        .add(FreeCitySeazeAction(f, then).as("Seize".hl))
                        .skip(then)
                else
                    NoAsk(f)(then)

            case FreeCitySeazeAction(f, then) =>
                seized = |(f)

                f.log("seized the initative")

                then

            case GainCourtCardAction(f, c @ OutrageSpreads, e, then) =>
                val next = DiscardCourtCardAction(f, c, then)

                Ask(f).group(c)
                    .each(Resources.all)(r => OutrageSpreadsAction(f, r, next).as(ResourceRef(r, None)))
                    .skip(next)

            case OutrageSpreadsAction(f, r, then) =>
                val l = factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f)

                l.foldLeft(then)((q, e) => OutrageAction(e, r, q))

            case GainCourtCardAction(f, c @ SwornGuardians, Some(e), then) =>
                BuryCourtCardAction(f, c, then)
//]]

            // COURT CARDS
            case GainCourtCardAction(f, c : GuildCard, e, then) =>
                then

            case DiscardCourtCardAction(f, c, then) =>
                f.log("discarded", c)

                f.loyal --> c --> discourt

                then

            case BuryCourtCardAction(f, c, then) =>
                f.log("buried", c)

                f.loyal --> c --> court

                then

            case UsedEffectCardAction(f, c, then) =>
                f.used :+= c

                then

            // TAX
            case TaxMainAction(f, x, then) =>
                val g = "Tax".hl

                Ask(f).group(g)
                    .some(systems)(s => f.at(s).cities./(c => TaxLoyalAction(f, x, s, c, then).as(c, "in", s, |(board.resource(s)).%(available)./(r => ("for", r, Image(r.name, styles.token))))(g).!(f.taxed.has(c), "taxed")))
                    .some(systems.%(f.rules))(s => factions.but(f)./~(e => e.at(s).cities./(c => TaxRivalAction(f, x, s, e, c, then).as(c, "in", s, |(board.resource(s)).%(available)./(r => ("for", r, Image(r.name, styles.token))))(g).!(f.taxed.has(c), "taxed"))))
                    .cancel

            case TaxLoyalAction(f, x, r, c, then) =>
                f.pay(x)

                f.log("taxed", c, "in", r, x.elemLog)

                f.taxed :+= c

                if (f.add(board.resource(r))) {
                    f.log("gained", board.resource(r))

                    AdjustResourcesAction(f, then)
                }
                else
                    then

            case TaxRivalAction(f, x, r, e, c, then) =>
                f.pay(x)

                f.log("taxed", c, "in", r, x.elemLog)

                f.taxed :+= c

                if (e.pooled(Agent) > e.outraged.num) {
                    e.reserve --> Agent --> f.captives

                    f.log("captured", Agent.of(e))
                }

                if (f.add(board.resource(r))) {
                    f.log("gained", board.resource(r))

                    AdjustResourcesAction(f, then)
                }
                else
                    then

            // MOVE
            case MoveMainAction(f, x, then) =>
                val pp = systems.%(f.at(_).hasA(Starport))
                val ss = systems./(r => r -> f.at(r).ships).%>(_.any).toMap

                Ask(f).group("Move from")
                    .each(ss.keys.% (pp.has).$)(r => MoveFromAction(f, r, ss(r), true,  x, CancelAction, then).as(r))
                    .each(ss.keys.%!(pp.has).$)(r => MoveFromAction(f, r, ss(r), false, x, CancelAction, then).as(r))
                    .cancel

            case MoveFromAction(f, r, l, cascade, x, alt, then) =>
                Ask(f).group("Move from", r, "to")
                    .each(board.connected(r))(d => MoveToAction(f, r, d, l, cascade && d.symbol == Gate && factions.but(f).exists(_.rules(d)).not, x, then).as(d))
                    .add(alt)

            case MoveToAction(f, r, d, l, cascade, x, then) =>
                val n = l.num
                val (damaged, fresh) = l.partition(f.damaged.has)
                val combinations = 1.to(n)./~(k => max(0, k - fresh.num).to(min(k, damaged.num))./(i => fresh.take(k - i) ++ damaged.take(i)))

                implicit val convert = (u : Figure, k : Int) => {
                    val status = u.faction.as[Faction].?(_.damaged.has(u)).??(1) + k

                    val prefix = (k < 2).??(u.faction.?./(_.short.toLowerCase + "-").|(""))
                    val suffix = (k == 1).??("-damaged") + (k == 2).??("-empty")

                    u.piece match {
                        case City => Image(prefix + "city" + suffix, styles.qbuilding)
                        case Starport => Image(prefix + "starport" + suffix, styles.qbuilding)
                        case Ship => Image(prefix + "ship" + suffix, styles.qship)
                    }
                }

                Ask(f).group("Move from", r, "to", d)
                    .each(combinations)(l => MoveListAction(f, r, d, l, cascade, x, then).as(l./(u => convert(u, damaged.has(u).??(1)))))
                    .cancel

            case MoveListAction(f, r, d, l, cascade, x, then) =>
                f.pay(x)

                f.log("moved", l.comma, "from", r, "to", d, x.elemLog)

                l --> d

                if (cascade)
                    Force(MoveFromAction(f, d, l, true, NoCost, then.as("Done"), then))
                else
                    Force(then)

            // BATTLE
            case BattleMainAction(f, x, then) =>
                Ask(f).group(f, "battles in", x)
                    .each(systems.%(f.at(_).hasA(Ship)).%(r => factions.but(f).exists(_.present(r))))(r => BattleSystemAction(f, x, r, then).as(r))
                    .cancel

            case BattleSystemAction(f, x, r, then) =>
                Ask(f).group(f, "battles in", r, x)
                    .each(factions.but(f).%(_.present(r)))(e => BattleFactionAction(f, x, r, e, then).as(e))
                    .cancel

            case BattleFactionAction(f, x, r, e, then) =>
                val ships = f.at(r).count(Ship) + (r.symbol == Gate).??(f.loyal.has(Gatekeepers).??(2))
                val canRaid = e.at(r).hasBuilding || systems.exists(e.at(_).hasBuilding).not
                val combinations : $[(Int, Int, Int)] = 1.to(ships).reverse./~(n => 0.to(min(canRaid.??(6), n))./~(raid => 0.to(min(6, n - raid))./~(assault => |(n - raid - assault).%(_ <= 6)./((_, assault, raid)))))

                Ask(f).group(f, "battles", e, "in", r, x)
                    .each(combinations){ case (skirmish, assault, raid) => BattleDiceAction(f, x, r, e, skirmish, assault, raid, then).as(skirmish.times(Image("skirmish-die", styles.token)), assault.times(Image("assault-die", styles.token)), raid.times(Image("raid-die", styles.token))) }
                    .cancel

            case BattleDiceAction(f, x, r, e, skirmish, assault, raid, then) =>
                f.pay(x)

                f.log("battled", e, "in", r, x.elemLog)

                Roll3[$[BattleResult], $[BattleResult], $[BattleResult]](skirmish.times(Skirmish.die), assault.times(Assault.die), raid.times(Raid.die), (l1, l2, l3) => BattleRolledAction(f, r, e, l1, l2, l3, then))

            case BattleRolledAction(f, r, e, l1, l2, l3, then) =>
                f.log("rolled",
                    l1./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)) ~
                    l2./(x => Image("assault-die-" + (Assault.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)) ~
                    l3./(x => Image("raid-die-" + (Raid.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token))
                )

                val next = BattleProcessAction(f, r, e, (l1 ++ l2 ++ l3).flatten, then)

                if (l1.any && f.loyal.has(Skirmishers)) {
                    val limit = f.resources.count(Weapon) + f.loyal.of[GuildCard].count(_.suit == Weapon)
                    val miss = $()
                    val hit = $(HitShip)
                    val misses = l1.count(miss)
                    val hits = l1.count(hit)
                    val rerollable = 1.to(min(limit, misses)).reverse./(_.times(miss)) ++ 1.to(min(limit, hits))./(_.times(hit))

                    Ask(f).group(Skirmishers)
                        .each(rerollable)(q => SkirmishersAction(f, r, e, (l1.diff(q) ++ l2 ++ l3).flatten, q, then).as("Reroll", q./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token))))
                        .skip(next)
                }
                else
                    NoAsk(f)(next)

            case SkirmishersAction(f, r, e, l, q, then) =>
                Roll[$[BattleResult]](q.num.times(Skirmish.die), n => SkirmishersRolledAction(f, r, e, l, q, n, then))

            case SkirmishersRolledAction(f, r, e, l, q, n, then) =>
                f.log("rerolled",
                    q./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)),
                    "to",
                    n./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)),
                    "with", Skirmishers)

                BattleProcessAction(f, r, e, l ++ n.flatten, then)

            case BattleProcessAction(f, r, e, l, then) =>
                val sd = l.count(OwnDamage)
                val ic = l.has(Intersept).??(e.at(r).ships.diff(e.damaged).num)
                val hs = l.count(HitShip)
                val bb = l.count(HitBuilding)
                val rd = l.count(RaidKey)

                if (sd > 0)
                    f.log("suffered", sd.hit)

                if (ic > 0)
                    f.log("got intercepted for", ic.hit)

                if (hs > 0)
                    f.log("dealt", hs.hit)

                if (bb > 0)
                    f.log("bombarded for", bb.hit)

                if (rd > 0)
                    f.log("raided with", rd.hl, "keys")

                AssignHitsAction(f, r, e, f, f.at(r).ships, sd + ic, 0, 0, AssignHitsAction(f, r, f, e, e.at(r), hs, bb, rd, then))

            case BattleRaidAction(f, r, e, raid, then) =>
                if (f.at(r).ships.any && raid > 0)
                    Ask(f).group("Raid", e, "with", raid.times(Image("raid-key", styles.token)).merge)
                        .each(e.resources.lazyZip(e.keys).toList.%<(_ != Nothingness)) { case (x, k) =>
                            BattleRaidResourceAction(f, e, x, k, BattleRaidAction(f, r, e, raid - k, then)).as(ResourceRef(x, |(k)))
                                .!(k > raid)
                                .!(e.loyal.has(SwornGuardians))
                        }
                        .each(e.loyal.of[GuildCard]) { c =>
                            BattleRaidCourtCardAction(f, e, c, BattleRaidAction(f, r, e, raid - c.keys, then)).as(c)
                                .!(c.keys > raid)
                                .!(e.loyal.has(SwornGuardians) && c != SwornGuardians)
                        }
                        .needOk
                        .done(then)
                else
                    NoAsk(f)(then)

            case BattleRaidResourceAction(f, e, x, k, then) =>
                e.remove(ResourceRef(x, |(k)))

                if (f.add(x)) {
                    f.log("stole", ResourceRef(x, |(k)))

                    AdjustResourcesAction(f, then)
                }
                else
                    then

            case BattleRaidCourtCardAction(f, e, c, then) =>
                e.loyal --> c --> f.loyal

                f.log("stole", c)

                GainCourtCardAction(f, c, |(e), then)

            case AssignHitsAction(self, r, f, e, l, hits, bombardments, raid, then) =>
                implicit val convert = (u : Figure, k : Int) => {
                    val status = u.faction.as[Faction].?(_.damaged.has(u)).??(1) + k

                    val prefix = (status < 2).??(u.faction.?./(_.short.toLowerCase + "-").|(""))
                    val suffix = (status == 1).??("-damaged") + (status == 2).??("-empty")

                    u.piece match {
                        case City => Image(prefix + "city" + suffix, styles.qbuilding)
                        case Starport => Image(prefix + "starport" + suffix, styles.qbuilding)
                        case Ship => Image(prefix + "ship" + suffix, styles.qship)
                    }
                }

                var h = hits
                var b = bombardments

                val ships = l.ships./(u => u.faction.as[Faction].?(_.damaged.has(u)).?(1).|(2)).sum
                val buildings = l.buildings./(u => u.faction.as[Faction].?(_.damaged.has(u)).?(1).|(2)).sum

                if (h > ships) {
                    b = b + (h - ships)
                    h = ships
                }

                if (b > buildings)
                    b = buildings

                if (h + b > 0)
                    XXSelectObjectsAction(self, l.sortBy(_.piece.is[Building].??(1)))
                        .withGroup(f, "dealt", hits.hit, (bombardments > 0).?(("and", bombardments.hlb), "Bombardment".s(bombardments).styled(styles.hit)), "to", e, "in", r,
                            $(convert(Figure(e, City, 0), 1), convert(Figure(e, Starport, 0), 1), convert(Figure(e, Ship, 0), 1), convert(Figure(e, City, 0), 2), convert(Figure(e, Starport, 0), 2), convert(Figure(e, Ship, 0), 2)).merge.div(xstyles.displayNone))
                        .withRule(_.num(h + b).all(d => d.ships.num <= h && d.buildings.num <= b))
                        .withMultipleSelects(u => 2 - e.damaged.has(u).??(1))
                        .withThen(d => DealHitsAction(self, r, f, e, d, raid, then))(_ => "Damage".hl)
                        .ask
                else
                    NoAsk(self)(then)

            case DealHitsAction(self, r, f, e, l, k, then) =>
                val dd = e.damaged ++ l
                val destroyed = dd.diff(dd.distinct)

                if (destroyed.any)
                    f.log("destroyed", destroyed.comma)

                e.damaged = e.damaged.diff(destroyed)

                val damaged = l.distinct.diff(destroyed)

                if (damaged.any)
                    f.log("damaged", damaged.comma)

                e.damaged ++= damaged

                destroyed --> f.trophies

                var next = then

                if (k > 0 && f.at(r).ships.any)
                    next = BattleRaidAction(f, r, e, k, next)

                destroyed.cities.foreach { u =>
                    next = RansackMainAction(f, e, next)
                    next = OutrageAction(f, board.resource(r), next)
                }

                next

            case OutrageAction(f, r, then) =>
                if (f.outraged.has(r).not) {
                    f.outraged :+= r

                    f.log("suffered", r, "outrage")
                }

                val discarded = f.resources.%(_ == r)

                if (discarded.any)
                    f.log("discarded", discarded./(_.elem).comma)

                f.resources = f.resources./{
                    case x if x == r => Nothingness
                    case x => x
                }

                f.loyal.of[GuildCard].notOf[LoyalGuild].foreach { c =>
                    if (c.suit == r) {
                        f.loyal --> c --> discourt

                        f.log("discarded", c)
                    }
                }

                then

            case RansackMainAction(f, e, then) =>
                Ask(f).group("Ransack".hl)
                    .each(market.%(c => Influence(c).exists(_.faction == e)))(c => RansackAction(f, c, then).as(c))
                    .needOk
                    .bail(then)

            case RansackAction(f, c, then) =>
                market --> c --> f.loyal

                f.log("ransacked", c)

                Influence(c).foreach { u =>
                    if (u.faction == f)
                        u --> f.reserve
                    else {
                        u --> f.trophies

                        f.log("executed", u)
                    }
                }

                GainCourtCardAction(f, c, None, ReplenishMarketAction(then))

            // BUILD
            case BuildMainAction(f, x, then) =>
                val ll = systems.%(f.present)
                val bb = ll.%(c => freeSlots(c) > 0)
                val ss = ll.%(f.at(_).hasA(Starport))
                val prefix = f.short + "-"
                def suffix(s : System) = factions.but(f).exists(_.rules(s)).??("-damaged")

                Ask(f)
                    .group("Build".hl)
                    .each(f.pool(City).??(bb))(s => BuildCityAction(f, x, s, then).as(City.of(f), Image(prefix + "city" + suffix(s), styles.qbuilding), "in", s))
                    .each(f.pool(Starport).??(bb))(s => BuildStarportAction(f, x, s, then).as(Starport.of(f), Image(prefix + "starport" + suffix(s), styles.qbuilding), "in", s))
                    .each(f.pool(Ship).??(ss))(s => BuildShipAction(f, x, s, then).as(Ship.of(f), Image(prefix + "ship" + suffix(s), styles.qship), "in", s).!(f.at(s).exists(u => u.piece == Starport && u.faction == f && f.built.has(u).not).not, "built"))
                    .cancel

            case BuildCityAction(f, x, r, then) =>
                f.pay(x)

                val u = f.reserve --> City.of(f)

                if (factions.but(f).exists(_.rules(r)))
                    f.damaged :+= u

                u --> r

                f.log("built", u, "in", r, x.elemLog)

                then

            case BuildStarportAction(f, x, r, then) =>
                f.pay(x)

                val u = f.reserve --> Starport.of(f)

                if (factions.but(f).exists(_.rules(r)))
                    f.damaged :+= u

                u --> r

                f.log("built", u, "in", r, x.elemLog)

                then

            case BuildShipAction(f, x, r, then) =>
                f.pay(x)

                val u = f.reserve --> Ship.of(f)

                if (factions.but(f).exists(_.rules(r)))
                    f.damaged :+= u

                u --> r

                f.built :+= f.at(r).%(u => u.piece == Starport && f.built.has(u).not).first.get

                f.log("built", u, "in", r, x.elemLog)

                then

            // REPAIR
            case RepairMainAction(f, x, then) =>
                val ll = systems.%(f.present)

                Ask(f)
                    .group("Repair".hl)
                    .some(ll)(r => f.at(r).intersect(f.damaged)./(u => RepairAction(f, x, r, u, then).as(u.piece.of(f), Image(u.faction.short + "-" + u.piece.name + "-damaged", u.piece.is[Building].?(styles.qbuilding).|(styles.qship)), "in", r)))
                    .cancel

            case RepairAction(f, x, r, u, then) =>
                f.pay(x)

                f.damaged :-= u

                f.log("repaired", u, "in", r, x.elemLog)

                then

            // INFLUENCE
            case InfluenceMainAction(f, x, then) =>
                Ask(f).group("Influence".hl)
                    .each(market)(c => InfluenceAction(f, x, c, then).as(c))
                    .cancel

            case InfluenceAction(f, x, c, then) =>
                f.pay(x)

                f.reserve --> Agent --> Influence(c)

                f.log("influenced", c, x.elemLog)

                then

            case SecureMainAction(f, x, then) =>
                Ask(f).group("Secure".hl)
                    .each(market)(c => SecureAction(f, x, c, then).as(c)
                        .!(Influence(c).$.use(l => l.%(_.faction == f).num <= factions.but(f)./(e => l.%(_.faction == e).num).max))
                        .!(f.can(Paranoid) && Influence(c).%(_.faction == f).num <= 1, "Paranoid")
                    )
                    .cancel

            case SecureAction(f, x, c, then) =>
                f.pay(x)

                market --> c --> f.loyal

                f.used :+= c

                f.log("secured", c, x.elemLog)

                Influence(c).foreach { u =>
                    if (u.faction == f)
                        u --> f.reserve
                    else {
                        u --> f.captives

                        f.log("captured", u)
                    }
                }

                GainCourtCardAction(f, c, None, ReplenishMarketAction(then))

            // MANUFACTURE
            case ManufactureMainAction(f, x, then) =>
                f.pay(x)

                f.log("manufactured", x.elemLog)

                if (f.add(Material)) {
                    f.log("gained", Material)

                    AdjustResourcesAction(f, then)
                }
                else
                    then

            // SYNTHESIZE
            case SynthesizeMainAction(f, x, then) =>
                f.pay(x)

                f.log("synthesize", x.elemLog)

                if (f.add(Fuel)) {
                    f.log("gained", Fuel)

                    AdjustResourcesAction(f, then)
                }
                else
                    then

            // TRADE
            case TradeMainAction(f, x, then) =>
                Ask(f).group("Trade".hl)
                    .some(factions.but(f).%(_.resources.but(Nothingness).any)) { e =>
                        e.resources.lazyZip(e.keys).toList.%<(_ != Nothingness).%<(r => systems.exists(s => e.at(s).cities.any && board.resource(s) == r && f.rules(s)))./ { case (r, k) =>
                            TakeResourceAction(f, e, r, k, GiveBackResourceMainAction(f, e, then)).as(e, ResourceRef(r, None))
                        }
                    }
                    .cancel

            case TakeResourceAction(f, e, x, k, then) =>
                e.remove(ResourceRef(x, |(k)))

                f.add(x)

                f.log("took in trade", ResourceRef(x, |(k)))

                then

            case GiveBackResourceMainAction(f, e, then) =>
                Ask(f).group("Give to", e)
                    .each(f.resources.but(Nothingness).distinct)(r => GiveBackResourceAction(f, e, r, then).as("Give back", ResourceRef(r, None)))
                    .needOk

            case GiveBackResourceAction(f, e, r, then) =>
                f.remove(ResourceRef(r, None))

                e.add(r)

                f.log("gave back", r)

                AdjustResourcesAction(f, AdjustResourcesAction(e, then))

            // WEAPON
            case AddBattleOptionAction(f, x, then) =>
                f.pay(x)

                f.anyBattle = true

                f.log("could use any card action as ", "Battle".styled(f), x.elemLog)

                then

            // TURN
            case StartChapterAction =>
                log(DoubleLine)
                log(SingleLine)
                log(DoubleLine)

                ambitionable = markers.drop(chapter).take(3).sortBy(_.high)
                declared = Map()

                chapter += 1

                log(("Chapter".hl ~ " " ~ chapter.hlb).styled(styles.title))

                Shuffle[DeckCard](deck, ShuffleDeckCardsAction(_, DealCardsAction))

            case ShuffleDeckCardsAction(l, then) =>
                l --> deck

                log("The action deck was shuffled")

                then

            case DealCardsAction =>
                factions.foreach { f =>
                    deck.$.take(6) --> f.hand

                    f.log("drew", 6.cards)
                }

                StartRoundAction

            case StartRoundAction =>
                log(DoubleLine)

                LeadMainAction(factions.first.get)

            case LeadMainAction(f) =>
                YYSelectObjectsAction(f, f.hand)
                    .withGroup(f.elem ~ " leads")
                    .withThen(LeadAction(f, _))("Play " ~ _.elem)("Play")
                    .withExtras(NoHand, PassAction(f).as("Pass".hh))

            case PassAction(f) =>
                if (f.hand.any)
                    passed += 1

                if (passed >= factions.%(_.hand.any).num) {
                    f.log("passed")

                    EndChapterAction
                }
                else {
                    factions = factions.drop(1).dropWhile(_.hand.none) ++ factions.take(1) ++ factions.drop(1).takeWhile(_.hand.none)

                    f.log("passed initative to", factions.first.get)

                    StartRoundAction
                }

            case LeadAction(f, d) =>
                passed = 0

                lead = |(d)

                f.log("led with", d)

                f.hand --> d --> f.played

                f.lead = true

                CheckAmbitionAction(f, d)

            case CheckAmbitionAction(f, d) =>
                val s = ambitionable.any.??(d.strength)

                val next = PreludeActionAction(f, d.suit, d.pips)

                Ask(f).group("Declare Ambition".hl)
                    .add(s.in(2, 7).?(DeclareAmbitionAction(f, Tycoon,  true, next))./(a => a.as(a.ambition)))
                    .add(s.in(3, 7).?(DeclareAmbitionAction(f, Tyrant,  true, next))./(a => a.as(a.ambition)))
                    .add(s.in(4, 7).?(DeclareAmbitionAction(f, Warlord, true, next))./(a => a.as(a.ambition)))
                    .add(s.in(5, 7).?(DeclareAmbitionAction(f, Keeper,  f.loyal.has(SecretOrder).not, next))./(a => a.as(a.ambition)))
                    .add(s.in(6, 7).?(DeclareAmbitionAction(f, Empath,  f.loyal.has(SecretOrder).not, next))./(a => a.as(a.ambition)))
                    .add(next.as("Skip"))

            case DeclareAmbitionAction(f, a, zero, then) =>
                f.log("declared", a, "ambition")

                declared += a -> (declared.get(a).|($) ++ $(ambitionable.last))

                ambitionable = ambitionable.dropRight(1)

                if (zero)
                    zeroed = true

                AmbitionDeclaredAction(f, a, $, then)

            case AmbitionDeclaredAction(f, a, used, then) =>
                var ask = Ask(f)

                if (f.can(Farseers) && used.has(Farseers).not)
                    ask = ask.each(factions.but(f))(e => FarseersMainAction(f, e, AmbitionDeclaredAction(f, a, used :+ Farseers, then)).as(e)(Farseers))

                if (f.can(Bold) && used.has(Bold).not)
                    ask = ask.add(BoldMainAction(f, $, AmbitionDeclaredAction(f, a, used :+ Bold, then)).as("Influence each card in court".hh)(Bold))

                ask.add(then.as("Done"))

            case BoldMainAction(f, influenced, then) =>
                Ask(f).group("Influence".hl)
                    .each(market)(c => InfluenceAction(f, NoCost, c, BoldMainAction(f, influenced :+ c, then)).as(c)
                        .!(influenced.has(c), "influenced")
                        .!(f.pooled(Agent) <= f.outraged.num, "no agents")
                    )
                    .cancelIf(influenced.none)
                    .done(influenced.any.?(then))

            case FollowAction(f) =>
                YYSelectObjectsAction(f, f.hand)
                    .withGroup(f.elem ~ " follows " ~ lead.get.elem)
                    .withThens(d => $(
                        SurpassAction(f, d).as("Surpass".styled(lead.get.suit).styled(xstyles.bold), "with", d).!(d.suit != lead.get.suit, "wrong suit").!(d.strength < lead.get.strength && zeroed.not, "low strength"),
                        CopyAction(f, d).as("Copy".styled(lead.get.suit).styled(xstyles.bold), "with", d),
                        PivotAction(f, d).as("Pivot".styled(d.suit).styled(xstyles.bold), "with", d).!(d.suit == lead.get.suit, "same suit"),
                    ))
                    .withExtras(NoHand)

            case SurpassAction(f, d) =>
                log(SingleLine)

                f.log("surpassed with", d)

                f.hand --> d --> f.played

                f.surpass = true

                if (seized.none && d.strength == 7) {
                    f.log("seized the initative")

                    seized = |(f)
                }

                CheckSeizeAction(f, PreludeActionAction(f, d.suit, d.pips))

            case CopyAction(f, d) =>
                log(SingleLine)

                f.log("copied with a card")

                f.hand --> d --> f.blind

                f.copy = true

                CheckSeizeAction(f, PreludeActionAction(f, lead.get.suit, 1))

            case PivotAction(f, d) =>
                log(SingleLine)

                f.log("pivoted with", d)

                f.hand --> d --> f.played

                f.pivot = true

                CheckSeizeAction(f, PreludeActionAction(f, d.suit, 1))

            case CheckSeizeAction(f, then) =>
                if (zeroed.not && ambitionable.any && f.played.any && f.can(GalacticBards)) {
                    val next = UsedEffectCardAction(f, GalacticBards, CheckSeizeAction(f, then))
                    val s = f.played.single.get.strength

                    Ask(f).group("Declare Ambition".hl)
                        .add(s.in(2, 7).?(DeclareAmbitionAction(f, Tycoon,  false, next))./(a => a.as(a.ambition)))
                        .add(s.in(3, 7).?(DeclareAmbitionAction(f, Tyrant,  false, next))./(a => a.as(a.ambition)))
                        .add(s.in(4, 7).?(DeclareAmbitionAction(f, Warlord, false, next))./(a => a.as(a.ambition)))
                        .add(s.in(5, 7).?(DeclareAmbitionAction(f, Keeper,  false, next))./(a => a.as(a.ambition)))
                        .add(s.in(6, 7).?(DeclareAmbitionAction(f, Empath,  false, next))./(a => a.as(a.ambition)))
                        .add(next.as("Skip"))
                }
                else
                if (seized.none && (f.hand.any || f.loyal.has(LatticeSpies))) {
                    YYSelectObjectsAction(f, f.hand)
                        .withGroup(f.elem ~ " can seize initiative")
                        .withThen(SeizeAction(f, _, then))("Seize with " ~ _.elem)("Seize")
                        .withExtras(NoHand, f.loyal.has(LatticeSpies).?(LatticeSeizeAction(f, LatticeSpies, then).as("Seize with", LatticeSpies)).|(NoHand), then.as("Skip".hh))
                }
                else
                    NoAsk(f)(then)

            case SeizeAction(f, d, then) =>
                f.log("seized initiative with a card")

                f.hand --> d --> f.blind

                seized = |(f)

                then

            case LatticeSeizeAction(f, c, then) =>
                f.log("seized initiative with", c)

                f.loyal --> c --> discourt

                seized = |(f)

                then

            case PreludeActionAction(f, s, p) =>
                implicit val ask = builder
                implicit val repeat = PreludeActionAction(f, s, p)
                implicit val group = f.elem ~ " " ~ "Prelude".hh

                f.resources.lazyZip(f.keys).foreach { (r, k) =>
                    if (r != Nothingness) {
                        val cost = PayResource(r, |(k))

                        if ((r == Material && f.outraged.has(Material).not) || f.loyal.has(LoyalEngineers)) {
                            build(f, cost)
                            repair(f, cost)
                        }

                        if ((r == Weapon && s != Aggression && f.outraged.has(Weapon).not) || f.loyal.has(LoyalMarines))
                            + AddBattleOptionAction(f, cost, repeat).as("Add", "Battle".styled(f), "option", cost)(group).!(f.anyBattle)

                        if ((r == Fuel && f.outraged.has(Fuel).not) || f.loyal.has(LoyalPilots))
                            move(f, cost)

                        if ((r == Relic && f.outraged.has(Relic).not) || f.loyal.has(LoyalKeepers))
                            secure(f, cost)

                        if ((r == Psionic && f.outraged.has(Psionic).not) || f.loyal.has(LoyalEmpaths)) {
                            factions(0).played./(_.suit)./{
                                case Administration =>
                                    tax(f, cost)
                                    repair(f, cost)
                                    influence(f, cost)
                                case Aggression =>
                                    battle(f, cost)
                                    move(f, cost)
                                    secure(f, cost)
                                case Construction =>
                                    build(f, cost)
                                    repair(f, cost)
                                case Mobilization =>
                                    move(f, cost)
                                    influence(f, cost)
                            }
                        }
                    }
                }

                if (f.can(MiningInterest))
                    + FillSlotsMainAction(f, Material, DiscardCourtCardAction(f, MiningInterest, repeat)).as("Fill up", ResourceRef(Material, None))(MiningInterest)

                if (f.can(ShippingInterest))
                    + FillSlotsMainAction(f, Fuel, DiscardCourtCardAction(f, ShippingInterest, repeat)).as("Fill up", ResourceRef(Fuel, None))(ShippingInterest)

                if (f.can(AdminUnion))
                    if (factions.exists(_.played.exists(_.suit == Administration)))
                        + ReserveCardMainAction(f, AdminUnion, factions./~(_.played.%(_.suit == Administration)), repeat).as("Take", Administration, "card")(AdminUnion)

                if (f.can(ConstructionUnion))
                    if (factions.exists(_.played.exists(_.suit == Construction)))
                        + ReserveCardMainAction(f, ConstructionUnion, factions./~(_.played.%(_.suit == Construction)), repeat).as("Take", Construction, "card")(ConstructionUnion)

                if (f.can(SpacingUnion))
                    if (factions.exists(_.played.exists(_.suit == Mobilization)))
                        + ReserveCardMainAction(f, SpacingUnion, factions./~(_.played.%(_.suit == Mobilization)), repeat).as("Take", Mobilization, "card")(SpacingUnion)

                if (f.can(ArmsUnion))
                    if (factions.exists(_.played.exists(_.suit == Aggression)))
                        + ReserveCardMainAction(f, ArmsUnion, factions./~(_.played.%(_.suit == Aggression)), repeat).as("Take", Aggression, "card")(ArmsUnion)

                if (f.can(MaterialCartel))
                    + StealResourceMainAction(f, |(Material), $(CancelAction), DiscardCourtCardAction(f, MaterialCartel, repeat)).as("Steal", ResourceRef(Material, None))(MaterialCartel)

                if (f.can(FuelCartel))
                    + StealResourceMainAction(f, |(Fuel), $(CancelAction), DiscardCourtCardAction(f, FuelCartel, repeat)).as("Steal", ResourceRef(Fuel, None))(FuelCartel)

                if (f.reserve.$.ships.any) {
                    if (f.can(Gatekeepers))
                        + ShipAtEachGateMainAction(f, DiscardCourtCardAction(f, Gatekeepers, repeat)).as("Place", Ship.of(f), "at each gate")(Gatekeepers)

                    systems.%(r => f.rules(r)).some.foreach { l =>
                        $(PrisonWardens, Skirmishers, CourtEnforcers, LoyalMarines).foreach { c =>
                            if (f.can(c))
                                + ShipsInSystemMainAction(f, l, DiscardCourtCardAction(f, c, repeat)).as("Place", 3.hlb, Ship.sof(f), "in a controlled system")(c)
                        }
                    }
                }

                if (f.can(Farseers)) {
                    + FarseersRedrawMainAction(f, DiscardCourtCardAction(f, Farseers, repeat)).as("Redraw cards")(Farseers)
                }

                if (f.can(SilverTongues)) {
                    + StealResourceMainAction(f, None, $(CancelAction), DiscardCourtCardAction(f, SilverTongues, repeat)).as("Steal any resource")(SilverTongues)

                    + StealGuildCardMainAction(f, $(CancelAction), DiscardCourtCardAction(f, SilverTongues, repeat)).as("Steal a Guild Card")(SilverTongues)
                }

                if (f.can(RelicFence) && available(Relic)) {
                    f.resources.lazyZip(f.keys).foreach { (r, k) =>
                        if (r != Nothingness) {
                            val cost = PayResource(r, |(k))

                            + FenceResourceAction(f, Relic, cost, UsedEffectCardAction(f, RelicFence, repeat)).as("Gain", ResourceRef(Relic, None), cost)(RelicFence)
                        }
                    }
                }

                if (f.can(ElderBroker) && (available(Material) || available(Fuel) || available(Weapon))) {
                    + GainResourcesAction(f, $(Material, Fuel, Weapon), DiscardCourtCardAction(f, ElderBroker, repeat)).as("Gain", ResourceRef(Material, None), ResourceRef(Fuel, None), ResourceRef(Weapon, None))(ElderBroker)
                }

                + EndPreludeAction(f, s, 0, p).as("End Prelude")(" ")

                ask(f)

            case EndPreludeAction(f, s, i, n) =>
                f.spent = $

                log(DottedLine)

                MainTurnAction(f, s, i, n)

            case MainTurnAction(f, s, i, n) if i >= n =>
                NoAsk(f)(EndTurnAction(f))

            case MainTurnAction(f, s, i, n) =>
                implicit val ask = builder
                implicit val repeat = MainTurnAction(f, s, i + 1, n)
                implicit val group = f.elem ~ " " ~ "Actions" ~ " " ~ (n - i).times(0x2726.toChar.toString.styled(s))

                val cost = Pip

                s @@ {
                    case Administration =>
                        tax(f, cost)
                        repair(f, cost)
                        influence(f, cost)
                    case Aggression =>
                        battle(f, cost)
                        move(f, cost)
                        secure(f, cost)
                    case Construction =>
                        build(f, cost)
                        repair(f, cost)
                    case Mobilization =>
                        move(f, cost)
                        influence(f, cost)
                }

                if (s != Aggression && f.anyBattle)
                    battle(f, cost)

                + EndTurnAction(f).as("Forfeit", (n - i).hl, "actions")(group)

                ask(f).needOk

            case EndTurnAction(f) =>
                f.taxed = $
                f.built = $
                f.used = $
                f.anyBattle = false

                val next = factions.dropWhile(_ != f).drop(1).%(_.hand.any).first

                if (next.any) {
                    current = next.get

                    FollowAction(next.get)
                }
                else
                    EndRoundAction

            case EndRoundAction =>
                factions.foreach { f =>
                    f.taking.foreach { d =>
                        d --> f.hand

                        f.log("took", d)
                    }

                    f.taking = $

                    f.discardAfterRound.foreach { c =>
                        c --> discourt

                        f.log("discarded", c)
                    }
                }

                Milestone(TransferRoundAction)

                TransferRoundAction

            case TransferRoundAction =>
                val next =
                    if (seized.any)
                        seized.get
                    else
                    if (options.has(BadInitiativeTransferChapter(chapter))) // !!!! TODO REMOVE !!!!
                        factions.%(_.played.single.exists(d => d.suit == lead.get.suit && d.strength >= zeroed.not.??(lead.get.strength))).first.|(current)
                    else
                        factions.sortBy(f => f.played.single.%(_.suit == lead.get.suit).%(_ => zeroed.not || f.lead.not)./(-_.strength).|(0)).first.get

                log(DoubleLine)

                if (next != current)
                    next.log("took the initiative")
                else
                    next.log("held the initiative")

                factions.foreach { f =>
                    f.lead = false
                    f.surpass = false
                    f.copy = false
                    f.pivot = false
                }

                current = next

                factions = factions.dropWhile(_ != current) ++ factions.takeWhile(_ != current)

                factions.foreach { f =>
                    f.played --> deck
                    f.blind --> deck
                }

                lead = None
                zeroed = false
                seized = None

                if (factions.exists(_.hand.any))
                    StartRoundAction
                else
                    EndChapterAction

            case EndChapterAction =>
                factions.foreach { f =>
                    f.hand --> deck
                }

                $(Tycoon, Tyrant, Warlord, Keeper, Empath).foreach { ambition =>
                    if (declared.contains(ambition)) {
                        val l = declared(ambition)
                        val high = l./(_.high).sum
                        val low = l./(_.low).sum

                        val records = factions./(f => f -> ambition @@ {
                            case Tycoon =>
                                f.resources.count(Material) +
                                f.resources.count(Fuel) +
                                f.loyal.of[GuildCard].count(_.suit == Material) +
                                f.loyal.of[GuildCard].count(_.suit == Fuel) +
                                f.loyal.has(MaterialCartel).??(availableNum(Material)) +
                                f.loyal.has(FuelCartel).??(availableNum(Fuel))
                            case Tyrant => f.captives.num
                            case Warlord => f.trophies.num
                            case Keeper => f.resources.count(Relic) + f.loyal.of[GuildCard].count(_.suit == Relic)
                            case Empath => f.resources.count(Psionic) + f.loyal.of[GuildCard].count(_.suit == Psionic)
                        }).%>(_ > 0).toMap

                        if (records.isEmpty) {
                            log("No one scored", ambition)
                        }
                        else {
                            val max = records.values.max
                            var ff = records.keys.%(f => records(f) == max).$
                            var ss = records.keys.%(f => records(f) == records.values.$.but(max).maxOr(0)).$

                            val first = (ff.num == 1).?(ff).|($)
                            val second = (ff.num == 1).?(ss.single.$).|(ff)

                            first.foreach { f =>
                                val p = high + (f.pooled(City) < 2).??(2) + (f.pooled(City) < 1).??(3)
                                f.power += p
                                f.log("scored first place", ambition, "for", p.power)
                            }

                            if (low > 0)
                            second.foreach { f =>
                                val p = low
                                f.power += p
                                f.log("scored second place", ambition, "for", p.power)
                            }
                        }
                    }
                }

                Milestone(CleanUpChapterAction)

            case CleanUpChapterAction =>
                var adjust : $[Faction] = $

                if (declared.contains(Tyrant)) {
                    factions.foreach { f =>
                        f.captives.foreach { u =>
                            u --> u.faction.as[Faction].get.reserve

                            f.log("returned", u)
                        }
                    }
                }

                if (declared.contains(Warlord)) {
                    factions.foreach { f =>
                        f.trophies.foreach { u =>
                            u --> u.faction.as[Faction].get.reserve

                            if (u.piece == City)
                                if (u.faction.pooled(City) >= 2)
                                    adjust ++= u.faction.as[Faction]

                            f.log("returned", u)
                        }
                    }
                }

                factions.%(_.loyal.has(MaterialCartel)).some.foreach { mc =>
                    factions.diff(mc).foreach { f =>
                        while (f.resources.has(Material)) {
                            f.remove(ResourceRef(Material, None))

                            f.log("discarded", Material, "due to", MaterialCartel)
                        }
                    }
                }

                factions.%(_.loyal.has(FuelCartel)).some.foreach { fc =>
                    factions.diff(fc).foreach { f =>
                        while (f.resources.has(Fuel)) {
                            f.remove(ResourceRef(Fuel, None))

                            f.log("discarded", Fuel, "due to", FuelCartel)
                        }
                    }
                }

                MultiAdjustResourcesAction(factions.intersect(adjust), CheckWinAction)

            case CheckWinAction =>
                if (chapter >= 5 || factions./(_.power).max >= 27) {
                    val winner = factions.%(_.power == factions./(_.power).max)(0)

                    Milestone(GameOverAction(winner))
                }
                else
                    Milestone(StartChapterAction)

            case GameOverAction(winner) =>
                val winners = $(winner)

                isOver = true

                winners.foreach(f => f.log("won"))

                GameOver(winners, "Game Over", winners./~(f => $(GameOverWonAction(null, f))))

            // HELPERS
            case a : SelfPerform =>
                a.perform(soft)(this)
        }
    }

}
