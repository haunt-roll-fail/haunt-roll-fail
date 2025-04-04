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


trait Color extends NamedToString with Styling with Elementary with Record {
    def short = name.take(1)
    def style = short.toLowerCase
    override def elem : Elem = name.styled(this).styled(styles.condensed)
}

trait Faction extends BasePlayer with Color

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
case object Intercept extends BattleResult
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
    val die = Die.from($, $(OwnDamage, HitShip), $(Intercept, HitShip), $(OwnDamage, HitShip, HitShip), $(HitShip, HitShip), $(OwnDamage, HitShip))
}
case object Raid extends BattleDie {
    val die = Die.from($(Intercept, RaidKey, RaidKey), $(OwnDamage, RaidKey), $(Intercept), $(OwnDamage, HitBuilding), $(HitBuilding, RaidKey), $(OwnDamage, HitBuilding))
}


case class AmbitionMarker(high : Int, low : Int)


trait Piece extends Record {
    def name = toString
    def plural = name + "s"
    def of(f : Color) = SomePieceOf(f, this)
    def sof(f : Color) : Elem = (f.name + " " + plural).styled(f)
}

trait PieceOf {
    def faction : Color
    def piece : Piece
}


trait Building extends Piece
case object Slot extends Building
case object City extends Building
case object Starport extends Building
case object Ship extends Piece
case object Agent extends Piece

case class SomePieceOf(faction : Color, piece : Piece) extends PieceOf with Elementary {
    def elem = (faction.name + " " + piece.name).styled(faction)
}

case class Figure(faction : Color, piece : Piece, index : Int) extends GameElementary {
    override def toString = "" + faction + "/" + piece + "/" + index
    def fresh(implicit game : Game) = faction.damaged.has(this).not
    def damaged(implicit game : Game) = faction.damaged.has(this)
    def elem(implicit game : Game) = (damaged.??("Damaged ") + game.unslotted.has(this).??("Cloud ") + faction.name + " " + piece.name).styled(faction)
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

case object Event extends Suit

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
    def ref = ResourceRef(resource, lock)
    def elem = "with " ~ ref.elem
    override def elemLog = "with " ~ resource.elem
}

case class MultiCost(l : $[Cost]) extends Cost {
    def elem = l./(_.elem).commaAnd.join(" ")
    override def elemLog = "with " ~ elem
}

object MultiCost {
    def apply(l : Cost*) : MultiCost = MultiCost(l.$.but(NoCost))
}

case class ResourceRef(resource : Resource, lock : |[Int]) extends Elementary {
    def elem = resource.elem ~ Image(resource.name, styles.token) ~ lock./(n => Image("keys-" + n, styles.token))
}


trait DeckCardLocation
case class Hand(faction : Faction) extends DeckCardLocation
case class Played(faction : Faction) extends DeckCardLocation
case class Blind(faction : Faction) extends DeckCardLocation
case object Deck extends DeckCardLocation
case object DeckDiscard extends DeckCardLocation

trait CourtLocation
case object CourtDeck extends CourtLocation
case object SideDeck extends CourtLocation
case object CourtMarket extends CourtLocation
case object CourtDiscard extends CourtLocation
case class DiscardAfterRound(faction : Faction) extends CourtLocation
case class Loyal(faction : Faction) extends CourtLocation
case class FateDeck(fate : Fate) extends CourtLocation
case class NoFateDeck(faction : Faction) extends CourtLocation


trait DeckCard extends Elementary with Record {
    def suit : Suit
    def id : ImageId
    def img = Image(id, styles.card)
    def strength : Int
    def pips : Int
}

case class ActionCard(suit : Suit, strength : Int, pips : Int) extends DeckCard {
    def id = suit + "-" + strength
    def elem = (" " + suit + " " + strength + " ").pre.spn(styles.outlined).styled(suit)
    def zeroed(b : Boolean) = (" " + suit + " " + b.?(0).|(strength) + " ").pre.spn(styles.outlined).styled(suit)
}

case class EventCard(index : Int) extends DeckCard {
    def suit = Event
    def id = "event"
    def elem = (" " + suit + " ").pre.spn(styles.outlined)
    def strength = 0
    def pips = 0
}

object DeckCards {
    def deck = $(
        ActionCard(Administration, 1, 4),
        ActionCard(Administration, 2, 4),
        ActionCard(Administration, 3, 3),
        ActionCard(Administration, 4, 3),
        ActionCard(Administration, 5, 3),
        ActionCard(Administration, 6, 2),
        ActionCard(Administration, 7, 1),
        ActionCard(Aggression, 1, 3),
        ActionCard(Aggression, 2, 3),
        ActionCard(Aggression, 3, 2),
        ActionCard(Aggression, 4, 2),
        ActionCard(Aggression, 5, 2),
        ActionCard(Aggression, 6, 2),
        ActionCard(Aggression, 7, 1),
        ActionCard(Construction, 1, 4),
        ActionCard(Construction, 2, 4),
        ActionCard(Construction, 3, 3),
        ActionCard(Construction, 4, 3),
        ActionCard(Construction, 5, 2),
        ActionCard(Construction, 6, 2),
        ActionCard(Construction, 7, 1),
        ActionCard(Mobilization, 1, 4),
        ActionCard(Mobilization, 2, 4),
        ActionCard(Mobilization, 3, 3),
        ActionCard(Mobilization, 4, 3),
        ActionCard(Mobilization, 5, 2),
        ActionCard(Mobilization, 6, 2),
        ActionCard(Mobilization, 7, 1),
    )
}


trait Effect extends Record

trait CourtCard extends Record with Elementary {
    def id : String
    def name : String
    def elem = name.styled(styles.title).hl
    def img = Image(id, styles.card)
}

case class GuildCard(id : String, effect : GuildEffect) extends CourtCard {
    def name = effect.name
    def suit = effect.suit
    def keys = effect.keys
}

case class VoxCard(id : String, effect : VoxEffect) extends CourtCard {
    def name = effect.name
}

abstract class GuildEffect(val name : String, val suit : Resource, val keys : Int) extends Effect with Elementary {
    def elem = name.styled(styles.title).hl
}

abstract class VoxEffect(val name : String) extends Effect with Elementary {
    def elem = name.styled(styles.title).hl
}


trait LoyalGuild { self : GuildEffect => }

case object LoyalEngineers    extends GuildEffect("Loyal Engineers",    Material, 3) with LoyalGuild
case object MiningInterest    extends GuildEffect("Mining Interest",    Material, 2)
case object MaterialCartel    extends GuildEffect("Material Cartel",    Material, 2)
case object AdminUnion        extends GuildEffect("Admin Union",        Material, 2)
case object ConstructionUnion extends GuildEffect("Construction Union", Material, 2)
case object FuelCartel        extends GuildEffect("Fuel Cartel",        Fuel,     2)
case object LoyalPilots       extends GuildEffect("Loyal Pilots",       Fuel,     3) with LoyalGuild
case object Gatekeepers       extends GuildEffect("Gatekeepers",        Fuel,     2)
case object ShippingInterest  extends GuildEffect("Shipping Interest",  Fuel,     2)
case object SpacingUnion      extends GuildEffect("Spacing Union",      Fuel,     2)
case object ArmsUnion         extends GuildEffect("Arms Union",         Weapon,   2)
case object PrisonWardens     extends GuildEffect("Prison Wardens",     Weapon,   2)
case object Skirmishers       extends GuildEffect("Skirmishers",        Weapon,   2)
case object CourtEnforcers    extends GuildEffect("Court Enforcers",    Weapon,   2)
case object LoyalMarines      extends GuildEffect("Loyal Marines",      Weapon,   3) with LoyalGuild
case object LatticeSpies      extends GuildEffect("Lattice Spies",      Psionic,  2)
case object Farseers          extends GuildEffect("Farseers",           Psionic,  2)
case object SecretOrder       extends GuildEffect("Secret Order",       Psionic,  2)
case object LoyalEmpaths      extends GuildEffect("Loyal Empaths",      Psionic,  3) with LoyalGuild
case object SilverTongues     extends GuildEffect("Silver Tongues",     Psionic,  2)
case object LoyalKeepers      extends GuildEffect("Loyal Keepers",      Relic,    3) with LoyalGuild
case object SwornGuardians    extends GuildEffect("Sworn Guardians",    Relic,    1)
case object ElderBroker       extends GuildEffect("Elder Broker",       Relic,    2)
case object RelicFence        extends GuildEffect("Relic Fence",        Relic,    2)
case object GalacticBards     extends GuildEffect("Galactic Bards",     Relic,    1)
case object MassUprising      extends VoxEffect  ("Mass Uprising")
case object PopulistDemands   extends VoxEffect  ("Populist Demands")
case object OutrageSpreads    extends VoxEffect  ("Outrage Spreads")
case object SongOfFreedom     extends VoxEffect  ("Song of Freedom")
case object GuildStruggle     extends VoxEffect  ("Guild Struggle")
case object CallToAction      extends VoxEffect  ("Call to Action")

object CourtCards {
    def base = $(
        GuildCard("bc01", LoyalEngineers   ),
        GuildCard("bc02", MiningInterest   ),
        GuildCard("bc03", MaterialCartel   ),
        GuildCard("bc04", AdminUnion       ),
        GuildCard("bc05", ConstructionUnion),
        GuildCard("bc06", FuelCartel       ),
        GuildCard("bc07", LoyalPilots      ),
        GuildCard("bc08", Gatekeepers      ),
        GuildCard("bc09", ShippingInterest ),
        GuildCard("bc10", SpacingUnion     ),
        GuildCard("bc11", ArmsUnion        ),
        GuildCard("bc12", PrisonWardens    ),
        GuildCard("bc13", Skirmishers      ),
        GuildCard("bc14", CourtEnforcers   ),
        GuildCard("bc15", LoyalMarines     ),
        GuildCard("bc16", LatticeSpies     ),
        GuildCard("bc17", Farseers         ),
        GuildCard("bc18", SecretOrder      ),
        GuildCard("bc19", LoyalEmpaths     ),
        GuildCard("bc20", SilverTongues    ),
        GuildCard("bc21", LoyalKeepers     ),
        GuildCard("bc22", SwornGuardians   ),
        GuildCard("bc23", ElderBroker      ),
        GuildCard("bc24", RelicFence       ),
        GuildCard("bc25", GalacticBards    ),
        VoxCard("bc26", MassUprising     ),
        VoxCard("bc27", PopulistDemands  ),
        VoxCard("bc28", OutrageSpreads   ),
        VoxCard("bc29", SongOfFreedom    ),
        VoxCard("bc30", GuildStruggle    ),
        VoxCard("bc31", CallToAction     ),
    )
}


trait Region {
    def -->(p : Piece)(implicit tracker : IdentityTracker[Region, Figure]) = tracker.get(this).%(_.piece == p).take(1).single.|!(p.name + " not found in " + this)
    def -->(p : PieceOf)(implicit tracker : IdentityTracker[Region, Figure]) = tracker.get(this).%(u => u.piece == p.piece && u.faction == u.faction).take(1).single.|!(p.piece.name + " of " + p.faction.name + " not found in " + this)
}

trait SpecialRegion extends Region
case class Reserve(f : Color) extends SpecialRegion
case class Outrage(f : Faction) extends SpecialRegion
case class Trophies(f : Faction) extends SpecialRegion
case class Captives(f : Faction) extends SpecialRegion
case class Favors(f : Faction) extends SpecialRegion
case object Scrap extends SpecialRegion
case class Influence(c : CourtCard) extends SpecialRegion


trait Symbol extends NamedToString with Record
case object Gate extends Symbol
case object Arrow extends Symbol
case object Crescent extends Symbol
case object Hex extends Symbol

case class System(cluster : Int, symbol : Symbol) extends Region with Elementary with Record {
    def name = (symbol.name + " " + cluster)

    def smb = (symbol @@ {
        case Gate => 0x2727.toChar.toString
        case Arrow => 0x2B9D.toChar.toString
        case Crescent => 0x263E.toChar.toString
        case Hex => 0x2B22.toChar.toString
    })

    def gate = symbol == Gate
    def elem = name.hlb ~ smb.hh
    def unstyledElem = name ~ smb
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
        (System(5, Hex), System(1, Crescent), $(System(5, Gate))),
        (System(4, Crescent), System(6, Arrow), $(System(1, Gate))),
    )

}

case object BoardFull extends BaseBoard {
    val name = "Full Board"
    val clusters = $(1, 2, 3, 4, 5, 6)

    val starting : $[(System, System, $[System])] = $
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
        def ofc(f : Color) = l.%(_.faction == f)
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
        def blights = l.%(u => u.piece == Blight)
        def agents = l.%(u => u.piece == Agent)

        def fresh(implicit game : Game) = l.%(u => u.faction.damaged.has(u).not)
        def damaged(implicit game : Game) = l.%(u => u.faction.damaged.has(u))

        def comma : $[Any] = l./~(e => $(Comma, e)).drop(1)
    }

}
//]]


trait ViewCard extends ViewObject[DeckCard] { self : UserAction =>
    def d : DeckCard
    def obj = d
}

trait ViewLeader extends ViewObject[Leader] { self : UserAction =>
    def l : Leader
    def obj = l
}

trait ViewLore extends ViewObject[Lore] { self : UserAction =>
    def l : Lore
    def obj = l
}

trait ViewSetup extends ViewObject[String] { self : UserAction =>
    def s : String
    def obj = s
}


trait Key

/*
trait BuildKey extends Key {
    val color : Color
    val area : Area
}
*/

trait SoftKeys

trait ThenDesc { self : ForcedAction =>
    def desc : Elem
}


trait GameImplicits {
    type Rolled = $[$[BattleResult]]

    implicit def factionToState(f : Faction)(implicit game : Game) : FactionState = game.states(f).as[FactionState].get
    implicit def blightsToState(f : Blights.type)(implicit game : Game) : BlightsState = game.states(f).as[BlightsState].get
    implicit def empireToState(f : Empire.type)(implicit game : Game) : EmpireState = game.states(f).as[EmpireState].get
    implicit def freeToState(f : Free.type)(implicit game : Game) : FreeState = game.states(f).as[FreeState].get
    implicit def colorToState(f : Color)(implicit game : Game) : ColorState = game.states(f)
    implicit def regionToContent(r : Region)(implicit game : Game) : $[Figure] = game.figures.get(r)
    implicit def cardLocationToContent(r : DeckCardLocation)(implicit game : Game) : $[DeckCard] = game.cards.get(r)
    implicit def courtLocationToContent(r : CourtLocation)(implicit game : Game) : $[CourtCard] = game.courtiers.get(r)

    def log(s : Any*)(implicit game : Game) {
        game.log(s : _*)
    }

    implicit class FactionEx(f : Color)(implicit game : Game) {
        def log(s : Any*) { if (game.logging) game.log((f +: s.$) : _*) }
    }

    implicit def descCard(g : Game, d : DeckCard) = d.img

    def options(implicit game : Game) = game.options
    def colors(implicit game : Game) = game.colors
    def factions(implicit game : Game) = game.factions
    def board(implicit game : Game) = game.board
    def systems(implicit game : Game) = game.board.systems
    def current(implicit game : Game) = game.current
    def campaign(implicit game : Game) = game.campaign
    def chapter(implicit game : Game) = game.chapter
    def round(implicit game : Game) = game.round
    def market(implicit game : Game) = game.market
    def deck(implicit game : Game) = game.deck
    def discard(implicit game : Game) = game.discard
    def lead(implicit game : Game) = game.lead
    def seized(implicit game : Game) = game.seized

    implicit def cards(implicit game : Game) = game.cards
    implicit def courtiers(implicit game : Game) = game.courtiers
    implicit def figures(implicit game : Game) = game.figures
}


//[[ BLACKER
abstract class ColorState(val faction : Color)(implicit game : Game) {
    val reserve : Region

    var damaged : $[Figure] = $

    var regent = false

    def ruleValue(s : System) : Int

    def rules(r : System) = faction.ruleValue(r) > game.colors.but(faction)./(_.ruleValue(r)).max

    def at(r : System) = game.figures.get(r).%(_.faction == faction)

    def present(r : System) = game.figures.get(r).exists(_.faction == faction)

    def pooled(p : Piece) = reserve.$./(_.piece).count(p)

    def pool(p : Piece) = reserve.$.exists(_.piece == p)
}

class BlightsState(override val faction : Blights.type)(implicit game : Game) extends ColorState(faction)(game) {
    override def ruleValue(s : System) : Int = 0

    val reserve : Region = game.figures.register(Reserve(faction), _.faction == faction,
        1.to(24)./(Figure(faction, Blight, _))
    )
}

class EmpireState(override val faction : Empire.type)(implicit game : Game) extends ColorState(faction)(game) {
    override def ruleValue(s : System) : Int = Empire.at(s).fresh.any.??(99)

    val reserve : Region = game.figures.register(Reserve(faction), _.faction == faction,
        1.to(15)./(Figure(faction, Ship, _))
    )
}

class FreeState(override val faction : Free.type)(implicit game : Game) extends ColorState(faction)(game) {
    override def ruleValue(s : System) : Int = 0

    val reserve : Region = game.figures.register(Reserve(faction), _.faction == faction,
        1.to(28*2)./(Figure(faction, City, _)) ++
        1.to(14*2)./(Figure(faction, Starport, _))
    )
}

class FactionState(override val faction : Faction)(implicit game : Game) extends ColorState(faction)(game) {
    val reserve : Region = figures.register(Reserve(faction), _.faction == faction,
        1.to(5)./(Figure(faction, City, _)) ++
        1.to(5)./(Figure(faction, Starport, _)) ++
        1.to(15)./(Figure(faction, Ship, _)) ++
        1.to(10)./(Figure(faction, Agent, _))
    )

    var outraged : $[Resource] = $

    val trophies = game.figures.register(Trophies(faction))

    val captives = game.figures.register(Captives(faction))

    val favors = game.figures.register(Favors(faction))

    var fatedeck : CourtLocation = game.courtiers.register(NoFateDeck(faction))

    var fates : $[Fate] = $
    var past : $[Fate] = $
    var leader : |[Leader] = None
    var lores : $[Lore] = $

    var power = 0

    var progress = 0
    var objective : |[Objective] = None

    var primus = false

    var resources : $[Resource] = $
    var spent : $[Resource] = $

    val cityKeys : $[Int] = $(3, 1, 1, 2, 1, 3)
    var extraKeys : $[Int] = $
    def keys = extraKeys ++ cityKeys
    def resourceSlots = extraKeys.num + $(6, 6, 6, 4, 3, 2)(pooled(City))

    def resKeys : $[(Resource, Int)] = resources.lazyZip(keys).toList.%<(_ != Nothingness)

    var anyBattle : Boolean = false

    var hand = game.cards.register(Hand(faction))
    var played = game.cards.register(Played(faction))
    var blind = game.cards.register(Blind(faction))
    var taking : $[DeckCard] = $

    val loyal = game.courtiers.register(Loyal(faction))
    val discardAfterRound = courtiers.register(DiscardAfterRound(faction))

    object taxed {
        var cities : $[Figure] = $
        var slots : $[System] = $
    }

    var worked : $[Figure] = $

    var used : $[Effect] = $
    var secured : $[GuildCard] = $

    var lead : Boolean = false
    var zeroed : Boolean = false
    var declared : Boolean = false
    var surpass : Boolean = false
    var copy : Boolean = false
    var pivot : Boolean = false
    var mirror : Boolean = false

    var adjust : Boolean = false

    var seen : $[DeckCard] = $

    def rivals = game.factions.but(faction)
    def others = game.colors.but(faction)

    override def ruleValue(s : System) = (game.campaign && faction.regent && game.current == faction && Empire.at(s).fresh.any && faction.at(s).any).?(999).|(faction.at(s).diff(damaged).count(Ship))

    def can(e : Effect) : Boolean = (loyal.exists(_.as[GuildCard].?(_.effect == e)) || lores.contains(e) || leader.exists(_.effects.has(e))) && used.has(e).not

    def canPrelude(e : GuildEffect) : Boolean = loyal.exists(_.as[GuildCard].%(_.effect == e).%!(secured.has).any) && used.has(e).not

    override def pooled(p : Piece) = super.pooled(p) - (p == Agent).??(outraged.num)

    override def pool(p : Piece) = (p == Agent).?(pooled(p) > 0).|(super.pool(p))

    def remove(x : ResourceRef) {
        val i = 0.until(resources.num).reverse.%(i => resources(i) == x.resource && x.lock./(_ == keys(i)).|(true)).sortBy(i => keys(i)).$.starting

        if (i.none)
            throw new Error("resource ref " + x + " not found")

        resources = resources.updated(i.get, Nothingness)
    }

    private def add(r : Resource) : Boolean = {
        if (game.available(r)) {
            if (resources.has(Nothingness).not)
                resources :+= Nothingness

            resources = resources.updated(resources.indexOf(Nothingness), r)

            adjust = true

            true
        }
        else
            false
    }

    def gain(r : Resource) {
        add(r)
    }

    def gain(r : Resource, message : $[Any]) {
        gain("gained", r, message)
    }

    def gain(verb : String, r : Resource, message : $[Any]) {
        if (add(r))
            faction.log(verb, r, message)
        else
            faction.log("could not gain", r, message)
    }

    def gain(verb : String, r : ResourceRef, message : $[Any]) {
        if (add(r.resource))
            faction.log(verb, r, message)
        else
            faction.log("could not gain", r, message)
    }

    def stealable(x : Resource) : Boolean = {
        resources.has(x) && faction.can(SwornGuardians).not
    }

    def pay(cost : Cost) {
        cost @@ {
            case PayResource(r, Some(lock)) =>
                val i = 0.until(resources.num).reverse.%(i => resources(i) == r && keys(i) == lock).$.starting

                if (i.none)
                    throw new Error("payment " + cost + " not found")

                resources = resources.updated(i.get, Nothingness)
                spent :+= r

            case MultiCost(l) =>
                l.foreach(pay)

            case _ =>
                // println("skipping payment " + cost)
        }
    }

    def advance(n : Int) {
        if (n != 0 && progress > 0) {
            progress -= n

            faction.log("advanced objective by", n.hlb)
        }
    }
}
//]]

trait Expansion {
    def perform(a : Action, soft : Void)(implicit game : Game) : Continue

    implicit class ActionMatch(val a : Action) {
        def @@(t : Action => Continue) = t(a)
        def @@(t : Action => Boolean) = t(a)
    }
}

case object NoHand extends HiddenInfo
case object NoLeadersAndLores extends HiddenInfo

case class ViewCardInfoAction(self : Faction, d : DeckCard) extends BaseInfo(Break ~ "Hand")(d.img) with ViewCard with OnClickInfo { def param = d }
case class ViewLeaderInfoAction(l : Leader) extends BaseInfo(Break ~ "Leaders")(l.img) with ViewLeader with OnClickInfo { def param = l }
case class ViewLoreInfoAction(l : Lore) extends BaseInfo(Break ~ "Lore")(l.img) with ViewLore with OnClickInfo { def param = l }
case class ViewSetupInfoAction(s : String) extends BaseInfo(Break ~ "Setup")(Image(s, styles.setupCard)) with ViewSetup with OnClickInfo { def param = s }


class Game(val setup : $[Faction], val options : $[Meta.O]) extends BaseGame with ContinueGame with LoggedGame {
    private implicit val game = this

    var isOver = false

    val campaign = options.of[CampaignOption].any

    val expansions : $[Expansion] =
        if (campaign)
            $(BlightExpansion, LoreExpansion, CommonExpansion)
        else
            options.of[LeadersAndLoreOption].any.$(LeadersExpansion, LoreExpansion) ++ $(BaseExpansion, CommonExpansion)

    var seating : $[Faction] = setup
    var factions : $[Faction] = setup
    var colors : $[Color] = setup ++ campaign.$(Blights, Empire, Free)
    var states = Map[Color, ColorState]()

    var leaders : $[Leader] = $
    var lores : $[Lore] = $
    var unusedLores : $[Lore] = $

    implicit val figures = new IdentityTracker[Region, Figure]
    val scrap = game.figures.register(Scrap)

    implicit val cards = new IdentityTracker[DeckCardLocation, DeckCard]
    implicit val courtiers = new IdentityTracker[CourtLocation, CourtCard]

    val deck = cards.register(Deck, content =
        DeckCards.deck.%(d => factions.num == 4 || (d.strength > 1 && d.strength < 7)) ++
        campaign.$(EventCard(1), EventCard(2), EventCard(3)).take(factions.num - 1)
    )

    val discard = cards.register(DeckDiscard)

    var seen : $[(Int, Faction, |[DeckCard])] = $

    val court = courtiers.register(CourtDeck, content = campaign.?(BlightCards.court).|(CourtCards.base))
    val sidedeck = courtiers.register(SideDeck, content = campaign.??(BlightCards.sidedeck ++ Lores.done))
    val market = courtiers.register(CourtMarket)
    val discourt = courtiers.register(CourtDiscard)

    var act : Int = 0
    var chapter : Int = 0
    var round : Int = 0
    var passed : Int = 0

    var lead : |[ActionCard] = None
    var seized : |[Faction] = None
    var decided : |[Color] = None

    var edicts : $[Edict] = $

    var drafts : $[NegotiationDraft] = $
    var draftsCount : Int = 0
    var negotiators : $[Faction] = $

    val markers : $[AmbitionMarker] = $(AmbitionMarker(2, 0), AmbitionMarker(3, 2), AmbitionMarker(5, 3), AmbitionMarker(4, 2), AmbitionMarker(6, 3), AmbitionMarker(9, 4), AmbitionMarker(4, 2))

    var ambitionable : $[AmbitionMarker] = $
    var declared : Map[Ambition, $[AmbitionMarker]] = Map()

    val board : BaseBoard = factions.num @@ {
        case _ if campaign => BoardFull
        case 4 => Board4Mixup1
        case 3 => Board3Frontiers
    }

    board.systems.foreach(r => figures.register(r))

    court.$./(c => figures.register(Influence(c)))

    sidedeck.$./(c => figures.register(Influence(c)))

    var protoGolems : Map[System, GolemType] = Map()

    var trust : $[Resource] = $

    def availableNum(r : Resource) = 5 - factions./(_.resources.count(r)).sum - factions./(_.spent.count(r)).sum - overridesHard.values.$.count(r) - trust.count(r)

    def available(r : Resource) = availableNum(r) > 0


    def at(s : System) = figures.get(s)

    var starting : $[(System, System, $[System])] = $

    var overridesSoft : Map[System, Resource] = Map()
    var overridesHard : Map[System, Resource] = Map()

    def resources(s : System) : $[Resource] = overridesHard.get(s)./($(_)) || overridesSoft.get(s)./($(_)) ||
        s.gate.?(systems.%(_.cluster == s.cluster).but(s).%(_.$.cities.any)./~(resources)) |
        $(board.resource(s))

    var unslotted : $[Figure] = $

    def freeSlots(s : System) = board.slots(s) - figures.get(s).%(_.piece.is[Building]).diff(unslotted).num

    var current : |[Faction] = None

    var highlightFaction : $[Faction] = $

    def viewHand(f : Faction) = f.hand./(ViewCardInfoAction(f, _))
    def viewLeaders(l : $[Leader]) = l./(ViewLeaderInfoAction(_))
    def viewLores(l : $[Lore]) = l./(ViewLoreInfoAction(_))

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = {
        self.%(states.contains)./~( f =>
            actions.has(NoHand).not.??(viewHand(f))
        ) ++
        actions.has(NoLeadersAndLores).not.??(viewLeaders(leaders)) ++
        actions.has(NoLeadersAndLores).not.??(viewLores(lores)) ++
        options.has(RandomizeStartingSystems).not.??(
            (chapter == 0 && setup.num == 3).$(ViewSetupInfoAction("setup-3p-02")) ++
            (chapter == 0 && setup.num == 4).$(ViewSetupInfoAction("setup-4p-01"))
        )
    }

    def convertForLog(s : $[Any]) : $[Any] = s./~{
        case Empty => None
        case NotInLog(_) => None
        case AltInLog(_, m) => |(m)
        case f : Faction => |(f.elem)
        case d : DeckCard => |(OnClick(d, d.elem.spn(xlo.pointer)))
        case c : CourtCard => |(OnClick(c, c.elem.spn(xlo.pointer)))
        case l : Leader => |(OnClick(l, l.elem.spn(xlo.pointer)))
        case l : Lore => |(OnClick(l, l.elem.spn(xlo.pointer)))
        case p : PayResource => |(p.elemLog)
        case l : $[Any] => convertForLog(l)
        case x => |(x)
    }

    override def log(s : Any*) {
        super.log(convertForLog(s.$) : _*)
    }

    def showFigure(u : Figure, hits : Int) = {
        val prefix = (u.faction == Empire).?("imperial-").|((hits < 2).??(u.faction.short.toLowerCase + "-"))
        val suffix = (hits == 1).??("-damaged") + (hits >= 2).??("-empty")

        u.piece match {
            case Agent => Image(prefix + "agent" + suffix, styles.qship)
            case City => Image(prefix + "city" + suffix, styles.qbuilding)
            case Starport => Image(prefix + "starport" + suffix, styles.qbuilding)
            case Ship => Image(prefix + "ship" + suffix, styles.qship)
            case Blight => Image("blight" + suffix, styles.qship)
        }
    }

    def build(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        + BuildMainAction(f, x, then).as("Build".styled(f), x)(group).!!!
    }

    def buildAlt(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        if (f.can(MiningInterest)) {
            + ManufactureMainAction(f, x, then).as("Manufacture".styled(f), x)(group)
        }

        if (f.can(ShippingInterest)) {
            + SynthesizeMainAction(f, x, then).as("Synthesize".styled(f), x)(group)
        }

        if (f.can(PrisonWardens) && f.captives.any) {
            + PressgangMainAction(f, x, then).as("Press Gang".styled(f), x)(group)
        }

        if (f.lores.has(LivingStructures)) {
            + NurtureMainAction(f, x, then).as("Nurture".styled(f), x)(group).!!!
        }
    }

    def repair(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        + RepairMainAction(f, x, then).as("Repair".styled(f), x)(group).!(f.damaged.none)
    }

    def repairAlt(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        if (f.lores.has(LivingStructures)) {
            + PruneMainAction(f, x, then).as("Prune".styled(f), x)(group)
        }
    }

    def move(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        + MoveMainAction(f, x, None, false, true, then).as("Move".styled(f), x, then.as[ThenDesc]./(_.desc))(group).!!!
    }

    def moveAlt(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        if (f.lores.has(SurvivalOverrides)) {
            + MartyrMainAction(f, x, then).as("Martyr".styled(f), x)(group).!!!
        }

        if (f.lores.has(ForceBeams)) {
            + GuideMainAction(f, x, then).as("Guide".styled(f), x)(group).!!!
        }
    }

    def battle(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        + BattleMainAction(f, x, None, false, true, then).as("Battle".styled(f), x, then.as[ThenDesc]./(_.desc))(group).!!!
    }

    def battleAlt(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        if (f.can(CourtEnforcers)) {
            val limit = f.resources.count(Weapon) + f.loyal.of[GuildCard].count(_.suit == Weapon)

            val l = market.%(c => Influence(c).%(_.faction != f).use(l => l.any && l.num < limit))

            + AbductMainAction(f, l, x, then).as("Abduct".styled(f), x)(group).!(l.none)
        }

        if (f.lores.has(GalacticRifles)) {
            + FireRiflesMainAction(f, x, then).as("Fire Rifles".styled(f), x)(group).!!!
        }
    }

    def secure(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        + SecureMainAction(f, x, None, false, true, then).as("Secure".styled(f), x, then.as[ThenDesc]./(_.desc))(group).!!!
    }

    def secureAlt(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
    }

    def influence(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        + InfluenceMainAction(f, x, None, false, true, then).as("Influence".styled(f), x, then.as[ThenDesc]./(_.desc))(group).!(f.pool(Agent).not, "no agents")
    }

    def influenceAlt(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        if (f.can(PrisonWardens) && f.captives.any) {
            + ExecuteMainAction(f, x, then).as("Execute".styled(f), x)(group)
        }
    }

    def tax(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        + TaxMainAction(f, x, None, then).as("Tax".styled(f), x)(group).!!!
    }

    def taxAlt(f : Faction, x : Cost, then : ForcedAction)(implicit builder : ActionCollector, group : Elem) {
        if (f.can(ElderBroker) && systems.exists(r => f.rules(r) && f.rivals.exists(e => e.at(r).cities.any))) {
            + TradeMainAction(f, x, then).as("Trade".styled(f), x)(group)
        }
    }

    def loggedPerform(action : Action, soft : Void) : Continue = {
        // println("> " + action)

        val c = action.as[SelfPerform]./(_.perform(soft)).|(internalPerform(action, soft))

        highlightFaction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a, _) => a./(_.faction)
            case _ => Nil
        }

        // println("< " + c)

        c
    }

    def internalPerform(action : Action, soft : Void) : Continue = {
        expansions.foreach { e =>
            e.perform(action, soft) @@ {
                case UnknownContinue =>
                case Force(another) =>
                    if (action.isSoft.not && another.isSoft)
                        soft()

                    return another.as[SelfPerform]./(_.perform(soft)).|(internalPerform(another, soft))
                case TryAgain => return internalPerform(action, soft)
                case c => return c
            }
        }

        throw new Error("unknown continue on " + action)
    }
}
