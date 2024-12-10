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
    def elem(implicit game : Game) = (damaged.??("Damaged ") + faction.name + " " + piece.name).styled(faction)
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
case object DeckDiscard extends DeckCardLocation

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
    def bid : String
    def cid : String
    def id(implicit game : Game) = campaign.?(cid).|(bid)
    def name : String
    def elem = name.styled(styles.title).hl
}

abstract class GuildCard(val bid : String, val cid : String, val name : String, val suit : Resource, val keys : Int) extends CourtCard
abstract class VoxCard(val bid : String, val cid : String, val name : String) extends CourtCard

trait LoyalGuild { self : GuildCard => }

case object LoyalEngineers    extends GuildCard("bc01",     "", "Loyal Engineers",    Material, 3) with LoyalGuild
case object MiningInterest    extends GuildCard("bc02", "cc01", "Mining Interest",    Material, 2)
case object MaterialCartel    extends GuildCard("bc03",     "", "Material Cartel",    Material, 2)
case object AdminUnion        extends GuildCard("bc04",     "", "Admin Union",        Material, 2)
case object ConstructionUnion extends GuildCard("bc05", "cc02", "Construction Union", Material, 2)
case object FuelCartel        extends GuildCard("bc06",     "", "Fuel Cartel",        Fuel,     2)
case object LoyalPilots       extends GuildCard("bc07",     "", "Loyal Pilots",       Fuel,     3) with LoyalGuild
case object Gatekeepers       extends GuildCard("bc08", "cc03", "Gatekeepers",        Fuel,     2)
case object ShippingInterest  extends GuildCard("bc09", "cc04", "Shipping Interest",  Fuel,     2)
case object SpacingUnion      extends GuildCard("bc10",     "", "Spacing Union",      Fuel,     2)
case object ArmsUnion         extends GuildCard("bc11", "cc06", "Arms Union",         Weapon,   2)
case object PrisonWardens     extends GuildCard("bc12", "cc05", "Prison Wardens",     Weapon,   2)
case object Skirmishers       extends GuildCard("bc13",     "", "Skirmishers",        Weapon,   2)
case object CourtEnforcers    extends GuildCard("bc14",     "", "Court Enforcers",    Weapon,   2)
case object LoyalMarines      extends GuildCard("bc15",     "", "Loyal Marines",      Weapon,   3) with LoyalGuild
case object LatticeSpies      extends GuildCard("bc16", "cc07", "Lattice Spies",      Psionic,  2)
case object Farseers          extends GuildCard("bc17",     "", "Farseers",           Psionic,  2)
case object SecretOrder       extends GuildCard("bc18",     "", "Secret Order",       Psionic,  2)
case object LoyalEmpaths      extends GuildCard("bc19",     "", "Loyal Empaths",      Psionic,  3) with LoyalGuild
case object SilverTongues     extends GuildCard("bc20", "cc08", "Silver Tongues",     Psionic,  2)
case object LoyalKeepers      extends GuildCard("bc21",     "", "Loyal Keepers",      Relic,    3) with LoyalGuild
case object SwornGuardians    extends GuildCard("bc22", "cc09", "Sworn Guardians",    Relic,    1)
case object ElderBroker       extends GuildCard("bc23", "cc10", "Elder Broker",       Relic,    2)
case object RelicFence        extends GuildCard("bc24",     "", "Relic Fence",        Relic,    2)
case object GalacticBards     extends GuildCard("bc25",     "", "Galactic Bards",     Relic,    1)
case object MassUprising      extends VoxCard  ("bc26",     "", "Mass Uprising")
case object PopulistDemands   extends VoxCard  ("bc27", "cc11", "Populist Demands")
case object OutrageSpreads    extends VoxCard  ("bc28",     "", "Outrage Spreads")
case object SongOfFreedom     extends VoxCard  ("bc29", "cc14", "Song of Freedom")
case object GuildStruggle     extends VoxCard  ("bc30",     "", "Guild Struggle")
case object CallToAction      extends VoxCard  ("bc31",     "", "Call to Action")
case object CouncilIntrigue   extends VoxCard  (    "", "cc12", "Populist Demands")
case object DimplomaticFiasco extends VoxCard  (    "", "cc13", "Dimplomatic Fiasco")
case object BlightLooms       extends VoxCard  (    "", "cc15", "Blight Looms")

object CourtCards {
    def base = $(
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

    def campaign = $(
        MiningInterest    ,
        ConstructionUnion ,
        Gatekeepers       ,
        ShippingInterest  ,
        PrisonWardens     ,
        ArmsUnion         ,
        LatticeSpies      ,
        SilverTongues     ,
        SwornGuardians    ,
        ElderBroker       ,
        PopulistDemands   ,
        CouncilIntrigue   ,
        DimplomaticFiasco ,
        SongOfFreedom     ,
        BlightLooms       ,
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


trait Key

/*
trait BuildKey extends Key {
    val color : Color
    val area : Area
}
*/

trait SoftKeys


trait GameImplicits {
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
    def court(implicit game : Game) = game.court
    def discourt(implicit game : Game) = game.discourt
    def deck(implicit game : Game) = game.deck
    def discard(implicit game : Game) = game.discard
    def lead(implicit game : Game) = game.lead
    def zeroed(implicit game : Game) = game.zeroed
    def seized(implicit game : Game) = game.seized


    implicit def cards(implicit game : Game) = game.cards
    implicit def courtiers(implicit game : Game) = game.courtiers
    implicit def figures(implicit game : Game) = game.figures
}


//[[ BLACKER
abstract class ColorState(val faction : Color)(implicit game : Game) {
    val reserve : Region

    var damaged : $[Figure] = $

    def ruleValue(r : System) : Int = faction.at(r).diff(damaged).count(Ship)

    def rules(r : System) = faction.ruleValue(r) > game.colors.but(faction)./(_.ruleValue(r)).max

    def at(r : System) = game.figures.get(r).%(_.faction == faction)

    def present(r : System) = game.figures.get(r).exists(_.faction == faction)

    def pooled(p : Piece) = reserve.$./(_.piece).count(p)

    def pool(p : Piece) = reserve.$.exists(_.piece == p)
}

class BlightsState(override val faction : Blights.type)(implicit game : Game) extends ColorState(faction)(game) {
    val reserve : Region = game.figures.register(Reserve(faction), _.faction == faction,
        1.to(24)./(Figure(faction, Blight, _))
    )
}

class EmpireState(override val faction : Empire.type)(implicit game : Game) extends ColorState(faction)(game) {
    val reserve : Region = game.figures.register(Reserve(faction), _.faction == faction,
        1.to(15)./(Figure(faction, Ship, _))
    )
}

class FreeState(override val faction : Free.type)(implicit game : Game) extends ColorState(faction)(game) {
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

    var fates : $[Fate] = $
    var past : $[Fate] = $
    var leader : |[Leader] = None
    var lores : $[Lore] = $

    var power = 0

    val cityKeys : $[Int] = $(3, 1, 1, 2, 1, 3)
    var extraKeys : $[Int] = $
    def keys = extraKeys ++ cityKeys
    def resourceSlots = extraKeys.num + $(6, 6, 6, 4, 3, 2)(pooled(City))

    var resources : $[Resource] = $
    var spent : $[Resource] = $

    var anyBattle : Boolean = false

    var hand = game.cards.register(Hand(faction))
    var played = game.cards.register(Played(faction))
    var blind = game.cards.register(Blind(faction))
    var taking : $[DeckCard] = $

    var loyal = game.courtiers.register(Loyal(faction))
    val discardAfterRound = courtiers.register(DiscardAfterRound(faction))

    var taxed : $[Figure] = $
    var built : $[Figure] = $
    var used : $[Effect] = $

    var lead : Boolean = false
    var surpass : Boolean = false
    var copy : Boolean = false
    var pivot : Boolean = false

    var adjust : Boolean = false

    def rivals = game.factions.but(faction)
    def others = game.colors.but(faction)

    def can(e : Effect) = (loyal.contains(e) || lores.contains(e) || leader.exists(_.effects.has(e))) && used.has(e).not

    override def pooled(p : Piece) = super.pooled(p) - (p == Agent).??(outraged.num)

    override def pool(p : Piece) = (p == Agent).?(pooled(p) > 0).|(super.pool(p))

    def remove(x : ResourceRef) {
        val i = 0.until(resources.num).reverse.%(i => resources(i) == x.resource && x.lock./(_ == keys(i)).|(true)).$.starting

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
        resources.has(x) && loyal.has(SwornGuardians).not
    }

    def pay(cost : Cost) {
        cost @@ {
            case PayResource(r, Some(lock)) =>
                val i = 0.until(resources.num).reverse.%(i => resources(i) == r && keys(i) == lock).$.starting

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

    implicit val cards = new IdentityTracker[DeckCardLocation, DeckCard]
    implicit val courtiers = new IdentityTracker[CourtLocation, CourtCard]
    implicit val figures = new IdentityTracker[Region, Figure]

    val deck = cards.register(Deck, content = DeckCards.deck.%(d => factions.num == 4 || (d.strength > 1 && d.strength < 7)))
    val discard = cards.register(DeckDiscard)
    var seen : $[DeckCard] = $
    var seenX : $[(Int, Faction, |[DeckCard])] = $

    val court = courtiers.register(CourtDeck, content = campaign.?(CourtCards.campaign).|(CourtCards.base))
    val market = courtiers.register(CourtMarket)
    val discourt = courtiers.register(CourtDiscard)

    var chapter : Int = 0
    var round : Int = 0
    var passed : Int = 0

    var lead : |[DeckCard] = None
    var zeroed : Boolean = false
    var seized : |[Faction] = None


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


    def availableNum(r : Resource) = 5 - factions./(_.resources.count(r)).sum - factions./(_.spent.count(r)).sum

    def available(r : Resource) = availableNum(r) > 0

    def at(s : System) = figures.get(s)

    def freeSlots(s : System) = board.slots(s) - figures.get(s).%(_.piece.is[Building]).num

    var current : Faction = null

    var highlightFaction : $[Faction] = $

    def viewHand(f : Faction) = f.hand./(ViewCardInfoAction(f, _))
    def viewLeaders(l : $[Leader]) = l./(ViewLeaderInfoAction(_))
    def viewLores(l : $[Lore]) = l./(ViewLoreInfoAction(_))

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = {
        self.%(states.contains)./~( f =>
            actions.has(NoHand).not.??(viewHand(f))
        ) ++
        actions.has(NoLeadersAndLores).not.??(viewLeaders(leaders)) ++
        actions.has(NoLeadersAndLores).not.??(viewLores(lores))
    }

    def convertForLog(s : $[Any]) : $[Any] = s./~{
        case Empty => None
        case NotInLog(_) => None
        case AltInLog(_, m) => |(m)
        case f : Faction => |(f.elem)
        case d : DeckCard => |(OnClick(d, d.elem.spn(xlo.pointer)))
        case c : CourtCard => |(OnClick(c, c.elem.spn(xlo.pointer)))
        case p : PayResource => |(p.elemLog)
        case l : $[Any] => convertForLog(l)
        case x => |(x)
    }

    override def log(s : Any*) {
        super.log(convertForLog(s.$) : _*)
    }

    def showFigure(u : Figure, hits : Int) = {
        val prefix = (hits < 2).??(u.faction.short.toLowerCase + "-")
        val suffix = (hits == 1).??("-damaged") + (hits >= 2).??("-empty")

        u.piece match {
            case Agent => Image(prefix + "agent" + suffix, styles.qship)
            case City => Image(prefix + "city" + suffix, styles.qbuilding)
            case Starport => Image(prefix + "starport" + suffix, styles.qbuilding)
            case Ship => Image(prefix + "ship" + suffix, styles.qship)
            case Blight => Image("blight" + suffix, styles.qship)
        }
    }

    def build(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + BuildMainAction(f, x, repeat).as("Build".styled(f), x)(group).!!!
            // .!{
            //     val ll = systems.%(f.present)
            //     val bb = ll.%(c => freeSlots(c) > 0)
            //     val ss = ll.%(f.at(_).hasA(Starport)).%(s => f.at(s).exists(u => u.piece == Starport && u.faction == f && f.built.has(u).not))
            //     (bb.none || (f.pool(City).not && f.pool(Starport).not)) && ss.none
            // }

        if (f.loyal.has(MiningInterest)) {
            + ManufactureMainAction(f, x, repeat).as("Manufacture".styled(f), x)(group)
        }

        if (f.loyal.has(ShippingInterest)) {
            + SynthesizeMainAction(f, x, repeat).as("Synthesize".styled(f), x)(group)
        }

        if (f.loyal.has(PrisonWardens) && f.captives.any) {
            + PressgangMainAction(f, x, repeat).as("Press Gang".styled(f), x)(group)
        }

        if (f.lores.has(LivingStructures)) {
            + NurtureMainAction(f, x, repeat).as("Nurture".styled(f), x)(group)
        }
    }

    def repair(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + RepairMainAction(f, x, repeat).as("Repair".styled(f), x)(group).!(f.damaged.none)

        if (f.lores.has(LivingStructures)) {
            + PruneMainAction(f, x, repeat).as("Prune".styled(f), x)(group)
        }
    }

    def move(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + MoveMainAction(f, x, repeat).as("Move".styled(f), x)(group).!!!

        if (f.can(SurvivalOverrides)) {
            + MartyrMainAction(f, x, repeat).as("Martyr".styled(f), x)(group).!!!
        }
    }

    def battle(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + BattleMainAction(f, x, repeat).as("Battle".styled(f), x)(group).!!!

        val limit = f.resources.count(Weapon) + f.loyal.of[GuildCard].count(_.suit == Weapon)

        if (f.loyal.has(CourtEnforcers)) {
            val l = market.%(c => Influence(c).%(_.faction != f).use(l => l.any && l.num < limit))

            + AbductMainAction(f, l, x, repeat).as("Abduct".styled(f), x)(group).!(l.none)
        }
    }

    def secure(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + SecureMainAction(f, x, repeat).as("Secure".styled(f), x)(group).!(market.exists(c => Influence(c).$.use(l => l.%(_.faction == f).num > f.rivals./(e => l.%(_.faction == e).num).max)).not)
    }

    def influence(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + InfluenceMainAction(f, x, repeat).as("Influence".styled(f), x)(group).!(f.pool(Agent).not, "no agents")

        if (f.loyal.has(PrisonWardens) && f.captives.any) {
            + ExecuteMainAction(f, x, repeat).as("Execute".styled(f), x)(group)
        }
    }

    def tax(f : Faction, x : Cost)(implicit builder : ActionCollector, group : Elem, repeat : ForcedAction) {
        + TaxMainAction(f, x, repeat).as("Tax".styled(f), x)(group).!!!

        if (f.loyal.has(ElderBroker) && systems.exists(r => f.rules(r) && f.rivals.exists(e => e.at(r).cities.any))) {
            + TradeMainAction(f, x, repeat).as("Trade".styled(f), x)(group)
        }
    }

    def loggedPerform(action : Action, soft : Void) : Continue = {
        // println("> " + action)

        val c = action.as[SelfPerform]./(_.perform(soft)).|(internalPerform(action, soft))

        highlightFaction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a) => a./(_.faction)
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
