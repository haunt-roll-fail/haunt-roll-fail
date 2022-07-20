package root
//
//
//
//
import logger._, colmat._
//
//
//
//

import colmat._

import hrf.tracker._

import hrf.elem._
import root.elem._

sealed trait Player extends hrf.base.BasePlayer with Elementary {
    def name : String
}

case class PlayerN(n : Int) extends Player {
    def short = "P" + n
    def name = "Player #" + n
    def elem = ("Player #" + n).hh
}

sealed trait AbstractSuit extends Record {
    def name = toString

    def short = this match {
        case AnySuit => "A".hl
        case s : Suit => s.name.take(1).styled(s)
    }

    def elem : Elem
}

object AbstractSuit {
    implicit class AbstractSuitEx(l : List[AbstractSuit]) {
        def ss = l./(_.short).merge
        def us = l./(_.name.take(1)).join("")
    }
}

sealed trait DiscardCost extends AbstractSuit

sealed trait CraftCost extends DiscardCost

case object AnySuit extends CraftCost with DiscardCost {
    def elem = "Any".hl
}

sealed trait CraftAsset extends AbstractSuit {
    def m(t : CraftCost) = t == this || t == AnySuit || this @@ {
        case s : OneOf => t @@ {
            case Fox if s.fox => true
            case Rabbit if s.rabbit => true
            case Mouse if s.mouse => true
            case _ => false
        }
        case _ => false
    }
}

case class OneOf(fox : Boolean, rabbit : Boolean, mouse : Boolean) extends CraftAsset {
    def elem = "OneOf".hl

}

sealed trait Suit extends DiscardCost with Named with Styling {
    def m(t : DiscardCost) = this match {
        case Bird => true
        case _ if t == AnySuit => true
        case s if t == s => true
        case s => t match {
            case _ => false
        }
    }
}
case object Bird extends Suit
trait BaseSuit extends Suit with CraftCost with CraftAsset
case object Fox extends BaseSuit
case object Rabbit extends BaseSuit
case object Mouse extends BaseSuit

trait Region extends GameElementary {
    def name : String
}

object Region {
    implicit class RegionCast(val r : Region) {
        def clearing = r match {
            case c : Clearing => Some(c)
            case _ => None
        }
    }

    def dir(from : Region, to : Region) = {
        val a = center(from)
        val b = center(to)
        (((180 - math.atan2(b._1 - a._1, b._2 - a._2) / math.Pi * 180) / 15).round * 15).toInt % 360
    }




    import AutumnBoard._
    import WinterBoard._
    import LakeBoard._
    import MountainBoard._
 
    def fixAutumn(a : (Int, Int)) = (a._1 + 20, a._2 + 20)

    def center(r : Region) : (Int, Int) = r @@ {
        case Hill => fixAutumn(274, 301)
        case Haven => fixAutumn(280, 1798)
        case Quarry => fixAutumn(2106, 506)
        case Weald => fixAutumn(1991, 1930)
        case Creek => fixAutumn(1304, 199)
        case Beach => fixAutumn(1034, 635)
        case Dune => fixAutumn(244, 903)
        case Glade => fixAutumn(742, 1224)
        case Waterfall => fixAutumn(1522, 1067)
        case Mountain => fixAutumn(2173, 1151)
        case Meadow => fixAutumn(881, 1981)
        case Pond => fixAutumn(1405, 1706)

        case AutumnN => List((837, 386), (1554, 463)).shuffle.head
        case Witchwood => List((1853, 804), (1160, 980)).shuffle.head
        case AutumnNW => (601, 770)
        case AutumnS => List((1194, 1346), (1560, 1400)).shuffle.head
        case AutumnSW => List((940, 1592), (650, 1749)).shuffle.head
        case AutumnE => (1893, 1377)
        case AutumnW => (403, 1344)
        
        
        case Mound => (322, 292)
        case Moor => (2104, 518)
        case Hedge => (2028, 1892)
        case Spire => (337, 1792)
        case Trench => (908, 388)
        case Pit => (1432, 530)
        case Drift => (334, 943)
        case Wade => (892, 1129)
        case Bend => (1527, 1037)
        case Ford => (2203, 1294)
        case Rock => (889, 1896)
        case Dale => (1408, 1607)
        
        case WinterNW => List((433, 629), (631, 927)).shuffle.head
        case Deadwood => List((913, 728), (1167, 777)).shuffle.head
        case WinterNE => (1946, 957)
        case WinterW => (532, 1312)
        case WinterSW => (748, 1592)
        case Mirkwood => (1194, 1346)
        case WinterSE => (1729, 1679)
        case WinterE => (1893, 1377)


        case Den => (360, 418)
        case Prairie => (1080, 272)
        case Vert => (1658, 502)
        case Shade => (2164, 728)
        case Lawn => (253, 1058)
        case Shoal => (769, 798)
        case Bay => (1569, 1070)
        case Yard => (2127, 1256)
        case Grove => (300, 1773)
        case Marsh => (821, 1467)
        case Alley => (1080, 1940)
        case Gulf => (2043, 1870)
        
        case LakeW => (418, 755)
        case LakeNW => (754, 465)
        case LakeN => (1224, 817)
        case LakeNE => (1328, 521)
        case AcresWild => (1860, 829)
        case HeartOfOak => (606, 1175)
        case LakeS => (722, 1793)
        case LakeSE => (1220, 1575)
        case LakeE => (1842, 1447)
        

        case Slope => (273, 378)
        case Ledge => (713, 761)
        case Mine => (1287, 356)
        case Peak => (2001, 495)
        case Brim => (215, 1273)
        case Pass => (1188, 886)
        case Valley => (1527, 1360)
        case Ridge => (2195, 1106)
        case Drain => (357, 1838)
        case Ramp => (820, 1486)
        case Cliff => (1388, 1900)
        case Crest => (2078, 1739)

        case MountainW => (354, 814)
        case MountainNW => (1044, 636)
        case MountainCW => (888, 1098)
        case MountainC => (1190, 1216)
        case MountainCN => (1438, 1015)
        case MountainN => (1608, 638)
        case MountainNE => (1916, 946)
        case MountainSE => (1952, 1404)
        case LiarWind => (528, 1442)
        case NadaMoonshine => (1207, 1622)

        case Burrow(_) => (1208, 999999)

        case _ => (0, 0); throw new Error("no center for " + r)
    }
}  


case class Forest(name : String, label : String = null) extends Region {
    def elem(g : Game) = g.board.forestName(this, g)
}

case class Clearing(name : String)(val capacity : Int) extends Region {
    def elem(g : Game) = name.styled(g.mapping(this))
}

trait ExtraRegion extends Region


trait Board {
    val name : String
    def clearings : List[Clearing]
    def forests : List[Forest]
    val ruins : List[Region]
    val diagonals : List[(Clearing, Clearing)]
    val inner : List[Clearing]
    def corners = diagonals./(_._1) ++ diagonals./(_._2)
    val rubble : List[(Clearing, Clearing)] = Nil
    val coastal : List[Clearing] = Nil
    val tower : List[Clearing] = Nil


    def opposite(r : Clearing) = diagonals./~{
        case (a, b) if a == r => Some(b)
        case (a, b) if b == r => Some(a)
        case _ => None
    }.single

    lazy val regions = clearings ++ forests

    def connected(c : Clearing) : List[Clearing]
    def fromForest(f : Forest) : List[Clearing]
    def byRiver(c : Clearing) : List[Clearing]

    lazy val riverside = clearings.%(c => byRiver(c).any)
    
    lazy val distance = clearings./(a => a -> clearings./(b => b -> {
        var n = 0
        var l = $(a)
        while (!l.contains(b)) {
            n += 1
            l = l./~(connected)
        }
        n
    }).toMap).toMap

    def forestName(f : Forest, g : Game, from : Option[Region] = None) : Elem = from match {
        case _ if f.label != null => f.label.styled(styles.forest)
        case Some(r : Forest) => "Forest".styled(styles.forest) ~ " near " ~ fromForest(f).diff(fromForest(r))./(_.elem(g)).join(" and ")
        case Some(r : Clearing) => "Forest".styled(styles.forest) ~ " near " ~ fromForest(f).but(r)./(_.elem(g)).join(" and ")
        case None => "Forest".styled(styles.forest) ~ " between " ~ fromForest(f)./(_.elem(g)).join(" and ")
    }
}

object AutumnBoard extends Board {
    val name = "Autumn"
 
    val Hill = Clearing("Hill")(1)
    val Glade = Clearing("Glade")(2)
    val Meadow = Clearing("Meadow")(2)
    val Mountain = Clearing("Mountain")(2)
    val Haven = Clearing("Haven")(1)
    val Creek = Clearing("Creek")(2)
    val Beach = Clearing("Beach")(2)
    val Weald = Clearing("Weald")(1)
    val Dune = Clearing("Dune")(2)
    val Waterfall = Clearing("Waterfall")(3)
    val Pond = Clearing("Pond")(2)
    val Quarry = Clearing("Quarry")(2)
    
    val defaultMapping = Map[Clearing, BaseSuit](
        Hill -> Fox,
        Glade -> Fox,
        Meadow -> Fox,
        Mountain -> Fox,
        Haven -> Rabbit,
        Creek -> Rabbit,
        Beach -> Rabbit,
        Weald -> Rabbit,
        Dune -> Mouse,
        Waterfall -> Mouse,
        Pond -> Mouse,
        Quarry -> Mouse
    )

    val clearings = $(Hill, Creek, Beach, Quarry, Dune, Glade, Waterfall, Mountain, Haven, Meadow, Pond, Weald)
    
    val ruins = $(Glade, Mountain, Beach, Waterfall)
    
    val diagonals = $((Hill, Weald), (Quarry, Haven))

    val inner = $(Beach, Glade, Waterfall)

    def connected(c : Clearing) = c match {
        case Hill => $(Creek, Beach, Dune)
        case Glade => $(Beach, Waterfall, Pond, Haven, Dune)
        case Meadow => $(Pond, Haven)
        case Mountain => $(Weald, Waterfall, Quarry)
        case Haven => $(Glade, Meadow, Dune)
        case Creek => $(Quarry, Hill)
        case Beach => $(Quarry, Glade, Hill)
        case Weald => $(Mountain, Pond, Waterfall)
        case Dune => $(Hill, Glade, Haven)
        case Waterfall => $(Mountain, Weald, Glade)
        case Pond => $(Weald, Meadow, Glade)
        case Quarry => $(Mountain, Creek, Beach)
    }

    val AutumnN = Forest("AutumnN")
    val AutumnNW = Forest("AutumnNW")
    val Witchwood = Forest("Witchwood", "Witchwood")
    val AutumnW = Forest("AutumnW")
    val AutumnS = Forest("AutumnS")
    val AutumnE = Forest("AutumnE")
    val AutumnSW = Forest("AutumnSW")

    val forests = $(AutumnN, AutumnNW, Witchwood, AutumnW, AutumnS, AutumnE, AutumnSW)

    def fromForest(f : Forest) = f match {
        case AutumnN => $(Hill, Creek, Quarry, Beach)
        case AutumnNW => $(Dune, Hill, Beach, Glade)
        case Witchwood => $(Glade, Beach, Quarry, Mountain, Waterfall)
        case AutumnW => $(Dune, Glade, Haven)
        case AutumnS => $(Glade, Waterfall, Weald, Pond)
        case AutumnE => $(Waterfall, Mountain, Weald)
        case AutumnSW => $(Haven, Glade, Pond, Meadow)
    }

    def byRiver(f : Clearing) = f match {
        case Haven => $(Pond)
        case Creek => $(Beach)
        case Beach => $(Creek, Waterfall)
        case Waterfall => $(Beach, Pond)
        case Pond => $(Waterfall, Haven)
        case _ => Nil
    }
}

object WinterBoard extends Board {
    val name = "Winter"
 
    val Mound = Clearing("Mound")(1)
    val Trench = Clearing("Trench")(2)
    val Pit = Clearing("Pit")(2)
    val Moor = Clearing("Moor")(1)

    val Drift = Clearing("Drift")(1)
    val Wade = Clearing("Wade")(3)
    val Bend = Clearing("Bend")(3)
    val Ford = Clearing("Ford")(1)

    val Spire = Clearing("Spire")(2)
    val Rock = Clearing("Rock")(2)
    val Dale = Clearing("Dale")(2)
    val Hedge = Clearing("Hedge")(2)

    val clearings = $(Mound, Trench, Pit, Moor, Drift, Wade, Bend, Ford, Spire, Rock, Dale, Hedge)
    
    val ruins = $(Wade, Bend, Rock, Dale)
    
    val diagonals = $((Mound, Hedge), (Moor, Spire))

    val inner = $(Wade, Bend)
    
    val WinterNW = Forest("WinterNW")
    val Deadwood = Forest("Deadwood", "Deadwood")
    val WinterNE = Forest("WinterNE")
    val WinterW = Forest("WinterW")
    val WinterSW = Forest("WinterSW")
    val Mirkwood = Forest("Mirkwood", "Mirkwood")
    val WinterSE = Forest("WinterSE")
    val WinterE = Forest("WinterE")

    val forests = $(WinterNW, Deadwood, WinterNE, WinterW, WinterSW, Mirkwood, WinterSE, WinterE)

    def connected(c : Clearing) = c match {
        case Mound => $(Trench, Wade, Drift)
        case Trench => $(Mound, Pit)
        case Pit => $(Trench, Moor)
        case Moor => $(Ford, Bend, Pit)
        case Drift => $(Mound, Spire)
        case Wade => $(Rock, Spire, Mound)
        case Bend => $(Moor, Hedge, Dale)
        case Ford => $(Hedge, Moor)
        case Spire => $(Wade, Rock, Drift)
        case Rock => $(Wade, Dale, Spire)
        case Dale => $(Bend, Hedge, Rock)
        case Hedge => $(Ford, Dale, Bend)
    }

    def fromForest(f : Forest) = f match {
        case WinterNW => $(Mound, Drift, Wade)
        case Deadwood => $(Mound, Trench, Pit, Moor, Bend, Wade)
        case WinterNE => $(Moor, Bend, Ford)
        case WinterW=> $(Drift, Wade, Spire)
        case WinterSW => $(Wade, Spire, Rock)
        case Mirkwood => $(Wade, Bend, Dale, Rock)
        case WinterSE => $(Bend, Dale, Hedge)
        case WinterE=> $(Bend, Ford, Hedge)
    }

    def byRiver(f : Clearing) = f match {
        case Drift => $(Wade)
        case Wade => $(Drift, Bend)
        case Bend => $(Wade, Ford)
        case Ford => $(Bend)
        case _ => Nil
    }
}

object LakeBoard extends Board {
    val name = "Lake"
 
    val Den = Clearing("Den")(1)
    val Prairie = Clearing("Prairie")(1)
    val Vert = Clearing("Vert")(2)
    val Shade = Clearing("Shade")(1)
    val Lawn = Clearing("Lawn")(1)
    val Shoal = Clearing("Shoal")(3)
    val Bay = Clearing("Bay")(3)
    val Yard = Clearing("Yard")(3)
    val Grove = Clearing("Grove")(1)
    val Marsh = Clearing("Marsh")(3)
    val Alley = Clearing("Alley")(1)
    val Gulf = Clearing("Gulf")(2)

    val clearings = $(Den, Prairie, Vert, Shade, Lawn, Shoal, Bay, Yard, Grove, Marsh, Alley, Gulf)
    
    val ruins = $(Marsh, Shoal, Bay, Yard)
    
    val diagonals = $((Den, Gulf), (Shade, Grove))
    
    val inner = $(Marsh, Shoal, Bay)

    override val coastal = $(Gulf, Marsh, Shoal, Bay)

    val LakeW = Forest("LakeW")
    val LakeNW = Forest("LakeNW")
    val LakeN = Forest("LakeN")
    val LakeNE = Forest("LakeNE")
    val AcresWild = Forest("AcresWild", "Acres Wild")
    val HeartOfOak = Forest("HeartOfOak", "Heart of Oak")
    val LakeS = Forest("LakeS")
    val LakeSE = Forest("LakeSE")
    val LakeE = Forest("LakeE")
 
    val forests = $(LakeW, LakeNW, LakeN, LakeNE, AcresWild, HeartOfOak, LakeS, LakeSE, LakeE)
 
    def connected(c : Clearing) = c match {
        case Den => $(Prairie, Shoal, Lawn)
        case Prairie => $(Den, Shoal, Bay, Vert)
        case Vert => $(Prairie, Bay, Shade)
        case Shade => $(Vert, Yard)
        case Lawn => $(Den, Shoal, Grove)
        case Shoal => $(Lawn, Den, Prairie)
        case Bay => $(Prairie, Vert, Yard)
        case Yard => $(Shade, Bay, Gulf)
        case Grove => $(Lawn, Marsh, Alley)
        case Marsh => $(Grove, Alley)
        case Alley => $(Marsh, Gulf, Grove)
        case Gulf => $(Alley, Yard)
    }

    def fromForest(f : Forest) = f match {
        case LakeW => $(Den, Shoal, Lawn)
        case LakeNW => $(Den, Shoal, Prairie)
        case LakeN => $(Shoal, Prairie, Bay)
        case LakeNE => $(Vert, Prairie, Bay)
        case AcresWild => $(Vert, Shade, Yard, Bay)
        case HeartOfOak => $(Marsh, Grove, Lawn, Shoal)
        case LakeS => $(Grove, Marsh, Alley)
        case LakeSE => $(Marsh, Alley, Gulf)
        case LakeE => $(Bay, Yard, Gulf)
    }

    def byRiver(f : Clearing) = f match {
        case Marsh => $(Shoal, Bay, Gulf)
        case Shoal => $(Marsh, Bay, Gulf)
        case Bay => $(Marsh, Shoal, Gulf)
        case Gulf => $(Marsh, Shoal, Bay)
        case _ => Nil
    }
}

object MountainBoard extends Board {
    val name = "Mountain"
 
    val Slope = Clearing("Slope")(2)
    val Ledge = Clearing("Ledge")(3)
    val Mine = Clearing("Mine")(1)
    val Peak = Clearing("Peak")(2)
    val Brim = Clearing("Brim")(1)
    val Pass = Clearing("Pass")(2)
    val Valley = Clearing("Valley")(3)
    val Ridge = Clearing("Ridge")(1)
    val Drain = Clearing("Drain")(2)
    val Ramp = Clearing("Ramp")(3)
    val Cliff = Clearing("Cliff")(1)
    val Crest = Clearing("Crest")(2)

    val clearings = $(Slope, Ledge, Mine, Peak, Brim, Pass, Valley, Ridge, Drain, Ramp, Cliff, Crest)
    
    val ruins = $(Ledge, Pass, Valley, Ramp)
    
    val diagonals = $((Slope, Crest), (Drain, Peak))
    
    val inner = $(Pass, Valley)

    override val rubble = $((Brim, Ledge), (Ledge, Mine), (Mine, Peak), (Ramp, Valley), (Valley, Ridge), (Cliff, Crest))
    
    override val tower = $(Pass)

    val MountainW = Forest("MountainW")
    val MountainNW = Forest("MountainNW")
    val MountainCW = Forest("MountainCW")
    val MountainC = Forest("MountainC")
    val MountainCN = Forest("MountainCN")
    val MountainN = Forest("MountainN")
    val MountainNE = Forest("MountainNE")
    val MountainSE = Forest("MountainSE")
    val LiarWind = Forest("LiarWind", "Liar Wind")
    val NadaMoonshine = Forest("NadaMoonshine", "Nada Moonshine")
 
    val forests = $(MountainW, MountainNW, MountainCW, MountainC, MountainCN, MountainN, MountainNE, MountainSE, LiarWind, NadaMoonshine)
 
    def connected(c : Clearing) = c match {
        case Slope => $(Ledge, Brim)
        case Ledge => $(Slope, Mine, Brim, Pass, Ramp)
        case Mine => $(Ledge, Peak, Pass, Valley)
        case Peak => $(Mine, Valley, Ridge)
        case Brim => $(Slope, Ledge, Drain)
        case Pass => $(Ledge, Mine, Valley, Ramp)
        case Valley => $(Mine, Peak, Pass, Ridge, Ramp, Crest)
        case Ridge => $(Peak, Valley, Crest)
        case Drain => $(Brim, Ramp)
        case Ramp => $(Ledge, Pass, Valley, Drain, Cliff)
        case Cliff => $(Ramp, Crest)
        case Crest => $(Valley, Ridge, Cliff)
    }

    def fromForest(f : Forest) = f match {
        case MountainW => $(Slope, Ledge, Brim)
        case MountainNW => $(Ledge, Mine, Pass)
        case MountainCW => $(Ledge, Pass, Ramp)
        case MountainC => $(Pass, Valley, Ramp)
        case MountainCN => $(Mine, Pass, Valley)
        case MountainN => $(Mine, Peak, Valley)
        case MountainNE => $(Peak, Valley, Ridge)
        case MountainSE => $(Valley, Ridge, Crest)
        case LiarWind => $(Ledge, Brim, Drain, Ramp)
        case NadaMoonshine => $(Valley, Ramp, Cliff, Crest)
    }

    def byRiver(f : Clearing) = f match {
        case Peak => $(Pass)
        case Pass => $(Peak, Drain)
        case Drain => $(Pass)
        case _ => Nil
    }
}


trait Effect extends Elementary with Record {
    def name : String = toString
    def elem = name.hl
}

trait FactionEffect extends Effect {
    def of(f : Faction) = name.styled(f)
}

trait BattleEffect extends Effect

case object Defenseless extends BattleEffect

trait HitTarget

trait Card extends Elementary {
    def name : String
    def suit : Suit
    def elem = (" " + name + " ").styled(xlo.pre, xstyles.outlined, styles.get(suit))
    def img : Image
}

case object Card extends Message {
    implicit class CardCast(val c : Card) {
        def dominance = c match {
            case d : Dominance => Some(d)
            case _ => None
        }
    }

    def elem(g : Game) = "card"
}

case class Cards(n : Int) extends Message with Elementary {
    def elem(g : Game) = (n == 1).?("card").|("cards")
    def elem = (n == 1).?("card").|("cards")
}

case object Cards extends Message {
    def elem(g : Game) = "cards"
}

object NCards extends {
    def apply(n : Int) = (n == 1).?("card").|("cards")
}


trait DeckCard extends Card {
    def id : String
    def img = Image(id, styles.card)
    override def toString = "DeckCard(" + id + ")"
}

trait CraftCard extends DeckCard {
    def cost : List[CraftCost]
}

case class Ambush(suit : Suit) extends DeckCard {
    def name = suit.name + " Ambush!"
    def id = suit.name.toLowerCase + "-ambush"
}

case class CraftItemCard(suit : Suit, id : String, cost : List[CraftCost], item : Item, vp : Int, name : String) extends CraftCard
case class CraftEffectCard(suit : Suit, id : String, cost : List[CraftCost], effect : Effect) extends CraftCard {
    def name = effect.name
}
case class Favor(suit : BaseSuit, id : String, name : String) extends CraftCard {
    val cost = $(suit, suit, suit)
}
case class Dominance(suit : Suit) extends DeckCard {
    def name = suit.name + " Dominance"
    def id = suit.name.toLowerCase + "-dominance"
}

trait Phase extends Elementary with Record {
    def elem = toString.styled(styles.phase)
}

case object Birdsong extends Phase
case object Daylight extends Phase
case object Evening extends Phase
case object Night extends Phase

// Birdsong (start)
case object BetterBurrowBank extends Effect {
    override def name = "Better Burrow Bank"
}

case object Saboteurs extends Effect

// Birdsong
case object RoyalClaim extends Effect {
    override def name = "Royal Claim"
}

case object StandAndDeliver extends Effect {
    override def name = "Stand and Deliver!"
}

case object FalseOrders extends Effect {
    override def name = "False Orders"
}

case object SwapMeet extends Effect {
    override def name = "Swap Meet"
}


// Birdsong (end)
case object EyrieEmigre extends Effect {
    override def name = "Eyrie \u00c9migr\u00e9"
}

// Daylight (start)
case object CommandWarren extends Effect {
    override def name = "Command Warren"
}

// Daylight
case object TaxCollector extends Effect {
    override def name = "Tax Collector"
}

case object Codebreakers extends Effect

case object PropagandaBureau extends Effect {
    override def name = "Propaganda Bureau"
}

case object LeagueOfAdventurousMice extends Effect {
    override def name = "League of Adventurous Mice"
}

// Evening (start)
case object Cobbler extends Effect

case object CharmOffensive extends Effect {
    override def name = "Charm Offensive"
}


// Evening
case object Informants extends Effect


// Passive
case object SoupKitchens extends Effect {
    override def name = "Soup Kitchens"
}

case object BoatBuilders extends Effect {
    override def name = "Boat Builders"
}

case object CorvidPlanners extends Effect {
    override def name = "Corvid Panners"
}

case object Tunnels extends Effect

case object MurineBroker extends Effect {
    override def name = "Murine Brokers"
}

case object MasterEngravers extends Effect {
    override def name = "Master Engravers"
}

case object CoffinMakers extends Effect {
    override def name = "Coffin Makers"
}


// Battle
case object Armorers extends BattleEffect

case object Sappers extends BattleEffect

case object BrutalTactics extends BattleEffect {
    override def name = "Brutal Tactics"
}

case object ScoutingParty extends BattleEffect {
    override def name = "Scouting Party"
}

case class Partisans(suit : BaseSuit) extends BattleEffect {
    override def name = suit.name + " Partisans"
}


object Deck {
    def fromOptions(l : List[GameOption], arity : Int, setup : List[Faction]) = l.of[DeckOption].last @@ {
        case StandardDeck => ambushes ++ items ++ effects ++ (arity > 2).??(dominances)
        case ExilesDeck => ambushes ++ items ++ effects2 ++ (arity > 2).??(dominances)
        case ExilesFavorsDeck => ambushes ++ items ++ effects2 ++ favors ++ (arity > 2).??(dominances)
        case MixedDeck => ambushes ++ items ++ mixed(setup) ++ (arity > 2).??(dominances)
    }

    val ambushes = $[DeckCard](
        Ambush(Bird),
        Ambush(Bird),
        Ambush(Fox),
        Ambush(Rabbit),
        Ambush(Mouse)
    )

    val dominances = $[DeckCard](
        Dominance(Bird),
        Dominance(Fox),
        Dominance(Rabbit),
        Dominance(Mouse)
    )

    val items = $[DeckCard](
        CraftItemCard(Bird, "birdy-bindle", $(Mouse), Bag, 1, "Birdy Bindle"),
        CraftItemCard(Bird, "woodland-runners", $(Rabbit), Boots, 1, "Woodland Runners"),
        CraftItemCard(Bird, "arms-trader", $(Fox, Fox), Sword, 2, "Arms Trader"),
        CraftItemCard(Bird, "bird-crossbow", $(Fox), Crossbow, 1, "Crossbow"),
        CraftItemCard(Fox, "gently-used-knapsack", $(Mouse), Bag, 1, "Gently Used Knapsack"),
        CraftItemCard(Fox, "fox-root-tea", $(Mouse), Teapot, 2, "Root Tea"),
        CraftItemCard(Fox, "fox-travel-gear", $(Rabbit), Boots, 1, "Travel Gear"),
        CraftItemCard(Fox, "protection-racket", $(Rabbit, Rabbit), Coins, 3, "Protection Racket"),
        CraftItemCard(Fox, "foxfolk-steel", $(Fox, Fox), Sword, 2, "Foxfolk Steel"),
        CraftItemCard(Fox, "anvil", $(Fox), Hammer, 2, "Anvil"),
        CraftItemCard(Rabbit, "smugglers-trail", $(Mouse), Bag, 1, "Smuggler's Trail"),
        CraftItemCard(Rabbit, "rabbit-root-tea", $(Mouse), Teapot, 2, "Root Tea"),
        CraftItemCard(Rabbit, "a-visit-to-friends", $(Rabbit), Boots, 1, "A Visit to Friends"),
        CraftItemCard(Rabbit, "bake-sale", $(Rabbit, Rabbit), Coins, 3, "Bake Sale"),
        CraftItemCard(Mouse, "mouse-in-a-sack", $(Mouse), Bag, 1, "Mouse-in-a-Sack"),
        CraftItemCard(Mouse, "mouse-root-tea", $(Mouse), Teapot, 2, "Root Tea"),
        CraftItemCard(Mouse, "mouse-travel-gear", $(Rabbit), Boots, 1, "Travel Gear"),
        CraftItemCard(Mouse, "investments", $(Rabbit, Rabbit), Coins, 3, "Investments"),
        CraftItemCard(Mouse, "sword", $(Fox, Fox), Sword, 2, "Sword"),
        CraftItemCard(Mouse, "mouse-crossbow", $(Fox), Crossbow, 1, "Crossbow"),
    )

    val effects = $[DeckCard](
        CraftEffectCard(Bird, "armorers", $(Fox), Armorers),
        CraftEffectCard(Bird, "armorers", $(Fox), Armorers),
        CraftEffectCard(Bird, "sappers", $(Mouse), Sappers),
        CraftEffectCard(Bird, "sappers", $(Mouse), Sappers),
        CraftEffectCard(Bird, "brutal-tactics", $(Fox, Fox), BrutalTactics),
        CraftEffectCard(Bird, "brutal-tactics", $(Fox, Fox), BrutalTactics),
        CraftEffectCard(Bird, "royal-claim", $(AnySuit, AnySuit, AnySuit, AnySuit), RoyalClaim),
        CraftEffectCard(Fox, "stand-and-deliver", $(Mouse, Mouse, Mouse), StandAndDeliver),
        CraftEffectCard(Fox, "stand-and-deliver", $(Mouse, Mouse, Mouse), StandAndDeliver),
        CraftEffectCard(Fox, "tax-collector", $(Fox, Rabbit, Mouse), TaxCollector),
        CraftEffectCard(Fox, "tax-collector", $(Fox, Rabbit, Mouse), TaxCollector),
        CraftEffectCard(Fox, "tax-collector", $(Fox, Rabbit, Mouse), TaxCollector),
        Favor(Fox, "favor-of-the-foxes", "Favor of the Foxes"),
        CraftEffectCard(Rabbit, "command-warren", $(Rabbit, Rabbit), CommandWarren),
        CraftEffectCard(Rabbit, "command-warren", $(Rabbit, Rabbit), CommandWarren),
        CraftEffectCard(Rabbit, "better-burrow-bank", $(Rabbit, Rabbit), BetterBurrowBank),
        CraftEffectCard(Rabbit, "better-burrow-bank", $(Rabbit, Rabbit), BetterBurrowBank),
        CraftEffectCard(Rabbit, "cobbler", $(Rabbit, Rabbit), Cobbler),
        CraftEffectCard(Rabbit, "cobbler", $(Rabbit, Rabbit), Cobbler),
        Favor(Rabbit, "favor-of-the-rabbits", "Favor of the Rabbits"),
        CraftEffectCard(Mouse, "scouting-party", $(Mouse, Mouse), ScoutingParty),
        CraftEffectCard(Mouse, "scouting-party", $(Mouse, Mouse), ScoutingParty),
        CraftEffectCard(Mouse, "codebreakers", $(Mouse), Codebreakers),
        CraftEffectCard(Mouse, "codebreakers", $(Mouse), Codebreakers),
        Favor(Mouse, "favor-of-the-mice", "Favor of the Mice"),
    )

    val favors = effects.%(_.isInstanceOf[Favor])

    def mixed(setup : List[Faction]) = $[DeckCard](
        CraftEffectCard(Bird, "armorers", $(Fox), Armorers),
        CraftEffectCard(Bird, "sappers", $(Mouse), Sappers), 
        CraftEffectCard(Bird, "brutal-tactics", $(Fox, Fox), BrutalTactics),
        CraftEffectCard(Bird, "royal-claim", $(AnySuit, AnySuit, AnySuit, AnySuit), RoyalClaim),
        CraftEffectCard(Bird, "eyrie-emigre", $(Fox, Fox), EyrieEmigre),
        CraftEffectCard(Fox, "stand-and-deliver", $(Mouse, Mouse, Mouse), StandAndDeliver),
        CraftEffectCard(Fox, "tax-collector", $(Fox, Rabbit, Mouse), TaxCollector),
        Favor(Fox, "favor-of-the-foxes", "Favor of the Foxes"),
        CraftEffectCard(Fox, "fox-partisans", $(Fox), Partisans(Fox)),
        CraftEffectCard(Fox, "propaganda-bureau", $(AnySuit, AnySuit, AnySuit), PropagandaBureau),
        CraftEffectCard(Fox, "false-orders", $(Fox), FalseOrders),
        CraftEffectCard(Fox, "informants", $(Fox, Fox), Informants), 
        CraftEffectCard(Rabbit, "command-warren", $(Rabbit, Rabbit), CommandWarren),
        CraftEffectCard(Rabbit, "better-burrow-bank", $(Rabbit, Rabbit), BetterBurrowBank),
        CraftEffectCard(Rabbit, "cobbler", $(Rabbit, Rabbit), Cobbler),
        Favor(Rabbit, "favor-of-the-rabbits", "Favor of the Rabbits"),
        CraftEffectCard(Rabbit, "rabbit-partisans", $(Rabbit), Partisans(Rabbit)),
        CraftEffectCard(Rabbit, "tunnels", $(Rabbit), Tunnels),
        CraftEffectCard(Rabbit, "charm-offensive", $(Rabbit), CharmOffensive),
        CraftEffectCard(Rabbit, "swap-meet", $(Rabbit), SwapMeet), 
        CraftEffectCard(Mouse, "codebreakers", $(Mouse), Codebreakers),
        Favor(Mouse, "favor-of-the-mice", "Favor of the Mice"),
        CraftEffectCard(Mouse, "mouse-partisans", $(Mouse), Partisans(Mouse)),
        CraftEffectCard(Mouse, "league-of-adventurous-mice", $(Mouse), LeagueOfAdventurousMice),
        CraftEffectCard(Mouse, "murine-broker", $(Mouse, Mouse), MurineBroker),
        CraftEffectCard(Mouse, "master-engravers", $(Mouse, Mouse), MasterEngravers),
    ) ++ (Nil ++
        setup.of[Mischief].none.?(CraftEffectCard(Bird, "corvid-planners", $(AnySuit, AnySuit), CorvidPlanners)) ++
        setup.has(RF).not.?(CraftEffectCard(Bird, "boat-builders", $(AnySuit, AnySuit), BoatBuilders)) ++
        setup.of[Insurgent].any.?(CraftEffectCard(Bird, "soup-kitchens", $(Fox, Rabbit, Mouse), SoupKitchens)) ++
        $(CraftEffectCard(Bird, "saboteurs", $(AnySuit), Saboteurs))
    ).take(3)

    val effects2 = $[DeckCard](
        CraftEffectCard(Bird, "saboteurs", $(AnySuit), Saboteurs),
        CraftEffectCard(Bird, "saboteurs", $(AnySuit), Saboteurs),
        CraftEffectCard(Bird, "saboteurs", $(AnySuit), Saboteurs),
        CraftEffectCard(Bird, "soup-kitchens", $(Fox, Rabbit, Mouse), SoupKitchens),
        CraftEffectCard(Bird, "boat-builders", $(AnySuit, AnySuit), BoatBuilders),
        CraftEffectCard(Bird, "corvid-planners", $(AnySuit, AnySuit), CorvidPlanners),
        CraftEffectCard(Bird, "eyrie-emigre", $(Fox, Fox), EyrieEmigre),
        CraftEffectCard(Fox, "fox-partisans", $(Fox), Partisans(Fox)),
        CraftEffectCard(Fox, "propaganda-bureau", $(AnySuit, AnySuit, AnySuit), PropagandaBureau),
        CraftEffectCard(Fox, "false-orders", $(Fox), FalseOrders),
        CraftEffectCard(Fox, "false-orders", $(Fox), FalseOrders),
        CraftEffectCard(Fox, "informants", $(Fox, Fox), Informants),
        CraftEffectCard(Fox, "informants", $(Fox, Fox), Informants),
        CraftEffectCard(Rabbit, "rabbit-partisans", $(Rabbit), Partisans(Rabbit)),
        CraftEffectCard(Rabbit, "tunnels", $(Rabbit), Tunnels),
        CraftEffectCard(Rabbit, "tunnels", $(Rabbit), Tunnels),
        CraftEffectCard(Rabbit, "charm-offensive", $(Rabbit), CharmOffensive),
        CraftEffectCard(Rabbit, "swap-meet", $(Rabbit), SwapMeet),
        CraftEffectCard(Rabbit, "swap-meet", $(Rabbit), SwapMeet),
        CraftEffectCard(Rabbit, "coffin-makers", $(Rabbit, Rabbit), CoffinMakers),
        CraftEffectCard(Mouse, "mouse-partisans", $(Mouse), Partisans(Mouse)),
        CraftEffectCard(Mouse, "league-of-adventurous-mice", $(Mouse), LeagueOfAdventurousMice),
        CraftEffectCard(Mouse, "league-of-adventurous-mice", $(Mouse), LeagueOfAdventurousMice),
        CraftEffectCard(Mouse, "murine-broker", $(Mouse, Mouse), MurineBroker),
        CraftEffectCard(Mouse, "master-engravers", $(Mouse, Mouse), MasterEngravers),
    )
    
    val full = ambushes ++ dominances ++ items ++ effects ++ effects2
}

sealed trait Item extends Record with Elementary {
    def name = toString
    def elem = name.styled(styles.itemText)
    def imgid = "item-" + name
    def img = Image(imgid, styles.piece)
    def imgEmpty = Image(imgid + "-empty", styles.piece)
    def imgD = Image(imgid, styles.iii)
    def imgEmptyD = Image(imgid + "-empty", styles.iii)
    def ref(exhausted : Boolean, damaged : Boolean) = ItemRef(this, exhausted, damaged)
    def pristine = ref(false, false)
    def exhaust = ref(true, false)
    def damage = ref(false, true)
}

case object Teapot extends Item
case object Boots extends Item
case object Sword extends Item
case object Torch extends Item
case object Crossbow extends Item
case object Hammer extends Item
case object Coins extends Item
case object Bag extends Item

case class ItemRef(item : Item, exhausted : Boolean, damaged : Boolean) extends HitTarget with Elementary {
    def elem = (damaged, exhausted) match {
        case (true , true ) => item.name.styled(styles.itemText, styles.damexh)
        case (true , false) => item.name.styled(styles.itemText, styles.damaged)
        case (false, true ) => item.name.styled(styles.itemText, styles.exhausted)
        case (false, false) => item.name.styled(styles.itemText)
    }
    def imgid = "item-" + item.name + damaged.??("-damaged") + exhausted.??("-exhausted")
    def img = Image(imgid, styles.piece)
    def imgD = Image(imgid, styles.iii)
    def exhaust = item.ref(true, damaged)
    def refresh = item.ref(false, damaged)
    def damage = item.ref(exhausted, true)
    def repair = item.ref(exhausted, false)
    def pristine = item.ref(false, false)
}

object ItemRef {
    implicit class ItemRefList(val l : List[ItemRef]) extends AnyVal {
        def ready = l.notExhausted.notDamaged
        def exhausted = l.%(_.exhausted)
        def damaged = l.%(_.damaged)
        def notExhausted = l.%!(_.exhausted)
        def notDamaged = l.%!(_.damaged)
        def count(i : Item) = l.%(_.item == i).num
        def sort = Item.order./~(i => l.%(_.item == i).sortBy(r => r.exhausted.??(1) + r.damaged.??(2)))
    }
}

object Item {
    def track = $(Teapot, Coins, Bag)./~(i => List.fill(3)(i.ref(false, false)))
    def mucho = order./~(i => List.fill(4)(i.ref(false, false)))
    def order = $(Teapot, Boots, Sword, Torch, Crossbow, Hammer, Coins, Bag)
}

trait QuasiItem {
    def elem(selected : Boolean) : Image
    def available : Boolean
    def real = this match {
        case r : RealItem => Some(r.ref)
        case _ => None
    }
    def figure = this match {
        case FigureRemove(ref) => Some(ref)
        case _ => None
    }
}

trait RealItem extends QuasiItem {
    def ref : ItemRef
}

object ItemEmptySpace extends QuasiItem {
    def elem(selected : Boolean) = Image("item-x-spacer")
    def available = false
}

object ItemPlaceholder extends QuasiItem {
    def elem(selected : Boolean) = Image("item-x-placeholder")
    def available = false
}

case class FigureRemove(ref : Figure) extends QuasiItem {
    def elem(selected : Boolean) = Image(selected.?(ref.piece.iem(ref.faction)).|(ref.piece.imgid(ref.faction)))
    def available = true
}

case class ToRefresh(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(ref.refresh).|(ref).imgid)
    def available = ref.exhausted
}

case class ToExhaust(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(ref.exhaust).|(ref).imgid)
    def available = ref.exhausted.not && ref.damaged.not
}

case class ToExhaustDamage(l : List[Item], ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(l.has(ref.item).?(ref.exhaust).|(ref.exhaust.damage)).|(ref).imgid)
    def available = ref.exhausted.not && ref.damaged.not
}


case class ToRepair(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(ref.repair).|(ref).imgid)
    def available = ref.damaged
}

case class ToDamage(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(ref.damage).|(ref).imgid)
    def available = ref.damaged.not
}

case class ToDiscard(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?("item-" + ref.item.name + "-empty").|(ref.imgid))
    def available = true
}

case class ToShow(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(ref.imgid)
    def available = false
}


trait Faction extends Player with Elementary with Styling { self =>
    val name : String
    val short : String
    val style : String

    def advertising : Elem
    def motto : Elem

    val pieces : List[Piece]
    val abilities : List[Effect]

    val transports : List[List[Transport]] = $($(RuledMove))

    def funName : Elem = NameReference(name, this)

    def note : Elem = Empty

    val priority : String

    val expansion : Expansion
    
    def ss : Elem = short.styled(this)

    def elem : Elem = name.styled(this).styled(styles.condensed, styles.italic)

    def warf = self match {
        case f : WarriorFaction => Some(f)
        case _ => None
    }
    
    def feline = self match {
        case f : Feline => Some(f)
        case _ => None
    }

    def aviary = self match {
        case f : Aviary => Some(f)
        case _ => None
    }

    def insurgent = self match {
        case f : Insurgent => Some(f)
        case _ => None
    }

    def trader = self match {
        case f : Trader => Some(f)
        case _ => None
    }

    def fanatic = self match {
        case f : Fanatic => Some(f)
        case _ => None
    }

    def mischief = self match {
        case f : Mischief => Some(f)
        case _ => None
    }

    def hero = self match {
        case f : Hero => Some(f)
        case _ => None
    }
    
    def underground = self match {
        case f : Underground => Some(f)
        case _ => None
    }

    def expedition = self match {
        case f : Expedition => Some(f)
        case _ => None
    }

    def horde = self match {
        case f : Horde => Some(f)
        case _ => None
    }

    protected implicit class P2F(val p : Piece) {
        def **(n : Int) = 1.to(n)./(_ => p)
    }
    
    def figures : List[Figure] = pieces.distinct./~(p => 1.to(pieces.count(p))./(Figure(self, p, _)))
}

trait WarriorFaction extends Faction with Fund {
    def warrior : Warrior
}

sealed trait Piece extends Record {
    def id = toString
    def name = id
    def plural = name + "s"
    def of(f : Faction) : Elem = name.styled(f)
    def sof(f : Faction) = plural.styled(f)

    def imgid(f : Faction) = f.short + "-" + id

    def iem(f : Faction) = this match {
        case _ : Warrior => f.short + "-" + id + "-empty"
        case _ : Building => "empty-building"
        case _ : Token => "empty-token"
    }

    def img(f : Faction) : Elem = Image(imgid(f), styles.piece)

    def imgd(f : Faction) = Image(imgid(f), this match {
        case _ : Warrior => styles.fund 
        case _ : Building => styles.dt.building
        case _ : Token => styles.dt.token
    })

    def imged(f : Faction) = this match {
        case _ : Warrior => Image(iem(f), styles.fund)
        case _ : Building => Image(iem(f), styles.dt.building)
        case _ : Token => Image(iem(f), styles.dt.token)
    }
}

object Piece {
    implicit class PieceList(val p : List[Piece]) {
        def ofFaction(f : Faction) = p./(_.of(f))
    }

    implicit class PieceCast(val p : Piece) {
        def building = p match {
            case b : Building => Some(b)
            case _ => None
        }
        def warrior = p match {
            case w : Warrior => Some(w)
            case _ => None
        }
        def warriorNotPawn = p match {
            case _ : Pawn => None
            case w : Warrior => Some(w)
            case _ => None
        }
        def token = p match {
            case t : Token => Some(t)
            case _ => None
        }
        def pawn = p match {
            case t : Pawn => Some(t)
            case _ => None
        }
        def plot = p match {
            case t : Plot => Some(t)
            case _ => None
        }
        def attacking = p match {
            case w : Warrior => Some(w)
            case p : Pawn => Some(p)
            case _ => None
        }
        def movable = p match {
            case m : Movable => Some(m)
            case _ => None
        }
        def ruling = p match {
            case b : Building => Some(b)
            case w : Warrior => Some(w)
            case _ => None
        }
        def scoring = p match {
            case t : Token => Some(t)
            case b : Building => Some(b)
            case _ => None
        }
        def relic = p match { 
            case r : Relic => Some(r)
            case _ => None
        }
    }
}  

case class Figure(faction : Faction, piece : Piece, index : Int) extends HitTarget with Elementary {
    def elem = piece.of(faction)
    def imgid = piece.imgid(faction)

    def priority = piece match {
        case _ : Pawn => 4
        case _ : Warrior => 3
        case _ : Token => 2
        case _ : Building => 1
    }
}

object Figure {
    implicit class FiguresEx[T](val t : T)(implicit val conv : T => List[Figure]) {
        def l = conv(t)
        def of(f : Faction) = l.%(_.faction == f)
        def one(p : Piece) = l.%(_.piece == p).lift(0).|!("one " + p + " not found among " + l.mkString(" | "))
        def factions = l./(_.faction).distinct
        def count(p : Piece) = l.%(u => u.piece == p).num
        def got(p : Piece) = count(p) > 0
        def sublist(s : List[Piece]) = {
            var rest = s
            l./~{ u =>
                if (rest.contains(u.piece)) {
                    rest = rest :- (u.piece)
                    Some(u)
                }
                else
                    None
            }
        }
        def sub(n : Int, p : Piece) = sublist(List.fill(n)(p))

        def warrior = l.%(u => u.piece.warrior.any)
        def warriorNotPawn = l.%(u => u.piece.warrior.any && u.piece.pawn.none)
        def building = l.%(u => u.piece.building.any)
        def token = l.%(u => u.piece.token.any)
        def scoring = l.%(u => u.piece.scoring.any)
        def movable = l.%(u => u.piece.movable.any)
        def attacking = l.%(u => u.piece.attacking.any)

        def plot = l./~(_.piece.plot).single
    }
}

trait Movable extends Piece
trait Scoring extends Piece

trait Token extends Piece with Scoring
trait Building extends Piece with Scoring
trait Warrior extends Piece with Movable {
    override def img(f : Faction) : Elem = Image(f.short + "-" + name, styles.warrior)
}
trait Pawn extends Piece with Movable {
    override def img(f : Faction) : Elem = Image(f.short + "-" + name, styles.warrior)
}

case class Ruins(items : List[Item]) extends Building {
    override def name = "Ruins"
}

case object Tower extends Pawn

trait Transport extends Record {
    def img : Elem = Empty 
    def allows(g : Game, f : Faction) : Boolean = true
    def allows(g : Game, f : Faction, o : Region) : Boolean = allows(g, f)
    def allows(g : Game, f : Faction, o : Region, d : Region) : Boolean = allows(g, f, o)
    def allows(g : Game, f : Faction, o : Region, d : Region, l : List[Movable]) : Boolean = allows(g, f, o, d)
    def order(l : List[Movable]) : Int = 0
    def sortBy(m : Movable) : Int = 0
}

object Transport {
    implicit class AbstractSuitEx(aa : List[List[Transport]]) {
        def **(bb : List[List[Transport]]) = aa./~(a => bb./(b => a ++ b))
    }
}

case object FreeMove extends Transport

case class Party(transport : List[Transport], movable : List[Movable])

case object Ferry extends Transport with Effect with Pawn {
    override def img = Image(name, styles.widepiece)
    override def name = "Ferry"
    
    override def allows(g : Game, f : Faction) = g.ferry.any && g.of(f).used.has(Ferry).not

    override def allows(g : Game, f : Faction, o : Region) = allows(g, f) && o @@ {
        case o : Clearing => g.ferry.has(o)
        case _ => false
    }

    override def allows(g : Game, f : Faction, o : Region, d : Region) = allows(g, f, o) && (o, d) @@ {
        case (o : Clearing, d : Clearing) => g.board.byRiver(o).has(d)
        case _ => false
    }
}

case object Roads extends Transport {
    override def allows(g : Game, f : Faction, o : Region, d : Region) = (o, d) @@ {
        case (o : Clearing, d : Clearing) => g.board.connected(o).has(d)
        case (o : Forest, d : Clearing) => g.fromForest(o).has(d)
        case _ => false
    }
}

case object BurrowRoads extends Transport {
    override def allows(g : Game, f : Faction) = f @@ {
        case f : Underground => true
        case _ => false
    }
    
    override def allows(g : Game, f : Faction, o : Region) = (f, o) @@ {
        case (f : Underground, o : Burrow) => o == g.ud(f).burrow
        case (f : Underground, o : Clearing) => g.at(o, f).%(_ == Tunnel).any
        case _ => false
    }
    
    override def allows(g : Game, f : Faction, o : Region, d : Region) = (f, o, d) @@ {
        case (f : Underground, o : Burrow, d : Clearing) => o == g.ud(f).burrow && g.at(d, f).%(_ == Tunnel).any
        case (f : Underground, o : Clearing, d : Burrow) => g.at(o, f).%(_ == Tunnel).any && d == g.ud(f).burrow 
        case _ => false
    }
}

case object TunnelRoads extends Transport {
    override def img = Image("tunnel", styles.widepiece)

    def crafting(g : Game, f : Faction, o : Region) = (f, o) @@ {
        case (f : Feline, o : Clearing) => g.at(o, f).has(Workshop)
        case (f : Aviary, o : Clearing) => g.at(o, f).has(Roost)
        case (f : Insurgent, o : Clearing) => g.at(o, f).has(Sympathy)
        case (f : Trader, o : Clearing) => g.at(o, f).of[TradePost].any 
        case (f : Fanatic, o : Clearing) => g.at(o, f).of[Garden].any 
        case (f : Underground, o : Clearing) => g.at(o, f).of[Building].any
        case (f : Mischief, o : Clearing) => g.at(o, f).of[Plot].any
        case (f : Horde, o : Clearing) => g.at(o, f).has(Stronghold)
        case (f : Expedition, o : Clearing) => g.at(o, f).of[WayStation].any
        case (f : OldExpedition, o : Clearing) => g.at(o, f).has(Station)
        case _ => false
    }

    override def allows(g : Game, f : Faction) = g.of(f).has(Tunnels)
    
    override def allows(g : Game, f : Faction, o : Region) = allows(g, f) && crafting(g, f, o)
    
    override def allows(g : Game, f : Faction, o : Region, d : Region) = allows(g, f, o) && crafting(g, f, d)
}

case object RuledMove extends Transport {
    override def allows(g : Game, f : Faction, o : Region, d : Region) = (o, d) @@ {
        case (o : Region, d : Region) => g.rule(f)(o) || g.rule(f)(d) || g.of(f).has(CorvidPlanners)
        case _ => false
    }
}

case object HalfWarriors extends Transport {
    override def allows(g : Game, f : Faction, o : Region) = (f, o) @@ {
        case (f : Faction, o : Region) => g.FactionEx(f).at(o)./~(_.piece.warriorNotPawn).any
    }

    override def allows(g : Game, f : Faction, o : Region, d : Region, l : List[Movable]) = (f, o, l) @@ {
        case (f : Faction, o : Region, l : List[Movable]) => l.forall(_.warriorNotPawn.any) && (g.FactionEx(f).at(o)./~(_.piece.warrior).num + 1) / 2 == l.num
    }
}


trait PlayerState {
    def game : Game
    def cloned() : PlayerState = null
    def has(e : Effect) = faction.abilities.contains(e) || effects.contains(e) || abilities.contains(e) || services.contains(e)
    def can(e : Effect) = has(e) && !used.contains(e) && !ignored.contains(e)
    
    def figures(name : String, init : List[Figure] = Nil, rule : Figure => Boolean = (e : Figure) => true) = game.pieces.another[Figure](name + "-" + faction.short, init, rule)
    def cards(name : String, init : List[DeckCard] = Nil, rule : DeckCard => Boolean = (e : DeckCard) => true) = game.cards.another[DeckCard](name + "-" + faction.short, init, rule)
    
    var scorelog : List[Elem] = Nil

    def abilities : List[Effect] = Nil
    var effects : List[Effect] = Nil
    var services : List[Effect] = Nil
    var used : List[Effect] = Nil
    var ignored : List[Effect] = Nil

    var vp : Int = 0
    var forTrade : List[ItemRef] = Nil
    var hand = cards("hand")
    var stuck = cards("stuck")
    val drawn = cards("drawn")
    var dominance : Option[Dominance] = None
    var coalition : Option[Faction] = None

    def totalWar = dominance.any && game.options.has(TotalWarDominance)

    var attacked : List[Faction] = Nil

    var crafted : List[CraftAsset] = Nil

    def craft : List[CraftAsset]

    var extracraft : List[CraftAsset] = Nil

    def copyTo(p : PlayerState) {
        p.effects = effects
        p.used = used
        p.vp = vp
        p.forTrade = forTrade
        p.hand = hand
        p.dominance = dominance
        p.coalition = coalition
    }
    
    def faction : Faction
  
    val pool = figures("pool", faction.figures, _.faction == faction)
    var limbo = figures("limbo", Nil, _.faction == faction)
    
    def pooled(p : Piece) = pool.%(_.piece == p).num
    
    def inpool(p : Piece) = pool.%(_.piece == p).any
    
    def all(p : Piece) = game.board.clearings./~(c => game.xclearings(c).%(_.faction == faction).%(_.piece == p)./(_ => c))
    
}

case object MorningCraft extends Effect
case object DaylightCraft extends Effect
case object EveningCraft extends Effect


case object Removing extends Message {
    def elem(g : Game) = Text("removing")
}

case class AsDespot(m : Message) extends Message {
    def elem(g : Game) = "as " ~ Despot.elem ~ " " ~ m.elem(g)
}

case object Striking extends Message {
    def elem(g : Game) = Text("striking")
}

case object Sanctifying extends Message {
    def elem(g : Game) = Text("sanctifying")
}

case class SomeHostile(m : Message) extends Message {
    def elem(g : Game) = m.elem(g) ~ " hostile"
}

case object Recovering extends Message {
    def elem(g : Game) = Text("recovering")
}









case object SuppressLog extends Message {
    def elem(g : Game) = Empty
}

case class ForfeitActions(n : Int) extends Message {
    def elem(g : Game) = "Forfeit " ~ n.hl ~ " action" ~ (n != 1).??("s") ~ "?"
}

case class DrewFromTheDeck(f : Faction, m : Message with GameElementary) extends Message {
    def elem(g : Game) = f.elem ~ " drew from the deck" ~ m.elem(g)
}

case class StoleFrom(f : Faction, e : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " stole from " ~ e.elem
}

case class GaveBack(f : Faction, e : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " gave back " ~ e.elem
}

case class PreBreak(m : Message with GameElementary) extends Message {
    def elem(g : Game) = Break ~ m.elem(g)
}

case object OwnHand extends Message {
    def elem(g : Game) = Text("Hand")
}

case class FactionHand(f : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " hand"
}

case class FactionSupporters(f : Insurgent) extends Message {
    def elem(g : Game) = Supporters.of(f)
}

case object Dominance extends Message {
    def elem(g : Game) = Text("Dominance")
}

case object DiscardPile extends Message {
    def elem(g : Game) = Text("Discard pile")
}

case class IofN(m : Message, i : Int, n : Int) extends Message {
    def elem(g : Game) = m.elem(g) ~ (n > 1).??(" (" ~ i.elem ~ "/" ~ n.elem ~ ")")
}

case class TakeCard(d : DeckCard) extends Message {
    def elem(g : Game) = " to take " ~ d.elem
}

case class ToReplace(c : Clearing, e : Faction, p : Piece) extends Message {
    def elem(g : Game) = " to replace " ~ p.of(e) ~ " in " ~ c.elem(g)
}

case class WageWar(i : Int, n : Int) extends Message {
    def elem(g : Game) = "(Wage War " ~ i.hl ~ "/" ~ n.elem ~ ") "
}

case object BattleCommandWarren extends Message {
    def elem(g : Game) = " with " ~ CommandWarren.elem
}

case class BattleTotalWar(n : Int) extends Message {
    def elem(g : Game) = " with " ~ "Total War".hl ~ " " ~ n.vp
}

case class March(i : Int, n : Int) extends Message {
    def elem(g : Game) = "(March " ~ i.hl ~ "/" ~ n.elem ~ ") "
}

case object MoveCobbler extends Message {
    def elem(g : Game) = " with " ~ Cobbler.elem
}

case class ToClearPath(a : Clearing, b : Clearing) extends Message {
    def elem(g : Game) = "to clear path between " ~ a.elem(g) ~ " and " ~ b.elem(g)
}


case class WithEffect(e : Effect) extends Message {
    def elem(g : Game) = " with " ~ e.elem
}

case class WithCard(d : Card) extends Message {
    def elem(g : Game) = " with " ~ d.elem
}

case object OnBalloon extends Message {
    def elem(g : Game) = " on " ~ "Balloon".hl
}

case class FalseOrders(f : Faction) extends Message {
    def elem(g : Game) = " on " ~ FalseOrders.elem ~ " from " ~ f.elem
}

case class InClearing(c : Clearing) extends Message {
    def elem(g : Game) = " in " ~ c.elem(g)
}

case class OfFaction(f : Faction) extends Message {
    def elem(g : Game) = " of " ~ f.elem
}

case class OnTurn(n : Int) extends Message {
    def elem(g : Game) = " on turn" ~ SpaceSpan ~ ("#" + n).hl
}

case object OnStart extends Message {
    def elem(g : Game) = Text(" on setup")
}

case class NotInLog(m : Message) extends Message {
    def elem(g : Game) = m.elem(g)
}

case class AltInLog(m : Message, alt : Message) extends Message {
    def elem(g : Game) = m.elem(g)
}

case class ConcatMessage(a : Message, b : Message) extends Message {
    def elem(g : Game) = a.elem(g) ~ b.elem(g)
}

object D4 extends Die[Int]($(0, 1, 2, 3))



trait ViewCard extends ViewObject[Card] { self : UserAction => 
    def d : Card
    def obj = d
}

trait ViewQuest extends ViewObject[Quest] { self : UserAction => 
    def q : Quest
    def obj = q
}

trait ViewLeader extends ViewObject[Leader] { self : UserAction => 
    def l : Leader
    def obj = l
}

trait ViewCharacter extends ViewObject[Character] { self : UserAction => 
    def c : Character
    def obj = c
}



case object NoHand extends HiddenInfo
case object NoPlots extends HiddenInfo
case object NoSupporters extends HiddenInfo
case object HiSupporters extends HiddenInfo

case object HiddenCancel extends Hidden with Cancel

trait FactionAction {
    def self : Faction
}

case class StartAction(version : String) extends StartGameAction
case object ProcessOptionsAction extends ForcedAction
case object DraftFactionsAction extends ForcedAction
case class AllRandomMappingAction(shuffled : List[Clearing]) extends ShuffledAction[Clearing]
case class NoClustersMappingAction(shuffled : List[Clearing]) extends ShuffledAction[Clearing]
case class SuitPairsMappingAction(shuffled : List[Clearing]) extends ShuffledAction[Clearing]
case class ConnectedMappingAction(shuffled : List[Clearing]) extends ShuffledAction[Clearing]
case class ThreeFourFiveMappingAction(shuffled : List[Clearing]) extends ShuffledAction[Clearing]
case class ThreeFourFiveSuitMappingAction(shuffled : List[BaseSuit], clearings : List[Clearing]) extends ShuffledAction[BaseSuit]

case object MappingFinishedAction extends ForcedAction
case object DraftCardsAction extends ForcedAction

case class DraftLimitAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class DraftLimitDiscardAction(f : Faction, l : List[DeckCard], then : ForcedAction) extends ForcedAction
case class ShuffleDeckAction(shuffled : List[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]



case object SetupNextAction extends ForcedAction
case class SetupFactionIndexAction(f : Faction, n : Int) extends ForcedAction
case class SetupRandomFactionIndexAction(random : Faction, n : Int) extends RandomAction[Faction]
case class SetupFactionRandomIndexAction(f : Faction, random : Int) extends RandomAction[Int]
case object DummyAction extends ForcedAction

case object Next extends ForcedAction
case object Repeat extends ForcedAction 

trait BirdsongQuestion extends FactionAction {
    override def self : Faction
    def question(g : Game) = self.elem ~ " (" ~ Birdsong.elem ~ ")"
}

case class ShufflePileAction(shuffled : List[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]

case class DrawCardsAction(f : Faction, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class DrawCardsFromDeckAction(f : Faction, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class AddCardsAction(f : Faction, then : ForcedAction) extends ForcedAction

case class FindAmbushAction(self : Faction, m : Message, then : ForcedAction) extends BaseAction(Evening)("Take", "Ambush".hl, "with", Informants) with Soft
case class TakeAmbushAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(Informants)(d.img) with ViewCard

case class HandLimitAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class HandLimitDiscardAction(f : Faction, l : List[DeckCard], then : ForcedAction) extends ForcedAction

case class MoveInitAction(f : Faction, tt : List[List[Transport]], m : Message, l : List[Region], all : List[Region], extra : List[UserAction], then : ForcedAction, self : Option[Faction] = None) extends ForcedAction with Soft

case class MoveFromAction(self : Faction, f : Faction, tt : List[List[Transport]], m : Message, from : Region, to : List[Region], then : ForcedAction) extends BaseAction(g => "Move " ~ m.elem(g) ~ " from" ~ (m == NoMessage).??(self.aviary./(f => " [" ~ g.ed(f).desc(Decree.Move) ~ "]").|(Empty)))(from) with Soft
case class MoveToAction(self : Faction, f : Faction, tt : List[List[Transport]], m : Message, from : Region, to : Region, then : ForcedAction) extends BaseAction(g => (m == SlipTo).?(m.elem(g)).|("Move " ~ m.elem(g) ~ " from " ~ from.elem(g) ~ " to"))(tt./(_./(_.img).merge), to, Image("move-deg-" + Region.dir(from, to), styles.token)) with Soft
case class MoveListAction(self : Faction, t : List[Transport], m : Message, from : Region, to : Region, l : List[Piece], then : ForcedAction) extends BaseAction(g => (m == SlipTo).?(m.elem(g)).|("Move " ~ m.elem(g) ~ " from " ~ from.elem(g) ~ " to") ~ " " ~ to.elem(g))(t./(_.img).merge, l./(_.img(self)).merge)
case class MoveListAlliedAction(self : Faction, t : List[Transport], m : Message, from : Region, to : Region, l : List[Piece], a : Faction, al : List[Piece], then : ForcedAction) extends BaseAction(g => "Move " ~ m.elem(g) ~ " from", from.clearing.|("forest"), "to", to)(t./(_.img).merge, l./(_.img(self)).merge, al./(p => p.img(a)).merge)
case class MoveWarriorsCompleteAction(f : Faction, from : Region, to : Region, then : ForcedAction) extends ForcedAction
case class MoveFinishedAction(f : Faction, from : Region, to : Region, then : ForcedAction) extends ForcedAction
case class FerryAction(f : Faction, from : Clearing, to : Clearing, then : ForcedAction) extends ForcedAction

case class MarchAction(self : Faction, n : Int, total : Int, then : ForcedAction) extends ForcedAction with Soft

case class OpportunityDiscardCardsAction(f : Faction, m : Message, n : Int, l : List[DeckCard], t : DiscardCost, then : ForcedAction, alt : ForcedAction) extends ForcedAction
case class OpportunityDiscardSelectCardAction(self : Faction, m : Message, t : DiscardCost, d : DeckCard, available : Boolean, then : ForcedAction) extends BaseAction(self, "can discard", (t != AnySuit).??("" ~ t.elem ~ " card"), m)(d.img) with ViewCard with ThenAction

case class OptionalDiscardCardAction(f : Faction, m : Message, t : DiscardCost, then : ForcedAction) extends ForcedAction with Soft
case class OpportunityDiscardCardAction(f : Faction, m : Message, t : DiscardCost, then : ForcedAction, alt : ForcedAction) extends ForcedAction
case class DiscardCardAction(self : Faction, m : Message, t : DiscardCost, d : DeckCard, available : Boolean, then : ForcedAction) extends BaseAction(self, "discards", m, (t != AnySuit).??("(" ~ t.elem ~ " card)"))(d.img) with ViewCard with ThenAction

case class PrepareDiscardCardsAction(f : Faction, l : List[DeckCard], then : ForcedAction) extends ForcedAction
case class DiscardRandomCardAction(f : Faction, then : ForcedAction) extends ForcedAction
case class PerformDiscardRandomCardAction(f : Faction, random : DeckCard, then : ForcedAction) extends RandomAction[DeckCard]

case class StealCardAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction
case class TakeCardAction(self : Faction, e : Faction, random : DeckCard, then : ForcedAction) extends RandomAction[DeckCard]
case class MassStealCardAction(self : Faction, l : List[Faction], then : ForcedAction) extends ForcedAction

case class BattleInitAction(f : Faction, m : Message, l : List[Clearing], extra : List[UserAction], then : ForcedAction) extends ForcedAction with Soft
case class BattleFromAction(self : Faction, m : Message, c : Clearing, l : List[Faction], then : ForcedAction, alt : UserAction with Cancel) extends BaseAction(self, "battles", m, "in", g => (m == NoMessage).??(self.aviary./(f => " [" ~ g.ed(f).desc(Decree.Battle) ~ "]").|(Empty)))(c) with Soft
case class BattleAllyAction(self : Faction, a : Faction, m : Message, c : Clearing, l : List[Faction], then : ForcedAction, alt : UserAction with Cancel) extends BaseAction(self, "battles", m, "in", c, "with ally")((self == a).?("Alone").|(a)) with Soft
case class BattleStartAction(self : Faction, a : Faction, m : Message, c : Clearing, o : Faction, i : Option[Hero], then : ForcedAction) extends BaseAction(self, (a != self).?("and"), (a != self).?(a), "battle" + (a == self).??("s"), m, "in", c)(o)
case class BattleStartedAction(b : Battle) extends ForcedAction

case class Battle(clearing : Clearing, attacker : Faction, ally : Option[Faction], defender : Faction, instigator : Option[Hero], ambush : Boolean, arbiter : Option[Hero], codef : Option[WarriorFaction], balance : Int, then : ForcedAction) extends Record {
    def parties = $(attacker, defender) ++ ally ++ instigator ++ codef
}
     
case class BattleAskAmbushAction(b : Battle) extends ForcedAction

case class BattleAmbushAction(self : Faction, b : Battle, d : DeckCard, have : Boolean, available : Boolean) extends BaseAction(self, have.?("can").|("can't"), "ambush", g => "(" ~ g.mapping(b.clearing).elem ~ ")")(d.img ~ available.?(d.elem).|(have.?(" ".pre).|(Empty))) with ViewCard
case class BattleAttackerCounterAmbushAction(b : Battle) extends ForcedAction
case class BattleCounterAmbushAction(self : Faction, b : Battle, d : DeckCard, have : Boolean, available : Boolean) extends BaseAction(self, have.?("can").|("can't"), "counter-ambush", g => "(" ~ g.mapping(b.clearing).elem ~ ")")(d.img ~ available.?(d.elem).|(have.?(" ".pre).|(Empty))) with ViewCard
case class BattleAmbushHitsAction(b : Battle, n : Int) extends ForcedAction

case class BattleDefenderPreRollAction(b : Battle) extends ForcedAction
case class BattleEnlistArbiterAction(self : Faction, b : Battle, arbiter : Hero) extends BaseAction(self, "can enlist an", "Arbiter".hl)(arbiter)
case class BattleEnlistDefenderAction(self : Faction, b : Battle, defender : WarriorFaction) extends BaseAction(self, "can enlist a", "Defender".hl)(defender)

case class BattleAttackerPreRollAction(b : Battle) extends ForcedAction


case class BattleRollAction(b : Battle) extends ForcedAction
case class BattleRolledAction(b : Battle, fr : Int, or : Int) extends RolledAction[Int] {
    def rolled = $(fr, or)
}
case class BattleAdjustRollAction(b : Battle, f : Faction, o : Faction, fs : Int, os : Int, fr : Int, or : Int) extends ForcedAction
case class BattleFinishRollAction(b : Battle, f : Faction, o : Faction, fs : Int, os : Int, fr : Int, or : Int, fh : Int, oh : Int) extends ForcedAction
case class BattleBonusAction(b : Battle, f : Faction, o : Faction, fs : Int, os : Int, fr : Int, or : Int, fh : Int, oh : Int, fe : Int, oe : Int) extends ForcedAction
case class BattlePostRollAction(b : Battle, fh : Int, oh : Int, fe : Int, oe : Int, ask : List[Faction]) extends ForcedAction
case class SappersAction(self : Faction, then : ForcedAction) extends BaseAction(self, "in battle")(Sappers)
case class PartisansAction(self : Faction, s : BaseSuit, l : List[DeckCard], then : ForcedAction) extends BaseAction(self, "in battle")(Partisans(s), dt.Remove, l)
case class SwiftStrikeAction(self : Hero, then : ForcedAction) extends BaseAction(self, "in battle")(SwiftStrike.of(self), Sword.img)
case class ArmorersAction(self : Faction, then : ForcedAction) extends BaseAction(self, "in battle")(Armorers)
case class BrutalTacticsAction(self : Faction, o : Faction, then : ForcedAction) extends BaseAction(self, "in battle")(BrutalTactics)
case class BattleResolveHitsAction(b : Battle, fh : Int, oh : Int) extends ForcedAction
case class BattleAssignHitsAction(f : Faction, b : Battle, n : Int, then : ForcedAction) extends ForcedAction
case class BattleDealHitsAction(self : Faction, b : Battle, l : List[Figure], then : ForcedAction) extends ForcedAction
case class BattleDealHitAction(self : Faction, b : Battle, p : Figure, n : Int, then : ForcedAction) extends ForcedAction
case class BattlePostHitInAction(b : Battle, e : Faction, f : Faction, p : Piece, then : ForcedAction) extends ForcedAction
case class BattlePostHitOutAction(b : Battle, e : Faction, f : Faction, p : Piece, then : ForcedAction) extends ForcedAction
case class BattleCleanupAction(b : Battle) extends ForcedAction
case class BattleCleanupAllyAction(b : Battle, f : Faction) extends ForcedAction
case class BattleCleanupDefenderAction(b : Battle, f : Faction) extends ForcedAction
case class BattleCleanupAttackerAction(b : Battle, f : Faction) extends ForcedAction
case class BattleForcedRemoveAction(b : Battle) extends ForcedAction
case class BattleResolvedAction(b : Battle) extends ForcedAction
case class BattleFinishedAction(b : Battle) extends ForcedAction

// who where whose what, and what next
case class ForcedRemoveAction(e : Faction, c : Clearing, f : Faction, p : Piece, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class ForcedRemoveEffectAction(e : Faction, c : Clearing, f : Faction, p : Piece, then : ForcedAction) extends ForcedAction with SideEffectOnly
case class ForcedRemoveScoreAction(e : Faction, c : Clearing, f : Faction, p : Piece, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class ForcedRemoveCleanupAction(e : Faction, then : ForcedAction) extends ForcedAction

trait Nuke extends Record
case object TotalAnnihilation extends Nuke
case object ClearSector extends Nuke
case object AntiMaterial extends Nuke

case class NukeAction(f : Faction, spare : List[Faction], l : List[Clearing], nuke : Nuke, then : ForcedAction) extends ForcedAction


case class WageWarAction(self : Faction, n : Int, total : Int, then : ForcedAction) extends ForcedAction with Soft


case class XCraftMainAction(f : Faction) extends ForcedAction with Soft
case class NiceCraftMainAction(self : Faction, l : List[CraftAsset], u : List[CraftAsset]) extends BaseAction()("Craft".styled(self), l.diff(u)./(dt.CraftSuit).merge ~ u.diff(l).any.??(" " ~ "-".hl ~ " " ~ u.diff(l)./(dt.CraftSuit).merge)) with Soft
case class CraftMenuAction(f : Faction) extends ForcedAction with Soft
case class CraftAction(f : Faction, d : DeckCard, m : Message, then : ForcedAction) extends ForcedAction with Soft
case class CraftAssignAction(self : Faction, d : DeckCard, all : List[CraftAsset], used : List[CraftAsset], m : Message, then : ForcedAction) extends BaseAction("Craft", d, all./(dt.CraftSuit).merge)(used./(dt.CraftSuit).merge)
case class CraftPerformAction(f : Faction, d : DeckCard, m : Message) extends ForcedAction
case class CraftScoreAction(f : Faction, d : CraftItemCard, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class CraftEffectAction(f : Faction, d : CraftEffectCard, m : Message, then : ForcedAction) extends ForcedAction
case class CraftFavorAction(f : Faction, d : Favor, m : Message, then : ForcedAction) extends ForcedAction
case class MurineBrokerAction(f : Faction, then : ForcedAction) extends ForcedAction

case class AcquireItemsAction(self : Faction, then : ForcedAction) extends ForcedAction

case class CreatePlayerAction(f : Faction) extends ForcedAction
case class FactionInitAction(f : Faction) extends ForcedAction
case class FactionSetupAction(f : Faction) extends ForcedAction
case class StartingCornerAction(f : Faction) extends ForcedAction
case class ChooseCornerAction(self : Faction, r : Clearing) extends BaseAction(self, "starts in")(r)
case class StartingRegionAction(f : Faction) extends ForcedAction
case class StartingClearingAction(f : Faction, r : Clearing) extends ForcedAction
case class StartingForestAction(self : Faction, r : Forest) extends BaseAction(self, "starts in")(g => g.board.forestName(r, g))
case class StartingBuildingsNextAction(self : Faction, r : Clearing, p : Building) extends ForcedAction
case class PlaceStartingBuildingAction(self : Faction, k : Clearing, r : Clearing, p : Building) extends BaseAction(self, "places", p.of(self), "in")(r)
case class PlaceStartingTokenAction(self : Faction, k : Clearing, r : Clearing, p : Token) extends BaseAction(self, "places", p.of(self), "in")(r)
case class PlaceStartingWarriorAction(self : Faction, r : Clearing, p : Warrior) extends BaseAction(self, "places", p.of(self), "in")(r)
case class PlaceStartingWarriorsAction(self : Faction, l : List[Clearing], p : Warrior, then : ForcedAction) extends BaseAction(self, "places", p.img(self).repeat(l.num), "in")(l)
case class PlaceStartingPiecesAction(self : Faction, k : Clearing, r : Clearing, l : List[Piece]) extends BaseAction(self, "places", l./(_.of(self)).comma, "in")(r)

case class AddRuinItemsAction(then : ForcedAction) extends ForcedAction
case class ShuffleRuinItemsAction(shuffled : List[Item], then : ForcedAction) extends ShuffledAction[Item]


case class AfterSetupAction(f : Faction, then : ForcedAction) extends ForcedAction
case object SetupDoneAction extends ForcedAction
case class DiscardDraftedAction(f : Faction, then : ForcedAction) extends ForcedAction
case object InitDoneAction extends ForcedAction
case class StartPlayerTurnAction(f : Faction) extends ForcedAction
case class EndPlayerTurnAction(f : Faction) extends ForcedAction

case class BetterBurrowBankAction(self : Faction, o : Faction) extends BaseAction(self, "shares", BetterBurrowBank, "with")(o)
case class SaboteurMainAction(self : Faction) extends BaseAction(Birdsong, "start")(Saboteurs) with Soft
case class SaboteurAction(self : Faction, e : Faction, d : DeckCard) extends BaseAction(Saboteurs, "discard")(d, "of", e)

case class RoyalClaimAction(self : Faction, n : Int) extends OptionAction(RoyalClaim, "(" ~ n.vp ~ ")") with BirdsongQuestion

case class StandAndDeliverAction(self : Faction, l : List[Faction], then : ForcedAction) extends OptionAction(StandAndDeliver) with BirdsongQuestion with Soft
case class StandAndDeliverFactionAction(self : Faction, t : Faction, then : ForcedAction) extends BaseAction(StandAndDeliver)(t)
case class StandAndDeliverCardAction(self : Faction, t : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction

case class SwapMeetAction(self : Faction, l : List[Faction], then : ForcedAction) extends OptionAction(SwapMeet) with BirdsongQuestion with Soft
case class SwapMeetFactionAction(self : Faction, t : Faction, then : ForcedAction) extends BaseAction(SwapMeet)(t)
case class SwapMeetReturnAction(self : Faction, t : Faction, then : ForcedAction) extends ForcedAction
case class SwapMeetReturnCardAction(self : Faction, t : Faction, d : DeckCard, then : ForcedAction) extends BaseAction("Give a card back to", t, "after", SwapMeet)(d.img) with ViewCard

case class FalseOrdersAction(self : Faction, l : List[Faction], then : ForcedAction) extends OptionAction(FalseOrders) with BirdsongQuestion with Soft
case class FalseOrdersFactionAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction(FalseOrders)(e) with Soft
case class FalseOrdersCompleteAction(self : Faction, then : ForcedAction) extends ForcedAction


case class EyrieEmigreDiscardAction(self : Faction) extends BaseAction()("Discard", EyrieEmigre)
case class EyrieEmigreAttackAction(self : Faction, l : List[Clearing]) extends ForcedAction

case class UsedEffectAction(f : Faction, e : Effect, then : ForcedAction) extends ForcedAction


case class PeekCardsMainAction(self : Faction, s : Elem, m : Message, l : List[DeckCard], then : ForcedAction) extends BaseAction(None)(s)

case class ViewCardInfoAction(self : Player, s : Elem, d : Card) extends BaseInfo(s)(d.img) with ViewCard with OnClickInfo { def param = d }
case class ActivateDominanceMainAction(self : Faction, l : List[Dominance], then : ForcedAction) extends BaseAction("\nDominance")("Activate Dominance") with Soft
case class ActivateDominanceAction(self : Faction, d : Dominance, then : ForcedAction) extends BaseAction("Activate Dominance")(d.img) with ViewCard

case class FormCoalitionMainAction(self : Hero, cc: List[Faction], l : List[Dominance], then : ForcedAction) extends BaseAction("\nDominance")("Form Coalition") with Soft
case class FormCoalitionFactionAction(self : Hero, x : Faction, l : List[Dominance], then : ForcedAction) extends BaseAction("Form Coalition with")(x) with Soft
case class FormCoalitionAction(self : Hero, x : Faction, d : Dominance, then : ForcedAction) extends BaseAction("Form Coalition with", x, " using")(d.img) with ViewCard

case class TakeDominanceMainAction(self : Faction, l : List[Dominance], then : ForcedAction) extends BaseAction("\nDominance")("Take a Dominance card from discard") with Soft
case class TakeDominanceAction(self : Faction, d : Dominance, then : ForcedAction) extends BaseAction("Take a Dominance card from discard")(d.img) with ViewCard with Soft
case class TakeDominanceTakeAction(self : Faction, d : Dominance, then : ForcedAction) extends ForcedAction

case class TaxCollectorMainAction(self : WarriorFaction, l : List[Clearing], then : ForcedAction) extends BaseAction(None)(TaxCollector) with Soft
case class TaxCollectorAction(self : WarriorFaction, c : Clearing, p : Piece, then : ForcedAction) extends BaseAction(TaxCollector)(p.of(self), "in", c)

case class PropagandaBureauMainAction(self : WarriorFaction, l : List[Clearing], then : ForcedAction) extends BaseAction(None)(PropagandaBureau) with Soft
case class PropagandaBureauSelectAction(self : WarriorFaction, c : Clearing, e : WarriorFaction, p : Piece, then : ForcedAction) extends BaseAction(PropagandaBureau)(p.of(e), p.img(e), "in", c) with Soft
case class PropagandaBureauAction(self : WarriorFaction, c : Clearing, e : WarriorFaction, p : Piece, then : ForcedAction) extends ForcedAction

case class LeagueOfAdventurousMiceMainAction(self : Faction, lm : List[Region], lw : List[Clearing], then : ForcedAction) extends BaseAction(None)(LeagueOfAdventurousMice) with Soft
case class LeagueOfAdventurousMiceAction(self : Faction, i : ItemRef, then : ForcedAction) extends ForcedAction
case class ExhaustForTradeItemAction(f : Faction, i : ItemRef, then : ForcedAction) extends ForcedAction

case class CharmOffensiveMainAction(f : Faction, l : List[Faction], then : ForcedAction) extends ForcedAction
case class CharmOffensiveAction(f : Faction, e : Faction, then : ForcedAction) extends ForcedAction



case class TalentScoutMainAction(self : WarriorFaction, s : Trader, l : List[Clearing], then : ForcedAction) extends BaseAction(None)(TalentScout(s)) with Soft
case class TalentScoutAction(self : WarriorFaction, s : Trader, c : Clearing, p : Piece, then : ForcedAction) extends BaseAction("Recruit", p.of(self), "with", TalentScout(s), "in")(c)

case class CodebreakersMainAction(self : Faction, l : List[Faction], then : ForcedAction) extends BaseAction(None)(Codebreakers) with Soft
case class CodebreakersAction(self : Faction, f : Faction, then : ForcedAction) extends BaseAction(Codebreakers)(f)
case class CodebreakersNotifyAction(self : Faction, f : Faction, d : DeckCard) extends BaseInfo(self, "views", f, "hand")(d.img) with ViewCard with OnClickInfo { def param = d }

case class ClearPathMainAction(self : Faction, then : ForcedAction) extends BaseAction(None)(ClearPath) with Soft
case class ClearPathBetweenAction(self : Faction, a : Clearing, b : Clearing, then : ForcedAction) extends BaseAction(ClearPath)("Clear path between", a, "and", b) with Soft
case class ClearPathAction(self : Faction, a : Clearing, b : Clearing, then : ForcedAction) extends ForcedAction


case object ClearPath extends Effect {
    override def name = "Clear Path"
}

trait TopLevelAction extends ForcedAction {
    def next : ForcedAction
}

case class BirdsongStartAction(f : Faction) extends ForcedAction

case class BirdsongNAction(n : Int, f : Faction) extends TopLevelAction {
    def next = (n >= 99).?(DaylightStartAction(f)).|(BirdsongNAction(n + 1, f))
}

case class DaylightStartAction(f : Faction) extends ForcedAction

case class DaylightNAction(n : Int, f : Faction) extends TopLevelAction {
    def next = (n >= 99).?(EveningStartAction(f)).|(DaylightNAction(n + 1, f))
}

case class EveningStartAction(f : Faction) extends ForcedAction

case class EveningNAction(n : Int, f : Faction) extends TopLevelAction {
    def next = (n >= 99).?(NightStartAction(f)).|(EveningNAction(n + 1, f))
}

case class NightStartAction(f : Faction) extends ForcedAction

case class CleanUpAction(f : Faction) extends ForcedAction
case class FactionCleanUpAction(f : Faction) extends ForcedAction

case class EveningDrawAction(f : Faction, n : Int) extends ForcedAction 
case class EveningHandLimitAction(f : Faction) extends ForcedAction 
case class EveningEndAction(f : Faction) extends ForcedAction 



case class EndTurnAction(self : Faction) extends BaseAction(None)("End Turn".hl)
case class EndTurnSoftAction(self : Faction, m : Message) extends BaseAction(None)("End Turn") with Soft
case class EndTurnCancelAction(self : Faction, m : Message) extends BaseUserAction(m)("Continue Turn".hl) with Cancel
case class EndTurnWarnAction(self : Faction, m : Message) extends BaseAction(m)("End Turn Anyway".styled(styles.hit))

case class WarnAction(self : Faction, then : ForcedAction, q : Elem, proceed : Elem, cancel : Elem) extends ForcedAction with Soft



case class TotalWarAction(f : WarriorFaction, then : TopLevelAction) extends ForcedAction
case class TotalWarRecruitAction(self : WarriorFaction, c : Clearing, n : Int, then : TopLevelAction) extends BaseAction("Total War".hl, MDash, n.vp, MDash, "Recruit in")(c)
case class TotalWarDoneAction(f : WarriorFaction, then : TopLevelAction) extends ForcedAction 


trait NoGameOverTrigger { self : Action => }

case object GameOverTopAction extends ForcedAction

trait GameOverBaseAction extends OutOfTurn with NoGameOverTrigger with Choice {
    def question(g : Game) = "Game Over"
}

trait GameOverBaseInfo extends Info { self : UserAction =>
    def question(g : Game) = "Game Over"
}

case class GameOverWonAction(self : Faction, f : Faction) extends OptionInfo(f, "won", "(" ~ NameReference(f.name, f).hl ~ ")") with GameOverBaseInfo
case class GameOverTextAction(self : Faction, t : Elem) extends OptionInfo(t) with GameOverBaseInfo
case class GameOverMapsAction(self : Faction) extends OptionAction("Maps") with GameOverBaseAction with Soft
case object GameOverSaveReplayAction extends OptionAction("Save Replay") with GameOverBaseAction with Soft with SpecialAction with NoClear with OnClickInfo { def param = this }

case class GameOverDoneAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Done") with Soft with OutOfTurn

case class GameOverMoveMapAction(self : Faction, v : Boolean) extends BaseAction("Maps")("Movements".hlIf(v)) with NoGameOverTrigger with Soft with OutOfTurn with NoClear
case class GameOverBattleMapAction(self : Faction, v : Boolean) extends BaseAction("Maps")("Battles".hlIf(v)) with NoGameOverTrigger with Soft with OutOfTurn with NoClear
case class GameOverNukeMapAction(self : Faction, v : Boolean) extends BaseAction("Maps")("Nukes".hlIf(v)) with NoGameOverTrigger with Soft with OutOfTurn with NoClear
case class GameOverGraveyardAction(self : Faction, v : Boolean) extends BaseAction("Maps")("Graveyard".hlIf(v)) with NoGameOverTrigger with Soft with OutOfTurn with NoClear

case class SelectFiguresAction(f : Faction, m : Elem, l : List[Figure], extra : List[UserAction])(r : ObjectSetRule[Figure] => ObjectSetRule[Figure])(then : List[Figure] => ForcedAction) extends ForcedAction with Soft with SelfPerform {
    def perform(g : Game) = {
        import g._
        
        implicit val convF = (x : Figure, s : Boolean) => FigureRemove(x).elem(s)(styles.iii)
        
        val figures = l
 
        XXSelectObjectsAction(f, figures, true)
            .withGroup(m)
            .withSplit($(figures.num))
            .withBreak({
                case 0 => Gap ~ Gap
                case _ => HorizontalBreak
            })
            .withRule(rule => r(rule.each(_ => true)))
            .withThen(l => then(l))(l => "Remove " ~ l./(u => u.piece.of(u.faction)).comma)
            .withExtra(extra)
    }
}

case class SelectFactionAction(self : Player, n : Int, f : PlayChoice) extends ForcedAction 

trait PlayChoice extends Record with Elementary {
    def faction : Faction
}

case class FactionChoice(faction : Faction) extends PlayChoice {
    def elem = faction.elem
}

case class FactionCharacterChoice(faction : Hero, c : Character) extends PlayChoice {
    def elem = faction.elem ~ " " ~ c.name
}



trait Highlight
case class PlaceHighlight(l : List[Clearing]) extends Highlight
case class BattleHighlight(c : Clearing) extends Highlight
case class NukeHighlight(l : List[Clearing]) extends Highlight
case class MoveHighlight(from : Region, to : Region) extends Highlight
case object NothingHighlight extends Highlight


case class TagFactionIndex(n : Int)
case class TagCharacter(h : Hero)

trait Reaction extends Record {
    val name : String
}

case object GoodGame extends Reaction {
    val name = "Good Game"
}

case object IHateYouAll extends Reaction {
    val name = "I Hate You All"
}

case object Crivens extends Reaction {
    val name = "Crivens!"
}

case object WellBeBack extends Reaction {
    val name = "We'll Be Back"
}

case object NoDiscount extends Reaction {
    val name = "No Quarter, No Discount"
}

case object DialingCthulhu extends Reaction {
    val name = "Dialing Cthulhu Octopuss"
}

case object Hallelujah extends Reaction {
    val name = "Hallelujah!"
}

case object FlipBombs extends Reaction {
    val name = "Flip Bombs, Not Tables"
}

case object AbandonShip extends Reaction {
    val name = "Abandon Ship!"
}

case object FriendshipIsMagic extends Reaction {
    val name = "Friendship Is Magic"
}

case object OneMoreTurn extends Reaction {
    val name = "One More Turn!!!"
}

case object IDontEvenWannaKnow extends Reaction {
    val name = "I Don't Even Wanna Know"
}


trait Expansion {
    def perform(game : Game, action : Action) : Continue
    def afterMoveTo(game : Game, f : Faction, to : Region, then : ForcedAction) : ForcedAction = then
    def extraMoveFrom(game : Game, f : Faction) : List[Region] = Nil
    def extraConnected(game : Game, f : Faction, from : Region) : List[Region] = Nil
}

case class SeatingOrderAction(shuffled : List[Player]) extends ShuffledAction[Player]

case class FactionsCombinationAction(random : List[Faction]) extends RandomAction[List[Faction]]

case class CharactersCombinationAction(shuffled : List[Character]) extends ShuffledAction[Character]

class Game(val players : List[Player], val setup : List[Faction], val logging : Boolean, val options : List[GameOption]) extends BaseGame with ContinueGame with LoggedGame { game =>
    if (options.has(FactionSeatingGiven) || options.has(SetupOrderPriority))
        require(setup.num == players.num)

    def arity = players.num

    var ftp = Map[Faction, Player]()
    var ptf = Map[Player, Faction]()

    players.of[Faction].foreach { f =>
        ftp += f -> f
        ptf += f -> f
    }

    var version : String = null

    var seating = $[Player]()

    var drawn = $[Faction]()
    
    var chars = Map[Hero, Character]()
    
    var fseating = Map[Int, Faction]()

    val expansions = setup./(_.expansion).distinct

    def onMoveTo(f : Faction, c : Region, action : ForcedAction) = {
        var q = action
        expansions.foreach { e =>
            q = e.afterMoveTo(this, f, c, q)
        }
        q
    }

    def extraMoveFrom(f : Faction) = {
        expansions./~(_.extraMoveFrom(this, f))
    }
    
    def extraConnected(f : Faction, from : Region) = {
        expansions./~(_.extraConnected(this, f, from))
    }
    
    def validDest(f : Faction) : List[Region] = (f @@ {
        case f : Hero => board.forests
        case f : Underground => $(f.burrow)
        case _ => Nil
    } ++ clearings).diff(scorched)

    
    object ui {
        var funNames : Boolean = true
        var rules : Boolean = true
        var movements : Boolean = false
        var battles : Boolean = false
        var nukes : Boolean = false
        var graveyard : Boolean = false
    }

    var gameover : GameOver = null

    var highlights : List[Highlight] = Nil

    var highlightFaction : List[Faction] = Nil
    
    val board : Board = options./~{
        case AutumnMap => Some(AutumnBoard)
        case WinterMap => Some(WinterBoard)
        case LakeMap => Some(LakeBoard)
        case MountainMap => Some(MountainBoard)
        case _ => None
    }.single.get

    var mapping : Map[Clearing, BaseSuit] = null

    implicit class ClearingSuit(c : Clearing) {
        def suit = mapping(c)
    }

    var graveyard : Map[Region, List[Figure]] = board.clearings./(_ -> Nil).toMap
    
    var lastlog : List[Any] = Nil

    def convertForLog(s : List[Any]) = s./~{
        case Empty => None
        case NotInLog(_) => None
        case AltInLog(_, m) => Some(m)
        case f : Faction => Some(f.elem.styled(styles.condensed))
        case e : GameModeElementary => Some(e.elem(this, ModeLog))
        case x => Some(x)
    }

    override def log(s : Any*) {
        if (s.contains(SuppressLog)) 
            return
            
        if (s.toList == $(SingleLine) && lastlog == $(SingleLine))
            return

        lastlog = s.toList

        super.log(convertForLog(s.toList) : _*)
    }

    override def logtemp(s : Any*) {
        if (s.contains(SuppressLog)) 
            return

        super.logtemp(convertForLog(s.toList) : _*)
    }

    def xelemRegeion(r : Region) = {
        r match {
            case r : Clearing => r.elem(this)
            case r : Forest => board.fromForest(r).some./(l => "forest between " ~ l(2).elem(this) ~ " and " ~ l(0).elem(this)).get
        }
    }


    val FoxRabbitMouse : List[BaseSuit] = $(Fox, Rabbit, Mouse)

    var pstates = Map[Faction, PlayerState]()
    var factions : List[Faction] = Nil
    
    var scorched : List[Clearing] = Nil
    var tower = board.tower
    var rubble : List[(Clearing, Clearing)] = board.rubble
    var ferry = board.coastal.take(1)

    var corners : List[Clearing] = Nil
    var homelands : List[Clearing] = Nil

    def clearings = board.clearings.diff(scorched)
    def riverside = board.riverside.diff(scorched)
    
    def traders = factions./~(_.trader)
    
    def connected(f : Faction)(o : Region) = {
        val tt = transports.but(Ferry).%(_.allows(game, f, o))
        validDest(f).but(o).%(d => tt.exists(_.allows(game, f, o, d)))
    }

    def fromForest(f : Forest) = board.fromForest(f).diff(scorched)
    
    val transports : List[Transport] = $(Roads, Riverboat, Ferry, Swimmers, BurrowRoads, TunnelRoads)

    def snaredFor(f : Faction)(o : Region) = o @@ {
        case o : Clearing => factions.but(f)./~(_.mischief).%(e => e.at(o).got(Snare) && e.hidden.has(o).not).any
        case _ => false
    }

    def movePlans(f : Faction, from : List[Region], transports : List[List[Transport]]) = {
        val ttt = transports.%(_.forall(_.allows(game, f)))
        val dest = validDest(f)
        
        from./~{ o =>
            val tt = ttt.%(_.forall(_.allows(game, f, o)))
            val l = dest.but(o).%(d => tt.exists(_.forall(_.allows(game, f, o, d))))
            val t = tt.%(t => l.exists(d => t.forall(_.allows(game, f, o, d))))
            (t.any && l.any).?(o -> (t, l))
        }.toMap
    }
    
    var ruins = board.ruins./(_ -> Ruins(Nil)).toMap

    var quests = List[Quest]()

    val cards = new ValueTracker[Card]
    var pile = cards.another[DeckCard]("pile", Deck.fromOptions(options, players.num, setup))
    var predrafts = players./(p => p -> cards.another[DeckCard]("draft-" + p)).toMap
    var deck = cards.another[DeckCard]("deck")
    var dominances = cards.another[Dominance]("dominances")

    implicit def faction2player(f : Faction) = of(f)
    implicit def feline2player(f : Feline) = mc(f)
    implicit def aviary2player(f : Aviary) = ed(f)
    implicit def insurgent2player(f : Insurgent) = wa(f)
    implicit def fanatic2player(f : Fanatic) = lc(f)
    implicit def trader2player(f : Trader) = rf(f)
    implicit def underground2player(f : Underground) = ud(f)
    implicit def mischief2player(f : Mischief) = cc(f)
    implicit def expedition2player(f : Expedition) = ki(f)
    implicit def oldExpedition2player(f : OldExpedition) = ok(f)
    implicit def horde2player(f : Horde) = lh(f)
    implicit def hero2player(f : Hero) = vb(f)
    
    class Discard(val faction : Option[Faction], val message : List[Any], val logged : Boolean = true) {
        def apply(f : Faction) = new Discard(Some(f), Nil)
        def apply(f : Faction, m : Any*) = new Discard(Some(f), m.toList)
        def quiet = new Discard(faction, message, false)
    }
    
    val discard = new Discard(None, Nil)
    
    def moveToPile(source : Location[Card], d : Card) {
        d match {
            case d : Dominance => source --> d --> dominances
            case d => source --> d --> pile
        }
    }

    implicit class SourceEx(source : Location[Card]) {
        def -->(l : List[Card]) = new SourceCardsEx(source, l)
        def -->(d : Card) = new SourceCardEx(source, d)
        def -->(dest : Location[Card]) = new SourceCardsEx(source, cards.get[Card](source)) --> dest
        def -->(dest : Discard) = new SourceCardsEx(source, cards.get[Card](source)) --> dest
    }
      
    class SourceCardsEx(source : Location[Card], l : List[Card]) {
        def -->(dest : Location[Card]) {
            l.foreach(new SourceCardEx(source, _) --> dest)
        }

        def -->(dest : Discard) {
            if (dest.logged)
                dest.faction.foreach(_.log("discarded", l, dest.message))
                
            l.foreach(new SourceCardEx(source, _) -#> dest)
        }
    }
      
    class SourceCardEx(source : Location[Card], d : Card) {
        def -->(dest : Location[Card]) {
            cards.move(source, d, dest)
        }

        def -->(dest : Discard) {
            if (dest.logged)
                dest.faction.foreach(_.log("discarded", d, dest.message))
            
            -#>(dest)
        }

        def -#>(dest : Discard) {
            val lsr = (factions.drop(1) ++ factions.take(1))./~(_.fanatic).%(_.has(LostSouls))

            if (lsr.any)
                source --> d --> lsr.first.lost
            else
                moveToPile(source, d)
        }
    }
      
    val pieces = new IdentityTracker[Figure]
    
    val xclearings = board.clearings./(c => c -> pieces.another[Figure](c)).toMap
    val xforests = board.forests./(c => c -> pieces.another[Figure](c)).toMap
    var xregions = List[ExtraRegion]()./(c => c -> pieces.another[Figure](c)).toMap
    var xcoffins = pieces.another[Figure](CoffinMakers)

    def coffins(l : List[Figure]) = l.foreach(coffin)
    
    def coffin(x : Figure) {
        if (x.piece.warrior.any && factions.%(_.has(CoffinMakers)).any)
            x --> xcoffins
        else
            x --> x.faction.pool
    }

    def buryAll() {
        xcoffins.foreach { x =>
            x --> x.faction.pool
        }
    }

    implicit def location2list(l : AnonymousLocation[Figure, Figure]) = l.get
    
    implicit class ForestEx(f : Forest) {
        def get = xforests(f).get
    }
    
    implicit class FigureEx[P <: Piece](u : Figure) {
        def -->(dest : Clearing) {
            pieces.move(u, xclearings(dest))
        }
        
        def -->(dest : Forest) {
            pieces.move(u, xforests(dest))
        }
        
        def -->(dest : ExtraRegion) {
            pieces.move(u, xregions(dest))
        }

        def -->(dest : Region) {
            dest match {
                case c : Clearing => u --> c
                case c : Forest => u --> c
                case c : ExtraRegion => u --> c
            }
        }
 
        def -->[S >: P <: Piece](dest : AnonymousLocation[Figure, Figure]) {
            pieces.move(u, dest)
        }
    }

    implicit class FigureListEx(l : List[Figure]) {
        def -->(dest : Clearing) {
            l.foreach(pieces.move(_, xclearings(dest)))
        }
        
        def -->(dest : Forest) {
            l.foreach(pieces.move(_, xforests(dest)))
        }

        def -->(dest : ExtraRegion) {
            l.foreach(pieces.move(_, xregions(dest)))
        }

        def -->(dest : AnonymousLocation[Figure, Figure]) {
            l.foreach(pieces.move(_, dest))
        }
    }

    val VPOn = (g : Game) => Text("undefined victory points on") : Elem
    val VPOff = (g : Game) => Text("undefined victory points off") : Elem

    val VP = (g : Game) => Text("undefined victory points") : Elem
    val ScoredVP = (g : Game) => Text("undefined victory points scored") : Elem
    val ForVP = (g : Game) => Text("undefined victory points for") : Elem
    
    implicit class FactionEx(f : Faction) {

        def log(s : Any*) { 
            if (logging) 
                game.log((f +: s.toList) : _*) 
        }

        def logtemp(s : Any*) { 
            if (logging) 
                game.logtemp((f +: s.toList) : _*) 
        }
        
        def oscore(vp : Int)(reason : (Game => Elem)*) {
            nscore(vp)(reason : _*)()
        }

        def nscore(vp : Int)(reason : (Game => Elem)*)(logged : Any*) {
            val vpcond = f.dominance.none && f.coalition.none

            val real = (vpcond || options.has(TotalWarDominance)) && vp != 0

            if (real)
                f.vp = max(0, f.vp + vp)
                
            val m = if (logged.any) {
                if (logged.contains(VP) || logged.contains(ScoredVP) || logged.contains(ForVP)) {
                    var on = false
                    logged.toList./{
                        case VPOn => on = true; Empty
                        case VPOff => on = false; Empty
                        case VP => (real.not && on).?(Empty).|(vp.abs.vp)
                        case ScoredVP => (real.not && on).?(Empty).|(("scored " ~ vp.abs.vp).styled(xlo.pre))
                        case ForVP => (real.not && on).?(Empty).|(("for " ~ vp.abs.vp).styled(xlo.pre))
                        case e => (real.not && on).?(Empty).|(e)
                    }.but(Empty)
                }
                else
                    throw new Error("no vp in logged")
            }
            else
                $(f, (vp > 0).?("scored").|("lost") ~ " ".pre ~ vp.abs.vp) ++ reason.toList./(_(game))
                
            if (logged.any || real)
                game.log(convertForLog(m) : _*)

            if (real) {
                val r = reason.toList./(_(game)).join(SpaceSpan)
                
                f.scorelog :+= vp.vp ~ SpaceSpan ~ r ~ SpaceSpan ~ "(turn" ~ SpaceSpan ~ ("#" + turn).hl ~ ")"
            }
        }
        
        def at(c : Clearing) = xclearings(c).%(_.faction == f)
        
        def at(l : AnonymousLocation[Figure, Figure]) = l.%(_.faction == f)

        def at(c : Forest) = xforests(c).%(_.faction == f)

        def at(c : ExtraRegion) = xregions(c).%(_.faction == f)
        
        def at(c : Region) : List[Figure] = c match {
            case c : Clearing => at(c)
            case c : Forest => at(c)
            case c : ExtraRegion => at(c)
        }

        def keep : Option[Clearing] = f match {
            case f : Feline => f.all(Keep).single
            case _ => None
        }

        def friends(e : Faction) = (f, e) match {
            case (a, b) if a == b => true
            case (a : Hero, b : Hero) => a.coalition != None && b.coalition != None && a.coalition == b.coalition
            case (a : Hero, o) => a.coalition == Some(o)
            case (o, b : Hero) => b.coalition == Some(o)
            case _ => false
        }
        
        def birdsongNewCard = f.can(StandAndDeliver) || f.can(SwapMeet)
        
        def birdsongOnly = f.can(StandAndDeliver) || f.can(SwapMeet) || f.can(FalseOrders)
        
        def birdsong : List[UserAction] = {
            var actions : List[UserAction] = Nil
            
            if (f.can(StandAndDeliver)) {
                val l = factions.but(f).%(_.hand.any)
                if (l.any)
                    actions :+= StandAndDeliverAction(f, l, Repeat)
            }
    
            if (f.can(SwapMeet)) {
                val l = factions.but(f).%(_.hand.any)
                if (l.any)
                    actions :+= SwapMeetAction(f, l, Repeat)
            }
    
            if (f.can(FalseOrders)) {
                val l = factions.but(f)./~(_.warf).%(e => clearings.%!(snaredFor(e)).%(c => e.at(c).warriorNotPawn.any).any)
                if (l.any)
                    actions :+= FalseOrdersAction(f, l, Repeat)
            }
    
            actions ++ expose
        }
        
        def daylightNewCard = f.can(TaxCollector)
        
        def daylightOnly = f.can(TaxCollector) || f.can(PropagandaBureau) || traders.exists(s => f.can(TalentScout(s)))
        
        def daylight : List[UserAction] = {
            var actions : List[UserAction] = Nil
            
            f.warf.foreach { f =>
                if (f.can(TaxCollector)) {
                    val cc = clearings.%(c => f.at(c).warrior.any)
                    actions :+= TaxCollectorMainAction(f, cc, Repeat).x(cc.none, "no warriors").x(deck.none && pile.none, "no cards")
                }

                if (f.can(PropagandaBureau)) {
                    val cc = clearings.%(canPlace(f)).%(c => factions.but(f).%(_.at(c).warrior.%(_.piece.pawn.none).any).any).%(c => f.hand.%(_.suit.m(c.suit)).any)
                    actions :+= PropagandaBureauMainAction(f, cc, Repeat).x(f.hand.none, "no cards").x(cc.none, "no targets").x(f.pooled(f.warrior) == 0, "no warriors")
                }

                traders.foreach { s =>
                    if (f.can(TalentScout(s))) {
                        val cc = s.tradeposts.%(canPlace(f))
                        actions :+= TalentScoutMainAction(f, s, cc, Repeat).x(cc.none, "no clearings").x(f.pooled(f.warrior) == 0, "no warriors")
                    }
                }
            }

            if (f.can(Codebreakers)) {
                val ff = factions.but(f).%(_.hand.any)
                if (ff.any)
                    actions :+= CodebreakersMainAction(f, ff, Repeat)
            }
    
            if (f.can(LeagueOfAdventurousMice) && f.forTrade.%!(_.exhausted).any) {
                val lm = moveFromR(f)
                val lb = clearings.%(c => attack(f)(c).any)
                
                actions :+= LeagueOfAdventurousMiceMainAction(f, lm, lb, Repeat).x(lm.none && lb.none)
            }

            actions ++= expose

            if (rubble.any && f.used.has(ClearPath).not)
                actions :+= ClearPathMainAction(f, Repeat).x(f.hand.none, "no cards").x(rubble.forall { case (a, b) => f.at(a).none && f.at(b).none })
    
            if (f.dominance.none) {
                f match { 
                    case f : Hero =>
                        if (factions.num >= 4) {
                            val dd = f.hand./~(_.dominance)
                            if (dd.any) {
                               val cc = factions.%!(_.hero.any).%(of(_).dominance.none)
                                if (cc.any) {
                                    val mn = cc./(of(_).vp).min
                                    actions :+= FormCoalitionMainAction(f, cc.%(of(_).vp == mn), dd, Repeat)
                                }
                            }
                        }
                    case _ =>
                        if (f.dominance.none && f.vp >= 10) {
                            val dd = f.hand./~(_.dominance)
                            if (dd.any)
                                actions :+= ActivateDominanceMainAction(f, dd, Repeat)
                        }
                }
            }
    
            if (dominances.any) {
                val dd = dominances.%(d => f.hand.%(_.suit.m(d.suit)).any)
                if (dd.any)
                    actions :+= TakeDominanceMainAction(f, dd, Repeat)
            }

            actions
        }
    
        def eveningNewCard = false
        
        def evening = expose
        
        def expose : List[UserAction] = {
            var actions : List[UserAction] = Nil
            
            factions.but(f)./~(_.mischief).foreach { e =>
                val l = e.hidden.%(c => f.at(c).any)
                if (l.any)
                    actions :+= ExposeMainAction(f, e, l, Repeat).x(e.hidden.%(c => f.at(c).any && f.hand.%(_.suit.m(c.suit)).any).none, "no matching cards")
            }
    
            actions
        }

    }
      
    def at(c : Clearing, f : Faction) = atRegionFaction(c, f)./(_.piece)

    def atRegionFaction(r : Region, f : Faction) : List[Figure] = r match {
        case c : Clearing => xclearings(c).%(_.faction == f)
        case c : Forest => xforests(c).%(_.faction == f)
    }

    def atRegion(r : Region) = r match {
        case c : Clearing => xclearings(c).get ++ ruins.get(c)./(Figure(null, _, 0)) ++ (tower.contains(c)).?(Figure(null, Tower, 0)) ++ (ferry.contains(c)).?(Figure(null, Ferry, 0))
        case c : Forest => xforests(c).get
    }
    
    def canBuild(f : Faction)(c : Clearing) = canPlace(f)(c) && (c.capacity > xclearings(c).get.building.num + ruins.get(c).any.??(1))
    
    def canPlace(f : Faction)(c : Clearing) = scorched.has(c).not && factions.but(f)./~(_.feline).%(_.at(c).got(Keep)).none && factions.but(f)./~(_.mischief).%(e => e.at(c).got(Snare) && e.hidden.has(c).not).none

    def hasKeep(c : Clearing) = xclearings(c).got(Keep)
    
    var uncrafted : List[Item] = $(Bag, Bag, Crossbow, Boots, Boots, Hammer, Sword, Sword, Teapot, Teapot, Coins, Coins)
    
    var turn : Int = 0
   
    var current : Faction = null

    var phase : Phase = Night
    
    def mc(f : Feline) : FelinePlayer = pstates(f).as[FelinePlayer].get
    def ed(f : Aviary) : AviaryPlayer = pstates(f).as[AviaryPlayer].get
    def wa(f : Insurgent) : InsurgentPlayer = pstates(f).as[InsurgentPlayer].get
    def rf(f : Trader) : TraderPlayer = pstates(f).as[TraderPlayer].get
    def lc(f : Fanatic) : FanaticPlayer = pstates(f).as[FanaticPlayer].get
    def ud(f : Underground) : UndergroundPlayer = pstates(f).as[UndergroundPlayer].get
    def cc(f : Mischief) : MischiefPlayer = pstates(f).as[MischiefPlayer].get
    def ki(f : Expedition) : ExpeditionPlayer = pstates(f).as[ExpeditionPlayer].get
    def ok(f : OldExpedition) : OldExpeditionPlayer = pstates(f).as[OldExpeditionPlayer].get
    def lh(f : Horde) : HordePlayer = pstates(f).as[HordePlayer].get
    def vb(f : Hero) : HeroPlayer = pstates(f).as[HeroPlayer].get

    def of(f : Faction) = pstates(f)

    def ruleValue(f : Faction, c : Region, hired : List[Trader]) : Int = {
        if (f.underground.%(_.burrow == c).any)
            return 999
        
        val sk = f.has(SoupKitchens)

        val result = f.at(c)./(_.piece @@ {
            case Garden(_) => 333
            case b : Building => 8
            case w : Warrior => 8
            case t : Token if sk => 8 * 2
            case _ => 0
        }).sum + (hired./(_.at(c).warrior.num).sum * 8)

        result + (result > 0 && f.has(RuleIfTied)).??(1)
    }
    
    def rule(f : Faction)(c : Region) = {
        val hired = traders.%(s => f.has(Mercenaries(s)) || f.has(Peacekeepers(s)))
     
        ruleValue(f, c, hired) > factions.but(f).diff(hired)./(e => ruleValue(e, c, Nil)).maxOr(0)
    }

    def ruleSelf(f : Faction)(c : Clearing) = ruleValue(f, c, Nil) > factions.but(f)./(e => ruleValue(e, c, Nil)).maxOr(0)
    
    def movable(f : Faction) = (clearings ++ extraMoveFrom(f)).%(c => f.at(c).movable.any).distinct
    
    def moveFromR(f : Faction) : List[Region] = movePlans(f, movable(f).%(o => snaredFor(f)(o).not), transports./($(_)) ** f.transports).keys.toList
    
    def moveFrom(f : Faction) = moveFromR(f).of[Clearing]
    
    def attackSelf(f : Faction)(c : Clearing) = (f.at(c).warrior.any || (f.at(c).any && f.hero./?(_.swords > 0))).??(factions.but(f).%(_.at(c).any))
    def attackAlly(f : Faction)(c : Clearing) = (f.at(c).any && f.hero.any).??(factions.but(f).%(_.at(c).warrior.any).%(a => f.hero./?(_.attitude(a) == Allied))./~(a => factions.but(f).but(a)).%(_.at(c).any))
    def attackMerc(f : Faction)(c : Clearing) = traders.%(s => f.has(Mercenaries(s))).%(s => s.at(c).warrior.any)./~(s => factions.but(f).but(s).%(_.at(c).any)).distinct
    def attack(f : Faction)(c : Clearing) = (attackSelf(f)(c) ++ attackAlly(f)(c) ++ attackMerc(f)(c)).distinct

    implicit def descCard(g : Game, d : DeckCard) = d.img
    
    implicit val convQI = (l : List[QuasiItem], n : Int, s : List[Int]) => (l(n) @@ {
        case q @ ToExhaustDamage(v, r) => ToExhaustDamage(v.diff(s.%(_ < n)./(l.apply)./~(_.real)./(_.item)), r)
        case q => q
    }).elem(s.has(n))(styles.iii)
        
    
    def loggedPerform(action : Action) : Continue = {
        val c = performZ(action)
        
        c match {
            case Ask(_, Nil, _) =>
                println("")
                println("")
                println("")
                println("WTF!!!")
                println("Empty Ask as a result of " + action)
            case _ =>
        }

        if (pstates.size == factions.num && !action.isInstanceOf[NoGameOverTrigger]) {
            if (gameover != null)
                return gameover

            val ww = factions.%(f => f.vp >= 30)
            if (ww.any) {
                val cw = factions.%(f => ww.%(w => f.coalition == Some(w)).any)
                val winners = ww ++ cw

                highlights :+= NothingHighlight
                highlights :+= NothingHighlight
                highlights :+= NothingHighlight
                
                log(winners./(_.elem).join(", "), "won")

                gameover = GameOver(winners, "Game Over" ~ Break ~ winners./(f => f.elem ~ " won" ~ Break ~ (of(f).vp >= 0).??(of(f).scorelog.join(Break))).join(Break), (null +: Nil)./(f => {
                    Ask(f, winners./~(w => 
                        $(GameOverWonAction(f, w)) ++ (of(w).vp >= 0).?(GameOverTextAction(f, of(w).scorelog.join(HorizontalBreak)))
                        ) ++ $(GameOverMapsAction(f)) ++ hrf.HRF.flag("offline").not.?(GameOverSaveReplayAction)
                    )
                }))

                return gameover
            }
        }

        highlightFaction = (c @@ {
            case Ask(f, _, _) => $(f)
            case MultiAsk(a) => a./(_.faction)
            case Notify(l, _, _) => l
            case _ => Nil
        }).of[Faction]
        
        def fix(f : Player) = f @@ {
            case f : Faction => ftp(f)
            case _ => f
        }
        
        c @@ {
            case c : Ask => c.copy(faction = fix(c.faction))
            case c : MultiAsk => c.copy(asks = c.asks./(c => c.copy(faction = fix(c.faction))))
            case c : Notify => c.copy(factions = c.factions./(fix))
            case c => c
        }
    }
    
    def viewHand(f : Faction) = f.hand./(ViewCardInfoAction(f, PreBreak(OwnHand).elem(game), _))
    def viewSupporters(f : Faction) = f.insurgent./?(f => f.supporters./(ViewCardInfoAction(f, PreBreak(FactionSupporters(f)).elem(game), _)))
    def viewQuests(f : Faction) = f.hero./?(f => quests.take(3)./(QuestInfoAction(f, _, f.inv)))
    def viewPlots(f : Faction) = f.mischief./?(f => f.hidden./~(c => f.at(c).plot./(p => PlotInfoAction(f, p, c))))
    def viewTraderCards(f : Faction) = factions.but(f)./~(_.trader)./~(t => t.hand./(ViewCardInfoAction(f, PreBreak(FactionHand(t)).elem(game), _)))
    def viewRetinue(f : Faction) = f.expedition./~(f => Retinue.all./~(r => (f.retinue(r) ++ f.complete(r))./(ViewCardInfoAction(f, Break ~ r.elem, _))))
    
    implicit class ActionsExExEx(l : List[UserAction])(implicit a : Action) {
        def birdsong(f : Faction) : List[UserAction] = l ++ f.birdsong
        def obirdsong(f : Faction) : List[UserAction] = (l ++ l.choice.??(f.birdsong))
        def daylight(f : Faction) : List[UserAction] = l ++ f.daylight
        def odaylight(f : Faction) : List[UserAction] = (l ++ l.choice.??(f.daylight))
        def evening(f : Faction) : List[UserAction] = l ++ f.evening
        def expose(f : Faction) : List[UserAction] = l ++ l.choice.??(f.expose)
        def noHand = l :+ NoHand
        def noPlots = l :+ NoPlots
        def noSup = l :+ NoSupporters
        def hiSup = l :+ HiSupporters
    }
    
    def craftableWith(f : Faction, craft : List[CraftAsset], used : List[CraftAsset], max : Int = 999) = (d : DeckCard) => d @@ {
        case cc : CraftCard if cc.cost.num > max => false
        case cc : CraftCard if (cc.cost ++ used).num > craft.num => false
        case cc : CraftCard if (cc.cost ++ used).but(AnySuit).diff(craft).any => false
        case cc : CraftItemCard if uncrafted.contains(cc.item).not => false
        case cc : CraftEffectCard if f.has(cc.effect) => false
        case cc : CraftCard => true
        case _ => false
    }
    
    def craftable(f : Faction) = craftableWith(f, f.craft ++ f.extracraft, f.crafted, f.trader./(_.funds.num).|(999))
    
    def info(waiting : List[Player], self : Option[Player], actions : List[UserAction]) : List[UserAction with Info] = {
        (self.of[Faction] ++ self./~(ptf.get)).toList.distinct.%(pstates.contains)./~( f => 
            actions.has(HiSupporters).??(viewSupporters(f)) ++
            actions.has(NoPlots).not.??(viewPlots(f)) ++ 
            actions.has(NoHand).not.??(viewHand(f)) ++ 
            (actions.has(NoSupporters).not && actions.has(HiSupporters).not).??(viewSupporters(f)) ++
            viewQuests(f) ++
            viewRetinue(f) ++
            viewTraderCards(f)
        ) ++
        self.of[PlayerN]./~( p => 
            actions.has(NoHand).not.??(predrafts.get(p)./~(_.get)./(ViewCardInfoAction(p, Break ~ p.elem ~ " drew cards", _)))
        )
    }

    def preinfo(waiting : List[Player], self : Option[Player], actions : List[UserAction]) : List[UserAction with Info] = {
        self @@ {
            case Some(f : Aviary) if current == f && f.leader != null =>
                import helper._

                val adding = actions./~{
                    case DecreeAction(self, h : List[DeckCard], l : List[Int], d : List[Decree]) if self == f =>
                        d.lazyZip(l).map((d, n) => d -> h(n).suit)

                    case _ => Nil
                }.groupMap(_._1)(_._2)

                val decree = Decree.all.map { d =>
                    val t = f.todo(d) ++ adding.get(d).|(Nil)
                    val o = f.done(d)
                    val r = t.diff(o).diff(o.diff(t)./(_ => Bird))
                    val c = t.diff(r)
                    val id = f.style + "-" + (d == Decree.Recruit && f.leader == Charismatic).??("double-") + d.name
                    c./(s => Image(id + "-" + s.name + "-done", styles.action)) ++ r./(s => Image(id + "-" + s.name, styles.action))
                }

                val dddd = &(&(&(decree(0)) ~ " " ~ &(decree(1))) ~ " " ~ &(decree(2) ~ " " ~ &(decree(3))))
            
                case object DecreeDescAction extends BaseInfo(dddd)(Empty) { val self = f }
                
                $(DecreeDescAction)

            case _ => Nil
        }
    }

    

    private[this] var next : ForcedAction = null
    private[this] var repeat : ForcedAction = null

    def performZ(a : Action) : Continue = {
        val xa = a match {
            case Next => next
            case Repeat => repeat
            case a => a
        }

        implicit val action = xa match {
            case a : TopLevelAction =>
                val twf = options.has(TotalWarDominance).??(factions.%(_.dominance.any).%(_.vp > 0)./~(_.warf))
                if (twf.any)
                    TotalWarAction(twf(0), a)
                else
                    a    
            case a => a
        }
        
        next = action match {
            case a : TopLevelAction => a.next
            case _ => next
        }

        repeat = action match {
            case a : TopLevelAction => a
            case _ => repeat
        }
        
        val cc = expansions./(_.perform(this, action)).but(UnknownContinue)

        if (cc.num > 1)
            if (cc.distinct.num > 1 && action.isInstanceOf[SideEffectOnly].not)
                throw new Error("Conflicting continue on " + action)

        if (cc.any) {
            cc(0)
        }
        else action match {
            // INIT
            case StartAction(v) =>
                version = v

                log("Root".hlb, "HRF".hl, hrf.Info.version.hlb)

                if (players.of[Faction].any)
                    log("Factions", players.of[Faction]./(_.elem).comma)

                options.foreach { o =>
                    log(o.group, o.valueOn)
                }
                
                Milestone(DraftFactionsAction)

            case DraftFactionsAction =>
                if (options.has(SeatingGiven))
                    Force(SeatingOrderAction(players))
                else
                    Shuffle(players, SeatingOrderAction)

            case SeatingOrderAction(t) =>
                seating = t

                if (options.has(SeatingRandom))
                    log("Seating", seating./(_.elem).comma)
                
                val militant = setup.of[Feline] ++ setup.of[Aviary] ++ setup.of[Underground] ++ setup.of[Horde] ++ setup.of[Expedition]
                
                val combinations =
                    if (setup.num == players.num)
                        $(setup)
                    else
                    if (militant.any)
                        militant./~(m => setup.but(m)./~(l => setup.but(m).but(l).combinations(players.num - 1)./(c => m +: (c :+ l))))
                    else
                        setup.combinations(players.num + 1).toList

                Random[$[Faction]](combinations, t => FactionsCombinationAction(t))

            case FactionsCombinationAction(t) =>
                drawn = t
                
                Shuffle[Character](HeroExpansion.characters, t => CharactersCombinationAction(t))
        
            case CharactersCombinationAction(t) =>
                drawn.of[Hero].lazyZip(t).foreach((h, c) => chars += h -> c)
                
                if (options.has(FactionSeatingRandom))
                    log("Drawn factions", drawn./(f => f.hero./(h => FactionCharacterChoice(h, chars(h))).|(FactionChoice(f)))./(_.elem).comma)
                
                Milestone(ProcessOptionsAction)
                
            case ProcessOptionsAction =>
                options./~{
                    case DefaultClearings if board == AutumnBoard => 
                        mapping = AutumnBoard.defaultMapping
                        Some(Force(DraftCardsAction))

                    case AllRandomClearings => 
                        Some(Shuffle[Clearing](board.clearings, AllRandomMappingAction(_)))
                        
                    case NoClustersClearings => 
                        Some(ShuffleUntil[Clearing](board.clearings, NoClustersMappingAction(_), l => {
                            val mapping = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)).toMap

                            def z(c : Clearing) = board.connected(c).%(mapping(_) == mapping(c)).num

                            board.clearings./(z).max == 0
                        }))

                    case SuitPairsClearings => 
                        Some(ShuffleUntil[Clearing](board.clearings, SuitPairsMappingAction(_), l => {
                            val mapping = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)).toMap

                            def z(c : Clearing) = board.connected(c).%(mapping(_) == mapping(c)).num

                            board.clearings./(z).%(_ != 1).none
                        }))
                        
                    case ConnectedClearings => 
                        Some(ShuffleUntil[Clearing](board.clearings, ConnectedMappingAction(_), l => {
                            val mapping = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)).toMap

                            def z(c : Clearing) = board.connected(c).%(mapping(_) == mapping(c)).num

                            board.clearings./(z).min > 0 && FoxRabbitMouse.%(s => board.clearings.%(mapping(_) == s)./(z).max < 2).none
                        }))

                    case ThreeFourFiveClearings => 
                        Some(Shuffle[Clearing](board.clearings, ThreeFourFiveMappingAction(_)))

                    case _ => None
                }.single.get 
            
            case AllRandomMappingAction(l) =>
                mapping = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)).toMap

                MappingFinishedAction
                
            case NoClustersMappingAction(l) =>
                mapping = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)).toMap

                def z(c : Clearing) = board.connected(c).%(_.suit == c.suit).num

                if (board.clearings./(z).max > 0)
                    Shuffle[Clearing](board.clearings, NoClustersMappingAction(_))
                else
                    Force(MappingFinishedAction)
                
            case SuitPairsMappingAction(l) =>
                mapping = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)).toMap

                def z(c : Clearing) = board.connected(c).%(_.suit == c.suit).num

                if (board.clearings./(z).%(_ != 1).any)
                    Shuffle[Clearing](board.clearings, SuitPairsMappingAction(_))
                else
                    Force(MappingFinishedAction)
                
            case ConnectedMappingAction(l) =>
                mapping = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)).toMap
                
                def z(c : Clearing) = board.connected(c).%(_.suit == c.suit).num

                if (board.clearings./(z).min == 0 || FoxRabbitMouse.%(s => board.clearings.%(_.suit == s)./(z).max < 2).any)
                    Shuffle[Clearing](board.clearings, ConnectedMappingAction(_))
                else
                    Force(MappingFinishedAction)

            case ThreeFourFiveMappingAction(l) =>
                Shuffle[BaseSuit](FoxRabbitMouse, ThreeFourFiveSuitMappingAction(_, l))
                
            case ThreeFourFiveSuitMappingAction(s, l) =>
                mapping = l.sortBy(c => c.capacity * 2 - board.ruins.has(c).??(1)).zip(s(0) * 5 ++ s(1) * 4 ++ s(2) * 3).toMap

                MappingFinishedAction
                    
            case MappingFinishedAction =>
                board.clearings.foreach { c =>
                    log(c, "was", mapping(c))
                }

                DraftCardsAction

            case DraftCardsAction if options.has(CardDraftFive) =>
                if (deck.num < arity * 5 && pile.any)
                    Shuffle[DeckCard](pile, ShufflePileAction(_, DraftCardsAction))
                else {
                    val pp = players.%(p => predrafts(p).none)

                    if (pp.any) {
                        val p = pp(0)
                 
                        val l = deck.get.take(5)

                        deck --> l --> predrafts(p)
                    
                        log(p, "drew", 5.of("card"))
                        
                        Notify($(p), l./(d => ViewCardInfoAction(p, p.elem ~ " drew " ~ 5.cards, d)) :+ NoHand, DraftCardsAction)
                    }
                    else
                        SetupNextAction 
                }
                
            case DraftCardsAction =>
                SetupNextAction

            case SetupNextAction =>
                val indices = 0.until(players.num).%!(fseating.contains).toList
                
                if (indices.any) {
                    val remaining = setup.%!(pstates.contains)
                    
                    if (options.has(FactionSeatingGiven) && options.has(SetupOrderPriority)) {
                        val f = remaining.minBy(_.priority)
                        val n = setup.indexOf(f)

                        SetupFactionIndexAction(f, n)
                    }
                    else
                    if (options.has(FactionSeatingGiven) && options.has(SetupOrderReverse)) {
                        val n = indices.max
                        val f = setup(n)
                        
                        SetupFactionIndexAction(f, n)
                    }
                    else
                    if (options.has(FactionSeatingRandom) && options.has(SetupOrderPriority)) {
                        val f = remaining.minBy(_.priority)

                        Random(indices, SetupFactionRandomIndexAction(f, _))
                    }
                    else
                    if (options.has(FactionSeatingRandom) && options.has(SetupOrderReverse)) {
                        val n = indices.max
                        
                        val l = drawn.intersect(remaining)./(f => f.hero./(h => FactionCharacterChoice(h, chars(h))).|(FactionChoice(f)))
                        
                        implicit def convert(f : PlayChoice) = f @@ {
                            case FactionCharacterChoice(f, c) =>
                                desc(Image(c.title)(styles.chartitle), Break, Image(c.img)(styles.quest)((c == Ranger).?(styles.ranger))((c == Adventurer).?(styles.adventurer)).div(styles.charblock) ~ c.starting./(_.img).merge, Break, c.special)
                            case FactionChoice(f) =>
                                desc(Image(f.short + "-title")(styles.chartitle), Break, Image(f.short + "-char")(styles.quest)(charstyles.get(f)).div(styles.charblock) ~ f.advertising, Break, f.motto)
                        }

                        YYSelectObjectsAction(seating(n), l)
                            .withGroup("Choose faction")
                            .withThen(f => SelectFactionAction(seating(n), n, f))("Play as " ~ _.elem)("~~~")

                    }
                    else
                        UnknownContinue
                }
                else {
                    factions = 0.until(players.num)./(fseating)
                    
                    factions.foldRight(SetupDoneAction : ForcedAction)((f, q) => AfterSetupAction(f, q))
                }

            case SelectFactionAction(p, n, f) =>
                ftp += f.faction -> p
                ptf += p -> f.faction

                log(p, "played", f)

                SetupFactionIndexAction(f.faction, n)
                
            case SetupRandomFactionIndexAction(f, n) =>
                SetupFactionIndexAction(f, n)

            case SetupFactionRandomIndexAction(f, n) =>
                SetupFactionIndexAction(f, n)
                
            case SetupFactionIndexAction(f, n) =>
                fseating += n -> f

                CreatePlayerAction(f)
                
            case InitDoneAction =>
                StartPlayerTurnAction(factions(0))

            case FactionInitAction(f) =>
                factions :+= f

                if (predrafts.get(ftp(f))./~(_.get).any) {
                    predrafts(ftp(f)) --> f.hand

                    FactionSetupAction(f)
                }
                else
                    DrawCardsAction(f, 3, NotInLog(OnStart), AddCardsAction(f, FactionSetupAction(f)))
                
            case StartingCornerAction(f) =>
                val cc = corners./~(board.opposite).diff(corners).some.|(board.corners.diff(corners))
                Ask(f, cc./(ChooseCornerAction(f, _))).needOk

            case ChooseCornerAction(f, r) =>
                corners :+= r
                StartingClearingAction(f, r)

            case PlaceStartingWarriorsAction(f, l, p, then) =>
                l.foreach(x => f.pool.one(p) --> x)

                l.distinct.foreach(x => f.log("placed", p.of(f).repeat(l.count(x)).comma, "in", x))

                then
                
            case AfterSetupAction(f : Hero, then) =>
                AddRuinItemsAction(then)
                
            case AfterSetupAction(f : Horde, then) if factions.of[Hero].none =>
                AddRuinItemsAction(then)

            case AfterSetupAction(f, then) =>
                then
                
            case AddRuinItemsAction(then) =>
                Shuffle[Item]($(Boots, Sword, Hammer, Bag), ShuffleRuinItemsAction(_, then))

            case ShuffleRuinItemsAction(l, then) =>
                ruins.keys.lazyZip(l).foreach { (r, i) => ruins += r -> Ruins(ruins(r).items :+ i) }

                then
                
            case SetupDoneAction =>
                (factions.diff(traders) ++ traders).foldRight(InitDoneAction : ForcedAction)((f, q) => DraftLimitAction(f, 3, q))
            
            case DraftLimitAction(f, n, then) =>
                if (f.hand.num <= n)
                    then
                else
                    XXSelectObjectsAction(f, f.hand)
                        .withGroup(f.elem ~ " keeps " ~ n.cards)
                        .withRule(_.num(n))
                        .withThen(l => DraftLimitDiscardAction(f, f.hand.diff(l), then))(l => "Keep".hl ~ l./(" " ~ _.elem))
                        .withExtra($(NoHand))
                    
            case DraftLimitDiscardAction(f, l, then) =>
                f.hand --> l --> deck

                Shuffle(deck, ShuffleDeckAction(_, then))

            case ShuffleDeckAction(l, then) =>
                deck --> l --> deck

                then

            // CARDS AND HAND
            case ShufflePileAction(l, then) =>
                pile --> l --> deck

                if (turn > 0)
                    log("The discard pile was shuffled")

                then
                
            case PeekCardsMainAction(f, s, m, l, then) =>
                Notify($(f), l./(d => ViewCardInfoAction(f, m.elem(game), d)), then)
    
            case DrawCardsAction(f, n, m, then) =>
                if (phase == Evening && f.has(Informants))
                    Ask(f)(FindAmbushAction(f, m, then))(DrawCardsFromDeckAction(f, n, m, then).as("Draw " ~ dt.CardBack.repeat(n).merge)).needOk
                else
                    DrawCardsFromDeckAction(f, n, m, then)

            case DrawCardsFromDeckAction(f, n, m, then) =>
                if (deck.num < n && pile.any)
                    Shuffle[DeckCard](pile, ShufflePileAction(_, DrawCardsAction(f, n, m, then)))
                else {    
                    if (deck.num < n) {
                        if (deck.num >= 2)
                            log("Only", deck.num.hl, "cards were left in the deck")
                        else
                        if (deck.num >= 1)
                            log("Only", deck.num.hl, "card was left in the deck")
                        else
                            log("No cards were left in the deck")
                    }
                
                    val x = min(n, deck.num)
                
                    if (x > 0) {
                        val l = deck.get.take(x)

                        if (f.drawn.any)
                            throw new Error("drawn overdrawn")

                        deck --> l --> f.drawn
                    
                        f.log("drew", n.of("card"), m)
                    
                        if (hrf.HRF.flag("fastsetup"))
                            then
                        else
                            Notify($(f), l./(d => ViewCardInfoAction(f, DrewFromTheDeck(f, m).elem(game), d)), then)
                    }
                    else
                        then
                }
                 
            case FindAmbushAction(f, m, then) =>
                Ask(f, pile./(d => TakeAmbushAction(f, d, then).x(d @@ { case Ambush(_) => false ; case _ => true })).cancel)

            case TakeAmbushAction(f, d, then) =>
                pile --> d --> f.drawn
                
                f.log("took", d, "from the dicard pile")
                
                then
                
            case AddCardsAction(f, then) =>
                f.drawn --> f.hand
                then
                
            case HandLimitAction(f, n, then) =>
                if (f.hand.num <= n)
                    then
                else
                if (f.hand.num <= 2*n && n > 3)
                    XXSelectObjectsAction(f, f.hand)
                        .withGroup(f.elem ~ " discards " ~ (f.hand.num - n).cards ~ " down to hand limit")
                        .withRule(_.num(f.hand.num - n))
                        .withThen(HandLimitDiscardAction(f, _, then))(l => "Discard".hl ~ l./(" " ~ _.elem))
                        .withExtra($(NoHand))
                else
                    XXSelectObjectsAction(f, f.hand)
                        .withGroup(f.elem ~ " keeps " ~ n.cards ~ (n == 5).??(" to hand limit"))
                        .withRule(_.num(n))
                        .withThen(l => HandLimitDiscardAction(f, f.hand.diff(l), then))(l => "Keep".hl ~ l./(" " ~ _.elem))
                        .withExtra($(NoHand))
                    
            case HandLimitDiscardAction(f, l, then) =>
                f.hand --> l --> discard(f, "due to hand limit")
                then

            case OpportunityDiscardCardsAction(f, m, n, l, t, then, alt) =>
                if (l.num == n) 
                    PrepareDiscardCardsAction(f, l, then)
                else {
                    val h = f.hand.diff(l)
                
                    if (l.num + h.%(_.suit.m(t)).num < n)
                        alt
                    else
                        Ask(f, h./(d => OpportunityDiscardSelectCardAction(f, IofN(m, l.num + 1, n), t, d, 1>0, OpportunityDiscardCardsAction(f, m, n, d +: l, t, then, alt)).x(d.suit.m(t).not)).skip(alt).noHand)
                }
                
            case OpportunityDiscardCardAction(f, m, t, then, alt) =>
                val h = f.hand.get
                
                if (h.%(_.suit.m(t)).none)
                    alt
                else
                    Ask(f, h./(d => OpportunityDiscardSelectCardAction(f, IofN(m, 1, 1), t, d, 1>0, PrepareDiscardCardsAction(f, $(d), then)).x(d.suit.m(t).not)).skip(alt).noHand)
                
            case OptionalDiscardCardAction(f, m, t, then) =>
                val h = f.hand.get
                
                if (h.%(_.suit.m(t)).none)
                    Ask(f).cancel
                else
                    Ask(f, h./(d => OpportunityDiscardSelectCardAction(f, IofN(m, 1, 1), t, d, 1>0, PrepareDiscardCardsAction(f, $(d), then)).x(d.suit.m(t).not)).cancel.noHand)
                
            case PrepareDiscardCardsAction(f, l, then) =>
                f.hand --> l --> f.drawn
                then
    
            case DiscardRandomCardAction(f, then) =>
                if (f.hand.any)
                    Random[DeckCard](f.hand, r => PerformDiscardRandomCardAction(f, r, then))
                else
                    then
                
            case PerformDiscardRandomCardAction(f, d, then) =>
                f.hand --> d --> discard(f, "randomly")
                then
    
            case StealCardAction(f, e, then) =>
                Random[DeckCard](e.hand, r => TakeCardAction(f, e, r, then))
                
            case TakeCardAction(f, e, d, then) =>
                e.hand --> d --> f.hand
                f.log("stole a card from", e)
                Notify($(f, e), $(ViewCardInfoAction(f, StoleFrom(f, e).elem(game), d)), then)
                
            case MassStealCardAction(f, e, then) =>
                e match {
                    case o :: r => 
                        val q = MassStealCardAction(f, r, then)
                        if (o.hand.any)
                            StealCardAction(f, o, q)
                        else
                            q
                    case Nil => 
                        then
                }

    
            // HELPERS
            case RefuseAction(then) =>
                then
    
            case SkipAction(then) =>
                then
    
            case DoneAction(then) =>
                then

            // REMOVE
            case ForcedRemoveAction(e, c, f, p, n, m, then) =>
                graveyard += c -> (graveyard(c) :+ Figure(f, p, -1))

                ForcedRemoveEffectAction(e, c, f, p, ForcedRemoveScoreAction(e, c, f, p, n, m, then))

            case ForcedRemoveEffectAction(e, c, f, p, then) =>
                then
                
            case ForcedRemoveScoreAction(e, c, f, p, n, m, then) =>
                e.oscore(n)(m, p.of(f))
                then

            case ForcedRemoveCleanupAction(f, then) =>
                coffins(f.limbo.get)
                then

            case NukeAction(f, spare, l, nuke, then) =>
                val t = l./~(c => factions.but(f).diff(spare)./~(e => e.at(c).%(_.piece match {
                    case _ if nuke == TotalAnnihilation => true
                    case _ : Pawn with Warrior => false
                    case _ if nuke == ClearSector => true
                    case _ : Building if nuke == AntiMaterial => true
                    case _ : Token if nuke == AntiMaterial => true
                    case _ => false
                })./(_ -> c)))
                
                val (v, z) = t.reverse.partition(_._1.piece == Vagabond)
                
                var q = then
                
                z./(_._1.faction).distinct.foreach { e =>
                    q = ForcedRemoveCleanupAction(e, q)
                }

                val n = z.%(_._1.piece.scoring.any).num
                
                v.foreach { case (x -> c) =>
                    q = BattleAssignHitsAction(x.faction, Battle(c, f, None, x.faction, None, false, None, None, 0, DummyAction), 3, q)
                }
                
                z.foreach { case (x -> c) =>
                    graveyard += c -> (graveyard(c) :+ Figure(x.faction, x.piece, -1))

                    x --> x.faction.limbo

                    f.log("removed", x.piece.of(x.faction), "in", c)
                    
                    q = ForcedRemoveEffectAction(f, c, x.faction, x.piece, q)
                }
            
                highlights :+= NukeHighlight(t./(_._2).distinct)
                
                f.oscore(n)("destroying everything in", t./(_._2).distinct./(_.elem(this)).join(", "))

                q


            // CRAFT
            case XCraftMainAction(f) =>
                val c = f.hand.%(craftable(f))
                val a = NiceCraftMainAction(f, f.craft ++ f.extracraft, f.crafted).x(f.hand.none).x(c.none, "nothing craftable")

                val extra = 
                    if (f.has(MorningCraft) && (c.any || f.birdsongNewCard))
                        Nil.birdsong(f)
                    else
                    if (f.has(DaylightCraft) && (c.any || f.daylightNewCard))
                        Nil.daylight(f)
                    else
                    if (f.has(EveningCraft) && (c.any || f.eveningNewCard))
                        Nil.evening(f)
                    else
                        Nil

                Ask(f)(a)(Next.as("End Craft"))(extra)
                
            case NiceCraftMainAction(f, _, _) =>
                Force(CraftMenuAction(f))
                
            case CraftMenuAction(f) =>
                val l = f.craft ++ f.extracraft
                val u = f.crafted
    
                YYSelectObjectsAction(f, f.hand)
                    .withGroup("Craft".styled(f) ~ " with " ~ l.diff(u)./(dt.CraftSuit).merge ~ u.diff(l).any.??(" " ~ "-".hl ~ " " ~ u.diff(l)./(dt.CraftSuit).merge))
                    .withRule(craftable(f))
                    .withThen(d => CraftAction(f, d, NoMessage, CraftPerformAction(f, d, NoMessage)))(d => desc("Craft", d, "with", d.asInstanceOf[CraftCard].cost./(dt.CraftSuit).merge))("Craft")
                    .withExtra($(NoHand, CancelAction))
                    
    
            case CraftAction(f, d : CraftCard, m, then) =>
                val a = (f.craft ++ f.extracraft).diff(f.crafted)
                val c : List[CraftAsset] = a.intersect(d.cost)
                val s : List[CraftAsset] = a.diff(d.cost)
                val t : List[CraftCost] = d.cost.diff(a)
                
                val x = s.combinations(t.num).filter(c => c.permutations.exists(p => p.lazyZip(t).forall((suit, cost) => suit.m(cost)))).toList
                
                Ask(f, x./(c ++ _)./(x => CraftAssignAction(f, d, a, x, m, then))).ocancel
                
            case CraftAssignAction(f, d, a, used, m, then) =>
                f.crafted ++= used
                
                then
                
            case CraftPerformAction(f, d, m) =>
                d match {
                    case d @ CraftItemCard(_, _, _, item, vp, _) =>
                        f.hand --> d --> discard
                        uncrafted :-= item
                        f.forTrade +:= item.pristine
                        CraftScoreAction(f, d, d.vp, m, MurineBrokerAction(f, Repeat))
                  
                    case d @ CraftEffectCard(_, _, _, effect) =>
                        f.hand --> d --> f.stuck
                        f.effects :+= effect
                        CraftEffectAction(f, d, m, Repeat)

                    case d @ Favor(suit, _, _) =>
                        f.hand --> d --> discard
                        CraftFavorAction(f, d, m, NukeAction(f, factions.%(f.friends), clearings.%(_.suit == suit), ClearSector, Repeat))
                }

            case MurineBrokerAction(f, then) =>
                var q : ForcedAction = then

                factions.but(f).%(_.has(MurineBroker)).foreach { b =>
                    q = DrawCardsAction(b, 1, WithEffect(MurineBroker), AddCardsAction(b, q))
                }
                
                q
                
            case CraftScoreAction(f, d, n, m, then) =>
                f.nscore(n + f.has(MasterEngravers).??(1))("crafting", d.item)(f, "crafted", d.item, d, "(" ~ d.cost.ss ~ ")", f.has(MasterEngravers).??("with " ~ MasterEngravers.elem), ForVP, m)
                
                AcquireItemsAction(f, then)
                
            case CraftEffectAction(f, d, m, then) =>
                f.log("prepared", d, "(" ~ d.cost.ss ~ ")", m)
                
                then

            case CraftFavorAction(f, d, m, then) =>
                f.log("was granted", d, "(" ~ d.cost.ss ~ ")", m)
                
                then

            case AcquireItemsAction(f, then) =>
                then


            // TOTAL WAR              
            case TotalWarAction(f, then) =>
                if (f.vp <= 0)
                    then
                else {
                    val r = (f.pooled(f.warrior) > 0).??(clearings.%(c => f.at(c).any).%(canPlace(f)))

                    if (r.any)
                        Ask(f)(r./(TotalWarRecruitAction(f, _, f.vp, then))).needOk
                    else {
                        val a = clearings.%(c => attack(f)(c).any)
                
                        if (a.any)
                            BattleInitAction(f, BattleTotalWar(f.vp), a, $(), TotalWarDoneAction(f, then))
                        else {
                            f.vp = 0

                            then
                        }
                    }
                }
    
            case TotalWarRecruitAction(f, c, _, then) =>
                f.pool.one(f.warrior) --> c
                f.log("recruited", f.warrior.of(f), "in", c, "with", "Total War".hl)
                TotalWarDoneAction(f, then)
                
            case TotalWarDoneAction(f, then) =>
                f.vp -= 1
                TotalWarAction(f, then)

            // BIRDSONG
            case StandAndDeliverAction(f, o, then) =>
                Ask(f, o./(StandAndDeliverFactionAction(f, _, then)).cancel)
    
            case StandAndDeliverFactionAction(f, o, then) =>
                f.used :+= StandAndDeliver
                
                o.oscore(1)("from", StandAndDeliver)
 
                StealCardAction(f, o, then)
                
            case SwapMeetAction(f, l, then) =>
                Ask(f, l./(SwapMeetFactionAction(f, _, then)).cancel)
    
            case SwapMeetFactionAction(f, o, then) =>
                f.used :+= SwapMeet
                
                f.log("took a card from", o, "with", SwapMeet)
 
                StealCardAction(f, o, SwapMeetReturnAction(f, o, then))
                
            case SwapMeetReturnAction(f, o, then) =>
                Ask(f, f.hand./(d => SwapMeetReturnCardAction(f, o, d, then)))(NoHand).needOk
                
            case SwapMeetReturnCardAction(f, o, d, then) =>
                f.log("gave", o, "a card in return")

                f.hand --> d --> o.hand

                Notify($(o), $(ViewCardInfoAction(f, GaveBack(f, o).elem(game), d)), then)

            case FalseOrdersAction(f, l, then) =>
                Ask(f)(l./(FalseOrdersFactionAction(f, _, then))).cancel

            case FalseOrdersFactionAction(f, e, then) =>
                MoveInitAction(e, transports./($(_)) ** e.transports./(_.but(RuledMove)) ** $($(HalfWarriors)), FalseOrders(f), movable(e), movable(e), $(CancelAction), FalseOrdersCompleteAction(f, then), Some(f))

            case FalseOrdersCompleteAction(f, then) =>
                f.effects :-= FalseOrders
                f.stuck --> f.stuck.%({
                    case CraftEffectCard(_, _, _, FalseOrders) => true
                    case _ => false
                }) --> discard

                then
            
            //  DAYLIGHT
            case ActivateDominanceMainAction(f, l, then) =>
                Ask(f, l./(ActivateDominanceAction(f, _, then)).cancel)
            
            case ActivateDominanceAction(f, d, then) =>
                f.hand --> d --> f.stuck
                f.dominance = Some(d)
                f.vp = 0
                f.log("activated", d)
                then
            
            case FormCoalitionMainAction(f, cc, l, then) =>
                Ask(f, cc./(FormCoalitionFactionAction(f, _, l, then)).cancel)
            
            case FormCoalitionFactionAction(f, x, l, then) =>
                Ask(f, l./(FormCoalitionAction(f, x, _, then)).cancel)
                
            case FormCoalitionAction(f, x, d, then) =>
                f.hand --> d --> f.stuck
                f.dominance = Some(d)
                f.vp = -999
                f.coalition = Some(x)
                f.log("formed coalition with", x, "using", d)
                if (f.attitude(x) == Hostile) {
                    f.attitude += x -> Indifferent
                    x.log("became", Indifferent, "to", f)
                }
                then
            
            case TakeDominanceMainAction(f, l, then) =>
                Ask(f, l./(TakeDominanceAction(f, _, then)).cancel)
    
            case TakeDominanceAction(f, d, then) =>
                OptionalDiscardCardAction(f, TakeCard(d), d.suit, TakeDominanceTakeAction(f, d, then))
                
            case TakeDominanceTakeAction(f, d, then) =>
                f.drawn --> discard(f, "to take", d)
                dominances --> d --> f.hand
                then
                
            case TaxCollectorMainAction(f, l, then) =>
                Ask(f, l./~(c => f.at(c).warrior./(_.piece).distinct./(p => TaxCollectorAction(f, c, p, then))).cancel)
                
            case TaxCollectorAction(f, c, p, then) =>
                f.used :+= TaxCollector
                f.at(c).one(p) --> f.pool
                DrawCardsAction(f, 1, ConcatMessage(WithEffect(TaxCollector), InClearing(c)), AddCardsAction(f, then))

            case PropagandaBureauMainAction(f, l, then) =>
                Ask(f)(l./~(c => factions.but(f)./~(_.warf)./~(e => e.at(c)./~(_.piece.warrior).%(_.pawn.none).distinct./(PropagandaBureauSelectAction(f, c, e, _, then))))).cancel
                
            case PropagandaBureauSelectAction(f, c, e, p, then) =>
                OptionalDiscardCardAction(f, ToReplace(c, e, p), c.suit, PropagandaBureauAction(f, c, e, p, then))

            case PropagandaBureauAction(f, c, e, p, then) =>
                highlights :+= BattleHighlight(c)

                f.used :+= PropagandaBureau

                e.at(c).one(p) --> e.limbo

                f.pool.one(f.warrior) --> c
                
                f.log("brainwashed", p.of(e), "in", c, WithEffect(PropagandaBureau), "and", f.drawn.get)
                
                f.drawn --> discard(f).quiet
                
                ForcedRemoveAction(f, c, e, p, p.scoring.any.??(1), NoMessage, ForcedRemoveCleanupAction(e, then))
               
            case TalentScoutMainAction(f, s, l, then) =>
                Ask(f)(l./(c => TalentScoutAction(f, s, c, f.warrior, then))).cancel
                
            case TalentScoutAction(f, s, c, p, then) =>
                f.used :+= TalentScout(s)
                f.pool.one(p) --> c
                f.log("recruited", p.of(f), "in", c, "with", TalentScout(s))
                then
                
            case CodebreakersMainAction(f, l, then) =>
                Ask(f, l./(CodebreakersAction(f, _, then)).cancel)
                
            case CodebreakersAction(f, o, then) =>
                f.used :+= Codebreakers
                f.log("employed", Codebreakers, "to look at", o, "cards")
                Ask(f, o.hand./(CodebreakersNotifyAction(f, o, _)).done(then).okNeeded)

            case LeagueOfAdventurousMiceMainAction(f, lm, lb, then) =>
                YYSelectObjectsAction(f, f.forTrade./(ToExhaust))
                    .withGroup(LeagueOfAdventurousMice.elem)
                    .withRule(_.ref.exhausted.not)
                    .withThens(i => 
                        lm.any.??($(MoveInitAction(f, Nil, WithEffect(LeagueOfAdventurousMice), lm, movable(f), $(CancelAction), ExhaustForTradeItemAction(f, i.ref, then)).as("Move", dt.Move))) ++
                        lb.any.??($(BattleInitAction(f, WithEffect(LeagueOfAdventurousMice), lb, $(CancelAction), ExhaustForTradeItemAction(f, i.ref, then)).as("Battle", dt.Battle)))
                    )
                    .withExtra($(CancelAction))
              
            case MoveListAction(f, t, m, from, to, l, ExhaustForTradeItemAction(ff, ii, then)) =>
                ExhaustForTradeItemAction(ff, ii, ForceAction(MoveListAction(f, t, m, from, to, l, then)))
            
            case BattleStartAction(f, a, m, c, o, i, ExhaustForTradeItemAction(ff, ii, then)) =>
                ExhaustForTradeItemAction(ff, ii, ForceAction(BattleStartAction(f, a, m, c, o, i, then)))

            case ExhaustForTradeItemAction(f, i, then) =>
                f.forTrade :-= i
                f.forTrade :+= i.exhaust

                f.log("exhausted", i.exhaust)

                then


            case ClearPathMainAction(f, then) =>
                Ask(f, rubble./{ case (a, b) => ClearPathBetweenAction(f, a, b, then).x(f.at(a).none && f.at(b).none, "no presense") }.cancel)
            
            case ClearPathBetweenAction(f, a, b, then) =>
                OptionalDiscardCardAction(f, ToClearPath(a, b), AnySuit, ClearPathAction(f, a, b, then))
    
            case ClearPathAction(f, a, b, then) =>
                f.nscore(1)("clearing path")(f, "cleared path between", a, "and", b, "with", f.drawn.get, ForVP)
                f.drawn --> discard(f).quiet
                rubble :-= (a, b)
                f.used :+= ClearPath
                then
 
            // TURN - START
            case StartPlayerTurnAction(f) =>
                highlights :+= NothingHighlight

                log(DoubleLine)

                Milestone(BirdsongStartAction(f))

            case BirdsongStartAction(f) =>
                turn += 1
                current = f
                log("Turn", ("#" + turn).hl, "-", f)
                f.used = Nil

                f.dominance match {
                    case Some(Dominance(Bird)) if board.diagonals.%(c => rule(f)(c._1) && rule(f)(c._2)).any =>
                        f.vp = 999
                        f.log("achieved", "Bird Dominance".styled(Bird))
                    case Some(Dominance(s)) if clearings.%(_.suit.m(s)).%(rule(f)).num >= 3 => 
                        f.vp = 999
                        f.log("achieved", (s.name + " Dominance").styled(s))
                    case _ =>
                }
                
                phase = Birdsong

                log(Birdsong)

                BirdsongNAction(0, f)

            case BirdsongNAction(0, f) =>
                val n = clearings.%(ruleSelf(f)).num
                if (f.can(RoyalClaim) && n > 0)
                    Ask(f, $(RoyalClaimAction(f, n)).skip(Next))
                else
                    Next

            case RoyalClaimAction(f, n) =>
                f.effects :-= RoyalClaim
                f.stuck --> f.stuck.%({
                    case CraftEffectCard(_, _, _, RoyalClaim) => true
                    case _ => false
                }) --> discard
                f.oscore(n)("asserting", RoyalClaim)
                Next
                
            case BirdsongNAction(5, f) =>
                if (f.has(CoffinMakers)) {
                    f.oscore(xcoffins.num / 5)("from", CoffinMakers)
                    buryAll()
                }

                Next
            
            case BirdsongNAction(10, f) =>
                Ask(f)(
                    f.can(BetterBurrowBank).??(factions.but(f)./(BetterBurrowBankAction(f, _)))
                )(
                    f.can(Saboteurs).??(factions.but(f)./~(e => e.stuck./(d => SaboteurAction(f, e, d))))
                )(
                    f.can(BetterBurrowBank).not.?(Next.as("Skip"))
                )
 
            case BetterBurrowBankAction(f, o) =>
                f.used :+= BetterBurrowBank
                f.log("used", BetterBurrowBank, "with", o.elem ~ ", " ~ ("each drew " ~ 1.hl ~ " card").spn(xlo.nowrap))
                DrawCardsAction(f, 1, SuppressLog, AddCardsAction(f, DrawCardsAction(o, 1, SuppressLog, AddCardsAction(o, Next))))
                     
            case SaboteurAction(f, e, d) =>
                f.effects :-= Saboteurs
                f.stuck --> f.stuck.%({
                    case CraftEffectCard(_, _, _, Saboteurs) => true
                    case _ => false
                }) --> discard

                val effect = d @@ { case CraftEffectCard(_, _, _, effect) => effect }

                if (effect == CoffinMakers)
                    buryAll()

                e.effects :-= effect
                e.stuck --> d --> discard
                
                f.log("used", Saboteurs, "to destroy", effect, "of", e)

                Repeat

            case BirdsongNAction(98, f) =>
                if (f.birdsongOnly)
                    Ask(f)(Nil.birdsong(f))(Next.as("End " ~ Birdsong.elem))
                else
                    Next

            case BirdsongNAction(99, f) =>
                if (f.has(EyrieEmigre))
                    MoveInitAction(f, Nil, WithEffect(EyrieEmigre), moveFrom(f), movable(f), $(EyrieEmigreDiscardAction(f)), EyrieEmigreAttackAction(f, Nil))
                else
                    Next

            case MoveFinishedAction(f, from, to : Clearing, EyrieEmigreAttackAction(ff, Nil)) =>
                MoveFinishedAction(f, from, to, EyrieEmigreAttackAction(ff, $(to)))

            case EyrieEmigreAttackAction(f, l) =>
                BattleInitAction(f, WithEffect(EyrieEmigre), l, $(EyrieEmigreDiscardAction(f)), Next)
                    
            case EyrieEmigreDiscardAction(f) =>
                f.effects :-= EyrieEmigre
                f.stuck --> f.stuck.%({
                    case CraftEffectCard(_, _, _, EyrieEmigre) => true
                    case _ => false
                }) --> discard(f).quiet
                
                f.log("failed", EyrieEmigre)

                Next
            
            // TURN - DAYLIGHT
            case DaylightStartAction(f) =>
                phase = Daylight
                
                log(Daylight)

                DaylightNAction(0, f)

            case DaylightNAction(0, f) =>
                val cw = f.has(CommandWarren).??(clearings.%(c => attack(f)(c).any))
                
                if (cw.any)
                    BattleInitAction(f, BattleCommandWarren, cw, $(Next.as("Skip " ~ CommandWarren.elem)), Next)
                else
                    Next

            // TURN - EVENING
            case EveningStartAction(f) =>
                phase = Evening

                log(Evening)

                EveningNAction(0, f)
                 
            case EveningNAction(0, f) =>
                if (f.can(Cobbler) || f.can(CharmOffensive))
                    (Ask(f)
                        (f.can(Cobbler).?(MoveInitAction(f, Nil, MoveCobbler, moveFromR(f), movable(f), $(CancelAction), UsedEffectAction(f, Cobbler, Repeat)).as(Cobbler)(Evening)))
                        (f.can(CharmOffensive).?(CharmOffensiveMainAction(f, factions.but(f), Repeat).as(CharmOffensive)(Evening)))
                        (Next.as("Skip"))
                        (f.evening)
                    )
                else
                    Next

            case UsedEffectAction(f, e, then) =>
                f.used :+= e

                then
                    
            case CharmOffensiveMainAction(f, l, then) =>
                Ask(f)(l./(e => CharmOffensiveAction(f, e, then).as(e, "gets", 1.vp)(CharmOffensive))).cancel

            case CharmOffensiveAction(f, e, then) =>
                e.oscore(1)("from", CharmOffensive)
                
                DrawCardsAction(f, 1, WithEffect(CharmOffensive), AddCardsAction(f, UsedEffectAction(f, CharmOffensive, Repeat)))

            // TURN - EVENING END    
            case EveningDrawAction(f, n) =>
                DrawCardsAction(f, n, NotInLog(OnTurn(turn)), AddCardsAction(f, EveningHandLimitAction(f)))
                
            case EveningHandLimitAction(f) =>
                HandLimitAction(f, 5, EveningEndAction(f))
                
            case EveningEndAction(f) =>
                CleanUpAction(f)

            case CleanUpAction(f) =>
                f.crafted = Nil
                f.extracraft = Nil
                f.services = Nil
                FactionCleanUpAction(f)

            case EndPlayerTurnAction(f) =>
                tower.foreach { c =>
                    if (rule(f)(c))
                        f.oscore(1)("controlling", "Tower".hl)
                }

                phase = Night

                factions = factions.drop(1) ++ factions.take(1)
                Milestone(StartPlayerTurnAction(factions(0)))

            // END
            case WarnAction(f, then, q, proceed, cancel) =>
                Ask(f)(Cancel.as(cancel)(q))(then.as(proceed)(q))
            
            case EndTurnAction(f) =>
                Next

            case EndTurnSoftAction(f, m) =>
                if (hrf.HRF.flag("fastsetup"))
                    Ask(f)(EndTurnWarnAction(f, m))
                else
                    Ask(f)(EndTurnCancelAction(f, m))(EndTurnWarnAction(f, m))
            
            case EndTurnWarnAction(f, m) =>
                Next

            // BATTLE
            case BattleInitAction(f, m, from, extra, then) =>
                val xx = from./(c => BattleFromAction(f, m, c, attack(f)(c), then, CancelAction))

                Ask(f, xx ++ extra)
            
            case BattleFromAction(f, m, c, l, then, alt) =>
                val ff = f.at(c).attacking.any.??($(f))
                
                val aa = f.hero./?(f => l.%!(_.hero.any).%(l.but(_).any).%(f.attitude(_) == Allied).%(_.at(c).warrior.any))
                
                val mm = traders.%(s => f.has(Mercenaries(s)) && s.at(c).warrior.any && l.but(s).any)
                
                val xx = (ff ++ aa ++ mm)./(a => BattleAllyAction(f, a, m, c, l.but(a), then, alt))
                
                if (xx.num == 1)
                    Force(xx.head)
                else
                    Ask(f, xx :+ alt)
                
            case BattleAllyAction(f, a, m, c, l, then, alt) =>
                val ll = f.attacked.intersect(l) ++ l.diff(f.attacked)
                Ask(f, ll./(BattleStartAction(f, a, m, c, _, None, then)) :+ alt)

            // Loot - attacker, cancel own rolled hits

            // Enlist Arbiter - defender
            // Scouting Party - attacker, no Ambush
            // Bitter - attacker and defender, convert mobs to warriors

            // Devout Knight - attacker and defender, cancel first hit
            // Armored - attacker and defender, cancel first hit
            // Stubborn - attacker and defender, cancel first hit
            
            // Ambush - defender, 2 hits
            // Ambush - attacker, cancel Ambush
    
            // Wrathful - attacker, extra hit
            // Commander - attacker, extra hit
            // Defenseless - attacker, extra hit
            // Paw Crossbow - attacker, extra hit
            // Embedded Agents - defender, extra hit
            
            // Brutal Tactics - attacker, extra hit / enemy 1 vp
            // Armorers - attacker and defender, cancel rolled hits, discard
            // Sappers - defender, extra hit, discard
            // Partisans - attacker and defender, extra hit, discard hand cards
            // Swift Strike - attacker and defender, extra hit, exhaust sword
    
            // Despot - attacker and defender, 1 vp if buildings or tokens removed
            // Infamy - attacker, 1 vp for each piece
            // Loot - attacker, take item
    
            case BattleStartAction(f, a, m, c, o, i, then) =>
                highlights :+= BattleHighlight(c)

                f.attacked = (o :: f.attacked).distinct

                log(SingleLine)
                
                val b = new Battle(c, f, (f != a).?(a), o, i, false, None, None, 0, then)
                
                if (f == a)
                    f.log("battled", o, "in", c, m)
                else
                    f.log("and", a, "battled", o, "in", c, m)
                
                val dl = o.hero./(_.swords).|(o.at(c).warrior.num) == 0

                if (f == a) {
                    log(f.hero.any.?(f).|(f.at(c)./(_.elem).join(", ")), "versus", dl.??("defenseless"), o.hero.any.?(o).|(o.at(c)./(_.elem).join(", ")))
                }
                else {
                    f.log("attacked with", f.at(c)./(_.elem).join(", "))
                    a.log("helped with", a.at(c).warrior./(_.elem).join(", "))
                    o.log(dl.?("was defenseless").|("defended") ~ (o.hero.none).??(" with " ~ o.at(c)./(_.elem).join(", ")))
                }
                
                
                var q : ForcedAction = BattleStartedAction(b)
                
                factions.but(f)./~(_.insurgent).%(_.at(c).got(Sympathy)).%(_.has(BattleOutrage)).foreach { outraged =>
                    q = OutrageAction(outraged, f, c, q)
                }
                
                q
    
            case BattleStartedAction(b) =>
                BattleDefenderPreRollAction(b)

            case BattleDefenderPreRollAction(b) =>
                val f = b.defender

                val a = factions.but(b.attacker).but(b.defender).but(b.ally).but(b.arbiter)./~(_.hero).%(_.has(Protector)).%(_.at(b.clearing).any).%(_.swords > 0)

                val p = (b.codef.none && f.has(FundsDefense)).??(f @@ {
                    case f : Trader => factions.but(b.attacker).but(b.defender).but(b.ally)./~(_.warf).%(d => f.funds.%(_.faction == d).any).%(_.at(b.clearing).warrior.any)
                    case _ => throw new Error("funds defense is for traders")
                })

                val e = a./(BattleEnlistArbiterAction(f, b, _)) ++ p./(BattleEnlistDefenderAction(f, b, _))
                
                val z = b.ambush.not && !b.attacker.has(ScoutingParty) && f.hand.any
                val q = f.hand.%(d => d == Ambush(b.clearing.suit) || d == Ambush(Bird))
                val h = f.hand./(d => BattleAmbushAction(f, b, d, q.any, q.contains(d)).x(q.contains(d).not))

                val t = BattleAttackerPreRollAction(b)

                if (!z && e.none)
                    t
                else
                if (z && q.any)
                    Ask(f, e.++(h).done(t).noHand)
                else
                    Ask(f, e.done(t).++(h).noHand.okNeeded)
                    
            case BattleEnlistArbiterAction(f, b, a) =>
                b.defender.log("asked", a, "protection")

                a.oscore(1)("as", "Arbiter".styled(a))
                
                BattleDefenderPreRollAction(b.copy(arbiter = Some(a)))

            case BattleEnlistDefenderAction(f : Trader, b, d) =>
                b.defender.log("asked", d, "protection")
                
                f.spend($(d))
                
                BattleDefenderPreRollAction(b.copy(codef = Some(d)))

            case BattleAmbushAction(f, b, d, true, true) =>
                f.hand --> d --> discard

                f.log("played", d)

                BattleAttackerCounterAmbushAction(b.copy(ambush = true))
    
            case BattleAttackerCounterAmbushAction(b) =>
                val f = b.attacker
                val z = f.hand.any
                val q = f.hand.%(d => d == Ambush(b.clearing.suit) || d == Ambush(Bird))
                val h = f.hand./(d => BattleCounterAmbushAction(f, b, d, q.any, q.contains(d)).x(q.contains(d).not))
                
                val t = BattleAmbushHitsAction(b, 2)
                
                if (!z)
                    t
                else
                if (z && q.any)
                    Ask(f, h.done(t).noHand)
                else
                    Ask(f, (Nil.done(t) ++ h).noHand.okNeeded)
    
            case BattleCounterAmbushAction(f, b, d, true, true) =>
                f.hand --> d --> discard

                f.log("counter-played", d)
                
                BattleDefenderPreRollAction(b)
                
            case BattleAmbushHitsAction(b, n) =>
                if (n > 0)
                    BattleAssignHitsAction(b.attacker, b, n, BattleAmbushHitsAction(b, 0))
                else
                if (b.attacker.at(b.clearing).attacking.none)
                    BattleCleanupAction(b)
                else
                    BattleDefenderPreRollAction(b)
    
            case BattleAttackerPreRollAction(b) =>
                BattleRollAction(b)

            case BattleRollAction(b) =>
                Roll[Int](D4 :: D4, l => BattleRolledAction(b, l.max, l.min), b.attacker.elem ~ " in " ~ b.clearing.elem(this) ~ " battles " ~ b.defender.elem)
                
            case BattleRolledAction(b, fr, or) =>
                val f = b.attacker
                val o = b.defender
                val c = b.clearing
                
                val as = b.ally @@ {
                    case Some(a) => a.at(c).warrior.num
                    case _ => 0
                }
                
                val ds = b.arbiter @@ {
                    case Some(a) => a.swords
                    case _ => 0
                } + b.codef @@ {
                    case Some(d) => d.at(c).warrior.num
                    case _ => 0
                }
                
                val fs = f.hero./(_.swords).|(f.at(c).warrior.num) + as
                val os = o.hero./(_.swords).|(o.at(c).warrior.num) + ds
                
                BattleAdjustRollAction(b, b.attacker, b.defender, fs, os, fr, or)

            case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if b.defender.can(GuerillaTactics) && fr > or =>
                BattleAdjustRollAction(b, f, o, fs, os, or, fr)

            case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if fs > 0 && b.attacker.used.has(Looters) =>
                BattleAdjustRollAction(b, f, o, 0, os, fr, or)

            case BattleAdjustRollAction(b, f, o, fs, os, fr, or) =>
                var fh = min(fr, fs)
                var oh = min(or, os)
                
                BattleFinishRollAction(b, f, o, fs, os, fr, or, fh, oh)
                
            case BattleFinishRollAction(b, f, o, fs, os, fr, or, fh, oh) =>
                f.log("rolled", fr.roll, (fh > 0).??("and can deal", fh.hits))
                o.log("rolled", or.roll, (oh > 0).??("and can deal", oh.hits))
                
                DelayedContinue(50, Force(BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, 0, 0)))

            case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) if os == 0 && o.used.has(Defenseless).not =>
                o.used :+= Defenseless

                f.log("dealt an extra", 1.hit, "to defenseless")

                BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)
                
            case BattleBonusAction(b, f : Aviary, o, fs, os, fr, or, fh, oh, fe, oe) if f.can(Commander) =>
                f.used :+= Commander
                
                f.log("dealt an extra", 1.hit, "as", Commander)

                BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)
                
            case BattleBonusAction(b, f : Feline, o, fs, os, fr, or, fh, oh, fe, oe) if f.can(BonusHit) =>
                f.used :+= BonusHit
                
                f.log("dealt an extra", 1.hit, "with", BonusHit.of(f))

                BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)
                
            case BattleBonusAction(b, f : Horde, o, fs, os, fr, or, fh, oh, fe, oe) if f.can(Wrathful) && f.region == Some(b.clearing) =>
                f.used :+= Wrathful
                
                f.log("dealt an extra", 1.hit, "as", Wrathful)

                BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)
                
            case BattleBonusAction(b, f, o : Mischief, fs, os, fr, or, fh, oh, fe, oe) if o.can(EmbeddedAgents) && o.hidden.has(b.clearing) =>
                o.used :+= EmbeddedAgents

                o.log("dealt an extra", 1.hit, "with", "Embedded Agents".styled(o))
                
                BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe + 1)

            case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) =>
                BattlePostRollAction(b, fh, oh, fe, oe, $(f, o))
    
            case BattlePostRollAction(b, fh, oh, fe, oe, ask) =>
                if (ask.none)
                    BattleResolveHitsAction(b, fh + fe, oh + oe)
                else {
                    val h = Map(b.attacker -> fh, b.defender -> oh)
                    val ih = Map(b.defender -> fh, b.attacker -> oh)
                    val a = ask.head
                    val full = $(a) ++ $(b.attacker, b.defender).but(a)
                    
                    var actions : List[UserAction] = Nil
    
                    if (a.has(Armorers) && ih(a) > 0)
                        actions :+= ArmorersAction(a, BattlePostRollAction(b, (a == b.attacker).??(fh), (a == b.defender).??(oh), fe, oe, full))
                        
                    if (a == b.attacker && a.can(BrutalTactics))
                        actions :+= BrutalTacticsAction(a, b.defender, BattlePostRollAction(b, fh, oh, fe + 1, oe, full))
                        
                    if (a == b.defender && a.has(Sappers))
                        actions :+= SappersAction(a, BattlePostRollAction(b, fh, oh, fe, oe + 1, full))
                        
                    if (a.can(Partisans(b.clearing.suit)))
                        actions :+= PartisansAction(a, b.clearing.suit, a.hand.%!(_.suit.m(b.clearing.suit)), BattlePostRollAction(b, fh, oh, fe + (a == b.attacker).??(1), oe + (a == b.defender).??(1), full))
                        
                    a match {
                        case a : Hero if a.can(SwiftStrike) && a.ready(Sword) > 0 =>
                            actions :+= SwiftStrikeAction(a, BattlePostRollAction(b, fh, oh, fe + (a == b.attacker).??(1), oe + (a == b.defender).??(1), full))
                        case _ =>
                    }
                    
                    Ask(a, actions.done(BattlePostRollAction(b, fh, oh, fe, oe, ask.drop(1))))
                }
    
    
            case ArmorersAction(f, then) =>
                f.effects :-= Armorers
                f.stuck --> f.stuck.%({
                    case CraftEffectCard(_, _, _, Armorers) => true
                    case _ => false
                }) --> discard
                f.log("used", Armorers, "to cancel rolled", "Hits".styled(styles.hit))
                then
                    
            case SappersAction(f, then) =>
                f.effects :-= Sappers
                f.stuck --> f.stuck.%({
                    case CraftEffectCard(_, _, _, Sappers) => true
                    case _ => false
                }) --> discard
                f.log("used", Sappers, "to deal an extra", 1.hit)
                then

            case PartisansAction(f, s, l, then) =>
                f.used :+= Partisans(s)

                f.hand --> l --> discard(f).quiet

                f.log("enlisted", Partisans(s), "for an extra", 1.hit, l.any.??("and discarded", l))

                then

            case SwiftStrikeAction(f, then) =>
                f.exhaust(Sword)
                f.log("made a", SwiftStrike.of(f), "dealing an extra", 1.hit, "for", Sword.exhaust)
                then                                                      
                    
            case BrutalTacticsAction(f, o, then) =>
                f.used :+= BrutalTactics
                f.log("employed", BrutalTactics, "to deal an extra", 1.hit)
                o.oscore(1)("from", BrutalTactics)
                then
                    
            case BattleResolveHitsAction(b, fh, oh) =>
                if (fh > 0)
                    BattleAssignHitsAction(b.defender, b, fh, BattleResolveHitsAction(b, 0, oh))
                else
                if (oh > 0)
                    BattleAssignHitsAction(b.attacker, b, oh, BattleResolveHitsAction(b, fh, 0))
                else
                    BattleCleanupAction(b)
    
            case BattleAssignHitsAction(_, _, 0, then) =>
                then

            case BattleAssignHitsAction(f, b, n, then) if f.can(Armored) && f.at(b.clearing).warrior.any =>
                f.used :+= Armored
                
                f.log("ignored the first hit being", Armored)
                
                BattleAssignHitsAction(f, b, n - 1, then)
                
            case BattleAssignHitsAction(f, b, n, then) if f.can(DevoutKnights) && f.at(b.clearing).warrior.any && f.at(b.clearing)./~(_.piece.relic).any =>
                f.used :+= DevoutKnights
                
                f.log("ignored the first hit being", DevoutKnights)
                
                BattleAssignHitsAction(f, b, n - 1, then)
                
                
            case BattleAssignHitsAction(f, b, n, then) if n > 0 =>
                val all = f.at(b.clearing)
                val own = all.warrior.sortBy(_.priority)
                val merc = ((f == b.attacker).??(b.ally./~(_.at(b.clearing).warrior)) ++ (f == b.defender).??(b.codef./~(_.at(b.clearing).warrior))).sortBy(_.priority)
                val rear = all.diff(own)

                val ww = own ++ merc
                val wo = rear.none || ww.num >= n
                val nn = min(n, ww.num + rear.num)

                SelectFiguresAction(f, "Remove " ~ nn.ofb(wo.?("warrior").|("piece")), ww ++ rear, Nil)(_.num(nn).all(l => l.intersect(rear).none || ww.diff(l).none).all(l => l.intersect(own).num >= l.intersect(merc).num || own.diff(l).none).all(l => l.intersect(merc).num + 1 >= l.intersect(own).num
                 || merc.diff(l).none))(l => BattleDealHitsAction(f, b, l, then))
                
            case BattleDealHitsAction(f, b, Nil, then) =>
                then

            case BattleDealHitsAction(f, b, p :: l, then) =>
                Force(BattleDealHitAction(f, b, p, 0, BattleDealHitsAction(f, b, l, then)))
                
            case BattleDealHitAction(f, b, x : Figure, _, then) =>
                val a = x.faction
                val p = x.piece
                
                x --> a.limbo
                
                val o = (f == b.defender).?(b.attacker).|(b.defender)
                
                BattlePostHitInAction(b, o, a, p, BattlePostHitOutAction(b, o, a, p, ForcedRemoveAction(b.instigator.|(o), b.clearing, a, p, p.scoring.any.??(1), Removing, then)))
  
            case BattlePostHitInAction(b, e, f, p, then) =>
                e.log("HIT IN", p.name.styled(f))

                then

            case BattlePostHitOutAction(b, e, f, p, then) =>
                then
                
            case BattleCleanupAction(b) =>
                if (b.ally.any)
                    BattleCleanupAllyAction(b, b.ally.get)
                else
                    BattleCleanupDefenderAction(b, b.defender)

            case BattleCleanupAllyAction(b, f) =>
                BattleCleanupDefenderAction(b, b.defender)

            case BattleCleanupDefenderAction(b, f) =>
                BattleCleanupAttackerAction(b, b.attacker)
            
            case BattleCleanupAttackerAction(b, f) =>
                BattleForcedRemoveAction(b)
                
            case BattleForcedRemoveAction(b) =>
                var q : ForcedAction = BattleResolvedAction(b)
                b.parties.foreach { f =>
                    q = ForcedRemoveCleanupAction(f, q)
                }
                q

            case BattleResolvedAction(b) =>
                BattleFinishedAction(b)

            case BattleFinishedAction(b) =>
                factions.foreach { f =>
                    f.used = f.used.%{
                        case e : BattleEffect => false
                        case _ => true
                    }
                    f.ignored = f.ignored.%{
                        case e : BattleEffect => false
                        case _ => true
                    }
                }

                log("Battle ended in", b.clearing)
                log(SingleLine)

                b.then

            case WageWarAction(f, n, total, then) =>
                val cc = clearings.%(c => attack(f)(c).any)
                if (n > total || cc.none)
                    then
                else
                    BattleInitAction(f, (total > 1).?(WageWar(n, total)).|(NoMessage), cc, $((n == 1).?(CancelAction).|(then.as("Forfeit " ~ (total - n + 1).of("battle")))), WageWarAction(f, n + 1, total, then))
                
                
            // MOVE
            case MoveInitAction(f, ttt, m, from, allfrom, extra, then, self) =>
                val plans = movePlans(f, from.%(o => snaredFor(f)(o).not), ttt.some.|(transports./($(_)) ** f.transports))
                
                Ask(self.|(f))(allfrom./(o => plans.get(o) @@ {
                    case Some((t, l)) => MoveFromAction(self.|(f), f, t, m, o, l, then)
                    case None => MoveFromAction(self.|(f), f, Nil, m, o, Nil, then).x(snaredFor(f)(o), "snared").x(true)
                }))(extra)
                
            case MoveFromAction(self, f, tt, m, from, to, then) =>
                Ask(self)(to./(d => MoveToAction(self, f, tt.%(_.forall(_.allows(game, f, from, d))), m, from, d, then).x(tt.exists(_.forall(_.allows(game, f, from, d))).not, "disallow"))).cancel
                                                                                                                                                                                    
            case MoveToAction(self, f, tt, m, from : Region, to : Region, then) =>
                val mv = f.at(from)./~(_.piece.movable)
                val cm = 1.to(mv.num)./~(n => mv.combinations(n))
                val pp = tt./~(t => cm.%(l => t.forall(_.allows(game, f, from, to, l)))./(l => l.sortBy(m => t./(_.sortBy(m)).sum))./(Party(t, _))).sortBy(p => p.transport./(t => t.order(p.movable)).sum)
                
                Ask(f)(pp./(p => MoveListAction(f, p.transport, m, from, to, p.movable, then))).ocancel
                
            case MoveListAction(f, t, m, from : Clearing, to : Clearing, l, then) if t.has(Ferry) =>
                f.log("moved", Ferry.elem ~ ",", l./(_.of(f)).comma, "from", from, "to", to, m)

                Force(MoveListAction(f, Nil, NoMessage, from, to, l, FerryAction(f, from, to, then)))
                
            case MoveListAction(f, t, m, from : Region, to : Region, l, then) if t.any =>
                f.log("moved", l./(_.of(f)).comma, "from", from, "to", to, m)

                Force(MoveListAction(f, Nil, NoMessage, from, to, l, then))
                
            case MoveListAction(f, Nil, m, from : Region, to : Region, l, then) =>
                highlights :+= MoveHighlight(from, to)

                l.foreach(p => f.at(from).one(p) --> to)
                
                MoveWarriorsCompleteAction(f, from, to, then)
                
            case FerryAction(f, from, to, then) =>
                f.used :+= Ferry

                ferry :-= from
                ferry :+= to
                
                DrawCardsAction(f, 1, WithEffect(Ferry), AddCardsAction(f, then))

            case MoveWarriorsCompleteAction(f, from, to, then) =>
                onMoveTo(f, to, MoveFinishedAction(f, from, to, then))
                
            case MoveFinishedAction(f, from, to, then) =>
                then

            case MarchAction(f, n, total, then) =>
                val cc = moveFromR(f)
                if (n > total || cc.none)
                    then
                else
                    MoveInitAction(f, Nil, (total > 1).?(March(n, total)).|(NoMessage), cc, movable(f), $((n == 1).?(CancelAction).|(then.as("Forfeit " ~ (total - n + 1).of("move")))), MarchAction(f, n + 1, total, then))
            
    
            // TOP LEVEL
            case a : TopLevelAction =>
                a.next
                
            // GAME OVER
            case GameOverTopAction =>
                GameOver(Nil, Empty, Nil) 

            case GameOverDoneAction(f, then) =>
                then
                
            case GameOverMapsAction(f) =>
                Ask(f, GameOverMoveMapAction(f, ui.movements) :: GameOverBattleMapAction(f, ui.battles) :: GameOverNukeMapAction(f, ui.nukes) :: GameOverGraveyardAction(f, ui.graveyard) :: GameOverDoneAction(f, GameOverTopAction))
                
            case GameOverMoveMapAction(f, _) =>
                ui.movements = ui.movements.not

                Force(GameOverMapsAction(f))

            case GameOverBattleMapAction(f, _) =>
                ui.battles = ui.battles.not

                Force(GameOverMapsAction(f))

            case GameOverNukeMapAction(f, _) =>
                ui.nukes = ui.nukes.not

                Force(GameOverMapsAction(f))

            case GameOverGraveyardAction(f, _) =>
                ui.graveyard = ui.graveyard.not

                Force(GameOverMapsAction(f))

            case a : SelfPerform =>
                a.perform(this)
        }
    }        

    def cloned() : Game = {
        val g = new Game(players, setup, false, options)
        g.pstates = pstates.keys./(f => f -> pstates(f).cloned()).toMap
        g.ruins = ruins
        g.quests = quests
        g.deck = deck
        g.pile = pile
        g.dominances = dominances
        g.uncrafted = uncrafted
        g.turn = turn
        g
    }

    def cleanFor(o : Faction) : Game = {
        def q(before : => Unit) = () => before

        val g = cloned()

        g
    }

    def oldClearingsSort(l : List[Clearing]) = l.sortBy {
            case AutumnBoard.Mountain => 20
            case AutumnBoard.Weald => 80
            case AutumnBoard.Pond => 50

            case AutumnBoard.Glade => 200
            case AutumnBoard.Waterfall => 500
            case AutumnBoard.Creek => 800
            case _ => 0
        }
        
    def newClearingSort(l : List[Clearing]) = l.sortBy(board.clearings.indexOf)
}
