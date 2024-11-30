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

import hrf.tracker.{ValueTracker, Location, AnonymousLocation}
import hrf.tracker3.IdentityTracker

import hrf.elem._
import root.elem._

sealed trait Player extends BasePlayer with Elementary {
    def name : String
}

case class PlayerN(n : Int) extends Player {
    def short = "P" + n
    def name = "Player #" + n
    def elem = ("Player #" + n).hh
}


sealed trait AbstractSuit extends Record {
    def name = toString

    def elem : Elem
}

object AbstractSuit {
    implicit class AbstractSuitEx(l : $[AbstractSuit]) {
        def ss = l./({
            case AnySuit => "A".hl
            case Frog => "G".styled(Frog)
            case s : Suit => s.name.take(1).styled(s)
        }).merge
        def us = l./(_.name.take(1)).join("")
    }
}

sealed trait SuitCost extends AbstractSuit {
    def matched(source : SuitAsset) = source.matches(this)
    def toList : $[Suit]
}

sealed trait SuitAsset extends AbstractSuit {
    def matches(target : SuitCost) : Boolean = this @@ {
        case _ if target == NoSuit => false
        case _ if target == AnySuit => true
        case s if target == s => true
        case Bird => true

        case OneOf(l) => l.exists(_.matches(target))
        case s => target @@ {
            case AnyOf(l) if l.exists(matches) => true
            case _ => false
        }
    }
}

case object NoSuit extends SuitCost {
    def elem = "No Suit".hl
    def toList = $
}

case object AnySuit extends SuitCost {
    def elem = "Any".hl
    def toList = $(Fox, Rabbit, Mouse, Bird, Frog)
}

case class AnyOf(suits : $[BaseSuit]) extends SuitCost {
    def elem = suits./(_.elem).spaceCommaOr
    def toList = suits
}

object AnyOf {
    def apply(suits : $[BaseSuit]) : SuitCost = suits @@ {
        case List(s) => s
        case l => new AnyOf(l)
    }
}

case class OneOf(suits : $[BaseSuit]) extends SuitAsset {
    def elem = suits./(_.elem).spaceCommaOr
}

object OneOf {
    def apply(suits : $[BaseSuit]) : SuitAsset = suits @@ {
        case List(s) => s
        case l => new OneOf(l)
    }
}


sealed trait Suit extends SuitCost with SuitAsset with Named with Styling {
    def toList = $(this)
}

case object Bird extends Suit
trait BaseSuit extends Suit
case object Fox extends BaseSuit
case object Rabbit extends BaseSuit
case object Mouse extends BaseSuit

trait Region extends GameElementary {
    def name = id
    def id = hrf.reflect.className(this)
    override def toString = id
}

trait Suitable

abstract class Clearing(val capacity : Int) extends Region with Suitable with Rulable {
    def elem(implicit game : Game) = game.mapping.get(this)./(s => s.single./(s => name.styled(s)).|(name.suited(s))).|(name.hl)
}

abstract class Forest(val label : |[String]) extends Region {
    def elem(implicit game : Game) = board.forestName(this)
}

abstract class NamedForest(label : String) extends Forest(Some(label))

abstract class UnnamedForest extends Forest(None)


trait Rulable { self : Region => }

trait ExtraRegion extends Region

trait SpecialRegion extends Region {
    def elem(implicit game : Game) : Elem = "[[" + this.name + "]]"
}

case object Pool extends SpecialRegion
case class Limbo(c : Clearing) extends SpecialRegion

case class RedirectRegion(redirect : Figure => (Faction, Region)) extends SpecialRegion


trait Board {
    val id : String
    val name : String
    def clearings : $[Clearing]
    def forests : $[Forest]
    val ruins : $[Clearing]
    val diagonals : $[(Clearing, Clearing)]
    val inner : $[Clearing]
    def corners = diagonals.lefts ++ diagonals.rights
    val rubble : $[(Clearing, Clearing)] = $
    val blizzard : $[(Clearing, Clearing)] = $

    val ferry : $[Clearing] = $
    val tower : $[Clearing] = $

    def opposite(r : Clearing) = diagonals./~{
        case (a, b) if a == r => Some(b)
        case (a, b) if b == r => Some(a)
        case _ => None
    }.single

    lazy val regions = clearings ++ forests

    def connected(c : Clearing) : $[Clearing]
    def fromForest(f : Forest) : $[Clearing]
    def byRiver(c : Clearing) : $[Clearing]
    def forestsConnected(o : Forest, d : Forest) : Boolean = fromForest(o).intersect(fromForest(d)).num > 1

    lazy val distance = clearings./(a => a -> clearings./(b => b -> {
        var n = 0
        var l = $(a)
        while (l.has(b).not) {
            n += 1
            l = l./~(connected)
        }
        n
    }).toMap).toMap

    def forestName(f : Forest, from : Option[Region] = None)(implicit game : Game) : Elem = from @@ {
        case _ if f.label.any   => f.label./(_.styled(styles.forest)).??
        case Some(r : Forest)   => game.desc("Forest".styled(styles.forest), "near", fromForest(f).diff(fromForest(r))./(_.elem).commaAnd)
        case Some(r : Clearing) => game.desc("Forest".styled(styles.forest), "near", fromForest(f).but(r)./(_.elem).commaAnd)
        case None               => game.desc("Forest".styled(styles.forest), "between", fromForest(f)./(_.elem).commaAnd)
    }

    def center(r : Region) : (Int, Int)

    def dir(from : Region, to : Region) = {
        val (ax, ay) = center(from)
        val (bx, by) = center(to)
        (((180 - math.atan2(bx - ax, by - ay) / math.Pi * 180) / 15).round * 15).toInt % 360
    }

    def port(r : Region)(flooded : $[Clearing]) : (Int, Int) = (r @@ {
        case _ if false => (0, 0); throw new Error("no port for " + r)
        case x => center(x)
    })


    lazy val slots : Map[Region, $[(Int, Int)]] = {
        clearings./{ c =>
            var l = gates(c)

            val (cx, cy) = center(c)
            var e = gates(c).take(0)

            var d = 120.0

            while (l.num + e.num < 10) {
                val x = cx + randomInRange(-180, +180)
                val y = cy + randomInRange(-180, +140)

                if ((l ++ e).all { case (ox, oy) => (ox - x).abs > d || (oy - y).abs > d }) {
                    e :+= (x, y)
                    d = 120.0
                }
                else
                    d -= 0.1
            }

            c -> (l ++ e.sortBy { case (x, y) => (cx - x).abs + (cy - y).abs })
        }.toMap
    }

    def gates(r : Region) : $[(Int, Int)] = r @@ {
        case c : Clearing =>
            val (x, y) = center(c)

            0.until(c.capacity)./(n => (x - 60 * (c.capacity - 1) + 120 * n, y))

        case _ => $()
    }

    def mid(a : Region, b : Region) : (Int, Int) = {
        val (x1, y1) = center(a)
        val (x2, y2) = center(b)

        ((x1 + x2) / 2, (y1 + y2) / 2)
    }

}

trait Effect extends Record

trait FactionEffect extends Effect {
    val name : String
    def of(f : Faction) = name.styled(f)
}

trait HiddenEffect extends Effect {
    val name = "{{" + toString + "}}"
}

trait DisplayEffect extends Effect with Elementary {
    val name : String
    override lazy val elem : Elem = name.hl
}


trait Phase extends Elementary with Record {
    def elem = toString.styled(styles.phase)
}

case object Birdsong extends Phase
case object Daylight extends Phase
case object Evening extends Phase
case object Night extends Phase


trait Faction extends Player with Elementary with Styling { self =>
    val name : String
    val short : String
    val style : String

    def advertising : Elem
    def motto : Elem
    def clashKey : Faction

    def pieces(options : $[Meta.O]) : $[Piece]
    def abilities(options : $[Meta.O]) : $[Effect]

    val transports : $[$[Transport]] = $($(RuledMove))

    def funName : Elem = NameReference(name, self)

    def note : Elem = Empty

    val priority : String

    lazy val ss : Elem = short.styled(self)

    def getElem : Elem = name.styled(self).styled(styles.condensed, styles.italic)

    lazy val elem : Elem = getElem

    lazy val title : Elem = name.styled(self).styled(xstyles.bold)

    lazy val birdsong0 = BirdsongNAction(0, this)
}

trait WarriorFaction extends Faction with Fund {
    def warrior : Warrior

    def initialDamagePriorities(options : $[Meta.O]) : $[Piece] = $
}

object Neutral extends WarriorFaction {
    def abilities(options : $[Meta.O]) = $
    def pieces(options : $[Meta.O]) = $

    def advertising = "Neutral"
    def clashKey = this
    def motto = "No Motto"
    val name = "Neutral"
    val priority = ""
    val short = ""
    val style = ""

    def warrior = Heart
}

sealed trait Piece extends Record {
    def id = toString
    def name = id
    def plural = name + "s"
    def of(f : Faction) : Elem = name.styled(f)
    def ofg(f : Faction)(implicit game : Game) : Elem = of(f)

    def sof(f : Faction) = plural.styled(f)
    def nof(n : Int)(f : Faction) = (n == 1).?(name).|(plural).styled(f)

    def imgid(f : Faction) = f.style + "-" + id

    def iem(f : Faction) = this match {
        case _ : Warrior => f.style + "-" + id + "-empty"
        case _ : Building => "empty-building"
        case _ : Token => "empty-token"
        case Vagabond => f.style + "-vagabond"
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

    def priority = this match {
        case _ : Pawn => 5
        case _ : Tenacious => 4
        case _ : Warrior => 3
        case _ : Token => 2
        case _ : Building => 1
    }
}

case class Figure(faction : Faction, piece : Piece, index : Int) extends HitTarget with GameElementary {
    def elem(implicit game : Game) = piece.of(faction)
    def imgid = piece.imgid(faction)
}

object Figure {
    def generate(faction : Faction, pieces : $[Piece]) : $[Figure] = pieces.distinct./~(p => 1.to(pieces.count(p))./(Figure(faction, p, _)))

    def fromTuple(x : (Faction, Piece)) = Figure(x._1, x._2, -1)

    implicit class FiguresEx[T](val t : T)(implicit val conv : T => $[Figure]) {
        def pieces = conv(t)./(_.piece)
    }
}

trait Movable extends Piece
trait Attacking extends Piece
trait Scoring extends Piece
trait Tenacious extends Piece

trait Token extends Piece with Scoring
trait Building extends Piece with Scoring

trait Warrior extends Piece with Movable with Attacking {
    override def img(f : Faction) : Elem = Image(imgid(f), styles.warrior)
}

trait Pawn extends Piece {
    override def img(f : Faction) : Elem = Image(imgid(f), styles.warrior)
}

case class Ruins(items : $[Item]) extends Building with Tenacious {
    override val name = "Ruins"
}

case object Tower extends Pawn with Tenacious
case object LostCity extends Pawn with Tenacious



trait FactionState {
    implicit val game : Game
    def faction : Faction

    val core = faction.abilities(options)

    var damagePriorities = faction.as[WarriorFaction]./~(_.initialDamagePriorities(options))

    def has(e : Effect) = core.contains(e) || effects.contains(e) || transient.contains(e) || services.contains(e)
    def can(e : Effect) = has(e) && !used.contains(e) && !ignored.contains(e)

    def cards(name : String, init : $[DeckCard] = $, rule : DeckCard => Boolean = (e : DeckCard) => true) = game.cards.another[DeckCard](name + "-" + faction.short, init, rule)
    def location(r : Region, rule : Figure => Boolean = _.faction == faction, content : $[Figure] = $) = game.pieces.register(faction -> r, rule, content)

    board.clearings.foreach(c => location(c))
    board.clearings.foreach(c => location(Limbo(c)))

    def transient : $[Effect] = $

    var effects : $[Effect] = $
    var used : $[Effect] = $
    var ignored : $[Effect] = $

    var services : $[Effect] = $

    var vp : Int = 0
    var scorelog : $[Elem] = $


    val hand = cards("hand")
    val stuck = cards("stuck")
    val drawn = cards("drawn")
    var dominance : |[Dominance] = None
    var coalition : |[Faction] = None
    var contracts : $[Hireling] = $

    var forTrade : $[ItemRef] = $

    def totalWar = dominance.any && options.has(TotalWarDominance)

    var attacked : $[Faction] = $

    var crafted : $[SuitAsset] = $

    def craft : $[SuitAsset]

    def frogCraft : $[SuitAsset] =
        factions.but(faction).of[InvasiveDDD]./~(e => effects.of[SuitLaborers].%(_.faction == e)./(_.suit)./~(s => e.all(PeacefulDDD(s))))./(_.asset) ++
        factions.but(faction).of[InvasiveCCC]./~(e => effects.of[Laborers].%(_.faction == e)./~(_ => FoxRabbitMouse./~(s => e.all(PeacefulCCC(s)))))./(_.asset) ++
        factions.but(faction).of[InvasiveBBB]./~(e => effects.of[Integration].%(_.faction == e)./(_.suit)./~(s => (e.all(PeacefulBBB(s)) ++ e.all(MilitantBBB(s)))))./(_.asset)

    var extraCraft : $[SuitAsset] = $

    val reserve = location(Pool, content = Figure.generate(faction, faction.pieces(options)))


    def limbo(c : Clearing) = faction -> Limbo(c)


    def from(r : Region) = faction -> r

    def fromQ(r : Region) = game.pieces.has(faction -> r).??(game.pieces.get(faction -> r))

    def present(r : Region) : Boolean = from(r).any

    def presence : $[Region] = clearings.%(present)

    def at(r : Region) = from(r).$./(_.piece)

    def pooled(p : Piece) = at(Pool).count(p)

    def pool(p : Piece) = at(Pool).has(p)

    def all(p : Piece) = clearings./~(c => at(c).count(p).times(c))

    def hasOnMap(p : Piece) = clearings.exists(c => from(c).$.exists(_.piece == p))


}

case object MorningCraft extends HiddenEffect
case object DaylightCraft extends HiddenEffect
case object EveningCraft extends HiddenEffect


case object SuppressLog extends Message {
    def elem(implicit game : Game) = Empty
}

case object Removing extends Message {
    def elem(implicit game : Game) = "removing"
}

trait ForfeitMessage extends Message {
    def real : Boolean
}

case class ForfeitActions(n : Int) extends ForfeitMessage {
    def elem(implicit game : Game) = "Forfeit " ~ n.hl ~ " action" ~ (n != 1).??("s") ~ "?"
    def real = n > 0
}

case class DrewFromTheDeck(f : Faction, m : Message with GameElementary) extends Message {
    def elem(implicit game : Game) = if (m == NoMessage) f.elem ~ " drew from the deck" else f.elem ~ " drew from the deck " ~ m.elem
}

case class StoleFrom(f : Faction, e : Faction) extends Message {
    def elem(implicit game : Game) = f.elem ~ " stole from " ~ e.elem
}

case class GaveBack(f : Faction, e : Faction) extends Message {
    def elem(implicit game : Game) = f.elem ~ " gave back " ~ e.elem
}

case class Gave(f : Faction, e : Faction) extends Message {
    def elem(implicit game : Game) = f.elem ~ " gave " ~ e.elem
}

case class PreBreak(m : Message with GameElementary) extends Message {
    def elem(implicit game : Game) = Break ~ m.elem
}

case object OwnHand extends Message {
    def elem(implicit game : Game) = "Hand"
}

case class FactionHand(f : Faction) extends Message {
    def elem(implicit game : Game) = f.elem ~ " hand"
}

case class FactionSupporters(f : Insurgent) extends Message {
    def elem(implicit game : Game) = Supporters.of(f)
}

case object DiscardPile extends Message {
    def elem(implicit game : Game) = "Discard pile"
}

case class IofN(m : Message, i : Int, n : Int) extends Message {
    def elem(implicit game : Game) = m.elem ~ (n > 1).?(" " ~ "(".hl ~ i.hlb ~ "/".hl ~ n.elem ~ ")".hl)
}

case class TakeCard(d : DeckCard) extends Message {
    def elem(implicit game : Game) = " to take " ~ d.elem
}

case class ToReplace(c : Clearing, e : Faction, p : Piece) extends Message {
    def elem(implicit game : Game) = " to replace " ~ p.of(e) ~ " in " ~ c.elem
}

case class WageWar(i : Int, n : Int) extends Message {
    def elem(implicit game : Game) = ("(".hl ~ "Wage War " ~ i.hlb ~ "/".hl ~ n.elem ~ ")".hl).styled(xlo.pre)
}

case class March(i : Int, n : Int) extends Message {
    def elem(implicit game : Game) = ("(".hl ~ "March " ~ i.hlb ~ "/".hl ~ n.elem ~ ")".hl).styled(xlo.pre)
}

case class ToClearPath(a : Clearing, b : Clearing) extends Message {
    def elem(implicit game : Game) = "to clear path between " ~ a.elem ~ " and " ~ b.elem
}


case class WithEffect(e : DisplayEffect) extends Message {
    def elem(implicit game : Game) = " with " ~ e.elem
}

case class AndDiscarded(e : DisplayEffect) extends Message {
    def elem(implicit game : Game) = " and discarded " ~ e.elem
}

case class WithCard(d : Card) extends Message {
    def elem(implicit game : Game) = " with " ~ d.elem
}

case object OnBalloon extends Message {
    def elem(implicit game : Game) = " on " ~ "Balloon".hl
}

case class InClearing(c : Clearing) extends Message {
    def elem(implicit game : Game) = " in " ~ c.elem
}

case class OfFaction(f : Faction) extends Message {
    def elem(implicit game : Game) = " of " ~ f.elem
}

case class OnTurn(n : Int) extends Message {
    def elem(implicit game : Game) = " on turn" ~ SpaceSpan ~ ("#" + n).hl
}

case object OnSetup extends Message {
    def elem(implicit game : Game) = " on setup"
}


trait ViewCard extends ViewObject[Card] { self : UserAction =>
    val d : Card
    def obj = d
}

trait ViewQuest extends ViewObject[Quest] { self : UserAction =>
    val q : Quest
    def obj = q
}

trait ViewLeader extends ViewObject[Leader] { self : UserAction =>
    val l : Leader
    def obj = l
}

trait ViewCharacter extends ViewObject[Character] { self : UserAction =>
    val c : Character
    def obj = c
}

trait ViewSpell extends ViewObject[Spell] { self : UserAction =>
    val s : Spell
    def obj = s
}


case object NoDrawnFactions extends HiddenInfo
case object NoHand extends HiddenInfo
case object NoPlots extends HiddenInfo
case object NoSupporters extends HiddenInfo
case object HiSupporters extends HiddenInfo

case object HiddenAmbush extends HiddenInfo
case object HiddenAssignHits extends HiddenInfo
case object HiddenFieldHospital extends HiddenInfo
case class HiddenClearing(clearing : Clearing) extends HiddenInfo

case object HiddenCancel extends Hidden with Cancel

trait FactionAction {
    val self : Faction
}

case object DummyAction extends ForcedAction

trait BirdsongQuestion extends FactionAction {
    def question(implicit game : Game) = self.elem ~ " (" ~ Birdsong.elem ~ ")"
}



















case class SelectFiguresAction(f : Faction, m : Elem, figures : $[Figure], extra : $[UserAction])(r : ObjectSetRule[Figure] => ObjectSetRule[Figure])(then : $[Figure] => ForcedAction) extends ForcedAction with Soft with SelfPerform {
    def perform(soft : Void)(implicit game : Game) = {
        implicit val convF = (x : Figure, s : Boolean) => FigureRemove(x).elem(s)(styles.iii)

        XXSelectObjectsAction(f, figures, true)
            .withGroup(m)
            .withSplit($(figures.num))
            .withBreak({
                case 0 => Empty
                case _ => HorizontalBreak
            })
            .withRule(rule => r(rule.each(_ => true)))

            .withThen(l => then(l))(l => game.desc("Remove", l./(u => u.piece.ofg(u.faction)).commaAnd))
            .withExtra(extra)
            .ask
    }
}

trait Highlight
case class PlaceHighlight(l : $[Clearing]) extends Highlight
case class BattleHighlight(c : Clearing) extends Highlight
case class NukeHighlight(l : $[Clearing]) extends Highlight
case class MoveHighlight(from : Region, to : Region) extends Highlight
case object NothingHighlight extends Highlight


trait Reaction extends Record {
    val name : String
}

case object GoodGame extends Reaction {
    val name = "Good Game" // VB
}

case object IHateYouAll extends Reaction {
    val name = "I Hate You All" // NB
}

case object Crivens extends Reaction {
    val name = "Crivens!" // LH
}

case object WellBeBack extends Reaction {
    val name = "We'll Be Back" // ED
}

case object NoDiscount extends Reaction {
    val name = "No Quarter, No Discount" // RF
}

case object DialingCthulhu extends Reaction {
    val name = "Dialing Cthulhu Octopuss" // WA
}

case object Hallelujah extends Reaction {
    val name = "Hallelujah!" // LC
}

case object FlipBombs extends Reaction {
    val name = "Flip Bombs, Not Tables" // CC
}

case object AbandonShip extends Reaction {
    val name = "Abandon Ship!" // pirate voice
}

case object FriendshipIsMagic extends Reaction {
    val name = "Friendship Is Magic" // cute voice
}

case object OneMoreTurn extends Reaction {
    val name = "One More Turn!!!" // 4X
}

case object IDontEvenWannaKnow extends Reaction {
    val name = "I Don't Even Wanna Know" // Daria
}


trait Expansion {
    val before : $[Expansion] = $
    val after : $[Expansion] = $
    def active(factions : $[Faction], options : $[Meta.O]) : Boolean
    def perform(action : Action, soft : Void)(implicit game : Game) : Continue
    def extraMoveFrom(f : Faction)(implicit game : Game) : $[Region] = $
    def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {}
    def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {}
    def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {}

    implicit class ActionMatch(val a : Action) {
        def @@(t : Action => Continue) = t(a)
        def @@(t : Action => Boolean) = t(a)
    }
}

abstract class FactionExpansion[F : ClassTag] extends Expansion {
    def active(factions : $[Faction], options : $[Meta.O]) = factions.of[F].any
}

abstract class MandatoryExpansion extends Expansion {
    def active(factions : $[Faction], options : $[Meta.O]) = true
}

case class SeatingOrderAction(shuffled : $[Player]) extends ShuffledAction[Player]

case class FactionsCombinationAction(random : $[Faction]) extends RandomAction[$[Faction]]

case class CharactersCombinationAction(shuffled : $[Character]) extends ShuffledAction[Character]

trait GameImplicits {

    implicit def felineState(f : Feline)(implicit game : Game) = game.states(f).as[FelinePlayer].get
    implicit def aviaryState(f : Aviary)(implicit game : Game) = game.states(f).as[AviaryPlayer].get
    implicit def insurgentState(f : Insurgent)(implicit game : Game) = game.states(f).as[InsurgentPlayer].get
    implicit def heroState(f : Hero)(implicit game : Game) = game.states(f).as[HeroPlayer].get
    implicit def fanaticState(f : Fanatic)(implicit game : Game) = game.states(f).as[FanaticPlayer].get
    implicit def traderState(f : Trader)(implicit game : Game) = game.states(f).as[TraderPlayer].get
    implicit def undergroundState(f : Underground)(implicit game : Game) = game.states(f).as[UndergroundPlayer].get
    implicit def mischiefState(f : Mischief)(implicit game : Game) = game.states(f).as[MischiefPlayer].get
    implicit def expeditionState(f : Expedition)(implicit game : Game) = game.states(f).as[ExpeditionPlayer].get
    implicit def oldExpeditionState(f : OldExpedition)(implicit game : Game) = game.states(f).as[OldExpeditionPlayer].get
    implicit def hordeState(f : Horde)(implicit game : Game) = game.states(f).as[HordePlayer].get
    implicit def utopiaState(f : Utopia)(implicit game : Game) = game.states(f).as[UtopiaPlayer].get
    implicit def casterState(f : Caster)(implicit game : Game) = game.states(f).as[CasterPlayer].get
    implicit def farmerState(f : Farmer)(implicit game : Game) = game.states(f).as[FarmerPlayer].get
    implicit def invasiveDDDState(f : InvasiveDDD)(implicit game : Game) = game.states(f).as[InvasiveDDDPlayer].get
    implicit def invasiveCCCState(f : InvasiveCCC)(implicit game : Game) = game.states(f).as[InvasiveCCCPlayer].get
    implicit def invasiveBBBState(f : InvasiveBBB)(implicit game : Game) = game.states(f).as[InvasiveBBBPlayer].get
    implicit def invasiveAAAState(f : InvasiveAAA)(implicit game : Game) = game.states(f).as[InvasiveAAAPlayer].get
    implicit def legalAAAState(f : LegalAAA)(implicit game : Game) = game.states(f).as[LegalAAAPlayer].get
    implicit def abductorAAAState(f : AbductAAA)(implicit game : Game) = game.states(f).as[AbductAAAPlayer].get

    implicit def streetBandState(f : StreetBand.type)(implicit game : Game) = game.states(f).as[StreetBandState].get
    implicit def popularBandState(f : PopularBand.type)(implicit game : Game) = game.states(f).as[PopularBandState].get
    implicit def highwayBanditsState(f : HighwayBandits.type)(implicit game : Game) = game.states(f).as[HighwayBanditsState].get
    implicit def felinePhysiciansState(f : FelinePhysicians.type)(implicit game : Game) = game.states(f).as[FelinePhysiciansState].get
    implicit def moleArtisiansState(f : MoleArtisians.type)(implicit game : Game) = game.states(f).as[MoleArtisiansState].get
    implicit def forestPatrolState(f : ForestPatrol.type)(implicit game : Game) = game.states(f).as[ForestPatrolState].get
    implicit def theExiteState(f : TheExile.type)(implicit game : Game) = game.states(f).as[TheExileState].get

    implicit def factionState(f : Faction)(implicit game : Game) = game.states(f)

    val FoxRabbitMouse = $[BaseSuit](Fox, Rabbit, Mouse)

    val D4 = Die.range(0 -> 3)

    val BaseSuitDie = Die.custom(FoxRabbitMouse)
    val GoldenDie = Die.from(1, 1, 1, 2, 2, 2)
    val SilverDie = Die.from(2, 2, 3, 3, 3, 4)

    def log(s : Any*)(implicit game : Game) {
        game.log(s : _*)
    }

    val VP = (g : Game) => Text("undefined victory points") : Elem
    val AndScoredVP = (g : Game) => Text("undefined victory points scored") : Elem
    val ScoredVP = (g : Game) => Text("undefined victory points scored") : Elem
    val ForVP = (g : Game) => Text("undefined victory points for") : Elem

    implicit class PlayerEx(f : Player)(implicit game : Game) {
        def notify(l : $[Info])(implicit game : Game) {
            game.notify(f, l)
        }
    }

    implicit class FactionLogScore(f : Faction)(implicit game : Game) {
        def log(s : Any*) {
            if (game.logging)
                game.log((f +: s.$) : _*)
        }

        def logtemp(s : Any*) {
            if (game.logging)
                game.logtemp((f +: s.$) : _*)
        }

        def oscore(vp : Int)(reason : (Game => Elem)*) {
            nscore(vp)(reason : _*)()
        }

        def nscore(vp : Int)(reason : (Game => Elem)*)(logged : Any*) {
            if (f.is[Hireling])
                return

            val vpcond = f.dominance.none && f.coalition.none

            val real = (vpcond || game.options.has(TotalWarDominance)) && vp != 0

            if (real)
                f.vp = max(0, f.vp + vp)

            val message = if (logged.any) {
                if (logged.intersect($(VP, AndScoredVP, ScoredVP, ForVP)).none)
                    throw new Error("no vp in logged")

                logged.$./{
                    case VP => real.not.?(Empty).|(vp.abs.vp)
                    case AndScoredVP => real.not.?(Empty).|(("and scored " ~ vp.abs.vp).styled(xlo.pre))
                    case ScoredVP => real.not.?(Empty).|(("scored " ~ vp.abs.vp).styled(xlo.pre))
                    case ForVP => real.not.?(Empty).|(("for " ~ vp.abs.vp).styled(xlo.pre))
                    case e => e
                }.but(Empty)
            }
            else
                $(f, (vp > 0).?("scored").|("lost") ~ " ".pre ~ vp.abs.vp) ++ reason.$./(_(game))

            if (logged.any || real)
                game.log(message : _*)

            if (real) {
                val r = reason.$./(_(game)).join(SpaceSpan)

                f.scorelog :+= vp.vp ~ SpaceSpan ~ r ~ SpaceSpan ~ "(turn" ~ SpaceSpan ~ ("#" + game.turn).hl ~ ")"
            }
        }
    }

    implicit class ClearingSuit(c : Suitable)(implicit game : Game) {
        def suits = game.mapping(c)
        def cost : SuitCost = suits @@ {
            case List(s) => s
            case l => AnyOf(l)
        }
        def asset : SuitAsset = suits @@ {
            case List(s) => s
            case l => OneOf(l)
        }
    }

    implicit class ClearingSuits(l : $[Region]) {
        def comma : (Game => Elem) = (game : Game) => game.desc(l./(_.elem(game)).comma)
    }

    implicit class FactionEx(f : Faction)(implicit game : Game) {
        def validDest : $[Region] = (f @@ {
            case f : Hero => game.board.forests
            case f : AbductAAA => game.board.forests
            case f : TheExile.type => game.board.forests
            case f : Underground => $(f.burrow)
            case _ => $()
        } ++ clearings).diff(game.scorched).diff(game.flooded)

        def moveFrom : $[Region] = f.movePlans(f.movable.%(f.canMoveFrom), game.transports./($) ** f.transports).keys.$

        def movable : $[Region] = (clearings ++ game.extraMoveFrom(f)).%(c => f.at(c).of[Movable].any).distinct

        def movePlans(from : $[Region], transports : $[$[Transport]]) = {
            val ttt = transports.%(_.forall(_.allows(f)))
            val dest = f.validDest




            from./~{ o =>
                val tt = ttt.%(_.forall(_.allows(f, o)))
                val l = dest.but(o).%(d => tt.exists(_.forall(_.allows(f, o, d))))
                val t = tt.%(t => l.exists(d => t.forall(_.allows(f, o, d))))
                (t.any && l.any).?(o -> (t, l))
            }.toMap
        }

        def canMoveFrom(o : Region) = o @@ {
            case o : Clearing if factions.but(f).of[Mischief].%!(f.friends).%(e => e.at(o).has(Snare) && e.hidden.has(o).not).any => false
            case o : Clearing if hirelings.has(StreetBand) && f.has(StreetBand).not && StreetBand.enchanted.has(o) => false
            case _ => true
        }

        def connectedFor(o : Region) = {
            val tt = game.transports.but(Ferry).but(OffTrail).%(_.allows(f, o))
            f.validDest.but(o).%(d => tt.exists(_.allows(f, o, d)))
        }

        def validAttackTarget(c : Clearing)(e : Faction) : Boolean = {
            if (f == e)
                return false

            if (e == RiverfolkFlotilla)
                return false

            if (e == StoicProtector)
                return false

            if (f.is[LegalAAA] && (f.at(c).has(AssemblyAAA) || f.at(c).has(ConvenedAAA)))
                return false

            /*
            if (e == FuriousProtector)
                return false
            */

            if (f.friends(e))
                return false

            if (e.at(c).none)
                return false

            if (e == BanditGangs && BanditGangs.owner.?(_.present(c)))
                return false

            if (e.has(StoicProtector) && StoicProtector.present(c))
                return false

            e.as[Trader].foreach { e =>
                if (f.has(Mercenaries(e)) && f.hasAttackingForce(c, Some(e)).not)
                    return false
            }

            if (e == SpringUprising)
                if (f.hand.exists(d => d.suit.matches(c.cost)).not)
                    return false

            if (e.is[Mudmen])
                return false

            if (e.at(c).all(_ == LivingShield))
                return false

            true
        }

        def canAttackSelf(c : Clearing) : Boolean = {
            val l = f.at(c)

            if (l.none)
                return false



            l.of[Attacking].any
        }

        def canAttackFromForest(c : Clearing) : Boolean = {
            f.as[TheExile.type].exists { f =>
                (f.ready.any || f.exhausted.any) && board.fromForest(f.region).has(c)
            }
        }

        def canAttackWithMerc(c : Clearing)(s : Trader) : Boolean = {
            if (f == s)
                return false

            if (f.has(Mercenaries(s)).not)
                return false

            if (s.at(c).of[Warrior].none)
                return false

            true
        }

        def hasAttackingForce(c : Clearing, except : |[Trader] = None) = (
            f.canAttackSelf(c)
            || traders.but(except).exists(f.canAttackWithMerc(c))
            || f.as[Hero].?(f => factions.but(f).exists(f.canAttackWithAlly(c)))
            || canAttackFromForest(c)
        )

        def canAttack(c : Clearing)(e : Faction) = hasAttackingForce(c) && f.validAttackTarget(c)(e)

        def canAttackList(c : Clearing) = hasAttackingForce(c).??(f.enemies.%(f.validAttackTarget(c)))

        def canAttackIn(c : Clearing) = canAttackList(c).any

        def canRemove(c : Clearing)(e : Faction) : Boolean = {
            if (e == RiverfolkFlotilla)
                return false

            if (e == StoicProtector)
                return false

            if (e.is[Mudmen])
                return false

            if (e.has(StoicProtector) && StoicProtector.present(c))
                return false

            if (e.at(c).any && e.at(c).all(_ == LivingShield))
                return false

            if (e.as[Hero].?(_.inv.exists(_.damaged.not).not))
                return false

            true
        }

        def canBuild(c : Clearing) = f.canPlace(c) && game.freeSlots(c) >= 1

        def canPlace(c : Region) : Boolean = c @@ {
            case c : Clearing => canPlace(c)
            case c : Forest => f.is[CommonAbduct]
            case Burrow(faction) => faction == f
            case _ => false
        }

        def cantPlaceReason(c : Region) : Elem = {
            c.as[Clearing].foreach { c =>
                if (game.scorched.has(c))
                    return "Scorched Earth".styled(styles.hit)

                if (game.flooded.has(c))
                    return "Flood".styled(ED)

                factions.but(f).of[Feline].%(_.at(c).has(Keep)).foreach { f =>
                    return Keep.of(f)
                }

                factions.but(f).of[Mischief].%(e => e.at(c).has(Snare) && e.hidden.has(c).not).foreach { f =>
                    return Snare.of(f)
                }
            }

            "No Reason"
        }

        def canPlace(c : Clearing) : Boolean =
            game.scorched.has(c).not &&
            game.flooded.has(c).not &&
            factions.but(f).of[Feline].%(_.keep.has(c)).none &&
            factions.but(f).of[Mischief].%(e => e.at(c).has(Snare) && e.hidden.has(c).not).none

        def ruleValue(c : Region, mercenaries : $[Trader], friends : $[Faction]) : Int = {
            if (game.states.contains(f).not)
                return 0

            c.as[Burrow].foreach { c =>
                if (f.as[Underground].%(_.burrow == c).any)
                    return 999

                return 0
            }

            val sk = f.has(SoupKitchens)
            val bk = f.has(BorschtKitchens)
            val fv = f.has(ForestVigil)

            val own = f.at(c)./{
                case Garden(_) => 333
                case b : Building => 4
                case w : Warrior => 4
                case t : Token if sk => 4 + 4
                case _ => 0
            }.sum

            val merced = mercenaries./(_.at(c)./{
                case w : Building => 4
                case w : Warrior => 4
                case _ => 0
            }.sum).sum

            val hired = hirelings.%(f.has)./(_.at(c)./{
                case b : Building => 4
                case w : Warrior => 4
                case _ => 0
            }.sum).sum

            val friended = friends./(_.at(c)./{
                case b : Building => 4
                case w : Warrior => 4
                case _ => 0
            }.sum).sum

            val borscht = bk.??(c.as[Clearing]./(game.freeSlots).|(0) * 4)
            val vigil = fv.??((own > 0).??(4) + (factions ++ game.unhired).forall(_.present(c).not).??(333))

            val result = own + borscht + vigil + merced + hired + friended

            val breaker = result > 0 && (f.has(RuleIfTied) || f.has(BluebirdNobles))



            result + breaker.??(1)
        }


        def rules(c : Region) : Boolean = f @@ {
            case h : Hireling if factions.%(_.has(h)).%(f => f.rules(c)).any => true
            case f =>
                val mercs = traders.%(s => f.has(Mercenaries(s)) || f.has(Peacekeepers(s)))
                val friends = (game.current == f).??(factions).but(f).%(e => e.as[Farmer].?(_.foes.has(f).not) || f.as[Farmer].?(_.foes.has(e).not))

                f.ruleValue(c, mercs, friends) > (factions ++ game.unhired).but(f).diff(mercs).diff(friends)./(e => e.ruleValue(c, $, $)).maxOr(0)
        }

        def ruleSelf(c : Clearing) = f.ruleValue(c, $, $) > (factions ++ game.unhired).but(f)./(e => e.ruleValue(c, $, $)).maxOr(0)

        def removeStuckEffect(effect : Effect) {
            f.effects :-= effect

            if (effect == CoffinMakers)
                game.coffins --> game.pool

            if (effect == BreakingDawn)
                game.dawn --> game.recycle

            if (effect == HighNoon)
                game.noon --> game.recycle

            if (effect == DuskAwakening)
                game.dusk --> game.recycle

            f.stuck --> f.stuck.%({
                case CraftEffectCard(_, _, _, e) => e == effect
                case _ => false
            }) --> discard.quiet
        }

        def craftableWith(craft : $[SuitAsset], used : $[SuitAsset], limit : Int = 999) = (d : DeckCard) => d @@ {
            case d if d.suit == Frog && f.is[InvasiveDDD] => false
            case d : CraftItemCard if game.uncrafted.has(d.item).not => false
            case d : CraftEffectCard if f.has(d.effect) => false
            case d : CraftCard if d.cost.num > limit => false
            case d : CraftCard if (d.cost ++ used).num > craft.num => false
            case d : CraftCard =>
                val assets : $[SuitAsset] = craft.diff(used)
                val costs : $[SuitCost] = d.cost

                var aa : $[SuitAsset] = assets.diff(costs)
                var cc : $[SuitCost] = costs.diff(assets)

                aa.permutations.exists { aa => cc.lazyZip(aa).forall((c, a) => a.matches(c)) }

            case _ => false
        }

        def craftable = craftableWith(f.craft ++ f.frogCraft ++ f.extraCraft, f.crafted, f.as[Trader]./(_.funds.$.num).|(999))

    }

    implicit class HeroEx(f : Hero)(implicit game : Game) {
        def canAttackWithAlly(c : Clearing)(a : Faction) : Boolean = {
            if (f == a)
                return false

            if (f.is[Hero].not)
                return false

            if (f.at(c).none)
                return false

            if (a.at(c).of[Warrior].none)
                return false

            if (f.allied(a).not)
                return false

            true
        }
    }

    implicit class HirelingEx(h : Hireling)(implicit game : Game) {
        def owner : |[Faction] = game.factions.%(_.has(h)).single
    }


    implicit class LocationEx(location : (Faction, Region))(implicit game : Game) {
        def exists = game.pieces.has(location)
        def $ = game.pieces.get(location)
        def any = $.any
        def none = $.none
        def num = $.num
        def pieces = $./(_.piece)
        def has(p : Piece) = $.exists(_.piece == p)
        def count(p : Piece) = $.count(_.piece == p)
    }

    implicit def locationToFigures(location : (Faction, Region))(implicit game : Game) : $[Figure] = location.$

    implicit class FiguresEx[T](location : T)(implicit game : Game, toFigures : T => $[Figure]) {
        private def figures = toFigures(location)

        def --?>(p : Piece) = figures.%(_.piece == p).take(1)
        def --?>(l : $[Piece]) = {
            var rest = l
            figures./~{ u =>
                if (rest.contains(u.piece)) {
                    rest = rest :- (u.piece)
                    Some(u)
                }
                else
                    None
            }
        }

        def -->(p : Piece) = --?>(p).some.|!("one " + p + " not found among " + figures.mkString(" | "))
        def -->(l : $[Piece]) = l.any.??(--?>(l).some.%(_.num == l.num).|!("list " + l + " not found among " + figures.mkString(" | ")))

        def -->(r : RedirectRegion) = figures.foreach(u => game.pieces.move(u, r.redirect(u)))

        def -->(location : (Faction, Region)) = figures.foreach(u => game.pieces.move(u, location))

        def -->(c : Region) = figures.foreach(u => game.pieces.move(u, u.faction -> c))
    }

    implicit class FigureEx(u : Figure)(implicit game : Game) {
        def -->(r : RedirectRegion) = game.pieces.move(u, r.redirect(u))

        def -->(location : (Faction, Region)) = game.pieces.move(u, location)

        def -->(c : Region) = game.pieces.move(u, u.faction -> c)
    }


    trait DiscardInfo
    case class FactionDiscard(faction : Faction, message : $[Any]) extends DiscardInfo
    case object QuietDiscard extends DiscardInfo
    case object DirectDiscard extends DiscardInfo

    object discard {
        def apply(f : Faction) = FactionDiscard(f, Nil)
        def apply(f : Faction, m : Any*) = FactionDiscard(f, m.$)
        def quiet = QuietDiscard
        def direct = DirectDiscard
    }

    implicit class SourceEx(source : Location[Card])(implicit game : Game) {
        def -->(l : $[Card]) = new SourceCardsEx(source, l)
        def -->(d : Card) = new SourceCardEx(source, d)
        def -->(dest : Location[Card]) = new SourceCardsEx(source, game.cards.get[Card](source)) --> dest
        def -->(dest : FactionDiscard) = new SourceCardsEx(source, game.cards.get[Card](source)) --> dest
        def -->(dest : QuietDiscard.type) = new SourceCardsEx(source, game.cards.get[Card](source)) --> dest
        def -->(dest : DirectDiscard.type) = new SourceCardsEx(source, game.cards.get[Card](source)) --> dest
    }

    class SourceCardsEx(source : Location[Card], l : $[Card])(implicit game : Game) {
        def -->(dest : Location[Card]) {
            l.foreach(new SourceCardEx(source, _) --> dest)
        }

        def -->(dest : QuietDiscard.type) {
            l.foreach(new SourceCardEx(source, _) --> dest)
        }

        def -->(dest : DirectDiscard.type) {
            l.foreach(new SourceCardEx(source, _) --> dest)
        }

        def -->(dest : FactionDiscard) {
            dest.faction.log("discarded", l, dest.message)

            l.foreach(new SourceCardEx(source, _) --> discard.quiet)
        }
    }

    class SourceCardEx(source : Location[Card], d : Card)(implicit game : Game) {
        def -->(dest : Location[Card]) {
            game.cards.move(source, d, dest)
        }

        def -->(dest : DirectDiscard.type) {
            d @@ {
                case d : Dominance => source --> d --> game.dominances
                case d if d.suit == Frog => source --> d -->{
                    factions.of[InvasiveDDD].single./(_.deck) ||
                    factions.of[InvasiveCCC].single./(_.pile) ||
                    factions.of[InvasiveBBB].single./(_.pile) ||
                    factions.of[InvasiveAAA].single./(_.pile) |
                    (throw new Error("nowhere to discard a frog card"))
                }
                case d => source --> d --> game.pile
            }
        }

        def -->(dest : QuietDiscard.type) {
            val lsr = (factions.drop(1) ++ factions.take(1)).of[Fanatic].%(_.has(LostSouls))

            lsr.starting @@ {
                case Some(l) => source --> d --> l.lost
                case _ => source --> d --> discard.direct
            }
        }

        def -->(dest : FactionDiscard) {
            dest.faction.log("discarded", d, dest.message)

            -->(QuietDiscard)
        }
    }

    def players(implicit game : Game) : $[Player] = game.players
    def options(implicit game : Game) : $[Meta.O] = game.options
    def board(implicit game : Game) : Board = game.board
    def clearings(implicit game : Game) : $[Clearing] = game.clearings
    def deck(implicit game : Game) = game.deck
    def pile(implicit game : Game) = game.pile
    def factions(implicit game : Game) : $[Faction] = game.factions
    def hirelings(implicit game : Game) : $[Hireling] = game.hirelings
    def traders(implicit game : Game) : $[Trader] = factions.of[Trader]

    implicit class FactionFriendsEx(f : Faction)(implicit game : Game) {
        def friends(e : Faction) : Boolean = (f, e) match {
            case (a, b) if a == b => true
            case (a : Hireling, b) => a.owner.?(_.friends(b))
            case (a, b : Hireling) => b.owner.?(_.friends(a))
            case (a : Hero, b : Hero) => a.coalition.any && b.coalition.any && a.coalition == b.coalition
            case (a : Hero, o) => a.coalition == Some(o)
            case (o, b : Hero) => b.coalition == Some(o)
            case _ => false
        }

        def enemies = (factions ++ hirelings).%!(_.friends(f))
    }

    implicit class FactionPhaseEx(f : Faction)(implicit game : Game) {
        def canAidExile = f.forTrade.any && TheExile.?%(hirelings.has).%(h => f.presence.exists(c => game.fromForest(h.region).contains(c))).any

        def birdsongNewCard = f.can(StandAndDeliver) || f.can(SwapMeet) || (f.can(Merchants) && f.forTrade.any) || canAidExile

        def birdsongOnly = f.can(StandAndDeliver) || f.can(SwapMeet) || (f.can(Merchants) && f.forTrade.any) || f.can(FalseOrders)

        def birdsongNewSupporter = f.can(FalseOrders)

        def birdsong : $[UserAction] = {
            implicit val ask = builder

            game.expansions.foreach(_.birdsong(f))

            ask.list
        }

        def daylightNewCard = f.can(TaxCollector) || canAidExile

        def daylightOnly = f.can(TaxCollector) || f.can(PropagandaBureau) || traders.exists(s => f.can(TalentScout(s)))

        def daylight : $[UserAction] = {
            implicit val ask = builder

            game.expansions.foreach(_.daylight(f))

            ask.list
        }

        def eveningNewCard = canAidExile

        def evening = {
            implicit val ask = builder

            game.expansions.foreach(_.evening(f))

            ask.list
        }

        def expose : $[UserAction] = {
            implicit val ask = builder

            MischiefExpansion.expose(f)

            ask.list
        }

    }

    implicit class AskEx(ask : Ask)(implicit game : Game) {
        def birdsong(f : Faction) = ask.add(f.birdsong)
        def daylight(f : Faction) = ask.add(f.daylight)
        def evening(f : Faction) = ask.add(f.evening)
    }


    implicit def descCard(g : Game, d : DeckCard) = d.img

    implicit val descQuasiItem = (l : $[QuasiItem], n : Int, s : $[Int]) => (l(n) @@ {
        case q @ ToExhaustDamage(v, r) => ToExhaustDamage(v.diff(s.%(_ < n)./(l.apply)./~(_.real)./(_.item)), r)
        case q => q
    }).elem(s.has(n))(styles.iii)

}


class Game(val players : $[Player], val candidates : $[Faction], val options : $[Meta.O]) extends BaseGame with ContinueGame with LoggedGame {
    if (options.has(FactionSeatingGiven) || options.has(SetupOrderPriority))
        require(candidates.num == players.num)

    private implicit val game = this

    def arity = players.num

    var ftp = Map[Faction, Player]()
    var ptf = Map[Player, Faction]()

    players.of[Faction].foreach { f =>
        ftp += f -> f
        ptf += f -> f
    }

    var seating = $[Player]()

    var drawn = $[Faction]()
    var chars = Map[Hero, Character]()

    def playChoices : $[PlayChoice] = drawn./(f => f.as[Hero]./(h => FactionCharacterChoice(h, chars(h))).|(FactionChoice(f)))

    var ordering = Map[Int, Faction]()

    def expansionsFor(l : $[Faction]) = {
        val all = $(
            TurnHelperExpansion,

            GameOverExpansion,

            DominanceExpansion,

            HirelingsExpansion,

            HeroExpansion,

            AviaryExpansion,
            FelineExpansion,
            InsurgentExpansion,
            FanaticExpansion,
            TraderExpansion,
            UndergroundExpansion,
            MischiefExpansion,
            ExpeditionExpansion,
            HordeExpansion,

            OldExpeditionExpansion,
            UtopiaExpansion,
            CasterExpansion,
            FarmerExpansion,
            InvasiveDDDExpansion,
            InvasiveCCCExpansion,
            InvasiveBBBExpansion,
            InvasiveAAAExpansion,
            LegalAAAExpansion,
            AbductAAAExpansion,

            StandardDeckExpansion,
            ExilesDeckExpansion,
            DuskDeckExpansion,

            SetupExpansion,
            MapsExpansion,
            CardsExpansion,
            CraftExpansion,
            BattleExpansion,
            MovementExpansion,
            TurnExpansion,
            RemovalExpansion
        )

        var rest = all.%(_.active(l, options))

        var ordered = rest.take(0)

        while (rest.any) {
            rest.%(_.after.intersect(rest).none).%(e => rest.%(_.before.has(e)).none).starting @@ {
                case None =>
                    warn("rest", rest)
                    warn("ordered", ordered)
                    throw new Error("can't order expansions")
                case Some(e) =>
                    rest :-= e
                    ordered :+= e
            }
        }

        ordered
    }

    var expansions : $[Expansion] = expansionsFor(candidates ++ options.of[IncludeFaction]./(_.faction))

    def extraMoveFrom(f : Faction) = {
        expansions./~(_.extraMoveFrom(f))
    }

    object ui {
        var funNames : Boolean = true
        var rules : Boolean = true
        var movements : Boolean = false
        var battles : Boolean = false
        var nukes : Boolean = false
        var graveyard : Boolean = false
    }

    var over : Option[GameOver] = None

    def isOver = over.any

    var highlights : $[Highlight] = $

    var highlightFaction : $[Faction] = $

    val board : Board = options./~{
        case AutumnMap => Some(AutumnBoard)
        case WinterMap => Some(WinterBoard)
        case LakeMap => Some(LakeBoard)
        case MountainMap => Some(MountainBoard)
        case TidalMap => Some(TidalBoard)
        case TundraMap => Some(TundraBoard)
        case GloomMap => Some(GloomBoard)
        case _ => None
    }.only

    var scorched : $[Clearing] = $
    var flooded : $[Clearing] = $
    var rubble : $[(Clearing, Clearing)] = board.rubble
    var blizzard : $[(Clearing, Clearing)] = $

    var ferry : $[Clearing] = $
    var tower : $[Clearing] = $
    var lostCity : $[Clearing] = $

    def landmarks : $[Clearing] = ferry ++ tower ++ lostCity

    var used : $[Effect] = $

    def clearings = board.clearings.diff(scorched).diff(flooded)
    def riverside = clearings.%(c => byRiver(c).any || board.byRiver(c).any)

    def fromForest(f : Forest) = board.fromForest(f).diff(scorched).diff(flooded)

    def byRiver(c : Clearing) = (board.byRiver(c) ++ board.connected(c).intersect(flooded)./~(board.connected).but(c)).distinct.diff(scorched).%(o => blizzard.has((c, o)).not && blizzard.has((o, c)).not)

    def connected(c : Clearing) = {
        val ll =
        board.connected(c)
            .diff(scorched)
            .diff(flooded)
            .%!(o => rubble  .has((c, o)) || rubble  .has((o, c)))
            .%!(o => blizzard.has((c, o)) || blizzard.has((o, c)))
            .%!(o => factions.of[Caster].%(game.states.contains).exists(f => f.growth.has((o, c)) || f.growth.has((c, o))))

        ll
    }

    var original : Map[Suitable, $[BaseSuit]] = Map()
    var mapping : Map[Suitable, $[BaseSuit]] = Map()

    var graveyard : Map[Region, $[Figure]] = board.clearings./(_ -> $()).toMap

    var lastlog : $[Any] = $

    private def convertForLog(s : $[Any]) : $[Any] = s./~{
        case Empty => None
        case NotInLog(_) => None
        case AltInLog(_, m) => Some(m)
        case f : Faction => Some(f.elem.styled(styles.condensed))
        case e : GameModeElementary => Some(e.elem(this, ModeLog))
        case d : DeckCard => Some(OnClick(d, d.elem.spn(xlo.pointer)))
        case l : List[Any] => convertForLog(l)
        case x => Some(x)
    }

    override def log(s : Any*) {
        val l = s.$

        if (l.has(SuppressLog))
            return

        if (l == $(SingleLine) && lastlog == $(SingleLine))
            return

        lastlog = l

        super.log(convertForLog(l) : _*)
    }

    override def logtemp(s : Any*) {
        if (s.contains(SuppressLog))
            return

        super.logtemp(convertForLog(s.$) : _*)
    }

    var states : Map[Faction, FactionState] = Map()
    var factions : $[Faction] = $

    var hirelings : $[Hireling] = $
    var unhired : $[Hireling] = $
    var thresholds : $[Int] = $

    var corners : $[Clearing] = $
    var homelands : $[Clearing] = $

    val transports = $[Transport](Roads, Riverboat, Ferry, Swimmers, BurrowRoads, TunnelRoads, OffTrail)

    def slots(c : Clearing) = c.capacity + options.has(KeepExtraBuildingSlot).??(factions.of[Feline].%(game.states.contains).%(_.at(c).has(Keep)).num)

    def freeSlots(c : Clearing) = slots(c) - factions./(_.at(c).of[Building].num).sum - hirelings./(_.at(c).of[Building].num).sum - ruins.get(c).any.??(1)

    var ruins : Map[Clearing, Ruins] = Map()

    var quests = $[Quest]()

    val cards = new ValueTracker[Card]
    val playingDeck = Deck.fromOptions(options, players.num, players.of[Faction])
    val predrafts = players./(p => p -> cards.another[DeckCard]("draft-" + p)).toMap
    val pile = cards.another[DeckCard]("pile", playingDeck)
    val deck = cards.another[DeckCard]("deck")
    val outOfGameCards = cards.another[DeckCard]("out-of-game-cards")
    val dominances = cards.another[Dominance]("dominances")

    val pieces = new IdentityTracker[(Faction, Region), Figure]

    lazy val coffins = game.pieces.register(Neutral -> CoffinMakers, _.piece.is[Warrior])

    lazy val dawn = game.pieces.register(Neutral -> BreakingDawn, _.piece.is[Warrior])
    lazy val noon = game.pieces.register(Neutral -> HighNoon, _.piece.is[Warrior])
    lazy val dusk = game.pieces.register(Neutral -> DuskAwakening, _.piece.is[Warrior])

    lazy val efforts = game.pieces.register(Neutral -> Effort, u => u.piece == Heart, 0.to(33)./(i => Figure(Neutral, Heart, i)))

    val dead = RedirectRegion { u =>
        if (u.piece.is[Warrior] && u.piece.is[Tenacious].not && factions.%(_.has(CoffinMakers)).any && u.faction.ignored.has(IgnoreWarriorRemovalEffects).not)
            coffins
        else
            u.faction.reserve
    }

    val recycle = RedirectRegion { u =>
        if (u.piece == Heart)
            efforts
        else
        if (u.piece.is[Warrior] && u.piece.is[Tenacious].not && factions.%(_.has(CoffinMakers)).any && u.faction.ignored.has(IgnoreWarriorRemovalEffects).not && options.has(UnthematicCoffinMakers))
            coffins
        else
            u.faction.reserve
    }

    val pool = RedirectRegion { u => u.faction.reserve }

    var uncrafted : $[Item] = $(Bag, Bag, Crossbow, Boots, Boots, Hammer, Sword, Sword, Teapot, Teapot, Coins, Coins)

    var turn : Int = 0

    var current : Faction = Neutral

    var phase : Phase = Night

    def displayRegion(r : Region) : $[Figure] = r match {
        case c : Forest =>
            (factions.of[Hero] ++ factions.of[Expedition] ++ hirelings.of[TheExile.type] ++ factions.of[OldExpedition]).%(states.contains)./~(_.from(c).$) ++
            (factions.of[CommonAbduct]).%(states.contains)./~(f => f.from(c).$ ++ f.from(Cage(c)).$)
        case c : Clearing if flooded.has(c) => $
        case c : Clearing =>
            (factions ++ hirelings).%(states.contains)./~(_.from(c).$) ++
            options.has(UntargetableKeep).??(factions.of[Feline]./~(f => f.keep.has(c).?(Figure(f, Keep, 0)))) ++
            ruins.get(c)./(Figure(Neutral, _, 0)) ++
            (tower.contains(c)).?(Figure(Neutral, Tower, 0)) ++
            (ferry.contains(c)).?(Figure(Neutral, Ferry, 0)) ++
            (lostCity.has(c)).?(Figure(Neutral, LostCity, 0)) ++
            (scorched.contains(c)).??(0.to(11)./(n => Figure(Neutral, ScorchedEarthMarker(n % 6 + 1), n)))
    }

    def viewHand(f : Faction) = f.hand./(ViewCardInfoAction(f, PreBreak(OwnHand).elem(game), _))
    def viewSupporters(f : Faction) = f.as[Insurgent].?(f => f.supporters./(ViewCardInfoAction(f, PreBreak(FactionSupporters(f)).elem(game), _)))
    def viewQuests(f : Faction) = f.as[Hero].?(f => quests.take(3)./(QuestInfoAction(f, _, f.inv)))
    def viewPlots(f : Faction) = f.as[Mischief].?(f => f.hidden./(c => PlotInfoAction(f, f.secret(c), c)))
    def viewTraderCards(f : Faction) = factions.but(f).of[Trader]./~(t => t.hand./(ViewCardInfoAction(f, PreBreak(FactionHand(t)).elem(game), _)))
    def viewRetinue(f : Faction) = f.as[Expedition]./~(f => Retinue.all./~(r => (f.retinue(r) ++ f.complete(r))./(ViewCardInfoAction(f, Break ~ r.elem, _))))
    def viewDrawnFactions() : $[BaseInfo] = (turn == 0 && drawn.any && (drawn.of[Hero].none || chars.any)).?? {
        val remaining = candidates.%!(states.contains)

        drawn./(f => f.as[Hero].%(_ => options.has(RandomCharacter))./(h => FactionCharacterChoice(h, chars(h))).|(FactionChoice(f)))./(PlayChoiceDescAction(_, remaining))
    }

    def info(waiting : $[Player], self : |[Player], actions : $[UserAction]) : $[Info] = {
        (self.of[Faction] ++ self./~(ptf.get)).$.distinct.%(states.contains)./~( f =>
            actions.has(NoDrawnFactions).not.??(viewDrawnFactions()) ++
            actions.has(HiSupporters).??(viewSupporters(f)) ++
            actions.has(NoPlots).not.??(viewPlots(f)) ++
            actions.has(NoHand).not.??(viewHand(f)) ++
            (actions.has(NoSupporters).not && actions.has(HiSupporters).not).??(viewSupporters(f)) ++
            viewQuests(f) ++
            viewRetinue(f) ++
            viewTraderCards(f).take(0)
        ) ++
        self.of[PlayerN]./~( p =>
            actions.has(NoDrawnFactions).not.??(viewDrawnFactions()) ++
            actions.has(NoHand).not.??(predrafts.get(p)./~(_.get)./(ViewCardInfoAction(p, Break ~ p.elem ~ " drew cards", _)))
        ) ++
        self.none.??(viewDrawnFactions())
    }

    def preinfo(waiting : $[Player], self : |[Player], actions : $[UserAction]) : $[Info] = {
        def fix(f : Player) = f @@ {
            case f : PlayerN => ptf.get(f).|(f)
            case f => f
        }

        self./(fix) @@ {
            case Some(f : Aviary) if current == f && f.leader.any =>
                val adding = actions./~{
                    case DecreeAction(self, h : $[DeckCard], l : $[Int], d : $[Decree]) if self == f =>
                        d.lazyZip(l).map((d, n) => d -> h(n).suit)

                    case _ => $()
                }.groupMap(_._1)(_._2)

                val decree = Decree.all.map { d =>
                    val t = f.todo(d) ++ adding.get(d).|($)
                    val o = f.done(d)
                    val r = t.diff(o).diff(o.diff(t)./(_ => Bird))
                    val c = t.diff(r)

                    val id = f.style + "-" + (d == Decree.Recruit && f.leader == Some(Charismatic)).??("double-") + d.name
                    c./(s => Image(id + "-" + s.name + "-done", styles.action)) ++ r./(s => Image(id + "-" + s.name, styles.action))
                }

                import helper.&

                val dddd = &(&(&(decree(0)) ~ " " ~ &(decree(1))) ~ " " ~ &(decree(2) ~ " " ~ &(decree(3))))

                case object DecreeDescAction extends BaseInfo(dddd)(Empty) { val self = f }

                $(DecreeDescAction)

            case _ => $()
        }
    }

    var repeat : |[TopLevelAction] = None

    def loggedPerform(action : Action, soft : Void) : Continue = {
        def fixPlayer(f : Player) = f @@ {
            case h : Hireling => ftp(h.owner.|(current))
            case f : Faction => ftp(f)
            case _ => f
        }

        val c = action.as[SelfPerform]./(_.perform(soft)).|(internalPerform(action, soft)) @@ {
            case c : Ask => c.copy(faction = fixPlayer(c.faction))
            case c : MultiAsk => c.copy(asks = c.asks./(c => c.copy(faction = fixPlayer(c.faction))))
            case c => c
        }

        highlightFaction = (c @@ {
            case Ask(_, Nil) => println("empty ask as a result of " + action); throw new Error("empty ask")
            case Ask(f, _) => $(f)
            case MultiAsk(a) => a./(_.faction)
            case _ => $()
        }).of[Faction]

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

    def cloned() : Game = this

    def cleanFor(o : Faction) : Game = this

}
