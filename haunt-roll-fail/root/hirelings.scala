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

case object NewHireling extends DisplayEffect {
    override val name = "New Hireling"
}

trait Hireling extends Faction with DisplayEffect {
    override lazy val elem : Elem = name.styled(this)
    val demoted : Boolean
    val setup : Boolean

    val priority = ""
    def abilities(options : $[Meta.O]) : $[Effect] = $

    def advertising = Empty
    def motto = Empty
}

trait HirelingState extends FactionState {
    def craft = $
}

class BasicHirelingState(val faction : Hireling)(implicit val game : Game) extends HirelingState


case object StreetBand extends Hireling with WarriorFaction {
    val name = "Street Band"
    val short = "STB"
    val style = "stb"

    val demoted = true
    val setup = false

    val clashKey = StreetBand

    val warrior = Weasel

    def pieces(options : $[Meta.O]) = Weasel *** 5
}

case object Weasel extends Warrior

class StreetBandState(val faction : StreetBand.type)(implicit val game : Game) extends HirelingState {
    var enchanted : $[Clearing] = $
}


trait PawnHireling extends Hireling

case object StoicProtector extends PawnHireling {
    val name = "Stoic Protector"
    val short = "STP"
    val style = "stp"

    val demoted = true
    val setup = false

    val clashKey = StoicProtector

    override val transports : $[$[Transport]] = $($(FreeMove))

    def pieces(options : $[Meta.O]) = Deer *** 1
}

case object Deer extends Pawn with Movable with Tenacious

class PawnHirelingState(val faction : PawnHireling)(implicit val game : Game) extends HirelingState {
    val pawn = reserve.$.only
}


case object BanditGangs extends Hireling with WarriorFaction {
    val name = "Bandit Gangs"
    val short = "BDG"
    val style = "bdg"

    val demoted = true
    val setup = false

    val clashKey = BanditGangs

    val warrior = Porcupine

    def pieces(options : $[Meta.O]) = Porcupine *** 4
}

case object Porcupine extends Warrior


case class SkipHeal(c : Clearing) extends RemoveEffect

case object FelinePhysicians extends Hireling {
    val name = "Feline Physicians"
    val short = "FPH"
    val style = "fph"

    val demoted = true
    val setup = false

    val clashKey = MC

    def pieces(options : $[Meta.O]) = $
}

class FelinePhysiciansState(val faction : FelinePhysicians.type)(implicit val game : Game) extends HirelingState {

}


case object BluebirdNobles extends Hireling {
    val name = "Bluebird Nobles"
    val short = "BBN"
    val style = "bbn"

    val demoted = true
    val setup = false

    val clashKey = ED

    def pieces(options : $[Meta.O]) = $
}


case object RabbitScouts extends Hireling with BattleEffect {
    val name = "Rabbit Scouts"
    val short = "RSC"
    val style = "rsc"

    val demoted = true
    val setup = false

    val clashKey = WA

    def pieces(options : $[Meta.O]) = $
}


case object Brigand extends Hireling {
    val name = "The Brigand"
    val short = "BRI"
    val style = "bri"

    val demoted = true
    val setup = false

    val clashKey = VB

    def pieces(options : $[Meta.O]) = $
}

case object BrigandExplore extends Effect
case object BrigandSteal extends Effect


case object LizardEnvoys extends Hireling {
    val name = "Lizard Envoys"
    val short = "LEN"
    val style = "len"

    val demoted = true
    val setup = false

    val clashKey = LC

    def pieces(options : $[Meta.O]) = $
}


case object OtterDivers extends Hireling {
    val name = "Otter Divers"
    val short = "ODV"
    val style = "odv"

    val demoted = true
    val setup = false

    val clashKey = RF

    def pieces(options : $[Meta.O]) = $
}


case object MoleArtisians extends Hireling {
    val name = "Mole Artisians"
    val short = "MAR"
    val style = "mar"

    val demoted = true
    val setup = false

    val clashKey = UD

    def pieces(options : $[Meta.O]) = $
}

class MoleArtisiansState(val faction : MoleArtisians.type)(implicit val game : Game) extends HirelingState {
    var display = cards("display")

    var revealed = cards("revealed")
}


case object RavenSentries extends Hireling with BattleEffect {
    val name = "Raven Sentries"
    val short = "RSN"
    val style = "rsn"

    val demoted = true
    val setup = false

    val clashKey = CC

    def pieces(options : $[Meta.O]) = $
}


case object RatSmugglers extends Hireling with Effect {
    val name = "Rat Smugglers"
    val short = "RSM"
    val style = "rsm"

    val demoted = true
    val setup = false

    val clashKey = LH

    def pieces(options : $[Meta.O]) = $
}


case object BadgerBodyguards extends Hireling with BattleEffect {
    val name = "Badger Bodyguards"
    val short = "BBG"
    val style = "bbg"

    val demoted = true
    val setup = false

    val clashKey = KI

    def pieces(options : $[Meta.O]) = $
}


case object StrayMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Region, d : Region) => f.rules(o) || f.rules(d)
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = (f, o, l) @@ {
        case (f : Faction, o : Region, l : $[Movable]) => f.at(o).of[Warrior].take(l.num) == l
    } && allows(f, o, d)
}

trait StrayCat extends Warrior {
    override def id = "stray-cat"
    override val name = toString
    override def plural = "Stray Cats"
}

case object StrayCat extends StrayCat {
    override val name = "Stray Cat"
}

case object Jeff extends StrayCat
case object Smokie extends StrayCat
case object Maru extends StrayCat

case object Momcat extends StrayCat
case object Black extends StrayCat
case object Dolphin extends StrayCat

case object Madchen extends StrayCat
case object Leo extends StrayCat
case object Monstra extends StrayCat

case object Takahara extends StrayCat
case object Kaban extends StrayCat
case object Rouge extends StrayCat

case object ForestPatrol extends Hireling with WarriorFaction {
    val name = "Forest Patrol"
    val short = "FRP"
    val style = "frp"

    val demoted = false
    val setup = true.not.not.not

    val clashKey = MC

    override val transports = $($(StrayMove))

    val warrior = StrayCat

    def pieces(options : $[Meta.O]) = $(Jeff, Smokie, Maru, Momcat, Black, Dolphin, Madchen, Leo, Monstra, Takahara, Kaban, Rouge)
}

case object FirstAid extends SpecialRegion

class ForestPatrolState(val faction : ForestPatrol.type)(implicit val game : Game) extends HirelingState {
    val hospital = location(FirstAid, u => u.faction == faction && u.piece.is[StrayCat])
}


case object Vulture extends Warrior

case object LastDynasty extends Hireling with WarriorFaction {
    val name = "Last Dynasty"
    val short = "LDN"
    val style = "ldn"

    val demoted = false
    val setup = true

    val clashKey = ED

    val warrior = Vulture

    def pieces(options : $[Meta.O]) = Vulture *** 5
}


case object Animal extends Warrior

case object SpringUprising extends Hireling with WarriorFaction with BattleEffect {
    val name = "Spring Uprising"
    val short = "SUP"
    val style = "sup"

    val demoted = false
    val setup = true

    val clashKey = WA

    val warrior = Animal

    def pieces(options : $[Meta.O]) = Animal *** 5
}


case object ForestOnlyRoads extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Forest, d : Forest) => game.board.forestsConnected(o, d)
        case _ => false
    }
}

case object Bear extends Pawn with Movable with Attacking

case object TheExile extends PawnHireling {
    val name = "The Exile"
    val short = "EXL"
    val style = "exl"

    val demoted = false
    val setup = true

    val clashKey = VB

    override val transports : $[$[Transport]] = $($(FreeMove, ForestOnlyRoads))

    def pieces(options : $[Meta.O]) = Bear *** 1
}

class TheExileState(val faction : TheExile.type)(implicit val game : Game) extends HirelingState {
    board.forests.foreach(c => location(c))

    val pawn = reserve.$.only

    var ready : $[Item] = $(Club, Club, Club)

    var exhausted : $[Item] = $

    def region : Forest = game.pieces.find(pawn) match {
        case Some((f, r : Forest)) => r
        case q => println(q); throw new Error("bear nowhere to be found")
    }
}

case object Aiding extends Message {
    def elem(implicit game : Game) = "aiding"
}


case object RiverfolkFlotilla extends PawnHireling {
    val name = "Riverfolk Flotilla"
    val short = "RFF"
    val style = "rff"

    val demoted = false
    val setup = true

    val clashKey = RF

    override def abilities(options : $[Meta.O]) = $(Riverboats)

    override val transports : $[$[Transport]] = $($(FreeMove, Riverboat))

    def pieces(options : $[Meta.O]) = Flotilla *** 1
}

case object Flotilla extends Pawn with Attacking with Movable with Tenacious


case object Chameleon extends Warrior

case object WarmSunProphets extends Hireling with WarriorFaction {
    val name = "Warm Sun Prophets"
    val short = "WSP"
    val style = "wsp"

    val demoted = false
    val setup = true

    val clashKey = LC

    val warrior = Chameleon

    def pieces(options : $[Meta.O]) = Chameleon *** 4
}


case object Magpie extends Warrior

case object CorvidSpies extends Hireling with WarriorFaction {
    val name = "Corvid Spies"
    val short = "CSP"
    val style = "csp"

    val demoted = false
    val setup = true

    val clashKey = CC

    val warrior = Magpie

    def pieces(options : $[Meta.O]) = Magpie *** 6
}


case object Mouldwarp extends Warrior
case object Foothold extends Token

case object SunwardExpedition extends Hireling with WarriorFaction {
    val name = "Sunward Expedition"
    val short = "SWE"
    val style = "swe"

    val demoted = false
    val setup = true

    val clashKey = UD

    val warrior = Mouldwarp

    def pieces(options : $[Meta.O]) = Mouldwarp *** 8 ++ Foothold *** 3
}


case object Vole extends Warrior
case object Vault extends Building

case object VaultKeepers extends Hireling with WarriorFaction {
    val name = "Vault Keepers"
    val short = "VLK"
    val style = "vlk"

    val demoted = false
    val setup = true

    val clashKey = KI

    val warrior = Vole

    def pieces(options : $[Meta.O]) = Vole *** 6 ++ Vault *** 6
}


case object Varmint extends Warrior

case object FlameBearers extends Hireling with WarriorFaction {
    val name = "Flame Bearers"
    val short = "FMB"
    val style = "fmb"

    val demoted = false
    val setup = true

    val clashKey = LH

    val warrior = Varmint

    def pieces(options : $[Meta.O]) = Varmint *** 6
}


trait RockStar extends Warrior {
    override def id = "rock-star"

    override def plural = "Rock Stars"
}

case object RockStar extends RockStar {
    override val name = "Rock Star"
}

case object IanAnderson extends RockStar { override val name = "Ian Anderson" }
case object ThomYorke extends RockStar { override val name = "Thom Yorke" }
case object FreddyMercury extends RockStar { override val name = "Freddy Mercury" }
case object SteveWilson extends RockStar { override val name = "Steve Wilson" }
case object ChrisKarrer extends RockStar { override val name = "Chris Karrer" }
case object TimSmith extends RockStar { override val name = "Tim Smith" }
case object Vangelis extends RockStar { override val name = "Vangelis" }
case object MarkKnopfler extends RockStar { override val name = "Mark Knopfler" }
case object JohnnyCash extends RockStar { override val name = "Johnny Cash" }
case object RickWakeman extends RockStar { override val name = "Rick Wakeman" }

case object DavidSylvian extends RockStar { override val name = "David Sylvian" }
case object BobDylan extends RockStar { override val name = "Bob Dylan" }
case object DavidBowie extends RockStar { override val name = "David Bowie" }
case object JeffLynne extends RockStar { override val name = "Jeff Lynne" }
case object RogerChapman extends RockStar { override val name = "Roger Chapman" }
case object PeterHammill extends RockStar { override val name = "Peter Hammill" }
case object SteveHackett extends RockStar { override val name = "Steve Hackett" }
case object WilliamDDrake extends RockStar { override val name = "William D. Drake" }
case object SandyDenny extends RockStar { override val name = "Sandy Denny" }
case object DaveCousins extends RockStar { override val name = "Dave Cousins" }
case object LouReed extends RockStar { override val name = "Lou Reed" }
case object KateBush extends RockStar { override val name = "Kate Bush" }
case object ElvisPresley extends RockStar { override val name = "Elvis Presley" }

case object PopularBand extends Hireling with WarriorFaction {
    val name = "Popular Band"
    val short = "PLB"
    val style = "plb"

    val demoted = false
    val setup = true

    val clashKey = StreetBand

    val warrior = RockStar

    def pieces(options : $[Meta.O]) =
        IanAnderson ::
        ThomYorke ::
        FreddyMercury ::
        SteveWilson ::
        ChrisKarrer ::
        TimSmith ::
        Vangelis ::
        MarkKnopfler ::
        JohnnyCash ::
        RickWakeman ::
        DavidSylvian ::
        BobDylan ::
        DavidBowie ::
        JeffLynne ::
        RogerChapman ::
        PeterHammill ::
        SteveHackett

}


case object Studio extends SpecialRegion {



}

class PopularBandState(val faction : PopularBand.type)(implicit val game : Game) extends HirelingState {
    var enchanted : $[Clearing] = $

    val studio = location(Studio, u => u.faction == faction && u.piece.is[RockStar])
}


case object Elk extends Pawn with Movable with Tenacious

case object FuriousProtector extends PawnHireling {
    val name = "Furious Protector"
    val short = "FPR"
    val style = "fpr"

    val demoted = false
    val setup = true

    val clashKey = StoicProtector

    override val transports : $[$[Transport]] = $($(FreeMove))

    def pieces(options : $[Meta.O]) = Elk *** 1
}


case object HighwayBandits extends Hireling {
    val name = "Highway Bandits"
    val short = "HWB"
    val style = "hwb"

    val demoted = false
    val setup = true

    val clashKey = BanditGangs

    def pieces(options : $[Meta.O]) = Bandit *** 4
}

case object Bandit extends Pawn

case class PathBetween(a : Clearing, b : Clearing) extends SpecialRegion

class HighwayBanditsState(val faction : HighwayBandits.type)(implicit val game : Game) extends HirelingState {
    val paths = clearings.combinations(2).$./(l => l(0) -> l(1)).%((a, b) => game.connected(a).has(b))./((a, b) => location(PathBetween(a, b), u => u.faction == faction && u.piece == Bandit))
}


case class HirelingTransfersAction(f : Faction) extends ForcedAction
case class GiveAwayHirelingAction(self : Faction, l : $[Hireling], h : Hireling) extends BaseAction(self, "gives away")(h) with Soft
case class GiveAwayHirelingToAction(self : Faction, h : Hireling, e : Faction) extends BaseAction(self, "gives away", h)(e)
case class HirelingPurchasesAction(f : Faction) extends ForcedAction
case class HireHirelingAction(self : Faction, h : Hireling) extends BaseAction(self, "hires")(h)
case class TakeHirelingAction(f : Faction, h : Hireling, then : ForcedAction) extends ForcedAction
case class ControlRolledAction(f : Faction, h : Hireling, n : Int, then : ForcedAction) extends RolledAction[Int] { def rolled = $(n) }
case class OnTakeHirelingAction(f : Faction, h : Hireling, then : ForcedAction) extends ForcedAction


case class PlaceHirelingWarriorAction(self : Player, h : Hireling, p : Piece, c : Clearing, then : ForcedAction) extends BaseAction(self, "places", p.of(h), "in")(c)
case class PlaceHirelingWarriorsAction(self : Player, h : Hireling, p : $[Piece], c : Clearing, then : ForcedAction) extends BaseAction(self, "places", p./(_.of(h)).comma, "in")(c)


case class FelinePhysiciansAction(f : WarriorFaction, c : Clearing, d : DeckCard, r : Region, then : ForcedAction) extends ForcedAction
case class FelinePhysiciansIgnoreAction(f : WarriorFaction, c : Clearing, then : ForcedAction) extends ForcedAction

case class MoleArtisiansRevealAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(MoleArtisians, "can keep revealed crafted card")(d.img) with ViewCard
case class MoleArtisiansDiscardAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction()("Discard")

case class BluebirdNoblesAction(self : Faction, n : Int) extends BaseAction()("Score", n.hl, "ruled clearings with", BluebirdNobles)

case class LizardEnvoysMainAction(self : Faction) extends BaseAction()("Take a recently discarded card with", LizardEnvoys) with Soft
case class LizardEnvoysAction(self : Faction, d : DeckCard) extends BaseAction(LizardEnvoys)(d.img) with ViewCard

case class StreetBandMainAction(self : Faction, l : $[Clearing]) extends BaseAction(Hirelings)(StreetBand) with Soft

case class BanditGangsMainAction(self : Faction, l : $[Clearing], r : $[$[Clearing]], o : Int) extends BaseAction(Hirelings)(BanditGangs) with Soft
case class BanditGangsMassRecruitAction(self : WarriorFaction, l : $[Clearing], o : Int) extends BaseAction("Recruit", l.num.times(self.warrior.imgd(self)).merge)(implicit g => l./(_.elem).comma.merge)

case class RatSmugglersMainAction(self : Faction, lm : $[Region], lw : $[Clearing], then : ForcedAction) extends BaseAction(Hirelings)(RatSmugglers) with Soft
case class RatSmugglersAction(self : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class DiscardItemCardAction(f : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction

case class BrigandExploreMainAction(self : Faction, l : $[Clearing], then : ForcedAction) extends BaseAction(Brigand)("Explore") with Soft
case class BrigandExploreAction(self : Faction, c : Clearing, then : ForcedAction) extends BaseAction(Brigand, "explores a ruin in")(c)
case class BrigandTakeRuinItemAction(self : Faction, c : Clearing, i : Item, then : ForcedAction) extends BaseAction(self, "explores ruins in", c, "and finds an item")("Take", i.img, i)
case class BrigandIgnoreRuinItemAction(self : Faction, c : Clearing, then : ForcedAction) extends BaseAction(None)("Don't take an item".styled(styles.hit))

case class BrigandStealMainAction(self : Faction, l : $[Faction], then : ForcedAction) extends BaseAction(Brigand)("Steal") with Soft
case class BrigandStealAction(self : Faction, i : ItemRef, e : Faction, then : ForcedAction) extends BaseAction(Brigand, "steals from")(e)

case class PopularBandStudioAction(self : Player, h : PopularBand.type, shuffled : $[RockStar]) extends ShuffledAction[RockStar]
case class PopularBandSetupAction(self : Player, h : PopularBand.type) extends ForcedAction

case class HighwayBanditMainPlaceAction(self : Player, h : HighwayBandits.type, alt : $[UserAction], then : ForcedAction) extends ForcedAction with Soft
case class HighwayBanditPlaceAction(self : Player, h : HighwayBandits.type, a : Clearing, b : Clearing, then : ForcedAction) extends ForcedAction
case class HighwayBanditsSetupAction(self : Player, h : HighwayBandits.type) extends ForcedAction


case class ShuffledStrayCatsAction(self : Player, h : ForestPatrol.type, shuffled : $[StrayCat]) extends ShuffledAction[StrayCat]
case class ForestPatrolMainAction(self : Faction, h : ForestPatrol.type, m : $[Region], b : $[Clearing], r : $[Clearing], p : Int) extends BaseAction(None)(ForestPatrol) with Soft
case class MovePatrolAction(self : Faction, l : $[Region]) extends BaseAction(ForestPatrol)("Move".styled(ForestPatrol), dt.Move, "then", "Battle".styled(ForestPatrol), dt.Battle) with Soft
case class MoveDonePatrolAction(self : Faction) extends ForcedAction
case class AttackPatrolAction(self : Faction, l : $[Clearing]) extends BaseAction(ForestPatrol)("Battle".styled(ForestPatrol), dt.Battle) with Soft

case class LastDynastyMainAction(self : Faction, h : LastDynasty.type, c : |[Clearing]) extends BaseAction(Hirelings)(LastDynasty) with Soft
case class LastDynastyRecruitAction(self : Faction, h : LastDynasty.type, c : Clearing) extends BaseAction("Recruit", implicit g => h.pooled(h.warrior).times(h.warrior.imgd(h)).merge, "and Battle in")(c)

case class UprisingSetupWarriorsAction(self : Player, h : SpringUprising.type, rolled : $[BaseSuit], done : $[Clearing], then : ForcedAction) extends RolledAction[BaseSuit]
case class PlaceUprisingWarriorsAction(self : Player, h : SpringUprising.type, l : $[BaseSuit], done : $[Clearing], then : ForcedAction) extends ForcedAction
case class SpringUprisingMainAction(self : Faction, h : SpringUprising.type) extends BaseAction()(h)
case class SpringUprisingAction(self : Faction, h : SpringUprising.type, s : BaseSuit) extends RolledAction[BaseSuit] { def rolled = $(s) }
case class SpringUprisingNukeAction(self : Faction, h : SpringUprising.type, c : Clearing) extends BaseAction("Remove", h.warrior.of(h), "and all enemies from")(c)
case class SpringUprisingRemoveAction(self : Faction, h : SpringUprising.type, c : Clearing) extends BaseAction("Remove", h.warrior.of(h), "from")(c)

case class AidTheExileMainAction(self : Faction, h : TheExile.type) extends BaseAction(Hirelings)("Aid", TheExile) with Soft

case class AidTheExileAction(self : Faction, h : TheExile.type, i : ItemRef) extends ForcedAction
case class TheExileMainAction(self : Faction, h : TheExile.type) extends BaseAction(Hirelings)(TheExile) with Soft
case class TheExileMoveAction(self : Faction, h : TheExile.type) extends BaseAction(TheExile)("Move".styled(TheExile), dt.Move) with Soft
case class TheExileBattleAction(self : Faction, h : TheExile.type) extends BaseAction(None)("Battle".styled(TheExile), dt.Battle) with Soft
case class TheExileExhaustAction(self : Faction, h : TheExile.type, n :  Int, then : ForcedAction) extends ForcedAction
case class TheExileDoneAction(self : Faction, h : TheExile.type) extends ForcedAction
case class StartingHirelingForestAction(self : Player, h : Hireling, r : Forest) extends BaseAction(h, "starts in")(implicit g => board.forestName(r))

case class WarmSunProphetsMainAction(self : Faction, h : WarmSunProphets.type) extends BaseAction(Hirelings)(WarmSunProphets) with Soft

case class RiverfolkFlotillaAction(self : Faction, h : RiverfolkFlotilla.type) extends BaseAction()("Draw", "cards".hh, "and move", Flotilla.of(h))
case class RiverfolkFlotillaMoveAction(self : Faction, h : RiverfolkFlotilla.type) extends ForcedAction
case class RiverfolkFlotillaBattleAction(self : Faction, h : RiverfolkFlotilla.type) extends ForcedAction

case class FlameBearersSetupAction(self : Player, h : FlameBearers.type) extends ForcedAction

case object Hirelings extends Message {
    def elem(implicit game : Game) = "Hirelings"
}

object HirelingsExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.of[HirelingsOption].but(NoHirelings).any

    override def extraMoveFrom(f : Faction)(implicit game : Game) = f @@ {
        case h : TheExile.type => $(h.region)
        case _ => $()
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        anytime(f)
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        StreetBand.?%(f.can).foreach { h =>
            + StreetBandMainAction(f, clearings.%(f.canPlace))
                .!(h.pool(Weasel).not, "no warriors")
        }

        BanditGangs.?%(f.can).foreach { h =>
            val cc = (h.pool(Porcupine)).??(clearings.%(f.present).%(f.canPlace))
            f.as[WarriorFaction]./ { f =>
                val bb = h.all(Porcupine).%(f.canPlace)
                val rr = bb.combinations(min(bb.num, f.pooled(f.warrior))).$
                + BanditGangsMainAction(f, cc, rr, bb.num)
                    .!(h.pool(Porcupine).not && f.pool(f.warrior).not, "no warriors")
                    .!(cc.none && rr.none, "can't place")
            }.| {
                + BanditGangsMainAction(f, cc, $, 0)
                    .!(h.pool(Porcupine).not, "no warriors")
                    .!(cc.none, "can't place")
            }
        }

        RatSmugglers.?%(f.can).foreach { h =>
            val lm = f.moveFrom
            val lb = clearings.%(c => f.canAttackList(c).any)

            + RatSmugglersMainAction(f, lm, lb, Repeat)
                .!(f.hand.get.of[CraftItemCard].none, "no craft cards")
                .!(lm.none && lb.none, "can't attack can't move")
        }

        if (f.has(Brigand)) {
            val rr = game.ruins.keys.$
            val rrr = rr.%(f.present)

            + BrigandExploreMainAction(f, rrr, Repeat).x(f.used.has(BrigandExplore), "used").x(rr.none, "no ruins").x(rrr.none, "no presence")
        }

        if (f.has(Brigand)) {
            val ff = factions.but(f).%(_.hand.any)
            val fff = ff.%(e => clearings.exists(c => f.present(c) && e.present(c)))

            + BrigandStealMainAction(f, fff, Repeat)
                .!(f.used.has(BrigandSteal), "used")
                .!(f.forTrade.forall(_.exhausted), "no items")
                .!(ff.none, "no cards")
                .!(fff.none, "no presence")
        }

        ForestPatrol.?%(f.can).foreach { h =>
            val lm = h.moveFrom
            val lb = clearings.%(c => h.canAttackList(c).any)

            val p = h.hospital.num
            val cc = (p > 0).??(clearings.%(f.canPlace).%(c => h.present(c)))

            + ForestPatrolMainAction(f, h, lm, lb, cc, p)
        }

        LastDynasty.?%(f.can).foreach { h =>
            + LastDynastyMainAction(f, h, h.presence.of[Clearing].single)
        }

        TheExile.?%(f.can).foreach { h =>
            + TheExileMainAction(f, h).!(h.ready.none, "no items")
        }

        WarmSunProphets.?%(f.can).foreach { h =>
            + WarmSunProphetsMainAction(f, h)
        }

        anytime(f)
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        anytime(f)
    }

    def anytime(f : Faction)(implicit game : Game, ask : ActionCollector) {
        TheExile.?%(hirelings.has).foreach { h =>
            val cc = f.presence.intersect(game.fromForest(h.region))
            val ii = f.forTrade

            + AidTheExileMainAction(f, h)
                .!(ii.none, "no items")
                .!(cc.none, "not near the forest")
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // THRESHOLDS
        case a if a.isSoft.not && game.turn > 0 && game.thresholds.any && game.thresholds.min <= factions./(_.vp).max =>
            game.thresholds :-= game.thresholds.min

            val f = factions.maxBy(_.vp)

            f.effects :+= NewHireling

            f.log("got a hireling")

            TryAgain

        // STREET BAND
        case SetupHirelingAction(f, h : StreetBand.type) =>
            game.states += h -> new StreetBandState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            SetupNextHirelingAction

        case StreetBandMainAction(f, l) =>
            Ask(f).each(l)(PlaceHirelingWarriorAction(f, StreetBand, Weasel, _, UsedEffectAction(f, StreetBand, Repeat))).cancel

        case ForcedRemoveTargetEffectAction(e, c, h @ StreetBand, p @ Weasel, then) =>
            if (StreetBand.enchanted.has(c))
                if (StreetBand.at(c).none)
                    StreetBand.enchanted :-= c

            then

        case BattlePostHitInAction(b, e, f, p @ Weasel, then) =>
            e.log("muted", p.name.styled(f))

            then

        // STOIC PROTECTOR
        case MoveListAction(self, h @ StoicProtector, t, m, from : Clearing, to : Clearing, l, then) if t.has(Ferry) =>
            h.log("moved on", Ferry.elem, "from", from, "to", to)

            Force(MoveListAction(self, h, $, NoMessage, from, to, l, FerryAction(self, from, to, then)))

        case MoveListAction(self, h @ StoicProtector, t, m, from : Clearing, to : Clearing, l, then) if t.any =>
            h.log("moved", "from", from, "to", to)

            Force(MoveListAction(self, h, $, NoMessage, from, to, l, then))

        case OnTakeHirelingAction(f, h @ StoicProtector, then) =>
            if (h.presence.none) {
                val l = clearings.%(f.present).%(f.canPlace)
                Ask(f)(l./(PlaceHirelingWarriorAction(f, h, Deer, _, then))).bail(then)
            }
            else
                then

        // BANDIT GANGS
        case BanditGangsMainAction(f, l, r, o) =>
            Ask(f)
              .add(f.as[WarriorFaction]./~(f => r./(BanditGangsMassRecruitAction(f, _, o))))
              .add(l./(PlaceHirelingWarriorAction(f, BanditGangs, Porcupine, _, UsedEffectAction(f, BanditGangs, Repeat))))
              .cancel

        case BanditGangsMassRecruitAction(f, l, o) =>
            game.highlights :+= PlaceHighlight(l.distinct)

            l.distinct.foreach { c =>
                val ll = l.%(_ == c)
                ll.foreach(r => f.reserve --> f.warrior --> r)
                f.log("recruited", ll./(_ => f.warrior)./(_.of(f)).comma, "in", c, "with", BanditGangs)
            }

            if (o > l.num && f.totalWar)
                f.oscore(o - l.num)("recruiting")

            UsedEffectAction(f, BanditGangs, Repeat)

        case BattlePostHitInAction(b, e, f, p @ Porcupine, then) =>
            e.log("shaved", p.name.styled(f))

            then

        case OnTakeHirelingAction(f, h @ BanditGangs, then) =>
            if (h.presence.none) {
                val l = clearings.%(f.present).%(f.canPlace)
                Ask(f)(l./(PlaceHirelingWarriorAction(f, h, Porcupine, _, then))).bail(then)
            }
            else
                NoAsk(f)(then)

        // FELINE PHYSICIANS
        case SetupHirelingAction(f, h : FelinePhysicians.type) =>
            game.states += h -> new FelinePhysiciansState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            SetupNextHirelingAction

        case ForcedRemoveProcessAction(f : WarriorFaction, then) if f.can(FelinePhysicians) && f.ignored.has(IgnoreWarriorRemovalEffects).not =>
            val l = (clearings.%(f.canPlace) ++ f.as[Underground]./(_.burrow)).%(f.present)
            val c = board.clearings.diff(f.ignored.of[SkipHeal]./(_.c)).maxBy(c => f.limbo(c).$.%(_.piece.is[Warrior]).num)

            if (l.any && f.limbo(c).$.%(_.piece.is[Warrior]).num > 0) {
                YYSelectObjectsAction(f, f.hand)
                    .withGroup(FelinePhysicians.elem ~ " can heal " ~ f.limbo(c).$.%(_.piece.is[Warrior])./(x => x.piece.img(x.faction)).merge ~ " with")
                    .withRule(_.matches(c.cost))
                    .withThensInfo(d => l./(r => FelinePhysiciansAction(f, c, d, r, then).as(r)("and place them in")))(l./(r => (Info(r) : InfoAction).apply("and place them in")))
                    .withExtra($(NoHand, FelinePhysiciansIgnoreAction(f, c, ForcedRemoveProcessAction(f, then)).as("Skip", FelinePhysicians)))
            }
            else {
                f.used :+= FelinePhysicians

                ForcedRemoveProcessAction(f, then)
            }

        case FelinePhysiciansAction(f, s, d, r, then) =>
            f.hand --> d --> discard.quiet

            val l = f.limbo(s).$

            l --> r

            FelinePhysicians.log("healed", l./(_.elem).comma, "to", r, "with", d)

            then

        case FelinePhysiciansIgnoreAction(f, c, then) =>
            f.ignored :+= SkipHeal(c)

            then

        // BLUEBIRD NOBLES
        case BluebirdNoblesAction(f, n) =>
            f.used :+= BluebirdNobles
            f.oscore(n /↓ 3)("with", BluebirdNobles)
            Repeat

        //  BRIGAND
        case BrigandExploreMainAction(f, l, then) =>
            Ask(f)(l./(BrigandExploreAction(f, _, then))).cancel

        case BrigandExploreAction(f, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            Brigand.logtemp("explored", c, "ruins")

            f.used :+= BrigandExplore

            implicit val ev : MatchesGood[$[Item]] = goodMatchList(goodMatch)

            val fromRuins = f @@ {
                case f : Hero => f.fromRuins
                case f : Horde => f.fromRuins
                case _ => Nil
            }

            val aa = game.ruins(c).items./(i => BrigandTakeRuinItemAction(f, c, i, then).!(fromRuins.contains(i)))

            Ask(f)(aa)(BrigandIgnoreRuinItemAction(f, c, then)).needOk

        case BrigandTakeRuinItemAction(f, c, i, then) =>
            val rest = game.ruins(c).items :- i

            if (rest.any)
                game.ruins += c -> Ruins(rest)
            else
                game.ruins -= c

            f.forTrade :+= i.pristine

            f @@ {
                case f : Hero => f.fromRuins :+= i
                case f : Horde => f.fromRuins :+= i
                case _ =>
            }

            Brigand.log("explored", c, "ruins", "and found", i)

            AcquireItemsAction(f, then)

        case BrigandIgnoreRuinItemAction(f, c, then) =>
            Brigand.log("explored", c, "ruins", "but took nothing")

            Repeat

        case BrigandStealMainAction(f, l, then) =>
            YYSelectObjectsAction(f, f.forTrade./(ToExhaust))
                .withGroup(Brigand.elem)
                .withRule(_.ref.exhausted.not)
                .withThens(i => l./(BrigandStealAction(f, i.ref, _, then)))
                .withExtra($(CancelAction))

        case BrigandStealAction(f, i, e, then) =>
            f.forTrade :-= i
            f.forTrade :+= i.exhaust

            f.used :+= BrigandSteal

            Brigand.log("stole a card from", e, "with", i)

            StealCardAction(f, e, Repeat)

        // LIZARD ENVOYS
        case LizardEnvoysMainAction(f) =>
            val l = game.pile.reverse.take(5).reverse
            val max = l./(d => l.%(_.suit == d.suit).num).max
            val s = l.%(d => l.%(_.suit == d.suit).num == max)./(_.suit).distinct

            Ask(f).each(l)(d => LizardEnvoysAction(f, d).!(d.suit.in(s).not).!(d.is[Ambush])).cancel.needOk

        case LizardEnvoysAction(f, d) =>
            f.used :+= LizardEnvoys

            game.pile --> d --> f.hand

            f.log("took", d, "from the dicard pile with", LizardEnvoys)

            Repeat

        // MOLE ARTISIANS
        case SetupHirelingAction(f, h : MoleArtisians.type) =>
            game.states += h -> new MoleArtisiansState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            SetupNextHirelingAction

        case MoleArtisiansRevealAction(f, d, q) =>
            MoleArtisians.display --> d --> MoleArtisians.revealed

            MoleArtisians.log("kept revealed", d)

            q

        case MoleArtisiansDiscardAction(f, d, q) =>
            MoleArtisians.display --> d --> discard.quiet

            q

        // RAT SMUGGLERS
        case RatSmugglersMainAction(f, lm, lb, then) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup(RatSmugglers.elem)
                .withRule(_.is[CraftItemCard])
                .withThens(d =>
                    lm.any.$(MoveInitAction(f, f, $, WithEffect(RatSmugglers), lm, f.movable, $(CancelAction), DiscardItemCardAction(f, d, then)).as("Move", dt.Move)) ++
                    lb.any.$(BattleInitAction(f, f, WithEffect(RatSmugglers), lb, $(CancelAction), DiscardItemCardAction(f, d, then)).as("Battle", dt.Battle))
                )
                .withExtra($(CancelAction))

        case MoveListAction(self, f, t, m, from, to, l, DiscardItemCardAction(ff, dd, then)) =>
            DiscardItemCardAction(ff, dd, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case BattleStartAction(self, f, a, m, c, o, i, DiscardItemCardAction(ff, dd, then)) =>
            DiscardItemCardAction(ff, dd, BattleStartAction(self, f, a, m, c, o, i, then))

        case LegalAAAConveneAction(f, h, c, e, DiscardItemCardAction(ff, dd, then)) =>
            DiscardItemCardAction(ff, dd, LegalAAAConveneAction(f, h, c, e, then))

        case DiscardItemCardAction(f, d, then) =>
            f.hand --> d --> discard.quiet

            f.log("discarded", d)

            then

        // POPULAR BAND
        case SetupHirelingAction(f, h : PopularBand.type) =>
            game.states += h -> new PopularBandState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Shuffle[RockStar](h.reserve.$.pieces.of[RockStar], l => PopularBandStudioAction(f, h, l))

        case PopularBandStudioAction(f, h, l) =>
            h.reserve --> h.studio

            h.studio --> l.take(5) --> h.reserve

            PopularBandSetupAction(f, h)

        case PopularBandSetupAction(f, h) =>
            if (h.presence.num == 2)
                SetupNextHirelingAction
            else
                Ask(f)
                    .each(clearings.diff(h.presence))(c => PlaceHirelingWarriorAction(f, h, h.reserve.$.pieces(0), c, PopularBandSetupAction(f, h)))
                    .needOk

        // FURIOUS PROTECTOR
        case SetupHirelingAction(f, h : FuriousProtector.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(clearings)(PlaceHirelingWarriorAction(f, h, Elk, _, SetupNextHirelingAction))

        // HIGHWAY BANDITS
        case SetupHirelingAction(f, h : HighwayBandits.type) =>
            game.states += h -> new HighwayBanditsState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            HighwayBanditsSetupAction(f, h)

        case HighwayBanditsSetupAction(f, h) =>
            if (h.reserve.num == 2)
                SetupNextHirelingAction
            else
                HighwayBanditMainPlaceAction(f, h, $, HighwayBanditsSetupAction(f, h))

        case HighwayBanditMainPlaceAction(f, h, alt, then) =>
            Ask(f)
                .each(clearings.combinations(2).$./(l => l(0) -> l(1)).%((a, b) => game.connected(a).has(b))){ case (a, b) => HighwayBanditPlaceAction(f, h, a, b, then).as(a, dt.Arrow, b) }
                .add(alt)
                .needOk

        case HighwayBanditPlaceAction(f, h, a, b, then) =>
            h.reserve --> Bandit --> PathBetween(a, b)

            h.log("placed", Bandit.of(h), "on the path between", a, "and", b)

            then

        // FOREST PATROL
        case SetupHirelingAction(f, h : ForestPatrol.type) =>
            game.states += h -> new ForestPatrolState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Shuffle[StrayCat](ForestPatrol.pieces(options), ShuffledStrayCatsAction(f, h, _))

        case ShuffledStrayCatsAction(f, h, l) =>
            clearings.lazyZip(l).foreach { (c, p) =>
                h.reserve --> p --> c
            }

            h.log("placed", StrayCat.sof(h), "through the forest")

            SetupNextHirelingAction

        case ForestPatrolMainAction(f, h, m, b, r, p) =>
            Ask(f)
              .add(MovePatrolAction(f, m))
              .add(AttackPatrolAction(f, b))
              .add(r./(PlaceHirelingWarriorsAction(f, h, h.hospital.pieces, _, UsedEffectAction(f, h, Repeat))))
              .cancel

        case MovePatrolAction(f, l) =>
            MoveInitAction(f, ForestPatrol, $, NoMessage, l, ForestPatrol.movable, $(CancelAction), MoveDonePatrolAction(f))

        case MoveDonePatrolAction(f) =>
            val l = clearings.%(ForestPatrol.canAttackIn)

            val done = UsedEffectAction(f, ForestPatrol, Repeat)

            BattleInitAction(f, ForestPatrol, NoMessage, l, $(done.as("Done")), done)

        case AttackPatrolAction(f, l) =>
            BattleInitAction(f, ForestPatrol, NoMessage, l, $(CancelAction), UsedEffectAction(f, ForestPatrol, Repeat))

        case PlaceHirelingWarriorsAction(f, ForestPatrol, l, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            ForestPatrol.hospital --> l --> c

            log(f, "placed", l./(_.of(ForestPatrol)).comma, "in", c)

            then

        case BattlePostHitInAction(b, e, f, p : StrayCat, then) =>
            e.log("intercepted", p.name.styled(f))

            then

        case ForcedRemoveTargetEffectAction(e, c, f : ForestPatrol.type, p : StrayCat, then) =>
            f.limbo(c) --> p --> f.hospital

            then

        // LAST DYNASTY
        case SetupHirelingAction(f, h : LastDynasty.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(clearings.diff(board.inner))(PlaceHirelingWarriorsAction(f, h, 5.times(Vulture), _, SetupNextHirelingAction))

        case LastDynastyMainAction(f, h, c) =>
            if (c.none)
                Ask(f).each(clearings.diff(board.inner))(LastDynastyRecruitAction(f, h, _)).cancel
            else
            if (h.rules(c.get))
                MoveInitAction(f, h, game.transports./($) ** h.transports ** $($(AllWarriors)), NoMessage, c.$, h.movable, $, BattleInitAction(f, h, NoMessage, $, $, UsedEffectAction(f, h, Repeat)))
            else
                BattleInitAction(f, h, NoMessage, c.$, $(CancelAction), BattleInitAction(f, h, NoMessage, c.$, $, UsedEffectAction(f, h, Repeat)))

        case MoveFinishedAction(f, from, to : Clearing, BattleInitAction(ff, h : LastDynasty.type, m, Nil, extra, then)) =>
            MoveFinishedAction(f, from, to, BattleInitAction(ff, h, m, $(to), extra, then))

        case LastDynastyRecruitAction(f, h, c) =>
            h.reserve --> c

            h.log("respawned in", c)

            BattleInitAction(f, h, NoMessage, $(c), $, UsedEffectAction(f, h, Repeat))

        case BattlePostHitInAction(b, e, f, p @ Vulture, then) =>
            e.log("baited", p.name.styled(f))

            then

        // SPRING UPRISING
        case SetupHirelingAction(f, h : SpringUprising.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Roll[BaseSuit]($(BaseSuitDie, BaseSuitDie), l => UprisingSetupWarriorsAction(f, h, l, $, SetupNextHirelingAction), f.elem ~ " rolls two uprising dice")

        case UprisingSetupWarriorsAction(f, h, l, done, then) =>
            h.log("rolled", l./(_.elem).comma)

            PlaceUprisingWarriorsAction(f, h, l, done, then)

        case PlaceUprisingWarriorsAction(f, h, l, done, then) if l.num == done.num =>
            then

        case PlaceUprisingWarriorsAction(f, h, l, done, then) =>
            val rest = l.diff(done./~(_.suits.single))

            Ask(f).each(clearings)(c => PlaceHirelingWarriorAction(f, SpringUprising, Animal, c, PlaceUprisingWarriorsAction(f, h, l, done :+ c, then)).!(rest.exists(_.matches(c.cost)).not))//.cancel

        case SpringUprisingMainAction(f, h) =>
            Roll[BaseSuit]($(BaseSuitDie), l => SpringUprisingAction(f, h, l(0)), f.elem ~ " rolls an uprising die")

        case SpringUprisingAction(f, h, s) =>
            h.log("rolled", s)

            val l = clearings.%(_.cost.matched(s))
            val u = l.%(h.present)
            val n = u.%(c => h.enemies.%(f.canRemove(c)).%(_.present(c)).any)

            f.used :+= SpringUprising

            Ask(f)
                .each(n)(c => SpringUprisingNukeAction(f, h, c))
                .each(u.diff(n))(c => SpringUprisingRemoveAction(f, h, c))
                .each(l)(c => PlaceHirelingWarriorAction(f, h, h.warrior, c, Repeat).!(f.canPlace(c).not, "can't place").!(h.pool(h.warrior).not, "maximum"))
                .needOk
                .bailout(Repeat.as("Skip"))

        case SpringUprisingNukeAction(f, h, c) =>
            h.log("started an", "Uprising".styled(h), "in", c)

            NukeAction(h, h.enemies.%(h.canRemove(c)), $(c), NukeType.ClearSector, ForceAction(SpringUprisingRemoveAction(f, h, c)))

        case SpringUprisingRemoveAction(f, h, c) =>
            h.from(c) --> h.warrior --> game.recycle

            h.log("removed", h.warrior.of(h), "from", c)

            Repeat

        case OnTakeHirelingAction(f, h : SpringUprising.type, then) =>
            if (h.presence.none)
                Roll[BaseSuit]($(BaseSuitDie), l => UprisingSetupWarriorsAction(f, h, l, $, then), f.elem ~ " rolls an uprising die")
            else
                then

        case BattlePostHitInAction(b, e, f, p @ Animal, then) =>
            e.log("kicked", p.name.styled(f))

            then

        // THE EXILE
        case SetupHirelingAction(f, h : TheExile.type) =>
            game.states += h -> new TheExileState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(board.forests)(StartingHirelingForestAction(f, h, _))

        case StartingHirelingForestAction(f, h : TheExile.type, r) =>
            h.pawn --> r

            h.log("started in", board.forestName(r))

            SetupNextHirelingAction

        case AidTheExileMainAction(f, h) =>
            YYSelectObjectsAction(f, f.forTrade./(ToDiscard))
                .withGroup("Aid " ~ TheExile.elem)
                .withThen(i => AidTheExileAction(f, h, i.ref))(i => "Aid " ~ TheExile.elem ~ " with " ~ i.ref.elem)("Aid " ~ TheExile.elem ~ " with")
                .withExtra($(CancelAction))

        case AidTheExileAction(f, h, i) =>
            f.forTrade :-= i

            if (i.exhausted)
                h.exhausted :+= i.item
            else
                h.ready :+= i.item

            f.oscore(1)("aiding", h, "with", i)

            DrawCardsAction(f, 1, Aiding, AddCardsAction(f, Repeat))

        case TheExileMainAction(f, h) =>
            Ask(f)
                .add(TheExileMoveAction(f, h).!(h.ready.num < 1))
                .add(TheExileBattleAction(f, h).!(h.ready.num < 2))
                .doneIf(f.used.has(TheExile))(TheExileDoneAction(f, h))
                .cancelIf(f.used.has(TheExile).not)

        case TheExileMoveAction(f, h) =>
            MoveInitAction(f, h, h.transports, NoMessage, $(h.region), h.movable, $, TheExileExhaustAction(f, h, 1, ForceAction(TheExileMainAction(f, h))))

        case TheExileBattleAction(f, h) =>
            BattleInitAction(f, h, NoMessage, board.fromForest(h.region), $, TheExileExhaustAction(f, h, 2, ForceAction(TheExileMainAction(f, h))))

        case move @ MoveListAction(self, f, t, m, from, to, l, then : TheExileExhaustAction) =>
            then.copy(then = ForceAction(move.copy(then = then.then)))

        case battle @ BattleStartAction(self, f, a, m, c, o, i, then : TheExileExhaustAction) =>
            then.copy(then = battle.copy(then = then.then))

        case TheExileExhaustAction(f, h, n, then) =>
            val e = min(n, h.ready.num)
            val d = n - e

            val el = h.ready.take(e)
            h.ready = h.ready.drop(e)
            h.exhausted = h.exhausted ++ el

            h.log("exhausted", el)

            if (d > 0) {
                val dl = h.exhausted.takeRight(d)
                h.exhausted = h.exhausted.dropRight(d)

                h.log("dropped", dl)
            }

            f.used :+= TheExile

            then

        case BattleAssignHitsAction(h : TheExile.type, b, n, _, then) =>
            TheExileExhaustAction(h.owner | game.current, h, n, then)

        case TheExileDoneAction(f, h) =>
            h.ready = h.exhausted ++ h.ready
            h.exhausted = $

            Repeat

        // RIVERFOLK FLOTILLА
        case SetupHirelingAction(f, h : RiverfolkFlotilla.type) =>
            game.states += h -> new PawnHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(clearings.diff(board.inner).intersect(game.riverside))(PlaceHirelingWarriorAction(f, h, Flotilla, _, SetupNextHirelingAction)).needOk

        case RiverfolkFlotillaAction(f, h : RiverfolkFlotilla.type) =>
            val c = h.all(Flotilla).only
            val l = factions.%(_.present(c))

            f.used :+= RiverfolkFlotilla

            l.foldLeft(RiverfolkFlotillaMoveAction(f, h) : ForcedAction)((q, e) => DrawCardsAction(e, 1, WithEffect(RiverfolkFlotilla), AddCardsAction(e, q)))

        case RiverfolkFlotillaMoveAction(f, h : RiverfolkFlotilla.type) =>
            val from = h.all(Flotilla)
            val plans = h.movePlans(from.%(h.canMoveFrom), game.transports./($) ** h.transports)

            val l = from./(o => plans.get(o) @@ {
                case Some((t, l)) => MoveFromAction(f, h, t, JustElem(RiverfolkFlotilla), o, l, RiverfolkFlotillaBattleAction(f, h))
                case None => MoveFromAction(f, h, $, JustElem(RiverfolkFlotilla), o, $, Repeat).!(f.canMoveFrom(o).not, "snared").!(true)
            })

            Ask(f)(l)

        case RiverfolkFlotillaBattleAction(f, h : RiverfolkFlotilla.type) =>
            BattleInitAction(f, h, NoMessage, h.all(Flotilla), $(Repeat.as("Skip", "Battle".hh)), Repeat)

        // WARM SUN PROPHETS
        case SetupHirelingAction(f, h : WarmSunProphets.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(game.ruins.keys.$.combinations(4).$)(l => l.foldLeft[ForcedAction](SetupNextHirelingAction)((then, c) => ForceAction(PlaceHirelingWarriorAction(f, h, h.warrior, c, then))).as(l.comma)(h)).needOk

        // CORVID SPIES
        case SetupHirelingAction(f, h : CorvidSpies.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(clearings.combinations(2).$.%(l => l(0).asset.matches(l(1).cost) || l(1).asset.matches(l(0).cost)))(l => l.foldLeft[ForcedAction](SetupNextHirelingAction)((then, c) => ForceAction(PlaceHirelingWarriorAction(f, h, h.warrior, c, then))).as(l.comma)(h)).needOk

        // SUNWARD EXPEDITION
        case SetupHirelingAction(f, h : SunwardExpedition.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f)
                .each(clearings)(c => PlaceHirelingWarriorsAction(f, h, $(h.warrior, h.warrior, h.warrior, Foothold), c, SetupNextHirelingAction))
                .needOk

        // FLAME BEARERS
        case SetupHirelingAction(f, h : FlameBearers.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            FlameBearersSetupAction(f, h)

        case FlameBearersSetupAction(f, h) =>
            if (h.all(h.warrior).num == 2)
                SetupNextHirelingAction
            else
                Ask(f)
                    .each(clearings)(c => PlaceHirelingWarriorAction(f, h, h.warrior, c, FlameBearersSetupAction(f, h)))
                    .needOk

        // VAULT KEEPERS
        case SetupHirelingAction(f, h : VaultKeepers.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f)
                .each(clearings.%(h.canBuild))(c => PlaceHirelingWarriorsAction(f, h, $(h.warrior, h.warrior, Vault), c, SetupNextHirelingAction))
                .needOk

        // SETUP
        case SetupHirelingAction(f, h : PawnHireling) =>
            game.states += h -> new PawnHirelingState(h)

            game.hirelings :+= h

            log(f, "setup pawn", h)

            SetupNextHirelingAction

        case SetupHirelingAction(f, h) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup basic", h)

            SetupNextHirelingAction

        // HELPERS
        case PlaceHirelingWarriorsAction(f, h, l, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            h.reserve --> l --> c

            log(f, "placed", l./(_.of(h)).comma, "in", c)

            then

        case PlaceHirelingWarriorAction(f, h, p, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            h.reserve --> p --> c

            log(f, "placed", p.of(h), "in", c)

            then

        // TURN
        case OnTakeHirelingAction(f, h, then) =>
            then

        case HireHirelingAction(f, h) =>
            f.effects :-= NewHireling

            game.unhired :-= h

            TakeHirelingAction(f, h, HirelingPurchasesAction(f))

        case _ => UnknownContinue
    }

}
