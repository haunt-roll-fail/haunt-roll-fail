package dwam
//
//
//
//
import logger._, colmat._
//
//
//
//

import hrf.tracker2._
import hrf.elem._

import dwam.elem._


trait Color extends Styling {
    def name = toString
    def short = name.take(1)
    def style = short.toLowerCase
}

trait Faction extends BasePlayer with Named with Color {
    def ss = name.take(1).styled(this)

}

case object Red extends Faction
case object Green extends Faction
case object Blue extends Faction
case object Yellow extends Faction
case object White extends Faction

trait NeutralColor extends Color
case object Troubles extends NeutralColor
case object Trolls extends NeutralColor
case object Demons extends NeutralColor

case class Area(index : Int, name : String, id : String, cost : Int)(e : Area => ScrollAction) extends Elementary {
    def effect = e(this)
    def elem = name.styled(styles.area)
    def img = Image("area:" + id, styles.card)
}

trait Board {
    val name : String
    def areas : List[Area]

    def starting : List[Area]

    def connected(c : Area) : List[Area]
    def flooded : List[Area]

    def indexed(i : Int) = areas.%(_.index == i).single.get

    lazy val distance = areas./(a => a -> areas./(b => b -> {
        var n = 0
        var l = $(a)
        while (!l.contains(b)) {
            n += 1
            l = l./~(connected)
        }
        n
    }).toMap).toMap
}

object StandardBoard extends Board {
    val name = "Standard Board"
 
    val DollySisters        = Area( 1, "Dolly Sisters", "dolly-sisters", 6)(SpreadMinionForN(_, 3))
    val UnrealEstate        = Area( 2, "Unreal Estate", "unreal-estate", 18)(_ => ScrollActions($(DrawCards(1), DiscardCards(1, 1))))
    val DragonsLanding      = Area( 3, "Dragon's Landing", "dragons-landing", 12)(_ => TakeN(2))
    val SmallGods           = Area( 4, "Small Gods", "small-gods", 18)(_ => IgnoreEventForN(3))
    val TheScours           = Area( 5, "The Scours", "the-scours", 6)(_ => DiscardCardTo(TakeN(2)))
    val TheHippo            = Area( 6, "The Hippo", "the-hippo", 12)(_ => TakeN(2))
    val TheShades           = Area( 7, "The Shades", "the-shades", 6)(SpreadTroubleFrom(_))
    val Dimwell             = Area( 8, "Dimwell", "dimwell", 6)(SpreadMinionForN(_, 3))
    val Longwall            = Area( 9, "Longwall", "longwall", 12)(_ => TakeN(1))
    val IsleOfGods          = Area(10, "Isle of Gods", "isle-of-gods", 12)(_ => RemoveTroubleForN(2))
    val SevenSleepers       = Area(11, "Seven Sleepers", "seven-sleepers", 18)(_ => TakeN(3))
    val NapHill             = Area(12, "Nap Hill", "nap-hill", 12)(_ => TakeN(1))
 
    val areas = $(
        DollySisters,
        NapHill,
        UnrealEstate,
        DragonsLanding,
        SevenSleepers,
        IsleOfGods,
        SmallGods,
        Longwall,
        TheScours,
        TheShades,
        TheHippo,
        Dimwell,
    )
    
    val flooded = areas.but(DragonsLanding).but(TheHippo)

    val starting = $(TheShades, TheScours, DollySisters)

    def connected(c : Area) = c match {
        case DollySisters   => $(NapHill, DragonsLanding, UnrealEstate)
        case UnrealEstate   => $(NapHill, DollySisters, DragonsLanding, SevenSleepers, SmallGods, IsleOfGods)
        case DragonsLanding => $(DollySisters, UnrealEstate, SmallGods)
        case SmallGods      => $(DragonsLanding, UnrealEstate, TheScours, TheHippo)
        case TheScours      => $(SmallGods, IsleOfGods, TheHippo, Dimwell, TheShades)
        case TheHippo       => $(SmallGods, TheScours, TheShades)
        case TheShades      => $(TheScours, TheHippo, Dimwell)
        case Dimwell        => $(TheScours, Longwall, TheShades)
        case Longwall       => $(SevenSleepers, IsleOfGods, Dimwell)
        case IsleOfGods     => $(SevenSleepers, UnrealEstate, TheScours, Longwall)
        case SevenSleepers  => $(NapHill, UnrealEstate, IsleOfGods, Longwall)
        case NapHill        => $(DollySisters, SevenSleepers, UnrealEstate)
    }
}


abstract class Character(val name : String, val id : String) extends Record {
    def img = Image("character:" + id, styles.card)
}

trait Lord { self : Character => }

case object Chrysoprase extends Character("Chrysoprase", "chrysoprase")
case object LordVetinari extends Character("Lord Vetinari", "lord-vetinari")
case object DragonKingOfArms extends Character("Dragon King of Arms", "dragon-king-of-arms")
case object CommanderVimes extends Character("Commander Vimes", "commander-vimes")
case object LordRust extends Character("Lord Rust", "lord-rust") with Lord
case object LordSelachii extends Character("Lord Selachii", "lord-selachii") with Lord
case object LordDeWorde extends Character("Lord de Worde", "lord-de-worde") with Lord

trait Claim[T <: Claim[T]] extends Elementary {
    def winning : Boolean
    def isBetter(c : T) : Boolean
    def elem : Elem
}

case class MonetaryClaim(money : Int, target : Int) extends Claim[MonetaryClaim] {
    def winning = money >= target
    def isBetter(c : MonetaryClaim) = money > c.money
    def elem : Elem = money.money ~ " out of needed " ~ target.money
}

case class RuleClaim(areas : Int, target : Int) extends Claim[RuleClaim] {
    def winning = areas >= target
    def isBetter(c : RuleClaim) = areas > c.areas
    def elem : Elem = areas.hl ~ " areas controlled out of needed " ~ target.hl ~ " areas"
}

case class SpyNetworkClaim(areas : Int, target : Int) extends Claim[SpyNetworkClaim] {
    def winning = areas >= target
    def isBetter(c : SpyNetworkClaim) = areas > c.areas
    def elem : Elem = areas.hl ~ " areas with spies out of needed " ~ target.hl ~ " areas"
}

case class TroubleClaim(areas : Int, target : Int) extends Claim[TroubleClaim] {
    def winning = areas >= target
    def isBetter(c : TroubleClaim) = areas > c.areas
    def elem : Elem = areas.hl ~ " areas with " ~ Trouble.elem ~ " out of needed " ~ target.hl ~ " areas"
}

case class EmptyDeckClaim(cards : Int) extends Claim[EmptyDeckClaim] {
    def winning = cards == 0
    def isBetter(c : EmptyDeckClaim) = cards < c.cards
    def elem : Elem = cards.cards ~ " in the deck"
}


case class ClaimRecord(c : Character, turn : Int, claim : Claim[_]) extends Elementary {
    def elem = c.name.hl ~ " ".pre ~ MDash ~ " Turn " ~ "#".hl ~ turn.hl ~ " ".pre ~ MDash ~ " " ~ claim.elem
}

object ClaimRecord {
    def better(aa : ClaimRecord, bb : ClaimRecord) = (aa.claim, bb.claim) match {
        case (a : MonetaryClaim,   b : MonetaryClaim  ) => $(a.isBetter(b).?(aa).|(bb))
        case (a : RuleClaim,       b : RuleClaim      ) => $(a.isBetter(b).?(aa).|(bb))
        case (a : SpyNetworkClaim, b : SpyNetworkClaim) => $(a.isBetter(b).?(aa).|(bb))
        case (a : TroubleClaim,    b : TroubleClaim   ) => $(a.isBetter(b).?(aa).|(bb))
        case (a : EmptyDeckClaim,  b : EmptyDeckClaim ) => $(a.isBetter(b).?(aa).|(bb))
        case _ => $(aa, bb)
    }
}



trait CardAction

case object PlayCard extends CardAction
case object PlaceMinion extends CardAction
case object Build extends CardAction
case class TakeMoney(n : Int) extends CardAction
case object Assassinate extends CardAction
case object RemoveTrouble extends CardAction
case object RandomEvent extends CardAction
case object PlayAnother extends CardAction
case class Scroll(action : ScrollAction) extends CardAction
case object RefillCards extends CardAction
case object TakeBackCheck extends CardAction
case object DiscardPlay extends CardAction

trait Effect extends Elementary {
    def elem : Elem = Text(toString)
}
case class RemoveMinionEffect(r : Area, f : Color) extends Effect {
    override def elem = Minion.of(f).elem ~ " removed from " ~ r.elem
}
case class RemoveBuildingEffect(r : Area, f : Faction) extends Effect {
    override def elem = Building.of(f).elem ~ " removed from " ~ r.elem
}
case class MoveAwayMinionEffect(r : Area, f : Faction, dest : List[Area]) extends Effect {
    override def elem = Minion.of(f).elem ~ " moved from " ~ r.elem
}
case class PlaceMinionEffect(r : Area, f : Color) extends Effect {
    override def elem = Minion.of(f).elem ~ " placed in " ~ r.elem
}
case class MalfunctionEffect(r : Area, f : Faction) extends Effect {
    override def elem = Building.of(f).elem ~ " malfunctioned in " ~ r.elem
}



abstract class ScrollAction(val elem : Elem) extends Record with Elementary

case object NIA extends ScrollAction("not implemented")

case class ScrollActions(l : List[ScrollAction]) extends ScrollAction(l./(_.elem).join(", "))
case class SoftScrollActions(l : List[ScrollAction]) extends ScrollAction(l./(_.elem).join(", ")) with SoftMark

case class StealNFromEveryone(n : Int) extends ScrollAction("Steal " ~ n.money ~ " from every player")
case class StealN(n : Int) extends ScrollAction("Steal " ~ n.money ~ " from a player") with SoftMark
case class TakeN(n : Int) extends ScrollAction("Take " ~ n.money)

case object TakeLoan extends ScrollAction("Take loan of " ~ 10.money ~ ", after game end " ~ -12.money)
case object HandPenalty extends ScrollAction("Hand penalty a card, unless another player gives " ~ 5.money) with SoftMark

case object MinionNoTrouble extends ScrollAction("Minion anywhere, don't place " ~ Trouble.elem) with SoftMark
case object SpreadTrouble extends ScrollAction("Place " ~ Trouble.elem ~ " near " ~ Trouble.elem) with SoftMark
case object PlaceTrouble extends ScrollAction("Place " ~ Trouble.elem) with SoftMark
case class SpreadTroubleFrom(r : Area) extends ScrollAction(Trouble.elem ~ " from " ~ r.elem) with SoftMark

case class SpreadMinion(r : Area) extends ScrollAction("Minion from " ~ r.elem) with SoftMark
case class RemoveMinion(r : Area) extends ScrollAction("Remove minion in " ~ r.elem) with SoftMark
case class SpreadMinionForN(r : Area, n : Int) extends ScrollAction("Minion from " ~ r.elem ~ " for " ~ n.money) with SoftMark
case object MinionAtBuilding extends ScrollAction("Place a minion at own building") with SoftMark
case object MinionAtTrouble extends ScrollAction("Place a minion at " ~ Trouble.elem) with SoftMark

case class DiscardCardTo(a : ScrollAction) extends ScrollAction("Discard card, " ~ a.elem) with SoftMark
case class DiscardCards(min : Int, max : Int) extends ScrollAction("Discard " ~ (min > 0 && min < max).??(min.hl ~ "-") ~ max.cards) with SoftMark
case class DrawCards(n : Int) extends ScrollAction("Take " ~ n.cards)
case object DrawCardsForBuildings extends ScrollAction("Take " ~ 1.cards ~ " for each building")

case class RemoveTroubleForN(n : Int) extends ScrollAction("Remove " ~ Trouble.elem ~ " for " ~ n.money) with SoftMark
case class IgnoreEventForN(n : Int) extends ScrollAction("Ignore event effect for " ~ n.money)

case class AnotherPlayerGivesNCards(n : Int) extends ScrollAction("Another player gives " ~ n.hl ~ " cards") with SoftMark
case class OtherPlayersGiveCardOrN(n : Int) extends ScrollAction("Other player give a card or " ~ n.money)
case class GiveCardTakeN(n : Int) extends ScrollAction("Give card, take " ~ n.money) with SoftMark
case class StealNCardsTakeOne(n : Int) extends ScrollAction("Steal randomly " ~ n.hl ~ " cards, take one") with SoftMark
case object DiscardEnemyCard extends ScrollAction("Discard another player's card") with SoftMark


case class EarnForMinionsIn(r : Area, k : Int) extends ScrollAction("Earn " ~ k.money ~ " for each minion in " ~ r.elem)
case class EarnForTrouble(k : Int) extends ScrollAction("Earn " ~ k.money ~ " for each " ~ Trouble.elem)
case class EarnForBuildings(k : Int) extends ScrollAction("Earn " ~ k.money ~ " for each building on board")
case class ScoreMinionsInTrouble(k : Int) extends ScrollAction("Earn " ~ k.money ~ " for each minion in an area with " ~ Trouble.elem) with SoftMark
case class DiscardCardsForMoney(k : Int) extends ScrollAction("Discard cards for " ~ k.money ~ " each") with SoftMark
case class RollToTakeNOrRemoveMinion(n : Int) extends ScrollAction("Roll die to take " ~ n.money ~ " from another player or remove own minion")
case object RollToRemoveMinion extends ScrollAction("Remove another player's or own minion")
case object OthersRemoveMinions extends ScrollAction("Make other players remove own minions")
case class DrawFromDiscard(n : Int) extends ScrollAction("Take randomly " ~ n.cards ~ " from discard")
case object ExchangeHands extends ScrollAction("Exchange hands") with SoftMark
case object RefillHand extends ScrollAction("Refill hand")

case object RerollCharacter extends ScrollAction("Change " ~ "Personality".hl)
case object ViewUnusedCharacters extends ScrollAction("View unused " ~ "Personalities".hl)

case object ExchangeMinions extends ScrollAction("Exchange minions") with SoftMark
case object MoveMinionAnywhere extends ScrollAction("Move minion anywhere") with SoftMark
case object MoveMinionFromTrouble extends ScrollAction("Move minion from " ~ Trouble.elem) with SoftMark
case object MoveEnemyMinion extends ScrollAction("Move another player's minion") with SoftMark
case object FireBrigade extends ScrollAction("Send " ~ "Fire Brigade".hl) with SoftMark
case object BusinessPlan extends ScrollAction("Get " ~ 4.money ~ " or lose " ~ 2.money ~ " or minion")
case object DoubleAction extends ScrollAction("Play two cards")
case object BuildHalfPrice extends ScrollAction("Build half price") with SoftMark
case class BuyBuilding(trouble : Boolean) extends ScrollAction("Buy another players building") with SoftMark
case class PayNToMove(n : Int) extends ScrollAction("Pay another player " ~ n.money ~ " to move minion") with SoftMark
case class PayNToAssassinate(n : Int) extends ScrollAction("Pay another player " ~ n.money ~ " to assassinate") with SoftMark
case class PayNToMakeAssassinate(n : Int) extends ScrollAction("Pay another player " ~ n.money ~ " to make assassinate") with SoftMark
case class ForcePay(n : Int) extends ScrollAction("Force another player give " ~ n.money ~ " another player") with SoftMark
case class RollNKill(n : Int) extends ScrollAction("Roll " ~ n.hl ~ " times and remove minions")



abstract class DeckCard(val name : String, effects : CardAction*) extends Elementary with Record {
    def id = name.toLowerCase.replace(' ', '-').filter(c => c.isLetter || c == '-')
    def actions = effects.toList
    def img = Image("card:" + id, styles.card)
    def elem = (" " + name + " ").pre.spn(xstyles.outlined)(Deck.green.has(this).?(styles.green))(Deck.brown.has(this).?(styles.brown))
}

trait Interrupt extends DeckCard

case object NoCard extends DeckCard("No Card")

case object MrBent extends DeckCard("Mr Bent", Scroll(TakeLoan), PlayAnother)
case object TheBankOfAnkhMorpork extends DeckCard("The Bank of Ankh-Morpork", Scroll(TakeLoan), PlayAnother)
case object TheBeggarsGuild extends DeckCard("The Beggars' Guild",  Scroll(AnotherPlayerGivesNCards(2)), PlaceMinion)
case object TheAnkhMorporkSunshineDragonSanctuary extends DeckCard("The Ankh-Morpork Sunshine Dragon Sanctuary", Scroll(OtherPlayersGiveCardOrN(1)), PlayAnother)
case object SergeantAngua extends DeckCard("Sergeant Angua", RemoveTrouble, PlayAnother)
case object TheAgonyAunts extends DeckCard("The Agony Aunts", Assassinate, TakeMoney(2), PlaceMinion)
case object TheSeamstressesGuild extends DeckCard("The Seamstresses' Guild", Scroll(GiveCardTakeN(2)), PlaceMinion)
case object MrPinMrTulip extends DeckCard("Mr Pin & Mr Tulip", Assassinate, TakeMoney(1)) { override val id = "mr-pin-mr-tulip" }
case object TheThievesGuild extends DeckCard("The Thieves' Guild", Scroll(StealNFromEveryone(2)), PlaceMinion)
case object WallaceSonky extends DeckCard("Wallace Sonky") with Interrupt
case object DrWhiteface extends DeckCard("Dr Whiteface", Scroll(HandPenalty), PlaceMinion)
case object ZorgoTheRetrophrenologist extends DeckCard("Zorgo the Retro-phrenologist", Scroll(RerollCharacter), Build)
case object QueenMolly extends DeckCard("Queen Molly", PlaceMinion, Scroll(AnotherPlayerGivesNCards(2)))
case object TheRoyalMint extends DeckCard("The Royal Mint", Build, TakeMoney(5))
case object Rincewind extends DeckCard("Rincewind", RandomEvent, Scroll(MoveMinionFromTrouble), PlayAnother)
case object RosiePalm extends DeckCard("Rosie Palm", PlaceMinion, Scroll(GiveCardTakeN(2)))
case object SacharissaCripslock extends DeckCard("Sacharissa Cripslock", Scroll(EarnForTrouble(1)), PlaceMinion)
case object ShonkyShop extends DeckCard("Shonky Shop", Scroll(DiscardCardsForMoney(1)), Build)
case object TheMendedDrum extends DeckCard("The Mended Drum", Build, TakeMoney(2))
case object Modo extends DeckCard("Modo", Scroll(DiscardCards(0, 1)), PlaceMinion)
case object NobbyNobbs extends DeckCard("Nobby Nobbs", Scroll(StealN(3)), PlayAnother)
case object TheOperaHouse extends DeckCard("The Opera House", Build, Scroll(EarnForMinionsIn(StandardBoard.IsleOfGods, 1)))
case object ThePeeledNuts extends DeckCard("The Peeled Nuts")
case object PinkPussyCatClub extends DeckCard("Pink Pussy Cat Club", TakeMoney(3), PlayAnother)
case object HereNNow extends DeckCard("Here'n'Now", Scroll(RollToTakeNOrRemoveMinion(3)), PlayAnother) { override val id = "here-n-now" }
case object Hex extends DeckCard("Hex", Scroll(DrawCards(3)), Build)
case object HistoryMonks extends DeckCard("History Monks", Scroll(DrawFromDiscard(4)), PlaceMinion)
case object InigoSkimmer extends DeckCard("Inigo Skimmer", Assassinate, TakeMoney(2))
case object LeonardOfQuirm extends DeckCard("Leonard of Quirm", Scroll(DrawCards(4)))
case object Librarian extends DeckCard("Librarian", Scroll(DrawCards(4)))
case object Gaspode extends DeckCard("Gaspode") with Interrupt
case object GimletsDwarfDelicatessen extends DeckCard("Gimlet's Dwarf Delicatessen", TakeMoney(3), PlaceMinion)
case object Groat extends DeckCard("Groat", PlaceMinion)
case object MrGryle extends DeckCard("Mr Gryle", Assassinate, TakeMoney(1))
case object HargasHouseOfRibs extends DeckCard("Harga's House of Ribs", TakeMoney(3), PlaceMinion)
case object HarryKing extends DeckCard("Harry King", PlaceMinion, Scroll(DiscardCardsForMoney(2)))
case object TheDuckman extends DeckCard("The Duckman", Scroll(MoveEnemyMinion))
case object TheDysk extends DeckCard("The Dysk", Build, Scroll(EarnForMinionsIn(StandardBoard.IsleOfGods, 1)))
case object TheFireBrigade extends DeckCard("The Fire Brigade", Scroll(FireBrigade), PlayAnother)
case object TheFoolsGuild extends DeckCard("The Fools' Guild", Scroll(HandPenalty), PlaceMinion)
case object FoulOleRon extends DeckCard("Foul Ole Ron", Scroll(MoveEnemyMinion), PlayAnother)
case object FreshStartClub extends DeckCard("Fresh Start Club") with Interrupt
case object MrBoggis extends DeckCard("Mr Boggis", Scroll(StealNFromEveryone(2)), PlaceMinion)
case object MrsCake extends DeckCard("Mrs Cake", Scroll(ViewUnusedCharacters), TakeMoney(2), Build)
case object CaptainCarrot extends DeckCard("Captain Carrot", PlaceMinion, RemoveTrouble, TakeMoney(1))
case object DrCruces extends DeckCard("Dr Cruces", Assassinate, TakeMoney(3))
case object CMOTDibbler extends DeckCard("CMOT Dibbler", Scroll(BusinessPlan), PlayAnother)
case object Drumknott extends DeckCard("Drumknott", Scroll(DoubleAction))

case object SybilVimes extends DeckCard("Sybil Vimes", TakeMoney(3), Scroll(BuyBuilding(false)))
case object MrTeatime extends DeckCard("Mr Teatime", TakeMoney(3), Assassinate, PlayAnother)
case object TheWatch extends DeckCard("The Watch", Build, RemoveTrouble)
case object WeeMadArthur extends DeckCard("Wee Mad Arthur", Scroll(BuildHalfPrice))
case object WilliamDeWorde extends DeckCard("William de Worde", PlaceMinion, Scroll(EarnForTrouble(1)))
case object Willikins extends DeckCard("Willikins", Scroll(MinionAtBuilding))
case object TheSeniorWrangler extends DeckCard("The Senior Wrangler", RandomEvent, Scroll(SpreadMinion(StandardBoard.UnrealEstate)), PlayAnother)
case object MrShine extends DeckCard("Mr Shine", Scroll(MinionNoTrouble))
case object MrSlant extends DeckCard("Mr Slant", Scroll(ScoreMinionsInTrouble(2)), Build)
case object TheSmokingGnu extends DeckCard("The Smoking Gnu", Scroll(MinionAtTrouble), PlayAnother)
case object Stanley extends DeckCard("Stanley", Scroll(StealNCardsTakeOne(2)), PlaceMinion)
case object Susan extends DeckCard("Susan") with Interrupt
case object PonderStibbons extends DeckCard("Ponder Stibbons", RandomEvent, Scroll(DoubleAction))
case object ThePostOffice extends DeckCard("The Post Office", Scroll(EarnForBuildings(1)), PlaceMinion)
case object ReacherGilt extends DeckCard("Reacher Gilt", Scroll(BuyBuilding(true)))
case object ProfessorOfRecentRunes extends DeckCard("Professor of Recent Runes", RandomEvent, Scroll(DrawCards(2)), PlayAnother)
case object ArchchancellorRidcully extends DeckCard("Archchancellor Ridcully", RandomEvent, Scroll(SpreadMinion(StandardBoard.UnrealEstate)), Scroll(SpreadMinion(StandardBoard.UnrealEstate)))
case object Ruby extends DeckCard("Ruby", PlaceMinion, Build)
case object Igor extends DeckCard("Igor") with Interrupt
case object TheLuggage extends DeckCard("The Luggage", Assassinate, Scroll(DiscardCards(0, 1)))
case object TheMob extends DeckCard("The Mob", Scroll(SpreadTrouble), PlaceMinion, PlayAnother)
case object MoistVonLipwig extends DeckCard("Moist von Lipwig", PlaceMinion, TakeMoney(3), Scroll(DrawCards(2)), PlayAnother)
case object DoctorMossyLawn extends DeckCard("Doctor Mossy Lawn") with Interrupt
case object PatriciansPalace extends DeckCard("Patrician's Palace", Build, TakeMoney(4), PlaceMinion)
case object EdwardDEath extends DeckCard("Edward d'Eath", Assassinate, TakeMoney(3), Build)
case object Errol extends DeckCard("Errol", Scroll(RollToRemoveMinion), PlayAnother)
case object Gargoyles extends DeckCard("Gargoyles", Scroll(DrawCardsForBuildings), Build)
case object DoctorHix extends DeckCard("Doctor Hix", RandomEvent, Scroll(PlaceTrouble), PlayAnother)
case object HobsonssLiveryStable extends DeckCard("Hobsons's Livery Stable", Scroll(PayNToMove(2)), Build)
case object Hubert extends DeckCard("Hubert", Scroll(ForcePay(3)), PlaceMinion)
case object Death extends DeckCard("DEATH", Assassinate, Assassinate, Build)
case object DeepDwarves extends DeckCard("Deep Dwarves", Scroll(MinionNoTrouble), PlayAnother)
case object SergeantDetritus extends DeckCard("Sergeant Detritus", RemoveTrouble, RemoveTrouble)
case object Dorfl extends DeckCard("Dorfl", Scroll(MoveMinionAnywhere), PlayAnother)
case object LordDowney extends DeckCard("Lord Downey", Assassinate, TakeMoney(3), Build)
case object Dwarves extends DeckCard("Dwarves", PlaceMinion, PlaceMinion)
case object SergeantCheeryLittlebottom extends DeckCard("Sergeant Cheery Littlebottom", Scroll(DrawCards(2)), RemoveTrouble)
case object OttoChriek extends DeckCard("Otto Chriek", Scroll(EarnForTrouble(1)), Build)
case object TheClacks extends DeckCard("The Clacks", Scroll(DrawCards(2)), TakeMoney(3), PlayAnother)
case object SergeantColon extends DeckCard("Sergeant Colon", RemoveTrouble, PlaceMinion)
case object CosmoLavish extends DeckCard("Cosmo Lavish", Scroll(PayNToMakeAssassinate(2)), PlayAnother)
case object TheDean extends DeckCard("The Dean", RandomEvent, Scroll(RemoveMinion(StandardBoard.UnrealEstate)), PlayAnother)
case object TheBursar extends DeckCard("The Bursar", RandomEvent, Scroll(ExchangeMinions), PlayAnother)
case object CableStreetParticulars extends DeckCard("Cable Street Particulars", Scroll(DiscardEnemyCard), PlaceMinion)
case object CantingCrew extends DeckCard("Canting Crew", Scroll(MoveEnemyMinion), PlaceMinion)
case object Carcer extends DeckCard("Carcer", Scroll(RollNKill(2)), PlayAnother)
case object TheChairOfIndefiniteStudies extends DeckCard("The Chair of Indefinite Studies", RandomEvent, Scroll(ExchangeHands), PlayAnother)
case object SirCharlesLavatory extends DeckCard("Sir Charles Lavatory", Scroll(EarnForBuildings(1)), Build)
case object AdoraBelleDearheart extends DeckCard("Adora Belle Dearheart", Scroll(MoveMinionAnywhere), PlaceMinion, Build)
case object TheAlchemistsGuild extends DeckCard("The Alchemists' Guild", Scroll(SoftScrollActions($(DiscardCards(0, 3), RefillHand))), Build)
case object TheAuditors extends DeckCard("The Auditors", Scroll(OthersRemoveMinions))
case object BuggySwires extends DeckCard("Buggy Swires", RemoveTrouble)
case object BurleighStronginthearm extends DeckCard("Burleigh & Stronginthearm", Scroll(PayNToAssassinate(2)), Build) { override val id = "burleigh-stronginthearm" }

            
abstract class EventCard(val name : String) extends Elementary with Record {
    def id = name.toLowerCase.replace(' ', '-').filter(c => c.isLetter || c == '-')
    def img = Image("event:" + id, styles.card)
    def elem = name.hl
}

case object Fire extends EventCard("Fire")
case object Fog extends EventCard("Fog")
case object MysteriousMurders extends EventCard("Mysterious Murders")
case object Riots extends EventCard("Riots")
case object Subsidence extends EventCard("Subsidence")
case object TheTrolls extends EventCard("Trolls")
case object BloodyStupidJohnson extends EventCard("Bloody Stupid Johnson")
case object DemonsFromTheDungeonDimensions extends EventCard("Demons from the Dungeon Dimensions")
case object TheDragon extends EventCard("The Dragon")
case object Earthquake extends EventCard("Earthquake")
case object Explosion extends EventCard("Explosion")
case object Flood extends EventCard("Flood")
                           

case object Deck {
    val events : List[EventCard] =
        if (hrf.HRF.flag("test"))
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood ::
            Flood
        else
            Fog ::
            MysteriousMurders ::
            Riots ::
            Subsidence ::
            TheTrolls ::
            BloodyStupidJohnson ::
            DemonsFromTheDungeonDimensions ::
            TheDragon ::
            Earthquake ::
            Explosion ::
            Flood
        
    def forPlayers(n : Int) = debug ++ (green ++ brown).diff((n <= 2).??($(Hubert, CosmoLavish)))

    def full : List[DeckCard] = green ++ brown
    
    val debug : List[DeckCard] = (WallaceSonky.repeat(12)).take(hrf.HRF.flag("test").??(12))
    
    val green : List[DeckCard] =
        MrBent ::
        TheBankOfAnkhMorpork ::
        TheBeggarsGuild ::
        TheAnkhMorporkSunshineDragonSanctuary ::
        SergeantAngua ::
        TheAgonyAunts ::
        TheSeamstressesGuild ::
        MrPinMrTulip ::
        TheThievesGuild ::
        WallaceSonky ::
        DrWhiteface ::
        ZorgoTheRetrophrenologist ::
        QueenMolly ::
        TheRoyalMint ::
        Rincewind ::
        RosiePalm ::
        SacharissaCripslock ::
        ShonkyShop ::
        TheMendedDrum ::
        Modo ::
        NobbyNobbs ::
        TheOperaHouse ::
        ThePeeledNuts ::
        PinkPussyCatClub ::
        HereNNow ::
        Hex ::
        HistoryMonks ::
        InigoSkimmer ::
        LeonardOfQuirm ::
        Librarian ::
        Gaspode ::
        GimletsDwarfDelicatessen ::
        Groat ::
        MrGryle ::
        HargasHouseOfRibs ::
        HarryKing ::
        TheDuckman ::
        TheDysk ::
        TheFireBrigade ::
        TheFoolsGuild ::
        FoulOleRon ::
        FreshStartClub ::
        MrBoggis ::
        MrsCake ::
        CaptainCarrot ::
        DrCruces ::
        CMOTDibbler ::
        Drumknott
        
    val brown : List[DeckCard] =
        SybilVimes ::
        MrTeatime ::
        TheWatch ::
        WeeMadArthur ::
        WilliamDeWorde ::
        Willikins ::
        TheSeniorWrangler ::
        MrShine ::
        MrSlant ::
        TheSmokingGnu ::
        Stanley ::
        Susan ::
        PonderStibbons ::
        ThePostOffice ::
        ReacherGilt ::
        ProfessorOfRecentRunes ::
        ArchchancellorRidcully ::
        Ruby ::
        Igor ::
        TheLuggage ::
        TheMob ::
        MoistVonLipwig ::
        DoctorMossyLawn ::
        PatriciansPalace ::
        EdwardDEath ::
        Errol :: 
        Gargoyles ::
        DoctorHix ::
        HobsonssLiveryStable ::
        Hubert ::
        Death ::
        DeepDwarves ::
        SergeantDetritus ::
        Dorfl ::
        LordDowney ::
        Dwarves ::
        SergeantCheeryLittlebottom ::
        OttoChriek ::
        TheClacks ::
        SergeantColon ::
        CosmoLavish ::
        TheDean ::
        TheBursar ::
        CableStreetParticulars ::
        CantingCrew ::
        Carcer ::
        TheChairOfIndefiniteStudies ::
        SirCharlesLavatory ::
        AdoraBelleDearheart ::
        TheAlchemistsGuild ::
        TheAuditors ::
        BuggySwires ::
        BurleighStronginthearm

    require(green.num == 48)
    require(brown.num == 53)
}

trait Piece extends Record {
    def name = toString
    def plural = name + "s"
}

trait PieceOf {
    def faction : Color
    def piece : Piece
}

case object Trouble extends Piece with PieceOf with Elementary {
    def faction = Troubles
    def piece = this
    def elem = name.styled(Troubles)
}

case object Building extends Piece {
    def of(f : Faction) = SomePieceOf(f, this)
}

case object Minion extends Piece {
    def of(f : Color) = SomePieceOf(f, this)
}

case class SomePieceOf(faction : Color, piece : Piece) extends PieceOf with Elementary {
    def elem = piece match {
        case Building => (faction.name + " Building").styled(faction)
        case Minion => faction match {
            case faction : Faction => (faction.name + " Minion").styled(faction)
            case Trolls => "Troll".styled(faction)
            case Demons => "Demon".styled(faction)
        }
    }
}


object Piece {
    implicit class PieceCast(val p : Piece) {
        def building = p match {
            case Building => Some(Building)
            case _ => None
        }
        def minion = p match {
            case Minion => Some(Minion)
            case _ => None
        }
        def trouble = p match {
            case Trouble => Some(Trouble)
            case _ => None
        }
    }
}
  

case class Figure(faction : Color, piece : Piece, index : Int) {
    override def toString = faction + "/" + piece + "/" + index
    def sp = SomePieceOf(faction, piece)
}

object Figure {
    implicit class FiguresEx[T](val t : T)(implicit val conv : T => List[Figure]) {
        def l = conv(t)
        def of(f : Color) = l.%(_.faction == f)
        def one(p : Piece) = l.%(_.piece == p).first
        def colors = l./(_.faction).distinct
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

        def minion = l.%(u => u.piece.minion.any)
        def building = l.%(u => u.piece.building.any)
        def trouble = l.%(u => u.piece.trouble.any)
    }
    
}


object D12 extends Die[Int]($(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

                            
                            
case class DrewFromTheDeck(f : Faction, m : Message with GameElementary) extends Message {
    def elem(g : Game) = f.elem ~ " drew from the deck" ~ m.elem(g)
}

case class DrewFromTheDiscard(f : Faction, m : Message with GameElementary) extends Message {
    def elem(g : Game) = f.elem ~ " drew from the discard" ~ m.elem(g)
}

case class ExchangedHand(f : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " exchanged hand"
}

case class DrewChar(f : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " drew " ~ "Personality".hl
}

case class TriggeredRandomEvent(f : Faction, e : EventCard) extends Message {
    def elem(g : Game) = e.name.hl
}

case class DrewNewChar(f : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " drew new " ~ "Personality".hl
}

case object UnusedChars extends Message {
    def elem(g : Game) = "Unused " ~ "Personalities".hl ~ "bar one"
}

case class ForN(n : Int) extends Message {
    def elem(g : Game) = "for " ~ n.money
}

case class TookFrom(f : Faction, e : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " took from " ~ e.elem
}

case class ReturnedTo(f : Faction, e : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " returned to " ~ e.elem
}

case class GaveTo(f : Faction, e : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " gave " ~ e.elem
}

case object DiscardCards extends Message {
    def elem(g : Game) = "Discard"
}



trait ViewCard extends ViewObject[DeckCard] { self : UserAction => 
    def d : DeckCard
    def obj = d
}

trait ViewChar extends ViewObject[Character] { self : UserAction => 
    def c : Character
    def obj = c
}

trait ViewArea extends ViewObject[Area] { self : UserAction => 
    def r : Area
    def obj = r
}

trait ViewEvent extends ViewObject[EventCard] { self : UserAction => 
    def e : EventCard
    def obj = e
}



case object StartAction extends StartGameAction
case class ShufflePileAction(shuffled : List[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]
case class ShuffleCharactersAction(shuffled : List[Character]) extends ShuffledAction[Character]
case class ShuffleEventCardsAction(shuffled : List[EventCard]) extends ShuffledAction[EventCard]

case object SetupNextAction extends ForcedAction

case class FactionInitAction(f : Faction) extends ForcedAction
case object InitDoneAction extends ForcedAction
case class StartPlayerTurnAction(f : Faction) extends ForcedAction
case class CheckWinAction(f : Faction) extends ForcedAction
case class ContinuePlayerTurnAction(f : Faction) extends ForcedAction
case class RefillAction(self : Faction, then : ForcedAction) extends BaseAction(None)("End turn")
case class CompleteActionTurnAction(f : Faction) extends ForcedAction
case class EndPlayerTurnAction(f : Faction) extends ForcedAction

case class GameOverAction(winners : List[Faction]) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won", "(" ~ NameReference(f.name, f).hl ~ ")")

case class DrawCardsAction(f : Faction, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class DrawingCardsAction(f : Faction, n : Int, l : List[DeckCard], m : Message, then : ForcedAction) extends ForcedAction
case class AddCardsAction(f : Faction, then : ForcedAction) extends ForcedAction

case class PlayCardMainAction(self : Faction) extends ForcedAction with Soft
case class CantPlayMainAction(self : Faction) extends BaseAction(Comment("Skip"))("Skip playing a card")

case class PlayCardAction(f : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class InterruptCardAction(f : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction

case class TakeMoneyMainAction(self : Faction, d : DeckCard, n : Int) extends BaseAction(d.name.hl)("Take", n.money)
case class SkipActionMainAction(self : Faction) extends BaseAction(Comment("Skip"))("Skip action")

case class AssassinateMainAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(d.name.hl)("Assassinate") with Soft
case class AssassinateAction(self : Faction, e : Color, r : Area, text : Boolean, then : ForcedAction) extends BaseAction("Assassinate")(Minion.of(e), "in", r)

case class RemoveTroubleMainAction(self : Faction, d : DeckCard, m : Message, then : ForcedAction) extends BaseAction(d.name.hl)("Remove", Trouble, m) with Soft
case class RemoveTroubleAction(self : Faction, r : Area, m : Message, then : ForcedAction) extends BaseAction("Remove", Trouble, m, "in")(r)


case class PlaceMinionMainAction(self : Faction, d : DeckCard, m : Message, then : ForcedAction) extends BaseAction(d.name.hl)("Place", Minion.of(self), m) with Soft
case class PlaceMinionAction(self : Faction, o : Option[Area], r : Area, trouble : Boolean, m : Message, then : ForcedAction) extends BaseAction("Place", Minion.of(self), m, o./(("from", _)),  "in")(r)
case class RemoveMinionIfMaxAction(self : Faction, m : Message, then : Action) extends ForcedAction with Soft
case class RemoveMinionMaxAction(self : Faction, o : Option[Area], m : Message, then : Action) extends BaseAction("Remove", Minion.of(self), "to place it elsewhere")(o) with Soft

case class BuildMainAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(d.name.hl)("Build", Building.of(self)) with Soft
case class BuildAction(self : Faction, r : Area, cost : Int, then : ForcedAction) extends BaseAction("Build", Building.of(self), "in")(r, "for", cost.money)

case class RandomEventMainAction(self : Faction, d : DeckCard) extends BaseAction(d.name.hl)("Random event")

case class PlayAnotherMainAction(self : Faction, d : DeckCard) extends BaseAction(d.name.hl)("Play another card") with Soft

case class ScrollMainAction(self : Faction, d : DeckCard, a : ScrollAction, then : ForcedAction) extends BaseAction(d.name.hl)(a) with SoftMarked

case class ScrollPerformAction(self : Faction, a : ScrollAction, then : ForcedAction) extends ForcedAction with SoftMarked

case class RandomEventAction(self : Faction, e : EventCard, then : ForcedAction) extends ForcedAction

case class TakeBackPlayedCardAction(self : Faction, d : DeckCard) extends BaseAction(None)("Take back", d, "and end turn with", DoctorMossyLawn) with SoftMarked

case class UseBuildingMainAction(self : Faction, r : Area, a : ScrollAction) extends BaseAction(Comment("Buildings"))(r, MDash, a) with SoftMarked

case class BuildingUsedAction(f : Faction, r : Area) extends ForcedAction

case class PayNMainAction(self : Faction, n : Int, then : ForcedAction) extends BaseAction(None)("Pay", n.money)
case class PayNAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class PayNQuietAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class PayNToAction(f : Faction, e : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class DiscardCardsAction(f : Faction, l : List[DeckCard], then : ForcedAction) extends ForcedAction


case class StealFromAction(self : Faction, t : Faction, n : Int, then : ForcedAction) extends ForcedAction

case class TakeFromAskAction(self : Faction, t : Faction, n : Int, then : ForcedAction) extends BaseAction("Take", n.money, "from")(t)
case class TakeFromAction(self : Faction, t : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class OrGiveMoneyAction(self : Faction, t : Faction, n : Int, then : ForcedAction) extends BaseAction("Or")("Give", n.money, "to", t)



case class SpreadTroubleToAction(self : Faction, r : Area, then : ForcedAction) extends BaseAction("Spread", Trouble, "to")(r)

case class AnotherPlayerGivesNCardsAction(self : Faction, t : Faction, n : Int, then : ForcedAction) extends BaseAction("Make give " ~ n.cards)(t)

case class HandPenaltyAction(self : Faction, t : Faction, then : ForcedAction) extends BaseAction("Hand penalty or " ~ 5.money ~ " from")(t)
case class HandPenaltyAcceptAction(self : Faction, then : ForcedAction) extends BaseAction("Hand penalty or " ~ 5.money)("Accept hand penalty")

case class FireBrigadeAction(self : Faction, t : Faction, then : ForcedAction) extends BaseAction("Send", "Fire Brigade".hl, "to")(t)
case class FireBrigadeAcceptAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction("Fire Brigade".hl, "demands", 5.money)("Don't pay")
case class FireBrigadeBurnAction(self : Faction, t : Faction, r : Area, then : ForcedAction) extends BaseAction("Fire Brigade".hl, "burns", Building.of(t), "in")(r)


case class CardOrNContinueAction(f : Faction, l : List[Faction], n : Int, then : ForcedAction) extends ForcedAction

case class RollToTakeNOrRemoveMinionAction(f : Faction, roll : Int, n : Int, then : ForcedAction) extends RolledAction[Int] {
    def rolled = $(roll)
}

case class RollToRemoveMinionAction(f : Faction, roll : Int, then : ForcedAction) extends RolledAction[Int] {
    def rolled = $(roll)
}

case class RollToTakeNOrPayNOrRemoveMinionAction(f : Faction, roll : Int, n : Int, k : Int, then : ForcedAction) extends RolledAction[Int] {
    def rolled = $(roll)
}

case class RollNKillMainAction(f : Faction, rolled : List[Area], then : ForcedAction) extends RolledAction[Area]
case class RollNKillContinueAction(f : Faction, areas : List[Area], then : ForcedAction) extends ForcedAction
case class RollNKillAction(self : Faction, e : Color, r : Area, areas : List[Area], then : ForcedAction) extends BaseAction("Remove", areas.num.minions, "from", areas.distinct.intersperse(", "))(Minion.of(e), "in", r)

case class MysteriousMurdersContinueAction(f : Faction, then : ForcedAction) extends ForcedAction
case class MysteriousMurdersNextAction(f : Faction, then : ForcedAction) extends ForcedAction
case class MysteriousMurdersMainAction(f : Faction, random : Area, then : ForcedAction) extends RandomAction[Area]
case class MysteriousMurdersAction(self : Faction, e : Color, r : Area, then : ForcedAction) extends BaseAction("Remove", 1.minions, "from", r)(Minion.of(e))


case class RemoveRolledBuildingsAction(f : Faction, rolled : List[Area], then : ForcedAction) extends RolledAction[Area]
case class FloodAreasAction(f : Faction, rolled : List[Area], then : ForcedAction) extends RolledAction[Area]

case class RemoveRolledEverythingAction(f : Faction, random : Area, then : ForcedAction) extends RandomAction[Area]
case class RemoveRolledBuildingIfNearAction(f : Faction, o : Option[Area], l : List[Area], random : Area, then : ForcedAction) extends RandomAction[Area]
case class BuildingMalfunctionAction(f : Faction, random : Area, then : ForcedAction) extends RandomAction[Area]

case class PlaceMinionsAction(f : Faction, e : Color, rolled : List[Area], then : ForcedAction) extends RolledAction[Area]
            
case class SellCardAskAction(self : Faction, e : Faction, d : DeckCard, n : Int, then : ForcedAction) extends ForcedAction
case class SellCardAction(self : Faction, e : Faction, d : DeckCard, n : Int, then : ForcedAction) extends ForcedAction


case class GiveCardsAction(self : Faction, e : Faction, l : List[DeckCard], then : ForcedAction) extends ForcedAction

case class DiscardCardsForMoneyAction(self : Faction, l : List[DeckCard], k : Int, then : ForcedAction) extends ForcedAction

case class DrawFromDiscardAction(f : Faction, n : Int, shuffled : List[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]

case class RerollCharacterAction(f : Faction, shuffled : List[Character], then : ForcedAction) extends ShuffledAction[Character]
case class ViewUnusedCharactersAction(f : Faction, shuffled : List[Character], then : ForcedAction) extends ShuffledAction[Character]

case class MoveMinionFromAction(self : Faction, e : Color, r : Area, l : List[Area], m : Message, then : ForcedAction) extends BaseAction("Move", m)(Minion.of(e), "from", r) with Soft
case class MoveMinionToAction(self : Faction, e : Color, r : Area, t : Area, m : Message, then : ForcedAction) extends BaseAction("Move", Minion.of(e), m, "from", r, "to")(t)
case class MoveMinionAction(f : Faction, e : Color, r : Area, t : Area, m : Message, then : ForcedAction) extends ForcedAction

case class ExchangeMinionsFromAction(self : Faction, e : Color, r : Area, then : ForcedAction) extends BaseAction("Exchange minion")(Minion.of(e), "in", r) with Soft
case class ExchangeMinionsToAction(self : Faction, e1 : Color, r1 : Area, e2 : Color, r2 : Area, then : ForcedAction) extends BaseAction("Exchange", Minion.of(e1), "in", r1, "with")((Minion.of(e2), "in", r2))
case class ExchangeMinionsAskAction(self : Faction, l : List[Faction], e1 : Color, r1 : Area, e2 : Color, r2 : Area, then : ForcedAction) extends ForcedAction
case class ExchangeMinionsAction(self : Faction, e1 : Color, r1 : Area, e2 : Color, r2 : Area, then : ForcedAction) extends ForcedAction


case class BuyBuildingAskAction(self : Faction, e : Faction, r : Area, then : ForcedAction) extends BaseAction("Buy")(Building.of(e), "in", r)
case class BuyBuildingAction(self : Faction, e : Faction, r : Area, then : ForcedAction) extends ForcedAction

case class ScoreMinionsAction(self : Faction, r : Area, k : Int, then : ForcedAction) extends BaseAction("Score", k.money, "for each minion in")(r)

case class PayNToMoveAction(self : Faction, e : Faction, n : Int, then : ForcedAction) extends BaseAction("To move, pay", n.money, "to")(e) with Soft
case class PayNToAssassinateAction(self : Faction, e : Faction, n : Int, then : ForcedAction) extends BaseAction("Pay", n.money, "to assassitate")(e)
case class PayNToMakeAssassinateAction(self : Faction, e : Faction, n : Int, then : ForcedAction) extends BaseAction("Pay", n.money, "to make assassitate")(e)

case class ForcePayAskAction(self : Faction, a : Faction, b : Faction, n : Int, then : ForcedAction) extends BaseAction("Force give money")(a, "gives", n.money, "to", b)
case class ForcePayAction(self : Faction, a : Faction, b : Faction, n : Int, then : ForcedAction) extends ForcedAction

case class StealNCardsMainAskAction(self : Faction, e : Faction, n : Int, then : ForcedAction) extends BaseAction("Take", n.cards, "from")(e)
case class StealNCardsMainAction(self : Faction, e : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class StealNCardsAction(f : Faction, e : Faction, n : Int, shuffled : List[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]
case class ChooseCardsToKeepAction(f : Faction, e : Faction, l : List[DeckCard], then : ForcedAction) extends ForcedAction
case class ChooseCardToKeepAction(self : Faction, e : Faction, l : List[DeckCard], d : DeckCard, then : ForcedAction) extends ForcedAction

case class DiscardEnemyCardAskAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction("View and discard", 1.cards, "from")(e)
case class DiscardEnemyCardMainAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction
case class DiscardEnemyCardAction(f : Faction, e : Faction, shuffled : List[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]
case class ChooseCardToDiscardAction(self : Faction, e : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class ExchangeHandsMainAskAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction("Exchange hand with")(e)
case class ExchangeHandsMainAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction
           
case class RemoveOwnMinionsContinueAction(l : List[Faction], then : ForcedAction) extends ForcedAction


case class SubsidenceContinueAction(f : Faction, then : ForcedAction) extends ForcedAction
case class RemoveBuildingAction(self : Faction, e : Faction, r : Area, then : ForcedAction) extends BaseAction("Pay or remove building")(Building.of(e), "in", r)

case class ProcessEffectsAction(ff : List[Faction], event : Boolean, text : Boolean, l : List[Effect], then : ForcedAction) extends ForcedAction
case class PerformEffectAction(e : Effect, then : ForcedAction) extends ForcedAction
case class PostEffectsAction(then : ForcedAction) extends ForcedAction

case class CancelEffectMoneyAction(self : Faction, event : Boolean, text : Boolean, l : List[Effect], e : Effect, n : Int, then : ForcedAction) extends BaseAction("Cancel effect for", n.money)("Prevent", e)
case class CancelEffectCardAction(self : Faction, event : Boolean, text : Boolean, l : List[Effect], e : Effect, d : DeckCard, then : ForcedAction) extends BaseAction("Cancel effect with", d)("Prevent", e)
case class ReplaceEffectCardAction(self : Faction, event : Boolean, text : Boolean, l : List[Effect], e : Effect, d : DeckCard, then : ForcedAction) extends BaseAction("Move instead with", d)("Replace", e)


case class ViewCardInfoAction(self : Faction, s : Message, d : DeckCard) extends BaseInfo(s)(d.img) with ViewCard with OnClickInfo { def param = d }
case class ViewCharacterInfoAction(self : Faction, s : Message, c : Character) extends BaseInfo(s)(c.img) with ViewChar with OnClickInfo { def param = c }
case class ViewAreaInfoAction(self : Faction, s : Message, r : Area) extends BaseInfo(s)(r.img) with ViewArea with OnClickInfo { def param = r }
case class ViewEventInfoAction(self : Faction, s : Message, e : EventCard) extends BaseInfo(s)(e.img) with ViewEvent with OnClickInfo { def param = e }

case class ClaimRecordInfoAction(self : Faction, record : ClaimRecord) extends BaseInfo(None)(self, MDash, record)

case object NoHand extends HiddenInfo
case object NoChar extends HiddenInfo
case object HiddenCheck extends HiddenInfo

case class PlayCard(f : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " plays"
}

case class Give(f : Faction) extends Message {
    def elem(g : Game) = "Give " ~ f.elem
}

case object Card extends Message {
    def elem(g : Game) = "card"
}

case class Cards(n : Int) extends Message {
    def elem(g : Game) = (n == 1).?("card").|("cards")
}

case object Cards extends Message {
    def elem(g : Game) = "cards"
}

case class GiveCardToOrN(f : Faction, t : Faction, n : Int) extends Message {
    def elem(g : Game) = f.elem ~ " gives " ~ n.money ~ " or a card to " ~ t.elem
}

case class GiveCardToTake(f : Faction, t : Faction, n : Int) extends Message {
    def elem(g : Game) = f.elem ~ " gives a card to " ~ t.elem ~ " to take " ~ n.money
}

case class PreBreak(m : Message with GameElementary) extends Message {
    def elem(g : Game) = Break ~ m.elem(g)
}

case object OwnHand extends Message {
    def elem(g : Game) = Text("Hand")
}

case object OwnCharacter extends Message {
    def elem(g : Game) = Text("Personality")
}

case object OwnBuilding extends Message {
    def elem(g : Game) = Text("Buildings")
}


class Player(val game : Game, val faction : Faction) {
    def figures(name : String, init : List[Figure] = Nil, rule : Figure => Boolean = (e : Figure) => true) = game.pieces.list(name + "-" + faction.short, init, rule)
    def cards(name : String, init : List[DeckCard] = Nil, rule : DeckCard => Boolean = (e : DeckCard) => true) = game.cards.list(name + "-" + faction.short, init, rule)
    
    var used : List[Area] = Nil
    var hand = cards("hand")
    var penalties = cards("penalties")
    var loans = cards("loans")
    val drawn = cards("drawn")

    def figures : List[Figure] = pieces.distinct./~(p => 1.to(pieces.count(p))./(Figure(faction, p, _)))
    
    val pieces = Building.repeat(6+6) ++ Minion.repeat(12)

    val pool = figures("pool", figures, _.faction == faction)

    var money : Int = 0

    var character : Option[Character] = None

    var records : List[ClaimRecord] = Nil

    def record(r : ClaimRecord) {
        if (records.%(o => ClaimRecord.better(o, r).has(r).not).none)
            records = r +: records.%(o => ClaimRecord.better(o, r).has(o))
    }
}

class Game(val board : Board, val setup : List[Faction], val logging : Boolean, val options : List[GameOption])  extends BaseGame with ContinueGame with LoggedGame { game =>
    var factions : List[Faction] = Nil
    var players = Map[Faction, Player]()

    implicit def faction2player(f : Faction) = players(f)
    
    val cards = new ValueTracker[DeckCard]

    var pile = cards.list("pile", Deck.forPlayers(setup.num))
    var deck = cards.list("deck")
    
    var play = cards.slot("play")
    var playing : DeckCard = NoCard
    
    class Discard(val faction : Option[Faction]) extends Destination[DeckCard] {
        def tracker = cards 
        def destination(source : Source[DeckCard], list : List[DeckCard]) = {
            if (list.any)
                faction.foreach(_.log("discarded", list))
    
            pile
        }

        def apply(f : Faction) = new Discard(Some(f))
    }
    
    val discard = new Discard(None)
    
    val pieces = new ValueTracker[Figure]
    
    val pool = game.pieces.list("neutral-pool", 
        1.to(12)./(Figure(Troubles, Trouble, _)) ++
        1.to(3)./(Figure(Trolls, Minion, _)) ++
        1.to(4)./(Figure(Demons, Minion, _))
    )

    val colors = Troubles :: Trolls :: Demons :: setup

    val xlocations = board.areas./(c => c -> pieces.list(c)).toMap

    implicit def location2list(l : ListLocation[Figure]) = l.get
    
    implicit def area2location(a : Area) = xlocations(a)

    implicit class ListLocationEx[T](val l : ListLocation[Figure]) {
        def -->(p : PieceOf) = SourceList[Figure](l, $(l.%(_.piece == p.piece).%(_.faction == p.faction).first))
    }

    implicit class AreaLocationEx[T](val l : Area) {
        def -->(p : PieceOf) = SourceList[Figure](l, $(l.%(_.piece == p.piece).%(_.faction == p.faction).first))
        def -->?(p : PieceOf) = SourceList[Figure](l, l.%(_.piece == p.piece).%(_.faction == p.faction).take(1))
    }
    
    
    implicit class FactionEx(f : Color) {
        def log(s : Any*) { if (logging) game.log((f +: s.toList) : _*) }
        
        def at(c : Area) = xlocations(c).%(_.faction == f)

        def all(p : Piece) = board.areas./~(c => xlocations(c).%(_.faction == f).%(_.piece == p)./(_ => c))
    }
      
    def at(c : Area, f : Color) = xlocations(c).%(_.faction == f)./(_.piece)

    def at(c : Area) = xlocations(c).get

    var characters : List[Character] = Nil

    var events : List[EventCard] = Nil

    var malfunctions : List[Area] = Nil
    
    var turn : Int = 0    
   
    var current : Faction = null

    var highlightFaction : List[Faction] = Nil

    var complete = List[CardAction]()
    var stack = List[CardAction]()
    
    def preinfo(waiting : List[Faction], self : Option[Faction], actions : List[UserAction]) : List[UserAction with Info] = Nil

    def viewHand(f : Faction) = f.hand./(ViewCardInfoAction(f, PreBreak(OwnHand), _))
    def viewCharacter(f : Faction) = f.character.toList./(ViewCharacterInfoAction(f, PreBreak(OwnCharacter), _))
    def viewBuildings(f : Faction) = f.all(Building)./(ViewAreaInfoAction(f, PreBreak(OwnBuilding), _))

    def extra(f : Faction) = (f.hand.get.has(DoctorMossyLawn) && play.any).??($(TakeBackPlayedCardAction(f, playing))) ++ f.all(Building)./(r => UseBuildingMainAction(f, r, r.effect).x(f.used.has(r)).x(malfunctions.has(r), "malfunctioned").x(r == StandardBoard.SmallGods))

    def info(waiting : List[Faction], self : Option[Faction], actions : List[UserAction]) : List[UserAction with Info] = {
        self.%(players.contains)./~( f => 
            actions.has(NoHand).not.??(viewHand(f)) ++ 
            viewBuildings(f) ++
            actions.has(NoChar).not.??(viewCharacter(f))
        )
    }

    implicit def descCard(g : Game, d : DeckCard) = d.img

    def loggedPerform(action : Action) : Continue = {
        println("> " + action)

        val c = performZ(action)
        
        if (players.size == factions.num) {
            val ww = Nil 
            if (ww.any) {
                val cw = factions
                return GameOver(ww ++ cw, "message", Nil)
            }
        }

        highlightFaction = c match {
            case Ask(f, _, _) => $(f)
            case MultiAsk(a) => a./(_.faction)
            case Notify(l, _, _) => l
            case _ => Nil
        }

        c
    }

    def removeMinion(f : Color, r : Area) {
        r --> Minion.of(f) --> (f match {
            case f : Faction => f.pool
            case _ : Color => pool
        })
        
        if (Troubles.at(r).any) {
            r --> Trouble --> pool

            log(Trouble, "is gone from", r)
        }
    }
     
    def performZ(a : Action) : Continue = {
        implicit val action = a

        action match {
            // INIT
            case StartAction =>
                val chars = (setup.num > 2).??($(Chrysoprase)) ++ $(LordVetinari, DragonKingOfArms, CommanderVimes, LordRust, LordSelachii, LordDeWorde)

                Shuffle[Character](chars, ShuffleCharactersAction(_))
                
            case ShuffleCharactersAction(l) =>
                characters = l

                log("Shuffled characters")

                Shuffle[EventCard](Deck.events, ShuffleEventCardsAction(_))
                
            case ShuffleEventCardsAction(l) =>
                events = l

                log("Shuffled events")

                Shuffle[DeckCard](pile, ShufflePileAction(_, SetupNextAction))
                
            case ShufflePileAction(l, then) =>
                pile --> l.intersect(Deck.debug ++ Deck.green.take(Deck.debug.num / 4)) --> deck
                pile --> l.intersect(Deck.green.drop(Deck.debug.num / 4)) --> deck
                pile --> l.intersect(Deck.brown) --> deck

                log("Shuffled deck")

                board.starting.foreach { r => pool --> Trouble --> r }
     
                Force(then)
                
            case SetupNextAction =>
                val pending = setup.%!(players.contains)
    
                if (pending.any) {
                        val f = pending.head

                    factions :+= f

                    players += f -> new Player(game, f)

                    f.character = Some(characters(setup.indexOf(f)))

                    f.log("drew", "Personality".hl)
                    
                    val then = FactionInitAction(f)

                    Notify($(f), $(ViewCharacterInfoAction(f, DrewChar(f), f.character.get), NoChar), then)
                }
                else {
                    factions = setup
                    InitDoneAction
                }

            case InitDoneAction =>
                StartPlayerTurnAction(factions(0))

            case FactionInitAction(f) =>
                board.starting.foreach { r => f.pool --> Minion.of(f) --> r }
                
                f.log("placed minions in", board.starting)

                f.money = 10

                f.log("took", 10.money)

                DrawCardsAction(f, 5, NoMessage, AddCardsAction(f, SetupNextAction))
                
            case DrawCardsAction(f, n, m, then) =>
                if (n <= 0)
                    then
                else
                if (deck.num < n && pile.any && false)
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
                
                    if (x > 0)
                        DrawingCardsAction(f, x, Nil, m, then)
                    else
                        then
                }
                    
            case DrawingCardsAction(f, n, l, m, then) =>
                if (l.num < n)
                    DrawingCardsAction(f, n, l :+ deck.diff(l)(0), m, then)
                else {
                    if (f.drawn.any)
                        throw new Error("drawn overdrawn")

                    deck --> l --> f.drawn
                    
                    f.log("drew", n.cards)
                    
                    Notify($(f), l./(d => ViewCardInfoAction(f, DrewFromTheDeck(f, m), d)), then)
                }
                
            case AddCardsAction(f, then) =>
                f.drawn --> f.hand

                then
                
            case StartPlayerTurnAction(f) =>
                log(DoubleLine)

                turn += 1
                current = f

                f.used = Nil

                playing = NoCard

                stack = $(PlayCard, RefillCards)
                
                log("Turn", ("#" + turn).hl, "-", f)
                                                                                   
                CheckWinAction(f)
                
            case CheckWinAction(f) =>
                val areas = board.areas.diff(Demons.all(Minion))
                
                val claim = f.character.get match {
                    case Chrysoprase => MonetaryClaim(f.money + areas.%(f.at(_).building.any)./(_.cost).sum - f.loans.num * 12, 50)
                    case _ : Lord => RuleClaim(areas.%(r => f.at(r).num > colors.but(f).but(Troubles)./(_.at(r).num).max).num, (14.5 / setup.num).round.toInt)
                    case LordVetinari => SpyNetworkClaim(areas.%(r => f.at(r).minion.any).num, 13 - setup.num)
                    case DragonKingOfArms => TroubleClaim(board.areas.%(r => Troubles.at(r).any).num, 8)
                    case CommanderVimes => EmptyDeckClaim(deck.num)
                }

                val record = ClaimRecord(f.character.get, turn, claim)

                if (hrf.HRF.flag("test"))
                    log(record)

                f.record(record)

                if (record.claim.winning)
                    GameOverAction($(f))
                else
                    ContinuePlayerTurnAction(f)
                
            case ContinuePlayerTurnAction(f) =>
                if (stack.none)
                    EndPlayerTurnAction(f)
                else {
                    stack(0) match {
                        case PlayCard if f.hand.%({ 
                            case _ : Interrupt => false
                            case _ => true
                        }).none =>
                            Ask(f)(CantPlayMainAction(f))(extra(f)).needOk
                            
                        case PlayCard =>
                            Force(PlayCardMainAction(f))
                            
                        case TakeBackCheck if f.hand.has(DoctorMossyLawn).not =>
                            CompleteActionTurnAction(f)
                        
                        case DiscardPlay =>
                            complete = Nil

                            play --> discard(f)

                            playing = NoCard

                            CompleteActionTurnAction(f)

                        case RefillCards =>
                            Ask(f)(RefillAction(f, CompleteActionTurnAction(f)))(extra(f))
                            
                        case a => 
                            var actions : List[UserAction] = Nil
                        
                            def fill(l : List[CardAction]) =
                                    (playing != NoCard).??(l.takeWhile(d => d != PlayCard && d != TakeBackCheck && d != DiscardPlay)./{
                                        case TakeMoney(n) => TakeMoneyMainAction(f, playing, n)
                                        case Assassinate => AssassinateMainAction(f, playing, CompleteActionTurnAction(f))
                                        case PlaceMinion => PlaceMinionMainAction(f, playing, NoMessage, CompleteActionTurnAction(f))
                                        case Build => BuildMainAction(f, playing, CompleteActionTurnAction(f))
                                        case RemoveTrouble => RemoveTroubleMainAction(f, playing, NoMessage, CompleteActionTurnAction(f))
                                        case RandomEvent => RandomEventMainAction(f, playing)
                                        case PlayAnother => PlayAnotherMainAction(f, playing)
                                        case Scroll(a) => ScrollMainAction(f, playing, a, CompleteActionTurnAction(f))
                                        case x => log("skipping fill for", x.toString); TakeMoneyMainAction(f, playing, 0)
                                    })./(_.x(true))
                        
                                
                            actions ++= fill(complete)
                            
                            actions :+= (a @@ {
                                case TakeMoney(n) => TakeMoneyMainAction(f, playing, n)
                                case Assassinate => AssassinateMainAction(f, playing, CompleteActionTurnAction(f))
                                case PlaceMinion => PlaceMinionMainAction(f, playing, NoMessage, CompleteActionTurnAction(f))
                                case Build => BuildMainAction(f, playing, CompleteActionTurnAction(f))
                                case RemoveTrouble => RemoveTroubleMainAction(f, playing, NoMessage, CompleteActionTurnAction(f))
                                case PlayAnother => PlayAnotherMainAction(f, playing)
                                case Scroll(a) => ScrollMainAction(f, playing, a, CompleteActionTurnAction(f))
                                case RandomEvent => RandomEventMainAction(f, playing)
                                case TakeBackCheck => HiddenCheck
                            })
                            
                            actions ++= fill(stack.drop(1))

                            actions :+= SkipActionMainAction(f).x(a == RandomEvent)
                                    
                            actions ++= extra(f)
                            
                            if (actions.any)
                                Ask(f)(actions).needOk
                            else
                                ContinuePlayerTurnAction(f)
                        } 
                }
                
            case PlayCardMainAction(f) =>
                YYSelectObjectsAction(f, f.hand)
                    .withRule({ 
                        case _ : Interrupt => false
                        case _ => true
                    })
                    .withGroup(f.elem ~ " plays a card")
                    .withThen(PlayCardAction(f, _, ContinuePlayerTurnAction(f)))("Play " ~ _.elem)("Play")
                    .withExtra(extra(f) ++ $(NoHand))
                
            case PlayAnotherMainAction(f, _) =>
                YYSelectObjectsAction(f, f.hand)
                    .withRule({ 
                        case _ : Interrupt => false
                        case _ => true
                    })
                    .withGroup(f.elem ~ " plays another card")
                    .withThen(PlayCardAction(f, _, ContinuePlayerTurnAction(f)))("Play " ~ _.elem)("Play")
                    .withExtra(extra(f) ++ $(NoHand, SkipActionMainAction(f)))
                
            case PlayCardAction(f, d, then) =>
                complete = Nil
                
                play --> discard(f)
            
                f.hand --> d --> play

                playing = d

                stack = d.actions ++ $(TakeBackCheck, DiscardPlay) ++ stack.drop(1)

                f.log("played", d)
                
                then
    
            case InterruptCardAction(f, d, then) =>
                f.hand --> d --> discard
                
                playing = d

                f.log("interrupted with", d)
                
                then
    
            case CantPlayMainAction(f) =>
                CompleteActionTurnAction(f)

            case TakeMoneyMainAction(f, _, n) =>
                f.log("took", n.money)

                f.money += n

                CompleteActionTurnAction(f)
                
            case AssassinateMainAction(f, _, then) =>
                Ask(f, board.areas./~{ r =>
                    Troubles.at(r).any.??(colors.but(f)./~{ e =>
                        e.at(r).minion./(_ => AssassinateAction(f, e, r, false, then))
                    })
                }).cancel.needOk
                
            case AssassinateAction(f, e, r, text, then) =>
                f.log("assassinated", Minion.of(e), "in", r)

                ProcessEffectsAction($(e).of[Faction], false, text, $(RemoveMinionEffect(r, e)), then)
                                
            case RemoveTroubleMainAction(f, _, m, then) =>
                Ask(f, board.areas./~{ r =>
                    Troubles.at(r).any.?(RemoveTroubleAction(f, r, m, then))
                }.cancel.okNeeded)
                
            case RemoveTroubleAction(f, r, m, then) =>
                f.log("removed", Trouble, "in", r, m)

                r --> Trouble --> pool

                then

            case RemoveMinionIfMaxAction(f, m, then) =>
                if (f.all(Minion).num == f.pieces.count(Minion))
                    Ask(f, f.all(Minion)./(r => RemoveMinionMaxAction(f, Some(r), m, then))).cancel
                else
                    Force(RemoveMinionMaxAction(f, None, m, then))
                    
            case PlaceMinionMainAction(f, d, m, then) =>
                RemoveMinionIfMaxAction(f, m, PlaceMinionMainAction(f, d, m, then))
                
            case RemoveMinionMaxAction(_, o, _, PlaceMinionMainAction(f, d, m, then)) =>
                val g = f.all(Minion).distinct.some./(e => (e ++ e./~(board.connected)).distinct).|(board.areas)
                Ask(f, board.areas.%(g.has).but(o)./(PlaceMinionAction(f, o, _, true, m, then)).cancel)

            case PlaceMinionAction(f, o, r, trouble, m, then) =>
                o.foreach { o =>
                    o --> Minion.of(f) --> f.pool
                
                    o -->? Trouble --> pool

                    f.log("removed", Minion.of(f), "from", r, "to place")
                }
            
                val already = Troubles.at(r).none && colors./~(_.at(r).minion).any

                f.pool --> Minion.of(f) --> r

                f.log("placed", Minion.of(f), "in", r, m)
                
                if (already && trouble) {
                    pool --> Trouble --> r
                    
                    log(Trouble, "appeared in", r)
                }
                
                then

            case BuildMainAction(f, _, then) =>
                Ask(f, board.areas./(r => BuildAction(f, r, r.cost, then).x(factions.%(_.at(r).building.any).any, "exists").x(f.at(r).minion.none, "no minion").x(Troubles.at(r).any, "trouble").x(r.cost > f.money, "not enough money")).cancel.okNeeded)
                                
            case BuildAction(f, r, cost, then) =>
                f.pool --> Building.of(f) --> r

                f.money -= cost

                f.used :+= r

                f.log("built", Building.of(f), "in", r, "for", cost.money)

                then

            case RandomEventMainAction(f, _) if events.none =>
                f.log("No random events left")
            
                CompleteActionTurnAction(f)


            case RandomEventMainAction(f, _) =>
                val e = events(0)

                events = events.drop(1)

                f.log("triggered", "random event", e.name.hl)
            
                val then = RandomEventAction(f, e, CompleteActionTurnAction(f))

                Notify(factions, $(ViewEventInfoAction(f, TriggeredRandomEvent(f, e), e)), then)
                 
            case UseBuildingMainAction(f, r, a) =>
                Force(ScrollMainAction(f, NoCard, a, BuildingUsedAction(f, r)))
                
            case BuildingUsedAction(f, r) =>
                f.used :+= r

                f.log("used", Building.of(f), "in", r)
                
                ContinuePlayerTurnAction(f)

            case SkipActionMainAction(f) =>
                CompleteActionTurnAction(f)

            case CompleteActionTurnAction(f) =>
                complete ++= stack.take(1)
                stack = stack.drop(1)

                ContinuePlayerTurnAction(f)

            case RefillAction(f, then) =>
                DrawCardsAction(f, 5 - f.hand.num - f.penalties.num, NoMessage, AddCardsAction(f, then))

            case EndPlayerTurnAction(f) =>
                f.used = Nil

                factions = factions.drop(1) ++ factions.take(1)

                StartPlayerTurnAction(factions(0))

            case TakeBackPlayedCardAction(f, d) =>
                complete = Nil

                play --> f.hand

                f.hand --> DoctorMossyLawn --> discard(f)

                playing = DoctorMossyLawn

                f.log("took back", d, "with", DoctorMossyLawn)
                
                Ask(f)(EndPlayerTurnAction(f).as("End Turn"))

            // HELPERS
            case ScrollMainAction(f, _, a, then) =>
                ScrollPerformAction(f, a, then)
                

            case PayNMainAction(f, n, then) =>
                PayNAction(f, n, then)

            case PayNAction(f, n, then) =>
                f.money -= n

                f.log("paid", n.money)

                then

            case PayNQuietAction(f, n, then) =>
                f.money -= n

                then

            case PayNToAction(f, e, n, then) =>
                f.money -= n
                e.money += n

                f.log("paid", n.money, "to", e)

                then

            case DiscardCardsAction(f, l, then) =>
                f.hand --> l --> discard(f)

                then
                
                
            // SCROLLS
            case StealFromAction(f, t, x, then) =>
                val n = min(x, t.money)

                if (n > 0) {
                    t.money -= n
                    f.money += n
            
                    f.log("took", n.money, "from", t)
                }
            
                then

            // MOVE
            case MoveMinionFromAction(f, e, r, l, m, then) =>
                Ask(f)(l./(MoveMinionToAction(f, e, r, _, m, then))).cancel
                
            case MoveMinionToAction(f, e : Faction, r, t, m, then) if f != e =>
                Ask(e)(e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)("Prevent moving", Minion.of(e), "from", r, "to", t)))(MoveMinionAction(f, e, r, t, m, then).as("Skip"))

            case MoveMinionToAction(f, e, r, t, m, then) =>
                MoveMinionAction(f, e, r, t, m, then)

            case MoveMinionAction(f, e, o, r, m, then) =>
                val trouble = Troubles.at(r).none && colors.%(_.at(r).minion.any).any
                
                o -->? Trouble --> pool

                o --> Minion.of(e) --> r
                
                f.log("moved", Minion.of(e), "from", o, "to", r)
                
                if (trouble) {
                    pool --> Trouble --> r

                    log(Trouble, "appeared in", r)
                }
                
                then

                
            // ACTION LIST
            case ScrollPerformAction(f, ScrollActions(Nil), then) =>
                then
                
            case ScrollPerformAction(f, ScrollActions(a :: l), then) =>
                ScrollPerformAction(f, a, ScrollPerformAction(f, ScrollActions(l), then))
                

            case ScrollPerformAction(f, SoftScrollActions(Nil), then) =>
                then
                
            case ScrollPerformAction(f, SoftScrollActions(a :: l), then) =>
                ScrollPerformAction(f, a, ScrollPerformAction(f, ScrollActions(l), then))
                

            // DRAW
            case ScrollPerformAction(f, DrawCards(n), then) =>
                DrawCardsAction(f, n, NoMessage, AddCardsAction(f, then))

                
            // DRAW FOR BUILDINGS
            case ScrollPerformAction(f, DrawCardsForBuildings, then) =>
                DrawCardsAction(f, f.all(Building).num, NoMessage, AddCardsAction(f, then))
                
                
            // DISCARD
            case ScrollPerformAction(f, DiscardCards(min, max), then) =>
                XXSelectObjectsAction(f, f.hand)
                    .withGroup("Discard " ~ (max > 1).?("up to " ~ max.hl ~ " cards").|("a card") ~ (min > 1).??(", min " ~ min.hl ~ " cards"))
                    .withRule(_.upTo(max).min(min))
                    .withThen(DiscardCardsAction(f, _, then))("Discard".hl ~ _./(" " ~ _.elem))
                    .withExtra($(NoHand) ++ (min == 0).?(CancelAction))
                    

            // DISCARD TO
            case ScrollPerformAction(f, DiscardCardTo(a), then) =>
                YYSelectObjectsAction(f, f.hand)
                    .withGroup("Discard a card to " ~ a.elem)
                    .withThen(d => DiscardCardsAction(f, $(d), ScrollPerformAction(f, a, then)))("Discard".hl ~ " " ~ _.elem)("Discard".hl)
                    .withExtra($(NoHand, CancelAction))


            // TAKE $
            case ScrollPerformAction(f, TakeN(n), then) =>
                f.money += n

                f.log("took", n.money)
                
                then
                

            // LOAN
            case ScrollPerformAction(f, TakeLoan, then) =>
                play --> f.loans

                f.money += 10

                f.log("took", 10.money, "loan")
                
                then
            
                
            // DOUBLE ACTION
            case ScrollPerformAction(f, DoubleAction, then) =>
                f.log("played two more cards")
                
                val (a, b) = stack.span(_ != DiscardPlay)

                stack = a ++ b.take(1) ++ $(PlayCard, PlayCard) ++ b.drop(1)
                
                then
                
               
            // SPREAD MINION
            case ScrollPerformAction(f, SpreadMinion(r), then) =>
                RemoveMinionIfMaxAction(f, NoMessage, ScrollPerformAction(f, SpreadMinion(r), then))
                
            case RemoveMinionMaxAction(_, o, _, ScrollPerformAction(f, SpreadMinion(r), then)) =>
                Ask(f)((r +: board.connected(r)).but(o)./(r => PlaceMinionAction(f, o, r, true, NoMessage, then))).cancel.needOk


            // SPREAD MINION FOR $
            case ScrollPerformAction(f, SpreadMinionForN(r, n), then) =>
                RemoveMinionIfMaxAction(f, NoMessage, ScrollPerformAction(f, SpreadMinionForN(r, n), then))
                
            case RemoveMinionMaxAction(_, o, _, ScrollPerformAction(f, SpreadMinionForN(r, n), then)) =>
                Ask(f)((r +: board.connected(r)).but(o)./(r => PlaceMinionAction(f, o, r, true, ForN(3), PayNAction(f, 3, then)).x(f.money < 3, "not enough money"))).cancel.needOk


            // MINION AT BUILDING
            case ScrollPerformAction(f, MinionAtBuilding, then) =>
                RemoveMinionIfMaxAction(f, NoMessage, ScrollPerformAction(f, MinionAtBuilding, then))

            case RemoveMinionMaxAction(_, o, _, ScrollPerformAction(f, MinionAtBuilding, then)) =>
                Ask(f)(f.all(Building).but(o)./(r => PlaceMinionAction(f, o, r, true, NoMessage, then))).cancel.needOk
 

            // MINION AT TROUBLE
            case ScrollPerformAction(f, MinionAtTrouble, then) =>
                RemoveMinionIfMaxAction(f, NoMessage, ScrollPerformAction(f, MinionAtTrouble, then))

            case RemoveMinionMaxAction(_, o, _, ScrollPerformAction(f, MinionAtTrouble, then)) =>
                Ask(f)(Troubles.all(Trouble).but(o)./(r => PlaceMinionAction(f, o, r, true, NoMessage, then))).cancel.needOk
 

            // MINION ANYWHERE
            case ScrollPerformAction(f, MinionNoTrouble, then) =>
                RemoveMinionIfMaxAction(f, NoMessage, ScrollPerformAction(f, MinionNoTrouble, then))

            case RemoveMinionMaxAction(_, o, _, ScrollPerformAction(f, MinionNoTrouble, then)) =>
                Ask(f)(board.areas.but(o)./(r => PlaceMinionAction(f, o, r, false, NoMessage, then))).cancel.needOk

                
            // CLEAR TROUBLE
            case ScrollPerformAction(f, RemoveTroubleForN(n), then) =>
                Ask(f, board.areas./~{ r =>
                    Troubles.at(r).any.?(RemoveTroubleAction(f, r, ForN(n), PayNAction(f, n, then)))
                }.cancel.okNeeded)


            // PLACE TROUBLE
            case ScrollPerformAction(f, PlaceTrouble, then) =>
                val ll = Troubles.all(Trouble)
                val l = board.areas.diff(ll)
                Ask(f)(l./(t => SpreadTroubleToAction(f, t, then).x(Troubles.at(t).any).x(colors.%(_.at(t).minion.any).none, "no minions"))).cancel.needOk
                
            case SpreadTroubleToAction(f, r, then) =>
                pool --> Trouble --> r

                f.log("spread", Trouble, "to", r)

                then
               

            // SPREAD TROUBLE
            case ScrollPerformAction(f, SpreadTrouble, then) =>
                val ll = Troubles.all(Trouble)
                val l = ll./~(board.connected).diff(ll)
                Ask(f)(board.areas.%(l.has)./(t => SpreadTroubleToAction(f, t, then).x(Troubles.at(t).any).x(colors.%(_.at(t).minion.any).none, "no minions"))).cancel.needOk
                

            // SPREAD TROUBLE FROM
            case ScrollPerformAction(f, SpreadTroubleFrom(r), then) =>
                Ask(f)((r +: board.connected(r))./(t => SpreadTroubleToAction(f, t, then).x(Troubles.at(t).any).x(colors.%(_.at(t).minion.any).none, "no minions"))).cancel.needOk
                

            // STEAL $
            case ScrollPerformAction(f, StealN(n), then) =>
                Ask(f, factions.but(f)./(e => TakeFromAskAction(f, e, n, then).x(e.money <= 0)).ocancel)
                
            case TakeFromAskAction(f, e, x, then) =>
                Ask(e)(e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)("Prevent", f, "stealing", x.money)))(TakeFromAction(f, e, x, then).as("Skip"))

            case TakeFromAction(f, e, x, then) =>
                StealFromAction(f, e, x, then)
                

            // STEAL FROM ALL    
            case ScrollPerformAction(f, StealNFromEveryone(k), then) =>
                var q : ForcedAction = then

                factions.reverse.but(f).foreach { e =>
                    val n = min(k, e.money)
                    if (n > 0) {
                        q = ForceAction(TakeFromAskAction(f, e, n, q))
                    }
                }
                
                q
                

            // STEAL FROM ALL    
            case ScrollPerformAction(f, ForcePay(n), then) =>
                Ask(f, factions.but(f)./~(a => factions.but(f).but(a)./(b => ForcePayAskAction(f, a, b, min(a.money, n), then)))).cancel
                
            case ForcePayAskAction(f, e, t, n, then) =>
                Ask(e)(e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)("Prevent giving", n.money, "to", t)))(ForcePayAction(f, e, t, n, then).as("Skip"))
                
            case ForcePayAction(f, e, t, n, then) =>
                e.money -= n
                t.money += n

                f.log("forced", a, "to give", n.money, "to", t)

                then

                
            // ENEMY GIVES CARDS
            case ScrollPerformAction(f, AnotherPlayerGivesNCards(n), then) =>
                Ask(f, factions.but(f)./(t => AnotherPlayerGivesNCardsAction(f, t, min(n, t.hand.num), then).x(t.hand.none)).ocancel)
 
            case AnotherPlayerGivesNCardsAction(f, e, n, then) =>
                f.log("asked", e, "for", n.hl, "card")

                XXSelectObjectsAction(e, e.hand)
                    .withGroup("Give " ~ n.cards ~ " to " ~ f.elem)
                    .withRule(_.num(n))
                    .withThen(GiveCardsAction(e, f, _, then))(l => "Give".hl ~ l./(" " ~ _.elem) ~ l.any.??(" " ~ ("to " ~ f.elem).spn(xlo.nowrap)))
                    .withExtra($(NoHand) ++ e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as("Prevent with", WallaceSonky)))
                    
            case GiveCardsAction(f, t, l, then) =>
                f.hand --> l --> t.hand
            
                f.log("gave", l.num.hl, "cards to", t)

                Notify($(t), l./(ViewCardInfoAction(t, GaveTo(f, t), _)), then)


            // ENEMIES GIVE CARD OR $    
            case ScrollPerformAction(f, OtherPlayersGiveCardOrN(n), then) =>
                f.log("asked others for", n.money, "or a card")
                
                CardOrNContinueAction(f, factions.but(f).%(t => t.money > 0 || t.hand.any), n, then)
                
            case CardOrNContinueAction(f, Nil, n, then) =>
                then

            case CardOrNContinueAction(f, e :: l, n, then) =>
                YYSelectObjectsAction(e, e.hand)
                    .withGroup(e.elem ~ " gives " ~ n.money ~ " or a card to " ~ f.elem)
                    .withThen(d => GiveCardsAction(e, f, $(d), CardOrNContinueAction(f, l, n, then)))("Give " ~ _.elem ~  " to " ~ f.elem)("Give card")
                    .withExtra($(NoHand, OrGiveMoneyAction(e, f, 1, CardOrNContinueAction(f, l, n, then))) ++ e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, CardOrNContinueAction(f, l, n, then)).as("Prevent with", WallaceSonky)))
                
            case OrGiveMoneyAction(f, t, x, then) =>
                val n = min(x, f.money)

                if (n > 0) {
                    f.money -= n
                    t.money += n
            
                    f.log("gave", n.money, "to", t)
                }

                then
                

            // GIVE CARD TAKE $
            case ScrollPerformAction(f, GiveCardTakeN(n), then) =>
                if (f.hand.none)
                    Ask(f)(Info("No cards in hand to give"))(CancelAction).needOk
                else
                    YYSelectObjectsAction(f, f.hand)
                        .withGroup(f.elem ~ " gives a card to take " ~ n.money)
                        .withThens(d => factions.but(f)./(e => SellCardAskAction(f, e, d, min(n, e.money), then).as("Sell " ~ d.elem ~  " to " ~ e.elem ~ " for " ~ min(n, e.money).money)))
                        .withExtra($(NoHand, CancelAction))
                        
            case SellCardAskAction(f, e, d, n, then) =>
                Ask(e)(e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)("Prevent giving", n.money, "to", e, "for", d)))(SellCardAction(f, e, d, n, then).as("Skip"))

            case SellCardAction(f, e, d, n, then) =>
                GiveCardsAction(f, e, $(d), StealFromAction(f, e, n, then))
                        
                        
            // STEAL CARDS TAKE ONE
            case ScrollPerformAction(f, StealNCardsTakeOne(n), then) =>
                Ask(f, factions.but(f)./(e => StealNCardsMainAskAction(f, e, n, then).x(e.hand.none)).ocancel)
                
            case StealNCardsMainAskAction(f, e, n, then) =>
                Ask(e)(e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)("Prevent", e, "stealing", 1.hl, "of", n.hl, "cards")))(StealNCardsMainAction(f, e, n, then).as("Skip"))

            case StealNCardsMainAction(f, e, n, then) =>
                Shuffle[DeckCard](e.hand, StealNCardsAction(f, e, n, _, then))
                
            case StealNCardsAction(f, e, n, ll, then) =>
                val l = ll.take(n)
                
                e.hand --> l --> f.hand
                
                f.log("selected", n.cards, "from", e)
                
                val t = ChooseCardsToKeepAction(f, e, l, then)

                Notify($(e), l./(ViewCardInfoAction(e, TookFrom(f, e), _)), t)
                
            case ChooseCardsToKeepAction(f, e, l, then) =>
                YYSelectObjectsAction(f, l)
                    .withGroup(f.elem ~ " chooses a card to keep")
                    .withThen(ChooseCardToKeepAction(f, e, l, _, then))("Keep " ~ _.elem)("Keep")

            case ChooseCardToKeepAction(f, e, l, d, then) =>
                val r = l.diff($(d))
                f.hand --> r --> e.hand

                f.log("returned", r.num.cards)
                
                Notify($(e), r./(ViewCardInfoAction(e, ReturnedTo(f, e), _)), then)


            // DISCARD ENEMY CARD
            case ScrollPerformAction(f, DiscardEnemyCard, then) =>
                Ask(f, factions.but(f)./(e => DiscardEnemyCardAskAction(f, e, then).x(e.hand.none)).ocancel)
                
            case DiscardEnemyCardAskAction(f, e, then) =>
                Ask(e)(e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)("Prevent", f, "discarding card")))(DiscardEnemyCardMainAction(f, e, then).as("Skip"))
            
            case DiscardEnemyCardMainAction(f, e, then) =>
                Shuffle(e.hand, DiscardEnemyCardAction(f, e, _, then))
                
            case DiscardEnemyCardAction(f, e, l, then) =>
                f.log("viewed cards of", e)
                
                YYSelectObjectsAction(f, l)
                    .withGroup(f.elem ~ " chooses a card to discard")
                    .withThen(ChooseCardToDiscardAction(f, e, _, then))("Discard " ~ _.elem)("Discard")
                
            case ChooseCardToDiscardAction(f, e, d, then) =>
                e.hand --> d --> discard(e)
                
                f.log("discarded", d, "from", e)

                Notify($(e), $(d)./(ViewCardInfoAction(e, TookFrom(f, e), _)), then)

                
            // EXCHANGE HAND
            case ScrollPerformAction(f, ExchangeHands, then) =>
                Ask(f, factions.but(f)./(e => ExchangeHandsMainAskAction(f, e, then)).ocancel)

            case ExchangeHandsMainAskAction(f, e, then) =>
                Ask(e)(e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)("Prevent exchanging hands with", f)))(ExchangeHandsMainAction(f, e, then).as("Skip"))
                
            case ExchangeHandsMainAction(f, e, then) =>
                val fh = f.hand.get
                val eh = e.hand.get

                f.hand --> fh --> e.hand
                e.hand --> eh --> f.hand

                f.log("exchanged hand with", e)
                
                Notify($(e), fh./(d => ViewCardInfoAction(f, ExchangedHand(f), d)), then)


            // REFILL HAND
            case ScrollPerformAction(f, RefillHand, then) =>
                Force(RefillAction(f, then))


            // HAND PENALTY
            case ScrollPerformAction(f, HandPenalty, then) =>
                Ask(f, factions.but(f)./(t => HandPenaltyAction(f, t, then)).ocancel)
                        
            case HandPenaltyAction(f, e, then) =>
                f.log("threatened", e, "with", playing.name.hl)
                
                Ask(e)(HandPenaltyAcceptAction(e, then))(OrGiveMoneyAction(e, f, 5, then))(e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)))

            case HandPenaltyAcceptAction(f, then) =>
                f.log("accepted", playing.name.hl)

                play --> f.penalties

                then


            // FIRE BRIGADE
            case ScrollPerformAction(f, FireBrigade, then) =>
                Ask(f, factions.but(f)./(e => FireBrigadeAction(f, e, then).x(e.all(Building).none, "no buildings")).ocancel)
                        
            case FireBrigadeAction(f, e, then) =>
                f.log("sent", "Fire Brigade".hl, "to", e)

                Ask(e)(FireBrigadeAcceptAction(e, f, then))(OrGiveMoneyAction(e, f, 5, then).x(e.money < 5, "not enough money"))(e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)))
                
            case FireBrigadeAcceptAction(f, e, then) =>
                f.log("refused to pay")
                
                Ask(e)(f.all(Building)./(FireBrigadeBurnAction(e, f, _, then))).refuse(then)
                               
            case FireBrigadeBurnAction(f, e, r, then) =>
                r --> Building.of(e) --> e.pool

                f.log("burned", Building.of(e), "in", r)

                then

            // EARN FOR MINIONS IN
            case ScrollPerformAction(f, EarnForMinionsIn(r, k), then) =>
                Force(ScoreMinionsAction(f, r, k, then))


            // EARN FOR TROUBLE
            case ScrollPerformAction(f, EarnForTrouble(k), then) =>
                val n = Troubles.all(Trouble).num * k

                f.money += n

                f.log("took", n.money, "for", Trouble)
                
                then


            // EARN FOR BUILDINGS
            case ScrollPerformAction(f, EarnForBuildings(k), then) =>
                val n = factions./(_.all(Building).num).sum * k

                f.money += n

                f.log("took", n.money, "for buildings")
                
                then


            // EARN FOR MINIONS IN TROUBLE
            case ScrollPerformAction(f, ScoreMinionsInTrouble(k), then) =>
                Ask(f)(board.areas./(r => ScoreMinionsAction(f, r, k, then).x(Troubles.at(r).none, "no trouble"))).cancel.needOk

            case ScoreMinionsAction(f, r, k, then) =>
                val n = colors./(_.at(r).minion.num).sum * k

                f.money += n

                f.log("took", n.money, "for minions in", r)
                
                then
                

            // DISCARD FOR $
            case ScrollPerformAction(f, DiscardCardsForMoney(k), then) =>
                XXSelectObjectsAction(f, f.hand)
                    .withGroup("Discard cards for " ~ k.money ~ " each")
                    .withThen(DiscardCardsForMoneyAction(f, _, k, then))(l => "Discard".hl ~ l./(" " ~ _.elem) ~ l.any.??(" " ~ ("for " ~ (k * l.num).money).spn(xlo.nowrap)))
                    .withExtra($(NoHand, CancelAction))
                
            case DiscardCardsForMoneyAction(f, l, k, then) =>
                f.hand --> l --> discard(f)

                val n = l.num * k

                f.money += n
                
                f.log("took", n.money, "for discarded cards")

                then


            // ROLL TO REMOVE MINION
            case ScrollPerformAction(f, RollToRemoveMinion, then) =>
                Roll[Int]($(D12), l => RollToRemoveMinionAction(f, l.single.get, then))
                
            case RollToRemoveMinionAction(f, roll, then) =>
                f.log("rolled", roll.roll)

                if (roll >= 7) {
                    Ask(f)(board.areas./~{ r =>
                        Troubles.at(r).any.??(colors./~{ e =>
                            e.at(r).minion.any.?(AssassinateAction(f, e, r, true, then))
                        })
                    }).needOk.bailw(then) { f.log("had no targets") }
                }
                else
                if (roll >= 2 + hrf.HRF.flag("test").??(3)) { 
                    log("No effect")

                    then
                }
                else
                if (f.all(Minion).any)
                    Ask(f, f.all(Minion)./(AssassinateAction(f, f, _, true, then))).needOk
                else {
                    f.log("had no minions")
                    
                    then
                }


            // ROLL TO STEAL OR LOSE MINION
            case ScrollPerformAction(f, RollToTakeNOrRemoveMinion(n), then) =>
                Roll[Int]($(D12), l => RollToTakeNOrRemoveMinionAction(f, l.single.get, n, then))
                
            case RollToTakeNOrRemoveMinionAction(f, roll, n, then) =>
                f.log("rolled", roll.roll)

                if (roll >= 7)
                    Ask(f, factions.but(f)./(t => TakeFromAskAction(f, t, n, then).x(t.money <= 0))).bail(then)
                else
                if (roll >= 2 + hrf.HRF.flag("test").??(3)) { 
                    log("No effect")

                    then
                }
                else
                if (f.all(Minion).any)
                    Ask(f, f.all(Minion)./(AssassinateAction(f, f, _, true, then))).needOk
                else {
                    f.log("had no minions")
                    
                    then
                }


            // ROLL AND KILL
            case ScrollPerformAction(f, RollNKill(n), then) =>
                Roll(Die(board.areas).repeat(n), l => RollNKillMainAction(f, l, then))
                
            case RollNKillMainAction(f, roll, then) =>
                f.log("rolled", roll./(_.index.roll))

                RollNKillContinueAction(f, roll, then)

            case RollNKillContinueAction(f, Nil, then) =>
                then
   
            case RollNKillContinueAction(f, l, then) =>
                Ask(f, board.areas.%(l.has)./~{ r =>
                    (colors.but(f) :+ f)./~{ e =>
                        e.at(r).minion./(_ => RollNKillAction(f, e, r, l, then))
                    }
                }).needOk.bailw(then) { log("No minions") }

            case RollNKillAction(f, e, r, l, then) =>
                Force(AssassinateAction(f, e, r, true, RollNKillContinueAction(f, l.diff($(r)), then)))
                

            // BUSINESS PLAN
            case ScrollPerformAction(f, BusinessPlan, then) =>
                Roll[Int]($(D12), l => RollToTakeNOrPayNOrRemoveMinionAction(f, l.single.get, 4, 2, then))
                
            case RollToTakeNOrPayNOrRemoveMinionAction(f, roll, n, k, then) =>
                f.log("rolled", roll.roll)

                if (roll >= 7)
                    Force(ScrollPerformAction(f, TakeN(n), then))
                else
                if (roll >= 2 + hrf.HRF.flag("test").??(3)) { 
                    log("No effect")
                
                    then
                }
                else
                if (f.all(Minion).any || f.money >= k)
                    Ask(f, f.all(Minion)./(AssassinateAction(f, f, _, true, then)) :+ PayNMainAction(f, k, then).x(f.money >= k)).needOk
                else {
                    f.log("had no minions and not enough money")
                    
                    then
                }

            // DRAW FROM DISCARD
            case ScrollPerformAction(f, DrawFromDiscard(n), then) =>
                Shuffle[DeckCard](pile, DrawFromDiscardAction(f, n, _, then))
                
            case DrawFromDiscardAction(f, n, l, then) =>
                pile --> l.take(n) --> f.hand

                f.log("drew", n.cards, "from discard")
                
                Notify($(f), l.take(n)./(d => ViewCardInfoAction(f, DrewFromTheDiscard(f, NoMessage), d)), then)


            // VIEW UNUSED CHARACTERS
            case ScrollPerformAction(f, ViewUnusedCharacters, then) =>
                Shuffle[Character](characters.diff(factions./~(_.character)), ViewUnusedCharactersAction(f, _, then))

            case ViewUnusedCharactersAction(f, l, then) =>
                f.log("viewed unused", "Personalities".hl)
 
                Ask(f, l.drop(1)./(ViewCharacterInfoAction(f, UnusedChars, _)).ok(then)).needOk
                

            // REROLL CHARACTER
            case ScrollPerformAction(f, RerollCharacter, then) =>
                Shuffle[Character](characters.diff(factions./~(_.character)), RerollCharacterAction(f, _, then))

            case RerollCharacterAction(f, l, then) =>
                f.character = Some(l(0))
                
                f.log("drew new", "Personality".hl)

                Notify($(f), $(ViewCharacterInfoAction(f, DrewNewChar(f), f.character.get)), then)
 

            // REMOVE MINION
            case ScrollPerformAction(f, RemoveMinion(r), then) =>
                Ask(f, colors./~{ t =>
                    t.at(r).minion.any.?(AssassinateAction(f, t, r, true, then))
                }).needOk.cancel


            // OTHERS REMOVE MINIONS
            case ScrollPerformAction(f, OthersRemoveMinions, then) =>
                RemoveOwnMinionsContinueAction(factions.but(f), then)

            case RemoveOwnMinionsContinueAction(Nil, then) =>
                then
            
            case RemoveOwnMinionsContinueAction(f :: l, then) =>
                Ask(f, board.areas./~{ r =>
                    f.at(r).minion./(_ => AssassinateAction(f, f, r, true, RemoveOwnMinionsContinueAction(l, then)))
                }).needOk.bailw(then) { f.log("has no minions") }
                
                
            // PAY TO ASSASSINATE
            case ScrollPerformAction(f, PayNToAssassinate(n), then) =>
                Ask(f, factions.but(f)./(e => PayNToAssassinateAction(f, e, n, then).x(f.money < n, "not enough money")).ocancel).needOk

            case PayNToAssassinateAction(f, e, n, then) =>
                f.money -= n
                e.money += n
            
                f.log("paid", n.money, "to", e)

                Ask(f, board.areas./~{ r =>
                    Troubles.at(r).any.??(colors.but(f)./~{ t =>
                        t.at(r).minion.any.?(AssassinateAction(f, t, r, true, then))
                    })
                }).needOk.bailw(then) { f.log("can't assasinate") }


            // PAY TO MAKE ASSASSINATE
            case ScrollPerformAction(f, PayNToMakeAssassinate(n), then) =>
                Ask(f, factions.but(f)./(e => PayNToMakeAssassinateAction(f, e, n, then).x(f.money < n, "not enough money")).ocancel).needOk

            case PayNToMakeAssassinateAction(f, e, n, then) =>
                f.money -= n
                e.money += n
            
                f.log("paid", n.money, "to", e)

                Ask(e, board.areas./~{ r =>
                    Troubles.at(r).any.??(colors.but(f)./~{ t =>
                        t.at(r).minion.any.?(AssassinateAction(e, t, r, true, then))
                    })
                }).needOk.bailw(then) { e.log("can't assasinate") }


            // PAY TO MOVE
            case ScrollPerformAction(f, PayNToMove(n), then) =>
                Ask(f, factions.but(f)./(e => PayNToMoveAction(f, e, n, then).x(f.money < n, "not enough money")).ocancel).needOk

            case PayNToMoveAction(f, e, n, then) =>
                Ask(f)(f.all(Minion)./(r => MoveMinionFromAction(f, f, r, board.areas, NoMessage, PayNToAction(f, e, n, then)))).cancel


            // MOVE ANYWHERE
            case ScrollPerformAction(f, MoveMinionAnywhere, then) =>
                Ask(f)(f.all(Minion)./(r => MoveMinionFromAction(f, f, r, board.areas, NoMessage, then))).cancel


            // MOVE OWN MINION FROM TROUBLE
            case ScrollPerformAction(f, MoveMinionFromTrouble, then) =>
                Ask(f)(f.all(Minion)./(r => MoveMinionFromAction(f, f, r, board.connected(r), NoMessage, then).x(Troubles.at(r).none))).cancel


            // MOVE ENEMY MINION
            case ScrollPerformAction(f, MoveEnemyMinion, then) =>
                Ask(f)(board.areas./~(r =>
                    colors.but(f)./~(e =>
                        e.at(r).minion./(_ => MoveMinionFromAction(f, e, r, board.connected(r), NoMessage, then))
                    )
                )).cancel


            // MOVE EXCHANGE
            case ScrollPerformAction(f, ExchangeMinions, then) =>
                Ask(f)(board.areas./~(r =>
                    colors./~(e =>
                        e.at(r).minion./(_ => ExchangeMinionsFromAction(f, e, r, then))
                    )
                )).needOk.cancel
                
            case ExchangeMinionsFromAction(f, e1, r1, then) =>
                Ask(f)(board.areas.but(r1)./~(r =>
                    colors./~(e =>
                        e.at(r).minion./(_ => ExchangeMinionsToAction(f, e1, r1, e, r, then))
                    )
                )).needOk.cancel

            case ExchangeMinionsToAction(f, e1, r1, e2, r2, then) =>
                ExchangeMinionsAskAction(f, $(e1, e2).of[Faction], e1, r1, e2, r2, then)

            case ExchangeMinionsAskAction(f, e :: l, e1, r1, e2, r2, then) =>
                val m = $("Prevent exchange", Minion.of(e1), "in", r1, "and", Minion.of(e2), "in", r2)
                Ask(e)(
                    e.hand.has(Gaspode).?(InterruptCardAction(e, Gaspode, then).as(Gaspode)(m))
                )(
                    e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)(m))
                )(
                    ExchangeMinionsAskAction(f, l, e1, r1, e2, r2, then).as("Skip")
                )
            
            case ExchangeMinionsAskAction(f, Nil, e1, r1, e2, r2, then) =>
                ExchangeMinionsAction(f, e1, r1, e2, r2, then)
            
            case ExchangeMinionsAction(f, e1, r1, e2, r2, then) =>
                r1 --> Minion.of(e1) --> r2
                r2 --> Minion.of(e2) --> r1

                log(Minion.of(e1), "in", r1, "exchanged places with", Minion.of(e2), "in", r2)

                $(r1, r2).foreach { r =>
                    val t = colors./~(_.at(r).minion).num > 1

                    if (t && Troubles.at(r).none) {
                        pool --> Trouble --> r
                    
                        log(Trouble, "appeared in", r)
                    }

                    if (t.not && Troubles.at(r).any) {
                        r --> Trouble --> pool
                    
                        log(Trouble, "is gone from", r)
                    }
                }

                then
                


            // BUY BUILDING
            case ScrollPerformAction(f, BuyBuilding(trouble), then) =>
                Ask(f)(board.areas./~(r =>
                    factions.but(f)./~(e =>
                        e.at(r).building./(_ => BuyBuildingAskAction(f, e, r, then).x(Troubles.at(r).any != trouble, trouble.??("no ") + "trouble").x(r.cost > f.money, "not enough money"))
                    )
                )).cancel
                
            case BuyBuildingAskAction(f, e, r, then) =>
                Ask(e)(e.hand.has(WallaceSonky).?(InterruptCardAction(e, WallaceSonky, then).as(WallaceSonky)("Prevent", f, "buying", Building.of(e), "in", r)))(BuyBuildingAction(f, e, r, then).as("Skip"))

            case BuyBuildingAction(f, e, r, then) =>
                r --> Building.of(e) --> e.pool

                f.pool --> Building.of(f) --> r
                
                f.money -= r.cost
                e.money += r.cost

                f.used :+= r

                f.log("bought", Building.of(e), "in", r)

                then


            // BUILD HALF PRICE
            case ScrollPerformAction(f, BuildHalfPrice, then) =>
                Ask(f, board.areas./(r => BuildAction(f, r, r.cost / 2, then).x(factions.%(_.at(r).building.any).any, "exists").x(f.at(r).minion.none, "no minion").x(Troubles.at(r).any, "trouble").x(r.cost / 2 > f.money, "not enough money")).cancel.okNeeded)
                

            // --- RANDOM EVENTS ---

            // FOG
            case RandomEventAction(f, Fog, then) =>
                val l = deck.take(5)

                deck --> l --> discard

                log("Fog discarded", l)

                then
 
            // RIOTS
            case RandomEventAction(f, Riots, then) =>
                if (Troubles.all(Trouble).num >= 8) {
                    log("Riots ended the game")
                }
                else 
                    log("Riots unsuccessful")

                then
                
            // DEMONS
            case RandomEventAction(f, DemonsFromTheDungeonDimensions, then) =>
                Roll(Die(board.areas).repeat(4), l => PlaceMinionsAction(f, Demons, l, then))
                
            // TROLLS
            case RandomEventAction(f, TheTrolls, then) =>
                Roll(Die(board.areas).repeat(3), l => PlaceMinionsAction(f, Trolls, l, then))
                
            case PlaceMinionsAction(f, e, roll, then) =>
                f.log("rolled", roll./(_.index.roll))

                ProcessEffectsAction(factions, true, false, roll./(PlaceMinionEffect(_, e)), then)

            // EXPLOSION
            case RandomEventAction(f, Explosion, then) =>
                Roll(Die(board.areas).repeat(1), l => RemoveRolledBuildingsAction(f, l, then))

            // EARTHQUAKE
            case RandomEventAction(f, Earthquake, then) =>
                Roll(Die(board.areas).repeat(2), l => RemoveRolledBuildingsAction(f, l, then))

            case RemoveRolledBuildingsAction(f, roll, then) =>
                f.log("rolled", roll./(_.index.roll))

                ProcessEffectsAction(factions, true, false, roll./~(r => factions./~(e => e.at(r).building./(_ => RemoveBuildingEffect(r, e)))), then)

            // FLOOD
            case RandomEventAction(f, Flood, then) =>
                Roll(Die(board.areas).repeat(2), l => FloodAreasAction(f, l, then))

            case FloodAreasAction(f, roll, then) =>
                f.log("rolled", roll./(_.index.roll))

                val l = roll.distinct.intersect(board.flooded)
                
                ProcessEffectsAction(factions, true, false, l./~(r => factions./~(e => e.at(r).minion./(_ => MoveAwayMinionEffect(r, e, board.connected(r).diff(l))))), then)
                
            // DRAGON
            case RandomEventAction(f, TheDragon, then) =>
                Random(board.areas, r => RemoveRolledEverythingAction(f, r, then))
 
            case RemoveRolledEverythingAction(f, r, then) =>
                f.log("rolled", r.index.roll)
                
                ProcessEffectsAction(factions, true, false, factions./~(e => e.at(r).building./(_ => RemoveBuildingEffect(r, e)) ++ e.at(r).minion./(_ => RemoveMinionEffect(r, e))) ++ 
                    colors.diff(factions).but(Troubles)./~(e => e.at(r).minion./(_ => RemoveMinionEffect(r, e))), then)

            // FIRE
            case RandomEventAction(f, Fire, then) =>
                Random(board.areas, RemoveRolledBuildingIfNearAction(f, None, Nil, _, then))

            case RemoveRolledBuildingIfNearAction(f, o, l, r, then) =>
                f.log("rolled", r.index.roll)

                val t = o./(o => board.connected(o).has(r).?(r)).|(Some(r))
                
                if (t.exists(r => factions.exists(e => e.at(r).building.any)))
                    Random(board.areas, RemoveRolledBuildingIfNearAction(f, Some(r), l :+ r, _, then))
                else
                    ProcessEffectsAction(factions, true, false, l./~(r => factions./~(e => e.at(r).building.any.?(RemoveBuildingEffect(r, e)))), then)

            // MURDERS
            case RandomEventAction(f, MysteriousMurders, then) =>
                var q = then

                factions.reverse.foreach { e => q = MysteriousMurdersNextAction(e, q) }
                
                q

            case MysteriousMurdersNextAction(f, then) =>
                Random(board.areas, r => MysteriousMurdersMainAction(f, r, then))
                
            case MysteriousMurdersMainAction(f, r, then) =>
                f.log("rolled", r.index.roll)

                Ask(f)((colors.but(f) :+ f)./~(e => e.at(r).minion./(_ => MysteriousMurdersAction(f, e, r, then)))).needOk.bailw(then) { log("No minions") }

            case MysteriousMurdersAction(f, e, r, then) =>
                ProcessEffectsAction(factions, true, false, $(RemoveMinionEffect(r, e)), then)
                
            case ProcessEffectsAction(f :: ff, event, text, l, then) =>
                val sg = (event && f.all(Building).has(StandardBoard.SmallGods)).??(l)./~{
                    case e @ RemoveMinionEffect(r, c) if c == f => Some(e)
                    case e @ RemoveBuildingEffect(r, c) if c == f => Some(e)
                    case e @ MalfunctionEffect(r, c) if c == f => Some(e)
                    case e @ MoveAwayMinionEffect(r, c, dest) if c == f => Some(e)
                    case e @ PlaceMinionEffect(r, c) if c.as[Faction].none && f.at(r).any => Some(e)
                    case _ => None
                }

                val ss = f.hand.has(Susan).??(l)./~{
                    case e @ RemoveMinionEffect(r, c) if c == f => Some(e)
                    case _ => None
                }

                val gp = (event.not && f.hand.has(Gaspode)).??(l)./~{
                    case e @ RemoveMinionEffect(r, c) if c == f => Some(e)
                    case e @ MoveAwayMinionEffect(r, c, dest) if c == f && dest.num < 11 => Some(e)
                    case _ => None
                }

                val fs = f.hand.has(FreshStartClub).??(l)./~{
                    case e @ RemoveMinionEffect(r, c) if c == f => Some(e)
                    case _ => None
                }

                val ig = f.hand.has(Igor).??(l)./~{
                    case e @ RemoveMinionEffect(r, c) if c == f => Some(e)
                    case _ => None
                }

                val ws = (text && f.hand.has(WallaceSonky)).??(l)./~{
                    case e @ RemoveMinionEffect(r, c) if c == f => Some(e)
                    case e @ RemoveBuildingEffect(r, c) if c == f => Some(e)
                    case e @ MalfunctionEffect(r, c) if c == f => Some(e)
                    case e @ MoveAwayMinionEffect(r, c, dest) if c == f => Some(e)
                    case _ => None
                }

                Ask(f)(
                    sg./(e => CancelEffectMoneyAction(f, event, text, l, e, 3, then).x(f.money < 3, "no money"))
                )(
                    ss./(e => CancelEffectCardAction(f, event, text, l, e, Susan, then))
                )(
                    gp./(e => CancelEffectCardAction(f, event, text, l, e, Gaspode, then))
                )(
                    ws./(e => CancelEffectCardAction(f, event, text, l, e, WallaceSonky, then))
                )(
                    fs./(e => ReplaceEffectCardAction(f, event, text, l, e, FreshStartClub, then))
                )(
                    ig./(e => ReplaceEffectCardAction(f, event, text, l, e, Igor, then))
                )(
                    ProcessEffectsAction(ff, event, text, l, then).as("Skip")
                )
                
            case CancelEffectMoneyAction(f, event, text, l, e, n, then) =>
                f.money -= n

                f.log("paid", n.money, "to prevent", e)
                
                ProcessEffectsAction(factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f), event, text, l.:-(e), then)

            case CancelEffectCardAction(f, event, text, l, e, d, then) =>
                f.hand --> d --> discard(f)

                f.log("used", d, "to prevent", e)
                
                ProcessEffectsAction(factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f), event, text, l.:-(e), then)

            case ReplaceEffectCardAction(f, event, text, l, e @ RemoveMinionEffect(r, ff : Faction), d, then) =>
                f.hand --> d --> discard(f)

                f.log("used", d, "to move away removed", Minion.of(ff), "in", r)
                
                ProcessEffectsAction(factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f), event, text, l.:-(e).:+(MoveAwayMinionEffect(r, ff, board.areas.but(r))), then)

            case ProcessEffectsAction(Nil, _, _, ll, then) =>
                var q : ForcedAction = PostEffectsAction(then)
                
                ll.reverse.foreach { e => q = PerformEffectAction(e, q) }

                q

            case PerformEffectAction(MoveAwayMinionEffect(r, f, l), then) =>
                Ask(f)(l./(MoveMinionToAction(f, f, r, _, NoMessage, then)))
                
            case PerformEffectAction(RemoveMinionEffect(r, f), then) =>
                removeMinion(f, r)
                
                log(Minion.of(f), "was removed from", r)

                then

            case PerformEffectAction(PlaceMinionEffect(r, f), then) =>
                val already = Troubles.at(r).none && (colors./~(_.at(r).minion).any || f == Demons)
                
                pool --> Minion.of(f) --> r
                
                log(Minion.of(f), "appeared in", r)
                
                if (already) {
                    pool --> Trouble --> r
                    
                    log(Trouble, "appeared in", r)
                }

                then
                
            case PerformEffectAction(RemoveBuildingEffect(r, f), then) =>
                r --> Building.of(f) --> f.pool

                log(Building.of(f), "was destroyed in", r)

                then

            case PerformEffectAction(MalfunctionEffect(r, f), then) =>
                malfunctions :+= r
 
                log(Building.of(f), "in", r, "malfunctioned")

                then
                                   
            case PostEffectsAction(then) =>
                then

                
            // SUBSIDENCE
            case RandomEventAction(f, Subsidence, then) =>
                var q = then

                factions.reverse.foreach { e => q = SubsidenceContinueAction(e, q) }

                q

            case SubsidenceContinueAction(f, then) =>
                val l = f.all(Building)

                if (l.none)
                    then
                else
                    Ask(f, PayNMainAction(f, l.num * 2, then).x(f.money < l.num * 2) :: l./(RemoveBuildingAction(f, f, _, SubsidenceContinueAction(f, then))))
                
            case RemoveBuildingAction(f, e, r, then) =>
                r --> Building.of(e) --> e.pool

                f.log("removed", Building.of(e), "in", r)

                then

            // BSJ
            case RandomEventAction(f, BloodyStupidJohnson, then) =>
                Random(board.areas, r => BuildingMalfunctionAction(f, r, then))

            case BuildingMalfunctionAction(f, r, then) =>
                f.log("rolled", r.index.roll)
                
                ProcessEffectsAction(factions, true, false, factions.%(_.at(r).building.any)./~(e => $(MalfunctionEffect(r, e)) ++ e.at(r).minion.any.?(RemoveMinionEffect(r, e))), then)
 
            // NOT IMPLEMENTED
            case ScrollPerformAction(f, a, then) =>
                log(a.toString.hl, "action not implemented")
 
                then
            
            case RandomEventAction(f, v, then) =>
                log(v.name.hl, "event not implemented")

                then

            // GAME OVER
            case GameOverAction(winners) =>
                GameOver(winners, "Game Over", $(
                    Ask(null, winners./~(f => $(ViewCharacterInfoAction(f, NoMessage, f.character.get), GameOverWonAction(null, f))) ++ (winners ++ factions.diff(winners))./~(f => 
                        f.records./(ClaimRecordInfoAction(f, _))
                    ))
                ))
                

            // HELPERS
            case a : SelfPerform =>
                a.perform(this)
        }
    }        
    
}
