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


trait CardEffect extends DisplayEffect

trait Card extends Elementary {
    def name : String
    def suit : Suit
    def elem = (hrf.elem.FigureSpace + name + hrf.elem.FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(suit))
    def img : Image
    def matches(cost : SuitCost) = suit.matches(cost)
}

case object Card extends Message {
    def elem(implicit game : Game) = "card"
}

case class Cards(n : Int) extends Message with Elementary {
    def elem(implicit game : Game) = (n == 1).?("card").|("cards")
    def elem = (n == 1).?("card").|("cards")
}

case object Cards extends Message {
    def elem(implicit game : Game) = "cards"
}

object NCards {
    def apply(n : Int) = (n == 1).?("card").|("cards")
}


trait DeckCard extends Card {
    def id : String
    def img = Image(id, styles.card).alt(altS)
    def altS = suit.toString + " " + name
    def altL : String
    override def toString = "DeckCard(" + id + ")"
}

trait CraftCard extends DeckCard {
    def cost : $[SuitCost]
}

case class Ambush(suit : Suit) extends DeckCard {
    def name = suit.name + " Ambush!"
    def id = suit.name + "-ambush"
    override def altS = name
    def altL = name + " - deal two hits to the attacker defending in " + (suit == Bird).?("any").|(suit.toString) + " clearing"
}

case class CraftItemCard(suit : Suit, id : String, cost : $[SuitCost], item : Item, vp : Int, name : String) extends CraftCard {
    def altL = altS + " - craft item " + item + " - for " + $("zero", "one", "two", "three")(vp) + " victory point" + (vp > 1).??("s") + " - crafting cost " + cost./(_.toString).join(" ")
}

case class CraftEffectCard(suit : Suit, id : String, cost : $[SuitCost], effect : CardEffect) extends CraftCard {
    def name = effect.name
    def altL = altS + " - craft effect " + effect + " - crafting cost " + cost./(_.toString).join(" ")
}

case class Favor(suit : BaseSuit, id : String, name : String) extends CraftCard {
    val cost = $(suit, suit, suit)
    def altL = altS + " - craft " + suit + " favor - removing all enemy pieces in matching clearings - crafting cost " + cost./(_.toString).join(" ")
}

trait Dominance extends DeckCard {
    val suit : Suit
}

case class BaseSuitDominance(suit : BaseSuit) extends Dominance {
    def name = suit.name + " Dominance"
    def id = suit.name + "-dominance"
    override def altS = name
    def altL = name + " - change own win condition - now win only if ruling 3 matching clearings at the start of the turn"
}

case class CornersDominance(suit : Suit) extends Dominance {
    def name = suit.name + " Dominance"
    def id = suit.name + "-dominance"
    override def altS = name
    def altL = name + " - change own win condition - now win only if ruling 2 opposite corner clearings at the start of the turn"
}

case object FrogDominance extends Dominance {
    val suit = Frog
    def name = suit.name + " Dominance"
    def id = suit.name + "-dominance-half"
    override def altS = name
    def altL = name + " - change own win condition - now win only if ruling more Frog clearings than each other player"
}


object Deck {
    def fromOptions(l : $[Meta.O], arity : Int, known : $[Faction]) = l.of[DeckOption].last @@ {
        case StandardDeck => ambushes ++ items ++ effectsBase ++ (arity > 2).??(dominances)
        case ExilesDeck => ambushes ++ items ++ effectsExiles ++ (arity > 2).??(dominances)
        case MixedDeck => ambushes ++ items ++ mixed(known) ++ (arity > 2).??(dominances)
        case DuskDeck => ambushes ++ items ++ effectsDusk ++ (arity > 2).??(dominances)
    }

    val ambushes = $[DeckCard](
        Ambush(Bird),
        Ambush(Bird),
        Ambush(Fox),
        Ambush(Rabbit),
        Ambush(Mouse),
    )

    val dominances = $[DeckCard](
        CornersDominance(Bird),
        BaseSuitDominance(Fox),
        BaseSuitDominance(Rabbit),
        BaseSuitDominance(Mouse),
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

    val effectsBase = $[DeckCard](
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

    val favors = effectsBase.of[Favor]

    def mixed(known : $[Faction]) = $[DeckCard](
        CraftEffectCard(Bird, "armorers", $(Fox), Armorers),
        CraftEffectCard(Bird, "sappers", $(Mouse), Sappers),
        CraftEffectCard(Bird, "brutal-tactics", $(Fox, Fox), BrutalTactics),
        CraftEffectCard(Bird, "royal-claim", $(AnySuit, AnySuit, AnySuit, AnySuit), RoyalClaim),
        CraftEffectCard(Bird, "borscht-kitchens", $(Fox, Rabbit, Mouse), BorschtKitchens),

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

        CraftEffectCard(Bird, "saboteurs", $(AnySuit), Saboteurs),
    ) ++ (
        known.of[Mischief].none.$(CraftEffectCard(Bird, "corvid-planners", $(AnySuit, AnySuit), CorvidPlanners)) ++
        (known.has(RF).not && known.of[CommonInvasive].none).$(CraftEffectCard(Bird, "boat-builders", $(AnySuit, AnySuit), BoatBuilders))
    )

    val effectsExiles = $[DeckCard](
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

    val effectsExtra = $[DeckCard](
        CraftEffectCard(Bird, "borscht-kitchens", $(Fox, Rabbit, Mouse), BorschtKitchens),
    )

    val effectsDusk = $[DeckCard](
        CraftEffectCard(Fox, "sly-dog", $(Fox), SlyDog),
        CraftEffectCard(Fox, "high-noon", $(Rabbit, AnySuit), HighNoon),
        CraftEffectCard(Fox, "tricky-claws", $(Fox), TrickyClaws),
        CraftEffectCard(Fox, "off-trail", $(Fox, AnySuit), OffTrail),
        CraftEffectCard(Fox, "off-trail", $(Fox, AnySuit), OffTrail),
        CraftEffectCard(Fox, "fox-guard", $(Fox), Guard(Fox)),
        CraftEffectCard(Fox, "service-of-the-foxes", $(Fox, Fox), ServiceOfTheFoxes),
        CraftEffectCard(Rabbit, "tricky-ears", $(Rabbit), TrickyEars),
        CraftEffectCard(Rabbit, "merchants", $(Rabbit), Merchants),
        CraftEffectCard(Rabbit, "merchants", $(Rabbit), Merchants),
        CraftEffectCard(Rabbit, "military-supplies", $(Rabbit, AnySuit), MilitarySupplies),
        CraftEffectCard(Rabbit, "military-supplies", $(Rabbit, AnySuit), MilitarySupplies),
        CraftEffectCard(Rabbit, "adventurers", $(Rabbit, Rabbit), Adventurers),
        CraftEffectCard(Rabbit, "rabbit-guard", $(Rabbit), Guard(Rabbit)),
        CraftEffectCard(Rabbit, "dusk-awakening", $(Mouse, AnySuit), DuskAwakening),
        CraftEffectCard(Rabbit, "service-of-the-rabbits", $(Rabbit, Rabbit), ServiceOfTheRabbits),
        CraftEffectCard(Mouse, "public-relations", $(Mouse, AnySuit), PublicRelations),
        CraftEffectCard(Mouse, "public-relations", $(Mouse, AnySuit), PublicRelations),
        CraftEffectCard(Mouse, "mouse-guard", $(Mouse), Guard(Mouse)),
        CraftEffectCard(Mouse, "tricky-tails", $(Mouse), TrickyTails),
        CraftEffectCard(Mouse, "old-friend", $(Mouse), OldFriend),
        CraftEffectCard(Mouse, "breaking-dawn", $(Fox, AnySuit), BreakingDawn),
        CraftEffectCard(Mouse, "service-of-the-mice", $(Mouse, Mouse), ServiceOfTheMice),
        CraftEffectCard(Bird, "spoil", $(AnySuit), Spoil),
        CraftEffectCard(Bird, "spoil", $(AnySuit), Spoil),
        CraftEffectCard(Bird, "spoil", $(AnySuit), Spoil),
        CraftEffectCard(Bird, "night-terrors", $(AnySuit, AnySuit), NightTerrors),
        CraftEffectCard(Bird, "night-terrors", $(AnySuit, AnySuit), NightTerrors),
        CraftEffectCard(Bird, "forest-vigil", $(Fox, Rabbit, Mouse), ForestVigil),
        CraftEffectCard(Bird, "hoarders", $(AnySuit), Hoarders),
    )

    val effectsRemixRedawn = $[DeckCard](
        CraftEffectCard(Fox, "sly-dog", $(Fox), SlyDog),
        CraftEffectCard(Rabbit, "adventurers", $(Rabbit, Rabbit), Adventurers),
    )

    val frogAAA = $[DeckCard](
        Ambush(Frog),
        BaseSuitDominance(Frog),
        Favor(Frog, "favor-of-the-frogs", "Favor of the Frogs"),
        CraftEffectCard(Frog, "frog-engineers", $(Frog), FrogEngineers),
        CraftEffectCard(Frog, "frog-engineers", $(Frog), FrogEngineers),
        CraftEffectCard(Frog, "frog-engineers", $(Frog), FrogEngineers),
        CraftEffectCard(Frog, "frog-engineers", $(Frog), FrogEngineers),
        CraftEffectCard(Frog, "frog-engineers", $(Frog), FrogEngineers),
        CraftEffectCard(Frog, "frog-engineers", $(Frog), FrogEngineers),
        CraftEffectCard(Frog, "frog-engineers", $(Frog), FrogEngineers),
        CraftEffectCard(Frog, "frog-engineers", $(Frog), FrogEngineers),
        CraftEffectCard(Frog, "frog-engineers", $(Frog), FrogEngineers),
    )

    val frogBBB = $[DeckCard](
        Ambush(Frog),
        Ambush(Frog),
        FrogDominance,
        CraftEffectCard(Frog, "favor-of-the-frogs-half", $(Frog, Frog), MilitantFavor(LDvB)),
        CraftEffectCard(Frog, "favor-of-the-frogs-half", $(Frog, Frog), MilitantFavor(LDvB)),
        CraftEffectCard(Frog, "imitation", $(Frog, Frog), Imitation),
        CraftEffectCard(Frog, "imitation", $(Frog, Frog), Imitation),
        CraftEffectCard(Frog, "imitation", $(Frog, Frog), Imitation),
        CraftEffectCard(Frog, "fox-integration", $(Fox), Integration(LDvB, Fox)),
        CraftEffectCard(Frog, "fox-integration", $(Fox), Integration(LDvB, Fox)),
        CraftEffectCard(Frog, "rabbit-integration", $(Rabbit), Integration(LDvB, Rabbit)),
        CraftEffectCard(Frog, "rabbit-integration", $(Rabbit), Integration(LDvB, Rabbit)),
        CraftEffectCard(Frog, "mouse-integration", $(Mouse), Integration(LDvB, Mouse)),
        CraftEffectCard(Frog, "mouse-integration", $(Mouse), Integration(LDvB, Mouse)),
    )

    val frogCCC = $[DeckCard](
        Ambush(Frog),
        Ambush(Frog),
        FrogDominance,
        CraftEffectCard(Frog, "favor-of-the-frogs-half", $(Frog, Frog), MilitantFavor(LDvC)),
        CraftEffectCard(Frog, "favor-of-the-frogs-half", $(Frog, Frog), MilitantFavor(LDvC)),
        CraftEffectCard(Frog, "imitation", $(Frog, Frog), Imitation),
        CraftEffectCard(Frog, "imitation", $(Frog, Frog), Imitation),
        CraftEffectCard(Frog, "imitation", $(Frog, Frog), Imitation),
        CraftEffectCard(Frog, "laborers", $(AnySuit), Laborers(LDvC)),
        CraftEffectCard(Frog, "laborers", $(AnySuit), Laborers(LDvC)),
        CraftEffectCard(Frog, "laborers", $(AnySuit), Laborers(LDvC)),
        CraftEffectCard(Frog, "peace-talks", $(Frog), PeaceTalks(LDvC)),
        CraftEffectCard(Frog, "peace-talks", $(Frog), PeaceTalks(LDvC)),
        CraftEffectCard(Frog, "peace-talks", $(Frog), PeaceTalks(LDvC)),
    )

    val frogDDD = $[DeckCard](
        Ambush(Frog),
        Ambush(Frog),
        FrogDominance,
        CraftEffectCard(Frog, "fox-laborers", $(AnySuit), SuitLaborers(LDvD, Fox)),
        CraftEffectCard(Frog, "rabbit-laborers", $(AnySuit), SuitLaborers(LDvD, Rabbit)),
        CraftEffectCard(Frog, "mouse-laborers", $(AnySuit), SuitLaborers(LDvD, Mouse)),
        CraftEffectCard(Frog, "peace-talks-flip", $(Frog), PeaceSootheTalks(LDvD)),
        CraftEffectCard(Frog, "peace-talks-flip", $(Frog), PeaceSootheTalks(LDvD)),
        CraftEffectCard(Frog, "incite-conflict", $(Frog, AnySuit), InciteConflict(LDvD)),
        CraftEffectCard(Frog, "incite-conflict", $(Frog, AnySuit), InciteConflict(LDvD)),
    )

    val catalog = ambushes ++ dominances ++ items ++ effectsBase ++ effectsExiles ++ effectsExtra ++ effectsDusk ++ frogDDD ++ frogCCC ++ frogBBB ++ frogAAA
}


case class PeekCardsMainAction(self : Faction, s : Elem, m : Message, l : $[DeckCard], then : ForcedAction) extends ForcedAction

case class ViewCardInfoAction(self : Player, s : Elem, d : Card) extends BaseInfo(s)(d.img) with ViewCard with OnClickInfo { def param = d }

case class ShufflePileAction(shuffled : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]

case class DrawCardsAction(f : Faction, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class DrawCardsFromDeckAction(f : Faction, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class AddCardsAction(f : Faction, then : ForcedAction) extends ForcedAction

case class HandLimitAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class HandLimitDiscardAction(f : Faction, l : $[DeckCard], then : ForcedAction) extends ForcedAction

case class OpportunityDiscardCardsAction(f : Faction, m : Message, n : Int, l : $[DeckCard], t : SuitCost, then : ForcedAction, alt : ForcedAction) extends ForcedAction
case class OpportunityDiscardSelectCardAction(self : Faction, m : Message, t : SuitCost, d : DeckCard, then : ForcedAction) extends BaseAction(self, "can discard", (t != AnySuit).?("" ~ t.elem ~ " card"), m)(d.img) with ViewCard with WrappedAction

case class OptionalDiscardCardAction(f : Faction, m : Message, t : SuitCost, then : ForcedAction) extends ForcedAction with Soft
case class OpportunityDiscardCardAction(f : Faction, m : Message, t : SuitCost, then : ForcedAction, alt : ForcedAction) extends ForcedAction

case class DiscardDrawnAction(self : Faction, then : ForcedAction) extends ForcedAction with ExpandThen

case class PrepareDiscardCardsAction(f : Faction, l : $[DeckCard], then : ForcedAction) extends ForcedAction with ExpandThen
case class DiscardRandomCardAction(f : Faction, then : ForcedAction) extends ForcedAction
case class PerformDiscardRandomCardAction(f : Faction, random : DeckCard, then : ForcedAction) extends RandomAction[DeckCard]

case class StealCardAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction
case class TakeCardAction(self : Faction, e : Faction, random : DeckCard, then : ForcedAction) extends RandomAction[DeckCard]
case class MassStealCardAction(self : Faction, l : $[Faction], then : ForcedAction) extends ForcedAction


object CardsExpansion extends MandatoryExpansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // CARDS AND HAND
        case ShufflePileAction(l, then) =>
            pile --> l --> deck

            if (game.turn > 0)
                log("The discard pile was shuffled")
            else
                log("The shared deck was shuffled")

            Milestone(then)

        case PeekCardsMainAction(f, s, m, l, then) =>
            f.notify(l./(d => ViewCardInfoAction(f, m.elem(game), d)))

            then

        case DrawCardsAction(f, n, m, then) =>
            DrawCardsFromDeckAction(f, n, m, then)

        case DrawCardsFromDeckAction(f, n, m, then) =>
            if (deck.num < n && pile.any)
                Shuffle[DeckCard](pile, ShufflePileAction(_, DrawCardsFromDeckAction(f, n, m, then)))
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

                    f.log("drew", n.of("card"), f.is[Trader].??(l), m)



                    f.notify(l./(d => ViewCardInfoAction(f, DrewFromTheDeck(f, m).elem(game), d)))
                }

                then
            }

        case AddCardsAction(f, then) =>
            f.drawn --> f.hand
            then

        // DISCARDS
        case HandLimitAction(f, nn, then) =>
            val min = nn
            val max = nn + f.has(PublicRelations).??(1)

            if (f.hand.num <= min)
                then
            else {
                soft()

                if (f.hand.num <= 2 * min && max > 3)
                    XXSelectObjectsAction(f, f.hand)
                        .withGroup(f.elem ~ " discards " ~ $(f.hand.num - max, f.hand.num - min).distinct./(_.cards).join(" or ") ~ " down to hand limit")
                        .withRuleNone(_.atLeast(f.hand.num - max).upTo(f.hand.num - min))
                        .withThen(HandLimitDiscardAction(f, _, then))(l => "Discard".hl ~ l.none.??(" None") ~ l./(" " ~ _.elem))
                        .withExtra($(NoHand))
                else
                    XXSelectObjectsAction(f, f.hand)
                        .withGroup(f.elem ~ " keeps " ~ $(min, max).distinct./(_.cards).join(" or ") ~ (min == 5 || max == 6).??(" to hand limit"))
                        .withRule(_.atLeast(min).upTo(max))
                        .withThen(l => HandLimitDiscardAction(f, f.hand.diff(l), then))(l => "Keep".hl ~ l./(" " ~ _.elem))
                        .withExtra($(NoHand))
            }

        case HandLimitDiscardAction(f, l, then) =>
            if (l.any)
                f.hand --> l --> discard(f, "due to hand limit")

            then

        case OpportunityDiscardCardsAction(f, m, n, l, t, then, alt) =>
            if (l.num == n)
                PrepareDiscardCardsAction(f, l, then)
            else {
                val h = f.hand.diff(l)

                if (l.num + h.%(_.matches(t)).num < n)
                    alt
                else {
                    soft()

                    Ask(f)(NoHand)
                      .each(h)(d =>
                        OpportunityDiscardSelectCardAction(f, IofN(m, l.num + 1, n), t, d,
                          OpportunityDiscardCardsAction(f, m, n, d +: l, t, then, alt)
                        ).x(d.matches(t).not))
                      .skip(alt)
                }
            }

        case OpportunityDiscardCardAction(f, m, t, then, alt) =>
            val h = f.hand.get

            if (h.%(_.matches(t)).none)
                Ask(f)(alt.as("Skip"))
            else {
                soft()

                Ask(f)(NoHand)
                  .each(h)(d =>
                    OpportunityDiscardSelectCardAction(f, IofN(m, 1, 1), t, d,
                      PrepareDiscardCardsAction(f, $(d), then)
                    ).x(d.matches(t).not))
                  .skip(alt)
            }

        case OptionalDiscardCardAction(f, m, t, then) if f.hand.%(_.matches(t)).none =>
            Ask(f).cancel

        case OptionalDiscardCardAction(f, m, t, then) =>
            Ask(f)(NoHand)
              .each(f.hand.get)(d =>
                OpportunityDiscardSelectCardAction(f, IofN(m, 1, 1), t, d,
                  PrepareDiscardCardsAction(f, $(d), then)
                ).!(d.matches(t).not))
              .cancel

        case PrepareDiscardCardsAction(f, l, then) =>
            f.hand --> l --> f.drawn
            then

        case DiscardDrawnAction(f, then) =>
            f.drawn --> discard(f)
            then

        case DiscardRandomCardAction(f, then) if f.hand.none =>
            then

        case DiscardRandomCardAction(f, then) =>
            Random[DeckCard](f.hand, r => PerformDiscardRandomCardAction(f, r, then))

        case PerformDiscardRandomCardAction(f, d, then) =>
            f.hand --> d --> discard(f, "randomly")
            then

        // STEAL
        case StealCardAction(f, e, then) =>
            Random[DeckCard](e.hand, r => TakeCardAction(f, e, r, then))

        case TakeCardAction(f, e, d, then) =>
            e.hand --> d --> f.hand

            f.notify($(ViewCardInfoAction(f, StoleFrom(f, e).elem(game), d)))
            e.notify($(ViewCardInfoAction(e, StoleFrom(f, e).elem(game), d)))

            then

        case MassStealCardAction(f, Nil, then) =>
            then

        case MassStealCardAction(f, e :: r, then) if e.hand.none =>
            MassStealCardAction(f, r, then)

        case MassStealCardAction(f, e :: r, then) =>
            f.log("stole a card from", e)

            StealCardAction(f, e, MassStealCardAction(f, r, then))

        case _ => UnknownContinue
    }

}
