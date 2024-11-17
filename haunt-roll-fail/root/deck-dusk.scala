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


case class Guard(suit : BaseSuit) extends CardEffect with BattleEffect {
    override val name = suit.name + " Guard"
}

case object TrickyClaws extends CardEffect with BattleEffect {
    override val name = "Tricky Claws"
}

case object TrickyEars extends CardEffect with BattleEffect {
    override val name = "Tricky Ears"
}

case object TrickyTails extends CardEffect with BattleEffect {
    override val name = "Tricky Tails"
}

case object BreakingDawn extends CardEffect with SpecialRegion {
    override val name = "Breaking Dawn"
    override def elem(implicit game : Game) = name.hl
}

case object HighNoon extends CardEffect with SpecialRegion {
    override val name = "High Noon"
    override def elem(implicit game : Game) = name.hl
}

case object DuskAwakening extends CardEffect with SpecialRegion {
    override val name = "Dusk Awakening"
    override def elem(implicit game : Game) = name.hl
}

case object SlyDog extends CardEffect with BattleEffect {
    override val name = "Sly Dog"
}

case object Merchants extends CardEffect {
    override val name = "Merchants"
}

case object OldFriend extends CardEffect {
    override val name = "Old Friend"
}

case object MilitarySupplies extends CardEffect {
    override val name = "Military Supplies"
}

case object PublicRelations extends CardEffect {
    override val name = "Public Relations"
}

case object Adventurers extends CardEffect {
    override val name = "Adventurers"
}

case object Spoil extends CardEffect {
    override val name = "Spoil"
}

case object NightTerrors extends CardEffect {
    override val name = "Night Terrors"
}

case object ForestVigil extends CardEffect {
    override val name = "Forest Vigil"
}

case object FowlPlay extends CardEffect with BattleEffect {
    override val name = "Fowl Play"
}

case object Hoarders extends CardEffect {
    override val name = "Hoarders"
}

trait ServiceOfThe extends CardEffect {
    val suit : BaseSuit
}

object ServiceOfThe {
    def apply(s : BaseSuit) = s @@ {
        case Fox => ServiceOfTheFoxes
        case Rabbit => ServiceOfTheRabbits
        case Mouse => ServiceOfTheMice
    }
}

case object ServiceOfTheFoxes extends ServiceOfThe {
    override val name = "Service of the Foxes"
    val suit = Fox
}

case object ServiceOfTheRabbits extends ServiceOfThe {
    override val name = "Service of the Rabbits"
    val suit = Rabbit
}

case object ServiceOfTheMice extends ServiceOfThe {
    override val name = "Service of the Mice"
    val suit = Mouse
}


case class SpoilMainAction(self : Faction, e : Faction, d : DeckCard) extends BaseAction(Spoil, "discards")(d, "of", e)
case class SpoilCounterAction(self : Faction, e : Faction, t : DeckCard, d : DeckCard) extends BaseAction("Counter", Spoil, "with")(d.img) with ViewCard
case class SpoilAction(self : Faction, e : Faction, d : DeckCard) extends ForcedAction

case class ServiceOfMainAction(self : Faction, s : BaseSuit) extends BaseAction()(ServiceOfThe(s)) with Soft
case class ServiceOfAction(self : Faction, s : BaseSuit, c : Clearing) extends BaseAction(ServiceOfThe(s), "in")(c)
case class ServiceOfFailAction(self : Faction, s : BaseSuit) extends BaseAction(ServiceOfThe(s), "in")("No suitable clearings")

case class MerchantsMainAction(self : Faction, then : ForcedAction) extends BaseAction(Daylight)(Merchants) with Soft
case class MerchantsAction(self : Faction, i : ItemRef, then : ForcedAction) extends ForcedAction
case class MerchantsContinueAction(self : Faction, then : ForcedAction) extends ForcedAction
case class MerchantsDiscardAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(Merchants, "discard", 1.cards)(d.img) with ViewCard

case class AdventurersAction(self : Faction, then : ForcedAction) extends ForcedAction

case class BreakingDawnMainAction(self : Faction, feed : Boolean, milk : Int, then : ForcedAction) extends ForcedAction with Soft
case class BreakingDawnFeedAction(self : Faction, then : ForcedAction) extends ForcedAction
case class BreakingDawnMilkAction(self : Faction, then : ForcedAction) extends ForcedAction

case class HighNoonMainAction(self : Faction, feed : Boolean, milk : Int, then : ForcedAction) extends ForcedAction with Soft
case class HighNoonFeedAction(self : Faction, then : ForcedAction) extends ForcedAction
case class HighNoonMilkAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class HighNoonAction(self : Hero, then : ForcedAction) extends ForcedAction
case class HighNoonPlaceAction(self : WarriorFaction, c : Region, then : ForcedAction) extends ForcedAction

case class DuskAwakeningMainAction(self : Faction, feed : Boolean, milk : Int, then : ForcedAction) extends ForcedAction with Soft
case class DuskAwakeningFeedAction(self : Faction, then : ForcedAction) extends ForcedAction
case class DuskAwakeningMilkAction(self : Faction, then : ForcedAction) extends ForcedAction

case class SlyDogMainAction(self : Faction, f : Faction, a : Faction, m : Message, c : Clearing, o : Faction, i : Option[Hero], then : ForcedAction) extends ForcedAction
case class SlyDogAction(self : Faction, e : Faction, t : Faction, then : ForcedAction) extends ForcedAction

case class MilitarySuppliesPayAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class MilitarySuppliesAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction("Discard for", MilitarySupplies)(d.img) with ViewCard
case class MilitarySuppliesSelfAction(self : Faction, then : ForcedAction) extends BaseAction()("Discard", MilitarySupplies, "itself")

case class FindAnyCardAction(self : Faction, m : Message, then : ForcedAction) extends BaseAction("Drawing", m)("Take any card from discard with", OldFriend) with Soft
case class TakeAnyCardAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(OldFriend)(d.img) with ViewCard


object DuskDeckExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(DuskDeck)

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (f.can(Merchants) && f.forTrade.exists(_.exhausted.not)) {
            + MerchantsMainAction(f, Repeat).!(deck.none && pile.none)
        }
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (f.can(HighNoon)) {
            + HighNoonMainAction(f, f.as[WarriorFaction].?(f => f.pool(f.warrior)), game.noon.num, Repeat).as(HighNoon)(Daylight)
        }
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // HELPER
        case BattleStartAction(self, f, a, m, c, o, i, UsedEffectAction(ff, NightTerrors, then)) =>
            UsedEffectAction(ff, NightTerrors, BattleStartAction(self, f, a, m, c, o, i, then))

        // SPOILS
        case CraftPerformAction(f, s @ CraftEffectCard(_, _, _, Spoil), m) =>
            f.hand --> s --> discard.quiet

            f.log("crafted", s, m)

            Ask(f)
                .some(factions.but(f))(e => e.stuck.$.of[CraftEffectCard]./(d => SpoilMainAction(f, e, d)))
                .needOk
                .bailw(Repeat) {
                    log("No target for", Spoil)
                }

        case SpoilMainAction(f, e, d @ CraftEffectCard(_, _, _, effect)) =>
            f.logtemp("tried to spoil", effect, "of", e)

            Ask(e)
                .each(e.hand)(c => SpoilCounterAction(e, f, d, c).!(c.as[CraftEffectCard].?(_.effect == Spoil).not))
                .skip(SpoilAction(f, e, d))
                .needOkIf(e.is[Trader].not && e.hand.any)

        case SpoilCounterAction(f, e, d, c) =>
            f.hand --> c --> discard.quiet

            f.log("countered spoiling of", d, "with", c)

            Repeat

        case SpoilAction(f, e, d @ CraftEffectCard(_, _, _, effect)) =>
            e.removeStuckEffect(effect)

            f.log("destroyed", effect, "of", e)

            e.oscore(1)("from", Spoil)

            Repeat

        // SERVICE OF THE
        case ServiceOfMainAction(f, s) =>
            Ask(f)
                .each(clearings.%(_.cost.matched(s)))(c =>
                    ServiceOfAction(f, s, c)
                        .!(f.as[WarriorFaction].?(_.at(c).of[Warrior].none), "no warriors")
                        .!(f.as[Hero].?(_.at(c).none), "not there")
                        .!(f.as[Hero].?(_.inv.count(Torch) == 0), "no torch")
                        .!(f.as[Hero].?(_.inv.notDamaged.count(Torch) == 0), "damaged torch")
                        .!(f.enemies.exists(_.present(c)).not, "no enemies")
                )
                .bailout(ServiceOfFailAction(f, s))
                .cancel
                .needOk

        case ServiceOfAction(f, s, c) =>
            f.removeStuckEffect(ServiceOfThe(s))

            f.log("was granted", ServiceOfThe(s), "in", c)

            NukeAction(f, f.enemies, $(c), NukeType.ClearSector, Repeat)

        case ServiceOfFailAction(f, s) =>
            f.removeStuckEffect(ServiceOfThe(s))

            f.log("failed to use", ServiceOfThe(s))

            Repeat

        // MERCHANTS
        case MerchantsMainAction(f, then) =>
            YYSelectObjectsAction(f, f.forTrade./(ToExhaust))
                .withGroup(Merchants.elem)
                .withRule(_.ref.exhausted.not)
                .withThen(i => MerchantsAction(f, i.ref, then))(i => "Draw " ~ 2.cards ~ ", discard " ~ 1.cards ~ " with " ~ i.ref.elem)("Draw " ~ 2.cards ~ ", discard " ~ 1.cards)
                .withExtras(CancelAction)

        case MerchantsAction(f, i, then) =>
            f.used :+= Merchants

            f.forTrade :-= i
            f.forTrade :+= i.exhaust

            f.log("exhausted", i.exhaust)

            DrawCardsAction(f, 2, WithEffect(Merchants), AddCardsAction(f, MerchantsContinueAction(f, then)))

        case MerchantsContinueAction(f, then) =>
            Ask(f)(NoHand).each(f.hand)(d => MerchantsDiscardAction(f, d, then)).needOk

        case MerchantsDiscardAction(f, d, then) =>
            f.hand --> d --> discard(f)

            then

        // ADVENTURERS
        case AdventurersAction(f, then) =>
            val lm = f.moveFrom
            val lb = clearings.%(c => f.canAttackList(c).any)

            log("Behold the Guild of Adventurers!")

            Ask(f)
                .add(MoveInitAction(f, f, $, WithEffect(Adventurers), lm, f.movable, $(CancelAction), then).as("Move", dt.Move)(Adventurers))
                .add(BattleInitAction(f, f, WithEffect(Adventurers), lb, $(CancelAction), then).as("Battle", dt.Battle)(Adventurers))
                .skip(then)

        case CharmOffensiveMainAction(f, l, then) =>
            Ask(f)(l./(e => CharmOffensiveAction(f, e, then).as(e, "gets", 1.vp)(CharmOffensive))).cancel

        case CharmOffensiveAction(f, e, then) =>
            e.oscore(1)("from", CharmOffensive)

            DrawCardsAction(f, 1, WithEffect(CharmOffensive), AddCardsAction(f, UsedEffectAction(f, CharmOffensive, Repeat)))

        // MILITARY SUPPLIES
        case MilitarySuppliesPayAction(f, then) =>
            Ask(f).each(f.hand)(d => MilitarySuppliesAction(f, d, then))(MilitarySuppliesSelfAction(f, then)).cancel.needOk

        case MilitarySuppliesAction(f, d, then) =>
            f.hand --> d --> discard.quiet

            f.log("discarded", d, "for", MilitarySupplies)

            then

        case MilitarySuppliesSelfAction(f, then) =>
            f.removeStuckEffect(MilitarySupplies)

            f.log("discarded", MilitarySupplies)

            then

        // BREAKING DAWN
        case BreakingDawnMainAction(f : WarriorFaction, feed, milk, then) =>
            Ask(f)
                .add(feed.?(BreakingDawnFeedAction(f, then).as("Add", f.warrior.of(f), f.warrior.imgd(f), "to", BreakingDawn)(BreakingDawn)))
                .add(feed.?(BreakingDawnFeedAction(f, BreakingDawnMilkAction(f, then)).as("Add", f.warrior.of(f), f.warrior.imgd(f), "to", BreakingDawn, "then", "score", (milk + 1).vp, "and discard")(BreakingDawn)))
                .add((milk > 0).?(BreakingDawnMilkAction(f, then).as("Score just", milk.vp, "and discard")(BreakingDawn)))
                .cancel
                .needOk

        case BreakingDawnMainAction(f : Hero, feed, milk, then) =>
            Ask(f)
                .add(BreakingDawnFeedAction(f, then).as("Put effort", Image("attitude-heart-full", styles.attitude), "into", BreakingDawn)(BreakingDawn))
                .add(BreakingDawnFeedAction(f, BreakingDawnMilkAction(f, then)).as("Put effort", Image("attitude-heart-full", styles.attitude), "into", BreakingDawn, "then", "score", (milk + 1).vp, "and discard")(BreakingDawn))
                .add((milk > 0).?(BreakingDawnMilkAction(f, then).as("Score just", milk.vp, "and discard")(BreakingDawn)))
                .cancel
                .needOk

        case BreakingDawnMainAction(f, feed, milk, then) =>
            Ask(f)
                .add(BreakingDawnMilkAction(f, then).as("Discard")(BreakingDawn))
                .cancel
                .needOk

        case BreakingDawnFeedAction(f, then) =>
            f.as[WarriorFaction].foreach { f =>
                val w = f.warrior

                f.reserve --> w --> game.dawn

                f.log("added", w.of(f), "to", BreakingDawn)
            }

            f.as[Hero].foreach { f =>
                game.efforts --> Heart --> game.dawn

                f.log("put effort info", BreakingDawn)
            }

            f.used :+= BreakingDawn

            then

        case BreakingDawnMilkAction(f, then) =>
            val n = game.dawn.num

            f.removeStuckEffect(BreakingDawn)

            f.oscore(n)("and discarded", BreakingDawn)

            then

        // HIGH NOON
        case HighNoonMainAction(f : WarriorFaction, feed, milk, then) =>
            Ask(f)
                .add(feed.?(HighNoonFeedAction(f, then).as("Add", f.warrior.of(f), "to", f.warrior.imgd(f))(HighNoon)))
                .add(feed.?(HighNoonFeedAction(f, HighNoonMilkAction(f, then)).as("Add", f.warrior.of(f), f.warrior.imgd(f), "to", HighNoon, "then", "place", (milk + 1).warriors, "and discard")(HighNoon)))
                .add((milk > 0).?(HighNoonMilkAction(f, then).as("Just place", milk.warriors, "and discard")(HighNoon)))
                .cancel
                .needOk

        case HighNoonMainAction(f : Hero, feed, milk, then) =>
            Ask(f)
                .add(HighNoonFeedAction(f, then).as("Put effort", Image("attitude-heart-full", styles.attitude), "into", HighNoon)(HighNoon))
                .add(HighNoonFeedAction(f, HighNoonMilkAction(f, then)).as("Put effort", Image("attitude-heart-full", styles.attitude), "into", HighNoon, "then", "battle", (milk + 1).stimes, "and discard")(HighNoon))
                .add((milk > 0).?(HighNoonMilkAction(f, then).as("Just battle", milk.stimes, "and discard")(HighNoon)))
                .cancel
                .needOk

        case HighNoonFeedAction(f, then) =>
            f.as[WarriorFaction].foreach { f =>
                val w = f.warrior

                f.reserve --> w --> game.noon

                f.log("added", w.of(f), "to", HighNoon)
            }

            f.as[Hero].foreach { f =>
                game.efforts --> Heart --> game.noon

                f.log("put effort info", HighNoon)
            }

            f.used :+= HighNoon

            then

        case HighNoonMilkAction(f : Hero, then) =>
            NoAsk(f)(HighNoonAction(f, then))

        case HighNoonAction(f, then) =>
            val n = game.noon.num

            f.removeStuckEffect(HighNoon)

            f.log("discarded", HighNoon)

            WageWarAction(f, 1, n, then)

        case HighNoonMilkAction(f : WarriorFaction, then) =>
            Ask(f).each(f.presence)(c => HighNoonPlaceAction(f, c, then).as(c)("Place", game.noon.$, "with", HighNoon, "to")).cancel

        case HighNoonPlaceAction(f, c, then) =>
            val l = game.noon.$

            game.noon --> c

            f.removeStuckEffect(HighNoon)

            f.log("placed", l, "in", c, "and discarded", HighNoon)

            then

        // DUSK AWAKENING
        case DuskAwakeningMainAction(f : WarriorFaction, feed, milk, then) =>
            Ask(f)
                .add(feed.?(DuskAwakeningFeedAction(f, then).as("Add", f.warrior.of(f), "to", f.warrior.imgd(f))(DuskAwakening)))
                .add(feed.?(DuskAwakeningFeedAction(f, DuskAwakeningMilkAction(f, then)).as("Add", f.warrior.of(f), f.warrior.imgd(f), "to", DuskAwakening, "then", "draw", (milk + 1).cards, "and discard")(DuskAwakening)))
                .add((milk > 0).?(DuskAwakeningMilkAction(f, then).as("Draw just", milk.cards, "and discard")(DuskAwakening)))
                .cancel
                .needOk

        case DuskAwakeningMainAction(f : Hero, feed, milk, then) =>
            Ask(f)
                .add(DuskAwakeningFeedAction(f, then).as("Put effort", Image("attitude-heart-full", styles.attitude), "into", DuskAwakening)(DuskAwakening))
                .add(DuskAwakeningFeedAction(f, DuskAwakeningMilkAction(f, then)).as("Put effort", Image("attitude-heart-full", styles.attitude), "into", DuskAwakening, "then", "draw", (milk + 1).cards, "and discard")(DuskAwakening))
                .add((milk > 0).?(DuskAwakeningMilkAction(f, then).as("Draw just", milk.cards, "and discard")(DuskAwakening)))
                .cancel
                .needOk

        case DuskAwakeningMainAction(f, feed, milk, then) =>
            Ask(f)
                .add(DuskAwakeningMilkAction(f, then).as("Discard")(DuskAwakening))
                .cancel
                .needOk

        case DuskAwakeningFeedAction(f, then) =>
            f.as[WarriorFaction].foreach { f =>
                val w = f.warrior

                f.reserve --> w --> game.dusk

                f.log("added", w.of(f), "to", DuskAwakening)
            }

            f.as[Hero].foreach { f =>
                game.efforts --> Heart --> game.dusk

                f.log("put effort info", DuskAwakening)
            }

            f.used :+= DuskAwakening

            then

        case DuskAwakeningMilkAction(f, then) =>
            val n = game.dusk.num

            f.removeStuckEffect(DuskAwakening)

            DrawCardsAction(f, n, AndDiscarded(DuskAwakening), AddCardsAction(f, then))

        // OLD FRIEND
        case DrawCardsAction(f, n, m, then) if f.has(OldFriend) =>
            Ask(f)(FindAnyCardAction(f, m, then))(DrawCardsFromDeckAction(f, n, m, then).as("Draw " ~ n.times(dt.CardBack).merge)).needOk

        case FindAnyCardAction(f, m, then) =>
            Ask(f)(pile./(d => TakeAnyCardAction(f, d, then))).cancel

        case TakeAnyCardAction(f, d, then) =>
            pile --> d --> f.drawn

            f.log("took", d, "from the dicard pile")

            f.removeStuckEffect(OldFriend)

            then

        case _ => UnknownContinue
    }

}
