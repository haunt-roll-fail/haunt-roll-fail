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


// Birdsong (start)
case object Saboteurs extends CardEffect {
    override val name = "Saboteurs"
}

// Birdsong
case object FalseOrders extends CardEffect {
    override val name = "False Orders"
}

case object SwapMeet extends CardEffect {
    override val name = "Swap Meet"
}

// Birdsong (end)
case object EyrieEmigre extends CardEffect {
    override val name = "Eyrie \u00c9migr\u00e9"
}

// Daylight
case object PropagandaBureau extends CardEffect {
    override val name = "Propaganda Bureau"
}

case object LeagueOfAdventurousMice extends CardEffect {
    override val name = "League of Adventurous Mice"
}

// Evening (start)
case object CharmOffensive extends CardEffect {
    override val name = "Charm Offensive"
}

// Evening
case object Informants extends CardEffect {
    override val name = "Informants"
}

// Passive
case object SoupKitchens extends CardEffect {
    override val name = "Soup Kitchens"
}

case object BorschtKitchens extends CardEffect {
    override val name = "Borscht Kitchens"
}

case object BoatBuilders extends CardEffect {
    override val name = "Boat Builders"
}

case object CorvidPlanners extends CardEffect {
    override val name = "Corvid Planners"
}

case object Tunnels extends CardEffect {
    override val name = "Tunnels"
}

case object MurineBroker extends CardEffect {
    override val name = "Murine Brokers"
}

case object MasterEngravers extends CardEffect {
    override val name = "Master Engravers"
}

case object CoffinMakers extends CardEffect with SpecialRegion {
    override val name = "Coffin Makers"
    override def elem(implicit game : Game) = name.hl
}

case class OnFalseOrders(f : Faction) extends Message {
    def elem(implicit game : Game) = " on " ~ FalseOrders.elem ~ " from " ~ f.elem
}


case class SaboteurMainAction(self : Faction) extends BaseAction(Birdsong, "start")(Saboteurs) with Soft
case class SaboteurAction(self : Faction, e : Faction, d : DeckCard) extends BaseAction(Saboteurs, "discard")(d, "of", e)

case class SwapMeetAction(self : Faction, l : $[Faction], then : ForcedAction) extends OptionAction(SwapMeet) with BirdsongQuestion with Soft
case class SwapMeetFactionAction(self : Faction, t : Faction, then : ForcedAction) extends BaseAction(SwapMeet)(t)
case class SwapMeetReturnAction(self : Faction, t : Faction, then : ForcedAction) extends ForcedAction
case class SwapMeetReturnCardAction(self : Faction, t : Faction, d : DeckCard, then : ForcedAction) extends BaseAction("Give a card back to", t, "after", SwapMeet)(d.img) with ViewCard

case class FalseOrdersAction(self : Faction, l : $[Faction], then : ForcedAction) extends OptionAction(FalseOrders) with BirdsongQuestion with Soft
case class FalseOrdersFactionAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction(FalseOrders)(e) with Soft
case class FalseOrdersCompleteAction(self : Faction, then : ForcedAction) extends ForcedAction

case class EyrieEmigreDiscardAction(self : Faction) extends BaseAction()("Discard", EyrieEmigre)
case class EyrieEmigreAttackAction(self : Faction, l : $[Clearing]) extends ForcedAction

case class PropagandaBureauMainAction(self : Faction, l : $[Clearing], then : ForcedAction) extends BaseAction(Daylight)(PropagandaBureau) with Soft
case class PropagandaBureauSelectAction(self : Faction, c : Clearing, e : WarriorFaction, p : Piece, then : ForcedAction) extends BaseAction(PropagandaBureau)(p.of(e), p.img(e), "in", c) with Soft
case class PropagandaBureauAction(self : Faction, c : Clearing, e : WarriorFaction, p : Piece, then : ForcedAction) extends ForcedAction
case class PropagandaBureauPlaceAction(self : Faction, c : Clearing, p : Piece, then : ForcedAction) extends ForcedAction

case class LeagueOfAdventurousMiceMainAction(self : Faction, lm : $[Region], lw : $[Clearing], then : ForcedAction) extends BaseAction(Daylight)(LeagueOfAdventurousMice) with Soft
case class ExhaustForTradeItemAction(f : Faction, i : ItemRef, then : ForcedAction) extends ForcedAction

case class CharmOffensiveMainAction(f : Faction, l : $[Faction], then : ForcedAction) extends ForcedAction with Soft
case class CharmOffensiveAction(f : Faction, e : Faction, then : ForcedAction) extends ForcedAction

case class FindAmbushAction(self : Faction, m : Message, then : ForcedAction) extends BaseAction(Evening)("Take", "Ambush".hl, "with", Informants) with Soft
case class TakeAmbushAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(Informants)(d.img) with ViewCard


object ExilesDeckExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(ExilesDeck) || options.has(MixedDeck)

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        f.can(SwapMeet).??(factions.but(f).%(_.hand.any)).some.foreach { l =>
            + SwapMeetAction(f, l, Repeat)
        }

        f.can(FalseOrders).??(factions.but(f).of[WarriorFaction].%(e => clearings.%(e.canMoveFrom).%(e.at(_).of[Warrior].notOf[Tenacious].any).any)).some.foreach { l =>
            + FalseOrdersAction(f, l, Repeat)
        }
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (f.can(PropagandaBureau)) {
            val cc = clearings.%(c => f.canPlace(c) || options.has(UnthematicPropagandaBureau)).%(c => f.enemies.%(f.canRemove(c)).%(_.at(c).of[Warrior].notOf[Tenacious].any).any).%(c => f.hand.%(_.matches(c.cost)).any)
            + PropagandaBureauMainAction(f, cc, Repeat)
                .!(f.hand.none, "no cards")
                .!(cc.none, "no targets")
                .!(f.as[WarriorFaction].?(f => f.pool(f.warrior)).not && options.has(UnthematicPropagandaBureau).not, "no warriors")
        }

        if (f.can(LeagueOfAdventurousMice) && f.forTrade.exists(_.exhausted.not)) {
            val lm = f.moveFrom
            val lb = clearings.%(c => f.canAttackList(c).any)

            + LeagueOfAdventurousMiceMainAction(f, lm, lb, Repeat).x(lm.none && lb.none)
        }
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SABOTEUR
        case SaboteurAction(f, e, CraftEffectCard(_, _, _, effect)) =>
            f.removeStuckEffect(Saboteurs)

            e.removeStuckEffect(effect)

            f.log("used", Saboteurs, "to destroy", effect, "of", e)

            Repeat

        // COFFIN MAKERS
        case BirdsongNAction(5, f) =>
            if (f.has(CoffinMakers)) {
                f.oscore(game.coffins.num /↓ 5)("from", CoffinMakers)

                game.coffins --> game.pool
            }

            Next

        // SWAP MEET
        case SwapMeetAction(f, l, then) =>
            Ask(f)(l./(SwapMeetFactionAction(f, _, then))).cancel

        case SwapMeetFactionAction(f, o, then) =>
            f.used :+= SwapMeet

            f.log("took a card from", o, "with", SwapMeet)

            StealCardAction(f, o, SwapMeetReturnAction(f, o, then))

        case SwapMeetReturnAction(f, o, then) =>
            Ask(f)(NoHand).each(f.hand)(d => SwapMeetReturnCardAction(f, o, d, then)).needOk

        case SwapMeetReturnCardAction(f, o, d, then) =>
            f.log("gave", o, "a card in return")

            f.hand --> d --> o.hand

            o.notify($(ViewCardInfoAction(f, GaveBack(f, o).elem(game), d)))

            then

        // FALSE ORDERS
        case FalseOrdersAction(f, l, then) =>
            Ask(f)(l./(FalseOrdersFactionAction(f, _, then))).cancel

        case FalseOrdersFactionAction(f, e, then) =>
            MoveInitAction(f, e, game.transports./($) ** e.transports./(_.but(RuledMove)) ** $($(HalfWarriors)), OnFalseOrders(f), e.movable, e.movable, $(CancelAction), FalseOrdersCompleteAction(f, then))

        case FalseOrdersCompleteAction(f, then) =>
            f.removeStuckEffect(FalseOrders)

            then

        // PROPAGANDA BUREAU
        case PropagandaBureauMainAction(f, l, then) =>
            Ask(f)(l./~(c => f.enemies.%(f.canRemove(c)).of[WarriorFaction]./~(e => e.at(c).of[Warrior].notOf[Tenacious].distinct./(PropagandaBureauSelectAction(f, c, e, _, then))))).cancel

        case PropagandaBureauSelectAction(f, c, e, p, then) =>
            OptionalDiscardCardAction(f, ToReplace(c, e, p), c.cost, PropagandaBureauAction(f, c, e, p, then))

        case PropagandaBureauAction(f, c, e, p, then) =>
            game.highlights :+= BattleHighlight(c)

            f.used :+= PropagandaBureau

            f.log("brainwashed", p.of(e), "in", c, WithEffect(PropagandaBureau), "and", f.drawn.get)

            f.drawn --> discard.quiet

            val q = f.as[WarriorFaction].%(f => f.pool(f.warrior)).%(_.canPlace(c)).foldLeft(ForcedRemoveFinishedAction(e, then) : ForcedAction)((q, f) => PropagandaBureauPlaceAction(f, c, f.warrior, q))

            TryForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), NoMessage, q, then)

        case PropagandaBureauPlaceAction(f, c, p, then) =>
            f.reserve --> p --> c

            then

        // LEAGUE OF ADVENTUROUS MICE
        case LeagueOfAdventurousMiceMainAction(f, lm, lb, then) =>
            YYSelectObjectsAction(f, f.forTrade./(ToExhaust))
                .withGroup(LeagueOfAdventurousMice.elem)
                .withRule(_.ref.exhausted.not)
                .withThens(i =>
                    lm.any.$(MoveInitAction(f, f, $, WithEffect(LeagueOfAdventurousMice), lm, f.movable, $(CancelAction), ExhaustForTradeItemAction(f, i.ref, then)).as("Move", dt.Move)) ++
                    lb.any.$(BattleInitAction(f, f, WithEffect(LeagueOfAdventurousMice), lb, $(CancelAction), ExhaustForTradeItemAction(f, i.ref, then)).as("Battle", dt.Battle))
                )
                .withExtra($(CancelAction))

        case MoveListAction(self, f, t, m, from, to, l, ExhaustForTradeItemAction(ff, ii, then)) =>
            ExhaustForTradeItemAction(ff, ii, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case BattleStartAction(self, f, a, m, c, o, i, ExhaustForTradeItemAction(ff, ii, then)) =>
            ExhaustForTradeItemAction(ff, ii, BattleStartAction(self, f, a, m, c, o, i, then))

        case ExhaustForTradeItemAction(f, i, then) =>
            f.used :+= LeagueOfAdventurousMice

            f.forTrade :-= i
            f.forTrade :+= i.exhaust

            f.log("exhausted", i.exhaust, "with", LeagueOfAdventurousMice)

            then

        // CHARM OFFENSIVE
        case CharmOffensiveMainAction(f, l, then) =>
            Ask(f)(l./(e => CharmOffensiveAction(f, e, then).as(e, "gets", 1.vp)(CharmOffensive))).cancel

        case CharmOffensiveAction(f, e, then) =>
            e.oscore(1)("from", CharmOffensive)

            DrawCardsAction(f, 1, WithEffect(CharmOffensive), AddCardsAction(f, UsedEffectAction(f, CharmOffensive, Repeat)))

        // INFORMANTS
        case DrawCardsAction(f, n, m, then) if game.phase == Evening && f.has(Informants) =>
            Ask(f)(FindAmbushAction(f, m, then))(DrawCardsFromDeckAction(f, n, m, then).as("Draw " ~ n.times(dt.CardBack).merge)).needOk

        case FindAmbushAction(f, m, then) =>
            Ask(f)(pile./(d => TakeAmbushAction(f, d, then).x(d @@ { case Ambush(_) => false ; case _ => true }))).cancel

        case TakeAmbushAction(f, d, then) =>
            pile --> d --> f.drawn

            f.log("took", d, "from the dicard pile")

            then

        // MASTER ENGRAVERS
        case CraftScoreAction(f, d, n, m, then) if f.has(MasterEngravers) =>
            f.nscore(n + 1)("crafting", d.item)(f, "crafted", d.item, d, "(" ~ d.cost.ss ~ ")", "with " ~ MasterEngravers.elem, ForVP, m)

            AcquireItemsAction(f, then)

        // EYRIE EMIGRE
        case BirdsongNAction(99, f) if f.has(EyrieEmigre) =>
            MoveInitAction(f, f, $, WithEffect(EyrieEmigre), f.moveFrom, f.movable, $(EyrieEmigreDiscardAction(f)), EyrieEmigreAttackAction(f, $))

        case MoveFinishedAction(f, from, to : Clearing, EyrieEmigreAttackAction(ff, Nil)) =>
            MoveFinishedAction(f, from, to, EyrieEmigreAttackAction(ff, $(to)))

        case EyrieEmigreAttackAction(f, l) =>
            BattleInitAction(f, f, WithEffect(EyrieEmigre), l, $(EyrieEmigreDiscardAction(f)), Next)

        case EyrieEmigreDiscardAction(f) =>
            f.removeStuckEffect(EyrieEmigre)

            f.log("failed", EyrieEmigre)

            Next

        case _ => UnknownContinue
    }

}
