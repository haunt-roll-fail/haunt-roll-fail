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
case object BetterBurrowBank extends CardEffect {
    override val name = "Better Burrow Bank"
}

// Birdsong
case object RoyalClaim extends CardEffect {
    override val name = "Royal Claim"
}

case object StandAndDeliver extends CardEffect {
    override val name = "Stand and Deliver!"
}

// Daylight (start)
case object CommandWarren extends CardEffect {
    override val name = "Command Warren"
}

// Daylight
case object TaxCollector extends CardEffect {
    override val name = "Tax Collector"
}

case object Codebreakers extends CardEffect {
    override val name = "Codebreakers"
}

// Evening (start)
case object Cobbler extends CardEffect {
    override val name = "Cobbler"
}


case class RoyalClaimAction(self : Faction, n : Int) extends OptionAction(RoyalClaim, "(" ~ n.vp ~ ")") with BirdsongQuestion

case class StandAndDeliverAction(self : Faction, l : $[Faction], then : ForcedAction) extends OptionAction(StandAndDeliver) with BirdsongQuestion with Soft
case class StandAndDeliverFactionAction(self : Faction, t : Faction, then : ForcedAction) extends BaseAction(StandAndDeliver)(t)
case class StandAndDeliverCardAction(self : Faction, t : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction

case class BetterBurrowBankAction(self : Faction, o : Faction) extends BaseAction(self, "shares", BetterBurrowBank, "with")(o)

case class CodebreakersMainAction(self : Faction, l : $[Faction], then : ForcedAction) extends BaseAction(Daylight)(Codebreakers) with Soft
case class CodebreakersAction(self : Faction, f : Faction, then : ForcedAction) extends BaseAction(Codebreakers)(f)
case class CodebreakersNotifyAction(self : Faction, f : Faction, d : DeckCard) extends BaseInfo(self, "views", f, "hand")(d.img) with ViewCard with OnClickInfo { def param = d }

case class TaxCollectorMainAction(self : WarriorFaction, l : $[Clearing], then : ForcedAction) extends BaseAction(Daylight)(TaxCollector) with Soft
case class TaxCollectorAction(self : WarriorFaction, c : Clearing, p : Piece, then : ForcedAction) extends BaseAction(TaxCollector)(p.of(self), "in", c)


object StandardDeckExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(StandardDeck) || options.has(MixedDeck)

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        f.can(StandAndDeliver).??(factions.but(f).%(_.hand.any)).some.foreach { l =>
            + StandAndDeliverAction(f, l, Repeat)
        }
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        f.as[WarriorFaction].foreach { f =>
            if (f.can(TaxCollector)) {
                val cc = clearings.%(c => f.at(c).of[Warrior].any)
                + TaxCollectorMainAction(f, cc, Repeat).!(cc.none, "no warriors").!(game.deck.none && game.pile.none, "no cards")
            }
        }

        if (f.can(Codebreakers)) {
            val ff = factions.but(f).%(_.hand.any)
            if (ff.any)
                + CodebreakersMainAction(f, ff, Repeat)
        }
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // ROYAL CLAIM
        case BirdsongNAction(0, f) =>
            val n = clearings.%(f.ruleSelf).num

            if (f.can(RoyalClaim) && n > 0)
                Ask(f)(RoyalClaimAction(f, n)).skip(Next)
            else
                Next

        case RoyalClaimAction(f, n) =>
            f.removeStuckEffect(RoyalClaim)

            f.oscore(n)("asserting", RoyalClaim)

            Next

        // BETTER BURROW BANK
        case BetterBurrowBankAction(f, o) =>
            f.used :+= BetterBurrowBank

            f.log("used", BetterBurrowBank, "with", o.elem ~ ",", ("each drew " ~ 1.hl ~ " card").spn(xlo.nowrap))

            DrawCardsAction(f, 1, SuppressLog, AddCardsAction(f, DrawCardsAction(o, 1, SuppressLog, AddCardsAction(o, Repeat))))

        // STAND AND DELIVER
        case StandAndDeliverAction(f, o, then) =>
            Ask(f)(o./(StandAndDeliverFactionAction(f, _, then))).cancel

        case StandAndDeliverFactionAction(f, o, then) =>
            f.used :+= StandAndDeliver

            f.log("stole a card from", o, "with", StandAndDeliver)

            o.oscore(1)("from", StandAndDeliver)

            StealCardAction(f, o, then)

        // TAX COLLECTOR
        case TaxCollectorMainAction(f, l, then) =>
            Ask(f)(l./~(c => f.at(c).of[Warrior].notOf[Tenacious].distinct./(p => TaxCollectorAction(f, c, p, then)))).cancel

        case TaxCollectorAction(f, c, p, then) =>
            f.used :+= TaxCollector

            f.from(c) --> p --> game.recycle

            DrawCardsAction(f, 1, ConcatMessage(WithEffect(TaxCollector), InClearing(c)), AddCardsAction(f, then))

        // CODEBREAKERS
        case CodebreakersMainAction(f, l, then) =>
            Ask(f)(l./(CodebreakersAction(f, _, then))).cancel

        case CodebreakersAction(f, o, then) =>
            f.used :+= Codebreakers
            f.log("employed", Codebreakers, "to look at", o, "cards")
            Ask(f).each(o.hand)(CodebreakersNotifyAction(f, o, _)).done(then).needOk

        case _ => UnknownContinue
    }

}
