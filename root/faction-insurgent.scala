package root

import root.gaming._

import colmat._

import hrf.elem._
import root.elem._

trait Insurgent extends WarriorFaction {
    val expansion = InsurgentExpansion

    val warrior = Critter
    
    val abilities = $(GuerillaTactics, MoveOutrage, RemoveOutrage, MartialLaw, SupportersLimit)
    
    val pieces = Critter ** 10 ++ Sympathy ** 10 ++ Base(Fox) ** 1 ++ Base(Rabbit) ** 1 ++ Base(Mouse) ** 1

    def cost = $(999, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1)
    def reward = $(999, 4, 4, 4, 3, 2, 2, 1, 1, 1, 0)

    def advertising = Sympathy.img(this).repeat(5).merge
    def motto = "Revolt".styled(this)
}

case object Sympathy extends Token
case class Base(suit : BaseSuit) extends Building {
    override def id = "Base"
    override def imgid(f : Faction) = f.short + "-" + id + "-" + suit.name
}
case object Critter extends Warrior

case object MartialLaw extends FactionEffect
case object CounterInsurgency extends FactionEffect
case object UnlimitedRevolt extends FactionEffect
case object Terror extends FactionEffect
case object DelayedScoring extends FactionEffect

case object GuerillaTactics extends BattleEffect {
    override def name = "Guerilla Tactics"
}

case object MoveOutrage extends FactionEffect
case object BattleOutrage extends FactionEffect
case object RemoveOutrage extends FactionEffect

case object NoBirdsOutrage extends FactionEffect


case object SupportersLimit extends FactionEffect

case object Supporters extends FactionEffect {
    def of(f : Faction, n : Int) = (n != 1).?(n.hlb ~ " " ~ "Supporters".styled(f)).|("a " ~ "Supporter".styled(f))
    def pl(f : Faction, n : Int) = (n != 1).?("Supporters".styled(f)).|("Supporter".styled(f))
}

case object Officers extends FactionEffect {
    def of(f : Faction, n : Int) = (n != 1).?(n.hlb ~ " " ~ "Officers".styled(f)).|("an " ~ "Officer".styled(f))
    def pl(f : Faction, n : Int) = (n != 1).?("Officers".styled(f)).|("Officer".styled(f))
    def imgd(f : Faction) = Image(f.short + "-" + "officer", styles.fund)
}

case object WA extends Insurgent {
    val name = "Woodland Alliance"
    override def funName = NameReference(name, this) ~ " Alliance"
    val short = "WA"
    val style = "wa"
    val priority = "C"
}

case object AF extends Insurgent {
    val name = "Animal Front"
    override def funName = NameReference(name, this) ~ " Front"
    val short = "AF"
    val style = "af"
    val priority = "C'"
    
    override val abilities = $(GuerillaTactics, MoveOutrage, BattleOutrage, NoBirdsOutrage, CounterInsurgency, Terror, DelayedScoring)
}

class InsurgentPlayer(val game : Game, val faction : Insurgent) extends PlayerState {
    var supporters = cards("supporters")
    val officers = figures("officers", Nil, _.piece == Critter)

    var acted = 0
    
    def craft = all(Sympathy)./(game.mapping)

    def bases = all(Base(Fox)) ++ all(Base(Rabbit)) ++ all(Base(Mouse))

    import game._

    def ofBaseSuit(s : Suit) = bases./(_.suit).%(s.m(_)).any
}

case class ToSupporters(f : Insurgent) extends Message {
    def elem(g : Game) = " to " ~ Supporters.of(f)
}

case class FactionDiscards(f : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " " ~ "discards"
}

case class SupportersLimit(f : Faction, n : Int) extends Message {
    def elem(g : Game) = " down to " ~ n.hl ~ " " ~ Supporters.of(f)
}

case class RevoltIn(f : Insurgent, c : Clearing) extends Message {
    def elem(g : Game) = f.elem ~ " starts " ~ "Revolt".styled(f) ~ " in " ~ c.elem(g) ~ " with"
}

case class SympathyIn(f : Insurgent, c : Clearing) extends Message {
    def elem(g : Game) = f.elem ~ " spreads " ~ Sympathy.of(f) ~ " to " ~ c.elem(g) ~ " with"
}

case class Mobilize(f : Insurgent) extends Message {
    def elem(g : Game) = f.elem ~ " mobilizes to " ~ Supporters.of(f)
}

case class TrainAnOfficer(f : Insurgent) extends Message {
    def elem(g : Game) = f.elem ~ " trains an " ~ Officers.of(f, 1) ~ " with"
}

case class TrainOfficers(f : Insurgent) extends Message {
    def elem(g : Game) = f.elem ~ " trains " ~ Officers.of(f) ~ " with"
}


trait WADaylightQuestion extends FactionAction {
    override def self : Insurgent
    def question(g : Game) = self.elem ~ " (" ~ "Daylight".styled(styles.phase) ~ ")"
}                                                      

trait InsurgentEveningQuestion extends FactionAction {
    override def self : Insurgent
    def question(g : Game) = self.elem ~ " (" ~ "Evening".styled(styles.phase) ~ ")" ~
        Div(
            1.to(g.wa(self).officers.num)./(_ => Image("action-black", styles.action)).take(g.wa(self).acted) ~
            1.to(g.wa(self).officers.num)./(_ => Image(self.style + "-action", styles.action)).drop(g.wa(self).acted)
        , styles.margined)
}


case class SupportersLimitAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class PerformDiscardSupportersAction(f : Faction, l : List[DeckCard], then : ForcedAction) extends ForcedAction

case class SpreadSympathyMainAction(f : Insurgent) extends ForcedAction

case class SpreadSympathyClearingAction(self : Insurgent, c : Clearing, d : DiscardCost, n : Int, vp : Int, s : List[Suit], limited : Boolean) extends 
    BaseAction("Spread", Sympathy.of(self), (vp > 0).??(vp.vp), "to" ~ Break ~
        limited.?(
            Span(s./(s => dt.CardSuit(s)).merge ~ dt.CardEmpty.*(5 - s.num).merge, styles.margined, styles.supporters5)
        ).|(
            Span(dt.CardEmpty ~ s./(s => dt.CardSuit(s)).merge ~ dt.CardEmpty, styles.margined, styles.supportersX)
        ))(c, (s.%(_.m(d)).num >= n).?((s.%(_ == d) ++ s.%(_ != d).%(_.m(d)))).|(d.repeat(n)).take(n)./(s => dt.CardSuit(s)).merge) with Soft

case class SpreadSympathyAction(f : Insurgent, c : Clearing, l : List[DeckCard]) extends ForcedAction

case class RevoltMainAction(f : Insurgent) extends ForcedAction

case class RevoltClearingAction(self : Insurgent, c : Clearing, d : DiscardCost, n : Int, s : List[Suit], limited : Boolean) extends 
    BaseAction("Start", "Revolt".styled(self), "in" ~ Break ~
        limited.?(
            Span(s./(s => dt.CardSuit(s)).merge ~ dt.CardEmpty.*(5 - s.num).merge, styles.margined, styles.supporters5)
        ).|(
            Span(dt.CardEmpty ~ s./(s => dt.CardSuit(s)).merge ~ dt.CardEmpty, styles.margined, styles.supportersX)
        ))(c, (s.%(_.m(d)).num >= n).?((s.%(_ == d) ++ s.%(_ != d).%(_.m(d)))).|(d.repeat(n)).take(n)./(s => dt.CardSuit(s)).merge) with Soft

case class RevoltAction(f : Insurgent, c : Clearing, l : List[DeckCard]) extends ForcedAction
case class RevoltBaseAction(f : Insurgent, c : Clearing) extends ForcedAction

case class RatMainAction(f : Insurgent) extends ForcedAction

case class MobilizeMainAction(self : Insurgent, l : List[DeckCard]) extends OptionAction("Mobilize".styled(self), Span(l./(d => dt.CardSuit(d.suit)).merge, styles.supportersX)) with WADaylightQuestion with Soft
case class MobilizeAction(self : Insurgent, l : List[DeckCard]) extends ForcedAction

case class TrainMainAction(self : Insurgent, s : List[Suit]) extends OptionAction("Train".styled(self), dt.CardSuit(s.single.|(AnySuit)), dt.Arrow, Officers.imgd(self)) with WADaylightQuestion with Soft
case class TrainAction(f : Insurgent, l : List[DeckCard]) extends ForcedAction

case class TerrorizeMainAction(self : Insurgent) extends OptionAction("Terrorize".styled(self)) with WADaylightQuestion with Soft
case class TerrorizeAction(self : Insurgent, d : DeckCard) extends BaseAction("Terrorize".styled(self), "a", "Supporter".styled(self))(d.img) with ViewCard with Soft
case class TerrorizeTakeAction(f : Insurgent, d : DeckCard) extends ForcedAction

case class RemoveSympathyAction(f : Insurgent, o : Faction, c : Clearing, then : ForcedAction) extends ForcedAction

case class OutrageAction(f : Insurgent, o : Faction, c : Clearing, then : ForcedAction) extends ForcedAction
case class OutrageCardAction(o : Faction, self : Faction, d : DeckCard, available : Boolean, then : ForcedAction) extends BaseAction(self, "surrenders a card to", Supporters.of(o))(d.img) with ViewCard
case class OutrageDiscardAction(f : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class DestroyBaseAction(f : Insurgent, c : Clearing, then : ForcedAction) extends ForcedAction

case class InsurgentEveningAction(f : Insurgent) extends ForcedAction
case class RatDoneAction(f : Insurgent) extends ForcedAction

case class RatAttackAction(self : Insurgent, l : List[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with InsurgentEveningQuestion with Soft
case class RatMoveAction(self : Insurgent, l : List[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with InsurgentEveningQuestion with Soft
case class RatRecruitAction(self : Insurgent, l : List[Clearing]) extends OptionAction("Recruit".styled(self), self.warrior.imgd(self)) with InsurgentEveningQuestion with Soft
case class RatRecruitClearingAction(self : Insurgent, c : Clearing) extends BaseAction("Recruit".styled(self), self.warrior.img(self), "in")(c)
case class RatOrganizeAction(self : Insurgent, l : List[Clearing], vp : Int) extends OptionAction("Organize".styled(self), self.warrior.imgd(self), dt.Arrow, Sympathy.imgd(self), (vp > 0).??(vp.vp)) with InsurgentEveningQuestion with Soft
case class RatOrganizeClearingAction(self : Insurgent, c : Clearing) extends BaseAction("Organize", Sympathy.of(self), "in")(c)


case class AddSupportersAction(f : Insurgent, then : ForcedAction) extends ForcedAction


object InsurgentExpansion extends Expansion {
    override def afterMoveTo(game : Game, f : Faction, to : Region, then : ForcedAction) = {
        import game._

        var q = then

        to match { 
            case c : Clearing =>
                game.factions.but(f)./~(_.insurgent).%(_.at(to).got(Sympathy)).%(_.has(MoveOutrage)).foreach { outraged =>
                    q = OutrageAction(outraged, f, c, q)
                }
            case _ =>
        }

        q
    }
        
    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            // SETUP
            case CreatePlayerAction(f : Insurgent) =>
                pstates += f -> new InsurgentPlayer(game, f)
                FactionInitAction(f)
                
            case FactionSetupAction(f : Insurgent) =>
                DrawCardsAction(f, 3, ToSupporters(f), AddSupportersAction(f, SetupNextAction))
 
            // HELPER
            case BattlePostHitInAction(b, e, f : Insurgent, Sympathy, then) =>
                e.log("quashed", Sympathy.of(f))
                then

            case BattlePostHitInAction(b, e, f : Insurgent, Base(s), then) =>
                e.log("overwhelmed", Base(s).of(f))
                then

            case BattlePostHitInAction(b, e, f : Insurgent, Critter, then) =>
                e.log("chased away", Critter.of(f))
                then

            case ForcedRemoveEffectAction(e, c, f : Insurgent, Sympathy, then) =>
                RemoveSympathyAction(f, e, c, then)
            
            case ForcedRemoveEffectAction(e, c, f : Insurgent, Base(_), then) =>
                DestroyBaseAction(f, c, then)

            case AddSupportersAction(f : Insurgent, then) =>
                f.drawn --> f.supporters

                if (f.has(SupportersLimit) && f.bases.none && f.supporters.num > 5)
                    f.supporters --> f.supporters.drop(5) --> discard(f, "too much", Supporters.of(f))
                
                then

            case SupportersLimitAction(f : Insurgent, n, then) =>
                if (f.supporters.num <= n)
                    then
                else
                    XXSelectObjectsAction(f, f.supporters)
                        .withGroup(f.elem ~ " discards " ~ (f.supporters.num - n).cards ~ " down to " ~ Supporters.of(f, n))
                        .withRule(_.num(f.supporters.num - n))
                        .withThen(PerformDiscardSupportersAction(f, _, then))(l => "Discard".hl ~ l./(" " ~ _.elem))
                        .withExtra($(NoSupporters))
                    
            case PerformDiscardSupportersAction(f : Insurgent, l, then) =>
                f.supporters --> l --> discard(f, "from", Supporters.of(f))
                then
    
            case RemoveSympathyAction(f : Insurgent, o, c, then) =>
                if (f.has(DelayedScoring)) {
                    val n = f.reward(f.pooled(Sympathy))
                    f.oscore(n)("from", Sympathy.of(f))
                }

                if (f.has(RemoveOutrage))
                    OutrageAction(f, o, c, then)
                else
                    then
                   
                    
            case OutrageAction(f : Insurgent, o, c, then) =>
                o.log("caused", "Outrage".styled(f), "in", c)
                
                val h = o.hand.%(_.suit.m(c.suit)).%(_.suit != Bird || !f.has(NoBirdsOutrage))
                
                if (h.any) {
                    Ask(o, o.hand./(d => OutrageCardAction(f, o, d, 1>0, then).x(h.contains(d).not)).okNeeded)
                }
                else {
                    val draw = DrawCardsAction(f, 1, ToSupporters(f), AddSupportersAction(f, then))
                    
                    if (o.hand.any) {
                        f.log("looked at", o, "cards, no", c.suit, "cards")
                        Force(PeekCardsMainAction(f, Empty, FactionHand(o), o.hand, draw))
                    }
                    else {
                        o.log("had no cards")
                        draw
                    }
                }
                    
            case OutrageCardAction(f : Insurgent, o, d, true, then) =>
                o.hand --> d --> f.drawn
                o.log("added a card to", Supporters.of(f))
                AddSupportersAction(f, then)
                    
            case DestroyBaseAction(f : Insurgent, c, then) =>
                val n = f.officers.num - f.officers.num / 2
                f.officers.sub(n, Critter) --> f.pool

                if (n > 0)
                    f.log("lost", Officers.of(f, n))

                val h = f.supporters.%(_.suit.m(c.suit))

                if (h.any)
                    f.supporters --> h --> discard(f, "from", Supporters.of(f))

                if (f.has(SupportersLimit) && f.bases.none && f.supporters.num > 5)
                    SupportersLimitAction(f, 5, then)
                else
                    then
                
            // TURN
            case BirdsongNAction(40, f : Insurgent) =>
                RevoltMainAction(f)

            case RevoltMainAction(f : Insurgent) =>
                def cost(c : Clearing) = 2 + f.has(CounterInsurgency).??(factions.but(f).%(ruleSelf(_)(c))./(_.at(c).warrior.num - f.at(c).warrior.num * 2)./(max(_, 0)).sum - 1)
                
                val g = f.all(Sympathy).%(c => f.pooled(Base(c.suit)) > 0 || f.has(UnlimitedRevolt))
                val l = g.%(c => f.supporters.%(_.suit.m(c.suit)).num >= cost(c))
 
                val aa = Ask(f, g./(c => RevoltClearingAction(f, c, c.suit, cost(c), f.supporters./(_.suit), f.bases.none && f.has(SupportersLimit)).x(l.has(c).not)).birdsong(f).done(Next).hiSup)

                Ask(f, g./(c => RevoltClearingAction(f, c, c.suit, cost(c), f.supporters./(_.suit), f.bases.none && f.has(SupportersLimit)).x(l.has(c).not)).birdsong(f).done(Next).hiSup)
                
            case RevoltClearingAction(f, c, _, n, _, _) =>
                XXSelectObjectsAction(f, f.supporters)
                    .withGroup(desc(f, "starts", "Revolt".styled(f), "in", c, "with", n.scards(c.suit)))
                    .withRule(_.num(n).each(_.suit.m(c.suit)))
                    .withThen(RevoltAction(f, c, _))(l => "Revolt".styled(f) ~ l.any.?(" in " ~ c.elem(game)))
                    .withExtra($(CancelAction, NoSupporters))
                
            case RevoltAction(f, c, l) =>
                f.supporters --> l --> discard(f).quiet

                f.log("started", "Revolt".styled(f), "in", c, "with", l)
                
                NukeAction(f, factions.%(f.friends), $(c), ClearSector, RevoltBaseAction(f, c))
                
            case RevoltBaseAction(f, c) =>
                if (f.pooled(Base(c.suit)) > 0) {
                    f.pool.one(Base(c.suit)) --> c
                
                    f.all(Sympathy).%(_.suit.m(c.suit)).foreach { _ =>
                        if (f.pooled(Critter) > 0)
                            f.pool.one(Critter) --> c
                    }
                    
                    if (f.pooled(Critter) > 0)
                        f.pool.one(Critter) --> f.officers
                
                    f.log("dug a", "Base".styled(f) ~ " in", c)
                }
                
                Repeat
                
            case BirdsongNAction(60, f : Insurgent) =>
                SpreadSympathyMainAction(f)

            case SpreadSympathyMainAction(f : Insurgent) =>
                val e = f.all(Sympathy)
                val g = (f.pooled(Sympathy) == 0).?(Nil).|(e.some./(_./~(connected(f)(_).of[Clearing]).distinct.diff(e)).|(clearings).%(canPlace(f)))
                val n = f.cost(f.pooled(Sympathy))
                def cost(c : Clearing) = n + f.has(MartialLaw).??(factions.but(f).%(_.at(c).warrior.num >= 3).any.??(1))
                val l = g.%(c => f.supporters.%(_.suit.m(c.suit)).num >= cost(c))
 
                Ask(f, g./(c => SpreadSympathyClearingAction(f, c, c.suit, cost(c), f.reward(f.pooled(Sympathy)), f.supporters./(_.suit), f.bases.none && f.has(SupportersLimit)).x(l.has(c).not)).birdsong(f).done(Next).hiSup)
                
            case SpreadSympathyClearingAction(f, c, _, n, _, _, _) =>
                XXSelectObjectsAction(f, f.supporters)
                    .withGroup(desc(f, "spreads", Sympathy.of(f), "to", c, "with", n.scards(c.suit)))
                    .withRule(_.num(n).each(_.suit.m(c.suit)))
                    .withThen(SpreadSympathyAction(f, c, _))(l => desc("Spread", Sympathy.of(f), l.any.?("to", c)))
                    .withExtra($(CancelAction, NoSupporters))
                
            case SpreadSympathyAction(f, c, l) =>
                highlights :+= PlaceHighlight($(c))

                f.supporters --> l --> discard(f).quiet

                val n = f.reward(f.pooled(Sympathy))

                f.pool.one(Sympathy) --> c
                
                f.nscore(n)("spreading", Sympathy.of(f))(f, "spread", Sympathy.of(f), "to", c, "with", l, VPOn, "and", ScoredVP)

                Repeat
 
            // TURN - MAIN WA
            case DaylightNAction(50, f : Insurgent) =>
                RatMainAction(f)
                
            case RatMainAction(f) =>
                var actions : List[UserAction] = Nil
                    
                actions :+= MobilizeMainAction(f, f.hand).x(f.hand.none, "no cards").x(f.has(SupportersLimit) && f.bases.none && f.supporters.num >= 5, "supporters limit")
                
                actions :+= TrainMainAction(f, f.bases./(_.suit)).x(f.pooled(Critter) == 0, "maximum").x(f.bases.none, "no bases").x(f.hand.none, "no cards").x(f.bases./(_.suit).%(s => f.hand.%(_.suit.m(s)).any).none, "no matching suit")
 
                if (f.has(Terror))
                    actions :+= TerrorizeMainAction(f).x(f.hand.%(_.suit == Bird).none, "no bird cards").x(f.supporters.num < 1, "no supporters")
 
                val c = f.hand.%(craftable(f))
                actions :+= NiceCraftMainAction(f, f.craft ++ f.extracraft, f.crafted).x(f.hand.none).x(c.none, "nothing craftable")

                Ask(f, actions.daylight(f).done(Next))
                
            case MobilizeMainAction(f, _) =>
                val max = min(f.hand.num, (f.has(SupportersLimit) && f.bases.none).?(5).|(999) - f.supporters.num)
                                        
                XXSelectObjectsAction(f, f.hand)
                    .withGroup(f.elem ~ " mobilizes " ~ (max == 1).?("a ").||((max < f.hand.num).?("up to " ~ max.hl ~ " ")) ~ Supporters.pl(f, max))
                    .withRule(_.upTo(max))
                    .withThen(MobilizeAction(f, _))(l => "Mobilize".hl ~ l./(" " ~ _.elem))
                    .withExtra($(CancelAction, NoHand))
                
            case MobilizeAction(f, l) =>
                highlights :+= NothingHighlight

                f.hand --> l --> f.supporters

                f.log("mobilized", Supporters.of(f, l.num))

                Repeat

            case TrainMainAction(f, n) =>
                val l = f.hand.%(d => f.ofBaseSuit(d.suit))
                val max = min(l.num, f.pooled(Critter))
 
                XXSelectObjectsAction(f, f.hand)
                    .withGroup(f.elem ~ " trains " ~ (max == 1).?("an ").||((max < l.num).?("up to " ~ max.hl ~ " ")) ~ Officers.of(f, max))
                    .withRule(_.upTo(max).each(d => f.ofBaseSuit(d.suit)))
                    .withThen(TrainAction(f, _))(l => "Train".hl ~ l.any.??(" " ~ Officers.of(f, l.num)))
                    .withExtra($(CancelAction, NoHand))
                
            case TrainAction(f, l) =>
                f.hand --> l --> discard(f).quiet

                f.pool.sub(l.num, Critter) --> f.officers

                f.log("trained", Officers.of(f, l.num), "with", l)

                Repeat

            case TerrorizeMainAction(f : Insurgent) =>
                Ask(f, f.supporters./(TerrorizeAction(f, _)).cancel)
                
            case TerrorizeAction(f : Insurgent, d) =>
                OptionalDiscardCardAction(f, TakeCard(d), Bird, TerrorizeTakeAction(f, d))

            case TerrorizeTakeAction(f : Insurgent, d) =>
                f.drawn --> discard(f)
                f.supporters --> d --> f.hand
                f.log("terrorized a", Supporters.of(f, 1))
                Repeat

            case EveningNAction(50, f : Insurgent) =>
                InsurgentEveningAction(f)

            case InsurgentEveningAction(f) =>
                var actions : List[UserAction] = Nil
                
                val att = clearings.%(c => attack(f)(c).any)
                actions :+= RatAttackAction(f, att).x(att.none)
                    
                actions :+= RatRecruitAction(f, f.bases).x(f.bases.none, "no bases").x(f.pooled(Critter) == 0 && f.totalWar.not, "maximum")
                
                val mvv = moveFrom(f)
                actions :+= RatMoveAction(f, mvv).x(mvv.none)
                    
                val orr = f.all(Critter).distinct.diff(f.all(Sympathy)).%(canPlace(f))
                val vp = (f.pooled(Sympathy) > 0 && f.has(DelayedScoring).not).??(f.reward(f.pooled(Sympathy)))
                actions :+= RatOrganizeAction(f, orr, vp).x(f.pooled(Sympathy) == 0, "maximum").x(orr.none)
                
                if (f.acted >= f.officers.num) {
                    actions = Nil
    
                    actions :+= EndTurnAction(f)
                }
                else
                    actions :+= EndTurnSoftAction(f, ForfeitActions(f.officers.num - f.acted))

                Ask(f, actions.evening(f))

            case RatDoneAction(f : Insurgent) =>
                f.acted += 1
                Repeat
                
            case RatAttackAction(f, l) =>
                BattleInitAction(f, NoMessage, l, $(CancelAction), RatDoneAction(f))
    
            case RatMoveAction(f, l) =>
                MoveInitAction(f, Nil, NoMessage, l, movable(f), $(CancelAction), RatDoneAction(f))
 
            case RatRecruitAction(f, l) =>
                if (f.pooled(f.warrior) == 0 && f.totalWar) {
                    f.oscore(1)("recruiting")
                    RatDoneAction(f)
                }
                else
                    Ask(f, l./(RatRecruitClearingAction(f, _)).cancel)
    
            case RatRecruitClearingAction(f : Insurgent, c) =>
                highlights :+= PlaceHighlight($(c))

                f.pool.one(Critter) --> c
                f.log("recruited", Critter.of(f), "in", c)
                RatDoneAction(f)

            case RatOrganizeAction(f, l, _) =>
                Ask(f, l./(RatOrganizeClearingAction(f, _)).cancel)
    
            case RatOrganizeClearingAction(f, c) =>
                highlights :+= PlaceHighlight($(c))
                
                val n = f.reward(f.pooled(Sympathy))
                
                f.at(c).one(Critter) --> f.pool
                f.pool.one(Sympathy) --> c

                if (f.has(DelayedScoring).not)
                    f.nscore(n)("organizing", Sympathy.of(f))(f, "organized", Sympathy.of(f), "in", c, VPOn, "for", ForVP)
            
                RatDoneAction(f)

            case NightStartAction(f : Insurgent) =>
                EveningDrawAction(f, 1 + f.bases.num)
                
            case FactionCleanUpAction(f : Insurgent) =>
                f.acted = 0
                EndPlayerTurnAction(f)

                
            case _ => UnknownContinue
        }
    }
}

