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

trait Insurgent extends WarriorFaction {
    val clashKey = WA

    val warrior = Critter

    def abilities(options : $[Meta.O]) = $(GuerillaTactics, MoveOutrage, RemoveOutrage, MartialLaw, SupportersLimit)

    def pieces(options : $[Meta.O]) = Critter *** 10 ++ Sympathy *** 10 ++ Base(Fox) *** 1 ++ Base(Rabbit) *** 1 ++ Base(Mouse) *** 1

    def cost = $(999, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1)
    def reward = $(999, 4, 4, 4, 3, 2, 2, 1, 1, 1, 0)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Critter, Sympathy, Base(Fox), Base(Rabbit), Base(Mouse))

    def advertising = 5.times(Sympathy.img(this)).merge
    def motto = "Revolt".styled(this)
}

case object Sympathy extends Token

case class Base(suit : BaseSuit) extends Building {
    override def id = "Base"
    override def imgid(f : Faction) = f.style + "-" + id + "-" + suit.name
}

case object Critter extends Warrior

case object MartialLaw extends Effect
case object CounterInsurgency extends Effect
case object UnlimitedRevolt extends Effect
case object Terror extends Effect
case object DelayedScoring extends Effect

case object GuerillaTactics extends BattleEffect {
    val name = "Guerilla Tactics"
}

case object MoveOutrage extends Effect
case object BattleOutrage extends Effect
case object RemoveOutrage extends Effect

case object NoBirdsOutrage extends Effect

case object SupportersLimit extends Effect

case object Supporters extends FactionEffect {
    val single = "Supporter"
    val name = single + "s"
    def of(f : Faction, n : Int) = (n != 1).?(n.hlb ~ " " ~ name.styled(f)).|("a " ~ single.styled(f))
    def pl(f : Faction, n : Int) = (n != 1).?(name.styled(f)).|(single.styled(f))
}

case object Officers extends SpecialRegion {
    val single = "Officer"
    override val name = single + "s"
    def of(f : Faction) = name.styled(f)
    def of(f : Faction, n : Int) = (n != 1).?(n.hlb ~ " " ~ name.styled(f)).|("an " ~ single.styled(f))
    def pl(f : Faction, n : Int) = (n != 1).?(name.styled(f)).|(single.styled(f))
    def imgd(f : Faction) = Image(f.style + "-" + "officer", styles.fund)
}

case object WA extends Insurgent {
    val name = "Woodland Alliance"
    override def funName = NameReference(name, this) ~ " Alliance"
    val short = "WA"
    val style = "wa"
    val priority = "C"
}

case object FU extends Insurgent {
    val name = "Forest Union"
    override def funName = NameReference(name, this) ~ " Union"
    val short = "FU"
    val style = "fu"
    val priority = "C'"
}

case object AF extends Insurgent {
    val name = "Animal Front"
    override def funName = NameReference(name, this) ~ " Front"
    val short = "AF"
    val style = "af"
    val priority = "C''"

    override def abilities(options : $[Meta.O]) = $(GuerillaTactics, MoveOutrage, BattleOutrage, NoBirdsOutrage, CounterInsurgency, Terror, DelayedScoring)

    override def note : Elem = SpacedDash ~ "unfinished " ~ "WA".styled(WA) ~ " variant"
}

class InsurgentPlayer(val faction : Insurgent)(implicit val game : Game) extends FactionState {
    var supporters = cards("supporters")
    val officers = location(Officers, u => u.faction == faction && u.piece == faction.warrior)

    var acted = 0

    def craft = all(Sympathy)./(_.asset)

    def bases = all(Base(Fox)) ++ all(Base(Rabbit)) ++ all(Base(Mouse))

    def suits = FoxRabbitMouse.%(hasBase)

    def hasBase(s : BaseSuit) = hasOnMap(Base(s))
}

case class ToSupporters(f : Insurgent) extends Message {
    def elem(implicit game : Game) = " to " ~ Supporters.of(f)
}

case class FactionDiscards(f : Faction) extends Message {
    def elem(implicit game : Game) = f.elem ~ " " ~ "discards"
}

case class SupportersLimit(f : Faction, n : Int) extends Message {
    def elem(implicit game : Game) = " down to " ~ n.hl ~ " " ~ Supporters.of(f)
}

case class RevoltIn(f : Insurgent, c : Clearing) extends Message {
    def elem(implicit game : Game) = f.elem ~ " starts " ~ "Revolt".styled(f) ~ " in " ~ c.elem ~ " with"
}

case class SympathyIn(f : Insurgent, c : Clearing) extends Message {
    def elem(implicit game : Game) = f.elem ~ " spreads " ~ Sympathy.of(f) ~ " to " ~ c.elem ~ " with"
}

case class Mobilize(f : Insurgent) extends Message {
    def elem(implicit game : Game) = f.elem ~ " mobilizes to " ~ Supporters.of(f)
}

case class TrainAnOfficer(f : Insurgent) extends Message {
    def elem(implicit game : Game) = f.elem ~ " trains an " ~ Officers.of(f, 1) ~ " with"
}

case class TrainOfficers(f : Insurgent) extends Message {
    def elem(implicit game : Game) = f.elem ~ " trains " ~ Officers.of(f) ~ " with"
}


trait WADaylightQuestion extends FactionAction {
    override def self : Insurgent
    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem
}

trait InsurgentEveningQuestion extends FactionAction {
    override def self : Insurgent
    def question(implicit game : Game) = self.elem~ SpacedDash ~ Evening.elem ~
        Div(
            1.to(self.officers.$.num)./(_ => Image("action-black", styles.action)).take(self.acted) ~
            1.to(self.officers.$.num)./(_ => Image(self.style + "-action", styles.action)).drop(self.acted)
        , styles.margined)
}


case class SupportersLimitAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class PerformDiscardSupportersAction(f : Faction, l : $[DeckCard], then : ForcedAction) extends ForcedAction

case class SpreadSympathyMainAction(f : Insurgent) extends ForcedAction with Soft

case class SpreadSympathyClearingAction(self : Insurgent, c : Clearing, d : SuitCost, n : Int, vp : Int, s : $[Suit], limited : Boolean) extends
    BaseAction("Spread", Sympathy.of(self), (vp > 0).?(vp.vp), "to" ~ Break ~
        limited.?(
            Span(s./(s => dt.CardSuit(s)).merge ~ (5 - s.num).times(dt.CardEmpty).merge, styles.margined, styles.supporters5)
        ).|(
            Span(dt.CardEmpty ~ s./(s => dt.CardSuit(s)).merge ~ dt.CardEmpty, styles.margined, styles.supportersX)
        ))(c, (s.%(d.matched).num >= n).?((s.%(_ == d) ++ s.%(_ != d).%(d.matched))).|(n.times(d)).take(n)./(s => dt.CardSuit(s)).merge) with Soft

case class SpreadSympathyAction(f : Insurgent, c : Clearing, l : $[DeckCard]) extends ForcedAction

case class RevoltMainAction(f : Insurgent) extends ForcedAction with Soft

case class RevoltClearingAction(self : Insurgent, c : Clearing, d : SuitCost, n : Int, s : $[Suit], limited : Boolean) extends
    BaseAction("Start", "Revolt".styled(self), "in" ~ Break ~
        limited.?(
            Span(s./(s => dt.CardSuit(s)).merge ~ (5 - s.num).times(dt.CardEmpty).merge, styles.margined, styles.supporters5)
        ).|(
            Span(dt.CardEmpty ~ s./(s => dt.CardSuit(s)).merge ~ dt.CardEmpty, styles.margined, styles.supportersX)
        ))(c, (s.%(d.matched).num >= n).?((s.%(_ == d) ++ s.%(_ != d).%(d.matched))).|(n.times(d)).take(n)./(s => dt.CardSuit(s)).merge) with Soft

case class RevoltAction(f : Insurgent, c : Clearing, s : BaseSuit, l : $[DeckCard]) extends ForcedAction
case class RevoltBaseAction(f : Insurgent, c : Clearing, s : BaseSuit) extends ForcedAction

case class InsurgentMainAction(f : Insurgent) extends ForcedAction

case class MobilizeMainAction(self : Insurgent, l : $[DeckCard]) extends OptionAction("Mobilize".styled(self), Span(l./(d => dt.CardSuit(d.suit)).merge, styles.supportersX)) with WADaylightQuestion with Soft with Only[MobilizeAction] { def tag = implicitly }
case class MobilizeAction(self : Insurgent, l : $[DeckCard]) extends ForcedAction

case class TrainMainAction(self : Insurgent, s : $[Suit]) extends OptionAction("Train".styled(self), dt.CardSuit(s.single.|(AnySuit)), dt.Arrow, Officers.imgd(self)) with WADaylightQuestion with Soft with Only[TrainAction] { def tag = implicitly }
case class TrainAction(f : Insurgent, l : $[DeckCard]) extends ForcedAction

case class TerrorizeMainAction(self : Insurgent) extends OptionAction("Terrorize".styled(self)) with WADaylightQuestion with Soft
case class TerrorizeAction(self : Insurgent, d : DeckCard) extends BaseAction("Terrorize".styled(self), "a", "Supporter".styled(self))(d.img) with ViewCard with Soft
case class TerrorizeTakeAction(f : Insurgent, d : DeckCard) extends ForcedAction

case class RemoveSympathyAction(f : Insurgent, o : Faction, c : Clearing, then : ForcedAction) extends ForcedAction

case class OutrageAction(f : Insurgent, o : Faction, c : Clearing, then : ForcedAction) extends ForcedAction
case class OutrageCardAction(o : Faction, self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(self, "surrenders a card to", Supporters.of(o))(d.img) with ViewCard
case class DestroyBaseAction(f : Insurgent, c : Clearing, then : ForcedAction) extends ForcedAction
case class DestroyBaseSuitAction(f : Insurgent, s : BaseSuit, then : ForcedAction) extends ForcedAction

case class InsurgentEveningAction(f : Insurgent) extends ForcedAction with Soft
case class InsurgentDoneAction(f : Insurgent) extends ForcedAction

case class InsurgentAttackAction(self : Insurgent, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with InsurgentEveningQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class InsurgentMoveAction(self : Insurgent, l : $[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with InsurgentEveningQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class InsurgentRecruitScoreAction(self : Insurgent) extends OptionAction("Recruit".styled(self), self.warrior.imgd(self)) with InsurgentEveningQuestion
case class InsurgentRecruitAction(self : Insurgent, l : $[Clearing]) extends OptionAction("Recruit".styled(self), self.warrior.imgd(self)) with InsurgentEveningQuestion with Soft with Only[InsurgentRecruitClearingAction] { def tag = implicitly }
case class InsurgentRecruitClearingAction(self : Insurgent, c : Clearing) extends BaseAction("Recruit".styled(self), self.warrior.img(self), "in")(c)
case class InsurgentOrganizeAction(self : Insurgent, l : $[Clearing], vp : Int) extends OptionAction("Organize".styled(self), self.warrior.imgd(self), dt.Arrow, Sympathy.imgd(self), (vp > 0).?(vp.vp)) with InsurgentEveningQuestion with Soft with Only[InsurgentOrganizeClearingAction] { def tag = implicitly }
case class InsurgentOrganizeClearingAction(self : Insurgent, c : Clearing) extends BaseAction("Organize", Sympathy.of(self), "in")(c)

case class AddSupportersAction(f : Insurgent, then : ForcedAction) extends ForcedAction


object InsurgentExpansion extends FactionExpansion[Insurgent] {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Insurgent) =>
            game.states += f -> new InsurgentPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Insurgent) =>
            DrawCardsAction(f, 3, ToSupporters(f), AddSupportersAction(f, SetupFactionsAction))

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

        case ForcedRemoveTargetEffectAction(e, c, f : Insurgent, Sympathy, then) =>
            RemoveSympathyAction(f, e.as[Hireling]./~(_.owner).|(e), c, then)

        case ForcedRemoveTargetEffectAction(e, c, f : Insurgent, Base(s), then) =>
            DestroyBaseSuitAction(f, s, then)

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

        case MoveCompleteAction(self, f, from, to : Clearing, l, e, then) if l.of[Warrior].any && factions.but(f).diff(e).of[Insurgent].any =>
            val insurgent = factions.but(f).diff(e).of[Insurgent]

            var q : ForcedAction = MoveCompleteAction(self, f, from, to, l, e ++ insurgent, then)

            insurgent.%!(i => f.as[Hireling].?(i.has)).%(_.at(to).has(Sympathy)).%(_.has(MoveOutrage)).foreach { i =>
                q = OutrageAction(i, self, to, q)
            }

            q

        case OutrageAction(f : Insurgent, o, c, then) =>
            o.log("caused", "Outrage".styled(f), "in", c)

            val h = o.hand.%(_.matches(c.cost)).%(_.suit != Bird || !f.has(NoBirdsOutrage))

            if (h.any) {
                soft()

                Ask(o).each(o.hand)(d => OutrageCardAction(f, o, d, then).!(h.has(d).not)).needOk
            }
            else {
                val draw = DrawCardsAction(f, 1, ToSupporters(f), AddSupportersAction(f, then))

                if (o.hand.any) {
                    f.log("looked at", o, "cards, no", c.suits, "cards")

                    NoAsk(o)(PeekCardsMainAction(f, Empty, FactionHand(o), o.hand, draw))
                }
                else {
                    o.log("had no cards")

                    NoAsk(o)(draw)
                }
            }

        case OutrageCardAction(f : Insurgent, o, d, then) =>
            o.hand --> d --> f.drawn
            o.log("added a card to", Supporters.of(f))
            AddSupportersAction(f, then)

        case DestroyBaseAction(f : Insurgent, c, then) =>
            val n = f.officers.$.num /↑ 2

            if (n > 0) {
                f.officers --> n.times(f.warrior) --> game.recycle

                f.log("lost", Officers.of(f, n))
            }

            val h = f.supporters.%(_.matches(c.cost))

            if (h.any)
                f.supporters --> h --> discard(f, "from", Supporters.of(f))

            if (f.has(SupportersLimit) && f.bases.none && f.supporters.num > 5)
                SupportersLimitAction(f, 5, then)
            else
                then

        case DestroyBaseSuitAction(f : Insurgent, s, then) =>
            val n = f.officers.$.num /↑ 2

            if (n > 0) {
                f.officers --> n.times(f.warrior) --> game.recycle

                f.log("lost", Officers.of(f, n))
            }

            val h = f.supporters.%(_.matches(s))

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
            def cost(c : Clearing) = 2 + f.has(CounterInsurgency).??(factions.but(f).%(_.ruleSelf(c))./(_.at(c).of[Warrior].num - f.at(c).of[Warrior].num * 2)./(max(_, 0)).sum - 1)

            val l = f.all(Sympathy).%(c => FoxRabbitMouse.exists(s => f.pool(Base(s)) && s.matches(c.cost)) || f.has(UnlimitedRevolt))
            val ll = l.%(c => f.supporters.%(_.matches(c.cost)).num >= cost(c))

            if (l.any)
                Ask(f)(HiSupporters)
                  .each(l)(c => RevoltClearingAction(f, c, c.cost, cost(c), f.supporters./(_.suit), f.bases.none && f.has(SupportersLimit)).x(ll.has(c).not))
                  .birdsong(f)
                  .done(Next)
            else
                Ask(f)(Next.as("No Revolt"))

        case RevoltClearingAction(f, c, _, n, _, _) =>
            (XXSelectObjectsAction(f, f.supporters)
                .withGroup(game.desc(f, "starts", "Revolt".styled(f), "in", c, "with", n.scards(c.cost)))
                .withRule(_.num(n).each(_.matches(c.cost)))
                .withThenList(FoxRabbitMouse.%(s => (f.pool(Base(s)) && s.matches(c.cost)) || f.has(UnlimitedRevolt)))
                    ((s, l) => RevoltAction(f, c, s, l))
                    ((s, l) => "Revolt".styled(f) ~ l.any.?(" in " ~ c.elem(game)) ~ (c.suits.num > 1).?(" with " ~ s.elem))
                .withExtra($(CancelAction, NoSupporters)))

        case RevoltAction(f, c, s, l) =>
            f.supporters --> l --> discard.quiet

            f.log("started", "Revolt".styled(f), "in", c, "with", l)

            NukeAction(f, f.enemies.%(f.canRemove(c)), $(c), NukeType.ClearSector, RevoltBaseAction(f, c, s))

        case RevoltBaseAction(f, c, s) =>
            if (f.pool(Base(s))) {
                if (f.canBuild(c)) {
                    f.reserve --> Base(s) --> c

                    f.log("dug a", (c.suits.num > 1).?(s), "Base".styled(f), "in", c)
                }

                val w = f.reserve --?> f.all(Sympathy).%(_.cost.matched(s)).num.times(f.warrior)

                if (w.any) {
                    w --> c

                    f.log("placed", w.num.times(f.warrior.of(f)).comma, "in", c)
                }

                val o = f.reserve --?> f.warrior
                if (o.any) {
                    o --> f.officers

                    f.log("enlisted an", Officers.of(f, 1))
                }
            }

            Repeat

        case BirdsongNAction(60, f : Insurgent) =>
            SpreadSympathyMainAction(f)

        case SpreadSympathyMainAction(f : Insurgent) =>
            val e = f.all(Sympathy)
            val g = (f.pool(Sympathy)).??(e.some./(_./~(f.connectedFor(_)).of[Clearing].distinct.diff(e)).|(clearings).%(f.canPlace))
            val n = f.cost(f.pooled(Sympathy))
            def cost(c : Clearing) = n + f.has(MartialLaw).??(factions.but(f).%(_.at(c).of[Warrior].num >= 3).any.??(1)) + options.has(Imprinting).??(factions.but(f).%(_.at(c).has(Sympathy)).num)
            val l = g.%(c => f.supporters.%(_.matches(c.cost)).num >= cost(c))

            if (f.supporters.any || f.birdsongNewSupporter)
                Ask(f)(HiSupporters)
                  .each(g)(c => SpreadSympathyClearingAction(f, c, c.cost, cost(c), f.reward(f.pooled(Sympathy)), f.supporters./(_.suit), f.bases.none && f.has(SupportersLimit)).x(l.has(c).not))
                  .birdsong(f)
                  .done(Next)
            else
                Ask(f)(Next.as("No Spread Sympathy"))

        case SpreadSympathyClearingAction(f, c, _, n, _, _, _) =>
            XXSelectObjectsAction(f, f.supporters)
                .withGroup(game.desc(f, "spreads", Sympathy.of(f), "to", c, "with", n.scards(c.cost)))
                .withRule(_.num(n).each(_.matches(c.cost)))
                .withThen(SpreadSympathyAction(f, c, _))(l => game.desc("Spread", Sympathy.of(f), l.any.?("to", c)))
                .withExtra($(CancelAction, NoSupporters))

        case SpreadSympathyAction(f, c, l) =>
            game.highlights :+= PlaceHighlight($(c))

            f.supporters --> l --> discard.quiet

            val n = f.reward(f.pooled(Sympathy))

            f.reserve --> Sympathy --> c

            f.nscore(n)("spreading", Sympathy.of(f))(f, "spread", Sympathy.of(f), "to", c, "with", l, AndScoredVP)

            Repeat

        // TURN - MAIN WA
        case DaylightNAction(50, f : Insurgent) =>
            InsurgentMainAction(f)

        case InsurgentMainAction(f) =>
            soft()

            var actions : $[UserAction] = $

            actions :+= MobilizeMainAction(f, f.hand).x(f.hand.none, "no cards").x(f.has(SupportersLimit) && f.bases.none && f.supporters.num >= 5, "supporters limit")

            actions :+= TrainMainAction(f, f.suits).!(f.pool(f.warrior).not, "maximum").!(f.bases.none, "no bases").!(f.hand.none, "no cards").!(f.suits.%(s => f.hand.%(_.matches(s)).any).none, "no matching suit")

            if (f.has(Terror))
                actions :+= TerrorizeMainAction(f).x(f.hand.%(_.suit == Bird).none, "no bird cards").x(f.supporters.num < 1, "no supporters")

            val c = f.hand.%(f.craftable)
            actions :+= NiceCraftMainAction(f, f.craft ++ f.frogCraft ++ f.extraCraft, f.crafted, Empty).x(f.hand.none).x(c.none, "nothing craftable")

            Ask(f)(actions).daylight(f).add(Next.as("End", "Daylight".styled(styles.phase)))

        case MobilizeMainAction(f, _) =>
            val max = min(f.hand.num, (f.has(SupportersLimit) && f.bases.none).?(5).|(999) - f.supporters.num)

            XXSelectObjectsAction(f, f.hand)
                .withGroup(f.elem ~ " mobilizes " ~ (max == 1).?("a ").||((max < f.hand.num).?("up to " ~ max.hl ~ " ")) ~ Supporters.pl(f, max))
                .withRule(_.upTo(max))
                .withThen(MobilizeAction(f, _))(l => "Mobilize".hl ~ l./(" " ~ _.elem))
                .withExtra($(CancelAction, NoHand))

        case MobilizeAction(f, l) =>
            game.highlights :+= NothingHighlight

            f.hand --> l --> f.supporters

            f.log("mobilized", Supporters.of(f, l.num))

            Repeat

        case TrainMainAction(f, n) =>
            val l = f.hand.%(_.matches(AnyOf(f.suits)))
            val max = min(l.num, f.pooled(f.warrior))

            XXSelectObjectsAction(f, f.hand)
                .withGroup(f.elem ~ " trains " ~ (max == 1).?("an ").||((max < l.num).?("up to " ~ max.hl ~ " ")) ~ Officers.of(f, max))
                .withRule(_.upTo(max).each(_.matches(AnyOf(f.suits))))
                .withThen(TrainAction(f, _))(l => "Train".hl ~ l.any.?(" " ~ Officers.of(f, l.num)))
                .withExtra($(CancelAction, NoHand))

        case TrainAction(f, l) =>
            f.hand --> l --> discard.quiet

            f.reserve --> l.num.times(f.warrior) --> f.officers

            f.log("trained", Officers.of(f, l.num), "with", l)

            Repeat

        case TerrorizeMainAction(f : Insurgent) =>
            Ask(f)(f.supporters./(TerrorizeAction(f, _))).cancel

        case TerrorizeAction(f : Insurgent, d) =>
            OptionalDiscardCardAction(f, TakeCard(d), Bird, TerrorizeTakeAction(f, d))

        case TerrorizeTakeAction(f : Insurgent, d) =>
            f.drawn --> discard(f)
            f.supporters --> d --> f.hand
            f.log("terrorized a", Supporters.of(f, 1))
            Repeat

        case EveningNAction(50, f : Insurgent) =>
            InsurgentEveningAction(f)

        case InsurgentEveningAction(f) if f.acted >= f.officers.$.num =>
            Ask(f)(Next.as("End Turn".hl)).evening(f)

        case InsurgentEveningAction(f) =>
            implicit val ask = builder

            val att = clearings.%(f.canAttackIn)
            + InsurgentAttackAction(f, att).x(att.none)

            val rll = f.bases.%(f.canPlace)
            if (rll.any && f.pool(f.warrior).not && f.totalWar)
                + InsurgentRecruitScoreAction(f)
            else
                + InsurgentRecruitAction(f, rll)
                    .!(f.bases.none, "no bases")
                    .!(rll.none, "can't place")
                    .!(f.pool(f.warrior).not, "maximum")

            val mvv = f.moveFrom.of[Clearing]
            + InsurgentMoveAction(f, mvv).x(mvv.none)

            val orr = f.all(f.warrior).distinct.diff(f.all(Sympathy)).%(f.canPlace)
            val vp = (f.pool(Sympathy) && f.has(DelayedScoring).not).??(f.reward(f.pooled(Sympathy)))
            + InsurgentOrganizeAction(f, orr, vp).x(f.pool(Sympathy).not, "maximum").x(orr.none)

            + EndTurnSoftAction(f, "Turn", ForfeitActions(f.officers.$.num - f.acted))

            ask(f).evening(f)

        case InsurgentDoneAction(f : Insurgent) =>
            f.acted += 1
            Repeat

        case InsurgentAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), InsurgentDoneAction(f))

        case InsurgentMoveAction(f, l) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), InsurgentDoneAction(f))

        case InsurgentRecruitScoreAction(f : Insurgent) =>
            f.oscore(1)("recruiting")
            InsurgentDoneAction(f)

        case InsurgentRecruitAction(f, l) =>
            Ask(f)(l./(InsurgentRecruitClearingAction(f, _))).cancel

        case InsurgentRecruitClearingAction(f : Insurgent, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> f.warrior --> c

            f.log("recruited", f.warrior.of(f), "in", c)

            InsurgentDoneAction(f)

        case InsurgentOrganizeAction(f, l, _) =>
            Ask(f).each(l)(InsurgentOrganizeClearingAction(f, _)).cancel

        case InsurgentOrganizeClearingAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            val n = f.reward(f.pooled(Sympathy))

            f.from(c) --> f.warrior --> game.recycle

            f.reserve --> Sympathy --> c

            if (f.has(DelayedScoring).not)
                f.nscore(n)("organizing", Sympathy.of(f))(f, "organized", Sympathy.of(f), "in", c, ForVP)

            InsurgentDoneAction(f)

        case NightStartAction(f : Insurgent) =>
            EveningDrawAction(f, 1 + f.bases.num)

        case FactionCleanUpAction(f : Insurgent) =>
            f.acted = 0

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
