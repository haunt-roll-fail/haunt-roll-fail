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

trait InvasiveBBB extends WarriorFaction with CommonInvasive {
    val clashKey = LDvB

    val warrior = FrogBBB

    def abilities(options : $[Meta.O]) = $(DaylightCraft, Swimmers)

    def pieces(options : $[Meta.O]) = FrogBBB *** 15 ++ FoxRabbitMouse./~(s => PeacefulBBB(s) *** 4) ++ FoxRabbitMouse./~(s => MilitantBBB(s) *** 4)

    override val transports : $[$[Transport]] = $($(RuledMove), $(Waterway))

    override def getElem : Elem = super.getElem ~ " " ~ "β".hh

    override def note : Elem = HorizontalBreak ~ "Version from " ~ "2024-09-17".hh

    def advertising = PeacefulBBB(Fox).img(this) ~ MilitantBBB(Fox).img(this) ~ PeacefulBBB(Fox).img(this) ~ MilitantBBB(Fox).img(this) ~ PeacefulBBB(Fox).img(this)

    def motto = "Infiltrate".styled(this)
}

case object FrogBBB extends Warrior with CommonFrogWarrior {
    override def id = "Frog"
    override def name = "Frog"
}

trait FrogTokenBBB extends Token with CommonFrogToken {
    val suit : BaseSuit
}

case class PeacefulBBB(suit : BaseSuit) extends FrogTokenBBB with CommonPeaceful {
    override def id = "Peaceful"
    override def imgid(f : Faction) = f.style + "-" + id
}

case class MilitantBBB(suit : BaseSuit) extends FrogTokenBBB with CommonMilitant {
    override def id = "Militant"
    override def imgid(f : Faction) = f.style + "-" + id
}

case object Imitation extends CardEffect {
    override val name = "Imitation"
}

case class Integration(faction : CommonInvasive, suit : BaseSuit) extends CardEffect {
    override val name = suit.name + " Integration"
}

case class MilitantFavor(f : CommonInvasive) extends CardEffect {
    override val name = "Favor of the Militant Frogs"
}

case object LDvB extends InvasiveBBB {
    val name = "Lilypad Diaspora"
    override def funName = NameReference(name, this) ~ " Diaspora"
    val short = "LDvB"
    val style = "LD"
    val priority = "T"
}


class InvasiveBBBPlayer(val faction : InvasiveBBB)(implicit val game : Game) extends FactionState {
    var acted = 0
    var extra = 0

    val pile = cards("frog-pile", Deck.frogBBB)
    val deck = cards("frog-deck")

    def craft = FoxRabbitMouse./~(s => all(PeacefulBBB(s))./(_.asset)) ++ FoxRabbitMouse./~(s => all(MilitantBBB(s))./(_.asset))
}


trait InvasiveBBBDaylightQuestion extends FactionAction {
    override def self : InvasiveBBB

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            self.acted.times(Image("action-black", styles.action, "")).take(self.acted) ~
            (3 - self.acted).times(Image(self.style + "-action", styles.action)) ~
            (self.extra - max(0, self.acted - 3)).times(Image("action-bird", styles.action)),
        styles.margined)
}

case class InvasiveBBBSetupClearingAction(self : InvasiveBBB, s : BaseSuit, c : Clearing) extends BaseAction(self, "starts in")(c, "(" ~ s.elem ~ ")")

case class InvasiveBBBAttackAction(self : InvasiveBBB, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with InvasiveBBBDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class InvasiveBBBMoveAction(self : InvasiveBBB, l : $[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with InvasiveBBBDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class InvasiveBBBRecruitAction(self : InvasiveBBB, n : Int, l : $[Clearing]) extends OptionAction("Recruit", l.num.times(self.warrior.imgd(self))) with InvasiveBBBDaylightQuestion with Soft with Only[PlacePieceClearingsAction] { def tag = implicitly }
case class InvasiveBBBDoneAction(f : InvasiveBBB, then : ForcedAction) extends ForcedAction

case class InvasiveBBBReconcileMainAction(f : InvasiveBBB) extends ForcedAction with Soft
case class InvasiveBBBReconcileFactionsAction(self : InvasiveBBB, l : $[Clearing], ee : $[Faction]) extends ForcedAction
case class InvasiveBBBReconcileAction(self : InvasiveBBB, l : $[Clearing], e : Faction, ee : $[Faction]) extends ForcedAction with Soft
case class InvasiveBBBReconcileClearingAction(self : InvasiveBBB, l : $[Clearing], e : Faction, c : Clearing, d : DeckCard, ee : $[Faction]) extends ForcedAction

case class InvasiveBBBAngerAction(self : InvasiveBBB, s : BaseSuit, l : $[Clearing]) extends BaseAction("Anger".hh, "or", "Soothe".hh)("Anger", l.comma)
case class InvasiveBBBSootheAction(self : InvasiveBBB, l : $[Clearing]) extends BaseAction()("Soothe", l.comma)

case class InvasiveBBBFrogRecruitAction(self : InvasiveBBB, l : $[Clearing], r : $[Clearing]) extends BaseAction("Recruit in")(l.comma)

case class InvasiveBBBShuffleFrogPileAction(f : InvasiveBBB, shuffled : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]
case class InvasiveBBBDrawCardsFromFrogDeckAction(f : Faction, e : InvasiveBBB, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class InvasiveBBBDiscardOneFrogCardAction(f : Faction, then : ForcedAction) extends ForcedAction with Soft
case class InvasiveBBBDiscardFrogCardAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction("Discard", Frog, "card")(d.img) with ViewCard
case class InvasiveBBBDiscardFrogCraftedAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction("Discard", Frog, "improvement")(d.img) with ViewCard

case class InvasiveBBBExtraFrogMainAction(self : InvasiveBBB, n : Int) extends OptionAction("Extra action".hl, n.times(dt.CardSuit(Frog))) with InvasiveBBBDaylightQuestion with Soft
case class InvasiveBBBExtraFrogAction(f : InvasiveBBB) extends ForcedAction

case class InvasiveBBBImitateItemAction(self : Faction, d : DeckCard, i : Item) extends BaseAction("Take item for", 1.vp)(i, i.img)

case class InvasiveBBBSettleMainAction(self : InvasiveBBB) extends ForcedAction with Soft
case class InvasiveBBBSettleAction(f : InvasiveBBB, d : DeckCard, c : Clearing, s : BaseSuit) extends ForcedAction

case class InvasiveBBBReprisalsAction(f : Faction, l : $[Clearing], then : ForcedAction) extends ForcedAction
case class InvasiveBBBReprisalsFlipAction(f : Faction, l : $[Clearing]) extends ForcedAction



object InvasiveBBBExpansion extends FactionExpansion[InvasiveBBB] {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : InvasiveBBB) =>
            game.states += f -> new InvasiveBBBPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : InvasiveBBB) =>
            Ask(f).some(clearings.diff(game.homelands).diff(board.inner))(c => FoxRabbitMouse.%(s => f.pooled(PeacefulBBB(s)) == 4).%(s => c.cost.matched(s))./(s => InvasiveBBBSetupClearingAction(f, s, c))).needOk.bail(SetupFactionsAction)

        case InvasiveBBBSetupClearingAction(f, s, c) =>
            f.reserve --> f.warrior --> c
            f.reserve --> f.warrior --> c
            f.reserve --> PeacefulBBB(s) --> c

            f.log("placed", f.warrior.of(f), Comma, f.warrior.of(f), Comma, PeacefulBBB(s).of(f), "in", c)

            game.mapping += c -> (game.mapping(c) ++ $(Frog))

            FactionSetupAction(f)

        case AfterSetupAction(f : InvasiveBBB, then) =>
            f.enemies.notOf[Hireling].foldRight(then)((e, q) => InvasiveBBBDrawCardsFromFrogDeckAction(e, f, 1, NoMessage, AddCardsAction(e, q)))

        // HELPER
        case InvasiveBBBShuffleFrogPileAction(f, l, then) =>
            f.pile --> l --> f.deck

            log("The", Frog.elem, "deck was shuffled")

            Milestone(then)

        case InvasiveBBBDrawCardsFromFrogDeckAction(f, e, n, m, then) =>
            if (e.deck.num < n && e.pile.any)
                Shuffle[DeckCard](e.pile, InvasiveBBBShuffleFrogPileAction(e, _, InvasiveBBBDrawCardsFromFrogDeckAction(f, e, n, m, then)))
            else {
                if (e.deck.num < n) {
                    if (e.deck.num >= 2)
                        log("Only", deck.num.hl, "cards were left in the deck")
                    else
                    if (e.deck.num >= 1)
                        log("Only", deck.num.hl, "card was left in the deck")
                    else
                        log("No cards were left in the deck")
                }

                val x = min(n, e.deck.num)

                if (x > 0) {
                    val l = e.deck.get.take(x)

                    if (f.drawn.any)
                        throw new Error("drawn overdrawn")

                    e.deck --> l --> f.drawn

                    f.log("drew", n.of(Frog.elem ~ " card"), m)

                    f.notify(l./(d => ViewCardInfoAction(f, DrewFromTheFrogDeck(f, e, m).elem(game), d)))
                }

                then
            }

        case ForcedRemoveTargetEffectAction(e, c, f : InvasiveBBB, p : FrogTokenBBB, then) =>
            game.mapping += c -> game.original(c)

            InvasiveBBBDiscardOneFrogCardAction(e, then)

        case InvasiveBBBDiscardOneFrogCardAction(f, then) =>
            val h = f.hand.%(d => d.matches(Frog) && d.suit != Bird)

            if (h.none)
                Ask(f).done(then)
            else
                Ask(f).each(h)(InvasiveBBBDiscardFrogCardAction(f, _, then)).needOk

        case InvasiveBBBDiscardFrogCardAction(f, d, then) =>
            f.hand --> d --> discard(f)

            then

        case BattlePostHitInAction(b, e, f : InvasiveBBB, FrogBBB, then) =>
            e.log("blew up", FrogBBB.of(f))

            then

        case BattlePostHitInAction(b, e, f : InvasiveBBB, p : PeacefulBBB, then) =>
            e.log("displaced", p.of(f))

            then

        case BattlePostHitInAction(b, e, f : InvasiveBBB, p : MilitantBBB, then) =>
            e.log("dispersed", p.of(f))

            then

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, Imitation), m) =>
            Ask(f).each(game.uncrafted.distinct)(InvasiveBBBImitateItemAction(f, d, _)).bail(Repeat)

        case InvasiveBBBImitateItemAction(f, d, item) =>
            game.uncrafted :-= item

            f.forTrade +:= item.pristine

            val x = CraftItemCard(Frog, "imitation", $(Frog, Frog), item, 1, "Imitation")

            val q = CraftScoreAction(f, x, x.vp, NoMessage, AnotherPlayerCraftsItemAction(f, Repeat))

            if (f.has(MoleArtisians)) {
                f.hand --> d --> MoleArtisians.display

                Ask(f)(MoleArtisiansRevealAction(f, d, q))(MoleArtisiansDiscardAction(f, d, q))
            }
            else {
                f.hand --> d --> discard.quiet

                q
            }

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, MilitantFavor(e)), m) =>
            f.hand --> d --> discard.quiet

            f.log("was granted", d, "(" ~ d.cost.ss ~ ")", m)

            NukeAction(f, f.enemies.but(e), clearings.%(e.at(_).of[MilitantBBB].any), NukeType.AntiPersonnel, Repeat)

        case BattleCleanupDefenderAction(b, f : InvasiveBBB) if f.at(b.clearing).of[PeacefulBBB].any =>
            val c = b.clearing

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(PeacefulBBB(s))) {
                    f.from(c) --> PeacefulBBB(s) --> f.reserve
                    f.reserve --> MilitantBBB(s) --> c

                    f.log("became", MilitantBBB(s).of(f), "in", c)
                }
            }

            BattleCleanupDefenderAction(b, f)

        // TURN
        case BirdsongNAction(30, f : InvasiveBBB) =>
            InvasiveBBBReconcileMainAction(f)

        case InvasiveBBBReconcileMainAction(f) =>
            val l = clearings.%(f.at(_).of[MilitantBBB].any)
            val ee = f.enemies.%(_.hand.any).%(e => l.exists(c => e.present(c)))

            Ask(f).each(ee.permutations.$)(ee => InvasiveBBBReconcileFactionsAction(f, l, ee).as(ee.comma)("Reconcile".hh)).done(Next)

        case InvasiveBBBReconcileFactionsAction(f, _, Nil) =>
            Repeat

        case InvasiveBBBReconcileFactionsAction(f, l, e :: ee) =>
            InvasiveBBBReconcileAction(f, l, e, ee)

        case InvasiveBBBReconcileAction(f, l, e, ee) =>
            val ll = l.%(e.present)

            if (ll.any)
                YYSelectObjectsAction(e, e.hand)
                    .withRule(_.suit != Frog)
                    .withGroup("Reconcile with " ~ f.elem)
                    .withThens(d => ll./(c => InvasiveBBBReconcileClearingAction(f, l, e, c, d, ee).as("In", c, "with", d)))
                    .withExtras(InvasiveBBBReconcileFactionsAction(f, l, ee).as("Skip"), NoHand)
            else
                Ask(f)(InvasiveBBBReconcileFactionsAction(f, l, ee).as("Skip"))

        case InvasiveBBBReconcileClearingAction(f, l, e, c, d, ee) =>
            e.log("reconciled in", c, "with", d)

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(MilitantBBB(s))) {
                    f.from(c) --> MilitantBBB(s) --> f.reserve
                    f.reserve --> PeacefulBBB(s) --> c

                    game.mapping += c -> (game.original(c) ++ $(Frog))
                }
            }

            e.hand --> d --> f.hand

            InvasiveBBBDrawCardsFromFrogDeckAction(e, f, 1, NoMessage, AddCardsAction(e, InvasiveBBBReconcileFactionsAction(f, l.but(c), ee)))

        case BirdsongNAction(60, f : InvasiveBBB) =>
            val l = clearings.%(f.at(_).of[MilitantBBB].any).%(game.mapping(_) != $(Frog))

            if (l.any) {
                log("Reprisals in", l./(_.elem(game)).comma)

                factions.foldRight(InvasiveBBBReprisalsFlipAction(f, l) : ForcedAction)((e, q) => InvasiveBBBReprisalsAction(e, l, q))
            }
            else
                Next

        case InvasiveBBBReprisalsAction(f, l, then) =>
            val ss = l.%(f.present)./~(game.mapping).but(Frog).distinct

            if (ss.any) {
                val dd = f.hand.%(_.suit.matched(OneOf(ss)))

                if (dd.any) {
                    f.hand --> dd --> discard(f)

                    then
                }
                else
                    then
            }
            else
                then

        case InvasiveBBBReprisalsFlipAction(f, l) =>
            l.foreach { c =>
                game.mapping += c -> $(Frog)
            }

            Next

        case DaylightNAction(20, f : InvasiveBBB) =>
            XCraftMainAction(f)

        case DaylightNAction(40, f : InvasiveBBB) if soft() =>
            val ml = clearings.%(f.at(_).of[MilitantBBB].any).%(f.rules)
            val sf = f.all(PeacefulBBB(Fox))
            val sr = f.all(PeacefulBBB(Rabbit))
            val sm = f.all(PeacefulBBB(Mouse))

            Ask(f)
                .add(sf.any.?(InvasiveBBBAngerAction(f, Fox, sf)))
                .add(sr.any.?(InvasiveBBBAngerAction(f, Rabbit, sr)))
                .add(sm.any.?(InvasiveBBBAngerAction(f, Mouse, sm)))
                .add(ml.any.?(InvasiveBBBSootheAction(f, ml)))((sf.none || sr.none || sm.none || ml.none).?(Next.as("Done")))
                .daylight(f)

        case InvasiveBBBAngerAction(f, s, l) =>
            l.foreach { c =>
                f.from(c) --> PeacefulBBB(s) --> f.reserve
                f.reserve --> MilitantBBB(s) --> c
            }

            f.log("angered in", l./(_.elem).comma)

            Next

        case InvasiveBBBSootheAction(f, l) =>
            FoxRabbitMouse.foreach { s =>
                l.foreach { c =>
                    if (f.at(c).has(MilitantBBB(s))) {
                        f.from(c) --> MilitantBBB(s) --> f.reserve
                        f.reserve --> PeacefulBBB(s) --> c

                        game.mapping += c -> (game.original(c) ++ $(Frog))
                    }
                }
            }

            f.log("soothed in", l./(_.elem).comma)

            Next

        case DaylightNAction(60, f : InvasiveBBB) if soft() =>
            val t = f.pooled(FrogBBB)
            val r = clearings.%(f.at(_).of[MilitantBBB].any).%(f.canPlace)

            if (t == 0 || r.none)
                Ask(f)(Next.as("No Recruit"))
            else
            if (t >= r.num)
                Ask(f)(InvasiveBBBFrogRecruitAction(f, r, r))
            else
                Ask(f)
                  .each(r.combinations(t).$)(InvasiveBBBFrogRecruitAction(f, _, r))
                  .daylight(f)

        case InvasiveBBBFrogRecruitAction(f, l, r) =>
            game.highlights :+= PlaceHighlight(l.distinct)

            l.distinct.foreach { c =>
                val n = l.count(c)
                f.reserve --> n.times(FrogBBB) --> c
            }

            f.log("recruited in", l./(_.elem).comma)

            if (r.num > l.num && f.totalWar)
                f.oscore(r.num - l.num)("recruiting")

            Next

        case DaylightNAction(80, f : InvasiveBBB) =>
            implicit val ask = builder

            if (f.acted < 3 + f.extra) {
                val att = clearings.%(f.canAttackIn)
                + InvasiveBBBAttackAction(f, att).!(att.none)

                val mvv = f.moveFrom.of[Clearing]
                + InvasiveBBBMoveAction(f, mvv).!(mvv.none)
            }

            + EndTurnSoftAction(f, "Turn", ForfeitActions(3 + f.extra - f.acted))

            ask(f).daylight(f)

        case InvasiveBBBAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), InvasiveBBBDoneAction(f, Repeat))

        case InvasiveBBBMoveAction(f, l) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), InvasiveBBBDoneAction(f, Repeat))

        case InvasiveBBBDoneAction(f, then) =>
            f.acted += 1

            then

        // EVENING
        case EveningNAction(20, f : InvasiveBBB) =>
            InvasiveBBBSettleMainAction(f)

        case InvasiveBBBSettleMainAction(f) =>
            val l = clearings.%(f.at(_).of[FrogTokenBBB].none).%(f.canPlace)

            if (l.any && f.hand.any)
                YYSelectObjectsAction(f, f.hand)
                    .withGroup("Settle".styled(f) ~ " in clearings")
                    .withRule(d => l.exists(_.cost.matched(d.suit)))
                    .withThens(d => l.%(_.cost.matched(d.suit))./~(c => FoxRabbitMouse.%(c.cost.matched).%(s => f.pooled(PeacefulBBB(s)) + f.pooled(MilitantBBB(s)) > 4)./(s => InvasiveBBBSettleAction(f, d, c, s).as(f.rules(c).?("Peaceful").|("Militant"), s, f.rules(c).?(PeacefulBBB(s)).|(MilitantBBB(s)).imgd(f), "in", c))))
                    .withExtra($(NoHand, Next.as("Done")))
                    .ask
                    .evening(f)

            else
                Ask(f)(Next.as("Done")("Settle".styled(f))).evening(f)


        case InvasiveBBBSettleAction(f, d, c, s) =>
            f.hand --> d --> discard.quiet

            f.log("settled with", d, "in", c)

            f.reserve --> f.rules(c).?(PeacefulBBB(s)).|(MilitantBBB(s)) --> c

            game.mapping += c -> (game.mapping(c) ++ $(Frog))

            Repeat

        case EveningNAction(60, f : InvasiveBBB) =>
            val b = FoxRabbitMouse./(s => f.pooled(PeacefulBBB(s))).sum - FoxRabbitMouse./(s => f.pooled(MilitantBBB(s))).sum

            if (b > 0)
                f.log("integration failed")
            else
                f.oscore(FoxRabbitMouse./(s => 4 - f.pooled(PeacefulBBB(s))).max)("itegrating")

            Next

        case NightStartAction(f : InvasiveBBB) =>
            val b = FoxRabbitMouse./(s => f.pooled(PeacefulBBB(s))).sum - FoxRabbitMouse./(s => f.pooled(MilitantBBB(s))).sum

            val n = if (b > 0)
                1
            else
                FoxRabbitMouse./(s => 4 - f.pooled(PeacefulBBB(s))).max + 1

            EveningDrawAction(f, n)

        case FactionCleanUpAction(f : InvasiveBBB) =>
            f.acted = 0

            f.extra = 0

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
