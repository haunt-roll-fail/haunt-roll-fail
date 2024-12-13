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


case class PeaceTalks(faction : CommonInvasive) extends CardEffect {
    override val name = "Peace Talks"
}

case class Laborers(faction : CommonInvasive) extends CardEffect {
    override val name = "Laborers"
}


trait InvasiveCCC extends WarriorFaction with CommonInvasive {
    val clashKey = LDvC

    val warrior = FrogCCC

    def abilities(options : $[Meta.O]) = $(DaylightCraft, Swimmers)

    def pieces(options : $[Meta.O]) = FrogCCC *** 20 ++ FoxRabbitMouse./~(s => PeacefulCCC(s) *** 4) ++ FoxRabbitMouse./~(s => MilitantCCC(s) *** 4)

    override val transports : $[$[Transport]] = $($(RuledMove), $(Waterway))

    override def getElem : Elem = super.getElem ~ " " ~ "γ".hh

    override def note : Elem = HorizontalBreak ~ "Version from " ~ "2024-10-01".hh

    def advertising = PeacefulCCC(Fox).img(this) ~ MilitantCCC(Fox).img(this) ~ PeacefulCCC(Fox).img(this) ~ MilitantCCC(Fox).img(this) ~ PeacefulCCC(Fox).img(this)

    def motto = "Infiltrate".styled(this)
}

case object FrogCCC extends Warrior with CommonFrogWarrior {
    override def id = "Frog"
    override def name = "Frog"
}

trait FrogTokenCCC extends Token with CommonFrogToken {
    val suit : BaseSuit
}

case class PeacefulCCC(suit : BaseSuit) extends FrogTokenCCC with CommonPeaceful {
    override def id = "Peaceful"
    override def imgid(f : Faction) = f.style + "-" + id
}

case class MilitantCCC(suit : BaseSuit) extends FrogTokenCCC with CommonMilitant {
    override def id = "Militant"
    override def imgid(f : Faction) = f.style + "-" + id
}



case object LDvC extends InvasiveCCC {
    val name = "Lilypad Diaspora"
    override def funName = NameReference(name, this) ~ " Diaspora"
    val short = "LDvC"
    val style = "LD"
    val priority = "T"
}


class InvasiveCCCPlayer(val faction : InvasiveCCC)(implicit val game : Game) extends FactionState {
    var acted = 0

    val pile = cards("frog-pile", Deck.frogCCC)
    val deck = cards("frog-deck")

    var latent = FoxRabbitMouse./(s => s -> 4.times(PeacefulCCC(s) : FrogTokenCCC)).toMap

    var hostile : $[Faction] = $

    def craft = FoxRabbitMouse./~(s => all(PeacefulCCC(s))./(_.asset)) ++ FoxRabbitMouse./~(s => all(MilitantCCC(s))./(_.asset))
}


trait InvasiveCCCDaylightQuestion extends FactionAction {
    override def self : InvasiveCCC

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            self.acted.times(Image("action-black", styles.action, "")).take(self.acted) ~
            (3 - self.acted).times(Image(self.style + "-action", styles.action)),
        styles.margined)
}

case class InvasiveCCCSetupClearingAction(self : InvasiveCCC, s : BaseSuit, c : Clearing) extends BaseAction(self, "starts in")(c, "(" ~ s.elem ~ ")")

case class InvasiveCCCAttackAction(self : InvasiveCCC, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with InvasiveCCCDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class InvasiveCCCMoveAction(self : InvasiveCCC, l : $[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with InvasiveCCCDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class InvasiveCCCRecruitAction(self : InvasiveCCC, n : Int, l : $[Clearing]) extends OptionAction("Recruit", l.num.times(self.warrior.imgd(self))) with InvasiveCCCDaylightQuestion with Soft with Only[PlacePieceClearingsAction] { def tag = implicitly }
case class InvasiveCCCDoneAction(f : InvasiveCCC, then : ForcedAction) extends ForcedAction

case class InvasiveCCCReconcileMainAction(f : InvasiveCCC) extends ForcedAction with Soft
case class InvasiveCCCReconcileContinueAction(f : InvasiveCCC, l : $[Clearing], first : Boolean) extends ForcedAction with Soft
case class InvasiveCCCReconcileClearingAction(f : InvasiveCCC, c : Clearing, then : ForcedAction) extends ForcedAction
case class InvasiveCCCReconcileClearingContinueAction(f : InvasiveCCC, c : Clearing, l : $[Faction], then : ForcedAction) extends ForcedAction
case class InvasiveCCCReconcileClearingCardAction(f : InvasiveCCC, c : Clearing, e : Faction, d : DeckCard, l : $[Faction], then : ForcedAction) extends ForcedAction
case class InvasiveCCCReconcileClearingSkipAction(f : InvasiveCCC, c : Clearing, e : Faction, l : $[Faction], then : ForcedAction) extends ForcedAction
case class InvasiveCCCReconcileClearingFactionAction(f : InvasiveCCC, c : Clearing, e : Faction, then : ForcedAction) extends ForcedAction

case class InvasiveCCCReprisalsMainAction(f : InvasiveCCC) extends ForcedAction with Soft
case class InvasiveCCCReprisalsAction(f : InvasiveCCC, c : Clearing) extends ForcedAction
case class InvasiveCCCReprisalsContinueAction(f : InvasiveCCC, c : Clearing) extends ForcedAction with Soft
case class InvasiveCCCReprisalsCardAction(f : InvasiveCCC, c : Clearing, e : Faction, d : DeckCard) extends ForcedAction
case class InvasiveCCCReprisalsFlipAction(f : InvasiveCCC, c : Clearing) extends ForcedAction

case class InvasiveCCCAngerAction(self : InvasiveCCC, s : BaseSuit, l : $[Clearing]) extends BaseAction("Anger".hh, "or", "Soothe".hh)("Anger", l.comma)
case class InvasiveCCCSootheAction(self : InvasiveCCC, l : $[Clearing]) extends BaseAction()("Soothe", l.comma)

case class InvasiveCCCFrogRecruitAction(self : InvasiveCCC, l : $[Clearing], r : $[Clearing]) extends BaseAction("Recruit in")(l.comma)

case class InvasiveCCCShuffleFrogPileAction(f : InvasiveCCC, shuffled : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]
case class InvasiveCCCDrawCardsFromFrogDeckAction(f : Faction, e : InvasiveCCC, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class InvasiveCCCDiscardOneFrogCardAction(f : Faction, then : ForcedAction) extends ForcedAction with Soft
case class InvasiveCCCDiscardFrogCardAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction("Discard", Frog, "card")(d.img) with ViewCard

case class InvasiveCCCImitateItemAction(self : Faction, d : DeckCard, i : Item) extends BaseAction("Take item for", 1.vp)(i, i.img)

case class InvasiveCCCSettleMainAction(f : InvasiveCCC) extends ForcedAction with Soft
case class InvasiveCCCSettleAction(f : InvasiveCCC, d : |[DeckCard], c : Clearing, s : BaseSuit) extends ForcedAction

case class InvasiveCCCIntegrateMainAction(f : InvasiveCCC, l : $[BaseSuit]) extends ForcedAction with Soft
case class InvasiveCCCIntegrateAction(f : InvasiveCCC, s : BaseSuit, d : DeckCard, l : $[BaseSuit]) extends ForcedAction

case class InvasiveCCCPeaceTalksAction(f : Faction, e : InvasiveCCC, c : Clearing) extends ForcedAction



object InvasiveCCCExpansion extends FactionExpansion[InvasiveCCC] {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : InvasiveCCC) =>
            game.states += f -> new InvasiveCCCPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : InvasiveCCC) =>
            val l = FoxRabbitMouse./~(s => f.all(PeacefulCCC(s)))
            if (l.num < 3)
                Ask(f).some(clearings.diff(l).diff(game.homelands).diff(board.inner))(c => FoxRabbitMouse.%(s => f.pool(PeacefulCCC(s))).%(s => c.cost.matched(s))./(s => InvasiveCCCSetupClearingAction(f, s, c))).needOk.bail(SetupFactionsAction)
            else
                Ask(f).bail(SetupFactionsAction)

        case InvasiveCCCSetupClearingAction(f, s, c) =>
            if (game.options.has(SetupTypeHomelands)) {
                game.homelands :+= c

                f.log("started in", c)
            }

            f.reserve --> f.warrior --> c
            f.reserve --> f.warrior --> c

            val t = f.latent(s).first
            f.latent += s -> f.latent(s).drop(1)

            f.reserve --> t --> c

            f.log("placed", f.warrior.of(f), Comma, f.warrior.of(f), Comma, t.of(f), "in", c)

            game.mapping += c -> (game.mapping(c) ++ $(Frog))

            FactionSetupAction(f)

        case AfterSetupAction(f : InvasiveCCC, then) =>
            f.enemies.notOf[Hireling].foldRight(then)((e, q) => InvasiveCCCDrawCardsFromFrogDeckAction(e, f, 1, NoMessage, AddCardsAction(e, q)))

        // HELPER
        case InvasiveCCCShuffleFrogPileAction(f, l, then) =>
            f.pile --> l --> f.deck

            log("The", Frog.elem, "deck was shuffled")

            Milestone(then)

        case InvasiveCCCDrawCardsFromFrogDeckAction(f, e, n, m, then) =>
            if (e.deck.num < n && e.pile.any)
                Shuffle[DeckCard](e.pile, InvasiveCCCShuffleFrogPileAction(e, _, InvasiveCCCDrawCardsFromFrogDeckAction(f, e, n, m, then)))
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

        case ForcedRemoveTargetEffectAction(e, c, f : InvasiveCCC, p : FrogTokenCCC, then) =>
            game.mapping += c -> game.original(c)

            f.latent += p.suit -> (p +: f.latent(p.suit))

            InvasiveCCCDiscardOneFrogCardAction(e, then)

        case InvasiveCCCDiscardOneFrogCardAction(f, then) =>
            val h = f.hand.%(d => d.matches(Frog) && d.suit != Bird)

            if (h.none)
                Ask(f).done(then)
            else
                Ask(f).each(f.hand)(d => InvasiveCCCDiscardFrogCardAction(f, d, then).!(h.has(d).not))(NoHand).needOk

        case InvasiveCCCDiscardFrogCardAction(f, d, then) =>
            f.hand --> d --> discard(f)

            then

        case BattlePostHitInAction(b, e, f : InvasiveCCC, FrogCCC, then) =>
            e.log("blew up", FrogCCC.of(f))

            then

        case BattlePostHitInAction(b, e, f : InvasiveCCC, p : PeacefulCCC, then) =>
            e.log("displaced", p.of(f))

            then

        case BattlePostHitInAction(b, e, f : InvasiveCCC, p : MilitantCCC, then) =>
            e.log("dispersed", p.of(f))

            then

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, Imitation), m) =>
            Ask(f).each(game.uncrafted.distinct)(InvasiveCCCImitateItemAction(f, d, _)).bailHard(Repeat)

        case InvasiveCCCImitateItemAction(f, d, item) =>
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

            NukeAction(f, f.enemies.but(e), clearings.%(e.at(_).of[MilitantCCC].any), NukeType.AntiPersonnel, Repeat)

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, PeaceTalks(e : InvasiveCCC)), m) =>
            f.hand --> d --> discard.quiet

            val l = clearings.%(c => e.at(c).use(l => l.of[Warrior].any && l.of[MilitantCCC].none))

            if (l.any) {
                f.log("has started", d, "(" ~ d.cost.ss ~ ")", m)

                Ask(f).each(clearings.%(c => e.at(c).use(l => l.of[Warrior].any && l.of[MilitantCCC].none)))(c => InvasiveCCCPeaceTalksAction(f, e, c).as(c)("Peace Talks".hl, "in")).needOk
            }
            else {
                f.log("has started", d, "(" ~ d.cost.ss ~ ")", m, "with no effect")

                Repeat
            }

        case InvasiveCCCPeaceTalksAction(f, e, c) =>
            NukeAction(f, $(e), $(c), NukeType.AntiPersonnel, Repeat)

        case BattleStartedAction(b) if b.defender.as[InvasiveCCC].?(_.at(b.clearing).of[PeacefulCCC].any) =>
            val c = b.clearing
            val f = b.defender

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(PeacefulCCC(s))) {
                    f.from(c) --> PeacefulCCC(s) --> f.reserve
                    f.reserve --> MilitantCCC(s) --> c

                    f.log("became", MilitantCCC(s).of(f), "in", c)
                }
            }

            BattleStartedAction(b)

        // TURN
        case BirdsongNAction(30, f : InvasiveCCC) =>
            InvasiveCCCReconcileMainAction(f)

        case InvasiveCCCReconcileMainAction(f) =>
            val ee = f.enemies
            val l = clearings.%(f.at(_).of[MilitantCCC].any)

            InvasiveCCCReconcileContinueAction(f, l, true)

        case InvasiveCCCReconcileContinueAction(f, l, first) =>
            if (l.any)
                Ask(f).each(l)(c => InvasiveCCCReconcileClearingAction(f, c, InvasiveCCCReconcileContinueAction(f, l.but(c), false)).as(c)("Reconcile in")).use(a => first.?(a.birdsong(f)).|(a)).needOk
            else
                Ask(f)(Next.as("Done")("Reconcile")).birdsong(f)

        case InvasiveCCCReconcileClearingAction(f, c, then) =>
            f.log("tried to reconcile in", c)

            f.hostile = $

            InvasiveCCCReconcileClearingContinueAction(f, c, f.enemies.%(_.hand.any).%(_.present(c)), then)

        case InvasiveCCCReconcileClearingContinueAction(f, c, ll, then) =>
            val l = ll.diff(f.hostile).%(_.drawn.none)
            val r = f.enemies.%(_.drawn.any)
            val q = Ask(f).each(r)(e => InvasiveCCCReconcileClearingFactionAction(f, c, e, then).as(e)("Reconcile", l.any.?("early"), "in", c, "with"))

            if (l.none) {
                if (r.any)
                    q
                else
                    Ask(f)(then.as("Done")("Failed to reconcile in", c))
            }
            else
                MultiAsk(
                    r.any.$(q.each(l)(e => Info(e.elem ~ " are thinking")).needOk) ++
                    l./(e =>
                        YYSelectObjectsAction(e, e.hand)
                            .withGroup(f.elem ~ " offers reconciliation in " ~ c.elem)
                            .withRule(_.suit.matched(Frog).not)
                            .withThen(d => InvasiveCCCReconcileClearingCardAction(f, c, e, d, ll, then))(d => "Reconcile with " ~ d.elem)("Reconcile")
                            .withExtras(InvasiveCCCReconcileClearingSkipAction(f, c, e, ll, then).as("Skip"), NoHand)
                            .ask
                    )
                )

        case InvasiveCCCReconcileClearingSkipAction(f, c, e, l, then) =>
            f.hostile :+= e

            InvasiveCCCReconcileClearingContinueAction(f, c, l, then)

        case InvasiveCCCReconcileClearingCardAction(f, c, e, d, l, then) =>
            e.hand --> d --> e.drawn

            e.log("offered a card")

            InvasiveCCCReconcileClearingContinueAction(f, c, l, then)

        case InvasiveCCCReconcileClearingFactionAction(f, c, e, then) =>
            f.log("reconciled in", c, "with", e)

            f.notify(e.drawn./(d => ViewCardInfoAction(f, f.elem ~ " got from " ~ e.elem, d)))

            e.drawn --> f.hand

            factions.foreach(x => x.drawn --> x.hand)

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(MilitantCCC(s))) {
                    f.from(c) --> MilitantCCC(s) --> f.reserve
                    f.reserve --> PeacefulCCC(s) --> c

                    game.mapping += c -> (game.original(c) ++ $(Frog))
                }
            }

            InvasiveCCCDrawCardsFromFrogDeckAction(e, f, 1, NoMessage, AddCardsAction(e, then))

        case BirdsongNAction(60, f : InvasiveCCC) =>
            InvasiveCCCReprisalsMainAction(f)

        case InvasiveCCCReprisalsMainAction(f) =>
            val l = clearings.%(f.at(_).of[MilitantCCC].any).%(game.mapping(_) != $(Frog))

            if (l.any)
                Ask(f).each(l)(c => InvasiveCCCReprisalsAction(f, c).as(c)("Reprisals in"))
            else
                Ask(f)(Next.as("Done"))

        case InvasiveCCCReprisalsAction(f, c) =>
            log("Reprisals in", c)

            InvasiveCCCReprisalsContinueAction(f, c)

        case InvasiveCCCReprisalsContinueAction(f, c) =>
            val cost = AnyOf(game.original(c))
            val l = factions.%(_.present(c)).%(_.drawn.none).%(_.hand.exists(_.matches(cost)))

            if (l.any)
                MultiAsk(l./(e =>
                    YYSelectObjectsAction(e, e.hand)
                        .withGroup(f.elem ~ " suffers reprisals in " ~ c.elem)
                        .withRule(_.matches(cost))
                        .withThen(d => InvasiveCCCReprisalsCardAction(f, c, e, d))(d => "Discard " ~ d.elem)("Discard a matching card")
                        .ask
                ))
            else
                Ask(f)(InvasiveCCCReprisalsFlipAction(f, c).as("Flip"))

        case InvasiveCCCReprisalsCardAction(f, c, e, d) =>
            e.hand --> d --> e.drawn

            InvasiveCCCReprisalsContinueAction(f, c)

        case InvasiveCCCReprisalsFlipAction(f, c) =>
            factions.%(_.drawn.any).foreach(x => x.drawn --> discard(x))

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(PeacefulCCC(s))) {
                    f.from(c) --> PeacefulCCC(s) --> f.reserve
                    f.reserve --> MilitantCCC(s) --> c

                    game.mapping += c -> (game.original(c) ++ $(Frog))
                }
            }

            game.mapping += c -> $(Frog)

            InvasiveCCCReprisalsMainAction(f)

        case EveningNAction(80, f : InvasiveCCC) =>
            soft()

            Ask(f).evening(f).done(Next)

        case DaylightNAction(20, f : InvasiveCCC) =>
            XCraftMainAction(f)

        case DaylightNAction(40, f : InvasiveCCC) if soft() =>
            val ml = clearings.%(f.at(_).of[MilitantCCC].any).%(f.rules)
            val sf = f.all(PeacefulCCC(Fox))
            val sr = f.all(PeacefulCCC(Rabbit))
            val sm = f.all(PeacefulCCC(Mouse))

            Ask(f)
                .add(sf.any.?(InvasiveCCCAngerAction(f, Fox, sf)))
                .add(sr.any.?(InvasiveCCCAngerAction(f, Rabbit, sr)))
                .add(sm.any.?(InvasiveCCCAngerAction(f, Mouse, sm)))
                .add(ml.any.?(InvasiveCCCSootheAction(f, ml)))((sf.none || sr.none || sm.none || ml.none).?(Next.as("Skip")))
                .daylight(f)

        case InvasiveCCCAngerAction(f, s, l) =>
            l.foreach { c =>
                f.from(c) --> PeacefulCCC(s) --> f.reserve
                f.reserve --> MilitantCCC(s) --> c
            }

            f.log("angered in", l./(_.elem).comma)

            Next

        case InvasiveCCCSootheAction(f, l) =>
            FoxRabbitMouse.foreach { s =>
                l.foreach { c =>
                    if (f.at(c).has(MilitantCCC(s))) {
                        f.from(c) --> MilitantCCC(s) --> f.reserve
                        f.reserve --> PeacefulCCC(s) --> c

                        game.mapping += c -> (game.original(c) ++ $(Frog))
                    }
                }
            }

            f.log("soothed in", l./(_.elem).comma)

            Next

        case DaylightNAction(60, f : InvasiveCCC) if soft() =>
            val t = f.pooled(FrogCCC)
            val r = clearings.%(f.at(_).of[MilitantCCC].any).%(f.canPlace)

            if (t == 0 || r.none)
                Ask(f)(Next.as("No Recruit"))
            else
            if (t >= r.num)
                Ask(f)(InvasiveCCCFrogRecruitAction(f, r, r))
            else
                Ask(f)
                  .each(r.combinations(t).$)(InvasiveCCCFrogRecruitAction(f, _, r))
                  .daylight(f)

        case InvasiveCCCFrogRecruitAction(f, l, r) =>
            game.highlights :+= PlaceHighlight(l.distinct)

            l.distinct.foreach { c =>
                val n = l.count(c)
                f.reserve --> n.times(FrogCCC) --> c
            }

            f.log("recruited in", l./(_.elem).comma)

            if (r.num > l.num && f.totalWar)
                f.oscore(r.num - l.num)("recruiting")

            Next

        case DaylightNAction(80, f : InvasiveCCC) =>
            implicit val ask = builder

            if (f.acted < 3) {
                val att = clearings.%(f.canAttackIn)
                + InvasiveCCCAttackAction(f, att).!(att.none)

                val mvv = f.moveFrom.of[Clearing]
                + InvasiveCCCMoveAction(f, mvv).!(mvv.none)
            }

            + EndTurnSoftAction(f, "Turn", ForfeitActions(3 - f.acted))

            ask(f).daylight(f)

        case InvasiveCCCAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), InvasiveCCCDoneAction(f, Repeat))

        case InvasiveCCCMoveAction(f, l) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), InvasiveCCCDoneAction(f, Repeat))

        case InvasiveCCCDoneAction(f, then) =>
            f.acted += 1

            then

        // EVENING
        case EveningNAction(20, f : InvasiveCCC) =>
            InvasiveCCCSettleMainAction(f)

        case InvasiveCCCSettleMainAction(f) =>
            val l = clearings.%(f.at(_).of[FrogTokenCCC].none).%(f.canPlace)
            val (r, u) = l.partition(f.rules)

            if (r.any || (u.any && f.hand.any))
                XXSelectObjectsAction(f, f.hand)
                    .withGroup("Settle".styled(f) ~ " in clearings")
                    .withRuleNone(_.upTo(1).each(d => u.exists(_.cost.matched(d.suit))))
                    .withThens(d =>
                        d./~(d => u.%(_.cost.matched(d.suit))./~(c => FoxRabbitMouse.%(f.latent(_).any).%(c.cost.matched)./(s => InvasiveCCCSettleAction(f, |(d), c, s).as(f.latent(s).starting./(_.of(f)), s, f.latent(s).starting./(_.imgd(f)), "in", c, "with", d))))
                        ++ (r./~(c => FoxRabbitMouse.%(f.latent(_).any).%(c.cost.matched)./(s => InvasiveCCCSettleAction(f, None, c, s).as(f.latent(s).starting./(_.of(f)), s, f.latent(s).starting./(_.imgd(f)), "in", c))))
                    )
                    .withExtra($(NoHand, Next.as("Done")))
                    .ask
                    .evening(f)
            else
                Ask(f)(Next.as("Done")("Settle".styled(f))).evening(f)


        case InvasiveCCCSettleAction(f, d, c, s) =>
            d.foreach(d => f.hand --> d --> discard.quiet)

            f.log("settled in", c, d.any.?("with"), d)

            f.reserve --> f.latent(s).first --> c

            f.latent += s -> f.latent(s).drop(1)

            game.mapping += c -> (game.mapping(c) ++ $(Frog))

            Repeat

        case EveningNAction(40, f : InvasiveCCC) =>
            InvasiveCCCIntegrateMainAction(f, FoxRabbitMouse)

        case InvasiveCCCIntegrateMainAction(f, l) =>
            if (l.any && f.hand.any)
                YYSelectObjectsAction(f, f.hand)
                    .withGroup("Integrate")
                    .withRule(d => l.%(s => 4 > f.pooled(PeacefulCCC(s))).exists(d.matches))
                    .withThensInfo(d => l.%(s => 4 > f.pooled(PeacefulCCC(s)))./(s => InvasiveCCCIntegrateAction(f, s, d, l.but(s)).as("Integrate", s, d.matches(s).?("with"), d.matches(s).?(d), "for", (4 - f.pooled(PeacefulCCC(s))).vp).!(d.matches(s).not)))(l.%(s => 4 > f.pooled(PeacefulCCC(s)))./(s => Info("Integrate", s, "for", (4 - f.pooled(PeacefulCCC(s))).vp)))
                    .withExtra($(NoHand, Next.as("Skip")))
                    .ask
                    .use(a => (l == FoxRabbitMouse).?(a.evening(f)).|(a))
            else
                Ask(f)(Next.as("Done"))

        case EveningNAction(60, f : InvasiveCCC) =>
            soft()

            Ask(f).evening(f).done(Next)

        case InvasiveCCCIntegrateAction(f, s, d, l) =>
            f.hand --> d --> discard.quiet

            f.nscore(4 - f.pooled(PeacefulCCC(s)))("integrating", s)(f, "integrated", s, "with", d, ForVP)

            InvasiveCCCIntegrateMainAction(f, l)

        case NightStartAction(f : InvasiveCCC) =>
            val n = 1 + FoxRabbitMouse.%(s => f.pooled(PeacefulCCC(s)) + f.pooled(MilitantCCC(s)) <= 5).num

            EveningDrawAction(f, n)

        case FactionCleanUpAction(f : InvasiveCCC) =>
            f.acted = 0

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
