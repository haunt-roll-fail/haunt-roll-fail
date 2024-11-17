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


case class PeaceSootheTalks(faction : CommonInvasive) extends CardEffect {
    override val name = "Peace Talks"
}

case class InciteConflict(faction : CommonInvasive) extends CardEffect {
    override val name = "Incite Conflict"
}

case class SuitLaborers(faction : CommonInvasive, suit : BaseSuit) extends CardEffect {
    override val name = suit.name + " Laborers"
}


trait InvasiveDDD extends WarriorFaction with CommonInvasive {
    val clashKey = LDvD

    val warrior = FrogDDD

    def abilities(options : $[Meta.O]) = $(DaylightCraft, Swimmers)

    def pieces(options : $[Meta.O]) = FrogDDD *** 20 ++ FoxRabbitMouse./~(s => PeacefulDDD(s) *** 4) ++ FoxRabbitMouse./~(s => MilitantDDD(s) *** 4)

    override val transports : $[$[Transport]] = $($(RuledMove), $(Waterway))

    override def getElem : Elem = super.getElem

    override def note : Elem = HorizontalBreak ~ "Version from " ~ "2024-10-24".hh

    def advertising = PeacefulDDD(Fox).img(this) ~ MilitantDDD(Fox).img(this) ~ PeacefulDDD(Fox).img(this) ~ MilitantDDD(Fox).img(this) ~ PeacefulDDD(Fox).img(this)

    def motto = "Infiltrate".styled(this)
}

case object FrogDDD extends Warrior with CommonFrogWarrior {
    override def id = "Frog"
    override def name = "Frog"
}

trait FrogTokenDDD extends Token with CommonFrogToken {
    val suit : BaseSuit
}

case class PeacefulDDD(suit : BaseSuit) extends FrogTokenDDD with CommonPeaceful {
    override def id = "Peaceful"
    override def imgid(f : Faction) = f.style + "-" + id
}

case class MilitantDDD(suit : BaseSuit) extends FrogTokenDDD with CommonMilitant {
    override def id = "Militant"
    override def imgid(f : Faction) = f.style + "-" + id
}



case object LDvD extends InvasiveDDD {
    val name = "Lilypad Diaspora"
    override def funName = NameReference(name, this) ~ " Diaspora"
    val short = "LDvD"
    val style = "LD"
    val priority = "T"
}


class InvasiveDDDPlayer(val faction : InvasiveDDD)(implicit val game : Game) extends FactionState {
    var acted = 0

    val deck = cards("frog-deck", Deck.frogDDD)

    var latent = FoxRabbitMouse./(s => s -> 4.times(PeacefulDDD(s) : FrogTokenDDD)).toMap

    var hostile : $[(Faction, Clearing)] = $

    var militate : $[Faction] = $

    def craft = FoxRabbitMouse./~(s => all(PeacefulDDD(s))./(_.asset)) ++ FoxRabbitMouse./~(s => all(MilitantDDD(s))./(_.asset))
}


trait InvasiveDDDDaylightQuestion extends FactionAction {
    override def self : InvasiveDDD

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            self.acted.times(Image("action-black", styles.action, "")).take(self.acted) ~
            (3 - self.acted).times(Image(self.style + "-action", styles.action)),
        styles.margined)
}

case class InvasiveDDDSetupClearingAction(self : InvasiveDDD, s : BaseSuit, c : Clearing) extends BaseAction(self, "starts in")(c, "(" ~ s.elem ~ ")")

case class InvasiveDDDAttackAction(self : InvasiveDDD, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with InvasiveDDDDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class InvasiveDDDMoveAction(self : InvasiveDDD, l : $[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with InvasiveDDDDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class InvasiveDDDRecruitAction(self : InvasiveDDD, n : Int, l : $[Clearing]) extends OptionAction("Recruit", l.num.times(self.warrior.imgd(self))) with InvasiveDDDDaylightQuestion with Soft with Only[PlacePieceClearingsAction] { def tag = implicitly }
case class InvasiveDDDDoneAction(f : InvasiveDDD, then : ForcedAction) extends ForcedAction

case class InvasiveDDDReconcileMainAction(f : InvasiveDDD) extends ForcedAction with Soft
case class InvasiveDDDReconcileClearingFactionAction(f : InvasiveDDD, c : Clearing, e : Faction) extends ForcedAction
case class InvasiveDDDReconcileCardAction(f : InvasiveDDD, c : Clearing, e : Faction, d : DeckCard) extends ForcedAction
case class InvasiveDDDReconcileRefuseAction(f : InvasiveDDD, c : Clearing, e : Faction) extends ForcedAction
case class InvasiveDDDReconcileAction(f : InvasiveDDD, c : Clearing, e : Faction) extends ForcedAction

case class InvasiveDDDReprisalsMainAction(f : InvasiveDDD) extends ForcedAction with Soft
case class InvasiveDDDReprisalsAction(f : InvasiveDDD, c : Clearing) extends ForcedAction
case class InvasiveDDDReprisalsContinueAction(f : InvasiveDDD, c : Clearing) extends ForcedAction with Soft
case class InvasiveDDDReprisalsCardAction(f : InvasiveDDD, c : Clearing, e : Faction, d : DeckCard) extends ForcedAction
case class InvasiveDDDReprisalsFlipAction(f : InvasiveDDD, c : Clearing) extends ForcedAction

case class InvasiveDDDAngerAction(self : InvasiveDDD, s : BaseSuit, l : $[Clearing]) extends BaseAction("Anger".hh)("Anger", l.comma)

case class InvasiveDDDFrogRecruitAction(self : InvasiveDDD, l : $[Clearing], r : $[Clearing]) extends BaseAction("Recruit in")(l.comma)

case class InvasiveDDDDrawCardsFromFrogDeckAction(f : Faction, e : InvasiveDDD, n : Int, m : Message, then : ForcedAction) extends ForcedAction with Soft
case class InvasiveDDDDrawFromFrogDeckAction(f : Faction, e : InvasiveDDD, l : $[DeckCard], m : Message, then : ForcedAction) extends ForcedAction


case class InvasiveDDDMilitateAction(f : Faction, e : InvasiveDDD, c : Clearing, then : ForcedAction) extends ForcedAction

case class InvasiveDDDDiscardOneFrogCardAction(f : Faction, then : ForcedAction) extends ForcedAction with Soft
case class InvasiveDDDDiscardFrogCardAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction("Discard", Frog, "card")(d.img) with ViewCard

case class InvasiveDDDSettleMainAction(f : InvasiveDDD) extends ForcedAction with Soft
case class InvasiveDDDSettleAction(f : InvasiveDDD, d : |[DeckCard], c : Clearing, s : BaseSuit) extends ForcedAction
case class InvasiveDDDSootheAction(f : InvasiveDDD, d : |[DeckCard], c : Clearing, s : BaseSuit) extends ForcedAction

case class InvasiveDDDIntegrateMainAction(f : InvasiveDDD, l : $[BaseSuit]) extends ForcedAction with Soft
case class InvasiveDDDIntegrateAction(f : InvasiveDDD, s : BaseSuit, d : DeckCard, l : $[BaseSuit]) extends ForcedAction

case class InvasiveDDDPeaceSootheTalksAction(f : Faction, e : InvasiveDDD, c : Clearing) extends ForcedAction
case class InvasiveDDDInciteConflictAction(f : Faction, e : InvasiveDDD, c : Clearing, t : Faction) extends ForcedAction



object InvasiveDDDExpansion extends FactionExpansion[InvasiveDDD] {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : InvasiveDDD) =>
            game.states += f -> new InvasiveDDDPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : InvasiveDDD) if soft() =>
            val l = FoxRabbitMouse./~(s => f.all(PeacefulDDD(s)))
            if (l.num < 3)
                Ask(f)
                    .some(clearings.diff(l).diff(game.homelands).diff(board.inner).%(f.canPlace))(c =>
                        FoxRabbitMouse.%(s => f.pooled(PeacefulDDD(s)) == 4).%(s => c.cost.matched(s))./(s => InvasiveDDDSetupClearingAction(f, s, c))
                    )
                    .needOk
                    .bail(SetupFactionsAction)
            else
                Ask(f).bail(SetupFactionsAction)

        case InvasiveDDDSetupClearingAction(f, s, c) =>
            if (game.options.has(SetupTypeHomelands)) {
                game.homelands :+= c

                f.log("started in", c)
            }

            f.reserve --> f.warrior --> c
            f.reserve --> f.warrior --> c

            val t = f.latent(s).first.get
            f.latent += s -> f.latent(s).drop(1)

            f.reserve --> t --> c

            f.log("placed", f.warrior.of(f), Comma, f.warrior.of(f), Comma, t.of(f), "in", c)

            game.mapping += c -> (game.mapping(c) ++ $(Frog))

            FactionSetupAction(f)

        case AfterSetupAction(f : InvasiveDDD, then) =>
            f.deck --> FrogDominance --> game.dominances

            f.log("made available", FrogDominance)

            then

        // HELPER
        case InvasiveDDDDrawCardsFromFrogDeckAction(f, e, n, m, then) =>
            XXSelectObjectsAction(f, Deck.frogDDD.intersect(e.deck))
                .withGroup("Draw", (n > 1).?(n.hlb), Frog, "card".s(n))
                .withRule(_.num(n))
                .withThen(l => InvasiveDDDDrawFromFrogDeckAction(f, e, l, m, then))(l => game.desc("Draw", l))

        case InvasiveDDDDrawFromFrogDeckAction(f, e, l, m, then) =>
            e.deck --> l --> f.hand

            f.log("drew", (l.num > 1).?(l.num.hl).|("a".txt), Frog, "card".s(l.num))

            f.notify(l./(d => ViewCardInfoAction(f, DrewFromTheFrogDeck(f, e, m).elem(game), d)))

            then

        case ForcedRemoveTargetEffectAction(e, c, f : InvasiveDDD, p : FrogTokenDDD, then) =>
            game.mapping += c -> game.original(c)

            f.latent += p.suit -> (p +: f.latent(p.suit))

            f.militate :+= e

            then

        case ForcedRemoveProcessAction(f : InvasiveDDD, then) =>
            if (f.militate.any) {
                val e = f.militate.first.get
                f.militate = f.militate.drop(1)

                val q = InvasiveDDDDiscardOneFrogCardAction(e, ForcedRemoveProcessAction(f, then))
                val l = clearings.%(c => f.at(c).of[PeacefulDDD].any)

                if (l.any)
                    Ask(e).each(l)(c => InvasiveDDDMilitateAction(e, f, c, q).as(c)("Militarize", f, "in"))
                else
                    q
            }
            else
                then

        case InvasiveDDDMilitateAction(e, f, c, then) =>
            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(PeacefulDDD(s))) {
                    f.from(c) --> PeacefulDDD(s) --> f.reserve
                    f.reserve --> MilitantDDD(s) --> c

                    e.log("angered", f, "in", c)
                }
            }

            then

        case InvasiveDDDDiscardOneFrogCardAction(f, then) =>
            val h = f.hand.%(d => d.matches(Frog) && d.suit != Bird)

            if (h.none)
                Ask(f).done(then)
            else
                Ask(f).each(f.hand)(d => InvasiveDDDDiscardFrogCardAction(f, d, then).!(h.has(d).not))(NoHand).needOk

        case InvasiveDDDDiscardFrogCardAction(f, d, then) =>
            f.hand --> d --> discard(f)

            then

        case BattlePostHitInAction(b, e, f : InvasiveDDD, FrogDDD, then) =>
            e.log("blew up", FrogDDD.of(f))

            then

        case BattlePostHitInAction(b, e, f : InvasiveDDD, p : PeacefulDDD, then) =>
            e.log("displaced", p.of(f))

            then

        case BattlePostHitInAction(b, e, f : InvasiveDDD, p : MilitantDDD, then) =>
            e.log("dispersed", p.of(f))

            then

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, InciteConflict(e : InvasiveDDD)), m) =>
            f.hand --> d --> discard.quiet

            val ee = e.enemies
            val l = clearings.%(e.at(_).of[Warrior].any).%(c => ee.exists(_.present(c)))

            if (l.any) {
                f.log("got to", d, "(" ~ d.cost.ss ~ ")", m)

                Ask(f).some(l)(c => ee.%(_.present(c))./(t => InvasiveDDDInciteConflictAction(f, e, c, t).as(t, "in", c)(e, "battles"))).needOk
            }
            else {
                f.log("got to", d, "(" ~ d.cost.ss ~ ")", m, "with no effect")

                Repeat
            }

        case InvasiveDDDInciteConflictAction(f, e, c, t) =>
            FoxRabbitMouse.foreach { s =>
                if (e.at(c).has(PeacefulDDD(s))) {
                    e.from(c) --> PeacefulDDD(s) --> e.reserve
                    e.reserve --> MilitantDDD(s) --> c

                    f.log("angered", e, "in", c)
                }
            }

            Force(BattleStartAction(e, e, e, WithEffect(InciteConflict(e)), c, t, None, Repeat))

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, PeaceSootheTalks(e : InvasiveDDD)), m) =>
            f.hand --> d --> discard.quiet

            val l = clearings.%(c => e.at(c).use(l => l.of[Warrior].any))

            if (l.any) {
                f.log("has started", d, "(" ~ d.cost.ss ~ ")", m)

                Ask(f).each(l)(c => InvasiveDDDPeaceSootheTalksAction(f, e, c).as(c)("Peace Talks".hl, "in")).needOk
            }
            else {
                f.log("has started", d, "(" ~ d.cost.ss ~ ")", m, "with no effect")

                Repeat
            }

        case InvasiveDDDPeaceSootheTalksAction(f, e, c) =>
            FoxRabbitMouse.foreach { s =>
                if (e.at(c).has(MilitantDDD(s))) {
                    e.from(c) --> MilitantDDD(s) --> e.reserve
                    e.reserve --> PeacefulDDD(s) --> c

                    game.mapping += c -> (game.original(c) ++ $(Frog))

                    f.log("soothed", e, "in", c)
                }
            }

            NukeAction(f, $(e), $(c), NukeType.AntiPersonnel, Repeat)

        case BattleStartedAction(b) if b.defender.as[InvasiveDDD].?(_.at(b.clearing).of[PeacefulDDD].any) =>
            val c = b.clearing
            val f = b.defender

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(PeacefulDDD(s))) {
                    f.from(c) --> PeacefulDDD(s) --> f.reserve
                    f.reserve --> MilitantDDD(s) --> c

                    f.log("became", MilitantDDD(s).of(f), "in", c)
                }
            }

            BattleStartedAction(b)

        // TURN
        case BirdsongNAction(30, f : InvasiveDDD) =>
            InvasiveDDDReconcileMainAction(f)

        case InvasiveDDDReconcileMainAction(f) =>
            val l = clearings.%(f.at(_).of[MilitantDDD].any)
            val ee = f.enemies
            val g = "Reconcile".hh

            Ask(f)
                .some(l)(c =>
                    ee.%(_.present(c))./(e =>
                        InvasiveDDDReconcileClearingFactionAction(f, c, e).as("In", c, "with", e)(g)
                            .!(f.hostile.has((e, c)), "refused")
                            .!(e.hand.none, "no cards")
                    ).some.|(
                        $(Info(c)(g))
                    )
                )
                .add(Next.as("Done")(g))
                .birdsong(f)

        case InvasiveDDDReconcileClearingFactionAction(f, c, e) =>
            f.logtemp("offered reconciliation in", c, "to", e)

            YYSelectObjectsAction(e, e.hand)
                .withGroup(f.elem ~ " offers reconciliation in " ~ c.elem)
                .withThen(d => InvasiveDDDReconcileCardAction(f, c, e, d))(d => "Reconcile with " ~ d.elem)("Reconcile")
                .withExtras(InvasiveDDDReconcileRefuseAction(f, c, e).as("Refuse"), NoHand)
                .ask

        case InvasiveDDDReconcileRefuseAction(f, c, e) =>
            e.log("refused to reconcile in", c, "with", f)

            f.hostile :+= (e, c)

            Repeat

        case InvasiveDDDReconcileCardAction(f, c, e, d) =>
            e.hand --> d --> f.hand

            e.log("gave a card and reconciled in", c, "with", f)

            f.notify($(ViewCardInfoAction(f, f.elem ~ " got from " ~ e.elem, d)))

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(MilitantDDD(s))) {
                    f.from(c) --> MilitantDDD(s) --> f.reserve
                    f.reserve --> PeacefulDDD(s) --> c

                    game.mapping += c -> (game.original(c) ++ $(Frog))
                }
            }

            if (f.deck.any)
                InvasiveDDDDrawCardsFromFrogDeckAction(e, f, 1, NoMessage, AddCardsAction(e, Repeat))
            else {
                f.log("deck was empty")

                Repeat
            }

        case BirdsongNAction(60, f : InvasiveDDD) =>
            InvasiveDDDReprisalsMainAction(f)

        case InvasiveDDDReprisalsMainAction(f) =>
            val l = clearings.%(f.at(_).of[MilitantDDD].any).%(game.mapping(_) != $(Frog))

            if (l.any)
                Ask(f).each(l)(c => InvasiveDDDReprisalsAction(f, c).as(c)("Reprisals in"))
            else
                Ask(f)(Next.as("Done"))

        case InvasiveDDDReprisalsAction(f, c) =>
            log("Reprisals in", c)

            InvasiveDDDReprisalsContinueAction(f, c)

        case InvasiveDDDReprisalsContinueAction(f, c) =>
            val cost = AnyOf(game.original(c))
            val l = factions.%(_.present(c)).%(_.drawn.none).%(_.hand.exists(_.matches(cost)))

            if (l.any)
                MultiAsk(l./(e =>
                    YYSelectObjectsAction(e, e.hand)
                        .withGroup(e.elem ~ " suffers reprisals in " ~ c.elem)
                        .withRule(_.matches(cost))
                        .withThen(d => InvasiveDDDReprisalsCardAction(f, c, e, d))(d => "Discard " ~ d.elem)("Discard a matching card")
                        .ask
                ))
            else
                Ask(f)(InvasiveDDDReprisalsFlipAction(f, c).as("Flip"))

        case InvasiveDDDReprisalsCardAction(f, c, e, d) =>
            e.hand --> d --> e.drawn

            InvasiveDDDReprisalsContinueAction(f, c)

        case InvasiveDDDReprisalsFlipAction(f, c) =>
            factions.%(_.drawn.any).foreach(x => x.drawn --> discard(x))

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(PeacefulDDD(s))) {
                    f.from(c) --> PeacefulDDD(s) --> f.reserve
                    f.reserve --> MilitantDDD(s) --> c

                    game.mapping += c -> (game.original(c) ++ $(Frog))
                }
            }

            game.mapping += c -> $(Frog)

            InvasiveDDDReprisalsMainAction(f)

        case EveningNAction(80, f : InvasiveDDD) =>
            soft()

            Ask(f).evening(f).done(Next)

        case DaylightNAction(20, f : InvasiveDDD) =>
            XCraftMainAction(f)

        case DaylightNAction(40, f : InvasiveDDD) if soft() =>
            val sf = f.all(PeacefulDDD(Fox))
            val sr = f.all(PeacefulDDD(Rabbit))
            val sm = f.all(PeacefulDDD(Mouse))

            Ask(f)
                .add(sf.any.?(InvasiveDDDAngerAction(f, Fox, sf)))
                .add(sr.any.?(InvasiveDDDAngerAction(f, Rabbit, sr)))
                .add(sm.any.?(InvasiveDDDAngerAction(f, Mouse, sm)))
                .skip(Next)
                .daylight(f)

        case InvasiveDDDAngerAction(f, s, l) =>
            l.foreach { c =>
                f.from(c) --> PeacefulDDD(s) --> f.reserve
                f.reserve --> MilitantDDD(s) --> c
            }

            f.latent += s -> f.latent(s)./(_ => MilitantDDD(s))

            f.log("angered", s, "in", l./(_.elem).comma)

            Next

        case DaylightNAction(60, f : InvasiveDDD) if soft() =>
            val t = f.pooled(FrogDDD)
            val r = clearings.%(f.at(_).of[MilitantDDD].any).%(f.canPlace)

            if (t == 0 || r.none)
                Ask(f)(Next.as("No Recruit"))
            else
            if (t >= r.num)
                Ask(f)(InvasiveDDDFrogRecruitAction(f, r, r))
            else
                Ask(f)
                  .each(r.combinations(t).$)(InvasiveDDDFrogRecruitAction(f, _, r))
                  .daylight(f)

        case InvasiveDDDFrogRecruitAction(f, l, r) =>
            game.highlights :+= PlaceHighlight(l.distinct)

            l.distinct.foreach { c =>
                val n = l.count(c)
                f.reserve --> n.times(FrogDDD) --> c
            }

            f.log("recruited in", l./(_.elem).comma)

            if (r.num > l.num && f.totalWar)
                f.oscore(r.num - l.num)("recruiting")

            Next

        case DaylightNAction(80, f : InvasiveDDD) =>
            implicit val ask = builder

            if (f.acted < 3) {
                val att = clearings.%(f.canAttackIn)
                + InvasiveDDDAttackAction(f, att).!(att.none)

                val mvv = f.moveFrom.of[Clearing]
                + InvasiveDDDMoveAction(f, mvv).!(mvv.none)
            }

            + EndTurnSoftAction(f, "Turn", ForfeitActions(3 - f.acted))

            ask(f).daylight(f)

        case InvasiveDDDAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), InvasiveDDDDoneAction(f, Repeat))

        case InvasiveDDDMoveAction(f, l) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), InvasiveDDDDoneAction(f, Repeat))

        case InvasiveDDDDoneAction(f, then) =>
            f.acted += 1

            then

        // EVENING
        case EveningNAction(20, f : InvasiveDDD) =>
            InvasiveDDDSettleMainAction(f)

        case InvasiveDDDSettleMainAction(f) =>
            val lm = clearings.%(f.at(_).of[MilitantDDD].any)
            val ln = clearings.%(f.at(_).of[FrogTokenDDD].none).%(f.canPlace)

            val (rm, um) = lm.partition(f.rules)
            val (rn, un) = ln.partition(f.rules)

            if (rm.any || rn.any || ((um.any || un.any) && f.hand.any))
                XXSelectObjectsAction(f, f.hand)
                    .withGroup("Settle".styled(f) ~ " or " ~ "Soothe".styled(f) ~ " in clearings")
                    .withRuleNone(_.upTo(1).each(d => (um ++ un).exists(_.cost.matched(d.suit))))
                    .withThens(d =>
                        d./~(d => um.%(_.cost.matched(d.suit))./~(c => FoxRabbitMouse.%(s => f.at(c).has(MilitantDDD(s)))./(s => InvasiveDDDSootheAction(f, |(d), c, s).as("Soothe", MilitantDDD(s).of(f), s, MilitantDDD(s).imgd(f), "in", c, "with", d))))
                        ++
                        (rm./~(c => FoxRabbitMouse.%(s => f.at(c).has(MilitantDDD(s)))./(s => InvasiveDDDSootheAction(f, None, c, s).as("Soothe", MilitantDDD(s).of(f), s, MilitantDDD(s).imgd(f), "in", c))))
                        ++
                        d./~(d => un.%(_.cost.matched(d.suit))./~(c => FoxRabbitMouse.%(f.latent(_).any).%(c.cost.matched)./(s => InvasiveDDDSettleAction(f, |(d), c, s).as("Settle", f.latent(s).first./(_.of(f)), s, f.latent(s).first./(_.imgd(f)), "in", c, "with", d))))
                        ++
                        (rn./~(c => FoxRabbitMouse.%(f.latent(_).any).%(c.cost.matched)./(s => InvasiveDDDSettleAction(f, None, c, s).as("Settle", f.latent(s).first./(_.of(f)), s, f.latent(s).first./(_.imgd(f)), "in", c))))
                    )
                    .withExtra($(NoHand, Next.as("Done")))
                    .ask
                    .evening(f)
            else
                Ask(f)(Next.as("Done")("Settle".styled(f))).evening(f)

        case InvasiveDDDSootheAction(f, d, c, s) =>
            d.foreach(d => f.hand --> d --> discard.quiet)

            f.log("soothed in", c, d.any.?("with"), d)

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(MilitantDDD(s))) {
                    f.from(c) --> MilitantDDD(s) --> f.reserve
                    f.reserve --> PeacefulDDD(s) --> c

                    game.mapping += c -> (game.original(c) ++ $(Frog))
                }
            }

            Repeat

        case InvasiveDDDSettleAction(f, d, c, s) =>
            d.foreach(d => f.hand --> d --> discard.quiet)

            f.log("settled in", c, d.any.?("with"), d)

            f.reserve --> f.latent(s).first.get --> c

            f.latent += s -> f.latent(s).drop(1)

            game.mapping += c -> (game.mapping(c) ++ $(Frog))

            Repeat

        case EveningNAction(40, f : InvasiveDDD) =>
            InvasiveDDDIntegrateMainAction(f, FoxRabbitMouse)

        case InvasiveDDDIntegrateMainAction(f, l) =>
            if (l.any && f.hand.any)
                YYSelectObjectsAction(f, f.hand)
                    .withGroup("Integrate")
                    .withRule(d => l.%(s => 4 > f.pooled(PeacefulDDD(s))).exists(d.matches))
                    .withThensInfo(d => l.%(s => 4 > f.pooled(PeacefulDDD(s)))./(s => InvasiveDDDIntegrateAction(f, s, d, l.but(s)).as("Integrate", s, d.matches(s).?("with"), d.matches(s).?(d), "for", (4 - f.pooled(PeacefulDDD(s))).vp).!(d.matches(s).not)))(l.%(s => 4 > f.pooled(PeacefulDDD(s)))./(s => Info("Integrate", s, "for", (4 - f.pooled(PeacefulDDD(s))).vp)))
                    .withExtra($(NoHand, Next.as("Done")))
                    .ask
                    .use(a => (l == FoxRabbitMouse).?(a.evening(f)).|(a))
            else
                Ask(f)(Next.as("Done"))

        case EveningNAction(60, f : InvasiveDDD) =>
            soft()

            Ask(f).evening(f).done(Next)

        case InvasiveDDDIntegrateAction(f, s, d, l) =>
            f.hand --> d --> discard.quiet

            f.nscore(4 - f.pooled(PeacefulDDD(s)))("integrating", s)(f, "integrated", s, "with", d, ForVP)

            InvasiveDDDIntegrateMainAction(f, l)

        case NightStartAction(f : InvasiveDDD) =>
            val n = 1 + FoxRabbitMouse.%(s => f.pooled(PeacefulDDD(s)) + f.pooled(MilitantDDD(s)) <= 5).num

            EveningDrawAction(f, n)

        case FactionCleanUpAction(f : InvasiveDDD) =>
            f.acted = 0

            f.hostile = $

            f.militate = $

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
