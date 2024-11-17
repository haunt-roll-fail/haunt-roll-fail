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

case object Frog extends BaseSuit {
    override def name = "Frog"
}

trait CommonInvasive extends WarriorFaction
trait CommonFrogWarrior { self : Warrior => }
trait CommonFrogToken { self : Token => }
trait CommonMilitant extends CommonFrogToken { self : Token => }
trait CommonPeaceful extends CommonFrogToken { self : Token => }

case class DrewFromTheFrogDeck(f : Faction, e : CommonInvasive, m : Message with GameElementary) extends Message {
    def elem(implicit game : Game) = f.elem ~ " drew from the " ~ Frog.elem ~ " deck" ~ m.but(NoMessage)./(" " ~ _.elem)
}

trait TillEndOfTurn { self : CardEffect => }





case object FrogEngineers extends CardEffect with TillEndOfTurn {
    override val name = "Frog Engineers"
}

trait InvasiveAAA extends WarriorFaction with CommonInvasive {
    val clashKey = TD

    val warrior = FrogAAA

    def abilities(options : $[Meta.O]) = $(DaylightCraft, Swimmers)

    def pieces(options : $[Meta.O]) = FrogAAA *** 15 ++ FoxRabbitMouse./~(s => PeacefulAAA(s) *** 4) ++ FoxRabbitMouse./~(s => MilitantAAA(s) *** 4)

    override def getElem : Elem = super.getElem ~ " " ~ "α".hh

    override def note : Elem = HorizontalBreak ~ "Version from " ~ "2024-08-03".hh

    def advertising = PeacefulAAA(Fox).img(this) ~ MilitantAAA(Fox).img(this) ~ PeacefulAAA(Fox).img(this) ~ MilitantAAA(Fox).img(this) ~ PeacefulAAA(Fox).img(this)

    def motto = "Infiltrate".styled(this)
}

case object FrogAAA extends Warrior with CommonFrogWarrior {
    override def id = "Frog"
    override def name = "Frog"
}

trait FrogTokenAAA extends Token with CommonFrogToken {
    val suit : BaseSuit
}

case class PeacefulAAA(suit : BaseSuit) extends FrogTokenAAA with CommonPeaceful {
    override def id = "Peaceful"
    override def imgid(f : Faction) = f.style + "-" + id
}

case class MilitantAAA(suit : BaseSuit) extends FrogTokenAAA with CommonMilitant {
    override def id = "Militant"
    override def imgid(f : Faction) = f.style + "-" + id
}

case object TD extends InvasiveAAA {
    val name = "Tidepool Diaspora"
    override def funName = NameReference(name, this) ~ " Diaspora"
    val short = "TD"
    val style = "TD"
    val priority = "T"
}


trait TDDaylightQuestion extends FactionAction {
    override def self : InvasiveAAA

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            self.acted.times(Image("action-black", styles.action, "")).take(self.acted) ~
            (3 - self.acted).times(Image(self.style + "-action", styles.action)) ~
            (self.extra - max(0, self.acted - 3)).times(Image("action-bird", styles.action)),
        styles.margined)
}

class InvasiveAAAPlayer(val faction : InvasiveAAA)(implicit val game : Game) extends FactionState {
    var acted = 0
    var extra = 0

    val pile = cards("frog-pile", Deck.frogAAA)
    val deck = cards("frog-deck")

    def craft = FoxRabbitMouse./~(s => all(PeacefulAAA(s))./(_.asset)) ++ can(FrogEngineers).??(FoxRabbitMouse./~(s => all(MilitantAAA(s))./(_.asset)))
}



case class InvasiveAAASetupClearingAction(self : InvasiveAAA, s : BaseSuit, c : Clearing) extends BaseAction(self, "starts in")(c, "(" ~ s.elem ~ ")")

case class InvasiveAAAAttackAction(self : InvasiveAAA, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with TDDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class InvasiveAAAMoveAction(self : InvasiveAAA, l : $[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with TDDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class InvasiveAAARecruitAction(self : InvasiveAAA, n : Int, l : $[Clearing]) extends OptionAction("Recruit", l.num.times(self.warrior.imgd(self))) with TDDaylightQuestion with Soft with Only[PlacePieceClearingsAction] { def tag = implicitly }
case class InvasiveAAADoneAction(f : InvasiveAAA, then : ForcedAction) extends ForcedAction

case class InvasiveAAAIntoleranceRolledAction(self : InvasiveAAA, s : BaseSuit, then : ForcedAction) extends RolledAction[BaseSuit] {
    def rolled = $(s)
}
case class InvasiveAAAIntoleranceAction(self : InvasiveAAA, s : BaseSuit, c : Clearing, then : ForcedAction) extends ForcedAction

case class InvasiveAAAShuffleFrogPileAction(f : InvasiveAAA, shuffled : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]
case class InvasiveAAADrawCardsFromFrogDeckAction(f : Faction, e : InvasiveAAA, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class InvasiveAAADiscardAllFrogCards(f : Faction, then : ForcedAction) extends ForcedAction

case class InvasiveAAARadicalizeMainAction(self : InvasiveAAA, all : $[Clearing], l : $[Clearing]) extends ForcedAction with Soft
case class InvasiveAAARadicalizeAction(self : InvasiveAAA, c : Clearing, then : ForcedAction) extends BaseAction("Radicalize and recruit in")(c)

case class InvasiveAAAExtraFrogMainAction(self : InvasiveAAA, n : Int) extends OptionAction("Extra action".hl, n.times(dt.CardSuit(Frog))) with TDDaylightQuestion with Soft
case class InvasiveAAAExtraFrogAction(f : InvasiveAAA) extends ForcedAction

case class InvasiveAAASettleMainAction(self : InvasiveAAA) extends ForcedAction with Soft
case class InvasiveAAASettleAction(f : InvasiveAAA, d : DeckCard, s : BaseSuit, l : $[Clearing]) extends ForcedAction

case class InvasiveAAAReconcileMainAction(f : Faction, e : InvasiveAAA, then : ForcedAction) extends ForcedAction with Soft
case class InvasiveAAAReconcileAction(f : Faction, e : InvasiveAAA, d : DeckCard, c : Clearing, then : ForcedAction) extends ForcedAction

case class InvasiveAAAReprisalsAction(f : Faction, l : $[Clearing], then : ForcedAction) extends ForcedAction
case class InvasiveAAAReprisalsFlipAction(f : Faction, l : $[Clearing]) extends ForcedAction



object InvasiveAAAExpansion extends FactionExpansion[InvasiveAAA] {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : InvasiveAAA) =>
            game.states += f -> new InvasiveAAAPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : InvasiveAAA) =>
            Ask(f).some(clearings.diff(game.homelands).diff(board.inner))(c => FoxRabbitMouse.%(s => f.pooled(PeacefulAAA(s)) == 4).%(s => c.cost.matched(s))./(s => InvasiveAAASetupClearingAction(f, s, c))).needOk.bail(SetupFactionsAction)

        case InvasiveAAASetupClearingAction(f, s, c) =>
            f.reserve --> f.warrior --> c
            f.reserve --> f.warrior --> c
            f.reserve --> PeacefulAAA(s) --> c

            f.log("placed", f.warrior.of(f), Comma, f.warrior.of(f), Comma, PeacefulAAA(s).of(f), "in", c)

            game.mapping += c -> (game.mapping(c) ++ $(Frog))

            FactionSetupAction(f)

        case AfterSetupAction(f : InvasiveAAA, then) =>
            factions.foldRight(then)((e, q) => InvasiveAAADrawCardsFromFrogDeckAction(e, f, 1, NoMessage, AddCardsAction(e, q)))

        // HELPER
        case InvasiveAAAShuffleFrogPileAction(f, l, then) =>
            f.pile --> l --> f.deck

            log("The", Frog.elem, "deck was shuffled")

            Milestone(then)

        case InvasiveAAADrawCardsFromFrogDeckAction(f, e, n, m, then) =>
            if (e.deck.num < n && e.pile.any)
                Shuffle[DeckCard](e.pile, InvasiveAAAShuffleFrogPileAction(e, _, InvasiveAAADrawCardsFromFrogDeckAction(f, e, n, m, then)))
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

        case ForcedRemoveTargetEffectAction(e, c, f : InvasiveAAA, p : FrogTokenAAA, then) =>
            game.mapping += c -> game.original(c)

            InvasiveAAADiscardAllFrogCards(e, then)

        case InvasiveAAADiscardAllFrogCards(f, then) =>
            val l = f.hand.%(d => d.matches(Frog) && d.suit != Bird)

            if (l.any)
                f.hand --> l --> discard(f)

            then


        case BattlePostHitInAction(b, e, f : InvasiveAAA, FrogAAA, then) =>
            e.log("blew up", FrogAAA.of(f))

            then

        case BattlePostHitInAction(b, e, f : InvasiveAAA, p : PeacefulAAA, then) =>
            e.log("displaced", p.of(f))

            then

        case BattlePostHitInAction(b, e, f : InvasiveAAA, p : MilitantAAA, then) =>
            e.log("dispersed", p.of(f))

            then

        // TURN
        case BirdsongNAction(30, f : InvasiveAAA) =>
            Roll[BaseSuit]($(BaseSuitDie), l => InvasiveAAAIntoleranceRolledAction(f, l(0), Next), f.elem ~ " rolls intolerance die")

        case InvasiveAAAIntoleranceRolledAction(f, s, then) =>
            val l = f.all(PeacefulAAA(s))

            if (l.any) {
                f.logtemp("rolled", s)

                Ask(f).each(l)(c => InvasiveAAAIntoleranceAction(f, s, c, then).as(c)("Intolerance spreads to")).needOk
            }
            else {
                f.log("rolled", s.elem ~ ", no matching tokens")

                then
            }

        case InvasiveAAAIntoleranceAction(f, s, c, then) =>
            f.log("rolled", s.elem ~ ", intolerance spread to", c)

            f.from(c) --> PeacefulAAA(s) --> f.reserve
            f.reserve --> MilitantAAA(s) --> c

            then

        case DaylightNAction(20, f : InvasiveAAA) =>
            XCraftMainAction(f)

        case DaylightNAction(40, f : InvasiveAAA) =>
            val l = clearings.%(f.at(_).of[FrogTokenAAA].any)

            InvasiveAAARadicalizeMainAction(f, l, l)

        case InvasiveAAARadicalizeMainAction(f, all, l) =>
            Ask(f).each(all)(c => InvasiveAAARadicalizeAction(f, c, InvasiveAAARadicalizeMainAction(f, all, l.but(c))).!(l.has(c).not).!(f.pool(f.warrior).not)).done(Next).daylight(f)

        case InvasiveAAARadicalizeAction(f, c, then) =>
            var flip = false

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(PeacefulAAA(s))) {
                    f.from(c) --> PeacefulAAA(s) --> f.reserve
                    f.reserve --> MilitantAAA(s) --> c
                    flip = true
                }
            }

            f.reserve --> f.warrior --> c

            f.log(flip.?("radicalized and"), "recruited in", c)

            then

        case DaylightNAction(60, f : InvasiveAAA) =>
            implicit val ask = builder

            if (f.acted < 3 + f.extra) {
                val att = clearings.%(f.canAttackIn)
                + InvasiveAAAAttackAction(f, att).!(att.none)

                val mvv = f.moveFrom.of[Clearing]
                + InvasiveAAAMoveAction(f, mvv).!(mvv.none)
            }

            + InvasiveAAAExtraFrogMainAction(f, f.hand.%(_.suit.matches(Frog)).num)
                .!(f.hand.%(_.suit.matches(Frog)).none, "no frog cards")

            + EndTurnSoftAction(f, "Turn", ForfeitActions(3 + f.extra - f.acted))

            ask(f).daylight(f)

        case InvasiveAAAExtraFrogMainAction(f, _) =>
            OptionalDiscardCardAction(f, GetExtra, Frog, InvasiveAAAExtraFrogAction(f))

        case InvasiveAAAExtraFrogAction(f) =>
            f.log("got an", "extra action".hl, "with", f.drawn.get)

            f.drawn --> discard.quiet

            f.extra += 1

            Repeat

        case InvasiveAAAAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), InvasiveAAADoneAction(f, Repeat))

        case InvasiveAAAMoveAction(f, l) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), InvasiveAAADoneAction(f, Repeat))

        case InvasiveAAADoneAction(f, then) =>
            f.acted += 1

            then

        case DaylightNAction(80, f : InvasiveAAA) =>
            InvasiveAAASettleMainAction(f)

        case InvasiveAAASettleMainAction(f) =>
            val l = clearings.%(f.at(_).of[FrogTokenAAA].none).%(f.rules)

            if (l.any && f.hand.any)
                YYSelectObjectsAction(f, f.hand)
                    .withGroup("Settle".styled(f) ~ " in ruled clearings")
                    .withRule(d => l.exists(_.cost.matched(d.suit)))
                    .withThens(d => FoxRabbitMouse.%(d.suit.matches)./~{ s =>
                        val cc = l.%(_.cost.matched(s))
                        1.to(min(cc.num, f.pooled(PeacefulAAA(s)) + f.pooled(MilitantAAA(s)) - 4)).reverse./~(cc.combinations)./(x => InvasiveAAASettleAction(f, d, s, x).as(x.comma, "(" ~ s.elem ~ ")"))
                     })
                    .withExtra($(NoHand, Next.as("Skip")))
                    .ask
                    .daylight(f)

            else
                Ask(f)(Next.as("Skip")("Settle".styled(f) ~ " in ruled clearings")).daylight(f)


        case InvasiveAAASettleAction(f, d, s, l) =>
            f.hand --> d --> discard.quiet

            f.log("settled with", d)

            l.foreach { c =>
                val n = 8 - f.pooled(PeacefulAAA(s)) - f.pooled(MilitantAAA(s))

                f.reserve --> PeacefulAAA(s) --> c

                f.nscore(n)("settling in", s)(f, "were", PeacefulAAA(s).of(f), "in", c, ForVP)

                game.mapping += c -> (game.mapping(c) ++ $(Frog))
            }

            Repeat

        // EVENING
        case EveningNAction(20, f : Fanatic) =>
            Ask(f).evening(f).done(Next)

        case EveningNAction(30, f : InvasiveAAA) =>
            val l = clearings.%(f.at(_).of[MilitantAAA].any)

            if (l.any)
                factions.but(f).foldRight(Next : ForcedAction)((e, q) => InvasiveAAAReconcileMainAction(e, f, q))
            else
                Next

        case InvasiveAAAReconcileMainAction(f, e, then) =>
            val l = clearings.%(e.at(_).of[MilitantAAA].any)

            if (l.any && f.hand.any)
                YYSelectObjectsAction(f, f.hand)
                    .withGroup("Reconcile with " ~ e.elem ~ " and draw a " ~ Frog.elem ~ " card")
                    .withThens(d => l./(c => InvasiveAAAReconcileAction(f, e, d, c, then).as("Reconcile in", c)))
                    .withExtra($(then.as("Skip")))
            else
                Ask(f)(then.as("Skip"))

        case InvasiveAAAReconcileAction(f, e, d, c, then) =>
            FoxRabbitMouse.foreach { s =>
                if (e.at(c).has(MilitantAAA(s))) {
                    e.from(c) --> MilitantAAA(s) --> e.reserve
                    e.reserve --> PeacefulAAA(s) --> c

                    game.mapping += c -> (game.original(c) ++ $(Frog))
                }
            }

            f.hand --> d --> e.hand

            e.notify($(ViewCardInfoAction(e, Gave(f, e).elem(game), d)))

            f.log("gave a card and reconciled with", e, "in", c)

            InvasiveAAADrawCardsFromFrogDeckAction(f, e, 1, NoMessage, AddCardsAction(f, then))

        case EveningNAction(40, f : InvasiveAAA) =>
            val l = clearings.%(f.at(_).of[MilitantAAA].any).%(game.mapping(_) != $(Frog))

            if (l.any) {
                log("Reprisals in", l./(_.elem(game)).comma)

                factions.foldRight(InvasiveAAAReprisalsFlipAction(f, l) : ForcedAction)((e, q) => InvasiveAAAReprisalsAction(e, l, q))
            }
            else
                Next

        case InvasiveAAAReprisalsAction(f, l, then) =>
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

        case InvasiveAAAReprisalsFlipAction(f, l) =>
            l.foreach { c =>
                game.mapping += c -> $(Frog)
            }

            Next

        case EveningNAction(60, f : InvasiveAAA) =>
            val n = FoxRabbitMouse./(s => f.pooled(PeacefulAAA(s))).sum - FoxRabbitMouse./(s => f.pooled(MilitantAAA(s))).sum

            if (n > 0)
                f.oscore(-n)("from reprisals")

            Next

        case NightStartAction(f : InvasiveAAA) =>
            val n = 1 + FoxRabbitMouse.%(s => f.pooled(PeacefulAAA(s)) + f.pooled(MilitantAAA(s)) <= 5).num

            Ask(f).each(0.to(n).$)(k => InvasiveAAADrawCardsFromFrogDeckAction(f, f, n - k, NoMessage, AddCardsAction(f, EveningDrawAction(f, k))).as((n - k).times(dt.CardBackFrog).merge ~ k.times(dt.CardBack).merge)("Draw Cards"))

        case FactionCleanUpAction(f : InvasiveAAA) =>
            f.acted = 0

            f.extra = 0

            CleanUpAction(f)

        // REFUGE
        case CraftEffectAction(f, d @ CraftEffectCard(_, _, _, FrogEngineers), m, then) =>
            if (f.is[InvasiveAAA]) {
                f.log("deployed", d, "(" ~ d.cost.ss ~ ")", m)
            }
            else {
                f.log("hired", d, "(" ~ d.cost.ss ~ ")", m)

                factions.of[InvasiveAAA].foreach { e =>
                    f.extraCraft ++= FoxRabbitMouse./~(s => e.all(PeacefulAAA(s))./(_.asset)) ++ FoxRabbitMouse./~(s => e.all(MilitantAAA(s))./(_.asset))
                }
            }

            then

        case BattleCleanupDefenderAction(b, f : InvasiveAAA) if f.at(b.clearing).of[PeacefulAAA].any =>
            val c = b.clearing

            FoxRabbitMouse.foreach { s =>
                if (f.at(c).has(PeacefulAAA(s))) {
                    f.from(c) --> PeacefulAAA(s) --> f.reserve
                    f.reserve --> MilitantAAA(s) --> c

                    f.log("became", MilitantAAA(s).of(f), "in", c)
                }
            }

            BattleCleanupDefenderAction(b, f)

        case _ => UnknownContinue
    }

}
