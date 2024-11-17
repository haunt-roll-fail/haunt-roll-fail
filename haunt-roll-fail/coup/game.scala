package coup
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

import hrf.tracker._

import hrf.elem._
import coup.elem._

trait Role extends Record with Named with Styling {
    def name = toString
}
case object Duke extends Role
case object Captain extends Role
case object Assassin extends Role
case object Contessa extends Role
case object Ambassador extends Role

case object Coup extends Role


case object HiddenCard {
    def img = Image("hidden", styles.card)
}

case class Card(role : Role, text : String, image : String) extends Record with Elementary {
    def elem = text.styled(role)
    def img = Image(image, styles.card)
}

case object Token {
    def img = Image("token", styles.token)
}

case object HiddenToken {
    def img = Image("token", styles.token, xlo.hidden)
}


object Deck {
    val normal =
        Card(Duke, "Duke", "duke") *** 3 ++
        Card(Captain, "Captain", "captain") *** 3 ++
        Card(Assassin, "Assassin", "assassin") *** 3 ++
        Card(Contessa, "Contessa", "contessa") *** 3 ++
        Card(Ambassador, "Ambassador", "ambassador") *** 3

    val alt =
        Card(Duke, "Duke", "duke") :: Card(Duke, "Duke", "duke2") :: Card(Duke, "Duke", "duke3") ::
        Card(Captain, "Captain", "captain") :: Card(Captain, "Captain", "captain2") :: Card(Captain, "Captain", "captain3") ::
        Card(Assassin, "Assassin", "assassin") :: Card(Assassin, "Assassin", "assassin2") :: Card(Assassin, "Assassin", "assassin3") ::
        Card(Contessa, "Contessa", "contessa") :: Card(Contessa, "Contessa", "contessa2") :: Card(Contessa, "Contessa", "contessa3") ::
        Card(Ambassador, "Ambassador", "ambassador") :: Card(Ambassador, "Ambassador", "ambassador2") :: Card(Ambassador, "Ambassador", "ambassador3")

    val full =
        Card(Duke, "Duke", "duke") ::
        Card(Duke, "Duke", "duke2") ::
        Card(Duke, "Duke", "duke3") ::
        Card(Duke, "Duke", "duke4") ::
        Card(Duke, "Duke", "duke5") ::
        Card(Duke, "Duke", "duke6") ::
        Card(Captain, "Captain", "captain") ::
        Card(Captain, "Captain", "captain2") ::
        Card(Captain, "Captain", "captain3") ::
        Card(Captain, "Captain", "captain4") ::
        Card(Captain, "Captain", "captain5") ::
        Card(Captain, "Captain", "captain6")
        Card(Assassin, "Assassin", "assassin") ::
        Card(Assassin, "Assassin", "assassin2") ::
        Card(Assassin, "Assassin", "assassin3") ::
        Card(Assassin, "Assassin", "assassin4") ::
        Card(Assassin, "Assassin", "assassin5") ::
        Card(Assassin, "Assassin", "assassin6") ::
        Card(Contessa, "Contessa", "contessa") ::
        Card(Contessa, "Contessa", "contessa2") ::
        Card(Contessa, "Contessa", "contessa3") ::
        Card(Contessa, "Contessa", "contessa4") ::
        Card(Contessa, "Contessa", "contessa5") ::
        Card(Contessa, "Contessa", "contessa6") ::
        Card(Ambassador, "Ambassador", "ambassador") ::
        Card(Ambassador, "Ambassador", "ambassador2") ::
        Card(Ambassador, "Ambassador", "ambassador3") ::
        Card(Ambassador, "Ambassador", "ambassador4") ::
        Card(Ambassador, "Ambassador", "ambassador5") ::
        Card(Ambassador, "Ambassador", "ambassador6")

    val weighted =
        Card(Duke, "Duke", "duke" ) *** (8*8) ++
        Card(Duke, "Duke", "duke2") *** (6*6) ++
        Card(Duke, "Duke", "duke3") *** (4*4) ++
        Card(Duke, "Duke", "duke4") *** (2*2) ++
        Card(Duke, "Duke", "duke5") *** (2*2) ++
        Card(Duke, "Duke", "duke6") *** (1*1) ++
        Card(Captain, "Captain", "captain" ) *** (8*8) ++
        Card(Captain, "Captain", "captain2") *** (7*7) ++
        Card(Captain, "Captain", "captain3") *** (6*6) ++
        Card(Captain, "Captain", "captain4") *** (4*4) ++
        Card(Captain, "Captain", "captain5") *** (2*2) ++
        Card(Captain, "Captain", "captain6") *** (1*1) ++
        Card(Assassin, "Assassin", "assassin" ) *** (8*8) ++
        Card(Assassin, "Assassin", "assassin2") *** (7*7) ++
        Card(Assassin, "Assassin", "assassin3") *** (6*6) ++
        Card(Assassin, "Assassin", "assassin4") *** (5*5) ++
        Card(Assassin, "Assassin", "assassin5") *** (2*2) ++
        Card(Assassin, "Assassin", "assassin6") *** (1*1) ++
        Card(Contessa, "Contessa", "contessa" ) *** (8*8) ++
        Card(Contessa, "Contessa", "contessa2") *** (8*8) ++
        Card(Contessa, "Contessa", "contessa3") *** (7*7) ++
        Card(Contessa, "Contessa", "contessa4") *** (6*6) ++
        Card(Contessa, "Contessa", "contessa5") *** (1*1) ++
        Card(Contessa, "Contessa", "contessa6") *** (1*1) ++
        Card(Ambassador, "Ambassador", "ambassador" ) *** (8*8) ++
        Card(Ambassador, "Ambassador", "ambassador2") *** (7*7) ++
        Card(Ambassador, "Ambassador", "ambassador3") *** (7*7) ++
        Card(Ambassador, "Ambassador", "ambassador4") *** (5*5) ++
        Card(Ambassador, "Ambassador", "ambassador5") *** (4*4) ++
        Card(Ambassador, "Ambassador", "ambassador6") *** (2*2)

}

trait Faction extends BasePlayer with Named with Styling {
    def name = toString
    def short = name.take(1)
    def style = name.toLowerCase
}

case object Amalthea extends Faction
case object Thebe extends Faction
case object Io extends Faction
case object Europa extends Faction
case object Ganymede extends Faction
case object Callisto extends Faction


trait ViewCard extends ViewObject[Card] { self : UserAction =>
    def card : Card
    def obj = card
}

trait ViewToken { self : UserAction => }


case object NoHand extends HiddenInfo


case class StartAction(version : String) extends StartGameAction with GameVersion
case class PreparedDeckAction(shuffled : List[Card]) extends ShuffledAction[Card]
case class ShuffleDeckAction(then : ForcedAction) extends ForcedAction
case class ShuffledDeckAction(shuffled : List[Card], then : ForcedAction) extends ShuffledAction[Card]
case object TurnStartAction extends ForcedAction
case object TurnEndAction extends ForcedAction
case object DealCardsAction extends ForcedAction
case class MainAction(f : Faction) extends ForcedAction

case class DoNothingAction(self : Faction) extends BaseAction("Do")("Nothing")

case class IncomeAction(self : Faction) extends BaseAction()("Income")

case class ForeignAidAction(self : Faction) extends BaseAction()("Foreign Aid")

case class TaxAction(self : Faction) extends BaseAction()("Tax".styled(Duke))
case class TaxMoneyAction(self : Faction) extends ForcedAction

case class StealAction(self : Faction) extends BaseAction()("Steal".styled(Captain)) with Soft
case class StealFromAction(self : Faction, f : Faction) extends BaseAction("Steal".styled(Captain), "from")(f)
case class StealBlockAction(f : Faction, self : Faction) extends ForcedAction
case class StealBlockWithAction(f : Faction, self : Faction, r : Role) extends BaseAction("Block", Captain, "with")(r)
case class StealMoneyAction(self : Faction, f : Faction) extends ForcedAction
case class StealBlockedAction(self : Faction, f : Faction) extends ForcedAction

case class AssassinateAction(self : Faction) extends BaseAction()("Assassinate".styled(Assassin)) with Soft
case class KillWhomAction(self : Faction, f : Faction) extends BaseAction("Assassinate".styled(Assassin))(f)
case class KillBlockAction(f : Faction, self : Faction) extends ForcedAction
case class KillBlockWithAction(f : Faction, self : Faction, r : Role) extends BaseAction("Block", Assassin, "with")(r)
case class KillKillAction(self : Faction, f : Faction) extends ForcedAction
case class KillBlockedAction(self : Faction, f : Faction) extends ForcedAction

case class ExchangeAction(self : Faction) extends BaseAction()("Exchange".styled(Ambassador))
case class DrawExchangeAction(self : Faction) extends ForcedAction
case class KeepCardsAction(self : Faction, l : List[Card], then : ForcedAction) extends ForcedAction

case class CoupAction(self : Faction) extends BaseAction()("Coup") with Soft
case class CoupWhomAction(self : Faction, f : Faction) extends BaseAction("Coup")(f)

case class CheckInfluenceAction(f : Faction, r : Role, then : ForcedAction, fail : ForcedAction) extends ForcedAction
case class ChallengeInfluenceAction(f : Faction, r : Role, then : ForcedAction, fail : ForcedAction) extends ForcedAction
case class RevealInfluenceAction(f : Faction, r : Role, c : Card, e : Faction, then : ForcedAction, fail : ForcedAction) extends ForcedAction
case class LoseRevealedInfluenceAction(f : Faction, then : ForcedAction) extends ForcedAction
case class LoseInfluenceAction(f : Faction, e : Any, then : ForcedAction) extends ForcedAction with Soft
case class ReplaceInfluenceAction(f : Faction, then : ForcedAction) extends ForcedAction
case class TakeInfluenceAction(f : Faction, then : ForcedAction) extends ForcedAction
case class FailAction(f : Faction, then : ForcedAction) extends ForcedAction

case class CheckBlockWithRoleAction(f : Faction, r : Role, then : ForcedAction, fail : ForcedAction) extends ForcedAction
case class BlockWithRoleAction(f : Faction, r : Role, then : ForcedAction, fail : ForcedAction) extends ForcedAction
case class BlockAction(f : Faction, self : Faction, r : Role, then : ForcedAction, fail : ForcedAction) extends BaseAction("Block", "Foreign Aid".hh, "with")(r) with Retry
case class PassBlockAction(f : Faction, self : Faction, r : Role, then : ForcedAction, fail : ForcedAction) extends BaseAction("Block", "Foreign Aid".hh, "with")("Pass") with Retry
case class BlockFailedAction(f : Faction, self : Faction, r : Role, then : ForcedAction, fail : ForcedAction) extends ForcedAction
case class BlockSuccessAction(f : Faction, self : Faction, r : Role, fail : ForcedAction) extends ForcedAction
case class TakeForeignAidAction(f : Faction) extends ForcedAction

case class ChallengeAction(self : Faction, f : Faction, r : Role, then : ForcedAction, fail : ForcedAction) extends BaseAction(f, "claims", r)("Challenge".hl) with Retry
case class PassAction(self : Faction, f : Faction, r : Role, then : ForcedAction, fail : ForcedAction) extends BaseAction(f, "claims", r)("Pass") with Retry

case class RevealRoleAction(self : Faction, c : Card, r : Role, then : ForcedAction) extends ForcedAction
case class RevealCardAction(self : Faction, c : Card, then : ForcedAction) extends ForcedAction

case class ViewCardInfoAction(self : Faction, card : Card) extends BaseInfo(self.name.styled(self)(styles.name))(card.img) with Info with ViewCard
case class ViewTokenAction(self : Faction) extends BaseInfo(self.name.styled(self)(styles.name))(Token.img) with Info with ViewToken
case class ViewHiddenTokenAction(self : Faction) extends BaseInfo(self.name.styled(self)(styles.name))(HiddenToken.img) with Info with ViewToken

case class GameOverAction(self : Faction, cards : List[Card]) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won", "(" ~ NameReference(f.name, f).hl ~ ")")


class Game(val setup : List[Faction]) extends BaseGame with ContinueGame with LoggedGame {
    type F = Faction

    var roles = Map[Faction, Role]()
    var factions = setup
    var challengers = List[Faction]()
    var blockers = List[Faction]()

    val cards = new ValueTracker[Card]

    val box = cards.another[Card]("deck", Deck.alt)

    val deck = cards.another[Card]("deck", Nil)

    class Player(f : Faction) {
        val hand = cards.another[Card]("hand-" + f)
        val reveal = cards.another[Card]("reveal-" + f)
        val lost = cards.another[Card]("lost-" + f)
        var money = 2
    }

    val players = factions./(f => f -> new Player(f)).toMap

    implicit def faction2player(f : Faction) = players(f)

    var claims = Map[Faction, List[Elem]]()

    def claim(f : Faction, r : Role) {
        claim(f, r.name.styled(r))
        roleClaims += f -> r
    }

    def targetOf(f : Faction, r : Role) {
        claim(f, "Target".hl)
        targetsOf += f -> r
    }

    def claim(f : Faction, c : Elem) { claims += f -> (claims.get(f).|(Nil) :+ c) }

    var over = false

    def isOver = over

    var highlightFaction : List[Faction] = Nil
    var roleClaims : Map[Faction, Role] = Map()
    var targetsOf : Map[Faction, Role] = Map()

    var current : Faction = null

    implicit class SourceEx(source : Location[Card]) {
        def -->(l : List[Card]) = new SourceCardsEx(source, l)
        def -->(d : Card) = new SourceCardEx(source, d)
        def -->(dest : Location[Card]) = new SourceCardsEx(source, cards.get[Card](source)) --> dest
    }

    class SourceCardsEx(source : Location[Card], l : List[Card]) {
        def -->(dest : Location[Card]) {
            l.foreach(new SourceCardEx(source, _) --> dest)
        }
    }

    class SourceCardEx(source : Location[Card], d : Card) {
        def -->(dest : Location[Card]) {
            cards.move(source, d, dest)
        }
    }

    var turn : Int = 0

    def loggedPerform(action : Action, soft : Void) : Continue = {
        val c = performInternal(action, soft)

        c match {
            case Ask(_, Nil) =>
                println("")
                println("")
                println("")
                println("Empty Ask as a result of " + action)
            case _ =>
        }

        highlightFaction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a) => a./(_.faction)
            case _ => Nil
        }

        c
    }

    def info(waiting : List[Faction], self : Option[Faction], actions : List[UserAction]) : $[Info] = {
        self./~(f => 1.to(f.money)./(_ => ViewTokenAction(f)).some.|($(ViewHiddenTokenAction(f))) ++ $(BreakAction) ++ actions.has(NoHand).not.??(f.hand./(ViewCardInfoAction(f, _))))
    }

    implicit def descCard(g : Game, d : Card) = d.img

    def performInternal(a : Action, soft : Void) : Continue = {
        implicit val action = a

        action match {
            case StartAction(version) =>
                log("HRF".hl, "version", gaming.version.hlb)
                log("Coup".hlb)

                if (version != gaming.version)
                    log("Saved game version", version.hlb)

                if (setup.num == 2) {
                    players(setup.head).money = 1

                    log(setup.head, "deducted", 1.hl, "credit")
                }

                Shuffle[Card](box, PreparedDeckAction(_), "Prepare deck")

            case PreparedDeckAction(r) =>
                val l = r.distinct
                val ll = l.%(_.role == Duke).take(3) ++ l.%(_.role == Captain).take(3) ++ l.%(_.role == Assassin).take(3) ++ l.%(_.role == Contessa).take(3) ++ l.%(_.role == Ambassador).take(3)

                box --> ll --> deck

                ShuffleDeckAction(DealCardsAction)

            case ShuffleDeckAction(then) =>
                Shuffle[Card](deck, ShuffledDeckAction(_, then), "Shuffle deck")

            case ShuffledDeckAction(r, then) =>
                log("Shuffled deck")

                deck --> r --> deck

                then

            case DealCardsAction =>
                factions.foreach { f =>
                    deck --> deck.head --> f.hand
                    deck --> deck.head --> f.hand
                }

                log("Dealt two cards to each player")

                Milestone(TurnStartAction)

            case TurnEndAction =>
                DelayedContinue(2000 + claims.values./(_.num).sum * 400, Milestone(TurnStartAction))

            case TurnStartAction =>
                factions = factions.%(players(_).hand.any)
                if (factions.num <= 1)
                    GameOverAction(factions.only, factions./~(players(_).hand))
                else {
                    val f = factions.head

                    factions = factions.drop(1) ++ factions.take(1)

                    current = f

                    claims = Map()
                    roleClaims = Map()
                    targetsOf = Map()

                    log(DoubleLine)
                    turn += 1
                    log("Turn", ("#" + turn).hl, f)

                    MainAction(f)
                }

            case MainAction(f) =>
                var actions = List[UserAction]()

                actions :+= IncomeAction(f)

                actions :+= ForeignAidAction(f)

                actions :+= TaxAction(f)

                actions :+= StealAction(f).x(factions.but(f).%(players(_).money > 0).none)

                actions :+= AssassinateAction(f).x(f.money < 3)

                actions :+= ExchangeAction(f)

                if (f.money >= 10)
                    actions = Nil

                actions :+= CoupAction(f).x(f.money < 7)

                Ask(f, actions)

            case IncomeAction(f) =>
                claim(f, "Income")

                f.money += 1

                claim(f, ("+" + 1).hl)

                log(f, "took Income", 1.hl, "credit")

                TurnEndAction

            case ForeignAidAction(f) =>
                log(f, "requested Foreign Aid")

                claim(f, "Foreign Aid")

                CheckBlockWithRoleAction(f, Duke, TakeForeignAidAction(f), TurnEndAction)

            case CheckBlockWithRoleAction(f, r, then, fail) =>
                blockers = factions.but(f)

                BlockWithRoleAction(f, r, then, fail)

            case BlockWithRoleAction(f, r, then, fail) =>
                if (blockers.none)
                    then
                else
                    MultiAsk(blockers./(b => Ask(b)(PassBlockAction(b, f, r, then, fail))(BlockAction(b, f, r, then, fail))))

            case PassBlockAction(b, f, r, then, fail) =>
                BlockFailedAction(b, f, r, then, fail)

            case BlockAction(b, f, r, then, fail) =>
                log(b, "blocked with", r)

                claim(b, r)

                CheckInfluenceAction(b, r, BlockSuccessAction(b, f, r, fail), BlockFailedAction(b, f, r, then, fail))

            case BlockSuccessAction(b, f, r, then) =>
                claim(f, "Blocked".hl)

                then

            case BlockFailedAction(b, f, r, then, fail) =>
                blockers = blockers.but(b)

                BlockWithRoleAction(f, r, then, fail)

            case TakeForeignAidAction(f) =>
                f.money += 2

                claim(f, ("+" + 2).hl)

                log(f, "took ", 2.hl, "credits")

                TurnEndAction

            case TaxAction(f) =>
                claim(f, Duke)

                log(f, "gathered", "Tax".styled(Duke))

                CheckInfluenceAction(f, Duke, TaxMoneyAction(f), TurnEndAction)

            case TaxMoneyAction(f) =>
                f.money += 3

                log(f, "took", 3.hl, "credits")

                claim(f, ("+" + 3).hl)

                TurnEndAction

            case StealAction(f) =>
                Ask(f, factions.but(f).%(players(_).money > 0)./(StealFromAction(f, _))).cancel

            case StealFromAction(f, v) =>
                claim(f, Captain)
                targetOf(v, Captain)

                log(f, "stole from", v)

                CheckInfluenceAction(f, Captain, StealBlockAction(f, v), TurnEndAction)

            case StealBlockAction(f, v) =>
                Ask(v)(v.hand.any.$(StealBlockWithAction(f, v, Captain), StealBlockWithAction(f, v, Ambassador))).refuse(StealMoneyAction(f, v))

            case StealBlockWithAction(f, v, r) =>
                claim(v, r)

                log(v, "blocked", f, "with", r)

                CheckInfluenceAction(v, r, StealBlockedAction(f, v), StealMoneyAction(f, v))

            case StealMoneyAction(f, v) =>
                val n = min(2, v.money)

                v.money -= n
                f.money += n

                claim(v, ("-" + n).hl)
                claim(f, ("+" + n).hl)

                log(f, "stole", n.hl, "credits from", v)

                TurnEndAction

            case StealBlockedAction(f, v) =>
                claim(f, "Blocked".hl)

                TurnEndAction

            case AssassinateAction(f) =>
                Ask(f, factions.but(f)./(KillWhomAction(f, _))).cancel

            case KillWhomAction(f, v) =>
                claim(f, Assassin)
                targetOf(v, Assassin)

                log(f, "assassinated", v)

                CheckInfluenceAction(f, Assassin, KillBlockAction(f, v), TurnEndAction)

            case KillBlockAction(f, v) =>
                log(f, "paid", 3.hl, "credits")

                f.money -= 3

                claim(f, ("-" + 3).hl)

                Ask(v)(v.hand.any.?(KillBlockWithAction(f, v, Contessa))).refuse(KillKillAction(f, v))

            case KillBlockWithAction(f, v, r) =>
                claim(v, Contessa)

                log(v, "blocked", f, "with", r)

                CheckInfluenceAction(v, r, KillBlockedAction(f, v), KillKillAction(f, v))

            case KillKillAction(f, v) =>
                if (v.hand.any) {
                    claim(v, "Kill".styled(styles.kill))

                    LoseInfluenceAction(v, (f, "assassinated", v), TurnEndAction)
                }
                else
                    TurnEndAction

            case KillBlockedAction(f, v) =>
                claim(f, "Blocked".hl)

                TurnEndAction

            case ExchangeAction(f) =>
                claim(f, Ambassador)

                log(f, "exchanged influence")

                CheckInfluenceAction(f, Ambassador, DrawExchangeAction(f), TurnEndAction)

            case DrawExchangeAction(f) =>
                val k = f.hand.num

                deck --> deck.head --> f.hand
                deck --> deck.head --> f.hand

                log(f, "drew influence")
                log(f, "drew influence")

                XXSelectObjectsAction(f, f.hand)
                    .withGroup("Keep " ~ k.cards)
                    .withRule(_.num(k))
                    .withThenElem(KeepCardsAction(f, _, ShuffleDeckAction(TurnEndAction)))("Keep".hl)
                    .withExtra($(NoHand))

            case KeepCardsAction(f, l, then) =>
                val r = f.hand.get.diff(l)

                f.hand --> r --> deck

                r.foreach(_ => log(f, "discarded influence"))

                then

            case CoupAction(f) =>
                Ask(f, factions.but(f)./(CoupWhomAction(f, _))).cancel

            case CoupWhomAction(f, v) =>
                claim(f, Coup)
                targetOf(v, Coup)

                f.money -= 7

                    log(f, "launched", "coup".hl, "against", v)
                log(f, "paid", 7.hl, "credits")

                LoseInfluenceAction(v, (f, "launched", "coup".hl, "against", v), TurnEndAction)

            case CheckInfluenceAction(f, r, then, fail) =>
                challengers = factions.but(f).%(players(_).hand.any)

                ChallengeInfluenceAction(f, r, then, fail)

            case ChallengeInfluenceAction(f, r, then, fail) =>
                if (challengers.none)
                    then
                else
                    MultiAsk(challengers./(c => Ask(c)(PassAction(c, f, r, then, fail))(ChallengeAction(c, f, r, then, fail))))

            case PassAction(c, f, r, then, fail) =>
                challengers = challengers.but(c)

                ChallengeInfluenceAction(f, r, then, fail)

            case ChallengeAction(e, f, r, then, fail) =>
                challengers = Nil

                claim(e, "Challenge")

                log(e, "challenged", r, "of", f)

                YYSelectObjectsAction(f, f.hand)
                    .withGroup(e, "challenged", r)
                    .withThen(RevealInfluenceAction(f, r, _, e, then, fail))(t => ("Reveal and " + (t.role == r).?("replace").|("lose")).spn((t.role != r).?(styles.kill)) ~ " " ~ t.elem)("Reveal")
                    .withExtra($(NoHand))

            case RevealInfluenceAction(f, r, c, e, then, fail) =>
                if (c.role == r)
                    RevealRoleAction(f, c, r, FailAction(e, LoseInfluenceAction(e, (e, "failed", "Challenge".hl), ReplaceInfluenceAction(f, then))))
                else
                    RevealRoleAction(f, c, r, FailAction(f, LoseRevealedInfluenceAction(f, fail)))

            case RevealRoleAction(f, c, r, then) =>
                f.hand --> c --> f.reveal

                log(f, "revealed", c.role)

                then

            case FailAction(f, then) =>
                claim(f, "Fail".styled(styles.kill))

                then

            case ReplaceInfluenceAction(f, then) =>
                f.reveal --> deck

                ShuffleDeckAction(TakeInfluenceAction(f, then))

            case TakeInfluenceAction(f, then) =>
                deck --> deck.head --> f.hand

                log(f, "replaced influence")

                then

            case LoseRevealedInfluenceAction(f, then) =>
                f.reveal --> f.lost

                log(f, "lost influence")

                factions = factions.%(f => f.hand.any || f.reveal.any)

                if (factions.num <= 1) {
                    val w = factions.only
                    GameOverAction(w, w.hand ++ w.reveal)
                }
                else
                    then

            case LoseInfluenceAction(f, e, then) =>
                if (f.hand.num == 1)
                    Ask(f)(DoAction(RevealCardAction(f, f.hand(0), LoseRevealedInfluenceAction(f, then))))
                else {
                    YYSelectObjectsAction(f, f.hand)
                        .withGroup(e)
                        .withThen(RevealCardAction(f, _, LoseRevealedInfluenceAction(f, then)))("Lose influence".styled(styles.kill) ~ " " ~ _.elem)("Lose".styled(styles.kill) ~ " influence")
                        .withExtra($(NoHand))
                }

            case RevealCardAction(f, c, then) =>
                f.hand --> c --> f.reveal

                then

            case GameOverAction(f, l) =>
                over = true

                log(f, "won with", l./(_.elem).commaAnd)

                GameOver($(f), "Game Over", $(f)./~(f => (f.hand ++ f.reveal)./(ViewCardInfoAction(f, _)) :+ GameOverWonAction(null, f)))

            case a : SelfPerform =>
                a.perform(soft)(this)
        }
    }
}
