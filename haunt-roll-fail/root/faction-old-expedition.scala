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

trait OldExpedition extends WarriorFaction {
    val clashKey = KI

    val warrior = OldBadger

    def abilities(options : $[Meta.O]) = $(Armored, DaylightCraft)

    override val transports : $[$[Transport]] = $($(RuledMove, CaravanMove))

    val relics = 4.times(OldTablet) ++ 4.times(OldJewel) ++ 4.times(OldIdol)

    def pieces(options : $[Meta.O]) = OldBadger *** 15 ++ Caravan *** 3 ++ Station *** 5 ++ OldTablet *** 4 ++ OldJewel *** 4 ++ OldIdol *** 4
}

case object Armored extends BattleEffect with FactionEffect {
    val name = "Armored"
}

case object OldBadger extends Warrior {
    override val name = "Old Badger"
    override val id = "Badger"
}

case object Caravan extends Token with Movable
case object Station extends Building

trait OldRelic extends Token

case object OldTablet extends OldRelic with Movable { override val id = "tablet" }
case object OldJewel extends OldRelic with Movable { override val id = "jewel" }
case object OldIdol extends OldRelic with Movable { override val id = "idol" }

trait Reward
case object RewardWarrior extends Reward
case object RewardCard extends Reward
case object RewardTwoCards extends Reward

object OldRelics {
    def all = $(OldTablet, OldJewel, OldIdol)
}

trait Mission extends Record {
    val name : String
}

case object MissionCraft extends Mission {
    val name = "Craft"
}

case object MissionMoveBattle extends Mission {
    val name = "Move and Battle"
}

case object MissionEscort extends Mission {
    val name = "Escort"
}

case object OK extends OldExpedition {
    val name = "Old Keepers"
    override def funName = NameReference(name, this) ~ " in Old Iron"
    val short = "OK"
    val style = "ok"
    val priority = "K"

    override def note : Elem = SpacedDash ~ "early design of " ~ KI.elem

    def advertising = 3.times(Caravan.img(this)).merge
    def motto = "Delve".styled(this)
}

class OldExpeditionPlayer(val faction : OldExpedition)(implicit val game : Game) extends FactionState {
    board.forests.foreach(c => location(c))

    var acted = 0
    var mission : Mission = null

    val revealed = cards("revealed")

    var caravaned : $[Clearing] = $
    var stationed : $[Clearing] = $

    var rewards = $[((OldRelic, Suit), Reward)](
        (OldIdol, Fox) -> RewardCard,
        (OldIdol, Mouse) -> RewardWarrior,
        (OldIdol, Rabbit) -> RewardWarrior,
        (OldIdol, Bird) -> RewardTwoCards,
        (OldTablet, Fox) -> RewardWarrior,
        (OldTablet, Mouse) -> RewardCard,
        (OldTablet, Rabbit) -> RewardWarrior,
        (OldTablet, Bird) -> RewardTwoCards,
        (OldJewel, Fox) -> RewardWarrior,
        (OldJewel, Mouse) -> RewardWarrior,
        (OldJewel, Rabbit) -> RewardCard,
        (OldJewel, Bird) -> RewardTwoCards
    ).toMap

    var mcrafted = 0
    var delving = false
    var recovering = false
    var unrecovered : $[(Clearing, OldRelic)] = $

    def stations = all(Station)
    def caravans = all(Caravan)

    def craft = stations./(_.asset) ++ caravans./(_.asset)
}

case object CaravanMove extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = (f, o) @@ {
        case (f : OldExpedition, o : Clearing) => f.at(o).of[Warrior].any
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && ({
        val warriors = l.count(OldBadger)
        val caravan = l.count(Caravan)
        val relics = l.count(OldTablet) + l.count(OldIdol) + l.count(OldJewel)

        ((caravan == 0 && warriors == 1) || (caravan == 1 && warriors >= 1 && warriors <= 3) /*|| (caravan == 2 && warriors >= 2 && warriors <= 5)*/) && (relics <= warriors)
    })

    override def order(l : $[Movable]) : Int = l.count(Caravan) * 900 + l.count(OldBadger) * 9 + l.count(OldTablet) + l.count(OldIdol) + l.count(OldJewel)
    override def sortBy(m : Movable) : Int = (m == Caravan).??(-2) + (m == OldBadger).??(-1)
}

trait OKDaylightQuestion extends FactionAction {
    override def self : OldExpedition
    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ " " ~ (self.acted < 4).?((self.acted + 1).hl ~ " / " ~ (4).elem).|("extra")
}

case class PlaceStartingTokenAction(self : Faction, k : Clearing, r : Clearing, p : Token) extends BaseAction(self, "places", p.of(self), "in")(r)
case class OldRelicRandomnessAction(f : OldExpedition, r : Clearing, rolled : $[Int]) extends RolledAction[Int]

case class ChooseMissionAction(f : OldExpedition, then : ForcedAction) extends ForcedAction
case class SelectMissionAction(self : OldExpedition, m : Mission, then : ForcedAction) extends BaseAction("Choose", "Mission".styled(self))(m.name.styled(self))

case class OldBadgerRevealCardMainAction(self : OldExpedition, s : SuitCost, then : ForcedAction, alt : UserAction) extends ForcedAction with Soft
case class OldBadgerRevealCardAction(self : OldExpedition, s : SuitCost, d : DeckCard, then : ForcedAction) extends BaseAction("Reveal", s.elem, "card")(d.img) with ViewCard

case class OldBadgerRecruitMainAction(f : OldExpedition) extends ForcedAction
case class RecruitOldBadgersAction(self : OldExpedition, l : $[Clearing], r : $[Clearing]) extends BaseAction("Recruit in")(l)

case class OldBadgerBuildMainAction(f : OldExpedition) extends ForcedAction
case class PlaceCaravanMainAction(self : OldExpedition, c : Clearing) extends BaseAction("Start", Caravan.of(self), "in")(c) with Soft
case class PlaceCaravanAction(self : OldExpedition, c : Clearing) extends ForcedAction
case class OldBuildStationMainAction(self : OldExpedition, c : Clearing) extends BaseAction("Build", Station.of(self), "in")(c) with Soft
case class OldBuildStationAction(self : OldExpedition, c : Clearing) extends ForcedAction

case class OldBadgerMainAction(f : OldExpedition) extends ForcedAction
case class OldBadgerMainDoneAction(f : OldExpedition) extends ForcedAction

case class OldBadgerAttackAction(self : OldExpedition, l : $[Clearing]) extends OptionAction("Battle".styled(self)) with OKDaylightQuestion with Soft
case class OldBadgerMissionAttackAction(self : OldExpedition, l : $[Clearing]) extends OptionAction("Mission Battle".styled(self)) with OKDaylightQuestion with Soft

case class OldBadgerMoveAction(self : OldExpedition, l : $[Clearing]) extends OptionAction("Move".styled(self)) with OKDaylightQuestion with Soft
case class OldBadgerMissionMoveAction(self : OldExpedition, l : $[Clearing]) extends OptionAction("Mission Move".styled(self)) with OKDaylightQuestion with Soft

case class OldBadgerEscortMainAction(self : OldExpedition, l : $[Clearing]) extends OptionAction("Escort".styled(self)) with OKDaylightQuestion with Soft
case class OldBadgerEscortAction(self : OldExpedition, c : Clearing) extends BaseAction("Escort".styled(self), "from")(c)

case class OldBadgerDelveMainAction(self : OldExpedition, l : $[Clearing]) extends OptionAction("Delve".styled(self)) with OKDaylightQuestion with Soft
case class OldBadgerDelveAction(self : OldExpedition, c : Clearing) extends BaseAction("Delve".styled(self), "in")(c) with Soft
case class OldBadgerDelveContinueAction(self : OldExpedition, c : Clearing) extends ForcedAction with Soft
case class OldBadgerDelveSelectAction(self : OldExpedition, c : Clearing, q : Forest, r : OldRelic) extends BaseAction("Delve".styled(self), "in", c)(r.of(self), "from", implicit g => g.board.forestName(q, Some(c))) with Soft
case class OldBadgerDelveRelicAction(self : OldExpedition, c : Clearing, q : Forest, r : OldRelic) extends ForcedAction

case class OldBadgerRecoverMainAction(self : OldExpedition) extends OptionAction("Recover".styled(self)) with OKDaylightQuestion with Soft
case class OldBadgerRecoverContinueAction(self : OldExpedition) extends ForcedAction with Soft
case class OldBadgerRecoverAction(self : OldExpedition, c : Clearing, r : OldRelic) extends BaseAction("Recover".styled(self))("Recover".styled(self), r.of(self), "in", c)
case class OldBadgerRecoverRolledAction(self : OldExpedition, c : Clearing, r : OldRelic, min : Int, max : Int) extends RolledAction[Int] { def rolled = $(min, max) }
case class OldBadgerRecoverSuccessAction(self : OldExpedition, c : Clearing, r : OldRelic) extends ForcedAction
case class OldBadgerRecoverAsAction(self : OldExpedition, c : Clearing, r : OldRelic, s : Suit) extends BaseAction("Recover".styled(self), r.of(self), "in", c)("As", s)
case class OldBadgerRecoverFailAction(self : OldExpedition, c : Clearing, r : OldRelic) extends ForcedAction
case class OldBadgerRecoverAmendAction(self : OldExpedition, c : Clearing, r : OldRelic, current : Int, target : Int) extends ForcedAction

case class MissionBattleFromAction(self : OldExpedition, c : Clearing) extends BaseAction(self, "battles on a mission in")(c) with Soft
case class MissionBattleFromDiscardAction(self : OldExpedition, c : Clearing) extends ForcedAction

case class MissionMoveFromAction(self : OldExpedition, c : Clearing) extends BaseAction(self, "moves on a mission from")(c) with Soft
case class MissionMoveFromDiscardAction(self : OldExpedition, c : Clearing) extends ForcedAction

case class OldReturnToWoodAction(self : Faction, c : Clearing, e : OldExpedition, p : OldRelic, q : Forest, then : ForcedAction) extends BaseAction("Return", p.of(e), "to forest")(implicit g => g.board.forestName(q))

case class ForMissionBattle(c : Clearing) extends Message {
    def elem(implicit game : Game) = " to battle in " ~ c.elem
}

case class ForMissionMove(c : Clearing) extends Message {
    def elem(implicit game : Game) = " to move from " ~ c.elem
}

case class ToRecover(f : Faction, c : Clearing, r : OldRelic) extends Message {
    def elem(implicit game : Game) = " to recover " ~ r.of(f) ~ " in " ~ c.elem
}

case class ToPlaceCaravan(f : Faction, c : Clearing) extends Message {
    def elem(implicit game : Game) = " to place " ~ Caravan.of(f) ~ " in " ~ c.elem
}

object OldExpeditionExpansion extends FactionExpansion[OldExpedition] {
    val OldD1000 = Die.range(0 -> 999)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : OldExpedition) =>
            game.states += f -> new OldExpeditionPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : OldExpedition) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : OldExpedition, c) =>
            Force(PlaceStartingTokenAction(f, c, c, Caravan))

        case PlaceStartingTokenAction(f : OldExpedition, r, c, Caravan) =>
            f.reserve --> Caravan --> c
            f.reserve --> 2.times(OldBadger) --> c

            f.log("placed", $(Caravan.of(f), OldBadger.of(f), OldBadger.of(f)).comma, "in", c)

            if (r == c)
                Ask(f)(game.connected(c).diff(board.inner)./(PlaceStartingTokenAction(f, c, _, Caravan)))
            else
                Roll[Int](0.until(24)./(_ => OldD1000), l => OldRelicRandomnessAction(f, r, l))

        case OldRelicRandomnessAction(f, r, l) =>
            val randomness = new Randomness(l ++ l)

            var nf = board.forests.%(q => board.fromForest(q).has(r)).shuffleWith(randomness)

            while (board.forests.diff(nf).any) {
                nf ++= board.forests.diff(nf).%(q => nf.%(w => game.fromForest(w).intersect(game.fromForest(q)).num > 1).any).shuffleWith(randomness)
            }

            var rl = f.relics.shuffleWith(randomness)

            rl.lazyZip(board.forests.shuffleWith(randomness) ++ nf.reverse).foreach { case (a, q) =>
                f.reserve --> a --> q
            }

            ChooseMissionAction(f, SetupFactionsAction)

        // HELPER
        case OldBadgerRevealCardMainAction(f, s, then, alt) =>
            Ask(f)(f.hand./(d => OldBadgerRevealCardAction(f, s, d, then).x(d.matches(s).not)).:+(alt))

        case OldBadgerRevealCardAction(f, _, d, then) =>
            f.hand --> d --> f.revealed

            f.log("revealed", d)

            then

        case ChooseMissionAction(f, then) =>
            Ask(f)(SelectMissionAction(f, MissionCraft, then) :: SelectMissionAction(f, MissionMoveBattle, then) :: SelectMissionAction(f, MissionEscort, then))

        case SelectMissionAction(f, m, then) =>
            f.mission = m

            f.log("chose", m.name.styled(f), "mission")

            then

        case CraftAssignAction(f : OldExpedition, d, all, used, m, then) =>
            f.crafted ++= used

            f.mcrafted += 1

            then

        case BattlePostHitInAction(b, e, f : OldExpedition, p : OldRelic, then) =>
            e.log("recovered", p.of(f))
            then

        case BattlePostHitInAction(b, e, f : OldExpedition, p @ OldBadger, then) =>
            e.log("pinched", p.of(f))
            then

        case BattlePostHitInAction(b, e, f : OldExpedition, p @ Caravan, then) =>
            e.log("looted", p.of(f))
            then

        case BattlePostHitInAction(b, e, f : OldExpedition, p @ Station, then) =>
            e.log("marauded", p.of(f))
            then

        case ForcedRemoveTargetEffectAction(e, c, f : OldExpedition, p : OldRelic, then) =>
            e.oscore(clearings.%(_.asset.matches(c.cost)).%(e.rules).num)("returning the relic", p.of(f))
            Ask(e, board.forests./(q => OldReturnToWoodAction(e, c, f, p, q, then)))

        case OldReturnToWoodAction(e, c, f, p, q, then) =>
            e.log("returned", p.of(f), "to", q)

            f.limbo(c) --> p --> q

            then

        // TURN
        case BirdsongNAction(40, f : OldExpedition) =>
            XCraftMainAction(f)

        case BirdsongNAction(41, f : OldExpedition) =>
            if (f.mission == MissionCraft && f.mcrafted > 0)
                DrawCardsAction(f, f.mcrafted, NoMessage, AddCardsAction(f, Next))
            else
                Next

        case BirdsongNAction(60, f : OldExpedition) =>
            OldBadgerBuildMainAction(f)

        case OldBadgerBuildMainAction(f) =>
            val ct = f.pool(Caravan).??(clearings.diff(f.caravaned).%(f.canPlace).%(c => f.at(c).of[Warrior].any))
            val st = f.pool(Station).??(clearings.diff(f.stationed).%(f.canBuild).%(c => f.at(c).of[Warrior].any).%(c => f.at(c).has(Caravan)))

            Ask(f)
              .each(ct)(c => PlaceCaravanMainAction(f, c).x(f.hand.%(_.matches(c.cost)).none, "no matching card"))
              .each(st)(c => OldBuildStationMainAction(f, c).x(f.hand.%(_.matches(c.cost)).none, "no matching card"))
              .done(Next)
              .birdsong(f)

        case PlaceCaravanMainAction(f, c) =>
                OptionalDiscardCardAction(f, ToPlaceCaravan(f, c), c.cost, PlaceCaravanAction(f, c))

        case PlaceCaravanAction(f, c) =>
            f.drawn --> discard(f)

            f.reserve --> Caravan --> c

            f.log("started", Caravan.of(f), "in", c)

            Repeat

        case OldBuildStationMainAction(f, c) =>
            OldBadgerRevealCardMainAction(f, c.cost, OldBuildStationAction(f, c), CancelAction)

        case OldBuildStationAction(f, c) =>
            f.from(c) --> Caravan --> f.reserve
            f.reserve --> Station --> c

            f.log("built", Station.of(f), "in", c)

            Repeat

        case BirdsongNAction(80, f : OldExpedition) =>
            log("Recruit")

            Next

        case BirdsongNAction(81, f : OldExpedition) =>
            OldBadgerRecruitMainAction(f)

        case OldBadgerRecruitMainAction(f) =>
            val t = f.pooled(OldBadger)
            val r = f.all(Station).%(f.canPlace)

            if (t >= r.num)
                Ask(f)(RecruitOldBadgersAction(f, r, r))
            else {
                val c = r.combinations(t).$
                if (c.num > 1)
                    Ask(f).each(c)(RecruitOldBadgersAction(f, _, r)).birdsong(f)
                else
                    Ask(f).each(c)(RecruitOldBadgersAction(f, _, r))
             }

        case RecruitOldBadgersAction(f, l, r) =>
            l.distinct.foreach { c =>
                val n = l.count(c)

                f.reserve --> n.times(f.warrior) --> c

                f.log("recruited", n.times(f.warrior.of(f)).comma, "in", c)
            }

            if (r.num > l.num && f.totalWar)
                f.oscore(r.num - l.num)("recruiting")

            Next

        case DaylightNAction(50, f : OldExpedition) =>
            OldBadgerMainAction(f)

        case OldBadgerMainAction(f) =>
            var actions : $[UserAction] = $


            val att = clearings.%(f.canAttackIn)
            actions :+= OldBadgerAttackAction(f, att).x(att.none)


            val mvv = f.moveFrom.of[Clearing]
            actions :+= OldBadgerMoveAction(f, mvv).x(mvv.none)


            val esc = clearings.%(c => f.at(c).of[Warrior].any)
            actions :+= OldBadgerEscortMainAction(f, esc).x(esc.none, "no warriors").x(deck.none && pile.none, "no cards available")


            val dlv0 = clearings
            val dlv1 = dlv0.%(c => board.forests.%(q => game.fromForest(q).has(c)).%(f.present).any)
            val dlv2 = dlv1.%(f.rules)
            val dlv3 = dlv2.%(c => f.hand.%(_.matches(c.cost)).any)

            actions :+= OldBadgerDelveMainAction(f, dlv3).x(dlv1.none, "no relics").x(dlv2.none, "no rule").x(dlv3.none, "no matching cards")


            val relics = OldRelics.all./(r => r -> f.all(r)).toMap

            val rcv0 = f.all(Station)
            val rcv1 = rcv0.%(c => OldRelics.all.exists(r => f.all(r).has(c)))
            val rcv2 = rcv1.%(c => OldRelics.all.exists(r => f.all(r).has(c) && (c.suits.exists(s => f.rewards.contains((r, s))) || f.rewards.contains((r, Bird)))))

            actions :+= OldBadgerRecoverMainAction(f)
                .!(rcv0.none, "no stations")
                .!(rcv1.none, "no relics")
                .!(rcv2.none, "no slots")


            if (f.acted >= 4)
                actions = $


            if (f.mission == MissionMoveBattle) {
                val att = clearings.%(f.canAttackIn)
                actions :+= OldBadgerMissionAttackAction(f, att).x(att.none).x(att.%(c => f.hand.exists(_.matches(c.cost))).none, "no matching cards")

                val mvv = f.moveFrom.of[Clearing]
                actions :+= OldBadgerMissionMoveAction(f, mvv).x(mvv.none).x(mvv./(c => f.hand.exists(_.matches(c.cost))).none, "no matching cards")
            }


            Ask(f)(actions).done(Next).daylight(f)

        case OldBadgerAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), OldBadgerMainDoneAction(f))

        case OldBadgerMoveAction(f, l) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, CancelAction +: f.daylight, OldBadgerMainDoneAction(f))

        case OldBadgerMissionAttackAction(f, l) =>
            Ask(f)(l./(c => MissionBattleFromAction(f, c).x(f.hand.%(_.matches(c.cost)).none))).cancel

        case MissionBattleFromAction(f, c) =>
            OptionalDiscardCardAction(f, ForMissionBattle(c), c.cost, MissionBattleFromDiscardAction(f, c))

        case MissionBattleFromDiscardAction(f, c) =>
            f.drawn --> discard(f)

            BattleInitAction(f, f, NoMessage, $(c), $(DoneAction(OldBadgerMainAction(f))), OldBadgerMainAction(f))

        case OldBadgerMissionMoveAction(f, l) =>
            Ask(f)(l./(c => MissionMoveFromAction(f, c).x(f.hand.%(_.matches(c.cost)).none))).cancel

        case MissionMoveFromAction(f, c) =>
            OptionalDiscardCardAction(f, ForMissionMove(c), c.cost, MissionMoveFromDiscardAction(f, c))

        case MissionMoveFromDiscardAction(f, c) =>
            f.drawn --> discard(f)

            MoveInitAction(f, f, $, NoMessage, $(c), f.movable, DoneAction(OldBadgerMainAction(f)) +: f.daylight, OldBadgerMainAction(f))

        case OldBadgerEscortMainAction(f, l) =>
            Ask(f)(l./(OldBadgerEscortAction(f, _))).cancel

        case OldBadgerEscortAction(f, c) =>
            f.from(c) --> OldBadger --> f.reserve

            DrawCardsAction(f, 1 + (f.mission == MissionEscort).??(1), NoMessage, AddCardsAction(f, OldBadgerMainDoneAction(f)))

        case OldBadgerDelveMainAction(f, l) =>
            Ask(f)(l./(OldBadgerDelveAction(f, _))).cancel

        case OldBadgerDelveAction(f, c) =>
            OldBadgerDelveContinueAction(f, c)

        case OldBadgerDelveContinueAction(f, c) =>
            Ask(f)(f.hand.%(_.matches(c.cost)).any.??(board.forests.%(q => game.fromForest(q).has(c))./~(q => f.at(q)./(p => OldBadgerDelveSelectAction(f, c, q, p.asInstanceOf[OldRelic])))))
              .cancelIf(f.delving.not)
              .done(f.delving.?(OldBadgerMainDoneAction(f)))

        case OldBadgerDelveSelectAction(f, c, q, r) =>
            OldBadgerRevealCardMainAction(f, c.cost, OldBadgerDelveRelicAction(f, c, q, r), f.delving.?(CancelAction).|(CancelAction))

        case OldBadgerDelveRelicAction(f, c, q, r) =>
            f.delving = true

            f.from(q) --> r --> c

            f.log("delved", r.of(f), "in", c)

            OldBadgerDelveContinueAction(f, c)

        case OldBadgerRecoverMainAction(f) =>
             OldBadgerRecoverContinueAction(f)

        case OldBadgerRecoverContinueAction(f) =>
            val rcv0 = f.all(Station)
            val rcv1 = rcv0.%(c => OldRelics.all./(r => f.all(r)).%(_.has(c)).any)
            val rcv2 = rcv1.%(c => OldRelics.all./(r => f.all(r).has(c) && (c.suits.exists(s => f.rewards.contains((r, s))) || f.rewards.contains((r, Bird)))).any)

            Ask(f)(f.all(Station).distinct./~(c => OldRelics.all./~(r => 1.to(f.at(c).count(r) - f.unrecovered.count((c, r)))./(_ => OldBadgerRecoverAction(f, c, r).x(c.suits.exists(s => f.rewards.contains((r, s))).not && f.rewards.contains((r, Bird)).not, "no slot")))))
              .cancelIf(f.recovering.not)
              .done(f.recovering.?(OldBadgerMainDoneAction(f)))
              .needOk

        case OldBadgerRecoverAction(f, c, r) =>
            Roll[Int](D4 :: D4, l => OldBadgerRecoverRolledAction(f, c, r, l(0), l(1)), f.elem ~ " recovers " ~ r.of(f) ~ " in " ~ c.elem(game))

        case OldBadgerRecoverRolledAction(f, c, r, a, b) =>
            f.recovering = true

            f.log("rolled", a.roll, " and ", b.roll)

            val rules = clearings.%(o => o.asset.matches(c.cost)).%(f.rules)

            f.log("rules", rules.num.hl, "clearings of", c.suits)

            if (rules.num >= max(a, b))
                OldBadgerRecoverSuccessAction(f, c, r)
            else
                OldBadgerRecoverAmendAction(f, c, r, rules.num, max(a, b))

        case OldBadgerRecoverSuccessAction(f, c, r) =>
            f.drawn --> discard(f)

            Ask(f)((c.suits./(s => (r, s)) :+ (r, Bird)).filter(f.rewards.contains).rights./(s => OldBadgerRecoverAsAction(f, c, r, s)))

        case OldBadgerRecoverAsAction(f, c, r, s) =>
            f.from(c) --> r --> f.reserve

            f.log("recovered", r.of(f), "from", c, "as", s)

            f.oscore(2)("recovering", r.of(f), "from", c)

            val w = f.rewards((r, s))

            f.rewards -= ((r, s))

            if (f.rewards.keys.$.filter(_._1 == r).none)
                f.oscore(3)("recovering all", (r.name + "s").styled(f))

            w match {
                case RewardCard =>
                    DrawCardsAction(f, 1, NoMessage, AddCardsAction(f, OldBadgerRecoverContinueAction(f)))

                case RewardTwoCards =>
                    DrawCardsAction(f, 2, NoMessage, AddCardsAction(f, OldBadgerRecoverContinueAction(f)))

                case RewardWarrior =>
                    f.reserve --?> f.warrior --> c

                    f.log("placed", f.warrior.of(f), "in", c)

                    OldBadgerRecoverContinueAction(f)
            }

        case OldBadgerRecoverFailAction(f, c, r) =>
            f.unrecovered :+= (c, r)

            OldBadgerRecoverContinueAction(f)

        case OldBadgerRecoverAmendAction(f, c, r, x, t) =>
            OpportunityDiscardCardsAction(f, ToRecover(f, c, r), t - x, $, c.cost, OldBadgerRecoverSuccessAction(f, c, r), OldBadgerRecoverFailAction(f, c, r))

        case OldBadgerMainDoneAction(f) =>
            f.acted += 1

            f.recovering = false
            f.delving = false
            f.unrecovered = $

            Repeat

        case EveningNAction(10, f : OldExpedition) =>
            val b = f.revealed.%(_.suit == Bird)
            f.revealed --> b --> discard(f)

            if (f.revealed.any) {
                f.revealed.get.foreach(f.log("got back", _))
                f.revealed --> f.hand
            }

            Next

        case EveningNAction(20, f : OldExpedition) =>
            ChooseMissionAction(f, Next)

        case NightStartAction(f : OldExpedition) =>
            EveningHandLimitAction(f)

        case FactionCleanUpAction(f : OldExpedition) =>
            f.acted = 0
            f.mcrafted = 0

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
