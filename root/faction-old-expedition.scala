package root

import root.gaming._

import colmat._

import hrf.elem._
import root.elem._

trait OldExpedition extends WarriorFaction {
    val expansion = OldExpeditionExpansion

    val warrior = OldBadger

    val abilities = List(Armored, DaylightCraft)

    override val transports : List[List[Transport]] = $($(RuledMove, CaravanMove))

    val relics = OldTablet * 4 ++ OldJewel * 4 ++ OldIdol * 4

    val pieces = OldBadger ** 15 ++ Caravan ** 3 ++ Station ** 5 ++ OldTablet ** 4 ++ OldJewel ** 4 ++ OldIdol ** 4
}

case object Armored extends BattleEffect

case object OldBadger extends Warrior {
    override def name = "Old Badger"
}

case object Caravan extends Token with Movable
case object Station extends Building

trait OldRelic extends Token

case object OldTablet extends OldRelic with Movable
case object OldJewel extends OldRelic with Movable
case object OldIdol extends OldRelic with Movable

trait Reward
case object RewardWarrior extends Reward
case object RewardCard extends Reward
case object RewardTwoCards extends Reward

object OldRelics {
    def all = List(OldTablet, OldJewel, OldIdol)
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

    def advertising = Caravan.img(this).repeat(3).merge
    def motto = "Delve".styled(this)
}

class OldExpeditionPlayer(val game : Game, val faction : OldExpedition) extends PlayerState {
    var acted = 0
    var mission : Mission = null

    val revealed = cards("revealed")

    var caravaned : List[Clearing] = Nil
    var stationed : List[Clearing] = Nil
    
    var rewards = List[((OldRelic, Suit), Reward)](
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
    var unrecovered : List[(Clearing, OldRelic)] = Nil

    def stations = all(Station)
    def caravans = all(Caravan)

    def craft = stations./(game.mapping) ++ caravans./(game.mapping)
}

case object CaravanMove extends Transport {
    override def allows(g : Game, f : Faction, o : Region) = (f, o) @@ {
        case (f : OldExpedition, o : Clearing) => 
            import g._

            f.at(o).warrior.any
        case _ => false
    }
    
    override def allows(g : Game, f : Faction, o : Region, d : Region, l : List[Movable]) = allows(g, f, o, d) && ({
        val warriors = l.count(OldBadger)
        val caravan = l.count(Caravan)
        val relics = l.count(OldTablet) + l.count(OldIdol) + l.count(OldJewel)

        ((caravan == 0 && warriors == 1) || (caravan == 1 && warriors >= 1 && warriors <= 3)) && (relics <= warriors)
    })
    
    override def order(l : List[Movable]) : Int = l.count(Caravan) * 900 + l.count(OldBadger) * 9 + l.count(OldTablet) + l.count(OldIdol) + l.count(OldJewel)
    override def sortBy(m : Movable) : Int = (m == Caravan).??(-2) + (m == OldBadger).??(-1)
}



trait OKDaylightQuestion extends FactionAction {
    override def self : OldExpedition
    def question(g : Game) = self.elem ~ " (" ~ "Daylight".styled(styles.phase) ~ ") " ~ (g.ok(self).acted < 4).?((g.ok(self).acted + 1).hl ~ " / " ~ (4).elem).|("extra")
}

object OldD1000 extends Die[Int](0.until(1000).toList)

case class OldRelicRandomnessAction(f : OldExpedition, r : Clearing, rolled : List[Int]) extends RolledAction[Int]

case class ChooseMissionAction(f : OldExpedition, then : ForcedAction) extends ForcedAction
case class SelectMissionAction(self : OldExpedition, m : Mission, then : ForcedAction) extends BaseAction("Choose", "Mission".styled(self))(m.name.styled(self))

case class OldBadgerRevealCardMainAction(self : OldExpedition, s : BaseSuit, then : ForcedAction, alt : UserAction) extends ForcedAction with Soft
case class OldBadgerRevealCardAction(self : OldExpedition, s : BaseSuit, d : DeckCard, then : ForcedAction) extends BaseAction("Reveal", s.elem, "card")(d.img) with ViewCard

case class OldBadgerRecruitMainAction(f : OldExpedition) extends ForcedAction
case class RecruitOldBadgersAction(self : Faction, l : List[Clearing], r : List[Clearing]) extends BaseAction("Recruit in")(l)

case class OldBadgerBuildMainAction(f : OldExpedition) extends ForcedAction
case class PlaceCaravanMainAction(self : OldExpedition, c : Clearing) extends BaseAction("Start", Caravan.of(self), "in")(c) with Soft
case class PlaceCaravanAction(self : OldExpedition, c : Clearing) extends ForcedAction
case class OldBuildStationMainAction(self : OldExpedition, c : Clearing) extends BaseAction("Build", Station.of(self), "in")(c) with Soft
case class OldBuildStationAction(self : OldExpedition, c : Clearing) extends ForcedAction

case class OldBadgerMainAction(f : OldExpedition) extends ForcedAction
case class OldBadgerMainDoneAction(f : OldExpedition) extends ForcedAction

case class OldBadgerAttackAction(self : OldExpedition, l : List[Clearing]) extends OptionAction("Battle".styled(self)) with OKDaylightQuestion with Soft
case class OldBadgerMissionAttackAction(self : OldExpedition, l : List[Clearing]) extends OptionAction("Mission Battle".styled(self)) with OKDaylightQuestion with Soft

case class OldBadgerMoveAction(self : OldExpedition, l : List[Clearing]) extends OptionAction("Move".styled(self)) with OKDaylightQuestion with Soft
case class OldBadgerMissionMoveAction(self : OldExpedition, l : List[Clearing]) extends OptionAction("Mission Move".styled(self)) with OKDaylightQuestion with Soft

case class OldBadgerEscortMainAction(self : OldExpedition, l : List[Clearing]) extends OptionAction("Escort".styled(self)) with OKDaylightQuestion with Soft
case class OldBadgerEscortAction(self : OldExpedition, c : Clearing) extends BaseAction("Escort".styled(self), "from")(c)

case class OldBadgerDelveMainAction(self : OldExpedition, l : List[Clearing]) extends OptionAction("Delve".styled(self)) with OKDaylightQuestion with Soft
case class OldBadgerDelveAction(self : OldExpedition, c : Clearing) extends BaseAction("Delve".styled(self), "in")(c) with Soft
case class OldBadgerDelveContinueAction(self : OldExpedition, c : Clearing) extends ForcedAction with Soft
case class OldBadgerDelveSelectAction(self : OldExpedition, c : Clearing, q : Forest, r : OldRelic) extends BaseAction("Delve".styled(self), "in", c)(r.of(self), "from", g => g.board.forestName(q, g, Some(c))) with Soft
case class OldBadgerDelveRelicAction(self : OldExpedition, c : Clearing, q : Forest, r : OldRelic) extends ForcedAction

case class OldBadgerRecoverMainAction(self : OldExpedition) extends OptionAction("Recover".styled(self)) with OKDaylightQuestion with Soft
case class OldBadgerRecoverContinueAction(self : OldExpedition) extends ForcedAction with Soft
case class OldBadgerRecoverAction(self : OldExpedition, c : Clearing, r : OldRelic) extends BaseAction("Recover".styled(self))("Recover".styled(self), r.of(self), "in", c)
case class OldBadgerRecoverRolledAction(self : OldExpedition, c : Clearing, r : OldRelic, min : Int, max : Int) extends RolledAction[Int] {
    def rolled = List(min, max)
}
case class OldBadgerRecoverSuccessAction(self : OldExpedition, c : Clearing, r : OldRelic) extends ForcedAction
case class OldBadgerRecoverAsAction(self : OldExpedition, c : Clearing, r : OldRelic, s : Suit) extends BaseAction("Recover".styled(self), r.of(self), "in", c)("As", s)
case class OldBadgerRecoverFailAction(self : OldExpedition, c : Clearing, r : OldRelic) extends ForcedAction
case class OldBadgerRecoverAmendAction(self : OldExpedition, c : Clearing, r : OldRelic, current : Int, target : Int) extends ForcedAction

case class MissionBattleFromAction(self : OldExpedition, c : Clearing) extends BaseAction(self, "battles on a mission in")(c) with Soft
case class MissionBattleFromDiscardAction(self : OldExpedition, c : Clearing) extends ForcedAction

case class MissionMoveFromAction(self : OldExpedition, c : Clearing) extends BaseAction(self, "moves on a mission from")(c) with Soft
case class MissionMoveFromDiscardAction(self : OldExpedition, c : Clearing) extends ForcedAction

case class OldReturnToWoodAction(self : Faction, c : Clearing, e : OldExpedition, p : OldRelic, q : Forest, then : ForcedAction) extends BaseAction("Return", p.of(e), "to forest")(g => g.board.forestName(q, g))

case class ForMissionBattle(c : Clearing) extends Message {
    def elem(g : Game) = " to battle in " ~ c.elem(g)
}

case class ForMissionMove(c : Clearing) extends Message {
    def elem(g : Game) = " to move from " ~ c.elem(g)
}

case class ToRecover(f : Faction, c : Clearing, r : OldRelic) extends Message {
    def elem(g : Game) = " to recover " ~ r.of(f) ~ " in " ~ c.elem(g)
}

case class ToPlaceCaravan(f : Faction, c : Clearing) extends Message {
    def elem(g : Game) = " to place " ~ Caravan.of(f) ~ " in " ~ c.elem(g)
}

object OldExpeditionExpansion extends Expansion {
    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            // SETUP
            case CreatePlayerAction(f : OldExpedition) =>
                pstates += f -> new OldExpeditionPlayer(game, f)
                FactionInitAction(f)
                
            case FactionSetupAction(f : OldExpedition) =>
                StartingCornerAction(f)

            case StartingClearingAction(f : OldExpedition, c) =>
                Force(PlaceStartingTokenAction(f, c, c, Caravan))
                
            case PlaceStartingTokenAction(f : OldExpedition, r, c, Caravan) =>
                f.pool.one(Caravan) --> c
                f.pool.sub(2, OldBadger) --> c
            
                f.log("placed", List(Caravan.of(f), OldBadger.of(f), OldBadger.of(f)).comma, "in", c)

                if (r == c) 
                    Ask(f, board.connected(c).diff(board.inner)./(PlaceStartingTokenAction(f, c, _, Caravan)))
                else
                    Roll[Int](0.until(24)./(_ => OldD1000).toList, l => OldRelicRandomnessAction(f, r, l))

            case OldRelicRandomnessAction(f, r, l) =>
                val randomness = new Randomness(l ++ l)

                var nf = board.forests.%(q => board.fromForest(q).has(r)).shuffleWith(randomness)
                
                while (board.forests.diff(nf).any) {
                    nf ++= board.forests.diff(nf).%(q => nf.%(w => fromForest(w).intersect(fromForest(q)).num > 1).any).shuffleWith(randomness)
                }

                var rl = f.relics.shuffleWith(randomness)
                
                rl.lazyZip(board.forests.shuffleWith(randomness) ++ nf.reverse).foreach { case (a, q) =>
                    f.pool.one(a) --> q
                }
                
                ChooseMissionAction(f, SetupNextAction)

            // HELPER
            case OldBadgerRevealCardMainAction(f, s, then, alt) =>
                Ask(f, f.hand./(d => OldBadgerRevealCardAction(f, s, d, then).x(d.suit.m(s).not)).:+(alt))
                
            case OldBadgerRevealCardAction(f, _, d, then) =>
                f.hand --> d --> f.revealed

                f.log("revealed", d)
                
                then
            
            case ChooseMissionAction(f, then) =>
                Ask(f, SelectMissionAction(f, MissionCraft, then) :: SelectMissionAction(f, MissionMoveBattle, then) :: SelectMissionAction(f, MissionEscort, then))
            
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
 
            case BattlePostHitInAction(b, e, f : OldExpedition, OldBadger @ p, then) =>
                e.log("pinched", p.of(f))
                then
 
            case BattlePostHitInAction(b, e, f : OldExpedition, Caravan @ p, then) =>
                e.log("looted", p.of(f))
                then
 
            case BattlePostHitInAction(b, e, f : OldExpedition, Station @ p, then) =>
                e.log("marauded", p.of(f))
                then
 
            case ForcedRemoveEffectAction(e, c, f : OldExpedition, p : OldRelic, then) =>
                e.oscore(clearings.%(_.suit.m(c.suit)).%(rule(e)).num)("returning the relic", p.of(f))
                Ask(e, board.forests./(q => OldReturnToWoodAction(e, c, f, p, q, then)))
            
            case OldReturnToWoodAction(e, c, f, p, q, then) =>
                e.log("returned", p.of(f), "to", q)
                f.limbo.one(p) --> q
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
                val ct = f.inpool(Caravan).??(clearings.diff(f.caravaned).%(canPlace(f)).%(c => f.at(c).warrior.any))
                val st = f.inpool(Station).??(clearings.diff(f.stationed).%(canBuild(f)).%(c => f.at(c).warrior.any).%(c => f.at(c).got(Caravan)))
                
                Ask(f, (ct./(c => PlaceCaravanMainAction(f, c).x(f.hand.%(_.suit.m(c.suit)).none, "no matching card")) ++ st./(c => OldBuildStationMainAction(f, c).x(f.hand.%(_.suit.m(c.suit)).none, "no matching card"))).done(Next).birdsong(f))

            case PlaceCaravanMainAction(f, c) =>
                OptionalDiscardCardAction(f, ToPlaceCaravan(f, c), c.suit, PlaceCaravanAction(f, c))
                
            case PlaceCaravanAction(f, c) =>
                f.drawn --> discard(f)

                f.pool.one(Caravan) --> c

                f.log("started", Caravan.of(f), "in", c)

                Repeat
                
            case OldBuildStationMainAction(f, c) =>
                OldBadgerRevealCardMainAction(f, c.suit, OldBuildStationAction(f, c), CancelAction)

            case OldBuildStationAction(f, c) =>
                f.at(c).one(Caravan) --> f.pool
                f.pool.one(Station) --> c

                f.log("built", Station.of(f), "in", c)

                Repeat
                
            case BirdsongNAction(80, f : OldExpedition) =>
                log("Recruit")

                Next

            case BirdsongNAction(81, f : OldExpedition) =>
                OldBadgerRecruitMainAction(f)
 
            case OldBadgerRecruitMainAction(f) =>
                val t = f.pooled(OldBadger)
                val r = f.all(Station).%(canPlace(f))
                
                if (t >= r.num)
                    Ask(f, RecruitOldBadgersAction(f, r, r) :: Nil)
                else {
                    val c = r.combinations(t).toList
                    if (c.num > 1)
                        Ask(f, c./(RecruitOldBadgersAction(f, _, r)).birdsong(f))
                    else
                        Ask(f, c./(RecruitOldBadgersAction(f, _, r)))
                 }
                    
            case RecruitOldBadgersAction(f, l, r) =>
                l.distinct.foreach { c => 
                    val ll = l.%(_ == c)
                    ll.foreach(r => f.pool.one(OldBadger) --> r)
                    f.log("recruited", ll./(_ => OldBadger)./(_.of(f)).comma ~ " in", c)
                }

                if (r.num > l.num && f.totalWar)
                    f.oscore(r.num - l.num)("recruiting") 
     
                Next                
                
            case DaylightNAction(50, f : OldExpedition) =>
                OldBadgerMainAction(f)
                
            case OldBadgerMainAction(f) =>
                var actions : List[UserAction] = Nil
                
                val esc = clearings.%(c => f.at(c).warrior.any)
                
                val att = clearings.%(c => attack(f)(c).any)
                actions :+= OldBadgerAttackAction(f, att).x(att.none)
                    
                val mvv = moveFrom(f)
                actions :+= OldBadgerMoveAction(f, mvv).x(mvv.none)

                actions :+= OldBadgerEscortMainAction(f, esc).x(esc.none, "no warriors").x(deck.none && pile.none, "no cards available")
                
                val dlv0 = clearings
                val dlv1 = dlv0.%(c => board.forests.%(q => fromForest(q).has(c)).%(q => f.at(q).any).any)
                val dlv2 = dlv1.%(rule(f))
                val dlv3 = dlv2.%(c => f.hand.%(_.suit.m(c.suit)).any)
 
                actions :+= OldBadgerDelveMainAction(f, dlv3).x(dlv1.none, "no relics").x(dlv2.none, "no rule").x(dlv3.none, "no matching cards")
 

                val relics = OldRelics.all./(r => r -> f.all(r)).toMap
                
                val rcv0 = f.all(Station)
                val rcv1 = rcv0.%(c => OldRelics.all./(r => f.all(r)).%(_.has(c)).any)
                val rcv2 = rcv1.%(c => OldRelics.all./(r => f.all(r).has(c) && (f.rewards.contains((r, c.suit)) || f.rewards.contains((r, Bird)))).any)
                
                actions :+= OldBadgerRecoverMainAction(f).x(rcv0.none, "no stations").x(rcv1.none, "no relics").x(rcv2.none, "no slots")

                if (f.acted >= 4)
                    actions = Nil
                     
                if (f.mission == MissionMoveBattle) {
                    val att = clearings.%(c => attack(f)(c).any)
                    actions :+= OldBadgerMissionAttackAction(f, att).x(att.none).x(att./(_.suit).distinct.%(s => f.hand.%(_.suit.m(s)).any).none, "no matching cards")
                    
                    val mvv = moveFrom(f)
                    actions :+= OldBadgerMissionMoveAction(f, mvv).x(mvv.none).x(mvv./(_.suit).distinct.%(s => f.hand.%(_.suit.m(s)).any).none, "no matching cards")
                }
                
                Ask(f, actions.done(Next).daylight(f))

            case OldBadgerAttackAction(f, l) =>
                BattleInitAction(f, NoMessage, l, $(CancelAction), OldBadgerMainDoneAction(f))
                
            case OldBadgerMoveAction(f, l) =>
                MoveInitAction(f, Nil, NoMessage, l, movable(f), CancelAction +: f.daylight, OldBadgerMainDoneAction(f))
                
            case OldBadgerMissionAttackAction(f, l) =>
                Ask(f, l./(c => MissionBattleFromAction(f, c).x(f.hand.%(_.suit.m(c.suit)).none)).cancel)

            case MissionBattleFromAction(f, c) =>
                OptionalDiscardCardAction(f, ForMissionBattle(c), c.suit, MissionBattleFromDiscardAction(f, c))
                
            case MissionBattleFromDiscardAction(f, c) =>
                f.drawn --> discard(f)

                BattleInitAction(f, NoMessage, $(c), $(DoneAction(OldBadgerMainAction(f))), OldBadgerMainAction(f))
 
            case OldBadgerMissionMoveAction(f, l) =>
                Ask(f, l./(c => MissionMoveFromAction(f, c).x(f.hand.%(_.suit.m(c.suit)).none)).cancel)

            case MissionMoveFromAction(f, c) =>
                OptionalDiscardCardAction(f, ForMissionMove(c), c.suit, MissionMoveFromDiscardAction(f, c))
                
            case MissionMoveFromDiscardAction(f, c) =>
                f.drawn --> discard(f)
                
                MoveInitAction(f, Nil, NoMessage, List(c), movable(f), DoneAction(OldBadgerMainAction(f)) +: f.daylight, OldBadgerMainAction(f))
                
            case OldBadgerEscortMainAction(f, l) =>
                Ask(f, l./(OldBadgerEscortAction(f, _)).cancel)

            case OldBadgerEscortAction(f, c) =>
                f.at(c).one(OldBadger) --> f.pool
                
                DrawCardsAction(f, 1 + (f.mission == MissionEscort).??(1), NoMessage, AddCardsAction(f, OldBadgerMainDoneAction(f)))

            case OldBadgerDelveMainAction(f, l) =>
                Ask(f, l./(OldBadgerDelveAction(f, _)).cancel)

            case OldBadgerDelveAction(f, c) =>
                OldBadgerDelveContinueAction(f, c)

            case OldBadgerDelveContinueAction(f, c) =>
                Ask(f, f.hand.%(_.suit.m(c.suit)).any.??(board.forests.%(q => fromForest(q).has(c))./~(q => f.at(q)./(p => OldBadgerDelveSelectAction(f, c, q, p.piece.asInstanceOf[OldRelic])))).cancelIf(f.delving.not).done(f.delving.?(OldBadgerMainDoneAction(f))))
                
            case OldBadgerDelveSelectAction(f, c, q, r) =>
                OldBadgerRevealCardMainAction(f, c.suit, OldBadgerDelveRelicAction(f, c, q, r), f.delving.?(CancelAction).|(CancelAction))
            
            case OldBadgerDelveRelicAction(f, c, q, r) =>
                f.delving = true

                f.at(q).one(r) --> c
                
                f.log("delved", r.of(f), "in", c)
                
                Force(OldBadgerDelveAction(f, c))

            case OldBadgerRecoverMainAction(f) =>
                 OldBadgerRecoverContinueAction(f)

            case OldBadgerRecoverContinueAction(f) =>
                val rcv0 = f.all(Station)
                val rcv1 = rcv0.%(c => OldRelics.all./(r => f.all(r)).%(_.has(c)).any)
                val rcv2 = rcv1.%(c => OldRelics.all./(r => f.all(r).has(c) && (f.rewards.contains((r, c.suit)) || f.rewards.contains((r, Bird)))).any)

                Ask(f, f.all(Station).distinct./~(c => OldRelics.all./~(r => 1.to(f.at(c).count(r) - f.unrecovered.count((c, r)))./(_ => OldBadgerRecoverAction(f, c, r).x(f.rewards.contains((r, c.suit)).not && f.rewards.contains((r, Bird)).not, "no slot")))).cancelIf(f.recovering.not).done(f.recovering.?(OldBadgerMainDoneAction(f))))
                
            case OldBadgerRecoverAction(f, c, r) =>
                Roll[Int](D4 :: D4, l => OldBadgerRecoverRolledAction(f, c, r, l(0), l(1)), f.elem ~ " recovers " ~ r.of(f) ~ " in " ~ c.elem(game))

            case OldBadgerRecoverRolledAction(f, c, r, a, b) =>
                f.recovering = true

                f.log("rolled", a.roll, " and ", b.roll)
                
                val rules = clearings.%(_.suit.m(c.suit)).%(rule(f))

                f.log("rules", rules.num.hl, "clearings of", c.suit)

                if (rules.num >= max(a, b))
                    OldBadgerRecoverSuccessAction(f, c, r)
                else
                    OldBadgerRecoverAmendAction(f, c, r, rules.num, max(a, b))

            case OldBadgerRecoverSuccessAction(f, c, r) =>
                f.drawn --> discard(f)

                Ask(f, ((r, c.suit) :: (r, Bird)).%(f.rewards.contains)./(_._2)./(s => OldBadgerRecoverAsAction(f, c, r, s)))

            case OldBadgerRecoverAsAction(f, c, r, s) =>
                f.at(c).one(r) --> f.pool

                f.log("recovered", r.of(f), "from", c, "as", s)
                
                f.oscore(2)("recovering", r.of(f), "from", c)

                val w = f.rewards((r, s))

                f.rewards -= ((r, s))

                if (f.rewards.keys.toList.%(_._1 == r).none)
                    f.oscore(3)("recovering all", (r.name + "s").styled(f))

                w match {
                    case RewardCard =>
                        DrawCardsAction(f, 1, NoMessage, AddCardsAction(f, OldBadgerRecoverContinueAction(f)))
                    
                    case RewardTwoCards =>
                        DrawCardsAction(f, 2, NoMessage, AddCardsAction(f, OldBadgerRecoverContinueAction(f)))

                    case RewardWarrior =>
                        f.pool.sub(1, f.warrior) --> c
    
                        f.log("placed", f.warrior.of(f), "in", c)

                        OldBadgerRecoverContinueAction(f)
                }
                
            case OldBadgerRecoverFailAction(f, c, r) =>
                f.unrecovered :+= (c, r)

                OldBadgerRecoverContinueAction(f)

            case OldBadgerRecoverAmendAction(f, c, r, x, t) =>
                OpportunityDiscardCardsAction(f, ToRecover(f, c, r), t - x, Nil, c.suit, OldBadgerRecoverSuccessAction(f, c, r), OldBadgerRecoverFailAction(f, c, r))
            
            case OldBadgerMainDoneAction(f) =>
                f.acted += 1 

                f.recovering = false
                f.delving = false
                f.unrecovered = Nil

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
                EndPlayerTurnAction(f)
                
            case _ => UnknownContinue
        }
    }
}

