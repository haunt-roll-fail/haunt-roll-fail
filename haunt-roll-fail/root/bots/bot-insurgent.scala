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


class GameEvaluationInsurgent(faction : Insurgent)(implicit game : Game) extends GameEvaluation(game, faction) {
    def eval(a : Action) : List[Evaluation] = {
        if (game.states.contains(self).not)
          return $(Evaluation(-1 - (math.random() * 7).~, "proto random"))

        var result : List[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val wa = self
        val bases = wa.bases

        val others = game.factions.but(WA)

        a.unwrap match {
            case RevoltClearingAction(_, r, _, _, _, _) =>
                bases.none |=> (game.connected(r).num * 100) -> "connected"
                true |=> (others./(_.at(r).of[Scoring].num).sum * 200 - 100) -> "destruction"

            case SpreadSympathyClearingAction(_, r, _, n, _, _, _) =>
                true |=> (game.connected(r).num * 100) -> "connected"
                true |=> ((10 - n) * 1000) -> "cost"

            case CraftAssignAction(_, CraftItemCard(_, _, _, _, n, _), _, _, _, _) =>
                true |=> (n * 100) -> "vp"

            case CraftAssignAction(_, CraftEffectCard(_, _, _, _), _, _, _, _) =>
                true |=> 400 -> "effect"

            case MobilizeAction(_, List(d)) =>
                bases.none && d.suit == Bird |=> 1000 -> "no bases bird"
                bases.none && d.suit != Bird |=> 800 -> "no bases non-bird"
                bases.any && d.suit == Bird |=> -200 -> "bases bird"
                bases.any && wa.suits.contains(d.suit) |=> -200 -> "bases match"
                bases.any && wa.suits.contains(d.suit).not |=> 200 -> "bases unmatch"

                d == Ambush(Bird) |=> -2000 -> "keep ambush"
                d == Ambush(Fox) |=> -2000 -> "keep ambush"
                d == Ambush(Rabbit) |=> -2000 -> "keep ambush"
                d == Ambush(Mouse) |=> -2000 -> "keep ambush"

            case TrainMainAction(_, _) =>
                wa.officers.num + bases.num < 8 |=> 3000 -> "train"

            case InsurgentRecruitClearingAction(_, r) =>
                self.at(r).of[Warrior].num < 2 |=> 1000 -> "less than two"
                self.at(r).of[Warrior].num < 3 |=> 400 -> "less than three"
                true |=> 100 -> "recruit"

            case InsurgentOrganizeClearingAction(_, _) =>
                true |=> 500 -> "organize"

            case MoveListAction(_, _, _, _, from, to : Clearing, List(Critter), _) =>
                self.at(to).none && self.canPlace(to) && wa.acted + 3 < wa.officers.num |=> 1200 -> "go organize"
                self.at(to).none && self.canPlace(to) && wa.acted + 1 < wa.officers.num |=> 300 -> "go organize"

            case ActivateDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case TakeDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case BattleAmbushAction(_, _, _, _, _, _) =>
                true |=> 1000 -> "ambush"

            case BattleCounterAmbushAction(_, _, _, _, _, _) =>
                true |=> 1000 -> "ct-ambush"


            case PayGiveWarriorsAction(_, s, n, _, GetServicesAction(_, _, List(offers))) =>
                true |=> 50 - (math.pow(10, 1 + 2 + n) * math.random()).round.toInt -> "cost"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }

}
