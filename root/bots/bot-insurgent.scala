package root

import root.gaming._

import colmat._

import hrf.base._
import hrf.bot._

class GameEvaluationWA(game : Game, faction : Insurgent) extends GameEvaluation(game, faction) {
    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil
        
        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }
        
        val g = game

        import g._

        val wa = g.wa(self)
        val bases = wa.bases

        val others = g.factions.but(WA)

        a.unwrap match {
            case RevoltClearingAction(_, r, _, _, _, _) =>
                bases.none |=> (g.board.connected(r).num * 100) -> "connected"
                true |=> (others./(g.at(r, _)./~(_.scoring).num).sum * 200 - 100) -> "destruction"
                
            case SpreadSympathyClearingAction(_, r, _, n, _, _, _) =>
                true |=> (g.board.connected(r).num * 100) -> "connected"
                true |=> ((10 - n) * 1000) -> "cost"
            
            case CraftAssignAction(_, CraftItemCard(_, _, _, _, n, _), _, _, _, _) =>
                true |=> (n * 100) -> "vp"

            case CraftAssignAction(_, CraftEffectCard(_, _, _, _), _, _, _, _) =>
                true |=> 400 -> "effect"

            case MobilizeAction(_, List(d)) =>
                bases.none && d.suit == Bird |=> 1000 -> "no bases bird"
                bases.none && d.suit != Bird |=> 800 -> "no bases non-bird"
                bases.any && d.suit == Bird |=> -200 -> "bases bird"
                bases.any && bases./(_.suit).contains(d.suit) |=> -200 -> "bases match"
                bases.any && bases./(_.suit).contains(d.suit).not |=> 200 -> "bases unmatch"

                d == Ambush(Bird) |=> -2000 -> "keep ambush"
                d == Ambush(Fox) |=> -2000 -> "keep ambush"
                d == Ambush(Rabbit) |=> -2000 -> "keep ambush"
                d == Ambush(Mouse) |=> -2000 -> "keep ambush"

            case TrainMainAction(_, _) =>
                wa.officers.num + bases.num < 8 |=> 3000 -> "train"

            case RatRecruitClearingAction(_, r) =>
                g.at(r, WA)./~(_.warrior).num < 2 |=> 1000 -> "less than two"
                g.at(r, WA)./~(_.warrior).num < 3 |=> 400 -> "less than three"
                true |=> 100 -> "recruit"

            case RatOrganizeClearingAction(_, _) =>
                true |=> 500 -> "organize"

            case MoveListAction(_, _, _, from, to : Clearing, List(Critter), _) =>
                g.at(to, WA).none && g.canPlace(self)(to) && wa.acted + 3 < wa.officers.num |=> 1200 -> "go organize"
                g.at(to, WA).none && g.canPlace(self)(to) && wa.acted + 1 < wa.officers.num |=> 300 -> "go organize"
                    

            case DiscardCardAction(_, _, _, d, _, _) =>
                d.suit == Bird |=> -1000 -> "keep bird"
                
            case ActivateDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case TakeDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"
                
            case BattleAmbushAction(_, _, _, _, _) =>
                true |=> 1000 -> "ambush"
                
            case BattleCounterAmbushAction(_, _, _, _, _) =>
                true |=> 1000 -> "ct-ambush"

                
            case PayGiveWarriorsAction(_, s, n, _, GetServicesAction(_, _, List(offers))) =>
                true |=> 50 - (math.pow(10, 1 + 2 + n) * math.random()).round.toInt -> "cost"
                

            case _ =>
        }


        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"
        
        result.sortBy(v => -abs(v.weight))
    }

}
      