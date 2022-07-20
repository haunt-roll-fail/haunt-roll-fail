package root

import root.gaming._

import colmat._

import hrf.base._
import hrf.bot._

class GameEvaluationMC(game : Game, faction : Feline) extends GameEvaluation(game, faction) {
    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil
        
        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val g = game
        val mc = g.mc(self)
        
        val others = g.factions.but(faction).%(g.pstates.contains)./(g.of)
        
        def dtk(c : Clearing) = g.of(self).all(Keep).single./(g.board.distance(_)(c)).|(9)
        
        a.unwrap match {
            case BattleStartAction(f, _, _ , c, o, _, _) if o == WA =>
                val enemy = g.at(c, o)
                enemy./~(_.warrior).none |=> (1000 + g.at(c, MC)./~(_.scoring).num * 10) -> "defenseless WA"
                
            case BuildPayAction(f, Sawmill, _, c, l) =>
                true |=> (500 - dtk(c) * 10) -> "build sawmill"

            case BuildPayAction(f, Recruiter, _, c, l) =>
                true |=> 400 -> "build recruiter"

            case BuildPayAction(f, Workshop, _, c, l) =>
                true |=> (300 + dtk(c) * 10) -> "build workshop"

            case ActivateDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case TakeDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case RecruitCatsAction(f, l, _) =>
                true |=> (200 + l.num * 20)-> "recruit"
                
            case MoveListAction(f, _, _, c : Clearing, d : Clearing, l, _) =>
                val cw = g.at(c, MC)./~(_.warrior)
                val dw = g.at(d, MC)./~(_.warrior)
                abs((cw.num - l.num) - (dw.num + l.num)) <= 1 |=> (100 + l.num * 10) -> "balance warriors"

            case BattleAmbushAction(_, _, _, _, _) =>
                true |=> 1000 -> "ambush"

            case BattleCounterAmbushAction(_, _, _, _, _) =>
                true |=> 1000 -> "ct-ambush"

            case PayGiveWarriorsAction(_, s, n, _, GetServicesAction(_, _, List(offers))) =>
                true |=> 50 - (math.pow(10, 2 + n) * math.random()).round.toInt -> "cost"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"
        
        result.sortBy(v => -abs(v.weight))
    }

}
      