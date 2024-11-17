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


class GameEvaluationFeline(faction : Feline)(implicit game : Game) extends GameEvaluation(game, faction) {
    def eval(a : Action) : List[Evaluation] = {
        if (game.states.contains(self).not)
          return $(Evaluation(-1 - (math.random() * 7).~, "proto random"))

        var result : List[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val g = game
        val mc = self

        def dtk(c : Clearing) = self.all(Keep).single./(g.board.distance(_)(c)).|(9)

        a.unwrap match {
            case BattleStartAction(_, _, _, _ , c, o, _, _) if o == WA =>
                val enemy = o.at(c)
                enemy.of[Warrior].none |=> (1000 + self.at(c).of[Scoring].num * 10) -> "defenseless WA"

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

            case MoveListAction(_, _, _, _, c : Clearing, d : Clearing, l, _) =>
                val cw = self.at(c).of[Warrior]
                val dw = self.at(d).of[Warrior]
                ((cw.num - l.num) - (dw.num + l.num)).abs <= 1 |=> (100 + l.num * 10) -> "balance warriors"

            case BattleAmbushAction(_, _, _, _, _, _) =>
                true |=> 1000 -> "ambush"

            case BattleCounterAmbushAction(_, _, _, _, _, _) =>
                true |=> 1000 -> "ct-ambush"

            case PayGiveWarriorsAction(_, s, n, _, GetServicesAction(_, _, List(offers))) =>
                true |=> 50 - (math.pow(10, 2 + n) * math.random()).round.toInt -> "cost"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }

}
