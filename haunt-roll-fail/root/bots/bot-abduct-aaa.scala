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


class GameEvaluationAbductAAA(faction : AbductAAA)(implicit game : Game) extends GameEvaluation(game, faction) {
    def eval(a : Action) : $[Evaluation] = {
        if (game.states.contains(self).not)
            return $(Evaluation(-1 - (math.random() * 7).~, "proto random"))

        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val g = game

        val others = g.factions.but(faction)

        a.unwrap match {
            case ActivateDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case TakeDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

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
