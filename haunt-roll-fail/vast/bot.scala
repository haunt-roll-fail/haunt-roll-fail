package vast
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

class BotXX(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        val ev = new GameEvaluation(game, f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluation(val game : Game, val self : Faction) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        import game._

        if (states.contains(self).not)
            return $

        a.unwrap match {
            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
