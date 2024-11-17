package dwam
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

        val character = states(self).character

        if (character.none)
            return $

        implicit class charToEval(val c : Character) {
            def |=> (e : (Int, String)) { if (character.has(c)) result +:= Evaluation(e._1, e._2) }
            def not = character.has(c).not
        }

        a.unwrap match {
            case PlayCardAction(_, d, _) if d.actions.has(Scroll(TakeLoan)) =>
                Chrysoprase.not |=> 1000 -> "more loans"
                Chrysoprase |=> -1000 -> "no loans"

            case PlayCardAction(_, d, _) if d.actions.has(RemoveTrouble) =>
                DragonKingOfArms |=> -1000 -> "leave trouble be"

            case RemoveTroubleAction(_, _, _, _) =>
                DragonKingOfArms |=> -1000 -> "leave trouble be"

            case SkipActionMainAction(_) =>
                true |=> -500 -> "am no skipper"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
