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

class BotXX(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        val ev =
            if (f == Amalthea || !true)
                new GameEvaluation03(game, f)
            else
                new GameEvaluation03(game, f)

        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

trait GEvaluation {
    def eval(a : Action) : $[Evaluation]
}
