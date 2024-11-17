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


class BotRR extends EvalBot {
    def eval(actions : List[UserAction])(implicit game : Game) : List[ActionEval] = {
        actions./{ a => ActionEval(a, Evaluation((random() * 10).round.toInt, "random") :: Nil) }
    }
}
