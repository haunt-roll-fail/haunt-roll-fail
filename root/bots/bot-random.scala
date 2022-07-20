package root

import root.gaming._

import colmat._

import hrf.base._
import hrf.bot._

class BotRR extends EvalBot {
    def eval(game : Game, actions : List[UserAction]) : List[ActionEval] = {                                        
        actions./{ a => ActionEval(a, Evaluation((random() * 10).round.toInt, "random") :: Nil) }
    }
}
      