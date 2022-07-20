package dwam

import colmat._

class BotXX(f : Faction) extends EvalBot {
    def eval(game : Game, actions : List[UserAction]) : List[ActionEval] = {
        val ev = new GameEvaluation(game, f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluation(val game : Game, val self : Faction) {
    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil
    
        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }
    
        import game._

        a match {
            case _ =>
        }       

        result.none |=> 0 -> "none"
      
        true |=> -((1 + math.random() * 7).round.toInt) -> "random"
        
        result.sortBy(v => -abs(v.weight))
    }
}
      