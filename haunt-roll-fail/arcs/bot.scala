package arcs
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
        val ev = new GameEvaluation(f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluation(val self : Faction)(implicit val game : Game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        if (game.states.contains(self).not)
            return $

        def appraise(x : Cost) {
            x @@ {
                case Pip =>
                case NoCost =>
                case AlreadyPaid =>
                case PayResource(resource, |(lock)) =>
                    true |=> -lock*100 -> "lock"
            }
        }

        a.unwrap @@ {
            case EndTurnAction(_) =>
                true |=> -1000 -> "dont skip actions"

            case BattleDiceAction(f, cost, s, e, n1, n2, n3, _) =>
                val k = f.at(s).ships.num
                true |=> k * (n1 + n2 + n3) * 100 -> "battle"

            case SecureAction(f, cost, c, _) =>
                true |=> Influence(c).$.%(_.faction == f).num * 10 -> "return agents"
                true |=> Influence(c).$.%(_.faction != f).num * 100 -> "capture agents"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
