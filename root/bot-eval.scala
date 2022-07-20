package root

import root.gaming._

import colmat._

import hrf.base._
import hrf.bot._

class BotXX(f : Faction) extends EvalBot {
    def eval(game : Game, actions : List[UserAction]) : List[ActionEval] = {                                        
        val ev = f match {
            case f : Feline => new GameEvaluationMC(game, f)
            case f : Aviary => new GameEvaluationED(game, f)
            case f : Insurgent => new GameEvaluationWA(game, f)
            case f : Trader => new GameEvaluationRF(game, f)
            case f : Hero => new GameEvaluationVB(game, f)
            case f : Fanatic => new GameEvaluationLC(game, f)
            case f : Underground => new GameEvaluationUD(game, f)
            case f : Mischief => new GameEvaluationCC(game, f)
            case f : Expedition => new GameEvaluationKI(game, f)
            case f : OldExpedition => new GameEvaluationOK(game, f)
            case f : Horde => new GameEvaluationLH(game, f)
        }
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

abstract class GameEvaluation[F <: Faction](val game : Game, val self : F) {
    val others = game.factions.%(_ != self)

    implicit class SelfFactionClassify(val f : F) {
    }

    implicit class FactionClassify(val f : Faction) {
    }

    implicit class FactionListClassify(val l : List[Faction]) {
    }    
    
    implicit class RegionClassify(val r : Region) {
    }

    def eval(a : Action) : List[Evaluation]
}
      