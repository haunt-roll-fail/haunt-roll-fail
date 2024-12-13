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

import hrf.base._
import hrf.bot._


class BotAdset(f : PlayerN) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        if (game.ptf.contains(f))
            new BotXX(game.ptf(f)).eval(actions)
        else
        actions./{ a =>
            ActionEval(a, a.unwrap match {
                case SelectFactionAction(_, _, FactionChoice(f : Feline)) => $(Evaluation(410, "cats"))
                case SelectFactionAction(_, _, FactionChoice(f : Aviary)) => $(Evaluation(420, "birds"))
                case SelectFactionAction(_, _, FactionChoice(f : Insurgent)) => $(Evaluation(430, "critters"))
                case SelectFactionAction(_, _, FactionCharacterChoice(f : Hero, _)) => $(Evaluation(440, "hero"))
                case _ => $(Evaluation((1 + math.random() * 500).~, "random"))
            })
        }
    }
}

class BotXX(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        val ev = f match {
            case f : Feline => new GameEvaluationFeline(f)
            case f : Aviary => new GameEvaluationAviary(f)
            case f : Insurgent => new GameEvaluationInsurgent(f)
            case f : Trader => new GameEvaluationTrader(f)
            case f : Hero => new GameEvaluationHero(f)
            case f : Fanatic => new GameEvaluationFanatic(f)
            case f : Underground => new GameEvaluationUnderground(f)
            case f : Mischief => new GameEvaluationMischief(f)
            case f : Expedition => new GameEvaluationExpedition(f)
            case f : OldExpedition => new GameEvaluationOldExpedition(f)
            case f : Horde => new GameEvaluationHorde(f)
            case f : Caster => new GameEvaluationCaster(f)
            case f : Utopia => new GameEvaluationUtopia(f)
            case f : Farmer => new GameEvaluationFarmer(f)
            case f : InvasiveAAA => new GameEvaluationInvasiveAAA(f)
            case f : InvasiveBBB => new GameEvaluationInvasiveBBB(f)
            case f : InvasiveCCC => new GameEvaluationInvasiveCCC(f)
            case f : InvasiveDDD => new GameEvaluationInvasiveDDD(f)
            case f : LegalAAA => new GameEvaluationLegalAAA(f)
            case f : AbductAAA => new GameEvaluationAbductAAA(f)
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

    implicit class FactionListClassify(val l : $[Faction]) {
    }

    implicit class RegionClassify(val r : Region) {
    }

    def eval(a : Action) : $[Evaluation]
}
