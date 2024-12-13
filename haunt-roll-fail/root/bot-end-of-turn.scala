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


class BotOT(f : Faction, b : Faction => Bot) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        val gc = game.cloned().cleanFor(f)

        val bots = game.factions./(f => f -> b(f)).toMap

        actions./{ a => ActionEval(a, ${
            implicit val game = gc.cloned()

            def vp(c : Continue) = c match {
                case GameOver(ww, _, _) => Some(ww.has(f).?(9999).|(-9999))
                case Force(EndPlayerTurnAction(e)) => Some(f.vp - game.factions.but(f)./(o => o.vp).max)
                case _ => None
            }

            var c = game.performContinue(None, a, false).continue

            while (vp(c).none) {
                c = c match {
                    case Ask(o, actions) => game.performContinue(None, bots(o.as[Faction].get).ask(actions, 0), false).continue
                    case DelayedContinue(_, c) => c
                    case Roll(dice, roll, _) => game.performContinue(None, roll(dice./(_.roll())), false).continue
                }
            }

            Evaluation(vp(c).get, "vp at the end of turn")
        })}
    }

}
