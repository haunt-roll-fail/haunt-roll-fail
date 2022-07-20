package root

import root.gaming._

import colmat._

import hrf.base._
import hrf.bot._

class BotOT(f : Faction, b : Faction => Bot) extends EvalBot {
    def eval(game : Game, actions : List[UserAction]) : List[ActionEval] = {                                        
        val gc = game.cloned().cleanFor(f)
        
        val bots = game.factions./(f => f -> b(f)).toMap

        actions./{ a => ActionEval(a, {   
            val g = gc.cloned()

            def vp(c : Continue) = c match {
                case GameOver(ww, _, _) => Some(ww.contains(f).?(9999).|(-9999))
                case Force(EndPlayerTurnAction(e)) => Some(g.of(f).vp - g.factions.but(f)./(o => g.of(o).vp).max)
                case _ => None
            }

            var c = g.perform(a)
  
            while (vp(c).none) {
                c = c match {
                    case Ask(o, actions, _) => g.perform(bots(o.asInstanceOf[Faction]).ask(g, actions, 0))
                    case Force(a) => g.perform(a)
                    case DelayedContinue(_, c) => c
                    case Roll(dice, roll, _) => g.perform(roll(dice./(_.values.shuffle.head)))
                    case Notify(_, _, then : ForcedAction) => g.perform(then)
                }
            }

            Evaluation(vp(c).get, "vp at the end of turn")
        } :: Nil)}
    }
    
}
      