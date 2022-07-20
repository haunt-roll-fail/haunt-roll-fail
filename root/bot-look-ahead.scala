package root

import root.gaming._

import colmat._

import hrf.base._
import hrf.bot._

class BotTT(faction : Faction, look : Int, universes : Int, breadth : Int, b : Faction => Bot) extends EvalBot {
    
    def score(g : Game)(f : Faction) : Int = {
        import g._

        (f match {
            case f : Feline => (f.hand.num - 3) * 4 + (f.all(Cat).num - 11) * 2 + (f.all(Recruiter).num - 1) * 3 + (f.all(Workshop).num - 1) * 2 + (f.all(Sawmill).num - 1) * 2
            case f : Aviary => (f.hand.num - 3) * 3 + (f.all(Hawk).num - 6) * 1 + (f.all(Roost).num - 1) * 5
            case f : Insurgent => (f.hand.num - 3) * 2 + (f.supporters.num - 3) * 3 + f.officers.num * 5 + f.all(Critter).num * 2 + f.all(Sympathy).num * 5
            case f : Hero => (f.hand.num - 3) * 3 + (f.quests.num * 5) + (f.inv.num - 4) * 20 + (f.inv.notExhausted.num - 4) * 1 + (f.inv.notDamaged.num - 4) * 2 - f.attitude.values.%(_ == Hostile).num * 12 - f.wasted.num * 15
        }) + (g.of(f).coalition match {
            case Some(e) => score(g)(e)
            case None => (g.of(f).dominance match {
                case Some(Dominance(Bird)) =>
                    List(0, 5, 10)(g.board.diagonals./(c => g.rule(f)(c._1).??(1) + g.rule(f)(c._2).??(1)).max)
                case Some(Dominance(s)) =>
                    List(0, 4, 8, 12, 16)(g.board.clearings.%(_.suit.m(s)).%(g.rule(f)).num)
                case None =>
                    g.of(f).vp
            }) * 10
        })
    }
    
    def relative(g : Game)(f : Faction) = score(g)(f) - g.factions.but(f).but(g.of(f).coalition).%(v => g.of(v).coalition != Some(f)).%(e => g.of(e).vp > 9 || g.of(e).dominance.any)./(score(g)).maxOr(0)
            
    def eval(game : Game, actions : List[UserAction]) : List[ActionEval] = {
        if (actions.num == 0)
            return actions./(ActionEval(_, Evaluation(0, "single") :: Nil))

        val initial = if (game.pstates.keys.num == game.factions.num) {
            relative(game)(faction)
        } else 0

        val start = game.turn
    
        val gcs = List.tabulate(universes)(_ => game.cloned().cleanFor(faction))
        
        val bots = game.factions./(f => f -> b(f)).toMap

        var aa = actions.take(0)
        
        while (aa.num < breadth && aa.num < actions.num) {
            aa :+= bots(faction).ask(game, actions.diff(aa), 0)
        }
        
        aa./(a => ActionEval(a, {
            val vps = gcs./{ gc =>
                val g = gc.cloned()
                                            
                def vp(c : Continue) = c match {
                    case GameOver(ww, _, _) => Some(ww.contains(faction).?(9999).|(-9999))
                    case Force(DaylightStartAction(e)) if look <= 0 => Some(relative(g)(faction))
                    case Force(EveningStartAction(e)) if look <= 0 => Some(relative(g)(faction))
                    case Force(EndPlayerTurnAction(e)) if look <= 1 => Some(relative(g)(faction))
                    case Force(EndPlayerTurnAction(e)) if look <= 2 && e == faction => Some(relative(g)(faction))
                    case Force(EndPlayerTurnAction(e)) if look <= 3 && e == faction && g.turn > start => Some(relative(g)(faction))
                    case _ => None
                }
            
                var c = g.perform(a)
            
                while (vp(c).none) {
                    c = c match {
                        case Ask(o, actions, _) => g.perform(bots(o.asInstanceOf[Faction]).ask(g, actions, 0))
                        case Force(a) => g.perform(a)
                        case DelayedContinue(_, c) => c
                        case Roll(dice, roll, _) => g.perform(roll(dice./(_.values.shuffle.head)))
                        case Shuffle(l, s, _) => game.perform(s(l.shuffle))
                        case Notify(_, _, then : ForcedAction) => g.perform(then)
                    }
                }
             
                vp(c).get
            }

            Evaluation(vps.sum, "vp at the end of turn " + initial + " > " + vps.mkString("|"))
        } :: Nil)) ++ actions.diff(aa)./(ActionEval(_, Evaluation(-10000, "ignored") :: Nil))
    }
    
}
      