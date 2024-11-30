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


class BotTT(faction : Faction, look : Int, universes : Int, breadth : Int, b : Faction => Bot) extends EvalBot {
    def score(f : Faction)(implicit game : Game) : Int = {
        (f match {
            case f : Feline => (f.hand.num - 3) * 4 + (f.all(Cat).num - 11) * 2 + (f.all(Recruiter).num - 1) * 3 + (f.all(Workshop).num - 1) * 2 + (f.all(Sawmill).num - 1) * 2
            case f : Aviary => (f.hand.num - 3) * 3 + (f.all(Hawk).num - 6) * 1 + (f.all(Roost).num - 1) * 5
            case f : Insurgent => (f.hand.num - 3) * 2 + (f.supporters.num - 3) * 3 + f.officers.$.num * 5 + f.all(Critter).num * 2 + f.all(Sympathy).num * 5
            case f : Hero => (f.hand.num - 3) * 3 + (f.quests.num * 5) + (f.inv.num - 4) * 20 + (f.inv.notExhausted.num - 4) * 1 + (f.inv.notDamaged.num - 4) * 2 - f.attitude.values.%(_ == Hostile).num * 12 - f.wasted.num * 15
        }) + (f.coalition match {
            case Some(e) => score(e)
            case None => (f.dominance match {
                case Some(CornersDominance(Bird)) =>
                    $(0, 5, 10)(board.diagonals./((a, b) => f.rules(a).??(1) + f.rules(b).??(1)).max)
                case Some(BaseSuitDominance(s)) =>
                    $(0, 4, 8, 12, 16)(board.clearings.%(_.cost.matched(s)).%(f.rules).num)
                case None =>
                    f.vp
            }) * 10
        })
    }

    def relative(f : Faction)(implicit game : Game) = {
        score(f) - factions.but(f).but(f.coalition).%(v => v.coalition != Some(f)).%(e => e.vp > 9 || e.dominance.any)./(score).maxOr(0)
    }

    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        if (actions.num == 0)
            return actions./(ActionEval(_, $(Evaluation(0, "single"))))

        val initial = if (game.states.keys.num == game.factions.num) {
            relative(faction)
        } else 0

        val start = game.turn

        val gcs = List.tabulate(universes)(_ => game.cloned().cleanFor(faction))

        val bots = game.factions./(f => f -> b(f)).toMap

        var aa = actions.take(0)

        while (aa.num < breadth && aa.num < actions.num) {
            aa :+= bots(faction).ask(actions.diff(aa), 0)
        }

        aa./(a => ActionEval(a, ${
            val vps = gcs./{ gc =>
                val g = gc.cloned()

                def vp(c : Continue) = c match {
                    case GameOver(ww, _, _) => Some(ww.contains(faction).?(9999).|(-9999))
                    case Force(DaylightStartAction(e)) if look <= 0 => Some(relative(faction))
                    case Force(EveningStartAction(e)) if look <= 0 => Some(relative(faction))
                    case Force(EndPlayerTurnAction(e)) if look <= 1 => Some(relative(faction))
                    case Force(EndPlayerTurnAction(e)) if look <= 2 && e == faction => Some(relative(faction))
                    case Force(EndPlayerTurnAction(e)) if look <= 3 && e == faction && g.turn > start => Some(relative(faction))
                    case _ => None
                }

                var c = g.performContinue(None, a, false).continue

                while (vp(c).none) {
                    c = c match {
                        case Ask(o, actions) => g.performContinue(None, bots(o.asInstanceOf[Faction]).ask(actions, 0)(g), false).continue
                        case DelayedContinue(_, c) => c
                        case Roll(dice, roll, _) => g.performContinue(None, roll(dice./(_.roll())), false).continue
                        case Shuffle(l, s, _) => g.performContinue(None, s(l.shuffle), false).continue
                    }
                }

                vp(c).get
            }

            Evaluation(vps.sum, "vp at the end of turn " + initial + " > " + vps.mkString("|"))
        })) ++ actions.diff(aa)./(ActionEval(_, $(Evaluation(-10000, "ignored"))))
    }

}
