package hrf.bot
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

trait BotGaming extends Gaming {
    case class Evaluation(weight : Int, desc : String)
    case class ActionEval(action : UserAction, evaluations : $[Evaluation])

    trait Bot {
        def ask(actions : $[UserAction], deviation : Double = 0.0)(implicit game : G) : UserAction
    }

    trait EvalBot extends Bot {
        def ask(actions : $[UserAction], deviation : Double = 0.0)(implicit game : G) : UserAction = {
            if (actions.none)
                throw new Error("empty actions ***")

            val aa = try {
                game.explode(actions, false, None).notOf[Hidden]
            }
            catch {
                case e : Exception =>
                    error(e)

                    warn("error on explode")

                    actions.foreach(a => +++(a))

                    ---
                    ---
                    ---
                    ---

                    actions.foreach {
                        case a : Cancel => Nil
                        case a : Info => Nil
                        case a : Soft =>
                            +++("soft", a)
                            game.explode($(a), false, None)
                        case _ =>
                    }

                    Nil
            }

            if (aa.none) {
                var l : $[String] = $
                l :+= ""
                l :+= ""
                l :+= ""
                actions.foreach { a =>
                    l :+= a.toString
                    l :+= ""
                    l :+= (game.explode($(a), false, None)).toString
                    l :+= ""
                    l :+= (game.explode($(a), false, None).notOf[Hidden]).toString
                }
                l :+= ""
                l :+= ""
                l :+= ""
                throw new Error("empty actions !!!\n" + actions./(_.toString).mkString("\n") + l.mkString("\n"))
            }

            askE(game, aa, deviation)
        }

        def askE(game : G, actions : $[UserAction], deviation : Double) : UserAction = {
            if (actions.none)
                throw new Error("empty actions ???")

            if (actions.num == 1)
                return actions.head

            val eas = eval(actions)(game)
            val o = eas.sortWith(compare)

            var v = o
            while (deviation > 0 && random() < deviation) {
                v = v.drop(1)
                if (v.none)
                    v = o
            }
            v.head.action
        }

        def sortByAbs(a : $[Int]) : $[Int] =
            a.sortBy(v => -v.abs)

        def compareEL(aaa : $[Int], bbb : $[Int]) : Int =
            (aaa, bbb) match {
                case (a :: aa, b :: bb) => (a == b).?(compareEL(aa, bb)).|((a > b).?(1).|(-1))
                case (0 :: _, Nil) => 0
                case (Nil, 0 :: _) => 0
                case (a :: _, Nil) => (a > 0).?(1).|(-1)
                case (Nil, b :: _) => (0 > b).?(1).|(-1)
                case (Nil, Nil) => 0
            }

        def compare(a : ActionEval, b : ActionEval) = compareEL(sortByAbs(a.evaluations./(_.weight)), sortByAbs(b.evaluations./(_.weight))) > 0

        def eval(actions : $[UserAction])(implicit game : G) : $[ActionEval]
    }

    case class DebugBot(list : $[UserAction] => $[ActionEval]) extends AskResult
}
