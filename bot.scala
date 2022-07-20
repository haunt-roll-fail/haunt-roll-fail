package hrf.bot
//
//
//
//
import logger._, colmat._
//
//
//
//

import hrf.base._

trait BotGaming extends Gaming {
    case class Evaluation(weight : Int, desc : String)
    case class ActionEval(action : UserAction, evaluations : List[Evaluation])

    trait Bot {
        def ask(game : G, actions : List[UserAction], error : Double) : UserAction
    }

    trait EvalBot extends Bot {
        def ask(game : G, actions : List[UserAction], error : Double) : UserAction = {
            if (actions.none)
                throw new Error("empty actions ***")
                
            val aa = try {
                game.explode(actions)
            }
            catch {
                case e =>
                    ===(e)

                    ===("error on explode")
                    
                    actions.foreach(===(_))
                    ===("")
                    ===("")
                    ===("")
                    ===("")

                    actions.foreach {
                        case a : Cancel => Nil
                        case a : Info => Nil
                        case a : Soft =>
                            ===(a)
                            game.explode(List(a))
                        case _ =>
                    }

                    Nil
            }
    
            if (aa.none)
                throw new Error("empty actions !!!")
                
            askE(game, aa, error)
        }
    
        def askE(game : G, actions : List[UserAction], error : Double) : UserAction = {
            if (actions.none)
                throw new Error("empty actions ---")
    
            if (actions.num == 1)
                return actions.head
                
            val eas = eval(game, actions)
            val o = eas.sortWith(compare)
    
            var v = o
            while (error > 0 && random() < error) {
                v = v.drop(1)
                if (v.none)
                    v = o
            }
            v.head.action
        }
    
        def sortByAbs(a : List[Int]) : List[Int] = 
            a.sortBy(v => -abs(v))
        
        def compareEL(aaa : List[Int], bbb : List[Int]) : Int =
            (aaa, bbb) match {
                case (a :: aa, b :: bb) => (a == b).?(compareEL(aa, bb)).|((a > b).?(1).|(-1))
                case (0 :: _, Nil) => 0
                case (Nil, 0 :: _) => 0
                case (a :: _, Nil) => (a > 0).?(1).|(-1)
                case (Nil, b :: _) => (0 > b).?(1).|(-1)
                case (Nil, Nil) => 0
            }
    
        def compare(a : ActionEval, b : ActionEval) = compareEL(sortByAbs(a.evaluations./(_.weight)), sortByAbs(b.evaluations./(_.weight))) > 0
    
        def eval(game : G, actions : List[UserAction]) : List[ActionEval]
    }
    
    case class DebugBot(list : List[UserAction] => List[ActionEval]) extends AskResult
}
