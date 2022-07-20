package root

import root.gaming._

import colmat._

import hrf.base._
import hrf.bot._

class GameEvaluationED(game : Game, faction : Aviary) extends GameEvaluation(game, faction) {
    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil
        
        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        case class CS(roost : Boolean, warriors : Int, opposition : Int, empty : Boolean, connected : List[Clearing])
        
        val g = game

        import g._

        val ed = g.ed(self)

        def pooled(p : Piece) = ed.pool.%(_.piece == p).num

        val others = g.factions.but(ED).%(g.pstates.contains)

        val maxcardloss = others.%(g.of(_).has(StandAndDeliver)).num + others.%(g.of(_).has(Steal)).num

        val all = g.board.clearings
        val state = all./(c => c -> CS(g.at(c, ED)./~(_.building).any, g.at(c, ED)./~(_.warrior).num, others./(g.at(c, _)./~(_.ruling).num).maxOr(0), g.canBuild(self)(c), g.board.connected(c))).toMap
        
        val rc = 1 + (ed.leader == Charismatic).??(1)
        
        var cache = Map[(Map[Clearing, CS], Map[Decree, List[Suit]]), Boolean]()
        
        def complete(state : Map[Clearing, CS], todo : Map[Decree, List[Suit]]) : Boolean = {
            val k = (state, todo)
            if (cache.contains(k))
                cache(k)
            else {
                val v = xcomplete(state, todo)
                cache += k -> v
                v
            }
        }
            
        def xcomplete(state : Map[Clearing, CS], todo : Map[Decree, List[Suit]]) : Boolean = {
            if (todo(Decree.Build).num + all.%(c => state(c).roost).num > 7)
                return false
 
            if (todo(Decree.Recruit).num * rc + state.values.toList./(_.warriors).sum > 20)
                false
                
            val phase = Decree.all.%(todo(_).any)
            if (phase.none)
                true
            else {
                phase.head match {
                    case Decree.Recruit =>
                        val r = todo(Decree.Recruit)
                        val x = List(Fox, Rabbit, Mouse).%(s => r.contains(s) || r.contains(Bird))
                        val m = (c : Clearing) => x.contains(c.suit)
                        
                        all.%(m).%(c => state(c).roost).foreach { c =>
                            val newtodo = todo  + (Decree.Recruit -> r.contains(c.suit).?(r :- c.suit).|(r :- Bird))

                            if (complete(state + (c -> state(c).copy(warriors = state(c).warriors + rc)), newtodo))
                                return true
                        }
                        false

                    case Decree.Move =>
                        val r = todo(Decree.Move)

                        val x = List(Fox, Rabbit, Mouse).%(s => r.contains(s) || r.contains(Bird))
                        val m = (c : Clearing) => x.contains(c.suit)

                        var v = all.%(m).%(c => state(c).warriors > 0)./~ { c =>
                            state(c).connected.%(t => state(c).warriors + state(c).roost.??(1) >= state(c).opposition || state(t).warriors + state(t).roost.??(1) >= state(t).opposition)./~ { t =>
                                List((c, t, max(1, state(c).warriors - (state(c).warriors + state(t).warriors) / 2)))
                            }
                        }

                        if (v.num > 8 - r.num)
                            v = v.shuffle.take(8 - r.num)

                        v.foreach { case (c, t, n) =>
                            val newtodo = todo  + (Decree.Move -> r.contains(c.suit).?(r :- c.suit).|(r :- Bird))
                            if (complete(state + (c -> state(c).copy(warriors = state(c).warriors - n)) + (t -> state(t).copy(warriors = state(t).warriors + n)), newtodo))
                                return true
                        }

                        false
                        
                    case Decree.Battle =>
                        val r = todo(Decree.Battle)
                        val x = List(Fox, Rabbit, Mouse).%(s => r.contains(s) || r.contains(Bird))
                        val m = (c : Clearing) => x.contains(c.suit)
                        all.%(m).%(c => state(c).warriors > 0).%(c => state(c).opposition > 0).foreach { c =>
                            val newtodo = todo  + (Decree.Battle -> r.contains(c.suit).?(r :- c.suit).|(r :- Bird))
                            if (complete(state + (c -> state(c).copy(warriors = state(c).warriors - min(2, state(c).opposition), opposition = state(c).opposition - 1, empty = state(c).empty || (state(c).opposition <= 1))), newtodo))
                                return true
                        }
                        false
                        
                    case Decree.Build =>
                        val r = todo(Decree.Build)
                        val x = List(Fox, Rabbit, Mouse).%(s => r.contains(s) || r.contains(Bird))
                        val m = (c : Clearing) => x.contains(c.suit)
                        all.%(m).%(c => state(c).empty).%(c => state(c).warriors > 0).%(c => state(c).roost.not).foreach { c =>
                            val newtodo = todo  + (Decree.Build -> r.contains(c.suit).?(r :- c.suit).|(r :- Bird))
                            if (complete(state + (c -> state(c).copy(roost = true)), newtodo))
                                return true
                        }
                        false
                        
                }
            }
        }
        
        val roosts = all.%(c => g.at(c, ED)./~(_.building).any)
        val warriors = all./~(c => g.at(c, ED)./~(_.warrior)./(_ => c))
        
        a.unwrap match {
            case DecreeAction(_, h, nn, cc) =>

                var todo = ed.todo
                
                nn.lazyZip(cc).foreach { case (n, x) =>
                    val d = h(n)
                    val last = n == nn.last
                    x match {
                        case Decree.Recruit =>
                            d.suit == Bird |=> -120 -> "bird no recruit"
                            todo(Decree.Recruit).contains(d.suit) |=> 100 -> "repeat recruit"
                            todo(Decree.Recruit).none |=> 50 -> "some recruit"
                            todo(Decree.Recruit).num < todo(Decree.Battle).num |=> 350 -> "less than battle"
                
                            val m = roosts.%(_.suit == d.suit || d.suit == Bird).num
                            m == 0 |=> -1000 -> "no matching roost"
                            m >= 2 |=> 120 -> "multi"
                            
                            val n = warriors.num 
                            n + (todo(Decree.Recruit).num + 1) * rc > 20 |=> -1000 -> "recruit overflow"
                            n + (todo(Decree.Recruit).num + 1) * rc * 2 - todo(Decree.Battle).num > 20 |=> -500 -> "recruit soon overflow"
                
                            last && !complete(state, todo + (x -> (todo(x) :+ d.suit))) |=> -500 -> "turmoil"
                
                        case Decree.Move =>
                            todo(Decree.Move).num < todo(Decree.Recruit).num |=> 50 -> "move good"
                            todo(Decree.Move).num < todo(Decree.Battle).num |=> 150 -> "move good"
                
                            d.suit != Bird && todo(Decree.Move).none && warriors.%(_.suit == d.suit).none |=> -1000 -> "no move from"
                            
                            last && !complete(state, todo + (x -> (todo(x) :+ d.suit))) |=> -500 -> "turmoil"
                
                        case Decree.Battle =>
                            d.suit != Bird |=> -350 -> "no bird battle"
                            todo(Decree.Battle).none |=> 300 -> "battle good"
                            todo(Decree.Battle).any |=> 200 -> "battle ok"
                
                            last && !complete(state, todo + (x -> (todo(x) :+ d.suit))) |=> -500 -> "turmoil"
                
                        case Decree.Build =>
                            d.suit != Bird |=> -600 -> "no bird build"
                            todo(Decree.Build).none |=> 400 -> "build good"
                            todo(Decree.Build).num == 1 |=> 50 -> "build ok"
                
                            last && !complete(state, todo + (x -> (todo(x) :+ d.suit))) |=> -500 -> "turmoil"
                    }
                    todo += x -> (todo(x) :+ d.suit)
                }

            case DoneAction(DaylightStartAction(_)) =>
                true |=> 100 -> "enough"

                !complete(state, ed.todo) |=> -500 -> "turmoil"

            case RecruitHawkAction(_, c) =>
                val d = Decree.Recruit
                val t = ed.todo(d)                                                                                                       
                val o = ed.done(d) :+ c.suit
                val r = t.diff(o).diff(o.diff(t)./(_ => Bird))
                
            case MoveListAction(_, _, _, c : Clearing, to : Clearing, l, _) =>
                val d = Decree.Move
                val t = ed.todo(d)
                val o = ed.done(d) :+ c.suit
                val r = t.diff(o).diff(o.diff(t)./(_ => Bird))

                !complete(state + (c -> state(c).copy(warriors = state(c).warriors - l.num)) + (to -> state(to).copy(warriors = state(to).warriors + l.num)), 
                    ed.todo + (Decree.Recruit -> Nil) + (Decree.Move -> r)) |=> -500 -> "turmoil"
                    
                val opposition = others./(g.at(c, _)./~(_.ruling).num).maxOr(0)

                warriors.count(to) == 0 && ed.todo(Decree.Battle).contains(to.suit) |=> 150 -> "go for battle"
                warriors.count(to) == 0 && ed.todo(Decree.Move).contains(to.suit) && l.num >= opposition |=> 160 -> "go for move"
                warriors.count(to) == 0 && ed.todo(Decree.Build).contains(to.suit) && g.canBuild(self)(c) && l.num >= opposition |=> 140 -> "go for build"
                warriors.count(to) == 0 && g.canBuild(self)(c) && l.num >= opposition |=> 130 -> "go for build"
                warriors.count(to) == 0 && g.canBuild(self)(c) |=> 120 -> "go for build"
                

                state(c).warriors - l.num >= state(c).opposition |=> 160 -> "rule origin"
                state(to).warriors + l.num >= state(to).opposition |=> 160 -> "rule dest"

                state(c).warriors - l.num >= 0 |=> 150 -> "any origin"
                state(to).warriors + l.num >= 0 |=> 150 -> "any dest"

                state(c).roost && state(c).warriors - l.num > 3 |=> 130 -> "three origin with roost"
                state(to).roost && state(to).warriors + l.num > 3 |=> 130 -> "three dest with roost"

                state(c).roost && state(c).warriors - l.num > 2 |=> 120 -> "two origin with roost"
                state(to).roost && state(to).warriors + l.num > 2 |=> 120 -> "two dest with roost"

                state(c).roost && state(c).warriors - l.num > 1 |=> 110 -> "one origin with roost"
                state(to).roost && state(to).warriors + l.num > 1 |=> 110 -> "one dest with roost"

                !state(c).roost && state(c).warriors - l.num > 3 |=> 130 -> "three origin no roost"
                !state(to).roost && state(to).warriors + l.num > 3 |=> 130 -> "three dest no roost"
                
                !state(c).roost && state(c).warriors - l.num > 2 |=> 120 -> "two origin no roost"
                !state(to).roost && state(to).warriors + l.num > 2 |=> 120 -> "two dest no roost"

                !state(c).roost && state(c).warriors - l.num > 1 |=> 110 -> "one origin no roost"
                !state(to).roost && state(to).warriors + l.num > 1 |=> 110 -> "one dest no roost"

            case BattleStartAction(_, _, _, c, e, _, _) =>
                val d = Decree.Move
                val t = ed.todo(d)
                val o = ed.done(d) :+ c.suit
                val r = t.diff(o).diff(o.diff(t)./(_ => Bird))

                val ew = g.at(c, e)./~(_.warrior).num
                val et = g.at(c, e)./~(_.token).num
                val eb = g.at(c, e)./~(_.building).num

                !complete(state + (c -> state(c).copy(warriors = state(c).warriors - min(3, ew), empty = state(c).empty || (eb > 0 && (ew + et) <= min(3, state(c).warriors)))),
                    ed.todo + (Decree.Recruit -> Nil) + (Decree.Move -> Nil) + (Decree.Battle -> r)) |=> -500 -> "turmoil"
            
                g.at(c, ED)./~(_.warrior).num > g.at(c, e)./~(_.warrior).num |=> 100 -> "more warriors"
                g.at(c, e)./~(_.scoring).any |=> 120 -> "scoring"
                g.at(c, e)./~(_.warrior).none |=> 100 -> "no warriors"
                g.at(c, ED)./~(_.building).any |=> 110 -> "own" 

            case AssignLeaderAction(_, l, _) =>
                l.starting.contains(Decree.Recruit) && pooled(Hawk) / (1 + (l == Charismatic).??(1)) < pooled(Roost) |=> -500 -> "not enough recruits"
                !l.starting.contains(Decree.Move) && ed.hand.%(d => g.of(self).all(Hawk).%(c => d.suit.m(c.suit)).any).num < 1 + maxcardloss |=> -2000 -> "no-move risk"
                !l.starting.contains(Decree.Build) && ed.hand.%(_.suit == Bird).none && pooled(Roost) > 2 |=> -400 -> "no build no bird"
                !l.starting.contains(Decree.Move) && !l.starting.contains(Decree.Build) && ed.vp < 10 && ed.hand.%(_.suit == Bird).num < 2 |=> -2000 -> "no-move no-build"

            case CraftAssignAction(_, CraftItemCard(_, _, _, _, n, _), _, _, _, _) =>
                true |=> (n * 100) -> "vp"

            case CraftAssignAction(_, CraftEffectCard(_, _, _, _), _, _, _, _) =>
                true |=> 400 -> "effect"

            case ActivateDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case TakeDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case BattleAmbushAction(_, _, _, _, _) =>
                true |=> 1000 -> "ambush"
                
            case BattleCounterAmbushAction(_, _, _, _, _) =>
                true |=> 1000 -> "ct-ambush"
 
            case PayGiveWarriorsAction(_, s, n, _, GetServicesAction(_, _, List(offers))) =>
                true |=> 50 - (math.pow(10, 2 + n) * math.random()).round.toInt -> "cost"

            case _ =>
        }


        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"
        
        result.sortBy(v => -abs(v.weight))
    }

}
      