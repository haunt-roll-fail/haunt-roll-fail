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


class GameEvaluationAviary(faction : Aviary)(implicit game : Game) extends GameEvaluation(game, faction) {
    def eval(a : Action) : List[Evaluation] = {
        if (game.states.contains(self).not)
          return $(Evaluation(-1 - (math.random() * 7).~, "proto random"))

        var result : List[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        case class CS(roost : Boolean, warriors : Int, opposition : Int, empty : Boolean, connected : List[Clearing])

        val ed = self

        val others = game.factions.but(ED).%(game.states.contains)

        val maxcardloss = others.%(_.has(StandAndDeliver)).num + others.%(_.has(Steal)).num

        val all = clearings
        val state = clearings./(c => c -> CS(self.at(c).of[Building].any, self.at(c).of[Warrior].num, others./(_.at(c).%(p => p.is[Warrior] || p.is[Building]).num).maxOr(0), ed.canBuild(c), game.connected(c))).toMap

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
            if (todo(Decree.Build).num + clearings.%(c => state(c).roost).num > 7)
                return false

            if (todo(Decree.Recruit).num * rc + state.values.$./(_.warriors).sum > 20)
                false

            val phase = Decree.all.%(todo(_).any)
            if (phase.none)
                true
            else {
                phase.head match {
                    case Decree.Recruit =>
                        val r = todo(Decree.Recruit)
                        val x = List(Fox, Rabbit, Mouse).%(s => r.contains(s) || r.contains(Bird))
                        val m = (c : Clearing) => x.exists(_.matches(c.cost))

                        clearings.%(m).%(c => state(c).roost).foreach { c =>
                            val newtodo = todo + (Decree.Recruit -> r.intersect(c.suits).any.?(r :- r.intersect(c.suits).head).|(r :- Bird))

                            if (complete(state + (c -> state(c).copy(warriors = state(c).warriors + rc)), newtodo))
                                return true
                        }
                        false

                    case Decree.Move =>
                        val r = todo(Decree.Move)

                        val x = List(Fox, Rabbit, Mouse).%(s => r.contains(s) || r.contains(Bird))
                        val m = (c : Clearing) => x.exists(_.matches(c.cost))

                        var v = clearings.%(m).%(c => state(c).warriors > 0)./~ { c =>
                            state(c).connected.%(t => state(c).warriors + state(c).roost.??(1) >= state(c).opposition || state(t).warriors + state(t).roost.??(1) >= state(t).opposition)./~ { t =>
                                List((c, t, max(1, state(c).warriors - (state(c).warriors + state(t).warriors) / 2)))
                            }
                        }

                        if (v.num > 8 - r.num)
                            v = v.shuffle.take(8 - r.num)

                        v.foreach { case (c, t, n) =>
                            val newtodo = todo + (Decree.Move -> r.intersect(c.suits).any.?(r :- r.intersect(c.suits).head).|(r :- Bird))
                            if (complete(state + (c -> state(c).copy(warriors = state(c).warriors - n)) + (t -> state(t).copy(warriors = state(t).warriors + n)), newtodo))
                                return true
                        }

                        false

                    case Decree.Battle =>
                        val r = todo(Decree.Battle)
                        val x = List(Fox, Rabbit, Mouse).%(s => r.contains(s) || r.contains(Bird))
                        val m = (c : Clearing) => x.exists(_.matches(c.cost))
                        clearings.%(m).%(c => state(c).warriors > 0).%(c => state(c).opposition > 0).foreach { c =>
                            val newtodo = todo + (Decree.Battle -> r.intersect(c.suits).any.?(r :- r.intersect(c.suits).head).|(r :- Bird))
                            if (complete(state + (c -> state(c).copy(warriors = state(c).warriors - min(2, state(c).opposition), opposition = state(c).opposition - 1, empty = state(c).empty || (state(c).opposition <= 1))), newtodo))
                                return true
                        }
                        false

                    case Decree.Build =>
                        val r = todo(Decree.Build)
                        val x = List(Fox, Rabbit, Mouse).%(s => r.contains(s) || r.contains(Bird))
                        val m = (c : Clearing) => x.exists(_.matches(c.cost))
                        clearings.%(m).%(c => state(c).empty).%(c => state(c).warriors > 0).%(c => state(c).roost.not).foreach { c =>
                            val newtodo = todo  + (Decree.Build -> r.intersect(c.suits).any.?(r :- r.intersect(c.suits).head).|(r :- Bird))
                            if (complete(state + (c -> state(c).copy(roost = true)), newtodo))
                                return true
                        }
                        false

                }
            }
        }

        val roosts = clearings.%(c => self.at(c).of[Building].any)
        val warriors = clearings./~(c => self.at(c).of[Warrior]./(_ => c))

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

                            val m = roosts.%(r => d.suit.matches(r.cost)).num
                            m == 0 |=> -1000 -> "no matching roost"
                            m >= 2 |=> 120 -> "multi"

                            val n = warriors.num
                            n + (todo(Decree.Recruit).num + 1) * rc > 20 |=> -1000 -> "recruit overflow"
                            n + (todo(Decree.Recruit).num + 1) * rc * 2 - todo(Decree.Battle).num > 20 |=> -500 -> "recruit soon overflow"

                            last && !complete(state, todo + (x -> (todo(x) :+ d.suit))) |=> -500 -> "turmoil"

                        case Decree.Move =>
                            todo(Decree.Move).num < todo(Decree.Recruit).num |=> 50 -> "move good"
                            todo(Decree.Move).num < todo(Decree.Battle).num |=> 150 -> "move good"

                            d.suit != Bird && todo(Decree.Move).none && warriors.%(w => d.suit.matches(w.cost)).none |=> -1000 -> "no move from"

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

            case AviaryRecruitAction(_, c, s, _) =>
                val d = Decree.Recruit
                val t = ed.todo(d)
                val o = ed.done(d) :+ s
                val r = t.diff(o).diff(o.diff(t)./(_ => Bird))

            case MoveListAction(_, _, _, _, c : Clearing, to : Clearing, l, _) =>
                val d = Decree.Move
                val t = ed.todo(d)
                val o = ed.done(d) :+ c.suits.intersect(t).headOption.|(Bird)
                val r = t.diff(o).diff(o.diff(t)./(_ => Bird))

                !complete(state + (c -> state(c).copy(warriors = state(c).warriors - l.num)) + (to -> state(to).copy(warriors = state(to).warriors + l.num)),
                    ed.todo + (Decree.Recruit -> Nil) + (Decree.Move -> r)) |=> -500 -> "turmoil"

                val opposition = others./(_.at(c).%(p => p.is[Warrior] || p.is[Building]).num).maxOr(0)

                warriors.count(to) == 0 && ed.todo(Decree.Battle).intersect(to.suits).any |=> 150 -> "go for battle"
                warriors.count(to) == 0 && ed.todo(Decree.Move).intersect(to.suits).any && l.num >= opposition |=> 160 -> "go for move"
                warriors.count(to) == 0 && ed.todo(Decree.Build).intersect(to.suits).any && ed.canBuild(c) && l.num >= opposition |=> 140 -> "go for build"
                warriors.count(to) == 0 && ed.canBuild(c) && l.num >= opposition |=> 130 -> "go for build"
                warriors.count(to) == 0 && ed.canBuild(c) |=> 120 -> "go for build"

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

            case BattleStartAction(_, _, _, _, c, e, _, _) =>
                val d = Decree.Move
                val t = ed.todo(d)
                val o = ed.done(d) :+ c.suits.intersect(t).headOption.|(Bird)
                val r = t.diff(o).diff(o.diff(t)./(_ => Bird))

                val ew = e.at(c).of[Warrior].num
                val et = e.at(c).of[Token].num
                val eb = e.at(c).of[Building].num

                !complete(state + (c -> state(c).copy(warriors = state(c).warriors - min(3, ew), empty = state(c).empty || (eb > 0 && (ew + et) <= min(3, state(c).warriors)))),
                    ed.todo + (Decree.Recruit -> Nil) + (Decree.Move -> Nil) + (Decree.Battle -> r)) |=> -500 -> "turmoil"

                self.at(c).of[Warrior].num > e.at(c).of[Warrior].num |=> 100 -> "more warriors"
                e.at(c).of[Scoring].any |=> 120 -> "scoring"
                e.at(c).of[Warrior].none |=> 100 -> "no warriors"
                self.at(c).of[Building].any |=> 110 -> "own"

            case AssignLeaderAction(_, Some(l), _) =>
                l.starting.contains(Decree.Recruit) && ed.pooled(Hawk) / (1 + (l == Charismatic).??(1)) < ed.pooled(Roost) |=> -500 -> "not enough recruits"
                !l.starting.contains(Decree.Move) && ed.hand.%(d => ed.all(Hawk).%(c => d.matches(c.cost)).any).num < 1 + maxcardloss |=> -2000 -> "no-move risk"
                !l.starting.contains(Decree.Build) && ed.hand.%(_.suit == Bird).none && ed.pooled(Roost) > 2 |=> -400 -> "no build no bird"
                !l.starting.contains(Decree.Move) && !l.starting.contains(Decree.Build) && ed.vp < 10 && ed.hand.%(_.suit == Bird).num < 2 |=> -2000 -> "no-move no-build"

            case CraftAssignAction(_, CraftItemCard(_, _, _, _, n, _), _, _, _, _) =>
                true |=> (n * 100) -> "vp"

            case CraftAssignAction(_, CraftEffectCard(_, _, _, _), _, _, _, _) =>
                true |=> 400 -> "effect"

            case ActivateDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case TakeDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case BattleAmbushAction(_, _, _, _, _, _) =>
                true |=> 1000 -> "ambush"

            case BattleCounterAmbushAction(_, _, _, _, _, _) =>
                true |=> 1000 -> "ct-ambush"

            case PayGiveWarriorsAction(_, s, n, _, GetServicesAction(_, _, List(offers))) =>
                true |=> 50 - (math.pow(10, 2 + n) * math.random()).round.toInt -> "cost"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }

}
