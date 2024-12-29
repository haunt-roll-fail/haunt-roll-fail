package arcs
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

class BotYY(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        val ev = new GameEvaluationYY(f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluationYY(val self : Faction)(implicit val game : Game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        if (game.states.contains(self).not)
            return $

        def ambition(a : Ambition) : Int = {
            game.declared.get(a).|(Nil)./(_.high).sum + game.ambitionable./(_.high).maxOr(0)
        }

        def appraise(x : Cost) : Int = {
            x @@ {
                case Pip => 0
                case NoCost => -100
                case AlreadyPaid => -100
                case MultiCost(l) => l./(appraise).sum
                case PayResource(resource, |(lock)) =>
                    lock * 100 + resource @@ {
                        case Material | Fuel => max(60, ambition(Tycoon) * 50)
                        case Relic => max(70, ambition(Keeper) * 100)
                        case Psionic => max(90, ambition(Empath) * 100)
                        case Weapon => 80
                        case Nothingness => 0
                    }
            }
        }

        def record(ambition : Ambition)(f : Faction) = ambition @@ {
            case Tycoon =>
                f.resources.count(Material) +
                f.resources.count(Fuel) +
                f.loyal.of[GuildCard].count(_.suit == Material) +
                f.loyal.of[GuildCard].count(_.suit == Fuel) +
                f.loyal.has(MaterialCartel).??(game.availableNum(Material)) +
                f.loyal.has(FuelCartel).??(game.availableNum(Fuel))
            case Tyrant => f.captives.num
            case Warlord => f.trophies.num
            case Keeper => f.resources.count(Relic) + f.loyal.of[GuildCard].count(_.suit == Relic)
            case Empath => f.resources.count(Psionic) + f.loyal.of[GuildCard].count(_.suit == Psionic)
        }

        def records(ambition : Ambition) = {
            factions./(f => f -> ambition @@ {
                case Tycoon =>
                    f.resources.count(Material) +
                    f.resources.count(Fuel) +
                    f.loyal.of[GuildCard].count(_.suit == Material) +
                    f.loyal.of[GuildCard].count(_.suit == Fuel) +
                    f.loyal.has(MaterialCartel).??(game.availableNum(Material)) +
                    f.loyal.has(FuelCartel).??(game.availableNum(Fuel))
                case Tyrant => f.captives.num
                case Warlord => f.trophies.num
                case Keeper => f.resources.count(Relic) + f.loyal.of[GuildCard].count(_.suit == Relic)
                case Empath => f.resources.count(Psionic) + f.loyal.of[GuildCard].count(_.suit == Psionic)
            }).toMap
        }

        a.unwrap @@ {
            case EndTurnAction(_) =>
                true |=> -1000 -> "dont skip actions"

            case DeclareAmbitionAction(f, ambition, zero, _) =>
                val r = record(ambition)(_)

                if (f == Red || true) {
                    f.rivals.num > 0 |=> (r(f) - f.rivals./(r).max) * 1000 -> "current record"
                    f.rivals.num > 1 |=> (r(f) - f.rivals./(r).sorted.dropLast.last) * 800 -> "current second record"
                    f.rivals.num > 2 |=> (r(f) - f.rivals./(r).sum - f.rivals.num) * 10 -> "current record sum"
                }
                else {
                    true |=> (r(f) - f.rivals./(r).max) * 1000 -> "current record"
                    true |=> (r(f) - f.rivals./(r).sum - f.rivals.num) * 10 -> "current record sum"
                }

            case BattleFactionAction(f, cost, effect, s, e, _) =>
                true |=> -appraise(cost) -> "cost"

                val own = f.at(s).ships
                val enemy = e.at(s).ships

                val str = own.num + own.fresh.num

                true |=> min(own.num * 250, enemy.num * 1000) -> "battle"
                true |=> min(own.num * 500, enemy.damaged.num * 1000) -> "battle damaged"
                true |=> own.fresh.num * 10 -> "own fresh"
                true |=> enemy.fresh.num * -10 -> "enemy fresh"

            case DealHitsAction(f, s, _, e, l, raid, effect, _, _) =>
                if (f == e) {
                    true |=> l.damaged.num * -100 -> "save damaged"
                }
                else {
                    true |=> l.damaged.num * 100 -> "finish damaged"
                    true |=> l.distinct.num * -10 -> "damage concentrate"
                    true |=> l.cities.num * -1000 -> "damage cities"
                    true |=> l.starports.num * 50 -> "damage starports"
                }

            case SecureAction(f, cost, effect, c, _) =>
                true |=> -appraise(cost) -> "cost"

                c.as[GuildCard].foreach { c =>
                    true |=> appraise(PayResource(c.suit, |(c.keys))) -> "profit"
                }


                val own = Influence(c).$.%(_.faction == f).num
                val enemy = Influence(c).$.%(_.faction != f).num

                true |=> own * 100 -> "return agents"
                true |=> enemy * 1000 -> "capture agents"

            case InfluenceAction(f, cost, c, effect, _) =>
                true |=> -appraise(cost) -> "cost"

                val own = Influence(c).$.%(_.faction == f).num
                val enemy = f.rivals./(e => Influence(c).$.%(_.faction == e).num).max

                // if (f == Red)
                //     true |=> (f.rivals./(e => (Influence(c).$.%(_.faction == e).num == enemy).??(e.power)).max - 40) -> "leader power"

                enemy - own ==  1 |=> 100 -> "even out"
                enemy - own ==  0 |=> 150 -> "break out"
                enemy - own == -1 |=> 50 -> "out do"

            case BuildCityAction(f, cost, s, effect, _) =>
                true |=> -appraise(cost) -> "cost"

                true |=> 50 -> "city good"

                f.rivals.exists(_.rules(s)) |=> -100 -> "damaged"

            case BuildStarportAction(f, cost, s, _) =>
                true |=> -appraise(cost) -> "cost"

                true |=> 30 -> "starport good"

                systems.exists(s => f.at(s).starports.any && f.rules(s)).not |=> 100 -> "no ruled starports"

                f.rivals.exists(_.rules(s)) |=> -100 -> "damaged"

            case BuildShipAction(f, cost, s, b, effect, _) =>
                true |=> -appraise(cost) -> "cost"

                true |=> 40 -> "ship good"

                f.rivals.exists(_.rules(s)) |=> -100 -> "damaged"

            case RepairAction(f, cost, s, u, _) =>
                true |=> -appraise(cost) -> "cost"

                true |=> 20 -> "repair ok"

            case TaxAction(f, cost, effect, s, u, loyal, _) =>
                true |=> -appraise(cost) -> "cost"
                loyal.not |=> 100 -> "capture"

            case MoveListAction(f, s, dest, l, cascade, cost, _) =>
                true |=> -appraise(cost) -> "cost"

            case AddBattleOptionAction(f, cost, _) =>
                true |=> -appraise(cost) -> "cost"

            case PassAction(f) =>
                true |=> -100 -> "dont pass"

            case LeadAction(f, ActionCard(suit, str, pips)) =>
                if (game.ambitionable.any) {
                    str.in(2, 7) |=> max(0, record(Tycoon )(f) - f.rivals./(record(Tycoon )).max) * 100000 + 10000 - str * 1000 -> "tycoon"
                    str.in(3, 7) |=> max(0, record(Tyrant )(f) - f.rivals./(record(Tyrant )).max) * 100000 + 10000 - str * 1000 -> "tyrant"
                    str.in(4, 7) |=> max(0, record(Warlord)(f) - f.rivals./(record(Warlord)).max) * 100000 + 10000 - str * 1000 -> "warlord"
                    str.in(5, 7) |=> max(0, record(Keeper )(f) - f.rivals./(record(Keeper )).max) * 100000 + 10000 - str * 1000 -> "keeper"
                    str.in(6, 7) |=> max(0, record(Empath )(f) - f.rivals./(record(Empath )).max) * 100000 + 10000 - str * 1000 -> "empath"
                }

                val seen = f.hand ++ f.seen ++ game.seen./~(_._3)
                val l = seen.%(_.suit == suit)./(_.strength)
                val r = (factions.num == 4).?(1.to(7)).|(2.to(6)).$.diff(l)

                str > r.maxOr(0) |=> pips * 100 -> "unsurpassable"

            case SurpassAction(f, ActionCard(suit, str, pips)) =>
                if (game.ambitionable.any) {
                    str.in(2, 7) |=> max(0, record(Tycoon )(f) - f.rivals./(record(Tycoon )).max) * -10 -> "tycoon"
                    str.in(3, 7) |=> max(0, record(Tyrant )(f) - f.rivals./(record(Tyrant )).max) * -10 -> "tyrant"
                    str.in(4, 7) |=> max(0, record(Warlord)(f) - f.rivals./(record(Warlord)).max) * -10 -> "warlord"
                    str.in(5, 7) |=> max(0, record(Keeper )(f) - f.rivals./(record(Keeper )).max) * -10 -> "keeper"
                    str.in(6, 7) |=> max(0, record(Empath )(f) - f.rivals./(record(Empath )).max) * -10 -> "empath"
                }

                true |=> pips * 5000 -> "actions"

                if (suit == Aggression)
                    market.foreach { c =>
                        Influence(c).$.use(l => l.%(_.faction == f).num > f.rivals./(e => l.%(_.faction == e).num).max) |=> 20000 -> "secure"
                    }

            case CopyAction(f, ActionCard(suit, str, pips)) =>
                if (game.ambitionable.any) {
                    str.in(2, 7) |=> max(0, record(Tycoon )(f) - f.rivals./(record(Tycoon )).max) * -10 -> "tycoon"
                    str.in(3, 7) |=> max(0, record(Tyrant )(f) - f.rivals./(record(Tyrant )).max) * -10 -> "tyrant"
                    str.in(4, 7) |=> max(0, record(Warlord)(f) - f.rivals./(record(Warlord)).max) * -10 -> "warlord"
                    str.in(5, 7) |=> max(0, record(Keeper )(f) - f.rivals./(record(Keeper )).max) * -10 -> "keeper"
                    str.in(6, 7) |=> max(0, record(Empath )(f) - f.rivals./(record(Empath )).max) * -10 -> "empath"
                }

                if (lead.get.suit == Aggression)
                    market.foreach { c =>
                        Influence(c).$.use(l => l.%(_.faction == f).num > f.rivals./(e => l.%(_.faction == e).num).max) |=> 20000 -> "secure"
                    }


                true |=> (2 * str + pips) * -1000 -> "lost card"

            case PivotAction(f, ActionCard(suit, str, pips)) =>
                if (game.ambitionable.any) {
                    str.in(2, 7) |=> max(0, record(Tycoon )(f) - f.rivals./(record(Tycoon )).max) * -10 -> "tycoon"
                    str.in(3, 7) |=> max(0, record(Tyrant )(f) - f.rivals./(record(Tyrant )).max) * -10 -> "tyrant"
                    str.in(4, 7) |=> max(0, record(Warlord)(f) - f.rivals./(record(Warlord)).max) * -10 -> "warlord"
                    str.in(5, 7) |=> max(0, record(Keeper )(f) - f.rivals./(record(Keeper )).max) * -10 -> "keeper"
                    str.in(6, 7) |=> max(0, record(Empath )(f) - f.rivals./(record(Empath )).max) * -10 -> "empath"
                }

                if (suit == Aggression)
                    market.foreach { c =>
                        Influence(c).$.use(l => l.%(_.faction == f).num > f.rivals./(e => l.%(_.faction == e).num).max) |=> 20000 -> "secure"
                    }

                true |=> (2 * str + pips) * -1000 -> "lost card"

            case SeizeAction(f, ActionCard(suit, str, pips), _) =>
                true |=> (2 * str + pips) * -1000 -> "lost card"

            case ReorderResourcesAction(f, l, _) =>
                true |=> l.lazyZip(f.keys).map { (r, k) =>
                    appraise(PayResource(r, |(0))) * k
                }.sum -> "appraisal"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
