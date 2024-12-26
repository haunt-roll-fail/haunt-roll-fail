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

class BotXX(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        val ev = new GameEvaluation(f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluation(val self : Faction)(implicit val game : Game) {
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

        a.unwrap @@ {
            case EndTurnAction(_) =>
                true |=> -1000 -> "dont skip actions"

            case DeclareAmbitionAction(f, ambition, zero, _) =>
                val marker = game.ambitionable.last

                val high = marker.high
                val low = marker.low

                val records = factions./(f => f -> ambition @@ {
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

                true |=> (records(f) - f.rivals./(records).max) * 1000 -> "current record"
                true |=> (records(f) - f.rivals./(records).sum - f.rivals.num) * 10 -> "current record sum"

            case BattleFactionAction(f, cost, effect, s, e, _) =>
                true |=> -appraise(cost) -> "cost"

                true |=> min(f.at(s).ships.num * 25, e.at(s).ships.num * 100) -> "battle"

            case SecureAction(f, cost, effect, c, _) =>
                true |=> -appraise(cost) -> "cost"

                val own = Influence(c).$.%(_.faction == f).num
                val enemy = Influence(c).$.%(_.faction != f).num

                true |=> own * 10 -> "return agents"
                true |=> enemy * 100 -> "capture agents"

            case InfluenceAction(f, cost, c, effect, _) =>
                true |=> -appraise(cost) -> "cost"

                val own = Influence(c).$.%(_.faction == f).num
                val enemy = f.rivals./(e => Influence(c).$.%(_.faction == e).num).max

                enemy - own ==  1 |=> 100 -> "even out"
                enemy - own ==  0 |=> 150 -> "break out"
                enemy - own == -1 |=> 50 -> "out do"

            case BuildCityAction(f, cost, s, effect, _) =>
                true |=> -appraise(cost) -> "cost"

            case BuildStarportAction(f, cost, s, _) =>
                true |=> -appraise(cost) -> "cost"

            case BuildShipAction(f, cost, s, b, effect, _) =>
                true |=> -appraise(cost) -> "cost"

            case RepairAction(f, cost, s, u, _) =>
                true |=> -appraise(cost) -> "cost"

            case TaxAction(f, cost, effect, s, u, loyal, _) =>
                true |=> -appraise(cost) -> "cost"
                loyal.not |=> 100 -> "capture"

            case MoveListAction(f, s, dest, l, cascade, cost, _) =>
                true |=> -appraise(cost) -> "cost"

            case AddBattleOptionAction(f, cost, _) =>
                true |=> -appraise(cost) -> "cost"

            case PassAction(f) =>
                true |=> -100 -> "dont pass"

            case SurpassAction(f, ActionCard(suit, str, pips)) =>
                true |=> pips * 100 -> "actions"

            case CopyAction(f, ActionCard(suit, str, pips)) =>
                true |=> (2 * str + pips) * -100 -> "lost card"

            case PivotAction(f, ActionCard(suit, str, pips)) =>
                true |=> (2 * str + pips) * -100 -> "lost card"

            case SeizeAction(f, ActionCard(suit, str, pips), _) =>
                true |=> (2 * str + pips) * -100 -> "lost card"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
