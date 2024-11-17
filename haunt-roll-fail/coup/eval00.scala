package coup
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

class GameEvaluation00(val game : Game, val self : Faction) extends GEvaluation {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        import game._

        def money = players(self).money
        def hand = players(self).hand.num

        def see(r : Role) = (setup./~(players(_).lost) ++ players(self).hand).%(_.role == r).num

        def has(r : Role) = players(self).hand.%(_.role == r).any

        def double(r : Role) = players(self).hand.%(_.role == r).num == 2

        def threat(f : Faction) = players(f).hand.num * players(f).money

        def maxthreat(f : Faction) = threat(f) >= factions.but(self)./(threat).max
        def minthreat(f : Faction) = threat(f) <= factions.but(self)./(threat).min

        a.unwrap match {
            case IncomeAction(_) =>
                true |=> 500 -> "income good"

            case ForeignAidAction(_) =>
                see(Duke) == 3 |=> 600 -> "safe"
                see(Duke) == 2 |=> 500 -> "risky"

            case TaxAction(_) =>
                has(Duke) |=> 700 -> "tax good"

            case StealFromAction(_, f) =>
                has(Captain) |=> (see(Captain) + see(Ambassador)) * 150 -> "steal good"
                has(Captain) |=> (threat(f) * 50) -> "steal from higher threat"


            case ExchangeAction(_) =>
                has(Ambassador) && has(Contessa).not && hand == 2 && money >= factions./(players(_).money).max |=> 700 -> "find contessa"

            case ChallengeAction(_, f, r, action, then) =>
                see(r) == 3 |=> 5000 -> "safe"
                see(r) == 2 && random() > 0.666 |=> 1000 -> "unsafe"
                see(r) == 1 && random() > 0.966 |=> 800 -> "risky"
                see(r) == 0 && random() > 0.999 |=> 600 -> "foolhardy"
                maxthreat(f) && random() > 0.888 |=> 1000 -> "maxthreat"

                action match {
                    case DrawExchangeAction(_) =>
                        true |=> -1000 -> "draw"

                    case TaxMoneyAction(_) =>
                        true |=> -900 -> "tax"

                    case StealBlockAction(_, v) =>
                        v == self |=> 900 -> "hey stop"

                    case StealBlockedAction(s, _) =>
                        s == self |=> 900 -> "hey stop"

                    case KillBlockAction(f, v) =>
                        hand == 1 && players(v).hand.num > 1 |=> 10000 -> "all in antiblock"

                    case KillBlockedAction(f, v) =>
                        true |=> 0 -> "no idea"

                    case BlockSuccessAction(f, v, r, _) =>
                        v != self |=> -700 -> "don't bother"


                    case _ : Soft =>
                }

            case PassAction(_, f, r, action, then) =>
                maxthreat(self) |=> -900 -> "stay low minthreat"

            case StealBlockWithAction(_, _, Captain) =>
                has(Captain) |=> 1000 -> "block with captain"

            case StealBlockWithAction(_, _, Ambassador) =>
                has(Ambassador) |=> 1000 -> "block with ambassador"

            case StealMoneyAction(_, _) =>
                true |=> 100 -> "thiefs"

            case KillKillAction(_, _) =>
                true |=> 100 -> "killers"

            case RevealInfluenceAction(_, r, c, _, _, _) =>
                c.role == r |=> 5000 -> "reveal correct role"
                c.role == Duke |=> 1000 -> "reveal duke"
                c.role == Ambassador |=> 900 -> "reveal ambassador"
                c.role == Captain |=> 800 -> "reveal captain"
                c.role == Assassin |=> 700 -> "reveal assassin"
                c.role == Contessa |=> 600 -> "reveal contessa"

            case RevealCardAction(_, c, _) =>
                c.role == Duke |=> 1000 -> "reveal duke"
                c.role == Ambassador |=> 900 -> "reveal ambassador"
                c.role == Captain |=> 800 -> "reveal captain"
                c.role == Assassin |=> 700 -> "reveal assassin"
                c.role == Contessa |=> 600 -> "reveal contessa"

            case KeepCardsAction(_, ll, _) =>
                val l = ll./(_.role)

                l.distinct.num < l.num |=> -2000 -> "discard double"
                l.has(Duke) |=> -1000 -> "discard duke"
                l.has(Ambassador) |=> -900 -> "discard ambassador"
                l.has(Captain) |=> -700 -> "discard captain"
                l.has(Assassin) |=> -800 -> "discard assassin"
                l.has(Contessa) |=> -600 -> "discard contessa"

            case KillBlockWithAction(_, f, r) =>
                has(r) |=> 5000 -> "block assassin"
                hand == 1 |=> 5000 -> "nothing to lose"

            case BlockAction(_, f, Duke, TakeForeignAidAction(v), _) =>
                has(Duke) && !minthreat(v) |=> 1000 -> "block aid"

            case PassBlockAction(_, f, Duke, TakeForeignAidAction(v), _) =>
                has(Duke) |=> 100 -> "don't block aid"

            case KillWhomAction(_, f) =>
                true |=> (threat(f) * 50) -> "kill higher threat"

            case CoupWhomAction(_, f) =>
                true |=> (threat(f) * 40) -> "coup higher threat"



            case _ : Soft =>
        }



        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
