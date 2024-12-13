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

class GameEvaluation02(val game : Game, val self : Faction) extends GEvaluation {
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

        def threat(f : Faction) = (players(f).hand.num - 1) * 6 + players(f).money

        def maxthreat(f : Faction) = threat(f) >= factions.but(self)./(threat).max
        def minthreat(f : Faction) = threat(f) <= factions.but(self)./(threat).min

        def duel = factions.%(f => players(f).hand.any).num == 2

        a.unwrap match {
            case IncomeAction(_) =>
                true |=> 500 -> "income good"

            case ForeignAidAction(_) =>
                see(Duke) == 3 |=> 1000 -> "safe"
                see(Duke) == 2 |=> 900 -> "risky"
                see(Duke) == 1 |=> 500 -> "risky"

            case TaxAction(_) =>
                has(Duke) |=> 700 -> "tax good"
                has(Duke) && duel |=> 1100 -> "race money"

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
                        v == self && (has(Ambassador) || has(Captain)) |=> -1000 -> "will block"

                    case StealBlockedAction(s, _) =>
                        s == self |=> 900 -> "hey stop"

                    case KillBlockAction(f, v) =>
                        v != self |=> -2000 -> "other guy"
                        v == self && has(Contessa) |=> -1000 -> "will block"
                        v == self && hand == 2 |=> -1000 -> "take hit"

                    case KillBlockedAction(f, v) =>
                        true |=> 0 -> "no idea"

                    case BlockSuccessAction(f, v, r, _) =>
                        v != self |=> -700 -> "don't bother"
                }

            case PassAction(_, f, r, action, then) =>
                true |=> 100 -> "i want to believe"



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


                val keep = ll./(_.role)


                keep.distinct.num == keep.num |=> 2000 -> "distinct"
                keep.has(Duke) |=> 600 -> "keep duke"
                keep.has(Ambassador) && keep.has(Captain).not |=> 700 -> "keep ambassador"
                keep.has(Captain) |=> 800 -> "keep captain"
                keep.has(Captain) && see(Captain) == 3 && see(Ambassador) == 3 |=> 8000 -> "keep captain no opposition"
                keep.has(Assassin) |=> 900 -> "keep assassin"
                keep.has(Contessa) && see(Assassin) < 3 |=> 1000 -> "keep contessa"
                keep.has(Assassin) && see(Contessa) == 3 |=> 1100 -> "keep assassin no opposition"

            case KillBlockWithAction(_, f, r) =>
                has(r) |=> 5000 -> "block assassin"
                hand == 1 |=> 5000 -> "nothing to lose"

            case BlockAction(_, f, Duke, TakeForeignAidAction(v), _) =>
                has(Duke) && !minthreat(v) |=> 1000 -> "block aid"

            case PassBlockAction(_, f, Duke, TakeForeignAidAction(v), _) =>
                has(Duke) |=> 100 -> "don't block aid"
                has(Duke).not |=> 500 -> "don't block aid"

            case KillWhomAction(_, f) =>
                has(Assassin) |=> (threat(f) * 50) -> "kill higher threat"
                see(Assassin) == 0 && players(f).hand.num > 1 && random() > 0.900 |=> 300 -> "risk it"

            case CoupWhomAction(_, f) =>
                true |=> (threat(f) * 400) -> "coup higher threat"
                duel |=> 10000 -> "showdown"



            case _ : Soft =>
        }



        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
