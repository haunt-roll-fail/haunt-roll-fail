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

class BotNew(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        val ev = new GameEvaluationNew(f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluationNew(val self : Faction)(implicit val game : Game) {
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
                case PayResource(Material, _) if self.can(MaterialCartel) => 0
                case PayResource(Fuel, _) if self.can(FuelCartel) => 0
                case PayResource(resource, lock) =>
                    lock.|(0) * 100 + resource @@ {
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
                f.can(MaterialCartel).??(game.availableNum(Material)) +
                f.can(FuelCartel).??(game.availableNum(Fuel))
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
                    f.can(MaterialCartel).??(game.availableNum(Material)) +
                    f.can(FuelCartel).??(game.availableNum(Fuel))
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

                f.rivals.num > 0 |=> (r(f) - f.rivals./(r).max) * 1000 -> "current record"
                f.rivals.num > 1 |=> (r(f) - f.rivals./(r).sorted.dropLast.last) * 800 -> "current second record"
                f.rivals.num > 2 |=> (r(f) - f.rivals./(r).sum - f.rivals.num) * 10 -> "current record sum"

            case BattleFactionAction(f, cost, effect, s, e, _) =>
                true |=> -appraise(cost) -> "cost"

                val own = f.at(s).ships
                val enemy = e.at(s).ships

                val str = own.num + own.fresh.num

                true |=> min(own.num * 60, enemy.num * 240) -> "battle"
                true |=> min(own.num * 120, enemy.damaged.num * 240) -> "battle damaged"
                true |=> own.fresh.num * 10 -> "own fresh"
                true |=> enemy.fresh.num * -20 -> "enemy fresh"

                enemy.fresh.none && e.at(s).buildings.any && own.fresh.num + own.num > 2 |=> 400 -> "raid"

            case BattleDiceAction(f, s, e, skirmish, assault, raid, used, then) =>
                val own = f.at(s).ships
                val enemy = e.at(s).ships

                // true |=> skirmish * 10 -> "skirmish good"
                true |=> (skirmish * 20 + assault * 10 + raid * 30) -> "dice value"
                (assault + raid > 0) && own.fresh.num < enemy.fresh.num + assault + raid |=> -400 -> "too risky"
                (enemy.fresh.num + enemy.num) * 2 < skirmish + 2 * assault |=> assault * -100 -> "why assault"

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

            case MoveListAction(f, from, to, l, cascade, cost, effect, _) if l.fresh.none =>
                l.fresh.none |=> -1000 -> "no fresh"

            case MoveListAction(f, from, to, l, cascade, cost, effect, _) if cascade && cost == NoCost && random() < 0.1 =>
                true |=> -1000 -> "combo breaker"

            case MoveListAction(f, from, to, l, cascade, cost, effect, _) =>
                cost != NoCost |=> -appraise(cost) -> "cost"

                val fromEnemyRuleValue = game.colors.but(f)./(_.ruleValue(from)).max
                val toEnemyRuleValue = game.colors.but(f)./(_.ruleValue(to)).max

                val fromOldOwnRuleValue = f.at(from).ships.fresh.num
                val toOldOwnRuleValue = f.at(to).ships.fresh.num

                val fromNewOwnRuleValue = f.at(from).ships.fresh.diff(l).num
                val toNewOwnRuleValue = f.at(to).ships.concat(l).fresh.num

                val fromCost = from.gate.??(4) + f.at(from).cities.num * 10 + f.at(from).starports.num * 3 + factions.but(f)./(e => e.at(from).use(l => l.cities.num * 10 + l.starports.num * 10 - l.ships.fresh.num)).sum
                val toCost   = to  .gate.??(4) + f.at(to)  .cities.num * 10 + f.at(to)  .starports.num * 3 + factions.but(f)./(e => e.at(to)  .use(l => l.cities.num * 10 + l.starports.num * 10 - l.ships.fresh.num)).sum

                // true |=> 0 -> ("fromCost=" + fromCost + " toCost=" + toCost)

                fromNewOwnRuleValue == fromEnemyRuleValue && fromEnemyRuleValue < fromOldOwnRuleValue |=> fromCost * -15 -> "move from eq"
                fromNewOwnRuleValue <  fromEnemyRuleValue && fromEnemyRuleValue < fromOldOwnRuleValue |=> fromCost * -20 -> "move from unrule"
                  toNewOwnRuleValue >    toEnemyRuleValue &&   toEnemyRuleValue >   toOldOwnRuleValue |=>   toCost * 20 -> "move to rule"
                  toNewOwnRuleValue >    toEnemyRuleValue &&   toEnemyRuleValue ==  toOldOwnRuleValue |=>   toCost * 10 -> "move to diseq rule"
                  toNewOwnRuleValue >    toEnemyRuleValue &&   toEnemyRuleValue <   toOldOwnRuleValue |=>   toCost * cascade.?(5).|(-5) -> "move to inc rule"

                true |=> (l.fresh.num + l.damaged.num) * 2 -> "move more"

            case BattleRaidResourceAction(f, e, r, keys, _) =>
                true |=> appraise(PayResource(r, None)) -> "cost"
                true |=> -keys * 5 -> "keys"

            case BattleRaidCourtCardAction(f, e, c, _) =>
                true |=> appraise(PayResource(c.suit, |(c.keys))) + 100 -> "cost"

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

                enemy - own ==  1 |=> 100 -> "even out"
                enemy - own ==  0 |=> 150 -> "break out"
                enemy - own == -1 |=> 50 -> "out do"

                c.as[GuildCard]./(_.suit).foreach { s =>
                    true |=> appraise(PayResource(s, None)) / 10 -> "card cost"
                }

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

                u.piece == Ship |=> 20 -> "repair ok"
                u.piece == Starport |=> 10 -> "repair not ok"
                u.piece == City |=> 10 -> "repair not ok"

                u.piece != Ship && f.at(s).ships.fresh.none && factions.but(f).exists(_.at(s).ships.any) |=> -15 -> "rival fleet"

            case TaxAction(f, cost, effect, s, u, loyal, _) =>
                true |=> -appraise(cost) -> "cost"
                loyal.not && u.?(_.faction.as[Faction].?(_.pool(Agent))) |=> 200 -> "capture"
                f.resourceSlots + cost.as[PayResource].$.num > f.resources.but(Nothingness).num |=> game.resources(s).%(game.available)./(r => appraise(PayResource(r, None))).maxOr(0) -> "gain"

            case MoveListAction(f, s, dest, l, cascade, cost, effect, _) =>
                true |=> -appraise(cost) -> "cost"

            case AddBattleOptionAction(f, cost, PreludeActionAction(_, suit, num)) =>
                true |=> -appraise(cost) -> "cost"

                suit == Aggression |=> -1000000 -> "can already"

                num >= 3 && systems.exists(s => f.at(s).ships.num > 1 && factions.but(f).exists(e => e.at(s).use(l => l.buildings.any || l.ships.num > 1))) |=> 10000 -> "use weapon to attack"

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

                if (suit == Aggression) {
                    val balance = min(pips, market.%(c => Influence(c).$.use(l => l.%(_.faction == f).num > f.rivals./(e => l.%(_.faction == e).num).max)).num) * 20000 -
                        f.rivals.%(_.hand.any).%(e => market.exists(c => Influence(c).$.use(l => l.%(_.faction == e).num > e.rivals./(e => l.%(_.faction == e).num).max))).num * 8000

                    true |=> balance -> "secure balance"
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

                val lest = lead.get.suit

                if (lest == Aggression)
                    market.foreach { c =>
                        Influence(c).$.use(l => l.%(_.faction == f).num > f.rivals./(e => l.%(_.faction == e).num).max) |=> 20000 -> "secure"
                    }

                true |=> -((2 * str + pips) * 1000
                    + (suit == Aggression).??(600) + (suit == Administration).??(400) + (suit == Mobilization).??(200)
                    - (lest == Aggression).??(600) - (lest == Administration).??(400) - (lest == Mobilization).??(200)
                    ) -> "lost card"

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

            case SeizeAction(f, c @ ActionCard(suit, str, pips), _) =>
                f.surpass && f.played.only.strength > factions.but(f).%!(_.zeroed)./~(e => e.played.%(_.suit == game.lead.get.suit))./(_.strength).maxOr(0) |=> -1000000 -> "surpassing"

                f.hand.but(c).notOf[EventCard].none |=> -10000000 -> "last card"

                factions.but(f).exists(_.hand.num <= f.hand.num).not |=> -10000000 -> "on back foot"

                val t = factions.but(f).%(_.surpass).%(e => e.played.only.strength > factions.but(e).%!(_.zeroed)./~(_.played.%(_.suit == game.lead.get.suit))./(_.strength).maxOr(0)).single.|(factions.first)

                factions.but(f).but(t).exists(_.power > t.power) |=> -1000000 -> "initiative with less power"

                suit == Aggression |=> -1000000 -> "dont seize with aggression"

                result ++= eval(LeadAction(f, c))./(e => e.copy(weight = -e.weight))

                true |=> 1000 -> "seize fest"

            case ReorderResourcesAction(f, l, _) =>
                true |=> l.lazyZip(f.keys).map { (r, k) =>
                    appraise(PayResource(r, None)) * k
                }.sum -> "appraisal"

            case FreeCityAction(f, s, u, _) =>
                u.faction == f |=> -1000 -> "dont free own city"
                u.faction != f |=> u.faction.as[Faction]./(_.power).|(0) * 10 -> "more power"

            case LatticeSeizeAction(f, c, _) =>
                true |=> -appraise(PayResource(c.suit, None)) -> "cost"

            case FarseersRedrawAction(f, l, _) =>
                true |=> -appraise(PayResource(Psionic, None)) -> "cost"

            case FenceResourceAction(f, r, cost, _) =>
                game.available(r).not |=> -10000 -> "unavailable"
                true |=> -appraise(cost) -> "cost"
                true |=> appraise(PayResource(r, |(3))) -> "cost"

            case ManufactureMainAction(f, cost, _) =>
                val r = Material
                game.available(r).not |=> -10000 -> "unavailable"
                true |=> -appraise(cost) -> "cost"
                true |=> appraise(PayResource(r, |(1))) -> "cost"

            case SynthesizeMainAction(f, cost, _) =>
                val r = Fuel
                game.available(r).not |=> -10000 -> "unavailable"
                true |=> -appraise(cost) -> "cost"
                true |=> appraise(PayResource(r, |(1))) -> "cost"

            case StealResourceAction(f, e, x, k, DiscardCourtCardAction(_, c, _)) =>
                true |=> -10000 -> "dont discard to steal"

            case FillSlotsMainAction(f, r, DiscardCourtCardAction(_, c, _)) =>
                val slots = f.resourceSlots - f.resources.but(Nothingness).num
                val available = game.availableNum(r) + f.rivals.%!(_.can(SwornGuardians))./(_.resources.count(r)).sum

                slots < 3 |=> -1000 -> "two slots or less"
                available < 3 |=> -1000 -> "two resources or less"

            case StealGuildCardAction(f, e, c, DiscardCourtCardAction(_, GuildCard(_, SilverTongues), _)) =>
                c.is[GuildCard] |=> appraise(PayResource(c.as[GuildCard].get.suit, |(c.as[GuildCard].get.keys))) -> "gain"
                true |=> -appraise(PayResource(Psionic, |(2))) -> "gain"

            case DiscardResourceNoEffectAction(f, x, _) =>
                true |=> -10000 -> "dont discard"

            case DiscardGuildCardNoEffectAction(f, c, _) =>
                true |=> -10000 -> "dont discard"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
