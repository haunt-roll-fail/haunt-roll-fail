package root

import root.gaming._

import colmat._

import hrf.base._
import hrf.bot._

class GameEvaluationVB(game : Game, faction : Hero) extends GameEvaluation(game, faction) {
    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil
        
        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val g = game

        import g._

        val vb = g.vb(self)

        val suit : Suit = vb.region match {
            case c : Clearing => mapping(c)
            case _ => Bird
        }

        val more = 6 + vb.ready(Bag) * 2 > vb.inv.diff(vb.dontCount).num

        val ai = vb.ready(Sword) + vb.ready(Hammer) + vb.ready(Torch) + vb.ready(Boots) + vb.ready(Crossbow)
        val ac = vb.region.clearing./(c => vb.hand.%(_.suit.m(c.suit)).num).|(0)
        val aid = min(ai, ac)

        def valueCard(d : DeckCard) = d match {
            case CraftEffectCard(_, _, c, _) if c.distinct.num > 1 => 150
            case CraftEffectCard(_, _, c, _) if c.num > 2 + (vb.character == Tinker).??(1) => 150

            case CraftItemCard(_, _, c, _, _, _) if c.distinct.num > 1 => 150
            case CraftItemCard(_, _, c, _, _, _) if c.num > 2 + (vb.character == Tinker).??(1) => 150
            case CraftItemCard(_, _, _, i, _, _) if g.uncrafted.contains(i).not => 150

            case CraftItemCard(_, _, c, _, _, _) if c.distinct == List(suit) && c.num <= vb.ready(Hammer) => 1200

            case Favor(_, _, _) if vb.character != Tinker => 200

            case Dominance(_) => 500

            case CraftEffectCard(_, _, _, _) => 600

            case CraftItemCard(_, _, _, _, _, _) => 700
            
            case Favor(_, _, _) if vb.character == Tinker => 800

            case Ambush(s) if s != Bird => 900
            case Ambush(Bird) => 1000
        }

        a.unwrap match {
            case t : ThenAction => 
                ???
            
 
            case StartingForestAction(_, r) => 
                true |=> g.board.ruins.%(g.board.fromForest(r).contains).num * (math.random() * 10).round.toInt * 100 -> "ruins"

            case MoveListAction(_, _, _, from : Clearing, r : Forest, _, _) =>
                vb.inv.damaged.num > 3 |=> 5000 -> "heal"
                vb.inv.damaged.num > 0 && vb.ready(Hammer) == 0 |=> 4000 -> "heal"
                vb.inv.exhausted.num > vb.inv.notExhausted.num |=> 3000 -> "rest"
                true |=> -1000 -> "skip"

            case DaylightStartAction(_) =>
                g.ruins.contains(vb.region) && vb.ready(Torch) > 0 |=> 1000 -> "ruins"

            case VagabondExploreMainAction(_, _) => 
                true |=> 10000 -> "explore"

            case TakeRuinItemAction(_, _, i, _) =>
                more || i == Bag |=> 10000 -> "explore and take"
                true |=> -1000 -> "or dont"

            case MoveListAction(_, _, _, from, c : Clearing, _, _) =>
                vb.ready(Torch) > 0 && g.ruins.contains(c) |=> 500 -> "go to ruins"
                g.ruins.contains(c) |=> 300 -> "go to ruins"
                g.ruins.contains(from) |=> -400 -> "stay in ruins" 
                g.board.connected(c).intersect(g.ruins.keys.toList).any |=> 250 -> "go nearer ruins" 
                true |=> 200 -> "go"
                vb.hand.%(_.suit.m(c.suit)).num == 1 |=> 300 -> "have suit 1"
                vb.hand.%(_.suit.m(c.suit)).num == 2 |=> 310 -> "have suit 2"
                vb.hand.%(_.suit.m(c.suit)).num == 3 |=> 320 -> "have suit 3"
                vb.hand.%(_.suit.m(c.suit)).num >= 4 |=> 330 -> "have suit 4"
                g.quests.take(3).%(_.suit == c.suit).%(q => List(q.a, q.b).diff(vb.inv.ready./(_.item).diff(List(Boots))).none).any |=> 350 -> "go quest"

            case AidExhaustAction(_, e, d, i) =>
                vb.attitude(e) == Hostile |=> -1000 -> "dont aid hostile"
                vb.attitude(e) == Indifferent |=> 350 -> "ind -> ami"
                vb.attitude(e) == Amiable && vb.aid(e) + aid >= 2 |=> 450 -> "ami -> fri"
                vb.attitude(e) == Friendly && vb.aid(e) + aid >= 3 |=> 550 -> "fri -> all"
                vb.attitude(e) == Allied |=> 650 -> "allied aid"
                more && g.of(e).forTrade.any |=> 2000 -> "get item"
                !more && g.of(e).forTrade.contains(Bag) |=> 2000 -> "get bag"
                !more && g.of(e).forTrade.contains(Teapot) |=> 2000 -> "get teapot"
                !more && g.of(e).forTrade.contains(Coins) |=> 2000 -> "get coins"
                i == Bag |=> -900 -> "bag"
                i == Teapot |=> -800 -> "teapot"
                i == Coins |=> -700 -> "coins"
                i == Torch |=> -300 -> "torch"
                i == Hammer |=> -300 -> "hammer"
                i == Crossbow |=> -200 -> "crossbow"
                i == Sword |=> -150 -> "sword"
                i == Boots |=> -100 -> "boots"
                true |=> -valueCard(d) -> "value card"

            case AidTakeItemAction(_, _, _, i) =>
                i.item == Bag |=> 9000 -> "bag"
                i.item == Teapot |=> 8000 -> "teapot"
                i.item == Coins |=> 7000 -> "coins"
                i.item == Torch |=> 6000 -> "torch"
                i.item == Hammer |=> 5000 -> "hammer"
                i.item == Crossbow |=> 4000 -> "crossbow"
                i.item == Sword |=> 3000 -> "sword"
                i.item == Boots |=> 2000 -> "boots"
                
            case ReadyItemsAction(_, l, _) =>
                l.foreach { r =>
                    val i = r.item
            
                    r.damaged |=> -10000 -> "damaged"
                    i == Bag |=> 9000 -> "bag"
                    i == Teapot |=> 8000 -> "teapot"
                    i == Coins |=> 7000 -> "coins"
                    i == Torch |=> 6000 -> "torch"
                    i == Hammer |=> 5000 -> "hammer"
                    i == Crossbow |=> 4000 -> "crossbow"
                    i == Sword |=> 3000 -> "sword"
                    i == Boots |=> 2000 -> "boots"
                }

            case DoneAction(EveningStartAction(VB)) =>
                true |=> 100 -> "done enough"

            case CraftAssignAction(_, CraftItemCard(_, _, _, i, _, _), _, _, _, _) =>
                i == Bag |=> 9000 -> "bag"
                i == Teapot |=> 8000 -> "teapot"
                i == Coins |=> 7000 -> "coins"
                i == Torch |=> 6000 -> "torch"
                i == Hammer |=> 5000 -> "hammer"
                i == Crossbow |=> 4000 -> "crossbow"
                i == Sword |=> 3000 -> "sword"
                i == Boots |=> 2000 -> "boots"
                
            case CraftAssignAction(_, _, _, _, _, _) =>
                true |=> 1000 -> "craft"

            case QuestAction(_, _, _, _, _) =>
                true |=> 500 -> "quest"

            case QuestRewardCardsAction(_, _, 2) =>
                true |=> 200 -> "cards"
                vb.hand.num + 2 + 1 + vb.ready(Bag) > 5 |=> -300 -> "overflow"

            case QuestRewardVPAction(_, _, n) =>
                true |=> (n * 100) -> "vps"
                vb.coalition.any |=> -1000 -> "coalition"

            case RepairItemAction(_, ItemRef(i, exh, _)) =>
                !exh |=> 200 -> "repair not exh"
                !exh && i == Hammer |=> 100000 -> "repair not exh hammer"
                i == Sword |=> 10000 -> "sword"
                i == Bag |=> 9000 -> "bag"
                i == Teapot |=> 8000 -> "teapot"
                i == Coins |=> 7000 -> "coins"
                i == Torch |=> 6000 -> "torch"
                i == Hammer |=> 5000 -> "hammer"
                i == Crossbow |=> 4000 -> "crossbow"
                i == Boots |=> 2000 -> "boots"

            case StrikeAction(_, _, e, p) =>
                p == Base(Fox) |=> 1200 -> "strike"
                p == Base(Rabbit) |=> 1200 -> "strike"
                p == Base(Mouse) |=> 1200 -> "strike"
                p == Sympathy |=> 1000 -> "strike"
                p == Roost |=> 900 -> "strike"
                p == Recruiter |=> 800 -> "strike"
                p == Sawmill |=> 700 -> "strike"
                p == Workshop |=> 600 -> "strike"
                
                p == Critter && vb.attitude(e) == Hostile |=> 400 -> "strike"
                p == Cat && vb.attitude(e) == Hostile |=> 300 -> "strike"
                p == Hawk && vb.attitude(e) == Hostile |=> 200 -> "strike"
                
            case DiscardCardAction(_, _, _, d, _, _) =>
                true |=> -valueCard(d) -> "value"

            case OutrageCardAction(_, _, d, _, _) =>
                true |=> -valueCard(d) -> "value"

            case DayLaborAction(_, d) =>
                true |=> valueCard(d) -> "day labor"

            case StealAction(_, e) =>
                true |=> (200 - g.of(e).hand.num * 10) -> "steal"

            case VagabondHideoutMainAction(_) =>
                vb.inv.damaged.num >= 3 |=> 300 -> "hideout 3+"
                vb.inv.damaged.num == 2 |=> 200 -> "hideout 2"

            case BattleStartAction(_, _, _, r, e, _, _) =>
                e.hero.none && vb.attitude(e) != Hostile && g.at(r, e)./~(_.warrior).any |=> -5000 -> "dont go hostile"
                g.at(r, e)./~(_.warrior).none |=> 1500 -> "no warriors"
                g.at(r, e)./~(_.warrior).num < vb.inv.notDamaged.count(Sword) |=> 400 -> "stronger"
                g.at(r, e)./~(_.warrior).num < vb.inv.exhausted.notDamaged.num |=> 100 -> "enough fodder"

            case BattleAmbushAction(_, _, _, _, _) =>
                true |=> 1000 -> "ambush"

            case BattleCounterAmbushAction(_, _, _, _, _) =>
                true |=> 1000 -> "ct-ambush"
                
                
            case PayExhaustItemsAction(_, s, n, items, GetServicesAction(_, _, List(offers))) =>
                true |=> 50 - (math.pow(10, 2 + n + items.count(Teapot)) * math.random()).round.toInt -> "cost"

                
            case _ =>
        }


        result.none |=> 0 -> "none"

        result.sortBy(v => -abs(v.weight))
    }

}
      