package vast
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

class BotDragon(f : Dragon.type) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        val ev = new GameEvaluationDragon(f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluationDragon(val self : Dragon.type)(implicit val game : Game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        import game._

        if (states.contains(self).not)
            return $

        if (self.position.none)
            return $

        val position = self.position.get

        a.unwrap match {
            case DragonEndAction(_) =>
                true |=> -1000 -> "dont end turn"

            case DragonMoveAction(_, dir, cost, _) =>
                cost == $(FreeMove) |=> 10 -> "use free move"

                val dest = position.add(dir)

                Knight.position.use(p => position.dist(p) < dest.dist(p)) |=>   400 -> "move away from knight"
                Knight.position.use(p => position.dist(p) > dest.dist(p)) |=> -1200 -> "dont move nearer to knight"

                if (self.hunger.value * 2 > self.eaten && self.awake.not) {
                    Goblins.tribes./~(_.position).foreach { p =>
                        dest.dist(p) < position.dist(p) |=> 50 -> "move nearer to goblins"
                    }
                }

                board.pattern(dest, Diamond).but(position).forall(p => board.get(p) == Emptiness) |=> -20 -> "dead end"

                if (self.greed.value > self.treasures && self.awake.not) {
                    val n = board.list(position).count(Chest)

                    board.list(dest).count(Chest) > n |=> 1300 -> "move to chest"
                    board.list(dest.add(dir)).count(Chest) > n |=> 1200 -> "move closer to chest"
                    board.pattern(dest, Diamond).but(position).exists(p => board.list(p).count(Chest) > n) |=> 300 -> "move closer to chests"
                    board.list(dest).count(Chest) < n |=> -1100 -> "dont move from chest"
                }

                // if (self.prideE.available) {
                //     board.pattern(dest, Around).but(position).count(p => board.list(p).count(Chest) > board.list(position).count(Chest)) |=> 1300 -> "move to chest"
                // }

                if (self.awake && self.underground) {
                    val l = board.inner.%(p => board.get(p).as[Explored].?(_.original.?(_.tokens.has(Crystal))))

                    if (l.any) {
                        l./(_.dist(dest)).min < l./(_.dist(position)).min |=> 10000 * (99 - l./(_.dist(dest)).min) -> "move to crystal"
                        board.entrance.dist(dest) < board.entrance.dist(position) |=> 5000 -> "move to entrance"
                        l.has(position) |=> -1000000 -> "dont move"
                    }
                }

                if (self.awake && self.underground.not) {
                    board.entrance.dist(dest) < board.entrance.dist(position) |=> 1000000 -> "move to entrance"
                    board.entrance.dist(dest) > board.entrance.dist(position) |=> -100000 -> "dont move from entrance"
                }

            case DragonAttackMainAction(_, _) =>
                Goblins.positions.intersect(board.pattern(position, FullSquare)).any  |=> (self.hunger.value * 2 > self.eaten).?(1000).|(100) -> "attack goblins"
                Goblins.positions.intersect(board.pattern(position, FullSquare)).none |=> -1500 -> "no goblins to attack"

            case DragonAttackAction(_, p) =>
                Goblins.positions.intersect(board.pattern(position, p)).none |=> -500 -> "no goblins to attack"

            case DragonHissAction(_, f, t, _) =>
                self.hunger.value * 2 > self.eaten |=> (f.tribe(t).position.get.dist(position) * 10 + 200) -> "hiss goblins far away"

            case DragonScorchMainAction(_, _) =>
                self.prideE.available |=> board.pattern(position, Around).count(p => board.get(p).is[HiddenTile]) * 100 -> "scorch"
                self.underground.not |=> 10000 -> "if blocked by hidden tiles"

            case DragonRevealMainAction(_, _) =>
                self.prideE.available |=> board.pattern(position, FullSquare).count(p => board.get(p).is[HiddenTile]) * 50 -> "reveal"
                self.underground.not |=> 5000 -> "if blocked by hidden tiles"

            case DragonRevealAction(_, p) =>
                board.pattern(position, p).exists(p => board.get(p).is[HiddenTile]).not |=> -500 -> "no tiles to reveal"

            case DragonWrathMainAction(_, _) =>
                self.underground.not |=> 3000 -> "wrath if blocked"

            case DragonWrathAction(_, p) =>
                board.pattern(position, p).exists(p => board.get(p) == Emptiness).not |=> -500 -> "no tiles to remove"

            case ForceMoveStraightAction(_, Knight, dir, n, _) =>
                Knight.position.add(dir).dist(position) > Knight.position.dist(position) |=> 100 -> "move knight away"

            case DragonPickTreasureAction(_, _) =>
                true |=> 1000 -> "pick treasure"

            case DragonPlaceGemAction(_, g) =>
                g == Flame |=> 300 -> "gem"
                g == Claw |=> 200 -> "gem"
                g == Wing |=> 100 -> "gem"

            case EndPlayerTurnAction(_) =>
                true |=> -1000 -> "dont end turn"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
