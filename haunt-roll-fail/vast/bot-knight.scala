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

class BotKnight(f : Knight.type) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : $[ActionEval] = {
        val ev = new GameEvaluationKnight(f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluationKnight(val self : Knight.type)(implicit val game : Game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        import game._

        if (states.contains(self).not)
            return $

        a.unwrap match {
            case EndPlayerTurnAction(_) =>
                true |=> -1000 -> "dont end turn"

            case KnightShieldAction(_, _) =>
                true |=> 200 -> "shield always good"

            case KnightMoveAction(_, dir, map, encounter) =>
                val dest = self.position.add(dir)

                encounter && board.list(dest).has(Event).not && self.grit < 10 |=> 500 -> "encounter"
                encounter.not |=> 400 -> "no encounter"
                self.path.has(dest) |=> -600 -> "was there"
                map |=> -100 -> "avoid map"
                board.list(dest).has(Chest) |=> 1000 -> "go for treasure"
                dest.dist(Relative(0, 0)) < self.position.dist(Relative(0, 0)) |=> 20 -> "keep closer to entrance"

                Dragon.position.foreach { p =>
                    dest.dist(p) < self.position.dist(p) |=> {
                        if (self.position.dist(p) / 2 + Dragon.armor < self.free + self.strength)
                            1000
                        else
                            50
                    } -> "approach dragon"
                }

            case AttackAmbushAction(_, _, _) =>
                true |=> 1000 -> "bring it on"

            case AttackAction(_, Dragon, _, _, _) =>
                true |=> 2000 -> "attack dragon"

            case AttackAction(_, Crystal, None, _, _) =>
                true |=> 3000 -> "attack crystal"

            case AttackAction(_, _, None, _, _) =>
                true |=> 1500 -> "attack something"

            case EncounterCollectAction(_) =>
                true |=> 1000 -> "continue"

            case KnightRevealTreasureAction(_, _) =>
                true |=> -2000 -> "don't reveal"

            case ClaimQuestAction(_, _, _) =>
                true |=> 10000 -> "claim quest"

            case StartEncounterAction(_) =>
                Dragon.position.has(self.position) |=> 2000 -> "attack dragon"
                board.list(self.position).has(Chest) |=> 1000 -> "take treasure"
                board.list(self.position).of[DragonGem].any |=> 1000 -> "take dragon gem"
                board.list(self.position).has(Event) |=> 10000 -> "dont delay event"

            case MightyAxeAction(_, _, _, _) =>
                true |=> 1000 -> "mighty axe"

            case CollectChestAction(_) =>
                true |=> 1000 -> "grab treasure"

            case CollectDragonGemAction(_, _) =>
                true |=> 500 -> "collect gem"

            case KnightAssignAction(_, Strength, EncounterAttacksAction(_, _, _)) if Dragon.position.has(self.position) =>
                Dragon.armor >= self.strength |=> 3000 -> "stronger"

            case KnightAssignAction(_, ElvishSword, EncounterAttacksAction(_, _, _)) if Dragon.position.has(self.position) =>
                Dragon.armor >= self.strength |=> 3300 -> "stronger"

            case KnightAssignAction(_, MightyAxe, EncounterAttacksAction(_, _, _)) if Dragon.position.has(self.position) =>
                Dragon.armor >= self.strength |=> 3600 -> "stronger"

            case KnightAssignAction(_, Strength, _) =>
                true |=> 250 -> "stronger"

            case KnightAssignAction(_, p, _) if self.perception <= self.encounters && (p == Perception || p == ElvishSword || p == PixieLantern) =>
                p == ElvishSword |=> 20 -> "elvish sword"
                p == PixieLantern |=> 10 -> "pixie lantern"
                self.free > 1 || self.movement > self.moves |=> 280 -> "more encounters"

            case KnightAssignAction(_, p, _) if self.movement <= self.moves && (p == Movement || p == HeroicBoots || p == PixieLantern) =>
                p == PixieLantern |=> 20 -> "pixie lantern"
                p == HeroicBoots |=> 10 -> "heroic boots"
                self.free > 1 || self.perception > self.encounters |=> 270 -> "more moves"


            case TakeTreasureAction(_, EquipmentTreasure(q)) =>
                self.grit > 30    |=> 1000 -> "take anyway"
                q == PotionKit    |=> -200 -> "dont take potion kit"
                q == EnchantedBow |=> -100 -> "dont take enchanted bow"
                q == PixieLantern |=> 1000 -> "take pixie lantern"
                q == ElvishSword  |=> 1000 -> "take elvish sword"
                q == MightyAxe    |=> 1000 -> "take mighty axe"
                q == HeroicBoots  |=>  500 -> "take heroic boots"
                q == Javelin      |=>    0 -> "doubt javelin"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
