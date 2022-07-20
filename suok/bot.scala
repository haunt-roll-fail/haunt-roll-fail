package suok

import colmat._

class BotXX(f : Faction) extends EvalBot {
    def eval(game : Game, actions : List[UserAction]) : List[ActionEval] = {
        val ev = new GameEvaluation(game, f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluation(val game : Game, val self : Faction) {
    def eval(a : Action) : List[Evaluation] = {
        var result : List[Evaluation] = Nil
    
        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }
    
        import game.factions
        import game.faction2player
        
        val alive = factions.%(_.alive)

        val (roles, variants) = {
            val f = self

            val roles : List[Role] = (f.expectation.neutrals.num == game.roles.%(_.side == Neutral).num).?(game.roles).|(game.roles.%(_.side != Neutral) ++ Roles.neutrals)
        
            val rm = roles.diff(factions.%(_.alive.not)./(_.role))

            val rs = alive./(e => e -> f.expectation.states./~(_(e)).distinct).toMap

            (rm, rs)
        }

        implicit class faction2knowledge(f : Faction) {
            def is(r : Role) = variants(f) == List(r)
            def can(r : Role) = variants(f).contains(r)
            def free = f.alive && f.jailed.not && f.chained.not
            def active(r : Role) = f.alive && f.open && f.jailed.not && f.chained.not && f.role == r
            def next = game.next(f)
            def prev = game.prev(f)
        }
        

        a match {
            case _ : ForcedAction => 

            case ExecuteAction(f, e) =>
                e.is(Assassin) |=> 500 -> "execute assassin"
                e.prev.active(Slave) && e.next.active(Slave) && e.next.next.active(Slave) |=> -500 -> "accidental revolt"
                e.prev.prev.active(Slave) && e.prev.active(Slave) && e.next.active(Slave) |=> -500 -> "accidental revolt"
                e.active(Slave) |=> 200 -> "ok i guess"
                e.is(Slave) |=> 100 -> "whatever"

            case AvoidArrestAction(f) if f.role == Sultan =>
                return eval(RoyalClaimAction(f, null))
                
            case RoyalClaimAction(f, then) =>
                roles.has(Assassin).not |=> 10000 -> "safe to claim"

                f.next.is(Guard) && f.next.next.can(Dancer).not |=> 5000 -> "guarded"
                f.prev.is(Guard) && f.prev.prev.active(Dancer).not |=> 5000 -> "guarded"

                alive.%(_.can(Assassin)).%(e => (e.next.is(Guard).not || e.next.next.can(Dancer)) && (e.prev.is(Guard).not || e.prev.prev.can(Dancer))).none |=> 4000 -> "assassins neutralized"
                
                true |=> -1000 -> "danger"

            case HideMainAction(f, _) if f.role == Sultan =>
                true |=> -1000 -> "dont"
 
                val t = (alive ++ alive).dropWhile(_ != f).drop(1).takeWhile(_.coronation.not)
 
                val g = (f.prev.prev.prev :: f.prev.prev :: f.prev :: f.next :: f.next.next :: f.next.next.next).%(_.is(Guard)).%(g => g.next.can(Dancer).not && g.prev.can(Dancer).not).num
                
                val a = t.%(_.jailed.not).%(_.can(Assassin))
                
                val z = a.num - t.%(_.is(Guard)).%(g => g.next.can(Dancer).not && g.prev.can(Dancer).not).%(g => a.has(g.next) || a.has(g.prev)).num - a.%(_.open).any.??(1)

                z > g |=> 10000 -> "real assassination threat"

            case AssassinateAction(f, e) =>
                val dg = (f.prev :: f.next :: e.prev :: e.next).but(f).but(e).%(_.is(Guard)).%(g => g.prev.active(Dancer).not && g.next.active(Dancer).not)
                dg.any |=> -15000 -> "death sentence"

                e.is(Sultan) |=> 10000 -> "target locked"

                val pg = (f.prev :: f.next :: e.prev :: e.next).but(f).but(e).%(_.can(Guard)).%(g => g.prev.active(Dancer).not && g.next.active(Dancer).not)
                pg.any |=> -5000 -> "danger"

                e.can(Sultan) && alive.%(_.can(Sultan)).num <= 2 |=> 1000 -> "very good"
                e.can(Guard) && roles.%(_ == Assassin).num > 1 |=> 800 -> "good"
                e.can(Assassin) |=> -900 -> "very bad"
                e.can(Slave) |=> -700 -> "bad"
                e.can(Dancer) |=> -600 -> "bad"
                e.can(Hunter) |=> 500 -> "ok"
                
            case InterceptAction(f, a, e) =>
                true |=> 1000 -> "why not"
                    
            case ArrestAction(f, e) =>
                e.is(Sultan) |=> -1000 -> "stupid"
                e.is(Guard) |=> -1000 -> "stupid"
                e.is(Assassin) |=> 5000 -> "yes"
                e.can(Assassin) && alive.%(_.can(Assassin)).num <= roles.%(_ == Assassin).num * 2 |=> 1000 -> "try guess"
                e.open.not && e.can(Slave) && e.next.can(Slave).not |=> 300 -> "break last chain"
                e.can(Sultan).not && e.can(Guard).not |=> 100 -> "ok"

                if (alive.%(_.coronation).any) {
                    val t = (alive ++ alive).dropWhile(_ != f).drop(1).takeWhile(_.coronation.not)

                    t.none || t.has(e) |=> 10000 -> "arrest arrest arrest"

                    e.open.not && e.can(Vizier) |=> 1000 -> "suspicious vizier"
                    e.can(Dancer) && e == f.next && f.prev.is(Sultan) |=> 1000 -> "suspicious dancer"
                    e.can(Dancer) && e.prev.can(Guard) && e.prev.prev.can(Assassin) |=> 1000 -> "suspicious dancer"
                    e.can(Slave) |=> 500 -> "just in case"
                }

            case SupportRevoltAction(f, _) =>
                roles.%(_ == Slave).num > 3 && alive.%(e => e.active(Slave) && e.prev.active(Slave) && e.next.active(Slave)).any |=> 10000 -> "join win"
                f.prev.active(Slave) && f.next.active(Slave) |=> 10000 -> "win"
                f.prev.active(Slave) && f.next.can(Slave) && f.next.jailed.not |=> 1000 -> "try at least"
                f.next.active(Slave) && f.prev.can(Slave) && f.prev.jailed.not |=> 1000 -> "try at least"

                f.next.coronation |=> 1000 -> "nothing to lose"
                
            case PeekAction(f, e) =>
                variants(e).num == 1 |=> -1000 -> "stupid"
                variants(e).num == 2 |=> 200 -> "ok"
                variants(e).num == 3 |=> 300 -> "ok"
                variants(e).num == 4 |=> 400 -> "ok"
                variants(e).num == 5 |=> 500 -> "ok"
                variants(e).num == 6 |=> 600 -> "ok"
                variants(e).num == 7 |=> 700 -> "ok"
                variants(e).num == 8 |=> 800 -> "ok"
                e.can(Guard)    && (e.next.is(Sultan) || e.prev.is(Sultan)) |=> 100 -> "near sultan"
                e.can(Assassin) && (e.next.is(Guard) || e.prev.is(Guard)) |=> 100 -> "near guard"
                e.can(Guard)    && (e.next.is(Assassin) || e.prev.is(Assassin)) |=> 100 -> "near assassin"
                e.can(Slave)    && e.next.is(Slave) && e.prev.is(Slave) |=> 100 -> "near slaves"
                e.can(Slave)    && e.next.is(Slave) && e.next.next.is(Slave) |=> 100 -> "near slaves"
                e.can(Slave)    && e.prev.is(Slave) && e.prev.prev.is(Slave) |=> 100 -> "near slaves"

            case _ =>
        }

        result.none |=> 0 -> "none"
      
        true |=> -((1 + math.random() * 7).round.toInt) -> "random"
        
        result.sortBy(v => -abs(v.weight))
    }
}
      