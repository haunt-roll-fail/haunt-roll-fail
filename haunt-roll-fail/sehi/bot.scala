package sehi
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
    def eval(actions : List[UserAction])(implicit game : Game) : List[ActionEval] = {
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

        import game._

        val known = knows(self)
        val role = roles(self)
        val party = role.party

        var friends = game.friends(self).%(alive.has)
        var enemies = game.enemies(self).%(alive.has)
        var others = alive.diff(friends).diff(enemies).but(self).shuffle.sortBy(karma)

        if (party == Liberal)
        while (others.num > 1) {
            enemies :+= others.head
            friends :+= others.last
            others = others.drop(1).dropRight(1)
        }

        def article(law : Card) = law @@ {
            case law if { println("checking law"); false } =>

            case law if party == Liberal && law.party == Fascist && fascist.num == 5 =>
                true |=> -1000 -> "last straw"

            case law if party == Liberal && law.party == Liberal && liberal.num == 4 =>
                true |=> 1000 -> "win"

            case law if party == Liberal && law.party == Fascist && self == president && fascist.num.between(3, 4) && game.enemies(self).any =>
                true |=> 800 -> "wanna kill"

            case law if party == Liberal && law.party == Liberal =>
                true |=> 500 -> "for the win"


            case law if party == Fascist && law.party == Liberal && liberal.num == 4 =>
                true |=> -1000 -> "last straw"

            case law if party == Fascist && law.party == Fascist && fascist.num == 5 =>
                true |=> 1000 -> "win"

            case law if party == Fascist && law.party == Fascist && self == president && fascist.num >= 3 =>
                true |=> 800 -> "wanna kill"

            case law if party == Fascist && law.party == Fascist && self == chancellor && friends.has(president).not && fascist.num >= 3 =>
                true |=> -700 -> "fear revenge"

            case law if party == Fascist && law.party == Fascist && self == chancellor && friends.has(president).not && role == Hitler =>
                true |=> -600 -> "stay low"

            case law if party == Fascist && law.party == Fascist =>
                true |=> 500 -> "for the chaos"

            case law =>
                true |=> 0 -> "i just dont know"


        }

        a.unwrap match {
            case VoteAction(_, p, c, Yes) if true =>
                true |=> 5000 -> "testing"

            case VoteAction(_, p, c, Yes) if self == p =>
                true |=> 500 -> "me for president"

            case VoteAction(_, p, c, Yes) if party == Fascist && fascist.num >= 3 && known.has(c) && roles(c) == Hitler =>
                true |=> 1000 -> "yes hitler"

            case VoteAction(_, p, c, No) if party == Liberal && fascist.num >= 3 && enemies.has(p) =>
                true |=> 1000 -> "not hitler"

            case VoteAction(_, p, c, No) if party == Fascist && liberal.num == 4 && friends.has(p).not =>
                true |=> 1000 -> "unfriendly candidate"

            case VoteAction(_, p, c, Yes) if friends.has(p) =>
                true |=> 500 -> "friend for president"

            case VoteAction(_, p, c, No) if party == Liberal && enemies.has(p) =>
                true |=> 500 -> "enemy begone"

            case VoteAction(_, p, c, Yes) if friends.has(c) =>
                true |=> 300 -> "friend for chancellor"

            case VoteAction(_, p, c, No) if party == Liberal && enemies.has(c) =>
                true |=> 300 -> "enemy begone"

            case VoteAction(_, p, c, Yes) =>
                true |=> 4 -> "yes man"


            case NominateChancellorAction(_, f) if party == Fascist =>
                true |=> 0 -> "be vague"

            case NominateChancellorAction(_, f) if party == Liberal && friends.has(f) =>
                true |=> 1000 -> "friend for chancellor"

            case ChancellorSelectedAction(_, _, a) =>
                article(a)

            case PresidentialSelectedAction(_, _, a, b) =>
                article(a)
                article(b)

                party == Liberal && self == president && a.party != b.party |=> 700 -> "check chancellor"

            case ExecuteFactionConfirmAction(_, f) =>
                friends.has(f) |=> -500 -> "spare friend"
                enemies.has(f) |=> 500 -> "kill enemy"
                game.friends(self).has(f) |=> -1000 -> "spare real friend"
                game.enemies(self).has(f) |=> 1000 -> "kill real enemy"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
