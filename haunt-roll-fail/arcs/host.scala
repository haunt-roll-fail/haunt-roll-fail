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

import scala.collection.parallel.CollectionConverters._

object Host extends hrf.host.BaseHost {
    val gaming = arcs.gaming
    val path = "arcs"

    def askBot(g : G, f : F, actions: $[UserAction]) = new BotXX(f).ask(actions, 0)(g)

    def factions = $(Red, White, Blue, Yellow)

    def batch = {
        val allComb = factions.combinations(4).$
        val repeat = 0.to(15).map(_ => factions)

        def allSeatings(factions : $[Faction]) = factions.permutations.$
        def randomSeating(factions : $[Faction]) = allSeatings(factions).shuffle.head

        val base = repeat./(l => () => new G(l, $)).$

        base
    }

    def factionName(f : F): String = f.name

    def serializer = arcs.Serialize
    def start = StartAction(version)
    def times = 5
    def winners(a : Action) = a @@ {
        case GameOverWonAction(_, f) => $(f)
    }
}
