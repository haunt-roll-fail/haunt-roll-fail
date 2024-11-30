package hrf.host
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

trait BaseHost {
    val gaming : hrf.base.Gaming

    val path : String

    import gaming._

    case class HostGameOverAction(winners : $[F]) extends ForcedAction

    def askBot(g : G, f : F, actions : $[UserAction]) : Action

    def askFaction(g : G, c : Continue) : Action = {
        c match {
            case ErrorContinue(e, _) =>
                throw e

            case Force(action) =>
                action

            case Log(_, _, continue) =>
                askFaction(g, continue)

            case Milestone(_, action) =>
                action

            case DelayedContinue(_, continue) =>
                askFaction(g, continue)

            case Roll(dice, rolled, tag) =>
                rolled(dice./(_.roll()))

            case Roll2(dice1, dice2, rolled, tag) =>
                rolled(dice1./(_.roll()), dice2./(_.roll()))

            case Roll3(dice1, dice2, dice3, rolled, tag) =>
                rolled(dice1./(_.roll()), dice2./(_.roll()), dice3./(_.roll()))

            case Shuffle(list, shuffled, tag) =>
                shuffled(list.shuffle)

            case Shuffle2(l1, l2, shuffled, tag) =>
                shuffled(l1.shuffle, l2.shuffle)

            case Shuffle3(l1, l2, l3, shuffled, tag) =>
                shuffled(l1.shuffle, l2.shuffle, l3.shuffle)

            case ShuffleUntil(list, shuffled, condition, tag) =>
                var r = list.shuffle

                while (!condition(r))
                    r = list.shuffle

                shuffled(r)

            case Random(list, chosen, tag) =>
                chosen(list.shuffle(0))

            case Ask(_, List(action)) =>
                action

            case MultiAsk(l) =>
                askFaction(g, l.shuffle.last)

            case Ask(f : F, actions) =>
                askBot(g, f, actions)

            case GameOver(winners, _, _) => HostGameOverAction(winners)
        }
    }

    def batch : $[() => G]

    def times : Int

    def start : StartGameAction

    def winners(a : Action) : $[F]

    def serializer : hrf.serialize.Serializer

    def factionName(f : F) : String

    def factions : $[F]

    def main(args : Array[String]) {
        var results : $[$[F]] = $

        1.to(times).foreach { i =>
            results = results ++ batch/*.par*/.map { g =>
                var log : $[String] = Nil
                def writeLog(s : String) {
                    log = s :: log
                }

                var aa : $[Action] = $

                try {
                    val game = g()

                    var continue : Continue = StartContinue
                    var a : Action = start

                    var n = 0
                    while (game.isOver.not && a.is[HostGameOverAction].not) {
                        n += 1

                        a match {
                            case a if a.isSoft =>
                            case a : ExternalAction =>
                                try {
                                    val sss = serializer.write(a.unwrap)
                                    val ppp = serializer.parseAction(sss)
                                    val aaa = serializer.write(ppp)

                                    if (sss != aaa) {
                                        println()
                                        println("UNMATCHING WRITE/PARSE")
                                        println()
                                        println()
                                        println()
                                        println("soft:" + a.isSoft)
                                        println(a)
                                        println()
                                        println(sss)
                                        println()
                                        println(ppp)
                                        println()
                                        println(aaa)
                                        println()
                                        println()
                                        println()
                                    }
                                }
                                catch {
                                    case e =>
                                        println()
                                        println("*")
                                        println("**")
                                        println("*")
                                        println()
                                        println()
                                        println()
                                        println()
                                        println()
                                        println()
                                        println()
                                        println()
                                        println()

                                        println(a)

                                        println()

                                        val sss = serializer.write(a)

                                        println(sss)

                                        val ppp = serializer.parseAction(sss)

                                        println(ppp)

                                        val aaa = serializer.write(ppp)

                                        println(aaa)
                                }

                            case _ =>
                        }

                        if (n > 3000)
                            throw null

                        continue = game.performContinue(|(continue), a, true).continue

                        aa :+= a

                        a = askFaction(game, continue)
                    }

                    val w = a.as[HostGameOverAction]./(_.winners).|(winners(a))
                    println(w.any.?(w./(factionName).mkString(", ")).|("Humanity") + " won (" + n + ")")
                    w
                }
                catch {
                    case e : Throwable if false.not =>
                        println(e)

                        import java.nio.file.{Paths, Files}
                        import java.nio.charset.StandardCharsets

                        Files.write(Paths.get(path + "/game-error-" + java.lang.System.currentTimeMillis + ".txt"), (
                            aa./(_.unwrap)./(serializer.write).mkString("\n") + "\n\n" +
                            aa./(serializer.write).mkString("\n") + "\n\n" +
                            (e.getMessage + "\n" + e.getStackTrace.mkString("\n")) + "\n\n" +
                            log.reverse.map("<div class='p'>" + _ + "</div>").mkString("\n")
                        ).getBytes(StandardCharsets.UTF_8))
                    Nil
                }
            }

            val wins = results.groupBy(w => w).view.mapValues(_.size).toMap

            println()

            wins.keys.$.sortBy(k => wins(k)).reverse.foreach { k =>
                println(k.any.?(k./(factionName).mkString(", ")).|("Humanity") + ": " + wins(k) + " " + "%6.0f".format(wins(k) * 100.0 / wins.values.sum) + "%")
            }

            println()

            factions.map { f =>
                val ww = wins.filterKeys(_.contains(f))
                val solo = ww.filterKeys(_.size == 1).values.sum
                val tie = ww.filterKeys(_.size > 1).values.sum
                (solo + tie) -> (factionName(f) + ": " + solo + "+" + tie + " " + "%6.0f".format((solo + tie) * 100.0 / wins.values.sum) + "%")
            }.sortBy(_._1).map(_._2).reverse.foreach(println)

            println("Humanity" + ": " + wins.filterKeys(_.size == 0).values.sum + " " + "%6.0f".format(wins.filterKeys(_.size == 0).values.sum * 100.0 / wins.values.sum) + "%")
            println("Total: " + results.num)
            println()
        }
    }
}
