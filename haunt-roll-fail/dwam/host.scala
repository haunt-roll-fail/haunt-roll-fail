package dwam
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

object Host {
    def askFaction(g : Game, c : Continue) : Action = {
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

            case Shuffle(list, shuffled, tag) =>
                shuffled(list.shuffle)

            case Shuffle2(l1, l2, shuffled, tag) =>
                shuffled(l1.shuffle, l2.shuffle)

            case Shuffle3(l1, l2, l3, shuffled, tag) =>
                shuffled(l1.shuffle, l2.shuffle, l3.shuffle)

            case ShuffleUntil(list, condition, shuffled, tag) =>
                var r = list.shuffle

                while (!condition(r))
                    r = list.shuffle

                shuffled(r)

            case Random(list, chosen, tag) =>
                chosen(list.shuffle(0))

            case Ask(_, List(action)) =>
                action

            // case Ask(f : Knight.type, actions) =>
            //     new BotKnight(f).ask(g, actions, 0)

            // case Ask(f : Dragon.type, actions) =>
            //     new BotDragon(f).ask(g, actions, 0)

            case Ask(f : Faction, actions) =>
                new BotXX(f).ask(g, actions, 0)
        }
    }

    def main(args:Array[String]) {
        val allFactions : $[Faction] = $(Red, Green, Blue, Yellow)
        val allComb = allFactions.combinations(4).$
        val factions = allFactions
        val repeat = 0.to(15).map(_ => factions)

        def allSeatings(factions : $[Faction]) = factions.permutations.$
        def randomSeating(factions : $[Faction]) = allSeatings(factions).shuffle.head

        var results : $[$[Faction]] = Nil

        val base = repeat

        1.to(20).foreach { i =>
            results = results ++ base/*.par*/.map { ff =>
                var log : $[String] = Nil
                def writeLog(s : String) {
                    log = s :: log
                }

                var aa : $[Action] = $

                try {
                    val seating = randomSeating(ff)
                    val game = new Game(StandardBoard, seating, $)

                    var continue : Continue = StartContinue
                    var a : Action = StartAction(dwam.version)

                    var n = 0
                    while (a.is[GameOverAction].not) {
                        n += 1

                        a match {
                            case a if a.isSoft =>
                            case a : ExternalAction =>
                                try {
                                    val sss = Serialize.write(a.unwrap)
                                    val ppp = Serialize.parseAction(sss)
                                    val aaa = Serialize.write(ppp)

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

                                        val sss = Serialize.write(a)

                                        println(sss)

                                        val ppp = Serialize.parseAction(sss)

                                        println(ppp)

                                        val aaa = Serialize.write(ppp)

                                        println(aaa)
                                }

                            case _ =>
                        }

                        if (n > 2000)
                            throw null

                        continue = game.performContinue(|(continue), a, true).continue

                        aa :+= a

                        a = askFaction(game, continue)
                    }

                    val w = a.asInstanceOf[GameOverAction].winners
                    println(w.any.?(w./(_.name).mkString(", ")).|("Humanity") + " won (" + n + ")")
                    w
                }
                catch {
                    case e : Throwable if false.not =>
                        println(e)

                        import java.nio.file.{Paths, Files}
                        import java.nio.charset.StandardCharsets

                        Files.write(Paths.get("dwam/game-error-" + System.currentTimeMillis + ".txt"), (
                            aa./(_.unwrap)./(Serialize.write).mkString("\n") + "\n\n" +
                            aa./(Serialize.write).mkString("\n") + "\n\n" +
                            (e.getMessage + "\n" + e.getStackTrace.mkString("\n")) + "\n\n" +
                            log.reverse.map("<div class='p'>" + _ + "</div>").mkString("\n")
                        ).getBytes(StandardCharsets.UTF_8))
                    Nil
                }
            }

            val wins = results.groupBy(w => w).view.mapValues(_.size).toMap

            println()

            wins.keys.$.sortBy(k => wins(k)).reverse.foreach { k =>
                println(k.any.?(k./(_.name).mkString(", ")).|("Humanity") + ": " + wins(k) + " " + "%6.0f".format(wins(k) * 100.0 / wins.values.sum) + "%")
            }

            println()

            allFactions.map { f =>
                val ww = wins.filterKeys(_.contains(f))
                val solo = ww.filterKeys(_.size == 1).values.sum
                val tie = ww.filterKeys(_.size > 1).values.sum
                (solo + tie) -> (f.name + ": " + solo + "+" + tie + " " + "%6.0f".format((solo + tie) * 100.0 / wins.values.sum) + "%")
            }.sortBy(_._1).map(_._2).reverse.foreach(println)

            println("Humanity" + ": " + wins.filterKeys(_.size == 0).values.sum + " " + "%6.0f".format(wins.filterKeys(_.size == 0).values.sum * 100.0 / wins.values.sum) + "%")
            println("Total: " + results.num)
            println()
        }
    }
}
