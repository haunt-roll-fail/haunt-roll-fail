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

import scala.collection.parallel.CollectionConverters._

object Host {
    def writeLog(s : String) {



    }

    case class GameOverAction(go : GameOver) extends OracleAction

    def askFaction(g : Game, c : Continue) : Action = {
        c match {
            case Force(action) =>
                action




            case Milestone(_, action) =>
                action




            case DelayedContinue(_, continue) =>
                askFaction(g, continue)

            case Roll(dice, rolled, tag) =>
                rolled(dice./(_.roll()))

            case Shuffle(list, shuffled, tag) =>
                shuffled(list.shuffle)

            case ShuffleUntil(list, shuffled, condition, tag) =>
                var r = list.shuffle

                while (!condition(r))
                    r = list.shuffle

                shuffled(r)

            case Random(list, chosen, tag) =>
                chosen(list.shuffle(0))

            case go : GameOver =>
                GameOverAction(go)

            case Ask(_, List(action)) =>
                action

            case MultiAsk(aa) =>
                askFaction(g, aa.shuffle(0))

            case Ask(f, actions) =>
                new BotXX(f).ask(g, actions, 0)
        }
    }

    def main(args:Array[String]) {
        val allFactions : $[Faction] = Meta.factions.take(4)
        val allComb = allFactions.combinations(4).$
        val factions = Meta.factions.take(4)
        val repeat = 0.to(400).map(_ => factions)

        def allSeatings(factions : $[Faction]) = factions.permutations.$
        def randomSeating(factions : $[Faction]) = allSeatings(factions).shuffle.head

        var results : $[$[Faction]] = Nil

        val base = allSeatings(factions).flatMap(l => 20.times(l))

        1.to(20).foreach { i =>
            results = results ++ base.par.map { ff =>
                var log : $[String] = Nil
                def writeLog(s : String) {
                    log = s :: log
                }

                try {
                    val game = new Game(randomSeating(ff))



                    var aa : $[Action] = $

                    var continue : Continue = StartContinue
                    var a : Action = StartAction(gaming.version)

                    var n = 0
                    while (a.is[GameOverAction].not) {
                        n += 1


                        a match {
                            case a : Soft =>
                            case a : ExternalAction =>
                                try {

                                val sss = Serialize.write(a.unwrap)
                                val ppp = Serialize.parseAction(sss)
                                val aaa = Serialize.write(ppp)

                                if (sss != aaa) {
                                    println()
                                    println()
                                    println(a)
                                    println(sss)
                                    println(ppp)
                                    println(aaa)
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


                        if (n > 1000000)
                            throw null

                        continue = game.performContinue(|(continue), a, true).continue

                        aa :+= a

                        a = askFaction(game, continue)
                    }

                    val w = a.asInstanceOf[GameOverAction].go.winners

                    w
                }
                catch {
                    case e : Throwable if false =>
                        println(e)

                        import java.nio.file.{Paths, Files}
                        import java.nio.charset.StandardCharsets

                        Files.write(Paths.get("game-error-" + System.currentTimeMillis + ".txt"), (e.getMessage + "\n" + e.getStackTrace.mkString("\n") + log.reverse.map("<div class='p'>" + _ + "</div>").mkString("\n")).getBytes(StandardCharsets.UTF_8))
                    Nil
                }
            }

            val wins = results.groupBy(w => w).mapValues(_.size)

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
