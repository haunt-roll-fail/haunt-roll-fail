package root
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

import fansi._

import scala.jdk.CollectionConverters._
import scala.collection.parallel.CollectionConverters._

import java.nio.file.{Paths, Files}


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

            case Roll(dice, rolled, _) =>
                rolled(dice./(_.roll()))

            case Shuffle(list, shuffled, _) =>
                shuffled(list.shuffle)

            case ShuffleUntil(list, condition, shuffled, _) =>
                var r = list.shuffle

                while (!condition(r))
                    r = list.shuffle

                shuffled(r)

            case Random(list, chosen, _) =>
                chosen(list.shuffle(0))

            case MultiAsk(l, _) =>
                askFaction(g, l.shuffle.head)

            case Ask(_, List(action)) =>
                action

            case Ask(f : Faction, actions) =>
                new BotXX(f).ask(actions, 0)(g)
        }
    }

    def writeFaction(f : Faction) : String = (f match {
        case MC => fansi.Color.True(0xd26228)(f.name)
        case BK => fansi.Color.True(0x2d9dd7)(f.name)
        case ED => fansi.Color.True(0x3b65a3)(f.name)
        case WA => fansi.Color.True(0x62ac4d)(f.name)
        case AF => fansi.Color.True(0xe1c012)(f.name)
        case VB => fansi.Color.True(0xccccab)(f.name)
        case NB => fansi.Color.True(0x000000)(fansi.Back.True(0xffffff)(f.name))
        case MB => fansi.Color.True(0x0000ff)(f.name)
        case RF => fansi.Color.True(0x5ebbab)(f.name)
        case SF => fansi.Color.True(0xe5ad02)(f.name)
        case LC => fansi.Color.True(0xb4e553)(f.name)
        case CC => fansi.Color.True(0x6615ad)(f.name)
        case RI => fansi.Color.True(0xdb0092)(f.name)
        case UD => fansi.Color.True(0xecba98)(f.name)
        case LH => fansi.Color.True(0xcf002c)(f.name)
        case KI => fansi.Color.True(0xa1b0b0)(f.name)
        case OK => fansi.Color.True(0x949599)(f.name)
        case XC => fansi.Color.True(0x4c3896)(f.name)
        case CUv2 => fansi.Color.True(0xc7afd9)(f.name)
        case FH => fansi.Color.True(0x989745)(f.name)
        case TD => fansi.Color.True(0x3c7174)(f.name)
        case _ : CommonInvasive => fansi.Color.True(0x3c7174)(f.name)
        case _ => fansi.Color.DarkGray(f.name)
    }).render

    def main(args : Array[String]) {
        val start = System.currentTimeMillis

        def allSeatings(l : $[Faction]) = l.permutations.$
        def allSeatingsCatFirst(l : $[Faction]) = l.permutations.$.%(s => s.contains(MC).?(s(0) == MC).|(true))
        def randomSeating(l : $[Faction]) = allSeatings(l).shuffle.head

        val factions : $[Faction] = $(MC, ED, WA, VB, RF, LC, CC, UD, KI, LH, XC, CUv2, FH, LDvD, TCvA, KDvA)
        val extra : $[Faction] = $(BK, OK, AF, SF, NB, MB)

        val basic = factions.take(4)
        val repeat = 0.to(15).map(_ => basic)

        val all = factions.combinations(4).$
        val squirrels = (factions ++ extra).combinations(4).$.%(_.has(SF))
        val frogs = (factions ++ extra).combinations(4).$.%(_.has(XC))
        val newfrogs = factions.combinations(4).$.%(_.has(LDvD))
        val knaves = factions.combinations(4).$.%(_.has(KDvA))
        val batsos = factions.combinations(4).$.%(_.has(TCvA))

        var results : $[$[Faction]] = $
        val runs = 5
        val input = batsos.shuffle.take(12)

        +++(input.num, "inputs,", runs, "runs")

        0.until(runs).foreach { i =>
            results = results ++ input.zipWithIndex.par.map { case (ff, j) =>
                var actions : $[Action] = $

                try {
                    val seating = randomSeating(ff)
                    val game = new Game(seating, seating, $(
                        AutumnMap,
                        DefaultClearings,
                        SetupTypeCorners,
                        MixedDeck,
                        // DuskDeck,
                        AdSetBuffOn,
                        NoHirelings,
                        FerryLandmark,
                        LostCityLandmark,
                        SeatingGiven,
                        FactionSeatingGiven,
                        SetupOrderPriority,
                        CardDraftStandard,
                    ) ++ seating./(IncludeFaction))

                    var continue : Continue = StartContinue
                    var a : Action = StartAction(root.version)

                    while (a.is[GameOverAction].not) {
                        actions +:= a

                        a match {
                            case _ if "debug" != "serialize" =>
                            case a if a.isSoft =>
                            case a : ExternalAction =>
                                try {
                                    val sss = Serialize.write(a.unwrap)
                                    val ppp = Serialize.parseAction(sss)
                                    val aaa = Serialize.write(ppp)

                                    if (sss != aaa) {
                                        ---
                                        +++("unmatching write/parse")
                                        ---
                                        ---
                                        ---
                                        +++("soft:", a.isSoft)
                                        +++(a)
                                        +++(sss)
                                        +++(ppp)
                                        +++(aaa)
                                        ---
                                    }
                                }
                                catch {
                                    case e : Throwable =>
                                        ---
                                        +++("error serializing")
                                        ---
                                        ---
                                        ---
                                        +++("action")
                                        +++(a)
                                        +++("serialize")
                                        val sss = Serialize.write(a)
                                        +++(sss)
                                        +++("parse")
                                        val ppp = Serialize.parseAction(sss)
                                        +++(ppp)
                                        +++("re-serialize")
                                        val aaa = Serialize.write(ppp)
                                        +++(aaa)
                                        ---
                                }
                            case _ =>
                        }

                        if (actions.num > 10000)
                            throw new Error("game never ended")

                        continue = game.performContinue(|(continue), a, true).continue

                        a = askFaction(game, continue)
                    }

                    val w = a.asInstanceOf[GameOverAction].winners

                    +++(w.some./(_./(writeFaction).mkString(", ")).|("Humanity"), "won")

                    Files.write(Paths.get("game-logs/log-" + start + "-" + i + "-" + j + ".txt"), actions.reverse./(_.unwrap)./(Serialize.write).asJava)

                    w
                }
                catch {
                    case e : Throwable =>
                        +++(e)

                        Files.write(Paths.get("errors/game-error-" + System.currentTimeMillis + ".txt"),
                            (actions.reverse./(_.unwrap)./(Serialize.write) ++ 4.times("") ++ $(e.getMessage) ++ e.getStackTrace.map(_.toString)).asJava)

                        $
                }
            }

            val wins = results.groupBy(w => w).view.mapValues(_.size).toMap

            ---

            wins.keys.$.sortBy(k => wins(k)).reverse.foreach { k =>
                +++(k.any.?(k./(_.name).mkString(", ")).|("Humanity") + ":", wins(k), "%6.0f".format(wins(k) * 100.0 / wins.values.sum) + "%")
            }

            ---

            input./~(x => x).distinct.map { f =>
                val ww = wins.view.filterKeys(_.contains(f))
                val solo = ww.view.filterKeys(_.size == 1).values.sum
                val tie = ww.view.filterKeys(_.size > 1).values.sum
                (solo + tie) -> (writeFaction(f) + ":", solo, "+", tie, "%6.0f".format((solo + tie) * 100.0 / wins.values.sum) + "%")
            }.sortBy(_._1).map(_._2).reverse.foreach(+++)

            +++("Humanity" + ":", wins.view.filterKeys(_.size == 0).values.sum, "%6.0f".format(wins.view.filterKeys(_.size == 0).values.sum * 100.0 / wins.values.sum) + "%")
            +++("Total: " + results.num)

            ---
        }
    }

}
