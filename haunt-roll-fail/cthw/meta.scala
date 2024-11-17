package cthw
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

import cthw.gaming._

import hrf.meta._
import hrf.elem._
import hrf.reflect._

object Meta extends MetaGame {
    val gaming = cthw.gaming

    type F = Faction

    def tagF = implicitly

    val name = "cthw"
    val label = "Cthulhu Wars"

    val factions = $(GC, CC, BG, YS)

    val minPlayers = 3

    val options = $[O]()

    val quickMin = 4
    val quickMax = 4

    def randomGameName() = {
        val n = List("Power", "Victory", "Glory", "Destiny", "Might", "Fight", "Right", "Betrayal", "Wood", "Land", "Air", "Ground").shuffle
        val c = List("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : List[Faction]) = {
        if (factions.num < 2)
            ErrorResult("Select at least two factions")
        else
        if (factions.num > 5)
            ErrorResult("Max five factions")
        else
            InfoResult("~~~")
    }

    def validateFactionSeatingOptions(factions : List[Faction], options : List[O]) = InfoResult("~~~")

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    def createGame(factions : List[Faction], options : List[O]) = new Game(factions, factions, options)

    def getBots(f : Faction) = f match {
        case _ => $("Normal", "Easy")
    }

    def getBot(f : Faction, b : String) = null

    def defaultBots : $[String] = $("Normal")

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) = factions.%(_.short == s).single

    def writeOption(o : O) = o.toString
    def parseOption(s : String) = options.%(o => o.toString == s)

    def parseAction(s : String) : Action = { println(s); Serialize.parseAction(s) }
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    override def bodyFont = Some("bohemian-typewriter")

    val assets =
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true)(
        ImageAsset("map"          ,    "earth35-dark") ::
        ImageAsset("map-regions"  ,    "earth35-place").makeLossless ::
    Nil) ::
    Nil
}
