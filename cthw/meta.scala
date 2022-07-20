package cthw

import cthw.gaming._

import colmat._

import hrf.meta._
import hrf.elem._
import hrf.reflect._

trait GameOption extends BaseGameOption

object Meta extends MetaGame {
    val gaming = cthw.gaming

    type F = Faction
    type G = Game
    type O = GameOption
    
    def tagF = implicitly

    val name = "cthw"
    val label = "Cthulhu Wars"

    val factions = $(GC, CC, BG, YS)

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
//    override def factionNote(f : Faction) : Elem = f.note

////    override def factionGlyph(f : F) = Some(f.style + "-glyph")
    
    def createGame(factions : List[Faction], options : List[O]) = new Game(factions, factions, true, options)
    
    def getBots(f : Faction) = f match {
        case _ => $("Normal", "Easy")
    }
    
    override def defaultBots = $("Normal")

    def getBot(f : Faction, b : String) = null /* (f, b) match {
        case (f : Faction, "Easy") => new BotXX(f)
        case (f : Faction, "Normal") => new BotXX(f)
    } */

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) = factions.%(_.short == s).single

    def writeOption(o : O) = o.toString
    def parseOption(s : String) = options.%(o => o.toString == s).single
    
    def parseAction(s : String) : Action = { println(s); Serialize.parseAction(s) }
    def writeAction(a : Action) : String = Serialize.write(a)
    
    val start = StartAction("")
    
    val assets = 
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true)(
        ImageAsset("map"          ,    "earth35-dark") ::
        ImageAsset("map-regions"  ,    "earth35-place") ::
    Nil) ::
    Nil
}
