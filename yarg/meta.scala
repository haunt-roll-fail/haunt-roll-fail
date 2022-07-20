package yarg

import colmat._

import hrf.meta._
import hrf.elem._

object Meta extends MetaGame { mmm =>
    val gaming = yarg.gaming
    
    type F = Faction
    type G = Game
    
    def tagF = implicitly

    val name = "yarg"
    val label = "Yar-Game"

    val factions = List(Pirates, FireKnights, Mages)

    val options = Nil
    
    val quickMin = 2
    val quickMax = 2

    def randomGameName() = {
        val n = List("Goverment", "Coup", "Elections", "Politics", "Mutiny").shuffle
        val c = List("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }
                                                               
    def validateFactionCombination(factions : List[Faction]) = (factions.num != 2).?(ErrorResult("Select two factions"))|(InfoResult(""))
    def validateFactionSeatingOptions(factions : List[Faction], options : List[O]) = InfoResult("---")
    
    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    def createGame(factions : List[Faction], options : List[O]) = new Game(factions, true)

    def getBots(f : Faction) = List("Normal")
    
    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Faction, "Normal") => new BotXX(f)
    }

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) : Option[Faction] = s match {
        case "P" => Some(Pirates)
        case s => factions.%(_.short == s).single
    }

    def writeOption(f : O) = "n/a"
    def parseOption(s : String) : Option[O] = None

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)
    
    val start = StartAction

    val assets =
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true, ext = "png")(
        ImageAsset("map", "map-green", "jpg") ::

        ImageAsset("fire-knight") ::
        
        ImageAsset("mage") ::

        ImageAsset("captain") ::
        ImageAsset("parrot") ::
        ImageAsset("pirate") ::
        ImageAsset("cabin-boy") ::

        ImageAsset("bar-health-start") ::
        ImageAsset("bar-health") ::
        ImageAsset("bar-health-end") ::

        ImageAsset("bar-mana-start") ::
        ImageAsset("bar-mana") ::
        ImageAsset("bar-mana-end") ::

        ImageAsset("bar-rage-start") ::
        ImageAsset("bar-rage") ::
        ImageAsset("bar-rage-end") ::

        ImageAsset("bar-empty-start") ::
        ImageAsset("bar-empty") ::
        ImageAsset("bar-empty-end") ::

    Nil) ::
    Nil
    
}
