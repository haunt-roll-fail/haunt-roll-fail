package suok

import colmat._

import hrf.meta._
import hrf.elem._

object Meta extends MetaGame { mmm =>
    val gaming = suok.gaming
    
    type F = Faction
    type G = Game
    
    def tagF = implicitly

    val name = "suok"
    val label = "Sultans of Karaya"

    val factions = List(Red, Pink, Orange, Yellow, White, Cyan, Green, Teal, Blue, Violet)
    
    val options = Nil
    
    override val gradualFactions : Boolean = true
    
    val quickMin = 5 + 2
    val quickMax = 10 - 3

    def randomGameName() = {
        val n = List("Goverment", "Coup", "Elections", "Politics", "Mutiny").shuffle
        val c = List("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }
                                                               
    def validateFactionCombination(factions : List[Faction]) = (factions.num < 5).?(ErrorResult("Select at least five factions")).|((factions.num > 10).?(ErrorResult("Max ten factions")).|(InfoResult("")))
    def validateFactionSeatingOptions(factions : List[Faction], options : List[O]) = InfoResult("---")
    
    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)
    
    def createGame(factions : List[Faction], options : List[O]) = new Game(factions, true)

    def getBots(f : Faction) = List("Normal")
    
    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Faction, "Normal") => new BotXX(f)
    }

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) : Option[Faction] = factions.%(_.short == s).single

    def writeOption(f : O) = "n/a"
    def parseOption(s : String) : Option[O] = None

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction

    val assets =
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true)(
        ImageAsset("assassin"      ) ::
        ImageAsset("chains"        ) ::
        ImageAsset("choice-blue"   ) ::
        ImageAsset("choice-red"    ) ::
        ImageAsset("crown"         ) ::
        ImageAsset("dancer"        ) ::
        ImageAsset("guard"         ) ::
        ImageAsset("hidden"        ) ::
        ImageAsset("hunter"        ) ::
        ImageAsset("oracle"        ) ::
        ImageAsset("outcome-blue"  ) ::
        ImageAsset("outcome-red"   ) ::
        ImageAsset("prison"        ) ::
        ImageAsset("slave"         ) ::
        ImageAsset("sleep"         ) ::
        ImageAsset("sultan"        ) ::
        ImageAsset("vizier"        ) ::
    Nil) :: Nil
}
