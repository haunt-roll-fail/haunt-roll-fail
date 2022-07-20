package sehi

import colmat._

import hrf.meta._
import hrf.elem._

object Meta extends MetaGame { mmm =>
    val gaming = sehi.gaming
    
    type F = Faction
    type G = Game
    
    def tagF = implicitly

    val name = "sehi"
    val label = "Secret Hitler"

    val factions = List(Red, Green, Blue, Yellow, White, Purple, Azure, Lime, Orange, Electric)

    val options = Nil
    
    override val gradualFactions : Boolean = true

    val quickMin = 5
    val quickMax = 10

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
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.num.between(1, 6))(
        ImageAsset("board", "board-5-6", "png" ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.num.between(7, 8))(
        ImageAsset("board", "board-7-8", "png" ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.num.between(9, 10))(
        ImageAsset("board", "board-9-10", "png" ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true)(
        ImageAsset("marker" ) ::

        ImageAsset("liberal-article-jury-trial"         ) ::
        ImageAsset("liberal-article-right-to-bear-arms" ) ::
        ImageAsset("liberal-article-habeas-corpus"      ) ::
        ImageAsset("liberal-article-right-to-work"      ) ::
        ImageAsset("liberal-article-school-choice"      ) ::
        ImageAsset("liberal-article-stand-your-ground"  ) ::

        ImageAsset("fascist-article"                    ) ::
        
        ImageAsset("party-liberal" ) ::
        ImageAsset("party-fascist" ) ::
        ImageAsset("liberal-1" ) ::
        ImageAsset("liberal-2" ) ::
        ImageAsset("liberal-3" ) ::
        ImageAsset("liberal-4" ) ::
        ImageAsset("liberal-5" ) ::
        ImageAsset("liberal-6" ) ::
        ImageAsset("fascist-a" ) ::
        ImageAsset("fascist-b" ) ::
        ImageAsset("fascist-c" ) ::
        ImageAsset("hitler" ) ::
    Nil)
    
}
