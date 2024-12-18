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

import hrf.meta._
import hrf.elem._

object Meta extends MetaGame { mmm =>
    val gaming = coup.gaming

    type F = Faction


    def tagF = implicitly

    val name = "coup"
    val label = "Coup"

    val factions = List(Amalthea, Thebe, Io, Europa, Ganymede, Callisto)

    val minPlayers = 2

    val options = Nil

    override val gradualFactions : Boolean = true

    val quickMin = 6
    val quickMax = 6

    def randomGameName() = {
        val n = $("Diplomacy", "Betrayal", "Intrigue", "Bluff", "Bribery", "Lies", "Secrecy", "Espionage", "Conspiracy", "Denial").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = (factions.num < 2).?(ErrorResult("Select at least two factions")).|((factions.num > 6).?(ErrorResult("Max six factions")).|(InfoResult("")))
    def validateFactionSeatingOptions(factions : $[Faction], options : List[O]) = InfoResult("")

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    def createGame(factions : $[Faction], options : $[O]) = new Game(factions)

    def getBots(f : Faction) = $("Normal")

    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Faction, "Normal") => new BotXX(f)
    }

    def defaultBots : $[String] = $("Normal")

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) : |[Faction] = factions.%(_.short == s).single

    def writeOption(f : O) = "n/a"
    def parseOption(s : String) = $

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    override def bodyFont = Some("ethnocentric")

    val assets =
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true)(
        ImageAsset("ambassador" ) ::
        ImageAsset("assassin" ) ::
        ImageAsset("captain" ) ::
        ImageAsset("contessa" ) ::
        ImageAsset("duke" ) ::

        ImageAsset("ambassador2" ) ::
        ImageAsset("assassin2" ) ::
        ImageAsset("captain2" ) ::
        ImageAsset("contessa2" ) ::
        ImageAsset("duke2" ) ::

        ImageAsset("ambassador3" ) ::
        ImageAsset("assassin3" ) ::
        ImageAsset("captain3" ) ::
        ImageAsset("contessa3" ) ::
        ImageAsset("duke3" ) ::

        ImageAsset("ambassador4" ) ::
        ImageAsset("assassin4" ) ::
        ImageAsset("captain4" ) ::
        ImageAsset("contessa4" ) ::
        ImageAsset("duke4" ) ::

        ImageAsset("ambassador5" ) ::
        ImageAsset("assassin5" ) ::
        ImageAsset("captain5" ) ::
        ImageAsset("contessa5" ) ::
        ImageAsset("duke5" ) ::

        ImageAsset("ambassador6" ) ::
        ImageAsset("assassin6" ) ::
        ImageAsset("captain6" ) ::
        ImageAsset("contessa6" ) ::
        ImageAsset("duke6" ) ::

        ImageAsset("hidden" ) ::
        ImageAsset("token" ) ::
        ImageAsset("aid" ) ::
    Nil) :: Nil
}
