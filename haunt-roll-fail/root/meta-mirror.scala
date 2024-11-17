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

import hrf.meta._
import hrf.elem._
import hrf.options._

import root.elem._


object MetaMirror extends MetaGame {
    val gaming = root.gaming

    type F = Faction

    def tagF = implicitly

    val name = "root-mirror"
    val label = "Root: Mirror"

    override def settingsList = Meta.settingsList
    override def settingsDefaults = Meta.settingsDefaults

    override val about = Meta.about

    override def settingsKey = Meta.settingsKey

    val factions = Meta.official.%(f => Meta.clones.exists(_.clashKey == f.clashKey)) ++ Meta.clones

    override def factionGroup(f : F) : |[Elem] = Meta.factionGroup(f)

    val minPlayers = 2

    val options = Meta.options

    override def optionsFor(n : Int, l : $[F]) = Meta.optionsFor(n, l)

    override val hiddenOptions = Meta.hiddenOptions

    override def defaultsFor(n : Int, l : $[F]) = Meta.defaultsFor(n, l).but(SetupOrderPriority).appended(SetupOrderReverse) ++ $(
        TheWoodMustFlow,
        OneTrueEmpire,
        Imprinting,
        RootingForTheUnderdog,
        DeathToTheInfidels,
        HostileTakeover,
        DoubleAgents,
        OldBoys,
        PeerPressure,
        SharedHistory
    )

    val quickMin = 4
    val quickMax = 4

    override val quickFactions = factions

    def randomGameName() = {
        val n = $("Power", "Victory", "Glory", "Destiny", "Might", "Fight", "Right", "Betrayal", "Wood", "Land", "Air", "Ground", "War", "Dominance", "Wind", "Struggle", "Upheaval", "Control", "Command", "Conquest", "Movement", "Revolution", "Season").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.head
    }

    def validateFactionCombination(factions : $[F]) =
        Meta.validateFactionCombination(factions) @@ {
            case r : ErrorResult => r
            case r =>
                if ($(
                    factions.of[Feline].num,
                    factions.of[Aviary].num,
                    factions.of[Insurgent].num,
                    factions.of[Hero].num,
                    factions.of[Fanatic].num,
                    factions.of[Trader].num,
                    factions.of[Underground].num,
                    factions.of[Mischief].num,
                    factions.of[Horde].num,
                    factions.of[Expedition].num
                ).exists(_ == 1))
                    ErrorResult("Unmatching faction")
                else
                    r @@[ValidationResult] {
                        case r : WarningResult => r
                        case _ => InfoResult("Root: Mirror")
                    }
        }

    def validateFactionSeatingOptions(factions : $[F], options : $[O]) = validateFactionCombination(factions) && Meta.validateFactionSeatingOptions(factions, options)

    def factionName(f : F) = Meta.factionName(f)
    def factionElem(f : F) = Meta.factionElem(f)
    override def factionNote(f : F) : Elem = Meta.factionNote(f)

    override def glyph(g : G) : |[String] = Meta.glyph(g)
    override def glyph(g : G, f : F) : |[String] = Meta.glyph(g, f)

    def createGame(factions : $[F], options : $[O]) = Meta.createGame(factions, options)

    def getBots(f : F) = Meta.getBots(f)

    override def defaultBots = Meta.defaultBots

    def getBot(f : F, b : String) = Meta.getBot(f, b)

    def writeFaction(f : F) = Meta.writeFaction(f)
    def parseFaction(s : String) = Meta.parseFaction(s)

    def writeOption(o : O) = o.toString
    def parseOption(s : String) = Meta.parseOption(s)

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    override val path = "root"

    override val intLinks = $(("Root: Standard Setup".spn -> "/play/root"), ("Root: Advanced Setup".spn -> "/play/root-adset"))

    override val extLinks = Meta.extLinks

    override def bodyFont = Meta.bodyFont

    val assets = Meta.assets./(m => ConditionalAssetsList((factions : $[F], options : $[O]) => m.condition(factions, options), m.path, m.prefix, m.ext)(m.list))

}
