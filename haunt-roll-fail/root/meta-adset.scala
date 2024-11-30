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


case class IncludeFaction(faction : Faction) extends GameOption with ToggleOption {
    val group = "Available Factions"
    val valueOn = faction.elem
}

object MetaAdset extends MetaGame {
    val gaming = root.gaming

    type F = PlayerN

    def tagF = implicitly

    val name = "root-adset"
    val label = "Root: Advanced Setup"

    override def settingsList = Meta.settingsList
    override def settingsDefaults = Meta.settingsDefaults

    override val about = Meta.about

    override def settingsKey = Meta.settingsKey

    val factions = $(PlayerN(1), PlayerN(2), PlayerN(3), PlayerN(4), PlayerN(5), PlayerN(6), PlayerN(7), PlayerN(8))

    val minPlayers = 2

    val realFactions = Meta.official ++ $(NB) ++ $(XC) ++ $(CUv2) ++ $(FH) ++ $(LDvD) ++ $(TCvA) ++ $(KDvA)

    val options = Meta.options

    override def optionsFor(n : Int, l : $[F]) = Meta.optionsFor(n, realFactions)

    override val hiddenOptions = Meta.hiddenOptions
        .diff($(SeatingGiven, SeatingRandom))
        .diff(realFactions./(IncludeFaction))
        .appended(RandomCharacter)

    override def optionPages(n : Int, l : $[F]) : $[$[GameOption]] = Meta.optionPages(n, realFactions)

    override def defaultsFor(n : Int, l : $[F]) = $(
        SeatingRandom,
        FactionSeatingRandom,
        AutumnMap,
        DefaultClearings, NoClustersClearings,
        MixedDeck,
        AdSetBuffOn,
        SetupTypeHomelands,
        SetupOrderReverse,
        CardDraftFive,
        FolkHeroCharacter,
        RandomCharacter,
        MapDefaultLandmarks,
        NoHirelings
    ) ++ realFactions./(IncludeFaction) ++ Meta.hirelings./(IncludeHireling)

    override def presetsFor(n : Int, l : $[F]) = $(
        ("Reset Options".spn, $, $),
        ("Official".hl ~ " Rules", $(
            AutoHitsAssignmentMode,
            AllRandomClearings,
            StandardDeck,
            NonBirdPartisans,
            UnthematicCoffinMakers,
            UnthematicPropagandaBureau,
            TunnelsIgnoreRaid,
            TunnelsIgnoreTradePosts,
        ), $),
        ("Official".hl ~ " | " ~ "Hirelings".styled(BanditGangs), $(
            AutoHitsAssignmentMode,
            AllRandomClearings,
            StandardDeck,
            NonBirdPartisans,
            UnthematicCoffinMakers,
            UnthematicPropagandaBureau,
            TunnelsIgnoreRaid,
            TunnelsIgnoreTradePosts,
        ) :+ (n @@ {
            case 2 => ThreeNormalHirelings
            case 3 => OneDemotedTwoNormalHirelings
            case 4 => TwoDemotedOneNormalHirelings
            case _ => ThreeDemotedHirelings
        }), $),
        ("Sunday".styled(Rabbit) ~ " Games", $(
            AutoHitsAssignmentMode,
            WarriorsOnlyInfamy, RevisedQuests,
            TinkerNoBag, HarrierNoCoins, ScoundrelTeapotNoBoot, ArbiterScoreCard, RoninSwiftTorch,
            LizardHandSizeSix,
            ExportToCommitments,
            ThreeOfEachPlot, DiversionPlot,
            MayorAssemblyAction,
            InstantRebuildKeep,
            TotalWarDominance,
            ThreeRandomHirelings,
            IncludeHireling(StreetBand),
            IncludeHireling(RatSmugglers),
            IncludeHireling(ForestPatrol),
            IncludeHireling(SpringUprising)
        ), $(NB)./(IncludeFaction) ++ Meta.hirelings./(IncludeHireling)),
        ("Official".hl ~ " | " ~ "Riverfolk".styled(RF) ~ " | " ~ "Exiles & Partisans".hh, $(
            AutoHitsAssignmentMode,
            AllRandomClearings,
            ExilesDeck,
            NonBirdPartisans,
            UnthematicCoffinMakers,
            UnthematicPropagandaBureau,
            TunnelsIgnoreRaid,
            TunnelsIgnoreTradePosts,
        ), realFactions.drop(6)./(IncludeFaction)),
        ("Official".hl ~ " | " ~ "Riverfolk".styled(RF) ~ " + " ~ "Underworld".styled(UD) ~ " | " ~ "Exiles & Partisans".hh, $(
            AutoHitsAssignmentMode,
            AllRandomClearings,
            ExilesDeck,
            NonBirdPartisans,
            UnthematicCoffinMakers,
            UnthematicPropagandaBureau,
            TunnelsIgnoreRaid,
            TunnelsIgnoreTradePosts,
        ), realFactions.drop(8)./(IncludeFaction)),
        ("Official".hl ~ " | " ~ "Riverfolk".styled(RF) ~ " + " ~ "Underworld".styled(UD) ~ " + " ~ "Marauder".styled(LH) ~ " | " ~ "Exiles & Partisans".hh, $(
            AutoHitsAssignmentMode,
            AllRandomClearings,
            ExilesDeck,
            NonBirdPartisans,
            UnthematicCoffinMakers,
            UnthematicPropagandaBureau,
            TunnelsIgnoreRaid,
            TunnelsIgnoreTradePosts,
        ), realFactions.drop(10)./(IncludeFaction)),
    )

    override val indistinguishableFactions : Boolean = true
    override val gradualFactions : Boolean = true

    val quickMin = 4
    val quickMax = 4

    override val quickFactions = factions.take(4)

    def randomGameName() = {
        val n = $("Power", "Victory", "Glory", "Destiny", "Might", "Fight", "Right", "Betrayal", "Wood", "Land", "Air", "Ground", "War", "Dominance", "Wind", "Struggle", "Upheaval", "Control", "Command", "Conquest", "Movement", "Revolution", "Season").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[F]) = InfoResult("")

    def validateFactionSeatingOptions(factions : $[F], options : $[O]) =
        if (realFactions./(IncludeFaction).intersect(options).num < factions.num)
            ErrorResult("Not enough factions selected")
        else
        if (realFactions./(IncludeFaction).intersect(options).num < factions.num + 1)
            WarningResult("Not enough factions selected")
        else
            InfoResult("")

    def factionName(f : F) = f.name
    def factionElem(f : F) = f.name.hh

    override def glyph(g : G) : Option[String] = Meta.glyph(g)
    override def glyph(g : G, f : F) : Option[String] = Some(f)./~(g.ptf.get)./~(f => Some(f.style + "-glyph").%!(_ => g.highlightFaction.has(f) && hrf.HRF.uptime() / 1000 % 2 == 1))

    def createGame(factions : $[F], options : $[O]) = new Game(factions, options.of[IncludeFaction]./(_.faction), options)

    def getBots(f : F) = $("None")

    override def defaultBots = $("None")

    def getBot(f : F, b : String) = new BotAdset(f)

    def writeFaction(f : F) = f.short
    def parseFaction(s : String) = factions.%(_.short == s).single

    def writeOption(o : O) = o.toString
    def parseOption(s : String) = Meta.parseOption(s)

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    override val path = "root"

    override val intLinks = $(("Root: Standard Setup".spn -> "root"), ("Root: Mirror".spn -> "root-mirror"))

    override val extLinks = Meta.extLinks

    override def bodyFont = Meta.bodyFont

    val assets = Meta.assets./(m => ConditionalAssetsList((factions : $[F], options : $[O]) => m.condition(Meta.factions, options), m.path, m.prefix, m.ext)(m.list))

}
