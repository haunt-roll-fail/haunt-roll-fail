package arcs
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
import hrf.options._
import hrf.elem._

case class UnknownOption(o : String) extends GameOption {
    val group = "Unknown"
    val valueOn = "Unknown Option " ~ o
}

case class BadInitiativeTransferChapter(n : Int) extends GameOption with ToggleOption {
    val group = "Incorrect Initiative Transfer".hh
    val valueOn = "Chapter " ~ n.hlb
}

case object LeadersAndLorePreset1 extends GameOption with ToggleOption {
    val group = "Leaders and Lore".hh
    val valueOn = "Preset".hh ~ " " ~ "#1".hlb
}

object Meta extends MetaGame { mmm =>
    val gaming = arcs.gaming

    type F = Faction

    def tagF = implicitly

    val name = "arcs"
    val label = "Arcs"

    val factions = $(Red, Yellow, White, Blue)

    val minPlayers = 3

    override val hiddenOptions =
        1.to(5)./(BadInitiativeTransferChapter)

    val options = $(LeadersAndLorePreset1) ++ hiddenOptions

    override val gradualFactions : Boolean = true

    val quickMin = 4
    val quickMax = 4

    def randomGameName() = {
        val n = $("Space", "Politics", "Betrayal", "Explosion", "Conquest", "Warp", "Renegade", "Sway", "Diplomacy", "Conflict").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num < 3).?(ErrorResult("Minimum three factions")) ||
        (factions.num < 2).?(ErrorResult("Minimum two factions")) ||
        (factions.num > 4).?(ErrorResult("Max four factions")) |
        InfoResult("Arcs")

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = validateFactionCombination(factions)

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    override def glyph(g : G) : |[String] = g.current.?./(_.style + "-glyph")
    override def glyph(f : F) : |[String] = |(f.style + "-glyph")
    override def glyph(g : G, f : F) : |[String] = glyph(f).%!(_ => g.highlightFaction.has(f) && hrf.HRF.uptime() / 1000 % 2 == 1)

    def createGame(factions : $[Faction], options : $[O]) = new Game(factions, options)

    def getBots(f : Faction) = $("Easy")

    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Faction, "Easy") => new BotXX(f)
    }

    def defaultBots : $[String] = $("Easy")

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) : Option[Faction] = factions.%(_.short == s).single

    def writeOption(o : O) = Serialize.write(o)
    def parseOption(s : String) = $(options.find(o => writeOption(o) == s) || options.find(o => o.toString == s) | (UnknownOption(s)))

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    override def bodyFont = Some("neue-kabel")
    override def titleFont = Some("fm-bolyar-pro-900")

    val assets =
    ConditionalAssetsList((factions : $[F], options : $[O]) => true)(
        ImageAsset("map") ::
        ImageAsset("map-regions").makeLossless ::
        ImageAsset("map-regions-select").makeLossless ::

        // ImageAsset("map-out-1") ::
        ImageAsset("map-out-2") ::
        ImageAsset("map-out-3") ::
        // ImageAsset("map-out-4") ::
        // ImageAsset("map-out-5") ::
        // ImageAsset("map-out-6") ::

        ImageAsset("map-ambitions-3") ::

        ImageAsset("ambitions").scaled(50) ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "icon")(
        ImageAsset("material").scaled(40) ::
        ImageAsset("fuel").scaled(40) ::
        ImageAsset("weapon").scaled(40) ::
        ImageAsset("relic").scaled(40) ::
        ImageAsset("psionic").scaled(40) ::

        ImageAsset("nothingness").scaled(40) ::
        ImageAsset("discard-resource").scaled(40) ::

        ImageAsset("material-outrage").scaled(40) ::
        ImageAsset("fuel-outrage").scaled(40) ::
        ImageAsset("weapon-outrage").scaled(40) ::
        ImageAsset("relic-outrage").scaled(40) ::
        ImageAsset("psionic-outrage").scaled(40) ::

        ImageAsset("keys-1").scaled(12.5) ::
        ImageAsset("keys-2").scaled(12.5) ::
        ImageAsset("keys-3").scaled(12.5) ::
        ImageAsset("half-keys-1").scaled(12.5) ::
        ImageAsset("half-keys-2").scaled(12.5) ::
        ImageAsset("half-keys-3").scaled(12.5) ::
        ImageAsset("card-back-small") ::
        ImageAsset("card-back-5") ::
        ImageAsset("b-glyph") ::
        ImageAsset("r-glyph") ::
        ImageAsset("w-glyph") ::
        ImageAsset("y-glyph") ::

        ImageAsset("raid-key") ::

        ImageAsset("agent-background") ::

        ImageAsset("assault-die-1") ::
        ImageAsset("assault-die-2") ::
        ImageAsset("assault-die-3") ::
        ImageAsset("assault-die-4") ::
        ImageAsset("assault-die-5") ::
        ImageAsset("assault-die-6") ::
        ImageAsset("assault-die") ::
        ImageAsset("raid-die-1") ::
        ImageAsset("raid-die-2") ::
        ImageAsset("raid-die-3") ::
        ImageAsset("raid-die-4") ::
        ImageAsset("raid-die-5") ::
        ImageAsset("raid-die-6") ::
        ImageAsset("raid-die") ::
        ImageAsset("skirmish-die-1") ::
        ImageAsset("skirmish-die-2") ::
        ImageAsset("skirmish-die-3") ::
        ImageAsset("skirmish-die-4") ::
        ImageAsset("skirmish-die-5") ::
        ImageAsset("skirmish-die-6") ::
        ImageAsset("skirmish-die") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "action")(
        ImageAsset("card-back") ::

        ImageAsset("administration-1") ::
        ImageAsset("administration-2") ::
        ImageAsset("administration-3") ::
        ImageAsset("administration-4") ::
        ImageAsset("administration-5") ::
        ImageAsset("administration-6") ::
        ImageAsset("administration-7") ::
        ImageAsset("aggression-1") ::
        ImageAsset("aggression-2") ::
        ImageAsset("aggression-3") ::
        ImageAsset("aggression-4") ::
        ImageAsset("aggression-5") ::
        ImageAsset("aggression-6") ::
        ImageAsset("aggression-7") ::
        ImageAsset("construction-1") ::
        ImageAsset("construction-2") ::
        ImageAsset("construction-3") ::
        ImageAsset("construction-4") ::
        ImageAsset("construction-5") ::
        ImageAsset("construction-6") ::
        ImageAsset("construction-7") ::
        ImageAsset("mobilization-1") ::
        ImageAsset("mobilization-2") ::
        ImageAsset("mobilization-3") ::
        ImageAsset("mobilization-4") ::
        ImageAsset("mobilization-5") ::
        ImageAsset("mobilization-6") ::
        ImageAsset("mobilization-7") ::

        ImageAsset("administration-number-1") ::
        ImageAsset("administration-number-2") ::
        ImageAsset("administration-number-3") ::
        ImageAsset("administration-number-4") ::
        ImageAsset("administration-number-5") ::
        ImageAsset("administration-number-6") ::
        ImageAsset("administration-number-7") ::
        ImageAsset("administration-pips-1") ::
        ImageAsset("administration-pips-2") ::
        ImageAsset("administration-pips-3") ::
        ImageAsset("administration-pips-4") ::
        ImageAsset("administration-pips-copy") ::
        ImageAsset("administration-pips-pivot") ::
        ImageAsset("administration-plaque") ::

        ImageAsset("aggression-number-1") ::
        ImageAsset("aggression-number-2") ::
        ImageAsset("aggression-number-3") ::
        ImageAsset("aggression-number-4") ::
        ImageAsset("aggression-number-5") ::
        ImageAsset("aggression-number-6") ::
        ImageAsset("aggression-number-7") ::
        ImageAsset("aggression-pips-1") ::
        ImageAsset("aggression-pips-2") ::
        ImageAsset("aggression-pips-3") ::
        ImageAsset("aggression-pips-copy") ::
        ImageAsset("aggression-pips-pivot") ::
        ImageAsset("aggression-plaque") ::

        ImageAsset("construction-number-1") ::
        ImageAsset("construction-number-2") ::
        ImageAsset("construction-number-3") ::
        ImageAsset("construction-number-4") ::
        ImageAsset("construction-number-5") ::
        ImageAsset("construction-number-6") ::
        ImageAsset("construction-number-7") ::
        ImageAsset("construction-pips-1") ::
        ImageAsset("construction-pips-2") ::
        ImageAsset("construction-pips-3") ::
        ImageAsset("construction-pips-4") ::
        ImageAsset("construction-pips-copy") ::
        ImageAsset("construction-pips-pivot") ::
        ImageAsset("construction-plaque") ::

        ImageAsset("mobilization-number-1") ::
        ImageAsset("mobilization-number-2") ::
        ImageAsset("mobilization-number-3") ::
        ImageAsset("mobilization-number-4") ::
        ImageAsset("mobilization-number-5") ::
        ImageAsset("mobilization-number-6") ::
        ImageAsset("mobilization-number-7") ::
        ImageAsset("mobilization-pips-1") ::
        ImageAsset("mobilization-pips-2") ::
        ImageAsset("mobilization-pips-3") ::
        ImageAsset("mobilization-pips-4") ::
        ImageAsset("mobilization-pips-copy") ::
        ImageAsset("mobilization-pips-pivot") ::
        ImageAsset("mobilization-plaque") ::

        ImageAsset("zeroed") ::
        ImageAsset("hidden") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "court", ext = "webp")(
        ImageAsset("bc01") ::
        ImageAsset("bc02") ::
        ImageAsset("bc03") ::
        ImageAsset("bc04") ::
        ImageAsset("bc05") ::
        ImageAsset("bc06") ::
        ImageAsset("bc07") ::
        ImageAsset("bc08") ::
        ImageAsset("bc09") ::
        ImageAsset("bc10") ::
        ImageAsset("bc11") ::
        ImageAsset("bc12") ::
        ImageAsset("bc13") ::
        ImageAsset("bc14") ::
        ImageAsset("bc15") ::
        ImageAsset("bc16") ::
        ImageAsset("bc17") ::
        ImageAsset("bc18") ::
        ImageAsset("bc19") ::
        ImageAsset("bc20") ::
        ImageAsset("bc21") ::
        ImageAsset("bc22") ::
        ImageAsset("bc23") ::
        ImageAsset("bc24") ::
        ImageAsset("bc25") ::
        ImageAsset("bc26") ::
        ImageAsset("bc27") ::
        ImageAsset("bc28") ::
        ImageAsset("bc29") ::
        ImageAsset("bc30") ::
        ImageAsset("bc31") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "leader", ext = "webp")(
        ImageAsset("leader01") ::
        ImageAsset("leader02") ::
        ImageAsset("leader03") ::
        ImageAsset("leader04") ::
        ImageAsset("leader05") ::
        ImageAsset("leader06") ::
        ImageAsset("leader07") ::
        ImageAsset("leader08") ::
        ImageAsset("leader09") ::
        ImageAsset("leader10") ::
        ImageAsset("leader11") ::
        ImageAsset("leader12") ::
        ImageAsset("leader13") ::
        ImageAsset("leader14") ::
        ImageAsset("leader15") ::
        ImageAsset("leader16") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "lore", ext = "webp")(
        ImageAsset("lore01") ::
        ImageAsset("lore02") ::
        ImageAsset("lore03") ::
        ImageAsset("lore04") ::
        ImageAsset("lore05") ::
        ImageAsset("lore06") ::
        ImageAsset("lore07") ::
        ImageAsset("lore08") ::
        ImageAsset("lore09") ::
        ImageAsset("lore10") ::
        ImageAsset("lore11") ::
        ImageAsset("lore12") ::
        ImageAsset("lore13") ::
        ImageAsset("lore14") ::
        ImageAsset("lore15") ::
        ImageAsset("lore16") ::
        ImageAsset("lore17") ::
        ImageAsset("lore18") ::
        ImageAsset("lore19") ::
        ImageAsset("lore20") ::
        ImageAsset("lore21") ::
        ImageAsset("lore22") ::
        ImageAsset("lore23") ::
        ImageAsset("lore24") ::
        ImageAsset("lore25") ::
        ImageAsset("lore26") ::
        ImageAsset("lore27") ::
        ImageAsset("lore28") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "ambition", scale = 41.4)(
        ImageAsset("ambition-values-6-3") ::
        ImageAsset("ambition-values-9-4") ::
        ImageAsset("ambition-values-3-2") ::
        ImageAsset("ambition-values-4-2") ::
        ImageAsset("ambition-values-5-3") ::
        ImageAsset("ambition-values-2-0") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "figure", scale = 11)(
        ImageAsset("ship-empty") ::
        ImageAsset("b-ship") ::
        ImageAsset("b-ship-damaged") ::
        ImageAsset("r-ship") ::
        ImageAsset("r-ship-damaged") ::
        ImageAsset("w-ship") ::
        ImageAsset("w-ship-damaged") ::
        ImageAsset("y-ship") ::
        ImageAsset("y-ship-damaged") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "figure", scale = 11)(
        ImageAsset("agent-empty") ::
        ImageAsset("b-agent") ::
        ImageAsset("b-agent-damaged") ::
        ImageAsset("r-agent") ::
        ImageAsset("r-agent-damaged") ::
        ImageAsset("w-agent") ::
        ImageAsset("w-agent-damaged") ::
        ImageAsset("y-agent") ::
        ImageAsset("y-agent-damaged") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "figure", scale = 38)(
        ImageAsset("building-empty-keys-1") ::
        ImageAsset("building-empty-keys-2") ::
        ImageAsset("building-empty-keys-1-3") ::
        ImageAsset("building-empty-plus-2") ::
        ImageAsset("building-empty-plus-3") ::

        ImageAsset("city-empty",     "building-empty") ::
        ImageAsset("starport-empty", "building-empty") ::
        ImageAsset("b-city") ::
        ImageAsset("b-city-damaged") ::
        ImageAsset("b-starport") ::
        ImageAsset("b-starport-damaged") ::
        ImageAsset("r-city") ::
        ImageAsset("r-city-damaged") ::
        ImageAsset("r-starport") ::
        ImageAsset("r-starport-damaged") ::
        ImageAsset("w-city") ::
        ImageAsset("w-city-damaged") ::
        ImageAsset("w-starport") ::
        ImageAsset("w-starport-damaged") ::
        ImageAsset("y-city") ::
        ImageAsset("y-city-damaged") ::
        ImageAsset("y-starport") ::
        ImageAsset("y-starport-damaged") ::
    $) ::
    $

    override def extLinks = $(
    ) ++ super.extLinks

}
