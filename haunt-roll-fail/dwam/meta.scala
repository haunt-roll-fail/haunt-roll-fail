package dwam
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
    val gaming = dwam.gaming

    type F = Faction


    def tagF = implicitly

    val name = "dwam"
    val label = "Discworld: Ankh-Morpork"

    val factions = $(Red, Yellow, Green, Blue, White)

    val minPlayers = 2

    override val quickFactions = factions.take(4)

    val options = Nil

    override val gradualFactions : Boolean = true

    val quickMin = 4
    val quickMax = 4

    def randomGameName() = {
        val n = $("Magic", "Octarine", "Death", "Fate", "Music", "Power").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num < 2).?(ErrorResult("Select at least two factions")) ||
        (factions.num > 5).?(ErrorResult("Max five factions")) ||
        (factions.has(White)).?(WarningResult("Discworld")) |
        InfoResult("Discworld")

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = InfoResult("Discworld")


    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    override def glyph(g : G) : |[String] = g.current.?./(_.style + "-glyph")
    override def glyph(f : F) : |[String] = |(f.style + "-glyph")
    override def glyph(g : G, f : F) : |[String] = glyph(f).%!(_ => g.highlightFaction.has(f) && hrf.HRF.uptime() / 1000 % 2 == 1)

    def createGame(factions : $[Faction], options : $[O]) = new Game(StandardBoard, factions, options)

    def getBots(f : Faction) = $("Normal")

    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Faction, "Normal") => new BotXX(f)
    }

    def defaultBots : $[String] = $("Normal")

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) : Option[Faction] = factions.%(_.short == s).single

    def writeOption(f : O) = "n/a"
    def parseOption(s : String) = $

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)


    override def bodyFont = Some("tico")

    val assets =
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true)(
        ImageAsset("map") ::
        ImageAsset("map-regions").makeLossless ::
        ImageAsset("map-regions-select").makeLossless ::

        ImageAsset("highlight-the-shades") ::
        ImageAsset("highlight-the-hippo") ::
        ImageAsset("highlight-the-scours") ::
        ImageAsset("highlight-dimwell") ::
        ImageAsset("highlight-longwall") ::
        ImageAsset("highlight-isle-of-gods") ::
        ImageAsset("highlight-seven-sleepers") ::
        ImageAsset("highlight-small-gods") ::
        ImageAsset("highlight-dragons-landing") ::
        ImageAsset("highlight-nap-hill") ::
        ImageAsset("highlight-unreal-estate") ::
        ImageAsset("highlight-dolly-sisters") ::

        ImageAsset("b-building" ) ::
        ImageAsset("g-building" ) ::
        ImageAsset("r-building" ) ::
        ImageAsset("y-building" ) ::
        ImageAsset("w-building" ) ::

        ImageAsset("b-glyph" ) ::
        ImageAsset("g-glyph" ) ::
        ImageAsset("r-glyph" ) ::
        ImageAsset("y-glyph" ) ::
        ImageAsset("w-glyph" ) ::

        ImageAsset("b-minion" ) ::
        ImageAsset("g-minion" ) ::
        ImageAsset("r-minion" ) ::
        ImageAsset("y-minion" ) ::
        ImageAsset("w-minion" ) ::

        ImageAsset("trouble" ) ::
        ImageAsset("demon" ) ::
        ImageAsset("troll" ) ::

        ImageAsset("deck" ) ::
        ImageAsset("card-back" ) ::
        ImageAsset("card-back-large" ) ::
        ImageAsset("pile" ) ::
        ImageAsset("pile-empty" ) ::

        ImageAsset("assassinate" ) ::
        ImageAsset("remove-trouble" ) ::
        ImageAsset("minion-highlight" ) ::
        ImageAsset("fire" ) ::
    Nil) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "cards", "character:")(
        ImageAsset("chrysoprase") ::
        ImageAsset("commander-vimes") ::
        ImageAsset("dragon-king-of-arms") ::
        ImageAsset("lord-vetinari") ::
        ImageAsset("lord-rust") ::
        ImageAsset("lord-selachii") ::
        ImageAsset("lord-de-worde") ::
    Nil) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "cards", "character:")(
        ImageAsset("chrysoprase") ::
        ImageAsset("commander-vimes") ::
        ImageAsset("dragon-king-of-arms") ::
        ImageAsset("lord-vetinari") ::
        ImageAsset("lord-rust") ::
        ImageAsset("lord-selachii") ::
        ImageAsset("lord-de-worde") ::
    Nil) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "cards", "area:")(
        ImageAsset("the-hippo") ::
        ImageAsset("the-scours") ::
        ImageAsset("small-gods") ::
        ImageAsset("dragons-landing") ::
        ImageAsset("unreal-estate") ::
        ImageAsset("dolly-sisters") ::
        ImageAsset("nap-hill") ::
        ImageAsset("seven-sleepers") ::
        ImageAsset("isle-of-gods") ::
        ImageAsset("longwall") ::
        ImageAsset("dimwell") ::
        ImageAsset("the-shades") ::
    Nil) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "icon", "icon:")(
        ImageAsset("another-card") ::
        ImageAsset("assassinate") ::
        ImageAsset("build") ::
        ImageAsset("event") ::
        ImageAsset("interrupt") ::
        ImageAsset("minion") ::
        ImageAsset("money") ::
        ImageAsset("remove-trouble") ::
        ImageAsset("scroll") ::
    $) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "cards", "event:")(
        ImageAsset("flood") ::
        ImageAsset("explosion") ::
        ImageAsset("earthquake") ::
        ImageAsset("the-dragon") ::
        ImageAsset("demons-from-the-dungeon-dimensions") ::
        ImageAsset("bloody-stupid-johnson") ::
        ImageAsset("trolls") ::
        ImageAsset("subsidence") ::
        ImageAsset("riots") ::
        ImageAsset("mysterious-murders") ::
        ImageAsset("fog") ::
        ImageAsset("fire") ::
    Nil) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "cards", "card:")(
        ImageAsset("no-card") ::

        ImageAsset("adora-belle-dearheart") ::
        ImageAsset("archchancellor-ridcully") ::
        ImageAsset("buggy-swires") ::
        ImageAsset("burleigh-stronginthearm") ::
        ImageAsset("cable-street-particulars") ::
        ImageAsset("canting-crew") ::
        ImageAsset("captain-carrot") ::
        ImageAsset("carcer") ::
        ImageAsset("cmot-dibbler") ::
        ImageAsset("cosmo-lavish") ::
        ImageAsset("death") ::
        ImageAsset("deep-dwarves") ::
        ImageAsset("doctor-hix") ::
        ImageAsset("doctor-mossy-lawn") ::
        ImageAsset("dorfl") ::
        ImageAsset("dr-cruces") ::
        ImageAsset("dr-whiteface") ::
        ImageAsset("drumknott") ::
        ImageAsset("dwarves") ::
        ImageAsset("edward-death") ::
        ImageAsset("errol") ::
        ImageAsset("foul-ole-ron") ::
        ImageAsset("fresh-start-club") ::
        ImageAsset("gargoyles") ::
        ImageAsset("gaspode") ::
        ImageAsset("gimlets-dwarf-delicatessen") ::
        ImageAsset("groat") ::
        ImageAsset("hargas-house-of-ribs") ::
        ImageAsset("harry-king") ::
        ImageAsset("here-n-now") ::
        ImageAsset("hex") ::
        ImageAsset("history-monks") ::
        ImageAsset("hobsonss-livery-stable") ::
        ImageAsset("hubert") ::
        ImageAsset("igor") ::
        ImageAsset("inigo-skimmer") ::
        ImageAsset("leonard-of-quirm") ::
        ImageAsset("librarian") ::
        ImageAsset("lord-downey") ::
        ImageAsset("modo") ::
        ImageAsset("moist-von-lipwig") ::
        ImageAsset("mr-bent") ::
        ImageAsset("mr-boggis") ::
        ImageAsset("mr-gryle") ::
        ImageAsset("mr-pin-mr-tulip") ::
        ImageAsset("mr-shine") ::
        ImageAsset("mr-slant") ::
        ImageAsset("mr-teatime") ::
        ImageAsset("mrs-cake") ::
        ImageAsset("nobby-nobbs") ::
        ImageAsset("otto-chriek") ::
        ImageAsset("patricians-palace") ::
        ImageAsset("pink-pussy-cat-club") ::
        ImageAsset("ponder-stibbons") ::
        ImageAsset("professor-of-recent-runes") ::
        ImageAsset("queen-molly") ::
        ImageAsset("reacher-gilt") ::
        ImageAsset("rincewind") ::
        ImageAsset("rosie-palm") ::
        ImageAsset("ruby") ::
        ImageAsset("sacharissa-cripslock") ::
        ImageAsset("sergeant-angua") ::
        ImageAsset("sergeant-cheery-littlebottom") ::
        ImageAsset("sergeant-colon") ::
        ImageAsset("sergeant-detritus") ::
        ImageAsset("shonky-shop") ::
        ImageAsset("sir-charles-lavatory") ::
        ImageAsset("stanley") ::
        ImageAsset("susan") ::
        ImageAsset("sybil-vimes") ::
        ImageAsset("the-agony-aunts") ::
        ImageAsset("the-alchemists-guild") ::
        ImageAsset("the-ankh-morpork-sunshine-dragon-sanctuary") ::
        ImageAsset("the-auditors") ::
        ImageAsset("the-bank-of-ankh-morpork") ::
        ImageAsset("the-beggars-guild") ::
        ImageAsset("the-bursar") ::
        ImageAsset("the-chair-of-indefinite-studies") ::
        ImageAsset("the-clacks") ::
        ImageAsset("the-dean") ::
        ImageAsset("the-duckman") ::
        ImageAsset("the-dysk") ::
        ImageAsset("the-fire-brigade") ::
        ImageAsset("the-fools-guild") ::
        ImageAsset("the-luggage") ::
        ImageAsset("the-mended-drum") ::
        ImageAsset("the-mob") ::
        ImageAsset("the-opera-house") ::
        ImageAsset("the-peeled-nuts") ::
        ImageAsset("the-post-office") ::
        ImageAsset("the-royal-mint") ::
        ImageAsset("the-seamstresses-guild") ::
        ImageAsset("the-senior-wrangler") ::
        ImageAsset("the-smoking-gnu") ::
        ImageAsset("the-thieves-guild") ::
        ImageAsset("the-watch") ::
        ImageAsset("wallace-sonky") ::
        ImageAsset("wee-mad-arthur") ::
        ImageAsset("william-de-worde") ::
        ImageAsset("willikins") ::
        ImageAsset("zorgo-the-retro-phrenologist") ::
    Nil)

    override def extLinks = $(
        ("Discworld: Ankh-Morpork Rules".txt -> "https://boardgamegeek.com/filepage/67676/ankh-morpork-rules-englishpdf")
    ) ++ super.extLinks

}
