package dwam

import colmat._

import hrf.meta._
import hrf.elem._

trait GameOption extends BaseGameOption

object Meta extends MetaGame { mmm =>
    val gaming = dwam.gaming
    
    type F = Faction
    type G = Game
    type O = GameOption

    def tagF = implicitly
 
    val name = "dwam"
    val label = "Discworld: Ankh-Morpork"

    val factions = List(Red, Yellow, Green, Blue, White)
    
    override val quickFactions = factions.take(4)

    val options = Nil
    
    override val gradualFactions : Boolean = true

    val quickMin = 4
    val quickMax = 4

    def randomGameName() = {
        val n = List("Magic", "Octarine", "Death", "Fate", "Music", "Power").shuffle
        val c = List("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }
                                                               
    def validateFactionCombination(factions : List[Faction]) = (factions.num < 2).?(ErrorResult("Select at least two factions")).|((factions.num > 4).?(ErrorResult("Max four factions")).|(InfoResult("")))
    def validateFactionSeatingOptions(factions : List[Faction], options : List[O]) = InfoResult("---")
    
    
    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    def createGame(factions : List[Faction], options : List[O]) = new Game(StandardBoard, factions, true, options)
    
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
        ImageAsset("map" ) ::
        ImageAsset("map-regions" ) ::

        ImageAsset("b-building" ) ::
        ImageAsset("g-building" ) ::
        ImageAsset("r-building" ) ::
        ImageAsset("y-building" ) ::
        ImageAsset("w-building" ) ::

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
        
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true, "cards", "character:")(
        ImageAsset("chrysoprase") ::
        ImageAsset("commander-vimes") ::
        ImageAsset("dragon-king-of-arms") ::
        ImageAsset("lord-vetinari") ::
        ImageAsset("lord-rust") ::
        ImageAsset("lord-selachii") ::
        ImageAsset("lord-de-worde") ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true, "cards", "character:")(
        ImageAsset("chrysoprase") ::
        ImageAsset("commander-vimes") ::
        ImageAsset("dragon-king-of-arms") ::
        ImageAsset("lord-vetinari") ::
        ImageAsset("lord-rust") ::
        ImageAsset("lord-selachii") ::
        ImageAsset("lord-de-worde") ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true, "cards", "area:")(
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
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true, "cards", "event:")(
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
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true, "cards", "card:")(
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
}
