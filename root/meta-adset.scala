package root

import root.gaming._

import colmat._

import hrf.meta._
import hrf.elem._

trait FactionsOption extends GameOption {
    def group = "Faction Pool"
}

case class IncludeFaction(faction : Faction) extends FactionsOption {
    def valueOn = faction.name.styled(faction)(xstyles.bold)
}

object MetaAdset extends MetaGame {
    val gaming = root.gaming

    type F = PlayerN
    type G = Game
    type O = root.GameOption

    def tagF = implicitly

    val name = "root-adset"
    val label = "Root: Advanced Setup"
    
    val factions = $(PlayerN(1), PlayerN(2), PlayerN(3), PlayerN(4), PlayerN(5), PlayerN(6), PlayerN(7), PlayerN(8))
    
    override def optionsFor(l : List[F]) = factionOptions ++ $(SeatingRandom, SeatingGiven) ++ Meta.optionsFor(Meta.factions).diff(hiddenOptions)
    
    val factionOptions = Meta.factions./(IncludeFaction)

    val defaultFactionOptions = (Meta.factions.take(10) :+ NB)./(IncludeFaction)
    
    val hiddenOptions = $(AdSetBuffOff, AdSetBuffOn) ++ $(SelectCharacter, RandomCharacter)
    
    val presetOptions = $(FactionSeatingRandom, SetupOrderReverse, SetupTypeHomelands, CardDraftFive, AdSetBuffOn, RandomCharacter)

    val options = factionOptions ++ Meta.options
  
    override def optimizeOptions(oo : List[O], l : List[F]) : List[O] = {
        var o = oo.diff(Meta.options) 

        if (oo.none)
            o = defaultFactionOptions

        o = o.diff(factionOptions.diff(defaultFactionOptions))

        val seating = (SeatingRandom +: oo).of[SeatingOption].last
            
        o ++ Meta.optimizeOptions(oo ++ $(seating) ++ presetOptions, Meta.factions).diff(presetOptions)
    }

    override val indistinguishableFactions : Boolean = true
    override val gradualFactions : Boolean = true

    val quickMin = 4
    val quickMax = 4
  
    override val quickFactions = factions.take(8)
 
    def randomGameName() = {
        val n = List("Power", "Victory", "Glory", "Destiny", "Might", "Fight", "Right", "Betrayal", "Wood", "Land", "Air", "Ground").shuffle
        val c = List("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : List[F]) = InfoResult("~~~")
 
    def validateFactionSeatingOptions(factions : List[F], options : List[O]) =
        if (factionOptions.intersect(options).num < factions.num + 1)
             ErrorResult("Not enough factions selected")
        else
            InfoResult("~~~")

    def factionName(f : F) = f.name
    def factionElem(f : F) = f.name.hh
 
    override def glyph(g : G) : Option[String] = Option(g.current)./(_.style + "-glyph")
    override def glyph(g : G, f : F) : Option[String] = Some(f)./~(g.ptf.get)./~(f => Some(f.style + "-glyph").%!(_ => g.highlightFaction.has(f) && hrf.HRF.uptime() / 1000 % 2 == 1))
    
    def createGame(factions : List[F], options : List[O]) = new Game(factions, options.of[IncludeFaction]./(_.faction), true, options ++ presetOptions)
    
    def getBots(f : F) = f match {
        case _ => $()
    }
    
    override def defaultBots = $()

    def getBot(f : F, b : String) = null

    def writeFaction(f : F) = f.short
    def parseFaction(s : String) = factions.%(_.short == s).single

    def writeOption(o : O) = o.toString
    def parseOption(s : String) = options.%(o => o.toString == s).single
    
    def parseAction(s : String) : Action = { println(s); Serialize.parseAction(s) }
    def writeAction(a : Action) : String = Serialize.write(a)
    
    val start = StartAction(gaming.version)
    
    override val path = "root"

    override val links : List[(String, String)] = $(("Standard Setup" -> "http://van.im:777/play/root"))

    val assets = Meta.assets./(m => ConditionalAssetsList((factions : List[F], options : List[O]) => m.condition(Meta.factions, options), (m.path != "").??("/") + m.path, m.prefix, m.ext)(m.list))
}
