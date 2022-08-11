package hrf.meta

import hrf.base._
import hrf.elem._

import colmat._

case class ImageAsset(name : String, path : String, filename : String, ext : String) {
    def src = path + (path != "").??("/") + filename + (ext != "").??(".") + ext
}

object ImageAsset {
    def apply(name : String) : ImageAsset = ImageAsset(name, "", name, "")
    def apply(name : String, filename : String) : ImageAsset = ImageAsset(name, "", filename, "")
    def apply(name : String, filename : String, ext : String) : ImageAsset = ImageAsset(name, "", filename, ext)
}

abstract class ValidationResult(val ok : Boolean, val message : String, val style : Style)

case class InfoResult(m : String) extends ValidationResult(true, m, xstyles.highlight)
case class WarningResult(m : String) extends ValidationResult(true, m, xstyles.warning)
case class ErrorResult(m : String) extends ValidationResult(false, m, xstyles.error)

/*
trait BaseOption {
    def values : List[BaseOptionValue]
}

trait BaseOptionValue
*/


trait BaseGameOption {
    def group : Elem
    def valueOn : Elem
    def valueOff : Elem = Text(valueOn.text)
}

trait ImportantOption extends BaseGameOption


trait MetaBase {
    val gaming : Gaming

    type G <: gaming.G
    type F <: gaming.F
    type O <: BaseGameOption
    
    implicit def tagF : scala.reflect.ClassTag[F] //= implicitly

    import gaming._

    val name : String
    val label : String

    val factions : List[F]
    val options : List[O]
    def optionsFor(l : List[F]) : List[O] = options

    val indistinguishableFactions : Boolean = false
    val gradualFactions : Boolean = false
    ///// val insignificantSeating : Boolean = false

    def randomGameName() : String
    
    def optimizeOptions(options : List[O], l : List[F]) : List[O] = options
    def validateFactionCombination(factions : List[F]) : ValidationResult
    def validateFactionSeatingOptions(factions : List[F], options : List[O]) : ValidationResult
    
    def minPlayers : Int = 2
    def maxPlayers : Int = factions.num
    val quickMin : Int
    val quickMax : Int
    def quickFactions = factions
    
    def factionName(f : F) : String
    def factionElem(f : F) : Elem
    def factionNote(f : F) : Elem = Empty
//    def factionGlyph(f : F) : Option[String] = None

    def glyph() : Option[String] = None
    def glyph(g : G) : Option[String] = glyph()
    def glyph(f : F) : Option[String] = glyph()
    def glyph(g : G, f : F) : Option[String] = glyph(f).||(glyph(g))
    
    def createGame(factions : List[F], options : List[O]) : G
    
    val start : ExternalAction

    def writeFaction(f : F) : String
    def parseFaction(s : String) : Option[F]

    def writeOption(f : O) : String
    def parseOption(s : String) : Option[O]

    def parseActionExternal(s : String) : ExternalAction = {
        parseAction(s) match {
            case a : ExternalAction => a
            case a : ForcedAction => DoAction(a)
        }
    }

    def writeActionExternal(a : ExternalAction) : String = {
        writeAction(a.unwrap)
    }

    def parseAction(s : String) : Action
    def writeAction(a : Action) : String
    
    case class ConditionalAssetsList(condition : (List[F], List[O]) => Boolean, path : String = "", prefix : String = "", ext : String = "png")(val list : List[ImageAsset]) {
        def get = {
            var r = list
            
            if (path != "") {
                r = r./(a => (a.path == "").?(a.copy(path = path)).|(a))
            }

            if (ext != "") {
                r = r./(a => (a.ext == "").?(a.copy(ext = ext)).|(a))
            }
            
            if (prefix != "") {
                r = r./(a => a.copy(name = prefix + a.name))
            }
            
            r
        }
    }

    def path = name
    
    val assets : List[ConditionalAssetsList]

    val links : List[(String, String)] = Nil
    
    case class Setup(seating : List[F], difficulty : Map[F, Difficulty], options : List[O])
    
    object Setup {
        def apply(factions : List[F], difficulty : Difficulty) : Setup = Setup(factions, factions.map(_ -> difficulty).toMap, optimizeOptions(Nil, factions))
    }
    
}

trait MetaBots extends MetaBase {
    import hrf.bot._
    
    val gaming : BotGaming

    import gaming._

    def getBots(g : F) : List[String]
    def getBot(g : F, bot : String) : gaming.EvalBot
    def defaultBots : $[String] = $("Normal")
}


trait MetaGame extends MetaBase with MetaBots
