package hrf.meta
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

import hrf.base._
import hrf.elem._
import hrf.options._

trait Laziness

object Laziness {
    case object Immediate extends Laziness
    case object Later extends Laziness
    case object OnDemand extends Laziness
}

case class ImageAsset(name : String, path : String, filename : String, ext : String, lzy : Laziness, scale : Double, lossless : Boolean) {
    def src = path + (path != "").??("/") + filename + (ext != "").??(".") + ext
    def makeLossless = copy(lossless = true)
    def scaled(s : Double) = copy(scale = s)
}

object ImageAsset {
    def apply(name : String) : ImageAsset = ImageAsset(name, "", name, "", Laziness.Immediate, 100, false)
    def apply(name : String, filename : String) : ImageAsset = ImageAsset(name, "", filename, "", Laziness.Immediate, 100, false)
    def apply(name : String, filename : String, ext : String) : ImageAsset = ImageAsset(name, "", filename, ext, Laziness.Immediate, 100, false)
    def apply(name : String, filename : String, ext : String, lzy : Laziness) : ImageAsset = ImageAsset(name, "", filename, ext, lzy, 100, false)
    def apply(name : String, filename : String, ext : String, lzy : Laziness, scale : Int) : ImageAsset = ImageAsset(name, "", filename, ext, lzy, scale, false)
    def apply(name : String, filename : String, ext : String, lzy : Laziness, scale : Int, lossless : Boolean) : ImageAsset = ImageAsset(name, "", filename, ext, lzy, scale, lossless)
}

abstract class ValidationResult(val ok : Boolean, val message : String, val style : Style) extends GoodMatch {
    def &&(v : ValidationResult) : ValidationResult = this @@ {
        case r : ErrorResult => r
        case r =>
            v @@ {
                case v : ErrorResult => v
                case v =>
                    r @@ {
                        case r : WarningResult => r
                        case r =>
                            v @@ {
                                case v : WarningResult => v
                                case v => v
                            }
                    }
            }
    }

    def &&(v : |[ValidationResult]) : ValidationResult = v./(this && _).|(this)
}

case class InfoResult(m : String) extends ValidationResult(true, m, xstyles.highlight)
case class WarningResult(m : String) extends ValidationResult(true, m, xstyles.warning)
case class ErrorResult(m : String) extends ValidationResult(false, m, xstyles.error)


trait GameOption extends BaseOption

trait MetaBase {
    val gaming : Gaming

    type G = gaming.G
    type F <: gaming.F
    type O = GameOption

    implicit def tagF : scala.reflect.ClassTag[F]

    import gaming._

    val name : String
    val label : String

    def settingsKey : String = name
    def settingsList : $[hrf.Setting] =
        $(hrf.CondensedButtonSpacing, hrf.NormalSpacing, hrf.ExpandedButtonSpacing) ++
        $(hrf.GameFontFace, hrf.CodeFontFace, hrf.SystemFontFace) ++
        $(hrf.SmallerFontSize, hrf.SmallFontSize, hrf.NormalFontSize, hrf.LargeFontSize, hrf.LargerFontSize) ++
        $(hrf.AlwaysFullScreen, hrf.TripleClickFullScreen, hrf.NeverFullScreen) ++
        $(hrf.SlowestScrollSpeed, hrf.SlowerScrollSpeed, hrf.SlowScrollSpeed, hrf.NormalScrollSpeed, hrf.FastScrollSpeed, hrf.FasterScrollSpeed, hrf.FastestScrollSpeed)

    def settingsDefaults : $[hrf.Setting] = $(hrf.NormalSpacing, hrf.GameFontFace, hrf.NormalFontSize, hrf.TripleClickFullScreen, hrf.NormalScrollSpeed)

    val about : $[Elem] = $

    val factions : $[F]

    def factionGroup(f : F) : |[Elem] = None

    val options : $[O]
    def optionsFor(n : Int, l : $[F]) : $[O] = options
    def optionPages(n : Int, l : $[F]) : $[$[O]] = $(optionsFor(n, l))
    def presetsFor(n : Int, l : $[F]) : $[(Elem, $[O], $[O])] = $()
    def defaultsFor(n : Int, l : $[F]) : $[O] = $
    def hiddenOptions : $[O] = $

    val indistinguishableFactions : Boolean = false
    val gradualFactions : Boolean = false
    val insignificantSeating : Boolean = false

    def randomGameName() : String

    def validateFactionCombination(factions : $[F]) : ValidationResult
    def validateFactionSeatingOptions(factions : $[F], options : $[O]) : ValidationResult

    def minPlayers : Int
    def maxPlayers : Int = factions.num

    def randomQuickGame() : (F, $[F], Map[F, String], $[O])

    def factionName(f : F) : String
    def factionElem(f : F) : Elem
    def factionNote(f : F) : Elem = Empty

    def glyph() : |[String] = None
    def glyph(g : G) : |[String] = glyph()
    def glyph(f : F) : |[String] = glyph()
    def glyph(g : G, f : F) : |[String] = glyph(f).||(glyph(g))

    def bodyFont : |[String] = None
    def bodyFontSupportsItalic : Boolean = false
    def titleFont : |[String] = bodyFont

    def createGame(factions : $[F], options : $[O]) : G

    val start : ExternalAction

    def writeFaction(f : F) : String
    def parseFaction(s : String) : |[F]

    def writeOption(f : O) : String
    def parseOption(s : String) : $[O]

    def parseActionExternal(s : String) : ExternalAction = {
        if (s.startsWith("// "))
            CommentAction(s.drop("// ".length))
        else
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

    case class ConditionalAssetsList(condition : ($[F], $[O]) => Boolean, path : String = "", prefix : String = "", ext : String = "png", lzy : Laziness = Laziness.Immediate, scale : Double = 100, lossless : Boolean = false)(val list : $[ImageAsset]) {
        def get = {
            var r = list

            if (path != "")
                r = r./(a => (a.path == "").?(a.copy(path = path)).|(a))

            if (ext != "")
                r = r./(a => (a.ext == "").?(a.copy(ext = ext)).|(a))

            if (prefix != "")
                r = r./(a => a.copy(name = prefix + a.name))

            if (lzy != Laziness.Immediate)
                r = r./(a => a.copy(lzy = lzy))

            if (scale != 100)
                r = r./(a => a.copy(scale = scale))

            if (lossless)
                r = r./(a => a.copy(lossless = true))

            r
        }
    }

    def path = name

    val assets : $[ConditionalAssetsList]

    def intLinks : $[(Elem, String)] = $

    def extLinks : $[(Elem, String)] = $

    def support : $[(String, String)] = $

}

trait MetaBots extends MetaBase {
    import hrf.bot._

    val gaming : BotGaming

    import gaming._

    def getBots(g : F) : $[String]
    def getBot(g : F, bot : String) : gaming.Bot
    def defaultBots : $[String]

    val quickMin : Int
    val quickMax : Int
    def quickFactions = factions
    def quickOptions = options./(_ -> 0.5).toMap

    def randomQuickGame() : (F, $[F], Map[F, String], $[O]) = {
        10000.times {
            val n = randomInRange(quickMin, quickMax)
            val l = quickFactions.shuffle.take(n)
            val f = l.shuffle.first
            val o = optionsFor(n, l)./~(o => quickOptions.get(o).%(_ > random())./(_ => o))

            validateFactionSeatingOptions(l, o) @@ {
                case ErrorResult(_) =>
                case _ =>
                    val d = l.but(f)./(e => e -> getBots(e).shuffle.first).toMap
                    return (f, l, d, o)
            }
        }

        ???
    }
}

trait MetaGame extends MetaBase with MetaBots
