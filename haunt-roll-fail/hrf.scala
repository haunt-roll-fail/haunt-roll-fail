package hrf
//
//
//
//
import hrf.colmat._
//
//
//
//

import hrf.elem._
import hrf.html._
import hrf.meta._
import hrf.web._
import hrf.base._
import hrf.ui._
import hrf.loader._
import hrf.options._

import org.scalajs.dom

import scalajs.js.timers.setTimeout
import scalajs.js.Dynamic

import scala.collection.mutable


object HRF {
    val version = BuildInfo.version

    val imageDataVersion = "as-of-0.8.87"

    def now() = new scalajs.js.Date()

    val startAt = now()

    def uptime() : Int = (now().getTime() - startAt.getTime()).toInt

    val defaultGlyph = getElem("icon").asInstanceOf[dom.html.Link].href

    def glyph(s : String) = getElem("icon").asInstanceOf[dom.html.Link].href = s

    val imageCache = new CachedBlobImageLoader("hrf-image-cache-" + imageDataVersion)
    val stringCache = new CachedStringLoader("hrf-string-cache-" + version)

    private val settings = getElem("settings").?

    def hash = dom.window.location.hash.drop(1)
    val search = dom.window.location.search.drop(1)

    private def settingsParam(p : String) = settings./~(_.getAttribute("data-" + p).?).but("")
    private def hashParam(p : String)  = hash.split('|').$./(_.split('=')).%(_(0) == p).single./(_.drop(1).join("=")).map(java.net.URLDecoder.decode(_, "UTF-8"))
    private def urlParam(p : String) = search.split('&').$./(_.split('=')).%(_(0) == p).single./(_.drop(1).join("=")).map(java.net.URLDecoder.decode(_, "UTF-8"))

    private val params = mutable.Map[String, |[String]]()

    dom.window.onhashchange = e => params.clear()

    def param(p : String) = params.getOrElseUpdate(p, settingsParam(p) || hashParam(p) || urlParam(p))

    def flag(p : String) = param(p).any

    def paramInt(p : String) = param(p)./~(_.toIntOption)

    def paramList(p : String) = param(p)./~(_.split(' ').$)

    val versionOverride = hashParam("version") || urlParam("version")

    var segments = dom.window.location.pathname.split('/').$.drop(3)

    val offline = flag("offline")
    val embedded = flag("embedded-assets")

    val metaUIs : $[(MetaGame, BaseUI)] =
        root.Meta -> root.UI ::
        root.MetaAdset -> root.UI ::
        root.MetaMirror -> root.UI ::
        cthw.Meta -> cthw.UI ::
        dwam.Meta -> dwam.UI ::
        vast.Meta -> vast.UI ::
        arcs.Meta -> arcs.UI ::
        coup.Meta -> coup.UI ::
        sehi.Meta -> sehi.UI ::
        suok.Meta -> suok.UI ::
        yarg.Meta -> yarg.UI ::
    $

    val metas = metaUIs.lefts.%(_.path != "root" || HRF.param("lobby").any || HRF.flag("qqqbbb") || offline)

    val html = dom.window.location.origin + "/play/"
    val script = dom.document.getElementById("script").asInstanceOf[dom.html.Script].src

    def onKey(filter : dom.KeyboardEvent => Boolean)(process : => Unit) {
        val old = dom.document.onkeyup
        dom.document.onkeyup = (e) => {
            if (filter(e)) {
                process
            }
            else
            if (scalajs.js.isUndefined(old).not && old != null) {
                old(e)
            }
        }
    }

    def writeOnlineGame(og : OnlineGame) : String = (
        $("version " + og.version) ++
        $("meta " + og.meta) ++
        $("title " + og.title) ++
        og.options./("option " + _) ++
        og.links./(l => "link " + l.faction + " " + l.key + " " + l.note) ++
        $("spectate " + og.spectate) ++
        og.status./("status " + _)
    ).join("\n")

    def parseOnlineGame(lobby : String, time : Double, body : String) : OnlineGame = {
        val lines = body.split('\n').$
        def p(prefix : String) = lines.%(_.startsWith(prefix))./(_.substring(prefix.length))
        def pp(prefix : String) = p(prefix)./(_.split(' ').$)

        OnlineGame(time, p("version ").single.|("unknown.version"), p("meta ").single.|("unknown-meta"), lobby, p("title ").single.|("Unnamed Game"), p("option "), pp("link ")./(l => PlayerLink(l(0), l(1), l.drop(2).join(" "))), p("spectate ").single.|("no/spectator/link"), p("status "))
    }

    def main(args : Array[String]) {
        val fonts = dom.document.fonts

        fonts.forEach { c =>
            fonts.load("20px " + c.family)
        }

        if (uptime() < 3000)
            fonts.forEach { c =>
                if (c.status != "loaded") {
                    setTimeout(20) { main(args) }
                    return
                }
            }

        if (fonts.status == "loading" && uptime() < 3000)
            setTimeout(20) { main(args) }
        else
        if (dom.document.readyState == dom.DocumentReadyState.complete)
            new HRFUI()
        else
            dom.window.onload = (e) => new HRFUI()
    }

}

class Quants(duration : Int, count : Int) {
    private var last = HRF.uptime()
    private var n = 0

    def continue(f : () => Unit) {
        val now = HRF.uptime()

        if (now - last < duration && n < count) {
            n += 1
            f()
        }
        else {
            last = now
            n = 0
            setTimeout(0)(continue(f))
        }
    }
}


trait Switches {
    def canSwitches : Boolean
    def getSwitches : $[String]
    def enableSwitch(s : String)
    def disableSwitch(s : String)
}

object NoSwitches extends Switches {
    def canSwitches = false
    def getSwitches = $
    def enableSwitch(s : String) {}
    def disableSwitch(s : String) {}
}

trait Callbacks {
    def switches : Switches
    def canPlayAgain : Boolean
    def playAgain() : Unit
    def saveReplay(onSave : => Unit) : Unit
    def saveReplayOnline(replaceBots : Boolean, mergeHumans : Boolean)(onSave : String => Unit) : Unit
    def settings : $[Setting]
    def editSettings(onEdit : => Unit) : Unit
}

sealed trait Difficulty

case object Off extends Difficulty
case object Recorded extends Difficulty
case object Human extends Difficulty
case class BotDebug(name : String) extends Difficulty
case class Bot(name : String) extends Difficulty
case object AllVsHuman extends Difficulty

case class OnlineGame(time : Double, version : String, meta : String, lobby : String, title : String, options : $[String], links : $[PlayerLink], spectate : String, status : $[String])

case class PlayerLink(faction : String, key : String, note : String)

case class BotAssigned(faction : String, bot : String)

case class HotseatGame(time : Double, version : String, meta : String, title : String, options : $[String], bots : $[BotAssigned], status : $[String])


class HRFUI {

    implicit val zr = Resources(ImageResources(Map(), Map(
        "question-mark" -> "/hrf/question-mark.png",
        "external-link" -> "/hrf/external-link.png",

        "battle" -> "/hrf/webp2/root/images/icon/battle.webp",
        "swap" -> "/hrf/webp2/root/images/icon/swap.webp",

        "vb-title-tinker" -> "/hrf/webp2/root/images/faction/hero/vb-title-tinker.webp",
        "vb-char-tinker"  -> "/hrf/webp2/root/images/faction/hero/vb-char-tinker.webp",
        "vb-title-harrier" -> "/hrf/webp2/root/images/faction/hero/vb-title-harrier.webp",
        "vb-char-harrier"  -> "/hrf/webp2/root/images/faction/hero/vb-char-harrier.webp",
        "vb-title-scoundrel" -> "/hrf/webp2/root/images/faction/hero/vb-title-scoundrel.webp",
        "vb-char-scoundrel"  -> "/hrf/webp2/root/images/faction/hero/vb-char-scoundrel.webp",
        "vb-title-arbiter" -> "/hrf/webp2/root/images/faction/hero/vb-title-arbiter.webp",
        "vb-char-arbiter"  -> "/hrf/webp2/root/images/faction/hero/vb-char-arbiter.webp",
        "vb-title-ronin" -> "/hrf/webp2/root/images/faction/hero/vb-title-ronin.webp",
        "vb-char-ronin"  -> "/hrf/webp2/root/images/faction/hero/vb-char-ronin.webp",
        "vb-title-folk-hero" -> "/hrf/webp2/root/images/faction/hero/vb-title-folk-hero.webp",
        "vb-char-folk-hero"  -> "/hrf/webp2/root/images/faction/hero/vb-char-folk-hero.webp",

        "item-boots"     -> "/hrf/webp2/root/images/item/item-boots.webp",
        "item-coins"     -> "/hrf/webp2/root/images/item/item-coins.webp",
        "item-sword"     -> "/hrf/webp2/root/images/item/item-sword.webp",
        "item-torch"     -> "/hrf/webp2/root/images/item/item-torch.webp",
        "item-bag"       -> "/hrf/webp2/root/images/item/item-bag.webp",
        "item-crossbow"  -> "/hrf/webp2/root/images/item/item-crossbow.webp",
        "item-hammer"    -> "/hrf/webp2/root/images/item/item-hammer.webp",
        "item-teapot"    -> "/hrf/webp2/root/images/item/item-teapot.webp",

        "quest:errand-fox"             -> "/hrf/webp2/root/images/card/quest/revised/errand-fox.webp",
        "quest:errand-rabbit"          -> "/hrf/webp2/root/images/card/quest/revised/errand-rabbit.webp",
        "quest:escort-mouse"           -> "/hrf/webp2/root/images/card/quest/revised/escort-mouse.webp",
        "quest:repair-a-shed-rabbit"   -> "/hrf/webp2/root/images/card/quest/revised/repair-a-shed-rabbit.webp",
        "quest:fundraising-fox"        -> "/hrf/webp2/root/images/card/quest/revised/fundraising-fox.webp",
        "quest:escort-fox"             -> "/hrf/webp2/root/images/card/quest/revised/escort-fox.webp",

        "borscht-kitchens"        -> "/hrf/webp2/root/images/card/deck/borscht-kitchens.webp",
    ), HRF.imageCache), () => Map())

    StyleRegister.add($(xstyles.pane))
    StyleRegister.add($(xstyles.outer))
    StyleRegister.add($(xstyles.inner, xstyles.pane.log))
    StyleRegister.add($(xstyles.choice, xstyles.chm, xstyles.chp, xstyles.thu, xstyles.xx, xlo.pointer))
    StyleRegister.add($(xlo.fullwidth, xlo.pre))

    val uir = new ElementAttachmentPoint(getElem("root-attachment-point"))

    val guir = new ElementAttachmentPoint(getElem("game-attachment-point"))

    val logger = new NewLogger(new ElementAttachmentPoint(getElem("log")), true)

    dom.window.onerror = (_, _, _, _, error) => {
        logger.alog(error.toString.spn.div(xstyles.error))
    }

    object action {
        val hook = getElem("action")
        val pane = new ElementAttachmentPoint(hook.parentElement.parentElement)
        val asker = new Asker(new ElementAttachmentPoint(hook.parentElement).appendContainer(
            Div(
                Div(
                    Div(Content,
                    xstyles.middleScrollIn),
                xstyles.middleScrollOut),
            xstyles.unselectable, xstyles.inner, xstyles.pane.action
            ), zr).attach, Map())
        val scroll = hook.parentElement.children(1)
        hook.remove()
    }

    object overlay {
        val hook = getElem("overlay")
        val pane = new ElementAttachmentPoint(hook.parentElement.parentElement)
        val asker = new Asker(new ElementAttachmentPoint(hook.parentElement).appendContainer(
            Div(
                Div(
                    Div(Content,
                    xstyles.middleScrollIn),
                xstyles.middleScrollOut),
            xstyles.unselectable, xstyles.inner, xstyles.pane.action
            ), zr).attach, Map())
        val scroll = hook.parentElement.children(1)
        hook.remove()
    }

    logger.alog(Empty ~ BuildInfo.name ~ " " ~ BuildInfo.version)

    def topMenu() {
        action.asker.zask(HRF.metas./(m => ZBasic("Haunt Roll Fail".hh(xstyles.larger110)(ExternalStyle("consolas")), m.label.spn(xstyles.larger110)(ExternalStyle(m.titleFont.|(""))), () => new HRFMetaUI(this, m, 0).withMeta(), ZBasic.choice.but(xstyles.thumargin))))
    }

    def topInfo() {
    }

    HRF.param("meta")./~(mn => HRF.metas.%(_.name == mn).single)./{m =>
        new HRFMetaUI(this, m, 800).withMeta()
    }.|{
        topMenu()
    }

}

class HRFMetaUI(val ui : HRFUI, val meta : MetaGame, delayMainMenu : Int)(implicit resources : Resources) {
    var settings : $[Setting] = $

    var history = new web.History("/play/" + meta.name, dom.window.location.search, dom.window.location.hash)

    def loadSettings() : $[Setting] = {
        val saved = Local.get(meta.settingsKey + ".settings", "").split(' ').$

        val preset = HRF.paramList("settings")

        OptionsState[Setting](meta.settingsList, preset.some.|(saved)./~(s => meta.settingsList.find(_.toString == s)), meta.settingsDefaults).checkDimmed().selected
    }

    def applySettings(settings : $[Setting]) {
        def setBodyStyle(style : String, italic : Boolean) {
            while (dom.document.body.classList.length > 0)
                dom.document.body.classList.remove(dom.document.body.classList.item(0))

            dom.document.body.classList.add(style)
            dom.document.styleSheets(0).as[dom.CSSStyleSheet].get.cssRules.iterator.$.of[dom.CSSStyleRule].filter(_.selectorText == ".italic").foreach(_.style.fontStyle = italic.??("italic"))
        }

        if (settings.has(GameFontFace))
            setBodyStyle(meta.bodyFont.|("luminari"), meta.bodyFontSupportsItalic)
        else
        if (settings.has(CodeFontFace))
            setBodyStyle("consolas", true)
        else
        if (settings.has(SystemFontFace))
            setBodyStyle("system-ui", true)

        val (margin, padding) = settings.of[ButtonSpacingSetting].lastOption @@ {
            case Some(CondensedButtonSpacing) => ("0.4ex", "0.2ex")
            case Some(NormalSpacing) | None => ("0.8ex", "0.4ex")
            case Some(ExpandedButtonSpacing) => ("1.2ex", "1.2ex")
        }

        getElem("font-size-adjust").style.fontSize = settings.of[FontSizeSetting].lastOption @@ {
            case Some(SmallerFontSize) => "84%"
            case Some(SmallFontSize) => "92%"
            case Some(NormalFontSize) | None => "100%"
            case Some(LargeFontSize) => "108%"
            case Some(LargerFontSize) => "120%"
        }

        dom.document.styleSheets(0).as[dom.CSSStyleSheet].get.cssRules.iterator.$.of[dom.CSSStyleRule].filter(_.selectorText == ".thumargin").foreach { r =>
            r.style = (
                "margin-top: " + margin + " !important;" +
                "margin-bottom: " + margin + " !important;" +
                "padding-top: " + padding + " !important;" +
                "padding-bottom: " + padding + " !important;").asInstanceOf[dom.CSSStyleDeclaration]
        }

        dom.window.asInstanceOf[Dynamic].clicksToSwitchFullScreen = settings.of[FullScreenSetting].lastOption @@? {
            case Some(AlwaysFullScreen) => dom.document.documentElement.requestFullscreen(); 0
            case Some(TripleClickFullScreen) | None => 3
            case Some(NeverFullScreen) => try { dom.document.exitFullscreen(); } catch { case e : Throwable => }; 999999
        }
    }

    def saveSettings(l : $[Setting]) {
        val ll = OptionsState[Setting](meta.settingsList, l, meta.settingsDefaults).checkDimmed().selected

        Local.set(meta.settingsKey + ".settings", ll./(_.toString).mkString(" "))
    }

    dom.window.onhashchange = {
        val old = dom.window.onhashchange

        e => {
            println("onhashchange")
            println("settings before "+ settings)

        old(e) ; settings = loadSettings() ; applySettings(settings)

            println("settings after "+ settings)
        }
    }

    def editSettings(then : => Unit) {
        ui.overlay.pane.vis()

        var ostate = OptionsState[Setting](meta.settingsList, settings, meta.settingsDefaults).checkDimmed()

        def ask() {
            ui.overlay.asker.zask(
                meta.settingsList./({ o =>
                    val state = ostate.selected.has(o)
                    val active = ostate.enabled(o)
                    ZBasic(o.group, state.?(o.valueOn).|(o.valueOff), (active || state).??(() => {
                        if (active) {
                            ostate = ostate.click(o)
                            settings = ostate.selected
                            saveSettings(settings)
                            applySettings(settings)
                            ask()
                        }
                    }), ZBasic.choice ++ $(xlo.fullwidth)).copy(clear = false)
                }) ++
                $(ZBasic("", "Reset All Settings", () => {
                    ostate = OptionsState[Setting](meta.settingsList, $, meta.settingsDefaults).checkDimmed()
                    settings = ostate.selected
                    saveSettings(settings)
                    applySettings(settings)
                    ask()
                }, ZBasic.choice ++ $(xlo.fullwidth))) ++
                $(ZBasic(" ", "Done".hh, () => {
                    ui.overlay.pane.invis()
                    then
                }, ZBasic.choice ++ $(xlo.fullwidth)))
            )
        }

        ask()

        val background = dom.document.getElementById("blurred-background").asInstanceOf[dom.html.Div]

        background.onclick = (e) => {
            background.onclick = null
            ui.overlay.pane.invis()
            then
        }
    }

    case class Setup(seating : $[meta.F], difficulty : Map[meta.F, Difficulty])

    def withMeta() {
        ui.logger.alog(meta.label.spn.div)

        if (meta.name == "cthw" && false.not) {
            dom.document.location.assign("https://cwo.im/")
            return
        }

        settings = loadSettings()
        saveSettings(settings)
        applySettings(settings.notOf[FontSizeSetting])

        ui.topInfo()

        dom.document.title = meta.label

        if (HRF.param("lobby").any || HRF.flag("replay")) {
            val (user, lj) = if (HRF.flag("replay")) {
                ("", new ReplayPhantomJournal[String](meta, getElem("lobby").textContent, identity))
            }
            else {
                val server = HRF.param("server").get
                val lobby = HRF.param("lobby").get
                val user = HRF.param("user").get
                val secret = HRF.param("secret").get

                (user, new ServerJournal[String](meta, server, user, secret, lobby, identity, identity))
            }

            var setup : Setup = null

            var enteredNames = Map[String, String]()
            var preNames = Map[String, String]()
            var users = Map[meta.F, String]()
            var switches = $[String]()
            var bots = Map[meta.F, String]()
            var seating = $[meta.F]()
            var options = $[meta.O]()
            var server : |[String] = None
            var title : |[String] = None
            var version : |[String] = None

            var position = 0
            var cycle = false

            def reread() { lj.read(position)(readLobby) }

            def readLobby(lines : $[String]) {
                position += lines.num

                lines.foreach { line =>
                    val l = line.split(' ').$
                    l(0) match {
                        case "meta" =>
                            val m = l(1).sanitize(32)
                            if (m != meta.name) {
                                val l = dom.document.location
                                val url = l.origin + "/play/" + m + "/" + l.pathname.split('/').drop(3).join("/") + l.search + l.hash
                                ui.logger.alog(" Reload " + url)
                                dom.document.location.assign(url)
                                throw new Error("incorrect meta")
                            }
                        case "version" =>
                            version = Some(l(1))./(v => if (v == "2") "0.8.73" else v)
                        case "user" =>
                            users += meta.parseFaction(l(1)).get -> l(2)
                        case "bot" =>
                            bots += meta.parseFaction(l(1)).get -> l(2)
                        case "seating" =>
                            seating = l.drop(1)./(f => meta.parseFaction(f).get)
                        case "options" =>
                            options = l.drop(1)./~(o => meta.parseOption(o))
                        case "server" =>
                            server = Some(l(1))
                        case "title" =>
                            title = Some(l.drop(1).join(" ").sanitize(32))
                        case "name" =>
                            enteredNames += l(1) -> (l.drop(2).join(" ").sanitize(32))
                        case "prename" =>
                            preNames += l(1) -> (l.drop(2).join(" ").sanitize(32))
                        case "enable" =>
                            switches :+= l.drop(1).join(" ")
                        case "disable" =>
                            switches --= l.drop(1).join(" ")
                        case _ =>
                    }
                }

                if (options.none)
                    options = meta.defaultsFor(seating.num, seating)

                val self = users.keys.%(f => users(f) == user).$

                self.single.foreach { f =>
                    val assetsources = meta.assets.%(_.condition(seating, options))./~(_.get)./(a => a.name -> (meta.path + "/images/" + a.src)).toMap

                    meta.glyph(f).foreach { s =>
                        assetsources.get(s.toLowerCase).foreach { src =>
                            HRF.glyph(src)
                        }
                    }
                }

                if (version.any && version.has(HRF.version).not && HRF.flag("override").not) {
                    val q = "This game was created on " ~ "HRF".hl ~ " version " ~ version.get.hl ~ (version.get == "0.8.73").??(" or earlier")

                    ui.action.asker.zask(
                        ZBasic(q, ("Load version ".hh ~ version.get.hlb), () => {
                            val l = dom.document.location
                            val search = ("version=" + version.get) +: l.search.drop(1).split('&').$.%(_.startsWith("version").not).but("")
                            val url = l.origin + l.pathname + "?" + search.join("&") + l.hash
                            ui.logger.alog(" Reload " + url)
                            dom.document.location.assign(url)
                        }) ::
                        ZBasic(q, "Try loading version " ~ HRF.version.hh, () => {
                            version = Some(HRF.version)
                            setTimeout(0) { reread() }
                            ()
                        })
                    )
                }
                else
                if (self.any && enteredNames.contains(user).not) {
                    def randomName() {
                        val adjective = $(
                            "Ardent", "Adventurous", "Agile", "Alert",
                            "Brave", "Bold",
                            "Clever", "Cunning", "Caring", "Calm", "Curious",
                            "Daring", "Diligent",
                            "Eager", "Eloquent",
                            "Fair", "Fearless",
                            "Good", "Generous", "Gentle", "Gracious",
                            "Honorable", "Honest", "Humble",
                            "Ingenious",
                            "Jolly", "Just",
                            "Kind",
                            "Loyal",
                            "Merciful", "Modest",
                            "Noble", "Nice",
                            "Observant",
                            "Proud", "Patient", "Prudent",
                            "Quick",
                            "Righteous", "Resolute", "Resilent",
                            "Selfless", "Sincere",
                            "Tough", "Tenacious",
                            "Unyielding",
                            "Vigorous", "Valiant",
                            "Wholesome", "Wise",
                            "Xenial",
                            "Yearning",
                            "Zealous")
                        val animal = $(
                            "Armadillo", "Albatross", "Ant", "Ape",
                            "Badger", "Barracuda", "Bear", "Beaver", "Bat", "Bee", "Boar", "Butterfly",
                            "Camel", "Cat", "Cow", "Cheetah", "Crab", "Crane", "Crocodile", "Crow",
                            "Dog", "Deer", "Duck", "Dolphin", "Donkey", "Dragonfly",
                            "Eagle", "Elephant", "Elk", "Eel", "Emu",
                            "Fox", "Fish", "Ferret", "Falcon", "Flamingo", "Frog",
                            "Giraffe", "Gazelle", "Gecko", "Goat", "Goose", "Gull",
                            "Hare", "Horse", "Hamster", "Hawk", "Hedgehog", "Hyena",
                            "Iguana",
                            "Jellyfish", "Jaguar",
                            "Kangaroo", "Koala",
                            "Lion", "Llama", "Lemur", "Leopard", "Lizard",
                            "Monkey", "Mouse", "Mongoose", "Moose",
                            "Newt", "Nightingale",
                            "Otter", "Octopus", "Opossum", "Ostrich", "Owl",
                            "Panda", "Pig", "Panther", "Parrot", "Pelican", "Penguin", "Porcupine",
                            "Quail",
                            "Raccoon", "Rat", "Raven", "Rhinoceros",
                            "Sheep", "Shark", "Sloth", "Snake", "Scorpion", "Skunk", "Squirell", "Salamander", "Seal", "Snail", "Swan",
                            "Turtle", "Tiger", "Tapir", "Toad",
                            "Urchin",
                            "Vulture",
                            "Wolf", "Walrus", "Whale", "Weasel",
                            "Zebra")
                        askName(adjective.shuffle.head + " " + animal.shuffle.head)
                    }

                    def postName(value : String, retries : Int) {
                        val name = value.split(' ').filter(_ != "").join(" ").sanitize(32)
                        if (name == value && retries < 3) {
                            setCookie("name", name, Some(3650))
                            lj.append($("name", user, name).join(" "))(reread())(postName(name, retries + 1))
                        }
                        else
                            askName(name)
                    }

                    def askName(name : String) {
                        ui.action.asker.iask(InputOption(|("Name").|("What's your name, punk?"), name, Nil, v => postName(v, 0)) :: BasicOption(" ", "Generate Random", Nil, () => randomName()))
                    }

                    askName(getCookie("name", ""))
                }
                else
                if (setup == null) {
                    dom.document.title = (self./(meta.factionName) :+ title.|("%untitled%") :+ meta.label).join(" - ")

                    setup = Setup(seating, seating./(_ -> HRF.param("debug")./(bot => BotDebug(bot)).|(Human)).toMap)
                    val state = OptionsState(meta.options, options, $)
                    setup = setup.copy(difficulty = setup.difficulty ++ bots./{ case (f, d) => f -> Bot(d) })

                    def names : Map[meta.F, String] = users.flatMap {
                        case (f, i) => preNames.get(i)./(f -> _)
                    } ++ users.flatMap {
                        case (f, i) => enteredNames.get(i)./(f -> _)
                    }


                    object ServerSwitches extends Switches {
                        def canSwitches = true
                        def getSwitches = switches
                        def enableSwitch(s : String) {
                            switches :+= s
                            lj.append($("enable", s).join(" ")){reread()}{reread()}
                        }
                        def disableSwitch(s : String) {
                            switches --= s
                            lj.append($("disable", s).join(" ")){reread()}{reread()}
                        }
                    }

                    val journal =
                        if (HRF.flag("replay"))
                            new ReplayPhantomJournal[meta.gaming.ExternalAction](meta, getElem("replay").textContent, s => meta.parseActionExternal(s))
                        else
                            new ServerJournal[meta.gaming.ExternalAction](meta, HRF.param("server").get, HRF.param("user").get, HRF.param("secret").get, server.get, s => meta.parseActionExternal(s), s => meta.writeActionExternal(s), HRF.paramInt("at") | 999999)

                    startGame(setup, state.selected, self, journal, title.|("%untitled%"), () => names, ServerSwitches)

                    reread()

                    if (HRF.flag("replay").not) {
                        val lobby = HRF.param("lobby").get
                        val user = HRF.param("user").get

                        val link = dom.document.location.pathname.split('/').$.drop(3).single.|("unknown/link")
                        val faction = users.keys.$.%(k => users(k) == user).single
                        val name = preNames.get(user).|("")

                        val prefix = meta.settingsKey + ".online.game." + lobby + "."

                        val rg = OnlineGame(HRF.now().getTime(), version.|("unknown.version"), meta.name, lobby, title.|("Unknown Title"), options.of[ImportantOption]./(meta.writeOption), faction.$./(f => PlayerLink(meta.writeFaction(f), link, name)), faction.none.??(link), $)

                        val time = Local.list(prefix)./(_.drop(prefix.length).toDouble).maxOr(0)

                        val og = (time > 0).?(HRF.parseOnlineGame(lobby, time, Local.get(prefix + time, ""))).|(rg)

                        val ng = og.copy(links = (og.links ++ rg.links).distinctBy(_.key), spectate = og.spectate.some || rg.spectate.some | "")

                        Local.set(prefix + ng.time, HRF.writeOnlineGame(ng))
                    }
                }
                else {
                    if (cycle.not) {
                        cycle = true
                        setTimeout(15000) {
                            cycle = false
                            reread()
                        }
                    }
                }
            }

            reread()
        }
        else
        if (HRF.flag("quick")) {
            def generateFactions() : $[meta.F] = {
                var i = 0

                while (i < 1000) {
                    var l = HRF.paramList("factions")./(meta.parseFaction(_).get).some.|({
                        val included = HRF.paramList("include")./(meta.parseFaction(_).get)
                        val excluded = HRF.paramList("exclude")./(meta.parseFaction(_).get)
                        val total = meta.quickMin.to(meta.quickMax).$.shuffle.head
                        included ++ meta.factions.diff(excluded).diff(included).shuffle.take(total).drop(included.num)
                    })

                    if (meta.validateFactionCombination(l).is[ErrorResult].not)
                        return l

                    i += 1
                }

                throw new Error("could not generate a valid quick faction combination")
            }

            val factions = generateFactions()

            var setup = Setup(factions, factions./(_ -> Bot(meta.defaultBots(0))).toMap)
            var options = OptionsState(meta.optionsFor(factions.num, factions), $, meta.defaultsFor(factions.num, factions)).checkDimmed()

            HRF.paramList("human")./(meta.parseFaction(_).get).some.|($(factions.shuffle.head)).foreach { h =>
                setup = setup.copy(difficulty = setup.difficulty + (h -> HRF.flag("debug").?(BotDebug(meta.defaultBots(0))).|(Human)))
            }

            HRF.paramList("options")./~(meta.parseOption).use { o =>
                options = OptionsState(meta.optionsFor(factions.num, factions), o, meta.defaultsFor(factions.num, factions)).checkDimmed()
            }

            val journal = new MemoryJournal[meta.gaming.ExternalAction](meta)

            startGame(setup, options.selected, $, journal, meta.randomGameName(), () => Map(), NoSwitches)
        }
        else
            if (hrf.HRF.flag("fastsetup"))
                startSetup(meta.indistinguishableFactions.?(meta.factions.take(4)).|(meta.factions.take(10).combinations(4).$.shuffle(0)), false)
            else {
                history.pushState("/play/" + meta.name, () => {})

                if (delayMainMenu > 0)
                    setTimeout(delayMainMenu) {
                        metaMenu()
                    }
                else {
                    metaMenu()
                }
            }
    }

    def metaMenu() {
        val title = meta.label.hl.spn(xstyles.larger110)(ExternalStyle(meta.titleFont.|("")))

        def goQuickGame() {
            history.pushState("/play/" + meta.name + "/quick", () => metaMenu())
            quickGame()
        }

        def goHotseat() {
            history.pushState("/play/" + meta.name + "/hotseat", () => metaMenu())
            customGame(false)
        }

        def goOnline() {
            history.pushState("/play/" + meta.name + "/online", () => metaMenu())
            onlineGame()
        }

        ui.action.asker.zask(
            (
                ZBasic(title, "Quick".hh, meta.factions.%(meta.getBots(_).intersect(meta.defaultBots).any).any.??(() => goQuickGame())) ::
                ZBasic(title, "Hotseat".hh, () => goHotseat()) ::
                ZBasic(title, "Online".hh, HRF.param("server").any.??(() => goOnline()))
            ) ++
            meta.intLinks./((t, l) => ZOption(Div("Other"), Link.internal(l + HRF.versionOverride./("?version=" + _).|(""), Div(t), ZBasic.choice ++ $(xstyles.hiddenLink)))) ++
            meta.extLinks./((t, l) => ZOption(Div("External Links"), Link(l, t.div(xstyles.divint) ~ Image("external-link")(xstyles.explain)(xstyles.clickThrough), ZBasic.infoch ++ $(xstyles.link)))) ++
            $(ZBasic(" ", Div("Settings"), () => miscellaneous()))
        )

        if (HRF.segments.startsWith($("quick")))
            goQuickGame()
        else
        if (HRF.segments.startsWith($("hotseat")))
            goHotseat()
        else
        if (HRF.segments.startsWith($("online")))
            goOnline()
        else
            HRF.segments = $
    }

    def miscellaneous() {
        val versions = $("0.8.105", "0.8.103")

        ui.action.asker.zask(
            versions./(v => ZBasic("Previous Stable Versions", "HRF " + v, () => {
                val l = dom.document.location
                val search = ("version=" + v) +: l.search.drop(1).split('&').$.%(_.startsWith("version").not).but("")
                val url = l.origin + l.pathname + "?" + search.join("&") + l.hash
                ui.logger.alog(("Reload " + url).spn.div, _ => {})
                dom.document.location.assign(url)
            }, ZBasic.choice ++ $(xlo.fullwidth) ++ $(xstyles.link))) ++
            $(ZBasic("UI", "Interface", () => {
                editSettings {
                    miscellaneous()
                }
            }, ZBasic.choice ++ $(xlo.fullwidth))) ++
            $(ZBasic("Local Storage", "Clear All Data", () => {
                hrf.web.Local.clear()
                settings = $
                applySettings(settings)
                miscellaneous()
            }, ZBasic.choice ++ $(xlo.fullwidth))) ++
            $(ZBasic("  ", Div("About"), () => aboutMenu())) ++
            $(ZBasic(" ", "Done".hh, () => {
                metaMenu()
            }, ZBasic.choice ++ $(xlo.fullwidth)))
        )
    }

    def aboutMenu() {
        ui.action.asker.zask(
            $(ZOption("", Link("https://github.com/haunt-roll-fail/haunt-roll-fail", "Source Code".spn.div, ZBasic.choice ++ $(xstyles.link)))) ++
            $(ZOption("", Link("https://boardgamegeek.com/user/hauntrollfail", "Contact".spn.div, ZBasic.choice ++ $(xstyles.link)))) ++
            $(ZOption("", OnClick(Div("History", ZBasic.choice ++ $(xstyles.link))), _ => showOverlayRaw(meta.label.styled(xstyles.bright)(xstyles.larger125), meta.about)).copy(clear = false)) ++
            $(ZBasic(" ", "Done".hh, () => {
                miscellaneous()
            }, ZBasic.choice ++ $(xlo.fullwidth)))
        )
    }

    def quickGame() {
        HRF.segments = $

        ui.action.asker.zask(meta.factions./(faction => ZBasic(meta.factionGroup(faction).|("Play as".txt), meta.factionElem(faction).spn(xstyles.bold) ~ meta.factionNote(faction), () => {
            ui.action.scroll.scrollTop = 0

            val ff = meta.factions.but(faction).%(meta.getBots(_).intersect(meta.defaultBots).any)
            def randomOpponents = meta.quickFactions.but(faction).shuffle.take(meta.quickMin.to(meta.quickMax).$.shuffle.head - 1)

            var factions = (faction +: randomOpponents).shuffle

            while (meta.validateFactionCombination(factions).ok.not && random() > 0.0001) {
                factions = (faction +: randomOpponents).shuffle

                while (meta.validateFactionCombination(factions).ok.not && random() > 0.0001) {
                    factions = factions.shuffle
                }
            }

            def askAdd() {
                val options = OptionsState(meta.optionsFor(factions.num, factions), $, meta.defaultsFor(factions.num, factions)).checkDimmed().selected

                while (meta.validateFactionCombination(factions).ok.not && random() > 0.0001) {
                    factions = factions.shuffle
                }

                val v = meta.validateFactionSeatingOptions(factions, options)

                ui.action.asker.zask(
                    $(ZBasic("Play as", meta.factionElem(faction) ~ meta.factionNote(faction))) ++
                    factions.but(faction)./(o => ZBasic("Against", meta.factionElem(o).spn(xstyles.bold) ~ meta.factionNote(o), () => {
                        factions :-= o
                        askAdd()
                    }, ZBasic.infoch)) ++
                    ZBasic("", v.ok.?("Start Game".hl ~ v.message.any.??(" | ")).|(Empty) ~ v.message.styled(v.style), v.ok.??(() => {
                        val seating = factions

                        var setup = Setup(seating, seating./(_ -> Bot(meta.defaultBots(0))).toMap)
                        setup = setup.copy(difficulty = setup.difficulty + (faction -> Human))

                        val journal = new MemoryJournal[meta.gaming.ExternalAction](meta)

                        startGame(setup, options, $, journal, meta.randomGameName(), () => Map(), NoSwitches)
                    })).? ++
                    ff.diff(factions)./(f => ZOption(Div(meta.factionGroup(f).|("Factions".txt)), OnClick(Div(Text(meta.factionName(f)), ZBasic.infoch)), _ => {
                        factions :+= f
                        askAdd()
                    })) ++
                    ZBasic(" ", "Cancel", () => {
                        history.popState()
                    }).?
                )
            }

            askAdd()
        })) :+ ZBasic(" ", "Cancel", () => {
            history.popState()
        }))
    }

    def onlineGame() {
        val prefix = meta.settingsKey + ".online.game."
        val l = Local.list(prefix)./(_.drop(prefix.length).split('.'))./(l => l(0) -> l(1).toDouble).sortBy(-_._2)
        val games = l./{ case (lobby, time) => HRF.parseOnlineGame(lobby, time, Local.get(prefix + lobby + "." + time, "")) }

        def goOnlineNew() {
            history.pushState("/play/" + meta.name + "/online/new", () => onlineGame())
            customGame(true)
        }

        def goOnlineArchive() {
            history.pushState("/play/" + meta.name + "/online/archive", () => onlineGame())
            onlineGameArchive()
        }

        def goOnlineLobby(lobby : String) {
            history.pushState("/play/" + meta.name + "/online/lobby/" + lobby, () => onlineGame())
            val g = games.%(_.lobby == lobby)
            if (g.any)
                showLinksMenu(g.shuffle(0))
            else {
                HRF.segments = $

                ui.logger.alog("Lobby info not found locally".spn.div(xstyles.error))
            }
        }

        ui.action.asker.zask(
            ZBasic(meta.label.hl, "New Online Game".hl.styled(xstyles.larger110), () => {
                goOnlineNew()
            }).?.$ ++
            games.%(_.status.has("archived").not)./(g => ZBasic("Recent Games", (new scalajs.js.Date(g.time).asInstanceOf[Dynamic].toLocaleString("sv-SE").toString.spn(xstyles.smaller75) ~ Break ~ g.title.hh).div($(xlo.fullwidth, xlo.fullheight)), () => {
                goOnlineLobby(g.lobby)
            })) ++
            ZBasic("  ", "Archived Games".hh, () => {
                goOnlineArchive()
            }).?.$.%(_ => games.exists(_.status.has("archived"))) ++
            ZBasic(" ", "Back", () => {
                history.popState()
            }).?
        )

        if (HRF.segments.startsWith($("online", "new")))
            goOnlineNew()
        else
        if (HRF.segments.startsWith($("online", "archive")))
            goOnlineArchive()
        else
        if (HRF.segments.startsWith($("online", "lobby")) && HRF.segments.num >= 3)
            goOnlineLobby(HRF.segments(2))
        else
            HRF.segments = $

        HRF.segments = $
    }

    def onlineGameArchive() {
        val prefix = meta.settingsKey + ".online.game."
        val l = Local.list(prefix)./(_.drop(prefix.length).split('.'))./(l => l(0) -> l(1).toDouble).sortBy(-_._2)
        val games = l./{ case (lobby, time) => HRF.parseOnlineGame(lobby, time, Local.get(prefix + lobby + "." + time, "")) }

        ui.action.asker.zask(
            games.%(_.status.has("archived"))./(g => ZBasic("Archived Games", (new scalajs.js.Date(g.time).asInstanceOf[Dynamic].toLocaleString("sv-SE").toString.spn(xstyles.smaller75) ~ Break ~ g.title.hh).div($(xlo.fullwidth, xlo.fullheight)), () => {
                showLinksMenu(g)
            })) ++
            ZBasic(" ", "Back", () => {
                history.popState()
            }).?
        )
    }

    def customGame(online : Boolean) {
        HRF.segments = $

        val ff = meta.factions

        if (meta.gradualFactions) {
            ui.action.asker.zask(
                ff.drop(meta.minPlayers - 1)./(f => ZOption(meta.label.hl, OnClick(Div((ff.indexOf(f) + 1).hlb(xstyles.larger125) ~ " Players", ZBasic.choice)), _ => {
                    val n = ff.indexOf(f) + 1
                    startSetup(meta.indistinguishableFactions.?(ff.take(n)).|(ff.combinations(n).$.%(c => meta.validateFactionCombination(c).is[ErrorResult].not).shuffle.sortBy(c => meta.validateFactionCombination(c).is[WarningResult]).apply(0)), online)
                })) ++
                ZBasic(" ", "Cancel", () => {
                    history.popState()
                }).?
            )
        }
        else {
            var opponents = $[meta.F]()

            def askAdd() {
                val t = "Play " ~ meta.label.hl ~ " " ~ (online).?("Online").|("Hotseat")
                val v = meta.validateFactionCombination(opponents)

                ui.action.asker.zask(
                    $(ZOption(t, Empty)) ++
                    opponents./(f => ZBasic(t, meta.factionElem(f).spn(xstyles.bold) ~ meta.factionNote(f), () => {
                        opponents :-= f
                        askAdd()
                    }, ZBasic.infoch ++ $(xstyles.optionOn))) ++
                    ZBasic(t, v.ok.?("Start Setup".hl ~ v.message.any.??(" | ")).|(Empty) ~ v.message.styled(v.style), (v.ok).??(() => {
                        startSetup(opponents, online)
                    })).? ++
                    ff.diff(opponents)./(f => ZOption(Div(meta.factionGroup(f).|("Play as".txt)), OnClick(Div(meta.factionElem(f) ~ meta.factionNote(f), ZBasic.choice)), _ => {
                        opponents :+= f
                        askAdd()
                    })) ++
                    ZBasic(" ", "Cancel", () => {
                        history.popState()
                    }).?
                )
            }

            askAdd()
        }
    }

    def startSetup(factions : $[meta.F], online : Boolean) {
        val optionsSaveKey = meta.name + "." + online.?("online").|("offline") + ".options"
        val saved = hrf.web.Local.get(optionsSaveKey, "").split(' ').$./~(_.some)
        val provided = HRF.paramList("options")
        val preset = provided.some.|(saved)./~(meta.parseOption(_)).intersect(meta.options)
        val matching = meta.optionsFor(factions.num, factions)
        val unneeded = preset.diff(matching)
        val needed = preset.intersect(matching)

        val defaults = meta.defaultsFor(factions.num, factions)
        var options = OptionsState(meta.optionsFor(factions.num, factions), $, needed.some./(_ ++ defaults.%(_.toggle.not)).|(defaults)).checkDimmed()

        val pages = {
            val pages = meta.optionPages(factions.num, factions)./(_.intersect(options.all))

            pages.take(1) ++ pages.drop(1).%(_.any)
        }

        val presets = meta.presetsFor(factions.num, factions)./{ case (title, included, excluded) =>
            val options = OptionsState(meta.optionsFor(factions.num, factions), included ++ meta.defaultsFor(factions.num, factions).diff(excluded), $).checkDimmed()
            (title, included, excluded, options)
        }

        var setup = Setup(factions, factions.map(_ -> Human).toMap)
        var notes = Map[meta.F, String]()

        def setupQuestions(page : Int) {
            val v = meta.validateFactionSeatingOptions(setup.seating, options.selected)
            val maxname = factions./(meta.factionName(_).length).max

            def padding(f : meta.F) = "".padTo(maxname - meta.factionName(f).length, ' ').pre.spn(xstyles.compressed)

            val vm = v.message.styled(v.style)

            ui.action.asker.zask(
                (page == 0).?? {
                    setup.seating./(f => ZOption("Setup Factions".styled(xstyles.larger125), Div(Empty ~ online.?(Input(notes.get(f).|(""), "Player #" + (setup.seating.indexOf(f) + 1), s => notes += f -> s.sanitize(32), 9, 16, ZBasic.inputT, ZBasic.inputD)) ~ " " ~ (padding(f) ~ meta.factionElem(f) ~ padding(f)).div(xstyles.width18ch) ~ " " ~
                        Parameter("difficulty", OnClick(Span((setup.difficulty(f) match {
                            case Human  => " Human ".pre.hl
                            case Bot(s) => " Bot / ".pre ~ (s + " ").pre.hl
                        }).div(xstyles.width14ex), xstyles.outlined))) ~
                        Div(
                            (setup.seating.head == f).?("   ".pre.spn(xstyles.larger125)).|(OnClick("up",   Span(" ▲ ".pre.spn(xstyles.larger125), xstyles.outlined))) ~
                            (setup.seating.last == f).?("   ".pre.spn(xstyles.larger125)).|(OnClick("down", Span(" ▼ ".pre.spn(xstyles.larger125), xstyles.outlined))),
                        xstyles.updown)
                        , ZBasic.choice :+ xstyles.player), {
                        case "difficulty" =>
                            setup = setup.copy(difficulty = setup.difficulty + (f -> ($(Human) ++ meta.getBots(f)./(Bot) ++ $(Human)).dropWhile(_ != setup.difficulty(f)).drop(1).head))
                            setupQuestions(page)
                        case "up" =>
                            val a = setup.seating.takeWhile(_ != f)
                            val b = setup.seating.dropWhile(_ != f)
                            setup = setup.copy(seating = a.dropRight(1) ++ b.take(1) ++ a.takeRight(1) ++ b.drop(1))
                            setupQuestions(page)
                        case "down" =>
                            val a = setup.seating.takeWhile(_ != f)
                            val b = setup.seating.dropWhile(_ != f)
                            setup = setup.copy(seating = a ++ b.drop(1).take(1) ++ b.take(1) ++ b.drop(2))
                            setupQuestions(page)
                    })) ++
                    presets./{ case (title, included, excluded, neu) =>
                        val matches = meta.options.intersect(neu.selected) == meta.options.intersect(options.selected)
                        ZOption("Presets".styled(xstyles.larger125), OnClick(Div(title, ZBasic.choice ++ matches.$(xstyles.optionOn, xstyles.bold) ++ $(xstyles.optionE))), _ => {
                            options = neu
                            setupQuestions(page)
                        })
                    }
                } ++
                meta.optionsFor(setup.seating.num, setup.seating).diff(meta.hiddenOptions).intersect(pages(page))./({ o =>
                    val state = options.selected.has(o)
                    val enabled = options.enabled(o)
                    val active = enabled
                    ZOption(o.group.styled(xstyles.larger125),
                        OnClick(Div(state.?(o.decorate(o.valueOn).styled(xstyles.bold)).|(o.decorate(o.valueOff))
                        ~ (o.explain.any).?(Parameter(o, OnClick(Image("question-mark")(xstyles.explain)))), (active || state).?(ZBasic.choice).|(ZBasic.info) ++ state.$(xstyles.optionOn) ++ $(xstyles.optionE))), {
                            case o : GameOption if o.explain.any =>
                                showOverlay(o.explain./(ZBasic(o.group ~ " " ~ MDash ~ " " ~ o.valueOn, _)), o.grow)
                                setTimeout(0) { setupQuestions(page) }
                            case Nil =>
                                if (active || state) {
                                    if (active) {
                                        val next = options.click(o)
                                        options = next

                                        hrf.web.Local.set(optionsSaveKey, (next.selected ++ next.dimmed ++ unneeded)./(meta.writeOption).join(" "))
                                    }
                                }
                                setupQuestions(page)
                        }
                    )
                }) ++
                (page == 0).$(
                    ZOption(vm, "Cancel".txt.div(ZBasic.choice)(xstyles.halfbutton).onClick, _ => {
                        ui.action.scroll.scrollTop = 0
                        history.popState()
                    })
                ) ++
                (page > 0).$(
                    ZOption(vm, "Back".txt.div(ZBasic.choice)(xstyles.halfbutton).onClick, _ => {
                        ui.action.scroll.scrollTop = 0
                        history.popState()
                    })
                ) ++
                v.ok.??(
                    (page == pages.length - 1).$(
                        ZOption(vm, "Start".hlb.div(ZBasic.choice)(xstyles.halfbutton).onClick, _ => {
                            history.destroy(pages.length - 1)

                            if (online)
                                arrangeOnlineGame(setup, options.selected, Map(), notes, false, meta.randomGameName(), HRF.now(), $(meta.start), None, g => {})
                            else {
                                val journal = new MemoryJournal[meta.gaming.ExternalAction](meta)

                                startGame(setup, options.selected, $, journal, meta.randomGameName(), () => Map(), NoSwitches)
                            }
                        })
                    ) ++
                    (page < pages.length - 1).$(
                        ZOption(vm, "Next".hh.div(ZBasic.choice)(xstyles.halfbutton).onClick, _ => {
                            ui.action.scroll.scrollTop = 0
                            history.pushState(null, () => setupQuestions(page))
                            setupQuestions(page + 1)
                        })
                    )
                ) ++
                v.ok.not.??(
                    (page == pages.length - 1).$(
                        ZOption(vm, "Start".hlb.div(ZBasic.info)(xstyles.halfbutton))
                    ) ++
                    (page < pages.length - 1).$(
                        ZOption(vm, "Next".hh.div(ZBasic.info)(xstyles.halfbutton))
                    )
                ) ++
                $

            )
        }

        setupQuestions(0)

        ui.action.scroll.scrollTop = 0
    }

    def startGame(setup : Setup, options : $[meta.O], self : $[meta.F], journal : Journal[meta.gaming.ExternalAction], title : String, names : () => Map[meta.F, String], swt : Switches) {
        val delay = HRF.paramInt("delay") | 30

        val seating = setup.seating.%(f => setup.difficulty(f) != Off)

        val assets = meta.assets.%(_.condition(seating, options))./~(_.get)

        val sources = assets./(a => a.name -> ("webp2/" + meta.path + "/images/" + a.copy(ext = "webp").src)).toMap
        val preload = HRF.embedded.?(assets./(a => a.name -> a.name)).|(assets.%(_.lzy == Laziness.Immediate)./(a => a.name -> sources(a.name)))
        val loader = HRF.embedded.?(new WrappedEmbeddedImageLoader(s => "asset-" + s)).|(HRF.imageCache)

        ui.logger.alog("Loading assets".spn.div)

        var i = 0

        loader.onLoad = (url, result) => {
            i += 1

            if (i % (preload.num / 23 + 1) == 0)
                ui.logger.alog("*".spn)
        }

        loader.wait(preload.rights.take(1*1*1*1*1*1+111111111)) {
            val loaded = preload./((key, url) => key -> loader.get(url)).toMap

            val resources = Resources(ImageResources(loaded, sources, HRF.imageCache), () => names().toMap)

            import meta.gaming._

            val main = this

            val callbacks = new Callbacks {
                def switches = swt
                def saveReplay(onSave : => Unit) = hrf.quine.Quine.save(meta)(title, seating, options, resources, journal, HRF.startAt, onSave)
                def saveReplayOnline(replaceBots : Boolean, mergeHumans : Boolean)(onSave : String => Unit) = {
                    journal.read(0) { actions =>
                        val factions = setup.seating.%(f => setup.difficulty(f) != Off)

                        val ss =
                            if (replaceBots)
                                Setup(factions, factions./(_ -> Human).toMap)
                            else
                                setup

                        val humans = factions.%(f => ss.difficulty(f) == Human)

                        arrangeOnlineGame(ss, options, Map(), humans./(_ -> "Player").toMap, mergeHumans, title, HRF.startAt, actions./(_.unwrap), |("SLG"), g => {
                            val l = dom.document.location
                            val url = "/play/" + meta.name + "/online/lobby/" + g.lobby + l.search + l.hash
                            println("reload " + url)
                            dom.document.location.assign(url)
                        })
                    }
                }
                def canPlayAgain = journal.is[MemoryJournal[_]]
                def playAgain() = {
                    ui.guir.clear()
                    startGame(setup : Setup, options : $[meta.O], self : $[meta.F], new MemoryJournal[meta.gaming.ExternalAction](meta), title : String, names : () => Map[meta.F, String], swt : Switches)
                }
                def editSettings(onEdit : => Unit) = {
                    ui.uir.show()
                    main.editSettings {
                        ui.uir.hide()
                        onEdit
                    }
                }
                def settings = main.settings
            }

            val gui = HRF.metaUIs.toMap.apply(meta).asInstanceOf[BaseUI { val mmeta : meta.type }]

            val renderer = gui.create(ui.guir, seating.num, options, resources, title, callbacks)

            ui.uir.hide()

            applySettings(settings)

            def ask(game : G, faction : meta.F) : AskResult = {
                if (faction == null)
                    AskHuman
                else
                setup.difficulty(faction) match {
                    case Human if journal.is[ServerJournal[_]].not =>
                        AskHuman
                    case Human if self.contains(faction) =>
                        AskHuman
                    case Human =>
                        WaitRemote
                    case BotDebug(botname) =>
                        DebugBot((actions : $[UserAction]) => {
                            val aa = game.explode(actions, false, None)
                            val bot = meta.getBot(faction, botname)
                            bot.eval(aa)(game).sortWith(bot.compare)
                        })
                    case Bot(botname) =>
                        AskBot((actions : $[UserAction]) => meta.getBot(faction, botname).ask(game, actions, 0.01))
                }
            }

            import meta.tagF

            Runner.run(meta)(seating, options, resources, delay, renderer, (g, f) => f.as[meta.F]./(f => ask(g, f)).|!("unsuitable ask"), journal)
        }

    }

    def arrangeOnlineGame(setup : Setup, options : $[meta.O], names : Map[meta.F, String], notes : Map[meta.F, String], merge : Boolean, title : String, time : scalajs.js.Date, actions : $[meta.gaming.Action], comment : |[String], onCreate : OnlineGame => Unit) {
        val server = HRF.param("server").get

        val factions = setup.seating.%(f => setup.difficulty(f) != Off)

        val humans = factions.%(f => setup.difficulty(f) == Human)
        val bots = factions.%(f => setup.difficulty(f) != Human)

        var plays = Map[String, String]()
        var users = Map[meta.F, String]()
        var spectate = ""
        val version = HRF.versionOverride./("?version=" + _).|("")

        post(server + "/new-user", title + " HOST") { mm =>
            val master = mm.split('\n').join("/")

            post(server + "/new-journal/" + master, title + " LOBBY") { lobby =>
                post(server + "/new-journal/" + master, title) { game =>
                    def writeLobby() {
                        var intro = $("meta " + meta.name, "version " + HRF.version, "title " + title) ++
                            humans./(f => "user " + meta.writeFaction(f) + " " + users(f)) ++
                            humans./~(f => notes.get(f)./(n => "prename " + users(f) + " " + n)) ++
                            humans./~(f => names.get(f)./(n => "name " + users(f) + " " + n)) ++
                            bots./(f => "bot " + meta.writeFaction(f) + " " + setup.difficulty(f).asInstanceOf[Bot].name) ++
                            comment./(c => "comment " + c).$ ++
                            $(
                                "seating " + factions./(meta.writeFaction).join(" "),
                                "options " + options./(meta.writeOption).join(" "),
                                "server " + game
                            )

                        post(server + "/append/" + master + "/" + lobby + "/0", intro.join("\n")) { _ =>
                            post(server + "/append/" + master + "/" + game + "/0", actions./(meta.writeAction).join(comment.?(" // " + _) + "\n") + comment.?(" // " + _)) { _ =>
                                val og = OnlineGame(time.getTime(), HRF.version, meta.name, lobby, title, options.of[ImportantOption]./(meta.writeOption), humans./(f => PlayerLink(meta.writeFaction(f), plays(users(f)), notes.get(f).|(""))), plays(spectate), $)

                                showLinksMenu(og)

                                Local.set(meta.settingsKey + ".online.game." + og.lobby + "." + og.time, HRF.writeOnlineGame(og))

                                onCreate(og)
                            }
                        }
                    }

                    post(server + "/new-play/" + master + "/" + lobby, "Spectator") { sp =>
                        spectate = sp.split('\n')(0)
                        plays += spectate -> sp.split('\n')(1)
                        post(server + "/grant-read/" + master + "/" + game + "/" + spectate, "") { _ =>
                            var n = humans.num
                            humans.take(merge.?(1).|(n)).foreach { f =>
                                post(server + "/new-play/" + master + "/" + lobby, meta.factionName(f)) { sc =>
                                    users += f -> sc.split('\n')(0)
                                    plays += users(f) -> sc.split('\n')(1)
                                    post(server + "/grant-read-append/" + master + "/" + game + "/" + users(f), "") { _ =>
                                        n -= 1
                                        if (merge) {
                                            humans.drop(1).foreach { f =>
                                                users += f -> sc.split('\n')(0)
                                                plays += users(f) -> sc.split('\n')(1)
                                            }
                                            writeLobby()
                                        }
                                        else
                                        if (n == 0)
                                            writeLobby()
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    def showLinksMenu(og : OnlineGame) {
        HRF.segments = $

        val timestamp = new scalajs.js.Date(og.time).asInstanceOf[Dynamic].toLocaleString("sv-SE").toString

        val meta = HRF.metas.%(_.name == og.meta).single.|(this.meta)

        def url(key : String) = (HRF.param("server").get + "/play/" + meta.name + "/" + key + (og.version != HRF.version || HRF.versionOverride.any).??("?version=" + og.version))

        var ca : Elem = "Copy all links".hl

        def linkMenu() {
            ui.action.asker.zask(
                $(ZBasic((meta.label.hl ~ " Online Game").spn(xstyles.larger110), Div(timestamp.spn(xstyles.smaller75) ~ Break ~ og.title.hl ~ og.options./~(meta.parseOption)./(o => Break ~ o.group ~ " " ~ o.valueOn).merge, $(xlo.fullwidth, xlo.fullheight)))) ++
                og.spectate.some./(s => ZOption("Spectator Link", (Link(url(s), "Spectate".txt, $(xstyles.link, xlo.fullwidth, xlo.fullheight)) ~ Image("external-link")(xstyles.explain)(xstyles.clickThrough)).div(ZBasic.choice))) ++
                og.links./(l => ZOption("Player Links", (Link(url(l.key), l.note.some./(_.hl).|("Play".txt) ~ " as ".spn(xstyles.normal) ~ meta.parseFaction(l.faction)./(meta.factionElem).|(l.faction.txt), $(xstyles.link, xlo.fullwidth, xlo.fullheight)) ~ Image("external-link")(xstyles.explain)(xstyles.clickThrough)).div(ZBasic.choice))) ++
                $(ZBasic("", ca, () => {
                    ca = clipboard(
                        ($(meta.label) ++
                            $(og.title) ++
                            og.options ++
                            og.spectate.some./(s => "Spectate " + url(s)) ++
                            og.links./(l => l.note.some./(_ + " ").|("") + l.faction + " " + url(l.key))
                        ).join("\n")
                    ).?("Copied links to clipboard").|("Error copying to clipboard").hl

                    linkMenu()
                })).%(_ => og.links.num > 1) ++
                og.status.has("archived").$(ZBasic("   ", "Move to " ~ "Recent".hh, () => {
                    Local.set(meta.settingsKey + ".online.game." + og.lobby + "." + og.time, HRF.writeOnlineGame(og.copy(status = og.status.but("archived"))))
                    history.popState()
                })) ++
                og.status.has("archived").not.$(ZBasic("  ", "Move to " ~ "Archive".hh, () => {
                    Local.set(meta.settingsKey + ".online.game." + og.lobby + "." + og.time, HRF.writeOnlineGame(og.copy(status = og.status :+ "archived")))
                    history.popState()
                })) ++
                $(ZBasic(" ", "Back", () => {
                    history.popState()
                }))
            )
        }

        linkMenu()
    }

    def showOverlay(l : $[ZOption], grow : Style) {
        ui.overlay.pane.vis()

        ui.overlay.asker.zask(l ++
            $(ZOption(Div(Empty, grow), Empty)) ++
            $(ZBasic(Empty, "Ok", () => {
                ui.overlay.pane.invis()
            }))
        )
    }

    def showOverlayRaw(title : Elem, l : $[Elem]) {
        ui.overlay.pane.vis()

        ui.overlay.asker.zask(
            $(ZBasic(title, l./(e => Div(e)).merge.div(xlo.flexvcenter))) ++
            $(ZBasic(Empty, "Ok", () => {
                ui.overlay.pane.invis()
            }))
        )
    }

}
