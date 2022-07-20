package hrf
//
//
//
//
import logger._, colmat._
//
//
//
//

import scala.scalajs.js.timers.setTimeout

import org.scalajs.dom

import scala.collection.mutable

import hrf.elem._
import hrf.html._
import hrf.meta._
import hrf.web._
import hrf.base._
import hrf.ui._
import hrf.loader._



trait Journal[T] {
    val meta : MetaGame

    def read(n : Int)(then : List[T] => Unit)
    def append(n : Int, m : T)(then : => Unit)(fail : => Unit)
}


class ServerJournal[T](val meta : MetaGame, server : String, user : String, secret : String, journal : String, parse : String => T, write : T => String, readlimit : Int = 999999) extends Journal[T] {
    def read(n : Int)(then : List[T] => Unit) {
        get(server + "/read/" + user + "/" + secret + "/" + journal + "/" + n) { l => then(l.split('\n').toList.but("").take(readlimit - n)./(parse)) }
    }
    
    def append(n : Int, m : T)(then : => Unit)(fail : => Unit) {
        postF(server + "/append/" + user + "/" + secret + "/" + journal + "/" + n, write(m))(_ => then)(fail)
    }
}

class MemoryJournal[T](val meta : MetaGame) extends Journal[T] {
    var actions = List[T]()

    def read(n : Int)(then : List[T] => Unit) {
        then(actions.drop(n))
    }
    
    def append(n : Int, m : T)(then : => Unit)(fail : => Unit) {
        if (actions.num == n) {
            actions :+= m
            then
        }
        else
            fail
    }
}

class ReplayJournal[T](val meta : MetaGame, data : String, parse : String => T, readlimit : Int = 999999) extends Journal[T] {
    ===("ReplayJournal")

    ===("raw " + data.split('\n').toList./(_.trim).but(""))

    val actions = data.split('\n').toList./(_.trim).but("")./(parse)

    ===(actions)

    def read(n : Int)(then : List[T] => Unit) {
        then(actions.drop(n).take(readlimit - n))
    }
    
    def append(n : Int, m : T)(then : => Unit)(fail : => Unit) {
        fail
    }
}

class XJournal[T](journal : Journal[T]) {
    private var list = List[T]()
    private var working = false
    
    def content = list
    
    def read(then : => Unit) {
        if (working)
            throw new Error("read while working")

        working = true

        journal.read(list.num) { l =>
            working = false

            list ++= l

            then
        }
    }
 
    def append(x : T)(then : => Unit)(fail : => Unit) {
        if (working)
            throw new Error("append while working")

        working = true
        
        journal.append(list.num, x) {
            working = false

            then
        } {
            working = false

            read(fail)
        }
    }
}

object HRF {
    val date = new scala.scalajs.js.Date()

    def uptime() = (new scala.scalajs.js.Date().getTime() - date.getTime()).toInt
    
    val html = dom.window.location.href
    val hash = dom.window.location.hash.substring(1)

    val defaultGlyph = getElem("icon").asInstanceOf[dom.html.Link].href

    def glyph(s : String) = getElem("icon").asInstanceOf[dom.html.Link].href = s
    
    val settings = Option(getElem("settings"))

    private val params = mutable.Map[String, Option[String]]()

    def param(p : String) = params.getOrElseUpdate(p, settings./~(_.getAttribute("data-" + p).?).but("").||(hash.split('|').map(_.split('=')).filter(_(0) == p).map(_.drop(1).mkString("=")).toList.single).but(""))
    
    def flag(p : String) = param(p).any

    val script = dom.document.getElementById("script").asInstanceOf[dom.html.Script].src
    
    val offline = flag("offline")

    if (!offline) {
        StringLoader.queue(html)
        StringLoader.queue(script)
    }

    def main(args : Array[String]) {
        if (dom.document.readyState == "complete")
            new HRFSoloUI()
        else
            dom.window.onload = (e) => new HRFSoloUI()
    }
}

class HRFSoloUI {
    import HRF._

    val umetas : List[(MetaGame, BaseUI)] = 
        root.Meta -> root.UI ::
        root.MetaAdset -> root.UI ::
        cthw.Meta -> cthw.UI ::
        dwam.Meta -> dwam.UI ::
        sehi.Meta -> sehi.UI ::
        coup.Meta -> coup.UI ::
        suok.Meta -> suok.UI ::
        yarg.Meta -> yarg.UI ::
    Nil

    val metas = umetas./(_._1)

    implicit val zr = Resources(Map(), () => Map())

    StyleRegister.add(List(xstyles.pane))
    StyleRegister.add(List(xstyles.outer))
    StyleRegister.add(List(xstyles.inner, xstyles.pane.log))
    StyleRegister.add(List(xstyles.inner, xstyles.pane.action, xstyles.unselectable))
    StyleRegister.add(List(xstyles.choice, xstyles.xx))
    StyleRegister.add(List(xstyles.choice, xstyles.pointer, xstyles.xx))
    StyleRegister.add(List(xstyles.choice, xstyles.chm, xstyles.chp, xstyles.thu, xstyles.xx, xstyles.pointer))
 
    val uir = new ElementAttachmentPoint(getElem("root-attachment-point"))
    

    val logger = new NewLogger(new ElementAttachmentPoint(getElem("log")), flag("scroll"))
    val asker = new Asker(getElem("action"), Map())
    
    val version = Info.name + " " + Info.version
    
    logger.zlog(Empty ~ version)

    asker.zask(metas./(m => ZBasic("", m.label)))

    setTimeout(400) { param("meta")./~(mn => metas.%(_.name == mn).single)./(withMeta).|(topMenu()) }

    def topMenu() {
        asker.zask(metas./(m => ZBasic("", m.label, () => withMeta(m))))
    }        

    def withMeta(meta : MetaGame) {
        dom.document.title = meta.label

        if (param("lobby").any || flag("replay")) {
            val (user, lj) = if (flag("replay")) {
                ("", new XJournal(new ReplayJournal[String](meta, getElem("lobby").textContent, identity)))
            }
            else {
                val server = param("server").get
                val lobby = param("lobby").get
                val user = param("user").get
                val secret = param("secret").get
        
                (user, new XJournal(new ServerJournal[String](meta, server, user, secret, lobby, identity, identity)))
            }

            var name = getCookie("name", "")
            var names = Map[String, String]()
            var users = Map[meta.F, String]()

            var setup : meta.Setup = null
            
            def getName(f : meta.F) = users.get(f)./~(names.get(_))

            def allNames = users.flatMap {
                case (f, i) => names.get(i)./(f -> _)
            }

            def readLobby() {
                def reread() { lj.read(readLobby()) }

                var bots = Map[meta.F, String]()
                var seating = List[meta.F]()
                var options = meta.optimizeOptions(Nil, Nil)
                var server : Option[String] = None
                var title : Option[String] = None

                users = Map()
                names = Map()

                lj.content.foreach { line =>
                    val l = line.split(' ').toList
                    l(0) match {
                        case "user" => 
                            users += meta.parseFaction(l(1)).get -> l(2)
                        case "bot" => 
                            bots += meta.parseFaction(l(1)).get -> l(2)
                        case "seating" => 
                            seating = l.drop(1)./(f => meta.parseFaction(f).get)
                        case "options" => 
                            options = meta.optimizeOptions(l.drop(1)./(f => meta.parseOption(f).get), seating)
                        case "server" => 
                            server = Some(l(1))
                        case "title" => 
                            title = Some(l.drop(1).mkString(" ").sanitize(32))
                        case "name" => 
                            names += l(1) -> (l.drop(2).mkString(" ").sanitize(32))
                        case _ =>
                    }
                }

                val self = users.keys.%(f => users(f) == user).toList
                
                self match {
                    case List(f) =>
                        val assetsources = meta.assets.%(_.condition(seating, options))./~(_.get)./(a => a.name -> (meta.path + "/images/" + a.src)).toMap
                        
                        meta.glyph(f).foreach { s =>
                            HRF.glyph(assetsources(s))
                        }

                    case _ =>
                }

                if (self.any && names.contains(user).not) {
                    def randomName() {
                        val ad = List("Ardent", "Adventurous", "Brave", "Clever", "Cunning", "Caring", "Daring", "Diligent", "Eager", "Fair", "Fearless", "Good", "Honorable", "Ingenious", "Jolly", "Kind", "Loyal", "Merciful", "Noble", "Observant", "Proud", "Patient", "Quick", "Righteous", "Resolute", "Selfless", "Tough", "Unyielding", "Vigorous", "Valiant", "Wholesome", "Xenial", "Yearning", "Zealous")
                        val an = List("Armadillo", "Badger", "Bear", "Beaver", "Bat", "Cat", "Cow", "Dog", "Deer", "Eagle", "Elephant", "Fox", "Giraffe", "Horse", "Iguana", "Jellyfish", "Kangaroo", "Lion", "Llama", "Lemur", "Monkey", "Mouse", "Newt", "Otter", "Octopus", "Panda", "Quail", "Raccoon", "Sheep", "Shark", "Sloth", "Snake", "Scorpion", "Turtle", "Tiger", "Urchin", "Vulture", "Wolf", "Zebra")
                        name = ad.shuffle.head + " " + an.shuffle.head
                        askName()
                    }
                    
                    def postName(value : String, retries : Int) {
                        name = value.split(' ').filter(_ != "").mkString(" ").sanitize(32)
                        if (name == value && retries < 3) {
                            setCookie("name", name, Some(365))
                            lj.append(List("name", user, name).mkString(" "))(reread())(postName(name, retries + 1))
                        }
                        else
                            askName()
                    }
 
                    def askName() {
                        asker.ask(InputOption("Name", name, Nil, v => postName(v, 0)) :: BasicOption("", "Random", Nil, () => randomName()))
                    }

                    askName()
                }
                else 
                if (setup == null) {
                    dom.document.title = self./(meta.factionName(_) + " - ").join("") + " - " + title.|("%untitled%") + " - " + meta.label
                    dom.document.title = (self./(meta.factionName) :+ title.|("%untitled%") :+ meta.label).join(" - ")
                
                    setup = meta.Setup(seating, Human)
                    setup = setup.copy(options = options)
                    setup = setup.copy(difficulty = setup.difficulty ++ bots./{ case (f, d) => f -> Bot(d) })
                    
                    startGame(meta)(setup)(self, server, title.|("%untitled%"), () => allNames)
                    
                    reread()
                }
                else {
                    setTimeout(15000) {
                        reread()
                    }
                }
            }

            lj.read(readLobby())
        }
        else {
            if (flag("quick").not)
                metaMenu(meta)
            else {
                val factions = param("factions")./~(_.split('+'))./(meta.parseFaction(_).get).some.|({
                    val included = param("include")./~(_.split('+'))./(meta.parseFaction(_).get)
                    val excluded = param("exclude")./~(_.split('+'))./(meta.parseFaction(_).get)
                    val total = meta.quickMin.to(meta.quickMax).toList.shuffle.first
                    included ++ meta.factions.diff(excluded).diff(included).shuffle.take(total).drop(included.num)
                })
                
                var setup = meta.Setup(factions, Bot(meta.defaultBots(0)))
                
                param("human")./~(_.split('+'))./(meta.parseFaction(_).get).some.|($(factions.shuffle.first)).foreach { h =>
                    setup = setup.copy(difficulty = setup.difficulty + (h -> flag("debug").?(BotDebug(meta.defaultBots(0))).|(Human)))
                }

                param("options")./~(_.split('+'))./(meta.parseOption(_).get).some.foreach { o =>
                    setup = setup.copy(options = meta.optimizeOptions(o, factions))
                }
                
                startGame(meta)(setup)(Nil, None, meta.randomGameName(), () => Map())
            }
        }           
                
    }

    def metaMenu(meta : MetaGame) {
        asker.zask((
            ZBasic(meta.label.hl, "Quick", meta.factions.%(meta.getBots(_).intersect(meta.defaultBots).any).any.??(() => quickGame(meta))) ::
            ZBasic(meta.label.hl, "Hotseat", () => customGame(meta)(false)) ::
            ZBasic(meta.label.hl, "Online", param("server").any.??(() => customGame(meta)(true)))) ++ 
            meta.links./(l => ZBasic("External Links", Link(l._2, Div(Text(l._1)), List(xstyles.link))))
        )
    }
    
    def quickGame(meta : MetaGame) {
        asker.zask(meta.factions./(faction => ZBasic("Play as", meta.factionElem(faction) ~ meta.factionNote(faction), () => {
            val ff = meta.factions.but(faction).%(meta.getBots(_).intersect(meta.defaultBots).any)
            var opponents = meta.quickFactions.but(faction).shuffle.take(meta.quickMin.to(meta.quickMax).toList.shuffle.first - 1)
            
            def askAdd() {
                val v = meta.validateFactionCombination(faction +: opponents)
                asker.zask(
                    $(ZBasic("Play as", Div(meta.factionElem(faction) ~ meta.factionNote(faction)))) ++
                    opponents./(f => ZBasic("Against", Div(meta.factionElem(f) ~ meta.factionNote(f)), () => {
                        opponents :-= f
                        askAdd()
                    })) ++
                    ZBasic("", v.ok.?("Start Game".hl ~ v.message.any.??(" | ")).|(Empty) ~ v.message.styled(v.style), v.ok.??(() => {
                        def allSeatings(factions : List[meta.F]) = factions.permutations.toList
                        def randomSeating(factions : List[meta.F]) = allSeatings(factions).shuffle.head
                        var setup = meta.Setup(randomSeating(faction +: opponents), Bot(meta.defaultBots(0)))
                        setup = setup.copy(difficulty = setup.difficulty + (faction -> Human))
                        startGame(meta)(setup)(Nil, None, meta.randomGameName(), () => Map())
                    })).? ++
                    ff.diff(opponents)./(f => ZOption(Div(Text("Factions")), OnClick(Div(Text(meta.factionName(f)), ZBasic.infoch)), _ => {
                        opponents :+= f
                        askAdd()
                    })) ++
                    ZBasic(" ", "Cancel", () => {
                        metaMenu(meta)
                    }).?
                )
            }
            
            askAdd()
            
        })) :+ ZBasic(" ", "Cancel", () => metaMenu(meta)))
    }
    
    def customGame(meta : MetaGame)(online : Boolean) {
        val ff = meta.factions

        if (meta.gradualFactions) {
            asker.zask(
                ff.drop(meta.minPlayers - 1)./(f => ZOption(Div(Text("Players")), OnClick(Div((ff.indexOf(f) + 1).hlb, ZBasic.choice)), _ => {
                    val n = ff.indexOf(f) + 1
                    startSetup(meta)(meta.indistinguishableFactions.?(ff.take(n)).|(ff.combinations(n).toList.shuffle(0)), online)
                })) ++
                ZBasic(" ", "Cancel", () => {
                    metaMenu(meta)
                }).?
            )
        }
        else {
            var opponents = List[meta.F]()
            
            def askAdd() {
                val t = Text("Play ") ~ meta.label.hl ~ " " ~ (online).?("Online").|("Hotseat")
                val v = meta.validateFactionCombination(opponents)
        
                asker.zask(
                    opponents./(f => ZBasic(t, meta.factionElem(f) ~ meta.factionNote(f), () => {
                        opponents :-= f
                        askAdd()
                    })) ++
                    ZBasic("", v.ok.?("Start Setup".hl ~ v.message.any.??(" | ")).|(Empty) ~ v.message.styled(v.style), (v.ok).??(() => {
                        startSetup(meta)(opponents, online)
                    })).? ++
                    ff.diff(opponents)./(f => ZOption(Div(Text("Factions")), OnClick(Div(Text(meta.factionName(f)), ZBasic.infoch)), _ => {
                        opponents :+= f
                        askAdd()
                    })) ++
                    ZBasic(" ", "Cancel", () => {
                        metaMenu(meta)
                    }).?
                )
            }
        
            askAdd()
        }
    }

    def startSetup(meta : MetaGame)(factions : List[meta.F], online : Boolean) {
        var setup = meta.Setup(factions, Human)
        var notes = Map[meta.F, String]()
        
        def setupQuestions() {
            val v = meta.validateFactionSeatingOptions(setup.seating, setup.options)
            val maxname = factions./(meta.factionName(_).length).max
            
            def padding(f : meta.F) = "".padTo(maxname - meta.factionName(f).length, ' ').pre.spn(xstyles.compressed)

            asker.zask(
                $(ZBasic(v.message.styled(v.style), "Start game".styled(xstyles.bright)(xstyles.larger125), v.ok.??(() => {
                    if (online)
                        arrangeOnlineGame(meta)(setup, notes)
                    else
                        startGame(meta)(setup)(Nil, None, meta.randomGameName(), () => Map())
                }))) ++
                setup.seating./(f => ZOption(Text("Setup Factions"), Div(Empty ~ online.?(Input(notes.get(f).|(""), s => notes += f -> s, 12, 16, ZBasic.inputT, ZBasic.inputD)) ~ " " ~ padding(f) ~ meta.factionElem(f) ~ padding(f) ~ " " ~ 
                    Parameter("difficulty", OnClick(Span((setup.difficulty(f) match {
                        case Human  => " Human ".pre.hl ~ "".padTo(7, ' ').pre
                        case Bot(s) => " Bot / ".pre ~ s.padTo(7, ' ').pre.hl 
                    }).spn(xstyles.larger125), xstyles.outlined))) ~ "   ".pre ~
                    Div( 
                        (setup.seating.head == f).?("   ".pre.spn(xstyles.larger125)).|(OnClick("up",   Span(" ▲ ".pre.spn(xstyles.larger125), xstyles.outlined))) ~ 
                        (setup.seating.last == f).?("   ".pre.spn(xstyles.larger125)).|(OnClick("down", Span(" ▼ ".pre.spn(xstyles.larger125), xstyles.outlined))),
                    xstyles.updown)
                    , ZBasic.choice), {
                    case "difficulty" =>
                        setup = setup.copy(difficulty = setup.difficulty + (f -> (List(Human) ++ meta.getBots(f)./(Bot) ++ List(Human)).dropWhile(_ != setup.difficulty(f)).drop(1).head))
                        setupQuestions()
                    case "up" =>
                        val a = setup.seating.takeWhile(_ != f)
                        val b = setup.seating.dropWhile(_ != f)
                        setup = setup.copy(seating = a.dropRight(1) ++ b.take(1) ++ a.takeRight(1) ++ b.drop(1))
                        setupQuestions()
                    case "down" =>
                        val a = setup.seating.takeWhile(_ != f)
                        val b = setup.seating.dropWhile(_ != f)
                        setup = setup.copy(seating = a ++ b.drop(1).take(1) ++ b.take(1) ++ b.drop(2))
                        setupQuestions()
                })) ++
                meta.optionsFor(setup.seating)./({ o =>
                    val state = setup.options.has(o)
                    val next = meta.optimizeOptions(state.?(setup.options.but(o)).|(setup.options :+ o), setup.seating)
                    val active = state.?(next.has(o).not).|(next.has(o)) || state
                    ZBasic(o.group, state.?(o.valueOn).|(o.valueOff), active.??(() => {
                        setup = setup.copy(options = next)
                        setupQuestions()
                    }))
                }) ++
                ZBasic(" ", "Cancel", () => {
                    metaMenu(meta)
                }).?
            )
        }
        
        setupQuestions()
        asker.top()
    }
    
    def startGame(meta : MetaGame)(setup : meta.Setup)(self : List[meta.F], journal : Option[String], title : String, names : () => Map[meta.F, String]) {
        val delay = param("delay")./~(_.toIntOption).|(30)
        
        val seating = setup.seating.%(f => setup.difficulty(f) != Off)
        val options = setup.options
        val assetsources = meta.assets.%(_.condition(seating, options))./~(_.get)./(a => a.name -> flag("offline").?(a.name).|(meta.path + "/images/" + a.src))
        val loader = flag("offline").?(new EmbeddedImageLoader(s => "asset-" + s)).|(ImageLoader)
        
        loader.wait(assetsources./(_._2)) {
            uir.clear()

            val assets = assetsources./(as => as._1 -> loader.get(as._2)).toMap
            
            if (flag("offline").not)            
                DataUrlLoader.queue(assets.values./(_.src).toList)
            
            val resources = Resources(assets, () => names().toMap)
            
            val game = meta.createGame(seating, options)
            val check = meta.createGame(seating, options)
            val ui = umetas.toMap.apply(meta).asInstanceOf[BaseUI { val gaming : meta.gaming.type }]

            import meta.gaming._
    
            val j = if (journal.any)
                new ServerJournal[ExternalAction](meta, param("server").get, param("user").get, param("secret").get, journal.get, s => meta.parseActionExternal(s), s => meta.writeActionExternal(s), param("at")./~(_.toIntOption).|(999999))
            else
            if (flag("replay"))
                new ReplayJournal[ExternalAction](meta, getElem("replay").textContent, s => meta.parseActionExternal(s))
            else
                new MemoryJournal[ExternalAction](meta)
            
            def saveReplay(onSave : => Unit) {
                hrf.quine.Quine.save(meta)(game, title, seating, options, resources, j, HRF.date, onSave)
            }

            val renderer = ui.create(uir, game, resources, title, saveReplay)
            
            def ask(faction : meta.F) : AskResult = {
                if (faction == null)
                    AskHuman
                else 
                setup.difficulty(faction) match {
                    case Human if journal.none =>
                        AskHuman
                    case Human if self.contains(faction) =>
                        AskHuman
                    case Human =>
                        WaitRemote
                    case BotDebug(botname) =>
                        DebugBot((actions : List[UserAction]) => {
                            val aa = game.explode(actions)
                            val bot = meta.getBot(faction, botname)
                            bot.eval(game, aa).sortWith(bot.compare)
                        })
                    case Bot(botname) => 
                        AskBot((actions : List[UserAction]) => meta.getBot(faction, botname).ask(game, actions, 0.01))
                }
            }
            
            import meta.tagF

            Runner.run(meta)(game, check, seating, options, resources, delay, renderer, f => (f == null).?(ask(null.asInstanceOf[meta.F])).||(f.as[meta.F]./(f => ask(f))).|!("unsuitable ask"), j)
        }
        
    }

    def arrangeOnlineGame(meta : MetaGame)(setup : meta.Setup, notes : Map[meta.F, String]) {
        val server = param("server").get
    
        val factions = setup.seating.%(f => setup.difficulty(f) != Off)
        val options = setup.options
        val humans = factions.%(f => setup.difficulty(f) == Human)
        val bots = factions.%(f => setup.difficulty(f) != Human)
        val name = meta.randomGameName()

        post(server + "/new-user", name + " HOST") { mm =>
            val master = mm.split('\n').mkString("/")

            post(server + "/new-journal/" + master, name + " LOBBY") { lobby =>
                post(server + "/new-journal/" + master, name) { game =>
                    var plays = Map[String, String]()
                    var users = Map[meta.F, String]()
                    var spectate = ""
            
                    var ca : Elem = Text("Copy all")
                    def linkMenu() {
                        asker.zask(ZBasic(name.hl, ca, () => {
                            ca = clipboard(
                                ($(meta.label) ++ 
                                    $(name) ++
                                    setup.options.of[ImportantOption]./(o => o.group.text + " " + o.valueOn.text) ++ 
                                    $("Spectate " + server + "/" + param("xplay").|("play") + "/" + meta.path + "/" + plays(spectate)) ++ 
                                    humans.map(f => notes.get(f)./(_ + " ").|("") + meta.writeFaction(f) + " " + server + "/" + param("xplay").|("play") + "/" + meta.name + "/" + plays(users(f)))
                                ).mkString("\n")
                            ).?("Copied links to clipboard").|("Error copying to clipboard").hl
                    
                            linkMenu()
                        }) +: ZBasic("", Link(server + "/" + param("xplay").|("play") + "/" + meta.name + "/" + plays(spectate), Div(Text("Spectate")), List(xstyles.link))) +: 
                            humans.map(f => ZBasic("", Link(server + "/" + param("xplay").|("play") + "/" + meta.name + "/" + plays(users(f)), Div(Empty ~ notes.get(f)./(_.hl ~ " ") ~ Text("Play as ") ~ meta.factionName(f)), List(xstyles.link)))))
                    }
                    
                    def writeLobby() {
                        var intro = List("meta " + meta.name, "version 2", "title " + name) ++ 
                            humans./(f => "user " + meta.writeFaction(f) + " " + users(f)) ++ 
                            bots./(f => "bot " + meta.writeFaction(f) + " " + setup.difficulty(f).asInstanceOf[Bot].name) ++ 
                            List(
                                "seating " + factions./(meta.writeFaction).mkString(" "),  
                                "options " + options./(meta.writeOption).mkString(" "),  
                                "server " + game
                            )
                            
                        post(server + "/append/" + master + "/" + lobby + "/0", intro.mkString("\n")) { _ =>
                            linkMenu()
                        }
                    }
                    
                    post(server + "/new-play/" + master + "/" + lobby, "Spectator") { sp =>
                        spectate = sp.split('\n')(0)
                        plays += spectate -> sp.split('\n')(1)
                        post(server + "/grant-read/" + master + "/" + game + "/" + spectate, "") { _ =>
                            var n = humans.num
                            humans.foreach { f =>
                                post(server + "/new-play/" + master + "/" + lobby, meta.factionName(f)) { sc =>
                                    users += f -> sc.split('\n')(0)
                                    plays += users(f) -> sc.split('\n')(1)
                                    post(server + "/grant-read-append/" + master + "/" + game + "/" + users(f), "") { _ =>
                                        n -= 1
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
    
}

