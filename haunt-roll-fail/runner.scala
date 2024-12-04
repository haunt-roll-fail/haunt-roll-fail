package hrf
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

import hrf.elem._
import hrf.html._
import hrf.meta._
import hrf.bot._
import hrf.ui._
import hrf.voice._

import org.scalajs.dom

import scalajs.js
import scalajs.js.timers.setTimeout

object Runner {
    def run(meta : MetaGame)(seating : $[meta.F], options : $[meta.O], resources : Resources, delay : Int, ui : meta.gaming.GameUI, auto : (meta.G, meta.gaming.F) => meta.gaming.AskResult, journal : Journal[meta.gaming.ExternalAction]) {
        import meta.gaming._

        sealed trait UIState
        case class UIContinue(c : Continue, aa : $[ExternalAction]) extends UIState
        case class UIStop(token : StopToken, launch : ((() => UIState) => Unit) => Unit) extends UIState
        case class UIAsk(c : Continue, faction : Option[F], actions : $[UserAction], waiting : $[meta.gaming.F]) extends UIState
        case class UIAskDebug(c : Continue, faction : F, actions : $[ActionEval]) extends UIState
        case class UIPerform(a : Action, aa : $[ExternalAction]) extends UIState
        case class UIRecord(info : String, c : Continue, a : ExternalAction) extends UIState
        case class UIRecordUndo(a : UndoAction) extends UIState
        case class UIInteractive(c : Continue, a : UserAction) extends UIState
        case class UIWait(c : Continue, factions : $[F]) extends UIState
        case class UIAcknowledged(c : Continue) extends UIState
        case class UIProcess(c : Continue, aa : $[ExternalAction]) extends UIState
        case class UIRead(c : Continue) extends UIState

        class StopToken(value : Double)

        def stop(launch : ((() => UIState) => Unit) => Unit) = {
            var token = new StopToken(0)

            UIStop(token, launch)
        }


        def newGame() = meta.createGame(seating, options)

        var check : Option[meta.G] = None
        var game = newGame()

        ui.currentGame = game

        var actions : $[ExternalAction] = $

        def time() = (new scalajs.js.Date()).getTime()

        def generateGame(n : Int) = {
            val d = time()

            var aaa = actions.take(n).reverse
            var lll = aaa.take(0)
            var skip = 0

            aaa.foreach { a =>
                if (skip > 0)
                    skip -= 1
                else {
                    a @@ {
                        case UndoAction(_, n, t) => skip = n
                        case a => lll +:= a
                    }
                }
            }

            val g = newGame()

            lll.foreach { a =>
                var c = g.performContinue(|(g.continue), a, false).nest

                while (c match {
                    case Log(_, _, continue) => c = continue; true
                    case DelayedContinue(_, Force(then)) => c = g.performContinue(|(g.continue), then, false).nest; true
                    case _ => false
                }) {}
            }

            g
        }

        def generateGameVoid(n : Int) = {
            var aaa = actions.take(n).reverse
            var lll = aaa.take(0)
            var skip = 0

            aaa.foreach { a =>
                if (skip > 0)
                    skip -= 1
                else {
                    a @@ {
                        case UndoAction(_, n, t) => skip = n
                        case a => lll +:= a
                    }
                }
            }

            val g = newGame()

            g.as[LoggedGame]./(_.logging = false)

            lll.dropRight(1).foreach { a =>
                if (a.isSoft)
                    warn("recorded action is soft", a)

                g.performVoid(a)
            }

            lll.lastOption.foreach { a =>
                var c = g.performContinue(|(g.continue), a, false).nest

                while (c match {
                    case Log(_, _, continue) => c = continue; true
                    case DelayedContinue(_, Force(then)) => c = g.performContinue(|(g.continue), then, false).nest; true
                    case _ => false
                }) {}
            }

            g.as[LoggedGame]./(_.logging = true)

            g
        }

        def advanceGame(g : G, from : Int, to : Int) : G = {
            val aaa = {
                var l = actions.take(0)
                var skip = 0

                actions.take(from).reverse.foreach { a =>
                    if (skip > 0)
                        skip -= 1
                    else {
                        a @@ {
                            case UndoAction(_, n, t) => skip = n
                            case a => l +:= a
                        }
                    }
                }

                l
            }

            val bbb = {
                var l = actions.take(0)
                var skip = 0

                actions.take(to).reverse.foreach { a =>
                    if (skip > 0)
                        skip -= 1
                    else {
                        a @@ {
                            case UndoAction(_, n, t) => skip = n
                            case a => l +:= a
                        }
                    }
                }

                l
            }

            if (bbb.take(aaa.num) != aaa)
                return generateGameVoid(to)

            bbb.drop(aaa.num).foreach { a =>
                g.performVoid(a)
            }

            g
        }


        case class LogLine(block : LazyBlock, action : Int, temp : Boolean)

        var lines : $[LogLine] = $

        var state : UIState = UIRead(StartContinue)

        var dirty = false
        var logged = 0
        var conflicted : Option[ExternalAction] = None
        var waitingFor : $[F] = $

        var softMaps = Map[Continue, Ask]()

        val self = seating.%(auto(game, _) match {
            case AskHuman => true
            case DebugBot(_) => true
            case _ => false
        })

        var lastInteracted = 0

        var sounds = false

        def redrawIfDirty() {
            if (dirty) {
                dirty = false
                ui.stopRewind()
                ui.updateStatus()
            }
        }

        val audio = dom.document.createElement("audio").asInstanceOf[dom.html.Audio]

        def sound(source : String, volume : Double) {
            audio.src = "sounds/" + source
            audio.volume = volume
            audio.play()
        }

        def striken(n : Int) = {
            var result = $[Int]()
            var skip = 0

            actions.take(n).indexed.reverse.foreach { (a, i) =>
                if (skip > 0) {
                    skip -= 1
                    result :+= i
                }
                else {
                    a @@ {
                        case UndoAction(_, n, t) => skip = n
                        case _ =>
                    }
                }
            }

            result
        }

        var current : Int = 0
        var line : Int = 0

        def updateLogDecor(l : Int, highlight : |[Int] = None) {
            if (lines.none)
                return

            current = lines(l - 1).action

            var st = striken(actions.num)

            while (st.contains(current - 1))
                st = striken(st.head + 1)

            lines.indexed.foreach { (l, i) =>
                l.block.get.nodes.foreach { e =>
                    val s = e.childNodes(0).asInstanceOf[dom.html.Element].style

                    if (highlight.has(l.action)) {
                        s.filter = "drop-shadow(0px 0px 6px white) brightness(2)"
                        s.textDecoration = ""
                    }
                    else
                    if (l.temp && i != lines.num - 1) {
                        s.display = "none"
                        s.filter = ""
                        s.textDecoration = ""
                    }
                    else
                    if (i >= line && st.contains(l.action - 1)) {
                        s.display = ""
                        s.filter = "grayscale(0.7) blur(2px)"
                        s.textDecoration = "line-through 0.4ch solid rgb(128, 128, 128, 0.8)"
                    }
                    else
                    if (i >= line) {
                        s.display = ""
                        s.filter = "grayscale(0.7) blur(2px)"
                        s.textDecoration = ""
                    }
                    else
                    if (st.contains(l.action - 1)) {
                        s.display = ""
                        s.filter = "grayscale(0.1) blur(1px)"
                        s.textDecoration = "line-through 0.4ch solid rgb(128, 128, 128, 0.8)"
                    }
                    else {
                        s.display = ""
                        s.filter = ""
                        s.textDecoration = ""
                    }
                }
            }
        }

        def bodyFilter(s : String) {
            val parent = dom.document.body
            val nodes = 0.until(parent.childNodes.length)./(parent.childNodes.item)
            nodes.foreach { c =>
                c.asInstanceOf[dom.html.Element].style.filter = s
            }
        }

        def continueScrollingTo(cont : dom.html.Element, prev : Double, current : Double, target : Double, delta : Int, onComplete : () => Unit, onAbort : () => Unit) {
            if (cont.scrollTop == current) {
                if ((cont.scrollTop - target).abs < delta || prev == current) {
                    cont.scrollTop = target
                    if (onComplete != null)
                        onComplete()
                }
                else {
                    if (cont.scrollTop + delta * 8 < target)
                        cont.scrollTop += (target - cont.scrollTop) / 8
                    else
                    if (cont.scrollTop - delta * 8 > target)
                        cont.scrollTop -= (cont.scrollTop - target) / 8
                    else
                    if (cont.scrollTop < target)
                        cont.scrollTop += delta
                    else
                        cont.scrollTop -= delta

                    val newval = cont.scrollTop

                    setTimeout(20) { continueScrollingTo(cont, current, newval, target, delta, onComplete, onAbort) }
                }
            }
            else {
                if (onAbort != null)
                    onAbort()
            }
        }

        def continueScrollingToLines(lines : $[LogLine], onComplete : () => Unit, onAbort : () => Unit) {
            val ll = lines./~(_.block.get.nodes.map(_.childNodes(0).asInstanceOf[dom.html.Element]))

            val low = ll./(e => e.offsetTop).min
            val high = ll./(e => e.offsetTop + e.offsetHeight).max
            val cont = ll(0).parentElement.parentElement.parentElement

            val scroll = cont.scrollTop
            val target = (low + high) / 2 - cont.clientHeight / 2
            val delta = 8
            val prev = target - (scroll - target)

            continueScrollingTo(cont, prev, scroll, target, delta, onComplete, onAbort)
        }

        def scrollActionToNum(n : Int) {
            val invert = true // n > current

            val ll = lines.%(_.action == n)./~(_.block.get.nodes.map(_.childNodes(0).asInstanceOf[dom.html.Element]))

            val low = ll./(e => e.offsetTop).min
            val high = ll./(e => e.offsetTop + e.offsetHeight).max
            val cont = ll(0).parentElement.parentElement.parentElement

            if ((n == actions.num || lines.last.action <= n) && invert) {
                scrollActionToLatest()
            }
            else {
                var k = invert.?(n).|(lines.%(_.temp.not)./(_.action).%(_ < n).maxOr(0))

                if (k == 0)
                    k = 1

                line = lines.takeWhile(_.action <= k).num

                setTimeout(0) {
                    ui.rewind(k, actions, None, scrollActionToLatest, undoTo, replayNext)

                    ui.overrideGame = generateGameVoid(k).?
                    ui.updateStatus()
                }
            }

            updateLogDecor(line, Some(n))

            val scroll = cont.scrollTop
            val target = (low + high) / 2 - cont.clientHeight / 2
            val delta = 8
            val prev = target - (scroll - target)

            setTimeout(0) {
                continueScrollingTo(cont, prev, scroll, target, delta, () => { updateLogDecor(line) }, null)
            }
        }

        def replayNext(speed : Int) {
            if (ui.overrideGame.any && line + 1 < lines.num) {
                if (lines(line).temp.not)
                    continueScrollingToLines($(line)./(lines), () => { setTimeout(speed) { replayNext(speed) } }, null)
                else
                    setTimeout(speed) { replayNext(speed) }

                if (line > 0) {
                    if (lines(line - 1).action != lines(line).action) {
                        ui.overrideGame = ui.overrideGame./(g => advanceGame(g, lines(line - 1).action, lines(line).action))
                        ui.updateStatus()
                    }
                }

                line += 1
                updateLogDecor(line)
                ui.rewind(lines(line).action, actions, Some(speed), scrollActionToLatest, undoTo, replayNext)

            }
            else
                scrollActionToLatest()
        }

        def scrollActionToLatest() {
            if (ui.overrideGame.any) {
                ui.overrideGame = None
                ui.stopRewind()
                ui.updateStatus()
            }

            line = lines.num

            updateLogDecor(line)
        }

        def undoTo(n : Int, total : Int) {
            if (actions.num == total) {
                state = UIRecordUndo(UndoAction(self, total - n, n))
                continueHandleState()
            }
            else
                scrollActionToLatest()
        }

        def interacted() {
            lastInteracted = HRF.uptime()
            logged = 0
        }

        def awayFor(t : Double) : Boolean = HRF.uptime() > lastInteracted + t

        def waitForInteraction() {
            if (!sounds)
                return

            if (awayFor(2000))
                sound("beacon.wav", 0.4 * 2)

            val now = lastInteracted

            def warn() {
                setTimeout(20000) {
                    if (lastInteracted == now) {
                        sound("warning.wav", 0.5 * 2)
                        warn()
                    }
                }
            }

            warn()
        }

        def continueHandleState() {
            val start = time()
            while (handleState()) {
                if (time() - start > HRF.paramInt("smooth").|(200)) {
                    setTimeout(0) { continueHandleState() }
                    return
                }
            }
        }

        def handleState() : Boolean = {
            state = state match {
                case UIContinue(c @ ErrorContinue(e, msg), aa) =>
                    warn("ErrorContinue:", msg)

                    setTimeout(0) { throw e }

                    val messages = $(
                        "giant bugs",
                        "toxic mushrooms",
                        "time out of joint",
                        "death rays",
                        "undead attack",
                        "elder god awakened",
                        "computer caught fire",
                        "teapot empty",
                        "division by zero",
                        "turn off and on again",
                        "DEADBEEF",
                        "reverse polarity",
                        "restart universe",
                        "nuclear launch detected",
                        "check voltage",
                        "research incomplete",
                        "infinite array length",
                        "apply pressure",
                        "sentient user",
                        "negative attitude",
                        "warranty void",
                        "copyright lapsed",
                        "blink twice if you need help",
                        "proceed on life",
                        "boot sector missing",
                        "abort launch sequence",
                        "air not breathable",
                        "feedback loop",
                        "lightspeed exceeded",
                        "tragic addiction",
                        "root access revoked",
                        "recovery in progress",
                        "imminent danger",
                        "I AM ERROR",
                        "details left to the imagination",
                        "apple is not a number",
                        "faith too small",
                        "quote bible",
                        "kernel panic",
                        "watch the sky for UFOs",
                        "radiation overdose",
                        "exact circumstances remain unknown",
                        "seek counsel",
                        "evidence inconclusive",
                        "whisky tango foxtrot",
                        "broken heart",
                        "deadlocked",
                        "probablity underestimated",
                        "this is awkward",
                    )

                    val m = messages(HRF.uptime() % messages.num)

                    UIProcess(Log(("Error: " + m).spn(xstyles.error), LogKind.Normal, MultiAsk($)), aa)

                case UIContinue(Force(a), Nil) =>
                    dirty = true
                    UIPerform(a, $)

                case UIContinue(DelayedContinue(time, continue), Nil) =>
                    dirty = true
                    UIContinue(continue, $)

                case UIContinue(c @ Ask(f, List()), aa) =>
                    UIProcess(Log(("Error: " + "zero ask " + f).spn(xstyles.error), LogKind.Normal, MultiAsk($)), aa)

                case UIContinue(c @ Ask(_, List(a : UserAction)), Nil) if a.isSoft =>
                    UIPerform(a, $)

                case UIContinue(c @ Ask(_, List(a : UserAction with DontRecord)), Nil) =>
                    UIPerform(a, $)

                case UIContinue(c @ Ask(_, List(a : UserAction)), Nil) =>
                    dirty = true
                    UIRecord("#unchoice", c, a)

                case UIContinue(c @ Ask(_, l), Nil) if c.choice.not =>
                    UIContinue(c.copy(actions = l.%(_.is[Info].not).single.$), Nil)

                case UIContinue(c @ Ask(_, List(a : UserAction, h : Hidden)), Nil) if h != HiddenOkAction =>
                    dirty = true
                    UIRecord("#unchoice", c, a)

                case UIContinue(c @ Ask(_, List(h : Hidden, a : UserAction)), Nil) if h != HiddenOkAction =>
                    dirty = true
                    UIRecord("#unchoice", c, a)

                case UIContinue(c @ Ask(faction, actions), Nil) =>
                    auto(game, faction) match {
                        case AskHuman => UIAsk(c, Some(faction), actions, $)
                        case AskBot(q) =>
                            waitingFor = $(faction)
                            ui.wait(self, waitingFor)
                            UIRecord("#bot " + faction, c, q(actions))
                        case DebugBot(q) => UIAskDebug(c, faction, q(actions))
                        case WaitRemote => UIWait(c, $(faction))
                    }

                case UIContinue(c @ MultiAsk(as), Nil) =>
                    dirty = true
                    val asks = as./~({
                        case a @ Ask(f, actions) => Some(Ask(f, actions))
                        case _ => None
                    })

                    lazy val waits = asks./~(ask => auto(game, ask.faction) match {
                        case WaitRemote => Some(ask.faction)
                        case _ => None
                    })

                    lazy val debug = asks./~(ask => auto(game, ask.faction) match {
                        case DebugBot(q) => Some(UIAskDebug(c, ask.faction, q(ask.actions)))
                        case _ => None
                    }).take(1).single

                    lazy val human = asks./~(ask => auto(game, ask.faction) match {
                        case AskHuman => Some(UIAsk(c, Some(ask.faction), ask.actions, waits))
                        case _ => None
                    }).take(1).single

                    lazy val bot = asks./~(ask => auto(game, ask.faction) match {
                        case AskBot(q) => Some(UIRecord("#bot multi-ask", c, q(ask.actions)))
                        case _ => None
                    }).take(1).single

                    debug || human || bot | UIWait(c, waits)

                case UIContinue(c @ Roll(dice, rolled, _), Nil) =>
                    dirty = true
                    UIRecord("#roll", c, rolled(dice./(_.roll())))

                case UIContinue(c @ Roll2(dice1, dice2, rolled, _), Nil) =>
                    dirty = true
                    UIRecord("#roll2", c, rolled(dice1./(_.roll()), dice2./(_.roll())))

                case UIContinue(c @ Roll3(dice1, dice2, dice3, rolled, _), Nil) =>
                    dirty = true
                    UIRecord("#roll3", c, rolled(dice1./(_.roll()), dice2./(_.roll()), dice3./(_.roll())))

                case UIContinue(c @ Roll(dice, rolled, _), Nil) =>
                    dirty = true
                    UIRecord("#roll", c, rolled(dice./(_.roll())))

                case UIContinue(c @ Shuffle(list, shuffled, _), Nil) =>
                    dirty = true
                    UIRecord("#shuffle", c, shuffled(list.shuffle))

                case UIContinue(c @ Shuffle2(l1, l2, shuffled, _), Nil) =>
                    dirty = true
                    UIRecord("#shuffle2", c, shuffled(l1.shuffle, l2.shuffle))

                case UIContinue(c @ Shuffle3(l1, l2, l3, shuffled, _), Nil) =>
                    dirty = true
                    UIRecord("#shuffle3", c, shuffled(l1.shuffle, l2.shuffle, l3.shuffle))

                case UIContinue(c @ ShuffleUntil(list, shuffled, condition, _), Nil) =>
                    dirty = true

                    var r = list.shuffle

                    while (!condition(r))
                        r = list.shuffle

                    UIRecord("#shuffle until", c, shuffled(r))

                case UIContinue(c @ Random(list, chosen, _), Nil) =>
                    dirty = true
                    UIRecord("#random", c, chosen(list.shuffle(0)))

                case UIContinue(c @ Milestone(message, then), Nil) =>
                    dirty = true
                    UIRecord(message, c, then.wrap)

                case UIContinue(c @ Then(then), Nil) if then.isSoft =>
                    UIPerform(then, $)

                case UIContinue(c @ Then(then), Nil) =>
                    dirty = true
                    UIRecord("#then", c, then.wrap)

                case UIContinue(c @ GameOver(w, m, l), Nil) =>
                    dirty = true

                    sounds = false

                    interacted()

                    UIAsk(c, None, l, $)

                case UIContinue(Force(a), aa) =>
                    dirty = true
                    UIPerform(a, aa)

                case UIContinue(log @ Log(l, k, c), aa) =>
                    UIProcess(log, aa)

                case UIProcess(Log(l, k, c), aa) =>
                    dirty = true

                    val nl = ui.alog(l, actions.num, {
                        case n : Int => scrollActionToNum(n)
                        case List(n : Int, a : Any) if current >= n => ui.onClick(a)
                    }, check.none)

                    lines :+= LogLine(nl, actions.num, k == LogKind.Temp)

                    logged += 1

                    line = lines.num

                    if (aa.none)
                        scrollActionToLatest()

                    if (Speech.enabled && aa.num < 6)
                        stop { later =>
                            Speech.initialize(Speech.say(l.text, later(() => UIContinue(c, aa)), later(() => UIContinue(c, aa))))
                            setTimeout(3000) { later(() => UIContinue(c, aa)) }
                        }
                    else
                        UIContinue(c, aa)

                case UIContinue(DelayedContinue(_, continue), aa) =>
                    dirty = true
                    UIContinue(continue, aa)

                case UIContinue(c, Nil) =>
                    error("unknown continue:", c)

                    val m = "unknown continue"

                    UIContinue(Log(("Error occured: " + m).spn(xstyles.error), LogKind.Normal, MultiAsk($)), Nil)

                case UIContinue(c, aa) =>
                    dirty = true
                    UIProcess(c, aa)

                case UIWait(c, ff) =>
                    if (waitingFor != ff) {
                        waitingFor = ff

                        ui.wait(self, waitingFor)

                        setTimeout(0) { hrf.web.timed(0)("scrollActionToLatest()") { scrollActionToLatest() } }
                    }

                    c match {
                        case Ask(f, actions) if random() < 0.05 =>
                            val a = ui.shadowAsk(f, actions)

                            if (HRF.flag("async-test"))
                                ui.alog("...", 0, _ => {})

                            if (a.any)
                                UIRecord("#shadow", c, a.get)
                            else
                                UIWait(c, ff)

                        case _ =>
                            stop { later =>
                                setTimeout(500) {
                                    later(() => UIRead(c))
                                }
                            }
                    }

                case UIInteractive(c, a) =>
                    interacted()

                    waitingFor = $

                    a match {
                        case a if a.isSoft => UIPerform(a, $)
                        case a => UIRecord("#interactive", c, a)
                    }

                case UIRecordUndo(a) =>
                    stop { later =>
                        journal.write(actions.num, a) {
                            later(() => UIRead(game.continue))
                        } {
                            later(() => UIRead(game.continue))
                        }
                    }

                case UIRecord(m, c, a) =>
                    if (check.none && actions.any)
                        check = hrf.web.timed(0)("generated check game at #" + actions.num) { Some(generateGame(actions.num)) }

                    if (check.none || check.get.validate(check.get.continue, a)) {
                        if (game.validate(game.continue, a)) {
                            stop { later =>
                                journal.write(actions.num, a, m.startsWith("#").not.?(m).%(_.any)) {
                                    later(() => UIRead(c))
                                } {
                                    later { () =>
                                        conflicted = a.unwrap match {
                                            case _ : Action with Retry => Some(a)
                                            case _ => None
                                        }
                                        UIRead(c)
                                    }
                                }
                            }
                        }
                        else {
                            +++("record validation failed," a, "\n  against game\n", game.continue)
                            stop { later => setTimeout(0) { later(() => UIRead(c)) } }
                        }
                    }
                    else {
                        +++("record validation failed ", a, "\n  against check\n", check.get.continue)
                        stop { later => setTimeout(0) { later(() => UIRead(c)) } }
                    }

                case UIRead(c) =>
                    stop { later =>
                        journal.read(actions.num) { aa =>
                            if (aa.any)
                                later(() => UIProcess(c, aa))
                            else
                            if (c == StartContinue)
                                later(() => UIRecord("#start", c, meta.start))
                            else
                                later(() => UIWait(c, waitingFor))
                        }
                    }

                case UIProcess(c, Nil) =>
                    UIContinue(c, $)

                case UIProcess(c, a :: aa) =>
                    actions :+= a

                    current = actions.num - 1

                    waitingFor = $

                    UIPerform(a, aa)

                case UIPerform(a : StartGameAction with GameVersion, aa) if a.version != HRF.version && HRF.version == "0.8.73" && HRF.flag("override").not =>
                    ui.alog("Game created on version " ~ a.version.hl, 0, _ => {})

                    val q = "This game was created on " ~ "HRF".hlb ~ " version " ~ a.version.hl

                    case object ContinuePlaying extends Choice {
                        def question(implicit g : G) = q
                        def option(implicit g : G) = "Continue playing version " ~ HRF.version.hl
                    }

                    case object ReloadVersion extends Choice {
                        def question(implicit g : G) = q
                        def option(implicit g : G) = "Reload using version " ~ a.version.hl
                    }

                    stop { later =>
                        ui.ask(None, ContinuePlaying :: ReloadVersion, {
                            case ContinuePlaying =>
                                setTimeout(0) { later(() => UIPerform(meta.start, aa)) }
                            case ReloadVersion => {
                                val l = dom.document.location
                                val search = ("version=" + a.version) +: l.search.drop(1).split('&').$.%(_.startsWith("version").not).but("")
                                val url = l.origin + l.pathname + "?" + search.join("&") + l.hash
                                ui.alog("reload " + url, 0, _ => {})
                                dom.document.location.assign(url)
                            }
                        })
                    }

                case UIPerform(UndoAction(ff, n, t), aa) =>
                    val l = ff.single./(f => ui.factionElem(f)).|("Players".hl) ~ " undid " ~ n.hl ~ " "

                    val st = striken(t + n - 1)
                    val ds = t.to(t + n - 1).$.diff(st)

                    game = hrf.web.timed(0)("generateGameVoid(" + t + ")") { generateGameVoid(t) }

                    check = None

                    ui.currentGame = game

                    val dd = ds./(i => actions(i))./~(ui.describeActionForUndo(_, ff.single))
                    val ee = (dd.distinct.num > 1 || (dd.num == 1 && n > 1)).?("action" + (n > 1).??("s") + ", including ").|("").spn ~ dd.distinct./(d => (dd.count(d) > 3).?("multple ".spn) ~ d.elem(dd.count(d))).join(", ")

                    UIContinue(Log(l ~ ee, LogKind.Normal, game.continue.unwrap), aa)

                case UIPerform(a, aa) if a.isSoft =>
                    var c = game.performContinue(|(game.continue), a, true).nest

                    game.continue.unwrap match {
                        case MultiAsk(l) =>
                            c match {
                                case c : Ask =>
                                    l.%(_.faction == c.faction).single.foreach { a =>
                                        softMaps += a -> c
                                    }
                                case _ =>
                            }
                        case _ =>
                    }

                    UIContinue(c, aa)

                case UIPerform(a, aa) =>
                    if (check.any) {
                        a match {
                            case a : ExternalAction if a.isSoft =>
                            case _ => check.get.performContinue(|(check.get.continue), a, true).nest
                        }
                    }

                    var c = game.performContinue(|(game.continue), a, true).nest

                    UIContinue(c, aa)

                case UIAsk(c, f, actions, w) if conflicted.any && game.validate(game.continue, conflicted.get) =>
                    +++("retry conflicted", conflicted.get)

                    UIRecord("#retry", c, conflicted.get)

                case UIAsk(c, f, aa, w) =>
                    setTimeout(0) { hrf.web.timed(0)("scrollActionToLatest()") { scrollActionToLatest() } }

                    if (conflicted.any && !game.validate(game.continue, conflicted.get)) {
                        +++("conflicted invalid", conflicted.get)
                        conflicted = None
                    }

                    val (xf, xaa) = f./~(f => softMaps.get(Ask(f, aa))) match {
                        case Some(Ask(f, aa)) => (|(f), aa)
                        case _ => (f, aa)
                    }

                    stop { later =>
                        ui.ask(xf, xaa, (a : UserAction) => later(() => UIInteractive(c, a)))

                        def backgroundCheck() {
                            setTimeout(500) {
                                journal.read(actions.num) { aa =>
                                    if (aa.any)
                                        later(() => UIProcess(c, aa))
                                    else
                                        backgroundCheck()
                                }
                            }
                        }

                        backgroundCheck()

                        waitForInteraction()
                    }

                case UIAskDebug(c, f, actions) =>
                    scrollActionToLatest()

                    case class EvalActionWrapper(ae : ActionEval) extends Choice {
                        def question(implicit g : G) = ae.action.question(g) ~ Comment(ae.action.toString)
                        def option(implicit g : G) = ae.action.option(game) ~ " (" ~ ae.evaluations.starting./(_.weight)./(v => Span(Text(v), (v > 0).?(evscore.good).|(evscore.bad))).|(Text(0)) ~ ")" ~ HorizontalBreak ~
                        ae.action.unwrap.toString.spn(evscore.action) ~ HorizontalBreak ~
                        ae.evaluations./(e => Span(
                            "(" ~ Span(Text(e.weight), (e.weight > 0).?(evscore.good).|(evscore.bad)) ~ " -> " ~ e.desc ~ ")", evscore.explain
                        )).join(HorizontalBreak)
                    }

                    stop { later =>
                        ui.ask(Some(f), actions.%(_.action.is[Hidden].not)./(wa => EvalActionWrapper(wa)), (eaw : UserAction) => {
                            val ae = eaw.asInstanceOf[EvalActionWrapper].ae

                            +++("eval", ae.action.question(game).text, "->", ae.action.option(game).text)
                            ae.evaluations.foreach { e =>
                                +++("  (", e.weight, "->", e.desc, ")")
                            }

                            later(() => UIInteractive(c, ae.action))
                        })
                    }
            }

            state match {
                case UIContinue(Log(_, _, _), Nil) if logged > 1 =>
                    setTimeout(hrf.HRF.paramInt("speed").|(240)) { continueHandleState() }
                    false

                case UIContinue(_, Nil) =>
                    if (dirty)
                        ui.wait(self, waitingFor)
                    redrawIfDirty()
                    true

                case UIContinue(Milestone(_, _), _) if false =>
                    setTimeout(0) { continueHandleState() }
                    false

                case UIStop(token, launch) =>
                    launch(action => {
                        state match {
                            case UIStop(t, _) if t == token =>
                                state = action()
                                continueHandleState()
                            case _ =>
                                +++("later failed")
                        }
                    })
                    false

                case _ =>
                    true
            }
        }

        ui.start()

        continueHandleState()

        def updateGlyph() {
            if (awayFor(15000))
                HRF.glyph(self.single./(meta.glyph(game, _)).|(meta.glyph(game))./(resources.images.getSource).|(HRF.defaultGlyph))
            else
                HRF.glyph(self.single./~(meta.glyph(game, _)).||(self.single./~(meta.glyph)).||(meta.glyph(game))./(resources.images.getSource).|(HRF.defaultGlyph))

            setTimeout(1000) { updateGlyph() }
        }

        updateGlyph()
    }
}
