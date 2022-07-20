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

import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout
import org.scalajs.dom

import hrf.elem._
import hrf.html._
import hrf.meta._
import hrf.bot._
import hrf.ui._

object Runner {
    def run(meta : MetaGame)(game : meta.G, check : meta.G, seating : $[meta.F], options : $[meta.O], resources : Resources, delay : Int, ui : meta.gaming.GameUI, auto : meta.gaming.F => meta.gaming.AskResult, journal : Journal[meta.gaming.ExternalAction]) {
        import meta.gaming._
        import game._

        class StopToken
        
        sealed trait UIState
        case class UIContinue(c : Continue, aa : $[ExternalAction]) extends UIState
        case class UIStop(token : StopToken) extends UIState
        case class UIAsk(c : Continue, faction : meta.gaming.F, actions : $[UserAction], waiting : List[meta.gaming.F]) extends UIState
        case class UIAskDebug(c : Continue, faction : meta.gaming.F, actions : $[ActionEval]) extends UIState
        case class UINotify(factions : $[meta.F], infos : $[UserAction], state : UIState, fall : Boolean) extends UIState
        case class UIPerform(a : Action, aa : $[ExternalAction]) extends UIState
        case class UIRecord(c : Continue, a : ExternalAction) extends UIState
        case class UIInteractive(c : Continue, a : UserAction) extends UIState
        case class UIWait(c : Continue, factions : $[meta.gaming.F]) extends UIState
        case class UIAcknowledged(c : Continue) extends UIState
        case class UIProcess(c : Continue, aa : $[ExternalAction]) extends UIState
        case class UIRead(c : Continue) extends UIState
        case class UIBackgroundRead(c : Continue) extends UIState
        
        var state : UIState = UIRecord(Force(meta.start), meta.start)
        var actions : List[ExternalAction] = Nil
        var lan : Map[HtmlBlock, Int] = Map()
        var temp : List[HtmlBlock] = Nil

        var dirty = false
        var logged = 0
        var conflicted : Option[UserAction with Retry] = None
        var notifies : List[Notify] = Nil
        var waitingFor : List[meta.gaming.F] = Nil
        
        val self = seating.%(auto(_) match {
            case AskHuman => true
            case DebugBot(_) => true
            case _ => false
        })
        
        def mapDiv(e : Elem) = (e == Empty).?(Empty).|(Div(e))
        
        def redrawIfDirty() {
            if (dirty) {
                dirty = false
                ui.updateStatus()
            }
        }

        val audio = dom.document.createElement("audio").asInstanceOf[dom.html.Audio]

        def sound(source : String, volume : Double) {
            import org.scalajs.dom
                
            audio.src = "sounds/" + source
            audio.volume = volume
            audio.play()
        }

        def scrollActionToNum(n : Int) {
            val g = meta.createGame(seating, options)

            actions.take(n).foreach { a =>
                var c = g.performContinue(a)
                
                while (c match {
                    case Log(_, _, continue) => c = continue; true
                    case DelayedContinue(_, Force(then)) => c = g.performContinue(then); true
                    case Notify(_, _, then) => c = g.performContinue(then); true
                    case _ => false
                }) {}
            }

            ui.overrideGame = g
            ui.updateStatus()
        
            lan.keys.foreach { l =>
                l.nodes.foreach { e =>
                    e.childNodes(0).asInstanceOf[dom.html.Element].style.asInstanceOf[js.Dynamic].filter = (lan(l) > n).??("grayscale(0.7) blur(1px)")
                }
            }
        }

        def scrollActionToLatest() {
            if (ui.overrideGame != null) {
                ui.overrideGame = null.asInstanceOf[meta.G]
                ui.updateStatus()
            
                lan.keys.foreach { l =>
                    l.nodes.foreach { e =>
                        e.childNodes(0).asInstanceOf[dom.html.Element].style.asInstanceOf[js.Dynamic].filter = ""
                    }
                }
            }
        }
        

        var lit = 0.0

        var sounds = false

        def interacted() {
            lit = (new scalajs.js.Date()).getTime()
            logged = 0
        }

        def awayFor(t : Double) : Boolean = (new scalajs.js.Date()).getTime() > lit + t

        def waitForInteraction() {
            if (!sounds)
                return

            if (awayFor(2000))
                sound("beacon.wav", 0.4 * 2)
                
            val now = lit

            def warn() {
                setTimeout(20000) {
                    if (lit == now) {
                        sound("warning.wav", 0.5 * 2)
                        warn()
                    }
                }
            }

            warn()
        }

        
        var t = new StopToken

        def cut() {
            t = new StopToken
        }

        def stop = UIStop(t)

        def continueHandleState() {
            while (handleState()) {}
        }

        def handleState() : Boolean = {
            cut()
        
            def later(s : UIState) {
                state match {
                    case UIStop(tt) if tt == t => 
                        state = s
                        continueHandleState() 
                    case _ => 
                        ===("LATER FAILED " + s)
                        ===("state " + state)
                        ===("t " + t)
                }
            }
        
            state = state match {
                case UIContinue(ErrorContinue(s), Nil) =>
                    ===(s)
                    ui.alog(s, 0, _ => {})
                    return false

                case UIContinue(Force(a), Nil) =>
                    dirty = true
                    UIPerform(a, Nil)
                    
                case UIContinue(DelayedContinue(time, continue), Nil) =>
                    dirty = true
                    UIContinue(continue, Nil)
                    
                case UIContinue(c @ Ask(_, List(a : UserAction), _), Nil) if a.isSoft =>
                    dirty = true
                    UIPerform(a, Nil)

                case UIContinue(c @ Ask(_, List(a : UserAction with DontRecord), _), Nil) =>
                    dirty = true
                    UIPerform(a, Nil)
                    
                case UIContinue(c @ Ask(_, List(a : UserAction), _), Nil) =>
                    dirty = true
                    UIRecord(c, a)
                    
                case UIContinue(c @ Ask(_, l, _), Nil) if l.choice.not =>
                    dirty = true
                    UIRecord(c, l.%(_.isInstanceOf[Info].not).single.get)
                    
                case UIContinue(c @ Ask(_, List(a : UserAction, h : Hidden), _), Nil) if h != HiddenOkAction =>
                    dirty = true
                    UIRecord(c, a)
                    
                case UIContinue(c @ Ask(_, List(h : Hidden, a : UserAction), _), Nil) if h != HiddenOkAction =>
                    dirty = true
                    UIRecord(c, a)
                    
                case UIContinue(c @ Ask(faction, actions, _), Nil) =>
                    dirty = true

                    waitingFor = $(faction)

                    auto(faction) match {
                        case AskHuman => UIAsk(c, faction, actions, Nil)
                        case AskBot(q) => ui.wait(self, waitingFor, notifies); UIRecord(c, q(actions))
                        case DebugBot(q) => UIAskDebug(c, faction, q(actions))
                        case WaitRemote => UIWait(c, $(faction))
                    }
        
                case UIContinue(c @ MultiAsk(as), Nil) =>
                    dirty = true
                    val asks = as./~({
                        case a @ Ask(f, actions, _) => Some(Ask(f, actions))
                        case _ => None
                    })
        
                    lazy val waits = asks./~(ask => auto(ask.faction) match {
                        case WaitRemote => Some(ask.faction)
                        case _ => None
                    })
                    
                    lazy val debug = asks./~(ask => auto(ask.faction) match {
                        case DebugBot(q) => Some(UIAskDebug(c, ask.faction, q(ask.actions)))
                        case _ => None
                    }).take(1).single

                    lazy val human = asks./~(ask => auto(ask.faction) match {
                        case AskHuman => Some(UIAsk(c, ask.faction, ask.actions, waits))
                        case _ => None
                    }).take(1).single

                    lazy val bot = asks./~(ask => auto(ask.faction) match {
                        case AskBot(q) => Some(UIRecord(c, q(ask.actions)))
                        case _ => None
                    }).take(1).single
                    
                    debug.||(human).||(bot).|(UIWait(c, waits))
                    
                case UIContinue(c @ Roll(dice, rolled, _), Nil) =>
                    dirty = true
                    UIRecord(c, rolled(dice./(_.values.shuffle.head)))
                    
                case UIContinue(c @ Shuffle(list, shuffled, _), Nil) =>
                    dirty = true
                    UIRecord(c, shuffled(list.shuffle))
                    
                case UIContinue(c @ ShuffleUntil(list, shuffled, condition, _), Nil) =>
                    dirty = true

                    var r = list.shuffle

                    while (!condition(r))
                        r = list.shuffle
                    
                    UIRecord(c, shuffled(r))
                    
                case UIContinue(c @ Random(list, chosen, _), Nil) =>
                    dirty = true
                    UIRecord(c, chosen(list.shuffle(0)))
                    
                case UIContinue(c @ Milestone(then), Nil) =>
                    dirty = true
                    UIRecord(c, MilestoneAction(then))
                    
                case UIContinue(c @ GameOver(w, m, l), _) =>
                    scrollActionToLatest()
                
                    dirty = true

                    sounds = false

                    interacted()
                    
                    UIContinue(MultiAsk(l), Nil)
                    
                case UIContinue(c @ Notify(users : List[meta.F], infos, then), aa) =>
                    dirty = true

                    val show = users./(auto).%({ 
                        case AskHuman => true
                        case DebugBot(_) => true
                        case _ => false
                    }).any
                    
                    val u = UIPerform(then, aa)
        
                    if (!show)
                        u
                    else
                    if (aa.none)
                        UINotify(users, infos, u, false)
                    else {
                        notifies +:= c
                        u
                    }
        
                case UIContinue(Force(a), aa) =>
                    dirty = true
                    UIPerform(a, aa)
                    
                case UIContinue(Log(l, k, c), aa) =>
                    scrollActionToLatest()

                    dirty = true

                    temp.foreach { e =>
                        e.delete()
                        lan -= e
                    }

                    temp = Nil
                    
                    val line = ui.alog(l, actions.num, {
                        case n : Int => scrollActionToNum(n)
                    })

                    lan += line -> actions.num

                    if (k == LogKind.Temp)
                        temp :+= line
                    
                    logged += 1

                    UIContinue(c, aa)
                    
                case UIContinue(DelayedContinue(_, continue), aa) =>
                    dirty = true
                    UIContinue(continue, aa)
        
                case UIContinue(c, aa) =>
                    dirty = true
                    UIProcess(c, aa)    
                
                case UIWait(c, ff) => 
                    waitingFor = ff
        
                    ui.wait(self, waitingFor, notifies)

                    setTimeout(500) { 
                        later(UIRead(c)) 
                    }
        
                    stop

                case UIInteractive(c, a) =>
                    interacted()

                    cut()
                    
                    notifies = Nil

                    waitingFor = Nil

                    a match {
                        case a if a.isSoft => 
                            ===(" ")
                            ===("################## " + a)
                            ===(" ")
                            UIPerform(a, Nil)
                        case a => UIRecord(c, a)
                    }
                        
                case UIRecord(c, a) =>
                    if (check.validate(a)) {
                        if (game.validate(a)) {
                            state = stop

                            journal.append(actions.num, a) {
                                ===("")
                                ===("")
                                ===("")
                                ===("")
                                ===("")
                                ===("")
                                ===("")
                                ===("")
                                ===("")
                                ===("")
                                ===("")
                                ===("")
                                later(UIRead(c))
                            } {
                                conflicted = a match {
                                    case a : UserAction with Retry => Some(a)
                                    case _ => None
                                }
                                later(UIRead(c))
                            }
                            
                            return false
                        }
                        else {
                            ===("record validation failed " + a + "\n  against game\n" + game.continue)
                            setTimeout(0) { later(UIRead(c)) }
                            return false
                        }
                    }
                    else {
                        ===("record validation failed " + a + "\n  against check\n" + check.continue)
                        setTimeout(0) { later(UIRead(c)) }
                        return false
                    }
        
                case UIRead(c) =>
                    state = stop

                    journal.read(actions.num) { aa =>
                        later(UIProcess(c, aa)) 
                    }
        
                    return false
        
                case UIProcess(c, Nil) =>
                    UIContinue(c, Nil)
                    
                case UIProcess(c, a :: aa) =>
                    cut()

                    actions :+= a
                    
                    UIPerform(a, aa)
                    
                case UIPerform(a, aa) =>
                    a match {
                        case a : ExternalAction if a.isSoft =>
                        case _ => check.performContinue(a)
                    }

                    val c = game.performContinue(a)

                    ui.wait(self, waitingFor, notifies)

                    UIContinue(c, aa)
                    
                case UIAsk(c, f, actions, w) if conflicted.any && game.validate(conflicted.get) =>
                    ===("retry conflicted " + conflicted.get)
                    UIRecord(c, conflicted.get)
                    
                case UIAsk(c, f, actions, w) =>
                    scrollActionToLatest()

                    if (conflicted.any && !game.validate(conflicted.get)) {
                        ===("conflicted invalid " + conflicted.get)
                        conflicted = None
                    }
                    
                    ui.ask(f, actions, w, notifies, (a : UserAction) => later(UIInteractive(c, a)))

                    waitForInteraction()
        
                    stop
                    
                case UIAskDebug(c, f, actions) =>
                    scrollActionToLatest()
                    
                    case class EvalActionWrapper(ae : ActionEval) extends Choice {
                        def question(g : G) = ae.action.question(g) ~ Comment(ae.action.toString)
                        def option(g : G) = ae.action.option(game) ~ " (" ~ ae.evaluations.headOption./(_.weight)./(v => Span(Text(v), (v > 0).?(xstyles.score.good).|(xstyles.score.bad))).|(Text(0)) ~ ")" ~ HorizontalBreak ~
                        ae.action.isInstanceOf[ThenAction].?(ae.action.unwrap.toString.spn) ~
                        ae.evaluations./(e => Span(
                            "(" ~ Span(Text(e.weight), (e.weight > 0).?(xstyles.score.good).|(xstyles.score.bad)) ~ " -> " ~ e.desc ~ ")", xstyles.score.explain
                        )).join(HorizontalBreak)
                    }

                    ui.ask(f, actions./(wa => EvalActionWrapper(wa)), Nil, notifies, (eaw : UserAction) => {
                        val ae = eaw.asInstanceOf[EvalActionWrapper].ae
                        
                        ===("eval " + (ae.action.question(game) ~ " -> " ~ ae.action.option(game)).text)
                        ae.evaluations.foreach { e => 
                            ===("  (" + e.weight + " -> " + e.desc + ")")
                        } 
                    
                        later(UIInteractive(c, ae.action))
                    })
                    
                    stop

                case UINotify(factions, infos, then, fall) =>
                    scrollActionToLatest()

                    if (hrf.HRF.flag("nonot"))
                        then
                    else {
                        ui.notify(factions, infos, { interacted(); later(then) })
                        
                        if (fall)
                            then
                        else {
                            waitForInteraction()
                                                
                            stop
                        }
                    }
            }
            
            state match {
                case UIContinue(Log(_, _, _), Nil) if logged > 1 => setTimeout(hrf.HRF.param("speed").|("250").toInt) { continueHandleState() }; false
                case UIContinue(_, Nil) => redrawIfDirty(); ui.wait(self, waitingFor, notifies); true
                case UIStop(_) => redrawIfDirty(); false
                case _ => true
            }
        }
        
        ui.start()

        continueHandleState()

        def updateGlyph() {
            if (awayFor(15000))
                HRF.glyph(self.single./(meta.glyph(game, _)).|(meta.glyph(game))./(resources.getImage(_).src).|(HRF.defaultGlyph))
            else
                HRF.glyph(self.single./~(meta.glyph(game, _)).||(self.single./~(meta.glyph)).||(meta.glyph(game))./(resources.getImage(_).src).|(HRF.defaultGlyph))

            setTimeout(1000) { updateGlyph() }
        }

        updateGlyph()
    }
}

