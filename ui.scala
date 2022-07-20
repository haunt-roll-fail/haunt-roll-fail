package hrf.ui
//
//
//
//
import logger._, colmat._
//
//
//
//

import org.scalajs.dom

import hrf.base._
import hrf.web._
import hrf.elem._
import hrf.html._
import hrf.meta._

import scala.collection.mutable

case class Rect(x : Int, y : Int, width : Int, height : Int)

trait BaseUI {
    val gaming : Gaming
    
    import gaming._

    def create(uir : hrf.html.ElementAttachmentPoint, game : G, resources : Resources, title : String, saveReplay : (=> Unit) => Unit) : gaming.GameUI
    
    trait GreyUI { self : gaming.GameUI =>
        val game : G
        val uir : ElementAttachmentPoint
        val resources : Resources
    
        def state = Option(overrideGame).|(game)

        var panes = Map[String, Container]()
        
        def newAbstractPane(name : String, e : Elem) = {
            val c = uir.appendContainer(e, resources)
            panes += name -> c
            c
        }

        def newOuterPane(name : String, e : Elem) = newAbstractPane(name, Div(Div(e, xstyles.unselectable, xstyles.outer), xstyles.pane))
        def newPane(name : String, e : Elem, styles : Style*) = newOuterPane(name, Div(e, xstyles.inner +: styles.toList))

        val actionDiv = newPane("action", Content.div(xlo.column)(xlo.fullheight), xstyles.pane.action)
        val logDiv = newPane("log", Div(Div(Div(Empty, xstyles.halfcharline) ~ Content ~ Div(Empty, xstyles.halfcharline), xstyles.logblurhor), xstyles.logblur), xstyles.pane.log)
    
        val preDiv = actionDiv.attach.appendContainer(Content.div(xlo.fullwidth)(xlo.flexhcenter), resources)
        val actDiv = actionDiv.attach.appendContainer(Content.div(xlo.fullwidth)(xlo.flexhcenter), resources)
        val postDiv = actionDiv.attach.appendContainer(Content.div(xlo.fullwidth)(xlo.flexhcenter), resources)

        val asker = new NewAsker(actDiv, resources.getImage)
        val preasker = new NewAsker(preDiv, resources.getImage)
        val postasker = new NewAsker(postDiv, resources.getImage)

        
        val logger = new NewLogger(logDiv.attach, true)
        

        def overlayScroll(e : Elem) =
            Div(
                Div(
                    Div(e, 
                    xstyles.middleScrollIn), 
                xstyles.middleScrollOut), 
            xstyles.unselectable, xstyles.inner, xstyles.pane.action)
         
        def overlayFit(e : Elem) =
            Div(
                Div(e, 
                xstyles.middleScrollOut), 
            xstyles.unselectable, xstyles.inner)
        
        def layout(width : Int, height : Int) : List[(String, Rect, Option[Double])]
    
        def factionElem(f : F) : Elem
    
        def info(self : Option[F], aa : List[UserAction]) : List[ZOption] = Nil
        
        def preinfo(self : Option[F], aa : List[UserAction]) : List[ZOption] = Nil
    
        def notify(factions : List[F], infos : List[UserAction], then : => Unit) {
            asker.scrollIfNeeded()
            
            preasker.zask(Nil)(resources)
            asker.zask(convertActions(factions.single, infos :+ OkAction(null), _ => then))(resources)
            postasker.zask(info(factions.single, infos))(resources)
        }
        
        def ask(faction : F, actions : List[UserAction], waiting : List[F], notifies : List[Notify], then : UserAction => Unit) {
            asker.scrollIfNeeded()

            preasker.zask(preinfo(Option(faction), actions) ++ notifications(notifies, Option(faction)))(resources)
            asker.zask(convertActions(Option(faction), actions, then))(resources)
            postasker.zask(info(Option(faction), actions))(resources)
        }
    
        def notifications(notifies : List[Notify], self : Option[F]) = {
            notifies./~ { n =>
                convertActions(self, n.infos) :+ ZOption(Empty, Break)
            }
        }
    
        def wait(self : List[F], factions : List[F], notifies : List[Notify]) {
            val zw = factions.any.??($(ZOption(Div("Waiting for " ~ factions./(f => factionElem(f)).comma)(xlo.fullwidth), Div(Text("z... z... z..."), ZBasic.info)(xlo.fullwidth))))
  
            preasker.zask(preinfo(self.single, Nil) ++ notifications(notifies, self.single))(resources)
            factions.any.??(asker.zask(zw)(resources))
            postasker.zask(info(self.single, Nil))(resources)
        }

        def onClick : Any => Unit

        def styleAction(faction : Option[F], actions : List[UserAction], a : UserAction, unavailable : Boolean, view : Option[Any]) : List[Style] = Nil

        def convertActions(faction : Option[F], actions : List[UserAction], then : UserAction => Unit = null) : List[ZOption] = {
            actions./~{ a =>
                def q = {
                    val q = a.question(game)
                    (q == Empty).?(q).|(Div(q)(xlo.fullwidth))
                }
        
                def o = a.option(game) ~ (a match {
                    case UnavailableReasonAction(a, reason) if reason != "" => reason.startsWith("|").?(Break).|(SpaceSpan) ~ Span(Text("(" + reason.substring(reason.startsWith("|").??(1)) + ")"), xstyles.smaller85)
                    case _ => Empty
                })
            
                def wrap(e : Elem) = a match {
                    case a : ElemWrap => a.wrap(game)(e)
                    case _ if o == Empty => Empty
                    case _ => e
                }
                
                a @@ {
                    case u : Unavailable => (u.action, true)
                    case a => (a, false)
                } @@ { case (a, unavailable) =>
        
                    val view = a @@ {
                        case a : ViewObject[_] => Some(a.obj)
                        case _ => None
                    }
                    
                    def ss = styleAction(faction, actions, a, unavailable, view)
                
                    def clear = a match {
                        case _ : NoClear => false
                        case _ => true
                    }
                
                    a match {
                        case a : Hidden => None
                
                        case a : OnClickInfo              => Some(ZOption(q, wrap(OnClick(Div(o, ss))), _ => { onClick(a.param) }, clear))
                        case a : Info                     => Some(ZOption(q, wrap(        Div(o, ss))))
                        case a if unavailable             => Some(ZOption(q, wrap(        Div(o, ss))))
                
                        case a if then == null => None
                
                        case a : Extra[_]                 => Some(ZOption(q, wrap(Div(o, ss)), s => {
                            val v = a.fromAny(s)./~(v => a.validate(v).?(v))
                            if (v.any)
                                then(a.update(v.get))
                            else
                                throw new Error("invalid extra value")
                        }, clear))
                        case a                            => Some(ZOption(q, wrap(OnOverOut(OnClick(Div(o, ss)))), _ => then(a), clear, _ => { updateHighlight(Some(a)) }, _ => { updateHighlight(None) } ))
                    }
        
                }
            }
        }
    
        def zlog(e : Elem, onClick : Any => Unit = null) : List[dom.Node] = {
            logger.zlog(e, onClick)(resources)
        }

        def alog(e : Elem, n : Int, onClick : Any => Unit) : HtmlBlock = {
            val line = e == DoubleLine || e == SingleLine
            
            logger.alog(OnClick(n, Hint("Action #" + n, (line.not.??("  ".pre) ~ e).div(xstyles.hanging)(xstyles.pointer))), onClick)(resources)
        }

        def resize() {
            val width = (dom.window.innerWidth * 1.0).round.toInt
            val height = (dom.window.innerHeight * 1.0).round.toInt
        
            var l = layout(width, height)
            
            l.foreach { case (name, rect, fontSize) =>
                val c = panes(name)
                c.show()
                c.node.style.left = (rect.x * 100.0 / width) + "vw"
                c.node.style.top = (rect.y * 100.0 / height) + "vh"
                c.node.style.width = (rect.width * 100.0 / width) + "vw"
                c.node.style.height = (rect.height * 100.0 / height) + "vh"
                fontSize.foreach { fontSize => c.node.style.fontSize = fontSize + "px" }
            }
            
            panes.keys.toList.diff(l./(_._1)).foreach { name => panes(name).hide() }
        
            updateStatus()
        }

        def start() {
            resize()

            dom.window.onresize = e => resize()
        }
    }
}

sealed trait AskOption {
    val group : String
    val option : String
    val styles : List[String]
}

case class InfoOption(group : String, option : String, styles : List[String]) extends AskOption
case class BasicOption(group : String, option : String, styles : List[String], onSelect : () => Unit) extends AskOption
case class ExtraOption(group : String, option : String, styles : List[String], onSelect : String => Unit) extends AskOption
case class InputOption(group : String, option : String, styles : List[String], onSelect : String => Unit, validate : String => Boolean = _ != "") extends AskOption
case class BreakOption(styles : List[String]) extends AskOption {
    val group = ""
    val option = ""
}

class Logger(logDiv : dom.html.Element, scroll : Boolean) {
    def zlog(e : Elem, onClick : Any => Unit = null)(implicit resources : Resources) : List[dom.Node] = {
        val body = new ElementAttachmentPoint(logDiv)

        val isScrolledToBottom = logDiv.scrollHeight - logDiv.clientHeight <= logDiv.scrollTop + 3

        val r = body.append(e, resources, onClick)
        
        if (isScrolledToBottom || scroll)
            scrollToBottom()

        r.nodes
    }

    def scrollToBottom() {
        logDiv.scrollTop = logDiv.scrollHeight - logDiv.clientHeight
    }

    def clear() {
        val body = new ElementAttachmentPoint(logDiv)

        body.clear()
    }
}

class NewLogger(body : AttachmentPoint, scroll : Boolean) {
    def zlog(e : Elem, onClick : Any => Unit = null)(implicit resources : Resources) : List[dom.Node] = {
        val logDiv = body.parent.parentNode.asInstanceOf[dom.html.Element]
        
        val isScrolledToBottom = logDiv.scrollHeight - logDiv.clientHeight <= logDiv.scrollTop + 3

        val r = body.append(e, resources, onClick)
        
        if (isScrolledToBottom || scroll)
            scrollToBottom()

        r.nodes
    }

    def alog(e : Elem, onClick : Any => Unit = null)(implicit resources : Resources) : HtmlBlock = {
        val logDiv = body.parent.parentNode.asInstanceOf[dom.html.Element]
        
        val isScrolledToBottom = logDiv.scrollHeight - logDiv.clientHeight <= logDiv.scrollTop + 3

        val r = body.append(e, resources, onClick)
        
        if (isScrolledToBottom || scroll)
            scrollToBottom()

        r
    }

    def scrollToBottom() {
        val logDiv = body.parent.parentNode.asInstanceOf[dom.html.Element]

        logDiv.scrollTop = logDiv.scrollHeight - logDiv.clientHeight
    }

    def clear() {
        body.clear()
    }
}


case class ZOption(group : Elem, option : Elem, click : Any => Unit = null, clear : Boolean = true, over : Any => Unit = null, out : Any => Unit = null)

object ZBasic {
    val choice   = $(xstyles.choice, xstyles.chm, xstyles.chp, xstyles.thu, xstyles.xx, xstyles.pointer)
    val info =     $(xstyles.info,   xstyles.chm, xstyles.chp, xstyles.thu, xstyles.xx)
    val infoch =   $(xstyles.info,   xstyles.chm, xstyles.chp, xstyles.thu, xstyles.xx, xstyles.pointer)

    val inputD    = $(xstyles.choice, xstyles.chm, xstyles.chp)
    val inputT    = $(xstyles.choice, xstyles.chm, xstyles.chp, xstyles.larger125)

    def apply(g : Elem, o : Elem, click : () => Unit = null) = 
        if (click != null)
            ZOption(g, OnClick(Div(o, choice)), _ => click())
        else
            ZOption(g, Div(o, info))

    def apply(g : Elem, o : String, click : () => Unit) : ZOption = ZBasic(g, Text(o), click)
    def apply(g : String, o : Elem, click : () => Unit) : ZOption = ZBasic(Text(g), o, click)
    def apply(g : String, o : String, click : () => Unit) : ZOption = ZBasic(Text(g), Text(o), click)
    def apply(g : Elem, o : String) : ZOption = ZBasic(g, Text(o), null)
    def apply(g : String, o : Elem) : ZOption = ZBasic(Text(g), o, null)
    def apply(g : String, o : String) : ZOption = ZBasic(Text(g), Text(o), null)
}

class Asker(actionDiv : dom.html.Element, assets : String => dom.html.Image) {
    def top() {
        actionDiv.scrollTop = 0
    }
    
    def zask(options : List[ZOption])(implicit resources : Resources) {
        val body = new ElementAttachmentPoint(actionDiv)

        body.clear()
        
        var prev : Elem = Empty

        options.foreach { w =>
            var g = w.group
            var o = w.option

            if (w.group == null) 
                throw new Error("Null Group for " + w.option)

            if (g != Empty && g != prev) {
                if (prev != Empty)
                    g = Concat(Break, g)
         
                prev = w.group
            }
            else
                g = Empty

            body.append(g, resources)
            body.append(o, resources, l => { 
                if (w.clear) {
                    body.clear()

                    if (w.out != null) 
                        w.out(l)
                }        

                w.click(l) 
            }, l => w.over(l), l => w.out(l))
        }

    }
 
    def ask(options : List[AskOption]) {
        clear(actionDiv)
        
        var prev : Option[String] = None

        options.foreach { w =>
            var g = Option(w.group)
            var o = w.option
            var s = w.styles

            if (g.any && g != prev) {
                if (prev.any)
                    g = g./("\n" + _)

                prev = Option(w.group)
            }
            else
                g = None

            if (g == Some("")) {
                g = None
                prev = None
            }
            
            g = g./(_.replace("\n", "<br/>"))

            s :+= "hrf-choice---hrf-chm---hrf-chp---hrf-thu---hrf-xx---hrf-pointer"

            w match {
                case w : InfoOption => s :+= "ignore"
                case w : BasicOption if w.onSelect == null => s :+= "ignore"
                case w : BreakOption => s ++= List("ignore", "breaker")
                case _ =>
            }

            if (o.startsWith("@[")) {
                val at = o.substring(0, o.indexOf("]@")).substring("@[".length)
                val r = o.substring(o.indexOf("]@")).substring("]@".length)

                val a = at.split('|').head
                val t = at.split('|').lift(1).|("card")
                
                if (true) {
                    o = "<img class='" + t + "' src='" + assets(a).src + "' />" + r 
                    s :+= t + "-option"
                }
                else {
                    o = "unknown asset [" + a + "]" + r 
                }
            }

            if (o == "---") {
                o = ""
                s = Nil
                g = None
            }
                
            if (g.any)
                actionDiv.appendChild(newDiv("", g.get))
                
            w match {
                case w : BasicOption if w.onSelect != null => 
                    val odiv = newDiv(s.mkString(" "), o)
                    odiv.onclick = (e) => { clear(actionDiv); w.onSelect() }
                    actionDiv.appendChild(odiv)
                
                case w : ExtraOption => 
                    val odiv = newDiv(s.mkString(" "), o)
                    0.until(odiv.childNodes.length)./(odiv.childNodes(_)).foreach { 
                        case e : dom.html.Element =>
                            e.getAttribute("data-sub").?./{s => 
                                e.onclick = (e) => {
                                    e.stopPropagation()
                                    clear(actionDiv)
                                    w.onSelect(s)
                                }
                            }
                        case _ =>
                    }
                    actionDiv.appendChild(odiv)
                
                case w : InputOption => 
                    val tdiv = newDiv(s, "", null)
                    tdiv.style.height = "3ch"
                    tdiv.style.asInstanceOf[scala.scalajs.js.Dynamic].alignContent = "center"

                    val tinp = newInput(s, o)
                    tinp.spellcheck = false
                    tinp.style.fontSize = "150%"

                    tdiv.appendChild(tinp)
                    actionDiv.appendChild(tdiv)

                    def valid() = w.validate(tinp.`value`)

                    def submit() {
                        clear(actionDiv)
                        w.onSelect(tinp.`value`)
                    }
                    
                    val odiv = newDiv(s ++ (o == "").?("ignore"), "Ok", () => { if (valid()) submit() })
                    actionDiv.appendChild(odiv)

                    tinp.focus()

                    def validate() {
                        if (valid())
                            odiv.classList.remove("ignore")
                        else
                            odiv.classList.add("ignore")
                    }
                    
                    tinp.oninput = (e) => {
                        validate()
                    }
                
                    tinp.onkeydown = (e) => {
                        validate()

                        if (e.keyCode == 13)
                            if (valid())
                                submit()
                    }
                
                case w => 
                    val odiv = newDiv(s.mkString(" "), o)
                    actionDiv.appendChild(odiv)
            }

        }
    }
}

class NewAsker(body : Container, assets : String => dom.html.Image) {
    def scrollTop() {
        body.node.scrollTop = 0
        body.node.parentElement.scrollTop = 0
        body.node.parentElement.parentElement.scrollTop = 0
    }
    
    var scroll = true

    def scrollIfNeeded() {
        if (scroll) {
            scrollTop()
            scroll = false
        }
    }

    def zask(options : List[ZOption])(implicit resources : Resources) {
        body.vis()
        body.attach.clear()
        
        var prev : Elem = Empty

        options.foreach { w =>
            var g = w.group
            var o = w.option

            if (w.group == null) 
                throw new Error("Null Group for " + w.option)

            if (g != Empty && g != prev) {
                if (prev != Empty)
                    g = Concat(Break.div(xlo.fullwidth), g)
         
                prev = w.group
            }
            else
                g = Empty

            body.attach.append(g, resources)
            body.attach.append(o, resources, l => { 
                if (w.clear) {
                    scroll = true
                    
                    body.invis()

                    if (w.out != null) 
                        w.out(l)
                }        

                w.click(l) 
            }, l => w.over(l), l => w.out(l))
        }

    }
}
