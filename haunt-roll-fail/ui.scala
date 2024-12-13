package hrf.ui
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
import hrf.web._
import hrf.elem._
import hrf.html._
import hrf.meta._

import org.scalajs.dom

import scala.collection.mutable


case class Rect(x : Int, y : Int, width : Int, height : Int) extends Record

case class PanePlacement(name : String, rect : Rect, fontSize : Option[Double]) extends Record


trait BaseUI {
    val mmeta : MetaBase

    def create(uir : hrf.html.ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) : mmeta.gaming.GameUI
}


sealed trait AskOption {
    val group : String
    val option : String
    val styles : List[String]
}

case class BasicOption(group : String, option : String, styles : List[String], onSelect : () => Unit) extends AskOption
case class InputOption(group : String, option : String, styles : List[String], onSelect : String => Unit, validate : String => Boolean = _ != "") extends AskOption


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


class NewLogger(body : AttachmentPoint, appendForcesScrollToEnd : Boolean) {
    val logDiv = body.parent.parentNode.asInstanceOf[dom.html.Element]

    def isScrolledToBottom = logDiv.scrollHeight - logDiv.clientHeight <= logDiv.scrollTop + 3

    def alog(e : Elem, onClick : Any => Unit = null, scrollIfAtEnd : Boolean = true)(implicit resources : Resources) : HtmlBlock = {
        val r = body.append(e, resources, onClick)

        if (scrollIfAtEnd || isScrolledToBottom)
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


trait LazyBlock {
    def get : HtmlBlock
}

class LogEntry(val logger : LazyLogger, val elem : Elem, val resources : Resources, val onClick : Any => Unit, var block : |[HtmlBlock]) extends LazyBlock {
    def get = block.|{ logger.force(); block.get }
}

class LazyLogger(val body : AttachmentPoint) {
    val logDiv = body.parent.parentNode.asInstanceOf[dom.html.Element]

    def isScrolledToBottom = logDiv.scrollHeight - logDiv.clientHeight <= logDiv.scrollTop + 3

    def scrollToBottom() {
        val logDiv = body.parent.parentNode.asInstanceOf[dom.html.Element]

        logDiv.scrollTop = logDiv.scrollHeight - logDiv.clientHeight + 1
    }

    def keepScrolled() {
        dom.window.requestAnimationFrame(e => {
            scrollToBottom()

            keepScrolled()
        })
    }

    if (hrf.HRF.flag("screenshot"))
        keepScrolled()

    var entries : $[LogEntry] = $

    def alog(elem : Elem, onClick : Any => Unit = null, delayed : Boolean)(implicit resources : Resources) : LogEntry = {
        val entry = new LogEntry(this, elem, resources, onClick, None)

        entries :+= entry

        if (delayed.not)
            force()

        entry
    }

    def force() {
        entries.foreach { e =>
            if (e.block.none)
                e.block = Some(body.append(e.elem, e.resources, e.onClick))
        }

        scrollToBottom()
    }

    def blog()(implicit resources : Resources) : Container = {
        body.appendContainer(Content, resources, null)
    }

}


case class ZOption(group : Elem, option : Elem, click : Any => Unit = null, clear : Boolean = true, over : Any => Unit = null, out : Any => Unit = null) extends GoodMatch

object ZBasic {
    val choice   = $(xstyles.choice, xstyles.xx, xstyles.chm, xstyles.chp, xstyles.thuc, xlo.fullwidth, xstyles.thumargin, xlo.pointer)
    val info     = $(xstyles.info,   xstyles.xx, xstyles.chm, xstyles.chp, xstyles.thuc, xlo.fullwidth, xstyles.thumargin)
    val infoch   = $(xstyles.info,   xstyles.xx, xstyles.chm, xstyles.chp, xstyles.thuc, xlo.fullwidth, xstyles.thumargin, xlo.pointer)

    val inputD   = $(xstyles.choice, xstyles.chm, xstyles.chp)
    val inputT   = $(xstyles.choice, xstyles.chm, xstyles.chp, xstyles.fontSize100)

    def apply(g : Elem, o : Elem, click : () => Unit, styles : $[Style] = null) =
        if (click != null)
            ZOption(g, OnClick(Div(o, Option(styles).|(choice))), _ => click())
        else
            ZOption(g, Div(o, Option(styles).|(info)))

    def apply(g : Elem, o : String, click : () => Unit) : ZOption = ZBasic(g, Text(o), click)
    def apply(g : String, o : Elem, click : () => Unit) : ZOption = ZBasic(Text(g), o, click)
    def apply(g : String, o : String, click : () => Unit) : ZOption = ZBasic(Text(g), Text(o), click)
    def apply(g : Elem, o : Elem) : ZOption = ZBasic(g, o, null)
    def apply(g : Elem, o : String) : ZOption = ZBasic(g, Text(o), null)
    def apply(g : String, o : Elem) : ZOption = ZBasic(Text(g), o, null)
    def apply(g : String, o : String) : ZOption = ZBasic(Text(g), Text(o), null)
}


class Asker(val body : AttachmentPoint, assets : String => dom.html.Image) {
    val actionDiv = body.parent

    val cont = body.appendContainer(Content.div(xlo.flexhcenter)(xlo.fullwidth)(xstyles.bottomPadding).div(xlo.column)(xlo.fullheight), Resources(ImageResources(Map(), Map(), hrf.HRF.imageCache), () => Map()))

    var scroll = true

    def top() {
        actionDiv.scrollTop = 0
    }

    def zask(options : List[ZOption])(implicit resources : Resources) {
        cont.vis()
        cont.attach.clear()

        var prev : Elem = Empty

        options.foreach { w =>
            var g = w.group
            var o = w.option

            if (w.group == null)
                throw new Error("Null Group for " + w.option)

            if (g != Empty && g != prev) {
                g = Concat(Break.div(xlo.fullwidth), g)

                prev = w.group
            }
            else
                g = Empty

            if (g != Empty)
                cont.attach.append(g.div(xlo.fullwidth), resources)

            cont.attach.append(o, resources, l => {
                if (w.clear) {
                    scroll = true

                    cont.invis()

                    if (w.out != null)
                        w.out(l)
                }

                w.click(l)
            }, l => w.over(l), l => w.out(l))
        }
    }

    def iask(options : List[AskOption]) {
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

            s :+= "hrf-choice---hrf-chm---hrf-chp---hrf-thu---hrf-xx---xlo-pointer"

            w match {
                case w : BasicOption if w.onSelect == null => s :+= "ignore"
                case _ =>
            }

            if (g.any)
                actionDiv.appendChild(newDiv("xlo-fullwidth---xlo-pre", g.get))

            w match {
                case w : BasicOption if w.onSelect != null =>
                    val odiv = newDiv(s.mkString(" "), o)
                    odiv.onclick = (e) => { clear(actionDiv); w.onSelect() }
                    actionDiv.appendChild(odiv)

                case w : InputOption =>
                    val tdiv = newDiv(s, "", null)
                    tdiv.style.height = "3ch"
                    tdiv.style.asInstanceOf[scalajs.js.Dynamic].alignContent = "center"

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


class NewAsker(body : Container, assets : String => dom.html.Image, header : |[String] = None) {
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

    def clear() {
        scroll = true
        body.invis()
    }

    def zask(options : List[ZOption])(implicit resources : Resources) {
        body.vis()
        body.attach.clear()
        header.foreach { h => body.attach.append(Header(1, h, $(ExternalStyle("screen-reader"))), resources) }

        var prev : Elem = "...".txt

        options.foreach { w =>
            var g = w.group
            var o = w.option

            if (w.group == null)
                throw new Error("Null Group for " + w.option)

            if (g != Empty && g != prev) {
                if (prev != "...".txt)
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
