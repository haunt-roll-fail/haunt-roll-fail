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

import org.scalajs.dom
import org.scalajs.dom.html

import scalajs.js.URIUtils
import scalajs.js.timers.setTimeout

package object web {
    def time() = (new scalajs.js.Date()).getTime()

    var tt = 0.0

    def timed[T](min : Int)(s : Any*)(f : => T) : T = {
        val start = time()
        val result : T = f
        val end = time()
        val delta = end - start

        if (delta >= min) {
            tt += delta
            warn("t:", s.mkString(" "), "-", delta, "ms", "(" + tt + " total)")
        }

        result
    }

    def timed2[T](f : => T) : (T, Double) = {
        val start = time()
        val result : T = f
        val end = time()
        val delta = end - start

        (result, delta)
    }

    implicit class Ascii(val s : String) extends AnyVal {
        def ascii = s.filter(c => c >= 32 && c < 128)
        def asciiplus = s.filter(c => (c >= 32 && c < 128) || (c > 158 && c < 256 && c.isLetter))
        def safe = ascii.filter(_ != '<').filter(_ != '>').filter(_ != '"').filter(_ != '\\')
        def safeplus = asciiplus.filter(_ != '<').filter(_ != '>').filter(_ != '"').filter(_ != '\\')
        def sanitize(n : Int) = s.safeplus.trim.take(n)
    }

    implicit class DomCssRuleUtils(val r : dom.CSSStyleRule) extends AnyVal {
        def style = r.asInstanceOf[dom.CSSStyleRule].style
        def selectorText = r.asInstanceOf[dom.CSSStyleRule].selectorText.as[String].|(null)
    }

    class History(base : String, query : String, hash : String) {
        private val gizmo = "gizmo"
        private var states : $[(String, () => Unit)] = $
        private var processing : Boolean = false

        dom.window.history.replaceState(gizmo, "")
        dom.window.history.pushState("initilaize 1", "")
        dom.window.history.forward()

        var ignore : Int = 0

        dom.window.onpopstate = (e) => {
            if (ignore > 0)
                ignore -= 1
            else
                popState()
        }

        def popState() {
            if (processing.not) {
                if (dom.window.history.state != gizmo) {
                    dom.window.history.back()
                    ignore += 1
                }

                processing = true

                val last = states.take(1)
                states = states.drop(1)

                val v = random()

                val old = states.lift(0)./(_._1).|(base)

                setTimeout(100) {
                    dom.window.history.pushState(v, "",  (old == null).?(null).|(old + query + hash))
                    dom.window.history.forward()
                }

                last./{ (url, action) =>
                    action()
                }

                processing = false
            }
        }

        def destroy(n : Int) {
            states = states.drop(n)
        }

        def pushState(url : String, onPop : () => Unit) {
            states +:= url -> onPop

            dom.window.history.pushState(url, "", (url == null).?(null).|(url + query + hash))
        }
    }

    def appendStylesheet(url : String) {
        val styleSheet = dom.document.createElement("link").asInstanceOf[html.Link]
        styleSheet.rel = "stylesheet"
        styleSheet.`type` = "text/css"
        styleSheet.href = url
        dom.document.head.appendChild(styleSheet)
    }

    def getElem(k : String) = dom.document.getElementById(k).asInstanceOf[html.Element]
    def getAsset(k : String) = dom.document.getElementById(k).asInstanceOf[html.Image]

    def fail(url : String) {
        +++("fail", url)
    }

    def post(url : String, data : String)(then : String => Unit) {
        var n = 3
        def retry() {
            postF(url, data)(then) {
                n += 1
                setTimeout(n * 1000) {
                    retry()
                }
            }
        }
        retry()
    }

    def postF(url : String, data : String)(then : String => Unit)(fail : => Unit) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail
        xhr.onload = (e : dom.Event) => if (xhr.status < 400) then(xhr.response.asInstanceOf[String]) else fail
        xhr.open("POST", url, true)
        xhr.responseType = "text"
        xhr.send(data)
    }

    def get(url : String)(then : String => Unit) {
        var n = 3
        def retry() {
            getF(url)(then) {
                n += 1
                setTimeout(n * 1000) {
                    retry()
                }
            }
        }

        retry()
    }

    def getF(url : String)(then : String => Unit)(fail : => Unit) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail
        xhr.onload = (e : dom.Event) => if (xhr.status < 400) then(xhr.response.asInstanceOf[String]) else fail
        xhr.open("GET", url, true)
        xhr.responseType = "text"
        xhr.send(null)
    }

    def clipboard(text : String) : Boolean = {
        val cb = getElem("clipboard").asInstanceOf[html.TextArea]
        cb.value = text
        cb.focus()
        cb.select()
        try {
            dom.document.execCommand("copy")
        } catch {
            case e : Throwable => false
        } finally {
        }
    }

    def addCSSRule(rule : String) {
        val sheet = dom.document.styleSheets(0).as[dom.CSSStyleSheet].get
        sheet.insertRule(rule, sheet.cssRules.length)
    }

    def setCookie(name : String, value : String, daysToLive : Option[Int]) {
        var cookie = name + "=" + URIUtils.encodeURIComponent(value)

        if (daysToLive.any)
            cookie += "; max-age=" + (daysToLive.get*24*60*60)

        dom.document.cookie = cookie
    }

    def getCookie(name : String, default : String) : String = {
        var cc = dom.document.cookie.split(';')

        cc.foreach { c =>
            val p = c.split('=')
            if (p(0).trim == name)
                return URIUtils.decodeURIComponent(p(1))
        }

        default
    }

    object Local {
        def set(name : String, value : String) : Unit = dom.window.localStorage.setItem(name, value)

        def get(name : String, default : String) : String = dom.window.localStorage.getItem(name).some.|(default)

        def list(prefix : String) : $[String] = 0.until(dom.window.localStorage.length)./(i => dom.window.localStorage.key(i)).%(_.startsWith(prefix))

        def clear() : Unit = dom.window.localStorage.clear()
    }

    def newDiv(cl : String, content : String, click : () => Unit = null) = {
        val p = dom.document.createElement("div").asInstanceOf[html.Div]
        p.className = cl
        p.innerHTML = content
        if (click != null)
            p.onclick = (e) => click()
        p
    }

    def newDiv(cl : List[String], content : String, click : () => Unit) = {
        val p = dom.document.createElement("div").asInstanceOf[html.Div]
        p.className = cl.mkString(" ")
        p.innerHTML = content
        if (click != null)
            p.onclick = (e) => click()
        p
    }

    def newInput(cl : List[String], content : String, click : () => Unit = null) = {
        val p = dom.document.createElement("input").asInstanceOf[html.Input]
        p.`type` = "text"
        p.className = cl.mkString(" ")
        p.value = content
        if (click != null)
            p.onclick = (e) => click()
        p
    }

    def clear(e : dom.Element) {
        while (e.hasChildNodes())
            e.removeChild(e.lastChild)
    }

    def hide(e : html.Element) {
        e.style.display = "none"
    }

    def show(e : html.Element) {
        e.style.display = ""
    }

    def cont(e : dom.Element) = e.parentNode.parentNode.asInstanceOf[html.Element]
}
