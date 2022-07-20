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

import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.URIUtils
import scala.scalajs.js.timers.setTimeout

package object web {
    implicit class Ascii(val s : String) {
        def ascii = s.filter(c => c >= 32 && c < 128)
        def asciiplus = s.filter(c => (c >= 32 && c < 128) || (c > 158 && c < 256 && c.isLetter))
        def safe = ascii.filter(_ != '<').filter(_ != '>').filter(_ != '"').filter(_ != '\\')
        def safeplus = asciiplus.filter(_ != '<').filter(_ != '>').filter(_ != '"').filter(_ != '\\')
        def sanitize(n : Int) = s.safeplus.trim.take(n)
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
        ===("fail " + url)
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
        val sheet = dom.document.styleSheets(0).asInstanceOf[dom.raw.CSSStyleSheet]
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
