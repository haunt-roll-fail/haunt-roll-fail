package hrf.loader
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
import scala.scalajs.js.typedarray._

import org.scalajs.dom
import org.scalajs.dom.raw
import org.scalajs.dom.html

package njs {
    @js.native
    @js.annotation.JSGlobal
    class Image extends dom.html.Image {
        var crossOrigin : String = js.native
        var onerror : js.Function1[dom.ErrorEvent, _] = js.native
    }
}

sealed trait LoadResult[+T]
case object Loading extends LoadResult[Nothing]
case object Error extends LoadResult[Nothing]
case class Done[T](result : T) extends LoadResult[T]

abstract class Loader[T] {
    private var results = Map[String, LoadResult[T]]()
    
    private var waitlist = List[(() => Unit, List[String])]()
    
    def load(url : String)(then : T => Unit) {
        wait(List(url)) {
            if (!error(url))
                then(get(url))
        }
    }
    
    def wait(urls : List[String])(then : => Unit) {
        urls.foreach(queue)

        val left = urls.filter(u => results(u) == Loading)

        lazy val t = then
        val tt = () => t

        if (left.isEmpty)
            tt()
        else
            waitlist +:= tt -> left
    }

    def check() {
        var (left, done) = waitlist.partition(_._2.exists(u => results(u) == Loading))
        waitlist = left
        done.foreach(_._1())
    }

    def fail(url : String) {
        ===("load error " + url)
        results += url -> Error
        check()
    }

    def put(url : String, result : T) {
        results += url -> Done(result)
        check()
    }
    
    def start(url : String) {
        results += url -> Loading
    }

    def registered(url : String) = results.contains(url)
 
    def ready(url : String) = results.get(url) match {
        case None => false
        case Some(Loading) => false
        case _ => true
    }

    def error(url : String) = results.get(url) == Some(Error)

    def get(url : String) = results.get(url) match {
        case Some(Done(v)) => v
        case _ => throw new Exception("url not loaded " + url)
    }
    
    def has(url : String) = results.get(url) match {
        case Some(Done(_)) => true
        case _ => false
    }
    
    def queue(url : String) : () => T = {
        if (!registered(url)) {
            start(url)
            process(url)
        }

        () => get(url)
    }
    
    def queue(urls : List[String]) {
        urls.foreach(queue)
    }

    def process(url : String) 
}

object ImageLoader extends Loader[html.Image] {
    def process(url : String) {
        if (js.typeOf(raw.URL) == "undefined" || js.typeOf(dom.window.asInstanceOf[js.Dynamic].FormData) != "undefined") {
            // Doesn't work in IE 10
            var img = new njs.Image()
            img.crossOrigin = "Anonymous"
            img.onerror = (e : dom.ErrorEvent) => fail(url)
            img.onload = (e : dom.Event) => put(url, img.asInstanceOf[html.Image])
            img.src = url
        }
        else {
            // Workaround for IE 10
            var xhr = new dom.XMLHttpRequest()
            xhr.onerror = (e : dom.ProgressEvent) => fail(url)
            xhr.onload = (e : dom.Event) => {
                var url = raw.URL.createObjectURL(xhr.response.asInstanceOf[dom.Blob])
                var img = new njs.Image()
                img.onload = (e : dom.Event) => {
                    raw.URL.revokeObjectURL(url)
                    put(url, img.asInstanceOf[html.Image])
                }
                img.onerror = (e : dom.ErrorEvent) => fail(url)
                img.src = url
            }
            xhr.open("GET", url, true)
            xhr.responseType = "blob"
            xhr.send(null)
        }
    }
}

class EmbeddedImageLoader(url2id : String => String) extends Loader[html.Image] {
    def process(url : String) {
        put(url, dom.document.getElementById(url2id(url)).asInstanceOf[html.Image])
    }
}

object StringLoader extends Loader[String] {
    def process(url : String) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => put(url, xhr.response.asInstanceOf[String])
        xhr.open("GET", url, true)
        xhr.responseType = "text"
        xhr.send(null)
    }
}

object XmlLoader extends Loader[raw.Node] {
    def process(url : String) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => put(url, (new raw.DOMParser).parseFromString(xhr.response.asInstanceOf[String], dom.MIMEType.`text/xml`))
        xhr.open("GET", url, true)
        xhr.responseType = "text"
        xhr.send(null)
    }
}

object JsonLoader extends Loader[js.Dynamic] {
    def process(url : String) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => put(url, js.JSON.parse(xhr.response.asInstanceOf[String]).asInstanceOf[js.Dynamic])
        xhr.open("GET", url, true)
        xhr.responseType = "text"
        xhr.send(null)
    }
}

object BinaryLoader extends Loader[ArrayBuffer] {
    def process(url : String) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => put(url, xhr.response.asInstanceOf[ArrayBuffer])
        xhr.open("GET", url, true)
        xhr.responseType = "arraybuffer"
        xhr.send(null)
    }
}

object DataUrlLoader extends Loader[String] {
    def process(url : String) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => {
            val r = new dom.FileReader()
            r.onload = (e : dom.Event) => put(url, r.result.asInstanceOf[String])
            r.readAsDataURL(xhr.response.asInstanceOf[dom.Blob])
        }
        xhr.open("GET", url, true)
        xhr.responseType = "blob"
        xhr.send(null)
    }
}

object Float32ArrayLoader extends Loader[Float32Array] {
    def process(url : String) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => put(url, new Float32Array(xhr.response.asInstanceOf[ArrayBuffer]))
        xhr.open("GET", url, true)
        xhr.responseType = "arraybuffer"
        xhr.send(null)
    }
}

object Uint16ArrayLoader extends Loader[Uint16Array] {
    def process(url : String) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => put(url, new Uint16Array(xhr.response.asInstanceOf[ArrayBuffer]))
        xhr.open("GET", url, true)
        xhr.responseType = "arraybuffer"
        xhr.send(null)
    }
}


