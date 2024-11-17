package hrf.loader
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

import scalajs.js
import scalajs.js.typedarray._
import scalajs.js.timers.setTimeout

import org.scalajs.dom
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

    private var waitlist = $[(() => Unit, $[String])]()

    def load(url : String)(then : T => Unit) {
        wait(List(url)) {
            if (!error(url))
                then(get(url))
        }
    }

    def wait(urls : $[String])(then : => Unit) {
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
        warn("load error " + url)
        results += url -> Error
        check()
    }

    var onLoad : (String, T) => Unit = null

    def put(url : String, result : T) {
        results += url -> Done(result)

        if (onLoad != null)
            onLoad(url, result)

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

    def queue(url : String) {
        if (!registered(url)) {
            start(url)
            process(url)
        }
    }

    def queue(urls : List[String]) {
        urls.foreach(queue)
    }

    def process(url : String)
}

trait ImageWrapper {
    def load(onLoad : html.Image => Unit, onFail : String => Unit)
    def get : html.Image
}

class RawImageWrapper(img : html.Image) extends ImageWrapper {
    def load(onLoad : html.Image => Unit, onFail : String => Unit) {
        onLoad(img)
    }
    def get : html.Image = img
}

class BlobImageWrapper(url : String, private var blob : dom.Blob) extends ImageWrapper {
    private var img : html.Image = null
    private var loaded = false

    def load(onLoad : html.Image => Unit, onFail : String => Unit) {
        if (loaded)
            return onLoad(img)

        if (img == null)
            img = new njs.Image()

        img.addEventListener("onerror", (e : dom.ErrorEvent) => {
            +++("blob to image failed " + url)

            onFail(url)
        })

        img.addEventListener("onload", (e : dom.Event) => {
            loaded = true

            onLoad(img)
        })

        if (blob != null) {
            img.src = dom.URL.createObjectURL(blob)
            blob = null
        }
    }

    def get : html.Image = {
        if (img == null)
            load(_ => {}, _ => {})

        img
    }
}

class WrappedEmbeddedImageLoader(url2id : String => String) extends Loader[ImageWrapper] {
    def process(url : String) {
        put(url, new RawImageWrapper(dom.document.getElementById(url2id(url)).asInstanceOf[html.Image]))
    }
}

class CachedBlobImageLoader(id : String) extends Loader[ImageWrapper] {
    val open = dom.window.caches.toOption.get.open(id)

    var requests = 0

    def process(url : String) {
        process(url, 0)
    }

    def process(url : String, tries : Int) {
        open.then { cache =>
            cache.`match`(url).then({ response =>
                if (js.isUndefined(response)) {
                    try {
                        if (tries == 0) {
                            cache.add(url).then { x =>
                                setTimeout(tries * 400) {
                                    process(url, tries + 1)
                                }
                                null
                            }
                        }
                        else {
                            setTimeout(tries * 400) {
                                process(url, tries + 1)
                            }
                        }
                    }
                    catch {
                        case e : Throwable =>
                            setTimeout(1000) {
                                process(url, 0)
                            }
                    }
                }
                else {
                    response.toOption.get.blob().then { blob : dom.Blob =>
                        put(url, new BlobImageWrapper(url, blob))
                        null
                    }
                }
                null
            })
            null
        }
    }
}

class CachedImageLoader(id : String) extends Loader[html.Image] {
    val open = dom.window.caches.toOption.get.open(id)

    var requests = 0

    def process(url : String) {
        process(url, 0)
    }

    def process(url : String, tries : Int) {
        open.then { cache =>
            cache.`match`(url).then({ response =>
                if (js.isUndefined(response)) {
                    try {
                        if (tries == 0) {
                            cache.add(url).then { x =>
                                setTimeout(tries * 400) {
                                    process(url, tries + 1)
                                }
                                null
                            }
                        }
                        else {
                            setTimeout(tries * 400) {
                                process(url, tries + 1)
                            }
                        }
                    }
                    catch {
                        case e : Throwable =>
                            setTimeout(1000) {
                                process(url, 0)
                            }
                    }
                }
                else {
                    response.toOption.get.blob().then { blob : dom.Blob =>
                        var img = new njs.Image()
                        img.onerror = (e : dom.ErrorEvent) => fail(url)
                        img.onload = (e : dom.Event) => put(url, img.asInstanceOf[html.Image])
                        img.src = dom.URL.createObjectURL(blob)
                        null
                    }
                }
                null
            })
            null
        }
    }
}


class CachedStringLoader(id : String) extends Loader[String] {
    val open = dom.window.caches.toOption.get.open(id)

    def process(url : String) {
        process(url, 0)
    }

    def process(url : String, tries : Int) {
        open.then { cache =>
            cache.`match`(url).then({ response =>
                if (js.isUndefined(response)) {
                    try {
                        if (tries == 0) {
                            cache.add(url).then { x =>
                                setTimeout(tries * 400) {
                                    process(url, tries + 1)
                                }
                                null
                            }
                        }
                        else {
                            setTimeout(tries * 400) {
                                process(url, tries + 1)
                            }
                        }
                    }
                    catch {
                        case e : Throwable =>
                            setTimeout(1000) {
                                process(url, 0)
                            }
                    }
                }
                else {
                    response.toOption.get.text().then {
                        s => put(url, s)
                        null
                    }
                }
                null
            })
            null
        }
    }
}

object ImageLoader extends Loader[html.Image] {
    def process(url : String) {
        if (js.typeOf(dom.URL) == "undefined" || js.typeOf(dom.window.asInstanceOf[js.Dynamic].FormData) != "undefined") {
            var img = new njs.Image()
            img.crossOrigin = "Anonymous"
            img.onerror = (e : dom.ErrorEvent) => fail(url)
            img.onload = (e : dom.Event) => put(url, img.asInstanceOf[html.Image])
            img.src = url
        }
        else {
            var xhr = new dom.XMLHttpRequest()
            xhr.onerror = (e : dom.ProgressEvent) => fail(url)
            xhr.onload = (e : dom.Event) => {
                var url = dom.URL.createObjectURL(xhr.response.asInstanceOf[dom.Blob])
                var img = new njs.Image()
                img.onload = (e : dom.Event) => {
                    dom.URL.revokeObjectURL(url)
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

object XmlLoader extends Loader[dom.Node] {
    def process(url : String) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => put(url, (new dom.DOMParser).parseFromString(xhr.response.asInstanceOf[String], dom.MIMEType.`text/xml`))
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
        xhr.setRequestHeader("Access-Control-Allow-Origin", "*")
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
