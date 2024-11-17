package hrf.canvas
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

import scalajs._

package njs {
    @js.native
    @js.annotation.JSGlobal
    object G_vmlCanvasManager extends js.Object {
        def initElement(canvas : html.Canvas) : Unit = js.native
    }
}

class Bitmap(w : Int, h : Int) {
    val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]

    if (js.typeOf(canvas.asInstanceOf[js.Dynamic].getContext) == "undefined")
        if (js.typeOf(njs.G_vmlCanvasManager) != "undefined")
            njs.G_vmlCanvasManager.initElement(canvas)

    if (js.typeOf(canvas.asInstanceOf[js.Dynamic].getContext) == "undefined")
        throw new Error("Could not initialize `canvas` element")

    canvas.width = w
    canvas.height = h

    def width = canvas.width
    def height = canvas.height

    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    def toDataURL : String = canvas.toDataURL("image/png")

    def data : dom.ImageData = context.getImageData(0, 0, canvas.width, canvas.height)
}

class CachedBitmap(val node : dom.html.Element) {
    private var bitmap : Bitmap = null

    def get(w : Int, h : Int) = {
        if (bitmap == null || bitmap.width != w || bitmap.height != h) {
            if (bitmap != null)
                if (bitmap.canvas.parentNode != null)
                    bitmap.canvas.parentNode.removeChild(bitmap.canvas)

            bitmap = new Bitmap(w, h)
        }

        node.appendChild(bitmap.canvas)

        bitmap
    }
}
