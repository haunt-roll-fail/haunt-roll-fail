package hrf.ui.sprites
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

case class XY(x : Double, y : Double)

case class Rectangle(x : Double, y : Double, width : Double, height : Double) {
    def move(p : XY) = Rectangle(x + p.x, y + p.y, width, height)
    def scale(s : Double) = Rectangle(x - width / 2 + width / 2 * s, y - height / 2 + height / 2 * s, width * s, height * s)
}


trait Image {
    def width : Int
    def height : Int
    def toCanvasImage : CanvasImage
    def renderable : dom.html.Element
    def ready : Boolean
}

class RawImage(image : dom.html.Image) extends Image {
    def ready = image.complete
    def width = image.width
    def height = image.height
    def renderable = image

    lazy val toCanvasImage = {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]

        canvas.width = width
        canvas.height = height

        val result = new CanvasImage(canvas)

        result.context.drawImage(image, 0, 0)

        result
    }
}

class RawImageRotated90(image : dom.html.Image) extends Image {
    def ready = image.complete
    def width = image.height
    def height = image.width
    def renderable = toCanvasImage.renderable

    lazy val toCanvasImage = {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]

        canvas.width = width
        canvas.height = height

        val result = new CanvasImage(canvas)

        result.context.translate(width / 2, height / 2)
        result.context.rotate(math.Pi / 2)
        result.context.translate(-width / 2, -height / 2)
        result.context.drawImage(image, 0, 0)

        result
    }
}

class RawImageRotated180(image : dom.html.Image) extends Image {
    def ready = image.complete
    def width = image.width
    def height = image.height
    def renderable = toCanvasImage.renderable

    lazy val toCanvasImage = {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]

        canvas.width = width
        canvas.height = height

        val result = new CanvasImage(canvas)

        result.context.translate(width / 2, height / 2)
        result.context.rotate(math.Pi)
        result.context.translate(-width / 2, -height / 2)
        result.context.drawImage(image, 0, 0)

        result
    }
}

class RawImageRotated270(image : dom.html.Image) extends Image {
    def ready = image.complete
    def width = image.height
    def height = image.width
    def renderable = toCanvasImage.renderable

    lazy val toCanvasImage = {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]

        canvas.width = width
        canvas.height = height

        val result = new CanvasImage(canvas)

        result.context.translate(width / 2, height / 2)
        result.context.rotate(-math.Pi / 2)
        result.context.translate(-height / 2, -width / 2)
        result.context.drawImage(image, 0, 0)

        result
    }
}


class CanvasImage(image : dom.html.Canvas) extends Image {
    def ready = true
    def width = image.width
    def height = image.height
    def renderable = image
    def toCanvasImage = this

    def context = image.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    def data = context.getImageData(0, 0, image.width, image.height)
    def toDataURL : String = image.toDataURL("image/png")
}

trait Filter {
    def transform(image : Image) : Image
}


case class ImageRect(image : Image, rect : Rectangle, alpha : Double)
object ImageRect {
    def apply(image : dom.html.Image, dx : Int, dy : Int) : ImageRect = ImageRect(new RawImage(image), Rectangle(-dx, -dy, image.width, image.height), 1.0)

    def apply(image : dom.html.Image, dx : Int, dy : Int, scale : Double) : ImageRect = ImageRect(new RawImage(image), Rectangle(-dx*scale, -dy*scale, image.width*scale, image.height*scale), 1.0)
}

case class Sprite(images : $[ImageRect], hitboxes : $[Rectangle], tag : |[Any] = None)

trait Renderable {
    def render(g : dom.CanvasRenderingContext2D) : Unit
}

case class RenderableSprite(sprite : Sprite, x : Double, y : Double, z : Double, scale : Double) extends Renderable {
    def render(g : dom.CanvasRenderingContext2D) {
        sprite.images.foreach { ir =>
            g.globalAlpha = ir.alpha
            if (ir.image.ready)
                g.drawImage(ir.image.renderable, x + ir.rect.x * scale, y + ir.rect.y * scale, ir.rect.width * scale, ir.rect.height * scale)
        }

        if (hrf.HRF.flag("hitboxes")) {
            g.globalAlpha = 0.5
            g.fillStyle = "#00ff00"

            sprite.hitboxes.foreach { h =>
                g.fillRect(x + h.x * scale, y + h.y * scale, h.width * scale, h.height * scale)
            }

            g.globalAlpha = 1.0
        }
    }
}

trait Layer {
    def renderables : $[RenderableSprite]

    def pick(x : Double, y : Double) : $[Any] = {
        renderables.%(_.sprite.tag.any)./~ { r =>
            r.sprite.hitboxes.flatMap { box =>
                if (x >= r.x + box.x * r.scale && x <= r.x + box.x * r.scale + box.width  * r.scale
                 && y >= r.y + box.y * r.scale && y <= r.y + box.y * r.scale + box.height * r.scale)
                    r.sprite.tag
                else
                    None
            }
        }
    }
}

class OrderedLayer extends Layer {
    private var list : $[RenderableSprite] = $

    def add(sprite : Sprite, scale : Double = 1.0, z : Double = 0)(x : Double, y : Double) {
        list :+= RenderableSprite(sprite, x, y, z, scale)
    }

    def clear() {
        list = $
    }

    def empty = list.none

    def renderables = list
}

class HitLayer[K](regions : Regions[K]) extends Layer {
    def renderables = $

    override def pick(x : Double, y : Double) : $[Any] = regions.regionAt(XY(x, y)).$
}

case class FitOptions(tries : Int = 80, kX : Double = 1.0, kY : Double = 1.0, threshold : Double = 0.23)

class FitLayer[K, T](regions : Regions[K], options : FitOptions) extends Layer {
    private var prev = Map[T, (K, RenderableSprite)]()
    private var curr = Map[T, (K, RenderableSprite)]()

    def addFixed(key : K, tag : T, z : Double = 0)(sprite : Sprite, scale : Double = 1.0)(x : Double, y : Double) {
        curr += tag -> (key -> RenderableSprite(sprite, x, y, z, scale))
    }

    def addHint(key : K, tag : T)(x : Double, y : Double) {
        curr += tag -> (key -> RenderableSprite(new Sprite($, $, None), x, y, 0, 1.0))
    }

    def addFloat(key : K, tag : T, z : Double = 0)(sprite : Sprite, scale : Double = 1.0) : XY = {
        val old = prev.get(tag).%(_._1 == key)./(_._2)./(o => XY(o.x, o.y))

        def penalty(p : XY) = {
            curr.values./{ case (k, r) =>
                if (k != tag) {
                    r.sprite.hitboxes./{ u =>
                        val d = u.move(XY(r.x, r.y))
                        sprite.hitboxes./{ v =>
                            val o = v.move(p)
                            val w = min(o.x + o.width, d.x + d.width) - max(o.x, d.x)
                            val h = min(o.y + o.height, d.y + d.height) - max(o.y, d.y)
                            val s = (w > 0 && h > 0).?(w * h).|(0)
                            s * (1.0 / (o.width * o.height) + 1.0 / (d.width * d.height))
                        }.sum
                    }.sum
                }
                else
                    0
           }.sum
        }

        old.%(o => regions.regionAt(o).has(key)).%(s => penalty(s) < options.threshold).foreach { s =>
            curr += tag -> (key -> RenderableSprite(sprite, s.x, s.y, z, scale))
            return XY(s.x, s.y)
        }

        val ctr = old.|(regions.center(key))
        val rnd = 1.to(options.tries)./(_ => regions.random(key))
        val r = rnd./(p => p -> ((p.x - ctr.x) * (p.x - ctr.x) * options.kX + (p.y - ctr.y) * (p.y - ctr.y) * options.kY) * (0.4 + 0.6 * random())).sortBy(_._2).map(_._1).minBy(penalty)
        val s = old.%(o => regions.regionAt(o).has(key)).%(o => penalty(o) < penalty(r) + options.threshold).|(r)

        curr += tag -> (key -> RenderableSprite(sprite, s.x, s.y, z, scale))

        XY(s.x, s.y)
    }

    def renderables = {
        curr.values./(_._2).$.sortBy(s => (s.z, s.y, s.x))
    }

    def flush() {
        prev = curr
        curr = Map()
    }
}

trait Regions[K] {
    def regionAt(p : XY) : |[K]
    def center(k : K) : XY
    def random(k : K) : XY
}

class IndexedImageRegions[K](image : Image, offsetX : Int, offsetY : Int, centers : Map[K, XY]) extends Regions[K] {
    if (image.ready.not)
        throw new Error("image.ready.not")

    if (image.width <= 0)
        throw new Error("image.width <= 0")

    if (image.height <= 0)
        throw new Error("image.height <= 0")

    val place = {
        val q = image.toCanvasImage.data
        Array.tabulate(q.width, q.height)((x, y) => q.data((y * q.width + x) * 4) * 0x010000 + q.data((y * q.width + x) * 4 + 1) * 0x0100 + q.data((y * q.width + x) * 4 + 2))
    }

    def center(k : K) : XY = centers(k)

    def regionAt(p : XY) : |[K] = {
        val px = p.x.~
        val py = p.y.~

        if (px < 0 || px >= place.length || py < 0 || py >= place(0).length)
            return None

        val r = place(px)(py)

        centers.foreach { case (k, XY(x, y)) =>
            if (place(x.~)(y.~) == r)
                return |(k)
        }

        None
    }

    def random(k : K) : XY = {
        val p = centers(k)
        val r = place(p.x.~)(p.y.~)

        while (r != 0) {
            val xx = (image.width * math.random()).toInt
            val yy = (image.height * math.random()).toInt

            if (place(xx)(yy) == r)
                return XY(xx, yy)
        }

        throw new Error("no region at " + p + " -> " + r)
    }
}

case class Margins(left : Double, top : Double, right : Double, bottom : Double)

object Margins {
    def apply(x : Double) : Margins = Margins(x, x, x, x)
    def apply(x : Double, y : Double) : Margins = Margins(x, y, x, y)
}

class Scene(layers : $[Layer], width : Double, height : Double, margins : Margins) {
    def render(g : dom.CanvasRenderingContext2D, ww : Int, hh : Int, zoom : Double, dx : Double, dy : Double) {
        object canvas {
            val width = ww
            val height = hh
        }

        g.setTransform(1, 0, 0, 1, 0, 0)

        g.clearRect(0, 0, canvas.width, canvas.height)

        val w = margins.left + width + margins.right
        val h = margins.top + height + margins.bottom

        g.setTransform(1, 0, 0, 1, 0, 0)


        g.translate(canvas.width / 2, canvas.height / 2)
        g.scale(zoom, zoom)
        g.translate(-canvas.width / 2, -canvas.height / 2)


        if (w * canvas.height < canvas.width * h) {
            g.translate((canvas.width - w * canvas.height / h) / 2, 0)
            g.scale(1.0 * canvas.height / h, 1.0 * canvas.height / h)
        }
        else {
            g.translate(0, (canvas.height - h * canvas.width / w) / 2)
            g.scale(1.0 * canvas.width / w, 1.0 * canvas.width / w)
        }

        g.translate(margins.left, margins.top)
        g.translate(dx, dy)

        layers./~(_.renderables).sortBy(_.z).foreach { r =>
            r.render(g)
        }
    }

    def toSceneCoordinates(x : Double, y : Double, ww : Int, hh : Int, zoom : Double, dx : Double, dy : Double) : XY = {
        object canvas {
            val width = ww
            val height = hh
        }

        var xx = x
        var yy = y

        val w = margins.left + width + margins.right
        val h = margins.top + height + margins.bottom

        xx -= canvas.width / 2
        yy -= canvas.height / 2
        xx /= zoom
        yy /= zoom
        xx += canvas.width / 2
        yy += canvas.height / 2

        if (w * canvas.height < canvas.width * h) {
            xx = xx - (canvas.width - w * canvas.height / h) / 2
            xx = xx * h / canvas.height
            yy = yy * h / canvas.height
        }
        else {
            yy = yy - (canvas.height - h * canvas.width / w) / 2
            xx = xx * w / canvas.width
            yy = yy * w / canvas.width
        }

        xx -= margins.left
        yy -= margins.top

        xx -= dx
        yy -= dy

        XY(xx, yy)
    }

    def pick(x : Double, y : Double, ww : Int, hh : Int, zoom : Double, dx : Double, dy : Double) : $[Any] = {
        val xy = toSceneCoordinates(x, y, ww, hh, zoom, dx, dy)

        var l = $[Any]()

        layers./~(_.pick(xy.x, xy.y)).reverse
    }

    def pick(xy : XY) : $[Any] = layers./~(_.pick(xy.x, xy.y)).reverse

}
