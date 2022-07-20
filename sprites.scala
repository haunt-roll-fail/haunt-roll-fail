package hrf.ui.sprites
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

import hrf.ui.sprites.Canvas._

trait Image {
    def width : Int
    def height : Int
    def toCanvasImage : CanvasImage
}

class RawImage(image : dom.html.Image) extends Image {
    def width = image.width
    def height = image.height

    lazy val toCanvasImage = {
        val c = Canvas.create(width, height)

        c.context.drawImage(image, 0, 0)

        new CanvasImage(c)
    }
}

class CanvasImage(image : dom.html.Canvas) extends Image {
    def width = image.width
    def height = image.height
    def toCanvasImage = this
}


object Canvas {
    def create(w : Int, h : Int) : dom.html.Canvas = {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]

        canvas.width = w
        canvas.height = h

        canvas
    }
    
    implicit class CanvasEx(val canvas : dom.html.Canvas) extends AnyVal {
        def context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

        def toDataURL : String = canvas.toDataURL("image/png")
    
        def data : dom.ImageData = context.getImageData(0, 0, canvas.width, canvas.height)
    }
}

case class Rect(x : Double, y : Double, width : Double, height : Double)

case class ImageRect(image : Image, rect : Rect)

trait Filter {
    def transform(image : Image) : Image
}

trait Sprite {
    def images : $[ImageRect]
    def hitboxes : $[Rect]
}

trait Renderable

case class RenderableSprite(sprite : Sprite, scale : Double, filters : $[Filter]) extends Renderable

trait Layer

class OrderedLayer extends Layer {
    def add(s : Renderable, x : Double, y : Double) = ???
}

class FitLayer[K](regions : Regions[K]) extends Layer {
    def addFixed(s : Renderable, x : Double, y : Double, tag : Any) = ???
    def addFloat(s : Renderable, x : Double, y : Double, tag : Any) = ???
}

trait Regions[K] {
    def regionAt(x : Int, y : Int) : Option[K]
    def center(k : K) : (Int, Int)
    def random(k : K) : (Int, Int)
}

class IndexedImageRegions[K](data : Image, offsetX : Int, offsetY : Int, centers : Map[K, (Int, Int)]) extends Regions[K] {
    def regionAt(x : Int, y : Int) : Option[K] = ???
    def center(k : K) : (Int, Int) = centers(k)
    def random(k : K) : (Int, Int) = ???
}

trait Scene {
    def layers : $[Layer]
}



