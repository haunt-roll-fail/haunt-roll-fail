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

import hrf.canvas._

import hrf.ui.sprites._



import org.scalajs.dom

import scalajs.js.timers.setTimeout

import scala.collection.mutable


trait GreyMapUI extends GreyUI { self : Gaming =>
    trait MapGUI extends GUI {
        def img(s : String) = resources.images.get(s)

        val mapSmall = newPane("map-small", Content)
        val overlayPane = newOuterPane("map-small-overlay", Content)
        overlayPane.invis()

        var lastScene : |[Scene] = None

        val mapBitmap = new CachedBitmap(mapSmall.attach.parent)
        val upscale = 2 / 2

        var zoomBase = 0.0

        def zoom = math.pow(1.0007, zoomBase)

        var dX = 0.0
        var dY = 0.0

        def processTargetClick(target : $[Any], xy : XY) : Unit
        def processRightClick(target : $[Any], xy : XY) : Unit
        def processHighlight(target : $[Any], xy : XY) : Unit

        def makeScene() : |[Scene]

        def adjustCenterZoomX() {

        }

        def drawMap() {
            makeScene().foreach { scene =>
                lastScene = |(scene)

                adjustCenterZoomX()

                val bitmap = {
                    val width = mapBitmap.node.clientWidth * dom.window.devicePixelRatio
                    val height = mapBitmap.node.clientHeight * dom.window.devicePixelRatio

                    val b = mapBitmap.get(width.~ * upscale, height.~ * upscale)

                    b.canvas.style.width = "100%"
                    b.canvas.style.height = "100%"
                    b.canvas.style.touchAction = "none"
                    b.canvas.style.pointerEvents = "none"

                    b
                }

                if (resources.images.incomplete.none)
                    scene.render(bitmap.context, bitmap.width, bitmap.height, zoom, dX, dY)
            }

            if (resources.images.incomplete.any)
                setTimeout(min(25, resources.images.incomplete.num) * 20)(drawMap())

            resources.images.incomplete = $
        }

        var moving = false
        var moved = false
        var movingFrom : |[XY] = None

        mapSmall.attach.parent.onpointerdown = (e) => {
            if (e.isPrimary) {
                val offsetX = e.offsetX * dom.window.devicePixelRatio
                val offsetY = e.offsetY * dom.window.devicePixelRatio

                val width = mapBitmap.node.clientWidth * dom.window.devicePixelRatio * upscale
                val height = mapBitmap.node.clientHeight * dom.window.devicePixelRatio * upscale

                lastScene.foreach { scene =>
                    mapSmall.attach.parent.style.cursor = "grab"

                    val xy = scene.toSceneCoordinates(offsetX, offsetY, width.~, height.~, zoom, dX, dY)

                    processHighlight(scene.pick(xy), xy)

                    drawMap()

                    moving = true
                    moved = false
                    movingFrom = |(xy)
                }
            }
        }

        mapSmall.attach.parent.onpointermove = (e) => {
            if (e.isPrimary) {
                val offsetX = e.offsetX * dom.window.devicePixelRatio
                val offsetY = e.offsetY * dom.window.devicePixelRatio

                val width = mapBitmap.node.clientWidth * dom.window.devicePixelRatio * upscale
                val height = mapBitmap.node.clientHeight * dom.window.devicePixelRatio * upscale

                lastScene.foreach { scene =>
                    if (moving) {
                        val xy = scene.toSceneCoordinates(offsetX, offsetY, width.~, height.~, zoom, 0, 0)

                        val movingTo = xy

                        dX = movingTo.x - movingFrom.get.x
                        dY = movingTo.y - movingFrom.get.y

                        moved = true
                    }
                    else {
                        mapSmall.attach.parent.style.cursor = "default"

                        val xy = scene.toSceneCoordinates(offsetX, offsetY, width.~, height.~, zoom, dX, dY)

                        processHighlight(scene.pick(xy), xy)
                    }

                    drawMap()
                }
            }

        }

        mapSmall.attach.parent.onpointerup = (e) => {
            if (e.isPrimary) {
                val offsetX = e.offsetX * dom.window.devicePixelRatio
                val offsetY = e.offsetY * dom.window.devicePixelRatio

                val width = mapBitmap.node.clientWidth * dom.window.devicePixelRatio * upscale
                val height = mapBitmap.node.clientHeight * dom.window.devicePixelRatio * upscale

                moving = false
                moved = false

                mapSmall.attach.parent.style.cursor = "default"
            }
        }

        mapSmall.attach.parent.onclick = (e) => {
            val offsetX = e.offsetX * dom.window.devicePixelRatio
            val offsetY = e.offsetY * dom.window.devicePixelRatio

            val width = mapBitmap.node.clientWidth * dom.window.devicePixelRatio * upscale
            val height = mapBitmap.node.clientHeight * dom.window.devicePixelRatio * upscale

            lastScene.foreach { scene =>
                val xy = scene.toSceneCoordinates(offsetX, offsetY, width.~, height.~, zoom, dX, dY)

                processTargetClick(scene.pick(xy), xy)

                moving = false
                moved = false

                mapSmall.attach.parent.style.cursor = "default"
            }
        }

        mapSmall.attach.parent.onpointerout = (e) => {
            if (e.isPrimary) {
                moving = false
                moved = false

                mapSmall.attach.parent.style.cursor = "default"
            }
        }

        var zoomFrom : Double = 1.0
        var touchFrom : $[XY] = $

        mapSmall.attach.parent.ontouchmove = (e) => {
            lastScene.foreach { scene =>
                val touchTo = e.targetTouches.iterator.$./(t => XY(t.clientX, t.clientY))

                def expanse(l : $[XY]) = {
                    val cx = l./(_.x).sum / l.num
                    val cy = l./(_.y).sum / l.num

                    math.sqrt(l./(p => (p.x - cx)*(p.x - cx) + (p.y - cy)*(p.y - cy)).sum)
                }

                if (touchTo.num != touchFrom.num) {
                    touchFrom = touchTo
                    zoomFrom = zoomBase
                }
                else {
                    if (expanse(touchTo) > 16)
                        zoomBase = zoomFrom - math.log(expanse(touchFrom) / expanse(touchTo)) / math.log(1.0007)

                    drawMap()
                }
            }
        }

        mapSmall.attach.parent.ontouchstart = (e) => {
            touchFrom = e.targetTouches.iterator.$./(t => XY(t.clientX, t.clientY))
            zoomFrom = zoomBase
        }

        mapSmall.attach.parent.onwheel = (e) => {
            lastScene.foreach { scene =>
                zoomBase += e.deltaY

                drawMap()
            }
        }

        mapSmall.attach.parent.oncontextmenu = (e) => {
            e.preventDefault()

            val offsetX = e.offsetX * dom.window.devicePixelRatio
            val offsetY = e.offsetY * dom.window.devicePixelRatio

            val width = mapBitmap.node.clientWidth * dom.window.devicePixelRatio * upscale
            val height = mapBitmap.node.clientHeight * dom.window.devicePixelRatio * upscale

            lastScene.foreach { scene =>
                val xy = scene.toSceneCoordinates(offsetX, offsetY, width.~, height.~, zoom, dX, dY)

                if (moved.not)
                    processRightClick(scene.pick(xy), xy)

                moving = false
                moved = false

                mapSmall.attach.parent.style.cursor = "default"
            }
        }
    }
}

