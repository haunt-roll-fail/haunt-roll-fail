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

import hrf.ui.sprites._

import org.scalajs.dom

import scalajs.js.timers.setTimeout

import scala.collection.mutable


trait GreyUI { self : Gaming =>
    trait CanvasPane {
        def container : Container
        def draw() : Unit
    }

    trait GUI extends GameUI {
        val uir : ElementAttachmentPoint
        val resources : Resources

        var panes = Map[String, Container]()

        def newAbstractPane(name : String, e : Elem) = {
            val c = uir.appendContainer(e, resources)
            panes += name -> c
            c
        }

        def newOuterPane(name : String, e : Elem) = newAbstractPane(name, Div(Div(e, xstyles.unselectable, xstyles.outer), xstyles.pane))
        def newPane(name : String, e : Elem, styles : Style*) = newOuterPane(name, Div(e, xstyles.inner +: styles.$))

        def newCanvasPane(name : String, upscale : Int)(doDraw : hrf.canvas.Bitmap => Unit) : CanvasPane = {
            val pane = newPane(name, Content)

            val cachedBitmap = new hrf.canvas.CachedBitmap(pane.attach.parent)

            new CanvasPane {
                def container = pane
                def draw() {
                    val bitmap = {
                        val width = cachedBitmap.node.clientWidth * dom.window.devicePixelRatio
                        val height = cachedBitmap.node.clientHeight * dom.window.devicePixelRatio

                        val b = cachedBitmap.get(width.~ * upscale, height.~ * upscale)

                        b.canvas.style.width = "100%"
                        b.canvas.style.height = "100%"
                        b.canvas.style.touchAction = "none"
                        b.canvas.style.pointerEvents = "none"

                        b
                    }

                    doDraw(bitmap)
                }
            }
        }

        def newCanvasPaneX(name : String, upscale : Int)(draw : () => |[Scene])(highlight : ($[Any], XY) => Unit)(onClick : ($[Any], XY) => Unit) : CanvasPane = {
            var zoomBase = 0.0

            def zoom = math.pow(1.0007, zoomBase)

            var dX = 0.0
            var dY = 0.0

            def processTargetClick(target : $[Any], xy : XY) : Unit = { onClick(target, xy) }
            def processRightClick(target : $[Any], xy : XY) : Unit = {}
            def processHighlight(target : $[Any], xy : XY) : Unit = { highlight(target, xy) }

            var lastScene : |[Scene] = None
            def makeScene() : |[Scene] = draw()

            def adjustCenterZoomX() {
                val width = 594
                val height = 1285
                val margins = Margins(0, 0, 0, 0)

                zoomBase = zoomBase.clamp(0, 990*2)

                val qX = (width + margins.left + margins.right) * (1 - 1 / zoom) / 2
                val minX = -qX + margins.right - zoomBase / 5
                val maxX = qX - margins.left + zoomBase / 5
                dX = dX.clamp(minX, maxX)

                val qY = (height + margins.top + margins.bottom) * (1 - 1 / zoom) / 2
                val minY = -qY + margins.bottom - zoomBase / 5
                val maxY = qY - margins.top + zoomBase / 5
                dY = dY.clamp(minY, maxY)

                // zoomBase = 0
                // dX = 0
                // dY = 0
            }

            var moving = false
            var moved = false
            var movingFrom : |[XY] = None

            var zoomFrom : Double = 1.0
            var touchFrom : $[XY] = $

            val pane = newPane(name, Content)
            val cachedBitmap = new hrf.canvas.CachedBitmap(pane.attach.parent)

            val cp = new CanvasPane {
                def container = pane
                def draw() {
                    makeScene().foreach { scene =>
                        lastScene = |(scene)

                        adjustCenterZoomX()

                        val bitmap = {
                            val width = cachedBitmap.node.clientWidth * dom.window.devicePixelRatio
                            val height = cachedBitmap.node.clientHeight * dom.window.devicePixelRatio

                            val b = cachedBitmap.get(width.~ * upscale, height.~ * upscale)

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
                        setTimeout(min(25, resources.images.incomplete.num) * 20)(draw())

                    resources.images.incomplete = $
                }
            }

            pane.attach.parent.onpointerdown = (e) => {
                if (e.isPrimary) {
                    val offsetX = e.offsetX * dom.window.devicePixelRatio
                    val offsetY = e.offsetY * dom.window.devicePixelRatio

                    val width = cachedBitmap.node.clientWidth * dom.window.devicePixelRatio * upscale
                    val height = cachedBitmap.node.clientHeight * dom.window.devicePixelRatio * upscale

                    lastScene.foreach { scene =>
                        pane.attach.parent.style.cursor = "grab"

                        val xy = scene.toSceneCoordinates(offsetX, offsetY, width.~, height.~, zoom, dX, dY)

                        processHighlight(scene.pick(xy), xy)

                        cp.draw()

                        moving = true
                        moved = false
                        movingFrom = |(xy)
                    }
                }
            }

            pane.attach.parent.onpointermove = (e) => {
                if (e.isPrimary) {
                    val offsetX = e.offsetX * dom.window.devicePixelRatio
                    val offsetY = e.offsetY * dom.window.devicePixelRatio

                    val width = cachedBitmap.node.clientWidth * dom.window.devicePixelRatio * upscale
                    val height = cachedBitmap.node.clientHeight * dom.window.devicePixelRatio * upscale

                    lastScene.foreach { scene =>
                        if (moving) {
                            val xy = scene.toSceneCoordinates(offsetX, offsetY, width.~, height.~, zoom, 0, 0)

                            val movingTo = xy

                            dX = movingTo.x - movingFrom.get.x
                            dY = movingTo.y - movingFrom.get.y

                            moved = true
                        }
                        else {
                            pane.attach.parent.style.cursor = "default"

                            val xy = scene.toSceneCoordinates(offsetX, offsetY, width.~, height.~, zoom, dX, dY)

                            processHighlight(scene.pick(xy), xy)
                        }

                        cp.draw()
                    }
                }

            }

            pane.attach.parent.onpointerup = (e) => {
                if (e.isPrimary) {
                    val offsetX = e.offsetX * dom.window.devicePixelRatio
                    val offsetY = e.offsetY * dom.window.devicePixelRatio

                    val width = cachedBitmap.node.clientWidth * dom.window.devicePixelRatio * upscale
                    val height = cachedBitmap.node.clientHeight * dom.window.devicePixelRatio * upscale

                    moving = false

                    pane.attach.parent.style.cursor = "default"
                }
            }

            pane.attach.parent.onclick = (e) => {
                val offsetX = e.offsetX * dom.window.devicePixelRatio
                val offsetY = e.offsetY * dom.window.devicePixelRatio

                val width = cachedBitmap.node.clientWidth * dom.window.devicePixelRatio * upscale
                val height = cachedBitmap.node.clientHeight * dom.window.devicePixelRatio * upscale

                lastScene.foreach { scene =>
                    val xy = scene.toSceneCoordinates(offsetX, offsetY, width.~, height.~, zoom, dX, dY)

                    if (moved.not)
                        processTargetClick(scene.pick(xy), xy)

                    moving = false
                    moved = false

                    pane.attach.parent.style.cursor = "default"
                }
            }

            pane.attach.parent.onpointerout = (e) => {
                if (e.isPrimary) {
                    moving = false
                    moved = false

                    pane.attach.parent.style.cursor = "default"
                }
            }

            pane.attach.parent.ontouchmove = (e) => {
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

                        cp.draw()
                    }
                }
            }

            pane.attach.parent.ontouchstart = (e) => {
                touchFrom = e.targetTouches.iterator.$./(t => XY(t.clientX, t.clientY))
                zoomFrom = zoomBase
            }

            pane.attach.parent.onwheel = (e) => {
                lastScene.foreach { scene =>
                    zoomBase += e.deltaY

                    cp.draw()
                }
            }

            pane.attach.parent.oncontextmenu = (e) => {
                e.preventDefault()

                val offsetX = e.offsetX * dom.window.devicePixelRatio
                val offsetY = e.offsetY * dom.window.devicePixelRatio

                val width = cachedBitmap.node.clientWidth * dom.window.devicePixelRatio * upscale
                val height = cachedBitmap.node.clientHeight * dom.window.devicePixelRatio * upscale

                lastScene.foreach { scene =>
                    val xy = scene.toSceneCoordinates(offsetX, offsetY, width.~, height.~, zoom, dX, dY)

                    if (moved.not)
                        processRightClick(scene.pick(xy), xy)

                    moving = false
                    moved = false

                    pane.attach.parent.style.cursor = "default"
                }
            }

            cp
        }

        val actionPane = newPane("action", Content.div(xlo.column)(xlo.fullheight), xstyles.pane.action)
        val logDiv = newPane("log", Div(Div(Div(Empty, xstyles.halfcharline) ~ Content ~ Div(Empty, xstyles.halfcharline), xstyles.logblurhor), xstyles.logblur), xstyles.pane.log)

        val preDiv = actionPane.attach.appendContainer(Content.div(xlo.fullwidth)(xlo.flexhcenter), resources)
        val actDiv = actionPane.attach.appendContainer(Content.div(xlo.fullwidth)(xlo.flexhcenter), resources)
        val swDiv = actionPane.attach.appendContainer(Content.div(xlo.fullwidth)(xlo.flexhcenter), resources)
        val postDiv = actionPane.attach.appendContainer(Content.div(xlo.fullwidth)(xlo.flexhcenter), resources)

        val preasker = new NewAsker(preDiv, resources.images.get)
        val asker = new NewAsker(actDiv, resources.images.get, Some("Actions"))
        val swasker = new NewAsker(swDiv, resources.images.get)
        val postasker = new NewAsker(postDiv, resources.images.get)

        val logger = new LazyLogger(logDiv.attach)

        val undoPane = newPane("undo", Content.div(xlo.column)(xlo.fullheight), xstyles.pane.action)
        undoPane.invis()

        val undoAsker = new NewAsker(undoPane.attach.appendContainer(Content.div(xlo.flexhcenter)(xlo.fullwidth), resources), resources.images.get)

        val settingsPane = newPane("settings", Content.div(xlo.column)(xlo.fullheight), xstyles.pane.action)
        settingsPane.invis()

        val settingsAsker = new NewAsker(settingsPane.attach.appendContainer(Content.div(xlo.flexhcenter)(xlo.fullwidth)(xstyles.bottomPadding), resources), resources.images.get)

        def overlayScroll(e : Elem) =
            Div(
                Div(
                    Div(e,
                    xstyles.middleScrollIn),
                xstyles.middleScrollOut),
            xstyles.unselectable, xstyles.inner, xstyles.pane.action)

        def overlayFit(e : Elem) =
            Div(
                Div(
                    Div(e,
                    xstyles.middleScrollInFit),
                xstyles.middleScrollOut),
            xstyles.unselectable, xstyles.inner)

        val settingsKey : String

        val layoutKey : String

        val layouter : hrf.ui.again.Layouter

        object prevLayout {
            var panes : $[PanePlacement] = $
            var width : Int = 1
            var height : Int = 1
        }

        object newLayout {
            var width : Int = 1
            var height : Int = 1
        }

        def layout(width : Int, height : Int)(onLayout : $[PanePlacement] => Unit) {
            val panes = hrf.HRF.paramList("panes")

            if (panes.any) {
                onLayout($)

                val ll = layouter.layouts./(l => l.copy(panes = l.panes.%(l => panes.has(l.name)))).%(l => panes.diff(l.panes./(_.name)).none)

                hrf.ui.again.Layouter(ll, layouter.process : _*).get(width, height)(new hrf.Quants(500, 200).continue) { lr =>
                    +++(lr.panes)

                    val result = lr.panes./(p => PanePlacement(p.name, Rect(p.x, p.y, p.width, p.height), Some(lr.fontSize)))

                    onLayout(result)
                }

                return
            }

            val key = settingsKey + "." + "layout-" + layoutKey + "." + width + "x" + height

            newLayout.width = width
            newLayout.height = height

            hrf.serialize.DefaultSerialize.parseExpression(hrf.web.Local.get(key, "[]")) @@ {
                case Some(l : $[_]) if hrf.HRF.flag("relayout").not && l.any && l.all(_.is[PanePlacement]) => onLayout(l./~(_.as[PanePlacement]))
                case _ =>
                    onLayout(prevLayout.panes./(p => p.copy(
                        rect = Rect(
                            p.rect.x * width /\/ prevLayout.width,
                            p.rect.y * height /\/ prevLayout.height,
                            p.rect.width * width /\/ prevLayout.width,
                            p.rect.height * height /\/ prevLayout.height,
                        ),
                        fontSize = p.fontSize./(_ * (width * height) / (prevLayout.width * prevLayout.height))
                    )))

                    setTimeout(1) {
                        layouter.get(width, height)(new hrf.Quants(500, 200).continue) { lr =>
                            val result = lr.panes./(p => PanePlacement(p.name, Rect(p.x, p.y, p.width, p.height), Some(lr.fontSize)))

                            if (newLayout.width == width && newLayout.height == height) {

                                prevLayout.panes = result
                                prevLayout.width = width
                                prevLayout.height = height

                                onLayout(result)
                            }

                            hrf.web.Local.set(key, hrf.serialize.DefaultSerialize.write(result))
                        }
                    } : Unit
            }
        }


        def factionElem(f : F) : Elem

        case object NoDesc extends UndoDescriptor {
            def elem(count : Int) = Empty
        }

        case class AnotherUndoDesc(initiators : $[F]) extends UndoDescriptor {
            def elem(count : Int) = ("undo" + (count > 1).??("s")).hl ~ " by " ~ initiators./(factionElem).comma
        }

        case class ActionByDesc(self : F) extends UndoDescriptor {
            def elem(count : Int) = ("action" + (count > 1).??("s") + " by ") ~ factionElem(self)
        }

        case class OwnActionDesc(self : F) extends UndoDescriptor {
            def elem(count : Int) = ("own action" + (count > 1).??("s"))
        }

        case class TextDesc(text : String) extends UndoDescriptor {
            def elem(count : Int) = (text + (count > 1).??("s")).spn
        }

        case class WarningDesc(text : String) extends UndoDescriptor {
            def elem(count : Int) = (text + (count > 1).??("s")).styled(xstyles.warning)
        }

        def describeActionForUndo(a : ExternalAction, self : Option[F]) : $[UndoDescriptor] = $(
            a.unwrap @@ {
                case UndoAction(initiators : $[F], _, _) => AnotherUndoDesc(initiators)
                case _ : RolledAction[_] => WarningDesc("dice roll")
                case _ : ShuffledAction[_] => WarningDesc("shuffle")
                case _ : Shuffled2Action[_, _] => WarningDesc("shuffle")
                case _ : Shuffled3Action[_, _, _] => WarningDesc("shuffle")
                case _ : RandomAction[_] => WarningDesc("random selection")
                case a : FactionAction if self.has(a.self).not => ActionByDesc(a.self)
                case a : FactionAction if self.has(a.self) => OwnActionDesc(a.self)
                case a => NoDesc
            }
        ).but(NoDesc)

        def replayMenu(n : Int, playing : Boolean, speed : Int, skip : Boolean, speeds : $[(String, Int)], targets : $[(Elem, Int)], prev : () => Unit, next : () => Unit, jump : Int => Unit, play : () => Unit, pause : () => Unit, setSpeed : Int => Unit, setSkip : Boolean => Unit, cancel : () => Unit) {
            actionPane.invis()
            settingsPane.invis()
            undoPane.vis()

            undoAsker.scrollIfNeeded()

            val g = ("Replay".hl ~ " Action #".spn ~ n.hlb).div ~ HorizontalBreak

            undoAsker.zask((
                $(
                    ZBasic(g, "<".hlb, () => {
                        prev()
                    }, ZBasic.choice ++ $(xstyles.shorter)),
                ) ++
                $(if (playing)
                    ZBasic(g, "Pause".hl, () => {
                        pause()
                    }, ZBasic.choice ++ $(xstyles.short))
                else
                    ZBasic(g, "Play".hl, () => {
                        play()
                    }, ZBasic.choice ++ $(xstyles.short))
                ) ++
                $(
                    ZBasic(g, ">".hlb, () => {
                        next()
                    }, ZBasic.choice ++ $(xstyles.shorter)),
                ) ++
                speeds./((s, n) => ZBasic("Speed" ~ HorizontalBreak, (n == speed).?(s.hlb).|(s.txt), () => setSpeed(n), ZBasic.choice ++ $(xstyles.shorter))) ++
                targets./((e, n) => ZBasic("Jump To", e, () => jump(n))) ++
                $(
                    ZBasic("", "Exit", () => {
                        undoPane.invis()
                        actionPane.vis()
                        cancel()
                    })
                )
            )./(_.copy(clear = false)))(resources)
        }

        def rewindX(n : Int, description : () => Elem, undo : () => Unit, replay : () => Unit, cancel : () => Unit) {
            actionPane.invis()
            settingsPane.invis()
            undoPane.vis()

            undoAsker.scrollIfNeeded()

            val g = "Game state after".spn.div ~ " ".pre ~ n.hlb ~ " ".pre ~ "actions".spn.div

            def top() {
                undoAsker.zask($(
                    ZBasic(g, "Replay".hh, () => {
                        undoPane.invis()
                        actionPane.vis()
                        replay()
                    }),
                    ZBasic(" ", "Undo".styled(xstyles.warning), () => {
                        undoAsker.zask(
                            $(ZBasic(("Undo " ~ description()).div, "Confirm".styled(xstyles.error), () => {
                                undoPane.invis()
                                actionPane.vis()
                                undoing()
                                setTimeout(0) { undo() }
                                ()
                            }),
                            ZBasic("", "Cancel", () => {
                                top()
                            }))
                        )(resources)
                    }),
                    ZBasic("", "Back", () => {
                        undoPane.invis()
                        actionPane.vis()
                        cancel()
                    })
                ))(resources)
            }

            top()
        }

        override def rewind(n : Int, actions : $[ExternalAction], speed : Option[Int], latest : () => Unit, undoTo : (Int, Int) => Unit, playback : Int => Unit) {
            actionPane.invis()
            settingsPane.invis()
            undoPane.vis()

            undoAsker.scrollIfNeeded()

            val g = "Game state after".spn.div ~ " ".pre ~ n.hlb ~ " ".pre ~ "actions".spn.div

            def top() {
                undoAsker.zask($(
                    ZBasic(g, "Replay from here".hh, () => {
                        playback(hrf.HRF.paramInt("replay-speed").|(300))
                        top()
                    }),
                    ZBasic(" ", "Undo to here".styled(xstyles.warning), () => {
                        val dd = actions.drop(n)./~(describeActionForUndo(_, None))
                        val ee = (dd.distinct.num > 1).?(", including ").|((dd.distinct.num > 0).?(", namely ").|("")).spn ~ dd.distinct./(d => (dd.count(d) > 3).?("multple ".spn) ~ d.elem(dd.count(d))).join(", ")

                        undoAsker.zask(
                            $(ZBasic(("Undo " ~ (actions.num - n).hl ~ " action".s(actions.num - n) ~ ee).div, "Confirm".styled(xstyles.error), () => {
                                undoPane.invis()
                                actionPane.vis()
                                undoing()
                                setTimeout(0) { undoTo(n, actions.num) }
                                ()
                            }),
                            ZBasic("", "Cancel", () => {
                                top()
                            }))
                        )(resources)
                    }),
                    ZBasic("", "Back", () => {
                        undoPane.invis()
                        actionPane.vis()
                        latest()
                    })
                ))(resources)
            }

            top()
        }

        override def stopRewind() {
            undoPane.invis()
            actionPane.vis()
        }

        def info(self : |[F], aa : $[UserAction]) : $[ZOption] = $

        def preinfo(self : |[F], aa : $[UserAction]) : $[ZOption] = $

        def ask(faction : |[F], actions : $[UserAction], then : UserAction => Unit) {
            asker.scrollIfNeeded()

            preasker.zask(preinfo(faction, actions))(resources)
            asker.zask(convertActions(faction, actions, then))(resources)
            postasker.zask(info(faction, actions))(resources)

            hideSwitches()
        }

        def wait(self : $[F], factions : $[F]) {
            if (factions.any) {
                val zw = factions.any.$(ZOption(Div("Waiting for " ~ convertDesc(factions./(f => factionElem(f)).comma).apply(game))(xlo.fullwidth), Div(Text("z... z... z..."), ZBasic.info)(xlo.fullwidth)))

                preasker.zask(preinfo(self.single, $))(resources)
                asker.zask(zw)(resources)
                postasker.zask(info(self.single, $))(resources)
            }

            drawSwitches(self)
        }

        val allSwitches : $[hrf.meta.GameOption] = $

        def switchesFor(f : F) : $[hrf.meta.GameOption] = $

        def switchesOn(f : F) : $[hrf.meta.GameOption] = $

        def enableSwitch(f : F, s : hrf.meta.GameOption) {}

        def disableSwitch(f : F, s : hrf.meta.GameOption) {}

        var switchesSelf = $[F]()

        def drawSwitches(self : $[F]) {
            switchesSelf = self
            redrawSwitches()
        }

        def redrawSwitches() {
            if (game.isOver)
                return hideSwitches()

            val sw = switchesSelf./~{ f =>
                val l = switchesFor(f)

                allSwitches.diff(l).intersect(switchesOn(f)).foreach { s => disableSwitch(f, s) }

                l./(s => ZBasic((Break ~ factionElem(f) ~ " Off Turn Actions").div, switchesOn(f).has(s).?(s.valueOn).|(s.valueOff), () => {
                    switchesOn(f).has(s).?(disableSwitch(f, s)).|(enableSwitch(f, s))
                    redrawSwitches()
                }, switchesOn(f).has(s).?(ZBasic.choice).|(ZBasic.info) ++ switchesOn(f).has(s).$(xstyles.optionOn) ++ $(xstyles.optionE)))
            }
            swasker.zask(sw)(resources)
        }

        def hideSwitches() {
            swasker.zask($)(resources)
        }

        def undoing() {
            val zw = $(ZOption(Div("Undoing...")(xlo.fullwidth), Div(Text("z... z... z..."), ZBasic.info)(xlo.fullwidth)))

            preasker.zask($)(resources)
            asker.zask(zw)(resources)
            swasker.zask($)(resources)
            postasker.zask($)(resources)
        }

        def onClick(a : Any) {}

        def styleAction(faction : Option[F], actions : $[UserAction], a : UserAction, unavailable : Boolean, view : |[Any]) : $[Style] = Nil

        def convertActions(faction : Option[F], actions : $[UserAction], then : UserAction => Unit = null) : $[ZOption] = {
            actions./~{ a =>
                def q = {
                    val q = a.question(currentGame)
                    (q == Empty).?(q).|(Div(q)(xlo.fullwidth))
                }

                def o = a.option(currentGame) ~ (a match {
                    case UnavailableReasonAction(a, reason) if reason != "" => reason.startsWith("|").?(Break).|(SpaceSpan) ~ Span(Text("(" + reason.substring(reason.startsWith("|").??(1)) + ")"), xstyles.smaller85)
                    case _ => Empty
                })

                def wrap(e : Elem) = a match {
                    case a : ElemWrap => a.wrap(currentGame)(e)
                    case _ if o == Empty => Empty
                    case _ => e
                }

                a @@ {
                    case u : Unavailable => (u.action, true)
                    case a => (a, false)
                } @@ { case (a, unavailable) =>

                    val view = a @@? {
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

        def alog(e : Elem, n : Int, onClick : Any => Unit, delayed : Boolean = false) : LazyBlock = {
            val line = e == DottedLine || e == SingleLine || e == DoubleLine

            val fs = hrf.elem.FigureSpace.toString

            logger.alog(OnClick(n, Hint("Action #" + n, (line.not.?((fs + fs).pre) ~ e ~ line.not.?(fs.pre)).div(xstyles.hanging))), onClick, delayed)(resources)
        }

        def blog() : Container = {
            logger.blog()(resources)
        }

        def resize() {
            val width = dom.window.innerWidth.~
            val height = dom.window.innerHeight.~

            layout(width, height) { l =>
                l.foreach { case PanePlacement(name, rect, fontSize) =>
                    val c = panes(name)
                    c.show()
                    c.node.style.left = rect.x + "px"
                    c.node.style.top = rect.y + "px"
                    c.node.style.width = rect.width + "px"
                    c.node.style.height = rect.height + "px"
                    fontSize.foreach { fontSize =>
                        c.node.style.fontSize = (fontSize * 3612 / height) + "%"
                    }
                }

                panes.keys.$.diff(l./(_.name)).foreach { name => panes(name).hide() }

                updateStatus()

                if (l.any)
                    if (dom.document.getElementById("backgroun-clouds") != null)
                        dom.document.getElementById("backgroun-clouds").remove()
            }
        }

        def start() {
            resize()

            dom.window.onresize = e => resize()
        }
    }
}
