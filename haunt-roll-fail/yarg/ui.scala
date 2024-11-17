package yarg
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

import hrf.elem._
import hrf.html._
import hrf.canvas._
import hrf.web._
import hrf.ui._
import hrf.ui.panes._

import org.scalajs.dom

import scala.collection.mutable


object UI extends BaseUI {
    val mmeta = Meta

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, resources)
}

class UI(val uir : ElementAttachmentPoint, arity : Int, val resources : Resources) extends GUI {
    def factionElem(f : Faction) = f.name.styled(f)

    val mapSmall = newPane("map-small", Content, styles.board)

    val d = 12

    val mapBitmapSmall = new CachedBitmap(mapSmall.attach.parent)
    var map = mapBitmapSmall

    val mp : dom.html.Image = resources.images.get("map")

    val images = mutable.Map[String, dom.html.Canvas]()

    def asset(id : String) = images.getOrElseUpdate(id, {
        val result = new Bitmap(resources.images.get(id).width, resources.images.get(id).height)
        result.context.drawImage(resources.images.get(id), 0, 0)

        result.canvas
    })

    def flipped(id : String) = images.getOrElseUpdate(id, {
        val result = new Bitmap(resources.images.get(id).width, resources.images.get(id).height)
        result.context.scale(-1, 1)
        result.context.drawImage(resources.images.get(id), 0, 0, -result.width, result.height)

        result.canvas
    })

    val grayscaled = mutable.Map[dom.html.Canvas, dom.html.Canvas]()

    def grayscale(b : dom.html.Canvas) = grayscaled.getOrElseUpdate(b, {
        val result = new Bitmap(b.width, b.height)
        result.context.drawImage(b, 0, 0)

        val q = result.context.getImageData(0, 0, result.width, result.height)

        var i = 0
        while (i < q.width) {
            var j = 0
            while (j < q.height) {
                val p = (j * q.width + i) * 4

                val r = q.data(p + 0)
                val g = q.data(p + 1)
                val b = q.data(p + 2)
                val a = q.data(p + 3)

                q.data(p + 0) = (r * 0.30 + g * 0.59 + b * 0.11).~
                q.data(p + 1) = (r * 0.30 + g * 0.59 + b * 0.11).~
                q.data(p + 2) = (r * 0.30 + g * 0.59 + b * 0.11).~
                q.data(p + 3) = a /~/ 3

                j += 1
            }

            i += 1
        }

        result.context.putImageData(q, 0, 0)

        result.canvas
    })

    val highlighted = mutable.Map[dom.html.Canvas, dom.html.Canvas]()

    def highlight(b : dom.html.Canvas) = highlighted.getOrElseUpdate(b, {
        val result = new Bitmap(b.width, b.height)
        result.context.drawImage(b, 0, 0)

        val q = result.context.getImageData(0, 0, result.width, result.height)

        var i = 0
        while (i < q.width) {
            var j = 0
            while (j < q.height) {
                val p = (j * q.width + i) * 4

                val r = q.data(p + 0)
                val g = q.data(p + 1)
                val b = q.data(p + 2)

                q.data(p + 0) = (r * 1.3).~
                q.data(p + 1) = (g * 1.3).~
                q.data(p + 2) = (b * 1.3).~

                j += 1
            }

            i += 1
        }

        result.context.putImageData(q, 0, 0)

        result.canvas
    })

    val whitened = mutable.Map[dom.html.Canvas, dom.html.Canvas]()

    def whiten(b : dom.html.Canvas) = whitened.getOrElseUpdate(b, {
        val result = new Bitmap(b.width, b.height)
        result.context.drawImage(b, 0, 0)

        val q = result.context.getImageData(0, 0, result.width, result.height)

        var i = 0
        while (i < q.width) {
            var j = 0
            while (j < q.height) {
                val p = (j * q.width + i) * 4

                val r = q.data(p + 0)
                val g = q.data(p + 1)
                val b = q.data(p + 2)
                val a = q.data(p + 3)

                q.data(p + 0) = 255
                q.data(p + 1) = 255
                q.data(p + 2) = 255
                q.data(p + 3) = a /~/ 5

                j += 1
            }

            i += 1
        }

        result.context.putImageData(q, 0, 0)

        result.canvas
    })

    val reddened = mutable.Map[dom.html.Canvas, dom.html.Canvas]()

    def redden(b : dom.html.Canvas) = reddened.getOrElseUpdate(b, {
        val result = new Bitmap(b.width, b.height)
        result.context.drawImage(b, 0, 0)

        val q = result.context.getImageData(0, 0, result.width, result.height)

        var i = 0
        while (i < q.width) {
            var j = 0
            while (j < q.height) {
                val p = (j * q.width + i) * 4

                val r = q.data(p + 0)
                val g = q.data(p + 1)
                val b = q.data(p + 2)
                val a = q.data(p + 3)

                q.data(p + 0) = 255
                q.data(p + 1) = 0
                q.data(p + 2) = 0
                q.data(p + 3) = a /~/ 5

                j += 1
            }

            i += 1
        }

        result.context.putImageData(q, 0, 0)

        result.canvas
    })

    val bluened = mutable.Map[dom.html.Canvas, dom.html.Canvas]()

    def bluen(b : dom.html.Canvas) = bluened.getOrElseUpdate(b, {
        val result = new Bitmap(b.width, b.height)
        result.context.drawImage(b, 0, 0)

        val q = result.context.getImageData(0, 0, result.width, result.height)

        var i = 0
        while (i < q.width) {
            var j = 0
            while (j < q.height) {
                val p = (j * q.width + i) * 4

                val r = q.data(p + 0)
                val g = q.data(p + 1)
                val b = q.data(p + 2)
                val a = q.data(p + 3)

                q.data(p + 0) = (r * 0.8).~
                q.data(p + 1) = (g * 0.8).~
                q.data(p + 2) = (128 + b * 0.7).~

                j += 1
            }

            i += 1
        }

        result.context.putImageData(q, 0, 0)

        result.canvas
    })

    def drawMap() {
        val bitmap = map.get(map.node.clientWidth*2, map.node.clientHeight*2)

        val g = bitmap.context
        g.setTransform(0.5, 0, 0, 0.5, 0, 0)

        g.clearRect(0, 0, bitmap.width, bitmap.height)

        val dw = d
        val dh = d

            if ((dw + mp.width + dw) * bitmap.height < bitmap.width * (dh + mp.height + dh)) {
                g.translate((bitmap.width - (dw + mp.width + dw) * bitmap.height / (dh + mp.height + dh)) / 2, 0)
                g.scale(bitmap.height ÷ (dh + mp.height + dh), bitmap.height /:/ (dh + mp.height + dh))
            }
            else {
                g.translate(0, (bitmap.height - (dh + mp.height + dh) * bitmap.width / (dw + mp.width + dw)) / 2)
                g.scale(bitmap.width /:/ (dw + mp.width + dw), bitmap.width /:/ (dw + mp.width + dw))
            }

        g.translate(dw, dh)
        g.drawImage(mp, 0, 0)

        if (game.players.none)
            return

        def drawBar(x : Int, y : Int, bar : String, max : Int, current : Int) {
            def bb(bar : String) = (max == 1).?($("-single")).|($("-start") ++ (max - 2).times("") ++ $("-end"))./("bar-" + bar + _)

            val ids = bb(bar).take(current) ++ bb("empty").drop(current)

            ids.indexed.foreach { (bar, n) =>
                g.drawImage(resources.images.get(bar), x + n * 60 - max * 30, y)
            }
        }

        def unitHeight(u : Figure) = (resources.images.get(u.unit.id).height * u.unit.k).round.toInt + 20 + (u.unit.maxHealth > 0).??(50) + (u.unit.maxMana > 0).??(50) + (u.unit.maxRage > 0).??(50) + 60

        def drawUnit(u : Figure, x : Int, y : Int) {
            var yy = y

            yy += 10

            if (u.unit.maxHealth > 0) {
                drawBar(x, yy, "health", u.unit.maxHealth, u.health)
                yy += 50
            }

            if (u.unit.maxMana > 0) {
                drawBar(x, yy, "mana", u.unit.maxMana, u.mana)
                yy += 50
            }

            if (u.unit.maxRage > 0) {
                drawBar(x, yy, "rage", u.unit.maxRage, u.rage)
                yy += 50
            }

            yy += 20

            val o = (x < mp.width / 2).?(flipped _).|(asset _).apply(u.unit.id)
            var a = o

            if (u.dead)
                a = grayscale(a)
            else {
                if (u.stun > 0)
                    a = bluen(a)

                if (u == game.active) {
                    0.until(24)./(_ * 2 * math.Pi / 24).foreach { i =>
                        g.drawImage(whiten(o), x - (a.width * u.unit.k) / 2 + math.sin(i) * 15, yy + math.cos(i) * 15, a.width * u.unit.k, a.height * u.unit.k)
                    }
                    a = highlight(a)
                }
                else
                if (game.damaged.contains(u)) {
                    0.until(24)./(_ * 2 * math.Pi / 24).foreach { i =>
                        g.drawImage(redden(o), x - (a.width * u.unit.k) / 2 + math.sin(i) * 15, yy + math.cos(i) * 15, a.width * u.unit.k, a.height * u.unit.k)
                    }
                }
            }

            g.drawImage(a, x - (a.width * u.unit.k) / 2, yy, a.width * u.unit.k, a.height * u.unit.k)
        }

        game.setup.zip($(450+10, 1830+10)).foreach { case (f, x) =>
            var y = mp.height / 2 - game.of(f).units./(unitHeight).sum / 2
            game.of(f).units.foreach { u =>
                drawUnit(u, x, y)
                y += unitHeight(u)
            }
        }
    }

    def updateStatus() {
        drawMap()
    }

    val settingsKey = Meta.settingsKey

    val layouter = ???
    val layoutKey = ???

    override def layout(width : Int, height : Int)(onLayout : $[PanePlacement] => Unit) {
        val font = FontDimensionInfo(72, 40, 72)

        val panes = (("map-small" -> new ImagePane(10, 3975, 2625)) :: ("log" -> new TextPane(10, font, 100, 58, 12)) :: ("action" -> new TextPane(10, font, 100, 58, 18))).toMap

        SplitX(panes("map-small"), SplitX(panes("log"), panes("action")))
            .dim(0, 0, width, height)

        val l = panes.view.mapValues(p => (Rect(p.x, p.y, p.w, p.h), p.is[TextPane].?(p.asInstanceOf[TextPane].fontSize))).$./{ case (a, (b, c)) => PanePlacement(a, b, c) }

        onLayout(l)
    }


    def onClick : Any => Unit = {
        case _ =>
    }

    override def convertActions(faction : Option[Faction], actions : List[UserAction], then : UserAction => Unit = null) = {
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

            val unavailable = a match {
                case _ : Unavailable => true
                case _ => false
            }

            def ss =
                (a match {
                    case _ if unavailable => List(xstyles.unavailableText)
                    case _ => Nil
                }) ++
                (a match {
                    case _ : Info => $(xstyles.info, xstyles.xx, xstyles.thu)
                    case _ => $(xstyles.choice, xstyles.xx, xstyles.thu)
                }) ++
                (a match {
                    case _ : Extra[_] => Nil
                    case _ : Choice => $(xlo.pointer)
                    case _ : Cancel => $(xlo.pointer)
                    case _ : OnClickInfo => $(xlo.pointer)
                    case _ => Nil
                }) ++
                $(xlo.fullwidth) ++
                (faction match {
                    case Some(f) => $(elem.borders.get(f))
                    case _ => Nil
                }) ++
                (a match {
                    case a : Selectable if a.selected => $(styles.selected)
                    case _ => Nil
                })

            def clear = a match {
                case _ : NoClear => false
                case _ => true
            }

            a match {
                case a : Hidden => None

                case a : OnClickInfo              => Some(ZOption(q, wrap(OnClick(Div(o, ss))), _ => { onClick(a.param) }, clear))
                case a : Info                     => Some(ZOption(q, wrap(        Div(o, ss))))

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
