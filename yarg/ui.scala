package yarg

import org.scalajs.dom

import colmat._

import hrf.elem._
import hrf.html._
import hrf.canvas._
import hrf.web._
import hrf.ui._

import scala.collection.mutable

object UI extends BaseUI {
    val gaming = yarg.gaming

    def create(uir : ElementAttachmentPoint, game: Game, resources: Resources, title : String, saveReplay: (=> Unit) => Unit) = new UI(uir, game, resources)
}

class UI(val uir : ElementAttachmentPoint, val game : Game, val resources : Resources) extends GameUI with UI.GreyUI {
    def factionElem(f : Faction) = f.name.styled(f)

    val mapSmall = newPane("map-small", Content, styles.board)
    
    val d = 12

    val mapBitmapSmall = new CachedBitmap(mapSmall.attach.parent)
    var map = mapBitmapSmall
    
    val mp : dom.html.Image = resources.assets("map")
    
    val images = mutable.Map[String, dom.html.Canvas]()

    def asset(id : String) = images.getOrElseUpdate(id, {
        val result = new Bitmap(resources.assets(id).width, resources.assets(id).height)
        result.context.drawImage(resources.assets(id), 0, 0)

        result.canvas
    })
    
    def flipped(id : String) = images.getOrElseUpdate(id, {
        val result = new Bitmap(resources.assets(id).width, resources.assets(id).height)
        result.context.scale(-1, 1)
        result.context.drawImage(resources.assets(id), 0, 0, -result.width, result.height)

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
               
                q.data(p + 0) = (r * 0.30 + g * 0.59 + b * 0.11).round.toInt
                q.data(p + 1) = (r * 0.30 + g * 0.59 + b * 0.11).round.toInt
                q.data(p + 2) = (r * 0.30 + g * 0.59 + b * 0.11).round.toInt
                q.data(p + 3) = (a / 3).round.toInt
        
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
               
                q.data(p + 0) = (r * 1.3).round.toInt
                q.data(p + 1) = (g * 1.3).round.toInt
                q.data(p + 2) = (b * 1.3).round.toInt
        
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
               
                q.data(p + 0) = (255).round.toInt
                q.data(p + 1) = (255).round.toInt
                q.data(p + 2) = (255).round.toInt
                q.data(p + 3) = (a / 5).round.toInt
        
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
               
                q.data(p + 0) = (255).round.toInt
                q.data(p + 1) = (0).round.toInt
                q.data(p + 2) = (0).round.toInt
                q.data(p + 3) = (a / 5).round.toInt
        
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
               
                q.data(p + 0) = (r * 0.8).round.toInt
                q.data(p + 1) = (g * 0.8).round.toInt
                q.data(p + 2) = (128 + b * 0.7).round.toInt
        
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
                g.scale(1.0 * bitmap.height / (dh + mp.height + dh), 1.0 * bitmap.height / (dh + mp.height + dh))
            }
            else {
                g.translate(0, (bitmap.height - (dh + mp.height + dh) * bitmap.width / (dw + mp.width + dw)) / 2)
                g.scale(1.0 * bitmap.width / (dw + mp.width + dw), 1.0 * bitmap.width / (dw + mp.width + dw))
            }

        g.translate(dw, dh)
        g.drawImage(mp, 0, 0)
        
        if (state.players.none)
            return

        def drawBar(x : Int, y : Int, bar : String, max : Int, current : Int) {
            def bb(bar : String) = (max == 1).?(List("-single")).|(List("-start") ++ "".repeat(max - 2) ++ List("-end"))./("bar-" + bar + _)

            val ids = bb(bar).take(current) ++ bb("empty").drop(current)
            
            ids.zipWithIndex.foreach { case (bar, n) =>
                g.drawImage(resources.assets(bar), x + n * 60 - max * 30, y)
            }
        }

        def unitHeight(u : Figure) = (resources.assets(u.unit.id).height * u.unit.k).round.toInt + 20 + (u.unit.maxHealth > 0).??(50) + (u.unit.maxMana > 0).??(50) + (u.unit.maxRage > 0).??(50) + 60

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

                if (u == state.active) {
                    0.until(24)./(_ * 2 * math.Pi / 24).foreach { i =>
                        g.drawImage(whiten(o), x - (a.width * u.unit.k) / 2 + math.sin(i) * 15, yy + math.cos(i) * 15, a.width * u.unit.k, a.height * u.unit.k)
                    }
                    a = highlight(a)
                }
                else
                if (state.damaged.contains(u)) {
                    0.until(24)./(_ * 2 * math.Pi / 24).foreach { i =>
                        g.drawImage(redden(o), x - (a.width * u.unit.k) / 2 + math.sin(i) * 15, yy + math.cos(i) * 15, a.width * u.unit.k, a.height * u.unit.k)
                    }
                }
            }

            g.drawImage(a, x - (a.width * u.unit.k) / 2, yy, a.width * u.unit.k, a.height * u.unit.k)
        }
        
        state.setup.zip($(450+10, 1830+10)).foreach { case (f, x) =>
            var y = mp.height / 2 - state.of(f).units./(unitHeight).sum / 2
            state.of(f).units.foreach { u =>
                drawUnit(u, x, y)
                y += unitHeight(u)
            }
        }
    }

    drawMap()
    
    def updateStatus() {
        drawMap()
    }
 
    def layout(width : Int, height : Int) : List[(String, Rect, Option[Double])] = {
        val font = FontDimensionInfo(72, 40, 72)
        
        val panes = (("map-small" -> new ImagePane(10, 3975, 2625)) :: ("log" -> new TextPane(10, font, 100, 58, 12)) :: ("action" -> new TextPane(10, font, 100, 58, 18))).toMap

        SplitX(panes("map-small"), SplitX(panes("log"), panes("action")))
            .dim(0, 0, width, height)
        
        panes.view.mapValues(p => (Rect(p.x, p.y, p.w, p.h), p.isInstanceOf[TextPane].?(p.asInstanceOf[TextPane].fontSize))).toList./{ case (a, (b, c)) => (a, b, c) }
    }
 


    def onClick : Any => Unit = {
        case _ =>
    }
    
    override def convertActions(faction : Option[Faction], actions : List[UserAction], then : UserAction => Unit = null) = {
        actions./~{ a =>
            def q = {
                val q = a.question(game)
                (q == Empty).?(q).|(Div(q)(xlo.fullwidth))
            }

            def o = a.option(game) ~ (a match {
                case UnavailableReasonAction(a, reason) if reason != "" => reason.startsWith("|").?(Break).|(SpaceSpan) ~ Span(Text("(" + reason.substring(reason.startsWith("|").??(1)) + ")"), xstyles.smaller85)
                case _ => Empty
            })
        
            def wrap(e : Elem) = a match {
                case a : ElemWrap => a.wrap(game)(e)
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
                    case _ : Choice => $(xstyles.pointer)
                    case _ : Cancel => $(xstyles.pointer)
                    case _ : OnClickInfo => $(xstyles.pointer)
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
