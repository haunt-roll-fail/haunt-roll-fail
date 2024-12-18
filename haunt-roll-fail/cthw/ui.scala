package cthw
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

import hrf.elem._
import hrf.html._
import hrf.canvas._
import hrf.web._
import hrf.ui._
import hrf.ui.again._

object UI extends BaseUI {
    val mmeta = Meta

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, resources)
}

class UI(val uir : ElementAttachmentPoint, arity : Int, val resources : Resources) extends GUI {
    val statuses = 1.to(arity)./(i => newPane("status-" + i, Content, styles.status, xlo.flexhtop))
    val overlayPane = newOuterPane("overlay", Content)
    val mapSmall = newPane("map-small", Content)

    var mapBitmap = new CachedBitmap(mapSmall.attach.parent)

    val (regions, pieces, scene) = {
        import hrf.ui.sprites._

        val regions = new IndexedImageRegions[Nothing](new RawImage(resources.images.get("map-regions")), 0, 0, Map())

        val mp = resources.images.get("map")
        val map = Sprite($(ImageRect(new RawImage(mp), Rectangle(0, 0, mp.width, mp.height), 1.0)), $)

        val background = new OrderedLayer
        background.add(map)(0, 0)

        val pieces = new FitLayer[Nothing, Nothing](regions, FitOptions(kX = 2))

        val scene = new Scene($(background, pieces), mp.width, mp.height, Margins(0, 0, 0, 0))

        (regions, pieces, scene)
    }


    def drawMap() {
        val bitmap = {
            val width = mapBitmap.node.clientWidth * dom.window.devicePixelRatio
            val height = mapBitmap.node.clientHeight * dom.window.devicePixelRatio

            val upscale = 2

            val b = mapBitmap.get(width.round.toInt * upscale, height.round.toInt * upscale)

            b.canvas.style.width = "100%"
            b.canvas.style.height = "100%"

            b
        }

        pieces.flush()
    }

    var overlay : Boolean = false

    def factionElem(f : Faction) = f.name.styled(f)

    def factionStatus(f : Faction, container : Container) {
        if (!game.players.contains(f))
            return


        val p = game.pstates(f)

        val user = resources.getName(f).|(f.name)
        val ename = (game.factions.contains(f).?(f.elem).|(" ".pre ~ (" " + f.name + "  ").pre.spn(styles.dead).spn(styles.used)))
        val username = ((user != f.name).?(user).|(" ")).pre

        {
            val info = ename.spn(styles.name) ~ Break ~ username.spn(styles.username)
            val money = Empty
            val cards = Empty
            val claim = Empty

            container.replace(info ~ HorizontalBreak ~ money ~ HorizontalBreak ~ cards ~ HorizontalBreak ~ claim, resources)
        }

        container.attach.parent.style.outline = ""
    }

    def updateStatus() {
        0.until(arity).foreach { n =>
            factionStatus(game.setup(n), statuses(n))
        }

        drawMap()
    }

    def onClick : Any => Unit = {
        case _ =>
    }

    val layouts = $(Layout("base", $(
        BasicPane("status", 16+1, 16, Priorities(top = 1, left = 1)),
        BasicPane("map-small", 80, 40, Priorities(maxYscale = 2)),
        BasicPane("log", 20, 10, Priorities(grow = 2, maxYscale = 1.2+2)),
        BasicPane("action", 23, 36+2, Priorities(bottom = 1, right = 1, grow = 1, maxXscale = 1.8+2, maxYscale = 1.2+2))
    )))./~(l =>
        l.copy(name = l.name + "-verdouble", boost = l.boost * 1.04 * (1 - arity % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-verdouble", kY = p.kY * ((arity + 1) / 2), kX = p.kX * 2))
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-hordouble", boost = l.boost * 1.04 * (1 - arity % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-hordouble", kX = p.kX * ((arity + 1) / 2), kY = p.kY * 2))
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-verdouble", boost = l.boost * 1.04 * (arity % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-verdouble", kY = p.kY * ((arity + 1) / 2), kX = 20 * 2))
            case p : BasicPane if p.name == "log" && arity % 2 == 1 => None
            case p : BasicPane if p.name == "aid" && arity % 2 == 1 => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-hordouble", boost = l.boost * 1.04 * (arity % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-hordouble", kX = 20 * ((arity + 1) / 2), kY = p.kY * 2))
            case p : BasicPane if p.name == "log" && arity % 2 == 1 => None
            case p : BasicPane if p.name == "aid" && arity % 2 == 1 => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-horizontal", boost = l.boost * 1.00, panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-horizontal", kX = p.kX * arity)
            case p => p
        }) ::
        l.copy(name = l.name + "-vertical", panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-vertical", kY = p.kY * arity)
            case p => p
        }) ::
        Nil
    ).%(_.boost > 0)

    val layouter = Layouter(layouts,
    _./~{
        case f if f.name == "status-horizontal" => 1.to(arity)./(n => f.copy(name = "status-" + n, x = f.x + ((n - 1) * f.width  /~/ arity), width  = (n * f.width  /~/ arity) - ((n - 1) * f.width  /~/ arity)))
        case f if f.name == "status-vertical"   => 1.to(arity)./(n => f.copy(name = "status-" + n, y = f.y + ((n - 1) * f.height /~/ arity), height = (n * f.height /~/ arity) - ((n - 1) * f.height /~/ arity)))
        case f if f.name == "status-hordouble"  =>
            val c = ((arity + 1) / 2)
            1.to(c * 2)./(n => f.copy(name = (n > arity).?("aid-log").|("status-" + n.toString),
                x = f.x + (((n - 1) % c) * f.width /~/ c),
                width = (n * f.width /~/ c) - ((n - 1) * f.width /~/ c),
                y = f.y + (n - 1) / c * (f.height / 2),
                height = (n > c).?(f.height - f.height / 2).|(f.height / 2))
            )
        case f if f.name == "status-verdouble"  =>
            val c = ((arity + 1) / 2)
            1.to(c * 2)./(n => f.copy(name = (n > arity).?("aid-log").|("status-" + (((n - 1) % c) * 2 + (n - 1) / c + 1).toString),
                y = f.y + (((n - 1) % c) * f.height /~/ c),
                height = (n * f.height /~/ c) - ((n - 1) * f.height /~/ c),
                x = f.x + (n - 1) / c * (f.width / 2),
                width = (n > c).?(f.width - f.width / 2).|(f.width / 2))
            )
        case f => $(f)
    },
    _./~{
        case f => $(f)
    },
    ff => ff ++ true.? {
        val ss = ff
        Fit("overlay", ss./(_.x).min, ss./(_.y).min, ss./(_.right).max - ss./(_.x).min, ss./(_.bottom).max - ss./(_.y).min)
    })

    val settingsKey = Meta.settingsKey

    val layoutKey = "v" + 1 + "." + "arity-" + arity

    override def preinfo(self : Option[Faction], aa : List[UserAction]) = {
        val ii = currentGame.info(Nil, self, aa)
        ii.any.??(convertActions(self, ii))
    }

    override def styleAction(faction : Option[Faction], actions : List[UserAction], a : UserAction, unavailable : Boolean, view : Option[Any]) = {
        val img = view @@ {
            case _ => false
        } || a @@ {
            case _ => false
        }

        a @@ {
            case _ if unavailable && img => $(xstyles.unavailableCard)
            case _ if unavailable => $(xstyles.unavailableText)
            case _ => Nil
        } ++
        a @@ {
            case _ if img => Nil
            case _ : Info => $(xstyles.info, xstyles.xx, xstyles.thu)
            case _ => $(xstyles.choice, xstyles.xx, xstyles.thu)
        } ++
        a @@ {
            case _ : Extra[_] => Nil
            case _ : Choice => $(xlo.pointer)
            case _ : Cancel => $(xlo.pointer)
            case _ : OnClickInfo => $(xlo.pointer)
            case _ => Nil
        } ++
        a @@ {
            case _ if img => $(styles.viewcard)
            case _ => $(xlo.fullwidth)
        } ++
        faction @@ {
            case Some(f) => $(elem.borders.get(f))
            case _ => Nil
        } ++
        a @@ {
            case _ => Nil
        } ++
        a @@ {
            case _ => Nil
        }
    }

}
