package sehi
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
    def factionElem(f : Faction) = f.name.styled(f)

    val statuses = 1.to(arity)./(n => newPane("status-" + n, Content, styles.status))
    val mapSmall = newPane("map-small", Content, styles.board)

    val d = 12

    val mapBitmapSmall = new CachedBitmap(mapSmall.attach.parent)
    var map = mapBitmapSmall

    val mp = resources.images.get("board")

    def drawMap() {
        val bitmap = map.get(map.node.clientWidth*2, map.node.clientHeight*2)

        val g = bitmap.context
        g.setTransform(0.5, 0, 0, 0.5, 0, 0)

        g.clearRect(0, 0, bitmap.width, bitmap.height)

        val dw = d
        val dh = d

        if (bitmap.height < bitmap.width || true)
        {
            if ((dw + mp.width + dw) * bitmap.height < bitmap.width * (dh + mp.height + dh)) {
                g.translate((bitmap.width - (dw + mp.width + dw) * bitmap.height / (dh + mp.height + dh)) / 2, 0)
                g.scale(1.0 * bitmap.height / (dh + mp.height + dh), 1.0 * bitmap.height / (dh + mp.height + dh))
            }
            else {
                g.translate(0, (bitmap.height - (dh + mp.height + dh) * bitmap.width / (dw + mp.width + dw)) / 2)
                g.scale(1.0 * bitmap.width / (dw + mp.width + dw), 1.0 * bitmap.width / (dw + mp.width + dw))
            }
        }
        else {
            g.translate(bitmap.width, 0)
            g.rotate(math.Pi / 2)

            if ((dw + mp.width + dw) * bitmap.width < bitmap.height * (dh + mp.height + dh)) {
                g.translate((bitmap.height - (dw + mp.width + dw) * bitmap.width / (dh + mp.height + dh)) / 2, 0)
                g.scale(1.0 * bitmap.width / (dh + mp.height + dh), 1.0 * bitmap.width / (dh + mp.height + dh))
            }
            else {
                g.translate(0, (bitmap.width - (dh + mp.height + dh) * bitmap.height / (dw + mp.width + dw)) / 2)
                g.scale(1.0 * bitmap.height / (dw + mp.width + dw), 1.0 * bitmap.height / (dw + mp.width + dw))
            }
        }

        g.translate(dw, dh)
        g.drawImage(mp, 0, 0)

        if (game.liberal.num >= 1)
            g.drawImage(resources.images.get("liberal-article-" + game.liberal(0).image), 629, 306)

        if (game.liberal.num >= 2)
            g.drawImage(resources.images.get("liberal-article-" + game.liberal(1).image), 1198, 306)

        if (game.liberal.num >= 3)
            g.drawImage(resources.images.get("liberal-article-" + game.liberal(2).image), 1767, 306)

        if (game.liberal.num >= 4)
            g.drawImage(resources.images.get("liberal-article-" + game.liberal(3).image), 2335, 306)

        if (game.liberal.num >= 5)
            g.drawImage(resources.images.get("liberal-article-" + game.liberal(4).image), 2914+6, 306)

        val x = 335
        val xx = 904

        if (game.fascist.num >= 1)
            g.drawImage(resources.images.get("fascist-article-" + game.fascist(0).image), x, 1623)

        if (game.fascist.num >= 2)
            g.drawImage(resources.images.get("fascist-article-" + game.fascist(1).image), x + (xx - x) * 1, 1623)

        if (game.fascist.num >= 3)
            g.drawImage(resources.images.get("fascist-article-" + game.fascist(2).image), x + (xx - x) * 2, 1623)

        if (game.fascist.num >= 4)
            g.drawImage(resources.images.get("fascist-article-" + game.fascist(3).image), x + (xx - x) * 3 + 2, 1623)

        if (game.fascist.num >= 5)
            g.drawImage(resources.images.get("fascist-article-" + game.fascist(4).image), x + (xx - x) * 4 + 4, 1623)

        if (game.fascist.num >= 6)
            g.drawImage(resources.images.get("fascist-article-" + game.fascist(5).image), x + (xx - x) * 5 + 14, 1623)


        game.frustration match {
            case 0 => g.drawImage(resources.images.get("marker"), 1387 - 50, 1145 - 50)
            case 1 => g.drawImage(resources.images.get("marker"), 1387 - 50 + 385, 1145 - 50)
            case 2 => g.drawImage(resources.images.get("marker"), 1387 - 50 + 385*2, 1145 - 50)
            case 3 => g.drawImage(resources.images.get("marker"), 1387 - 50 + 385*3, 1145 - 50)
        }
    }



    var fullscreen = false
    mapSmall.attach.parent.onclick = (e) => {
        if (fullscreen)
            dom.document.exitFullscreen()
        else
            dom.document.documentElement.requestFullscreen()

        fullscreen = fullscreen.not
    }

    def factionStatus(f : Faction, container : Container) {
        if (!game.roles.contains(f))
            return

        var friends = game.friends(f).%(game.alive.has)
        var enemies = game.enemies(f).%(game.alive.has)
        var others = game.alive.diff(friends).diff(enemies).but(f).shuffle.sortBy(game.karma)

        if (game.roles(f).party == Liberal)
        while (others.num > 1) {
            enemies :+= others.head
            friends :+= others.last
            others = others.drop(1).dropRight(1)
        }

        {
            val user = resources.getName(f).|(f.name)

            val limited = (game.president == f).?(President.elem).|((game.chancellor == f).?(Chancellor.elem).|(game.limited.contains(f).?("~~ term limited ~~".styled(styles.limited)).|(" ".pre)))
            val name = game.alive.contains(f).?(f.name.styled(f) : Elem).|(" ".pre ~ (" " + f.name + "  ").pre.styled(styles.dead).styled(styles.used)).styled(styles.name)
            val faction = (user != f.name).?(Text(user)).|(" ".pre).styled(styles.username)

            val vote = (game.votes.num == game.alive.num).??(game.votes.get(f)./(_.elem)).|(" ".pre)

            val open = Div(game.roles(f).img, styles.viewcard)

            if (game.over.not)
                container.replace(name ~ Break ~ limited ~ Break ~ faction ~ Break ~ vote, resources)
            else
                container.replace(name ~ Break ~ open, resources)
        }
    }

    def updateStatus() {
        0.until(arity).foreach { n =>
            factionStatus(game.setup(n), statuses(n))
        }

        drawMap()
    }

    val zoom = 0.72 * 0.5

    val layouts = $(Layout("base",
        $(
            BasicPane("status", 6.48, 5.76*1.4, Priorities(top = 1, left = 1, grow = 0)),
            BasicPane("map-small", 53*0.56, 35*0.56, Priorities(maxYscale = 2)),
            BasicPane("log", 18, 6+arity, Priorities(grow = 3+1, maxYscale = 4)),
            BasicPane("action", 19 - (arity < 9).??(4), 19+1+1, Priorities(bottom = 1, right = 1, grow = 2+2, maxXscale = 1.8+2, maxYscale = 1.2+2))
        )
    ))./~(l =>
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
        case f => $(f)
    },
    _./~{
        case f => $(f)
    },
    ff => ff ++ true.not.? {
        val ss = ff
        Fit("overlay", ss./(_.x).min, ss./(_.y).min, ss./(_.right).max - ss./(_.x).min, ss./(_.bottom).max - ss./(_.y).min)
    })

    val settingsKey = Meta.settingsKey

    val layoutKey = "v" + 1 + "." + "arity-" + arity

    def onClick : Any => Unit = {
        case _ =>
    }

    override def preinfo(self : Option[Faction], aa : List[UserAction]) = {
        val ii = currentGame.info(Nil, self, aa)
        ii.any.??(convertActions(self, ii))
    }

    override def styleAction(faction : |[F], actions : $[UserAction], a : UserAction, unavailable : Boolean, view : |[Any]) : $[Style] = {
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

            def ss =
                (a match {
                    case UnavailableReasonAction(_ : ViewCard, _) => $(xstyles.unavailableCard)
                    case _ : Unavailable => $(xstyles.unavailableText)
                    case _ if unavailable && view.any => $(xstyles.unavailableCard)
                    case _ if unavailable => $(xstyles.unavailableText)
                    case _ => Nil
                }) ++
                (a match {
                    case _ : Info => $(xstyles.info)
                    case _ if view.any => $()
                    case _ => $(xstyles.choice, xstyles.xx, xstyles.thu)
                }) ++
                (a match {
                    case _ : Extra[_] => Nil
                    case _ : Choice => $(xlo.pointer)
                    case _ : OnClickInfo => $(xlo.pointer)
                    case _ => Nil
                }) ++
                (a match {
                    case UnavailableReasonAction(_ : ViewCard, _) => $(styles.viewcard)
                    case _ : ViewCard => $(styles.viewcard)
                    case _ if view.any => $(styles.viewcard)
                    case _ => $(xlo.fullwidth)
                }) ++
                (faction match {
                    case Some(f) => $(elem.borders.get(f))
                    case _ => Nil
                }) ++
                (a match {
                    case a : Selectable if a.selected => $(styles.selected)
                    case _ => Nil
                }) ++
                view @@ {
                    case Some(_) => Nil
                    case _ => Nil
                }


            def clear = a match {
                case _ : NoClear => false
                case _ => true
            }

            ss
    }

    def convertActionsX(faction : Option[Faction], actions : List[UserAction], then : UserAction => Unit = null) = {
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

            def ss =
                (a match {
                    case UnavailableReasonAction(_ : ViewCard, _) => $(xstyles.unavailableCard)
                    case _ : Unavailable => $(xstyles.unavailableText)
                    case _ => Nil
                }) ++
                (a match {
                    case _ : Info => $(xstyles.info)
                    case _ => $(xstyles.choice, xstyles.xx, xstyles.thu)
                }) ++
                (a match {
                    case _ : Extra[_] => Nil
                    case _ : Choice => $(xlo.pointer)
                    case _ : OnClickInfo => $(xlo.pointer)
                    case _ => Nil
                }) ++
                (a match {
                    case UnavailableReasonAction(_ : ViewCard, _) => $(styles.viewcard)
                    case _ : ViewCard => $(styles.viewcard)
                    case _ => $(xlo.fullwidth)
                }) ++
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
