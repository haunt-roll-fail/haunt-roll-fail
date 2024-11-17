package coup
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

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, resources, callbacks)
}

class UI(val uir : ElementAttachmentPoint, arity : Int, val resources : Resources, callbacks : hrf.Callbacks) extends GUI {
    val statuses = 1.to(arity)./(i => newPane("status-" + i, Content, styles.status, xlo.flexhtop))
    val aid = newOuterPane("aid", Content)
    val overlayPane = newOuterPane("overlay", Content)

    var overlay : Boolean = false

    def factionElem(f : Faction) = f.name.styled(f)

    def factionStatus(f : Faction, container : Container) {




        if (!game.players.contains(f))
            return


        val p = game.players(f)

        val user = resources.getName(f).|(f.name)
        val ename = (game.factions.contains(f).?(f.elem).|(" ".pre ~ (" " + f.name + "  ").pre.spn(styles.dead).spn(styles.used)))
        val username = ((user != f.name).?(user).|(" ")).pre

        {
            val info = ename.spn(styles.name) ~ Break ~ username.spn(styles.username)
            val money = p.money.times(Div(Image("token", styles.token), styles.viewcard)).some.|($(Div(Image("token", styles.token), styles.viewcard, xstyles.hidden)))
            val cards = p.hand./(c => Div(Image("hidden", styles.card), styles.viewcard)) ++ (p.reveal ++ p.lost.reverse)./(c => Div(c.img, styles.viewcard))
            val c = game.claims.get(f).|(Nil)
            val claim = c.join((c.num > 3).?("|").|(" | "))

            container.replace(info ~ HorizontalBreak ~ money ~ HorizontalBreak ~ cards ~ HorizontalBreak ~ claim, resources)
        }

        if (game.roleClaims.contains(f))
            container.attach.parent.style.outline = "3px solid " + elem.outlines.colors(game.roleClaims(f))
        else
        if (game.targetsOf.contains(f))
            container.attach.parent.style.outline = "3px dashed " + elem.outlines.colors(game.targetsOf(f))
        else
        if (f == game.current)
            container.attach.parent.style.outline = "3px solid #aaaaaa"
        else
        if (game.claims.contains(f))
            container.attach.parent.style.outline = "3px dotted #aaaaaa"
        else
        if (game.highlightFaction.has(f))
            {}
        else
            container.attach.parent.style.outline = ""
    }

    def updateStatus() {
        0.until(arity).foreach { n =>
            factionStatus(currentGame.setup(n), statuses(n))
        }
        if (overlay) {
            overlayPane.attach.parent.style.borderWidth = "4px"
            overlayPane.replace(overlayFit(Image("aid", xstyles.artwork))(xstyles.seeThrough).onClick.param("hide-aid"), resources, onClick)
            overlayPane.vis()
            aid.clear()
        }
        else {
            overlayPane.clear()
            overlayPane.invis()
            aid.replace(overlayFit(Image("aid", xstyles.artwork)).onClick.param("show-aid"), resources, onClick)
        }

    }

    def onClick : Any => Unit = {
        case "show-aid" =>
            overlay = true
            updateStatus()

        case "hide-aid" =>
            overlay = false
            updateStatus()

        case Nil =>
            overlay = false
            overlayPane.invis()

        case _ =>
    }

    def overlayScrollX(e : Elem) = overlayScroll(e)(xstyles.seeThrough).onClick
    def overlayFitX(e : Elem) = overlayFit(e)(xstyles.seeThrough).onClick

    def showOverlay(e : Elem, onClick : Any => Unit) {

        overlayPane.vis()
        overlayPane.replace(e, resources, onClick, _ => {}, _ => {})
    }

    val zoom = 0.72

    val layouts = $(Layout("base",
        $(
            BasicPane("status", 16+1+1, 16, Priorities(top = 1, left = 1, grow = 1+3)),
            BasicPane("aid", 10, 6, Priorities(maxYscale = 2)),
            BasicPane("log", 20, 10, Priorities(grow = 3+1, maxYscale = 4)),
            BasicPane("action", 23, 38, Priorities(bottom = 1, right = 1, grow = 2+2, maxXscale = 1.8+2, maxYscale = 1.2+2))
        )
        ./(p => p.copy(kX = p.kX * zoom, kY = p.kY * zoom))
    ))./~(l =>
        l.copy(name = l.name + "-verdouble", boost = l.boost * 1.04 * (1 - arity % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-verdouble", kY = p.kY * ((arity + 1) / 2), kX = p.kX * 2))
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-hordouble", boost = l.boost * 1.04 * (1 - arity % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-hordouble", kX = p.kX * ((arity + 1) / 2), kY = p.kY * 2))
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-verdouble", boost = l.boost * 1.04 * (arity % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-verdouble", kY = p.kY * ((arity + 1) / 2), kX = p.kX * 2))
            case p : BasicPane if p.name == "log" && arity % 2 == 1 => None
            case p : BasicPane if p.name == "aid" && arity % 2 == 1 => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-hordouble", boost = l.boost * 1.04 * (arity % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-hordouble", kX = p.kX * ((arity + 1) / 2), kY = p.kY * 2))
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
        case f if f.name == "action" => $(f, f.copy(name = "undo"), f.copy(name = "settings"))
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
        case f if f.name == "aid-log" => f.copy(name = "aid", height = 6 * f.height / 16) :: f.copy(name = "log", y = f.y + 6 * f.height / 16, height = f.height - 6 * f.height / 16)
        case f => $(f)
    },
    ff => ff ++ true.? {
        val ss = ff
        Fit("overlay", ss./(_.x).min, ss./(_.y).min, ss./(_.right).max - ss./(_.x).min, ss./(_.bottom).max - ss./(_.y).min)
    })


    val settingsKey = Meta.settingsKey

    val layoutKey = "v" + 1 + "." + "arity-" + arity


    override def preinfo(self : |[Faction], aa : $[UserAction]) = {
        val ii = currentGame.info(Nil, self, aa)
        ii.any.??(convertActions(self, ii))
    }

    override def info(self : |[Faction], aa : $[UserAction]) = {
        (currentGame.isOver && hrf.HRF.flag("replay").not).$(
            ZBasic(Break ~ Break ~ Break, "Save Replay".hh, () => {
                callbacks.saveReplay {
                }
            }).copy(clear = false)
        ) ++
        (currentGame.isOver && callbacks.canPlayAgain).$(
            ZBasic(Break ~ Break ~ Break, "Play Again".hh, () => {
                callbacks.playAgain()
            }).copy(clear = false)
        ) ++
        (hrf.HRF.param("lobby").none).$(
            ZBasic(Break ~ Break ~ Break, "Save Game Online".hh, () => {
                showOverlay(overlayScrollX("Save Game Online".hlb(xstyles.larger125) ~
                    ("Save".hlb).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(xstyles.width60ex).pointer.onClick.param("***") ~
                    ("Save and replace bots with humans".hh).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(xstyles.width60ex).pointer.onClick.param("///") ~
                    ("Save as a single-player multi-handed game".hh).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(xstyles.width60ex).pointer.onClick.param("###") ~
                    ("Cancel".txt).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(xstyles.width60ex).pointer.onClick.param("???")
                ).onClick, {
                    case "***" => callbacks.saveReplayOnline(false, false) { url => onClick(Nil) }
                    case "///" => callbacks.saveReplayOnline(true , false) { url => onClick(Nil) }
                    case "###" => callbacks.saveReplayOnline(true , true ) { url => onClick(Nil) }
                    case _ => onClick(Nil)
                })
            }, ZBasic.infoch).copy(clear = false)
        ) ++
        $(ZBasic(Break ~ Break, "Notifications".spn, () => { onClick("notifications", self) }, ZBasic.infoch).copy(clear = false)).%(_ => self.any || game.isOver) ++
        $(ZBasic(Break ~ Break, "Interface".spn, () => { callbacks.editSettings { updateStatus() } }, ZBasic.infoch).copy(clear = false))
    }

    override def styleAction(faction : Option[Faction], actions : List[UserAction], a : UserAction, unavailable : Boolean, view : Option[Any]) = {
        val img = view @@ {
            case Some(_ : Card) => true
            case _ => false
        } || a @@ {
            case _ : ViewToken => true
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
            case _ => $(xstyles.choice, xstyles.xx, xstyles.thu, xstyles.thumargin)
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
            case _ if img && actions.has(NoHand) => $(xlo.first)
            case _ => Nil
        } ++
        a @@ {
            case a : Selectable if a.selected => $(styles.selected)
            case _ => Nil
        }
    }
}
