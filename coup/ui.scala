package coup

import org.scalajs.dom

import colmat._

import hrf.elem._
import hrf.html._
import hrf.canvas._
import hrf.web._
import hrf.ui._
import hrf.ui.again._

object UI extends BaseUI {
    val gaming = Meta.gaming
    
    def create(uir : ElementAttachmentPoint, game : Game, resources: Resources, title : String, saveReplay: (=> Unit) => Unit) = new UI(uir, game, resources)
}

class UI(val uir : ElementAttachmentPoint, val game : Game, val resources : Resources) extends GameUI with UI.GreyUI {
    val statuses = 1.to(game.setup.num)./(i => newPane("status-" + i, Content, styles.status, xlo.flexhtop))
    val aid = newOuterPane("aid", Content)
    val overlayPane = newOuterPane("overlay", Content)
    
    var overlay : Boolean = false
                                   
    def factionElem(f : Faction) = f.name.styled(f)
        
    def factionStatus(f : Faction, container : Container) {
        val _state = state

        import _state._
        
        if (!players.contains(f))
            return


        val p = players(f)
        
        val user = resources.getName(f).|(f.name)
        val ename = (state.factions.contains(f).?(f.elem).|(" ".pre ~ (" " + f.name + "  ").pre.spn(styles.dead).spn(styles.used)))
        val username = ((user != f.name).?(user).|(" ")).pre
        
        {
            val info = ename.spn(styles.name) ~ Break ~ username.spn(styles.username)
            val money = (Div(Image("token", styles.token), styles.viewcard).repeat(p.money)).some.|($(Div(Image("token", styles.token), styles.viewcard, xstyles.hidden)))
            val cards = p.hand./(c => Div(Image("hidden", styles.card), styles.viewcard)) ++ (p.reveal ++ p.lost.reverse)./(c => Div(c.img, styles.viewcard))
            val c = claims.get(f).|(Nil)
            val claim = c.join((c.num > 3).?("|").|(" | "))
            
            container.replace(info ~ HorizontalBreak ~ money ~ HorizontalBreak ~ cards ~ HorizontalBreak ~ claim, resources)
        }

        if (state.roleClaims.contains(f))
            container.attach.parent.style.outline = "3px solid " + elem.outlines.colors(state.roleClaims(f))
        else
        if (state.targetsOf.contains(f))
            container.attach.parent.style.outline = "3px dashed " + elem.outlines.colors(state.targetsOf(f))
        else
        if (f == state.current)
            container.attach.parent.style.outline = "3px solid #aaaaaa"
        else
        if (state.claims.contains(f))
            container.attach.parent.style.outline = "3px dotted #aaaaaa"
        else
        if (state.highlightFaction.has(f))
            {} 
        else
            container.attach.parent.style.outline = ""
    }                                                                                              
    
    def updateStatus() {
        0.until(game.setup.num).foreach { n =>
            factionStatus(game.setup(n), statuses(n))
        }
        if (overlay) {
            overlayPane.attach.parent.style.borderWidth = "4px"
            overlayPane.replace(overlayFit(Image("aid", xstyles.artwork))(xstyles.seeThrough).onClick("hide-aid"), resources, onClick)
            overlayPane.show()
            aid.clear()
        }
        else {
            overlayPane.clear()
            overlayPane.hide()
            aid.replace(overlayFit(Image("aid", xstyles.artwork)).onClick("show-aid"), resources, onClick)
        }

    }
    
    def onClick : Any => Unit = {
        case "show-aid" => 
            overlay = true
            updateStatus()

        case "hide-aid" =>
            overlay = false
            updateStatus()

        case _ =>
    }

    val layouts = $(Layout("base", $(
        BasicPane("status", 16+1, 16, Priorities(top = 1, left = 1)),
        BasicPane("aid", 20/2, 12/2, Priorities(maxYscale = 2)),
        BasicPane("log", 20, 10, Priorities(grow = 2, maxYscale = 1.2+2)),
        BasicPane("action", 23, 36+2, Priorities(bottom = 1, right = 1, grow = 1, maxXscale = 1.8+2, maxYscale = 1.2+2))
    )))./~(l => 
        l.copy(name = l.name + "-verdouble", boost = l.boost * 1.04 * (1 - state.setup.num % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-verdouble", kY = p.kY * ((state.setup.num + 1) / 2), kX = p.kX * 2))
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-hordouble", boost = l.boost * 1.04 * (1 - state.setup.num % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-hordouble", kX = p.kX * ((state.setup.num + 1) / 2), kY = p.kY * 2))
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-verdouble", boost = l.boost * 1.04 * (state.setup.num % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-verdouble", kY = p.kY * ((state.setup.num + 1) / 2), kX = 20 * 2))
            case p : BasicPane if p.name == "log" && state.setup.num % 2 == 1 => None
            case p : BasicPane if p.name == "aid" && state.setup.num % 2 == 1 => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-hordouble", boost = l.boost * 1.04 * (state.setup.num % 2), panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-hordouble", kX = 20 * ((state.setup.num + 1) / 2), kY = p.kY * 2))
            case p : BasicPane if p.name == "log" && state.setup.num % 2 == 1 => None
            case p : BasicPane if p.name == "aid" && state.setup.num % 2 == 1 => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-horizontal", boost = l.boost * 1.00, panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-horizontal", kX = p.kX * state.setup.num)                                         
            case p => p
        }) ::
        l.copy(name = l.name + "-vertical", panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-vertical", kY = p.kY * state.setup.num)
            case p => p
        }) ::
        Nil
    ).%(_.boost > 0)
    
    val layouter = Layouter(layouts, 
    _./~{
        case f if f.name == "status-horizontal" => 1.to(game.setup.num)./(n => f.copy(name = "status-" + n, x = f.x + ((n - 1) * f.width  / state.setup.num).round.toInt, width  = (n * f.width  / state.setup.num).round.toInt - ((n - 1) * f.width  / state.setup.num).round.toInt))
        case f if f.name == "status-vertical"   => 1.to(game.setup.num)./(n => f.copy(name = "status-" + n, y = f.y + ((n - 1) * f.height / state.setup.num).round.toInt, height = (n * f.height / state.setup.num).round.toInt - ((n - 1) * f.height / state.setup.num).round.toInt))
        case f if f.name == "status-hordouble"  => 
            val c = ((state.setup.num + 1) / 2)
            1.to(c * 2)./(n => f.copy(name = (n > game.setup.num).?("aid-log").|("status-" + n.toString), 
                x = f.x + (((n - 1) % c) * f.width / c).round.toInt, 
                width = (n * f.width / c).round.toInt - ((n - 1) * f.width / c).round.toInt, 
                y = f.y + (n - 1) / c * (f.height / 2), 
                height = (n > c).?(f.height - f.height / 2).|(f.height / 2))
            )
        case f if f.name == "status-verdouble"  => 
            val c = ((state.setup.num + 1) / 2)
            1.to(c * 2)./(n => f.copy(name = (n > game.setup.num).?("aid-log").|("status-" + (((n - 1) % c) * 2 + (n - 1) / c + 1).toString), 
                y = f.y + (((n - 1) % c) * f.height / c).round.toInt, 
                height = (n * f.height / c).round.toInt - ((n - 1) * f.height / c).round.toInt, 
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
    

    def layout(width : Int, height : Int) : List[(String, Rect, Option[Double])] = {
        val lr = layouter.get(width, height)
        
        lr.panes./(p => (p.name, Rect(p.x, p.y, p.width, p.height), Some(lr.fontSize)))
    }

    def layout2(width : Int, height : Int) : List[(String, Rect, Option[Double])] = {
        val font = FontDimensionInfo(72, 40, 72)
        
        val statuses = 1.to(game.setup.num)./(i => "status-" + i)
        
        val panes = statuses./(s => s -> new TextPane(10, font, 100, 28, 14.3)).toMap + ("log" -> new TextPane(10, font, 80, 44, 10)) + ("action" -> new TextPane(10, font, 100, 36, 16))

        SplitX(SplitEven(statuses./(panes)), SplitX(panes("log"), panes("action")))
            .dim(0, 0, width, height)
        
        panes.view.mapValues(p => (Rect(p.x, p.y, p.w, p.h), p.isInstanceOf[TextPane].?(p.fontSize))).toList./{ case (a, (b, c)) => (a, b, c) }
    }
 
    override def preinfo(self : Option[Faction], aa : List[UserAction]) = {
        val ii = game.info(Nil, self, aa)
        ii.any.??(convertActions(self, ii))
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
            case _ => $(xstyles.choice, xstyles.xx, xstyles.thu)
        } ++
        a @@ {
            case _ : Extra[_] => Nil
            case _ : Choice => $(xstyles.pointer)
            case _ : Cancel => $(xstyles.pointer)
            case _ : OnClickInfo => $(xstyles.pointer)
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
