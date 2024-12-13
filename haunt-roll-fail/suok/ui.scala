package suok
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
import hrf.ui.panes._

object UI extends BaseUI {
    val gaming = suok.gaming
    val mmeta = Meta

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, resources)
}

class UI(uir : ElementAttachmentPoint, arity : Int, resources : Resources) extends GameUI {
    def newPane(styles : Style*) = uir.appendContainer(Div(Div(Div(Content, xstyles.unselectable +: xstyles.inner +: styles.$), xstyles.outer), xstyles.pane), resources)

    val actionDiv = newPane(xstyles.pane.action)
    val logDiv = newPane(xstyles.pane.log)

    val asker = new NewAsker(actionDiv, resources.images.get)
    val logger = new LazyLogger(logDiv.attach)

    val statuses = 1.to(arity)./(n => newPane(styles.status))

    val statusBitmaps = statuses

    def factionStatus(f : Faction, container : Container) {
        if (!game.players.contains(f))
            return

        val _state = game

        import _state.faction2player

        val user = resources.getName(f).|(f.name)
        val ename = (f.alive.?(user.styled(f)).|(" ".pre ~ (" " + user + "  ").pre.styled(styles.deadname)))

        val info = ename.styled(styles.name)

        val knowledge = {
            val expectation = (game.factions :+ HiddenRole)./(e => e -> f.expectation.states./~(_(e)).distinct).toMap
            val knowledge = (game.factions :+ HiddenRole)./(e => e -> f.xknowledge.states./~(_(e)).distinct).toMap

            f.alive.?(
                (game.factions :+ HiddenRole)./(e =>
                    e.elem ~ " = " ~ (expectation(e)./(_.short)).merge ~ knowledge(e).diff(expectation(e)).any.?(" | " ~ knowledge(e).diff(expectation(e))./(_.short).merge)
                ).join(Break)
            ).|(
                (game.factions :+ HiddenRole)./(e =>
                    e.name ~ " = " ~ (e == HiddenRole).?(game.hidden).|(e.role).short
                ).join(Break)
            )

        }

        var over = game.over

        val cards = Div(f.open.?(f.alive.?(f.role.long).|(Image(f.role.image, styles.rcard, styles.dead))).|(over.?(Image(f.role.image, styles.rcard, styles.hidden)).|(Image("hidden", styles.rcard))), styles.viewcard)

        val side = f.role match {
            case _ if !f.open => Empty
            case Vizier if f.choice == Loyal => Image("choice-blue", styles.scard)
            case Vizier if f.choice == Rebel => Image("choice-red", styles.scard)
            case Oracle if f.choice == Loyal => Image("outcome-blue", styles.scard)
            case Oracle if f.choice == Rebel => Image("outcome-red", styles.scard)
            case _ => Empty
        }

        val status = Div(f.coronation.?(Image("crown", styles.scard)) ~ " " ~ side ~ " " ~ f.chained.?(Image("chains", styles.scard)) ~ " " ~ f.jailed.?(Image("prison", styles.scard)) ~ " " ~ (f.tired && f.marked.not).?(Image("sleep", styles.scard)), styles.viewcard)

        container.replace(info ~ Break ~ knowledge ~ Break ~ cards ~ Break ~ status, resources)

        if (f == game.current)
            container.attach.parent.style.outline = "2px solid #eeeeee"
        else
        if (game.highlight.contains(f))
            container.attach.parent.style.outline = "2px dashed #aaaaaa"
        else
            container.attach.parent.style.outline = ""
    }

    def updateStatus() {
        0.until(arity).foreach { n =>
            factionStatus(game.setup(n), statusBitmaps(n))
        }
    }

    def resize() {
        val w = dom.window.innerWidth * 1.0
        val h = dom.window.innerHeight * 1.0
        val font = FontDimensionInfo(72, 40, 72)

        val panes = statuses./(s => s -> new TextPane(10, font, 100, 22, 18.2+9)).toMap + (logDiv -> new TextPane(10, font, 80, 44, 10)) + (actionDiv -> new TextPane(10, font, 100, 36, 18))

        SplitX(SplitEven(statuses./(panes)), SplitX(panes(logDiv), panes(actionDiv)))
            .dim(0, 0, w.toInt, h.toInt)

        panes.keys.foreach { p =>
            val c = p.node
            c.style.left = panes(p).x + "px"
            c.style.top = panes(p).y + "px"
            c.style.width = panes(p).w + "px"
            c.style.height = panes(p).h + "px"
            panes(p) match {
                case t : TextPane => c.style.fontSize = t.fontSize + "px"
                case _ =>
            }
        }

        updateStatus()
    }

    def onClick(a : Any) {}

    def factionElem(f : F) : Elem = Meta.factionElem(f)

    def describeActionForUndo(a : ExternalAction, self : Option[F]) = Nil

    def info(self : Option[Faction], aa : List[UserAction]) = {
        val ii = currentGame.info(Nil, self, aa)
        ii.any.??(ZOption(Empty, Break) +: convertActions(self, ii))
    }

    def preinfo(self : Option[Faction], aa : List[UserAction]) = {
        val ii = currentGame.preinfo(Nil, self, aa)
        ii.any.??(convertActions(self, ii))
    }

    def wait(self : List[Faction], factions : List[Faction]) {
        val zw = List(ZOption("Waiting for " ~ factions./(f => Meta.factionElem(f)).comma, Div(Text("z... z... z..."), $(xstyles.info))))

        if (self.any)
            asker.zask(zw ++ info(self.single, Nil))(resources)
        else
            asker.zask(zw)(resources)
    }

    def notify(factions : List[Faction], infos : List[UserAction], then : => Unit) {
        asker.zask(convertActions(factions.single, infos, _ => then) ++ info(factions.single, Nil))(resources)
    }

    def ask(faction : Option[Faction], actions : List[UserAction], then : UserAction => Unit) {
        asker.zask(preinfo(faction, actions) ++ convertActions(faction, actions, then) ++ info(faction, actions))(resources)
    }

    def convertActions(faction : Option[Faction], actions : List[UserAction], then : UserAction => Unit = null) = {
        actions./~{ a =>
            def q = {
                val q = a.question(currentGame)
                (q == Empty).?(q).|(Div(q))
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
                    case UnavailableReasonAction(_ : ViewCard, _) => List(xstyles.unavailableCard)
                    case _ : Unavailable => List(xstyles.unavailableText)
                    case _ => Nil
                }) ++
                (a match {
                    case _ : Info => List(xstyles.info)
                    case _ => List(xstyles.choice, xstyles.xx)
                }) ++
                (a match {
                    case _ : Extra[_] => Nil
                    case _ : Choice => List(xlo.pointer)
                    case _ : OnClickInfo => List(xlo.pointer)
                    case _ => Nil
                }) ++
                (a match {
                    case UnavailableReasonAction(_ : ViewCard, _) => List(styles.viewcard)
                    case _ : ViewCard => List(styles.viewcard)
                    case _ => Nil
                }) ++
                (faction match {
                    case Some(f) => List(elem.borders.get(f))
                    case _ => Nil
                }) ++
                (a match {
                    case a : Selectable if a.selected => List(styles.selected)
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

    def alog(e : Elem, n : Int, onClick : Any => Unit, delayed : Boolean = false) : LazyBlock = {
        logger.alog(OnClick(n, Hint("Action #" + n, Div(e, xlo.pointer))), onClick, delayed)(resources)
    }

    def blog() : Container = {
        logger.blog()(resources)
    }

    def start() {
        resize()

        dom.window.onresize = e => resize()
    }
}
