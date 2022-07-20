package dwam

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

import hrf.canvas._

import hrf.web._
import hrf.ui._

import hrf.elem._
import hrf.html._

import dwam.elem._

import hrf.ui.again._

object UI extends BaseUI {
    val gaming = dwam.gaming
    
    def create(uir : ElementAttachmentPoint, game: Game, resources: Resources, title : String, saveReplay: (=> Unit) => Unit) = new UI(uir, game, resources)
}

class UI(val uir : ElementAttachmentPoint, val game : Game, val resources : Resources) extends GameUI with UI.GreyUI {
    def factionElem(f : Faction) = f.name.styled(f)

    val statuses = 1.to(game.setup.num)./(i => newPane("status-" + i, Content, styles.status, styles.fstatus))
    val statusGame = newOuterPane("status-game", Content)
    val mapSmall = newPane("map-small", Content)
    val overlayPane = newOuterPane("map-small-overlay", Content)
    
    var overlay = false
    var fullscreen = false
        
    val d = 12

    val mapBitmapSmall = new CachedBitmap(mapSmall.attach.parent)
    var map = mapBitmapSmall
    
    val findAnother = {
        val mplace = resources.getImage("map-regions")
        val placeb = new Bitmap(mplace.width, mplace.height)
        placeb.context.drawImage(mplace, 0, 0)
        val placed = placeb.context.getImageData(0, 0, placeb.width, placeb.height).data
        val place = Array.tabulate(placeb.width, placeb.height)((x, y) => placed((y * placeb.width + x) * 4) * 0x010000 + placed((y * placeb.width + x) * 4 + 1) * 0x0100 + placed((y * placeb.width + x) * 4 + 2))

        (x : Int, y : Int) => {
            val p = place(x)(y)

            if (p == 0)
                throw new Error("0 at " + (x, y))

            var xx = 0
            var yy = 0
            do {
                xx = (placeb.width * math.random()).toInt
                yy = (placeb.height * math.random()).toInt
            }
            while (place(xx)(yy) != p)
            (xx, yy)
        }
    }
        
    case class DrawRect(key : String, x : Int, y : Int, width : Int, height : Int, cx : Int = 0, cy : Int = 0)
    
    case class DrawItem(region : Area, faction : Color, piece : Piece, x : Int, y : Int) {
        val pr = faction.?./(_.short.toLowerCase + "-").|("")

        val rect : DrawRect = piece match {
            case Building => { DrawRect(pr + "building", x - 72, y - 222, 148, 228) }
            case Minion if faction == Demons => { DrawRect("demon", x - 43, y - 139, 85, 143) }
            case Minion if faction == Trolls => { DrawRect("troll", x - 61, y - 95, 122, 100) }
            case Minion => { DrawRect(pr + "minion", x - 52, y - 82, 102, 86) }
            case Trouble => { DrawRect("trouble", x - 50, y - 50, 100, 100) }

            case _ => 
                println($(region : Area, faction : Color, piece : Piece, x : Int, y : Int).mkString(" | "))
                null
        }
    }
        
    var oldPositions : List[DrawItem] = Nil
    var oldGates : List[Area] = Nil
    
    val mp = resources.getImage("map")

    def drawMap() {
        import StandardBoard._
 
        def center(r : Area) = (r match {
            case NapHill => (350, 600)
            case DollySisters => (1350, 550)
            case DragonsLanding => (1550, 650)
            case UnrealEstate => (1050, 850)
            case SevenSleepers => (250, 950)
            case SmallGods => (1250, 950)
            case IsleOfGods => (950, 1150)
            case Longwall => (550, 1250)
            case Dimwell => (650, 1450)
            case TheHippo => (1550, 1450)
            case TheScours => (1250, 1150)
            case TheShades => (1150, 1750)
            
            case _ => (0, 0)

            case _ => (0, 0); throw new Error("no center for " + r)
        })
        
        def gatesXY(r : Area) = r match {
            case null => $((217, 284))
            
            case _ => Nil 
        }

        val bitmap = {
            val width = map.node.clientWidth * dom.window.devicePixelRatio
            val height = map.node.clientHeight * dom.window.devicePixelRatio
        
            val upscale = 2

            val b = map.get(width.round.toInt * upscale, height.round.toInt * upscale)

            b.canvas.style.width = "100%" 
            b.canvas.style.height = "100%"
            
            b
        }
    
        val g = bitmap.context
        g.setTransform(1, 0, 0, 1, 0, 0)

        g.clearRect(0, 0, bitmap.width, bitmap.height)
        
        val dw = d
        val dh = d
   
        if (bitmap.height < bitmap.width || true) {
            if ((dw + mp.width + dw) * bitmap.height < bitmap.width * (dh + mp.height + dh)) {
                g.translate((bitmap.width - (dw + mp.width + dw) * bitmap.height / (dh + mp.height + dh)) / 2, 0)
                g.scale(1.0 * bitmap.height / (dh + mp.height + dh), 1.0 * bitmap.height / (dh + mp.height + dh))
            }
            else {
                g.translate(0, (bitmap.height - (dh + mp.height + dh) * bitmap.width / (dw + mp.width + dw)) / 2)
                g.scale(1.0 * bitmap.width / (dw + mp.width + dw), 1.0 * bitmap.width / (dw + mp.width + dw))
            }
            g.translate(dw, dh)
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
            g.translate(dw, dh)
        }

        g.drawImage(mp, 0, 0)

        var saved = oldPositions
        oldPositions = Nil

        var draws : List[DrawItem] = Nil
        
        state.board.areas.foreach { r =>
            
            var fixed : List[DrawItem] = Nil
            var tofix : List[DrawItem] = Nil
            var all : List[DrawItem] = Nil
            var sticking : List[DrawItem] = Nil
            var free : List[DrawItem] = Nil
            
            var gates = gatesXY(r)

            val figures = state.at(r)
            
            figures.foreach { p => 
                p.piece match {
                    case _ =>
                        saved.find(o => o.region == r && o.piece == p.piece && o.faction == p.faction) match {
                            case Some(o) if o.rect.key == DrawItem(r, p.faction, p.piece, 0, 0).rect.key => 
                                p.piece match {
                                    case _ => 
                                        sticking +:= o
                                        saved = saved.but(o)
                                }
                            case _ => 
                                p.piece match {
                                    case _ => 
                                        free +:= DrawItem(r, p.faction, p.piece, 0, 0)
                                }
                        }
                }
            }

            while (tofix.any && gates.any) {
                val f = tofix.head
                val (x, y) = gates.head
                fixed :+= f.copy(x = x, y = y)
                tofix = tofix.drop(1)
                gates = gates.drop(1)
            }
            
            free ++= tofix

            if (free.num > 3) {
                free = free ++ sticking
                sticking = Nil
            }
            
            def rank(d : DrawItem) = d.piece match {
                case Trouble => 2
                case Minion => 3
                case Building => 5
                case _ => -1
            }
            
            val (px, py) = center(r)

            free.sortBy(d => -rank(d)).foreach { d =>
                sticking +:= Array.tabulate(40)(n => findAnother(px, py)).sortBy { case (x, y) => (abs(x - px) * 5 + abs(y - py)) }.map { case (x, y) => DrawItem(d.region, d.faction, d.piece, x, y) }.minBy { dd =>
                    (draws ++ fixed ++ sticking).map { oo =>
                        val d = dd.rect
                        val o = oo.rect
                        val w = min(o.x + o.width, d.x + d.width) - max(o.x, d.x)
                        val h = min(o.y + o.height, d.y + d.height) - max(o.y, d.y)
                        val s = (w > 0 && h > 0).?(w * h).|(0)
                        s * (1.0 / (o.width * o.height) + 1.0 / (d.width * d.height))
                    }.sum
                }
                    
            }

            draws ++= fixed
            draws ++= sticking
            oldPositions ++= draws
        }

        draws.sortBy(d => d.y).foreach { d =>
            g.drawImage(resources.getImage(d.rect.key), d.rect.x, d.rect.y)
        }

        g.globalAlpha = 1.0
    }
        
    def factionStatus(f : Faction) {
        val container = statuses(game.setup.indexOf(f))

        val _state = state

        import _state._
        

        if (!state.players.contains(f)) {
            container.replace(Div(Div(f.name).styled(f), styles.smallname, xstyles.pointer), resources)
            return
        }

        val name = Div(Div(f.name).styled(f), styles.smallname, xstyles.pointer)

        val hand = Hint("Hand: " + f.hand.num + " cards", 
            f.hand.none.?(Text("~~~")).|(
                (f.hand.num > 5).?(
                    Image("deck", styles.fund5).*(f.hand.num / 5) ~ Image("card-back", styles.fund).*(f.hand.num % 5)
                ).|(
                    Image("card-back", styles.fund).*(f.hand.num).merge
                )
            )
        )

        val moneyhand = Div(f.money.money ~ " |  ".pre.spn(styles.narrow) ~ hand)
        
        val loans = f.loans./(d => Span("Loan".hl, styles.effect)).merge 

        val penalties = f.penalties./(d => Span("Penalty".hl, styles.effect)).merge 

        container.replace(name ~ moneyhand ~ loans ~ penalties, resources)

        if (f == state.current)
            container.attach.parent.style.outline = "2px solid #aaaaaa"
        else
        if (state.highlightFaction.has(f))
            container.attach.parent.style.outline = "2px dashed #aaaaaa"
        else
            container.attach.parent.style.outline = ""
    }

    def gameStatus() {
        val play = Div(OnClick(state.playing, state.playing.img), styles.cardX, styles.inline, styles.nomargin, (state.playing != NoCard).?(xstyles.pointer).|(xstyles.nostyle))

        val cards = Div(
            state.deck.num.hl ~ OnClick("view-deck", Image("deck", styles.pile, xstyles.pointer)) ~ "  ".pre ~ 
            state.pile.num.hl ~ OnClick("view-discard", Image("pile" + state.pile.none.??("-empty"), styles.pile, xstyles.pointer))
        )
        
        statusGame.replace(overlayFit(Div(play ~ cards, styles.gstatus, styles.artwork)), resources, onClick)
    }
    
    def updateStatus() {
        0.until(game.setup.num).foreach { n =>
            factionStatus(game.setup(n))
        }
        gameStatus()
        
        if (overlay)
            overlayPane.show()
        else
            overlayPane.hide()
        
        drawMap()
    }

    mapSmall.attach.parent.ondblclick = (e) => {
        if (fullscreen)
            dom.document.exitFullscreen()
        else
            dom.document.documentElement.requestFullscreen()

        fullscreen = fullscreen.not
    }

    val layouts = $(Layout("base", $(
        BasicPane("status", 13, 6, Priorities(top = 1, right = 1, maxXscale = 1.8, maxYscale = 1.8)),
        BasicPane("status-game", 14, 24, Priorities(maxXscale = 1.8, maxYscale = 1.4)),
        BasicPane("log", 30, 13),
        BasicPane("map-small", 42, 50, Priorities(top = 2, left = 1, grow = -1)),
        BasicPane("action-a", 71-7, 26, Priorities(bottom = 1, right = 1, grow = 1)),
        BasicPane("action-b", 43-7+2+2, 46, Priorities(bottom = 1, right = 1, grow = 1, maxXscale = 1.2))
    )))./~(l => 
        l.copy(name = l.name + "-fulldim", panes = l.panes./{
            case p : BasicPane if p.name == "map-small" => FullDimPane(p.name, p.kX, p.kY, p.pr)
            case p => p
        }, boost = 1.2) :: 
        l.copy(name = l.name + "-plus20", panes = l.panes./{
            case p : BasicPane if p.name == "map-small" => BasicPane(p.name, p.kX * 1.2, p.kY * 1.2, p.pr)
            case p => p
        }, boost = 1.1) ::
        l.copy(name = l.name + "-normal")
    )./~(l => 
        l.copy(name = l.name + "-horizontal", boost = l.boost * 1.02, panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-horizontal", kX = p.kX * state.setup.num)
            case p => p
        }) ::
        l.copy(name = l.name + "-vertical", panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-vertical", kY = p.kY * state.setup.num)
            case p => p
        })
    )./~(l =>
        l.copy(name = l.name + "-actionA", panes = l.panes./~{
            case p : BasicPane if p.name == "action-a" => Some(p.copy(name = "action"))
            case p : BasicPane if p.name == "action-b" => Nil
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-actionB", panes = l.panes./~{
            case p : BasicPane if p.name == "action-a" => Nil
            case p : BasicPane if p.name == "action-b" => Some(p.copy(name = "action"))
            case p => Some(p)
        }) ::
        Nil
    )
    
    val layouter = Layouter(layouts, _./~{
        case f if f.name == "map-small" => $(f, f.copy(name = "map-small-overlay"))
        case f if f.name == "status-horizontal" => 1.to(state.setup.num)./(n => f.copy(name = "status-" + n, x = f.x + ((n - 1) * f.width  / state.setup.num).round.toInt, width  = (n * f.width  / state.setup.num).round.toInt - ((n - 1) * f.width  / state.setup.num).round.toInt))
        case f if f.name == "status-vertical"   => 1.to(state.setup.num)./(n => f.copy(name = "status-" + n, y = f.y + ((n - 1) * f.height / state.setup.num).round.toInt, height = (n * f.height / state.setup.num).round.toInt - ((n - 1) * f.height / state.setup.num).round.toInt))
        case f => $(f)
    })

    def layout(width : Int, height : Int) : List[(String, Rect, Option[Double])] = {
        val lr = layouter.get(width, height)
        
        lr.panes./(p => (p.name, Rect(p.x, p.y, p.width, p.height), Some(lr.fontSize)))
    }
    
    def overlayScrollX(e : Elem) = overlayScroll(e)(styles.seeThroughInner).onClick
    def overlayFitX(e : Elem) = overlayFit(e)(styles.seeThroughInner).onClick

    def showOverlay(e : Elem, onClick : Any => Unit) {
        overlay = true

        overlayPane.show()

        overlayPane.replace(e, resources, onClick)
    }

    def onClick : Any => Unit = {
        case c : Character => 
            showOverlay(overlayFitX(Image("character:" + c.id, styles.artwork)).onClick, onClick)

        case a : Area => 
            showOverlay(overlayFitX(Image("area:" + a.id, styles.artwork)).onClick, onClick)

        case e : EventCard => 
            showOverlay(overlayFitX(Image("event:" + e.id, styles.artwork)).onClick, onClick)
 
        case NoCard => 

        case d : DeckCard => 
            showOverlay(overlayFitX(Image("card:" + d.id, styles.artwork)).onClick, onClick)

        case "view-discard" => 
            showOverlay(overlayScrollX(Div("Discard Pile") ~ 
                state.pile./{ d => OnClick(d, Div(d.img, styles.cardX, xstyles.xx, styles.inline, styles.nomargin, xstyles.pointer)) }.merge).onClick, onClick)

        case "view-deck" => 
            showOverlay(overlayScrollX(Div("Draw Deck") ~ 
                1.to(state.deck.num)./{ n => Div(Image("card-back-large", styles.card), xstyles.info, styles.inline) }.merge).onClick, onClick)
            
        case Nil => 
            overlay = false

            overlayPane.hide()

            overlayPane.clear()

        case x => 
            println("unknown onClick: " + x)
    }

    override def info(self : Option[Faction], aa : List[UserAction]) = {
        val ii = game.info(Nil, self, aa)
        ii.any.??(ZOption(Empty, Break) +: convertActions(self, ii))
    }
    
    override def preinfo(self : Option[Faction], aa : List[UserAction]) = {
        val ii = game.preinfo(Nil, self, aa)
        ii.any.??(convertActions(self, ii))
    }
    
    override def convertActions(faction : Option[Faction], actions : List[UserAction], then : UserAction => Unit = null) = {
        actions./~{ a =>
            def q = {
                val q = a.question(game)
                (q == Empty).?(q).|(Div(q, styles.group))
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
            
            val card = a match {
                case UnavailableReasonAction(_ : ViewObject[DeckCard], _) => true
                case _ : ViewObject[DeckCard] => true
                case _ => false
            }

            def ss = 
                (a match {
                    case _ if unavailable && card => $(xstyles.unavailableCard)
                    case _ if unavailable => $(xstyles.unavailableText)
                    case _ => Nil
                }) ++
                (a match {
                    case _ if card => $(styles.cardX)
                    case _ : Info => $(xstyles.info)
                    case _ => $(xstyles.choice)
                }) ++
                $(xstyles.xx, xstyles.chm, xstyles.chp) ++
                (a match {
                    case _ if card => $(styles.halfmargin)
                    case _ => $(xstyles.thu)
                }) ++
                (a match {
                    case _ : Extra[_] => Nil
                    case _ : Choice => $(xstyles.pointer)
                    case _ : OnClickInfo => $(xstyles.pointer)
                    case _ => Nil
                }) ++
                (a match {
                    case _ if card => $(styles.inline, styles.nomargin)
                    case _ => $(xlo.fullwidth)
                }) ++
                (faction match {
                    case Some(f) => $(elem.borders.get(f))
                    case _ => Nil
                }) ++
                (a match {
                    case a : Selectable if a.selected && card => $(styles.cardS)
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
