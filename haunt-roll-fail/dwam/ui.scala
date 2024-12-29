package dwam
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

import hrf.canvas._

import hrf.web._
import hrf.ui._

import hrf.elem._
import hrf.html._

import dwam.elem._

import hrf.ui.again._
import hrf.ui.sprites._

import scalajs.js.timers.setTimeout


object UI extends BaseUI {
    val mmeta = Meta

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, resources, callbacks)
}

class UI(val uir : ElementAttachmentPoint, arity : Int, val resources : Resources, callbacks : hrf.Callbacks) extends MapGUI {
    def factionElem(f : Faction) = f.name.styled(f)

    val statuses = 1.to(arity)./(i => newPane("status-" + i, Content, styles.status, styles.fstatus, ExternalStyle("hide-scrollbar")))
    val statusGame = newOuterPane("status-game", Content)

    object regions {
        import StandardBoard._

        val centers = Map[Area, XY](
            NapHill -> XY(605, 533),
            DollySisters -> XY(1263, 441),
            DragonsLanding -> XY(1626, 806),
            UnrealEstate -> XY(1096, 666),
            SevenSleepers -> XY(558, 905),
            SmallGods -> XY(1627, 1157),
            IsleOfGods -> XY(967, 1121),
            Longwall -> XY(350, 1244),
            Dimwell -> XY(582, 1659),
            TheHippo -> XY(1508, 1615),
            TheScours -> XY(1212, 1320),
            TheShades -> XY(1060, 1604)
        )

        lazy val place = new IndexedImageRegions[Area](new RawImage(img("map-regions")), 0, 0, centers)

        lazy val select = new IndexedImageRegions[Area](new RawImage(img("map-regions-select")), 0, 0, centers)
    }

    lazy val pieces = new FitLayer[Area, Figure](regions.place, FitOptions(kX = 2))

    lazy val highlighted = new OrderedLayer

    val width = 1936
    val height = 2310
    val margins = Margins(-70, -210, -60, -300)

    lazy val scene = {
        val mp = img("map")
        val mr = img("map-regions-select")

        val background = new OrderedLayer
        background.add(Sprite($(ImageRect(new RawImage(mp), Rectangle(0, 0, mp.width, mp.height), 1.0)), $))(0, 0)


        val areas = new HitLayer(regions.select)

        new Scene($(background, highlighted, areas, pieces), mp.width, mp.height, margins)
    }

    override def adjustCenterZoomX() {
        zoomBase = zoomBase.clamp(-990, 990)

        val qX = (width + margins.left + margins.right) * (1 - 1 / zoom) / 2
        val minX = -qX + margins.right
        val maxX = qX - margins.left
        dX = dX.clamp(minX, maxX)

        val qY = (height + margins.top + margins.bottom) * (1 - 1 / zoom) / 2
        val minY = -qY + margins.bottom
        val maxY = qY - margins.top
        dY = dY.clamp(minY, maxY)
    }

    var highlightCoordinates : |[XY] = None
    var highlightAssassinate = $[Figure]()
    var highlightFire = $[Figure]()
    var highlightBuy = $[Figure]()
    var highlightMove = $[Figure]()
    var highlightRemoveTrouble = $[Figure]()
    var highlightSpreadTrouble = $[Area]()
    var highlightPlaceMinion = Map[Area, $[Color]]()
    var highlightBuild = Map[Area, $[Color]]()
    var highlightUseBuilding = $[Figure]()
    var highlightAreas = $[Area]()

    def processRightClick(target : $[Any], xy : XY) {

    }

    def processHighlight(target : $[Any], xy : XY) {
        highlightCoordinates = |(xy)

        target match {
            case Plaque(r) :: _ => mapSmall.attach.parent.style.cursor = "help"
            case x => mapSmall.attach.parent.style.cursor = "crosshair"
        }

        highlightAssassinate = target match {
            case (r : Area, f : Figure) :: _ if keys.of[AssassinateKey].exists(k => k.color == f.faction && k.area == r && k.index == f.index && f.piece == Minion) => $(f)
            case x => $
        }

        highlightFire = target match {
            case (r : Area, f : Figure) :: _ if keys.of[BurnKey].exists(k => k.color == f.faction && k.area == r  && f.piece == Building) => $(f)
            case x => $
        }

        highlightBuy = target match {
            case (r : Area, f : Figure) :: _ if keys.of[BuyKey].exists(k => k.color == f.faction && k.area == r  && f.piece == Building) => $(f)
            case x => $
        }

        highlightMove = target match {
            case (r : Area, f : Figure) :: _ if keys.of[MoveKey].exists(k => k.color == f.faction && k.from == r && k.index == f.index) => $(f)
            case x => $
        }






        highlightRemoveTrouble = target match {
            case (r : Area, f : Figure) :: _ if keys.of[RemoveTroubleKey].exists(k => f.faction == Troubles && k.area == r) => $(f)
            case x => $
        }

        highlightPlaceMinion = {
            val area = target.of[Area].single
            val kk = area./~(r => keys.of[RecruitKey].%(_.area == r))
            kk./(k => k.area -> $(k.color)).toMap
        }

        highlightBuild = {
            val area = target.of[Area].single
            val kk = area./~(r => keys.of[BuildKey].%(_.area == r))
            kk./(k => k.area -> $(k.color)).toMap
        }

        highlightSpreadTrouble = {
            val area = target.of[Area].single
            val kk = area./~(r => keys.of[SpreadTroubleKey].%(_.area == r))
            kk./(k => k.area)
        }

        highlightUseBuilding = $

        target.of[(Area, Figure)].single.foreach { case (r, f) =>
            keys.of[UseBuildingKey].%(k => k.color == f.faction && k.area == r).single.foreach { k =>
                highlightUseBuilding = $(f)
            }
        }
        if (highlightAssassinate.any)
            mapSmall.attach.parent.style.cursor = "pointer"

        if (highlightFire.any)
            mapSmall.attach.parent.style.cursor = "pointer"

        if (highlightBuy.any)
            mapSmall.attach.parent.style.cursor = "pointer"

        if (highlightRemoveTrouble.any)
            mapSmall.attach.parent.style.cursor = "pointer"

        if (highlightUseBuilding.any)
            mapSmall.attach.parent.style.cursor = "pointer"
    }

    def processTargetClick(target : $[Any], xy : XY) {
        target.of[(Area, Figure)].single.foreach { case (r, f) =>
            keys.of[AssassinateKey].%(k => k.color == f.faction && k.area == r && k.index == f.index).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[(Area, Figure)].single.foreach { case (r, f) =>
            keys.of[BurnKey].%(k => k.color == f.faction && k.area == r).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[(Area, Figure)].single.foreach { case (r, f) =>
            keys.of[BuyKey].%(k => k.color == f.faction && k.area == r).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[(Area, Figure)].single.foreach { case (r, f) =>
            keys.of[MoveKey].%(k => k.color == f.faction && k.from == r && k.index == f.index).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[(Area, Figure)].single.foreach { case (r, f) =>
            keys.of[RemoveTroubleKey].%(k => f.faction == Troubles && k.area == r).single.foreach { k =>
                return onClick(k)
            }
        }







        target.of[Area].single.foreach { case r =>
            keys.of[RecruitKey].%(k => k.area == r).single.foreach { k =>
                k.color.as[Faction].foreach { f =>
                    game.states(f).pool.minion.starting.foreach { m =>
                        pieces.addHint(r, m)(xy.x, xy.y)
                    }
                }
                return onClick(k)
            }
        }

        target.of[Area].single.foreach { case r =>
            keys.of[SpreadTroubleKey].%(k => k.area == r).single.foreach { k =>
                game.pool.trouble.starting.foreach { m =>
                    pieces.addHint(r, m)(xy.x, xy.y)
                }
                return onClick(k)
            }
        }

        target.of[Area].single.foreach { case r =>
            keys.of[BuildKey].%(k => k.area == r).single.foreach { k =>
                k.color.as[Faction].foreach { f =>
                    game.states(f).pool.building.starting.foreach { b =>
                        pieces.addHint(r, b)(xy.x, xy.y)
                    }
                }
                return onClick(k)
            }
        }

        target.of[(Area, Figure)].single.foreach { case (r, f) =>
            keys.of[UseBuildingKey].%(k => k.color == f.faction && k.area == r).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[Plaque].single.foreach { p =>
            return onClick(p.area)
        }

        lastActions.of[Cancel].single.foreach { a =>
            return onClick(a)
        }

        println("processTargetClick unresolved " + target)
    }

    case class Plaque(area : Area)

    def makeScene() : |[Scene] = {
        if (img("map").complete.not | img("map-regions").complete.not | img("map-regions-select").complete.not)
            return None

        pieces.flush()

        game.board.areas.reverse.foreach { r =>
            val figures = game.at(r)

            val extra = highlightPlaceMinion.get(r)./~(l => l.indexed./((f, n) => Figure(f, Minion, -(n + 1)))) ++
                highlightBuild.get(r)./~(l => l.indexed./((f, n) => Figure(f, Building, -(n + 1)))) ++
                highlightSpreadTrouble.%(_ == r)./(_ => Figure(Troubles, Trouble, -(0 + 1)))


            import hrf.ui.sprites._


            pieces.addFixed(r, Figure(Troubles, Trouble, game.board.areas.indexOf(r) + 1000))(Sprite($, $(Rectangle(-120, -60, 240, 60)), $(Plaque(r))))(regions.centers(r).x, regions.centers(r).y)

            (extra ++ figures).foreach { p =>
                def prefix = p.faction.?./(_.short.toLowerCase + "-").|("")



                val assassinationTarget = keys.of[AssassinateKey].exists(k => k.color == p.faction && k.area == r && k.index == p.index)
                val moveTarget = keys.of[MoveKey].exists(k => k.color == p.faction && k.from == r && k.index == p.index)
                val target = assassinationTarget || moveTarget

                val selected = extra.has(p) || keys.of[SelectedKey].exists(k => k.sColor == p.faction && k.sArea == r && k.sIndex == p.index)




                val a = p.piece match {
                    case Building => ImageRect(img(prefix + "building"), 72, 222, 1.0 + 0.4 * selected.??(1))
                    case Minion if p.faction == Demons => ImageRect(img("demon"), 43, 139)
                    case Minion if p.faction == Trolls => ImageRect(img("troll"), 61, 95)
                    case Minion => ImageRect(img(prefix + "minion"), 52, 82, 1.0 + 0.4 * selected.??(1))
                    case Trouble => ImageRect(img("trouble"), 50, 50, 1.0 + 0.4 * selected.??(1))
                }


                var q = p.piece match {
                    case Building => Sprite($(a), $(Rectangle(-60, -200, 124, 206)), $((r, p)))
                    case Minion if p.faction == Demons => Sprite($(a), $(a.rect), $((r, p)))
                    case Minion if p.faction == Trolls => Sprite($(a), $(a.rect), $((r, p)))
                    case Minion => Sprite($(a), $(Rectangle(-50, -30, 100, 30), Rectangle(-20, -80, 40, 50)), $((r, p)))
                    case Trouble => Sprite($(a), $(Rectangle(-44, -44, 88, 88)), $((r, p)))
                    case _ => Sprite($(a), $(a.rect))
                }

                var z = p.piece match {
                    case Trouble => 2
                    case Minion => 3
                    case Building => 3
                }

                if (target && p.piece == Minion) {
                    val highlight = ImageRect(img("minion-highlight"), 52, 82).copy(alpha = 0.5)
                    q = q.copy(images = q.images :+ highlight)
                    z = 4
                }

                if (highlightAssassinate.has(p)) {
                    val kill = ImageRect(img("assassinate"), 76, 146, 0.7)
                    q = q.copy(images = q.images :+ kill)
                    z = 4
                }

                if (highlightFire.has(p)) {
                    val kill = ImageRect(img("fire"), 55, 128, 2)
                    q = q.copy(images = q.images :+ kill)
                    z = 4
                }

                if (highlightBuy.has(p)) {
                    val kill = ImageRect(img("icon:money"), 85, 180, 1)
                    q = q.copy(images = q.images :+ kill)
                    z = 4
                }







                if (highlightRemoveTrouble.has(p)) {
                    val remove = ImageRect(img("remove-trouble"), 70, 70, 0.7)
                    q = q.copy(images = q.images :+ remove)
                    z = 4
                }

                if (extra.has(p)) {
                    q = q.copy(images = q.images./(i => i.copy(alpha = 0.7)), hitboxes = $)
                    pieces.addFixed(r, p, z + 8)(q)(highlightCoordinates.get.x, highlightCoordinates.get.y)
                }
                else {
                    val xy = pieces.addFloat(r, p, z)(q)






                }


            }
        }

        highlighted.clear()

        val highlightAreas = (keys.of[RecruitKey]./(_.area) ++ keys.of[BuildKey]./(_.area) ++ keys.of[SpreadTroubleKey]./(_.area)).distinct

        highlightAreas.some.|(game.highlightAreas).distinct.foreach { r =>
            highlighted.add(Sprite($(ImageRect(img("highlight-" + r.id), 0, 0)), $))(0, 0)
        }

        |(scene)
    }

    def factionStatus(f : Faction) {
        val container = statuses(game.setup.indexOf(f))

        val _state = game

        import _state.faction2player

        val name = resources.getName(f).|(f.name)

        if (!game.states.contains(f)) {
            container.replace(Div(Div(name).styled(f), styles.smallname, xlo.pointer), resources)
            return
        }

        val title = Div(Div(name).styled(f), styles.smallname, xlo.pointer)

        val hand = Hint("Hand: " + f.hand.num + " cards",
            f.hand.none.?(Text("~~~")).|(
                (f.hand.num > 5).?(
                    (f.hand.num / 5).times(Image("deck", styles.fund5)) ~ (f.hand.num % 5).times(Image("card-back", styles.fund))
                ).|(
                    (f.hand.num).times(Image("card-back", styles.fund)).merge
                )
            )
        )

        val moneyhand = Div(f.money.money ~ hrf.elem.FigureSpace.pre ~ hand)(xstyles.larger110)

        val loans = f.loans./(d => " Loan ".hl).merge

        val penalties = f.penalties./(d => " Penalty ".hl).merge


        val actions = keys.of[FactionKey].%(_.target == f)./(k => k.verb.txt.div($(
            xstyles.choice,
            xstyles.xx,
            xstyles.chm,




            elem.borders.get(k.self)
        )).pointer.onClick.param(k))

        container.replace((title ~ moneyhand ~ (loans ~ penalties).div(styles.effect) ~ actions).div(xstyles.fillHeight), resources, onClick)

        if (f == game.current)
            container.attach.parent.style.outline = "2px solid #aaaaaa"
        else
        if (game.highlightFaction.has(f))
            container.attach.parent.style.outline = "2px dashed #aaaaaa"
        else
            container.attach.parent.style.outline = ""
    }

    def gameStatus() {
        val play = Div(OnClick(game.playing, game.playing.img), styles.cardX, styles.inline, styles.nomargin, styles.nopadding, (game.playing != NoCard).?(xlo.pointer).|(xstyles.nostyle))

        val cards = Div(
            game.deck.num.hl ~ OnClick("view-deck", Image("deck", styles.pile, xlo.pointer)) ~ "  ".pre ~
            game.pile.num.hl ~ OnClick("view-discard", Image("pile" + game.pile.none.??("-empty"), styles.pile, xlo.pointer))
        )

        statusGame.replace(overlayFit(Div(play ~ cards, styles.gstatus)), resources, onClick)
    }

    def updateStatus() {
        0.until(arity).foreach { n =>
            factionStatus(game.setup(n))
        }
        gameStatus()

        if (overlayPane.visible)
            overlayPane.vis()
        else
            overlayPane.invis()

        drawMap()
    }

    val layoutZoom = 0.49

    val layouts = $(Layout("base", 
        $(
            BasicPane("status", 13+2, 6+4, Priorities(top = 3, right = 2, maxXscale = 1.8, maxYscale = 1.8)),

            BasicPane("status-game", 18, 32+1, Priorities(maxXscale = 1.1, maxYscale = 1.1)),

            BasicPane("log", 30+2, 13, Priorities(right = 1)),

            BasicPane("map-small", 42+21, 42+21, Priorities(top = 2, left = 1, grow = -1)),
            BasicPane("action-a", 64, 26, Priorities(bottom = 1, right = 3, grow = 1)),
            BasicPane("action-b", 40+1+1+1+1+11, 46+1, Priorities(bottom = 1, right = 3, grow = 1, maxXscale = 1.2))
        )
       ./(p => p.copy(kX = p.kX * layoutZoom, kY = p.kY * layoutZoom))
    ))./~(l =>
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
            case p : BasicPane if p.name == "status" => p.copy(name = "status-horizontal", kX = p.kX * arity)
            case p => p
        }) ::
        l.copy(name = l.name + "-vertical", panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-vertical", kY = p.kY * arity)
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
        case f if f.name == "action" => $(f, f.copy(name = "undo"), f.copy(name = "settings"))
        case f if f.name == "map-small" => $(f, f.copy(name = "map-small-overlay"))
        case f if f.name == "status-horizontal" => 1.to(arity)./(n => f.copy(name = "status-" + n, x = f.x + ((n - 1) * f.width  /~/ arity), width  = (n * f.width  /~/ arity) - ((n - 1) * f.width  /~/ arity)))
        case f if f.name == "status-vertical"   => 1.to(arity)./(n => f.copy(name = "status-" + n, y = f.y + ((n - 1) * f.height /~/ arity), height = (n * f.height /~/ arity) - ((n - 1) * f.height /~/ arity)))
        case f => $(f)
    })

    val settingsKey = Meta.settingsKey

    val layoutKey = "v" + 1 + "." + "arity-" + arity

    def overlayScrollX(e : Elem) = overlayScroll(e)(styles.seeThroughInner).onClick
    def overlayFitX(e : Elem) = overlayFit(e)(styles.seeThroughInner).onClick

    def showOverlay(e : Elem, onClick : Any => Unit) {
        overlayPane.vis()
        overlayPane.replace(e, resources, onClick, _ => {}, _ => {})
    }

    override def onClick(a : Any) = a @@ {
        case ("notifications", Some(f : Faction)) =>
            shown = $
            showNotifications($(f))

        case ("notifications", None) =>
            shown = $
            showNotifications(game.factions)

        case "areas" =>
            showOverlay(overlayScrollX(Div("Areas") ~
                game.board.areas./{ d => OnClick(d, Div(d.img, styles.cardX, xstyles.xx, styles.inline, styles.nomargin, xlo.pointer)) }.merge).onClick, onClick)

        case "personalities" =>
            showOverlay(overlayScrollX(Div("Personalities") ~
                Characters.forPlayers(game.setup.num)./{ d => OnClick(d, Div(d.img, styles.cardX, xstyles.xx, styles.inline, styles.nomargin, xlo.pointer)) }.merge).onClick, onClick)

        case c : Character =>
            showOverlay(overlayFitX(Image("character:" + c.id, styles.artwork)).onClick, onClick)

        case area : Area =>
            val actions = lastActions.%(_.unwrap @@ {
                case UseBuildingMainAction(_, r, _) if r == area => true
                case a : BuyKey if a.area == area => true
                case _ => false
            })

            if (actions.any)
                showOverlay(overlayScrollX(Image("area:" + area.id, styles.artwork) ~
                    actions./(a => convertActions(None, $(a), onClick).only.option.pointer.onClick.param(a).div(xstyles.xx)(xstyles.width60ex))
                ).onClick, onClick)
            else
                showOverlay(overlayFitX(Image("area:" + area.id, styles.artwork)).onClick, onClick)

        case e : EventCard =>
            showOverlay(overlayFitX(Image("event:" + e.id, styles.artwork)).onClick, onClick)

        case NoCard =>

        case card : DeckCard =>
            val actions = lastActions.%(_.unwrap @@ {

                case GiveCardsAction(_, _, $(d), _) if card == d => true
                case DiscardCardsAction(_, $(d), _) if card == d => true
                case a : CardKey if a.card == card => true
                case _ => false
            })

            if (actions.any) {
                showOverlay(overlayFitX(Image("card:" + card.id, styles.artwork84) ~
                    actions./(a => convertActions(None, $(a), onClick).only.option.pointer.onClick.param(a).div(xstyles.xx)(xstyles.width60ex))
                ).onClick, onClick)
            }
            else
                showOverlay(overlayFitX(Image("card:" + card.id, styles.artwork)).onClick, onClick)

        case action : Action if lastThen != null =>
            clearOverlay()

            highlightAssassinate = $
            highlightFire = $
            highlightBuy = $
            highlightMove = $
            highlightRemoveTrouble = $
            highlightSpreadTrouble = $
            highlightPlaceMinion = Map()
            highlightBuild = Map()

            val then = lastThen
            lastThen = null
            lastActions = $
            keys = $

            asker.clear()

            then(action.as[UserAction].||(action.as[ForcedAction]./(_.as("Do Action On Click"))).|(throw new Error("non-user non-forced action in on click handler")))


        case "view-discard" =>
            showOverlay(overlayScrollX(Div("Discard Pile") ~
                game.pile./{ d => OnClick(d, Div(d.img, styles.cardX, xstyles.xx, styles.inline, styles.nomargin, xlo.pointer)) }.merge).onClick, onClick)

        case "view-deck" =>
            showOverlay(overlayScrollX(Div("Draw Deck") ~
                1.to(game.deck.num)./{ n => Div(Image("card-back-large", styles.card), styles.cardX, xstyles.xx, styles.inline, styles.nomargin) }.merge).onClick, onClick)

        case Nil =>
            clearOverlay()

        case x =>
            println("unknown onClick: " + x)
    }

    def clearOverlay() {
        overlayPane.invis()
        overlayPane.clear()
    }

    override def info(self : |[Faction], aa : $[UserAction]) = {
        val ii = currentGame.info($, self, aa)
        ii.any.??($(ZOption(Empty, Break)) ++ convertActions(self.of[Faction], ii)) ++
            $(ZBasic(Break ~ Break, "Areas".hh, () => { onClick("areas") }).copy(clear = false)) ++
            $(ZBasic(Break ~ Break, "Personalities".hh, () => { onClick("personalities") }).copy(clear = false)) ++
            (currentGame.isOver && hrf.HRF.flag("replay").not).$(
                ZBasic(Break ~ Break ~ Break, "Save Replay As File".hh, () => {
                    showOverlay(overlayScrollX("Saving Replay...".hl.div).onClick, null)

                    callbacks.saveReplay {
                        overlayPane.invis()
                        overlayPane.clear()
                    }
                }).copy(clear = false)
            ) ++
            (hrf.HRF.param("lobby").none && hrf.HRF.offline.not).$(
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
                }).copy(clear = false)
            ) ++
            $(ZBasic(Break ~ Break, "Notifications".spn, () => { onClick("notifications", self) }).copy(clear = false)).%(_ => self.any || game.isOver) ++
            $(ZBasic(Break ~ Break, "Interface".spn, () => { callbacks.editSettings { updateStatus() } }).copy(clear = false))
    }

    override def preinfo(self : |[Faction], aa : $[UserAction]) = {
        val ii = currentGame.preinfo(Nil, self, aa)
        ii.any.??(convertActions(self, ii))
    }

    var shown : $[Notification] = $

    override def showNotifications(self : $[F]) : Unit = {
        val newer = game.notifications
        val older = shown

        shown = game.notifications

        val display = newer.diff(older).%(_.factions.intersect(self).any)./~(n => convertActions(self.single, n.infos)).some./~(_ :+ ZOption(Empty, Break))

        if (display.none)
            return

        overlayPane.vis()

        overlayPane.attach.clear()

        val ol = overlayPane.attach.appendContainer(overlayScrollX(Content), resources, onClick)

        val asker = new NewAsker(ol, img)

        asker.zask(display)(resources)
    }

    override def wait(self : $[F], factions : $[F]) {
        lastActions = $
        lastThen = null

        showNotifications(self)

        super.wait(self, factions)
    }

    var lastActions : $[UserAction] = $
    var lastThen : UserAction => Unit = null

    var keys = $[Key]()

    override def ask(faction : |[F], actions : $[UserAction], then : UserAction => Unit) {
        lastActions = actions
        lastThen = then

        showNotifications(faction.$)

        keys = actions./~(a => a.as[Key] || a.unwrap.as[Key])

        keys ++= actions.of[SoftKeys].%(_.isSoft)./~(game.performContinue(None, _, false).continue match {
            case Ask(f, l) if faction.has(f) => l./~(a => a.as[Key] || a.unwrap.as[Key])
            case _ => $()
        })

        updateStatus()

        super.ask(faction, actions, a => {
            clearOverlay()
            keys = $
            then(a)
        })
    }

    override def convertActions(faction : |[Faction], actions : $[UserAction], then : UserAction => Unit = null) = {
        actions./~{ a =>
            def q = {
                val q = a.question(currentGame)

                (q == Empty).?(q).|(Div(q)(xlo.fullwidth)(styles.group))
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

            val notYet = a match {
                case _ : UnavailableYetAction => true
                case _ => false
            }

            val card = a match {
                case UnavailableReasonAction(_ : ViewObject[_], _) => true
                case _ : ViewObject[_] => true
                case _ => false
            }

            def ss =
                (a match {
                    case _ if unavailable && notYet => $(xstyles.unavailableCard)
                    case _ if unavailable && card => $(xstyles.unavailableCard)
                    case _ if unavailable => $(xstyles.unavailableText)
                    case _ => $()
                }) ++
                (a match {
                    case _ if card => $(styles.cardX)
                    case _ : Info => $(xstyles.info)
                    case _ => $(xstyles.choice)
                }) ++
                $(xstyles.xx, xstyles.chm, xstyles.chp) ++
                (a match {
                    case _ if card => $(styles.halfmargin)
                    case _ => $(xstyles.thu, xstyles.thumargin)
                }) ++
                (a match {
                    case _ : Extra[_] => $()
                    case _ : Choice => $(xlo.pointer)
                    case _ : OnClickInfo => $(xlo.pointer)
                    case _ => $()
                }) ++
                (a match {
                    case _ if card => $(styles.inline, styles.nomargin)
                    case _ => $(xlo.fullwidth)
                }) ++
                (faction match {
                    case Some(f) => $(elem.borders.get(f))
                    case _ => $()
                }) ++
                (a match {
                    case a : Selectable if a.selected && card => $(styles.cardS)
                    case a : Selectable if a.selected => $(styles.selected)
                    case _ => $()
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
