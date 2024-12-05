package arcs
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

import arcs.elem._

import hrf.ui.again._
import hrf.ui.sprites._

import scalajs.js.timers.setTimeout


object UI extends BaseUI {
    val mmeta = Meta

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[hrf.meta.GameOption], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, options, resources, callbacks)
}

class UI(val uir : ElementAttachmentPoint, arity : Int, val options : $[hrf.meta.GameOption], val resources : Resources, callbacks : hrf.Callbacks) extends MapGUI {
    def factionElem(f : Faction) = f.name.styled(f)

    val statuses = 1.to(arity)./(i => newPane("status-" + i, Content, styles.status, styles.fstatus, ExternalStyle("hide-scrollbar")))

    val campaign : Boolean = options.of[CampaignOption].any

    def starport : String = callbacks.settings.has(StarStarports).?("starport-alt").|("starport")

    val court : CanvasPane = newCanvasPane("court", 2) { bitmap =>
        val n = 4 + campaign.??(1)
        val d = 12

        val cards = new OrderedLayer

        0.until(n).foreach { i =>
            if (i < game.market.num) {
                val c = game.market.$(i)

                cards.add(Sprite($(ImageRect(new RawImage(img(c.id)), Rectangle(0, 0, 744, 1039), 1.0)), $))(744*i + d*i, 1039*0)
            }
        }

        0.until(4).foreach { i =>
            if (i < game.market.num) {
                val c = game.market.$(i)

                val l = game.figures.get(Influence(c))

                val scale = 2.6
                val shadow = 3
                l.foreach { u =>
                    val w = (l.num < 7).?(112).|(744 / l.num)
                }

                l.foreach { u =>
                    val w = (l.num < 7).?(112).|(744 / l.num)
                    cards.add(Sprite($(ImageRect(new RawImage(img("agent-background")), Rectangle(0, 0, (42+shadow+shadow)*scale, (68+shadow+shadow)*scale), 1.0)), $))(744*i + d*i + 744/2 - w / 2 * l.num + w * l.indexOf(u) - shadow*scale, 400 - shadow*scale)
                    cards.add(Sprite($(ImageRect(new RawImage(img(u.faction.short + "-agent")), Rectangle(0, 0, 42*scale, 68*scale), 1.0)), $))(744*i + d*i + 744/2 - w / 2 * l.num + w * l.indexOf(u), 400)
                }
            }
        }

        val scene = new Scene($(cards), 744*n + d*3, 1039*1, Margins(d, d, d, d))

        if (resources.images.incomplete.none)
            scene.render(bitmap.context, bitmap.width, bitmap.height, 1, 0, 0)

        if (resources.images.incomplete.any)
            setTimeout(min(25, resources.images.incomplete.num) * 20)(court.draw())

        resources.images.incomplete = $
    }

    court.container.attach.parent.onclick = (e) => {
        val offsetX = e.offsetX * dom.window.devicePixelRatio
        val offsetY = e.offsetY * dom.window.devicePixelRatio

        val width = court.container.attach.parent.clientWidth * dom.window.devicePixelRatio * upscale
        val height = court.container.attach.parent.clientHeight * dom.window.devicePixelRatio * upscale

        onClick(game.market((offsetX.~ * (4 + campaign.??(1)) /↓ width.~).clamp(0, 3)))
    }

    val ambitions : |[CanvasPane] = campaign.?(newCanvasPane("ambitions", 2) { bitmap =>
        val mp = img("ambitions")

        val background = new OrderedLayer
        background.add(Sprite($(ImageRect(new RawImage(mp), Rectangle(0, 0, mp.width, mp.height), 1.0)), $))(0, 0)

        val tokens = new OrderedLayer

        game.ambitionable.lift(0)./(m => tokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 123, 139), 1.0)), $))(20, 82))
        game.ambitionable.lift(1)./(m => tokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 123, 139), 1.0)), $))(154, 82))
        game.ambitionable.lift(2)./(m => tokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 123, 139), 1.0)), $))(288, 82))

        game.declared.get(Tycoon)./(l => l.indexed./{ (m, i) =>
            tokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 123, 139), 1.0)), $))(154 + 134*(2*i - l.num + 1)/2, 270)
        })
        game.declared.get(Tyrant)./(l => l.indexed./{ (m, i) =>
            tokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 123, 139), 1.0)), $))(154 + 134*(2*i - l.num + 1)/2, 470)
        })
        game.declared.get(Warlord)./(l => l.indexed./{ (m, i) =>
            tokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 123, 139), 1.0)), $))(154 + 134*(2*i - l.num + 1)/2, 668)
        })
        game.declared.get(Keeper)./(l => l.indexed./{ (m, i) =>
            tokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 123, 139), 1.0)), $))(154 + 134*(2*i - l.num + 1)/2, 867)
        })
        game.declared.get(Empath)./(l => l.indexed./{ (m, i) =>
            tokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 123, 139), 1.0)), $))(154 + 134*(2*i - l.num + 1)/2, 1067)
        })

        val scene = new Scene($(background, tokens), mp.width, mp.height, Margins(0, 0, 0, 0))

        if (resources.images.incomplete.none)
            scene.render(bitmap.context, bitmap.width, bitmap.height, 1, 0, 0)

        if (resources.images.incomplete.any)
            setTimeout(min(25, resources.images.incomplete.num) * 20)(ambitions.get.draw())

        resources.images.incomplete = $
    })

    object regions {
        val centers = Map[System, XY](
            System(1, Gate) -> XY(1300, 550),
            System(1, Arrow) -> XY(1050, 360),
            System(1, Crescent) -> XY(1320, 130),
            System(1, Hex) -> XY(1630, 400),
            System(2, Gate) -> XY(1630, 780),
            System(2, Arrow) -> XY(1810, 580),
            System(2, Crescent) -> XY(1920, 730),
            System(2, Hex) -> XY(1900, 900),
            System(3, Gate) -> XY(1590, 1110),
            System(3, Arrow) -> XY(1860, 1060),
            System(3, Crescent) -> XY(2110, 1420),
            System(3, Hex) -> XY(1670, 1370),
            System(4, Gate) -> XY(1170, 1260),
            System(4, Arrow) -> XY(1350, 1660),
            System(4, Crescent) -> XY(940, 1700),
            System(4, Hex) -> XY(570, 1630),
            System(5, Gate) -> XY(870, 990),
            System(5, Arrow) -> XY(640, 1230),
            System(5, Crescent) -> XY(240, 1160),
            System(5, Hex) -> XY(530, 900),
            System(6, Gate) -> XY(910, 690),
            System(6, Arrow) -> XY(630, 730),
            System(6, Crescent) -> XY(630, 520),
            System(6, Hex) -> XY(830, 450),
        )

        val gates = Map[System, $[XY]](
            System(1, Arrow) -> $(XY(1049, 223), XY(1166, 170)),
            System(1, Crescent) -> $(XY(1434, 212)),
            System(1, Hex) -> $(XY(1745, 147), XY(1846, 228)),
            System(2, Arrow) -> $(XY(2010, 440)),
            System(2, Crescent) -> $(XY(2300, 618)),
            System(2, Hex) -> $(XY(2116, 880), XY(2221, 936)),
            System(3, Arrow) -> $(XY(2186, 1127)),
            System(3, Crescent) -> $(XY(1846, 1249)),
            System(3, Hex) -> $(XY(1929, 1534), XY(1830, 1610)),
            System(4, Arrow) -> $(XY(1529, 1573), XY(1430, 1497)),
            System(4, Crescent) -> $(XY(1060, 1584), XY(1159, 1660)),
            System(4, Hex) -> $(XY(776, 1505)),
            System(5, Arrow) -> $(XY(255, 1458)),
            System(5, Crescent) -> $(XY(434, 1101)),
            System(5, Hex) -> $(XY(223, 876), XY(125, 952)),
            System(6, Arrow) -> $(XY(431, 683)),
            System(6, Crescent) -> $(XY(397, 313), XY(299, 389)),
            System(6, Hex) -> $(XY(678, 228)),
        )

        lazy val place = new IndexedImageRegions[System](new RawImage(img("map-regions")), 0, 0, centers)

        lazy val select = new IndexedImageRegions[System](new RawImage(img("map-regions-select")), 0, 0, centers)
    }

    lazy val pieces = new FitLayer[System, Figure](regions.place, FitOptions())

    lazy val outOfPlay = new OrderedLayer

    lazy val highlighted = new OrderedLayer

    lazy val ambTokens = new OrderedLayer

    val width = 2528
    val height = 1776
    val margins = Margins(0, 0, 0, 0)

    lazy val scene = {
        val mp = img("map-no-slots")
        val mr = img("map-regions")
        val ms = img("map-regions-select")

        val background = new OrderedLayer
        background.add(Sprite($(ImageRect(new RawImage(mp), Rectangle(0, 0, mp.width, mp.height), 1.0)), $))(0, 0)

        val areas = new HitLayer(regions.select)

        new Scene($(background, outOfPlay, highlighted, areas, pieces, ambTokens), mp.width, mp.height, margins)
    }

    override def adjustCenterZoomX() {
        zoomBase = zoomBase.clamp(-990, 990*2)

        val qX = (width + margins.left + margins.right) * (1 - 1 / zoom) / 2
        val minX = -qX + margins.right - zoomBase / 5
        val maxX = qX - margins.left + zoomBase / 5
        dX = dX.clamp(minX, maxX)

        val qY = (height + margins.top + margins.bottom) * (1 - 1 / zoom) / 2
        val minY = -qY + margins.bottom - zoomBase / 5
        val maxY = qY - margins.top + zoomBase / 5
        dY = dY.clamp(minY, maxY)
    }

    var highlightCoordinates : |[XY] = None
    var highlightAssassinate = $[Figure]()
    var highlightFire = $[Figure]()
    var highlightBuy = $[Figure]()
    var highlightMove = $[Figure]()
    var highlightRemoveTrouble = $[Figure]()
    var highlightSpreadTrouble = $[System]()
    var highlightPlaceMinion = Map[System, $[Color]]()
    var highlightBuild = Map[System, $[Color]]()
    var highlightUseBuilding = $[Figure]()
    var highlightAreas = $[System]()

    def processRightClick(target : $[Any], xy : XY) {
        // lastActions.of[Cancel].single.foreach(onClick)
    }

    def processHighlight(target : $[Any], xy : XY) {
        highlightCoordinates = |(xy)
    }

    def processTargetClick(target : $[Any], xy : XY) {
        lastActions.of[Cancel].single.foreach { a =>
            return onClick(a)
        }

        println("processTargetClick unresolved " + target)
    }

    case class Plaque(area : System)

    def makeScene() : |[Scene] = {
        if (img("map-no-slots").complete.not | img("map-regions").complete.not | img("map-regions-select").complete.not)
            return None

        outOfPlay.clear()

        1.to(6).diff(game.board.clusters).foreach { i =>
            val mo = img("map-out-" + i)

            outOfPlay.add(Sprite($(ImageRect(new RawImage(mo), Rectangle(0, 0, mo.width, mo.height), 1.0)), $))(0, 0)
        }

        1.to(6).diff(game.board.clusters).intersect($(3)).$.starting.foreach { i =>
            val am = img("map-ambitions-" + i)

            outOfPlay.add(Sprite($(ImageRect(new RawImage(am), Rectangle(0, 0, am.width, am.height), 1.0)), $))(0, 0)
        }

        pieces.flush()

        systems.reverse.foreach { s =>
            val figures = game.at(s)
            var gates = regions.gates.get(s).|(Nil).sortBy(_.y)

            val extra : $[Figure] = $

            import hrf.ui.sprites._

            (extra ++ figures ++ 1.to(game.freeSlots(s))./(i => Figure(Free, Slot, systems.indexOf(s) * 10 + i))).foreach { p =>
                def prefix = p.faction.as[Faction]./(_.short.toLowerCase + "-").|((p.faction == Empire).?("imperial-").|(""))

                val target = false

                val selected = extra.has(p)

                val status = (p.piece != Slot).??(p.faction.damaged.has(p).??("-damaged"))

                val a = p.piece match {
                    case Slot => $(ImageRect(img("city-empty"), 61, 61, 1.0 + 0.4 * selected.??(1)).copy(alpha = 0.4))
                    case City => $(ImageRect(img(prefix + "city" + status), 61, 61, 1.0 + 0.4 * selected.??(1)))
                    case Starport => $(ImageRect(img(prefix + starport + status), 61, 61, 1.0 + 0.4 * selected.??(1)))
                    case Ship => $(ImageRect(img(prefix + "ship" + status), 97, 71, 1.0 + 0.4 * selected.??(1)))
                    case Blight => $(ImageRect(img("blight" + status), 43, 79, 1.0 + 0.4 * selected.??(1)))
                }

                var q = p.piece match {
                    case Slot | City | Starport => Sprite(a, $(Rectangle(-60, 21, 120, 28), Rectangle(-46, -7, 92, 28), Rectangle(-32, -35, 64, 28), Rectangle(-18, -54, 36, 19)), |((s, p)))
                    case Ship => Sprite(a, $(Rectangle(-70, -10, 135, 20), Rectangle(-97, -30, 194, 20), Rectangle(5, -50, 75, 20), Rectangle(35, -70, 35, 20)), |((s, p)))
                    case _ => Sprite(a, $(a.head.rect))
                }

                var z = p.piece match {
                    case Ship | Blight => 4
                    case Slot | City | Starport => 3
                }

                if (extra.has(p)) {
                    q = q.copy(images = q.images./(i => i.copy(alpha = 0.7)), hitboxes = $)
                    pieces.addFixed(s, p, z + 8)(q)(highlightCoordinates.get.x, highlightCoordinates.get.y)
                }
                else
                if (p.piece == Starport && gates.any) {
                    gates.starting.foreach { g =>
                        pieces.addFixed(s, p, z)(q)(g.x, g.y)
                    }
                    gates = gates.dropFirst
                }
                else
                if (p.piece.is[Building] && gates.any) {
                    gates.ending.foreach { g =>
                        pieces.addFixed(s, p, z)(q)(g.x, g.y)
                    }
                    gates = gates.dropLast
                }
                else {
                    val xy = pieces.addFloat(s, p, z)(q)
                }
            }
        }

        highlighted.clear()

        ambTokens.clear()

        1.to(6).diff(game.board.clusters).intersect($(3)).$.starting.foreach { i =>
            val x1 = 1724
            val y1 = 1017

            game.ambitionable.lift(0)./(m => ambTokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 111, 125), 1.0)), $))( 34+x1, 82+y1))
            game.ambitionable.lift(1)./(m => ambTokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 111, 125), 1.0)), $))(154+x1, 82+y1))
            game.ambitionable.lift(2)./(m => ambTokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 111, 125), 1.0)), $))(274+x1, 82+y1))

            game.declared.get(Tycoon)./(l => l.indexed./{ (m, i) =>
                ambTokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 111, 125), 1.0)), $))(154 + 120*(2*i - l.num + 1)/2+x1, 250+y1)
            })

            val x2 = 1724 + 391
            val y2 = 1017 - 359
            game.declared.get(Tyrant)./(l => l.indexed./{ (m, i) =>
                ambTokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 111, 125), 1.0)), $))(154 + 120*(2*i - l.num + 1)/2+x2, 430+y2)
            })
            game.declared.get(Warlord)./(l => l.indexed./{ (m, i) =>
                ambTokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 111, 125), 1.0)), $))(154 + 120*(2*i - l.num + 1)/2+x2, 608+y2)
            })
            game.declared.get(Keeper)./(l => l.indexed./{ (m, i) =>
                ambTokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 111, 125), 1.0)), $))(154 + 120*(2*i - l.num + 1)/2+x2, 787+y2)
            })
            game.declared.get(Empath)./(l => l.indexed./{ (m, i) =>
                ambTokens.add(Sprite($(ImageRect(new RawImage(img("ambition-values-" + m.high + "-" + m.low)), Rectangle(0, 0, 111, 125), 1.0)), $))(154 + 120*(2*i - l.num + 1)/2+x2, 967+y2)
            })

            if (game.factions.forall(game.states.contains)) {
                if (game.factions.exists(_.loyal.has(MaterialCartel)).not)
                1.to(game.availableNum(Material)).foreach { i =>
                    ambTokens.add(Sprite($(ImageRect(new RawImage(img(Material.id)), Rectangle(-32, -32, 64, 64), 1.0)), $))(1933 - 68*2, 1390 + i * 68)
                }

                if (game.factions.exists(_.loyal.has(FuelCartel)).not)
                1.to(game.availableNum(Fuel)).foreach { i =>
                    ambTokens.add(Sprite($(ImageRect(new RawImage(img(Fuel.id    )), Rectangle(-32, -32, 64, 64), 1.0)), $))(1933 - 68*1, 1390 + i * 68)
                }

                1.to(game.availableNum(Weapon)).foreach { i =>
                    ambTokens.add(Sprite($(ImageRect(new RawImage(img(Weapon.id  )), Rectangle(-32, -32, 64, 64), 1.0)), $))(1933 + 68*0, 1390 + i * 68)
                }

                1.to(game.availableNum(Relic)).foreach { i =>
                    ambTokens.add(Sprite($(ImageRect(new RawImage(img(Relic.id   )), Rectangle(-32, -32, 64, 64), 1.0)), $))(1933 + 68*1, 1390 + i * 68)
                }

                1.to(game.availableNum(Psionic)).foreach { i =>
                    ambTokens.add(Sprite($(ImageRect(new RawImage(img(Psionic.id )), Rectangle(-32, -32, 64, 64), 1.0)), $))(1933 + 68*2, 1390 + i * 68)
                }
            }
        }

        |(scene)
    }

    def factionStatus(f : Faction) {
        val container = statuses(game.seating.indexOf(f))

        val name = resources.getName(f).|(f.name)

        if (!game.states.contains(f)) {
            container.replace(Div(Div(name).styled(f), styles.smallname, xlo.pointer), resources)
            return
        }

        val initative = game.seized.%(_ == f)./(_ => DoubleDagger).||((game.factions.first == f && game.seized.none).?(Dagger)).|("")

        val title = Div(Div(initative.styled(styles.title)(styles.initative) ~ name).styled(f), styles.smallname, xlo.pointer)
        val hand = Hint("Hand: " + f.hand.num + " cards",
            f.hand.none.?("~~~".txt ~ Image("card-back-small", styles.fund, xlo.hidden)).|(
                (f.hand.num > 55).?(
                    (f.hand.num / 5).times(Image("card-back-5", styles.fund)) ~ (f.hand.num % 5).times(Image("card-back-small", styles.fund))
                ).|(
                    (f.hand.num).times(Image("card-back-small", styles.fund)).merge
                )
            )
        )

        val powerHand = (f.power.power ~ " ".pre ~ hand).div(xstyles.larger110)

        val outrage = f.outraged./(r => Image(r.name + "-outrage", styles.token)).merge.div(styles.outrageLine)

        val ww = max(f.resourceSlots, f.resources.num)
        val keys = (f.keys.take(f.resourceSlots)./(n => Image("half-keys-" + n, styles.token)) ++ (ww - f.resourceSlots).times(Image("half-keys-1", styles.token)(xstyles.hidden))).merge.div(styles.keyLine)
        val res = (f.resources./(r => Image(r.name, styles.token)).merge ~ (ww - f.resources.num).times(Image("nothingness", styles.token))).div
        val pieces = (
            (
                (5 - f.pooled(City)).hlb.styled(xstyles.smaller85) ~ "×" ~ Image(f.short + "-city", styles.building) ~ " " ~
                (5 - f.pooled(Starport)).hlb.styled(xstyles.smaller85) ~ "×" ~ Image(f.short + "-" + starport, styles.building)
            ).& ~ " " ~
            (
                (15 - f.pooled(Ship)).hlb.styled(xstyles.smaller85) ~ "×" ~ Image(f.short + "-ship", styles.ship) ~ " " ~
                (10 - f.pooled(Agent) - f.outraged.num).hlb.styled(xstyles.smaller85) ~ "×" ~ Image(f.short + "-agent", styles.ship)
            ).&
        ).div

        val trophies = f.trophies./( u =>
            Image(u.faction.short + "-" + u.piece.name + "-damaged", u.piece.is[Building].?(styles.building).|(styles.ship))
        )./(_.&).join(" ").div

        val captives = f.captives./( u =>
            Image(u.faction.short + "-" + u.piece.name, u.piece.is[Building].?(styles.building).|(styles.ship))
        )./(_.&).join(" ").div

        val leader = f.leader.$./(l => l.elem.div(xstyles.smaller75)(styles.cardName).pointer.onClick.param(l)).merge
        val lores = f.lores./(l => l.elem.div(xstyles.smaller75)(styles.cardName).pointer.onClick.param(l)).merge

        val loyal = f.loyal.$.of[GuildCard]./(c => ((Image("keys-" + c.keys, styles.tokenTop) ~ c.elem ~ Image(c.suit.name, styles.tokenTop)).div(xstyles.smaller75) ~ (c @@ {
            case MaterialCartel => game.availableNum(Material).times(Image(Material.name, styles.tokenTop)).merge
            case FuelCartel => game.availableNum(Fuel).times(Image(Fuel.name, styles.tokenTop)).merge
            case _ => Empty
        })).pointer.onClick.param(c)).merge

        val play = f.played.starting./ { d =>
            (Image((game.zeroed && f == game.factions(0)).?("zeroed").|(d.suit.name + "-number-" + d.strength), styles.plaque) ~ Image((game.lead.get.suit != d.suit).?(d.suit + "-pips-pivot").|(d.suit + "-pips-" + d.pips), styles.plaque) ~ Image(d.suit + "-plaque", styles.plaque)).div(styles.plaqueContainer)
        }.||(f.blind.starting./ { d =>
            (Image("hidden", styles.plaque) ~ Image(game.lead.get.suit + "-pips-copy", styles.plaque) ~ Image(game.lead.get.suit + "-plaque", styles.plaque)).div(styles.plaqueContainer)
        }).|(Empty)

        val seized = game.seized.%(_ == f)./(_ => "Seized Initative".hl.div(styles.title)(xstyles.smaller50)).||((game.factions(0) == f && game.seized.none).?("Initative".hl.div(styles.title)(xstyles.smaller50))).|(Empty)

        val content = ((title ~ powerHand ~ leader ~ lores ~ outrage ~ keys ~ res ~ loyal ~ trophies ~ captives).div ~ "~".txt.div(xstyles.hidden) ~ play).div(styles.statusUpper)(xlo.flexVX)(ExternalStyle("hide-scrollbar")).pointer.onClick.param(f) ~
            play.div(styles.play)

        container.replace(content, resources, {
            case f : Faction => onFactionStatus(f, false)
            case x => onClick(x)
        })

        if (f == game.current && game.isOver)
            container.attach.parent.style.background = f @@ {
                case Red => "#680016"
                case Yellow => "#684f19"
                case Blue => "#05274c"
                case White => "#666666"
            }
        else
        if (f == game.current)
            container.attach.parent.style.outline = "2px solid #aaaaaa"
        else
        if (game.highlightFaction.has(f))
            container.attach.parent.style.outline = "2px dashed #aaaaaa"
        else
            container.attach.parent.style.outline = ""
    }

    def onFactionStatus(implicit f : Faction, isMore : Boolean) : Unit = {
        def desc(l : Any*) = game.desc(l : _*).div
        def more(l : Any*) = isMore.?(desc(l : _*))
        def less(l : Any*) = isMore.not.?(desc(l : _*))
        def moreGap = isMore.?(HGap)
        def lessGap = isMore.not.?(HGap)

        def info() =
            less(("More Info".hh).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(f, !isMore)) ~
            more(("Less Info".hh).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(f, !isMore))

        val ww = max(f.resourceSlots, f.resources.num)

        showOverlay(overlayScrollX((
            HGap ~
            HGap ~
            HGap ~
            f.elem.larger.larger.larger.styled(xstyles.bold) ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            desc("Power".hl.larger) ~
            more("(victory points)") ~
            HGap ~
            HGap ~
            desc(f.power.power.larger.larger) ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            desc("Cards".hl.larger) ~
            HGap ~
            desc(f.hand.num.times(Image("card-back", styles.token3x))) ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            (f.outraged.any).?{
                desc("Outrage".hl.larger) ~
                more("(resources of these type can't be used for actions)") ~
                HGap ~
                HGap ~
                desc(f.outraged./(r => Image(r.name + "-outrage", styles.token3x))) ~
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                HGap
            } ~
            desc("Resources".hl.larger) ~
            HGap ~
            HGap ~
            desc(f.keys.take(f.resourceSlots)./(n => Image("keys-" + n, styles.token3x)) ++ (ww - f.resourceSlots).times(Image("discard-resource", styles.token3x))) ~
            HGap ~
            desc(f.resources./(r => Image(r.name, styles.token3x)) ++ (ww - f.resources.num).times(Image("nothingness", styles.token3x))) ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            desc("Cities".hl.larger) ~
            HGap ~
            HGap ~
            desc(systems./~(f.at(_).cities)./(u => Image(u.faction.short + "-city" + f.damaged.has(u).??("-damaged"), styles.token3x)),
                $(
                    Image("building-empty-keys-1", styles.token3x),
                    Image("building-empty-keys-2", styles.token3x),
                    Image("building-empty-keys-1-3", styles.token3x),
                    Image("building-empty-plus-2", styles.token3x),
                    Image("building-empty-plus-3", styles.token3x),
                ).drop(5 - f.pooled(City))
            ) ~
            (f.pooled(City) < 2).?(desc("Total bonus for won ambitions", "+" ~ ((f.pooled(City) < 2).??(2) + (f.pooled(City) < 1).??(3)).power)) ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            desc("Starports".hl.larger) ~
            HGap ~
            HGap ~
            desc(systems./~(f.at(_).starports)./(u => Image(u.faction.short + "-" + starport + f.damaged.has(u).??("-damaged"), styles.token3x)), f.pooled(Starport).times(Image(starport + "-empty", styles.token3x))) ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            desc("Ships".hl.larger) ~
            HGap ~
            HGap ~
            desc(systems./~(f.at(_).ships)./(u => Image(u.faction.short + "-ship" + f.damaged.has(u).??("-damaged"), styles.ship3x)), f.pooled(Ship).times(Image("ship-empty", styles.ship3x))) ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            desc("Agents".hl.larger) ~
            HGap ~
            HGap ~
            desc(game.market./~(Influence(_).$).%(_.faction == f)./(u => Image(u.faction.short + "-agent", styles.ship3x)), f.pooled(Agent).times(Image("agent-empty", styles.ship3x))) ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            (f.trophies.any).? {
                desc("Trophies".hl.larger) ~
                HGap ~
                HGap ~
                desc(f.trophies./(u => Image(u.faction.short + "-" + u.piece.name + "-damaged", (u.piece == Ship).?(styles.ship3x).|(styles.ship3x)))) ~
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                HGap
            } ~
            (f.captives.any).? {
                desc("Captives".hl.larger) ~
                HGap ~
                HGap ~
                desc(f.captives./(u => Image(u.faction.short + "-" + u.piece.name, (u.piece == Ship).?(styles.ship3x).|(styles.ship3x)))) ~
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                HGap
            } ~
            (f.loyal.any).? {
                desc("Loyalists".hl.larger) ~
                HGap ~
                f.loyal./(c => OnClick(c, Div(Image(c.id, styles.courtCard), styles.cardX, xstyles.xx, styles.inline, styles.nomargin, xlo.pointer))).merge.div ~
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                HGap
            } ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            info() ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap ~
            HGap
        ).div(xlo.flexvcenter)(styles.infoStatus)), {
            case (f : Faction, more : Boolean) => onFactionStatus(f, more)
            case (c : CourtCard) => onClick(c)
            case _ =>
                overlayPane.invis()
                overlayPane.clear()
        })
    }

    def updateStatus() {
        0.until(arity).foreach { n =>
            factionStatus(game.seating(n))
        }

        if (overlayPane.visible)
            overlayPane.vis()
        else
            overlayPane.invis()

        drawMap()
        court.draw()
        ambitions.foreach(_.draw())
    }

    val layoutZoom = 0.49 * 0.88

    val kkk = 1.18

    val layouts = $(Layout("base",
        $(
            BasicPane("status", 5*kkk*arity, 22*kkk, Priorities(top = 3, left = 2, maxXscale = 1.8*11111111, maxYscale = 1.8, grow = 1)),
            BasicPane("court", (61.5/4)*kkk*(4 + campaign.??(1)), 22*kkk, Priorities(top = 3, right = 3, maxXscale = 1.0, maxYscale = 1.0, grow = 0)),
            BasicPane("log", 32, 13+3, Priorities(right = 1)),
            BasicPane("map-small", 71, 50, Priorities(top = 2, left = 1, grow = -1)),
            BasicPane("action-a", 64, 36, Priorities(bottom = 1, right = 3, grow = 1)),
            BasicPane("action-b", 55, 47, Priorities(bottom = 1, right = 3, grow = 1, maxXscale = 1.2))
        ).++(
            campaign.?(BasicPane("ambitions", 16*0.8, 44*0.8, Priorities(grow = -4)))
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
            case p : BasicPane if p.name == "action-b" => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-actionB", panes = l.panes./~{
            case p : BasicPane if p.name == "action-a" => None
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

    val layoutKey = "v" + 1 + "." + campaign.?("campaign").|("base") + "." + "arity-" + arity

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

        case card : DeckCard =>
            showOverlay(overlayFitX(Image(card.imgid, styles.artwork)).onClick, onClick)

        case card : CourtCard =>
            showOverlay(overlayFitX(Image(card.id, styles.artwork)).onClick, onClick)

        case fate : Fate =>
            showOverlay(overlayFitX(Image(fate.id, styles.artwork)).onClick, onClick)

        case leader : Leader =>
            showOverlay(overlayFitX(Image(leader.id, styles.artwork)).onClick, onClick)

        case lore : Lore =>
            showOverlay(overlayFitX(Image(lore.id, styles.artwork)).onClick, onClick)

        case $(f : Faction, x) =>
            onClick(x)

        case "discourt" =>
            showOverlay(overlayScrollX(Div("Court Cards Discard Pile") ~
                game.discourt./(c => OnClick(c, Div(Image(c.id, styles.courtCard), styles.cardX, xstyles.xx, styles.inline, styles.nomargin, xlo.pointer))).merge
            ).onClick, onClick)

        case "discard" =>
            showOverlay(overlayScrollX(Div("Action Cards Discard Pile") ~
                game.discard./(c => OnClick(c, Div(Image(c.imgid, styles.card), styles.cardX, xstyles.xx, styles.inline, styles.nomargin, xlo.pointer))).merge
            ).onClick, onClick)

        case "showndeck" =>
            showOverlay(overlayScrollX(Div("Played Cards Pile") ~
                game.seen./(c => OnClick(c, Div(Image(c.imgid, styles.card), styles.cardX, xstyles.xx, styles.inline, styles.nomargin, xlo.pointer))).merge
            ).onClick, onClick)

        case "seen" =>
            showOverlay(overlayScrollX(Div("Played Action Cards".hl) ~
                game.seenX.groupBy(_._1).$.sortBy(_._1)./{ case (n, l) =>
                    Div("Round " ~ n.hh) ~
                    l./{ case (_, f, d) =>
                        OnClick(d, Div(Image(d./(_.imgid).|("card-back"), styles.card), xstyles.choice, xstyles.xx, styles.cardI, elem.borders.get(f), styles.inline, xlo.pointer))
                    }.merge
                }.merge
            ).onClick, onClick)
        case "ll" =>
            showOverlay(overlayScrollX(
                Div("Leaders Draft") ~
                game.leaders./(c => Div(Image(c.id, styles.leaderCard), styles.cardX, xstyles.xx, styles.inline)).merge ~
                Div("Lore Draft") ~
                game.lores./(c => Div(Image(c.id, styles.card), styles.cardX, xstyles.xx, styles.inline)).merge
            ), onClick)
        case "readout" =>
            showOverlay(overlayScrollX((
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                HGap ~
                systems./~(s =>
                    $(
                    game.factions.%(_.rules(s)).single./(f => s.name.styled(f)).|(s.name.txt).larger.styled(xstyles.bold),
                    HGap,
                    game.desc((s.symbol != Gate).?(game.board.resource(s).use(r => ResourceRef(r, None)))).div,
                    HGap
                    ) ++
                    game.factions./(_.at(s)).%(_.any).sortBy(l => l.buildings.num * 20 + l.ships.num).reverse./(_.sortBy(_.piece.is[Building].not)./(u => Image(u.faction.short + "-" + u.piece.name + u.faction.damaged.has(u).??("-damaged"), (u.piece == Ship).?(styles.ship3x).|(styles.token3x))))./(game.desc(_).div(styles.figureLine)) ++
                    $(game.desc(game.freeSlots(s).times(Image(starport + "-empty", styles.token3x))).div(styles.figureLine)) ++
                    $(
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap,
                    HGap
                    )
                )
            ).div(xlo.flexvcenter)(styles.infoStatus)), onClick)

        case "readout" =>
            showOverlay(overlayScrollX((
                systems./(s =>
                    s.elem.div ~
                    HGap ~
                    HGap ~
                    (game.at(s)./(u => Image(u.faction.short + "-" + u.piece.name + "-damaged", (u.piece == Ship).?(styles.ship3x).|(styles.ship3x)))).merge.div ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).merge ~
                "".hl.div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick
            ).div(xlo.flexvcenter)(styles.infoStatus)), onClick)

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

        case Nil =>
            clearOverlay()

        case Left(x) => onClick(x)
        case Right(x) => onClick(x)

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
            (options.has(SplitDiscardPile)).$(ZBasic(Break ~ Break, "Action Cards Discard Pile".hh, () => { onClick("discard") }).copy(clear = false)) ++
            $(ZBasic(Break ~ Break, "Played Action Cards".hh, () => { onClick("seen") }).copy(clear = false)) ++
            $(ZBasic(Break ~ Break, "Court Cards Discard Pile".hh, () => { onClick("discourt") }).copy(clear = false)) ++
            ((game.leaders.num > 1 || game.lores.num > 1).$(ZBasic(Break ~ Break, "Leaders & Lore Draft".hh, () => { onClick("ll") }).copy(clear = false))) ++
            $(ZBasic(Break ~ Break, "Map Readout".hh, () => { onClick("readout") }).copy(clear = false)) ++
            (currentGame.isOver && hrf.HRF.flag("replay").not).$(
                ZBasic(Break ~ Break ~ Break, "Save Replay As File".hh, () => {
                    showOverlay(overlayScrollX("Saving Replay...".hl.div).onClick, null)

                    callbacks.saveReplay {
                        overlayPane.invis()
                        overlayPane.clear()
                    }
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
                }).copy(clear = false)
            ) ++
            $(ZBasic(Break ~ Break, "Notifications".spn, () => { onClick("notifications", self) }).copy(clear = false)).%(_ => self.any || game.isOver) ++
            $(ZBasic(Break ~ Break, "Interface".spn, () => { callbacks.editSettings { updateStatus() } }).copy(clear = false))
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

    override def styleAction(faction : |[F], actions : $[UserAction], a : UserAction, unavailable : Boolean, view : |[Any]) : $[Style] =
        view @@ {
            case _ if unavailable.not => $()
            case Some(_ : Figure) => $(styles.unquasi)
            case Some(_) => $(xstyles.unavailableCard)
            case _ => $(xstyles.unavailableText)
        } ++
        a @@ {
            case _ if view.any && view.get.is[Resource] => $(styles.card0, styles.circle)
            case _ : Info => $(xstyles.info)
            case _ if unavailable => $(xstyles.info)
            case _ => $(xstyles.choice)
        } ++
        a @@ {
            case _ if view.any && view.get.is[Resource] => $()
            case _ => $(xstyles.xx, xstyles.chp, xstyles.chm)
        } ++
        faction @@ {
            case Some(f : Faction) => $(elem.borders.get(f))
            case _ => $()
        } ++
        a @@ {
            case a : Selectable if a.selected => $(styles.selected)
            case _ => $()
        } ++
        view @@ {
            case Some(_ : Figure)                    => $(styles.inline, styles.quasi) ++
                a @@ {
                    case a : XXSelectObjectAction[_] => $($(), $(styles.selfigure1), $(styles.selfigure2))(a.selecting.count(a.n))
                    case a : XXDeselectObjectAction[_] => $($(), $(styles.selfigure1), $(styles.selfigure2))(a.selecting.count(a.n))
                    case _ => $()
                }
            case Some(_) => $(styles.inline)
            case _ => $(xstyles.thu, xstyles.thumargin, xlo.fullwidth)
        } ++
        a @@ {
            case _ if unavailable => $()
            case _ : Extra[_] => $()
            case _ : Choice | _ : Cancel | _ : Back | _ : OnClickInfo => $(xlo.pointer)
            case _ => $()
        }

}
