package vast
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

import vast.elem._

import hrf.ui.again._
import hrf.ui.sprites._

import scalajs.js.timers.setTimeout


object UI extends BaseUI {
    val mmeta = Meta

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, resources, callbacks)
}

class UI(val uir : ElementAttachmentPoint, arity : Int, val resources : Resources, callbacks : hrf.Callbacks) extends MapGUI {
    def factionElem(f : Faction) = f.name.styled(f)

    val statuses = 1.to(arity)./(i => newPane("status-" + i, Content, styles.status, styles.fstatus))

    mapSmall.replace((Div("North", styles.title) ~ (Div("West", styles.title) ~ Div("East", styles.title)).div(styles.dirsub) ~ Div("South", styles.title)).div(styles.directions)(xstyles.overlay), resources)

    def processRightClick(target : $[Any], xy : XY) {
    }

    def processTargetClick(target : $[Any], xy : XY) {
        target.of[Relative].single.foreach { p =>
            keys.of[TileKey].%(_.position == p).single.foreach { k =>
                return onClick(k)
            }
        }
    }

    def processHighlight(target : $[Any], xy : XY) {
        highlightCoordinates = |(xy)

        highlightPeekTile = target match {
            case (p : Relative) :: _ if lastFaction.has(Cave) || game.isOver => $(p).%(game.board.valid).%(game.board.get(_).is[HiddenTile])
            case _ => $
        }

        highlightSelectTile = target match {
            case (p : Relative) :: _ if keys.of[TileKey].exists(_.position == p) => $(p)
            case _ => $
        }

        if (highlightSelectTile.any)
            mapSmall.attach.parent.style.cursor = "pointer"
        else
        if (highlightPeekTile.any)
            mapSmall.attach.parent.style.cursor = "help"
    }

    var highlightCoordinates : |[XY] = None
    var highlightPeekTile : $[Relative] = $
    var highlightSelectTile : $[Relative] = $

    def makeScene() : |[Scene] = {
        if (game.board.width * game.board.height <= 1)
            return None

        val background = new OrderedLayer

        0.until(game.board.width).foreach { i =>
            0.until(game.board.height).foreach { j =>
                val tile = game.board.cells(i)(j)

                val (image, dir) = tile @@ {
                    case Emptiness => ("", North)
                    case HiddenTile(_, tribe, _) => ("hidden-" + tribe, North)
                    case Pending => ("pending", North)
                    case Revealing => ("revealing", North)
                    case Explored(name, _, dir, _) => (name, dir)
                    case Magma(name, dir) => (name, dir)
                    case River(name, dir) => (name, dir)
                }

                if (image != "") {
                    val img = dir match {
                        case North => new RawImage(resources.images.get(image))
                        case East => new RawImageRotated90(resources.images.get(image))
                        case South => new RawImageRotated180(resources.images.get(image))
                        case West => new RawImageRotated270(resources.images.get(image))
                    }

                    if (image == "magma")
                        background.add(Sprite($(ImageRect(img, Rectangle(i*308, j*308, 308*2, 308*2), 1.0)), $))(0, 0)
                    else
                    if (image == "river")
                        background.add(Sprite($(ImageRect(img, Rectangle(i*308, j*308-308, 308, 308*4), 1.0)), $))(0, 0)
                    else
                        background.add(Sprite($(ImageRect(img, Rectangle(i*308, j*308, 308, 308), 1.0)), $(Rectangle(i*308, j*308, 308, 308)), $(Relative(i - game.board.center.x, j - game.board.center.y))))(0, 0)
                }
                else
                    background.add(Sprite($, $(Rectangle(i*308, j*308, 308, 308)), $(Relative(i - game.board.center.x, j - game.board.center.y))))(0, 0)

                if (highlightPeekTile.has(Relative(i - game.board.center.x, j - game.board.center.y)))
                    tile.as[HiddenTile].foreach { tile =>
                        val img = new RawImage(resources.images.get(tile.tile.name))
                        background.add(Sprite($(ImageRect(img, Rectangle(i*308, j*308, 308, 308), 0.3)), $))(0, 0)
                    }
            }
        }

        game.board.bombed.foreach { case (p, dir) =>
            val i = p.x + game.board.center.x
            val j = p.y + game.board.center.y

            val img = dir match {
                case North => new RawImage(resources.images.get("bombed"))
                case East => new RawImageRotated90(resources.images.get("bombed"))
                case South => new RawImageRotated180(resources.images.get("bombed"))
                case West => new RawImageRotated270(resources.images.get("bombed"))
            }

            background.add(Sprite($(ImageRect(img, Rectangle(i*308 + dir.dx * 308/2, j*308 + dir.dy * 308/2, 308, 308), 1.0)), $))(0, 0)
        }

        game.board.rockslides.foreach { case (p, dir) =>
            val i = p.x + game.board.center.x
            val j = p.y + game.board.center.y

            val img = dir match {
                case North => new RawImageRotated90(resources.images.get("rockslide"))
                case East => new RawImageRotated180(resources.images.get("rockslide"))
                case South => new RawImageRotated270(resources.images.get("rockslide"))
                case West => new RawImage(resources.images.get("rockslide"))
            }

            background.add(Sprite($(ImageRect(img, Rectangle(i*308 + dir.dx * 308/2, j*308 + dir.dy * 308/2, 308, 308), 1.0)), $))(0, 0)
        }

        0.until(game.board.width).foreach { i =>
            0.until(game.board.height).foreach { j =>
                val p = Relative(i - game.board.center.x, j - game.board.center.y)

                val peek = highlightPeekTile.has(p)

                val tokens : $[Token] = game.board.tokens(i)(j) ++ peek.??(game.board.cells(i)(j).as[HiddenTile]./~(_.tokens))

                val pk = peek.?(0.6+0.4).|(1.0)

                tokens.foreach {
                    case FlameWall(_) =>
                        background.add(Sprite($(ImageRect(new RawImage(resources.images.get("flame-wall")), Rectangle(i*308, j*308, 308, 308), 1.0 * pk)), $))(0, 0)
                    case _ =>
                }

                tokens.foreach {
                    case Crystal =>
                        background.add(Sprite($(ImageRect(new RawImage(resources.images.get("crystal")), Rectangle(i*308, j*308, 308, 308), 1.0 * pk)), $))(0, 0)
                    case _ =>
                }

                val highlightChest = lastActions.has(HighlightChestAt(p))

                val l = (tokens.of[Chest.type].drop(highlightChest.??(1)) ++ tokens.of[DragonGem])
                val h = game.factions.exists(_.positions.has(p)).??(60)

                l.indexed.foreach { (t, k) =>
                    val s = t @@ {
                        case Chest => "treasure"
                        case DragonGem(_, power) => "gem-" + power.id
                    }

                    background.add(Sprite($(ImageRect(new RawImage(resources.images.get(s)), Rectangle(i*308 + 154 - 160/2 + (l.num - 1 - 2*k) * 40, j*308 + 154 + h - 160/2, 160, 160), 1.0 * pk)), $))(0, 0)
                }

                tokens.foreach {
                    case Ambush =>
                        background.add(Sprite($(ImageRect(new RawImage(resources.images.get("ambush")), Rectangle(i*308, j*308, 308, 308), 0.7)), $))(0, 0)
                    case Event =>
                        background.add(Sprite($(ImageRect(new RawImage(resources.images.get("event")), Rectangle(i*308, j*308, 308, 308), 0.7)), $))(0, 0)
                    case _ =>
                }
            }
        }

        0.to(10).foreach { w =>
            (game.factions.drop(1) ++ game.factions.take(1)).foreach {
                case f : Knight.type if f.strength == w =>
                    val p = f.position
                    val pk = highlightPeekTile.has(p).?(0.5).|(1.0)
                    background.add(Sprite($(ImageRect(new RawImage(resources.images.get("knight")), Rectangle((p.x + game.board.center.x) * 308, (p.y + game.board.center.y) * 308, 308, 308), 1.0 * pk)), $))(0, 0)
                case f : Dragon.type if f.underground && f.position.any && f.armor == w =>
                    val p = f.position.get
                    val pk = highlightPeekTile.has(p).?(0.5).|(1.0)
                    background.add(Sprite($(ImageRect(new RawImage(resources.images.get("dragon-sleeping")), Rectangle((p.x + game.board.center.x) * 308, (p.y + game.board.center.y) * 308, 308, 308), 1.0 * pk)), $))(0, 0)
                case f : Dragon.type if f.underground.not && f.position.any && f.armor == w =>
                    val p = f.position.get
                    val pk = highlightPeekTile.has(p).?(0.5).|(1.0)
                    background.add(Sprite($(ImageRect(new RawImage(resources.images.get("dragon-awake")), Rectangle((p.x + game.board.center.x) * 308, (p.y + game.board.center.y) * 308, 308, 308), 1.0 * pk)), $))(0, 0)
                case f : Goblins.type =>
                    f.tribes.foreach { t =>
                        t.position.foreach { p =>
                            if (t.strength == w) {
                                val h = game.factions.of[Dragon.type].exists(_.position.has(p)).??(-60)

                                val l = f.tribes.%(_.position.has(p))
                                val n = l.indexOf(t)
                                val pk = highlightPeekTile.has(p).?(0.5).|(1.0)
                                background.add(Sprite($(ImageRect(new RawImage(resources.images.get("tribe-" + t.tribe)), Rectangle((p.x + game.board.center.x) * 308 + (l.num - 1) * 30 - n * 60, (p.y + game.board.center.y) * 308 + h, 308, 308), 1.0 * pk)), $))(0, 0)
                            }
                        }
                    }
                case _ =>
            }
        }

        lastActions.of[HighlightChestAt]./(_.position).foreach { p =>
            background.add(Sprite($(ImageRect(new RawImage(resources.images.get("chest-highlight")), Rectangle((p.x + game.board.center.x) * 308, (p.y + game.board.center.y) * 308, 308, 308), 1.0)), $))(0, 0)
        }

        lastFaction.%(game.board.letters.contains).foreach { f =>
            0.until(game.board.width).foreach { i =>
                0.until(game.board.height).foreach { j =>
                    game.board.letters(f)(i)(j).foreach { s =>
                        val img = new RawImage(resources.images.get("letter-" + s))
                        background.add(Sprite($(ImageRect(img, Rectangle(i*308, j*308, 308, 308), 1.0)), $))(0, 0)
                    }
                }
            }
        }

        highlightSelectTile.foreach { p =>
            val img = new RawImage(resources.images.get("selecting"))
            background.add(Sprite($(ImageRect(img, Rectangle((p.x + game.board.center.x)*308, (p.y + game.board.center.y)*308, 308, 308), 1.0)), $))(0, 0)
        }

        |(new Scene($(background), game.board.width*308, game.board.height*308, Margins(-200, -200, -200, -200)))
    }

    override def adjustCenterZoomX() {
        zoomBase = zoomBase.clamp(-1200, 1200)
        dX = dX.clamp(-game.board.width*308 / 2 + 308 * 3 / 2, game.board.width*308 / 2 - 308 * 3 / 2)
        dY = dY.clamp(-game.board.height*308 / 2 + 308 * 3 / 2, game.board.height*308 / 2 - 308 * 3 / 2)
    }

    def factionStatus(f : Faction) {
        val container = statuses(game.setup.indexOf(f))

        val title = Div(Div(f.name.styled(f)(styles.smallname) ~ resources.getName(f)./(" " ~ "(" ~ _.hl ~ ")")), xlo.pointer)

        val stats = f @@ {
            case _ if game.states.contains(f).not => Empty
            case f : Knight.type =>
                ("Health " ~ f.health.hl ~ "/" ~ 7.hh).div ~
                ("Grit " ~ f.grit.hl ~ "/" ~ $(5, 11, 18, 26, 35, 45).%(_ > f.grit).starting./(_.hh).|("max".hh)).div ~
                ("Stamina " ~ (f.free.times(dt.Stamina) ~ min(f.stamina - f.lost, f.assigned.num).times(dt.Used) ~ f.lost.times(dt.Lost) ~ f.poison.times(dt.Poison)).&).div ~
                ((f.movement - f.moves).times(dt.MovementK) ~ (f.perception - f.encounters).times(dt.PerceptionK) ~ (f.strength - f.strbonus).times(dt.StrengthK) ~ (f.strbonus).times(dt.Strength)).div ~
                f.basic.%(f.assigned.has)./(e => (e.elem ~ f.assigned.count(e).times(dt.Stamina).some./(" " ~ _)).div.onClick.param(e)) ~
                f.arsenal./(e => (e.elem ~ f.assigned.count(e).times(dt.Stamina).some./(" " ~ _)).div.onClick.param(e)) ~
                f.stash.num.times("Treasure".hh.div)
            case f : Goblins.type =>
                ("Rage".styled(styles.hit) ~ " " ~ f.rage.hl ~ " | ".hh ~ "Secrets" ~ " " ~ f.hand.num.hl).div ~
                f.revealed.some./(_./(s => s.elem.onClick.param(s)).join(" | ".hh).div) ~
                f.tribes./ { t =>
                    t.hidden.?(t.tribe.name.hh).|(t.tribe.name.styled(f)).div ~
                    (min(t.population, t.strength).times(dt.Population(t.tribe)) ~ (t.population - t.strength).times(dt.Malaise(t.tribe)) ~ (t.strength - t.population).times(dt.Strength) ~ (t.bonus > 0 && t.malaise).?(dt.StrengthMalaise)).div ~
                    t.monsters./(m => m.elem.div.onClick.param(m)).merge ~
                    t.effects.notOf[Monster]./(e => e.elem.div.onClick.param(e)).merge
                }
            case f : Dragon.type =>
                ("Health " ~ f.health.hl ~ "/" ~ 5.hh).div ~
                ("Wakefulness " ~ f.wakefulness.hl ~ f.awake.not.?("/" ~ 11.hh)).div ~
                ("Armor " ~ f.armor.hl ~ " | ".hh ~ "Spirit " ~ f.spirit.hl).div ~
                ("Powers " ~ f.powers.of[PowerCard].num.hl ~ " | ".hh ~ "Gems " ~ f.gems.num.hl).div ~
                f.shriek.?("Shriek".hl.div) ~
                ("Greed " ~ min(f.greed.value, f.treasures).times(dt.SlothTreasure) ~ (f.greed.value - f.treasures).times(dt.Sloth) ~ (4 - f.greed.value).times(dt.SlothFree)).div ~
                ("Hunger " ~ min(f.hunger.value, f.eaten / 2).times(dt.SlothEatenTwo) ~ min(f.hunger.value - f.eaten / 2, f.eaten % 2).times(dt.SlothEatenOne) ~ (f.hunger.value - f.eaten / 2 - f.eaten % 2).times(dt.Sloth) ~ (4 - f.hunger.value).times(dt.SlothFree)).div ~
                ("Pride " ~ (f.prideE.value.times(dt.Sloth) ~ (4 - f.prideE.value).times(dt.SlothFree) ~ "'".hh ~ f.prideG.value.times(dt.Sloth) ~ (1 - f.prideG.value).times(dt.SlothFreeGems) ~ "'".hh ~ f.prideS.value.times(dt.Sloth) ~ (1 - f.prideS.value).times(dt.SlothFreeStay)).&).div

            case f : Cave.type =>
                ("Omens " ~ f.omens.num.hl).div ~
                ("Tiles " ~ game.tiles.num.hl).div ~
                ("Crystalization " ~ game.shrunk.hl ~ "/" ~ "5".hh).div ~
                ("Hatred " ~ game.collapse.?("Infinite").|("Boundless").hl).div

            case _ => Empty
        }

        container.replace((title ~ stats).div(xstyles.fillHeight).pointer.onClick.param(f), resources, {
            case f : Faction => onFactionStatus(f, false)
            case x => onClick(x)
        })

        if (f == game.current)
            container.attach.parent.style.outline = "2px solid #aaaaaa"
        else
        if (game.highlightFaction.has(f))
            container.attach.parent.style.outline = "2px dashed #aaaaaa"
        else
            container.attach.parent.style.outline = ""
    }

    def onFactionStatus(implicit faction : Faction, isMore : Boolean, chapter : |[String] = None) : Unit = {
        def desc(l : Any*) = game.desc(l : _*).div
        def more(l : Any*) = isMore.?(desc(l : _*))
        def less(l : Any*) = isMore.not.?(desc(l : _*))
        def moreGap = isMore.?(HGap)
        def lessGap = isMore.not.?(HGap)

        def info() =
            less(("More Info".hh).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(faction, !isMore)) ~
            more(("Less Info".hh).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(faction, !isMore))

        faction @@ {
            case f : Knight.type =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.styled(styles.get(faction)).larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("knight-board")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : Goblins.type =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.styled(styles.get(faction)).larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("goblins-board")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : Dragon.type =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.styled(styles.get(faction)).larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("dragon-board")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : Cave.type =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.styled(styles.get(faction)).larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("cave-board")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f =>
                println("no faction status overlay for " + f + " " + isMore + " " + chapter)
        }
    }

    def updateStatus() {
        0.until(arity).foreach { n =>
            factionStatus(game.setup(n))
        }

        if (overlayPane.visible)
            overlayPane.vis()
        else
            overlayPane.invis()

        drawMap()
    }

    val layoutZoom = 0.49*1.12

    val layouts = $(Layout("base",
        $(
            BasicPane("status", 17, 25, Priorities(top = 3, right = 2, maxXscale = 1.8, maxYscale = 1.8)),
            BasicPane("log", 30+2, 13, Priorities(right = 1)),
            BasicPane("map-small", 42+21+6, 42+21+6, Priorities(top = 2, left = 1, grow = -1)),
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
        overlayPane.replace(e, resources, onClick)
    }

    override def onClick(a : Any) = a @@ {
        case ("notifications", Some(f : Faction)) =>
            shown = $
            showNotifications($(f))

        case ("notifications", None) =>
            shown = $
            showNotifications(game.factions)

        case q : Equipment =>
            showOverlay(overlayFitX(Image(q.id, styles.artwork)).onClick, onClick)

        case e : Event =>
            showOverlay(overlayFitX(Image("event-" + e.id, styles.artwork)).onClick, onClick)

        case s : SideQuest =>
            showOverlay(overlayFitX(Image("sidequest-" + s.id, styles.artwork)).onClick, onClick)

        case c : WarCard =>
            showOverlay(overlayFitX(Image("war-card-" + c.name, styles.artwork)).onClick, onClick)

        case m : Monster =>
            showOverlay(overlayFitX(Image("monster-" + m.id, styles.artwork)).onClick, onClick)

        case s : Secret =>
            showOverlay(overlayFitX(Image("secret-" + s.id, styles.artwork)).onClick, onClick)

        case action : Action if lastThen != null =>
            clearOverlay()

            val then = lastThen
            lastThen = null
            lastActions = $
            keys = $

            asker.clear()

            then(action.as[UserAction].||(action.as[ForcedAction]./(_.as("Do Action On Click"))).|(throw new Error("non-user non-forced action in on click handler")))

        case Nil =>
            clearOverlay()

        case $(f : Faction, x : Equipment) =>
            onClick(x)

        case $(f : Faction, x : Secret) =>
            onClick(x)

        case $(f : Faction, x : Monster) =>
            onClick(x)

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

        val asker = new NewAsker(ol, resources.images.get)

        asker.zask(display)(resources)
    }

    override def wait(self : $[F], factions : $[F]) {
        lastActions = $
        lastThen = null
        lastFaction = lastFaction || self.single

        showNotifications(self)

        super.wait(self, factions)
    }

    var lastActions : $[UserAction] = $
    var lastThen : UserAction => Unit = null
    var lastFaction : |[Faction] = None

    var keys = $[Key]()

    override def ask(faction : |[F], actions : $[UserAction], then : UserAction => Unit) {
        lastActions = actions
        lastThen = then
        lastFaction = faction

        drawMap()

        showNotifications(faction.$)

        keys = actions./~(a => a.as[Key] || a.unwrap.as[Key])

        keys ++= actions.of[SoftKeys]./~(game.performContinue(None, _, false).continue @@ {
            case Ask(f, l) if faction.has(f) => l.of[Key]
            case _ => $()
        })

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

            val source = a match {
                case UnavailableReasonAction(a, _) => a
                case a => a
            }

            val card = source match {
                case _ : ViewObject[_] => true
                case _ => false
            }

            val power = source match {
                case a : ViewObject[_] if a.obj.is[DragonCard] => true
                case _ => false
            }

            val omen = source match {
                case a : ViewObject[_] if a.obj.is[OmenCard] => true
                case _ => false
            }

            val tile = source match {
                case a : ViewObject[_] if a.obj.is[HiddenTile] => true
                case _ => false
            }

            def ss =
                (a match {
                    case _ if unavailable && card => $(xstyles.unavailableCard)
                    case _ if unavailable => $(xstyles.unavailableText)
                    case _ => Nil
                }) ++
                (a match {
                    case _ if tile => $(styles.cardX, styles.cardT)
                    case _ if power => $(styles.cardX, styles.cardP)
                    case _ if omen => $(styles.cardX, styles.cardT)
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
                    case _ : Extra[_] => Nil
                    case _ : Choice => $(xlo.pointer)
                    case _ : OnClickInfo => $(xlo.pointer)
                    case _ => Nil
                }) ++
                (source match {
                    case _ if card => $(styles.inline)
                    case a : MoveAction => $(styles.short) ++ (a.direction.dy == 0).$(styles.yyy)
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

                case a : Extra[_]                 => Some(ZOption(q, wrap(        Div(o, ss)), s => {
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
