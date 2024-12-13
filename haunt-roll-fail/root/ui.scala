package root
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

import hrf.canvas._
import hrf.voice._

import hrf.ui._
import hrf.ui.again._
import hrf.ui.sprites._

import hrf.elem._
import hrf.html._
import hrf.web._
import hrf.meta._
import hrf.options._

import root.elem._

import scala.collection.mutable

import org.scalajs.dom

import scalajs.js.timers.setTimeout


object UI extends BaseUI {
    val mmeta = Meta

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, options, resources, title, callbacks)
}

object UIAdset extends BaseUI {
    val mmeta = MetaAdset

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, options, resources, title, callbacks)
}


class UI(val uir : ElementAttachmentPoint, arity : Int, options : $[Meta.O], val resources : Resources, title : String, callbacks : hrf.Callbacks) extends GUI {
    def factionElem(f : Player) = (currentGame != null).?(currentGame.ptf.get(f).|(f)).|(f) @@ {
        case f : Faction => f.name.styled(f).styled(styles.condensed, styles.italic)
        case PlayerN(n) => ("Player " ~ ("#" + n).hl).spn(styles.condensed)(styles.italic)
    }

    val statuses = 1.to(arity)./(i => newPane("status-" + i, Content, styles.status, styles.fstatus, ExternalStyle("hide-scrollbar")))
    val statusGame = newPane("status-game", Content, ExternalStyle("hide-scrollbar"))
    val mapSmall = newPane("map-small", Content)

    val overlayPane = newOuterPane("overlay", Content)
    overlayPane.invis()

    val replayPane = newPane("replay", Content.div(xlo.column)(xlo.fullheight), xstyles.pane.action)
    replayPane.invis()

    val replayAsker = new NewAsker(replayPane.attach.appendContainer(Content.div(xlo.flexhcenter)(xlo.fullwidth), resources), resources.images.get)

    lazy val mapid = game.board.id + ":"

    var map = new CachedBitmap(mapSmall.attach.parent)

    mapSmall.replace(Div(Div(title, styles.title), xstyles.overlay), resources)

    lazy val regions = new IndexedImageRegions[Region](new RawImage(resources.images.get(mapid + "map-regions")), 0, 0, game.board.clearings./(c => c -> XY(game.board.center(c)._1, game.board.center(c)._2)).toMap)

    lazy val pieces = new FitLayer[Region, Figure](regions, FitOptions(kX = 2))

    object layers {
        val background = new OrderedLayer
        background.add(sprite(mapid + "map"))(0, 0)

        val clearings = new OrderedLayer
        val rule = new OrderedLayer
    }


    lazy val scene = {
        val mp = resources.images.get(mapid + "map")

        val d = options.has(AutumnMap).?(0).|(12)

        new Scene($(layers.background, layers.clearings, layers.rule, pieces), mp.width, mp.height, Margins(d, d, d, d))
    }

    val sprites = mutable.Map[String, Sprite]()

    def sprite(key : String) = sprites.getOrElseUpdate(key, {
        val image = resources.images.get(key)
        Sprite($(ImageRect(new RawImage(image), Rectangle(0, 0, image.width, image.height), 1.0)), $)
    })


    lazy val findAnother = {

        val mplace = resources.images.get(mapid + "map-regions")
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

    case object FreeBuildingSlot extends Building
    case object Battle extends Token
    case object Placement extends Token
    case object Nuke extends Token

    case class DrawRect(key : String, x : Double, y : Double, width : Double, height : Double)

    case class DrawItem(region : Region, faction : Faction, piece : Piece, x : Int, y : Int, scale : Double = 1.0) {
        val pr = faction.but(Neutral).?(_.style + "-")

        val forest = region.is[Forest]

        val icon : DrawRect = piece match {
            case FreeBuildingSlot => { DrawRect(mapid + "building-slot", -50, -50, 100, 100) }
            case Battle => { DrawRect("clearing-battle", -50, -50, 100, 100) }
            case Placement => { DrawRect("clearing-placement", -50, -50, 100, 100) }
            case Nuke => { DrawRect("clearing-nuke", -50, -50, 100, 100) }

            case Sawmill => { DrawRect(pr + "sawmill", -50, -50, 100, 100) }
            case Workshop => { DrawRect(pr + "workshop", -50, -50, 100, 100) }
            case Recruiter => { DrawRect(pr + "recruiter", -50, -50, 100, 100) }
            case Roost => { DrawRect(pr + "roost", -50, -50, 100, 100) }
            case Base(Fox) => { DrawRect(pr + "base-fox", -50, -50, 100, 100) }
            case Base(Rabbit) => { DrawRect(pr + "base-rabbit", -50, -50, 100, 100) }
            case Base(Mouse) => { DrawRect(pr + "base-mouse", -50, -50, 100, 100) }
            case Garden(Fox) => { DrawRect(pr + "garden-fox", -50, -50, 100, 100) }
            case Garden(Rabbit) => { DrawRect(pr + "garden-rabbit", -50, -50, 100, 100) }
            case Garden(Mouse) => { DrawRect(pr + "garden-mouse", -50, -50, 100, 100) }
            case Ruins(_) => { DrawRect("ruins", -50, -50, 100, 100) }
            case Citadel => { DrawRect(pr + "citadel", -50, -50, 100, 100) }
            case Market => { DrawRect(pr + "market", -50, -50, 100, 100) }

            case Tower => { DrawRect("tower", -49, -145, 97, 153) }
            case Ferry => { DrawRect("ferry", -74, -53, 145, 109) }
            case LostCity => { DrawRect("lost-city", -80, -101, 160, 106) }

            case Wood => { DrawRect(pr + "wood", -45, -45, 90, 90) }
            case Sympathy => { DrawRect(pr + "sympathy", -45, -45, 90, 90) }
            case TradePost(Fox) => { DrawRect(pr + "trade-post-fox", -45, -45, 90, 90) }
            case TradePost(Rabbit) => { DrawRect(pr + "trade-post-rabbit", -45, -45, 90, 90) }
            case TradePost(Mouse) => { DrawRect(pr + "trade-post-mouse", -45, -45, 90, 90) }
            case Tunnel => { DrawRect(pr + "tunnel", -45, -45, 90, 90) }
            case Catapult => { DrawRect(pr + "catapult", -45, -45, 90, 90) }



            case HiddenPlot => { DrawRect(pr + "plot", -45, -45, 90, 90) }
            case Bomb => { DrawRect(pr + "bomb", -45, -45, 90, 90) }
            case Snare => { DrawRect(pr + "snare", -45, -45, 90, 90) }
            case Extortion => { DrawRect(pr + "extortion", -45, -45, 90, 90) }
            case Raid => { DrawRect(pr + "raid", -45, -45, 90, 90) }
            case Diversion => { DrawRect(pr + "diversion", -45, -45, 90, 90) }

            case Mob => { DrawRect(pr + "mob", -45, -45, 90, 90) }
            case Stronghold => { DrawRect(pr + "stronghold", -50, -50, 100, 100) }

            case Caravan => { DrawRect(pr + "caravan", -45, -45, 90, 90) }
            case Station => { DrawRect(pr + "station", -50, -50, 100, 100) }
            case OldTablet => { DrawRect(pr + "tablet" + forest.??("-hidden"), -45, -45, 90, 90) }
            case OldJewel => { DrawRect(pr + "jewel" + forest.??("-hidden"), -45, -45, 90, 90) }
            case OldIdol => { DrawRect(pr + "idol" + forest.??("-hidden"), -45, -45, 90, 90) }

            case Relic(r, value) => { DrawRect(pr + r + "-" + value.toString, -45, -45, 90, 90) }
            case HiddenRelic(r) => { DrawRect(pr + r + "-" + "hidden", -45, -45, 90, 90) }

            case WayStation(a, p) => { DrawRect(pr + "waystation-" + a + "-" + p, -50, -50, 100, 100) }

            case Keep if x > 1000 => { DrawRect("castle-m", -250, -320, 1, 1) }
            case Keep => { DrawRect("castle", -250, -320, 1, 1) }

            case ScorchedEarthMarker(n) => { DrawRect("scorched-earth-" + n, -45, -45, 90, 90) }

            case Cat => { DrawRect(pr + "cat", -35, -82, 70, 92) }
            case Hawk => { DrawRect(pr + "hawk", -43, -80, 76, 90) }
            case Critter => { DrawRect(pr + "critter", -42, -72, 83, 83) }
            case Otter => { DrawRect(pr + "otter", -37, -77, 74, 87) }
            case Lizard => { DrawRect(pr + "lizard", -41, -81, 80, 88) }
            case Mole => { DrawRect(pr + "mole", -54, -82, 91, 87) }
            case Raven => { DrawRect(pr + "raven", -43, -76, 74, 83) }
            case Rat => { DrawRect(pr + "rat", -45, -85, 91, 89) }
            case Warlord => { DrawRect(pr + "warlord", -44, -119, 108, 122) }
            case Badger => { DrawRect(pr + "badger", -52, -97, 91, 103) }
            case OldBadger => { DrawRect(pr + "badger", -37, -92, 86, 97) }
            case Vagabond => { DrawRect(pr + "vagabond", -46, -81, 91, 92) }
            case Squirrel => { DrawRect(pr + "squirrel", -54, -91, 92, 97) }

            case Porcupine => { DrawRect(pr + "porcupine", -51, -100, 102, 105) }
            case Bandit => { DrawRect(pr + "bandit", -51, -100, 102, 105) }
            case Weasel => { DrawRect(pr + "weasel" + StreetBand.enchanted.contains(region).??("-playing"), -70, -91, 113, 97) }
            case _ : RockStar => { DrawRect(pr + "rock-star" + PopularBand.enchanted.contains(region).??("-playing"), -70, -91, 113, 97) }
            case Deer => { DrawRect(pr + "deer", -65, -135, 130, 142) }
            case Elk => { DrawRect(pr + "elk", -65, -135, 130, 142) }

            case _ : StrayCat => { DrawRect(pr + "stray-cat", -40, -84, 80, 92) }
            case Vulture => { DrawRect(pr + "vulture", -50, -85, 82, 92) }
            case Animal => { DrawRect(pr + "animal", -34, -83, 68, 88) }
            case Bear => { DrawRect(pr + "bear", -42, -98, 82, 102) }
            case Flotilla => { DrawRect(pr + "flotilla", -92, -128, 172, 142) }
            case Chameleon => { DrawRect(pr + "chameleon", -36, -82, 79, 88) }
            case Magpie => { DrawRect(pr + "magpie", -34, -78, 88, 84) }
            case Mouldwarp => { DrawRect(pr + "mouldwarp", -46, -71, 79, 77) }
            case Foothold => { DrawRect(pr + "foothold", -45, -45, 90, 90) }
            case Varmint => { DrawRect(pr + "varmint", -46, -86, 94, 92) }
            case Vole => { DrawRect(pr + "vole", -53, -79, 87, 85) }
            case Vault => { DrawRect(pr + "vault", -50, -50, 100, 100) }

            case Foxy => { DrawRect(pr + "foxy", -42, -84, 81, 90) }
            case Palace => { DrawRect(pr + "palace", -50, -50, 100, 100) }
            case LivingShield => { DrawRect(pr + "living-shield", -50, -50, 100, 100) }

            case Wizard => { DrawRect(pr + "frog", -42, -105, 84, 110) }
            case School => { DrawRect(pr + "school", -50, -50, 100, 100) }
            case Statue => { DrawRect(pr + "statue", -45, -45, 90, 90) }
            case Growth => { DrawRect(pr + "growth", -45, -45, 90, 90) }

            case Mudman => { DrawRect(pr + "mudman", -73, -149, 146, 153) }

            case Hamster => { DrawRect(pr + "hamster", -41, -80, 80, 86) }
            case Sheriff => { DrawRect(pr + "sheriff", -41, -119, 99, 125) }
            case Farm => { DrawRect(pr + "farm", -50, -50, 100, 100) }
            case Windmill => { DrawRect(pr + "windmill", -50, -50, 100, 100) }
            case Grain => { DrawRect(pr + "Grain", -45, -45, 90, 90) }

            case _ : CommonFrogWarrior => { DrawRect(pr + "frog", -38, -83, 76, 90) }
            case _ : CommonPeaceful => { DrawRect(pr + "peaceful", -45, -45, 90, 90) }
            case _ : CommonMilitant => { DrawRect(pr + "militant", -45, -45, 90, 90) }

            case _ : CommonBatWarrior => { DrawRect(pr + "bat", -37, -99, 74, 106) }
            case AssemblyAAA => { DrawRect(pr + "assembly", -45, -45, 90, 90) }
            case ConvenedAAA => { DrawRect(pr + "convened", -45, -45, 90, 90) }
            case CommuneAAA => { DrawRect(pr + "commune", -50, -50, 100, 100) }

            case SkunkAAACaptain(Birdsong) => { DrawRect(pr + "captain-birdsong", -42, -81, 85, 86) }
            case SkunkAAACaptain(Daylight) => { DrawRect(pr + "captain-daylight", -42, -81, 85, 86) }
            case SkunkAAACaptain(Evening)  => { DrawRect(pr + "captain-evening" , -42, -81, 85, 86) }
            case _ : CommonSkunkWarrior => { DrawRect(pr + "skunk", -36, -81, 70, 86) }

            case _ =>
                println("" + region + " | " + faction + " | " + piece + " | " + x + " | " + y)
                null
        }

        val rect = {
            val i = icon
            if (i != null)
                icon.copy(x = x + (i.x * scale), y = y + (i.y * scale), width = i.width * scale, height = i.height * scale)
            else
                null
        }
    }

    var oldPositions : $[DrawItem] = $
    var oldGates : $[Region] = $

    lazy val mp = resources.images.get(mapid + "map")
    var deadmp : Bitmap = null

    def drawMap() {
        if (resources.images.get(mapid + "map").complete.not)
            return

        if (resources.images.get(mapid + "map-regions").complete.not)
            return

        val bitmap = {
            val width = map.node.clientWidth * dom.window.devicePixelRatio
            val height = map.node.clientHeight * dom.window.devicePixelRatio

            val upscale = 2

            val b = map.get(width.round.toInt * upscale, height.round.toInt * upscale)

            b.canvas.style.width = "100%"
            b.canvas.style.height = "100%"

            b
        }

        pieces.flush()

        if (false)
            if (layers.clearings.empty) {
                if (game.clearings.forall(game.mapping.contains)) {
                    game.clearings.foreach { c =>
                        val p = regions.center(c)
                        val dx = c.name.length * 12 + 38

                        val nm = c.name.replace(' ', '-') + "-" + game.mapping(c).single.|(game.mapping(c).head)

                        if (callbacks.settings.has(hrf.GameFontFace) && resources.images.has("clearing-name-luminari-" + nm))
                            layers.clearings.add(sprite("clearing-name-luminari-" + nm))(p.x - 240, p.y + 160)
                        else
                            layers.clearings.add(sprite("clearing-" + nm))(p.x - 240, p.y + 165)

                        layers.clearings.add(sprite("clearing-suit-" + game.mapping(c).head))(p.x + dx - 30, p.y + 165)
                        layers.clearings.add(sprite("clearing-suit-" + game.mapping(c).head))(p.x - dx - 30, p.y + 165)
                    }
                }
            }

        if (false)
            game.clearings.foreach { c =>
                val p = regions.center(c)

                (factions ++ game.unhired).foreach { f =>
                    if (game.states.contains(f))
                    if (f.rules(c)) {
                        f match {
                            case f : WarriorFaction =>
                                val r = DrawItem(c, f, f.warrior, p.x.~, p.y.~, 4.5).rect
                                layers.clearings.add(sprite(r.key).use(x => x.copy(images = x.images./(i => i.copy(alpha = 0.28)))))(r.x, r.y + 150)
                                layers.clearings.add(sprite(r.key).use(x => x.copy(images = x.images./(i => i.copy(alpha = 0.28)))))(r.x, r.y + 150+50)

                            case f : Hero =>

                            case _ => println("non-warrior faction rule")
                        }
                    }
                }
            }

        val g = bitmap.context
        g.setTransform(1, 0, 0, 1, 0, 0)

        g.clearRect(0, 0, bitmap.width, bitmap.height)

        val d = options.has(AutumnMap).?(0).|(12)
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

        if (deadmp == null && currentGame.ui.graveyard) {
            deadmp = new Bitmap(mp.width, mp.height)
            deadmp.context.drawImage(mp, 0, 0)

            val q = deadmp.context.getImageData(0, 0, deadmp.width, deadmp.height)

            var i = 0
            while (i < q.width) {
                var j = 0
                while (j < q.height) {
                    val p = (j * q.width + i) * 4

                    val r = q.data(p + 0)
                    val g = q.data(p + 1)
                    val b = q.data(p + 2)

                    q.data(p + 0) = (r * 0.30 + g * 0.59 + b * 0.11).round.toInt
                    q.data(p + 1) = (r * 0.30 + g * 0.59 + b * 0.11).round.toInt
                    q.data(p + 2) = (r * 0.30 + g * 0.59 + b * 0.11).round.toInt

                    j += 1
                }

                i += 1
            }

            deadmp.context.putImageData(q, 0, 0)
        }

        if (currentGame.ui.graveyard)
            g.drawImage(deadmp.canvas, 0, 0)
        else
            g.drawImage(mp, 0, 0)

        game.rubble.foreach { case (from, to) =>
            val pp = $("rubble-" + from.name + "-" + to.name, "rubble-" + to.name + "-" + from.name)
            pp.%(resources.images.has).take(1)./(p => g.drawImage(resources.images.get(p), 0, 0))
        }

        if (highlight.any) {
            def extractClearings(o : Any) : $[Clearing] = o match {
                case c : Clearing => $(c)
                case v : Option[_] => extractClearings(v.$)
                case l : List[_] => l.flatMap(extractClearings)
                case p : Product => p.productIterator.flatMap(extractClearings).$
                case _ => $()
            }

            val l = extractClearings(highlight.get)

            if (l.num == 2) {
                val pp = $("path-" + l(0).name + "-" + l(1).name, "path-" + l(1).name + "-" + l(0).name)
                pp.%(resources.images.has).take(1).foreach(p => g.drawImage(resources.images.get(p), 20, 20))
            }

            l.foreach { c =>
                g.globalAlpha = 1.0
                val (x, y) = game.board.center(c)
                g.drawImage(resources.images.get("clearing-highlight-placement"), x - 300, y - 300)
            }
        }

        val hhh =
            if (game.over.none)
                game.highlights./~(x => $(x, x)).reverse.zip($(1.0)).reverse
            else
                game.highlights.% {
                    case MoveHighlight(_, _) => currentGame.ui.movements
                    case BattleHighlight(_) => currentGame.ui.battles
                    case NukeHighlight(_) => currentGame.ui.nukes
                    case _ => false
                }./((_, 1.0))

        hhh.foreach { h =>
            g.globalAlpha = h._2
            h._1 match {
                case PlaceHighlight(l) =>
                    l.foreach { c =>
                        val (x, y) = game.board.center(c)
                        g.drawImage(resources.images.get("clearing-highlight-placement"), x - 300, y - 300)
                    }
                case BattleHighlight(c) =>
                    val (x, y) = game.board.center(c)
                    g.drawImage(resources.images.get("clearing-highlight-battle"), x - 300, y - 300)
                case MoveHighlight(from, to) =>
                    val pp = $(mapid + "path-" + from.name + "-" + to.name, "path-" + to.name + "-" + from.name)
                    pp.%(resources.images.has).take(1)./{ p => g.drawImage(resources.images.get(p), 20, 20) }
                case _ =>
            }
        }

        def drawRule() {
            game.flooded.foreach { c =>
                c @@ {
                    case TidalBoard.HouseBoat => g.drawImage(resources.images.get(mapid + "flood-north"), 0, 0)
                    case TidalBoard.Stilts => g.drawImage(resources.images.get(mapid + "flood-south-east"), 0, 0)
                    case TidalBoard.Wetlands => g.drawImage(resources.images.get(mapid + "flood-south-west"), 0, 0)
                    case c =>
                        val (x, y) = game.board.center(c)
                        g.drawImage(resources.images.get(mapid + "flood"), x - 248 - 40, y - 213 + 10 - (c != TidalBoard.HouseBoat).??(30))
                }
            }

            if (game.clearings.forall(game.mapping.contains)) {
                game.clearings.foreach { c =>
                    val (x, y) = game.board.center(c)
                    val dx = c.name.length * 12 + 38

                    val nmn = c.name.replace(' ', '-')
                    val nmi = resources.images.get(mapid + "clearing-name-" + nmn)
                    val stn = game.mapping(c)./(_.name).distinct.join("-")
                    val sti = resources.images.get("text-tint:" + stn)

                    if (nmi.complete && sti.complete) {
                        val nmt = new Bitmap(nmi.width, nmi.height)
                        nmt.context.drawImage(nmi, 0, 0)

                        nmt.context.globalCompositeOperation = "multiply"
                        nmt.context.drawImage(sti, 0, 0)

                        nmt.context.globalCompositeOperation = "destination-in"
                        nmt.context.drawImage(nmi, 0, 0)

                        g.drawImage(nmt.canvas, x - 240, y + 165)
                    }

                    val suits = game.mapping(c)

                    suits.indexed.foreach { (s, n) =>
                        g.drawImage(resources.images.get("clearing-suit-" + s), x - 30 + dx + 40 * n + (n > 0).??(5) + (n > 2).??(5), y + 165)
                        g.drawImage(resources.images.get("clearing-suit-" + s), x - 30 - dx - 40 * n - (n > 0).??(5) - (n > 2).??(5), y + 165)
                    }

                    val offs = game.original(c).diff(suits)

                    offs.indexed.foreach { (s, i) =>
                        val n = i + suits.num
                        g.drawImage(resources.images.get("clearing-suit-" + s + "-off"), x - 30 + dx + 40 * n + (n > 0).??(10) + (n > 2).??(5), y + 165)
                        g.drawImage(resources.images.get("clearing-suit-" + s + "-off"), x - 30 - dx - 40 * n - (n > 0).??(10) - (n > 2).??(5), y + 165)
                    }

                    (factions ++ game.unhired).foreach { f =>
                        if (game.states.contains(f))
                        if (f.rules(c)) {
                            f match {
                                case f : WarriorFaction =>
                                    if (callbacks.settings.has(ShowRule)) {
                                        val r = DrawItem(c, f, f.warrior, x, y, 4.5).rect
                                        g.globalAlpha = 0.28
                                        g.drawImage(resources.images.get(r.key), r.x, r.y + 150, r.width, r.height)
                                        g.globalAlpha = 1.0
                                    }

                                case f : Hero =>
                                    if (callbacks.settings.has(ShowRule)) {
                                        val r = DrawItem(c, f, Vagabond, x, y, 4.5).rect
                                        g.globalAlpha = 0.28
                                        g.drawImage(resources.images.get(r.key), r.x, r.y + 150, r.width, r.height)
                                        g.globalAlpha = 1.0
                                    }

                                case _ => println("non-warrior faction rule")
                            }
                        }
                    }
                }
            }
        }

        g.globalAlpha = currentGame.ui.graveyard.?(0.5).|(1.0)

        val nowoods = (game.turn > 0) && factions.of[Hero].none && factions.of[Expedition].none && factions.of[OldExpedition].none && factions.of[CommonAbduct].none && hirelings.has(TheExile).not

        if (nowoods.not)
            g.drawImage(resources.images.get(mapid + "map-woods"), 0, 0)

        var saved = oldPositions
        oldPositions = $

        var draws : $[DrawItem] = $

        if (currentGame.states.contains(HighwayBandits)) {
            val h = HighwayBandits
            h.paths.rights.filter(r => h.present(r)).foreach { case p : PathBetween =>
                val (x, y) = game.board.mid(p.a, p.b)
                draws :+= DrawItem(p, h, Bandit, x, y + 20)
            }
        }

        if (currentGame.states.contains(XC)) {
            val h = XC
            h.growth.foreach { case (a, b) =>
                val (x, y) = game.board.mid(a, b)
                draws :+= DrawItem(PathBetween(a, b), h, Growth, x, y + 20)
            }
        }

        val noruins = (game.turn > 0) && factions.of[Hero].none && factions.of[Horde].none && game.hirelings.has(Brigand).not

        game.board.regions.foreach { r =>
            var fixed : $[DrawItem] = $
            var tofix : $[DrawItem] = $
            var all : $[DrawItem] = $
            var sticking : $[DrawItem] = $
            var free : $[DrawItem] = $

            val (unruins, figures) = currentGame.ui.graveyard.?(game.graveyard.get(r).|(Nil)).|(game.displayRegion(r)).partition(_.piece.is[Ruins] && noruins)

            var gates = game.board.slots.get(r).|(Nil).take(r.as[Clearing].?(game.slots)).drop(unruins.num)

            figures.foreach { p =>
                p.piece match {
                    case Keep =>
                        val (x, y) = game.board.center(r)
                        fixed :+= DrawItem(r, p.faction, p.piece, x, y)
                    case Ferry if AutumnBoard.clearings.contains(r) || LakeBoard.clearings.contains(r) =>
                        val (x, y) = game.board.port(r)(game.flooded)
                        fixed :+= DrawItem(r, p.faction, p.piece, x, y)
                    case Tower if r == MountainBoard.Pass =>
                        val (x, y) = (1127, 1014)
                        fixed :+= DrawItem(r, p.faction, p.piece, x, y)
                    case _ =>
                        saved.find(o => o.region == r && o.piece == p.piece && o.faction == p.faction) match {
                            case Some(o) if o.rect.key == DrawItem(r, p.faction, p.piece, 0, 0).rect.key =>
                                p.piece match {
                                    case b : Building =>
                                        fixed +:= o
                                        saved = saved.but(o)
                                        gates = gates.but((o.x, o.y))
                                    case _ =>
                                        sticking +:= o
                                        saved = saved.but(o)
                                }
                            case _ =>
                                p.piece match {
                                    case b : Building =>
                                        tofix +:= DrawItem(r, p.faction, p.piece, 0, 0)
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

            if (r.is[Clearing] && game.clearings.contains(r)) {
                gates.foreach { case (x, y) =>
                    fixed :+= DrawItem(r, Neutral, FreeBuildingSlot, x, y)
                }
            }

            free ++= tofix

            if (free.num > 3) {
                free = free ++ sticking
                sticking = $
            }

            def rank(d : DrawItem) = d.piece match {
                case Wood => 0
                case w : Warrior => 1
                case t : Token => 2
                case p : Pawn => 3
                case b : Building => 5
            }

            val (px, py) = game.board.center(r)

            free.sortBy(d => -rank(d)).foreach { d =>
                sticking +:= Array.tabulate(40)(n => findAnother(px, py))
                  .sortBy { case (x, y) => ((x - px).abs * 5 + (y - py).abs) }
                  .map { case (x, y) => DrawItem(d.region, d.faction, d.piece, x, y) }
                  .minBy { dd =>
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

        g.globalAlpha = currentGame.ui.graveyard.?(0.6).|(1.0)

        draws.%(_.piece == Keep).sortBy(d => d.y - (d.piece.is[Token]).??(10000) - (d.piece == FreeBuildingSlot).??(20000) - (d.piece == Keep).??(40000)).foreach { d =>
            g.drawImage(resources.images.get(d.rect.key), d.rect.x, d.rect.y)
        }

        game.board.blizzard.foreach { case (from, to) =>
            val (x, y) = game.board.mid(from, to)

            if (game.blizzard.has((from, to)))
                g.drawImage(resources.images.get(mapid + "blizzard"), x - 150, y - 150)
            else
                g.drawImage(resources.images.get(mapid + "blizzard-mark"), x - 32, y - 32)
        }

        drawRule()

        draws.%(_.piece != Keep).sortBy(d => d.y - (d.piece.is[Token]).??(10000) - (d.piece == FreeBuildingSlot).??(20000) - (d.piece == Keep).??(40000)).foreach { d =>
            g.drawImage(resources.images.get(d.rect.key), d.rect.x, d.rect.y)
        }

        hhh.foreach { h =>
            g.globalAlpha = h._2
            h._1 match {
                case NukeHighlight(l) =>
                    l.foreach { c =>
                        val (x, y) = game.board.center(c)
                        g.drawImage(resources.images.get("clearing-highlight-nuke"), x - 300, y - 300)
                    }
                case _ =>
            }
        }

        if ("cat" == "paw") {
            val cat = $(
                (1, 10), (1, 9), (1, 8), (1, 7), (1, 6), (1, 5), (1, 4), (5, 9),
                (2, 3), (3, 2), (4, 2), (5, 1), (6, 1), (7, 1), (8, 1), (6, 9),
                (9, 1), (10, 1), (11, 1), (12, 1), (13, 1), (14, 2), (15, 2),
                (16, 3), (17, 4), (17, 5), (17, 6), (17, 7),
                (17, 8), (17, 9), (17, 10), (16, 11), (16, 12), (16, 13),
                (15, 14), (15, 15), (14, 16), (13, 15), (12, 14), (11, 12),
                (10, 12), (9, 12), (8, 12), (7, 12), (6, 14), (5, 15), (4, 16),
                (3, 15), (3, 14), (2, 13), (2, 12), (2, 11), (3, 8), (4, 9),
                (9, 5), (6, 7), (7, 8), (5, 8), (5, 7), (4, 7), (9, 6),
                (8, 4), (7, 5), (10, 4), (11, 5), (11, 8), (13, 8), (13, 7),
                (12, 7), (12, 9), (13, 9), (14, 9), (14, 7), (15, 8),
            )

            cat.foreach { case (x, y) =>
                g.drawImage(resources.images.get("mc-recruiter"), mp.width / 2 - 19 * 50 + x * 100, mp.height / 2 + 16 * 50 - y * 100)
            }
        }

        g.globalAlpha = 1.0

        if (resources.images.incomplete.any) {
            resources.images.incomplete = $

            setTimeout(200)(drawMap())
        }

        var alt = ""
        var altE : Elem = Empty
        var connected = ""
        var connectedE : Elem = Empty

        game.board.regions.foreach { r =>
            var gates = game.board.slots.get(r).|(Nil).take(r.as[Clearing].?(game.slots))

            val figures = currentGame.ui.graveyard.?(game.graveyard.get(r).|(Nil)).|(game.displayRegion(r))

            val pieces = figures./(_.piece)

            if (game.mapping.any)
                if (figures.any || r.is[Forest].not) {
                    alt += (r.name + ": " + pieces.none.??("empty") + pieces.distinct./(p => pieces.count(p).use(n => (n == 1).?(p.name).|(n + " " + p.plural))).join(", ") + "; ")
                    altE ~= (r.name + ": " + pieces.none.??("empty") + pieces.distinct./(p => pieces.count(p).use(n => (n == 1).?(p.name).|(n + " " + p.plural))).join(", ")).txt.div
                }
        }

        if (game.mapping.any)
            game.board.regions.of[Clearing].%(game.mapping.contains).foreach { r =>
                connected += r.as[Clearing]./(_.asset.toString + " clearing ").|("") + r.name + " - connected to " + game.connected(r)./(_.name).join(", ") + "; "
                connectedE ~= (r.as[Clearing]./(_.asset.toString + " clearing ").|("") + r.name + " - connected to " + game.connected(r)./(_.name).join(", ")).txt.div
            }

        mapSmall.replace(Div(Div(title, styles.title) ~ Div(Header(1, "Board", $) ~ altE ~ Header(1, "Map", $) ~ connectedE, ExternalStyle("screen-reader")), xstyles.overlay), resources)
    }

    hrf.HRF.onKey(e => e.ctrlKey.not && e.altKey.not && e.shiftKey.not && e.code == "KeyV") {
        if (Speech.enabled) {
            Speech.cancel()
            Speech.enabled = false
        }
        else {
            Speech.enabled = true
            Speech.say("Voice activated. Press Alt-V for instructions. Press Shift-V for voice options.")
        }
    }

    Speech.onKey("KeyV", alt = true) {
        Speech.say("Press 'G' - game state overview. Press 'Y' - board state. Press 'Q' - available actions. Press 'T' - cards in hand. Press Shift with any button for more info. Press Alt with any button for instructions.")
    }

    Speech.onKey("KeyV", shift = true) {
        Speech.say("Press 'Z' - next voice. Press 'X' - slower. Press 'C' - faster.")
    }

    Speech.onKey("KeyZ") {
        Speech.voice += 1
        Speech.say("Next voice.")
    }

    Speech.onKey("KeyX") {
        Speech.rate = min(Speech.rate - 0.2, 3)
        Speech.say("Talking slower, 1 2 3 4 5.")
    }

    Speech.onKey("KeyC") {
        Speech.rate = max(Speech.rate + 0.2, 0.2)
        Speech.say("Talking faster, 1 2 3 4 5.")
    }

    Speech.onKey("KeyT") {
        lastSelf.of[Faction]./{ self =>
            game.states.get(self)./{ player =>
                player.hand.$.some./{ l =>
                    Speech.say(l.num + " card" + (l.num > 1).??("s") +" in hand. " + l./(_.altS).mkString(". "))
                } | {
                    Speech.say("No cards in hand.")
                }
            }
        }.|(Speech.say("No current player."))
    }

    Speech.onKey("KeyT", shift = true) {
        lastSelf.of[Faction]./{ self =>
            game.states.get(self)./{ player =>
                player.hand.$.some./{ l =>
                    Speech.say(l./(_.altL).mkString(". "))
                } | {
                    Speech.say("No cards in hand.")
                }
            }
        }.|(Speech.say("No current player."))
    }

    Speech.onKey("KeyG") {
        var s = ""

        factions.foreach { f =>
            val vp = f.vp
            val h = f.hand.num
            s += "Faction " + f.name + ", " + f.vp + " victory points, " + (h == 0).?("no").|("" + h) + " card" + (h != 1).??("s") + " in hand. "
        }

        Speech.say(s)
    }

    Speech.onKey("KeyY") {
        var s = ""

        factions.sortBy(f => game.clearings.%(f.rules).num).reverse.foreach { f =>
            val n = game.clearings.%(f.rules).num
            val k = game.clearings.%(f.present).num
            s += f.name + " rules " + (n == 0).?("no").|("" + n) + " clearing" + (n != 1).??("s") + ", present in " + (k == 0).?("no").|("" + k) + " clearing" + (k != 1).??("s") + ". "
        }

        Speech.say(s)
    }

    Speech.onKey("KeyY", alt = true) {
        Speech.say("Press UIOP, HJKL, BNM-comma to list pieces in a clearing. Buttons on the keyboard roughly match position of clearings on the map.")
    }

    $(
        "KeyU" -> AutumnBoard.Hill,
        "KeyI" -> AutumnBoard.Beach,
        "KeyO" -> AutumnBoard.Creek,
        "KeyP" -> AutumnBoard.Quarry,
        "KeyH" -> AutumnBoard.Dune,
        "KeyJ" -> AutumnBoard.Glade,
        "KeyK" -> AutumnBoard.Waterfall,
        "KeyL" -> AutumnBoard.Mountain,
        "KeyB" -> AutumnBoard.Haven,
        "KeyN" -> AutumnBoard.Meadow,
        "KeyM" -> AutumnBoard.Pond,
        "Comma" -> AutumnBoard.Weald,
    )./{ (k, c) =>
        Speech.onKey(k) {
            val noruins = (game.turn > 0) && factions.of[Hero].none && factions.of[Horde].none

            val (unruins, figures) = currentGame.ui.graveyard.?(game.graveyard.get(c).|(Nil)).|(game.displayRegion(c)).partition(_.piece.is[Ruins] && noruins)

            var gates = game.board.slots.get(c).|(Nil).take(game.slots(c)).drop(unruins.num)

            val pp = figures./(_.piece)

            val pieces = pp ++ (game.slots(c) - pp.of[Building].num - unruins.num).times(FreeBuildingSlot)

            val alt = (c.name + ": " + pp.none.??("empty, ") + pieces.distinct./(p => pieces.count(p).use(n => (n == 1).?(p.name).|(n + " " + p.plural))).join(", ") + "; ")

            Speech.say(alt)
        }

        Speech.onKey(k, shift = true) {
            val connected = c.asset.toString + " clearing " + c.name + " - connected to " + game.connected(c)./(_.name).join(", ") + ". " + game.byRiver(c).some./("River to " + _.mkString(" and ") + ". ").|("")

            Speech.say(connected)
        }
    }

    def sayCurrentActions(unavailable : Boolean) {
        if (lastWaiting.any) {
            Speech.say("Waiting " + lastWaiting.any.??(" for ") + lastWaiting./(_.name).mkString(", "))
            return
        }

        if (lastActions.none) {

            return
        }

        val ll = lastActions.notOf[Hidden].notOf[Cancel]
        val l = ll.drop(lastActionsOffset)
        var q = l(0).question.text
        var s = q + " ... "
        1.to(9).foreach { i =>
            if (i - 1 < l.length) {
                val (action, available) = l(i - 1) @@ {
                    case u : Unavailable => (u.action, false)
                    case a => (a, true)
                }

                if (action.question.text != q) {
                    q = action.question.text
                    s += " ... " + q
                }

                if (available || unavailable)
                    s += action.is[Info].not.?(i + ", ").|("") + action.as[Selectable].?(_.selected).?(" selected ").|(available.not.??(" unavailable ")) + action.option.text + ". "
            }
        }

        if (ll.num > 9) {
            s += "0, More actions. "
        }

        if (lastActions.notOf[Hidden].of[Cancel].any) {
            s += "Backspace, Cancel. "
        }

        Speech.say(s)
    }

    Speech.onKey("KeyQ") {
        sayCurrentActions(false)
    }

    Speech.onKey("KeyQ", shift = true) {
        sayCurrentActions(true)
    }

    Speech.onKey("KeyQ", alt = true) {
        Speech.say("Press Q to list all available actions. Press Shift-Q to list all actions, including unavailable.")
    }

    1.to(9).foreach { n =>
        Speech.onKey("Digit" + n) {
            if (lastActions.any) {
                val l = lastActions.notOf[Hidden].notOf[Cancel].drop(lastActionsOffset)
                if (n - 1 < l.length) {
                    val action = l(n - 1)
                    val q = action.question.text
                    val s = action.option.text
                    if (action.is[Info].not && action.isSoft) {
                        overlayPane.invis()
                        overlayPane.clear()

                        val then = lastThen
                        lastThen = null
                        lastActions = $
                        lastActionsOffset = 0
                        lastActionsChosen = None
                        lastWaiting = $

                        asker.clear()

                        then(action)
                    }
                    else
                    if (action.is[Info]) {
                        Speech.say(s)
                    }
                    else {
                        lastActionsChosen = Some(n - 1)

                        Speech.say("confirm " + q + s + " with Enter.")
                    }
                }
            }
        }
    }

    1.to(9).foreach { n =>
        Speech.onKey("Digit" + n, shift = true) {
            if (lastActions.any) {
                val l = lastActions.notOf[Hidden].notOf[Cancel].drop(lastActionsOffset)
                if (n - 1 < l.length) {
                    val action = l(n - 1)
                    val q = action.question.text
                    val s = action.option.text
                    if (action.is[Info].not && action.isSoft) {
                        Speech.say(q + " - " + s + " ... Press without Shift to enter the sub-menu.")
                    }
                    else
                    if (action.is[Info]) {
                        Speech.say(s)
                    }
                    else {
                        lastActionsChosen = Some(n - 1)
                        Speech.say(q + " - " + s + " ... Press " + n + " and then Enter to confirm.")

                    }
                }
            }
        }
    }

    Speech.onKey("Digit" + 0) {
        if (lastActions.any) {
            lastActionsOffset += 9
            val l = lastActions.notOf[Hidden].notOf[Cancel].drop(lastActionsOffset)
            if (l.none)
                lastActionsOffset = 0

            lastActionsChosen = None

            sayCurrentActions(false)
        }
    }

    Speech.onKey("Digit" + 0, shift = true) {
        Speech.say("Skip to further action options.")
    }

    Speech.onKey("Backspace", shift = true) {
        Speech.say("Exit sub-menu.")
    }

    Speech.onKey("Backspace") {
        val cancel = lastActions.notOf[Hidden].of[Cancel].starting

        if (cancel.any) {
            overlayPane.invis()
            overlayPane.clear()

            val then = lastThen
            lastThen = null
            lastActions = $
            lastActionsOffset = 0
            lastActionsChosen = None
            lastWaiting = $

            asker.clear()

            then(cancel.get)
        }
    }

    Speech.onKey("Enter") {
        if (lastActions.any && lastActionsChosen.any) {
            val action = lastActions.notOf[Hidden].notOf[Cancel].drop(lastActionsOffset).drop(lastActionsChosen.get).head

            overlayPane.invis()
            overlayPane.clear()

            val then = lastThen
            lastThen = null
            lastActions = $
            lastActionsOffset = 0
            lastActionsChosen = None
            lastWaiting = $

            asker.clear()

            then(action)
        }
    }

    Speech.onKey("NumpadEnter") {
        if (lastActions.any && lastActionsChosen.any) {
            val action = lastActions.notOf[Hidden].notOf[Cancel].drop(lastActionsOffset).drop(lastActionsChosen.get).head

            overlayPane.invis()
            overlayPane.clear()

            val then = lastThen
            lastThen = null
            lastActions = $
            lastActionsOffset = 0
            lastActionsChosen = None
            lastWaiting = $

            asker.clear()

            then(action)
        }
    }


    val statusBitmaps = statuses

    def factionStatus(f : Faction, container : Container) {
        import helper._

        val nameres = resources.copy(names = () => {
            var r = resources.names()
            game.players.foreach { p =>
                if (r.contains(p))
                    if (game.ptf.contains(p))
                        r += game.ptf(p) -> r(p)
            }
            r
        })

        if (!game.states.contains(f)) {
            container.replace(Div(Div(nameres.getName(f).|(f.name)).styled(f), styles.smallname, xlo.pointer), resources)
            return
        }

        val factionName = (currentGame.ui.funNames && nameres.getName(f).any).?(f.funName).|(f.name)
        val characterName = f.as[Hero]./(_.character./(_.name).|("~~~"))
        val title = (factionName ~ characterName./(" " + _).|("")).styled(f).div(styles.smallname).pointer.onClick.param("fun-name")

        def domHint(s : Suit) = s match {
            case Bird => "Bird Domination: wins if rules two opposite corners at the start of the turn"
            case s => s.name + " Domination: wins if rules there " + s.name + " clearings at the start of the turn"
        }

        val handcards = (f.hand.num > 5).?(
            (f.hand.num / 5).times(Image("deck", styles.card5info)) ~ (f.hand.num % 5).times(dt.CardBackInfo)
        ).|(
            (f.hand.num).times(dt.CardBackInfo).merge
        )

        val hand : Elem = (f @@ {
            case f : Trader =>
                (f.hand.none).?(Hint("Hand: no cards", "~~~")).|(
                    Hint("Hand:\n" + f.hand.get./(_.name).join("\n"),
                        f.hand.get./(_.suit)./(s => dt.CardSuitInfo(s)).merge)
                    )
            case f : Fanatic =>
                (f.hand.none && f.revealed.none).?(Hint("Hand: no cards", "~~~")).|(
                    Hint("Hand: " + f.hand.num + " cards" + f.revealed.any.??("\nRevealed:\n" + f.revealed.get./(_.name).join("\n")),
                        handcards ~ f.revealed.get./(_.suit)./(s => dt.CardSuitInfo(s)).merge)
                    )
            case f : Underground =>
                (f.hand.none && f.revealed.none).?(Hint("Hand: no cards", "~~~")).|(
                    Hint("Hand: " + f.hand.num + " cards" + f.revealed.any.??("\nRevealed:\n" + f.revealed.get./(_.name).join("\n")),
                        handcards ~ f.revealed.get./(_.suit)./(s => dt.CardSuitInfo(s)).merge)
                    )
            case _ =>
                f.hand.none.?(Hint("Hand: no cards", "~~~")).|(
                    Hint("Hand: " + f.hand.num + " cards", handcards)
                )
        }).pointer.onClick.param(("view-hand", f))

        val vp = f.coalition./(a => Hint("Coalition with " + a.name + ": wins if " + a.name + " wins", a.ss)).||(f.dominance./(d => Hint(domHint(d.suit), ("(" + d.suit.name + ")").styled(d.suit)))).|(Hint("Victory points: " + f.vp, f.vp.vp)).pointer.onClick.param(("view-scorelog", f))

        val vphand = Div(vp ~ Image("card-separator", styles.cardbackinfo) ~ hand)

        def icon(p : Fund) : Elem = p match {
            case f : WarriorFaction => Image((f.style + "-" + f.warrior.id), styles.fund)
        }

        def letter(p : Fund) : Elem = p match {
            case f : WarriorFaction => f.warrior.name.take(1).styled(f)
        }

        def iconx(p : Fund) : Elem = p match {
            case f : WarriorFaction => Image(f.warrior.imgid(f), styles.wr)
        }

        def pack(l : $[Fund], n : Int, icon : Fund => Elem) : Elem = {
            if (2 * l.num <= n) {
                l./(icon) ~ "".padTo(n - 2 * l.num, ' ').pre
            } else {
                val r = l.distinct.sortBy(w => l.count(w)).reverse./(w => (l.count(w) > 1).??("" + l.count(w)) + "  ").mkString(" ")
                l.distinct.sortBy(w => l.count(w)).reverse./(w => (l.count(w) > 1).?("" ~ l.count(w).hl) ~ icon(w)).join("+") ~ "".padTo(n - r.length, ' ').pre
            }
        }

        def pack2(l : $[Fund], icon : Fund => Elem) : Elem = l./(icon).merge

        val effects = (f.effects ++ f.services)./(e => Hint(e match {
            case Armorers => "Armorers: Ignore all rolled hits taken in battle (discard after use)"
            case Sappers => "Sappers: Deal an extra hit in battle as defender (discard after use)"
            case BrutalTactics => "Brutal Tactics: May deal an extra hit in battle as attacker, but defender will score one point"
            case RoyalClaim => "Royal Claim: Score one point per clearing ruled at the start of the turn (discard after use)"
            case StandAndDeliver => "Stand and Deliver! May take a random card from another player in Birdsong, but that player will score one point"
            case TaxCollector => "Tax Collector: May remove one warrior to draw a card once in Daylight"
            case CommandWarren => "Command Warren: May initiate a battle at the start of Daylight"
            case BetterBurrowBank => "Better Burrow Bank: Draw a card at the start of Birdsong, choose another player to draw a card"
            case Cobbler => "Cobbler: May take a move at the start of Evening"
            case ScoutingParty => "Scouting Party: Unaffected by Ambush cards"
            case Codebreakers => "Codebreakers: May look at another player's hand once in Daylight"
            case _ => "???"
        }, (e @@ {
                case e : DisplayEffect => e.elem
                case e => ("{{" + e.toString + "}}").hl
            } ~ e.as[Hireling]./(h => hrf.elem.FigureSpace.toString.pre ~ f.contracts.count(h).times(hrf.elem.Dagger.hlb).merge)).div(styles.effect) ~
            (e == TheExile).?(
                (TheExile.exhausted./(i => Image(i.exhaust.imgid, styles.ii)).merge ~ TheExile.ready./(i => Image(i.imgid, styles.ii)).merge).div(styles.warline)(xstyles.smaller85)
            ) ~
            (e == CoffinMakers  && game.coffins.any).?(pack (game.coffins.$./~(_.faction.as[WarriorFaction]), 2, iconx).div(styles.warline)) ~
            (e == BreakingDawn  && game.dawn   .any).?(pack2(game.dawn   .$./~(_.faction.as[WarriorFaction]),    iconx).div(styles.warline)) ~
            (e == HighNoon      && game.noon   .any).?(pack2(game.noon   .$./~(_.faction.as[WarriorFaction]),    iconx).div(styles.warline)) ~
            (e == DuskAwakening && game.dusk   .any).?(pack2(game.dusk   .$./~(_.faction.as[WarriorFaction]),    iconx).div(styles.warline)) ~
            Empty
        ).pointer.onClick.param((f, e))).merge

        val trade = f.forTrade.any.? {
            Gap ~ Gap ~ f.forTrade./(i => Image(i.imgid, styles.ii)).merge.div(xstyles.smaller85)
        }

        def emtb(s : String) = s.split('|').$./{
            case " " => Image("empty-building")(styles.building)
            case "+" => Image("empty-building-card")(styles.building)
            case "1" => Image("empty-building-1")(styles.building)
            case "2" => Image("empty-building-2")(styles.building)
            case "3" => Image("empty-building-3")(styles.building)
            case "4" => Image("empty-building-4")(styles.building)
            case "5" => Image("empty-building-5")(styles.building)
            case "1+" => Image("empty-building-1-card")(styles.building)
            case "2+" => Image("empty-building-2-card")(styles.building)
            case "3+" => Image("empty-building-3-card")(styles.building)
            case "4+" => Image("empty-building-4-card")(styles.building)
            case "5+" => Image("empty-building-5-card")(styles.building)
            case "1p" => Image("empty-building-1p")(styles.building)
            case "2p" => Image("empty-building-2p")(styles.building)
        }

        val fs : Elem = f match {
            case f : Feline =>
                val sm = "Sawmills\n" + f.all(Sawmill).num + " on the map\n" + f.pooled(Sawmill) + " available" + "\n\n"
                val ws = "Workshops\n" + f.all(Workshop).num + " on the map\n" + f.pooled(Workshop) + " available" + "\n\n"
                val rc = "Recruiters\n" + f.all(Recruiter).num + " on the map\n" + f.pooled(Recruiter) + " available" + "\n \n"

                Hint("Wood\n" + f.all(Wood).num + " on the map\n" + f.pooled(Wood) + " available", (Image(f.style + "-wood", styles.token) *** f.all(Wood).num) ~ (Image("empty-token", styles.token) *** f.pooled(Wood))) ~
                Gap ~
                Div(
                    Image("empty-building-cost-0", styles.building) ~
                    Image("empty-building-cost-1", styles.building) ~
                    Image("empty-building-cost-2", styles.building) ~
                    Image("empty-building-cost-3", styles.building) ~
                    Image("empty-building-cost-3", styles.building) ~
                    Image("empty-building-cost-4", styles.building)
                , styles.satchel) ~
                Hint(sm + "1st Sawmill: cost 0 Wood"      , (Image((f.all(Sawmill).num >= 1).?(f.style + "-sawmill").|("empty-building"  ))(styles.building))) ~
                Hint(sm + "2nd Sawmill: cost 1 Wood, 1 VP", (Image((f.all(Sawmill).num >= 2).?(f.style + "-sawmill").|("empty-building-1"))(styles.building))) ~
                Hint(sm + "3rd Sawmill: cost 2 Wood, 2 VP", (Image((f.all(Sawmill).num >= 3).?(f.style + "-sawmill").|("empty-building-2"))(styles.building))) ~
                Hint(sm + "4th Sawmill: cost 3 Wood, 3 VP", (Image((f.all(Sawmill).num >= 4).?(f.style + "-sawmill").|("empty-building-3"))(styles.building))) ~
                Hint(sm + "5th Sawmill: cost 3 Wood, 4 VP", (Image((f.all(Sawmill).num >= 5).?(f.style + "-sawmill").|("empty-building-4"))(styles.building))) ~
                Hint(sm + "6th Sawmill: cost 4 Wood, 5 VP", (Image((f.all(Sawmill).num >= 6).?(f.style + "-sawmill").|("empty-building-5"))(styles.building))) ~
                Break ~
                Hint(ws + "1st Workshop: cost 0 Wood"   + (f == BK).?(", extra move"             ).|(""    ), (Image((f.all(Workshop).num >= 1).?(f.style + "-workshop").|("empty-building"  ), styles.building))) ~
                Hint(ws + "2nd Workshop: cost 1 Wood, " + (f == BK).?("1 VP, extra battle"       ).|("2 VP"), (Image((f.all(Workshop).num >= 2).?(f.style + "-workshop").|("empty-building-2"), styles.building))) ~
                Hint(ws + "3rd Workshop: cost 2 Wood, " + (f == BK).?("2 VP, extra move"         ).|("2 VP"), (Image((f.all(Workshop).num >= 3).?(f.style + "-workshop").|("empty-building-2"), styles.building))) ~
                Hint(ws + "4th Workshop: cost 3 Wood, " + (f == BK).?("3 VP, extra card draw"    ).|("3 VP"), (Image((f.all(Workshop).num >= 4).?(f.style + "-workshop").|("empty-building-3" + (f == BK).??("-card")), styles.building))) ~
                Hint(ws + "5th Workshop: cost 3 Wood, " + (f == BK).?("4 VP, extra battle"       ).|("4 VP"), (Image((f.all(Workshop).num >= 5).?(f.style + "-workshop").|("empty-building-4"), styles.building))) ~
                Hint(ws + "6th Workshop: cost 4 Wood, " + (f == BK).?("5 VP, extra hit attacking").|("5 VP"), (Image((f.all(Workshop).num >= 6).?(f.style + "-workshop").|("empty-building-5"), styles.building))) ~
                Break ~
                Hint(rc + "1st Recruiter: cost 0 Wood"                       , (Image((f.all(Recruiter).num >= 1).?(f.style + "-recruiter").|("empty-building"       ))(styles.building))) ~
                Hint(rc + "2nd Recruiter: cost 1 Wood, 1 VP"                 , (Image((f.all(Recruiter).num >= 2).?(f.style + "-recruiter").|("empty-building-1"     ))(styles.building))) ~
                Hint(rc + "3rd Recruiter: cost 2 Wood, 2 VP, extra card draw", (Image((f.all(Recruiter).num >= 3).?(f.style + "-recruiter").|("empty-building-2-card"))(styles.building))) ~
                Hint(rc + "4th Recruiter: cost 3 Wood, 3 VP"                 , (Image((f.all(Recruiter).num >= 4).?(f.style + "-recruiter").|("empty-building-3"     ))(styles.building))) ~
                Hint(rc + "5th Recruiter: cost 3 Wood, 3 VP, extra card draw", (Image((f.all(Recruiter).num >= 5).?(f.style + "-recruiter").|("empty-building-3-card"))(styles.building))) ~
                Hint(rc + "6th Recruiter: cost 4 Wood, 4 VP"                 , (Image((f.all(Recruiter).num >= 6).?(f.style + "-recruiter").|("empty-building-4"     ))(styles.building))) ~
                (f.keepDamage > 0).?(("Keep Damage " ~ f.keepDamage.hl ~ "/" ~ 3.hl).div(styles.minister)) ~
                (game.current == f).?(Gap ~ 1.to(3 + f.extra)./(_ => Image("action-black", styles.building)).take(f.acted) ~ (1.to(3)./(_ => Image(f.style + "-action", styles.building)) ++ (0.until(f.extra)./(_ => Image("action-bird", styles.building)))).drop(f.acted)) ~
                options.has(Catapults).?(Gap ~ Hint("Catapults\n" + f.all(Catapult).num + " on the map\n" + f.pooled(Catapult) + " available", (Image(f.style + "-catapult", styles.token) *** f.all(Catapult).num) ~ (Image("empty-token", styles.token) *** f.pooled(Catapult)))) ~
                Div(Hint("Cats\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-cat-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-cat", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-cat-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-cat-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : Aviary =>
                val h = "Decree\n\n" + {
                    val c = Decree.all.map { d =>
                        val t = f.todo(d)
                        val o = f.done(d)
                        val r = t.diff(o).diff(o.diff(t)./(_ => Bird))
                        val c = t.diff(r)
                        c.any.?(d.name + ": " + c./(_.name).join(", "))
                    }.flatten

                    val r = Decree.all.map { d =>
                        val t = f.todo(d)
                        val o = f.done(d)
                        val r = t.diff(o).diff(o.diff(t)./(_ => Bird))
                        val c = t.diff(r)
                        r.any.?(d.name + ": " + r./(_.name).join(", "))
                    }.flatten

                    if (c.any && r.any)
                        "Completed\n" + c.join("\n") + "\n\n" + "Remaining\n" + r.join("\n")
                    else
                        (c ++ r).join("\n")
                }

                val decree = Decree.all.map { d =>
                    val t = f.todo(d)
                    val o = f.done(d)
                    val r = t.diff(o).diff(o.diff(t)./(_ => Bird))
                    val c = t.diff(r)
                    val id = f.style + "-" + (d == Decree.Recruit && f.leader == Some(Charismatic)).??("double-") + d.name
                    c./(s => Image(id + "-" + s.name + "-done", styles.building)) ++ r./(s => Image(id + "-" + s.name, styles.building))
                }

                val dddd = &(&(&(decree(0)) ~ " " ~ &(decree(1))) ~ " " ~ &(decree(2) ~ " " ~ &(decree(3))))

                f.leader./(l => Hint(l.name + " leader\n" + (l match {
                    case Builder => "Full VP crafting items"
                    case Charismatic => "Double warrior recruit"
                    case Commander => "Extra hit when attacking"
                    case Despot => "Extra 1 VP destroying buildings or tokens in battle"
                }), l.name.hl.div(styles.minister))).|("~~~".styled(f)).pointer.onClick.param(("view-leaders", f)) ~
                Gap ~
                Hint(h, dddd.pointer.onClick.param(("view-decree", f : Faction))) ~
                (f.tolerance > 0).?(("Turmoil Tolerance " ~ f.skipped.hl ~ "/" ~ f.tolerance.hl).div(styles.minister)) ~
                Gap ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "1 Roost"                           , (Image((f.all(Roost).num >= 1).?(f.style + "-roost").|("empty-building"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "2 Roosts: 1 VP"                    , (Image((f.all(Roost).num >= 2).?(f.style + "-roost").|("empty-building-1"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "3 Roosts: 2 VP, draw 1 extra card" , (Image((f.all(Roost).num >= 3).?(f.style + "-roost").|("empty-building-2-card"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "4 Roosts: 3 VP, draw 1 extra card" , (Image((f.all(Roost).num >= 4).?(f.style + "-roost").|("empty-building-3"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "5 Roosts: 4 VP, draw 1 extra card" , (Image((f.all(Roost).num >= 5).?(f.style + "-roost").|("empty-building-4"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "6 Roosts: 4 VP, draw 2 extra cards", (Image((f.all(Roost).num >= 6).?(f.style + "-roost").|("empty-building-4-card"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "7 Roosts: 5 VP, draw 2 extra cards", (Image((f.all(Roost).num >= 7).?(f.style + "-roost").|("empty-building-5"), styles.building))) ~
                Div(Hint("Hawks\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-hawk-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-hawk", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-hawk-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-hawk-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : Insurgent =>
                Gap ~
                FoxRabbitMouse./(s => Hint(s.name + " Base: " + f.suits.has(s).?("on the map").|("available") + ", draw 1 extra card", f.suits.has(s).?(Image(f.style + "-base-" + s.name, styles.building)).|(Image("empty-building-card", styles.building)))) ~
                Gap ~
                Image("empty-token-cost-1", styles.tokenHeight) ~
                Hint("Sympathy\n" + f.all(Sympathy).num + " on the map\n" + f.pooled(Sympathy) + " available" + "\n\n" +  "1st Sympathy: cost 1 card"       , (Image((f.all(Sympathy).num >=  1).?(f.style + "-sympathy").|("empty-token"  ), styles.token))) ~
                Hint("Sympathy\n" + f.all(Sympathy).num + " on the map\n" + f.pooled(Sympathy) + " available" + "\n\n" +  "2nd Sympathy: cost 1 card, 1 VP" , (Image((f.all(Sympathy).num >=  2).?(f.style + "-sympathy").|("empty-token-1"), styles.token))) ~
                Hint("Sympathy\n" + f.all(Sympathy).num + " on the map\n" + f.pooled(Sympathy) + " available" + "\n\n" +  "3rd Sympathy: cost 1 card, 1 VP" , (Image((f.all(Sympathy).num >=  3).?(f.style + "-sympathy").|("empty-token-1"), styles.token))) ~
                " " ~
                Image("empty-token-cost-2", styles.tokenHeight) ~
                Hint("Sympathy\n" + f.all(Sympathy).num + " on the map\n" + f.pooled(Sympathy) + " available" + "\n\n" +  "4th Sympathy: cost 2 cards, 1 VP", (Image((f.all(Sympathy).num >=  4).?(f.style + "-sympathy").|("empty-token-1"), styles.token))) ~
                Hint("Sympathy\n" + f.all(Sympathy).num + " on the map\n" + f.pooled(Sympathy) + " available" + "\n\n" +  "5th Sympathy: cost 2 cards, 2 VP", (Image((f.all(Sympathy).num >=  5).?(f.style + "-sympathy").|("empty-token-2"), styles.token))) ~
                Hint("Sympathy\n" + f.all(Sympathy).num + " on the map\n" + f.pooled(Sympathy) + " available" + "\n\n" +  "6th Sympathy: cost 2 cards, 2 VP", (Image((f.all(Sympathy).num >=  6).?(f.style + "-sympathy").|("empty-token-2"), styles.token))) ~
                Break ~
                Image("empty-token-cost-3", styles.tokenHeight) ~
                Hint("Sympathy\n" + f.all(Sympathy).num + " on the map\n" + f.pooled(Sympathy) + " available" + "\n\n" +  "7th Sympathy: cost 3 cards, 3 VP", (Image((f.all(Sympathy).num >=  7).?(f.style + "-sympathy").|("empty-token-3"), styles.token))) ~
                Hint("Sympathy\n" + f.all(Sympathy).num + " on the map\n" + f.pooled(Sympathy) + " available" + "\n\n" +  "8th Sympathy: cost 3 cards, 4 VP", (Image((f.all(Sympathy).num >=  8).?(f.style + "-sympathy").|("empty-token-4"), styles.token))) ~
                Hint("Sympathy\n" + f.all(Sympathy).num + " on the map\n" + f.pooled(Sympathy) + " available" + "\n\n" +  "9th Sympathy: cost 3 cards, 4 VP", (Image((f.all(Sympathy).num >=  9).?(f.style + "-sympathy").|("empty-token-4"), styles.token))) ~
                Hint("Sympathy\n" + f.all(Sympathy).num + " on the map\n" + f.pooled(Sympathy) + " available" + "\n\n" + "10th Sympathy: cost 3 cards, 4 VP", (Image((f.all(Sympathy).num >= 10).?(f.style + "-sympathy").|("empty-token-4"), styles.token))) ~
                Gap ~
                Gap ~
                Div(
                    f.bases.none.?(
                        (Hint("Supporters: " + (f.supporters.num == 0).?("no cards").|((f.supporters.num == 1).?("1 cards").|(f.supporters.num + " cards")) + ", max 5 cards",
                            f.supporters.num.times(dt.CardBackInfo) ~ (5 - f.supporters.num).times(dt.CardEmptyInfo), styles.supporters5))
                        ).|(
                            (Hint("Supporters: " + (f.supporters.num == 0).?("no cards").|((f.supporters.num == 1).?("1 cards").|(f.supporters.num + " cards")),
                            dt.CardEmptyInfo ~ f.supporters.num.times(dt.CardBackInfo).merge ~ dt.CardEmptyInfo, styles.supportersX))
                        ),
                styles.centered) ~
                Gap ~
                (game.current == f).?(1.to(f.officers.num)./(_ => Image("action-black", styles.building)).take(f.acted) ~ (1.to(f.officers.num)./(_ => Image(f.style + "-action", styles.building))).drop(f.acted)) ~
                Div(Hint("Critters\n" + f.all(f.warrior).num + " on the map\n" + f.officers.num + " officer".s(f.officers.num) + "\n" + f.pooled(f.warrior) + " available",
                &(&(Image(f.style + "-critter", styles.wr) *** f.all(Critter).num) ~ &(Image(f.style + "-critter-empty", styles.wr) *** (f.pooled(Critter) / 2)) ~ &(Image(f.style + "-critter-empty", styles.wr) *** (f.pooled(Critter) - f.pooled(Critter) / 2))) ~ &(Image(f.style + "-officer", styles.wr) *** f.officers.num)), styles.warline)

            case f : Fanatic =>
                Gap ~
                FoxRabbitMouse./(s =>
                    f.outcast.has(s).?(Image("outcast-" + s + f.hated.??("-hated"), styles.token)) ~
                    (Image(f.style + "-garden-" + s.name, styles.building) *** f.all(Garden(s)).num) ~ emtb(" |2+| |3|4").drop(f.all(Garden(s)).num) ~
                    f.outcast.has(s).?(Image("outcast-" + s + f.hated.??("-hated"), styles.token))
                ).join(Break) ~
                Gap ~
                Hint("Lost Souls\n" + f.lost.some./(l => FoxRabbitMouse./(s => f.lost.count(_.suit == s) + " " + s.name).join("\n")).|("none"),
                f.lost.some./(_./(_.suit)./(s => dt.CardSuitInfo(s)).merge.pointer.onClick.param(("view-lost-souls", f))).|("~~~".spn)) ~ Break ~
                Div(Hint("Lizards\n" + f.acolytes.num + " acolyte".s(f.acolytes.num) + "\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                    (f.acolytes.num > 0).?((Image(f.style + "-acolyte-x5", styles.wr) *** (f.acolytes.num / 5)) ~ (Image(f.style + "-acolyte", styles.wr) *** (f.acolytes.num % 5)) ~ Break) ~
                    &((Image(f.style + "-lizard-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-lizard", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                    &((Image(f.style + "-lizard-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-lizard-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : Trader =>
                val r1 = f.payments.$./(_.faction.asInstanceOf[WarriorFaction])
                val r2 = f.funds   .$./(_.faction.asInstanceOf[WarriorFaction])
                val r3 = f.commited.$./(_.faction.asInstanceOf[WarriorFaction])

                val tps = FoxRabbitMouse./(s => &(
                    (Image(f.style + "-trade-post-" + s.name + "-destroyed", styles.tokengl) *** f.gone.pieces.count(TradePost(s))) ~
                    (Image(f.style + "-trade-post-" + s.name, styles.tokengl) *** f.all(TradePost(s)).num) ~
                    (Image("empty-token-2", styles.tokengl) *** f.pooled(TradePost(s)))))

                Empty.div(styles.moneybox) ~
                Hint("Payments",   &&(dt.Income ~ " ".pre ~ pack(r1, 14, icon))).div(styles.moneybox) ~
                Hint("Funds",      &&(dt.Current ~ " ".pre ~ pack(r2, 14, icon))).div(styles.moneybox) ~
                Hint("Commited",   &&(dt.Swap ~ " ".pre ~ pack(r3, 14, icon))).div(styles.moneybox) ~
                &(&(tps(0)) ~ Span(" ", styles.narrow) ~ &(tps(1)) ~ Span(" ", styles.narrow) ~ &(tps(2))) ~ Break ~
                Gap ~
                (f.prices.values.forall(_ > 0)).? {
                    (
                        f.services./(s => Image(f.style + "-price-" + f.prices(s) + "-selected")(styles.price)) ~
                        Break ~
                        f.services./(s => s.img(f)(styles.service))
                    ).div(xstyles.smaller85)
                } ~
                Div(Hint("Otters\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-" + f.warrior + "-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-" + f.warrior, styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-" + f.warrior + "-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-" + f.warrior + "-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : Mischief =>
                Gap ~
                Plot.all./~(p => Image(f.style + "-" + p.name, styles.token) *** f.all(p).diff(f.hidden).num) ~
                (Image(f.style + "-plot", styles.token) *** f.hidden.num) ~
                (Image("empty-token", styles.token) *** (Plot.all./(f.pooled).sum - f.hidden.num)) ~
                (game.current == f).?(Gap ~ f.exert.?(
                    1.to(3)./(_ => Image("action-black", styles.building)) ~
                    Image("action-bird", styles.building),
                ).|(
                    1.to(3)./(_ => Image("action-black", styles.building)).take(f.acted) ~
                    1.to(3)./(_ => Image(f.style + "-action", styles.building)).drop(f.acted) ~
                    Image("action-bird", styles.building),
                )) ~
                Div(Hint("Ravens\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-raven-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-raven", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-raven-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-raven-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : Underground =>
                Gap ~
                &((Image(f.style + "-tunnel", styles.token) *** f.all(Tunnel).num) ~ (3 - f.all(Tunnel).num).times(Image("empty-token", styles.token))) ~
                Gap ~
                &((Image(f.style + "-citadel", styles.building) *** f.all(Citadel).num) ~ emtb("1p|2p|2p").drop(f.all(Citadel).num)) ~ " " ~
                &((Image(f.style + "-market", styles.building) *** f.all(Market).num) ~ emtb("+|+|+").drop(f.all(Market).num)) ~
                Gap ~
                $(2, 3, 4)./(r => Hint(Map(2 -> "Squires", 3 -> "Nobles", 4 -> "Lords")(r), Div((f.ministers.%(_.rank == r).intersect(f.swayed)./(m => f.worked.has(m).?(m.name.spn).|(m.name.hl)) ++ (Image(f.style + "-crown-" + r, styles.building) *** (3 - (f.swayed ++ f.retired).%(_.rank == r).num)) ++ (Image(f.style + "-crown-empty", styles.building) *** f.retired.%(_.rank == r).num)).spaced.merge), styles.minister)) ~
                (game.current == f).?(Gap ~ 1.to(2)./(_ => Image("action-black", styles.building)).take(f.acted) ~ (1.to(2)./(_ => Image(f.style + "-action", styles.building))).drop(f.acted)) ~
                Div(Hint("Moles\n" + f.at(f.burrow).num + " in Burrow\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                f.at(f.burrow).any.?(Gap ~ Div(&(
                    (Image(f.style + "-young-x5", styles.wr) *** (f.at(f.burrow).num / 5)) ~
                    (Image(f.style + "-young", styles.wr) *** (f.at(f.burrow).num % 5))
                ), styles.burrow, styles.inline, styles.get(f)) ~ Break) ~
                &((Image(f.style + "-mole-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-mole", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-mole-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-mole-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : Horde =>
                val iiii = game.uncrafted ++ factions./~(_.forTrade)./(_.item) ++ game.ruins.values./~(_.items)
                val iii = iiii.sortBy(i => -(iiii.count(i) + f.forTrade./(_.item).has(i).??(9)))
                val ii = {
                    var r = iii.take(0)
                    var x = iii
                    while (x.any) {
                        val n = x.distinct
                        r ++= n
                        x = x.diff(n)
                    }
                    r
                }

                val ic = ii.%(f.items.command.has).sortBy(i => f.hoard.command.has(i).??(1))
                val ip = ii.%(f.items.prowess.has).sortBy(i => f.hoard.prowess.has(i).??(1))

                (Image(f.style + "-mob", styles.token) *** f.all(Mob).num) ~ (Image("empty-token", styles.token) *** f.pooled(Mob)) ~
                Gap ~
                (Image(f.style + "-stronghold", styles.building) *** f.all(Stronghold).num) ~ (Image("empty-building", styles.building) *** f.pooled(Stronghold)) ~ Break ~
                f.mood.name.hl.div(styles.minister) ~
                Div(
                    Div(Image("satchel-top-2" + (f.command == 2).??("-" + f.style), styles.ii) ~ Image("satchel-top", styles.ii) ~ Image("satchel-top-3" + (f.command == 3).??("-" + f.style), styles.ii) ~ Image("satchel-top-4" + (f.command == 4).??("-" + f.style), styles.ii), styles.satchel) ~
                    Div(Image("satchel-left-1" + (f.command == 1).??("-" + f.style), styles.iih) ~
                        f.hoard.command./(i => Image("item-" + i.name, styles.ii)) ~ (ic./(i => Image("item-" + i.name + "-empty", styles.ii)) ++ 4.times(Image("item-x-placeholder", styles.ii))).take(4).dropRight(f.hoard.command.num) ~
                        Image("satchel-right-empty", styles.iih)
                    ) ~
                    Div(Image("satchel-middle-start", styles.ii) ~ 3.times(Image("satchel-middle-cont", styles.ii)), styles.satchel) ~
                    Div(Image("satchel-left-1" + (f.prowess == 1).??("-" + f.style), styles.iih) ~
                        f.hoard.prowess./(i => Image("item-" + i.name, styles.ii)) ~ (ip./(i => Image("item-" + i.name + "-empty", styles.ii)) ++ 4.times(Image("item-x-placeholder", styles.ii))).take(4).dropRight(f.hoard.prowess.num) ~
                        Image("satchel-right-empty", styles.iih)
                    ) ~
                    Div(Image("satchel-bottom-2" + (f.prowess == 2).??("-" + f.style), styles.ii) ~ Image("satchel-bottom", styles.ii) ~ Image("satchel-bottom-3" + (f.prowess == 3).??("-" + f.style), styles.ii) ~ Image("satchel-bottom-4" + (f.prowess == 4).??("-" + f.style), styles.ii), styles.satchel)
                , xstyles.smaller85) ~
                (game.current == f).?(f.actionInfo(styles.building, styles.building)) ~
                Div(Hint("Rats\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-rat-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-rat", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-rat-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-rat-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : Expedition =>
                Relic.types./(rt => (f.recovered(rt).$./(r => r.imgid) ++ (4 - f.recovered(rt).num).times(f.style + "-" + rt.name + "-empty"))./(Image(_, styles.token)))./(&).join(Break) ~ Break ~
                (f.wst.%(w => f.all(w).any)./(_.imgid(f)) ++ (3 - f.wst.%(w => f.all(w).any).num).times("empty-building-card"))./(Image(_, styles.building)).merge ~ Break ~
                (Retinue.all./(r => f.complete(r)./(d => f.style + "-" + r.id + "-" + d.suit.name + "-done") ++ f.retinue(r)./(d => f.style + "-" + r.id + "-" + d.suit.name)).%(_.any)./(_./(Image(_, styles.building)).merge)./(&).spaced.merge).pointer.onClick.param(("view-retinue", f)) ~ Break ~
                Div(Hint("Badgers\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-badger-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-badger", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-badger-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-badger-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : OldExpedition =>
                Hint("Caravans\n" + f.all(Caravan).num + " on the map\n" + f.pooled(Caravan) + " available",
                    (Image(f.style + "-caravan", styles.token) *** f.all(Caravan).num) ~ (Image("empty-token", styles.token) *** f.pooled(Caravan))) ~ Break ~
                Hint("Stations\n" + f.all(Station).num + " on the map\n" + f.pooled(Station) + " available",
                    (Image(f.style + "-station", styles.building) *** f.all(Station).num) ~ (Image("empty-building", styles.building) *** f.pooled(Station))) ~ Break ~
                Hint("Mission: " + f.mission.?./(_.name).|("not chosen yet"),
                   f.mission.?./(_.name).|("~~~").styled(f)) ~ Break ~
                OldRelics.all./(r =>
                    $(Fox, Mouse, Rabbit, Bird)./(s =>
                        Image(f.rewards.contains((r, s)).?("empty-token-" + s.name).|(f.style + "-" + r.name), styles.token)).merge).join(Break) ~ Break ~
                Div(Hint("Badgers\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-badger-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-badger", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-badger-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-badger-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : Utopia =>
                ("Corruption " ~ f.corruption.styled(styles.hit)).div(styles.minister) ~
                ("Draw " ~ f.draw.hl ~ " " ~ Cards(f.draw).elem).div(styles.minister) ~
                ("Freedom " ~ f.freedom.vp).div(styles.minister) ~
                (Image(f.style + "-palace", styles.building) *** f.all(Palace).num) ~ (Image("empty-building", styles.building) *** f.pooled(Palace)) ~ Break ~
                (game.current == f).?(Gap ~ 1.to(f.acted + f.extra)./(_ => Image("action-black", styles.building)) ~ (f.acted.until(f.corruption - f.extra)./(_ => Image(f.style + "-action", styles.building)))) ~
                Div(Hint("Foxies\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-foxy-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-foxy", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-foxy-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-foxy-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : Caster =>
                val ss = f.spells

                (Image(f.style + "-statue", styles.token) *** f.all(Statue).num) ~ (Image("empty-token", styles.token) *** f.pooled(Statue)) ~ Break ~ Gap ~
                (Image(f.style + "-school", styles.building) *** f.all(School).num) ~ (Image("empty-building", styles.building) *** f.pooled(School)) ~ Break ~ Gap ~
                ss./(_.cost).distinct./(c => ss.%(_.cost == c)./(_.toString.hl).spaced.merge.div(styles.minister)) ~
                (Image(f.style + "-magic", styles.token) *** f.mana) ~ (Image("empty-token", styles.token) *** (8 - f.mana)) ~ Break ~
                (game.current == f).?(Gap ~ 1.to(f.acted)./(_ => Image("action-black", styles.building)) ~ (f.acted.until(4)./(_ => Image(f.style + "-action", styles.building)))) ~
                Div(Hint("Frogs\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-frog-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-frog", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-frog-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-frog-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : Farmer =>
                (Image(f.style + "-grain", styles.token) *** f.all(Grain).num) ~ (Image("empty-token", styles.token) *** f.pooled(Grain)) ~ Break ~ Gap ~
                (Image(f.style + "-farm", styles.building) *** f.all(Farm).num) ~ (Image("empty-building", styles.building) *** f.pooled(Farm)) ~ Break ~ Gap ~
                (Image(f.style + "-windmill", styles.building) *** f.all(Windmill).num) ~ (Image("empty-building", styles.building) *** f.pooled(Windmill)) ~ Break ~ Gap ~
                game.candidates.intersect(factions).but(f)./(o => Image((o.style + "-" + o.as[WarriorFaction]./(_.warrior.id).|("vagabond")), styles.fund) ~ Image("attitude-" + f.foes.has(o).?("hostile").|("heart-full"), styles.attitude)).some./(_.join(Image("card-separator", styles.cardbackinfo)).div(styles.warline)).|("~~~" ~ Break) ~
                (game.current == f).?(Gap ~ 1.to(f.acted)./(_ => Image("action-black", styles.building)) ~ (f.acted.until(f.all(Farm).num + 2)./(_ => Image(f.style + "-action", styles.building)))) ~
                Div(Hint("Hamsters\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-hamster-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-hamster", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-hamster-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-hamster-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : InvasiveDDD =>
                FoxRabbitMouse./{ s =>
                    val p = 4 - f.pooled(PeacefulDDD(s))
                    val m = 4 - f.pooled(MilitantDDD(s))

                    Image("outcast-" + s + false.??("-hated"), styles.token) ~
                    0.until(4)./{ i =>
                        if (i < p)
                            Image(f.style + "-peaceful", styles.token)
                        else
                        if (i < p + m)
                            Image(f.style + "-militant", styles.token)
                        else
                            Image("empty-token" + f.latent(s)(i - p - m).is[MilitantDDD].??("-militant") + (i == 2).??("-card"), styles.token)
                    }
                }.join(Break) ~
                Gap ~
                (
                    (f.deck.num.formatted("%2d").hl.styled(styles.doubleFigures) ~ Image("deck-frog", styles.pile)).&.pointer.onClick.param("view-frog-deck", f)
                ).div(xstyles.smaller75) ~
                (game.current == f).?(Gap ~ 1.to(3)./(_ => Image("action-black", styles.building)).take(f.acted) ~ (1.to(3)./(_ => Image(f.style + "-action", styles.building))).drop(f.acted)) ~
                Div(Hint("Frogs\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-frog-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-frog", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-frog-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-frog-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : InvasiveCCC =>
                FoxRabbitMouse./{ s =>
                    val p = 4 - f.pooled(PeacefulCCC(s))
                    val m = 4 - f.pooled(MilitantCCC(s))

                    Image("outcast-" + s + false.??("-hated"), styles.token) ~
                    0.until(4)./{ i =>
                        if (i < p)
                            Image(f.style + "-peaceful", styles.token)
                        else
                        if (i < p + m)
                            Image(f.style + "-militant", styles.token)
                        else
                            Image("empty-token" + f.latent(s)(i - p - m).is[MilitantCCC].??("-militant") + (i == 2).??("-card"), styles.token)
                    }
                }.join(Break) ~
                Gap ~
                (
                    (f.deck.num.formatted("%2d").hl.styled(styles.doubleFigures) ~ Image("deck-frog", styles.pile)).&.pointer.onClick.param("view-frog-deck", f) ~
                    (f.pile.num.formatted("%2d").hl.styled(styles.doubleFigures) ~ Image("pile-" + f.pile.any.?(f.pile.last.suit).|("empty"), styles.pile)).&.pointer.onClick.param("view-frog-discard", f)
                ).div(xstyles.smaller75) ~
                (game.current == f).?(Gap ~ 1.to(3)./(_ => Image("action-black", styles.building)).take(f.acted) ~ (1.to(3)./(_ => Image(f.style + "-action", styles.building))).drop(f.acted)) ~
                Div(Hint("Frogs\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-frog-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-frog", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-frog-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-frog-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : InvasiveBBB =>
                FoxRabbitMouse./{ s =>
                    val p = 4 - f.pooled(PeacefulBBB(s))
                    val m = 4 - f.pooled(MilitantBBB(s))

                    Image("outcast-" + s + false.??("-hated"), styles.token) ~
                    p.times(Image(f.style + "-peaceful", styles.token)) ~
                    m.times(Image(f.style + "-militant", styles.token)) ~
                    (Image("empty-token", styles.token) :: Image("empty-token-1", styles.token) :: Image("empty-token-2-card", styles.token) :: Image("empty-token-3", styles.token)).drop(p + m)

                }.join(Break) ~
                Gap ~
                (
                    (f.deck.num.formatted("%2d").hl.styled(styles.doubleFigures) ~ Image("deck-frog", styles.pile)).&.pointer.onClick.param("view-frog-deck", f) ~
                    (f.pile.num.formatted("%2d").hl.styled(styles.doubleFigures) ~ Image("pile-" + f.pile.any.?(f.pile.last.suit).|("empty"), styles.pile)).&.pointer.onClick.param("view-frog-discard", f)
                ).div(xstyles.smaller75) ~
                (game.current == f).?(Gap ~ 1.to(3 + f.extra)./(_ => Image("action-black", styles.building)).take(f.acted) ~ (1.to(3)./(_ => Image(f.style + "-action", styles.building)) ++ (0.until(f.extra)./(_ => Image("action-bird", styles.building)))).drop(f.acted)) ~
                Div(Hint("Frogs\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-frog-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-frog", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-frog-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-frog-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : InvasiveAAA =>
                FoxRabbitMouse./{ s =>
                    val p = 4 - f.pooled(PeacefulAAA(s))
                    val m = 4 - f.pooled(MilitantAAA(s))

                    Image("outcast-" + s + false.??("-hated"), styles.token) ~
                    p.times(Image(f.style + "-peaceful", styles.token)) ~
                    m.times(Image(f.style + "-militant", styles.token)) ~
                    (Image("empty-token", styles.token) :: Image("empty-token-1", styles.token) :: Image("empty-token-2-card", styles.token) :: Image("empty-token-3", styles.token)).drop(p + m)

                }.join(Break) ~
                Gap ~
                (
                    (f.deck.num.formatted("%2d").hl.styled(styles.doubleFigures) ~ Image("deck-frog", styles.pile)).pointer.onClick.param("view-frog-deck", f) ~
                    (f.pile.num.formatted("%2d").hl.styled(styles.doubleFigures) ~ Image("pile-" + f.pile.any.?(f.pile.last.suit).|("empty"), styles.pile)).pointer.onClick.param("view-frog-discard", f)
                ).div(xstyles.smaller75) ~
                (game.current == f).?(Gap ~ 1.to(3 + f.extra)./(_ => Image("action-black", styles.building)).take(f.acted) ~ (1.to(3)./(_ => Image(f.style + "-action", styles.building)) ++ (0.until(f.extra)./(_ => Image("action-bird", styles.building)))).drop(f.acted)) ~
                Div(Hint("Frogs\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-frog-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-frog", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-frog-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-frog-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : LegalAAA =>
                val edicts = f.edicts./(e => (
                    (e.name.styled(f)).div(styles.effect).pointer.onClick.param(e) ~ (e == RouseLoyalists).??(f.loyalists./(d => dt.CardSuitInfo(d.suit).pointer.onClick.param(d)))
                )).merge

                f.all(ConvenedAAA).num.times(Image(f.style + "-convened", styles.token)) ~ f.all(AssemblyAAA).num.times(Image(f.style + "-assembly", styles.token)) ~ (f.pooled(AssemblyAAA) + f.pooled(ConvenedAAA) - 12).times(Image("empty-token", styles.token)) ~ Break ~ Gap ~
                f.all(CommuneAAA).num.times(Image(f.style + "-commune", styles.building)) ~ f.pooled(CommuneAAA).times(Image("empty-building", styles.building)) ~ Break ~ Gap ~
                edicts ~
                Div(Hint("Bats\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-bat-x5", styles.wr) *** (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-bat", styles.wr) *** (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-bat-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-bat-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline)

            case f : AbductAAA =>
                f.phases./(p => (Image(f.style + "-captain-" + p, styles.wr) ~ " " ~ f.characters(p).name.hl).div(styles.minister) ~ (f.items(p)./(i => Image(i.imgid, styles.ii)) ++ f.incoming(p)./(i => Image(i.exhaust.imgid, styles.ii))).merge.div(xstyles.smaller85)) ~
                Div(Hint("Skunks\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-skunk-x5", styles.wr) *** (f.allr(f.warrior).num / 5)) ~ (Image(f.style + "-skunk", styles.wr) *** (f.allr(f.warrior).num % 5))) ~
                &((Image(f.style + "-skunk-empty", styles.wr) *** (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-skunk-x5-empty", styles.wr) *** (f.pooled(f.warrior) / 5)))), styles.warline) ~
                Gap ~ Gap ~ f.bag./(i => Image(i.exhaust.imgid, styles.ii)).merge.div(xstyles.smaller75)

            case f : Hero =>
                val track = f.inv.intersect(Item.track).sort./(_.item)
                val satchel = f.inv.diff(Item.track).sort
                val columns = 3 + track.count(Bag)
                val (row1, row2) = satchel.splitAt((satchel.num + 1) / 2)

                Gap ~
                f.quests.some./(_./(s => Image("quest-" + s.name, styles.token)).merge).|("~~~") ~ Break ~
                game.candidates./~(o => f.attitude.get(o)).but(Indifferent).any.?(Div(game.candidates./~(o => f.attitude.get(o).but(Indifferent)./(a => &(Image((o.style + "-" + o.asInstanceOf[WarriorFaction].warrior.id), styles.fund) ~ (a match {
                    case Hostile => Image("attitude-hostile", styles.attitude)
                    case Amiable => Image("attitude-heart-full", styles.attitude) ~ (Image("attitude-heart-empty", styles.attitude) *** f.aid.get(o).|(0))
                    case Friendly => (Image("attitude-heart-full", styles.attitude) *** 2) ~ (Image("attitude-heart-empty", styles.attitude) *** f.aid.get(o).|(0))
                    case Allied => (Image("attitude-heart-full", styles.attitude) *** 3) ~ Empty
                })))).join(Image("card-separator", styles.cardbackinfo)), styles.warline)).|("~~~" ~ Break) ~
                Gap ~
                Div(
                    track.but(Bag).any.?(Div(track.but(Bag)./(i => Image(i.imgid, styles.ii)).merge)) ~
                    Div((Image("satchel-top", styles.ii) *** columns).merge, styles.satchel) ~
                    Div(row1./(i => Image(i.imgid, styles.ii)) ~ (Image("item-x-placeholder", styles.ii) *** (columns - row1.num))) ~
                    Div(row2./(i => Image(i.imgid, styles.ii)) ~ (Image("item-x-placeholder", styles.ii) *** (columns - row2.num))) ~
                    Div((Image("satchel-bottom", styles.ii) *** columns).merge, styles.satchel) ~
                    track.%(_ == Bag).any.?(Div((Image("item-x-spacer", styles.ii) *** 3) ~ track.%(_ == Bag)./(i => Image(i.imgid, styles.ii))))
                , xstyles.smaller85)
        }

        val s = Gap ~ title ~ vphand ~ fs.div(styles.warline) ~ effects ~ trade

        container.replace(s.div(xstyles.fillHeight).pointer.onClick.param(f), nameres, {
            case List(f, "fun-name") =>
                currentGame.ui.funNames = currentGame.ui.funNames.not
                updateStatus()

            case List(f, x) => onClick(x)
            case f : Faction => onFactionStatus(f, false)
            case x => onClick(x)
        })

        if (f == game.current)
            container.attach.parent.style.outline = "0.2ch solid #aaaaaa"
        else
        if (game.highlightFaction.has(f))
            container.attach.parent.style.outline = "0.2ch dashed #aaaaaa"
        else
            container.attach.parent.style.outline = ""
    }

    case class CardPeekByDesc(self : F) extends UndoDescriptor {
        def elem(count : Int) = "card peek".s(count).spn(xstyles.error) ~ " by " ~ factionElem(self)
    }

    override def describeActionForUndo(a : ExternalAction, self : |[F]) : $[UndoDescriptor] = $(
        a.unwrap @@ {
            case CodebreakersAction(f, _, _) => CardPeekByDesc(f)
            case a : ForcedAction =>
                a.productIterator.$.of[F] @@ {
                    case f :: _ if self.has(f).not => ActionByDesc(f)
                    case _ => NoDesc
                }
            case a => NoDesc
        }
    ).but(NoDesc).some.|(super.describeActionForUndo(a, self))

    def overlayScrollX(e : Elem) = overlayScroll(e)(styles.seeThroughInner).onClick
    def overlayFitX(e : Elem) = overlayFit(e)(styles.seeThroughInner).onClick

    def showOverlay(e : Elem, onClick : Any => Unit) {
        overlayPane.vis()

        overlayPane.replace(e, resources, onClick)
    }

    def tokenLine(id : Int => String, count : Int, rest : Int, empty : Int => String = _ => "empty-token") =
        0.until(count + rest)./(n => Image((n < count).?(id(n)).|(empty(n)))(styles.token3x)).merge.div

    def buildingLine(id : Int => String, count : Int, rest : Int, empty : Int => String = _ => "empty-building") =
        0.until(count + rest)./(n => Image((n < count).?(id(n)).|(empty(n)))(styles.building3x)).merge.div

    def itemLine(id : Int => String, count : Int, total : Int, empty : Int => String = _ => "item-x-placeholder") =
        0.until(total)./(n => Image((n < count).?(id(n)).|(empty(n)))(styles.iii)).merge.div

    def warriorLine(id : String, count : Int, rest : Int) = helper.&(
        helper.&((Image(id + "-x5"   , styles.wr3x) *** (count / 5)) ~ (Image(id              , styles.wr3x) *** (count % 5))) ~
        helper.&((Image(id + "-empty", styles.wr3x) *** (rest  % 5)) ~ (Image(id + "-x5-empty", styles.wr3x) *** (rest  / 5)))
    )

    def warriorLineExtra(id : String, al : String, count : Int, extra : Int, rest : Int) = helper.&(
        helper.&((Image(id + "-x5"   , styles.wr3x) *** (count / 5)) ~ (Image(id              , styles.wr3x) *** (count % 5))) ~
        helper.&((Image(al + "-x5"   , styles.wr3x) *** (extra / 5)) ~ (Image(al              , styles.wr3x) *** (extra % 5))) ~
        helper.&((Image(id + "-empty", styles.wr3x) *** (rest  % 5)) ~ (Image(id + "-x5-empty", styles.wr3x) *** (rest  / 5)))
    )

    def warriorLineNoGroup(id : String, count : Int, rest : Int) = helper.&(
        helper.&(Image(id, styles.wr3x) *** count) ~ helper.&(Image(id + "-empty", styles.wr3x) *** rest)
    )

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
            case f : Underground if chapter.has("ministers") =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Ministers".hl.larger.larger) ~
                    f.ministers./{ minister =>
                        val swayed = f.swayed.has(minister)
                        val lost = swayed.not && (f.swayed ++ f.retired).%(_.rank == minister.rank).num >= 3
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap ~
                        desc() ~
                        desc(minister.name.styled(f).larger ~ swayed.?(FigureSpace ~ Image(f.style + "-crown-" + minister.rank, styles.crown)) ~ lost.?(FigureSpace ~ Image(f.style + "-crown-empty", styles.crown))) ~
                        (swayed.not && lost.not).?(desc(minister.rank.times(dt.CardBackInfo).merge, dt.Arrow, (minister.rank - 1).vp)) ~
                        HGap ~
                        Image("info:minister-" + minister.short)(styles.card) ~
                        HGap ~
                        desc(minister.long(f)) ~
                        HGap ~
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
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, "moods") => onFactionStatus(f, false, Some("moods"))
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : Underground if chapter.none =>
                val sway = min(clearings.%(f.present).num, f.hand.num)
                val potential = f.ministers.diff(f.swayed).%(_.rank <= sway).%(m => (f.swayed ++ f.retired).%(_.rank == m.rank).num < 3)

                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Recruit".styled(styles.phase), dt.Arrow, "Assembly".styled(styles.phase), dt.Arrow, "Parliament".styled(styles.phase), dt.Arrow, "Sway".styled(styles.phase), dt.Arrow, "Return Revealed".styled(styles.phase), dt.Arrow, "Craft".styled(styles.phase)) ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    more("Assembly".hl.larger) ~
                    more("Two times", "move".hh, Comma, "", "battle".hh, Comma, "", "build".hh, Comma, "", "recruit".hh, "or", "dig".hh, Dot) ~
                    moreGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Tunnels".hl.larger) ~
                    more("Tunnel".hl.larger, "is a", "token".hh, Comma, "it", "connects".hh, "the", Burrow(f), "to the clearing it is in.") ~
                    more("Can be", "dug".hh, "in any clearing discarding a matching", "card".hh, "during", "Assembly".f, Dot) ~
                    more("If all", Tunnel.sof(f), "are on the map, an old one can be rerouted when digging.") ~
                    tokenLine(_ => f.style + "-tunnel", f.all(Tunnel).num, f.pooled(Tunnel)) ~
                    f.all(Tunnel).some./(l => desc("Connect" ~ (f.all(Tunnel).num == 1).??("s"), Burrow(f), "to", l./(_.elem).commaAnd)) ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    more("Parlament".hl.larger) ~
                    more("Take an action of each", "minister".hh, "in any order.") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Sway".hl.larger) ~
                    more("Can sway one", "Minister".f, "per turn,") ~
                    more("revealing", "cards".hh, "up to his", "rank".hh, Comma) ~
                    more("each card matching a different", "clearing".hh, "where present.") ~
                    more("Gain", "1".styled(styles.vp) ~ "/" ~ "2".styled(styles.vp) ~ "/" ~ "3".styled(styles.vp) ~ " " ~ "vp".styled(styles.vp) ~ " and a " ~ "Crown".hh ~ " swaying.") ~
                    clearings.exists(f.present).?(
                        moreGap ~
                        moreGap ~
                        moreGap ~
                        moreGap ~
                        desc("Presence in", clearings.%(f.present)./(_.elem).comma, f.hand.any.$("and", f.hand.num.hl, "card".s(f.hand.num), "in hand", potential.any.?(Comma).|(Dot))) ~
                        potential.any.?(desc("might".hh, "be able to sway",
                            $(
                                (potential.exists(_.rank == 2)).?("a " ~ "Squire".f),
                                (potential.exists(_.rank == 3)).?("a " ~ "Noble".f),
                                (potential.exists(_.rank == 4)).?("a " ~ "Lord".f)
                            ).flatten.commaOr, Dot
                        ))
                    ) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Ministers".hl.larger) ~
                    $(2, 3, 4).%(r => f.swayed.exists(_.rank == r))./(r =>
                        desc(
                            isMore.?(r @@ {
                                case 2 => "Squires:".hh
                                case 3 => "Nobles:".hh
                                case 4 => "Lords:".hh
                            }),
                            f.swayed.%(_.rank == r)./(m => m.of(f).larger ~ FigureSpace ~ Image(f.style + "-crown-" + m.rank, styles.crown)).join(FigureSpace ~ FigureSpace ~ FigureSpace)
                        )
                    ) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Crowns".hl.larger) ~
                    desc(f.ministers.diff(f.swayed).diff(f.retired)./(m => Image(f.style + "-crown-" + m.rank, styles.crown)).merge) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    more("Buildings".hl.larger) ~
                    more("Can build in a", "ruled".hh, "clearing,") ~
                    more("revealing".f, "a", "matching".hh, "card during", "Assembly".f, Comma) ~
                    more("or", "any".hh, "card with", Foremole.of(f), Dot) ~
                    more("Both", Citadel.sof(f), "and", Market.sof(f), "allow", "crafting".hh, Dot) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Citadels".hl.larger) ~
                    more("Citadel".hl.larger, "is a", "building".hh, Comma, "it increases", "recruiting".hh, Dot) ~
                    buildingLine(_ => f.style + "-citadel", f.all(Citadel).num, f.pooled(Citadel), i => "empty-building-mole-" + (i > 0).?(2).|(1)) ~
                    f.all(Citadel).use { l =>
                        val r = $(1, 2, 4, 6)(l.num)
                        less("Recruit", r.hlb.larger, "Mole".s(r).f,                                          l.any.?(Comma), l.any.?((           "craft", l./(_.asset)./(dt.CraftSuit).merge))) ~
                        more("Recruit", r.hlb.larger, "Mole".s(r).f, "in", Burrow(f), "at the start of turn", l.any.?(Comma), l.any.?(("provide", "craft", l./(_.asset)./(dt.CraftSuit).merge)))
                    } ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Markets".hl.larger) ~
                    more("Market".hl.larger, "is a", "building".hh, Comma, "it provides additional", "card".hh, "draw.") ~
                    buildingLine(_ => f.style + "-market", f.all(Market).num, f.pooled(Market), _ => "empty-building-card") ~
                    f.all(Market).use { l =>
                        val r = 1 + l.num
                        less("Draw", r.hlb.larger, "card".s(r).hh,                       l.any.?(Comma), l.any.?((           "craft", l./(_.asset)./(dt.CraftSuit).merge))) ~
                        more("Draw", r.hlb.larger, "card".s(r).hh, "at the end of turn", l.any.?(Comma), l.any.?(("provide", "craft", l./(_.asset)./(dt.CraftSuit).merge)))
                    } ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    more("The Price of Failure".hl.larger) ~
                    more("If", "any".hh, "number of", Citadel.sof(f), "or", Market.sof(f), "are removed,") ~
                    more("the highest ranking", "Minister".f, "goes away,") ~
                    more("and a", "random card".hh, "is discarded.") ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    moreGap ~
                    more("Revealed Cards".hl.larger) ~
                    more("All revealed", "Bird".styled(Bird), "cards are discarded,") ~
                    more("the rest goes back to the hand.") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Moles".hl.larger) ~
                    more("Moles".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.at(f.burrow).num.hl.larger, "in", Burrow(f), Comma, f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLineExtra(f.style + "-mole", f.style + "-young", f.all(f.warrior).num, f.at(f.burrow).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                    HGap ~
                    ("Ministers".styled(f)).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(f, "ministers") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, "ministers") => onFactionStatus(f, false, Some("ministers"))
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })


            case f : Horde if chapter.none =>
                showOverlay(overlayScrollX((

                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Raze".styled(styles.phase), dt.Arrow, "Recruit".styled(styles.phase), f.region.none.$(dt.Arrow, "Anoint".styled(styles.phase)), dt.Arrow, "Mood".styled(styles.phase), dt.Arrow, "Craft".styled(styles.phase), dt.Arrow, "Command".styled(styles.phase), dt.Arrow, "Advance".styled(styles.phase), dt.Arrow, "Incite".styled(styles.phase), dt.Arrow, "Oppress".styled(styles.phase)) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Oppress".hl.larger) ~
                    more("Oppress".hl.larger, "scores", "clearings".hh, " where no other faction is present.") ~
                    more(1.hl, "or", 2.hl, "clearings", Dash, 1.vp, Semicolon, 3.hl, "or", 4.hl, Dash, 2.vp, Semicolon, "if", 5.hl, Dash, 3.vp, Semicolon, 6.hl, "or more", Dash, 4.vp) ~
                    clearings.%(f.present).%(f.rules).%(c => f.enemies.forall(_.present(c).not)).use(l => desc("Oppressing".f, l.any.?(l.num.hlb.larger).|("no".spn), "clearing".s(l.num), l.any.?((l.num > 1).?(Colon).|(Comma)), l./(_.elem).commaAnd)) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("The".spn.larger, "Warlord".hl.larger) ~
                    more("The".spn.larger, "Warlord".hl.larger, "is a", "warrior".hh, Semicolon, "if killed in", "battle".hh, Comma, "a new one is annointed next turn") ~
                    more("If alive, each turn recruits as many", "Rats".f, "as his", "Prowess".f, Dot) ~
                    Image(f.style + "-warlord" + f.region.none.??("-empty"), styles.building3x) ~
                    f.region./(r =>
                        if (f.canPlace(r))
                            desc("The", "Warlord".f, "recruits", f.prowess.hlb.larger, "Rat".s(f.prowess).f)
                        else
                            desc("The", "Warlord".f, "won't recruit because of the", f.cantPlaceReason(r))
                    ).||(f.all(Rat).distinct.%(f.canPlace).some./(l =>
                        desc("The", "Warlord".f, "will replace", (l.num > 1).?("any").|("a"), "Rat".f, "in", l./(_.elem).commaOr, "next turn")
                    )).|(
                        desc("The", "Warlord".f, "will reappear anywhere on the map next turn")
                    ) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    more("Mood".hl.larger, "provides a different bonus to the", "Warlord".f, "each turn.") ~
                    more("Items".hh, "in the", "Hoard".f, "prevent certain", "Moods".f, Dot) ~
                    f.region.any.?(
                        less("Mood".hl.larger) ~
                        Image("info:mood-" + f.mood)(styles.illustration) ~
                        f.mood.name.styled(f).larger ~
                        desc(f.mood.long(f))
                    ) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Mob".hl.larger) ~
                    more("Mob".hl.larger, "is a", "token".hh, Comma, "it", "razes".f, "other", "tokens".hh, "and", "buildings".hh ~ ", and gets", "items".hh, "from", "ruins".hh, Dot) ~
                    more("Can be", "incited".f, "with a "~ "card".hh, Comma, "then it can spread by itself.") ~
                    tokenLine(_ => f.style + "-mob", f.all(Mob).num, f.pooled(Mob)) ~
                    f.all(Mob).some.%(_ => f.pool(Mob))./(l => clearings.diff(l).%(c => f.connectedFor(c).intersect(l).any).%(f.canPlace)).map { l =>
                        desc("Can spread to", l./(_.elem).commaOr)
                    } ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Strongholds".hl.larger) ~
                    more("Stronghold".hl.larger, "is a", "building".hh, Comma, "it recruits a", Rat.of(f), "each turn, allows", "crafting".hh, Dot) ~
                    more("Can be built with a", "card".hh, "in a", "ruled".hh, "clearing", Dot) ~
                    more("Items".hh, "can be", "crafted".hh, "either for", "victory points".styled(styles.vp), "or to put in the", "Hoard".f, Dot) ~
                    buildingLine(_ => f.style + "-stronghold", f.all(Stronghold).num, f.pooled(Stronghold)) ~
                    f.all(Stronghold).some.map(l =>
                        desc("Recruit", l.num.hlb.larger, "Rat".s(l.num).f, Comma, "craft", l./(_.asset)./(dt.CraftSuit).merge)
                    ) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("The".spn.larger, "Hoard".hl.larger) ~
                    more("The".spn.larger, "Hoard".hl.larger, "stores", "items".hh, "that increase the number of actions.") ~
                    less("Command".f.larger, Dash, f.command.hlb.larger, "action".s(f.command), "to", "Command the Hundreds".styled(styles.phase)) ~
                    more("Command".hl.larger, "grants", f.command.hlb.larger, "action".s(f.command), "each turn to", "build".hh, "or", "move".hh, "or", "battle".hh, Dot) ~
                    more(Bag.name.spn(styles.itemInfo), Comma, Boots.name.spn(styles.itemInfo), Comma, Coins.name.spn(styles.itemInfo), "further increase it.") ~
                    itemLine(n => "item-" + f.hoard.command(n).name, f.hoard.command.num, 4, n => "item-x-placeholder" + $("-2p", "-0p", "-3p", "-4p")(n)) ~
                    less("Prowess".f.larger, Dash, f.prowess.hlb.larger, "action".s(f.prowess), "to", "Advance the Warlord".styled(styles.phase)) ~
                    more("Prowess".hl.larger, "grants", f.prowess.hlb.larger, "action".s(f.prowess), "each turn to", "move".hh, "and then", "battle".hh, "with the", "Warlord".f) ~
                    more($(Sword, Hammer, Teapot, Crossbow)./(_.name.spn(styles.itemInfo)).comma, "further increase it.") ~
                    itemLine(n => "item-" + f.hoard.prowess(n).name, f.hoard.prowess.num, 4, n => "item-x-placeholder" + $("-2p", "-0p", "-3p", "-4p")(n)) ~
                    more("Apart from "~ "crafting", "with", "Strongholds".f, Comma, "items".hh, "can also be obtained") ~
                    more("by", "razing".f, "ruins".hh, "or", "looting".f, "other factions in", "battle".hh, Dot) ~
                    HGap ~
                    HGap ~
                    game.ruins.values./~(_.items).$.sortBy(i => Item.order.indexOf(i)).distinct.diff(f.fromRuins).some./(l =>
                        desc(l./(_.name.spn(styles.itemInfo)).comma, (l.num > 1 || l.diff($(Boots, Coins)).none).?("are").|("is"), "available in the", "ruins".hh, Dot)
                    ) ~
                    factions.but(f).%(_.forTrade.any)./(e => desc(e.forTrade./(_.item.name.spn(styles.itemInfo)).comma, "can be", "looted".f, "from", e.elem, Dot)).merge ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Rats".hl.larger) ~
                    more("Rats".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-rat", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                    HGap ~
                    ("Moods".styled(f)).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(f, "moods") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, "moods") => onFactionStatus(f, false, Some("moods"))
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : Horde if chapter.has("moods") =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Moods".hl.larger) ~
                    Mood.all./(mood =>
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap ~
                        desc() ~
                        desc((mood.name.styled(f).larger ~ mood.item.but(Torch)./(i => FigureSpace ~ Image("item-" + i.name + (f.hoard.command ++ f.hoard.prowess).has(i).??("-exhausted"), styles.ii))).div(xlo.flexhcenter)) ~
                        HGap ~
                        Image("info:mood-" + mood)(styles.illustration) ~
                        desc(mood.long(f)) ~
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap
                    ) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, "moods") => onFactionStatus(f, false, Some("moods"))
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : Expedition if chapter.none =>
                showOverlay(overlayScrollX((

                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Encamp".styled(styles.phase), dt.Arrow, "Decamp".styled(styles.phase), dt.Arrow, "Recruit".styled(styles.phase), dt.Arrow, "Craft".styled(styles.phase), dt.Arrow) ~
                    desc("Move".styled(styles.phase), dt.Arrow, "Battle".styled(styles.phase) ~ " then " ~ "Delve".styled(styles.phase), dt.Arrow, "Move".styled(styles.phase) ~ " or " ~ "Recover".styled(styles.phase)) ~
                    desc(dt.Arrow, "Live Off".styled(styles.phase), dt.Arrow, "Retinue".styled(styles.phase)) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Relics".hl.larger) ~
                    more("Relic".hl.larger, "is a", "token".hh, Comma, "one of the three types", Comma, Tablet.name.styled(f), Comma, Jewel.name.styled(f), "or", Idol.name.styled(f), Dot) ~
                    more("There are", "four".hh, "Relics".styled(f), "of each type", "valued", 1.styled(styles.vp), Comma, 2.styled(styles.vp), Comma, 3.styled(styles.vp), "and", 3.styled(styles.vp), Dot) ~
                    more("Recovering".styled(f), "each", "Relic".styled(f), "scores", "victory points".styled(styles.vp), "equal to its value.") ~
                    more("Each three", "Relics".styled(f), "of different", "types".hh, "additionally give", 2.vp, Dot) ~
                    more("Enemies score", 2.vp, "removing a", "Relic".styled(f), Comma, "and place it back into the forest", Dot) ~
                    Relic.types./ { rt =>
                        tokenLine(n => f.recovered(rt).$(n).imgid, f.recovered(rt).num, 4 - f.recovered(rt).num, _ => f.style + "-" + rt.name + "-empty")
                    } ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Waystations".hl.larger) ~
                    more("Waystation".hl.larger, "is a", "building".hh, Comma, "it allows", "recruiting", Badger.sof(f), "and", "recovering".hh, "Relics".styled(f), Dot) ~
                    more("Also provide", "crafting".hh, Comma, "and one extra", "card".hh, "draw each", Dot) ~
                    more("A", Badger.of(f), "can", "encamp".hh, "in a clearing with a free building slot to become a", "Waystation".styled(f), Dot) ~
                    more("A", "Waystation".styled(f), "can", "decamp".hh, "to become a", Badger.of(f), "again", Dot) ~


                    buildingLine(n => f.wst.%(w => f.all(w).any).apply(n).imgid(f), 6 - f.wst./(f.pooled).sum, f.wst./(f.pooled).sum - 3) ~
                    f.wst./~(w => f.all(w)).some.map(l =>
                        desc("Recruit", 2.hlb.larger, Badger.sof(f), "with", l./(_.cost).distinct./(dt.CardSuit).merge, Comma, "craft", l./(_.asset)./(dt.CraftSuit).merge)
                    ) ~
                    f.wst./~(w => f.all(w)).use { l =>
                        val r = 1 + l.num
                        less("Draw", r.hlb.larger, "card".s(r).hh) ~
                        more("Draw", r.hlb.larger, "card".s(r).hh, "at the end of turn.")
                    } ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    clearings.%(f.rules).use(l => desc("Ruling".f, l.none.?("no"), "clearing".s(l.num), l.any.?((l.num > 1).?(Colon).|(Empty)), l./(_.elem).commaAnd)) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Badgers".hl.larger) ~
                    more("Badgers".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    more("When", "moving", "each", Badger.of(f), "can take one", "Relic".styled(f), "with him", Dot) ~
                    more(Badger.of(f), "ignore the first", "hit".styled(styles.hit), "in battle if they have a ", "Relic".styled(f), "with them", Dot) ~
                    more("At the end of turn, if more than", "three".hh, Badger.sof(f), "are in a clearing, one", Badger.of(f), "is removed", Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-badger", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                    HGap ~
                    ("Retinue".styled(f)).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(f, "retinue") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, "retinue") => onFactionStatus(f, false, Some("retinue"))
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : Expedition if chapter.has("retinue") =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Retinue".hl.larger) ~
                    HGap ~
                    desc("Up to", 10.hh, "cards total, each card provides an", "action".hh, Dot) ~
                    desc("In the", Evening, "can either", "add".hh, "any number of", "cards".hh, "here", Comma) ~
                    desc("or", "shift".hh, "one card", "from one action to another", Dot) ~
                    Retinue.all.%(d => f.retinue(d).any || f.complete(d).any)./(d =>
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap ~
                        desc(d) ~
                        HGap ~
                        d.@@ {
                            case RetinueMove =>
                                desc("Move".hh, "from a matching", "clearing".hh, Dot)
                            case BattleThenDelve =>
                                desc("Battle".hh, "in a matching", "clearing".hh, Dot) ~
                                desc("If", Badger.sof(f), "rule".hh, "the clearing after battle", Comma) ~
                                desc("or if there were no enemies", Comma) ~
                                desc("then they can", "delve".hh, "to take a", "Relic".styled(f), "from an adjacent forest", Dot) ~
                                desc("If the", "value".styled(styles.vp), "of the", "Relic".styled(f), "is more") ~
                                desc("than the number of", "ruled".hh, "clearings around the forest", Comma) ~
                                desc("the card is", "discarded".hh, "thereafter", Dot)
                            case MoveOrRecover => desc(
                                desc("Either", "Move".hh, "from a matching", "clearing".hh, Comma, "or") ~
                                desc("start", "recovering".hh, "Relics".styled(f), "from a matching", "clearing".hh, Dot) ~
                                desc("To", "recover".hh, "a", "Relic".styled(f), Comma, "a", "Waystation".styled(f), "of the matching", "type".hh, "is needed in the same clearing", Dot) ~
                                desc("Can", "recover".hh, "multiple", "Relics".styled(f), "with one", "card".hh, "if the number of", "ruled".hh, "clearings of the matching", "suit".hh, "at least equals the value of all", "Relics".styled(f), "but the last", Dot) ~
                                desc("If the value of the last", "Relic".styled(f), "exceeded that number", Comma, "the card is", "discarded".hh, "thereafter", Dot)
                            )
                        } ~
                        HGap ~
                        (f.complete(d).get ++ f.retinue(d).get)./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge ~
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap ~
                        HGap
                    ) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap

                ).div(xlo.flexvcenter)), {
                    case (f : Faction, "moods") => onFactionStatus(f, false, Some("moods"))
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : Caster if chapter.none =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~

                    Image("xc-board")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Frogs".hl.larger) ~
                    more("Frogs".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-frog", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                    HGap ~
                    ("Spells".styled(f)).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(f, "spells") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, "spells") => onFactionStatus(f, false, Some("spells"))
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : Caster if chapter.has("spells") =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Spells".hl.larger) ~
                    HGap ~
                    f.spellbooks./(s => OnClick(s, Div(s.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined))) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap

                ).div(xlo.flexvcenter)), {
                    case x => onClick(x)
                })

            case f : Utopia  =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc(("Bribery".hl ~ " is partially implemented" ~ " as a moral compromise on the part of the developer.").styled(xstyles.warning)) ~
                    Image(f @@ {
                        case CU => "cu-board-v1"
                        case CUv2 => "cu-board-v3"
                    } : String)(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Foxies".hl.larger) ~
                    more("Foxies".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-foxy", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : Farmer =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~

                    Image("fh-board-v2")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Hamsters".hl.larger) ~
                    more("Hamsters".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-hamster", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : OldExpedition =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("ok-board")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Badgers".hl.larger) ~
                    more("Badgers".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-badger", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : InvasiveDDD if chapter.has("deck") =>
                showOverlay(overlayScrollX((Frog.elem ~ " Cards").hl.div ~
                    Deck.frogDDD./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

            case f : InvasiveDDD =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.elem.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("ld-board-d")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Frogs".hl.larger) ~
                    more("Frogs".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-frog", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                    HGap ~
                    ("Frog Deck".styled(f)).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(f, "deck") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, "deck") => onFactionStatus(f, false, Some("deck"))
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : InvasiveCCC if chapter.has("deck") =>
                showOverlay(overlayScrollX((Frog.elem ~ " Cards").hl.div ~
                    Deck.frogCCC./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

            case f : InvasiveCCC =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.elem.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("ld-board-c")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Frogs".hl.larger) ~
                    more("Frogs".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-frog", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                    HGap ~
                    ("Frog Deck".styled(f)).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(f, "deck") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, "deck") => onFactionStatus(f, false, Some("deck"))
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : InvasiveBBB =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.elem.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("ld-board")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Frogs".hl.larger) ~
                    more("Frogs".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-frog", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : InvasiveAAA =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.elem.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("td-board")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Frogs".hl.larger) ~
                    more("Frogs".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-frog", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : LegalAAA if chapter.has("deck") =>
                showOverlay(overlayScrollX(("Edicts").styled(f).div ~
                    EdictsAAA.deck./{ d => OnClick(d, Div(Image(d.id, styles.card), xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

            case f : LegalAAA =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.elem.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("tc-board-a")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("tc-board-a-back")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Bats".hl.larger) ~
                    more("Bats".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-bat", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    ("Edicts".styled(f)).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(f, "deck") ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Faction, "deck") => onFactionStatus(f, false, Some("deck"))
                    case (f : Faction, more : Boolean) => onFactionStatus(f, more, None)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })


            case f : AbductAAA =>
                showOverlay(overlayScrollX((
                    HGap ~
                    HGap ~
                    HGap ~
                    f.elem.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    Image("kd-board-a")(styles.factionboard) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Skunks".hl.larger) ~
                    more("Skunks".hl.larger, "are", "warriors".hh, Comma, "they", "battle".hh, "and provide", "rule".hh, Dot) ~
                    desc(f.allr(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    warriorLine(f.style + "-skunk", f.allr(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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

    def onHirelingStatus(implicit faction : Hireling, isMore : Boolean) : Unit = {
        def desc(l : Any*) = game.desc(l : _*).div
        def more(l : Any*) = isMore.?(desc(l : _*))
        def less(l : Any*) = isMore.not.?(desc(l : _*))
        def moreGap = isMore.?(HGap)
        def lessGap = isMore.not.?(HGap)

        def info() =
            less(("More Info".hh).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(faction, !isMore)) ~
            more(("Less Info".hh).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(new CustomStyle(rules.width("60ex"))(new StylePrefix("test")){}).pointer.onClick.param(faction, !isMore))

        faction @@ {
            case f : StreetBand.type =>
                showOverlay(overlayScrollX((
                    (Image("info:street-band-1")(styles.illustration) ~ Image("info:street-band-2")(styles.illustration)).div ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Enchant".hl.larger) ~
                    more("Enemy factions can't leave the", "clearing".hh, "with a ", Weasel.of(f), "on the same turn they", "entered".hh, "it") ~
                    more("They can leave the clearing freely", "before".hh, "entering") ~
                    more("Killing all", Weasel.sof(f), "in a clearing ends the spell there") ~
                    f.enchanted.some./(l => desc("Enchanted clearing".s(l.num), (l.num > 1).?(Colon).|(Dash), l./(_.elem).commaAnd)) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc(Daylight) ~
                    desc("Place a", Weasel.of(f), "in any clearing") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Weasels".hl.larger) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    warriorLineNoGroup(f.style + "-weasel", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Hireling, more : Boolean) => onHirelingStatus(f, more)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : BanditGangs.type =>
                showOverlay(overlayScrollX((
                    (Image("info:bandit-gangs-1")(styles.illustration) ~ Image("info:bandit-gangs-2")(styles.illustration)).div ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc(Daylight) ~
                    less("Recruit own", "warriors".hh, "at each", f.warrior.of(f)) ~
                    more("Recruit controller's faction", "warriors".hh) ~
                    more("in each clearing where", f, "are present,") ~
                    more("one for each", f.warrior.of(f)) ~
                    desc("~".hl, "or", "~".hl) ~
                    less("Place", f.warrior.of(f), "with their controller") ~
                    more("Place", f.warrior.of(f), "anywhere where controllers's faction is present") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Quills".hl.larger) ~
                    more(f, "can't be battled if their controller is present") ~
                    f.presence.intersect(f.owner.?(_.presence)).some./(l => desc("Can't be battled in", l./(_.elem).commaAnd)) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Porcupines".hl.larger) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    warriorLineNoGroup(f.style + "-porcupine", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Hireling, more : Boolean) => onHirelingStatus(f, more)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : MoleArtisians.type =>
                showOverlay(overlayScrollX((
                    Image("info:mole-artisians")(styles.illustration) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Crafted", "item cards".hh, "return to the hand at the end of turn") ~
                    more("The", "controller".hh, "can", "reveal".hh, "an item card when", "crafting".hh, Comma, "instead of", "discarding".hh, "it.") ~
                    more("At the end of", Evening, "all cards revealed this way go back to the hand.") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    more("Works", "multiple".hh, "times per turn.") ~
                    more("The", "controller".hh, "can end their turn with six or more cards.") ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Hireling, more : Boolean) => onHirelingStatus(f, more)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : RatSmugglers.type =>
                showOverlay(overlayScrollX((
                    Image("info:rat-smugglers")(styles.illustration) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc(Daylight) ~
                    desc("The", "controller".hh, "can discard a", "card".hh, "with an", "item".hh, "on it to", "move".hh, "or", "battle".hh) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    more("Works", "multiple".hh, "times per turn") ~
                    more("It is", "not".hh, "necessary to be able to", "craft".hh, "the card") ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Hireling, more : Boolean) => onHirelingStatus(f, more)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : StoicProtector.type =>
                showOverlay(overlayScrollX((

                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Prevents", "battle".hh, "or", "removal".hh) ~
                    more("Where", Deer.of(f), "is present, enemies cannot battle or remove", "controller's".hh, "faction pieces.") ~
                    more("If enemies deal", "hits".styled(styles.hit), "defending in", "battle".hh, Comma, "those are negated.") ~
                    more(Deer.of(f), "itself cannot be battled or removed too.") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Start".hl, "of", Birdsong) ~
                    less("Move", Deer.of(f), "ignoring rule") ~
                    more("Move", Deer.of(f), "to any adjanced clearing regardless of", "rule".hh, Dot) ~
                    more(Snare.of(CC), "prevents movement", Dot) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    more("When hired for the first time,", Deer.of(f), "is placeed in any clearing with owner's faction pieces.") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Deer".hl.larger) ~
                    Image(f.style + "-deer", styles.wr3x) ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Hireling, more : Boolean) => onHirelingStatus(f, more)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : ForestPatrol.type =>
                showOverlay(overlayScrollX((
                    Image("info:forest-patrol")(styles.illustration) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc(Daylight) ~
                    desc(StrayCat.sof(f), "can", "move".hh, "and then can", "battle".hh, "anywhere") ~
                    desc("~".hl, "or", "~".hl) ~
                    desc("Place all", StrayCat.sof(f), "from the", "hospital".hh, "to any clearing with", StrayCat.of(f)) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~

                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Patrols".hl.larger) ~
                    desc(clearings./~(f.at).num.hl.larger, "on the map,", f.hospital.num.hl.larger, "in the", "hospital".hh) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    warriorLineNoGroup(f.style + "-stray-cat", clearings./~(f.at).num, f.hospital.num) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    more("Whenever any number of", StrayCat.sof(f), "are removed,", "one".hh, "of them goes to the", "hospital".hh) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Hireling, more : Boolean) => onHirelingStatus(f, more)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : LastDynasty.type =>
                showOverlay(overlayScrollX((
                    Image("info:last-dynasty")(styles.illustration) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc(Daylight) ~
                    more("If", f, "is not present on the map, place all", f.warrior.sof(f)) ~
                    more("in a clearing on the map edge and", "battle".hh) ~
                    less("Respawn all", f.warrior.sof(f), "if none and ", "battle".hh) ~
                    desc("~".hl, "or else", "~".hl) ~
                    more("If", f, "rules".hh, "its clearing,", "move".hh, "all", Vulture.sof(f), "and then", "battle".hh) ~
                    less("If", "rule".hh, "then", "move".hh, "all", "and", "battle".hh) ~
                    desc("~".hl, "or else", "~".hl) ~
                    more("Battle".hh, "and then", "battle".hh, "again if able") ~
                    less("Battle".hh, "twice") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    more("On Setup".hl.larger) ~
                    more("Place all", f.warrior.sof(f), "in a clearing on the map edge") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Vultures".hl.larger) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    warriorLineNoGroup(f.style + "-vulture", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Hireling, more : Boolean) => onHirelingStatus(f, more)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : SpringUprising.type =>
                showOverlay(overlayScrollX((
                    Image("info:spring-uprising")(styles.illustration) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Start".hl, "of", Birdsong) ~
                    desc("Roll between", Fox, ", ", Rabbit, "and", Mouse) ~
                    desc("In a matching", "clearing".hh) ~
                    desc("Place", Animal.of(f)) ~
                    desc("~".hl, "or", "~".hl) ~
                    desc("Remove", Animal.of(f), "and all enemy pieces") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Insurgent".hl.larger) ~
                    more("To battle", f, "attacker", "must".hh, "first discard a", "card".hh, "matching their", "clearing") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    more("On Take".hl.larger) ~
                    more("If", f, "is not present on the map,") ~
                    more("roll between", Fox, ", ", Rabbit, "and", Mouse, "and place", Animal.of(f), "in a matching clearing") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    more("On Setup".hl.larger) ~
                    more("Roll between", Fox, ", ", Rabbit, "and", Mouse, " twice and place", Animal.sof(f), "in a matching clearings") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Animals".hl.larger) ~
                    desc(f.all(f.warrior).num.hl.larger, "on the map,", f.pooled(f.warrior).hl.larger, "in reserve") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    warriorLineNoGroup(f.style + "-animal", f.all(f.warrior).num, f.pooled(f.warrior)) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Hireling, more : Boolean) => onHirelingStatus(f, more)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : TheExile.type =>
                showOverlay(overlayScrollX((
                    Image("info:the-exile")(styles.illustration) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc(Daylight) ~
                    less("Exhaust", 1.hl, "item to", "move".hh, Bear.of(f)) ~
                    less("Exhaust", 2.hl, "items to", "battle".hh, "with", Bear.of(f)) ~
                    more("Exhaust", 1.hl, "item to", "move".hh, Bear.of(f), "to an adjanced forest.") ~
                    more("Exhaust", 2.hl, "items to", "battle".hh, "with", Bear.of(f), "in an adjanced clearing.") ~
                    more("Deals", "hits".styled(styles.hit), "up to the number of", "items".hh, Dot) ~
                    more("Taken", "hits".styled(styles.hit), "exhaust".hh, "items, or", "remove".hh, "items if all are exhausted.") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    less("Give items to score", 1.vp, "and draw a", "card".hh) ~
                    more("All players on their turn can give a crafted", "item".hh, "to score", 1.vp, "and draw a", "card".hh, Dot) ~
                    more("Requires a faction piece in a clearing adjanced to the forest with the", Bear.of(f), Dot) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Items".hl.larger) ~
                    itemLine(n => "item-" + (n < f.exhausted.num).?(f.exhausted(n).name + "-exhausted").|(f.ready(n - f.exhausted.num).name), f.exhausted.num + f.ready.num, f.exhausted.num + f.ready.num) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Bear".hl.larger) ~
                    Image(f.style + "-bear", styles.wr3x) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Hireling, more : Boolean) => onHirelingStatus(f, more)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f : RiverfolkFlotilla.type =>
                showOverlay(overlayScrollX((
                    Image("info:flotilla")(styles.illustration) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    f.name.f.larger.larger ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc(Birdsong) ~
                    less("All faction present draw a card") ~
                    less(Flotilla.of(f), "moves along the river, and then may battle") ~
                    more("Each player with factions pieces at the", Flotilla.of(f), "may draw a card.") ~
                    more(Flotilla.of(f), "must move along the river, ignoring rule.") ~
                    more("Then ", Flotilla.of(f), "may battle, dealing up to", 3.hits, Dot) ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    more(Flotilla.of(f), "is a pawn, it cannot be battled or removed.") ~
                    less(Flotilla.of(f), "cannot be battled or removed") ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    HGap ~
                    desc("Flotilla".hl.larger) ~
                    Image(f.style + "-flotilla", styles.wr3x) ~
                    HGap ~
                    HGap ~
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
                ).div(xlo.flexvcenter)), {
                    case (f : Hireling, more : Boolean) => onHirelingStatus(f, more)
                    case _ =>
                        overlayPane.invis()
                        overlayPane.clear()
                })

            case f =>
                println("no hireling status overlay for " + f)
        }
    }

    def onClickMore(f : Any) = f @@ {
        case f : Faction => println("onClickMore: " + f)

        case Nil =>
            overlayPane.invis()
            overlayPane.clear()
    }

    override def onClick(a : Any) = a @@ {
        case _ if { seen = game.notifications ; false } =>

        case ("notifications", Some(f : Faction)) =>
            seen = $
            showNotifications($(f))

        case ("notifications", Some(f : PlayerN)) =>
            seen = $
            showNotifications(currentGame.ptf.get(f).$)

        case ("notifications", None) =>
            seen = $
            showNotifications(factions)

        case ("view-scorelog", f : Faction) =>
            showOverlay(overlayScrollX((f.name.styled(f) ~ " scored").div ~
                f.scorelog./(_.div(xstyles.info)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(styles.extrachp)).merge), onClick)

        case "view-discard" =>
            showOverlay(overlayScrollX("Discard Pile".hl.div ~
                game.pile./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case ("view-frog-discard", f : InvasiveCCC) =>
            showOverlay(overlayScrollX((Frog.elem ~ " Discard Pile").hl.div ~
                f.pile./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case ("view-frog-discard", f : InvasiveBBB) =>
            showOverlay(overlayScrollX((Frog.elem ~ " Discard Pile").hl.div ~
                f.pile./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case ("view-frog-discard", f : InvasiveAAA) =>
            showOverlay(overlayScrollX((Frog.elem ~ " Discard Pile").hl.div ~
                f.pile./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case "view-dominances" =>
            showOverlay(overlayScrollX("Dominances".hl.div ~
                game.dominances./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case "view-quests" =>
            showOverlay(overlayScrollX("Quests".hl.div ~
                game.quests.take(3)./{ q => OnClick(q, Div(q.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case "view-deck" =>
            showOverlay(overlayScrollX("Draw Deck".hl.div ~
                1.to(game.deck.num)./{ n => Div(Image("card-back-art", styles.card), xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined) }.merge), onClick)

        case ("view-frog-deck", f : InvasiveDDD) =>
            showOverlay(overlayScrollX((Frog.elem ~ " Deck").hl.div ~
                Deck.frogDDD./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case ("view-frog-deck", f : InvasiveCCC) =>
            showOverlay(overlayScrollX((Frog.elem ~ " Draw Deck").hl.div ~
                1.to(f.deck.num)./{ n => Div(Image("card-frog-back-art", styles.card), xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined) }.merge), onClick)

        case ("view-frog-deck", f : InvasiveBBB) =>
            showOverlay(overlayScrollX((Frog.elem ~ " Draw Deck").hl.div ~
                1.to(f.deck.num)./{ n => Div(Image("card-frog-back-art", styles.card), xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined) }.merge), onClick)

        case ("view-frog-deck", f : InvasiveAAA) =>
            showOverlay(overlayScrollX((Frog.elem ~ " Draw Deck").hl.div ~
                1.to(f.deck.num)./{ n => Div(Image("card-frog-back-art", styles.card), xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined) }.merge), onClick)

        case ("view-hand", f : Trader) =>
            showOverlay(overlayScrollX((f.name.styled(f) ~ " hand").div ~
                f.hand./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case ("view-hand", f : Fanatic) =>
            showOverlay(overlayScrollX((f.name.styled(f) ~ " hand").div ~
                1.to(f.hand.num)./{ n => Div(Image("card-back-art", styles.card), xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined) }.merge ~
                f.revealed./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case ("view-hand", f : Underground) =>
            showOverlay(overlayScrollX((f.name.styled(f) ~ " hand").div ~
                1.to(f.hand.num)./{ n => Div(Image("card-back-art", styles.card), xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined) }.merge ~
                f.revealed./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case ("view-hand", f : Faction) =>
            showOverlay(overlayScrollX((f.name.styled(f) ~ " hand").div ~
                1.to(f.hand.num)./{ n => Div(Image("card-back-art", styles.card), xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined) }.merge), onClick)

        case ("view-decree", f : Aviary) =>
            showOverlay(overlayScrollX(
                Decree.all.%(d => f.decree(d).any)./(d =>
                    Div(d.name.hl) ~
                    f.decree(d).get./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge
                ).merge
            ), onClick)

        case ("view-retinue", f : Expedition) =>
            showOverlay(overlayScrollX(
                Retinue.all.%(d => f.retinue(d).any || f.complete(d).any)./(d =>
                    Div(d.elem) ~
                    (f.complete(d).get ++ f.retinue(d).get)./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge
                ).merge
            ), onClick)

        case ("view-lost-souls", f : Fanatic) =>
            showOverlay(overlayScrollX(
                Div("Lost Souls".hl ~ f.lost.some./(l => SpacedDash ~ FoxRabbitMouse./~(s => f.lost.count(_.suit == s).use(n => (n > 0).?(n.hlb ~ " " ~ s.elem))).join(", "))) ~
                f.lost./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge
            ), onClick)

        case ("view-leaders", f : Aviary) =>
            def lc(l : Leader) = convertActions(Some(f), $(InfoLeaderAction(f, l)), null)./(_.option).merge
            showOverlay(overlayScrollX(
                f.leader./("Leader".hl.div ~ lc(_)).|(Empty) ~
                f.contenders.some./(" ".pre.div ~ "Available".hl.div ~ _./(lc).merge).|(Empty) ~
                f.retired.some./(" ".pre.div ~ "Deposed".hl.div ~ _./(lc).merge).|(Empty)
            ), onClick)

        case "view-items" =>
            val craftable = $(Bag, Boots, Crossbow, Sword, Teapot, Coins, Bag, Boots, Hammer, Sword, Teapot, Coins)
            var available = game.uncrafted

            var uncrafted : $[Elem] = $

            craftable.foreach { c =>
                if (available.has(c)) {
                    available :-= c
                    uncrafted :+= Image("item-" + c.name, styles.iii)
                }
                else
                    uncrafted :+= Image("item-" + c.name + "-empty", styles.iii)
            }

            val items = (uncrafted.take(6) ~ Break ~ uncrafted.drop(6)).div(styles.margined)

            val ruins = game.ruins.values./~(_.items).$.sortBy(i => Item.order.indexOf(i))

            showOverlay(overlayScrollX("Craftable Items".hl.div ~ items ~ ruins.some./(Break ~ "In Ruins".hl.div ~ _./(i => Image("item-" + i.name, styles.iii)).merge).|(Empty)), onClick)

        case (f : Faction, h : Hireling) =>
            onHirelingStatus(h, false)

        case (f : Faction, e : Effect) =>
            val s = f.stuck.collect {
                case d @ CraftEffectCard(_, _, _, ee) if ee == e => d
            }

            if (s.any)
                onClick(s(0))
            else
                println("no info for effect " + e)

        case FaithfulRetainer =>
            showOverlay(overlayFitX(Image("artwork:" + "faithful-retainer", xstyles.artwork)), onClick)




        case d : DeckCard =>
            if (resources.images.hasSource("artwork:" + d.id))
                showOverlay(overlayFitX(Image("artwork:" + d.id, xstyles.artwork)), onClick)
            else
                showOverlay(overlayFitX(Image(d.id, xstyles.artwork)), onClick)

        case s : EdictAAA =>
            showOverlay(overlayFitX(Image(s.id, xstyles.artwork)), onClick)

        case s : Spell =>
            showOverlay(overlayFitX(Image("xc-spell-" + s.name, xstyles.artwork)), onClick)

        case q : Quest =>
            showOverlay(overlayFitX(Image("artwork:" + q.id, xstyles.artwork)), onClick)

        case l : Leader =>
            showOverlay(overlayFitX(Image("ed-" + l.name, xstyles.artwork)), onClick)

        case c : Character =>
            showOverlay(overlayFitX(Image("vb-char-" + c.name, xstyles.artwork)), onClick)

        case Nil =>
            overlayPane.invis()
            overlayPane.clear()

        case x =>
            println("unknown onClick: " + x)
    }

    def gameStatus(container : Container) {
        val hor = container.node.clientWidth > container.node.clientHeight * 3 + 1

        val quests = game.quests.any.?((game.quests.take(3)./(q => Image("quest-" + q.suit.name, styles.token) ~ " " ~ q.elem).join(Break).div(hor.not.?(styles.centerquest))).pointer.onClick.param("view-quests"))

        val discard =
            (
                (game.deck.num.formatted("%2d").hl.styled(styles.doubleFigures) ~ Image("deck", styles.pile)).pointer.onClick.param("view-deck") ~
                Gap ~
                (game.pile.num.formatted("%2d").hl.styled(styles.doubleFigures) ~ Image("pile-" + game.pile.any.?(game.pile.last.suit).|("empty"), styles.pile)).pointer.onClick.param("view-discard")
            ).div(xlo.flexhcenter)(hor.?(styles.verdeck))

        val dominances = game.dominances./(d => Image(d.id, styles.dominance)).merge.pointer.onClick.param("view-dominances")

        val craftable = $(Bag, Boots, Crossbow, Sword, Teapot, Coins, Bag, Boots, Hammer, Sword, Teapot, Coins)
        var available = game.uncrafted

        var uncrafted : $[Elem] = $

        craftable.foreach { c =>
            if (available.has(c)) {
                available :-= c
                uncrafted :+= Image("item-" + c.name, styles.ii)
            }
            else
                uncrafted :+= Image("item-" + c.name + "-empty", styles.ii)
        }

        val items = (uncrafted.take(6).merge.div(xlo.flexhcenter)(xlo.flexnowrap) ~ uncrafted.drop(6).merge.div(xlo.flexhcenter)(xlo.flexnowrap)).div(xlo.pointer).onClick.param("view-items")

        val s = $(|(items), quests, |(discard), |(dominances)).flatten.but(Empty)./(_.div(styles.skipline)).merge

        container.replace((s.div(xlo.flexhcenter)(styles.gstatus)), resources, onClick)
    }

    def updateStatus() {
        0.until(arity).foreach { n =>
            if (game.ordering.contains(n))
                factionStatus(game.ordering(n), statusBitmaps(n))
            else
            if (game.seating.any)
                statusBitmaps(n).replace(Div(Div(resources.getName(game.seating(n)).|(game.seating(n).name).hh), styles.smallname, xlo.pointer), resources)
            else
                statusBitmaps(n).replace(Div(Div(""), styles.smallname, xlo.pointer), resources)
        }

        gameStatus(statusGame)

        if (overlayPane.visible)
            overlayPane.vis()
        else
            overlayPane.invis()

        if (settingsPane.visible)
            settingsPane.vis()
        else
            settingsPane.invis()

        drawMap()
    }

    var highlight : |[UserAction] = None

    val zoom = 0.72

    val layouts = $(Layout("base", $(
                BasicPane("status", 12.2+0.8+0.8-0.8-0.8, 16.4, Priorities(top = 2, right = 2, maxXscale = 1.8, maxYscale = 1.4)),
                BasicPane("status-game-a", 10.5+0.5, 10, Priorities(grow = -1, maxXscale = 1.8, maxYscale = 2.4)),
                BasicPane("status-game-b", 35, 4.4, Priorities(top = 1, grow = -1, maxXscale = 3.0, maxYscale = 3.2)),
                BasicPane("log-a", 30, 10*4/4, Priorities(grow = 1, bottom = 1, top = -1, right = 1)),
                BasicPane("log-b", 20, 22*4/4, Priorities(grow = 1, bottom = 1, top = -1, right = 1)),

                BasicPane("map-small", 60*0.82*2*2, 55*0.82*2*2, Priorities(top = 3, left = 1, grow = -1, maxXscale = 1.2, maxYscale = 1.2)),
                BasicPane("action-a", 48, 22+2-1, Priorities(bottom = 2, right = 2, grow = 2)),
                BasicPane("action-b", 41, 38, Priorities(bottom = 2, right = 2, grow = 1, maxXscale = 1.2))
            )
            ./(p => p.copy(kX = p.kX * zoom, kY = p.kY * zoom))
        )
    )./~(l =>
        l.copy(name = l.name + "-fulldim", panes = l.panes./{
            case p : BasicPane if p.name == "map-small" => FullDimPane(p.name, p.kX, p.kY, p.pr)
            case p => p
        }, boost = 1.1) ::
        l.copy(name = l.name + "-plus20", panes = l.panes./{
            case p : BasicPane if p.name == "map-small" => BasicPane(p.name, p.kX * 1.2, p.kY * 1.2, p.pr)
            case p => p
        }, boost = 1.04) ::
        l.copy(name = l.name + "-minus20", panes = l.panes./{
            case p : BasicPane if p.name == "map-small" => BasicPane(p.name, p.kX * 0.8, p.kY * 0.8, p.pr)
            case p => p
        }, boost = 0.88) ::
        l.copy(name = l.name + "-normal")
    )./~(l =>
        l.copy(name = l.name + "-verdouble", boost = l.boost * 0.99, panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-verdouble", kY = p.kY * ((arity + 1) / 2), kX = p.kX * 2))
            case p : BasicPane if p.name.startsWith("status-game") && arity % 2 == 1 => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-hordouble", boost = l.boost * 0.99, panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-hordouble", kX = p.kX * ((arity + 1) / 2), kY = p.kY * 2))
            case p : BasicPane if p.name.startsWith("status-game") && arity % 2 == 1 => None
            case p => Some(p)
        }) ::
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
        $
    )./~(l =>
        l.copy(name = l.name + "-logA", panes = l.panes./~{
            case p : BasicPane if p.name == "log-a" => Some(p.copy(name = "log"))
            case p : BasicPane if p.name == "log-b" => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-logB", panes = l.panes./~{
            case p : BasicPane if p.name == "log-a" => None
            case p : BasicPane if p.name == "log-b" => Some(p.copy(name = "log"))
            case p => Some(p)
        }) ::
        $
    )./~(l =>
        l.copy(name = l.name + "-gameStatusA", panes = l.panes./~{
            case p : BasicPane if p.name == "status-game-a" => Some(p.copy(name = "status-game"))
            case p : BasicPane if p.name == "status-game-b" => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-gameStatusB", boost = l.boost * 1.001, panes = l.panes./~{
            case p : BasicPane if p.name == "status-game-a" => None
            case p : BasicPane if p.name == "status-game-b" => Some(p.copy(name = "status-game"))
            case p => Some(p)
        }) ::
        $
    )

    val layouter = Layouter(layouts.%(l => hrf.HRF.param("layout").none || hrf.HRF.param("layout").get == l.name), _./~{
        case f if f.name == "map-small" => $(f, f.copy(name = "overlay"))
        case f if f.name == "action" => $(f, f.copy(name = "undo"), f.copy(name = "settings"))
        case f if f.name == "status-horizontal" => 1.to(arity)./(n => f.copy(name = "status-" + n, x = f.x + ((n - 1) * f.width  / arity.toDouble).round.toInt, width  = (n * f.width  / arity.toDouble).round.toInt - ((n - 1) * f.width  / arity.toDouble).round.toInt))
        case f if f.name == "status-vertical"   => 1.to(arity)./(n => f.copy(name = "status-" + n, y = f.y + ((n - 1) * f.height / arity.toDouble).round.toInt, height = (n * f.height / arity.toDouble).round.toInt - ((n - 1) * f.height / arity.toDouble).round.toInt))
        case f if f.name == "status-hordouble" =>
            val c = ((arity + 1) / 2)
            1.to(c * 2)./(n => f.copy(name = "status-" + (n > arity).?("game").|(n.toString),
                x = f.x + (((n - 1) % c) * f.width / c.toDouble).round.toInt,
                width = (n * f.width / c.toDouble).round.toInt - ((n - 1) * f.width / c.toDouble).round.toInt,
                y = f.y + (n - 1) / c * (f.height / 2),
                height = (n > c).?(f.height - f.height / 2).|(f.height / 2))
            )
        case f if f.name == "status-verdouble" =>
            val c = ((arity + 1) / 2)
            1.to(c * 2)./(n => f.copy(name = "status-" + (n > arity).?("game").|((((n - 1) % c) * 2 + (n - 1) / c + 1).toString),
                y = f.y + (((n - 1) % c) * f.height / c.toDouble).round.toInt,
                height = (n * f.height / c.toDouble).round.toInt - ((n - 1) * f.height / c.toDouble).round.toInt,
                x = f.x + (n - 1) / c * (f.width / 2),
                width = (n > c).?(f.width - f.width / 2).|(f.width / 2))
            )
        case f => $(f)
    })

    val settingsKey = Meta.settingsKey

    val layoutKey = "v" + 2 + "." + "arity-" + arity

    var lastSelf : |[Player] = None

    override def info(self : |[Player], aa : $[UserAction]) = {
        lastSelf = self

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

    override def preinfo(self : |[Player], aa : $[UserAction]) = {
        val ii = currentGame.preinfo($, self, aa)
        ii.any.??(convertActions(self, ii))
    }

    var seen : $[Notification] = $

    override def showNotifications(self : $[F]) : Unit = {
        val newer = game.notifications
        val older = seen

        val display = newer.diff(older).%(_.factions.intersect(self ++ self./(fix)).any)./~(n => convertActions(self.single, n.infos)).some./~(_ :+ ZOption(Empty, Break))

        if (display.none)
            return

        overlayPane.vis()

        overlayPane.attach.clear()

        val ol = overlayPane.attach.appendContainer(overlayScrollX(Content), resources, onClick)

        val asker = new NewAsker(ol, resources.images.get)

        asker.zask(display)(resources)
    }

    def fix(f : Player) = f @@ {
        case f : PlayerN => game.ptf.get(f).|(f)
        case f => f
    }

    override def wait(self : $[Player], factions : $[Player]) {
        val was = lastWaiting

        lastThen = null
        lastActions = $
        lastActionsOffset = 0
        lastActionsChosen = None
        lastWaiting = factions

        if (was != lastWaiting)
            sayCurrentActions(false)

        showNotifications(self)

        super.wait(self./(fix), factions./(fix))
    }

    override val allSwitches = $(
        UseAutoAssignHits,
        SkipAmbushSuit(Fox),
        SkipAmbushSuit(Rabbit),
        SkipAmbushSuit(Mouse),
        SkipFieldHospitalsCount(1),
        SkipFieldHospitalsSuit(Fox),
        SkipFieldHospitalsSuit(Rabbit),
        SkipFieldHospitalsSuit(Mouse),
        UsePartisansSuit(Fox),
        UsePartisansSuit(Rabbit),
        UsePartisansSuit(Mouse),
        SkipPartisansSuit(Fox),
        SkipPartisansSuit(Rabbit),
        SkipPartisansSuit(Mouse)
    ) ++
    Meta.factions./~{
        case f : Hero => HeroExpansion.characters(options)./(c => PickFactionIfAvailable(FactionCharacterChoice(f, c)))
        case f => $(PickFactionIfAvailable(FactionChoice(f)))
    }

    override def switchesFor(f : Player) : $[hrf.meta.GameOption] =
        f.as[PlayerN]./~(f => allSwitches.%{
            case _ if "autopick" != "meaningless" => false
            case PickFactionIfAvailable(p) if game.ptf.contains(f) => false
            case PickFactionIfAvailable(p) if game.playChoices.has(p) => true
            case _ => false
        }) ++
        fix(f).as[Faction]./~(f => allSwitches.%{
            case PickFactionIfAvailable(s) => false
            case _ if callbacks.settings.has(NoOutOfTurn) => false
            case _ if game.states.contains(f).not => false
            case UseAutoAssignHits if f.is[Hero] && f.contracts.none => false
            case UseAutoAssignHits => f.presence.any || f.contracts.distinct.exists(_.presence.any)
            case SkipAmbushSuit(_) if f.hand.none => false
            case SkipAmbushSuit(s) if callbacks.settings.has(BasicOutOfTurn) && (f.hand.has(Ambush(Bird)) || f.hand.has(Ambush(s))) => false
            case SkipAmbushSuit(s) if f.is[Hero] => f.as[Hero]./~(_.region.as[Clearing]).?(_.cost.matched(s))
            case SkipAmbushSuit(_) => true
            case SkipFieldHospitalsCount(_) if f.is[Feline].not => false
            case SkipFieldHospitalsCount(_) if f.hand.none => false
            case SkipFieldHospitalsCount(_) => true
            case SkipFieldHospitalsSuit(_) if f.is[Feline].not => false
            case SkipFieldHospitalsSuit(_) if f.hand.none => false
            case SkipFieldHospitalsSuit(s) if callbacks.settings.has(BasicOutOfTurn) && f.hand.exists(_.matches(s)) => false
            case SkipFieldHospitalsSuit(_) => true
            case UsePartisansSuit(s) if callbacks.settings.has(BasicOutOfTurn) && f.hand.exists(_.matches(s).not) => false
            case SkipPartisansSuit(s) => f.has(Partisans(s))
            case UsePartisansSuit(s) => f.has(Partisans(s))
        })

    def isEnabled(f : Faction, s : hrf.meta.GameOption) = callbacks.switches.getSwitches.has(     Meta.writeFaction(f) + " "  + Meta.writeOption(s))
    def isEnabled(f : PlayerN, s : hrf.meta.GameOption) = callbacks.switches.getSwitches.has(MetaAdset.writeFaction(f) + " "  + MetaAdset.writeOption(s))

    override def switchesOn(f : Player) : $[hrf.meta.GameOption] =
        fix(f).as[Faction]./~(f => allSwitches.%(isEnabled(f, _))) ++
             f.as[PlayerN]./~(f => allSwitches.%(isEnabled(f, _)))

    override def enableSwitch(f : Player, s : hrf.meta.GameOption) {
        s @@ {
            case UsePartisansSuit(s) => disableSwitch(f, SkipPartisansSuit(s))
            case SkipPartisansSuit(s) => disableSwitch(f, UsePartisansSuit(s))
            case PickFactionIfAvailable(p) => f.as[PlayerN].foreach { f =>
                switchesFor(f).but(s).of[PickFactionIfAvailable].%(isEnabled(f, _)).foreach(disableSwitch(f, _))
            }
            case _ =>
        }

        fix(f).as[Faction].foreach(f => callbacks.switches.enableSwitch(     Meta.writeFaction(f) + " "  + Meta.writeOption(s)))
             f.as[PlayerN].foreach(f => callbacks.switches.enableSwitch(MetaAdset.writeFaction(f) + " "  + MetaAdset.writeOption(s)))
    }

    override def disableSwitch(f : Player, s : hrf.meta.GameOption) {
        fix(f).as[Faction].foreach(f => callbacks.switches.disableSwitch(     Meta.writeFaction(f) + " "  + Meta.writeOption(s)))
             f.as[PlayerN].foreach(f => callbacks.switches.disableSwitch(MetaAdset.writeFaction(f) + " "  + MetaAdset.writeOption(s)))
    }

    override def shadowAsk(faction : Player, actions : $[UserAction]) : |[UserAction] =
        fix(faction).as[Faction]./~(shadowCheck(_, actions)) ||
        fix(faction).as[PlayerN]./~(shadowCheck(_, actions))

    def shadowCheck(f : PlayerN, actions : $[UserAction]) : |[UserAction] = {
        lazy val expand = actions./~{
            case a : HalfExplode => a.expand(None)
            case _ : Info => $
            case _ : Hidden => $
            case a => $(a)
        }./~{
            case _ : Info => None
            case _ : Hidden => None
            case a => Some(a)
        }

        val test = hrf.HRF.flag("async-test")

        val r = random() / test.?(8).|(1)

        if (r < 0.20)
            expand./(_.unwrap).of[SelectFactionAction].%(a => isEnabled(f, PickFactionIfAvailable(a.f))).foreach { a =>
                if (test) alog("auto select faction".hl, 0, _ => {})

                return expand.%(_.unwrap == a).single
            }

        None
    }

    def shadowCheck(f : Faction, actions : $[UserAction]) : |[UserAction] = {
        if (game.turn <= 0)
            return None

        lazy val choice = actions./~{
            case _ : Info => None
            case _ : Hidden => None
            case a => Some(a)
        }

        lazy val expand = actions./~{
            case a : HalfExplode => a.expand(None)
            case _ : Info => $
            case _ : Hidden => $
            case a => $(a)
        }./~{
            case _ : Info => None
            case _ : Hidden => None
            case a => Some(a)
        }

        lazy val clearing = actions.of[HiddenClearing]./(_.clearing).single

        val test = hrf.HRF.flag("async-test")

        val r = random() / test.?(8).|(1)

        if (r < 0.02)
            if (actions.has(HiddenAmbush) && clearing.exists(c => c.suits.forall(s => isEnabled(f, SkipAmbushSuit(s))))) {
                if (test) alog("auto skip ambush".hl, 0, _ => {})

                return Some(choice.last)
            }

        if (r < 0.40)
            if (actions.has(HiddenAssignHits) && isEnabled(f, UseAutoAssignHits)) {
                if (test) alog("auto assign hits?".hl, 0, _ => {})

                val values = expand.of[XXObjectsSelectedAction[Figure]]./(_.values)

                val variants = values./(_.indexed./{
                    case Figure(faction, piece : Base, _) -> n => Figure(faction, piece, n)
                    case Figure(faction, piece, _) -> _ => Figure(faction, piece, 0)
                }.toSet).distinct

                if (variants.num == 1)
                    return Some(expand(0))
            }

        if (r < 0.08)
            actions./(_.unwrap).of[FieldHospitalsIgnoreAction].%(a => a.c.suits.forall(s => isEnabled(f, SkipFieldHospitalsSuit(s)))).foreach { a =>
                if (test) alog("auto skip field hospitals".hl, 0, _ => {})

                return Some(a.wrap)
            }

        if (r < 0.08)
            actions./(_.unwrap).of[FieldHospitalsIgnoreAction].%(a => isEnabled(f, SkipFieldHospitalsCount(1)) && f.as[Feline].?(_.limbo(a.c).$.%(_.piece.is[Warrior]).num) <= 1).foreach { a =>
                if (test) alog("auto skip field hospitals".hl, 0, _ => {})

                return Some(a.wrap)
            }

        if (r < 0.08)
            actions./(_.unwrap).of[PartisansAction].%(a => isEnabled(f, UsePartisansSuit(a.s))).foreach { a =>
                if (test) alog("auto use partisans".hl, 0, _ => {})

                return Some(a)
            }

        if (r < 0.08)
            actions./(_.unwrap).of[PartisansAction].%(a => isEnabled(f, SkipPartisansSuit(a.s))).foreach { a =>
                if (test) alog("auto skip partisans".hl, 0, _ => {})

                expand.but(a).single.foreach { r =>
                    return Some(r)
                }
            }

        None
    }

    var lastActions : $[UserAction] = $
    var lastThen : UserAction => Unit = null
    var lastActionsOffset : Int = 0
    var lastActionsChosen : |[Int] = None
    var lastWaiting : $[Player] = $

    override def ask(faction : |[Player], actions : $[UserAction], then : UserAction => Unit) {
        lastActions = actions
        lastThen = then
        lastActionsOffset = 0
        lastActionsChosen = None
        lastWaiting = $

        if (Speech.enabled)
            sayCurrentActions(false)

        showNotifications(faction.$)

        lazy val choice = actions./~{
            case _ : Info => None
            case _ : Hidden => None
            case a => Some(a)
        }

        lazy val expand = actions./~{
            case a : HalfExplode => a.expand(None)
            case _ => $
        }./~{
            case _ : Info => None
            case _ : Hidden => None
            case a => Some(a)
        }

        if (actions.has(HiddenAmbush) && choice.num == 1 && (callbacks.settings.has(AutoAmbush))) {
            scalajs.js.timers.setTimeout(0) { then(choice(0)) }
            return
        }

        if (actions.has(HiddenAssignHits)) {
            val variants = expand.of[XXObjectsSelectedAction[Figure]]./(_.values)

            val choice = variants./(_.indexed./{
                case Figure(faction, piece : Base, _) -> n => Figure(faction, piece, n)
                case Figure(faction, piece, _) -> _ => Figure(faction, piece, 0)
            }.toSet).distinct

            if (actions.has(HiddenAssignHits) && choice.num == 1 && (callbacks.settings.has(AutoAssignHits))) {
                scalajs.js.timers.setTimeout(0) { then(expand(0)) }
                return
            }
        }

        if (game.turn < hrf.HRF.paramInt("fastsetup").|(0) && game.over.none) {
            val all = game.explode(actions, false, None)
            scalajs.js.timers.setTimeout(0) { then(all.shuffle(0)) }
            return
        }

        super.ask(faction, actions, then)
    }

    override def updateHighlight(a : |[UserAction]) {
        if (callbacks.settings.has(HighlightClearings)) {
            highlight = a
            updateStatus()
        }
        else
            highlight = None
    }

    override def styleAction(faction : |[F], actions : $[UserAction], a : UserAction, unavailable : Boolean, view : |[Any]) : $[Style] =
        view @@ {
            case _ if unavailable.not => $()
            case Some(_ : QuasiItem) => $(styles.unquasi)
            case Some(_ : Figure) => $(styles.unquasi)
            case Some(_) => $(xstyles.unavailableCard)
            case _ => $(xstyles.unavailableText)
        } ++
        a @@ {
            case _ : Info => $(xstyles.info)
            case _ if unavailable => $(xstyles.info)
            case _ => $(xstyles.choice)
        } ++
        $(xstyles.xx, xstyles.chp, xstyles.chm) ++
        faction @@ {
            case Some(f : Faction) => $(elem.borders.get(f))
            case Some(f : PlayerN) => game.ptf.get(f)./(elem.borders.get).$
            case _ => $()
        } ++
        a @@ {
            case a : Selectable if a.selected => $(styles.selected)
            case _ => $()
        } ++
        view @@ {
            case Some(_ : Character)  => $(styles.inline, styles.charback) ++ a.selected.?(styles.selchar)
            case Some(_ : PlayChoice) => $(styles.inline, styles.charback) ++ a.selected.?(styles.selchar)
            case Some(_ : Figure)                    => $(styles.inline, styles.quasi) ++ a.selected.?(styles.selfigure)
            case Some(x : QuasiItem) if x.figure.any => $(styles.inline, styles.quasi) ++ a.selected.?(styles.selfigure)
            case Some(_ : QuasiItem)                 => $(styles.inline, styles.quasi) ++ a.selected.?(styles.selquasi)
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
