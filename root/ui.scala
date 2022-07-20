package root
//
//
//
//
import logger._, colmat._
//
//
//
//

import root.gaming._

import colmat._

import org.scalajs.dom

import hrf.canvas._

import hrf.ui._
import hrf.ui.again._

import hrf.elem._
import hrf.html._


import root.elem._

object UI extends BaseUI {
    val gaming = root.gaming
    
    def create(uir : hrf.html.ElementAttachmentPoint, game : Game, resources : Resources, title : String, saveReplay : (=> Unit) => Unit) = new UI(uir, game, resources, title, saveReplay)
}

class UI(val uir : ElementAttachmentPoint, val game : Game, val resources : Resources, title : String, saveReplay : (=> Unit) => Unit) extends GameUI with UI.GreyUI {
    def factionElem(f : Player) = f @@ {
        case f : Faction => f.name.styled(f)
        case f : PlayerN => f.name
    }

    val statuses = 1.to(game.arity)./(i => newPane("status-" + i, Content, styles.status, styles.fstatus))
    val statusGame = newPane("status-game", Content)
    val statusGameX = newPane("status-game-x", Content)
    val mapSmall = newPane("map-small", Content)
    val overlayPane = newOuterPane("overlay", Content)
    
    var overlay = false
    var fullscreen = false

    val mapBitmapSmall = new CachedBitmap(mapSmall.attach.parent)
    var map = mapBitmapSmall
    
    mapSmall.replace(Div(Div(title, styles.title), xstyles.overlay), resources)

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
        
    case object Cell extends Building
    case object Battle extends Token
    case object Placement extends Token
    case object Nuke extends Token

    case class DrawRect(key : String, x : Double, y : Double, width : Double, height : Double)
    
    case class DrawItem(region : Region, faction : Faction, piece : Piece, x : Int, y : Int, scale : Double = 1.0) {
        val pr = faction.?./(_.short.toLowerCase + "-").|("")

        val forest = region.isInstanceOf[Forest]

        val icon : DrawRect = piece match {
            case Cell => { DrawRect("building-slot", -50, -50, 100, 100) }
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

            case Wood => { DrawRect(pr + "wood", -45, -45, 90, 90) }
            case Sympathy => { DrawRect(pr + "sympathy", -45, -45, 90, 90) }
            case TradePost(Fox) => { DrawRect(pr + "trade-post-fox", -45, -45, 90, 90) }
            case TradePost(Rabbit) => { DrawRect(pr + "trade-post-rabbit", -45, -45, 90, 90) }
            case TradePost(Mouse) => { DrawRect(pr + "trade-post-mouse", -45, -45, 90, 90) }
            case Tunnel => { DrawRect(pr + "tunnel", -45, -45, 90, 90) }

            case _ : Plot if state.cc(faction.mischief.get).hidden.contains(region) => { DrawRect(pr + "plot", -45, -45, 90, 90) }

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
            
            case Relic(r, value) => { DrawRect(pr + r + "-" + value./(_.toString).|("hidden"), -45, -45, 90, 90) }

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

            case _ => 
                println(List(region : Region, faction : Faction, piece : Piece, x : Int, y : Int).mkString(" | "))
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
        
    var oldPositions : List[DrawItem] = Nil
    var oldGates : List[Region] = Nil
    
    val mp = resources.getImage("map")
    var deadmp : Bitmap = null

    def fixAutumn(a : (Int, Int)) = (a._1 + 20, a._2 + 20)

    def drawMap() {
        import AutumnBoard._
        import WinterBoard._
        import LakeBoard._
        import MountainBoard._
 
        def center(r : Region) = (r match {
            case Hill => fixAutumn(274, 301)
            case Haven => fixAutumn(280, 1798)
            case Quarry => fixAutumn(2106, 506)
            case Weald => fixAutumn(1991, 1930)
            case Creek => fixAutumn(1304, 199)
            case Beach => fixAutumn(1034, 635)
            case Dune => fixAutumn(244, 903)
            case Glade => fixAutumn(742, 1224)
            case Waterfall => fixAutumn(1522, 1067)
            case Mountain => fixAutumn(2173, 1151)
            case Meadow => fixAutumn(881, 1981)
            case Pond => fixAutumn(1405, 1706)

            case AutumnN => List((837, 386), (1554, 463)).shuffle.head
            case Witchwood => List((1853, 804), (1160, 980)).shuffle.head
            case AutumnNW => (601, 770)
            case AutumnS => List((1194, 1346), (1560, 1400)).shuffle.head
            case AutumnSW => List((940, 1592), (650, 1749)).shuffle.head
            case AutumnE => (1893, 1377)
            case AutumnW => (403, 1344)
            
            
            case Mound => (322, 292)
            case Moor => (2104, 518)
            case Hedge => (2028, 1892)
            case Spire => (337, 1792)
            case Trench => (908, 388)
            case Pit => (1432, 530)
            case Drift => (334, 943)
            case Wade => (892, 1129)
            case Bend => (1527, 1037)
            case Ford => (2203, 1294)
            case Rock => (889, 1896)
            case Dale => (1408, 1607)
            
            case WinterNW => List((433, 629), (631, 927)).shuffle.head
            case Deadwood => List((913, 728), (1167, 777)).shuffle.head
            case WinterNE => (1946, 957)
            case WinterW => (532, 1312)
            case WinterSW => (748, 1592)
            case Mirkwood => (1194, 1346)
            case WinterSE => (1729, 1679)
            case WinterE => (1893, 1377)


            case Den => (360, 418)
            case Prairie => (1080, 272)
            case Vert => (1658, 502)
            case Shade => (2164, 728)
            case Lawn => (253, 1058)
            case Shoal => (769, 798)
            case Bay => (1569, 1070)
            case Yard => (2127, 1256)
            case Grove => (300, 1773)
            case Marsh => (821, 1467)
            case Alley => (1080, 1940)
            case Gulf => (2043, 1870)
            
            case LakeW => (418, 755)
            case LakeNW => (754, 465)
            case LakeN => (1224, 817)
            case LakeNE => (1328, 521)
            case AcresWild => (1860, 829)
            case HeartOfOak => (606, 1175)
            case LakeS => (722, 1793)
            case LakeSE => (1220, 1575)
            case LakeE => (1842, 1447)
            

            case Slope => (273, 378)
            case Ledge => (713, 761)
            case Mine => (1287, 356)
            case Peak => (2001, 495)
            case Brim => (215, 1273)
            case Pass => (1188, 886)
            case Valley => (1527, 1360)
            case Ridge => (2195, 1106)
            case Drain => (357, 1838)
            case Ramp => (820, 1486)
            case Cliff => (1388, 1900)
            case Crest => (2078, 1739)

            case MountainW => (354, 814)
            case MountainNW => (1044, 636)
            case MountainCW => (888, 1098)
            case MountainC => (1190, 1216)
            case MountainCN => (1438, 1015)
            case MountainN => (1608, 638)
            case MountainNE => (1916, 946)
            case MountainSE => (1952, 1404)
            case LiarWind => (528, 1442)
            case NadaMoonshine => (1207, 1622)
                        
            case _ => (0, 0); throw new Error("no center for " + r)
        })
        

        def ferry(r : Region) = (r match {
            case Shoal => (992, 989)
            case Bay => (1300, 1145)
            case Marsh => (1090, 1377)
            case Gulf => (1756, 1727)
                        
            case _ => (0, 0); throw new Error("no ferry for " + r)
        })

        def gatesXY(r : Region) = r match {
            case Hill => List((217, 284))./(fixAutumn)
            case Glade => List((849, 1206), (748, 1313))./(fixAutumn)
            case Meadow => List((955, 1865), (914, 1999))./(fixAutumn)
            case Mountain => List((2201, 1068), (2094, 1183))./(fixAutumn)
            case Haven => List((360, 1797))./(fixAutumn)
            case Creek => List((1371, 152), (1239, 168))./(fixAutumn)
            case Beach => List((1063, 751), (928, 585))./(fixAutumn)
            case Weald => List((1953, 1907))./(fixAutumn)
            case Dune => List((306, 816), (201, 1015))./(fixAutumn)
            case Waterfall => List((1475, 1143), (1590, 1072), (1488, 997))./(fixAutumn)
            case Pond => List((1401, 1611), (1321, 1740))./(fixAutumn)
            case Quarry => List((2103, 481), (2157, 587))./(fixAutumn)
            
            case Mound => List((300, 286))
            case Trench => List((940, 324), (908, 434))
            case Pit => List((1472, 482), (1378, 576))
            case Moor => List((2116, 526))
            case Drift => List((320, 930))
            case Wade => List((836, 1040), (980, 1170), (848, 1174))
            case Bend => List((1432, 948), (1486, 1114), (1558, 996))
            case Ford => List((2220, 1328))
            case Spire => List((410, 1774), (284, 1740))
            case Rock => List((878, 1950), (916, 1844))
            case Dale => List((1372, 1558), (1438, 1658))
            case Hedge => List((2004, 1966), (2032, 1850))

            case Den => List((360, 455))
            case Prairie => List((1119, 282))
            case Vert => List((1711, 508), (1591, 542))
            case Shade => List((2231, 780))
            case Lawn => List((294, 1074))
            case Shoal => List((841, 806), (781, 707), (719, 834))
            case Bay => List((1633, 980), (1496, 1027), (1584, 1162))
            case Yard => List((2151, 1382), (2050, 1228), (2170, 1261))
            case Grove => List((266, 1789))
            case Marsh => List((794, 1552), (843, 1420), (912, 1536))
            case Alley => List((1114, 1914))
            case Gulf => List((2078, 1894), (1981, 1771))
            
            case Slope => List((326, 394), (238, 293))
            case Ledge => List((760, 699), (733, 813), (639, 723))
            case Mine => List((1290, 349))
            case Peak => List((2082, 463), (1995, 556))
            case Brim => List((226, 1258))
            case Pass => List((1129, 795), (1219, 961))
            case Valley => List((1483, 1272), (1498, 1394), (1616, 1326))
            case Ridge => List((2185, 1091))
            case Drain => List((277, 1795),  (432, 1856))
            case Ramp => List((767, 1548), (800, 1435), (883, 1531))
            case Cliff => List((1398, 1869))
            case Crest => List((2025, 1678), (2118, 1766))
            
            case c : Clearing => 
                val (x, y) = Region.center(c)

                0.until(c.capacity)./(n => (x - 60 * (c.capacity - 1) + 120 * n, y))
            
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
        
        val d = game.options.has(AutumnMap).?(0).|(12)
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

        if (deadmp == null && game.ui.graveyard) {
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

        if (game.ui.graveyard)
            g.drawImage(deadmp.canvas, 0, 0)
        else
            g.drawImage(mp, 0, 0)

        state.rubble.foreach { case (from, to) =>
            val pp = $("rubble-" + from.name + "-" + to.name, "rubble-" + to.name + "-" + from.name)
            pp.%(resources.hasImage).take(1)./(p => g.drawImage(resources.getImage(p), 0, 0))
        }
        
        if (highlight.any) {
            val l = highlight.get.productIterator.toList./~ {
                case c : Clearing => Some(c)
                case _ => None
            }
            
            if (l.num == 2) {
                val pp = $("path-" + l(0).name + "-" + l(1).name, "path-" + l(1).name + "-" + l(0).name)
                pp.%(resources.hasImage).take(1).foreach(p => g.drawImage(resources.getImage(p), 0, 0))
            }
        }

        val hhh = 
            if (state.gameover == null) 
                state.highlights./~(x => $(x, x)).reverse.zip(1.0 :: Nil).reverse
            else
                state.highlights.% {
                    case MoveHighlight(_, _) => game.ui.movements
                    case BattleHighlight(_) => game.ui.battles
                    case NukeHighlight(_) => game.ui.nukes
                    case _ => false
                }./((_, 1.0))

        hhh.foreach { h =>
            g.globalAlpha = h._2
            h._1 match {
                case PlaceHighlight(l) => 
                    l.foreach { c => 
                        val (x, y) = Region.center(c)
                        g.drawImage(resources.getImage("clearing-highlight-placement"), x - 300, y - 300)
                    }
                case BattleHighlight(c) => 
                    val (x, y) = Region.center(c)
                    g.drawImage(resources.getImage("clearing-highlight-battle"), x - 300, y - 300)
                case MoveHighlight(from, to) =>
                    val pp = $("path-" + from.name + "-" + to.name, "path-" + to.name + "-" + from.name)
                    pp.%(resources.hasImage).take(1)./{ p => g.drawImage(resources.getImage(p), 0, 0) }
                case _ =>
            }
        }

        def drawRule() {
            if (state.mapping != null) {
                state.clearings.foreach { c =>
                    val (x, y) = Region.center(c)
                    val dx = c.name.length * 12 + 38
                    g.drawImage(resources.getImage("clearing-" + c.name + "-" + state.mapping(c)), x - 240, y + 165)
                    g.drawImage(resources.getImage("clearing-suit-" + state.mapping(c)), x + dx - 30, y + 165)
                    g.drawImage(resources.getImage("clearing-suit-" + state.mapping(c)), x - dx - 30, y + 165)
                    state.factions.foreach { f =>
                        if (state.pstates.contains(f))
                        if (state.rule(f)(c)) {
                            f match {
                                case f : WarriorFaction => 
                                    if (game.ui.rules) {
                                        val r = DrawItem(c, f, f.warrior, x, y, 4.5).rect
                                        g.globalAlpha = 0.3
                                        g.drawImage(resources.getImage(r.key), r.x, r.y + 150, r.width, r.height)
                                        g.globalAlpha = 1.0
                                    }

                                case _ => ===("non-warrior faction rule")
                            }
                        }
                    }
                }
            }
        }

        g.globalAlpha = game.ui.graveyard.?(0.5).|(1.0)
        
        g.drawImage(resources.getImage("map-woods"), 0, 0)

        var saved = oldPositions
        oldPositions = Nil

        var draws : List[DrawItem] = Nil
        
        state.board.regions.foreach { r =>
            
            var fixed : List[DrawItem] = Nil
            var tofix : List[DrawItem] = Nil
            var all : List[DrawItem] = Nil
            var sticking : List[DrawItem] = Nil
            var free : List[DrawItem] = Nil
            
            var gates = gatesXY(r)

            val figures = game.ui.graveyard.?(state.graveyard.get(r).|(Nil)).|(state.atRegion(r))
            
            figures.foreach { p => 
                p.piece match {
                    case Keep => 
                        val (x, y) = Region.center(r)
                        fixed :+= DrawItem(r, p.faction, p.piece, x, y)
                    case Ferry => 
                        val (x, y) = ferry(r)
                        fixed :+= DrawItem(r, p.faction, p.piece, x, y)
                    case Tower => 
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
            
            if (r.is[Clearing] && state.scorched != Some(r)) {
                gates.foreach { case (x, y) => if (x != 0 && y != 0) fixed :+= DrawItem(r, null, Cell, x, y) }
            }

            free ++= tofix

            if (free.num > 3) {
                free = free ++ sticking
                sticking = Nil
            }
            
            def rank(d : DrawItem) = d.piece match {
                case Wood => 0
                case w : Warrior => 1
                case t : Token => 2
                case p : Pawn => 3
                case b : Building => 5
            }
            
            val (px, py) = Region.center(r)

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

        g.globalAlpha = game.ui.graveyard.?(0.6).|(1.0)
        
        draws.%(_.piece == Keep).sortBy(d => d.y - (d.piece.is[Token]).??(10000) - (d.piece == Cell).??(20000) - (d.piece == Keep).??(40000)).foreach { d =>
            g.drawImage(resources.getImage(d.rect.key), d.rect.x, d.rect.y)
        }
    
        drawRule()

        draws.%(_.piece != Keep).sortBy(d => d.y - (d.piece.isInstanceOf[Token]).??(10000) - (d.piece == Cell).??(20000) - (d.piece == Keep).??(40000)).foreach { d =>
            g.drawImage(resources.getImage(d.rect.key), d.rect.x, d.rect.y)
        }
    
        hhh.foreach { h =>
            g.globalAlpha = h._2
            h._1 match {
                case NukeHighlight(l) => 
                    l.foreach { c =>
                        val (x, y) = Region.center(c)
                        g.drawImage(resources.getImage("clearing-highlight-nuke"), x - 300, y - 300)
                    }
                case _ =>
            }
        }
        
        /*
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
            g.drawImage(resources.getImage("mc-recruiter"), mp.width / 2 - 19 * 50 + x * 100, mp.height / 2 + 16 * 50 - y * 100)
        }
        */

        g.globalAlpha = 1.0
    }
        
    val statusBitmaps = statuses

    def factionStatus(f : Faction, container : Container) {
        val _state = state

        import _state._
        
        import helper._

        
        if (!state.pstates.contains(f)) {
            container.replace(Div(Div(f.name).styled(f), styles.smallname, xlo.pointer), resources)
            return
        }

        val nameres = resources.copy(names = () => {
            var r = resources.names()
            state.players.foreach { p =>
                if (r.contains(p))
                    if (ptf.contains(p))
                        r += ptf(p) -> r(p)
            }
            r
        })

        val name = OnClick("fun-name", Div(((game.ui.funNames && nameres.getName(f).any).?(f.funName).|(f.name) ~ f.hero./?(f => " " + f.character.?./(_.name).|("~~~"))).styled(f), styles.smallname, xlo.pointer))

        val p = of(f)
        
        def domHint(s : Suit) = s match {
            case Bird => "Bird Domination: wins if rules two opposite corners at the start of the turn"
            case s => s.name + " Domination: wins if rules there " + s.name + " clearings at the start of the turn"
        }
                           
        val handcards = (f.hand.num > 5).?(
            Image("deck", styles.card5info).*(f.hand.num / 5) ~ dt.CardBackInfo.*(f.hand.num % 5)
        ).|(
            dt.CardBackInfo.*(f.hand.num).merge
        )

        val hand : Elem = f match {
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
        }
        
        val vp = p.coalition./(a => Hint("Coalition with " + a.name + ": wins if " + a.name + " wins", a.ss)).||(p.dominance./(d => Hint(domHint(d.suit), ("(" + d.suit.name + ")").styled(d.suit)))).|(Hint("Victory points: " + p.vp, p.vp.vp))

        val vphand = Div(vp ~ " |  ".pre.styled(styles.narrow) ~ hand)

        def icon(p : Fund) : Elem = p match {
            case AnyFund => "?"
            case f : WarriorFaction => Image((f.short + "-" + f.warrior.name), styles.fund)
            case f : WarriorFaction => f.warrior.name.take(1).styled(f)
        }
        
        def iconx(p : Fund) : Elem = p match {
            case f : WarriorFaction => Image(f.warrior.imgid(f), styles.wr)
        }
        
        def pack(l : List[Fund], n : Int, icon : Fund => Elem) : Elem = {
            if (2 * l.num <= n) {
                l./(icon) ~ "".padTo(n - 2 * l.num, ' ').pre
            } else {
                val r = l.distinct.sortBy(w => l.count(w)).reverse./(w => (l.count(w) > 1).??("" + l.count(w)) + "  ").mkString(" ")
                l.distinct.sortBy(w => l.count(w)).reverse./(w => (l.count(w) > 1).??("" ~ l.count(w).hl) ~ icon(w)).join("+") ~ "".padTo(n - r.length, ' ').pre
            }
        }
                
        val effects = (p.effects ++ p.services)./(e => Hint(e match {
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
        }, e.elem.div(styles.effect) ~ (e == CoffinMakers && xcoffins.get.any).?(pack(xcoffins.get./~(_.faction.as[WarriorFaction]), 2, iconx).div(styles.warline)))).merge
        
        val trade = p.forTrade.any.?? {
            Gap ~ Gap ~ p.forTrade./(i => Image(i.imgid, styles.ii)).merge.div(xstyles.smaller85)
        }
        
        def emtb(s : String) = s.split('|').toList./{
            case " " => Image("empty-building", styles.building)
            case "+" => Image("empty-building-card", styles.building)
            case "1" => Image("empty-building-1", styles.building)
            case "2" => Image("empty-building-2", styles.building)
            case "3" => Image("empty-building-3", styles.building)
            case "4" => Image("empty-building-4", styles.building)
            case "5" => Image("empty-building-5", styles.building)
            case "1+" => Image("empty-building-1-card", styles.building)
            case "2+" => Image("empty-building-2-card", styles.building)
            case "3+" => Image("empty-building-3-card", styles.building)
            case "4+" => Image("empty-building-4-card", styles.building)
            case "5+" => Image("empty-building-5-card", styles.building)
            case "1p" => Image("empty-building-1p", styles.building)
            case "2p" => Image("empty-building-2p", styles.building)
        }
        
        val fs : Elem = f match {
            case f : Feline =>
                val sm = "Sawmills\n" + f.all(Sawmill).num + " on the map\n" + f.pooled(Sawmill) + " available" + "\n\n"
                val ws = "Workshops\n" + f.all(Workshop).num + " on the map\n" + f.pooled(Workshop) + " available" + "\n\n"
                val rc = "Recruiters\n" + f.all(Recruiter).num + " on the map\n" + f.pooled(Recruiter) + " available" + "\n \n"
                
                Hint("Wood\n" + f.all(Wood).num + " on the map\n" + f.pooled(Wood) + " available", (Image(f.style + "-wood", styles.token) * f.all(Wood).num) ~ (Image("empty-token", styles.token) * f.pooled(Wood))) ~
                Gap ~
                Div(
                    Image("empty-building-cost-0", styles.building) ~
                    Image("empty-building-cost-1", styles.building) ~
                    Image("empty-building-cost-2", styles.building) ~
                    Image("empty-building-cost-3", styles.building) ~
                    Image("empty-building-cost-3", styles.building) ~
                    Image("empty-building-cost-4", styles.building)
                , styles.satchel) ~
                Hint(sm + "1st Sawmill: cost 0 Wood"      , (Image((f.all(Sawmill).num >= 1).?(f.style + "-sawmill").|("empty-building"), styles.building))) ~
                Hint(sm + "2nd Sawmill: cost 1 Wood, 1 VP", (Image((f.all(Sawmill).num >= 2).?(f.style + "-sawmill").|("empty-building-1"), styles.building))) ~
                Hint(sm + "3rd Sawmill: cost 2 Wood, 2 VP", (Image((f.all(Sawmill).num >= 3).?(f.style + "-sawmill").|("empty-building-2"), styles.building))) ~
                Hint(sm + "4th Sawmill: cost 3 Wood, 3 VP", (Image((f.all(Sawmill).num >= 4).?(f.style + "-sawmill").|("empty-building-3"), styles.building))) ~
                Hint(sm + "5th Sawmill: cost 3 Wood, 4 VP", (Image((f.all(Sawmill).num >= 5).?(f.style + "-sawmill").|("empty-building-4"), styles.building))) ~
                Hint(sm + "6th Sawmill: cost 4 Wood, 5 VP", (Image((f.all(Sawmill).num >= 6).?(f.style + "-sawmill").|("empty-building-5"), styles.building))) ~
                Break ~
                Hint(ws + "1st Workshop: cost 0 Wood"   + (f == BK).?(", extra move")             .|("")    , (Image((f.all(Workshop).num >= 1).?(f.style + "-workshop").|("empty-building"), styles.building))) ~
                Hint(ws + "2nd Workshop: cost 1 Wood, " + (f == BK).?("1 VP, extra battle")       .|("2 VP"), (Image((f.all(Workshop).num >= 2).?(f.style + "-workshop").|("empty-building-2"), styles.building))) ~
                Hint(ws + "3rd Workshop: cost 2 Wood, " + (f == BK).?("2 VP, extra move")         .|("2 VP"), (Image((f.all(Workshop).num >= 3).?(f.style + "-workshop").|("empty-building-2"), styles.building))) ~
                Hint(ws + "4th Workshop: cost 3 Wood, " + (f == BK).?("3 VP, extra card draw")    .|("3 VP"), (Image((f.all(Workshop).num >= 4).?(f.style + "-workshop").|("empty-building-3" + (f == BK).??("-card")), styles.building))) ~
                Hint(ws + "5th Workshop: cost 3 Wood, " + (f == BK).?("4 VP, extra battle")       .|("4 VP"), (Image((f.all(Workshop).num >= 5).?(f.style + "-workshop").|("empty-building-4"), styles.building))) ~
                Hint(ws + "6th Workshop: cost 4 Wood, " + (f == BK).?("5 VP, extra hit attacking").|("5 VP"), (Image((f.all(Workshop).num >= 6).?(f.style + "-workshop").|("empty-building-5"), styles.building))) ~
                Break ~
                Hint(rc + "1st Recruiter: cost 0 Wood"                       , (Image((f.all(Recruiter).num >= 1).?(f.style + "-recruiter").|("empty-building"), styles.building))) ~
                Hint(rc + "2nd Recruiter: cost 1 Wood, 1 VP"                 , (Image((f.all(Recruiter).num >= 2).?(f.style + "-recruiter").|("empty-building-1"), styles.building))) ~
                Hint(rc + "3rd Recruiter: cost 2 Wood, 2 VP, extra card draw", (Image((f.all(Recruiter).num >= 3).?(f.style + "-recruiter").|("empty-building-2-card"), styles.building))) ~
                Hint(rc + "4th Recruiter: cost 3 Wood, 3 VP"                 , (Image((f.all(Recruiter).num >= 4).?(f.style + "-recruiter").|("empty-building-3"), styles.building))) ~
                Hint(rc + "5th Recruiter: cost 3 Wood, 3 VP, extra card draw", (Image((f.all(Recruiter).num >= 5).?(f.style + "-recruiter").|("empty-building-3-card"), styles.building))) ~
                Hint(rc + "6th Recruiter: cost 4 Wood, 4 VP"                 , (Image((f.all(Recruiter).num >= 6).?(f.style + "-recruiter").|("empty-building-4"), styles.building))) ~
                (state.current == f).??(Gap ~ 1.to(3 + f.extra)./(_ => Image("action-black", styles.building)).take(f.acted) ~ (1.to(3)./(_ => Image(f.style + "-action", styles.building)) ++ (0.until(f.extra)./(_ => Image("action-bird", styles.building)))).drop(f.acted)) ~
                Div(Hint("Cats\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-cat-x5", styles.wr) * (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-cat", styles.wr) * (f.all(f.warrior).num % 5))) ~ 
                &((Image(f.style + "-cat-empty", styles.wr) * (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-cat-x5-empty", styles.wr) * (f.pooled(f.warrior) / 5)))), styles.warline)
        
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
                    val id = f.style + "-" + (d == Decree.Recruit && f.leader == Charismatic).??("double-") + d.name
                    c./(s => Image(id + "-" + s.name + "-done", styles.building)) ++ r./(s => Image(id + "-" + s.name, styles.building))
                }

                val dddd = &(&(&(decree(0)) ~ " " ~ &(decree(1))) ~ " " ~ &(decree(2) ~ " " ~ &(decree(3))))
                                
                Option(state.ed(f).leader)./(l => Hint(l.name + " leader\n" + (l match {
                    case Builder => "Full VP crafting items"
                    case Charismatic => "Double warrior recruit"
                    case Commander => "Extra hit when attacking"
                    case Despot => "Extra 1 VP destroying buildings or tokens in battle"
                }), l.name.hl.div(styles.minister))).|("~~~".styled(f)) ~
                Gap ~
                Hint(h, dddd) ~
                Gap ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "1 Roost"                           , (Image((f.all(Roost).num >= 1).?(f.style + "-roost").|("empty-building"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "2 Roosts: 1 VP"                    , (Image((f.all(Roost).num >= 2).?(f.style + "-roost").|("empty-building-1"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "3 Roosts: 2 VP, draw 1 extra card" , (Image((f.all(Roost).num >= 3).?(f.style + "-roost").|("empty-building-2-card"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "4 Roosts: 3 VP, draw 1 extra card" , (Image((f.all(Roost).num >= 4).?(f.style + "-roost").|("empty-building-3"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "5 Roosts: 4 VP, draw 1 extra card" , (Image((f.all(Roost).num >= 5).?(f.style + "-roost").|("empty-building-4"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "6 Roosts: 4 VP, draw 2 extra cards", (Image((f.all(Roost).num >= 6).?(f.style + "-roost").|("empty-building-4-card"), styles.building))) ~
                Hint("Roosts\n" + f.all(Roost).num + " on the map\n" + f.pooled(Roost) + " available" + "\n\n" + "7 Roosts: 5 VP, draw 2 extra cards", (Image((f.all(Roost).num >= 7).?(f.style + "-roost").|("empty-building-5"), styles.building))) ~
                Div(Hint("Hawks\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-hawk-x5", styles.wr) * (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-hawk", styles.wr) * (f.all(f.warrior).num % 5))) ~ 
                &((Image(f.style + "-hawk-empty", styles.wr) * (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-hawk-x5-empty", styles.wr) * (f.pooled(f.warrior) / 5)))), styles.warline)
                
            case f : Insurgent => 
                Gap ~
                FoxRabbitMouse./(s => Hint(s.name + " Base: " + state.wa(f).bases./(_.suit).has(s).?("on the map").|("available") + ", draw 1 extra card", state.wa(f).bases./(_.suit).has(s).?(Image(f.style + "-base-" + s.name, styles.building)).|(Image("empty-building-card", styles.building)))) ~
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
                    state.wa(f).bases.none.?(
                        (Hint("Supporters: " + (f.supporters.num == 0).?("no cards").|((f.supporters.num == 1).?("1 cards").|(f.supporters.num + " cards")) + ", max 5 cards", 
                            dt.CardBackInfo.*(f.supporters.num).merge ~ dt.CardEmptyInfo.*(5 - f.supporters.num).merge, styles.supporters5))
                        ).|(
                            (Hint("Supporters: " + (f.supporters.num == 0).?("no cards").|((f.supporters.num == 1).?("1 cards").|(f.supporters.num + " cards")), 
                            dt.CardEmptyInfo ~ dt.CardBackInfo.*(f.supporters.num).merge ~ dt.CardEmptyInfo, styles.supportersX))
                        ), 
                styles.centered) ~
                Gap ~
                (state.current == f).??(1.to(f.officers.num)./(_ => Image("action-black", styles.building)).take(f.acted) ~ (1.to(f.officers.num)./(_ => Image(f.style + "-action", styles.building))).drop(f.acted)) ~
                Div(Hint("Critters\n" + f.all(f.warrior).num + " on the map\n" + f.officers.num + " officer" + (f.officers.num != 1).??("s") + "\n" + f.pooled(f.warrior) + " available",
                &(&(Image(f.style + "-critter", styles.wr) * f.all(Critter).num) ~ &(Image(f.style + "-critter-empty", styles.wr) * (f.pooled(Critter) / 2)) ~ &(Image(f.style + "-critter-empty", styles.wr) * (f.pooled(Critter) - f.pooled(Critter) / 2))) ~ &(Image(f.style + "-officer", styles.wr) * f.officers.num)), styles.warline)
                
            case f : Fanatic =>
                Gap ~
                FoxRabbitMouse./(s => 
                    (f.outcast == s).??(Image("outcast-" + s + f.hated.??("-hated"), styles.token)) ~ 
                    (Image(f.style + "-garden-" + s.name, styles.building) * f.all(Garden(s)).num) ~ emtb(" |2+| |3|4").drop(f.all(Garden(s)).num) ~ 
                    (f.outcast == s).??(Image("outcast-" + s + f.hated.??("-hated"), styles.token))
                ).join(Break) ~ 
                Gap ~
                Hint("Lost Souls\n" + f.lost.some./(l => FoxRabbitMouse./(s => f.lost.count(_.suit == s) + " " + s.name).join("\n")).|("none"),
                f.lost.some./(_./(_.suit)./(s => dt.CardSuitInfo(s)).merge).|("~~~")) ~ Break ~
                Div(Hint("Lizards\n" + f.acolytes.num + " acolyte" + (f.acolytes.num != 1).??("s") + "\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                    (f.acolytes.num > 0).??((Image(f.style + "-acolyte-x5", styles.wr) * (f.acolytes.num / 5)) ~ (Image(f.style + "-acolyte", styles.wr) * (f.acolytes.num % 5)) ~ Break) ~
                    &((Image(f.style + "-lizard-x5", styles.wr) * (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-lizard", styles.wr) * (f.all(f.warrior).num % 5))) ~
                    &((Image(f.style + "-lizard-empty", styles.wr) * (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-lizard-x5-empty", styles.wr) * (f.pooled(f.warrior) / 5)))), styles.warline)
            
            case f : Trader =>
                val r1 = state.rf(f).payments./(f => f.faction.asInstanceOf[WarriorFaction])
                val r2 = state.rf(f).funds./(f => f.faction.asInstanceOf[WarriorFaction])
                val r3 = state.rf(f).commited./(f => f.faction.asInstanceOf[WarriorFaction])
        
                val tps = FoxRabbitMouse./(s => &(
                    (Image(f.style + "-trade-post-" + s.name + "-destroyed", styles.tokengl) * f.graveyard.get.count(TradePost(s))) ~
                    (Image(f.style + "-trade-post-" + s.name, styles.tokengl) * f.all(TradePost(s)).num) ~
                    (Image("empty-token-2", styles.tokengl) * f.pooled(TradePost(s)))))
                
                Empty.div(styles.moneybox) ~
                Hint("Payments",   &&(dt.Income ~ " ".pre ~ pack(r1, 14, icon))).div(styles.moneybox) ~
                Hint("Funds",      &&(dt.Current ~ " ".pre ~ pack(r2, 14, icon))).div(styles.moneybox) ~
                Hint("Commited",   &&(dt.Swap ~ " ".pre ~ pack(r3, 14, icon))).div(styles.moneybox) ~
                &(&(tps(0)) ~ Span(" ", styles.narrow) ~ &(tps(1)) ~ Span(" ", styles.narrow) ~ &(tps(2))) ~ Break ~
                Gap ~
                (f.prices.values.forall(_ > 0)).?? {
                    (   
                        f.services./(s => Image(f.style + "-price-" + f.prices(s) + "-selected")(styles.price)) ~
                        Break ~
                        f.services./(s => s.img(f)(styles.service))
                    ).div(xstyles.smaller85)
                } ~
                Div(Hint("Otters\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-" + f.warrior + "-x5", styles.wr) * (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-" + f.warrior, styles.wr) * (f.all(f.warrior).num % 5))) ~ 
                &((Image(f.style + "-" + f.warrior + "-empty", styles.wr) * (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-" + f.warrior + "-x5-empty", styles.wr) * (f.pooled(f.warrior) / 5)))), styles.warline)
                
            
            case f : Mischief =>
                Gap ~
                Plot.all./~(p => Image(f.style + "-" + p.name, styles.token) * f.all(p).diff(f.hidden).num) ~
                (Image(f.style + "-plot", styles.token) * f.hidden.num) ~
                (Image("empty-token", styles.token) * (Plot.all./(f.pooled).sum)) ~
                (state.current == f).??(Gap ~ f.exert.?(
                    1.to(3)./(_ => Image("action-black", styles.building)) ~
                    Image("action-bird", styles.building), 
                ).|(
                    1.to(3)./(_ => Image("action-black", styles.building)).take(f.acted) ~
                    1.to(3)./(_ => Image(f.style + "-action", styles.building)).drop(f.acted) ~
                    Image("action-bird", styles.building),
                )) ~
                Div(Hint("Ravens\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-raven-x5", styles.wr) * (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-raven", styles.wr) * (f.all(f.warrior).num % 5))) ~ 
                &((Image(f.style + "-raven-empty", styles.wr) * (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-raven-x5-empty", styles.wr) * (f.pooled(f.warrior) / 5)))), styles.warline)
                
            case f : Underground =>
                Gap ~
                &((Image(f.style + "-tunnel", styles.token) * f.all(Tunnel).num) ~ Image("empty-token", styles.token).repeat(3 - f.all(Tunnel).num)) ~ 
                Break ~
                &((Image(f.style + "-citadel", styles.building) * f.all(Citadel).num) ~ emtb("1p|2p|2p").drop(f.all(Citadel).num)) ~ " " ~
                &((Image(f.style + "-market", styles.building) * f.all(Market).num) ~ emtb("+|+|+").drop(f.all(Market).num)) ~
                Gap ~
                List(2, 3, 4)./(r => Hint(Map(2 -> "Squires", 3 -> "Nobles", 4 -> "Lords")(r), Div((f.ministers.%(_.rank == r).intersect(f.swayed)./(m => f.worked.has(m).?(m.short.spn).|(m.short.hl)) ++ (Image(f.style + "-crown-" + r, styles.building) * (3 - (f.swayed ++ f.retired).%(_.rank == r).num)) ++ (Image(f.style + "-crown-empty", styles.building) * f.retired.%(_.rank == r).num)).join(" ")), styles.minister)) ~
                (state.current == f).??(Gap ~ 1.to(2)./(_ => Image("action-black", styles.building)).take(f.acted) ~ (1.to(2)./(_ => Image(f.style + "-action", styles.building))).drop(f.acted)) ~
                Div(Hint("Moles\n" + f.at(f.burrow).num + " in Burrow\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                f.at(f.burrow).any.??(Gap ~ Div(&(
                    (Image(f.style + "-young-x5", styles.wr) * (f.at(f.burrow).num / 5)) ~ 
                    (Image(f.style + "-young", styles.wr) * (f.at(f.burrow).num % 5))
                ), styles.burrow, styles.inline, styles.get(f)) ~ Break) ~
                &((Image(f.style + "-mole-x5", styles.wr) * (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-mole", styles.wr) * (f.all(f.warrior).num % 5))) ~
                &((Image(f.style + "-mole-empty", styles.wr) * (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-mole-x5-empty", styles.wr) * (f.pooled(f.warrior) / 5)))), styles.warline)
  
            case f : Horde =>
                val iiii = uncrafted ++ factions./~(_.forTrade)./(_.item) ++ ruins.values./~(_.items)
                val iii = iiii.sortBy(i => -(iiii.count(i) + f.forTrade./(_.item).contains(i).??(9)))
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

                val ic = ii.%($(Boots, Bag, Coins).contains).sortBy(i => f.hoard.command.contains(i).??(1))
                val ip = ii.%($(Sword, Hammer, Teapot, Crossbow).contains).sortBy(i => f.hoard.prowess.contains(i).??(1))
                
                (Image(f.style + "-mob", styles.token) * f.all(Mob).num) ~ (Image("empty-token", styles.token) * f.pooled(Mob)) ~ 
                Gap ~
                (Image(f.style + "-stronghold", styles.building) * f.all(Stronghold).num) ~ (Image("empty-building", styles.building) * f.pooled(Stronghold)) ~ Break ~
                f.mood.name.hl.div(styles.minister) ~
                Div(
                    Div(Image("satchel-top-2" + (f.command == 2).??("-" + f.style), styles.ii) ~ Image("satchel-top", styles.ii) ~ Image("satchel-top-3" + (f.command == 3).??("-" + f.style), styles.ii) ~ Image("satchel-top-4" + (f.command == 4).??("-" + f.style), styles.ii), styles.satchel) ~
                    Div(Image("satchel-left-1" + (f.command == 1).??("-" + f.style), styles.iih) ~ 
                        f.hoard.command./(i => Image("item-" + i.name, styles.ii)) ~ (ic./(i => Image("item-" + i.name + "-empty", styles.ii)) ++ Image("item-x-placeholder", styles.ii).repeat(4)).take(4).dropRight(f.hoard.command.num) ~
                        Image("satchel-right-empty", styles.iih)
                    ) ~
                    Div(Image("satchel-middle-start", styles.ii) ~ Image("satchel-middle-cont", styles.ii).repeat(3), styles.satchel) ~
                    Div(Image("satchel-left-1" + (f.prowess == 1).??("-" + f.style), styles.iih) ~ 
                        f.hoard.prowess./(i => Image("item-" + i.name, styles.ii)) ~ (ip./(i => Image("item-" + i.name + "-empty", styles.ii)) ++ Image("item-x-placeholder", styles.ii).repeat(4)).take(4).dropRight(f.hoard.prowess.num) ~
                        Image("satchel-right-empty", styles.iih)
                    ) ~
                    Div(Image("satchel-bottom-2" + (f.prowess == 2).??("-" + f.style), styles.ii) ~ Image("satchel-bottom", styles.ii) ~ Image("satchel-bottom-3" + (f.prowess == 3).??("-" + f.style), styles.ii) ~ Image("satchel-bottom-4" + (f.prowess == 4).??("-" + f.style), styles.ii), styles.satchel)
                , xstyles.smaller85) ~
                (state.current == f).??(f.actionInfo(styles.building, styles.building)) ~
                Div(Hint("Rats\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-rat-x5", styles.wr) * (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-rat", styles.wr) * (f.all(f.warrior).num % 5))) ~ 
                &((Image(f.style + "-rat-empty", styles.wr) * (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-rat-x5-empty", styles.wr) * (f.pooled(f.warrior) / 5)))), styles.warline)
                
            case f : Expedition =>
                (f.wst.%(w => f.all(w).any)./(_.imgid(f)) ++ "empty-building-card".repeat(3 - f.wst.%(w => f.all(w).any).num))./(Image(_, styles.building)).merge ~ Break ~
                Retinue.all./(r => f.complete(r)./(d => f.style + "-" + r.id + "-" + d.suit.name + "-done") ++ f.retinue(r)./(d => f.style + "-" + r.id + "-" + d.suit.name)).%(_.any)./(_./(Image(_, styles.building)).merge)./(&).join(" ") ~ Break ~
                Relic.types./(rt => (f.recovered(rt)./(r => r.imgid) ++ "empty-token".repeat(4 - f.recovered(rt).num))./(Image(_, styles.token)))./(&).join(Break) ~
                Div(Hint("Badgers\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-badger-x5", styles.wr) * (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-badger", styles.wr) * (f.all(f.warrior).num % 5))) ~ 
                &((Image(f.style + "-badger-empty", styles.wr) * (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-badger-x5-empty", styles.wr) * (f.pooled(f.warrior) / 5)))), styles.warline)
                
            case f : OldExpedition =>
                Hint("Caravans\n" + f.all(Caravan).num + " on the map\n" + f.pooled(Caravan) + " available",
                    (Image(f.style + "-caravan", styles.token) * f.all(Caravan).num) ~ (Image("empty-token", styles.token) * f.pooled(Caravan))) ~ Break ~
                Hint("Stations\n" + f.all(Station).num + " on the map\n" + f.pooled(Station) + " available",
                    (Image(f.style + "-station", styles.building) * f.all(Station).num) ~ (Image("empty-building", styles.building) * f.pooled(Station))) ~ Break ~
                Hint("Mission: " + f.mission.?./(_.name).|("not chosen yet"),
                   f.mission.?./(_.name).|("~~~").styled(f)) ~ Break ~
                OldRelics.all./(r => 
                    List(Fox, Mouse, Rabbit, Bird)./(s => 
                        Image(f.rewards.contains((r, s)).?("empty-token-" + s.name).|(f.style + "-" + r.name), styles.token)).merge).join(Break) ~ Break ~ 
                Div(Hint("Badgers\n" + f.all(f.warrior).num + " on the map\n" + f.pooled(f.warrior) + " available",
                &((Image(f.style + "-badger-x5", styles.wr) * (f.all(f.warrior).num / 5)) ~ (Image(f.style + "-badger", styles.wr) * (f.all(f.warrior).num % 5))) ~ 
                &((Image(f.style + "-badger-empty", styles.wr) * (f.pooled(f.warrior) % 5)) ~ (Image(f.style + "-badger-x5-empty", styles.wr) * (f.pooled(f.warrior) / 5)))), styles.warline)
                
            case f : Hero =>
                val track = f.inv.intersect(Item.track).sort./(_.item)
                val satchel = f.inv.diff(Item.track).sort
                val columns = 3 + track.count(Bag)
                val (row1, row2) = satchel.splitAt((satchel.num + 1) / 2)
                
                Gap ~
                f.quests.some./(_./(s => Image("quest-" + s.name, styles.token)).merge).|("~~~") ~ Break ~
                state.setup./~(o => f.attitude.get(o)).but(Indifferent).any.?(Div(state.setup./~(o => f.attitude.get(o).but(Indifferent)./(a => &(Image((o.short + "-" + o.asInstanceOf[WarriorFaction].warrior.name), styles.fund) ~ (a match {
                    case Hostile => Image("attitude-hostile", styles.attitude)
                    case Amiable => Image("attitude-heart-full", styles.attitude) ~ (Image("attitude-heart-empty", styles.attitude) * f.aid.get(o).|(0))
                    case Friendly => (Image("attitude-heart-full", styles.attitude) * 2) ~ (Image("attitude-heart-empty", styles.attitude) * f.aid.get(o).|(0))
                    case Allied => (Image("attitude-heart-full", styles.attitude) * 3) ~ Empty
                })))).join(" |  ".pre.styled(styles.narrow)), styles.warline)).|("~~~" ~ Break) ~
                Gap ~
                Div(
                    track.but(Bag).any.??(Div(track.but(Bag)./(i => Image(i.imgid, styles.ii)).merge)) ~
                    Div((Image("satchel-top", styles.ii) * columns).merge, styles.satchel) ~
                    Div(row1./(i => Image(i.imgid, styles.ii)) ~ (Image("item-x-placeholder", styles.ii) * (columns - row1.num))) ~
                    Div(row2./(i => Image(i.imgid, styles.ii)) ~ (Image("item-x-placeholder", styles.ii) * (columns - row2.num))) ~
                    Div((Image("satchel-bottom", styles.ii) * columns).merge, styles.satchel) ~
                    track.%(_ == Bag).any.??(Div((Image("item-x-spacer", styles.ii) * 3) ~ track.%(_ == Bag)./(i => Image(i.imgid, styles.ii))))
                , xstyles.smaller85)
        }
        
        val s = Gap ~ name ~ vphand ~ fs.div(styles.warline) ~ effects ~ trade
        
        container.replace(s.div, nameres, {
            case "fun-name" => 
                if (math.random() > 0.5)
                    game.ui.funNames = game.ui.funNames.not
                else
                    game.ui.rules = game.ui.rules.not

                updateStatus()
        })

        if (f == state.current)
            container.attach.parent.style.outline = "0.2ch solid #aaaaaa"
        else
        if (state.highlightFaction.has(f))
            container.attach.parent.style.outline = "0.2ch dashed #aaaaaa"
        else
            container.attach.parent.style.outline = ""
    }

    def overlayScrollX(e : Elem) = overlayScroll(e)(styles.seeThroughInner).onClick
    def overlayFitX(e : Elem) = overlayFit(e)(styles.seeThroughInner).onClick
    
    def showOverlay(e : Elem, onClick : Any => Unit) {
        overlay = true

        overlayPane.show()

        overlayPane.replace(e, resources, onClick)
    }

    def onClick : Any => Unit = {
        case GameOverSaveReplayAction =>
            showOverlay(overlayScrollX("Saving Replay...".hl.div).onClick, null)
                
            saveReplay(onClick(Nil))
            
        case "view-discard" => 
            showOverlay(overlayScrollX(Div("Discard Pile") ~ 
                state.pile./{ d => OnClick(d, Div(d.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)

        case "view-quests" => 
            showOverlay(overlayScrollX(Div("Quests") ~ 
                state.quests.take(3)./{ q => OnClick(q, Div(q.img, xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined)) }.merge), onClick)
                
        case "view-deck" => 
            showOverlay(overlayScrollX(Div("Draw Deck") ~ 
                1.to(state.deck.num)./{ n => Div(Image("card-back-large", styles.card), xstyles.info, xstyles.xx, xstyles.chm, xstyles.chp, styles.inline, styles.margined) }.merge), onClick)

        case "view-items" => 
            val craftable = List(Bag, Boots, Crossbow, Sword, Teapot, Coins, Bag, Boots, Hammer, Sword, Teapot, Coins)
            var available = state.uncrafted
        
            var uncrafted : List[Elem] = Nil
        
            craftable.foreach { c =>
                if (available.has(c)) {
                    available :-= c
                    uncrafted :+= Image("item-" + c.name, styles.iii)
                }
                else
                    uncrafted :+= Image("item-" + c.name + "-empty", styles.iii)
            }
            
            val items = Div(uncrafted.take(6) ~ Break ~ uncrafted.drop(6), xlo.pointer, styles.margined)

            showOverlay(overlayScrollX(Div("Craftable Items") ~ items), onClick)
            
        case d : DeckCard => 
            showOverlay(overlayFitX(Image("artwork:" + d.id, xstyles.artwork)), onClick)

        case q : Quest => 
            showOverlay(overlayFitX(Image("artwork:" + q.id, xstyles.artwork)), onClick)

        case l : Leader => 
            showOverlay(overlayFitX(Image("ed-" + l.name, xstyles.artwork)), onClick)

        case c : Character => 
            showOverlay(overlayFitX(Image("vb-char-" + c.name, xstyles.artwork)), onClick)
 
        case Nil => 
            overlay = false

            overlayPane.hide()

            overlayPane.clear()

        case x => 
            println("unknown onClick: " + x)
    }

    def gameStatus(container : Container) {
        val hor = container.node.clientWidth > container.node.clientHeight * 3 + 1
        
        val quests = state.quests.any.??(OnClick("view-quests", state.quests.take(3)./(q => Image("quest-" + q.suit.name, styles.token) ~ " " ~ q.elem).join(Break).div(xlo.pointer)(hor.not.?(styles.centerquest))))
        
        val discard = 
            (
                (state.deck.num.formatted("%2d").pre.hl ~ Image("deck", styles.pile)).spn(xlo.pointer).onClick("view-deck") ~ 
                Gap ~
                (state.pile.num.formatted("%2d").pre.hl ~ Image("pile-" + state.pile.any.?(state.pile.last.suit).|("empty"), styles.pile)).spn(xlo.pointer).onClick("view-discard")
            ).div(xlo.flexhcenter)(hor.?(styles.verdeck))
        
        val dominances = state.dominances./(d => OnClick(d, Image(d.suit.name + "-dominance", styles.dominance, xlo.pointer))).merge

        val craftable = List(Bag, Boots, Crossbow, Sword, Teapot, Coins, Bag, Boots, Hammer, Sword, Teapot, Coins)
        var available = state.uncrafted

        var uncrafted : List[Elem] = Nil

        craftable.foreach { c =>
            if (available.has(c)) {
                available :-= c
                uncrafted :+= Image("item-" + c.name, styles.ii)
            }
            else
                uncrafted :+= Image("item-" + c.name + "-empty", styles.ii)
        }
        
        val items = (uncrafted.take(6) ~ Break ~ uncrafted.drop(6)).div(xlo.pointer).onClick("view-items")

        val s = $(items, quests, discard, dominances).but(Empty)./(_.div(styles.skipline)).merge
     
        container.replace((s.div(xlo.flexhcenter)(styles.gstatus)), resources, onClick)
    }
    
    def updateStatus() {
        0.until(game.arity).foreach { n =>
            if (game.fseating.contains(n))
                factionStatus(game.fseating(n), statusBitmaps(n))
            else
            if (game.seating.any)
                statusBitmaps(n).replace(Div(Div(resources.getName(game.seating(n)).|(game.seating(n).name).hh), styles.smallname, xlo.pointer), resources)
        }
        gameStatus(statusGame)
        gameStatus(statusGameX)
        
        if (overlay)
            overlayPane.show()
        else
            overlayPane.hide()

        drawMap()
    }

    var highlight : Option[UserAction] = None

    override def updateHighlight(a : Option[UserAction]) {
    }
        
    mapSmall.attach.parent.ondblclick = (e) => {
        if (fullscreen)
            dom.document.exitFullscreen()
        else
            dom.document.documentElement.requestFullscreen()

        fullscreen = fullscreen.not
    }
    
    drawMap()
    
    val layouts = $(Layout("base", $(
        BasicPane("status", 12.2, 16.4, Priorities(top = 2, right = 2, maxXscale = 1.8, maxYscale = 1.8)),
        BasicPane("status-game-a", 10.5, 10, Priorities(grow = -1, maxXscale = 1.8, maxYscale = 1.4)),
        BasicPane("status-game-b", 35, 4, Priorities(top = 1, grow = -1, maxXscale = 1.8, maxYscale = 1.4)),
        BasicPane("log", 30, 10, Priorities(grow = 1, bottom = 1, top = -1, right = 1)),
        BasicPane("map-small", 60*0.82, 55*0.82, Priorities(top = 3, left = 1, grow = -1, maxXscale = 1.2, maxYscale = 1.2)),
        BasicPane("action-a", 48, 22, Priorities(bottom = 2, right = 2, grow = 2)),
        BasicPane("action-b", 41, 38, Priorities(bottom = 2, right = 2, grow = 1, maxXscale = 1.2))
    )))./~(l =>
        l.copy(name = l.name + "-fulldim", panes = l.panes./{
            case p : BasicPane if p.name == "map-small" => FullDimPane(p.name, p.kX, p.kY, p.pr)
            case p => p
        }, boost = 1.1) :: 
        l.copy(name = l.name + "-plus20", panes = l.panes./{
            case p : BasicPane if p.name == "map-small" => BasicPane(p.name, p.kX * 1.2, p.kY * 1.2, p.pr)
            case p => p
        }, boost = 1.05) ::
        l.copy(name = l.name + "-normal")
    )./~(l =>
        l.copy(name = l.name + "-verdouble", boost = l.boost * 0.99, panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-verdouble", kY = p.kY * ((state.arity + 1) / 2), kX = p.kX * 2))
            case p : BasicPane if p.name.startsWith("status-game") && state.arity % 2 == 1 => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-hordouble", boost = l.boost * 0.99, panes = l.panes./~{
            case p : BasicPane if p.name == "status" => Some(p.copy(name = "status-hordouble", kX = p.kX * ((state.arity + 1) / 2), kY = p.kY * 2))
            case p : BasicPane if p.name.startsWith("status-game") && state.arity % 2 == 1 => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-horizontal", boost = l.boost * 1.02, panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-horizontal", kX = p.kX * state.arity)                                         
            case p => p
        }) ::
        l.copy(name = l.name + "-vertical", panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-vertical", kY = p.kY * state.arity)
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
    )./~(l => 
        l.copy(name = l.name + "-gameStatusA", panes = l.panes./~{
            case p : BasicPane if p.name == "status-game-a" => Some(p.copy(name = "status-game"))
            case p : BasicPane if p.name == "status-game-b" => Nil
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-gameStatusB", boost = l.boost * 1.02, panes = l.panes./~{
            case p : BasicPane if p.name == "status-game-a" => Nil
            case p : BasicPane if p.name == "status-game-b" => Some(p.copy(name = "status-game"))
            case p => Some(p)
        }) ::
        Nil
    )
    
    val layouter = Layouter(layouts.%(l => hrf.HRF.param("layout").none || hrf.HRF.param("layout").get == l.name), _./~{
        case f if f.name == "map-small" => $(f, f.copy(name = "overlay"))
        case f if f.name == "status-horizontal" => 1.to(state.arity)./(n => f.copy(name = "status-" + n, x = f.x + ((n - 1) * f.width  / state.arity).round.toInt, width  = (n * f.width  / state.arity).round.toInt - ((n - 1) * f.width  / state.arity).round.toInt))
        case f if f.name == "status-vertical"   => 1.to(state.arity)./(n => f.copy(name = "status-" + n, y = f.y + ((n - 1) * f.height / state.arity).round.toInt, height = (n * f.height / state.arity).round.toInt - ((n - 1) * f.height / state.arity).round.toInt))
        case f if f.name == "status-hordouble"  => 
            val c = ((state.arity + 1) / 2)
            1.to(c * 2)./(n => f.copy(name = "status-" + (n > state.arity).?("game").|(n.toString), 
                x = f.x + (((n - 1) % c) * f.width / c).round.toInt, 
                width = (n * f.width / c).round.toInt - ((n - 1) * f.width / c).round.toInt, 
                y = f.y + (n - 1) / c * (f.height / 2), 
                height = (n > c).?(f.height - f.height / 2).|(f.height / 2))
            )
        case f if f.name == "status-verdouble"  => 
            val c = ((state.arity + 1) / 2)
            1.to(c * 2)./(n => f.copy(name = "status-" + (n > state.arity).?("game").|((((n - 1) % c) * 2 + (n - 1) / c + 1).toString),
                y = f.y + (((n - 1) % c) * f.height / c).round.toInt, 
                height = (n * f.height / c).round.toInt - ((n - 1) * f.height / c).round.toInt, 
                x = f.x + (n - 1) / c * (f.width / 2), 
                width = (n > c).?(f.width - f.width / 2).|(f.width / 2))
            )
        case f => $(f)
    })

    def layout(width : Int, height : Int) : List[(String, Rect, Option[Double])] = {
        val lr = layouter.get(width, height)
        
        lr.panes./(p => (p.name, Rect(p.x, p.y, p.width, p.height), Some(lr.fontSize)))
    }
            
    override def info(self : Option[Player], aa : List[UserAction]) = {
        val ii = game.info(Nil, self, aa)
        ii.any.??(ZOption(Empty, Break) +: convertActions(self.of[Faction], ii))
    }
    
    override def preinfo(self : Option[Player], aa : List[UserAction]) = {
        val ii = game.preinfo(Nil, self, aa)
        ii.any.??(convertActions(self, ii))
    }
    
    def notifyX(faction : Player, notifies : List[Notify]) {
        overlayPane.show()

        overlayPane.attach.clear()
        
        val ol = overlayPane.attach.appendContainer(overlayScrollX(Content), resources, onClick)

        val asker = new NewAsker(ol, resources.getImage)

        asker.zask(notifies./~(n => convertActions(Option(faction), n.infos) :+ ZOption(Empty, Break)))(resources)
    }
    
    override def wait(self : List[Player], factions : List[Player], notifies : List[Notify]) {
        def fix(f : Player) = f @@ {
            case f : PlayerN => state.ptf.get(f).|(f)
            case f => f
        }

        super.wait(self./(fix), factions./(fix), notifies)
    }
        
    override def ask(faction : Player, actions : List[UserAction], waiting : List[Player], notifies : List[Notify], then : UserAction => Unit) {
        if (notifies.any)
            notifyX(faction, notifies)
            
        super.ask(faction, actions, waiting, Nil, then)
    }
    
    override def styleAction(faction : Option[F], actions : List[UserAction], a : UserAction, unavailable : Boolean, view : Option[Any]) : List[Style] =
        view @@ {
            case _ if unavailable.not => Nil
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
        $(xstyles.xx, xstyles.chm, xstyles.chp) ++
        faction @@ {
            case Some(f : Faction) => $(elem.borders.get(f))
            case Some(f : PlayerN) => state.ptf.get(f)./(elem.borders.get)
            case _ => Nil             
        } ++
        a @@ {
            case a : Selectable if a.selected => $(styles.selected)
            case _ => Nil
        } ++
        view @@ {
            case Some(_ : Character)  => $(styles.inline, styles.charback) ++ a.selected.??($(styles.selchar))
            case Some(_ : PlayChoice) => $(styles.inline, styles.charback) ++ a.selected.??($(styles.selchar))
            case Some(_ : Figure)                    => $(styles.inline, styles.quasi) ++ a.selected.??($(styles.selfigure))
            case Some(x : QuasiItem) if x.figure.any => $(styles.inline, styles.quasi) ++ a.selected.??($(styles.selfigure))
            case Some(_ : QuasiItem)                 => $(styles.inline, styles.quasi) ++ a.selected.??($(styles.selquasi))
            case Some(_) => $(styles.inline)
            case _ => $(xstyles.thu, xlo.fullwidth)
        } ++
        a @@ {
            case _ if unavailable => Nil
            case _ : Extra[_] => Nil
            case _ : Choice | _ : Cancel | _ : Back | _ : OnClickInfo => $(xlo.pointer)
            case _ => Nil
        }
}

