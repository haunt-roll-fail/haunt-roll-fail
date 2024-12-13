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

import hrf.serialize._
import hrf.reflect._

import fastparse._, NoWhitespace._


object Serialize extends Serializer {
    val gaming = root.gaming

    def writeFaction(f : gaming.F) = f @@ {
        case f : Meta.F => Meta.writeFaction(f)
        case f : MetaAdset.F => MetaAdset.writeFaction(f)
    }

    def parseFaction(s : String) = Meta.parseFaction(s) || MetaAdset.parseFaction(s)

    def parseRootFaction(s : String) = Meta.parseFaction(s)
    def parseHireling(s : String) = Meta.parseFaction(s)

    val prefix = "root."

    override def write(o : Any) : String = o match {
        case r : Burrow => r.name + "(" + write(r.faction) + ")"
        case r : Region => r.id
        case f : Faction => f.short
        case p : Figure => p.faction.short + "/" + write(p.piece) + "/" + p.index
        case l : Leader => l.name
        case c : Character => c.toString
        case s : Suit => s.name
        case d : Decree => d.name
        case m : Minister => m.toString
        case i : Item => i.name
        case i : ItemRef => "%" + i.exhausted.??("%") + i.damaged.??("%%") + i.item.name
        case d : DeckCard => "#" + Deck.catalog.indexOf(d)
        case q : Quest => "&" + Quest.all.indexOf(q)
        case _ => super.write(o)
    }

    case class EElderSign(value : Int) extends Expr
    case class ECard(value : Int) extends Expr
    case class EQuest(value : Int) extends Expr
    case class EOffer(a : String, b : Int) extends Expr
    case class EItemRef(a : String, b : String) extends Expr
    case class EUnitRef(a : String, b : String, c : Int) extends Expr
    case class EUnitRefP(a : String, b : EApply, c : Int) extends Expr

    def es[T : P] = P( "$" ~ ("0" | "1" | "2" | "3").! ).map(_.toInt).map(EElderSign)

    def card[T : P] = P( "#" ~ number ).map(o => ECard(o.value))

    def quest[T : P] = P( "&" ~ number ).map(o => EQuest(o.value))

    def offer[T : P] = P( symbol ~ "->" ~ number ).map(o => EOffer(o._1.value, o._2.value))

    def itemref[T : P] = P( ("%%%%" | "%%%" | "%%" | "%").! ~ symbol ).map(o => EItemRef(o._1, o._2.value))

    def unitrefp[T : P] = P( symbol ~ "/" ~ action ~ "/" ~ number ).map(o => EUnitRefP(o._1.value, o._2, o._3.value))

    def unitref[T : P] = P( symbol ~ "/" ~ symbol ~ "/" ~ number ).map(o => EUnitRef(o._1.value, o._2.value, o._3.value))

    override def expr[T : P] : P[Expr] = P( space ~ ( unitrefp | unitref | itemref | offer | es | card | quest | base ) ~ space )

    override def parseExpr(e : Expr) : Any = e match {
        case ESymbol("Any") => AnySuit
        case ESymbol("OnStart") => OnSetup

        case ESymbol("North") => AutumnBoard.AutumnN
        case ESymbol("East") => AutumnBoard.AutumnE
        case ESymbol("NorthEast") => AutumnBoard.Witchwood
        case ESymbol("NorthWest") => AutumnBoard.AutumnNW
        case ESymbol("South") => AutumnBoard.AutumnS
        case ESymbol("SouthWest") => AutumnBoard.AutumnSW
        case ESymbol("West") => AutumnBoard.AutumnW

        case ESymbol(s) => parseFaction(s) || parseRegion(s) || parseDecree(s) || parseSymbol(s) | super.parseExpr(e)
        case ECard(v) => Deck.catalog(v)
        case EQuest(v) => Quest.all(v)
        case EUnitRef(a, b, c) => Figure(parseRootFaction(a).||(parseHireling(a)).|!("can't parse faction " + a), parseSymbol(b).get.asInstanceOf[Piece], c)
        case EUnitRefP(a, b, c) => Figure(parseRootFaction(a).||(parseHireling(a)).|!("can't parse faction " + a), parseExpr(b).asInstanceOf[Piece], c)
        case EItemRef(a, b) => ItemRef(parseItem(b).get, a.length % 2 == 0, a.length > 2)

        case _ => super.parseExpr(e)
    }

    override def parseAction(s : String) = {
        val r = super.parseAction(s)

        r
    }

    def parseRegion(s : String) : Option[Region] = $(AutumnBoard, WinterBoard, LakeBoard, MountainBoard, TidalBoard, TundraBoard, GloomBoard)./~(_.regions).%(_.id == s).single

    def parseDecree(s : String) : Option[Decree] = Decree.all.%(_.name == s).single

    def parseItem(s : String) : Option[Item] = parseSymbol(s).map(_.asInstanceOf[Item])

}
