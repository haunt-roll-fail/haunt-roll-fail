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

import hrf.serialize._

import fastparse._, NoWhitespace._

object Serialize extends Serializer {
    val gaming = arcs.gaming

    def writeFaction(f : F) = Meta.writeFaction(f)
    def parseFaction(s : String) = Meta.parseFaction(s)

    val prefix = "arcs."

    override def write(o : Any) : String = o match {
        case p : Figure => p.faction.short + "/" + write(p.piece) + "/" + p.index
        case c : Color => c.name
        case _ => super.write(o)
    }

    case class EUnitRef(a : String, b : String, c : Int) extends Expr
    case class EUnitRefP(a : String, b : EApply, c : Int) extends Expr

    def unitref[T : P] = P( symbol ~ "/" ~ symbol ~ "/" ~ number ).map(o => EUnitRef(o._1.value, o._2.value, o._3.value))
    def unitrefp[T : P] = P( symbol ~ "/" ~ action ~ "/" ~ number ).map(o => EUnitRefP(o._1.value, o._2, o._3.value))

    override def expr[T : P] : P[Expr] = P( space ~ ( unitrefp | unitref | base ) ~ space )

    override def parseExpr(e : Expr) : Any = e match {
        case EUnitRef(a, b, c) => Figure(parseFaction(a).|!("can't parse faction " + a), parseSymbol(b).get.asInstanceOf[Piece], c)
        case EUnitRefP(a, b, c) => Figure(parseFaction(a).|!("can't parse faction " + a), parseExpr(b).asInstanceOf[Piece], c)

        case ESymbol("Intersept") => parseExpr(ESymbol("Intercept"))

        case _ => super.parseExpr(e)
    }

    override def parseAction(s : String) : Action = {
        val r = super.parseAction(s)

        r
    }


}
