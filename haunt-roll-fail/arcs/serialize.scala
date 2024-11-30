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

        case _ => super.parseExpr(e)
    }

    override def parseAction(s : String) : Action = {
        // 0.8.108 --> 0.8.109
        // if (s.startsWith("AssignHitsAction"))
        //     return CommentAction(s)

        val r = super.parseAction(s
            // 0.8.106 --> 0.8.108
            .replace("UsedCourtCardAction", "UsedEffectCardAction")
            // 0.8.108 --> 0.8.109
            .use { s =>
                if (s.startsWith("AssignHitsAction"))
                    "ForceInvalidAction(DoAction(" + s + "))"
                else
                    s
            }
            // 0.8.108 --> 0.8.109
            .use { s =>
                val deal = "DealHitsAction"
                val assign = "AssignHitsAction"
                val battle = "BattleRaidAction"
                val main = "MainTurnAction"
                val prelude = "PreludeActionAction"

                var r = s

                if (r.contains(main) && r.contains(battle)) {
                    if (r.contains(deal) && r.contains(assign).not) {
                        val $(a, b) = r.splt(battle)
                        val $(c, d) = b.splt(main)

                        val $(_, _, _, _, raid, _) = c.splt(",")

                        r = a + raid + ", " + main + d.dropRight(1)
                    }

                    if (r.contains(deal) && r.contains(assign)) {
                        val $(a, b) = r.splt(assign)

                        r = a + "0, " + assign + b
                    }

                    if (r.contains(assign)) {
                        val $(a, b) = r.splt(assign)
                        val $(c, d) = b.splt(battle)
                        val $(e, f) = d.splt(main)
                        val $(_, _, _, _, raid, _) = e.splt(",")

                        r = a + assign + c.dropRight(1) + raid + ", " + main + f.dropRight(1)
                    }

                    if (r.contains(battle)) {
                        val $(a, b) = r.splt(battle)
                        val $(c, d) = b.splt(main)

                        val $(_, _, _, _, raid, _) = c.splt(",")

                        if (raid == " 0")
                            r = a + main + d.dropRight(1)
                    }
                }

                if (r.contains(prelude) && r.contains(battle)) {
                    if (r.contains(deal) && r.contains(assign).not) {
                        val $(a, b) = r.splt(battle)
                        val $(c, d) = b.splt(prelude)

                        val $(_, _, _, _, raid, _) = c.splt(",")

                        r = a + raid + ", " + prelude + d.dropRight(1)
                    }

                    if (r.contains(deal) && r.contains(assign)) {
                        val $(a, b) = r.splt(assign)

                        r = a + "0, " + assign + b
                    }

                    if (r.contains(assign)) {
                        val $(a, b) = r.splt(assign)
                        val $(c, d) = b.splt(battle)
                        val $(e, f) = d.splt(prelude)
                        val $(_, _, _, _, raid, _) = e.splt(",")

                        r = a + assign + c.dropRight(1) + raid + ", " + prelude + f.dropRight(1)
                    }

                    if (r.contains(battle)) {
                        val $(a, b) = r.splt(battle)
                        val $(c, d) = b.splt(prelude)
                        val $(_, _, _, _, raid, _) = c.splt(",")

                        if (raid == " 0")
                            r = a + prelude + d.dropRight(1)
                    }
                }

                r
            }
        )

        r
    }

}
