package cthw
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

import cthw.gaming._

import hrf.serialize._
import hrf.reflect._

import fastparse._, NoWhitespace._

object Serialize extends Serializer {
    val gaming = cthw.gaming

    def writeFaction(f : gaming.F) = Meta.writeFaction(f)

    def parseFaction(s : String) = Meta.parseFaction(s)

    def parseRootFaction(s : String) = Meta.parseFaction(s)

    val prefix = "cthw."

    override def write(o : Any) : String = o match {
        case _ => super.write(o)
    }

    var version : String = null

    override def parseAction(s : String) = {
        val r = super.parseAction(s)

        r
    }
}
