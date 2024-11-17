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

import hrf.serialize._

object Serialize extends Serializer {
    val gaming = dwam.gaming

    def writeFaction(f : F) = Meta.writeFaction(f)
    def parseFaction(s : String) = Meta.parseFaction(s)

    val prefix = "dwam."

    override def write(o : Any) : String = o match {
        case c : Color => c.name
        case _ => super.write(o)
    }

    override def parseAction(s : String) = {
        val r = super.parseAction(s)

        r
    }

}
