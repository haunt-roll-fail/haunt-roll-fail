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

import hrf.serialize._

object Serialize extends Serializer {
    val gaming = vast.gaming

    def writeFaction(f : F) = Meta.writeFaction(f)
    def parseFaction(s : String) = Meta.parseFaction(s)

    val prefix = "vast."

    override def write(o : Any) : String = o match {
        case _ => super.write(o)
    }

    override def parseAction(s : String) = {
        val r = super.parseAction(s)

        r
    }

}
