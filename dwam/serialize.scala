package dwam

import colmat._

import hrf.serialize._

object Serialize extends Serializer {
    val gaming = dwam.gaming

    def writeFaction(f : F) = Meta.writeFaction(f)
    def parseFaction(s : String) = Meta.parseFaction(s)

    val prefix = "dwam."
}
