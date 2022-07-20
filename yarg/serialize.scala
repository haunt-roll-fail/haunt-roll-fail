package yarg

import colmat._

import hrf.serialize._

object Serialize extends Serializer {
    val gaming = yarg.gaming

    def writeFaction(f : F) = Meta.writeFaction(f)
    def parseFaction(s : String) = Meta.parseFaction(s)

    val prefix = "yarg."
}
