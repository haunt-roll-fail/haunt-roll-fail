package suok

import colmat._

import hrf.serialize._

object Serialize extends Serializer {
    val gaming = suok.gaming

    def writeFaction(f : F) = Meta.writeFaction(f)
    def parseFaction(s : String) = Meta.parseFaction(s)
    
    val prefix = "suok."
}
