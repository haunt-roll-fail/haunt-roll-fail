package cthw

import cthw.gaming._

import colmat._

import hrf.serialize._
import hrf.reflect._

import fastparse._, NoWhitespace._

object Serialize extends Serializer {
    val gaming = cthw.gaming
  
    def writeFaction(f : gaming.F) = Meta.writeFaction(f)

    def parseFaction(s : String) = Meta.parseFaction(s)

    def parseRootFaction(s : String) = Meta.parseFaction(s)
    
    val prefix = "cthw."

    override def write(o : Any)(implicit tag : scala.reflect.ClassTag[gaming.F]) : String = o match {
        case _ => super.write(o)
    }

    var version : String = null

    override def parseAction(s : String) = {
        val r = super.parseAction(s)
 
        r
    }

    //def parseRegion(s : String) : Option[Region] = List(AutumnBoard, WinterBoard, LakeBoard, MountainBoard)./~(_.regions).%(_.name == s).single
    
    //def parseDecree(s : String) : Option[Decree] = Decree.all.%(_.name == s).single

    //def parseItem(s : String) : Option[Item] = parseSymbol(s).map(_.asInstanceOf[Item])
}
