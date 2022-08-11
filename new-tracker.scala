package hrf.tracker2
//
//
//
//
import logger._, colmat._
//
//
//
//

trait Tracker[T] {
    def get(location : Location[T]) : List[T]
    def moveList(old : Location[T], entities : List[T], location : Location[T])
}

class ValueTracker[T] extends Tracker[T] {
    private var locations : List[Location[T]] = Nil
    private var accepts : Map[Location[T], T => Boolean] = Map()
    private var validates : Map[Location[T], List[T] => Boolean] = Map()
    private var l2e : Map[Location[T], List[T]] = Map()
    
    def list(key : Any = "*", content : List[T] = Nil, accept : T => Boolean = (e : T) => true, validate : List[T] => Boolean = (l : List[T]) => true) : ListLocation[T] = {
        val l = new ListLocation[T](key, this)
        register(l, content, accept, validate)
        l
    }
    
    def slot(key : Any = "*", content : Option[T] = None, accept : T => Boolean = (e : T) => true) : SlotLocation[T] = {
        val l = new SlotLocation[T](key, this)
        register(l, content.toList, accept, e => e.num <= 1)
        l
    }
    
    def register(location : Location[T], content : List[T] = Nil, accept : T => Boolean = (e : T) => true, validate : List[T] => Boolean = (l : List[T]) => true) {
        if (locations.contains(location))
            throw new Error("location already registered " + location)
            
        content.foreach { e => 
            if (!accept(e))
                throw new Error("entity doesn't match rule " + e + " at " + location)
        }

        if (!validate(content))
            throw new Error("entities doesn't match rule " + content + " at " + location)

            
        locations :+= location
        
        accepts += location -> accept
        validates += location -> validate

        l2e += location -> content
    }
    /*
    def move(old : Location[T], entity : T, location : Location[T]) {
        if (!locations.contains(old))
            throw new Error("location not registered " + old)
            
        if (!locations.contains(location))
            throw new Error("location not registered " + location)
            
        if (!accepts(location)(entity))
            throw new Error("entity doesn't match rule " + entity + " at " + location)
            
        if (!l2e(old).contains(entity)) {
            dump()
            throw new Error("old location doesn't have entity " + entity)
        }

        l2e += (old -> l2e(old).diff($(entity)))
        l2e += location -> (l2e(location) :+ entity)
    }
    */

    def moveList(old : Location[T], entities : List[T], location : Location[T]) {
        if (!locations.contains(old))
            throw new Error("location not registered old " + old)
            
        if (!locations.contains(location))
            throw new Error("location not registered " + location)
            
        if (entities.diff(l2e(old)).any) {
            dump()
            throw new Error("old location doesn't have all entities " + entities)
        }
            
        entities.foreach { entity =>
            if (!accepts(location)(entity))
                throw new Error("entity doesn't match rule " + entity + " at " + location)
        }
            
        l2e += old -> l2e(old).diff(entities)
        l2e += location -> (l2e(location) ++ entities)
     
        if (!validates(old)(l2e(old)))
            throw new Error("entities doesn't match rule " + l2e(old) + " at old " + old)
            
        if (!validates(location)(l2e(location)))
            throw new Error("entities doesn't match rule " + l2e(location) + " at " + location)
    }

    def get(location : Location[T]) : List[T] = {
        if (!locations.contains(location))
            throw new Error("location not registered " + location)

        l2e(location)
    }

    def dump() {
        ===("")
        ===("--------------------------------------------------------------------------")
        locations.foreach(l => ===("LOCATION [ " + l.key + " ] ==> " + l2e(l).mkString(" | ")))
        ===("--------------------------------------------------------------------------")
        ===("")
    }

}

trait OfTracker[T] {
    def tracker : Tracker[T]
}

trait Destination[T] extends OfTracker[T] {
    def destination(source : Source[T], l : List[T]) : Location[T]
}

trait Source[T] extends OfTracker[T] {
    def source(l : List[T], destination : Destination[T]) : Location[T]
    def -->(l : List[T]) = SourceList(this, l)
    def -->(e : T) = SourceList(this, $(e))
}

case class SourceList[T](source : Source[T], list : List[T]) {
    def -->(destination : Destination[T]) {
        source.tracker.moveList(source.source(list, destination), list, destination.destination(source, list))
    }
}

trait Location[T] extends Destination[T] with Source[T] {
    def key : Any
    def -->(dest : Destination[T]) {
        val l = tracker.get(this)
        tracker.moveList(this, l, dest.destination(this, l))
    }
}

class ListLocation[T](val key : Any, val tracker : Tracker[T]) extends Location[T] {
    def destination(source : Source[T], l : List[T]) = this
    def source(l : List[T], destination : Destination[T]) = this
    private def list = tracker.get(this)
    def get = list
    def has(e : T) = list.contains(e)
    def first = list.head
    def any = list.nonEmpty
    def none = list.isEmpty
    def num = list.size
    def some = if (list.isEmpty) None else Some(list)
//    def apply(x : String) : Float = ???
    def %(p : T => Boolean) = list.filter(p)
    def %!(p : T => Boolean) = list.filterNot(p)
    def /[U](p : T => U) = list.map(p)
    def /~[U](p : T => scala.collection.IterableOnce[U]) = list.flatMap(p)
    def foreach(p : T => Unit) = list.foreach(p)
    override def toString = "" + key
}

object ListLocation {
    implicit def toList[T](ll : ListLocation[T]) = ll.get
}

class SlotLocation[T](val key : Any, val tracker : Tracker[T]) extends Location[T] {
    def destination(source : Source[T], l : List[T]) = this
    def source(l : List[T], destination : Destination[T]) = this
    private def list = tracker.get(this)
    def get = list.single
    def any = list.nonEmpty
    def none = list.isEmpty
    def %(p : T => Boolean) = get.filter(p)
    def %!(p : T => Boolean) = get.filterNot(p)
    def /[U](p : T => U) = get.map(p)
    def /~[U](p : T => scala.collection.IterableOnce[U]) = list.flatMap(p)
    def foreach(p : T => Unit) = get.foreach(p)
    override def toString = "" + key
}

object ObjectLocation {
    implicit def toOption[T](sl : SlotLocation[T]) = sl.get
}


