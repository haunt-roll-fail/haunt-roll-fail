package hrf.tracker2
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


trait Tracker[T] {
    def get(location : Location[T]) : $[T]
    def moveList(old : Location[T], entities : $[T], location : Location[T])
}

class ValueTracker[K, T] extends Tracker[T] {
    private var locations : $[Location[T]] = Nil
    private var accepts : Map[Location[T], T => Boolean] = Map()
    private var validates : Map[Location[T], $[T] => Boolean] = Map()
    private var mapping : Map[Location[T], $[T]] = Map()

    def list(key : K, content : $[T] = Nil, accept : T => Boolean = (e : T) => true, validate : $[T] => Boolean = (l : $[T]) => true) : ListLocation[K, T] = {
        val l = new ListLocation[K, T](key, this)
        register(l, content, accept, validate)
        l
    }

    def slot(key : K, content : Option[T] = None, accept : T => Boolean = (e : T) => true) : SlotLocation[K, T] = {
        val l = new SlotLocation[K, T](key, this)
        register(l, content.$, accept, e => e.num <= 1)
        l
    }

    def register(location : Location[T], content : $[T] = Nil, accept : T => Boolean = (e : T) => true, validate : $[T] => Boolean = (l : $[T]) => true) {
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

        mapping += location -> content
    }

    def moveList(old : Location[T], entities : $[T], location : Location[T]) {
        if (!locations.contains(old))
            throw new Error("location not registered old " + old)

        if (!locations.contains(location))
            throw new Error("location not registered " + location)

        if (entities.diff(mapping(old)).any) {
            dump()
            throw new Error("old location doesn't have all entities " + entities)
        }

        entities.foreach { entity =>
            if (!accepts(location)(entity))
                throw new Error("entity doesn't match rule " + entity + " at " + location)
        }

        mapping += old -> mapping(old).diff(entities)
        mapping += location -> (mapping(location) ++ entities)

        if (!validates(old)(mapping(old)))
            throw new Error("entities doesn't match rule " + mapping(old) + " at old " + old)

        if (!validates(location)(mapping(location)))
            throw new Error("entities doesn't match rule " + mapping(location) + " at " + location)
    }

    def get(location : Location[T]) : $[T] = {
        if (!locations.contains(location))
            throw new Error("location not registered " + location)

        mapping(location)
    }

    def dump() {
        +++("")
        +++("--------------------------------------------------------------------------")
        locations.foreach(l => +++("LOCATION [ " + l.key + " ] ==> " + mapping(l).mkString(" | ")))
        +++("--------------------------------------------------------------------------")
        +++("")
    }

}


trait OfTracker[T] {
    def tracker : Tracker[T]
}

trait Destination[T] extends OfTracker[T] {
    def destination(source : Source[T], l : $[T]) : Location[T]
}

trait Source[T] extends OfTracker[T] {
    def source(l : $[T], destination : Destination[T]) : Location[T]
    def -->(l : $[T]) = SourceList(this, l)
    def -->(e : T) = SourceList(this, $(e))
}

case class SourceList[T](source : Source[T], list : $[T]) {
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

class ListLocation[K, T](val key : K, val tracker : Tracker[T]) extends Location[T] {
    def destination(source : Source[T], l : $[T]) = this
    def source(l : $[T], destination : Destination[T]) = this
    private def list = tracker.get(this)
    def get = list
    def has(e : T) = list.contains(e)
    def first = list.head
    def any = list.nonEmpty
    def none = list.isEmpty
    def num = list.size
    def some = if (list.isEmpty) None else Some(list)
    def %(p : T => Boolean) = list.filter(p)
    def %!(p : T => Boolean) = list.filterNot(p)
    def /[U](p : T => U) = list.map(p)
    def /~[U](p : T => scala.collection.IterableOnce[U]) = list.flatMap(p)
    def foreach(p : T => Unit) = list.foreach(p)
    override def toString = "" + key
}

object ListLocation {
    implicit def toList[K, T](ll : ListLocation[K, T]) = ll.get
}

class SlotLocation[K, T](val key : K, val tracker : Tracker[T]) extends Location[T] {
    def destination(source : Source[T], l : $[T]) = this
    def source(l : $[T], destination : Destination[T]) = this
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
    implicit def toOption[K, T](sl : SlotLocation[K, T]) = sl.get
}
