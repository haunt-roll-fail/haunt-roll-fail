package hrf.tracker
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


trait Location[+T] {
    def key : Any
}

trait Tracker[T] {
    def get[S <: T](location : Location[T]) : $[S]
}

class IdentityTracker[T] extends Tracker[T] {
    private var locations : $[Location[T]] = Nil
    private var rules : Map[Location[T], T => Boolean] = Map()
    private var entities : $[T] = Nil
    private var l2e : Map[Location[T], $[T]] = Map()
    private var e2l : Map[T, Location[T]] = Map()

    def another[S <: T](key : Any, content : $[S] = Nil, rule : S => Boolean = (e : S) => true) : AnonymousLocation[S, T] = {
        val l = new AnonymousLocation[S, T](key, this)
        register(l, content, rule)
        l
    }

    def register[S <: T](location : Location[T], content : $[S] = Nil, rule : S => Boolean = (e : S) => true) {
        if (locations.contains(location))
            throw new Error("location already registered " + location)

        content.foreach { e =>
            if (entities.contains(e))
                throw new Error("entity already registered " + e + " at " + location)

            if (!rule(e))
                throw new Error("entity doesn't match rule " + e + " at " + location)
        }

        locations :+= location

        rules += location -> ((e : T) => rule(e.asInstanceOf[S]))

        entities ++= content

        l2e += location -> content

        content.foreach(e2l += _ -> location)
    }

    def move(entity : T, location : Location[T]) {
        if (!entities.contains(entity))
            throw new Error("entity not registered " + entity)

        if (!locations.contains(location))
            throw new Error("location not registered " + location)

        if (!rules(location)(entity))
            throw new Error("entity doesn't match rule " + entity + " at " + location)

        val old = e2l(entity)

        l2e += (old -> l2e(old).filter(_ != entity))
        l2e += location -> (l2e(location) :+ entity)

        e2l += entity -> location
    }

    def dump() {
        +++("")
        +++("--------------------------------------------------------------------------")
        l2e.keys.foreach(k => +++("" + k + " -> " + l2e(k).mkString(" ")))
        e2l.keys.foreach(k => +++("" + k + " -> " + e2l(k)))
        +++("--------------------------------------------------------------------------")
        +++("")
    }

    def find(e : T) = e2l(e)
    def get[S <: T](location : Location[T]) : $[S] = l2e(location).map(_.asInstanceOf[S])
}

class ValueTracker[T] extends Tracker[T] {
    private var locations : $[Location[T]] = Nil
    private var rules : Map[Location[T], T => Boolean] = Map()
    private var l2e : Map[Location[T], $[T]] = Map()

    def another[S <: T](key : Any = "*", content : $[S] = Nil, rule : S => Boolean = (e : S) => true) : AnonymousLocation[S, T] = {
        val l = new AnonymousLocation[S, T](key, this)
        register(l, content, rule)
        l
    }

    def anotherT(content : $[T] = Nil, rule : T => Boolean = (e : T) => true) : AnonymousLocation[T, T] = {
        val l = new AnonymousLocation[T, T]("---", this)
        register(l, content, rule)
        l
    }

    def register[S <: T](location : Location[T], content : $[S] = Nil, rule : S => Boolean = (e : S) => true) {
        if (locations.contains(location))
            throw new Error("location already registered " + location)

        content.foreach { e =>
            if (!rule(e))
                throw new Error("entity doesn't match rule " + e + " at " + location)
        }

        locations :+= location

        rules += location -> ((e : T) => rule(e.asInstanceOf[S]))

        l2e += location -> content
    }

    def move(old : Location[T], entity : T, location : Location[T]) {
        if (!locations.contains(location))
            throw new Error("location not registered " + location)

        if (!rules(location)(entity))
            throw new Error("entity doesn't match rule " + entity + " at " + location)

        if (!l2e(old).contains(entity)) {
            dump()
            throw new Error("old location doesn't have entity " + entity)
        }

        l2e += (old -> l2e(old).diff(List(entity)))
        l2e += location -> (l2e(location) :+ entity)
    }

    def get[S <: T](location : Location[T]) : $[S] = l2e(location).map(_.asInstanceOf[S])

    def dump() {
        +++("")
        +++("--------------------------------------------------------------------------")
        locations.foreach(l => +++("LOCATION [ " + l.key + " ] ==> " + l2e(l).mkString(" | ")))
        +++("--------------------------------------------------------------------------")
        +++("")
    }

}

class AnonymousLocation[S, T >: S](val key : Any, tracker : Tracker[T]) extends Location[S] {
    private def l = tracker.get[S](this)
    def get = l
    def first = l.head
    def any = l.nonEmpty
    def none = l.isEmpty
    def num = l.size
    def some = if (l.isEmpty) None else Some(l)
    def %(p : S => Boolean) = l.filter(p)
    def has(e : S) = l.contains(e)
    def %!(p : S => Boolean) = l.filterNot(p)
    def /[U](p : S => U) = l.map(p)
    def /~[U](p : S => scala.collection.IterableOnce[U]) = l.flatMap(p)
    def foreach(p : S => Unit) = l.foreach(p)
    override def toString = "" + key
}

object AnonymousLocation {
    implicit def toList[S](avl : AnonymousLocation[S, _]) = avl.get
}
