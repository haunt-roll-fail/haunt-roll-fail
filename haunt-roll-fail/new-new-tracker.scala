package hrf.tracker3
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

trait Tracker[K, T] {
    def get(key : K) : $[T]
}

class IdentityTracker[K, T : ClassTag] extends Tracker[K, T] {
    private var base = implicitly[ClassTag[T]]
    private var tags : Map[K, ClassTag[_]] = Map()
    private var rules : Map[K, T => Boolean] = Map()
    private var entities : $[T] = $
    private var k2e : Map[K, $[T]] = Map()
    private var e2k : Map[T, K] = Map()

    def all = entities

    def keys = tags.keys.$

    def has(key : K) = keys.has(key)

    def register[E <: K](key : E, rule : T => Boolean = (e : T) => true, content : $[T] = $) : E = registerOf[E, T](key, rule, content)

    def registerOf[E <: K, S <: T : ClassTag](key : E, rule : S => Boolean = (e : S) => true, content : $[S] = $) : E = {
        if (tags.contains(key))
            throw new Error("key already registered " + key)

        content.foreach { e =>
            if (entities.has(e))
                throw new Error("entity already registered " + e + " at " + key)

            if (!rule(e))
                throw new Error("entity doesn't match rule " + e + " at " + key)
        }

        tags = tags + (key -> implicitly[ClassTag[S]])

        rules += key -> ((e : T) => e.as[S].?(rule))

        entities ++= content

        k2e += key -> content

        content.foreach(e2k += _ -> key)

        key
    }

    def find(e : T) : Option[K] = e2k.get(e)

    def get(key : K) : $[T] = getOf[T](key)

    def getOf[S <: T : ClassTag](key : K) : $[S] = {
        if (tags.contains(key).not)
            throw new Error("key not registered " + key)

        val tag = implicitly[ClassTag[S]]

        val result = k2e(key)

        if (tag == base || tag == tags(key))
            return result.asInstanceOf[$[S]]

        throw new Error("unmatching tag for " + key + " " + tag)
    }

    def move(entity : T, key : K) {
        if (!entities.has(entity))
            throw new Error("entity not registered " + entity)

        if (!keys.has(key))
            throw new Error("key not registered " + key)

        if (!rules(key)(entity))
            throw new Error("entity doesn't match rule " + entity + " at " + key)

        val old = e2k(entity)

        k2e += old -> k2e(old).but(entity)
        k2e += key -> k2e(key).add(entity)

        e2k += entity -> key
    }

    def dump() {
        +++("")
        +++("--------------------------------------------------------------------------")
        k2e.keys.foreach(k => +++("" + k + " -> " + k2e(k).mkString(" ")))
        e2k.keys.foreach(k => +++("" + k + " -> " + e2k(k)))
        +++("--------------------------------------------------------------------------")
        +++("")
    }
}

class ValueTracker[K, T : ClassTag] extends Tracker[K, T] {
    private var base = implicitly[ClassTag[T]]
    private var tags : Map[K, ClassTag[_]] = Map()
    private var rules : Map[K, T => Boolean] = Map()
    private var entities : $[T] = $
    private var k2e : Map[K, $[T]] = Map()
    private var e2k : Map[T, K] = Map()

    def all = entities

    def keys = tags.keys.$

    def has(key : K) = keys.has(key)

    def register[E <: K](key : E, rule : T => Boolean = (e : T) => true, content : $[T] = $) : E = registerOf[E, T](key, rule, content)

    def registerOf[E <: K, S <: T : ClassTag](key : E, rule : S => Boolean = (e : S) => true, content : $[S] = $) : E = {
        if (tags.contains(key))
            throw new Error("key already registered " + key)

        content.foreach { e =>
            if (entities.has(e))
                throw new Error("entity already registered " + e + " at " + key)

            if (!rule(e))
                throw new Error("entity doesn't match rule " + e + " at " + key)
        }

        tags = tags + (key -> implicitly[ClassTag[S]])

        rules += key -> ((e : T) => e.as[S].?(rule))

        entities ++= content

        k2e += key -> content

        content.foreach(e2k += _ -> key)

        key
    }

    def find(e : T) : Option[K] = e2k.get(e)

    def get(key : K) : $[T] = getOf[T](key)

    def getOf[S <: T : ClassTag](key : K) : $[S] = {
        if (tags.contains(key).not)
            throw new Error("key not registered " + key)

        val tag = implicitly[ClassTag[S]]

        val result = k2e(key)

        if (tag == base || tag == tags(key))
            return result.asInstanceOf[$[S]]

        throw new Error("unmatching tag for " + key + " " + tag)
    }

    def move(entity : T, key : K) {
        if (!entities.has(entity))
            throw new Error("entity not registered " + entity)

        if (!keys.has(key))
            throw new Error("key not registered " + key)

        if (!rules(key)(entity))
            throw new Error("entity doesn't match rule " + entity + " at " + key)

        val old = e2k(entity)

        k2e += old -> k2e(old).but(entity)
        k2e += key -> k2e(key).add(entity)

        e2k += entity -> key
    }

    def dump() {
        +++("")
        +++("--------------------------------------------------------------------------")
        k2e.keys.foreach(k => +++("" + k + " -> " + k2e(k).mkString(" ")))
        e2k.keys.foreach(k => +++("" + k + " -> " + e2k(k)))
        +++("--------------------------------------------------------------------------")
        +++("")
    }
}
