package hrf.tracker4
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

object implicits {
    implicit val tracker = this

    implicit class KEx[K, T](val k : K)(implicit tracker : IdentityTracker[K, T]) {
        def -->(l : $[T]) = {
            l.foreach { obj =>
                if (tracker.find(obj).has(k).not)
                    throw new Error("obj " + obj + " not found at " + k)
            }

            l
        }

        def -->(obj : T) = {
            if (tracker.find(obj).has(k).not)
                throw new Error("obj " + obj + " not found at " + k)

            obj
        }
        def -->(to : K) = tracker.get(k).foreach { u =>
            tracker.move(u, to)
        }
    }

    implicit class TEx[K, T](val u : T)(implicit tracker : IdentityTracker[K, T]) {
        def -->(to : K) {
            tracker.move(u, to)
        }
    }

    implicit class TListEx[K, T](val list : $[T])(implicit tracker : IdentityTracker[K, T]) {
        def -->(to : K) {
            list.foreach { u =>
                tracker.move(u, to)
            }
        }
    }
}

class IdentityTracker[K, T] {
    private var rules : Map[K, T => Boolean] = Map()
    private var entities : $[T] = $
    private var k2e : Map[K, $[T]] = Map()
    private var e2k : Map[T, K] = Map()

    def all : $[T] = entities

    def keys : $[K] = rules.keys.$

    def has(key : K) = rules.contains(key)

    def register(key : K, rule : T => Boolean = (e : T) => true, content : $[T] = $) : K = {
        if (rules.contains(key))
            throw new Error("key already registered " + key)

        content.foreach { e =>
            if (entities.has(e))
                throw new Error("entity already registered " + e + " at " + key)

            if (!rule(e))
                throw new Error("entity doesn't match rule " + e + " at " + key)
        }

        rules += key -> rule

        entities ++= content

        k2e += key -> content

        content.foreach(e2k += _ -> key)

        key
    }

    def find(e : T) : |[K] = e2k.get(e)

    def get(key : K) : $[T] = {
        if (rules.contains(key).not)
            throw new Error("key not registered " + key)

        k2e(key)
    }

    def move(entity : T, key : K) {
        if (entities.has(entity).not)
            throw new Error("entity not registered " + entity)

        if (rules.contains(key).not)
            throw new Error("key not registered " + key)

        if (rules(key)(entity).not)
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
