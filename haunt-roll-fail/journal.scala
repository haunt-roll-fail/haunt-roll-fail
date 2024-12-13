package hrf
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

import hrf.meta._
import hrf.web._


trait Journal[T] {
    val meta : MetaGame

    def known : Int

    def read(n : Int)(then : $[T] => Unit) : Unit
    def write(n : Int, message : T)(then : => Unit)(fail : => Unit) : Unit = write(n, message, None)(then)(fail)
    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) : Unit
    def append(message : T, comment : |[String] = None, retries : Int = 9)(then : => Unit)(fail : => Unit) : Unit = write(known, message, comment)(then) {
        if (retries > 0)
            read(known) { l =>
                append(message, comment, retries - 1)(then)(fail)
            }
        else
            fail
    }
}


class ServerJournal[T](val meta : MetaGame, server : String, user : String, secret : String, val journal : String, parse : String => T, write : T => String, readLimit : Int = 999999) extends Journal[T] {
    var size : Int = 0

    def known = size

    def read(n : Int)(then : $[T] => Unit) {
        get(server + "/read/" + user + "/" + secret + "/" + journal + "/" + n) { l =>
            val messages = l.split('\n').$.but("").take(readLimit - n)./(parse)
            size = n + messages.num
            then(messages)
        }
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        val prefix = comment./("// " + _ + "\n").|("")
        postF(server + "/append/" + user + "/" + secret + "/" + journal + "/" + n, prefix + write(message))(_ => { size += 1 ; then})(fail)
    }
}


class ServerPhantomJournal[T](val meta : MetaGame, val server : String, user : String, secret : String, journal : String, parse : String => T, write : T => String, readLimit : Int = 999999) extends Journal[T] {
    var actions = $[T]()

    def known = actions.num

    def read(n : Int)(then : $[T] => Unit) {
        if (actions.any)
            then(actions.drop(n))
        else
            get(server + "/read/" + user + "/" + secret + "/" + journal + "/" + 0) { l =>
                actions = l.split('\n').$.but("").take(readLimit - n)./(parse)
                then(actions.drop(n))
            }
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        if (actions.none)
            fail
        else
        if (actions.num == n) {
            actions :+= message
            then
        }
        else
            fail
    }
}


class MemoryJournal[T](val meta : MetaGame) extends Journal[T] {
    var actions = $[T]()

    def known = actions.num

    def read(n : Int)(then : $[T] => Unit) {
        then(actions.drop(n))
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        if (actions.num == n) {
            actions :+= message
            then
        }
        else
            fail
    }
}


class LocalStorageJournal[T](val meta : MetaGame) extends Journal[T] {
    var actions = $[T]()

    def known = actions.num

    def read(n : Int)(then : $[T] => Unit) {
        then(actions.drop(n))
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        if (actions.num == n) {
            actions :+= message
            then
        }
        else
            fail
    }
}


class ReplayJournal[T](val meta : MetaGame, data : String, parse : String => T, readLimit : Int = 999999) extends Journal[T] {
    val actions = data.split('\n').$./(_.trim).but("")./(parse)

    def known = actions.num

    def read(n : Int)(then : $[T] => Unit) {
        then(actions.drop(n).take(readLimit - n))
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        fail
    }
}


class ReplayPhantomJournal[T](val meta : MetaGame, data : String, parse : String => T, readLimit : Int = 999999) extends Journal[T] {
    var actions = data.split('\n').$./(_.trim).but("")./(parse)

    def known = actions.num

    def read(n : Int)(then : $[T] => Unit) {
        then(actions.drop(n).take(readLimit - n))
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        if (actions.none)
            fail
        else
        if (actions.num == n) {
            actions :+= message
            then
        }
        else
            fail
    }
}
