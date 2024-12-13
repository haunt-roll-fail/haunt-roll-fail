package hrf.base
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

import hrf.elem._
import hrf.html._
import hrf.meta._
import hrf.bot._
import hrf.ui._


trait Timelines { self : Gaming =>

    class Moment(val action : ExternalAction, val prev : Int, var validated : |[Boolean], var continue : |[Continue], var effects : |[$[UIEffect]], var state : |[G], var execution : |[Double])

    class Timeline(newGame : () => G) {
        private var moments : $[Moment] = $

        def continue : Continue = {
            val t = trace(moments.num - 1).%!(moment(_).action.is[UndoAction])

            println(trace(moments.num - 1))
            println(t)

            if (t.none)
                StartContinue
            else {
                force(t.last, defaultForceLast)

                moment(t.last).continue.get
            }
        }

        def length = moments.num

        def append(a : ExternalAction) {
            moments :+= (a match {
                case UndoAction(_, _, n) => new Moment(a, n - 1, None, None, None, None, None)
                case _     => new Moment(a, moments.num - 1, None, None, None, None, None)
            })
        }

        def trace(n : Int) : $[Int] = {
            var t = $[Int]()
            var i = min(n, moments.num - 1)

            while (i >= 0) {
                t +:= i

                i = moments(i).prev
            }

            t
        }

        val defaultForceLast = 40 / 4
        val defaultForceStart = 40 / 4

        def current = last

        def last : G =
            if (moments.none)
                newGame()
            else {
                val n = moments.num - 1

                force(moments.num - 1, defaultForceLast)

                moments(n).state.get
            }

        def moment(n : Int) : Moment = moments(validateIndex(n))

        def validateIndex(n : Int) : Int =
            if (n < 0 || n >= moments.num)
                throw new Error("incorrect index " + n + ", of 0..max " + (moments.num - 1))
            else
                n

        def force(n : Int, k : Int) {
            pprint.pprintln("FORCE")
            pprint.pprintln((n : Int, k : Int))

            validateIndex(n)

            val t = trace(n).%(moments(_).action.is[UndoAction].not)
            val u = t.dropRight(k)
            val s = u.%(moments(_).state.any).lastOption
            val w = u.lastOption.|(0)

            val p = s./(s => t.dropWhile(_ != s).drop(1)).|(t)
            var g = s./~(s => moments(s).state).|(newGame())
            var c : |[Continue] = s./(s => moments(s).continue).|(|(StartContinue))

            s.foreach(moments(_).state = None)

            p.foreach { i =>
                val inRange = i <= defaultForceStart || i >= w
                val log = moments(i).effects.none && inRange
                val validate = inRange

                g.as[LoggedGame]./(_.logging = log)

                if (log || validate) {
                    val (r, d) = hrf.web.timed2(g.performContinue(c.%(_ => validate), moments(i).action, c.%(_ => validate).any))

                    if (validate && c.any) {
                        moments(i).validated = |(true)
                        moments(i).continue = |(r.continue)
                    }

                    if (log) {
                        moments(i).effects = |(r.effects)
                    }

                    moments(i).execution = |(d)

                    c = |(r.continue)
                }
                else {
                    val (_, d) = hrf.web.timed2(g.performVoid(moments(i).action))

                    moments(i).execution = |(d)

                    c = None
                }

                g.as[LoggedGame]./(_.logging = true)
            }

            moments(n).state = |(g)
        }

        def game(n : Int) : G = ???
    }
}
