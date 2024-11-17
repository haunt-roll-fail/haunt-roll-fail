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

import scalajs.js
import scalajs.js.annotation._


object logger {

    val pp : pprint.PPrinter = pprint.copy(
        additionalHandlers = {
            case value : Int => pprint.Tree.Literal(f"$value")
            case value : Double => pprint.Tree.Literal(f"$value%1.5f")
            case value : List[_] => pprint.Tree.Apply("$", value.iterator.map(x => pp.treeify(x, false, false)))
        }
    )

    type ConsoleOutput = js.Symbol

    implicit def anyToConsoleOutputTop(a : scala.Any) : ConsoleOutput = (a match {
        case a : IterableOnce[_] => anyToConsoleOutput(a)
        case a : Product => a.productIterator.map(e => anyToConsoleOutput(e)).mkString(" ")
        case a => anyToConsoleOutput(a)
    }).asInstanceOf[ConsoleOutput]

    def anyToConsoleOutput(a : scala.Any) : ConsoleOutput = (a match {
        case a : String => a
        case a => "" + pp.apply(a)
    }).asInstanceOf[ConsoleOutput]

    def --- : Unit = +++("")

    @js.native
    @JSGlobal("console")
    object +++ extends js.Object {
        @JSName("log")
        def apply(a : ConsoleOutput) : Unit = js.native
    }

    @js.native
    @JSGlobal("console")
    object warn extends js.Object {
        @JSName("warn")
        def apply(a : ConsoleOutput) : Unit = js.native
    }

    @js.native
    @JSGlobal("console")
    object error extends js.Object {
        @JSName("error")
        def apply(a : ConsoleOutput) : Unit = js.native
    }

}
