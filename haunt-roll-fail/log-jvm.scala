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

object logger {
    val pp : pprint.PPrinter = pprint.copy(
        additionalHandlers = {
            case value : Int => pprint.Tree.Literal(f"$value")
            case value : Double => pprint.Tree.Literal(f"$value%1.5f")
            case value : $[_] => pprint.Tree.Apply("$", value.iterator.map(x => pp.treeify(x, false, false)))
        }
    )

    case class ConsoleOutput(s : String)

    implicit def anyToConsoleOutputTop(a : scala.Any) : ConsoleOutput = (a match {
        case a : IterableOnce[_] => anyToConsoleOutput(a)
        case a : Product => ConsoleOutput(a.productIterator.map(e => anyToConsoleOutput(e).s).mkString(" "))
        case a => anyToConsoleOutput(a)
    })

    def anyToConsoleOutput(a : scala.Any) : ConsoleOutput = ConsoleOutput(a match {
        case a : String => a
        case a => "" + pp.apply(a)
    })

    implicit def printToAny(f : ConsoleOutput => Unit) : Any => Unit = (a : scala.Any) => f(a)
    implicit def printToUnit(f : ConsoleOutput => Unit) : Unit = f("")

    def --- : Unit = println()

    def +++(a : ConsoleOutput) : Unit = println(a.s)

    def warn(a : ConsoleOutput) : Unit = println(a.s)

    def error(a : ConsoleOutput) : Unit = println(a.s)

}
