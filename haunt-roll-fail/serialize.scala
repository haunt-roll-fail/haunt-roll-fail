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

import hrf.base._
import hrf.reflect._

import fastparse._
import fastparse.NoWhitespace._

import scalajs.reflect._

import scala.collection.immutable.SortedMap
import scala.collection.immutable.ListMap


package object serialize {
    object DummyGaming extends Gaming {
        type F = BasePlayer
        type G = BaseGame

        val gaming = this

        object styles extends hrf.elem.BaseStyleMapping("dummy") {}
    }

    object DefaultSerialize extends Serializer {
        val gaming = DummyGaming

        val prefix = "dummy"

        def parseFaction(s : String) = None
        def writeFaction(f : BasePlayer) = ""
    }

    trait Serializer {
        val gaming : Gaming
        val prefix : String

        private val gprefix = "hrf.base.Gaming$"
        private val eprefix = "hrf.elem.package$"

        import gaming._

        def writeFaction(f : gaming.F) : String
        def parseFaction(s : String) : Option[gaming.F]

        def write(o : Any) : String = o match {
            case b : Boolean => b.toString
            case n : Int => n.toString
            case d : Double => d.formatted("%f")
            case s : String => "\"" + s + "\""
            case f : gaming.F => writeFaction(f)
            case Some(x) => "Some(" + write(x) + ")"
            case None => "None"
            case a : Record => className(a) + a.productIterator.$.some.?(_./(write).mkString("(", ", ", ")"))
            case ss : List[_] => ss./(write).mkString("[", ", ", "]")
            case kv : Map[_, _] => kv./{ case (k, v) => write(k) + " -> " + write(v) }.mkString("{", ", ", "}")
            case x => "<!-- " + x + " -->"
        }

        trait Expr
        case class ESymbol(value : String) extends Expr
        case class EInt(value : Int) extends Expr
        case class EDouble(value : Double) extends Expr
        case class EBool(value : Boolean) extends Expr
        case class EString(value : String) extends Expr
        case object ENone extends Expr
        case class ESome(value : Expr) extends Expr
        case class EList(values : List[Expr]) extends Expr
        case class EPair(pair : (Expr, Expr)) extends Expr
        case class EMap(values : List[EPair]) extends Expr
        case class EApply(f : String, params : List[Expr]) extends Expr

        def space[* : P] = P{ CharsWhileIn(" \r\n", 0) }

        def symbol[* : P] = P{ (CharIn("A-Z") ~ CharsWhileIn("A-Za-z0-9").?).! }.map(ESymbol)

        def number[* : P] = P{ ("-".? ~ CharsWhileIn("0-9")).! }.map(_.toInt).map(EInt)

        def fractional[* : P] = P{ ("-".? ~ CharsWhileIn("0-9") ~ "." ~ CharsWhileIn("0-9")).! }.map(_.toDouble).map(EDouble)

        def string[* : P] = P{ "\"" ~/ CharsWhile(c => c != '\"' && c != '\\', 0).! ~ "\"" }.map(EString)

        def pfalse[* : P] = P{ "false" }.map(_ => EBool(false))

        def ptrue[* : P] = P{ "true" }.map(_ => EBool(true))

        def list[* : P] = P{ "[" ~/ params ~ "]" }.map(EList)

        def map[* : P] = P{ "{" ~/ pairs ~ "}" }.map(EMap)

        def pairs[* : P] = P{ pair.rep(sep = ","./) }.map(_.$)

        def pair[* : P] = P{ expr ~ "->" ~ expr }.map(EPair)

        def some[* : P] = P{ "Some" ~ space ~ "(" ~/ expr ~ ")" }.map(o => ESome(o))

        def none[* : P] = P{ "None" }.map(o => ENone)

        def base[* : P] : P[Expr] = P{ some | none | action | symbol | fractional | number | pfalse | ptrue | list | map | string }

        def expr[* : P] : P[Expr] = P{ space ~ base ~ space }

        def main[* : P] : P[Expr] = P{ action | symbol }

        def action[* : P] = P{ space ~ symbol ~ space ~ "(" ~/ space ~ params ~ space ~ ")" ~ space }.map(o => EApply(o._1.value, o._2))

        def params[* : P] = P{ expr.rep(sep = ","./) }.map(_.$)

        def parseAction(s : String) : Action = {
            if (s == "StartAction")
                return parseAction("StartAction(\"unknown\")")

            try {
                parse(s, main(_)) match {
                    case Parsed.Success(a, _) => parseExpr(a).asInstanceOf[Action]
                    case f @ Parsed.Failure(label, index, extra) =>
                        println("CantParseAction: " + label + " " + index + " " + extra + "\n\n" + f)
                        CantParseAction(s)
                }
            }
            catch {
                case e : Error =>
                    e.printStackTrace()
                    println("CantParseAction: " + s + "\n\n" + "...")
                    CantParseAction(s)
            }
        }

        def parseExpression(s : String) : Option[Any] = {
            parse(s, base(_)) match {
                case Parsed.Success(a, _) => Some(parseExpr(a))
                case f @ Parsed.Failure(label, index, extra) =>
                    println(label + " " + index + " " + extra + "\n\n" + f)
                    None
            }
        }

        def parseExpr(e : Expr) : Any = e match {
            case ESymbol("CancelAction") => CancelAction
            case ESymbol("NoMessage") => NoMessage
            case ESymbol(s) => parseFaction(s) || lookupObject(prefix + s) || lookupObject(eprefix + s) || lookupObject(gprefix + s + "$") || lookupClass(gprefix + s + "$", 0)./(_.apply($())) || lookupClass(gprefix + s + "$", 1)./(_.apply($(gaming))) |!("unknown object " + s)
            case EInt(n) => n
            case EDouble(d) => d
            case EBool(b) => b
            case EString(s) => s
            case EList(l) => l.map(parseExpr)
            case EMap(l) => ListMap(l.map(x => parseExpr(x.pair._1) -> parseExpr(x.pair._2)) : _*)
            case EApply("CancelAction", Nil) => CancelAction
            case EApply("NoMessage", Nil) => NoMessage
            case EApply("Rect", params) => lookupClass("hrf.ui.Rect", 4)./(_.apply(params.map(parseExpr))).|!("cant parse " + e)
            case EApply("PanePlacement", params) => lookupClass("hrf.ui.PanePlacement", 3)./(_.apply(params.map(parseExpr))).|!("cant parse " + e)
            case EApply(f, Nil) => lookupObject(prefix + f).||(lookupObject(eprefix + f)).||(lookupObject(gprefix + f)).||(lookupClass(gprefix + f + "$", 1)./(_.apply($(gaming)))).|!("unknown object " + f)
            case EApply(f, params) => lookupClass(prefix + f, params.num).||(lookupClass(eprefix + f, params.num))./(_.apply(params.map(parseExpr))).||(lookupClass(gprefix + f, params.num + 1)./(_.apply((gaming +: params.map(parseExpr))))).|!("unknown class " + f)
            case ENone => None
            case ESome(e) => Some(parseExpr(e))
        }

        def parseSymbol(s : String) = lookupObject(prefix + s)
    }
}
