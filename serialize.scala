package hrf

import colmat._

import scala.scalajs.reflect._

import fastparse._, NoWhitespace._

import hrf.base._
import hrf.reflect._

import scala.collection.immutable.SortedMap
import scala.collection.immutable.ListMap

package object serialize {
    trait Serializer {
        val gaming : Gaming
        val prefix : String

        private val gprefix = "hrf.base.Gaming$"
        private val eprefix = "hrf.elem.package$"
        
        import gaming._
    
        def writeFaction(f : gaming.F) : String
        def parseFaction(s : String) : Option[gaming.F]
        
        def write(o : Any)(implicit tag : scala.reflect.ClassTag[gaming.F]) : String = o match {
            case b : Boolean => b.toString
            case n : Int => n.toString
            case s : String => "\"" + s + "\""
            case f : gaming.F => writeFaction(f) 
            case Some(x) => "Some(" + write(x) + ")"
            case None => "None"
            case a : Record => className(a) + a.productIterator.toList./(write).mkString("(", ", ", ")")
            case ss : List[_] => ss./(write).mkString("[", ", ", "]")
            case kv : Map[_, _] => kv./{ case (k, v) => write(k) + " -> " + write(v) }.mkString("{", ", ", "}")
        }
            
        trait Expr
        case class ESymbol(value : String) extends Expr
        case class EInt(value : Int) extends Expr
        case class EBool(value : Boolean) extends Expr
        case class EString(value : String) extends Expr
        case object ENone extends Expr
        case class ESome(value : Expr) extends Expr
        case class EList(values : List[Expr]) extends Expr
        case class EPair(pair : (Expr, Expr)) extends Expr
        case class EMap(values : List[EPair]) extends Expr
        case class EApply(f : String, params : List[Expr]) extends Expr
        
        def space[_ : P] = P( CharsWhileIn(" \r\n", 0) )
    
        def symbol[_ : P] = P( (CharIn("A-Z") ~ CharsWhileIn("A-Za-z0-9").?).! ).map(ESymbol)
    
        def number[_ : P] = P( CharsWhileIn("0-9\\-").! ).map(_.toInt).map(EInt)
        
        def string[_ : P] = P( "\"" ~/ CharsWhile(c => c != '\"' && c != '\\', 0).! ~ "\"").map(EString)
    
        def pfalse[_ : P] = P( "false" ).map(_ => EBool(false))
        
        def ptrue[_ : P] = P( "true" ).map(_ => EBool(true))
        
        def list[_ : P] = P( "[" ~/ params ~ "]").map(EList)
        
        def map[_ : P] = P( "{" ~/ pairs ~ "}").map(EMap)

        def pairs[_ : P] = P( pair.rep(sep = ","./) ).map(_.toList)

        def pair[_ : P] = P( expr ~ "->" ~ expr ).map(EPair)
        
        def some[_ : P] = P( "Some" ~ space ~ "(" ~/ expr ~ ")").map(o => ESome(o))
        
        def none[_ : P] = P( "None" ).map(o => ENone)
        
        def base[_ : P] : P[Expr] = P( some | none | action | symbol | number | pfalse | ptrue | list | map | string )
        
        def expr[_ : P] : P[Expr] = P( space ~ base ~ space )
    
        def action[_ : P] = P( space ~ symbol ~ space ~ "(" ~/ space ~ params ~ space ~ ")" ~ space).map(o => EApply(o._1.value, o._2))
    
        def params[_ : P] = P( expr.rep(sep = ","./) ).map(_.toList)
        
        def parseAction(s : String) : Action = {
            parse(s, action(_)) match {
                case Parsed.Success(a, _) => parseExpr(a).asInstanceOf[Action]
                case f @ Parsed.Failure(label, index, extra) => throw new Error(label + " " + index + " " + extra + "\n\n" + f)
            }
        }
    
        def parseExpr(e : Expr) : Any = e match {
            case ESymbol(s) => parseFaction(s).|(lookupObject(prefix + s).||(lookupObject(eprefix + s)).get)
            case EInt(n) => n
            case EBool(b) => b
            case EString(s) => s
            case EList(l) => l.map(parseExpr)
            case EMap(l) => ListMap(l.map(x => parseExpr(x.pair._1) -> parseExpr(x.pair._2)) : _*)
            case EApply("CancelAction", Nil) => CancelAction
            case EApply("NoMessage", Nil) => NoMessage
            case EApply(f, Nil) => lookupObject(prefix + f).||(lookupObject(eprefix + f)).||(lookupObject(gprefix + f)).|(lookupClass(gprefix + f + "$", 1).|!("can't load object " + f).apply(List(gaming)))
            case EApply(f, params) => lookupClass(prefix + f, params.num).||(lookupClass(eprefix + f, params.num))./(_.apply(params.map(parseExpr))).|(lookupClass(gprefix + f, params.num + 1).|!("can't load class " + f).apply((gaming +: params.map(parseExpr))))
            case ENone => None
            case ESome(e) => Some(parseExpr(e))
        }
        
        def parseSymbol(s : String) = lookupObject(prefix + s)
    }
}   
    