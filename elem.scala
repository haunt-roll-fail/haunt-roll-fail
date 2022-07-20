package hrf

import colmat._

package object elem {
    import hrf.reflect.className

    def classToStyleName(o : AnyRef) = className(o).toList./(c => c.isUpper.?(" " + c.toLower).|("" + c)).join("").trim.replace(' ', '-')
    
    abstract class Style {
        val name : String
        val prefix : StylePrefix
        val rules : Seq[CSSRule]
    }

    abstract class LocalStyle extends Style {
    }

    abstract class CustomStyle(val rules : CSSRule*)(implicit val prefix : StylePrefix) extends LocalStyle {
        val name = classToStyleName(this)
    }
    
    case class ObjectStyle(o : Styling)(val rules : CSSRule*)(implicit val prefix : StylePrefix) extends LocalStyle {
        val name = classToStyleName(o)
    }
    
    case class ExternalStyle(ename : String = "") extends Style {
        val name = ename.some.|(classToStyleName(this))
        val prefix = StylePrefix("")
        val rules = Nil
    }

    trait Styling
    
    case class StylePrefix(name : String) {
        def p = name.some.any.??(name + "-")
    }

    trait CSSRule {
        def get : List[SimpleCSSRule]
    }
    case class SimpleCSSRule(name : String, value : String) extends CSSRule {
        def get = $(this)
    }
    case class RepeatCSSRule(names : List[String], value : String) extends CSSRule {
        def get = names./(SimpleCSSRule(_, value))
    }

    object rules {
        trait R {
            def name : String = this.getClass.getName.split('$').drop(2).filter(_.trim.any).join("-")
            def apply(value : String) = SimpleCSSRule(name, value)
        }
        abstract class RR(names : String*) {
            def name : String = this.getClass.getName.split('$').drop(2).filter(_.trim.any).join("-")
            def apply(value : String) = RepeatCSSRule(names.toList./(name + "-" + _), value)
        }
        abstract class RS(names : String*) {
            def name : String = this.getClass.getName.split('$').drop(2).filter(_.trim.any).dropRight(1).join("-")
            def apply(value : String) = RepeatCSSRule(names.toList./(name + "-" + _), value)
        }
        abstract class RI(names : String*) {
            def apply(value : String) = RepeatCSSRule(names.toList, value)
        }
        
        case object color extends R
        object font { 
            case object size extends R
            case object weight extends R
            case object style extends R
            case object variant extends R
        }
        case object white {
            case object space extends R
        }
        case object letter {
            case object spacing extends R
        }
        case object visibility extends R
        case object display extends R
        case object width extends R
        case object height extends R
        case object left extends R
        case object top extends R
        case object right extends R
        case object bottom extends R

        case object max {
            case object width extends R
            case object height extends R
        }

        case object min {
            case object width extends R
            case object height extends R
        }

        case object margin extends RR("left", "right", "top", "bottom") {
            case object hor extends RS("left", "right")
            case object ver extends RS("top", "bottom")
            case object left extends R
            case object right extends R
            case object top extends R
            case object bottom extends R
        }
        case object padding extends RR("left", "right", "top", "bottom") {
            case object hor extends RS("left", "right")
            case object ver extends RS("top", "bottom")
            case object left extends R
            case object right extends R
            case object top extends R
            case object bottom extends R
        }
        case object border {
            case object width extends R
            case object color extends R
            case object style extends R
            case object radius extends R
            case object bottom {
                case object width extends R
                case object color extends R
                case object style extends R
            }
        }
        case object outline {
            case object width extends R
            case object color extends R
            case object style extends R
            case object offset extends R
        }
        case object background {
            case object color extends R
        }
        case object cursor extends R
        object text {
            case object align extends R
            case object indent extends R
            case object shadow extends R
            case object overflow extends R
            object decoration {
                case object line extends R
                case object style extends R
            }
        }
        object overflow {
            case object x extends R
            case object y extends R
        }
        object box {
            case object sizing extends R
        }
        object vertical {
            case object align extends R
        }
        object position extends R
        object filter extends R
        object line {
            case object height extends R
        }
        object justify {
            case object content extends R
        }
        object align {
            case object items extends R
            case object content extends R
        }
        object flex extends R {
            case object wrap extends R
            case object basis extends R
            case object direction extends R
            case object shrink extends R
            case object grow extends R
        }
        object order extends R
        object objectfit extends R {
            override def name = "object-fit"
        }
        object zIndex extends R {
            override def name = "z-index"
        }
        case object transform extends R
        object clip {
            case object path extends R
        }
    }
    
    trait StyleMapping {
        def get(o : Styling) : Style
    }

    abstract class BaseStyleMapping(prefix : String) extends StyleMapping {
        protected implicit val p = StylePrefix(prefix)

        private var mapping = Map[Styling, Style]()
        
        def get(o : Styling) : Style = mapping.get(o).|!("Unregistered styling object: " + o)
        
        protected implicit class stylingToRules(o : Styling) {
            def -->(rules : CSSRule*) {
                if (mapping.contains(o))
                    throw new Error(o.toString + " already registered")
    
                mapping += o -> ObjectStyle(o)(rules : _*)(p)
            }

            def -->(rules : List[CSSRule]) {
                if (mapping.contains(o))
                    throw new Error(o.toString + " already registered")
    
                mapping += o -> ObjectStyle(o)(rules : _*)(p)
            }
        }
        
        protected def ---->(o : Styling)(rules : CSSRule*) {
            if (mapping.contains(o))
                throw new Error(o.toString + " already registered")

            mapping += o -> ObjectStyle(o)(rules : _*)(p)
        }
    }                                                    
 
    trait Styled {
        val style : Style
    }

    val SingleLine = Span(Text("------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"), xstyles.compressed)
    val DoubleLine = Span(Text("================================================================================================================================================================================================"), xstyles.compressed)
    
    implicit class StylesId(val styles : Iterable[Style]) extends AnyVal {
        def id = {
            val (ii, ee) = styles.partition(_.rules.any)

            val p : Option[String] = None
            
            val xx = p./(_ + ii./(_.name).join("_")).|(ii./(s => s.prefix.p + s.name).join("---")).some ++ ee./(_.name)
            xx.join(" ")
        }
    }

    sealed trait Elem extends hrf.base.Record {
        def text : String
        def variants : List[List[Style]]
        def ~(e : Elem) = (this == Empty).?(e).|((e == Empty).?(this).|(Concat(this, e)))
        def ~(e : List[Elem]) : Elem = (this == Empty).?(e.merge).|(Concat(this, e.merge))
        def ~(e : String) : Elem = this ~ Text(e)
        def map(f : Elem => Elem) = (this == Empty).?(Empty).|(f(this))
        def ~(e : Option[Elem]) : Elem = e.none.?(this).|((this == Empty).?(e.get).|(Concat(this, e.get)))
        def onClick = OnClick(this)
        def onClick(p : Any) = Parameter(p, OnClick(this))
    }    
    
    implicit class ElemsList(val l : List[Elem]) {
        private def ll = l.but(Empty)
        def ~(e : Elem) = ll.any.?(Concat(merge, e)).|(e)
        def ~(e : String) : Elem = this.~(Text(e))
        def ~(e : List[Elem]) = (ll ++ e).merge
        def join(j : Elem) : Elem = ll.any.?(ElemList(ll, j)).|(Empty)
        def join(s : String) : Elem = join(Text(s))
        def merge = join(Empty)
        def comma = join(", ")
    }
 
    case class Text(s : String) extends Elem {
        def text = s
        def variants = Nil
    }

    object Text {
        def apply(n : Int) : Text = Text("" + n)
    }
    
    case class Span(e : Elem, styles : List[Style]) extends Elem {
        def text = e.text
        def variants = e.variants ++ styles.some
        def apply(s : Style) = copy(styles = styles :+ s)
        def apply(s : Option[Style]) = copy(styles = styles ++ s)
    }
    
    object Span {
        def apply(e : Elem, styles : Style*) : Span = Span(e, styles.toList)
    }

    case class Div(e : Elem, styles : List[Style]) extends Elem {
        def text = e.text
        def variants = e.variants ++ styles.some
        def apply(s : Style) = copy(styles = styles :+ s)
        def apply(s : Option[Style]) = copy(styles = styles ++ s)
    }

    object Div {
        def apply(e : Elem, styles : Style*) : Div = Div(e, styles.toList)
    }

    case class Link(url : String, e : Elem, styles : List[Style]) extends Elem {
        def text = e.text
        def variants = e.variants ++ styles.some
    }

    case class Hint(hint : String, e : Elem, styles : List[Style]) extends Elem {
        def text = e.text
        def variants = e.variants
    }

    object Hint {
        def apply(hint : String, e : Elem, styles : Style*) : Hint = Hint(hint, e, styles.toList)
    }

    case class Image(image : String, styles : List[Style]) extends Elem {
        def text = "@"
        def variants = styles.some.toList
        def apply(s : Style) = copy(styles = styles :+ s)
        def apply(s : Option[Style]) = copy(styles = styles ++ s)
    }
 
    object Image {
        def apply(image : String, styles : Style*) : Image = Image(image, styles.toList)
    }

    case class Concat(a : Elem, b : Elem) extends Elem {
        def text = a.text + b.text
        def variants = a.variants ++ b.variants
    }
    
    case class ElemList(l : List[Elem], e : Elem = Empty) extends Elem {
        def text = l./(_.text).join(e.text)
        def variants = l./~(_.variants) ++ (l.num > 1).??(e.variants)
    }

    val SpaceSpan = Span(Text(" "))
    val HorizontalBreak = Div(Empty, xlo.break)
    
    case object Empty extends Elem {
        def text = ""
        def variants = Nil
    }

    case object Break extends Elem {
        def text = "\n"
        def variants = Nil
    }

    case class NameReference(s : String, o : Any) extends Elem {
        def text = s
        def variants = Nil
    }
    
    case class Parameter(p : Any, e : Elem) extends Elem {
        def text = e.text
        def variants = e.variants
    }

    case class OnClick(e : Elem) extends Elem {
        def text = e.text
        def variants = e.variants
    }
 
    object OnClick {
        def apply(p : Any, e : Elem) : Parameter = Parameter(p, OnClick(e))
    }
 
    case class OnOverOut(e : Elem) extends Elem {
        def text = e.text
        def variants = e.variants
    }
 
    case class ContentDiv(id : String, styles : List[Style]) extends Elem {
        def text = ""
        def variants = Nil ++ styles.some 
    }

    case class NamedContent(id : String) extends Elem {
        def text = "<!-- {{{ " + id + " --><!-- " + id + " }}} -->"
        def variants = Nil
    }

    case object Content extends Elem {
        def text = "<!-- {{{ --><!-- }}} -->"
        def variants = Nil
    }

    case class Ref(id : String, e : Elem) extends Elem {
        def text = e.text
        def variants = e.variants
    }

    case class Comment(s : Any) extends Elem {
        def text = "<!-- " + s + " --->"
        def variants = Nil
    }

    case class Input(v : String, onChange : String => Unit, size : Int, max : Int, istyles : List[Style], dstyles : List[Style]) extends Elem {
        def text = v
        def variants = Nil
    }
    
    case class ElemContainer(elem : Elem)

    implicit def string2elem(s : String) = Text(s)

    implicit def int2elem(n : Int) = ElemContainer(Text(n.toString))
    
    implicit class ElemBool(val b : Boolean) {
        def ??(f : => Elem) = if (b) f else Empty
    }

    implicit class ElemOption[T](val o : Option[T]) {
        def ??(t : T => Elem) = o./(t).|(Empty)
    }



    implicit class ElementInt(val n : Int) {
        def styled(a : Style, r : Style*) = Span(Text(n), (a +: r) : _*)
        def hl : Elem = styled(xstyles.highlight)
        def hh : Elem = styled(xstyles.halfhigh)
        def hlb : Elem = styled(xstyles.highlight, xstyles.bold)
    }
    
    implicit class ElementString(val s : String) extends AnyVal {
        def styled(a : Style, r : Style*) = Span(Text(s), (a +: r) : _*)
        def hl = styled(xstyles.highlight)
        def hh : Elem = styled(xstyles.halfhigh)
        def hlb : Elem = styled(xstyles.highlight, xstyles.bold)
        def hlIf(v : Boolean) = v.?(styled(xstyles.highlight)).|(Text(s))
        def spn = Span(s)
        def pre = spn(xlo.pre)
    }
    
    implicit class ElementElem(val s : Elem) extends AnyVal {
        def spn = Span(s)
        def div = Div(s)
        def styled(a : Style) = Span(s, a)
        def styled(a : Style, b : Style) = Span(s, a, b)
        def hl = spn(xstyles.highlight)
        def block = spn(xlo.inlineBlock)
    }
    
    
    implicit class ElemStylingInt(val n : Int)(implicit styles : StyleMapping) {
        def styled(s : Styling) = Span(n.toString, styles.get(s))
    }
    
    implicit class ElemStylingString(val t : String)(implicit styles : StyleMapping) {
        def styled(s : Styling) = Span(t, styles.get(s))
    }
    
    implicit class ElemStylingElem(val e : Elem)(implicit styles : StyleMapping) {
        def styled(s : Styling) = Span(e, styles.get(s))
    }

    
}
