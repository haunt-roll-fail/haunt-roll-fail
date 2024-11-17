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

import hrf.reflect.className

package object elem {

    def classToStyleName(o : AnyRef) = className(o).toList./(c => c.isUpper.?(" " + c.toLower).|("" + c)).join("").trim.replace(' ', '-')

    val MDash = 0x2014.toChar.toString
    val SpacedDash = " " + MDash + " "
    val FigureSpace = 0x2007.toChar.toString
    val InfoCircle = 0x24D8.toChar.toString
    val Dagger = 0x2020.toChar.toString
    val DoubleDagger = 0x2021.toChar.toString

    trait Postfix extends Elem

    case object Join extends SpecialText(" ")
    case object Dash extends SpecialText(0x2014.toChar.toString)
    case object Comma extends SpecialText(",") with Postfix
    case object Dot extends SpecialText(".") with Postfix
    case object Semicolon extends SpecialText(";") with Postfix
    case object Colon extends SpecialText(":") with Postfix
    case object Space extends SpecialText(" ")

    abstract class Style extends GoodMatch {
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
            def apply(value : String) = RepeatCSSRule(names.$./(name + "-" + _), value)
        }
        abstract class RS(names : String*) {
            def name : String = this.getClass.getName.split('$').drop(2).filter(_.trim.any).dropRight(1).join("-")
            def apply(value : String) = RepeatCSSRule(names.$./(name + "-" + _), value)
        }
        abstract class RI(names : String*) {
            def apply(value : String) = RepeatCSSRule(names.$, value)
        }

        case object color extends R
        object font {
            case object size extends R
            case object weight extends R
            case object style extends R
            case object variant extends R
            case object family extends R
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
        case object background extends R {
            case object color extends R
            case object image extends R
            case object clip extends RI("background-clip", "-webkit-background-clip")
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
        object overflow extends R {
            case object x extends R
            case object y extends R
            case object wrap extends R
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
        object objectFit extends R {
            override def name = "object-fit"
        }
        object zIndex extends R {
            override def name = "z-index"
        }
        case object transform extends R
        object clip {
            case object path extends R
        }
        object pointer {
            case object events extends R
        }
        object touch {
            case object action extends R
        }
        case object opacity extends R
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

    val SingleLine = Span(Text("--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"), xstyles.compressed)
    val DoubleLine = Span(Text("================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================"), xstyles.compressed)

    implicit class StylesId(val styles : Iterable[Style]) extends AnyVal {
        def id = {
            val (ii, ee) = styles.partition(_.rules.any)

            val p : Option[String] = None

            val xx = p./(_ + ii./(_.name).join("_")).|(ii./(s => s.prefix.p + s.name).join("---")).some ++ ee./(_.name)

            xx.join(" ")
        }
    }

    sealed trait Elem extends hrf.base.Record with GoodMatch {
        def text : String
        def variants : List[List[Style]]
        def ~(e : Elem) = (this == Empty).?(e).|((e == Empty).?(this).|(Concat(this, e)))
        def ~(e : List[Elem]) : Elem = (this == Empty).?(e.merge).|(Concat(this, e.merge))
        def ~(e : String) : Elem = this ~ Text(e)
        def ~(e : Char) : Elem = this ~ Text(e.toString)
        def map(f : Elem => Elem) = (this == Empty).?(Empty).|(f(this))
        def ~(e : |[Elem]) : Elem = e.none.?(this).|((this == Empty).?(e.get).|(Concat(this, e.get)))
        def onClick : Elem = (this == Empty).?(Empty).|(OnClick(this))
        def pointer = (this == Empty).?(Empty).|(Span(this, xlo.pointer, xlo.contents))
        def param(p : Any) = (this == Empty).?(Empty).|(Parameter(p, this))
    }

    implicit class ElemsList(val l : $[Elem]) {
        private def ll = l.but(Empty)
        def ~(e : Elem) = ll.any.?(Concat(merge, e)).|(e)
        def ~(e : String) : Elem = this.~(Text(e))
        def ~(e : $[Elem]) = (ll ++ e).merge
        def ~(e : |[Elem]) : Elem = e./(l ~ _).|(l.merge)
        def join(j : Elem) : Elem = ll.any.?(ElemList(ll, j)).|(Empty)
        def merge : Elem = ll.any.?(ElemList(ll, Empty)).|(Empty)
        def spaced : $[Elem] = ll./~(e => $(Space, e)).drop(1)
        def comma : $[Elem] = ll./~(e => $(Comma, e)).drop(1)
        def commaAnd : $[Elem] = ll.dropRight(1).some.?(_.comma :+ Text("and")) ++ ll.lastOption
        def commaOr : $[Elem] = ll.dropRight(1).some.?(_.comma :+ Text("or")) ++ ll.lastOption
        def spaceComma : Elem = ll./~(e => $(Comma, Text(" "), e)).drop(2).merge
        def spaceCommaOr : Elem = ll.dropRight(1).some./(_.spaceComma ~ Text(" or ")).?? ~ ll.lastOption
    }

    implicit def optionElemToElem(o : |[Elem]) = o.|(Empty)

    trait SpecialElem extends Elem {
        def underlying : Elem
        def text = underlying.text
        def variants = underlying.variants
    }

    abstract class SpecialText(s : String) extends SpecialElem {
        def underlying = Text(s)
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
        def apply(s : |[Style]) = copy(styles = styles ++ s)
        def apply(s : $[Style]) = copy(styles = styles ++ s)
    }

    object Span {
        def apply(e : Elem, styles : Style*) : Span = Span(e, styles.$)
    }

    case class Div(e : Elem, styles : List[Style]) extends Elem {
        def text = e.text
        def variants = e.variants ++ styles.some
        def apply(s : Style) = copy(styles = styles :+ s)
        def apply(s : |[Style]) = copy(styles = styles ++ s)
        def apply(s : $[Style]) = copy(styles = styles ++ s)
    }

    object Div {
        def apply(e : Elem, styles : Style*) : Div = Div(e, styles.$)
    }

    sealed trait LinkTarget
    case object LinkTargetBlank extends LinkTarget
    case object LinkTargetSelf extends LinkTarget


    case class Link(url : String, e : Elem, styles : List[Style], target : LinkTarget) extends Elem {
        def text = e.text
        def variants = e.variants ++ styles.some
    }

    case class Header(level : Int, s : String, styles : $[Style]) extends Elem {
        def text = "" // !!
        def variants = styles.some.$
    }

    object Link {
        def apply(url : String, e : Elem, styles : List[Style]) : Link = Link(url, e, styles, LinkTargetBlank)
        def internal(url : String, e : Elem, styles : List[Style]) : Link = Link(url, e, styles, LinkTargetSelf)
    }

    case class Hint(hint : String, e : Elem, styles : List[Style]) extends Elem {
        def text = e.text
        def variants = e.variants
    }

    object Hint {
        def apply(hint : String, e : Elem, styles : Style*) : Hint = Hint(hint, e, styles.$)
    }

    trait ImageIdPart {
        def name : String
    }

    case class ImageId(id : String) {
        def +(o : ImageIdPart) = ImageId(id + o.name)
        def +(s : String) = ImageId(id + s)
        def +(n : Int) = ImageId(id + n)
    }

    implicit def stringToImageId(s : String) = ImageId(s)
    implicit def imageIdPartToImageId(s : ImageIdPart) = ImageId(s.name)

    case class Image(image : ImageId, styles : $[Style], description : |[String] = None) extends Elem {
        def text = description.|(image.id.split('-').dropWhile(_.length < 3).join(" "))
        def variants = styles.some.$
        def apply(s : Style) = copy(styles = styles :+ s)
        def apply(s : |[Style]) = copy(styles = styles ++ s)
        def alt(s : String) = copy(description = Some(s))
    }

    object Image {
        def apply(image : ImageId, styles : Style*) : Image = Image(image, styles.$, None)
        def apply(image : ImageId, style : Style, desc : String) : Image = Image(image, $(style), Some(desc))
        def apply(image : ImageId, desc : String) : Image = Image(image, $, Some(desc))
        def apply(image : ImageId, style : Styling)(implicit qstyles : StyleMapping) : Image = Image(image, $(qstyles.get(style)), None)
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
    val HGap = Div(Empty, xlo.break, xlo.gap)

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

    object OnOverOut {
        def apply(p : Any, e : Elem) : Parameter = Parameter(p, OnOverOut(e))
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

    case class Input(v : String, placeholder : String, onChange : String => Unit, size : Int, max : Int, istyles : List[Style], dstyles : List[Style]) extends Elem {
        def text = v
        def variants = Nil
    }

    case class ElemContainer(elem : Elem)

    implicit def string2elem(s : String) = Text(s)

    implicit def int2elem(n : Int) = ElemContainer(Text(n.toString))

    implicit class ElemOptionNew[T](val o : Option[Elem]) {
        def ?? = o.|(Empty)
    }


    implicit class ElementInt(val n : Int) {
        def styled(a : Style, r : Style*) = Span(Text(n), (a +: r) : _*)
        def hl  = styled(xstyles.highlight)
        def hh  = styled(xstyles.halfhigh)
        def hlb = styled(xstyles.highlight, xstyles.bold)
    }

    implicit class ElementString(val s : String) extends AnyVal {
        def styled(a : Style, r : Style*) = Span(Text(s), (a +: r) : _*)
        def hl  = styled(xstyles.highlight)
        def hh  = styled(xstyles.halfhigh)
        def hlb = styled(xstyles.highlight, xstyles.bold)
        def hlIf(v : Boolean) = v.?(styled(xstyles.highlight)).|(Text(s))
        def spn = Span(s)
        def txt = Text(s)
        def pre = spn(xlo.pre)
        def & = spn(xlo.inlineBlock)
    }

    implicit class ElementElem(val s : Elem) extends AnyVal {
        def spn = Span(s)
        def div = Div(s)
        def styled(a : Style) = Span(s, a)
        def styled(a : Style, b : Style) = Span(s, a, b)
        def styled(l : $[Style]) = Span(s, l)
        def hl = spn(xstyles.highlight)
        def hh = spn(xstyles.halfhigh)
        def hlb = spn(xstyles.highlight)(xstyles.bold)
        def block = spn(xlo.inlineBlock)
        def & = spn(xlo.inlineBlock)
    }


    implicit class ElemStylingInt(val n : Int)(implicit qstyles : StyleMapping) {
        def styled(s : Styling) = Span(n.toString, qstyles.get(s))
        def styled(s : Option[Styling]) = Span(n.toString, s./(qstyles.get).$)
    }

    implicit class ElemStylingString(val t : String)(implicit qstyles : StyleMapping) {
        def styled(s : Styling) = Span(t, qstyles.get(s))
        def styled(s : Option[Styling]) = Span(t, s./(qstyles.get).$)
    }

    implicit class ElemStylingElem(val e : Elem)(implicit qstyles : StyleMapping) {
        def styled(s : Styling) = Span(e, qstyles.get(s))
        def styled(s : Option[Styling]) = Span(e, s./(qstyles.get).$)
    }

}
