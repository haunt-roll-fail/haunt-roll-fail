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

import hrf.elem._

import scalajs.js

import org.scalajs.dom

package object html {

    case class Resources(images : ImageResources, names : () => Map[Any, String]) {
        def getName(o : Any) : Option[String] = names().get(o)
        def getStyles(l : Seq[Style]) : $[String] = StyleRegister.add(l.$)
    }

    case class ImageResources(loaded : Map[String, hrf.loader.ImageWrapper], sources : Map[String, String], loader : hrf.loader.Loader[hrf.loader.ImageWrapper]) {
        var incomplete : $[String] = $
        def has(key : String) = loaded.contains(key.toLowerCase)
        def get(key : String) : dom.html.Image = {
            val img = loaded.get(key.toLowerCase).|(throw new Error("asset " + key + " not found")).get

            if (img.complete.not)
                incomplete :+= key

            img
        }
        def hasSource(key : String) = sources.contains(key.toLowerCase)
        def getSource(key : String) : String = loaded.get(key.toLowerCase)./(_.get.src).||(sources.get(key.toLowerCase)).|(throw new Error("asset " + key + " not found"))
        def getBlobSource(key : String)(onLoad : String => Unit) {
            loaded.get(key.toLowerCase)./(_.get.src) @@ {
                case Some(url) => onLoad(url)
                case None => sources.get(key.toLowerCase) @@ {
                    case Some(url) => loader.wait($(url)) { onLoad(loader.get(url).get.src) }
                    case None => throw new Error("asset " + key + " not found")
                }
            }
        }
    }

    class Container(val block : HtmlBlock, val attach : AttachmentPoint) {
        def node = block.nodes.only
        def shown = block.shown
        def hide() = block.hide()
        def show() = block.show()
        def visible = block.visible
        def invis() = block.invis()
        def vis() = block.vis()
        def replace(elem : Elem, resources : Resources, click : Any => Unit = null, over : Any => Unit = null, out : Any => Unit = null) : HtmlBlock = {
            attach.clear()
            attach.append(elem, resources, click, over, out)
        }
        def clear() {
            attach.clear()
        }
    }

    class HtmlBlock(val parent : dom.html.Element, val nodes : $[dom.html.Element]) {
        private var _shown = true
        def shown = _shown
        def hide() = { _shown = false; nodes.foreach(_.style.display = "none") }
        def show() = { _shown = true; nodes.foreach(_.style.display = "") }

        private var _visible = true
        def visible = _visible
        def invis() = { _visible = false; nodes.foreach(_.style.visibility = "hidden") }
        def vis() = { _visible = true; nodes.foreach(_.style.visibility = "") }

        def delete() = nodes.foreach(e => e.parentElement.removeChild(e))
    }

    trait AttachmentPoint {
        def parent : dom.html.Element
        def clear()
        def append(elem : Elem, resources : Resources, click : Any => Unit = null, over : Any => Unit = null, out : Any => Unit = null) : HtmlBlock
        def appendContainer(elem : Elem, resources : Resources, click : Any => Unit = null, over : Any => Unit = null, out : Any => Unit = null) : Container
    }

    class CommentsAttachmentPoint(val begin : dom.Comment, val end : dom.Comment) extends AttachmentPoint {
        def parent = {
            require(begin.parentNode != null)
            require(end.parentNode != null)
            require(begin.parentNode == end.parentNode)
            begin.parentNode.asInstanceOf[dom.html.Element]
        }

        def clear() {
            val parent = this.parent

            var del = false
            val nodes = 0.until(parent.childNodes.length)./(parent.childNodes.item)
            nodes.foreach { c =>
                if (c == end)
                    del = false

                if (del)
                    parent.removeChild(c)

                if (c == begin)
                    del = true
            }
        }

        def append(elem : Elem, resources : Resources, click : Any => Unit = null, over : Any => Unit = null, out : Any => Unit = null) = {
            val parent = this.parent

            val l = materialize(elem, resources, click, over, out, Nil, null)
            l.foreach(parent.insertBefore(_, end))
            new HtmlBlock(parent, l)
        }

        def appendContainer(elem : Elem, resources : Resources, click : Any => Unit = null, over : Any => Unit = null, out : Any => Unit = null) = {
            val parent = this.parent

            var cm = Map[String, AttachmentPoint]()
            def onContent(s : String, c : AttachmentPoint) = cm += s -> c
            val l = materialize(elem, resources, click, over, out, Nil, onContent)
            l.foreach(parent.insertBefore(_, end))
            new Container(new HtmlBlock(parent, l), cm("inside"))
        }
    }

    class ElementAttachmentPoint(val parent : dom.html.Element) extends AttachmentPoint {
        def hide() = parent.style.display = "none"
        def show() = parent.style.display = ""
        def invis() = parent.style.visibility = "hidden"
        def vis() = parent.style.visibility = ""

        def clear() {
            while (parent.hasChildNodes())
                parent.removeChild(parent.lastChild)
        }

        def append(elem : Elem, resources : Resources, click : Any => Unit = null, over : Any => Unit = null, out : Any => Unit = null) = {
            val l = materialize(elem, resources, click, over, out, Nil, null)
            l.foreach(parent.appendChild)
            new HtmlBlock(parent, l)
        }

        def appendContainer(elem : Elem, resources : Resources, click : Any => Unit = null, over : Any => Unit = null, out : Any => Unit = null) = {
            var cm = Map[String, AttachmentPoint]()
            def onContent(s : String, c : AttachmentPoint) = cm += s -> c
            val l = materialize(elem, resources, click, over, out, Nil, onContent)
            l.foreach(parent.appendChild)
            new Container(new HtmlBlock(parent, l), cm("inside"))
        }

        def appendMultiContainer(elem : Elem, resources : Resources) : ($[dom.Node], Map[String, AttachmentPoint]) = {
            var cm = Map[String, AttachmentPoint]()
            def onContent(s : String, c : AttachmentPoint) = cm += s -> c
            val l = materialize(elem, resources, null, null, null, Nil, onContent, true)
            l.foreach(parent.appendChild)
            (l, cm)
        }

        def appendContainerGetRoot(elem : Elem, resources : Resources) : dom.html.Element = appendContainer(elem : Elem, resources : Resources).attach.asInstanceOf[ElementAttachmentPoint].parent
        def appendContainerGetAP(elem : Elem, resources : Resources) : ElementAttachmentPoint = appendContainer(elem : Elem, resources : Resources).attach.asInstanceOf[ElementAttachmentPoint]
    }

    def htmlString(elem : Elem, r : Resources) : String = elem match {
        case Empty => ""
        case s : SpecialElem => htmlString(s.underlying, r)
        case x if x == HorizontalBreak => "<hr/>"
        case Text(s) => s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
        case Span(e, styles) => "<span class='" + styles.id + "'>" + htmlString(e, r) + "</span>"
        case Div(e, styles) => "<div class='" + styles.id + "'>" + htmlString(e, r) + "</div>"
        case Image(image, styles, desc) => "<img class='" + styles.id + "' src='" + r.images.getSource(image.id) + "' alt='" + desc.|(image.id) + "' />"
        case Concat(a, b) => htmlString(a, r) + "" + htmlString(b, r)
        case ElemList(l, j) => l./(e => htmlString(e, r)).join(htmlString(j, r))
        case Break => "<br/>"
    }

    def materialize(elem : Elem, resources : Resources, click : Any => Unit, over : Any => Unit, out : Any => Unit, params : $[Any], att : (String, AttachmentPoint) => Unit) : $[dom.html.Element] =
        materialize(elem : Elem, resources : Resources, click : Any => Unit, over : Any => Unit, out : Any => Unit, params : $[Any], att : (String, AttachmentPoint) => Unit, true)./(_.asInstanceOf[dom.html.Element])

    def materialize(elem : Elem, resources : Resources, click : Any => Unit, over : Any => Unit, out : Any => Unit, params : $[Any], att : (String, AttachmentPoint) => Unit, top : Boolean) : $[dom.Node] = elem match {
        case Empty => Nil

        case s : SpecialElem =>
            materialize(s.underlying, resources, click, over, out, params, att, top)

        case Concat(a, b) =>
            materialize(a, resources, click, over, out, params, att, top) ++ materialize(b, resources, click, over, out, params, att, top)

        case ElemList(l, e) =>
            l./~(e :: _).drop(1)./~(materialize(_, resources, click, over, out, params, att, top))

        case Text(s) =>
            val r = dom.document.createElement("span").asInstanceOf[dom.html.Span]
            r.dataset("elem") = "text"
            r.appendChild(dom.document.createTextNode(s))
            $(r)

        case Comment(s) =>
            $(dom.document.createComment("" + s))

        case Break =>
            $(dom.document.createElement("br"))

        case Span(e, styles) =>
            val r = dom.document.createElement("span").asInstanceOf[dom.html.Span]
            resources.getStyles(styles).foreach(r.classList.add)
            materialize(e, resources, click, over, out, params, att, false).foreach(r.appendChild)
            $(r)

        case Div(e, styles) =>
            val r = dom.document.createElement("div").asInstanceOf[dom.html.Div]
            resources.getStyles(styles).foreach(r.classList.add)
            materialize(e, resources, click, over, out, params, att, false).foreach(r.appendChild)
            $(r)

        case Header(lvl, s, styles) =>
            val r = dom.document.createElement("h" + lvl).asInstanceOf[dom.html.Heading]
            resources.getStyles(styles).foreach(r.classList.add)
            r.dataset("elem") = "header"
            r.appendChild(dom.document.createTextNode(s))
            $(r)

        case Image(s, styles, desc) =>
            val r = dom.document.createElement("img").asInstanceOf[dom.html.Image]
            r.ondragstart = (e) => false
            r.dataset("source") = s.id
            r.asInstanceOf[js.Dynamic].crossorigin = "use-credentials"
            resources.getStyles(styles).foreach(r.classList.add)
            r.dataset("src") = resources.images.getSource(s.id)
            resources.images.getBlobSource(s.id) { url => r.src = url }
            r.alt = desc.|(s.id.split('-').dropWhile(_.length < 3).join(" "))
            $(r)

        case ContentDiv(id, styles) =>
            val r = dom.document.createElement("span").asInstanceOf[dom.html.Span]
            resources.getStyles(styles).foreach(r.classList.add)
            att(id, new ElementAttachmentPoint(r))
            $(r)

        case Content =>
            val a = dom.document.createComment("{{{")
            val b = dom.document.createComment("}}}")
            att("inside", new CommentsAttachmentPoint(a, b))
            $(a, b)

        case Hint(h, e, styles) =>
            val r = dom.document.createElement("span").asInstanceOf[dom.html.Span]
            r.title = h
            resources.getStyles(styles).foreach(r.classList.add)
            materialize(e, resources, click, over, out, params, att, false).foreach(r.appendChild)
            $(r)

        case Link(url, e, styles, target) =>
            val r = dom.document.createElement("a").asInstanceOf[dom.html.Link]
            r.href = url
            r.target = target @@ {
                case LinkTargetBlank => "_blank"
                case LinkTargetSelf => "_self"
            }
            r.rel = "noopener noreferrer nofollow"
            resources.getStyles(styles).foreach(r.classList.add)
            materialize(e, resources, click, over, out, params, att, false).foreach(r.appendChild)
            $(r)

        case NameReference(s, o) =>
            materialize(Text(resources.getName(o).|(s)), resources, click, over, out, params, att, top)

        case Parameter(p, e) =>
            materialize(e, resources, click, over, out, params :+ p, att, top)

        case Input(v, placeholder, onChange, size, max, istyles, dstyles) =>
            val t = dom.document.createElement("input").asInstanceOf[dom.html.Input]
            t.`type` = "text"
            t.placeholder = placeholder
            t.spellcheck = false
            t.value = v
            t.maxLength = max
            t.size = size
            resources.getStyles(istyles).foreach(t.classList.add)

            if (onChange != null)
                t.oninput = (e) => onChange(t.`value`)

            val r = dom.document.createElement("div").asInstanceOf[dom.html.Div]
            resources.getStyles(dstyles).foreach(r.classList.add)
            r.appendChild(t)

            $(r)

        case OnClick(e) =>
            materialize(e, resources, click, over, out,params, att, top).single match {
                case Some(r : dom.html.Element) =>
                    r.dataset("onclick") = "*"
                    r.onclick = params match {
                        case $(o) => (e) => { e.stopPropagation(); click(o) }
                        case _ => (e) => { e.stopPropagation(); click(params) }
                    }
                    r.onmousedown = (e) => e.stopPropagation()
                    $(r)
                case Some(_) => throw new Error("OnClick only on dom.html.Element")
                case None => throw new Error("OnClick only on single elem")
            }

        case OnOverOut(e) =>
            materialize(e, resources, click, over, out, params, att, top).single match {
                case Some(r : dom.html.Element) =>
                    r.dataset("onoverout") = "*"
                    r.onmouseover = params match {
                        case $(o) => (e) => over(o)
                        case _ => (e) => over(params)
                    }
                    r.onmouseout = params match {
                        case $(o) => (e) => out(o)
                        case _ => (e) => out(params)
                    }
                    $(r)
                case Some(_) => throw new Error("OnOverOut only on dom.html.Element")
                case None => throw new Error("OnOverOut only on single elem")
            }
    }


    object StyleRegister {
        private var existing : $[String] = Nil

        def add(styles : $[Style]) : $[String] = {
            val ss = styles.%(_.rules.any)

            if (ss.id.any && existing.has(ss.id).not) {
                var names = Set[String]()
                var rules = $[SimpleCSSRule]()

                ss.reverse.foreach { s =>
                    rules ++= s.rules./~(_.get).%(_.name.in(names).not)
                    names ++= s.rules./~(_.get)./(_.name)
                }

                val r = "." + ss.id + " {\n" + rules./(r => "    " + r.name + ": " + r.value + ";\n").join("") + "}\n"

                web.addCSSRule(r)

                existing :+= ss.id
            }

            (ss.id.some.$ ++ styles.%(_.rules.none)./(_.name))
        }
    }

}
