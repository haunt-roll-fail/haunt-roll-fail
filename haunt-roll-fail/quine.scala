package hrf.quine
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

import hrf._
import hrf.loader._
import hrf.meta._
import hrf.html._

import scalajs.js

import org.scalajs.dom

import scala.collection.decorators._


object Quine {
    def openTag(name : String) = "<!-- { | " + name + " | { -->"
    def closeTag(name : String) = "<!-- } | " + name + " | } -->"

    def replaceSections(replace : $[(String, $[String])])(s : String) : $[String] = replace match {
        case Nil => $(s)
        case (name, value) :: rest =>
            val a = openTag(name)
            val b = closeTag(name)

            val l1 = s.splt(a)

            val l2 = l1.take(1)./(_.splt(b) match {
                case List(x) => x
                case _ => throw new Error("orphaned closing tag " + b)
            }) ++ l1.drop(1)./(_.splt(b) match {
                case List(_, y) => y
                case List(_) => throw new Error("missing closing tag " + b)
                case _ => throw new Error("extra closing tag " + b)
            })

            val l3 = l2./(replaceSections(rest)).intersperse($(a) ++ value ++ $(b)).flatten

            l3
    }

    def replaceUrls(substitutions : $[(String, String)])(l : $[String]) : $[String] = l./{ s =>
        substitutions.foldLeft(s) { case (s, (url, data)) =>
            val a = openTag(url)
            val b = closeTag(url)

            val l1 : $[String] = s.splt(a)

            val l2 : $[String] = l1.take(1)./(_.splt(b) match {
                case List(x) => x
                case _ => throw new Error("orphaned closing tag " + b)
            }) ++ l1.drop(1)./(_.splt(b) match {
                case List(_, y) => y
                case List(_) => throw new Error("missing closing tag " + b)
                case Nil => throw new Error("extra closing tag " + b)
            })

            val l3 : $[String] = l2.intersperse(data)

            l3.join("")
        }
    }

    def save(meta : MetaGame)(title : String, seating : $[meta.F], options : $[meta.O], resources : Resources, journal : Journal[meta.gaming.ExternalAction], filename : String, replay : Boolean, server : String, onSave : => Unit) {
        val tab = "\n        "

        if (HRF.embedded) {
            val html : String = HRF.originalOuterHtml

            journal.read(0) { actions =>
                val story = actions./(meta.writeActionExternal)
                val lobby = $("meta " + meta.name, "version " + meta.gaming.version, "title " + title) ++
                    seating./(f => "user " + meta.writeFaction(f) + " player-" + meta.writeFaction(f).toLowerCase) ++
                    seating.%(f => resources.getName(f).any)./(f => "name player-" + meta.writeFaction(f).toLowerCase + " " + resources.getName(f).get) ++
                    $("seating " + seating./(meta.writeFaction).join(" ")) ++
                    $("options " + options./(meta.writeOption).join(" "))

                val result = replaceSections($(
                    "settings".toUpperCase -> $(tab, "<title id=\"settings\" data-embedded-assets=\"true\" data-server=\"" + server + "\" data-replay=\"", replay.??("true"), "\" data-meta=\"", meta.name, "\" >", title, " | ", meta.label, "</title>", tab),
                    "replay".toUpperCase -> replay.??($(tab, "<div id=\"lobby\" style=\"display: none\" >") ++ lobby./~($(tab, "    ", _)) ++ $(tab, "</div>", tab, "<div id=\"replay\" style=\"display: none\" >") ++ story./~($(tab, "    ", _)) ++ $(tab, "</div>", tab)),
                ))(html).join("")

                val blob = {
                    import scalajs.js.typedarray._
                    import scalajs.js.JSConverters._

                    new dom.Blob(js.Array(result.getBytes().toTypedArray), new dom.BlobPropertyBag { `type` = "text/html" })
                }

                val link = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
                link.href = dom.URL.createObjectURL(blob)
                link.asInstanceOf[js.Dynamic].download = filename.replace(" ", "_").replace("/", "-").replace(":", "-") + ".html"
                dom.document.body.appendChild(link)
                link.click()
                onSave
            }
        }
        else
        HRF.stringLoader.wait($(HRF.html, HRF.script)) {
            val html = HRF.stringLoader.get(HRF.html).splt("\n")
            val script = HRF.stringLoader.get(HRF.script)

            val urls = html.%(_.trim.startsWith("url(\""))./~(_.splt("\"").lift(1))

            val nhtml = html./(s => s.trim.startsWith("url(\"").?(s.splt("\"").use(l => l(0) + openTag("URL " + l(1)) + l(1) + closeTag("URL " + l(1)) + l.drop(2).join("\""))).|(s))

            val loader = HRF.embedded.?(new EmbeddedImageSourceLoader(s => "asset-" + s)).|(DataUrlLoader)

            loader.wait(urls ++ resources.images.sources.values.$) {
                journal.read(0) { actions =>
                    val assets = resources.images.sources.keys.$./~(k => $(tab, "<image style=\"display: none\" id=\"asset-", k, "\" src=\"", loader.get(resources.images.sources(k)), "\" />"))

                    val story = actions./(meta.writeActionExternal)
                    val lobby = $("meta " + meta.name, "version " + meta.gaming.version, "title " + title) ++
                        seating./(f => "user " + meta.writeFaction(f) + " player-" + meta.writeFaction(f).toLowerCase) ++
                        seating.%(f => resources.getName(f).any)./(f => "name player-" + meta.writeFaction(f).toLowerCase + " " + resources.getName(f).get) ++
                        $("seating " + seating./(meta.writeFaction).join(" ")) ++
                        $("options " + options./(meta.writeOption).join(" "))

                    val uhtml = replaceUrls(urls./(u => ("URL " + u) -> DataUrlLoader.get(u)))(nhtml)

                    val result = replaceSections($(
                        "base href".toUpperCase -> $(tab),
                        "settings".toUpperCase -> $(tab, "<title id=\"settings\" data-embedded-assets=\"true\" data-server=\"" + server + "\" data-replay=\"", replay.??("true"), "\" data-meta=\"", meta.name, "\" >", title, " | ", meta.label, "</title>", tab),
                        "script".toUpperCase -> $(tab, "<script id=\"script\" type=\"text/javascript\" >\n", script, tab, "</scr", "ipt>", tab),
                        "replay".toUpperCase -> replay.??($(tab, "<div id=\"lobby\" style=\"display: none\" >") ++ lobby./~($(tab, "    ", _)) ++ $(tab, "</div>", tab, "<div id=\"replay\" style=\"display: none\" >") ++ story./~($(tab, "    ", _)) ++ $(tab, "</div>", tab)),
                        "assets".toUpperCase -> (assets ++ $(tab)),
                        "cache invalidation".toUpperCase -> $(tab)
                    ))(uhtml.join("\n")).join("")

                    val blob = {
                        import scalajs.js.typedarray._
                        import scalajs.js.JSConverters._

                        new dom.Blob(js.Array(result.getBytes().toTypedArray), new dom.BlobPropertyBag { `type` = "text/html" })
                    }

                    val link = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
                    link.href = dom.URL.createObjectURL(blob)
                    link.asInstanceOf[js.Dynamic].download = filename.replace(" ", "_").replace("/", "-").replace(":", "-") + ".html"
                    dom.document.body.appendChild(link)
                    link.click()
                    onSave
                }
            }
        }
    }

}
