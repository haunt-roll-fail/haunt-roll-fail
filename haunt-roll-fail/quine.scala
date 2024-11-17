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
    def replaceSections(replace : $[(String, $[String])])(s : String) : $[String] = replace match {
        case Nil => $(s)
        case (name, value) :: rest =>
            val a = "<!-- { | " + name.toUpperCase + " | { -->"
            val b = "<!-- } | " + name.toUpperCase + " | } -->"

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

    def save(meta : MetaGame)(title : String, seating : $[meta.F], options : $[meta.O], resources : Resources, journal : Journal[meta.gaming.ExternalAction], date : js.Date, onSave : => Unit) {
        HRF.stringCache.wait($(HRF.html, HRF.script)) {
            DataUrlLoader.wait(resources.images.sources.values.$) {
                journal.read(0) { actions =>
                    val html = HRF.stringCache.get(HRF.html)
                    val script = HRF.stringCache.get(HRF.script)

                    val tab = "\n        "

                    val assets = resources.images.sources.keys.$./~(k => $(tab, "<image style=\"display: none\" id=\"asset-", k, "\" src=\"", DataUrlLoader.get(resources.images.sources(k)), "\" />"))

                    val replay = actions./(meta.writeActionExternal)
                    val lobby = $("meta " + meta.name, "version " + meta.gaming.version, "title " + title) ++
                        seating./(f => "user " + meta.writeFaction(f) + " player-" + meta.writeFaction(f).toLowerCase) ++
                        seating.%(f => resources.getName(f).any)./(f => "name player-" + meta.writeFaction(f).toLowerCase + " " + resources.getName(f).get) ++
                        $("seating " + seating./(meta.writeFaction).join(" ")) ++
                        $("options " + options./(meta.writeOption).join(" "))

                    val result = replaceSections($(
                        "settings" -> $(tab, "<title id=\"settings\" data-offline=\"true\" data-online=\"false\" data-embedded-assets=\"true\" data-replay=\"true\" data-meta=\"", meta.name, "\" >" + title + " " + meta.name + "</title>", tab),
                        "script" -> $(tab, "<script id=\"script\" type=\"text/javascript\" >\n", script, tab, "</scr", "ipt>", tab),
                        "replay" -> ($(tab, "<div id=\"lobby\" style=\"display: none\" >") ++ lobby./~($(tab, "    ", _)) ++ $(tab, "</div>", tab, "<div id=\"replay\" style=\"display: none\" >") ++ replay./~($(tab, "    ", _)) ++ $(tab, "</div>", tab)),
                        "assets" -> (assets ++ $(tab))
                    ))(html).join("")

                    val blob = {
                        import scalajs.js.typedarray._
                        import scalajs.js.JSConverters._

                        new dom.Blob(js.Array(result.getBytes().toTypedArray), new dom.BlobPropertyBag { `type` = "text/html" })
                    }

                    val name = "hrf--" + meta.name + "--" + date.toISOString().take(16).replace("T", "--")

                    val link = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
                    link.href = dom.URL.createObjectURL(blob)
                    link.asInstanceOf[js.Dynamic].download = name.replace(" ", "_").replace("/", "-").replace(":", "-") + ".html"
                    dom.document.body.appendChild(link)
                    link.click()
                    onSave
                }
            }
        }
    }

}
