package hrf.quine

import colmat._

import scala.scalajs.js

import org.scalajs.dom

import hrf._
import hrf.loader._
import hrf.meta._
import hrf.html._

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

    def save(meta : MetaGame)(game : meta.G, title : String, seating : $[meta.F], options : $[meta.O], resources : Resources, journal : Journal[meta.gaming.ExternalAction], date : js.Date, onSave : => Unit) {
        journal.read(0) { actions =>
            val html = hrf.loader.StringLoader.get(HRF.html)
            val script = hrf.loader.StringLoader.get(HRF.script)
            
            val tab = "\n        "
            
            val assets = resources.assets.keys.toList.sortBy(k => resources.assets(k).height.toString.reverse.padTo(4, '0').reverse + " " + k)./~(k => $(tab, "<image style=\"display: none\" id=\"asset-", k, "\" src=\"", hrf.loader.DataUrlLoader.get(resources.assets(k).src), "\" />"))
            
            val replay = actions./(meta.writeActionExternal)
            val lobby = $("meta " + meta.name, "version 2", "title " + title) ++
                seating./(f => "user " + meta.writeFaction(f) + " player-" + meta.writeFaction(f).toLowerCase) ++
                seating.%(f => resources.getName(f).any)./(f => "name player-" + meta.writeFaction(f).toLowerCase + " " + resources.getName(f)) ++
                $("seating " + seating./(meta.writeFaction).join(" ")) ++
                $("options " + options./(meta.writeOption).join(" "))
  
            val result = replaceSections($(
                "settings" -> $(tab, "<title id=\"settings\" data-offline=\"true\" data-online=\"false\" data-embedded-assets=\"true\" data-replay=\"true\" data-meta=\"", meta.name, "\" >" + title + " " + meta.name + "</title>", tab),
                "script" -> $(tab, "<script id=\"script\" type=\"text/javascript\" >\n", script, tab, "</scr", "ipt>", tab),
                "replay" -> ($(tab, "<div id=\"lobby\" style=\"display: none\" >") ++ lobby./~($(tab, "    ", _)) ++ $(tab, "</div>", tab, "<div id=\"replay\" style=\"display: none\" >") ++ replay./~($(tab, "    ", _)) ++ $(tab, "</div>", tab)),
                "assets" -> (assets ++ $(tab))
            ))(html).join("")
            
            val blob = {
                import scala.scalajs.js.typedarray._
                import scala.scalajs.js.JSConverters._
                
                new dom.Blob(js.Array(result.getBytes().toTypedArray), dom.BlobPropertyBag("text/html"))
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
