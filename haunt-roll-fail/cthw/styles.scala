package cthw
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

package object elem {

    object styles extends BaseStyleMapping("cthw") {
        val base = xstyles

        import rules._

        GC --> color("#008000")
        CC --> color("#3344ff")
        BG --> color("#ff0000")
        YS --> color("#cccc00")
        SL --> color("#ef764b")
        WW --> color("#a5cbe1")
        OW --> color("#995593")

        object sea extends CustomStyle()
        object hit extends CustomStyle()
        object dead extends CustomStyle()
        object used extends CustomStyle()
        object money extends CustomStyle()
        object name extends CustomStyle()
        object username extends CustomStyle()
        object token extends CustomStyle()
        object viewcard extends CustomStyle()

        object status extends CustomStyle(
            border.width("4px"),
            border.width("0.4vmin"),
            text.align("center"),
            font.size("90%"),
            overflow.x("hidden"),
            overflow.y("hidden"),
            text.overflow("ellipsis")
        )
    }

    object borders extends BaseStyleMapping("cthw-border") {
        import rules._

    }

    implicit class ElemInt(val n : Int) extends AnyVal {
        def of(s : String) = (n != 1).?(n.hl ~ " " ~ s ~ "s").|("a " ~ s)
        def ofb(s : String) = (n != 1).?(n.hlb ~ " " ~ s ~ "s").|("a " ~ s)
    }

}
