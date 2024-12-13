package coup
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
    object styles extends BaseStyleMapping("coup") {
        import rules._

        Duke --> color("#f32d74")
        Assassin --> (color("#2a2c28"), text.shadow("0px 1px 2px #e0f0ff"))
        Contessa --> color("#e94327")
        Ambassador --> color("#c2dc21")
        Captain --> color("#5bbfee")
        Coup --> color("#ffffff")

        Ganymede --> color("#708090")
        Io --> color("#908070")
        Europa --> color("#809070")
        Callisto --> color("#709080")
        Amalthea --> color("#907080")
        Thebe --> color("#807090")

        object selected extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), outline.color("#ffffff"), outline.style("solid"), outline.width("0.3vmin"))

        object token extends CustomStyle(display("block"), width("2ex"))

        object dead extends CustomStyle(color("#707070"))
        object used extends CustomStyle(color("darkred"), text.decoration.line("line-through"), text.decoration.style("wavy"))

        object card extends CustomStyle(display("block"), width("15ex"))

        object kill extends CustomStyle(color("crimson"))

        object name extends CustomStyle(font.weight("bold"), padding("0"), font.size("150%"))
        object username extends CustomStyle(font.size("60%"))

        object viewcard extends CustomStyle(display("inline-block"), margin("0.3ex"))

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

    implicit class ElemInt(val n : Int) extends AnyVal {
        def cards = (n != 1).?(n.hl ~ " cards").|("a card")
    }

    object borders extends BaseStyleMapping("coup-border") {
        import rules._

        Ganymede --> border.color("#708090")
        Io --> border.color("#908070")
        Europa --> border.color("#809070")
        Callisto --> border.color("#709080")
        Amalthea --> border.color("#907080")
        Thebe --> border.color("#807090")
    }

    object outlines extends BaseStyleMapping("coup-border") {
        val xcolors = Map[Role, String](
            Duke -> "#f32d74",
            Assassin -> "#e0f0ff",
            Contessa -> "#e94327",
            Ambassador -> "#c2dc21",
            Captain -> "#5bbfee",
            Coup -> "#ffffff"
        )

        val colors = Map[Role, String](
            Duke -> "#f32d74",
            Assassin -> "#e94327",
            Contessa -> "#ffd700",
            Ambassador -> "#b2ec21",
            Captain -> "#5bbfee",
            Coup -> "#ffffff"
        )
    }

}