package yarg
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

package object elem {
    import hrf.elem._

    def mod(from : Int, to : Int = -999) : Elem =
        (if (from == 0)
            "0".hl
        else
        if (from > 0)
            ("+" ~ from.toString).hl
        else
            ("-" ~ from.abs.toString).hl) ~ (to != -999).?(hrf.elem.MDash.toString ~ to.toString.hl)

    object styles extends BaseStyleMapping("yarg") {
        import rules._

        Pirates --> color("#007FFF")
        FireKnights --> color("#FF0000")
        Mages --> color("#c000c0")

        object health extends CustomStyle(color("#cc2012"))
        object mana extends CustomStyle(color("#0b3dcc"))
        object rage extends CustomStyle(color("#c4c400"))

        object inline extends CustomStyle(display("inline-block"))
        object margined extends CustomStyle(margin("0.3ch"))

        object selected extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), outline.color("#ffffff"), outline.style("solid"), outline.width("0.3vmin"))

        object card extends CustomStyle(display("block"), width("9ch"))

        object dead extends CustomStyle(color("#707070"))
        object used extends CustomStyle(color("darkred"), text.decoration.line("line-through"), text.decoration.style("wavy"))

        object kill extends CustomStyle(color("crimson"))

        object name extends CustomStyle(font.weight("bold"))
        object username extends CustomStyle(font.size("60%"))

        object board extends CustomStyle(border.width("1vmin"))

        object yes extends CustomStyle(border.width("1px"), border.style("solid"), color("black"), background.color("white"))
        object no extends CustomStyle(border.width("1px"), border.style("solid"), color("white"), background.color("black"))
        object vote extends CustomStyle(display("inline-block"), width("5ch"), text.align("center"))
        object widevote extends CustomStyle(box.sizing("border.box"), width("100%"))

        object president extends CustomStyle(border.width("1px"), border.style("solid"), color("black"), background.color("#cccccc"), font.weight("bold"))
        object chancellor extends CustomStyle(border.width("1px"), border.style("solid"), color("white"), background.color("#444444"), font.style("italic"))
        object title extends CustomStyle(font.variant("small-caps"), font.size("80%"), display("inline-block"), width("11ch"), text.align("center"))

        object limited extends CustomStyle(font.style("italic"), color("gray"), font.size("60%"))

        object status extends CustomStyle(
            border.width("4px"),
            border.width("0.4vmin"),
            text.align("center"),
            font.size("90%"),
            white.space("nowrap"),
            overflow.x("hidden"),
            overflow.y("hidden"),
            text.overflow("ellipsis")
        )
    }

    object borders extends BaseStyleMapping("yarg-border") {
        import rules._

        Pirates --> outline.color("#0055AA")
        FireKnights --> outline.color("#AA0000")
        Mages --> outline.color("#800080")
    }
}
