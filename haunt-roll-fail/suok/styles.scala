package suok
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

    object styles extends BaseStyleMapping("suok") {
        import rules._

        Violet --> color("#8e24aa")
        Teal --> color("teal")
        Pink --> color("pink")
        Green --> color("#00cc00")
        Red --> color("red")
        Cyan --> color("cyan")
        Yellow --> color("yellow")
        White --> color("#eeeeee")
        Blue --> color("#0040ff")
        Orange --> color("orange")

        Loyal --> color("#2ca8f9")
        Rebel --> color("#f7422d")
        Neutral --> color("#38763c")

        Effect --> color("#4a3a2a")

        object inline extends CustomStyle(display("inline-block"))
        object margined extends CustomStyle(margin("0.3ch"))

        object selected extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), outline.color("#ffffff"), outline.style("solid"), outline.width("0.3vmin"))

        object card extends CustomStyle(display("block"), width("9ch"))
        object rcard extends CustomStyle(display("block"), width("18ch"))
        object scard extends CustomStyle(display("inline-block"), width("9ch"))

        object dead extends CustomStyle(filter("grayscale(1) opacity(0.4)"))
        object hidden extends CustomStyle(filter("opacity(0.6)"))

        object deadname extends CustomStyle(color("#707070"))
        object used extends CustomStyle(color("darkred"), text.decoration.line("line-through"), text.decoration.style("wavy"))

        object kill extends CustomStyle(color("crimson"))

        object name extends CustomStyle(font.weight("bold"))
        object username extends CustomStyle(font.size("60%"))

        object board extends CustomStyle(border.width("1vmin"))

        object yes extends CustomStyle(border.width("1px"), border.style("solid"), color("black"), background.color("white"))
        object no extends CustomStyle(border.width("1px"), border.style("solid"), color("white"), background.color("black"))
        object vote extends CustomStyle(display("inline-block"), width("5ch"), text.align("center"))
        object longvote extends CustomStyle(box.sizing("border.box"), width("100%"))

        object president extends CustomStyle(border.width("1px"), border.style("solid"), color("black"), background.color("#cccccc"), font.weight("bold"))
        object chancellor extends CustomStyle(border.width("1px"), border.style("solid"), color("white"), background.color("#444444"), font.style("italic"))
        object title extends CustomStyle(font.variant("small-caps"), font.size("80%"), display("inline-block"), width("11ch"), text.align("center"))

        object limited extends CustomStyle(font.style("italic"), color("gray"), font.size("60%"))

        object viewcard extends CustomStyle(display("inline-block"), margin("0.3ch"))

        object break extends CustomStyle(margin("-0.3ch"))

        object current extends CustomStyle(outline.width("2px"), outline.style("solid"), outline.color("white"))

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

    object borders extends BaseStyleMapping("sehi-border") {
        import rules._

        Violet --> border.color("#8e24aa")
        Teal --> border.color("teal")
        Pink --> border.color("pink")
        Green --> border.color("#00cc00")
        Red --> border.color("red")
        Cyan --> border.color("cyan")
        Yellow --> border.color("yellow")
        White --> border.color("#eeeeee")
        Blue --> border.color("#0040ff")
        Orange --> border.color("orange")
    }
}
