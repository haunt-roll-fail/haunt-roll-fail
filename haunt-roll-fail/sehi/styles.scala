package sehi
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

    object styles extends BaseStyleMapping("sehi") {
        import rules._

        Purple --> color("purple")
        Lime --> color("lime")
        Azure --> color("#007FFF")
        Green --> color("green")
        Red --> color("red")
        Electric --> color("darkcyan")
        Yellow --> color("yellow")
        White --> color("white")
        Blue --> color("blue")
        Orange --> color("orange")

        Liberal --> color("#60c6d8")
        Fascist --> color("#e66444")

        object inline extends CustomStyle(display("inline-block"))
        object margined extends CustomStyle(margin("0.3ex"))

        object selected extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), outline.color("#ffff00"), outline.style("solid"), outline.width("0.3vmin"))

        object card extends CustomStyle(display("block"), width("10ex"))

        object dead extends CustomStyle(color("#707070"))
        object used extends CustomStyle(color("darkred"), text.decoration.line("line-through"), text.decoration.style("wavy"))

        object kill extends CustomStyle(color("crimson"))

        object name extends CustomStyle(font.weight("bold"))
        object username extends CustomStyle(font.size("60%"))

        object board extends CustomStyle(border.width("1vmin"))

        object yes extends CustomStyle(border.width("1px"), border.style("solid"), color("black"), background.color("white"))
        object no extends CustomStyle(border.width("1px"), border.style("solid"), color("white"), background.color("black"))
        object vote extends CustomStyle(display("inline-block"), width("6ex"), text.align("center"), text.indent("0"))
        object widevote extends CustomStyle(box.sizing("border.box"), width("100%"))

        object president extends CustomStyle(border.width("1px"), border.style("solid"), color("black"), background.color("#cccccc"), font.weight("bold"))
        object chancellor extends CustomStyle(border.width("1px"), border.style("solid"), color("white"), background.color("#444444"), font.style("italic"))
        object title extends CustomStyle(font.variant("small-caps"), font.size("80%"), display("inline-block"), width("11ch"), text.align("center"), text.indent("0"))

        object limited extends CustomStyle(font.style("italic"), color("gray"), font.size("60%"))

        object viewcard extends CustomStyle(display("inline-block"), margin("0.3ex"), outline.style("none"))

        object status extends CustomStyle(
            border.width("4px"),
            border.width("0.4vmin"),
            text.align("center"),
            font.size("110%"),
            white.space("nowrap"),
            overflow.x("hidden"),
            overflow.y("hidden"),
            text.overflow("ellipsis")
        )
    }

    object borders extends BaseStyleMapping("sehi-border") {
        import rules._

        Purple --> border.color("purple")
        Lime --> border.color("lime")
        Azure --> border.color("#007FFF")
        Green --> border.color("green")
        Red --> border.color("red")
        Electric --> border.color("darkcyan")
        Yellow --> border.color("yellow")
        White --> border.color("white")
        Blue --> border.color("blue")
        Orange --> border.color("orange")
    }
}
