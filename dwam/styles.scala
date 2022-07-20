package dwam

import colmat._

package object elem {
    import hrf.elem._

    val MDash = Text(0x2014.toChar.toString)
    
    object styles extends BaseStyleMapping("dwam") {
        import rules._

        Red --> color("#cf002c")
        Yellow --> color("#d09f31")
        Green --> color("#087033")
        Blue --> color("#0b4e98")
        White --> color("#cccccc")

        object green extends CustomStyle(color("#8a9255"))
        object brown extends CustomStyle(color("#a6783e"))
        
        Troubles --> (color("#000000") :: text.shadow("-1px -1px 0px #aaaaaa, 1px -1px 0px #aaaaaa, -1px 1px 0px #aaaaaa, 1px 1px 0px #aaaaaa"))
        Demons --> color("#f53105")
        Trolls --> color("#472c22")
        
        object money extends CustomStyle(color("gold"))
        object hit extends CustomStyle(color("#dc143c"))
        object area extends CustomStyle(color("#88aa88"))
        
        object roll extends CustomStyle(border.width("1px"), border.style("solid"), border.radius("2ch"), font.size("120%"), color("white"))

        object fund extends CustomStyle(width("1.5ch"), vertical.align("bottom"), padding.left("0.10ch"), padding.right("0.10ch"))
        object fund5 extends CustomStyle(width("3.0ch"), vertical.align("bottom"), padding.left("0.10ch"), padding.right("0.10ch"))

        object pile extends CustomStyle(width("4.5ch"), vertical.align("middle"), padding.left("0.5ch"), padding.right("0.5ch"))

        object narrow extends CustomStyle(letter.spacing("-0.5ch"), vertical.align("middle"))
        
        object card extends CustomStyle(display("block"), width("18ch"), height("28.85ch"))
        object cardX extends CustomStyle(padding("5px"), padding("0.5vmin"), margin("8px"), margin("0.8vmin"), border.color("transparent"), border.style("solid"), border.width("0.3vmin"), border.radius("2ch"))
        object cardS extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), border.color("#ffffff"))
        
        object group extends CustomStyle(margin("0.3ch"))
        object inline extends CustomStyle(display("inline-block"))
        object nomargin extends CustomStyle(margin("0"))
        object halfmargin extends CustomStyle(margin.left("0.2ch"), margin.right("0.2ch"), margin.top("0.2ch"), margin.bottom("0.2ch"))
        object selected extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), outline.color("#ffffff"), outline.style("solid"), outline.width("0.3vmin"))
 
        object smallname extends CustomStyle(font.size("90%"), font.weight("bold"))
 
        object center extends CustomStyle(text.align("center"))
        object effect extends CustomStyle(font.size("80%"), font.variant("small-caps"), line.height("90%"), margin("0.3ch"))
        
        object artwork extends CustomStyle(max.height("100%"), max.width("100%"), margin("auto"))
        object middleScrollOut extends CustomStyle(display("flex"), align.items("center"), flex.wrap("wrap"), height("100%"), width("100%"))
        object middleScrollIn extends CustomStyle(overflow.y("auto"), height("auto"), margin("auto"))
        object seeThroughInner extends CustomStyle(background.color("#222222e0"))
 
        object status extends CustomStyle(
            border.width("4px"),
            border.width("0.4vmin"),
            text.align("center"), 
            overflow.x("hidden"),
            overflow.y("auto"),
            text.overflow("ellipsis")
        )

        object fstatus extends CustomStyle(font.size("115%"))
        object gstatus extends CustomStyle(font.size("110%"), text.align("center"))
    }
    
    implicit class ElemInt(val n : Int) extends AnyVal {
        def money = (n >= 0).?(("$" + n).styled(styles.money)).|(("-$" + n.abs).styled(styles.hit))
        def cards = (n != 1).?(n.hl ~ " cards").|("a card")
        def minions = (n != 1).?(n.hl ~ " minions").|("a minion")
        def roll = (" " + n + " ").pre.spn(styles.roll)
    }

    object borders extends BaseStyleMapping("dwam-border") {
        import rules._

        Red --> outline.color("#680016")
        Yellow --> outline.color("#d09f31")
        Green --> outline.color("#087033")
        Blue --> outline.color("#0b4e98")
        White --> outline.color("#cccccc")
    }
}
