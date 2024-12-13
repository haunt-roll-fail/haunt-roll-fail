package dwam
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

    object dt {
        val Arrow = Span(Text(0x2192.toChar.toString))
        val PlayAnother = Image("icon:another-card", styles.token)
        val Scroll = Image("icon:scroll", styles.token)
        val Money = Image("icon:money", styles.token)
        val Assassinate = Image("icon:assassinate", styles.token)


        def Minion(f : Faction) = Image(f.style + "-" + "minion", styles.tokenMinion)
        def Build(f : Faction) = Image(f.style + "-" + "building", styles.tokenBuilding)
        val RemoveTrouble = Image("icon:remove-trouble", styles.token)
        val Interrupt = Image("icon:interrupt", styles.token)
        val Event = Image("icon:event", styles.token)
    }

    object styles extends BaseStyleMapping("dwam") {
        import rules._

        Red --> color("#cf002c")
        Yellow --> color("#d09f31")
        Green --> color("#087033")
        Blue --> color("#0b4e98")
        White --> color("#cccccc")


        object token extends CustomStyle(width("4ex"), vertical.align("middle"), padding("0ex"))
        object tokenMinion extends CustomStyle(width("3.2ex"), vertical.align("middle"), padding.left("0.4ex"), padding.right("0.4ex"), padding.top("0.65ex"), padding.bottom("0.65ex"))
        object tokenBuilding extends CustomStyle(height("2.8ex"), vertical.align("middle"), padding("0.6ex"))

        object condensed extends CustomStyle(letter.spacing("-0.125ch"))
        object italic extends CustomStyle(font.style("italic"))




        object green extends CustomStyle(color("#8a9255"))
        object brown extends CustomStyle(color("#a6783e"))

        Troubles --> (color("#000000") :: text.shadow("-1px -1px 0px #aaaaaa, 1px -1px 0px #aaaaaa, -1px 1px 0px #aaaaaa, 1px 1px 0px #aaaaaa"))
        Demons --> color("#f53105")
        Trolls --> color("#472c22")


        object money extends CustomStyle(color("gold"))
        object hit extends CustomStyle(color("#dc143c"))
        object area extends CustomStyle(color("#88aa88"))


        object roll extends CustomStyle(border.width("2px"), border.style("solid"), border.radius("2ex"), font.size("108%"), font.weight("bold"), color("#dc143c"), background.color("#000000"), width("3.4ex"), height("3.4ex"), text.indent("0"), display("inline-flex"), justify.content("center"), align.items("center"))
        object rollOut extends CustomStyle(font.size("108%"), font.weight("bold"), color("#dc143c"), background.color("#dc143c"), width("4.4ex"), height("4.4ex"), text.indent("0"), display("inline-flex"), justify.content("center"), align.items("center"), margin.top("-1ex"), margin.bottom("-1ex"), margin.left("-0.5ex"), margin.right("-0.5ex"))
        object rollIn extends CustomStyle(background.color("#000000"), width("3.6ex"), height("3.6ex"), display("inline-flex"), justify.content("center"), align.items("center"))
        object expandRoll extends CustomStyle(margin.top("0"), margin.bottom("0"))
        object hexagon extends CustomStyle(clip.path("polygon(50% 5%, 90% 27%, 90% 70%, 50% 90%, 10% 70%, 10% 27%)"))

        object fund extends CustomStyle(height("2.5ex"), vertical.align("middle"))
        object fund5 extends CustomStyle(height("2.5ex"), vertical.align("middle"))


        object pile extends CustomStyle(height("3.3ex"), vertical.align("middle"), padding.left("0.5ex"), padding.right("0.5ex"))

        object narrow extends CustomStyle(letter.spacing("-0.5ch"), vertical.align("middle"))


        object card extends CustomStyle(display("block"), width("19.60ex"), height("31.42ex"))
        object cardX extends CustomStyle(padding("5px"), padding("0.5vmin"), margin("8px"), margin("0.8vmin"), border.color("transparent"), border.style("solid"), border.width("0.3vmin"), border.radius("2ex"))
        object cardS extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), border.color("#ffffff"))


        object group extends CustomStyle(margin.top("0.5ex"), margin.bottom("0.5ex"))
        object inline extends CustomStyle(display("inline-block"))
        object nomargin extends CustomStyle(margin("0"))
        object nopadding extends CustomStyle(padding("0"))
        object halfmargin extends CustomStyle(margin.left("0.2ex"), margin.right("0.2ex"), margin.top("0.2ex"), margin.bottom("0.2ex"))

        object selected extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), outline.color("#ffffff"), outline.style("solid"), outline.width("0.3vmin"))

        object smallname extends CustomStyle(font.size("90%"), font.weight("bold"))

        object center extends CustomStyle(text.align("center"))
        object effect extends CustomStyle(font.size("60%"), font.variant("small-caps"))


        object artwork extends CustomStyle(max.height("100%"), max.width("100%"), margin("auto"))
        object artwork84 extends CustomStyle(max.height("84%"), max.width("100%"))
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
        object gstatus extends CustomStyle(font.size("110%"), max.height("100%"), max.width("100%"), display("flex"), flex.direction("column"), justify.content("space-evenly"), align.items("center"), width("100%"), height("100%"))
    }

    implicit class ElemElem(val s : String) extends AnyVal {

        def roll = s.spn(styles.roll)
    }

    implicit class ElemInt(val n : Int) extends AnyVal {
        def money = (n >= 0).?(("$" + n).styled(styles.money)).|(("-$" + n.abs).styled(styles.hit))
        def cards = (n != 1).?(n.hl ~ " cards").|("a card")
        def minions = (n != 1).?(n.hl ~ " minions").|("a minion")
        def roll = n.toString.spn(styles.rollIn)(styles.hexagon).spn(styles.rollOut)(styles.hexagon)
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
