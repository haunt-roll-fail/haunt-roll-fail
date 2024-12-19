package arcs
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

    object styles extends BaseStyleMapping("arcs") {
        import rules._

        Red --> color("#cf002c")
        Yellow --> color("#d09f31")
        // Green --> color("#087033")
        Blue --> color("#0b4e98")
        White --> color("#cccccc")

        Material --> color("#9d4d8a")
        Fuel --> color("#886b29")
        Weapon --> color("#c05728")
        Relic --> color("#98d1dc")
        Psionic --> color("#e36386")

        Administration --> $(color("#898a82"), font.weight("bold"))
        Aggression --> $(color("#a82024"), font.weight("bold"))
        Construction --> $(color("#d65329"), font.weight("bold"))
        Mobilization --> $(color("#2e899b"), font.weight("bold"))

        object title extends CustomStyle(font.family("FMBolyarPro-900"), font.size("84%"))

        object token extends CustomStyle(width("2.8ex"), vertical.align("middle"), margin("0.2ex"))
        object building extends CustomStyle(height("3ex"), vertical.align("middle"), margin.left("-0.1ex"), margin.right("-0.1ex"), margin.top("-0.1ex"), margin.bottom("0.2ex"))
        object ship extends CustomStyle(height("2.4ex"), vertical.align("middle"), margin.left("-0.1ex"), margin.right("-0.1ex"), margin.top("-0.1ex"), margin.bottom("0.2ex"))
        object plaque extends CustomStyle(height("4.2ex"), vertical.align("middle"), margin("-0.1ex"))
        object plaqueContainer extends CustomStyle(margin("0.1ex"))
        object tokenMinion extends CustomStyle(width("3.2ex"), vertical.align("middle"), padding.left("0.4ex"), padding.right("0.4ex"), padding.top("0.65ex"), padding.bottom("0.65ex"))
        object tokenBuilding extends CustomStyle(height("2.8ex"), vertical.align("middle"), padding("0.6ex"))

        object token2x extends CustomStyle(width("5.6ex"), vertical.align("middle"), margin("0.1ex"))
        object token3x extends CustomStyle(width("8.4ex"), vertical.align("middle"), margin("0.0ex"))
        object ship3x extends CustomStyle(height("7.2ex"), vertical.align("middle"), margin.left("-0.3ex"), margin.right("-0.3ex"), margin.top("-0.3ex"), margin.bottom("0.6ex"))

        object qship extends CustomStyle(height("5.6ex"), vertical.align("middle"), margin("0.0ex"))
        object qbuilding extends CustomStyle(height("6.9ex"), vertical.align("middle"), margin.top("-0.5ex"), margin.bottom("-0.8ex"))

        object circle extends CustomStyle(border.radius("5ex"))

        object condensed extends CustomStyle(letter.spacing("-0.125ch"))
        object italic extends CustomStyle(font.style("italic"))

        object green extends CustomStyle(color("#8a9255"))
        object brown extends CustomStyle(color("#a6783e"))

        object money extends CustomStyle(color("gold"))
        object power extends CustomStyle(color("#e7ce4d"))
        object hit extends CustomStyle(color("#dc143c"))
        object area extends CustomStyle(color("#88aa88"))

        object roll extends CustomStyle(border.width("2px"), border.style("solid"), border.radius("2ex"), font.size("108%"), font.weight("bold"), color("#dc143c"), background.color("#000000"), width("3.4ex"), height("3.4ex"), text.indent("0"), display("inline-flex"), justify.content("center"), align.items("center"))
        object rollOut extends CustomStyle(font.size("108%"), font.weight("bold"), color("#dc143c"), background.color("#dc143c"), width("4.4ex"), height("4.4ex"), text.indent("0"), display("inline-flex"), justify.content("center"), align.items("center"), margin.top("-1ex"), margin.bottom("-1ex"), margin.left("-0.5ex"), margin.right("-0.5ex"))
        object rollIn extends CustomStyle(background.color("#000000"), width("3.6ex"), height("3.6ex"), display("inline-flex"), justify.content("center"), align.items("center"))
        object expandRoll extends CustomStyle(margin.top("0"), margin.bottom("0"))
        object hexagon extends CustomStyle(clip.path("polygon(50% 5%, 90% 27%, 90% 70%, 50% 90%, 10% 70%, 10% 27%)"))

        object fund extends CustomStyle(height("2.6ex"), vertical.align("middle"), margin("0.2ex"))
        object figureLine extends CustomStyle(margin.left("1ex"), margin.right("1ex"))

        object pile extends CustomStyle(height("3.3ex"), vertical.align("middle"), padding.left("0.5ex"), padding.right("0.5ex"))

        object narrow extends CustomStyle(letter.spacing("-0.5ch"), vertical.align("middle"))

        object card extends CustomStyle(display("block"), width("14.88ex"), height("20.78ex"))
        object leaderCard extends CustomStyle(display("block"), width("14.88ex"), height("25.50ex"))
        object courtCard extends CustomStyle(display("block"), width("20.00ex"), height("27.93ex"))
        object fateCard extends CustomStyle(display("block"), width("22.32ex"), height("37.75ex"))
        object setupCard extends CustomStyle(display("block"), width("50ex"))
        object card0 extends CustomStyle(padding("5px"), padding("0.5vmin"), margin("0px"), margin("0.0vmin"), border.color("transparent"), border.style("solid"), border.width("0.3vmin"))
        object cardX extends CustomStyle(padding("5px"), padding("0.5vmin"), margin("8px"), margin("0.8vmin"), border.color("transparent"), border.style("solid"), border.width("0.3vmin"))
        object cardS extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), border.color("#ffffff"))
        object cardI extends CustomStyle(margin("1.2ex"), outline.width("0.8ex"))

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

        object statusUpper extends CustomStyle(height("100%"), overflow.x("hidden"), overflow.y("auto"))
        object play extends CustomStyle(margin.top("-4.2ex"))
        object initative extends CustomStyle(font.size("160%"), line.height("0"), vertical.align("sub"), color("#ffffff"))
        object cardName extends CustomStyle(margin.top("-0.6ex"), margin.bottom("-0.9ex"))


        object quasi extends CustomStyle(background.color("transparent"), padding("0.2ch"), margin("0.1ch"), outline.style("none"), border.style("solid"), border.width("0.2ch"), border.color("transparent"))
        object selfigure1 extends CustomStyle(border.color("#dc143c"), border.style("dashed"))
        object selfigure2 extends CustomStyle(border.color("#dc143c"), border.style("solid"))
        object unquasi extends CustomStyle(filter("brightness(0.9)"))

        object keyLine extends CustomStyle(margin.top("-0.8ex"), margin.bottom("-0.6ex"))
        object outrageLine extends CustomStyle(margin.top("-0.1ex"), margin.bottom("-0.6ex"))
        object tokenTop extends CustomStyle(width("2.8ex"), vertical.align("top"), margin("0.2ex"))

        object infoStatus extends CustomStyle(line.height("100%"))

        object notDoneYet extends CustomStyle(color("darkred"), text.decoration.line("line-through"), text.decoration.style("wavy"))

        object outlined extends CustomStyle(border.width("2px"), border.width("0.2ex"), border.style("solid"), background.color("#222222"), margin("0.1ex"), display("inline-block"), text.indent("0"))
    }

    implicit class ElemString(val s : String) extends AnyVal {
        def roll = s.spn(styles.roll)
    }

    implicit class ElemElem(val elem : Elem) extends AnyVal {
        def larger = elem.styled(xstyles.larger125)
    }

    implicit class ElemInt(val n : Int) extends AnyVal {
        def power = (n >= 0).?(0x27C5.toChar.toString.hh ~ n.styled(xstyles.bold).styled(styles.power) ~ 0x27C6.toChar.toString.hh).|(("-" + n.abs + "P").styled(styles.hit))
        def cards = (n != 1).?(n.hl ~ " cards").|("a card")
        def minions = (n != 1).?(n.hl ~ " minions").|("a minion")
        def roll = n.toString.spn(styles.rollIn)(styles.hexagon).spn(styles.rollOut)(styles.hexagon)
        def hit = (n != 1).?(n.hlb ~ " Hits").|(1.hlb ~ " Hit").styled(styles.hit)
    }

    object borders extends BaseStyleMapping("arcs-border") {
        import rules._

        Red --> outline.color("#680016")
        Yellow --> outline.color("#684f19")
        // Green --> outline.color("#087033")
        Blue --> outline.color("#05274c")
        White --> outline.color("#666666")
    }
}
