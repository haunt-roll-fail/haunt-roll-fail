package vast
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
        val Stamina = Image("stamina", styles.token)
        val Used = Image("stamina-used", styles.token)
        val Discard = Image("stamina-discard", styles.token)
        val Poison = Image("stamina-poison", styles.token)
        val Lost = Image("stamina-lost", styles.token)
        val MovementK = Image("movement-knight", styles.token)
        val PerceptionK = Image("perception-knight", styles.token)
        val StrengthK = Image("strength-knight", styles.token)

        val Movement = Image("movement", styles.token)
        val Perception = Image("perception", styles.token)
        val Strength = Image("strength", styles.token)
        val Sloth = Image("sloth", styles.token)
        val SlothFree = Image("sloth-free", styles.token)
        val SlothFreeStay = Image("sloth-free-stay", styles.token)
        val SlothFreeGems = Image("sloth-free-gems", styles.token)
        val SlothTreasure = Image("sloth-treasure", styles.token)
        val SlothEatenOne = Image("sloth-eaten-one", styles.token)
        val SlothEatenTwo = Image("sloth-eaten-two", styles.token)
        def Population(t : Tribe) = Image("population-" + t.name, styles.token)
        val PopulationLost = Image("population-lost", styles.token)
        def Malaise(t : Tribe) = Image("malaise-" + t.name, styles.token)
        val StrengthMalaise = Image("malaise-strength", styles.token)

        val Claw = Image("power-claw", styles.token)
        val Flame = Image("power-flame", styles.token)
        val Wing = Image("power-wing", styles.token)
        val PowerAny = Image("power-any", styles.token)

        val DragonDie = Image("dragon-die", styles.token)
        def Pattern(p : Pattern) = Image("pattern-" + p.toString, styles.token)
    }

    object styles extends BaseStyleMapping("vast") {
        import rules._

        Knight --> color("#b28534")
        Goblins --> color("#397c3c")
        Dragon --> color("#d8182a")
        Cave --> color("#8270a1")
        Thief --> color("#49494b")

        North --> transform("")
        East --> transform("rotate(90deg)")
        South --> transform("rotate(180deg)")
        West --> transform("rotate(270deg)")

        object condensed extends CustomStyle(letter.spacing("-0.125ch"))
        object italic extends CustomStyle(font.style("italic"))

        object green extends CustomStyle(color("#8a9255"))
        object brown extends CustomStyle(color("#a6783e"))

        object crystal extends CustomStyle(color("#ff438c"))

        object directions extends CustomStyle(display("flex"), flex.direction("column"), justify.content("space-between"), pointer.events("none"))
        object dirsub extends CustomStyle(display("flex"), flex.direction("row"), justify.content("space-between"))

        object title extends CustomStyle(color("#ffffff"), font.size("100%"), filter("drop-shadow(0px 0px 6px #000000) drop-shadow(0px 0px 6px #000000) drop-shadow(0px 0px 6px #000000)"), text.align("center"), margin.left("0.6ch"), margin.right("0.6ch"))

        object money extends CustomStyle(color("gold"))
        object hit extends CustomStyle(color("#dc143c"))
        object area extends CustomStyle(color("#88aa88"))

        object roll extends CustomStyle(border.width("2px"), border.style("solid"), border.radius("2ex"), font.size("108%"), font.weight("bold"), color("#dc143c"), background.color("#000000"), width("3.4ex"), height("3.4ex"), text.indent("0"), display("inline-flex"), justify.content("center"), align.items("center"))
        object rollOut extends CustomStyle(font.size("108%"), font.weight("bold"), color("#dc143c"), background.color("#dc143c"), width("4.4ex"), height("4.4ex"), text.indent("0"), display("inline-flex"), justify.content("center"), align.items("center"), margin.top("-1ex"), margin.bottom("-1ex"), margin.left("-0.5ex"), margin.right("-0.5ex"))
        object rollIn extends CustomStyle(background.color("#000000"), width("3.6ex"), height("3.6ex"), display("inline-flex"), justify.content("center"), align.items("center"))
        object expandRoll extends CustomStyle(margin.top("0"), margin.bottom("0"))
        object hexagon extends CustomStyle(clip.path("polygon(50% 5%, 90% 27%, 90% 70%, 50% 90%, 10% 70%, 10% 27%)"))

        object fund extends CustomStyle(height("2.8ex"), vertical.align("middle"))
        object fund5 extends CustomStyle(height("2.8ex"), vertical.align("middle"))
        object token extends CustomStyle(width("2ex"), vertical.align("middle"), padding("0.11ex"))

        object pile extends CustomStyle(width("4.5ex"), vertical.align("middle"), padding.left("0.5ex"), padding.right("0.5ex"))

        object narrow extends CustomStyle(letter.spacing("-0.5ch"), vertical.align("middle"))

        object tile extends CustomStyle(display("block"), width("16ex"), height("16ex"))
        object abs extends CustomStyle(position("absolute"))

        object power extends CustomStyle(display("block"), width("8ex"), height("8ex"))

        object sloth extends CustomStyle(color("#be7c4f"))

        object card extends CustomStyle(display("block"), width("21.00ex"), height("29.54ex"))

        object cardX extends CustomStyle(padding("0.5ex"), margin("0.8ex"), border.color("transparent"), border.style("solid"), border.width("0.3ex"), border.radius("1.8ex"))
        object cardS extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), border.color("#ffffff"))
        object cardP extends CustomStyle(border.radius("5ex"))
        object cardT extends CustomStyle(border.radius("0ex"))

        object group extends CustomStyle(margin("0.3ex"))
        object inline extends CustomStyle(display("inline-block"))
        object nomargin extends CustomStyle(margin("0"))
        object nopadding extends CustomStyle(padding("0"))
        object halfmargin extends CustomStyle(margin.left("0.2ex"), margin.right("0.2ex"), margin.top("0.2ex"), margin.bottom("0.2ex"))
        object selected extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), outline.color("#ffffff"), outline.style("solid"), outline.width("0.3vmin"))

        object smallname extends CustomStyle(font.size("90%"), font.weight("bold"))

        object center extends CustomStyle(text.align("center"))
        object effect extends CustomStyle(font.size("80%"), font.variant("small-caps"), line.height("90%"), margin("0.3ch"))

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

        object short extends CustomStyle(width("26%"), min.height("4.4ex"), margin("0.8ex"))
        object yyy extends CustomStyle(position("relative"), top("3.4ex"))

        object winnerKnight extends CustomStyle(width("22ex"), margin.bottom("-5ex"), margin.top("-4ex"))
        object winnerDragon extends CustomStyle(width("22ex"), margin.bottom("-4ex"), margin.top("-1ex"))
        object winnerGoblins extends CustomStyle(width("22ex"), margin.bottom("-3ex"), margin.top("-2ex"), margin.left("-5ex"), margin.right("-5ex"))
        object winnerCave extends CustomStyle(width("22ex"), margin.bottom("-1ex"), margin.top("-0ex"))

        object illustration extends CustomStyle(max.height("12ex"), max.width("12ex"))
        object factionboard extends CustomStyle(height("54ex"))
    }

    implicit class ElemElem(val elem : Elem) extends AnyVal {
        def larger = elem.styled(xstyles.larger125)
    }

    implicit class ElemString(val s : String) extends AnyVal {
        def roll = s.spn(styles.roll)
    }

    implicit class ElemInt(val n : Int) extends AnyVal {
        def money = (n >= 0).?(("$" + n).styled(styles.money)).|(("-$" + n.abs).styled(styles.hit))
        def cards = (n != 1).?(n.hl ~ " cards").|("a card")
        def minions = (n != 1).?(n.hl ~ " minions").|("a minion")
        def roll = n.toString.spn(styles.rollIn)(styles.hexagon).spn(styles.rollOut)(styles.hexagon)
    }

    object borders extends BaseStyleMapping("vast-border") {
        import rules._

        Knight --> outline.color("#b28534")
        Goblins --> outline.color("#397c3c")
        Dragon --> outline.color("#d8182a")
        Cave --> outline.color("#8270a1")
        Thief --> outline.color("#49494b")
    }
}
