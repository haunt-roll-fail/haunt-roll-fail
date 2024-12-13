package root
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
    val Gap = " ".pre.div(styles.gap)

    object dt {
        val Arrow = Span(Text(0x2192.toChar.toString))
        val ArrowWide = Span(" " ~ 0x2192.toChar.toString ~ " ")
        val Move = Image("move", styles.token, "")
        val Battle = Image("battle", styles.token, "")
        val Swap = Image("swap", styles.token)
        val Remove = Image("remove", styles.token)
        val Income = Image("income", styles.token)
        val Current = Image("current", styles.token)
        val AnyCard = Image("card-suit-" + AnySuit + "-gray", styles.cardback, "any suit")
        val CardBack = Image("card-back", styles.cardback, "card")
        val CardBackFrog = Image("card-back-frog", styles.cardback, "card")
        def CardSuit(s : SuitCost) : Image = s @@ {
            case s : Suit => Image("card-suit-" + s + "-gray", styles.dt.token, s.toString)
            case AnySuit => Image("card-suit-" + AnySuit + "-gray", styles.dt.token, "any suit")
            case AnyOf(l) if l == FoxRabbitMouse => Image("card-suit-" + AnySuit + "-gray", styles.dt.token, "any suit")
            case _ => Image("card-suit-" + AnySuit + "-gray", styles.dt.token, "unknown suit")
        }
        val CardEmpty = Image("card-empty", styles.cardback)
        val CardBackInfo = Image("card-back", styles.cardbackinfo, "card")
        def CardSuitInfo(s : SuitCost) = Image("card-suit-" + s + "-gray", styles.cardbackinfo)
        val CardEmptyInfo = Image("card-empty", styles.cardbackinfo, "no card")

        val ItemAny = Image("item-any", styles.piece, "any item")
        val ItemAnyDamaged = Image("item-any-damaged", styles.piece, "damage any item")
        def CraftSuit(s : AbstractSuit) : Image = s @@ {
            case s : BaseSuit => Image("craft-suit-" + s, styles.dt.token, s.toString)
            case s @ AnySuit => Image("craft-suit-" + s, styles.dt.token, "any suit")
            case OneOf(List(s)) => Image("craft-suit-" + s, styles.dt.token, s.toString)
            case OneOf(l) => Image("craft-suit-" + l.mkString("-"), styles.dt.token, l.mkString("-"))
        }

        val VP = Span("VP", xstyles.smaller75, styles.vp)
    }

    object helper {
        def &(e : Elem) = Span(e, styles.block)
        def &(e : $[Elem]) = Span(e.merge, styles.block)
        def &&(e : Elem) = Span(e, styles.nowrap)
        def &&(e : $[Elem]) = Span(e.merge, styles.nowrap)
    }

    object styles extends BaseStyleMapping("root") {
        val base = xstyles

        import rules._

        object dt extends BaseStyleMapping("root-dt") {
            object building extends CustomStyle(width("3.0ex"), vertical.align("middle"), padding.left("0.10ex"), padding.right("0.10ex"))
            object token    extends CustomStyle(width("2.7ex"), vertical.align("middle"), padding.left("0.10ex"), padding.right("0.10ex"))
        }

        MC --> color("#d26228")
        BK --> color("#2d9dd7")

        ED --> color("#3b65a3")
        PE --> color("#a33b42")

        WA --> color("#62ac4d")
        FU --> color("#e1c012")
        AF --> color("#e1c012")

        VB --> color("#ccccab")
        NB --> (color("#000000") :: text.shadow("-1px -1px 0px #ffffff, 1px -1px 0px #ffffff, -1px 1px 0px #ffffff, 1px 1px 0px #ffffff"))
        MB --> (color("#0000ff") :: text.shadow("-1px -1px 0px #ff0000, 1px -1px 0px #ff0000, -1px 1px 0px #ff0000, 1px 1px 0px #ff0000"))

        RF --> color("#5ebbab")
        WC --> color("#53e5cd")

        SF --> color("#e5ad02")

        LC --> color("#b4e553")
        CM --> color("#53e560")

        CC --> color("#6615ad")
        RI --> color("#db0092")

        UD --> color("#ecba98")
        DR --> color("#e6ec98")

        LH --> color("#cf002c")
        LK --> (color("#000000") :: text.shadow("-1px -1px 0px #cf002c, 1px -1px 0px #cf002c, -1px 1px 0px #cf002c, 1px 1px 0px #cf002c"))

        KI --> color("#a1b0b0")

        OK --> color("#949599")

        XC --> color("#4c3896")
        Mudmen(XC) --> color("#659857")

        CU --> color("#c7afd9")
        CUv2 --> color("#c7afd9")

        FH --> color("#989745")

        TD --> color("#3c7174")
        LDvB --> color("#3c7174")
        LDvC --> color("#3c7174")
        LDvD --> color("#3c7174")

        TCvA --> color("#954a2e")

        KDvA --> color("#d1d2d4")

        StreetBand --> color("#0c93de")
        StoicProtector --> color("#d2a02a")
        BanditGangs --> color("#f17497")
        FelinePhysicians --> color("#d26228")
        BluebirdNobles --> color("#3b65a3")
        RabbitScouts --> color("#62ac4d")
        Brigand --> color("#ccccab")
        LizardEnvoys --> color("#b4e553")
        OtterDivers --> color("#5ebbab")
        MoleArtisians --> color("#ecba98")
        RavenSentries --> color("#6615ad")
        RatSmugglers --> color("#cf002c")
        BadgerBodyguards --> color("#a1b0b0")

        PopularBand --> color("#0c93de")
        FuriousProtector --> color("#d2a02a")
        HighwayBandits --> color("#f17497")
        ForestPatrol --> color("#d26228")
        LastDynasty --> color("#3b65a3")
        SpringUprising --> color("#62ac4d")
        TheExile --> color("#ccccab")
        WarmSunProphets --> color("#b4e553")
        RiverfolkFlotilla --> color("#5ac4be")
        CorvidSpies --> color("#6615ad")
        SunwardExpedition --> color("#ecba98")
        FlameBearers --> color("#cf002c")
        VaultKeepers --> color("#a1b0b0")

        Fox --> color("#e53e36")
        Rabbit --> color("#f4e74f")
        Mouse --> color("#f68b56")
        Bird --> color("#6ac0ba")
        Frog --> color("#3c7174")

        object FoxFrog extends CustomStyle(background("linear-gradient(to bottom, #e53e36 0%, #e53e36 50%, #3c7174 70%, #3c7174 100%)"), background.clip("text"), color("transparent"))
        object RabbitFrog extends CustomStyle(background("linear-gradient(to bottom, #f4e74f 0%, #f4e74f 50%, #3c7174 70%, #3c7174 100%)"), background.clip("text"), color("transparent"))
        object MouseFrog extends CustomStyle(background("linear-gradient(to bottom, #f68b56 0%, #f68b56 50%, #3c7174 70%, #3c7174 100%)"), background.clip("text"), color("transparent"))

        object FoxRabbitMouse extends CustomStyle(background("linear-gradient(to bottom, #e53e36 0%, #e53e36 46%, #f4e74f 51%, #f4e74f 54%, #f68b56 74%, #f68b56 100%)"), background.clip("text"), color("transparent"))
        object FoxRabbitMouseFrog extends CustomStyle(background("linear-gradient(to bottom, #e53e36 0%, #e53e36 30%, #f4e74f 40%, #f4e74f 40%, #f68b56 60%, #f68b56 60%, #3c7174 80%, #3c7174 100%)"), background.clip("text"), color("transparent"))

        object gold extends CustomStyle(color("gold"))

        object quest extends CustomStyle(display("inline-block"), width("18ch"), position("relative"), zIndex("1"))
        object chartitle extends CustomStyle(display("inline-block"), width("18.8ch"), margin("-0.4ch"))

        object adventurer extends CustomStyle(bottom("1.2ch"))
        object ranger extends CustomStyle(bottom("1.1ch"))
        object folk extends CustomStyle(bottom("3.6ch"))

        object card extends CustomStyle(display("block"), width("18ch"))
        object charblock extends CustomStyle(display("block"), width("18ch"), height("18ch"), position("relative"))
        object charback extends CustomStyle(background.color("#2b2b2b"))

        object illustration extends CustomStyle(height("16ch"))
        object factionboard extends CustomStyle(height("64ex"))

        object quasi extends CustomStyle(background.color("transparent"), padding("0.2ch"), margin("0.1ch"), outline.style("none"), border.style("solid"), border.width("0.2ch"), border.color("transparent"))
        object selquasi extends CustomStyle(border.color("#ffffff"), border.radius("1.6ch"))
        object selfigure extends CustomStyle(border.color("#dc143c"), border.radius("1.6ch"))
        object unquasi extends CustomStyle(filter("brightness(0.9)"))
        object discard extends CustomStyle(filter("brightness(0.8) greyscale(1)"))

        object columns3 extends CustomStyle(width("24ch"), height("1ch"))
        object columns4 extends CustomStyle(width("32ch"), height("1ch"))
        object columns5 extends CustomStyle(width("40ch"), height("1ch"))
        object columns6 extends CustomStyle(width("48ch"), height("1ch"))

        val columns = Map(3 -> columns3, 4 -> columns4, 5 -> columns5, 6 -> columns6)

        object decree extends CustomStyle(cursor("todo"))

        object doubleFigures extends CustomStyle(display("inline-block"), width("2ch"), text.align("right"))

        object inline extends CustomStyle(display("inline-block"))
        object margined extends CustomStyle(margin("0.3ch"))

        object block extends CustomStyle(display("inline-block"))
        object centered extends CustomStyle(display("flex"), justify.content("center"))

        object fund extends CustomStyle(width("1.5ch"), height("2.0ch"), objectFit("contain"), vertical.align("middle"), padding.left("0.25ch"), padding.right("0.25ch"))
        object piece extends CustomStyle(width("3.0ch"), height("3.0ch"), objectFit("contain"), vertical.align("middle"), padding.left("0.25ch"), padding.right("0.25ch"), padding.top("0.25ch"), padding.bottom("0.25ch"))
        object widepiece extends CustomStyle(width("4.0ch"), height("3.0ch"), objectFit("contain"), vertical.align("middle"), padding.left("0.25ch"), padding.right("0.25ch"), padding.top("0.25ch"), padding.bottom("0.25ch"))
        object warrior extends CustomStyle(width("3.0ch"), height("4.0ch"), objectFit("contain"), vertical.align("middle"), padding.left("0.25ch"), padding.right("0.25ch"))

        object aidamp extends CustomStyle(padding.left("0.25ch"), padding.right("0.10ch"))

        object minister extends CustomStyle(letter.spacing("-0.05ch"), font.weight("bold"), font.size("75%"), line.height("2ch"))

        object crown extends CustomStyle(width("3.2ex"), vertical.align("bottom"))

        object pile extends CustomStyle(width("4.5ch"), vertical.align("middle"), padding.left("0.5ch"), padding.right("0.5ch"))

        object outcast extends CustomStyle(width("3.3ch"), vertical.align("middle"), padding.left("1.5ch"), padding.right("1.5ch"))

        object dominance extends CustomStyle(width("4.3ch"), vertical.align("bottom"), padding.left("0.15ch"), padding.right("0.15ch"))
        object attitude extends CustomStyle(width("1.2ch"), vertical.align("bottom"), padding.left("0.05ch"), padding.right("0.05ch"))

        object action extends CustomStyle(width("4.3ch"), vertical.align("middle"), margin("0.1ch"))

        object wr extends CustomStyle(height("1.8ex"), vertical.align("middle"), padding.left("0.15ch"), padding.right("0.15ch"))
        object wr3x extends CustomStyle(height("5.4ch"), vertical.align("middle"), padding.left("0.15ch"), padding.right("0.15ch"))

        object narrow extends CustomStyle(letter.spacing("-0.5ch"), vertical.align("middle"))

        object ii extends CustomStyle(width("3ch"), vertical.align("middle"))
        object iih extends CustomStyle(height("3ch"), vertical.align("middle"))
        object iii extends CustomStyle(width("7ch"), height("7ch"), objectFit("contain"), vertical.align("bottom"))
        object satchel extends CustomStyle(line.height("20%"))

        object service extends CustomStyle(width("3.0ch"), vertical.align("middle"), margin.hor("0.2ch"), outline.width("0.1ch"), outline.color("white"), outline.style("solid"))
        object price extends CustomStyle(width("2.6ch"), vertical.align("middle"), margin.hor("0.4ch"))


        object warline extends CustomStyle(line.height("2ch"))

        object skipline extends CustomStyle(padding("0.25ch"))

        object building extends CustomStyle(width("2ch"), vertical.align("bottom"))

        object building3x extends CustomStyle(width("6ch"), vertical.align("middle"))

        object token extends CustomStyle(width("2ex"), vertical.align("middle"), padding("0.11ex"))
        object tokengl extends CustomStyle(width("2ex"), vertical.align("middle"), padding.ver("0.11ex"))

        object token3x extends CustomStyle(width("6ex"), vertical.align("middle"), padding("0.33ex"))

        object tokenHeight extends CustomStyle(height("2ex"), vertical.align("middle"), padding("0.11ex"))

        object cardback extends CustomStyle(width("2ch"), vertical.align("middle"), padding("0.1ch"))

        object cardbackinfo extends CustomStyle(width("1.5ch"), vertical.align("middle"), padding("0.1ch"))
        object card5info extends CustomStyle(width("3.0ch"), vertical.align("middle"), padding.left("0.1ch"), padding.right("0.2ch"))

        object section extends CustomStyle(font.weight("bold"), font.size("80%"), text.decoration.line("underline"))

        object nowrap extends CustomStyle(white.space("nowrap"))

        object wrap extends CustomStyle(white.space("normal"))

        object smbr extends CustomStyle(max.height("0.5ch"))

        object gap extends CustomStyle(height("0.2ch"), width("1ch"))

        object name extends CustomStyle(font.size("120%"), font.weight("bold"))
        object smallname extends CustomStyle(font.size("90%"), font.weight("bold"))
        object title extends CustomStyle(color("#ffffff"), font.size("100%"), font.weight("bold"), filter("drop-shadow(0px 0px 6px #000000) drop-shadow(0px 0px 6px #000000) drop-shadow(0px 0px 6px #000000)"), text.align("left"))

        object condensed extends CustomStyle(letter.spacing("-0.125ch"))
        object italic extends ExternalStyle

        object seeThroughInner extends CustomStyle(background.color("#222222ee"))

        object selected extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), outline.color("#ffffff"), outline.style("solid"), outline.width("0.3vmin"))
        object selchar extends CustomStyle(background.color("#707070"), zIndex("1"))

        object darken extends CustomStyle(color("#404040"))

        object on extends CustomStyle(background.color("white"))
        object off extends CustomStyle(background.color("black"))

        object outlineX extends CustomStyle(border.width("1px"), border.style("solid"), background.color("#222222"))

        object phase extends CustomStyle(color("#88aa88"))

        object forest extends CustomStyle(color("#277278"))

        object itemText extends CustomStyle(
            font.variant("small-caps"),
            color("#ccccab"),
            background.color("black"),
            border.width("2px"),
            border.width("0.15vh"),
            border.style("solid"),
            margin("-4px"),
            margin("-0.3vh"),
            padding.left("4px"),
            padding.left("0.3vh"),
            padding.right("4px"),
            padding.right("0.3vh")
        )

        object itemInfo extends CustomStyle(
            font.variant("small-caps"),
            color("#ccccab"),
        )

        object supporters5 extends CustomStyle(outline.width("0.2ch"), outline.style("solid"), outline.color("#315627"), display("inline-block"), line.height("0"))
        object supportersX extends CustomStyle(outline.width("0.2ch"), outline.style("dashed"), outline.color("#315627"), display("inline-block"), white.space("nowrap"), line.height("0"))
        object burrow extends CustomStyle(padding.bottom("0.3ch"), padding.left("0.3ch"), padding.right("0.3ch"), border.width("0.2ch"), border.radius("1ch"), border.style("dotted"))

        object fundbox extends CustomStyle(outline.width("0.2ch"), outline.style("dashed"), outline.color("#696969"), display("inline-block"), white.space("nowrap"), line.height("4ch"), padding("0.5ch"), margin("0.6ch"), background.color("#111111"))
        object moneybox extends CustomStyle(border.bottom.width("0.1ch"), border.bottom.style("dashed"), border.bottom.color("#aaaaaa"), display("flex"), white.space("nowrap"))

        object effect extends CustomStyle(font.size("80%"), font.variant("small-caps"), line.height("1.2ch"), transform("scale(1, 0.8)"))

        object damaged extends CustomStyle(color("#dc143c"))
        object exhausted extends CustomStyle(color("#777777"))
        object damexh extends CustomStyle(color("#552222"))

        object vp extends CustomStyle(color("cyan"))

        object extrachp extends CustomStyle(padding.left("4ch"), padding.right("4ch"))

        object roll extends CustomStyle(border.width("1px"), border.style("solid"), border.radius("2ch"), font.size("120%"), color("white"), background.color("black"), display("inline-block"), width("2.3ch"), text.indent("0"), text.align("center"))

        object rollOut extends CustomStyle(font.size("108%"), font.weight("bold"), color("#ffffff"), background.color("#ffffff"), width("4.0ex"), height("4.0ex"), text.indent("0"), display("inline-flex"), justify.content("center"), align.items("center"), margin.top("-1ex"), margin.bottom("-1ex"), margin.left("-0.5ex"), margin.right("-0.5ex"))
        object rollIn extends CustomStyle(background.color("#000000"), width("3.6ex"), height("3.6ex"), display("inline-flex"), justify.content("center"), align.items("center"))
        object expandRoll extends CustomStyle(margin.top("0"), margin.bottom("0"))
        object hexagon extends CustomStyle(clip.path("polygon(50% 10%, 88% 37%, 74% 80%, 26% 80%, 12% 37%)"))


        object hit extends CustomStyle(color("#dc143c"))

        object log extends CustomStyle(font.size("108%"))

        Indifferent --> Nil
        Amiable --> color("#8f8f84")
        Friendly --> color("#aeae98")
        Allied --> color("#ccccab")
        Hostile --> color("#dc143c")

        object status extends CustomStyle(
            border.width("4px"),
            border.width("0.4vmin"),
            text.align("center"),
            overflow.x("hidden"),
            overflow.y("overlay"),
            text.overflow("ellipsis")
        )

        object fstatus extends CustomStyle(font.size("117%"))
        object gstatus extends CustomStyle(font.size("89%"), width("100%"), height("100%"), align.content("stretch"), justify.content("space-around"))

        object verdeck extends CustomStyle(height("6.5ch"), width("9ch"), justify.content("left"))
        object centerquest extends CustomStyle(text.align("center"))
    }

    object borders extends BaseStyleMapping("root-border") {
        import rules._

        MC --> outline.color("#693114")
        BK --> outline.color("#164e6b")
        ED --> outline.color("#1e3352")
        PE --> outline.color("#511d21")
        WA --> outline.color("#315627")
        FU --> outline.color("#706006")
        AF --> outline.color("#706006")
        VB --> outline.color("#666656")
        NB --> outline.color("#777777")
        MB --> outline.color("#770077")
        RF --> outline.color("#2f5e56")
        WC --> outline.color("#29722b")
        SF --> outline.color("#735601")
        LC --> outline.color("#5a7329")
        CM --> outline.color("#297266")
        CC --> outline.color("#6614ae")
        RI --> outline.color("#6e0049")
        UD --> outline.color("#755c4b")
        DR --> outline.color("#73764c")
        LH --> outline.color("#690017")
        LK --> outline.color("#690017")
        OK --> outline.color("#4a4b4d")
        KI --> outline.color("#505858")
        XC --> outline.color("#261c4b")
        CU --> outline.color("#63576c")
        CUv2 --> outline.color("#63576c")
        FH --> outline.color("#4c4b22")
        TD --> outline.color("#1e383a")
        LDvB --> outline.color("#1e383a")
        LDvC --> outline.color("#1e383a")
        LDvD --> outline.color("#1e383a")
        TCvA --> outline.color("#4a2517")
        KDvA --> outline.color("#68696a")
    }

    object charstyles extends BaseStyleMapping("root-border") {
        import rules._

        MC --> $(top("-1ch"))
        BK --> $(top("-1ch"))
        ED --> $(width("16ch"), left("2ch"), top("-1.2ch"), clip.path("polygon(0% 0%, 89% 0%, 100% 58%, 96% 100%, 0% 100%)"))
        PE --> $(width("16ch"), left("2ch"), top("-1.2ch"), clip.path("polygon(0% 0%, 89% 0%, 100% 58%, 96% 100%, 0% 100%)"))
        WA --> $(height("19ch"), width("auto"), left("-0.4ch"), top("-0.5ch"))
        FU --> $(height("19ch"), width("auto"), left("-0.4ch"), top("-0.5ch"))
        AF --> $(height("19ch"), width("auto"), left("-0.4ch"), top("-0.5ch"))
        VB --> Nil
        NB --> Nil
        MB --> Nil
        RF --> $(width("16ch"), top("-2ch"))
        WC --> $(width("16ch"), top("-2ch"))
        SF --> Nil
        LC --> Nil
        CM --> Nil
        CC --> $(height("16ch"), width("auto"), left("-0.6ch"), top("2.2ch"), clip.path("polygon(1% 0%, 92% 0%, 92% 58%, 100% 60%, 100% 67%, 93% 73%, 83% 100%, 1% 100%)"))
        RI --> $(height("16ch"), width("auto"), left("-0.6ch"), top("2.2ch"), clip.path("polygon(1% 0%, 92% 0%, 92% 58%, 100% 60%, 100% 67%, 93% 73%, 83% 100%, 1% 100%)"))
        UD --> $(height("17.6ch"), width("auto"), left("0.2ch"), top("0.8ch"), clip.path("polygon(0% 0%, 67% 0%, 100% 56%, 100% 60%, 75% 100%, 0% 100%)"))
        DR --> $(height("17.6ch"), width("auto"), left("0.2ch"), top("0.8ch"), clip.path("polygon(0% 0%, 67% 0%, 100% 56%, 100% 60%, 75% 100%, 0% 100%)"))
        LH --> $(height("18ch"), width("auto"), left("-2.2ch"), clip.path("polygon(1% 54%, 10% 63%, 10% 0%, 100% 0%, 100% 100%, 24% 100%, 20% 96%, 1% 76%)"))
        LK --> $(height("18ch"), width("auto"), left("-2.2ch"), clip.path("polygon(1% 54%, 10% 63%, 10% 0%, 100% 0%, 100% 100%, 24% 100%, 20% 96%, 1% 76%)"))
        OK --> $(height("18ch"), width("auto"), left("-3ch"), clip.path("polygon(0% 0%, 1% 0%, 5% 4%, 92% 0%, 92% 60%, 101% 67%, 99% 72%, 90% 69%, 73% 100%, 24% 100%, 30% 50%, 0% 16%)"))
        KI --> $(height("18ch"), width("auto"), left("-3ch"), clip.path("polygon(0% 0%, 1% 0%, 5% 4%, 92% 0%, 92% 60%, 101% 67%, 99% 72%, 90% 69%, 73% 100%, 24% 100%, 30% 50%, 0% 16%)"))
        XC --> $(width("17ch"), top("-1ch"))
        CU --> $(width("16ch"), top("-1.5ch"))
        CUv2 --> $(width("16ch"), top("-1.5ch"))
        FH --> $(width("15ch"), top("-0.1ch"))
        TD --> $(width("15ch"), top("-0.8ch"))
        LDvB --> $(width("15ch"), top("-0.8ch"))
        LDvC --> $(width("15ch"), top("-0.8ch"))
        LDvD --> $(width("15ch"), top("-0.8ch"))
        TCvA --> $(width("15ch"), top("-1.5ch"))
        KDvA --> $(width("22ch"), top("-0.8ch"), clip.path("polygon(0% 0%, 86% 0%, 100% 23%, 100% 40%, 71% 100%, 0% 100%)"))
    }

    implicit class ElemElem(val elem : Elem) extends AnyVal {
        def larger = elem.styled(xstyles.larger125)
    }

    implicit class ElemString(val str : String) extends AnyVal {
        def f(implicit faction : Faction) = str.styled(styles.get(faction))

        def suited(l : $[Suit]) : Elem = l.distinct @@ {
            case $(Fox, Frog) => str.styled(styles.FoxFrog)
            case $(Rabbit, Frog) => str.styled(styles.RabbitFrog)
            case $(Mouse, Frog) => str.styled(styles.MouseFrog)
            case $(Fox, Rabbit, Mouse) => str.styled(styles.FoxRabbitMouse)
            case $(Fox, Rabbit, Mouse, Frog) => str.styled(styles.FoxRabbitMouseFrog)
            case _ => (str + "?" + l.mkString("|")).hl
        }
    }

    implicit class ElemInt(val n : Int) extends AnyVal {
        def xvp = (n + " VP").styled(styles.vp)
        def vp = Span(n.elem ~ Span(Span(" ".pre, xstyles.smaller75) ~ "VP", xstyles.smaller75), styles.vp)

        def hits = ehits(" ".txt)
        def ehits(e : Elem) = n.styled(styles.hit)(xstyles.bold) ~ e ~ ("Hit" + (n != 1).??("s")).styled(styles.hit)
        def hit = (n != 1).?(n + " Hits").|("Hit").styled(styles.hit)

        def roll = n.toString.spn(styles.rollIn)(styles.hexagon).spn(styles.rollOut)(styles.hexagon)

        def cards    = (n != 1).?(n.hlb ~ " cards"   ).|("a card")
        def warriors = (n != 1).?(n.hlb ~ " warriors").|("a warrior")
        def stimes   = (n != 1).?(n.hlb ~ " times"   ).|("once")
        def scards(s : SuitCost) = (n != 1).?(n.hlb).|("a".spn) ~ " " ~ s.elem ~ " " ~ (n != 1).?("cards").|("card")
        def of(s : String) = (n != 1).?(n.hl ~ " " ~ s ~ "s").|("a " ~ s)
        def of(s : Elem) = (n != 1).?(n.hl ~ " " ~ s ~ "s").|("a " ~ s)
        def ofb(s : String) = (n != 1).?(n.hlb ~ " " ~ s ~ "s").|("a " ~ s)
    }

}
