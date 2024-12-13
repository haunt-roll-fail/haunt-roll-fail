package hrf.elem

import rules._
import rules.min
import rules.max

object xlo {
    private implicit val prefix = StylePrefix("xlo")

    object nowrap extends CustomStyle(white.space("nowrap"))
    object prewrap extends CustomStyle(white.space("pre-wrap"))
    object pre extends CustomStyle(white.space("pre"))

    object break extends CustomStyle(flex.basis("100%"), width("0px"), height("0px"), overflow.x("hidden"), overflow.y("hidden"), display("inline-block"))
    object gap extends CustomStyle(min.height("0.4ex"))

    object flexhcenter extends CustomStyle(display("flex"), align.items("center"), justify.content("center"),                           flex.wrap("wrap"))
    object flexvcenter extends CustomStyle(display("flex"), align.items("center"), justify.content("center"), flex.direction("column"), flex.wrap("wrap"))

    object flexVVcenter extends CustomStyle(display("flex"), align.items("center"), justify.content("space-between"), flex.direction("column"), flex.wrap("wrap"))
    object flexVX extends CustomStyle(display("flex"), align.items("center"), justify.content("space-between"), flex.direction("column"), flex.wrap("nowrap"))

    object flexnowrap extends CustomStyle(flex.wrap("nowrap"))

    object grow extends CustomStyle(height("26vh"))

    object grow4 extends CustomStyle(height("4vh"))
    object grow5 extends CustomStyle(height("5vh"))
    object grow6 extends CustomStyle(height("6vh"))
    object grow7 extends CustomStyle(height("7vh"))
    object grow8 extends CustomStyle(height("8vh"))
    object grow9 extends CustomStyle(height("9vh"))
    object grow10 extends CustomStyle(height("10vh"))
    object grow11 extends CustomStyle(height("11vh"))
    object grow12 extends CustomStyle(height("12vh"))
    object grow13 extends CustomStyle(height("13vh"))
    object grow14 extends CustomStyle(height("14vh"))
    object grow15 extends CustomStyle(height("15vh"))
    object grow16 extends CustomStyle(height("16vh"))
    object grow17 extends CustomStyle(height("17vh"))
    object grow18 extends CustomStyle(height("18vh"))
    object grow19 extends CustomStyle(height("19vh"))
    object grow20 extends CustomStyle(height("20vh"))
    object grow21 extends CustomStyle(height("21vh"))
    object grow22 extends CustomStyle(height("22vh"))

    object flexhtop    extends CustomStyle(display("flex"), align.content("flex-start"), justify.content("center"), flex.wrap("wrap"))

    object column extends CustomStyle(display("flex"), flex.direction("column"), align.items("center"))

    object inlineBlock extends CustomStyle(display("inline-block"))

    object fullwidth extends CustomStyle(width("100%"))
    object fullheight extends CustomStyle(height("100%"))

    object pointer extends CustomStyle(cursor("pointer"))
    object contents extends CustomStyle(display("contents"))

    object first extends CustomStyle(order("-999"))

    object hidden extends CustomStyle(visibility("hidden"))

    object overlay extends CustomStyle(position("absolute"), left("0%"), top("0%"), width("100%"), height("100%"))

    object middleScrollOut extends CustomStyle(display("flex"), align.items("center"), flex.wrap("wrap"), height("100%"), width("100%"))
    object middleScrollIn extends CustomStyle(overflow.y("auto"), height("auto"), margin("auto"))
}

object xch {
    private implicit val prefix = StylePrefix("dch")


    object mtop extends CustomStyle(margin.top("2ex"))
}

object xfonts {
    object luminari extends ExternalStyle
}

object xstyles {
    private implicit val prefix = StylePrefix("hrf")

    object nostyle extends CustomStyle()

    object todo extends CustomStyle(cursor("todo"))

    object notification extends CustomStyle(padding.top("1ex"), border.width("0.2ex"), border.radius("1ex"), border.style("dotted"))

    object bottomPadding extends CustomStyle(padding.bottom("2ex"))

    object width10ch extends CustomStyle(width("10ex"))
    object width18ch extends CustomStyle(width("18ex"))

    object width14ex extends CustomStyle(width("14ex"))

    object width60ex extends CustomStyle(width("60ex"))

    object linelarger extends CustomStyle(min.height("3.5ex"), margin.top("-3ex"), margin.bottom("-3ex"))

    object smaller50 extends CustomStyle(font.size("50%"))
    object smaller75 extends CustomStyle(font.size("75%"))
    object smaller85 extends CustomStyle(font.size("85%"))
    object fontSize100 extends CustomStyle(font.size("100%"))
    object larger110 extends CustomStyle(font.size("110%"))
    object larger125 extends CustomStyle(font.size("125%"))
    object larger150 extends CustomStyle(font.size("150%"))
    object larger200 extends CustomStyle(font.size("200%"))

    object highlight extends CustomStyle(color("#d3d3d3"))
    object halfhigh extends CustomStyle(color("#a0a0a0"))
    object bold extends CustomStyle(font.weight("bold"))
    object bright extends CustomStyle(color("#ffffff"))
    object warning extends CustomStyle(color("#ffa500"))
    object error extends CustomStyle(color("#dc143c"))

    object outlined extends CustomStyle(border.width("2px"), border.width("0.2ex"), border.style("solid"), background.color("#222222"))

    object unselectable extends ExternalStyle
    object thumargin extends ExternalStyle
    object logblur extends ExternalStyle
    object logblurhor extends ExternalStyle

    object halfcharline extends CustomStyle(height("0.5ex"), width("100%"))

    object link extends CustomStyle(color("#87ceeb"), text.decoration.line("none"))

    object hiddenLink extends CustomStyle(color("#a0a0a0"), text.decoration.line("none"))

    object xx extends CustomStyle(display("flex"), align.items("center"), justify.content("center"), white.space("pre-wrap"), flex.wrap("wrap"))

    object thu extends CustomStyle(min.height("3ex"), margin.left("4ex"), margin.right("4ex"), margin.top("0.8ex"), margin.bottom("0.8ex")) {
        object condensed   extends CustomStyle(min.height("3ex"), margin.left("4ex"), margin.right("4ex"), margin.top("0.4ex"), margin.bottom("0.4ex"))
        object expanded    extends CustomStyle(min.height("3ex"), margin.left("4ex"), margin.right("4ex"), margin.top("1.6ex"), margin.bottom("1.6ex"))
    }

    object thuc extends CustomStyle(min.height("3ex"), margin.left("4ex"), margin.right("4ex"), margin.top("0.4ex"), margin.bottom("0.4ex"))

    object divint extends CustomStyle(margin.left("5ex"), margin.right("5ex"))

    object thula extends CustomStyle(min.height("2.2ex"), font.size("136%"))

    object halfbutton extends CustomStyle(width("30%"), margin.left("2ex"), margin.right("2ex"))

    object horbreak extends CustomStyle(width("100%"))

    object choice          extends CustomStyle(outline.color("#999999"), outline.style("solid"), outline.width("3px"), outline.width("0.3vmin"), background.color("#111111"))
    object info            extends CustomStyle(outline.color("#333333"), outline.style("solid"), outline.width("1px"), outline.width("0.1vmin"), background.color("#1a1a1a"))
    object chp             extends CustomStyle(padding.left("0.4ex"), padding.right("0.4ex"), padding.top("0.4ex"), padding.bottom("0.4ex"))
    object chm             extends CustomStyle(margin.left("0.4ex"), margin.right("0.4ex"), margin.top("0.4ex"), margin.bottom("0.4ex"))

    object normal          extends CustomStyle(color("#707070"))

    object fs100           extends CustomStyle(font.size("100%"))

    object unavailableCard extends CustomStyle(filter("grayscale(0.4) brightness(0.6)"))
    object unavailableText extends CustomStyle(filter("grayscale(1.0) contrast(0.8) brightness(0.8)"))

    object hidden extends CustomStyle(visibility("hidden"))
    object displayNone extends CustomStyle(display("none"))

    object pane extends CustomStyle(position("absolute"), zIndex("100")) {
        object action extends CustomStyle(text.align("center"), font.size("110%"), overflow.y("auto"), padding("10px"), padding("1vmin"), overflow("overlay"))
        object log extends CustomStyle(font.size("110%"), overflow.y("auto"))
    }

    object updown extends CustomStyle(height("7ex"), display("flex"), flex.direction("column"), justify.content("space-between"), margin.top("-0.4ex"), margin.bottom("-0.4ex"), position("absolute"), right("5ex"))

    object short extends CustomStyle(width("26%"), margin("0.8ex"))
    object shorter extends CustomStyle(width("5%"), margin("0.8ex"))

    object player extends CustomStyle(min.height("6.2ex"), padding.right("3.8ex"))

    object overlay extends CustomStyle(position("absolute"), left("0%"), top("0%"), width("100%"), height("100%"))

    object compressed extends CustomStyle(letter.spacing("-0.5ex"), white.space("nowrap"))

    object artworkX extends CustomStyle(max.height("100%"), max.width("100%"), margin("auto"))
    object artwork extends CustomStyle(height("100%"), width("100%"), objectFit("contain"))
    object middleScrollOut extends CustomStyle(display("flex"), align.items("center"), flex.wrap("wrap"), height("100%"), width("100%"))
    object middleScrollIn extends CustomStyle(overflow.y("auto"), height("auto"), margin("auto"))
    object middleScrollInFit extends CustomStyle(height("100%"), width("100%"), display("flex"), flex.direction("column"), flex.wrap("nowrap"), align.content("center"), justify.content("space-evenly"), align.items("center"))

    object seeThrough extends CustomStyle(background.color("#222222e0"))

    object hanging extends CustomStyle(text.indent("-5.5ex"), padding.left("4.5ex"), padding.right("0.5ex"))

    object optionOnX extends CustomStyle(background("linear-gradient(to right, #666 0%, #111 4.5ch, #111 calc(100% - 4.5ch), #666 100%)"))
    object optionOnY extends CustomStyle(background("linear-gradient(to right, #111 0%, #555 4.5ch, #555 calc(100% - 4.5ch), #111 100%)"))
    object optionOn extends CustomStyle(background("linear-gradient(to right, #111 0%, #555 15%, #222 35%, #222 65%, #555 85%, #111 100%)"))

    object explain extends CustomStyle(position("absolute"), right("5.1ex"), height("2.6ex"), width("2.6ex"), cursor("help"))
    object preicon extends CustomStyle(position("absolute"), left("5.1ex"), height("2.6ex"), width("2.6ex"))

    object clickThrough extends CustomStyle(pointer.events("none"))

    object optionE extends CustomStyle(padding.left("3ex"), padding.right("3ex"))

    object outer extends CustomStyle(
        width("100%"),
        height("100%"),
        box.sizing("border-box"),
        border.style("solid"),
        border.color("#111111"),
        border.width("3px"),
        border.width("0.3ex"),
        outline.color("#111111"),
        outline.style("solid"),
        outline.width("1px"),
        outline.offset("-3.5px"),
        outline.offset("-0.35ex")
    )

    object inner extends CustomStyle(
        border.style("solid"),
        border.color("#222222"),
        border.width("2px"),
        border.width("0.2ex"),
        width("100%"),
        height("100%"),
        background.color("#222222"),
        overflow.x("hidden"),
        overflow.y("hidden"),
        box.sizing("border-box"),
        position("relative")
    )

    object fillHeight extends CustomStyle(min.height("100%"))
}

object evscore {
    private implicit val prefix = StylePrefix("evscore")

    object action extends CustomStyle(overflow.wrap("anywhere"))
    object explain extends CustomStyle(font.size("75%"))
    object good extends CustomStyle(color("#ffffff"))
    object bad extends CustomStyle(color("#9F2344"))
}
