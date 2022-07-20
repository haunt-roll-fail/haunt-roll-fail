package hrf.elem

import colmat._

    object xlo {
        private implicit val prefix = StylePrefix("xlo")
        
        import rules._

        object nowrap extends CustomStyle(white.space("nowrap"))
        object prewrap extends CustomStyle(white.space("pre-wrap"))
        object pre extends CustomStyle(white.space("pre"))

        object break extends CustomStyle(flex.basis("100%"), width("0px"), height("0px"), overflow.x("hidden"), overflow.y("hidden"), display("inline-block"))

        object flexhcenter extends CustomStyle(display("flex"), align.items("center"), justify.content("center"),                           flex.wrap("wrap"))
        object flexvcenter extends CustomStyle(display("flex"), align.items("center"), justify.content("center"), flex.direction("column"), flex.wrap("wrap"))

        object flexhtop    extends CustomStyle(display("flex"), align.content("flex-start"), justify.content("center"), flex.wrap("wrap"))

        object column extends CustomStyle(display("flex"), flex.direction("column"), align.items("center"))

        object inlineBlock extends CustomStyle(display("inline-block"))
        
        object fullwidth extends CustomStyle(width("100%"))
        object fullheight extends CustomStyle(height("100%"))
        
        object pointer extends CustomStyle(cursor("pointer"))
        
        object first extends CustomStyle(order("-999"))

        object hidden extends CustomStyle(visibility("hidden"))
         
        object overlay extends CustomStyle(position("absolute"), left("0%"), top("0%"), width("100%"), height("100%"))
        
        object middleScrollOut extends CustomStyle(display("flex"), align.items("center"), flex.wrap("wrap"), height("100%"), width("100%"))
        object middleScrollIn extends CustomStyle(overflow.y("auto"), height("auto"), margin("auto"))
    }

    object xch {
        private implicit val prefix = StylePrefix("dch")
        
        import rules._

        object mtop extends CustomStyle(margin.top("2ch"))

        
    }

    object xstyles {
        private implicit val prefix = StylePrefix("hrf")
        
        import rules._

        object nostyle extends CustomStyle()

        object todo extends CustomStyle(cursor("todo"))

        object linelarger extends CustomStyle(min.height("3.5ch"), margin.top("-3ch"), margin.bottom("-3ch"))

        object smaller75 extends CustomStyle(font.size("85%"))
        object smaller85 extends CustomStyle(font.size("85%"))
        object larger150 extends CustomStyle(font.size("150%"))
        object larger125 extends CustomStyle(font.size("125%"))

        object highlight extends CustomStyle(color("#d3d3d3"))
        object halfhigh extends CustomStyle(color("#b0b0b0"))
        object bold extends CustomStyle(font.weight("bold"))
        object bright extends CustomStyle(color("#ffffff"))
        object warning extends CustomStyle(color("#ffa500"))
        object error extends CustomStyle(color("#dc143c"))

        object outlined extends CustomStyle(border.width("2px"), border.width("0.2ch"), border.style("solid"), background.color("#222222"))
        
        object unselectable extends ExternalStyle
        object logblur extends ExternalStyle
        object logblurhor extends ExternalStyle

        object halfcharline extends CustomStyle(height("0.5ch"), width("100%"))

        object link extends CustomStyle(color("#87ceeb"), text.decoration.line("none"))

        object xx extends CustomStyle(display("flex"), align.items("center"), justify.content("center"), white.space("pre-wrap"), flex.wrap("wrap"))

        object thu extends CustomStyle(min.height("3ch"), margin.left("4ch"), margin.right("4ch"), margin.top("0.8ch"), margin.bottom("0.8ch"))

        object thula extends CustomStyle(min.height("2.2ch"), font.size("136%"))
        
        object pointer extends CustomStyle(cursor("pointer"))

        object horbreak extends CustomStyle(width("100%"))

        object choice          extends CustomStyle(outline.color("#999999"), outline.style("solid"), outline.width("3px"), outline.width("0.3vmin"), background.color("#111111"))
        object info            extends CustomStyle(outline.color("#333333"), outline.style("solid"), outline.width("1px"), outline.width("0.1vmin"), background.color("#1a1a1a"))
        object chm             extends CustomStyle(margin.left("0.4ch"), margin.right("0.4ch"), margin.top("0.4ch"), margin.bottom("0.4ch"))
        object chp             extends CustomStyle(padding.left("0.4ch"), padding.right("0.4ch"), padding.top("0.4ch"), padding.bottom("0.4ch"))

        object fs100           extends CustomStyle(font.size("100%"))

        object unavailableCard extends CustomStyle(filter("grayscale(0.4) brightness(0.6)"))
        object unavailableText extends CustomStyle(filter("grayscale(1.0) contrast(0.8) brightness(0.8)"))

        object hidden extends CustomStyle(visibility("hidden"))
         
        object pane extends CustomStyle(position("absolute")) {
            object action extends CustomStyle(text.align("center"), font.size("110%"), overflow.y("auto"), padding("10px"), padding("1vmin"))
            object status extends CustomStyle()
            object log extends CustomStyle(font.size("110%"), overflow.y("auto"))
        }
        
        object updown extends CustomStyle(height("7ch"), display("flex"), flex.direction("column"), justify.content("space-between"), margin.top("-0.4ch"), margin.bottom("-0.4ch"))

        object overlay extends CustomStyle(position("absolute"), left("0%"), top("0%"), width("100%"), height("100%"))
        
        object compressed extends CustomStyle(letter.spacing("-0.5ch"), white.space("nowrap"))

        object artworkX extends CustomStyle(max.height("100%"), max.width("100%"), margin("auto"))
        object artwork extends CustomStyle(height("100%"), width("100%"), objectfit("contain"))
        object middleScrollOut extends CustomStyle(display("flex"), align.items("center"), flex.wrap("wrap"), height("100%"), width("100%"))
        object middleScrollIn extends CustomStyle(overflow.y("auto"), height("auto"), margin("auto"))
        
        object seeThrough extends CustomStyle(background.color("#222222e0"))

        object hanging extends CustomStyle(text.indent("-5.5ch"), padding.left("4.5ch"), padding.right("0.5ch"))
        
        object outer extends CustomStyle(
            border.style("solid"),
            border.color("#111111"),
            border.width("3px"), 
            border.width("0.3ch"), 
            width("100%"),
            height("100%"),
            box.sizing("border-box")
        )
            
        object inner extends CustomStyle(
            border.style("solid"),
            border.color("#222222"),
            border.width("2px"), 
            border.width("0.2ch"), 
            width("100%"),
            height("100%"),
            background.color("#222222"),
            overflow.x("hidden"),
            overflow.y("hidden"),
            box.sizing("border-box"),
            position("relative"),
            outline.offset("-0.2ch")
        )
       
        object score {
            object explain extends CustomStyle(font.size("75%"))
            object good extends CustomStyle(color("#ffffff"))
            object bad extends CustomStyle(color("#9F2344"))
        }
    }
