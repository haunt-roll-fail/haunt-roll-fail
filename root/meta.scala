package root

import root.gaming._

import colmat._

import hrf.meta._
import hrf.elem._
import hrf.reflect._

trait GameOption extends BaseGameOption

trait MapOption extends GameOption with ImportantOption {
    def group = "Map"
}

case object AutumnMap extends MapOption {
    def valueOn = "Autumn".styled(Mouse)(xstyles.bold)
}

case object WinterMap extends MapOption {
    def valueOn = "Winter".styled(Bird)
}

case object LakeMap extends MapOption {
    def valueOn = "Lake".styled(Rabbit)
}

case object MountainMap extends MapOption {
    def valueOn = "Mountain".styled(Fox)
}


trait ClearingsOption extends GameOption {
    def group = "Clearings"
}

case object DefaultClearings extends ClearingsOption {
    def valueOn = "Default".hlb
}

case object AllRandomClearings extends ClearingsOption {
    def valueOn = "All Random".hlb
}

case object NoClustersClearings extends ClearingsOption {
    def valueOn = "No Clusters".hlb
}

case object SuitPairsClearings extends ClearingsOption {
    def valueOn = "Suit Pairs".hlb
}

case object ConnectedClearings extends ClearingsOption {
    def valueOn = "Connected".hlb
}

case object ThreeFourFiveClearings extends ClearingsOption {
    def valueOn = "3".hlb ~ "/" ~ "4".hlb ~ "/" ~ "5".hlb
}


trait DeckOption extends GameOption with ImportantOption {
    def group = "Deck"
}

case object StandardDeck extends DeckOption {
    def valueOn = "Standard".hlb
}

case object ExilesDeck extends DeckOption {
    def valueOn = "Exiles & Partisans".hlb
}

case object ExilesFavorsDeck extends DeckOption {
    def valueOn = "Exiles & Partisans & Favors".hlb
}

case object MixedDeck extends DeckOption {
    def valueOn = "Mixed".hlb
}


trait SeatingOption extends GameOption {
    def group = "Seating Order"
}

case object SeatingGiven extends SeatingOption {
    def valueOn = "Given".hlb
}

case object SeatingRandom extends SeatingOption {
    def valueOn = "Random".hlb
}


trait FactionSeatingOption extends GameOption {
    def group = "Faction Seating Order"
}

case object FactionSeatingGiven extends FactionSeatingOption {
    def valueOn = "Given".hlb
}

case object FactionSeatingRandom extends FactionSeatingOption {
    def valueOn = "Random".hlb
}


trait SetupOrderOption extends GameOption {
    def group = "Setup Order"
}

case object SetupOrderPriority extends SetupOrderOption {
    def valueOn = "Faction Priorities".hlb
}

case object SetupOrderReverse extends SetupOrderOption {
    def valueOn = "Reverse Turn Order".hlb
}


trait SetupTypeOption extends GameOption {
    def group = "Setup Type"
}

case object SetupTypeCorners extends SetupTypeOption {
    def valueOn = "Corners".hlb
}

case object SetupTypeHomelands extends SetupTypeOption {
    def valueOn = "Homelands".hlb
}


trait CardDraftOption extends GameOption {
    def group = "Card Draft"
}

case object CardDraftStandard extends CardDraftOption {
    def valueOn = "Standard".hlb
}

case object CardDraftFive extends CardDraftOption {
    def valueOn = "3".hlb ~ " of " ~ 5.hlb
}


trait AdSetBuffOption extends GameOption {
    def group = "Ad Set Buff"
}

case object AdSetBuffOff extends AdSetBuffOption {
    def valueOn = "Off".hlb
}

case object AdSetBuffOn extends AdSetBuffOption {
    def valueOn = "On".hlb
}


trait InfamyOption extends GameOption {
    def group = "Infamy"
}

case object StandardInfamy extends InfamyOption {
    def valueOn = "Standard".hlb
}

case object WarriorsOnlyInfamy extends InfamyOption {
    def valueOn = "Warriors Only".hlb
}


trait CharacterOption extends GameOption {
    def group = "Vagabond Character"
}

case object SelectCharacter extends CharacterOption {
    def valueOn = "Select".hlb
}

case object RandomCharacter extends CharacterOption {
    def valueOn = "Random".hlb
}


trait DominanceOption extends GameOption {
    def group = "Dominance"
}

case object StandardDominance extends DominanceOption {
    def valueOn = "Standard".hlb
}

case object TotalWarDominance extends DominanceOption {
    def valueOn = "Total War".hlb
}


object Meta extends MetaGame {
    val gaming = root.gaming

    type F = Faction
    type G = Game
    type O = GameOption
    
    def tagF = implicitly

    val name = "root"
    val label = "Root"

    val factions = List(MC, ED, WA, VB, LC, RF, UD, CC, LH, KI, OK, BK, AF, RI, SF, NB, MB)

    override def optionsFor(l : List[F]) =
        $(AutumnMap, WinterMap, LakeMap, MountainMap) ++
        $(DefaultClearings, NoClustersClearings, AllRandomClearings, SuitPairsClearings, ConnectedClearings, ThreeFourFiveClearings) ++
        $(StandardDeck, ExilesDeck, ExilesFavorsDeck, MixedDeck) ++
        (l./~(_.feline).any || l./~(_.fanatic).any || l./~(_.mischief).any).??($(AdSetBuffOff, AdSetBuffOn)) ++
        l./~(_.hero).any.??($(SelectCharacter, RandomCharacter)) ++
        l./~(_.hero).any.??($(StandardInfamy, WarriorsOnlyInfamy)) ++
        (l.num > 2).??($(StandardDominance, TotalWarDominance))
        
    val hiddenOptions =
        $(SeatingGiven, SeatingRandom) ++
        $(FactionSeatingGiven, FactionSeatingRandom) ++
        $(SetupOrderPriority, SetupOrderReverse) ++
        $(SetupTypeCorners, SetupTypeHomelands) ++
        $(CardDraftStandard, CardDraftFive)

    val options = optionsFor(factions) ++ hiddenOptions
  
    override def optimizeOptions(oo : List[O], l : List[F]) : List[O] = {
        val map = (AutumnMap +: oo).of[MapOption].last
        
        val clearings = (NoClustersClearings +: DefaultClearings +: oo).of[ClearingsOption].%!(_ == DefaultClearings && map != AutumnMap).last

        val deck = (MixedDeck +: oo).of[DeckOption].last

        val adsetbuff = (AdSetBuffOn +: oo).of[AdSetBuffOption].last
 
        val seating = (SeatingGiven +: oo).of[SeatingOption].last

        val fseating = (FactionSeatingGiven +: oo).of[FactionSeatingOption].last

        val order = (SetupOrderPriority +: oo).of[SetupOrderOption].last

        val setup = (SetupTypeCorners +: oo).of[SetupTypeOption].last

        val cards = (CardDraftStandard +: oo).of[CardDraftOption].last

        val character = (SelectCharacter +: oo).of[CharacterOption].last

        val infamy = (StandardInfamy +: oo).of[InfamyOption].last

        val dominance = (StandardDominance +: oo).of[DominanceOption].last

        $(map, clearings, deck, adsetbuff, order, seating, fseating, setup, cards, character, infamy, dominance).intersect(optionsFor(l) ++ hiddenOptions)
    }

    val quickMin = 4
    val quickMax = 4
  
    override val quickFactions = factions.take(8)
 
    def randomGameName() = {
        val n = List("Power", "Victory", "Glory", "Destiny", "Might", "Fight", "Right", "Betrayal", "Wood", "Land", "Air", "Ground").shuffle
        val c = List("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def reach(factions : List[Faction]) = 
        factions./~(_.feline).num * 10 +
        factions./~(_.aviary).num * 8 +
        factions./~(_.insurgent).num * 3 +
        factions./~(_.trader).num * 5 +
        factions./~(_.fanatic).num * 2 +
        factions./~(_.underground).num * 8 +
        factions./~(_.mischief).num * 3 +
        factions./~(_.expedition).num * 7 +
        factions./~(_.horde).num * 8 +
        factions./~(_.hero).num.inlist(0 :: 5 :: 7 :: 9)
                
    def validateFactionCombination(factions : List[Faction]) = {
        if (factions.num < 2)
            ErrorResult("Select at least two factions")
        else
        if (factions.num > 8)
            ErrorResult("Max eight factions")
        else
        if (factions.intersect($(MC, ED, LC, UD, BK, LH, OK, KI)).num > 4)
            ErrorResult("Too many corner factions")
        else
        if (reach(factions) < factions.num.inlist(0 :: 0 :: 17 :: 18 :: 21 :: 25 :: 28 :: 28 :: 28))
            WarningResult("Reach " + reach(factions))
        else
            InfoResult("Reach " + reach(factions))
    }

    def validateFactionSeatingOptions(factions : List[Faction], options : List[O]) = {
        val ff = factions./~(_.hero)./(h => (factions.dropWhile(_ != h) ++ factions.takeWhile(_ != h)).drop(1).takeWhile(_.hero.none).num)
        if (ff.maxOr(0) - ff.minOr(0) > 1)
            WarningResult("Vagabonds seated unevenly")
        else
            InfoResult("~~~")
    }

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)
    override def factionNote(f : Faction) : Elem = f.note

    override def glyph(g : G) : Option[String] = Option(g.current)./(_.style + "-glyph")
    override def glyph(f : F) : Option[String] = Some(f.style + "-glyph")
    override def glyph(g : G, f : F) : Option[String] = glyph(f).%!(_ => g.highlightFaction.has(f) && hrf.HRF.uptime() / 1000 % 2 == 1)
    
    def createGame(factions : List[Faction], options : List[O]) = new Game(factions, factions, true, options)
    
    def getBots(f : Faction) = f match {
        case MC => $("Easy")
        case ED => $("Easy")
        case WA => $("Easy")
        case VB => $("Easy")
        case _ => $("None")
    }
    
    override def defaultBots = $("Easy", "None")

    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Faction, "Easy") => new BotXX(f)
        case (f : Faction, "None") => new BotXX(f)
        case (f : Faction, "Normal") => new BotTT(f, 1, 6, 3, o => new BotXX(o))
    }    

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) = factions.%(_.short == s).single

    def writeOption(o : O) = o.toString
    def parseOption(s : String) = options.%(o => o.toString == s).single
    
    def parseAction(s : String) : Action = { println(s); Serialize.parseAction(s) }
    def writeAction(a : Action) : String = Serialize.write(a)
    
    val start = StartAction(gaming.version)
    
    val assets = 
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true)(
        ImageAsset("ruins"                ) ::
        ImageAsset("ferry"                ) ::
        ImageAsset("tower"                ) ::
        ImageAsset("riverboat"            ) ::
        ImageAsset("balloon"              ) ::
        ImageAsset("swimmers"             ) ::
        ImageAsset("tunnel"              ) ::

        ImageAsset("clearing-highlight-placement" ) ::
        ImageAsset("clearing-highlight-battle"    ) ::
        ImageAsset("clearing-highlight-nuke"      ) ::

        ImageAsset("clearing-suit-fox"    ) ::
        ImageAsset("clearing-suit-rabbit" ) ::
        ImageAsset("clearing-suit-mouse"  ) ::

        ImageAsset("empty-building"       ) ::
        ImageAsset("empty-building-card"  ) ::
        ImageAsset("empty-building-1"     ) ::
        ImageAsset("empty-building-2"     ) ::
        ImageAsset("empty-building-3"     ) ::
        ImageAsset("empty-building-4"     ) ::
        ImageAsset("empty-building-5"     ) ::
        ImageAsset("empty-building-1-card") ::
        ImageAsset("empty-building-2-card") ::
        ImageAsset("empty-building-3-card") ::
        ImageAsset("empty-building-4-card") ::
        ImageAsset("empty-building-5-card") ::
        ImageAsset("empty-building-1p"    ) ::
        ImageAsset("empty-building-2p"    ) ::
        ImageAsset("empty-building-cost-0") ::
        ImageAsset("empty-building-cost-1") ::
        ImageAsset("empty-building-cost-2") ::
        ImageAsset("empty-building-cost-3") ::
        ImageAsset("empty-building-cost-4") ::
        ImageAsset("empty-token"          ) ::
        ImageAsset("empty-token-1"        ) ::
        ImageAsset("empty-token-2"        ) ::
        ImageAsset("empty-token-3"        ) ::
        ImageAsset("empty-token-4"        ) ::
        ImageAsset("empty-token-5"        ) ::
        ImageAsset("empty-token-cost-1"   ) ::
        ImageAsset("empty-token-cost-2"   ) ::
        ImageAsset("empty-token-cost-3"   ) ::
        ImageAsset("empty-token-fox"      ) ::
        ImageAsset("empty-token-rabbit"   ) ::
        ImageAsset("empty-token-mouse"    ) ::
        ImageAsset("empty-token-bird"     ) ::
        ImageAsset("empty-token-anysuit"  ) ::

        ImageAsset("craft-suit-fox"      ) ::
        ImageAsset("craft-suit-rabbit"   ) ::
        ImageAsset("craft-suit-mouse"    ) ::
        ImageAsset("craft-suit-anysuit"  ) ::
       
        
        ImageAsset("cross"                ) ::

        ImageAsset("card-back"             ) ::
        ImageAsset("card-empty"            ) ::
        ImageAsset("card-suit-bird"        ) ::
        ImageAsset("card-suit-fox"         ) ::
        ImageAsset("card-suit-rabbit"      ) ::
        ImageAsset("card-suit-mouse"       ) ::
        ImageAsset("card-suit-bird-gray"   ) ::
        ImageAsset("card-suit-fox-gray"    ) ::
        ImageAsset("card-suit-rabbit-gray" ) ::
        ImageAsset("card-suit-mouse-gray"  ) ::
        ImageAsset("card-suit-anysuit-gray"  ) ::
        ImageAsset("card-suit-bird-white"  ) ::
        ImageAsset("card-suit-fox-white"   ) ::
        ImageAsset("card-suit-rabbit-white") ::
        ImageAsset("card-suit-mouse-white" ) ::

        ImageAsset("deck" ) ::
        ImageAsset("pile" ) ::
        ImageAsset("pile-empty" ) ::
        ImageAsset("pile-bird" ) ::
        ImageAsset("pile-fox" ) ::
        ImageAsset("pile-rabbit" ) ::
        ImageAsset("pile-mouse" ) ::

        ImageAsset("action-black" ) ::
        ImageAsset("action-bird" ) ::

        ImageAsset("item-x-placeholder" ) ::
        ImageAsset("item-x-placeholder-1p" ) ::
        ImageAsset("item-x-placeholder-2p" ) ::
        ImageAsset("item-x-placeholder-3p" ) ::
        ImageAsset("item-x-placeholder-4p" ) ::

        ImageAsset("satchel-top"        ) ::
        ImageAsset("satchel-top-2"      ) ::
        ImageAsset("satchel-top-3"      ) ::
        ImageAsset("satchel-top-4"      ) ::
        ImageAsset("satchel-bottom"     ) ::
        ImageAsset("satchel-bottom-2"   ) ::
        ImageAsset("satchel-bottom-3"   ) ::
        ImageAsset("satchel-bottom-4"   ) ::

        ImageAsset("satchel-middle"       ) ::
        ImageAsset("satchel-middle-start" ) ::
        ImageAsset("satchel-middle-cont"  ) ::
        
        ImageAsset("satchel-left-1"        ) ::
        ImageAsset("satchel-right-empty"   ) ::

        ImageAsset("satchel-thin-top-3" ) ::
        ImageAsset("satchel-thin-top-4" ) ::
        ImageAsset("satchel-thin-top-5" ) ::
        ImageAsset("satchel-thin-top-6" ) ::
        
        ImageAsset("satchel-thin-bottom-3" ) ::
        ImageAsset("satchel-thin-bottom-4" ) ::
        ImageAsset("satchel-thin-bottom-5" ) ::
        ImageAsset("satchel-thin-bottom-6" ) ::
        
        ImageAsset("item-boots"         ) ::
        ImageAsset("item-coins"         ) ::
        ImageAsset("item-sword"         ) ::
        ImageAsset("item-torch"         ) ::
        ImageAsset("item-bag"           ) ::
        ImageAsset("item-crossbow"      ) ::
        ImageAsset("item-hammer"        ) ::
        ImageAsset("item-teapot"        ) ::

        ImageAsset("item-boots-exhausted"     ) ::
        ImageAsset("item-coins-exhausted"     ) ::
        ImageAsset("item-sword-exhausted"     ) ::
        ImageAsset("item-torch-exhausted"     ) ::
        ImageAsset("item-bag-exhausted"       ) ::
        ImageAsset("item-crossbow-exhausted"  ) ::
        ImageAsset("item-hammer-exhausted"    ) ::
        ImageAsset("item-teapot-exhausted"    ) ::
                                              
        ImageAsset("item-boots-damaged"       ) ::
        ImageAsset("item-coins-damaged"       ) ::
        ImageAsset("item-sword-damaged"       ) ::
        ImageAsset("item-torch-damaged"       ) ::
        ImageAsset("item-bag-damaged"         ) ::
        ImageAsset("item-crossbow-damaged"    ) ::
        ImageAsset("item-hammer-damaged"      ) ::
        ImageAsset("item-teapot-damaged"      ) ::

        ImageAsset("item-boots-damaged-exhausted"     ) ::
        ImageAsset("item-coins-damaged-exhausted"     ) ::
        ImageAsset("item-sword-damaged-exhausted"     ) ::
        ImageAsset("item-torch-damaged-exhausted"     ) ::
        ImageAsset("item-bag-damaged-exhausted"       ) ::
        ImageAsset("item-crossbow-damaged-exhausted"  ) ::
        ImageAsset("item-hammer-damaged-exhausted"    ) ::
        ImageAsset("item-teapot-damaged-exhausted"    ) ::
        
        ImageAsset("item-boots-empty"     ) ::
        ImageAsset("item-coins-empty"     ) ::
        ImageAsset("item-sword-empty"     ) ::
        ImageAsset("item-torch-empty"     ) ::
        ImageAsset("item-bag-empty"       ) ::
        ImageAsset("item-crossbow-empty"  ) ::
        ImageAsset("item-hammer-empty"    ) ::
        ImageAsset("item-teapot-empty"    ) ::
                                              
        ImageAsset("item-any"         ) ::
        ImageAsset("item-any-damaged" ) ::
        ImageAsset("building-any"     ) ::

        ImageAsset("swap"    ) ::
        ImageAsset("move"    ) ::
        ImageAsset("battle"  ) ::
        ImageAsset("remove"  ) ::
        ImageAsset("income"  ) ::
        ImageAsset("current"  ) ::

        ImageAsset("move-deg-0"    ) ::
        ImageAsset("move-deg-15"   ) ::
        ImageAsset("move-deg-30"   ) ::
        ImageAsset("move-deg-45"   ) ::
        ImageAsset("move-deg-60"   ) ::
        ImageAsset("move-deg-75"   ) ::
        ImageAsset("move-deg-90"   ) ::
        ImageAsset("move-deg-105"  ) ::
        ImageAsset("move-deg-120"  ) ::
        ImageAsset("move-deg-135"  ) ::
        ImageAsset("move-deg-150"  ) ::
        ImageAsset("move-deg-165"  ) ::
        ImageAsset("move-deg-180"  ) ::
        ImageAsset("move-deg-195"  ) ::
        ImageAsset("move-deg-210"  ) ::
        ImageAsset("move-deg-225"  ) ::
        ImageAsset("move-deg-240"  ) ::
        ImageAsset("move-deg-255"  ) ::
        ImageAsset("move-deg-270"  ) ::
        ImageAsset("move-deg-285"  ) ::
        ImageAsset("move-deg-300"  ) ::
        ImageAsset("move-deg-315"  ) ::
        ImageAsset("move-deg-330"  ) ::
        ImageAsset("move-deg-345"  ) ::
        
        ImageAsset("card-back-large"             ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => options.has(AutumnMap), "autumn")(
        ImageAsset("map"          ,    "map-bright-new-expand", "jpg" ) ::
        ImageAsset("map-regions"  ) ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-white") ::
        
        ImageAsset("clearing-beach-fox"           ) ::
        ImageAsset("clearing-creek-fox"           ) ::
        ImageAsset("clearing-dune-fox"            ) ::
        ImageAsset("clearing-glade-fox"           ) ::
        ImageAsset("clearing-haven-fox"           ) ::
        ImageAsset("clearing-hill-fox"            ) ::
        ImageAsset("clearing-meadow-fox"          ) ::
        ImageAsset("clearing-mountain-fox"        ) ::
        ImageAsset("clearing-pond-fox"            ) ::
        ImageAsset("clearing-quarry-fox"          ) ::
        ImageAsset("clearing-waterfall-fox"       ) ::
        ImageAsset("clearing-weald-fox"           ) ::
        
        ImageAsset("clearing-beach-rabbit"           ) ::
        ImageAsset("clearing-creek-rabbit"           ) ::
        ImageAsset("clearing-dune-rabbit"            ) ::
        ImageAsset("clearing-glade-rabbit"           ) ::
        ImageAsset("clearing-haven-rabbit"           ) ::
        ImageAsset("clearing-hill-rabbit"            ) ::
        ImageAsset("clearing-meadow-rabbit"          ) ::
        ImageAsset("clearing-mountain-rabbit"        ) ::
        ImageAsset("clearing-pond-rabbit"            ) ::
        ImageAsset("clearing-quarry-rabbit"          ) ::
        ImageAsset("clearing-waterfall-rabbit"       ) ::
        ImageAsset("clearing-weald-rabbit"           ) ::
        
        ImageAsset("clearing-beach-mouse"           ) ::
        ImageAsset("clearing-creek-mouse"           ) ::
        ImageAsset("clearing-dune-mouse"            ) ::
        ImageAsset("clearing-glade-mouse"           ) ::
        ImageAsset("clearing-haven-mouse"           ) ::
        ImageAsset("clearing-hill-mouse"            ) ::
        ImageAsset("clearing-meadow-mouse"          ) ::
        ImageAsset("clearing-mountain-mouse"        ) ::
        ImageAsset("clearing-pond-mouse"            ) ::
        ImageAsset("clearing-quarry-mouse"          ) ::
        ImageAsset("clearing-waterfall-mouse"       ) ::
        ImageAsset("clearing-weald-mouse"           ) ::
        
        ImageAsset("path-beach-quarry"          ) ::
        ImageAsset("path-creek-quarry"          ) ::
        ImageAsset("path-dune-hill"             ) ::
        ImageAsset("path-glade-beach"           ) ::
        ImageAsset("path-glade-dune"            ) ::
        ImageAsset("path-glade-pond"            ) ::
        ImageAsset("path-glade-waterfall"       ) ::
        ImageAsset("path-haven-dune"            ) ::
        ImageAsset("path-haven-glade"           ) ::
        ImageAsset("path-haven-meadow"          ) ::
        ImageAsset("path-hill-beach"            ) ::
        ImageAsset("path-hill-creek"            ) ::
        ImageAsset("path-meadow-pond"           ) ::
        ImageAsset("path-mountain-quarry"       ) ::
        ImageAsset("path-pond-weald"            ) ::
        ImageAsset("path-waterfall-mountain"    ) ::
        ImageAsset("path-weald-mountain"        ) ::
        ImageAsset("path-weald-waterfall"       ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => options.has(WinterMap), "winter")(
        ImageAsset("map"          ,    "map-bright", "jpg" ) ::
        ImageAsset("map-regions"  ) ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-black") ::

        ImageAsset("clearing-bend-fox"         ) ::
        ImageAsset("clearing-bend-mouse"       ) ::
        ImageAsset("clearing-bend-rabbit"      ) ::
        ImageAsset("clearing-dale-fox"         ) ::
        ImageAsset("clearing-dale-mouse"       ) ::
        ImageAsset("clearing-dale-rabbit"      ) ::
        ImageAsset("clearing-drift-fox"         ) ::
        ImageAsset("clearing-drift-mouse"       ) ::
        ImageAsset("clearing-drift-rabbit"      ) ::
        ImageAsset("clearing-ford-fox"         ) ::
        ImageAsset("clearing-ford-mouse"       ) ::
        ImageAsset("clearing-ford-rabbit"      ) ::
        ImageAsset("clearing-hedge-fox"         ) ::
        ImageAsset("clearing-hedge-mouse"       ) ::
        ImageAsset("clearing-hedge-rabbit"      ) ::
        ImageAsset("clearing-moor-fox"         ) ::
        ImageAsset("clearing-moor-mouse"       ) ::
        ImageAsset("clearing-moor-rabbit"      ) ::
        ImageAsset("clearing-mound-fox"         ) ::
        ImageAsset("clearing-mound-mouse"       ) ::
        ImageAsset("clearing-mound-rabbit"      ) ::
        ImageAsset("clearing-pit-fox"         ) ::
        ImageAsset("clearing-pit-mouse"       ) ::
        ImageAsset("clearing-pit-rabbit"      ) ::
        ImageAsset("clearing-rock-fox"         ) ::
        ImageAsset("clearing-rock-mouse"       ) ::
        ImageAsset("clearing-rock-rabbit"      ) ::
        ImageAsset("clearing-spire-fox"         ) ::
        ImageAsset("clearing-spire-mouse"       ) ::
        ImageAsset("clearing-spire-rabbit"      ) ::
        ImageAsset("clearing-trench-fox"         ) ::
        ImageAsset("clearing-trench-mouse"       ) ::
        ImageAsset("clearing-trench-rabbit"      ) ::
        ImageAsset("clearing-wade-fox"         ) ::
        ImageAsset("clearing-wade-mouse"       ) ::
        ImageAsset("clearing-wade-rabbit"      ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => options.has(LakeMap), "lake")(
        ImageAsset("map"          ,    "map-bright", "jpg" ) ::
        ImageAsset("map-regions"  ) ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-white") ::
     
        ImageAsset("clearing-alley-fox"  ) ::
        ImageAsset("clearing-alley-mouse"  ) ::
        ImageAsset("clearing-alley-rabbit"  ) ::
        ImageAsset("clearing-bay-fox"  ) ::
        ImageAsset("clearing-bay-mouse"  ) ::
        ImageAsset("clearing-bay-rabbit"  ) ::
        ImageAsset("clearing-den-fox"  ) ::
        ImageAsset("clearing-den-mouse"  ) ::
        ImageAsset("clearing-den-rabbit"  ) ::
        ImageAsset("clearing-grove-fox"  ) ::
        ImageAsset("clearing-grove-mouse"  ) ::
        ImageAsset("clearing-grove-rabbit"  ) ::
        ImageAsset("clearing-gulf-fox"  ) ::
        ImageAsset("clearing-gulf-mouse"  ) ::
        ImageAsset("clearing-gulf-rabbit"  ) ::
        ImageAsset("clearing-lawn-fox"  ) ::
        ImageAsset("clearing-lawn-mouse"  ) ::
        ImageAsset("clearing-lawn-rabbit"  ) ::
        ImageAsset("clearing-marsh-fox"  ) ::
        ImageAsset("clearing-marsh-mouse"  ) ::
        ImageAsset("clearing-marsh-rabbit"  ) ::
        ImageAsset("clearing-prairie-fox"  ) ::
        ImageAsset("clearing-prairie-mouse"  ) ::
        ImageAsset("clearing-prairie-rabbit"  ) ::
        ImageAsset("clearing-shade-fox"  ) ::
        ImageAsset("clearing-shade-mouse"  ) ::
        ImageAsset("clearing-shade-rabbit"  ) ::
        ImageAsset("clearing-shoal-fox"  ) ::
        ImageAsset("clearing-shoal-mouse"  ) ::
        ImageAsset("clearing-shoal-rabbit"  ) ::
        ImageAsset("clearing-vert-fox"  ) ::
        ImageAsset("clearing-vert-mouse"  ) ::
        ImageAsset("clearing-vert-rabbit"  ) ::
        ImageAsset("clearing-yard-fox"  ) ::
        ImageAsset("clearing-yard-mouse"  ) ::
        ImageAsset("clearing-yard-rabbit"  ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => options.has(MountainMap), "mountain")(
        ImageAsset("map"          ,    "map-bright", "jpg" ) ::
        ImageAsset("map-regions"  ) ::
        ImageAsset("map-woods"  ) ::
        
        ImageAsset("building-slot", "empty-building-white") ::
     
        ImageAsset("clearing-brim-fox"       ) ::
        ImageAsset("clearing-brim-mouse"     ) ::
        ImageAsset("clearing-brim-rabbit"    ) ::
        ImageAsset("clearing-cliff-fox"      ) ::
        ImageAsset("clearing-cliff-mouse"    ) ::
        ImageAsset("clearing-cliff-rabbit"   ) ::
        ImageAsset("clearing-crest-fox"      ) ::
        ImageAsset("clearing-crest-mouse"    ) ::
        ImageAsset("clearing-crest-rabbit"   ) ::
        ImageAsset("clearing-drain-fox"      ) ::
        ImageAsset("clearing-drain-mouse"    ) ::
        ImageAsset("clearing-drain-rabbit"   ) ::
        ImageAsset("clearing-ledge-fox"      ) ::
        ImageAsset("clearing-ledge-mouse"    ) ::
        ImageAsset("clearing-ledge-rabbit"   ) ::
        ImageAsset("clearing-mine-fox"       ) ::
        ImageAsset("clearing-mine-mouse"     ) ::
        ImageAsset("clearing-mine-rabbit"    ) ::
        ImageAsset("clearing-pass-fox"       ) ::
        ImageAsset("clearing-pass-mouse"     ) ::
        ImageAsset("clearing-pass-rabbit"    ) ::
        ImageAsset("clearing-peak-fox"       ) ::
        ImageAsset("clearing-peak-mouse"     ) ::
        ImageAsset("clearing-peak-rabbit"    ) ::
        ImageAsset("clearing-ramp-fox"       ) ::
        ImageAsset("clearing-ramp-mouse"     ) ::
        ImageAsset("clearing-ramp-rabbit"    ) ::
        ImageAsset("clearing-ridge-fox"      ) ::
        ImageAsset("clearing-ridge-mouse"    ) ::
        ImageAsset("clearing-ridge-rabbit"   ) ::
        ImageAsset("clearing-slope-fox"      ) ::
        ImageAsset("clearing-slope-mouse"    ) ::
        ImageAsset("clearing-slope-rabbit"   ) ::
        ImageAsset("clearing-valley-fox"     ) ::
        ImageAsset("clearing-valley-mouse"   ) ::
        ImageAsset("clearing-valley-rabbit"  ) ::
        
        ImageAsset("rubble-mine-peak"  ) ::
        ImageAsset("rubble-ledge-mine"  ) ::
        ImageAsset("rubble-brim-ledge"  ) ::
        ImageAsset("rubble-ramp-valley"  ) ::
        ImageAsset("rubble-valley-ridge"  ) ::
        ImageAsset("rubble-cliff-crest"  ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.%(_.feline.any).any)(
        ImageAsset("castle"            ) ::
        ImageAsset("castle-m"          ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(MC))(
        ImageAsset("mc-glyph"          ) ::
        ImageAsset("mc-title"          ) ::
        ImageAsset("mc-char"           ) ::
        ImageAsset("mc-cat"            ) ::
        ImageAsset("mc-cat-empty"      ) ::
        ImageAsset("mc-cat-x5"         ) ::
        ImageAsset("mc-cat-x5-empty"   ) ::
        ImageAsset("mc-sawmill"        ) ::
        ImageAsset("mc-workshop"       ) ::
        ImageAsset("mc-recruiter"      , "mc-recruiter-new") ::
        ImageAsset("mc-wood"           ) ::
        ImageAsset("mc-keep"           ) ::
        ImageAsset("mc-action"         ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(BK))(
        ImageAsset("bk-glyph"          ) ::
        ImageAsset("bk-cat"            ) ::
        ImageAsset("bk-cat-empty"      , "mc-cat-empty") ::
        ImageAsset("bk-cat-x5"         ) ::
        ImageAsset("bk-cat-x5-empty"   , "mc-cat-x5-empty") ::
        ImageAsset("bk-sawmill"        ) ::
        ImageAsset("bk-workshop"       ) ::
        ImageAsset("bk-recruiter"      , "bk-recruiter-new") ::
        ImageAsset("bk-wood"           ) ::
        ImageAsset("bk-keep"           , "mc-keep") ::
        ImageAsset("bk-action"         ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(ED))(
        ImageAsset("ed-glyph"          ) ::
        ImageAsset("ed-title"          ) ::
        ImageAsset("ed-char"           ) ::
        ImageAsset("ed-hawk"           ) ::
        ImageAsset("ed-hawk-empty"     ) ::
        ImageAsset("ed-hawk-x5"        ) ::
        ImageAsset("ed-hawk-x5-empty"  ) ::
        ImageAsset("ed-roost"          ) ::
        ImageAsset("builder"           , "cards/builder") ::
        ImageAsset("charismatic"       , "cards/charismatic") ::
        ImageAsset("commander"         , "cards/commander") ::
        ImageAsset("despot"            , "cards/despot") ::
        ImageAsset("ed-builder"        ) ::
        ImageAsset("ed-charismatic"    ) ::
        ImageAsset("ed-commander"      ) ::
        ImageAsset("ed-despot"         ) ::

        ImageAsset("ed-recruit-bird"           ) ::
        ImageAsset("ed-recruit-fox"            ) ::
        ImageAsset("ed-recruit-rabbit"         ) ::
        ImageAsset("ed-recruit-mouse"          ) ::
        ImageAsset("ed-double-recruit-bird"      ) ::
        ImageAsset("ed-double-recruit-fox"       ) ::
        ImageAsset("ed-double-recruit-rabbit"    ) ::
        ImageAsset("ed-double-recruit-mouse"     ) ::
        ImageAsset("ed-move-bird"              ) ::
        ImageAsset("ed-move-fox"               ) ::
        ImageAsset("ed-move-rabbit"            ) ::
        ImageAsset("ed-move-mouse"             ) ::
        ImageAsset("ed-battle-bird"            ) ::
        ImageAsset("ed-battle-fox"             ) ::
        ImageAsset("ed-battle-rabbit"          ) ::
        ImageAsset("ed-battle-mouse"           ) ::
        ImageAsset("ed-build-bird"             ) ::
        ImageAsset("ed-build-fox"              ) ::
        ImageAsset("ed-build-rabbit"           ) ::
        ImageAsset("ed-build-mouse"            ) ::

        ImageAsset("ed-recruit-bird-done"      ) ::
        ImageAsset("ed-recruit-fox-done"       ) ::
        ImageAsset("ed-recruit-rabbit-done"    ) ::
        ImageAsset("ed-recruit-mouse-done"     ) ::
        ImageAsset("ed-double-recruit-bird-done"      ) ::
        ImageAsset("ed-double-recruit-fox-done"       ) ::
        ImageAsset("ed-double-recruit-rabbit-done"    ) ::
        ImageAsset("ed-double-recruit-mouse-done"     ) ::
        ImageAsset("ed-move-bird-done"         ) ::
        ImageAsset("ed-move-fox-done"          ) ::
        ImageAsset("ed-move-rabbit-done"       ) ::
        ImageAsset("ed-move-mouse-done"        ) ::
        ImageAsset("ed-battle-bird-done"       ) ::
        ImageAsset("ed-battle-fox-done"        ) ::
        ImageAsset("ed-battle-rabbit-done"     ) ::
        ImageAsset("ed-battle-mouse-done"      ) ::
        ImageAsset("ed-build-bird-done"        ) ::
        ImageAsset("ed-build-fox-done"         ) ::
        ImageAsset("ed-build-rabbit-done"      ) ::
        ImageAsset("ed-build-mouse-done"       ) ::
        
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(WA))(
        ImageAsset("wa-glyph"         ) ::
        ImageAsset("wa-title"         ) ::
        ImageAsset("wa-char"           ) ::
        ImageAsset("wa-critter"       ) ::
        ImageAsset("wa-critter-empty" ) ::
        ImageAsset("wa-officer"       ) ::
        ImageAsset("wa-supporter"     ) ::
        ImageAsset("wa-sympathy"      ) ::
        ImageAsset("wa-base-fox"      ) ::
        ImageAsset("wa-base-rabbit"   ) ::
        ImageAsset("wa-base-mouse"    ) ::
        ImageAsset("wa-action"        ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(AF))(
        ImageAsset("af-glyph"         ) ::
        ImageAsset("af-critter"       ) ::
        ImageAsset("af-critter-empty" ) ::
        ImageAsset("af-officer"       ) ::
        ImageAsset("af-supporter"     ) ::
        ImageAsset("af-sympathy"      ) ::
        ImageAsset("af-base-fox"      ) ::
        ImageAsset("af-base-rabbit"   ) ::
        ImageAsset("af-base-mouse"    ) ::
        ImageAsset("af-action"        ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(RF))(
        ImageAsset("rf-glyph"           ) ::
        ImageAsset("rf-title"           ) ::
        ImageAsset("rf-char"           ) ::
        ImageAsset("rf-otter"           ) ::
        ImageAsset("rf-otter-empty"     ) ::
        ImageAsset("rf-otter-x5"        ) ::
        ImageAsset("rf-otter-x5-empty"  ) ::
        ImageAsset("rf-trade-post-fox"    ) ::
        ImageAsset("rf-trade-post-rabbit" ) ::
        ImageAsset("rf-trade-post-mouse"  ) ::
        ImageAsset("rf-trade-post-fox-destroyed"    ) ::
        ImageAsset("rf-trade-post-rabbit-destroyed" ) ::
        ImageAsset("rf-trade-post-mouse-destroyed"  ) ::
 
        ImageAsset("rf-price-1") ::
        ImageAsset("rf-price-2") ::
        ImageAsset("rf-price-3") ::
        ImageAsset("rf-price-4") ::
        ImageAsset("rf-price-4-selected") ::
        ImageAsset("rf-price-3-selected") ::
        ImageAsset("rf-price-2-selected") ::
        ImageAsset("rf-price-1-selected") ::
        ImageAsset("rf-service-mercs") ::
        ImageAsset("rf-service-boats") ::
        ImageAsset("rf-service-cards") ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(SF))(
        ImageAsset("sf-glyph") ::
        ImageAsset("sf-squirrel"           ) ::
        ImageAsset("sf-squirrel-empty"     ) ::
        ImageAsset("sf-squirrel-x5"        ) ::
        ImageAsset("sf-squirrel-x5-empty"  ) ::

        ImageAsset("sf-trade-post-fox"    ) ::
        ImageAsset("sf-trade-post-rabbit" ) ::
        ImageAsset("sf-trade-post-mouse"  ) ::
        ImageAsset("sf-trade-post-fox-destroyed"    , "rf-trade-post-fox-destroyed"    ) ::
        ImageAsset("sf-trade-post-rabbit-destroyed" , "rf-trade-post-rabbit-destroyed" ) ::
        ImageAsset("sf-trade-post-mouse-destroyed"  , "rf-trade-post-mouse-destroyed"  ) ::
 
        ImageAsset("sf-price-1") ::
        ImageAsset("sf-price-2") ::
        ImageAsset("sf-price-3") ::
        ImageAsset("sf-price-4") ::
        ImageAsset("sf-price-4-selected") ::
        ImageAsset("sf-price-3-selected") ::
        ImageAsset("sf-price-2-selected") ::
        ImageAsset("sf-price-1-selected") ::
        ImageAsset("sf-service-cards") ::
        ImageAsset("sf-service-balloon") ::
        ImageAsset("sf-service-outsourcing") ::
        ImageAsset("sf-service-peacekeepers") ::
        ImageAsset("sf-service-shares") ::
        ImageAsset("sf-service-talent-scout") ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(LC))(
        ImageAsset("lc-glyph"            ) ::
        ImageAsset("lc-title"            ) ::
        ImageAsset("lc-char"             ) ::
        ImageAsset("lc-lizard"           ) ::
        ImageAsset("lc-lizard-x5"        ) ::
        ImageAsset("lc-lizard-empty"     ) ::
        ImageAsset("lc-lizard-x5-empty"  ) ::
        ImageAsset("lc-acolyte"          ) ::
        ImageAsset("lc-acolyte-x5"       ) ::
        ImageAsset("lc-garden-fox"       ) ::
        ImageAsset("lc-garden-rabbit"    ) ::
        ImageAsset("lc-garden-mouse"     ) ::
        ImageAsset("outcast-fox"         ) ::
        ImageAsset("outcast-fox-hated"   ) ::
        ImageAsset("outcast-rabbit"      ) ::
        ImageAsset("outcast-rabbit-hated") ::
        ImageAsset("outcast-mouse"       ) ::
        ImageAsset("outcast-mouse-hated" ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(UD))(
        ImageAsset("ud-glyph"          ) ::
        ImageAsset("ud-title"          ) ::
        ImageAsset("ud-char"           ) ::
        ImageAsset("ud-mole"           ) ::
        ImageAsset("ud-mole-x5"        ) ::
        ImageAsset("ud-mole-empty"     ) ::
        ImageAsset("ud-mole-x5-empty"  ) ::
        ImageAsset("ud-young"          ) ::
        ImageAsset("ud-young-x5"       ) ::
        ImageAsset("ud-citadel"        ) ::
        ImageAsset("ud-market"         ) ::
        ImageAsset("ud-tunnel"         ) ::
        ImageAsset("ud-crown"          ) ::
        ImageAsset("ud-crown-2"          ) ::
        ImageAsset("ud-crown-3"          ) ::
        ImageAsset("ud-crown-4"          ) ::
        ImageAsset("ud-crown-empty"    ) ::
        ImageAsset("ud-action"         ) ::
        ImageAsset("ud-building"       ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(CC))(
        ImageAsset("cc-glyph"          ) ::
        ImageAsset("cc-title"          ) ::
        ImageAsset("cc-char"           ) ::
        ImageAsset("cc-raven"          ) ::
        ImageAsset("cc-raven-empty"    ) ::
        ImageAsset("cc-raven-x5"       ) ::
        ImageAsset("cc-raven-x5-empty" ) ::
        ImageAsset("cc-bomb"           ) ::
        ImageAsset("cc-snare"          ) ::
        ImageAsset("cc-extortion"      ) ::
        ImageAsset("cc-raid"           ) ::
        ImageAsset("cc-plot"           ) ::
        ImageAsset("cc-diversion"      , "cc-plot"            ) ::
        ImageAsset("cc-action"         ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(RI))(
        ImageAsset("ri-glyph"           ) ::
        ImageAsset("ri-raven"           ) ::
        ImageAsset("ri-raven-empty"     , "cc-raven-empty"     ) ::
        ImageAsset("ri-raven-x5"        ) ::
        ImageAsset("ri-raven-x5-empty"  , "cc-raven-x5-empty"  ) ::
        ImageAsset("ri-bomb"            ) ::
        ImageAsset("ri-snare"           ) ::
        ImageAsset("ri-extortion"       ) ::
        ImageAsset("ri-raid"            ) ::
        ImageAsset("ri-plot"            ) ::
        ImageAsset("ri-diversion"       , "ri-plot"            ) ::
        ImageAsset("ri-action"          ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(LH))(
        ImageAsset("lh-glyph"          ) ::
        ImageAsset("lh-title"          ) ::
        ImageAsset("lh-char"           ) ::
        ImageAsset("lh-rat"            ) ::
        ImageAsset("lh-rat-empty"      ) ::
        ImageAsset("lh-rat-x5"         ) ::
        ImageAsset("lh-rat-x5-empty"   ) ::
        ImageAsset("lh-warlord"        ) ::
        ImageAsset("lh-warlord-empty"  ) ::
        ImageAsset("lh-stronghold"     ) ::
        ImageAsset("lh-mob"            ) ::
        ImageAsset("lh-mob-q"          ) ::
        ImageAsset("lh-move"           ) ::
        ImageAsset("lh-move-done"      ) ::
        ImageAsset("lh-battle"         ) ::
        ImageAsset("lh-battle-done"    ) ::
        ImageAsset("lh-action"         ) ::

        ImageAsset("satchel-left-1-lh"     ) ::
        ImageAsset("satchel-top-2-lh"   ) ::
        ImageAsset("satchel-top-3-lh"   ) ::
        ImageAsset("satchel-top-4-lh"   ) ::
        ImageAsset("satchel-bottom-2-lh"   ) ::
        ImageAsset("satchel-bottom-3-lh"   ) ::
        ImageAsset("satchel-bottom-4-lh"   ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(KI))(
        ImageAsset("ki-glyph"          ) ::
        ImageAsset("ki-title"          ) ::
        ImageAsset("ki-char"           ) ::
        
        ImageAsset("faithful-retainer", "cards/v2/faithful-retainer", "jpg") ::

        ImageAsset("ki-badger-empty") ::
        ImageAsset("ki-badger-x5-empty") ::
        ImageAsset("ki-badger-x5") ::
        ImageAsset("ki-badger") ::
        ImageAsset("ki-idol-1") ::
        ImageAsset("ki-idol-2") ::
        ImageAsset("ki-idol-3") ::
        ImageAsset("ki-idol-hidden") ::
        ImageAsset("ki-jewel-1") ::
        ImageAsset("ki-jewel-2") ::
        ImageAsset("ki-jewel-3") ::
        ImageAsset("ki-jewel-hidden") ::
        ImageAsset("ki-tablet-1") ::
        ImageAsset("ki-tablet-2") ::
        ImageAsset("ki-tablet-3") ::
        ImageAsset("ki-tablet-hidden") ::
        ImageAsset("ki-waystation-idol-jewel") ::
        ImageAsset("ki-waystation-idol-tablet") ::
        ImageAsset("ki-waystation-jewel-idol") ::
        ImageAsset("ki-waystation-jewel-tablet") ::
        ImageAsset("ki-waystation-tablet-idol") ::
        ImageAsset("ki-waystation-tablet-jewel") ::    
 
        ImageAsset("ki-move-bird-done"       ,  "ed-move-bird-done"         ) ::
        ImageAsset("ki-move-fox-done"        ,  "ed-move-fox-done"          ) ::
        ImageAsset("ki-move-rabbit-done"     ,  "ed-move-rabbit-done"       ) ::
        ImageAsset("ki-move-mouse-done"      ,  "ed-move-mouse-done"        ) ::
        ImageAsset("ki-battle-bird-done"     ,  "ed-battle-bird-done"       ) ::
        ImageAsset("ki-battle-fox-done"      ,  "ed-battle-fox-done"        ) ::
        ImageAsset("ki-battle-rabbit-done"   ,  "ed-battle-rabbit-done"     ) ::
        ImageAsset("ki-battle-mouse-done"    ,  "ed-battle-mouse-done"      ) ::
        ImageAsset("ki-recover-bird-done"    ) ::
        ImageAsset("ki-recover-fox-done"     ) ::
        ImageAsset("ki-recover-rabbit-done"  ) ::
        ImageAsset("ki-recover-mouse-done"   ) ::
        
        ImageAsset("ki-move-bird"       ,  "ed-move-bird"         ) ::
        ImageAsset("ki-move-fox"        ,  "ed-move-fox"          ) ::
        ImageAsset("ki-move-rabbit"     ,  "ed-move-rabbit"       ) ::
        ImageAsset("ki-move-mouse"      ,  "ed-move-mouse"        ) ::
        ImageAsset("ki-battle-bird"     ,  "ed-battle-bird"       ) ::
        ImageAsset("ki-battle-fox"      ,  "ed-battle-fox"        ) ::
        ImageAsset("ki-battle-rabbit"   ,  "ed-battle-rabbit"     ) ::
        ImageAsset("ki-battle-mouse"    ,  "ed-battle-mouse"      ) ::
        ImageAsset("ki-recover-bird"    ) ::
        ImageAsset("ki-recover-fox"     ) ::
        ImageAsset("ki-recover-rabbit"  ) ::
        ImageAsset("ki-recover-mouse"   ) ::
        
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.has(OK))(
        ImageAsset("ok-glyph"           ) ::
        ImageAsset("ok-badger"          ) ::
        ImageAsset("ok-badger-empty"    ) ::
        ImageAsset("ok-badger-x5"       ) ::
        ImageAsset("ok-badger-x5-empty" ) ::
        ImageAsset("ok-caravan"         ) ::
        ImageAsset("ok-station"         ) ::
        ImageAsset("ok-tablet"          ) ::
        ImageAsset("ok-jewel"           ) ::
        ImageAsset("ok-idol"            ) ::
        ImageAsset("ok-tablet-hidden"   ) ::
        ImageAsset("ok-jewel-hidden"    ) ::
        ImageAsset("ok-idol-hidden"     ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.%(_.hero.any).any)(
        ImageAsset("vb-glyph"          ) ::
        ImageAsset("nb-glyph"          ) ::
        ImageAsset("mb-glyph"          ) ::
        ImageAsset("vb-vagabond"       ) ::
        ImageAsset("nb-vagabond"       ) ::
        ImageAsset("mb-vagabond"       ) ::

        ImageAsset("item-x-spacer"     ) ::

        ImageAsset("quest-fox"        ) ::
        ImageAsset("quest-rabbit"     ) ::
        ImageAsset("quest-mouse"      ) ::
        
        ImageAsset("attitude-heart-empty" ) ::
        ImageAsset("attitude-heart-full"  ) ::
        ImageAsset("attitude-hostile"     ) ::

        ImageAsset("scorched-earth-1"  ) ::
        ImageAsset("scorched-earth-2"  ) ::
        ImageAsset("scorched-earth-3"  ) ::
        ImageAsset("scorched-earth-4"  ) ::
        ImageAsset("scorched-earth-5"  ) ::
        ImageAsset("scorched-earth-6"  ) ::

        ImageAsset("vb-char-thief" ) ::
        ImageAsset("vb-char-tinker" ) ::
        ImageAsset("vb-char-ranger" ) ::
        ImageAsset("vb-char-arbiter" ) ::
        ImageAsset("vb-char-vagrant" ) ::
        ImageAsset("vb-char-scoundrel" ) ::
        ImageAsset("vb-char-adventurer" ) ::
        ImageAsset("vb-char-ronin" ) :: 
        ImageAsset("vb-char-harrier" ) ::

        ImageAsset("vb-title-thief" ) ::
        ImageAsset("vb-title-tinker" ) ::
        ImageAsset("vb-title-ranger" ) ::
        ImageAsset("vb-title-arbiter" ) ::
        ImageAsset("vb-title-vagrant" ) ::
        ImageAsset("vb-title-scoundrel" ) ::
        ImageAsset("vb-title-adventurer" ) ::
        ImageAsset("vb-title-ronin" ) :: 
        ImageAsset("vb-title-harrier" ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.%(_.hero.any).any, "cards", "quest:", ext = "jpg")(
        ImageAsset("errand-fox"             ) ::
        ImageAsset("errand-rabbit"          ) ::
        ImageAsset("escort"                 ) ::
        ImageAsset("expel-bandits-mouse"    ) ::
        ImageAsset("expel-bandits-rabbit"   ) ::
        ImageAsset("fend-off-a-bear-mouse"  ) ::
        ImageAsset("fend-off-a-bear-rabbit" ) ::
        ImageAsset("fundraising"            ) ::
        ImageAsset("give-a-speech-fox"      ) ::
        ImageAsset("give-a-speech-rabbit"   ) ::
        ImageAsset("guard-duty-mouse"       ) ::
        ImageAsset("guard-duty-rabbit"      ) ::
        ImageAsset("logistic-help-fox"      ) ::
        ImageAsset("logistic-help-mouse"    ) ::
        ImageAsset("repair-a-shed"          ) ::
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true, "cards/v2", ext = "jpg")(
        ImageAsset("bird-ambush"            ) ::
        ImageAsset("fox-ambush"             ) ::
        ImageAsset("mouse-ambush"           ) ::
        ImageAsset("rabbit-ambush"          ) ::

        ImageAsset("bird-dominance"         ) ::
        ImageAsset("fox-dominance"          ) ::
        ImageAsset("mouse-dominance"        ) ::
        ImageAsset("rabbit-dominance"       ) ::
        
        ImageAsset("a-visit-to-friends"     ) ::
        ImageAsset("anvil"                  ) ::
        ImageAsset("arms-trader"            ) ::
        ImageAsset("bake-sale"              ) ::
        ImageAsset("bird-crossbow"          ) ::
        ImageAsset("birdy-bindle"           ) ::
        ImageAsset("fox-root-tea"           ) ::
        ImageAsset("fox-travel-gear"        ) ::
        ImageAsset("foxfolk-steel"          ) ::
        ImageAsset("gently-used-knapsack"   ) ::
        ImageAsset("investments"            ) ::
        ImageAsset("mouse-crossbow"         ) ::
        ImageAsset("mouse-in-a-sack"        ) ::
        ImageAsset("mouse-root-tea"         ) ::
        ImageAsset("mouse-travel-gear"      ) ::
        ImageAsset("protection-racket"      ) ::
        ImageAsset("rabbit-root-tea"        ) ::
        ImageAsset("smugglers-trail"        ) ::
        ImageAsset("sword"                  ) ::
        ImageAsset("woodland-runners"       ) ::

        ImageAsset("armorers"               ) ::
        ImageAsset("better-burrow-bank"     ) ::
        ImageAsset("brutal-tactics"         ) ::
        ImageAsset("cobbler"                ) ::
        ImageAsset("codebreakers"           ) ::
        ImageAsset("command-warren"         ) ::
        ImageAsset("favor-of-the-foxes"     ) ::
        ImageAsset("favor-of-the-mice"      ) ::
        ImageAsset("favor-of-the-rabbits"   ) ::
        ImageAsset("royal-claim"            ) ::
        ImageAsset("sappers"                ) ::
        ImageAsset("scouting-party"         ) ::
        ImageAsset("stand-and-deliver"      ) ::
        ImageAsset("tax-collector"          ) ::
        
        ImageAsset("boat-builders"          ) ::
        ImageAsset("charm-offensive"        ) ::
        ImageAsset("coffin-makers"          ) ::
        ImageAsset("corvid-planners"        ) ::
        ImageAsset("eyrie-emigre"           ) ::
        ImageAsset("false-orders"           ) ::
        ImageAsset("fox-partisans"          ) ::
        ImageAsset("informants"             ) ::
        ImageAsset("league-of-adventurous-mice" ) ::
        ImageAsset("master-engravers"       ) ::
        ImageAsset("mouse-partisans"        ) ::
        ImageAsset("murine-broker"          ) ::
        ImageAsset("propaganda-bureau"      ) ::
        ImageAsset("rabbit-partisans"       ) ::
        ImageAsset("saboteurs"              ) ::
        ImageAsset("soup-kitchens"          ) ::
        ImageAsset("swap-meet"              ) ::
        ImageAsset("tunnels"                ) ::        
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => true, "artwork", "artwork:", ext = "jpg")(
        ImageAsset("bird-ambush"            ) ::
        ImageAsset("fox-ambush"             ) ::
        ImageAsset("mouse-ambush"           ) ::
        ImageAsset("rabbit-ambush"          ) ::

        ImageAsset("bird-dominance"         ) ::
        ImageAsset("fox-dominance"          ) ::
        ImageAsset("mouse-dominance"        ) ::
        ImageAsset("rabbit-dominance"       ) ::
        
        ImageAsset("a-visit-to-friends"     ) ::
        ImageAsset("anvil"                  ) ::
        ImageAsset("arms-trader"            ) ::
        ImageAsset("bake-sale"              ) ::
        ImageAsset("bird-crossbow"          ) ::
        ImageAsset("birdy-bindle"           ) ::
        ImageAsset("fox-root-tea"           ) ::
        ImageAsset("fox-travel-gear"        ) ::
        ImageAsset("foxfolk-steel"          ) ::
        ImageAsset("gently-used-knapsack"   ) ::
        ImageAsset("investments"            ) ::
        ImageAsset("mouse-crossbow"         ) ::
        ImageAsset("mouse-in-a-sack"        ) ::
        ImageAsset("mouse-root-tea"         ) ::
        ImageAsset("mouse-travel-gear"      ) ::
        ImageAsset("protection-racket"      ) ::
        ImageAsset("rabbit-root-tea"        ) ::
        ImageAsset("smugglers-trail"        ) ::
        ImageAsset("sword"                  ) ::
        ImageAsset("woodland-runners"       ) ::

        ImageAsset("armorers"               ) ::
        ImageAsset("better-burrow-bank"     ) ::
        ImageAsset("brutal-tactics"         ) ::
        ImageAsset("cobbler"                ) ::
        ImageAsset("codebreakers"           ) ::
        ImageAsset("command-warren"         ) ::
        ImageAsset("favor-of-the-foxes"     ) ::
        ImageAsset("favor-of-the-mice"      ) ::
        ImageAsset("favor-of-the-rabbits"   ) ::
        ImageAsset("royal-claim"            ) ::
        ImageAsset("sappers"                ) ::
        ImageAsset("scouting-party"         ) ::
        ImageAsset("stand-and-deliver"      ) ::
        ImageAsset("tax-collector"          ) ::
        
        ImageAsset("boat-builders"          ) ::
        ImageAsset("charm-offensive"        ) ::
        ImageAsset("coffin-makers"          ) ::
        ImageAsset("corvid-planners"        ) ::
        ImageAsset("eyrie-emigre"           ) ::
        ImageAsset("false-orders"           ) ::
        ImageAsset("fox-partisans"          ) ::
        ImageAsset("informants"             ) ::
        ImageAsset("league-of-adventurous-mice" ) ::
        ImageAsset("master-engravers"       ) ::
        ImageAsset("mouse-partisans"        ) ::
        ImageAsset("murine-broker"          ) ::
        ImageAsset("propaganda-bureau"      ) ::
        ImageAsset("rabbit-partisans"       ) ::
        ImageAsset("saboteurs"              ) ::
        ImageAsset("soup-kitchens"          ) ::
        ImageAsset("swap-meet"              ) ::
        ImageAsset("tunnels"                ) ::        
    Nil) ::
    ConditionalAssetsList((factions : List[Faction], options : List[O]) => factions.%(_.hero.any).any, "artwork/quest", "artwork:", ext = "jpg")(
        ImageAsset("errand-fox"             ) ::
        ImageAsset("errand-rabbit"          ) ::
        ImageAsset("escort"                 ) ::
        ImageAsset("expel-bandits-mouse"    ) ::
        ImageAsset("expel-bandits-rabbit"   ) ::
        ImageAsset("fend-off-a-bear-mouse"  ) ::
        ImageAsset("fend-off-a-bear-rabbit" ) ::
        ImageAsset("fundraising"            ) ::
        ImageAsset("give-a-speech-fox"      ) ::
        ImageAsset("give-a-speech-rabbit"   ) ::
        ImageAsset("guard-duty-mouse"       ) ::
        ImageAsset("guard-duty-rabbit"      ) ::
        ImageAsset("logistic-help-fox"      ) ::
        ImageAsset("logistic-help-mouse"    ) ::
        ImageAsset("repair-a-shed"          ) ::
    Nil)
        
    override val links : List[(String, String)] = $(("Advanced Setup" -> "http://van.im:777/play/root-adset"))
}
