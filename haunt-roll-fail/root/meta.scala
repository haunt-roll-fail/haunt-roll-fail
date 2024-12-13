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

import root.elem._

import hrf.meta._
import hrf.elem._
import hrf.reflect._
import hrf.options._



import helper.&

case class UnknownOption(o : String) extends GameOption {
    val group = "Unknown"
    val valueOn = "Unknown Option " ~ o
}

trait MapOption extends GameOption with OneOfGroup with ImportantOption {
    val group = "Map"
}

case object AutumnMap extends MapOption {
    val valueOn = "Autumn".styled(Mouse)
}

case object WinterMap extends MapOption {
    val valueOn = "Winter".styled(Bird)
}

case object LakeMap extends MapOption {
    val valueOn = "Lake".styled(Rabbit)
}

case object MountainMap extends MapOption {
    val valueOn = "Mountain".styled(Fox)
}

case object TidalMap extends MapOption {
    val valueOn = "Tidal Flats".styled(ED)
    override val explain = $(
        "A beautiful map by " ~ "Endgamer1331".hl ~ "!",
        "At the start of the game a randomly chosen central clearing is " ~ "Flooded".styled(ED) ~ " and all neighbouring clearings become " ~ "coastal clearings connected by water.",
        &("The " ~ "Boardwalk".styled(UD) ~ " paths are just regular paths,") ~ &(" but they don't divide forests.")
    )
}

case object TundraMap extends MapOption {
    val valueOn = "Tundra".styled(RF)
    override val explain = $(
        "A winter map by " ~ "Endgamer1331".hl ~ "!",
        "Each time all players make a turn, a die is rolled and the " ~ "Blizzard".hl ~ "moves, blocking another path between clearings, or the river."
    )
}

case object GloomMap extends MapOption {
    val valueOn = "Gloom".styled(styles.forest)
    override val explain = $(
        "A spooky map by " ~ "totgeboren".hl ~ "!",
        "The river divides forests.",
        "The clearing in the " ~ "River Mouth".hh ~ " are connected by river to each other.",
        "The " ~ "Jaw Clearings".hl ~ " are connected to a single nearby forest each."
    )
}


trait ClearingsOption extends GameOption with OneOfGroup {
    val group = "Clearings"
}

case object DefaultClearings extends ClearingsOption {
    val valueOn = "Default".hh
    override def required(all : $[BaseOption]) = $($(AutumnMap))
    override val explain = $("As printed on the " ~ "Autumn".styled(Mouse) ~ " map," ~ HorizontalBreak ~ "tree leaves color matches the clearing suit.")
}

case object AllRandomClearings extends ClearingsOption {
    val valueOn = "All Random".hh
    override val explain = $("All suits are assigned randomly.")
}

case object NoClustersClearings extends ClearingsOption {
    val valueOn = "No Clusters".hh
    override val explain = $("All suits are assigned randomly," ~ HorizontalBreak ~ "no two connected clearings share suit.")
}

case object SuitPairsClearings extends ClearingsOption {
    val valueOn = "Suit Pairs".hh
    override val explain = $("All suits are assigned randomly," ~ HorizontalBreak ~ "each clearing is connected to exactly one another clearing of the same suit.")
}

case object ConnectedClearings extends ClearingsOption {
    val valueOn = "Connected".hh
    override val explain = $("All suits are assigned randomly," ~ HorizontalBreak ~ "clearings of the same suit are all connected.")
}

case object ThreeFourFiveClearings extends ClearingsOption {
    val valueOn = "3".hh ~ "/" ~ "4".hh ~ "/" ~ "5".hh
    override val explain = $(
        "A random suit " ~ "is assigned" ~ " to the " ~ "three".hh ~ " clearings " ~ "with the most building slots.",
        "Another random suit " ~ "is assigned" ~ " to the " ~ "five".hh ~ " clearings " ~ "with the least building slots.",
        "Remaining suit " ~ "is assigned" ~ " to the remaining " ~ "four".hh ~ " clearings."
    )
}


trait DeckOption extends GameOption with OneOfGroup with ImportantOption {
    val group = "Deck"
}

case object StandardDeck extends DeckOption {
    val valueOn = "Friends and Favors".hh
}

case object ExilesDeck extends DeckOption {
    val valueOn = "Exiles and Partisans".hh
}

case object MixedDeck extends DeckOption {
    val valueOn = "Mixed".hh
    override val explain = $(
        "Both official decks mixed to take one instance" ~ &("of the most " ~ "effect cards".hh ~ "."),
        "Scouting Party".hh ~ " and " ~ "Coffin Makers".hh ~ " are excluded.",
        "Soup Kitchens".hh ~ " are replaced with " ~ "Borscht Kitchens".hh ~ ".",
        "Boat Builders".hh ~ " are excluded if " ~ RF.name.styled(RF) ~ " is in the game.",
        "Corvid Planners".hh ~ " are excluded if " ~ CC.name.styled(CC) ~ " is in the game.",
        "Ambushes, dominances and item craft cards same as in all decks.",
        Image("borscht-kitchens", styles.card)
    )
}

case object DuskDeck extends DeckOption {
    val valueOn = "Dawn and Dusk".hh
    override val explain = $(
        "An alternative deck by " ~ "its_rudimentary".hl ~ " and " ~ "Endgamer1331".hl ~ "!",
        "For " ~ "Service".styled(styles.FoxRabbitMouse) ~ " and " ~ "Phase".styled(styles.phase) ~ " cards optional rules for " ~ VB.elem ~ " interaction are available.",
    )
}

case object NonBirdPartisans extends GameOption with ToggleOption {
    val group = "Cards"
    val valueOn = "Partisans".styled(styles.get(Bird)) ~ SpacedDash ~ "Discard".hh ~ " " ~ "Birds".styled(Bird)
    override def required(all : $[BaseOption]) = $($(ExilesDeck), $(MixedDeck))
    override val explain = $(
        "As printed, " ~ "Partisan".hh ~ " cards don't imply that the " ~ Bird.elem ~ " cards are discarded.",
        "This option forces the discard of the " ~ Bird.elem ~ " cards.",
    )
}

case object UnthematicCoffinMakers extends GameOption with ToggleOption {
    val group = "Cards"
    val valueOn = "Coffin Makers".styled(styles.get(Rabbit)) ~ SpacedDash ~ "Any Warrior Recycle".hh
    override def required(all : $[BaseOption]) = $($(ExilesDeck))
    override val explain = $(
        "Without this option, only " ~ "forcibly".styled(styles.hit) ~ " removed warriors go to " ~ (FigureSpace + "Coffin Makers" + FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Rabbit)) ~ ".",
        "This option expands this to all instances of warriors returned to the supply."
    )
}

case object UnthematicPropagandaBureau extends GameOption with ToggleOption {
    val group = "Cards"
    val valueOn = "Propaganda Bureau".styled(styles.get(Fox)) ~ SpacedDash ~ "Optional Placement".hh
    override def required(all : $[BaseOption]) = $($(ExilesDeck), $(MixedDeck))
    override val explain = $(
        "With this option, " ~ "Propaganda Bureau".styled(styles.get(Fox)) ~ " can remove an enemy " ~ "warrior".hh ~ " even if own " ~ "warrior".hh ~ " can't be placed" ~ " afterwards."
    )
}

case object TunnelsIgnoreRaid extends GameOption with ToggleOption {
    val group = "Cards"
    val valueOn = "Tunnels".styled(styles.get(Rabbit)) ~ SpacedDash ~ "Ignore ".hh ~ "Raid".styled(CC)
    override def required(all : $[BaseOption]) = $($(ExilesDeck), $(MixedDeck))./~(d => $(d ++ $(IncludeFaction(CC)), d ++ $(IncludeFaction(RI))))
    override val explain = $(
        "With this option, triggered " ~ "Raid".styled(CC) ~ " doesn't benefit from " ~ (FigureSpace + "Tunnels" + FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Rabbit)) ~ "."
    )
}

case object TunnelsIgnoreTradePosts extends GameOption with ToggleOption {
    val group = "Cards"
    val valueOn = "Tunnels".styled(styles.get(Rabbit)) ~ SpacedDash ~ "Ignore ".hh ~ "Trade Posts".styled(RF)
    override def required(all : $[BaseOption]) = $($(ExilesDeck), $(MixedDeck))./~(d => $(d ++ $(IncludeFaction(RF))))
    override val explain = $(
        "With this option, " ~ "TradePosts".styled(RF) ~ " aren't considered crafting pieces for " ~ (FigureSpace + "Tunnels" + FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Rabbit)) ~ "."
    )
}


trait SeatingOption extends GameOption with OneOfGroup {
    val group = "Seating Order"
}

case object SeatingGiven extends SeatingOption {
    val valueOn = "Given".hh
    override val explain = $(
        "Players are seated in the order entered.",
    )
}

case object SeatingRandom extends SeatingOption {
    val valueOn = "Random".hh
    override val explain = $(
        "Players' seating order is randomized.",
    )
}


trait FactionSeatingOption extends GameOption with OneOfGroup {
    val group = "Faction Seating Order"
}

case object FactionSeatingGiven extends FactionSeatingOption {
    val valueOn = "Given".hh
}

case object FactionSeatingRandom extends FactionSeatingOption {
    val valueOn = "Random".hh
}


trait SetupOrderOption extends GameOption with OneOfGroup {
    val group = "Setup Order"
}

case object SetupOrderPriority extends SetupOrderOption {
    val valueOn = "Faction Priorities".hh
}

case object SetupOrderReverse extends SetupOrderOption {
    val valueOn = "Reverse Turn Order".hh
}


trait SetupTypeOption extends GameOption with OneOfGroup {
    val group = "Setup Type"
}

case object SetupTypeCorners extends SetupTypeOption {
    val valueOn = "Corners".hh
    override val explain = $(
        "Most militant factions setup in the map corners.",
    )
}

case object SetupTypeHomelands extends SetupTypeOption {
    val valueOn = "Homelands".hh
    override val explain = $(
        "Factions setup by claiming " ~ &("clearings as " ~ "Homelands".hh ~ "."),
    )
}


trait CardDraftOption extends GameOption with OneOfGroup {
    val group = "Card Draft"
}

case object CardDraftStandard extends CardDraftOption {
    val valueOn = "Draw Three".hh
}

case object CardDraftFive extends CardDraftOption {
    val valueOn = "Draw Five".hh ~ " / " ~ "Keep Three".hh
}


case object AutoHitsAssignmentMode extends GameOption with ToggleOption {
    val group = "Async"
    val valueOn = "Auto Hits Assignment".hh
    override val explain = $(
        &("In battle, all " ~ "hits".styled(styles.hit) ~ " are assigned automatically") ~ " " ~ &("if there's no choice in how they can be assigned.")
    )
}

case object AutoAmbushSkippingMode extends GameOption with ToggleOption {
    val group = "Async"
    val valueOn = "Auto Ambush Skipping".hh
    override val explain = $(
        &("Ambush".styled(Bird) ~ " prompts are skipped " ~ "if the player doesn't have any.".&)
    )
}

case object ForcedAsyncMode extends GameOption with ToggleOption {
    val group = "Async"
    val valueOn = "Forced Async Mode".styled(xstyles.warning)
    override def required(all : $[BaseOption]) = $($(AutoHitsAssignmentMode, AutoAmbushSkippingMode))
    override def decorate(elem : Elem) = Image("swap")(styles.token) ~ " " ~ elem ~ " " ~ Image("swap")(styles.token)
    override val explain = $(
        &("Ambush".styled(Bird) ~ " prompts are skipped " ~ "if the player doesn't have any.".&),
        &("Par".styled(Fox) ~ "tis".styled(Rabbit) ~ "ans".styled(Mouse) ~ " are always used " ~ "if no cards have to be discarded.".&),
        &("All " ~ "hits".styled(styles.hit) ~ " are assigned automatically."),
        &("At the end of the turn players with more than one type of " ~ "tokens".hh ~ " or " ~ "buildings".hh ~ " in any one clearing set the " ~ "order".hl ~ " in which they would be " ~ "removed".styled(styles.hit) ~ " in battle."),
        &(Vagabond.of(VB) ~ " player sets the " ~ "order".hl ~ " " ~ ("for at least " ~ "half".hh ~ " of their items").& ~ " " ~ ("to be " ~ "damaged".styled(styles.hit) ~ " in battle " ~ ("or with " ~ "revolts".styled(WA) ~ " or " ~ "bombs".styled(CC) ~ ".").&).&),
        &(MC.elem ~ " player can assign every " ~ "card".hh ~ " in their hand to be either " ~ "skipped".hl ~ " for " ~ "Field Hospitals".styled(MC) ~ ", " ~ "used".hh ~ " for " ~ "any".hl ~ " number of " ~ Cat.sof(MC) ~ ", or " ~ "used".hh ~ " for at least " ~ "two".hl ~ " " ~ Cat.sof(MC) ~ "."),
    )
}


trait AdSetBuffOption extends GameOption with OneOfGroup {
    val group = "AdSet " ~ "MC".styled(MC) ~ ", " ~ "LC".styled(LC) ~ ", " ~ "CC".styled(CC) ~ " Setup Buffs"
}

case object AdSetBuffOn extends AdSetBuffOption {
    val valueOn = "On".hh
}

case object AdSetBuffOff extends AdSetBuffOption {
    val valueOn = "Off".hh
}

trait MirrorOption extends GameOption with ToggleOption {
    val group = "Mirror Factions"
}

case object TheWoodMustFlow extends MirrorOption {
    val valueOn = "The Wood Must Flow".hh

    override val explain = $(
        "Cats".styled(MC) ~ " capture other " ~ "Cats'".styled(BK) ~ " " ~ "buildings".hh ~ ", " ~ "wood".hh ~ " and " ~ "keep".hh ~ " instead of destroying them, " ~ &("if they have the same available in their supply."),
        "They don't score points capturing.",
    )
}

case object OneTrueEmpire extends MirrorOption {
    val valueOn = "One True Empire".hh

    override val explain = $(
        "When " ~ "Birds".styled(ED) ~ " " ~ "turmoil".hh ~ ", the " ~ "bird".styled(Bird) ~ " cards from the " ~ "Decree".hh ~ " are not discarded, they are added to other " ~ "Birds'".styled(PE) ~ " " ~ "Decree".hh ~ " in the same columns.",
        "With each " ~ "turmoil".hh ~ " " ~ "Birds".styled(ED) ~ " increase their turmoil " ~ "tolerance".hh ~ " that allows them to skip one more unfulfillable order before turmoiling."
    )
}

case object Imprinting extends MirrorOption {
    val valueOn = "Imprinting".hh

    override val explain = $(
        "Enemy " ~ "Sympathy".styled(WA) ~ " " ~ "increases the cost of spreading " ~ "Sympathy".styled(FU) ~ " by " ~ "one".hh ~ ".",
        "Stacks with the " ~ "Martial Law".hh ~ "."
    )
}

case object RootingForTheUnderdog extends MirrorOption {
    val valueOn = "Rooting for the Underdog".hh

    override val explain = $(
        "Vagabond".styled(VB) ~ " refreshes " ~ "one more".hh ~ " item in the " ~ Birdsong.elem ~ " " ~ &("if they have less " ~ "VP".styled(styles.vp) ~ " than other " ~ "Vagabonds".styled(NB) ~ "."),
        "Vagabond".styled(VB) ~ " refreshes " ~ "one less".hh ~ " item in the " ~ Birdsong.elem ~ " " ~ &("if they have more " ~ "VP".styled(styles.vp) ~ " than other " ~ "Vagabonds".styled(NB) ~ ".")
    )
}

case object DeathToTheInfidels extends MirrorOption {
    val valueOn = "Death to the Infidels".hh

    override val explain = $(
        "Lizards".styled(LC) ~ " " ~ "deal an extra " ~ "hit".styled(styles.hit) ~ " attacking other " ~ "Lizards".styled(CM) ~ ".",
        "Discarded " ~ "cards".hh ~ " go to the " ~ "Lost Souls".hh ~ " " ~ "of the player" ~ " next in the turn order."
    )
}

case object HostileTakeover extends MirrorOption {
    val valueOn = "Hostile Takeover".hh
}

case object DoubleAgents extends MirrorOption {
    val valueOn = "Double Agents".hh

    override val explain = $(
        "Hidden " ~ "Plots".styled(CC) ~ " " ~ "deal double extra " ~ "hits".styled(styles.hit) ~ " to other " ~ "Crows".styled(RI) ~ "."
    )
}

case object OldBoys extends MirrorOption {
    val valueOn = "Old Boys".hh

    override val explain = $(
        "A " ~ "Minister".hh ~ " " ~ "can only be swayed by one " ~ "Underground".styled(UD) ~ " faction at a time.",
        "Swaying a " ~ "Minister".hh ~ " of another " ~ "Underground".styled(DR) ~ " faction costs one more " ~ "card".hh ~ ", " ~ "and doesn't provide victory points.",
        "Losing a " ~ "Minister".hh ~ " to enemy " ~ "swaying".hh ~ " has no negative consequences and leaves the " ~ "crown".hh ~ " available."
    )
}

case object PeerPressure extends MirrorOption {
    val valueOn = "Peer Pressure".hh

    override val explain = $(
        "Mob".styled(LH) ~ " immediately " ~ "converts".hh ~ " enemy " ~ "Rats".styled(LK) ~ " to their own if they are without " ~ "Warlord".styled(LK) ~ " or " ~ "Stronghold".styled(LK) ~ ".",
        "If " ~ "Mob".styled(LH) ~ " is placed in a clearing with another "  ~ "Mob".styled(LK) ~  "," ~ " both disperse, nobody scores anything."
    )
}

case object SharedHistory extends MirrorOption {
    val valueOn = "Shared History".hh
}






case object UntargetableKeep extends GameOption with ToggleOption {
    val group = MC.title
    val valueOn = "Keep".styled(MC) ~ SpacedDash ~ "Untargetable".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override def blocked(all : $[BaseOption]) = $($(IndestructibleKeep), $(ThreeHitKeep), $(InstantRebuildKeep))
    override val explain = $(
        "The " ~ Keep.of(MC) ~ " doesn't count as token," ~ HorizontalBreak ~ "it can't be removed from the map," ~ HorizontalBreak ~ " and it can't be assigned hits."
    )
}

case object IndestructibleKeep extends GameOption with ToggleOption {
    val group = MC.title
    val valueOn = "Keep".styled(MC) ~ SpacedDash ~ "Indestructible".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override def blocked(all : $[BaseOption]) = $($(UntargetableKeep), $(ThreeHitKeep), $(InstantRebuildKeep))
    override val explain = $(
        "The " ~ Keep.of(MC) ~ " can't be removed from the map," ~ HorizontalBreak ~ " although a " ~ "victory point".styled(styles.vp) ~ " is scored" ~ HorizontalBreak ~ "each time it would be removed."
    )
}

case object ThreeHitKeep extends GameOption with ToggleOption {
    val group = MC.title
    val valueOn = "Keep".styled(MC) ~ SpacedDash ~ "Takes ".hh ~ 3.hit
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override def blocked(all : $[BaseOption]) = $($(UntargetableKeep), $(IndestructibleKeep))
    override val explain = $(
        "The first "~ "two".hh ~ " times the " ~ Keep.of(MC) ~ " is " ~ "removed".styled(styles.hit) ~ " it " ~ "isn't".hl ~ "," ~ HorizontalBreak ~ "although a " ~ "victory point".styled(styles.vp) ~ " is scored" ~ HorizontalBreak ~ "each time it would be removed."
    )
}

case object InstantRebuildKeep extends GameOption with ToggleOption {
    val group = MC.title
    val valueOn = "Keep".styled(MC) ~ SpacedDash ~ "Instant Rebuild".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override def blocked(all : $[BaseOption]) = $($(UntargetableKeep), $(IndestructibleKeep))
    override val explain = $(
        "After the " ~ Keep.of(MC) ~ " is removed," ~ HorizontalBreak ~ &(&(MC.elem ~ " can immediately rebuild it ") ~ &("in another clearing she rules,") ~ " " ~ &("discarding a " ~ "matching card".hh ~ "."))
    )
}

case object SawmillMassOverwork extends GameOption with ToggleOption {
    val group = MC.title
    val valueOn = Sawmill.sof(MC) ~ SpacedDash ~ "Mass Overwork".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
}

case object WorkshopEveningCraft extends GameOption with ToggleOption {
    val group = MC.title
    val valueOn = Workshop.sof(MC) ~ SpacedDash ~ "Evening".styled(styles.phase) ~ " Craft".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
}

case object WorkshopActionBonuses extends GameOption with ToggleOption {
    val group = MC.title
    val valueOn = Workshop.sof(MC) ~ SpacedDash ~ "Action Bonuses".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
}

case object KeepExtraBuildingSlot extends GameOption with ToggleOption {
    val group = MC.title
    val valueOn = "Keep".styled(MC) ~ SpacedDash ~ "Extra Building Slot".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override val explain = $(
        "The clearing with the " ~ Keep.of(MC) ~ " has an extra building slot as long as it stands."
    )
}

case object Catapults extends GameOption with ToggleOption {
    val group = MC.title
    val valueOn = "Catapults".styled(MC) ~ SpacedDash ~ "Siege Tokens".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override val explain = $(
        "Spend " ~ 1.hl ~ " " ~ Wood.of(MC) ~ " to place a catapult token at a " ~ Workshop.of(MC) ~ ".",
        "When moving, can move " ~ 1.hl ~ " " ~ Catapult.of(MC) ~ " for every " ~ 4.hl ~ " " ~ Cat.sof(MC) ~ " that move.",
        "At the start of battle as the attacker, " ~ MC.elem ~ " may declare that they want to siege the defender.",
        "In that case, the number of rolled hit is limited by the number of " ~ Catapult.sof(MC) ~ "," ~ " and rolled hits can only be assigned to " ~ "tokens".hh ~ " and " ~ "buildings".hh ~ "."
    )
}


case object WarriorsOnlyInfamy extends GameOption with ToggleOption {
    val group = VB.title
    val valueOn = "Infamy".styled(VB) ~ SpacedDash ~ "Warriors Only".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        VB.elem ~ " scores " ~ "Infamy".styled(VB) ~ " only for killing " ~ "warriors".hh ~ "."
    )
}

case object LimitedInfamy extends GameOption with ToggleOption {
    val group = VB.title
    val valueOn = "Infamy".styled(VB) ~ SpacedDash ~ "Max".hh ~ " " ~ (1 + " VP").styled(styles.vp) ~ " per Battle".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        VB.elem ~ " scores " ~ "Infamy".styled(VB) ~ " only once per " ~ "enemy faction".hh ~ " per " ~ "battle".hh ~ "."
    )
}

case object RevisedQuests extends GameOption with ToggleOption {
    val group = VB.title
    val valueOn = "Quests".styled(VB) ~ SpacedDash ~ "Revised Quest Deck".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        VB.elem ~ " uses the " ~ Link("https://boardgamegeek.com/thread/2810553/revised-quest-deck", "Revised Quest Deck", $(xstyles.link)) ~ " by " ~ "Phoenix1147".hl ~ ".",
        "Six of the original quests have their item requirements changed to make questing more viable.",
        Image("quest:" + "errand-fox", styles.quest) ~ Image("quest:" + "errand-rabbit", styles.quest) ~ Image("quest:" + "escort-mouse", styles.quest) ~ Break ~
        Image("quest:" + "repair-a-shed-rabbit", styles.quest) ~ Image("quest:" + "fundraising-fox", styles.quest) ~ Image("quest:" + "escort-fox", styles.quest),
    )
    override val grow = xlo.grow4
}


trait VagabondCharOption {
    def charImage(character : Character, options : $[GameOption]) =
        convertDesc($(
            Image(character.title)(styles.chartitle),
            Break,
            Image(character.img)(styles.quest)((character == Ranger).?(styles.ranger))((character == Adventurer).?(styles.adventurer))((character == FolkHero).?(styles.folk)).div(styles.charblock) ~ character.starting(options)./(_.img).merge,
            Break,
            character.special./(_.of(VB)
        )))(null, null)(null).div(xstyles.xx)(xstyles.chp)(xstyles.chm)(xstyles.info)(styles.inline)(styles.charback)
}

case object TinkerNoBag extends GameOption with ToggleOption with VagabondCharOption {
    val group = VB.title
    val valueOn = Tinker.name.styled(VB) ~ SpacedDash ~ "No Starting ".hh ~ Bag.name.spn(styles.itemInfo)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        Tinker.name.styled(VB) ~ " starts without a " ~ Bag.elem ~ ".",
        charImage(Tinker, $(this))
    )
    override val grow = xlo.grow16
}

case object HarrierNoCoins extends GameOption with ToggleOption with VagabondCharOption {
    val group = VB.title
    val valueOn = Harrier.name.styled(VB) ~ SpacedDash ~ "No Starting ".hh ~ Coins.name.spn(styles.itemInfo)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        Harrier.name.styled(VB) ~ " starts without " ~ Coins.elem ~ ".",
        charImage(Harrier, $(this))
    )
}

case object ScoundrelTeapotNoBoot extends GameOption with ToggleOption with VagabondCharOption {
    val group = VB.title
    val valueOn = Scoundrel.name.styled(VB) ~ SpacedDash ~ Teapot.name.spn(styles.itemInfo) ~ " instead of ".hh ~ Boots.name.spn(styles.itemInfo)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        Scoundrel.name.styled(VB) ~ " starts with " ~ Teapot.elem ~ " instead of " ~ Boots.elem ~ ".",
        charImage(Scoundrel, $(this))
    )
}

case object ArbiterScoreCard extends GameOption with ToggleOption with VagabondCharOption {
    val group = VB.title
    val valueOn = Arbiter.name.styled(VB) ~ SpacedDash ~ Protector.of(VB) ~ " grants ".hh ~ "1 Card".styled(Bird)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        Arbiter.name.styled(VB) ~ " doesn't get a " ~ "victory point".styled(styles.vp) ~ " from his " ~ Protector.of(VB) ~ " abilitiy, draws a " ~ "card".hh ~ " instead.",
        charImage(Arbiter, $(this))
    )
}

case object RoninSwiftTorch extends GameOption with ToggleOption with VagabondCharOption {
    val group = VB.title
    val valueOn = Ronin.name.styled(VB) ~ SpacedDash ~ SwiftStrike.of(VB) ~ " with ".hh ~ Torch.name.spn(styles.itemInfo)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        Ronin.name.styled(VB) ~ " uses " ~ Torch.elem ~ " instead of the " ~ Sword.elem ~ " to trigger " ~ SwiftStrike.of(VB) ~ ".",
        charImage(Ronin, $(this))
    )
}

case object FolkHeroCharacter extends GameOption with ToggleOption with VagabondCharOption {
    val group = VB.title
    val valueOn = FolkHero.name.styled(VB) ~ " " ~ "character"
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        FolkHero.name.styled(VB) ~ SpacedDash ~ "character drawn by " ~ "Kyle Ferrin".hl ~ " and designed by " ~ "S.P. Shaman".hl ~ " in the loving memory of " ~ "t1mothy".styled(styles.get(MC))(xstyles.bold)(xstyles.larger125) ~ ".",
        "When aiding a " ~ "hostile".styled(styles.hit) ~ " faction," ~ " " ~ &("repairs".hh ~ " an item and changes their attitude to " ~ "indifferent".hh ~ "."),
        charImage(FolkHero, $(this))
    )
}


case object LimitedTrampleDiscard extends GameOption with ToggleOption {
    val group = LC.title
    val valueOn = "Trampled ".hh ~ "Gardens".styled(LC) ~ SpacedDash ~ "Discard Only ".hh ~ 1.styled(styles.hit) ~ " Card".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(LC)))
    override val explain = $(
        "If several ".hh ~ "Gardens".styled(LC) ~ " are removed at once, " ~ LC.elem ~ " discards only " ~ 1.hl ~ " card."
    )
}

case object RevengeFromAnyRemoval extends GameOption with ToggleOption {
    val group = LC.title
    val valueOn = "Revenge".styled(LC) ~ SpacedDash ~ "Warriors Removed Out of Turn".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(LC)))
    override val explain = $(
        "All " ~ "Lizards".styled(LC) ~ " removed out of turn become " ~ "Acolytes".hl ~ "," ~ HorizontalBreak ~ "not only those killed defending in battle."
    )
}

case object LizardHandSizeSix extends GameOption with ToggleOption {
    val group = LC.title
    val valueOn = "Hand".styled(LC) ~ SpacedDash ~ "Hand Size ".hh ~ "Six".styled(Bird) ~ " Cards".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(LC)))
    override val explain = $(
        LC.elem ~ " discards down to " ~ 6.styled(Bird) ~ " cards in the evening," ~ HorizontalBreak ~ "not to " ~ 5.styled(Bird) ~ " as all factions."
    )
}

case object ExportToCommitments extends GameOption with ToggleOption {
    val group = RF.title
    val valueOn = "Export".styled(RF) ~ SpacedDash ~ "Add to Commitments".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(RF)))
    override val explain = $(
        "Exported " ~ "funds".styled(RF) ~ " go straight to " ~ "Commitments".styled(RF) ~ ", not to " ~ "Payments".styled(RF) ~ "."
    )
}

case object MayorAssemblyAction extends GameOption with ToggleOption {
    val group = UD.title
    val valueOn = "Mayor".styled(UD) ~ SpacedDash ~ "Performs ".hh ~ "Assembly".styled(UD) ~ " Action".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(UD)))
    override val explain = $(
        "Mayor".styled(UD) ~ " " ~ "digs".hh ~ ", " ~ "builds".hh ~ ", " ~ "moves".hh ~ ", " ~ "battles".hh ~ " or " ~ "recruits".hh ~ Break ~ "instead of copying other ministers."
    )
}

case object ThreeOfEachPlot extends GameOption with ToggleOption {
    val group = CC.title
    val valueOn = "More Plots".styled(CC) ~ SpacedDash ~ "Three of each Plot".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(CC)), $(IncludeFaction(RI)))
    override val explain = $(
        CC.elem ~ " get " ~ "three".hh ~ " instances of each plot in supply," ~ HorizontalBreak ~ "not " ~ "two".hh ~ " as default."
    )
}

case object EmbeddedAgentsBeforeBattle extends GameOption with ToggleOption {
    val group = CC.title
    val valueOn = "Embedded Agents".styled(CC) ~ SpacedDash ~ 1.hit ~ " before Battle".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(CC)), $(IncludeFaction(RI)))
    override val explain = $(
        "Hidden plots deal a " ~ 1.hit ~ " before the roll in the battle," ~ HorizontalBreak ~ "like a half-" ~ "ambush".hh ~ " that can't be countered.",
        "Plots don't deal an extra " ~ 1.hit ~ " after the roll."
    )
}

case object DiversionPlot extends GameOption with ToggleOption {
    val group = CC.title
    val valueOn = "Diversion".styled(CC) ~ SpacedDash ~ "New Plot Type".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(CC)), $(IncludeFaction(RI)))
    override val explain = $(
        "Diversion".styled(CC) ~ SpacedDash ~ "a new type of plot available to " ~ CC.elem ~ ".",
        "This plot can't ever be " ~ "exposed".hh ~ "," ~ HorizontalBreak ~ "exposing always fails as if the plot was guessed wrong.",
        "But it can never be " ~ "flipped".hh ~ " and scored either."
    )
}

case object BrutalHonesty extends GameOption with ToggleOption {
    val group = CC.title
    val valueOn = "Brutal Honesty".hh ~ SpacedDash ~ "Better " ~ "Bombs".styled(CC)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(CC)), $(IncludeFaction(RI)))
    override val explain = $(
        "Bombs".styled(CC) ~ " can be plotted revealed.",
        ("In battle,").& ~ " " ~ (CC.elem ~ " can remove").& ~ " " ~ ("a hidden or revealed " ~ "Bomb".styled(CC)).& ~ " " ~ ("in lieu of playing an " ~ "Ambush".styled(Bird) ~ ",").& ~ " " ~ "both offensively and defensively."
    )
}

case object AssassinPlot extends GameOption with ToggleOption {
    val group = CC.title
    val valueOn = "New Plot Type".hh ~ SpacedDash ~ "Assassin".styled(CC)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(CC)), $(IncludeFaction(RI)))
    override val explain = $(
        "Assassin".styled(CC) ~ SpacedDash ~ "a new type of plot available to " ~ CC.elem,
        "On flip, either " ~ "place".hh ~ " a warrior in a matching clearing, or " ~ "remove".hh ~ " an enemy warrior in a matching clearing",
        "Afterwards, remove the plot token",
    )
}

case object ColumnBonusCardVP extends GameOption with ToggleOption {
    val group = KI.title
    val valueOn = "Completed Column".styled(KI) ~ SpacedDash ~ (1 + " VP").styled(styles.vp) ~ " and ".hh ~ "1 Card".styled(Bird)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(KI)))
    override val explain = $(
        "Instead of " ~ &("scoring " ~ 2.vp) ~ " for completing" ~ &(" a set of " ~ "Relics".hh ~ ",") ~ &("score " ~ 1.vp ~ " and " ~ &("draw " ~ "1 Card".styled(Bird) ~ "."))
    )
}

case object SeparateItemsInRuins extends GameOption with ToggleOption {
    val group = LH.title
    val valueOn = "Items in Ruins".styled(LH) ~ SpacedDash ~ "Own Set".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))./(_ ++ $(IncludeFaction(LH)))
    override val explain = $(
        "In games with " ~ LH.elem ~ " and " ~ VB.elem ~ " there are items in ruins for " ~ "both".hh ~ " factions."
    )
}

case object RandomCharacter extends GameOption with ToggleOption {
    val group = VB.title
    val valueOn = "Random Character".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
}


case object TotalWarDominance extends GameOption with ToggleOption {
    val group = "Dominance"
    val valueOn = "Total War".styled(styles.hit)
    override def decorate(elem : Elem) = Image("battle")(styles.token) ~ " " ~ elem ~ " " ~ Image("battle")(styles.token)
    override val explain = $(
        &("Any " ~ "faction".hh ~ " with activated " ~ "Dominance".hl) ~ &(" continues" ~ " to score " ~ &("victory points".styled(styles.vp) ~ ", ")) ~ &("only they" ~ " are immediately used."),
        &(&("After " ~ "any action".hh ~ " is complete") ~ " " ~ "during which that faction scored points,") ~ " " ~ &("even not on their turn,") ~ " " ~ &(&("that faction places a " ~ "warrior".hl) ~ " " ~  &("for each " ~ "point".styled(styles.vp)) ~ " " ~ &("anywhere with their pieces.")),
        &("Each time that faction " ~ "doesn't have " ~ &("a warrior in supply, ")) ~ &("it " ~ "battles".hl ~ " anywhere instead."),
        &("And when the faction " ~ &("recruits warriors " ~ &("through other means, "))) ~ &("but the supply runs out, ") ~ &(&("it scores " ~ 1.vp) ~ &(" for each warrior not recruited."))
    )
}


trait LandmarksOption extends GameOption {
    val group = "Landmarks"
}

trait Landmark extends LandmarksOption with ToggleOption with Record with Elementary {
    val name : String
    def valueOn = name.hh
    def elem = name.hl
}

case object MapDefaultLandmarks extends LandmarksOption {
    val valueOn = "Map Default".hh
    override def forcedOff(all : $[BaseOption]) = all.of[LandmarksOption].but(this).%(_.group == this.group)
}

case object NoLandmarks extends LandmarksOption {
    val valueOn = "None".hh
    override def forcedOff(all : $[BaseOption]) = all.of[LandmarksOption].but(this).%(_.group == this.group)
}

case object FerryLandmark extends Landmark {
    val name = "Ferry"
    override def forcedOff(all : $[BaseOption]) = $(MapDefaultLandmarks, NoLandmarks)
}

case object LostCityLandmark extends Landmark {
    val name = "Lost City"
    override def forcedOff(all : $[BaseOption]) = $(MapDefaultLandmarks, NoLandmarks)
}

case object TowerLandmark extends Landmark {
    val name = "Tower"
    override def forcedOff(all : $[BaseOption]) = $(MapDefaultLandmarks, NoLandmarks)
}



trait HirelingsOption extends GameOption with OneOfGroup {
    val group = "Hirelings"
}

case object NoHirelings extends HirelingsOption {
    val valueOn = "None".hh
}

case object ThreeDemotedHirelings extends HirelingsOption {
    val valueOn = "Demoted".hh ~ ", " ~ "Demoted".hh ~ ", " ~ "Demoted".hh
}

case object TwoDemotedOneNormalHirelings extends HirelingsOption {
    val valueOn = "Demoted".hh ~ ", " ~ "Demoted".hh ~ ", " ~ "Staunch".styled(styles.hit)
}

case object OneDemotedTwoNormalHirelings extends HirelingsOption {
    val valueOn = "Demoted".hh ~ ", " ~ "Staunch".styled(styles.hit) ~ ", " ~ "Staunch".styled(styles.hit)
}

case object ThreeNormalHirelings extends HirelingsOption {
    val valueOn = "Staunch".styled(styles.hit) ~ ", " ~ "Staunch".styled(styles.hit) ~ ", " ~ "Staunch".styled(styles.hit)
}

case object ThreeRandomHirelings extends HirelingsOption {
    val valueOn = "Random".styled(StreetBand) ~ ", " ~ "Random".styled(StoicProtector) ~ ", " ~ "Random".styled(BanditGangs)
}

case object SelectedHirelings extends HirelingsOption {
    val valueOn = "All Selected".hh
    override val explain = $(
        &("Any number of " ~ &("Hirelings".hh ~ " selected")) ~ " will be available in the game," ~ Break ~ &("with hiring threshold " ~ &("every " ~ 4.vp ~ ".")),
        &("Some " ~ "Hirelings".hh ~ " can be dropped") ~ " if " ~ &("Faction".hh ~ "/" ~ "Hireling".hh ~ " clashing") ~ " is disallowed if that would leave" ~ &(" too few factions to draft.")
    )
}


trait HirelingsClashOption extends GameOption with ToggleOption {
    val group = "Hirelings Clash"
}

case object HirelingHirelingClash extends HirelingsClashOption {
    val valueOn = "Allow Hireling".hh ~ "/" ~ "Hireling Clash".hh
    override def required(all : $[BaseOption]) = all.of[IncludeHireling]./~(o => all.of[IncludeHireling].%(_.hireling.clashKey == o.hireling.clashKey)./(h => $(o, h)))
    override val explain = $(
        "Both " ~ "Staunch".styled(styles.hit) ~ " and " ~ "Demoted".hh ~ " versions" ~ &(" of the same Hireling" ~ &(" can be drawn."))
    )
}

case object FactionHirelingClash extends HirelingsClashOption {
    val valueOn = "Allow Faction".hh ~ "/" ~ "Hireling Clash".hh
    override def required(all : $[BaseOption]) = all.of[IncludeFaction]./~(o => all.of[IncludeHireling].%(_.hireling.clashKey == o.faction.clashKey)./(h => $(o, h)))
    override val explain = $(
        "Both a faction and" ~ &(" a matching hireling ") ~ "can be drawn."
    )
}


case class IncludeHireling(hireling : Hireling) extends GameOption with ToggleOption {
    val group = hireling.demoted.?("Demoted".hh).|("Staunch".styled(styles.hit)) ~ " Hirelings"
    val valueOn = hireling.name.styled(hireling)
    override def required(all : $[BaseOption]) =
        if (hireling.demoted)
            $($(ThreeDemotedHirelings), $(TwoDemotedOneNormalHirelings), $(OneDemotedTwoNormalHirelings), $(ThreeRandomHirelings), $(SelectedHirelings))
        else
            $($(TwoDemotedOneNormalHirelings), $(OneDemotedTwoNormalHirelings), $(ThreeNormalHirelings), $(ThreeRandomHirelings), $(SelectedHirelings))
}


trait AssignHitsOption extends hrf.Setting with OneOfGroup {
    val group = "Assign Hits"
}

case object PromptAssignHits extends AssignHitsOption {
    val valueOn = "Always Prompt".hlb
}

case object AutoAssignHits extends AssignHitsOption {
    val valueOn = "Auto Assign If No Choice".hlb ~ " (Only When Online)".hl
}


trait AmbushOption extends hrf.Setting with OneOfGroup {
    val group = "Ambush!"
}

case object PromptAmbush extends AmbushOption {
    val valueOn = "Always Prompt".hlb
}

case object AutoAmbush extends AmbushOption {
    val valueOn = "Auto Skip If None".hlb ~ " (Only When Online)".hl
}


trait OutOfTurnOption extends hrf.Setting with OneOfGroup {
    val group = "Out of Turn Actions"
}

case object NoOutOfTurn extends OutOfTurnOption {
    val valueOn = "None".hlb
}

case object BasicOutOfTurn extends OutOfTurnOption {
    val valueOn = "Basic".hlb
}

case object DetailedOutOfTurn extends OutOfTurnOption {
    val valueOn = "Detailed".hlb
}


trait RuleOption extends hrf.Setting with OneOfGroup {
    val group = "Clearing Rule"
}

case object ShowRule extends RuleOption {
    val valueOn = "Show".hlb
}

case object HideRule extends RuleOption {
    val valueOn = "Hide".hlb
}


trait HighlightsOption extends hrf.Setting with OneOfGroup {
    val group = "Highlights"
}

case object HighlightNone extends HighlightsOption {
    val valueOn = "None".hlb
}

case object HighlightClearings extends HighlightsOption {
    val valueOn = "Clearings".hlb
}

trait Switch extends GameOption {
    val group = ""
}

case object UseAutoAssignHits extends Switch {
    val valueOn = "Auto " ~ "Assign Hits".hl ~ " in " ~ "Battle".hl ~ " (If No Choice)"
}

case class SkipAmbushSuit(suit : BaseSuit) extends Switch {
    val valueOn = "Skip " ~ "Ambush".hl ~ " in " ~ suit.elem
}

case class SkipFieldHospitalsSuit(suit : BaseSuit) extends Switch {
    val valueOn = "Skip " ~ "Field Hospitals".hl ~ " in " ~ suit.elem
}

case class SkipPartisansSuit(suit : BaseSuit) extends Switch {
    val valueOn = "Skip " ~ "Partisans".hl ~ " in " ~ suit.elem
}

case class UsePartisansSuit(suit : BaseSuit) extends Switch {
    val valueOn = "Use " ~ "Partisans".hl ~ " in " ~ suit.elem
}

case class SkipFieldHospitalsCount(n : Int) extends Switch {
    val valueOn = "Skip " ~ "Field Hospitals".hl ~ " for " ~ 1.hlb ~ " Warrior"
}

case class PickFactionIfAvailable(f : PlayChoice) extends Switch {
    val valueOn = "Pick " ~ f.elem ~ " If Available"
}




object Meta extends MetaGame {
    val gaming = root.gaming

    type F = Faction

    def tagF = implicitly

    val name = "root"
    val label = "Root"

    override def settingsList = super.settingsList ++ $(PromptAssignHits, AutoAssignHits, PromptAmbush, AutoAmbush, ShowRule, HideRule, NoOutOfTurn, BasicOutOfTurn, DetailedOutOfTurn, HighlightClearings, HighlightNone)
    override def settingsDefaults = super.settingsDefaults ++ $(PromptAssignHits, PromptAmbush, ShowRule, NoOutOfTurn, HighlightNone)

    override val about = $(
        "A quaint and quirky adaptation of the " ~ "R".styled(Fox).larger ~ "O".styled(Rabbit).larger ~ "O".styled(Mouse).larger ~ "T".styled(Bird).larger ~ " board game.",
        Gap,
        Gap,
        Gap,
        Gap,
        Gap,
        "It was a dark and stormy night in " ~ "2019".styled(Bird) ~ ".",
        "Just having finished my first play of the game, I knew", "something was wrong.",
        "It was obvious the game needed a thousand plays to shine.",
        "No way my friends were ready for such an ordeal.",
        "There was only one way...",
        Gap,
        Gap,
        Gap,
        Gap,
        Gap,
        "Three weeks later, the first version of the app with bots was ready.",
        "Written in " ~ "Scala".styled(Fox) ~ ", with too much implicits.",
        "Made public in " ~ "2020".styled(Bird) ~ " during the lockdown.",
        "Some of my friends were up for a ride after all.",
        "We played most every " ~ "Sunday".styled(Rabbit) ~ " ever since.",
        Gap,
        Gap,
        Gap,
        Gap,
        Gap,
        "Thanks to " ~ "Cole Wehrle".styled(Bird) ~ " and " ~ "Leder Games".styled(Bird) ~ " for developing",
        "such an immersive game.",
        "Thanks to " ~ "my friends".styled(Fox) ~ " for putting up with so many",
        "gaming sessions turned debuggїng sessions.",
        "Thanks to " ~ "the people".styled(Mouse) ~ " on " ~ &(Link("https://discord.gg/[REDACTED]", "Root Jeu en Ligne", $(xstyles.link))) ~ ", " ~
        &(Link("https://discord.gg/woodland-warriors-476234833572397056", "Woodland Warriors", $(xstyles.link))) ~ " and " ~
        &(Link("https://boardgamegeek.com/boardgame/237182/root/forums", "Root BGG forum", $(xstyles.link))).hh ~ " for support, suggestions and ideas.",
        "Thanks to everyone making fan content.",
        Gap,
        Gap,
        Gap,
        Gap,
        Gap,
        "Sincerely yours " ~ "Haunt Roll Fail".hl,
    )

    val official = $[Faction](MC, ED, WA, VB, LC, RF, UD, CC, LH, KI)
    val clones = $[Faction](BK, PE, FU, NB, CM, DR, RI, LK)
    val fun = $[Faction](KDvA, TCvA, LDvD, LDvC, LDvB, TD, FH, XC, CUv2, CU, OK, AF, SF, MB)

    val factions = official ++ clones ++ fun

    override def factionGroup(f : F) : |[Elem] = clones.has(f).?("Mirror Factions".txt) || fun.has(f).?("Fun Factions".txt)

    val minPlayers = 2

    val hirelings = $(StreetBand, StoicProtector, BanditGangs, MoleArtisians, RatSmugglers, ForestPatrol, LastDynasty, SpringUprising, TheExile, RiverfolkFlotilla)

    override def optionPages(n : Int, l : $[F]) : $[$[GameOption]] = $(
        hiddenOptions ++
        $(AutoHitsAssignmentMode, AutoAmbushSkippingMode, ForcedAsyncMode) ++
        $(SetupTypeCorners, SetupTypeHomelands) ++
        (n > 2).?(TotalWarDominance) ++
        $,
        $(AutumnMap, WinterMap, LakeMap, MountainMap, TidalMap, TundraMap, GloomMap) ++
        $(DefaultClearings, NoClustersClearings, AllRandomClearings, SuitPairsClearings, ConnectedClearings, ThreeFourFiveClearings) ++
        $(MapDefaultLandmarks, NoLandmarks, FerryLandmark, LostCityLandmark, TowerLandmark) ++
        $,
        $(StandardDeck, ExilesDeck, MixedDeck, DuskDeck) ++
        $(NonBirdPartisans, UnthematicCoffinMakers, UnthematicPropagandaBureau) ++
        l.of[Mischief].any.$(TunnelsIgnoreRaid) ++
        l.of[Trader].any.$(TunnelsIgnoreTradePosts) ++
        $,
        (l.of[Feline].num > 1).$(TheWoodMustFlow) ++
        (l.of[Aviary].num > 1).$(OneTrueEmpire) ++
        (l.of[Insurgent].num > 1).$(Imprinting) ++
        (l.of[Hero].num > 1).$(RootingForTheUnderdog) ++
        (l.of[Fanatic].num > 1).$(DeathToTheInfidels) ++
        (l.of[Trader].num > 1).$(HostileTakeover) ++
        (l.of[Mischief].num > 1).$(DoubleAgents) ++
        (l.of[Underground].num > 1).$(OldBoys) ++
        (l.of[Horde].num > 1).$(PeerPressure) ++
        (l.of[Expedition].num > 1).$(SharedHistory) ++
        (l.of[Feline].any || l.of[Fanatic].any || l.of[Mischief].any).$(AdSetBuffOn, AdSetBuffOff) ++
        l.of[Feline].any.$(UntargetableKeep, IndestructibleKeep, ThreeHitKeep, InstantRebuildKeep, KeepExtraBuildingSlot, SawmillMassOverwork, WorkshopEveningCraft, WorkshopActionBonuses, Catapults) ++
        l.of[Hero].any.$(RandomCharacter, FolkHeroCharacter) ++
        l.of[Hero].any.$(WarriorsOnlyInfamy, LimitedInfamy, RevisedQuests) ++
        l.of[Hero].any.$(TinkerNoBag, HarrierNoCoins, ScoundrelTeapotNoBoot, ArbiterScoreCard, RoninSwiftTorch) ++
        l.of[Fanatic].any.$(LimitedTrampleDiscard, RevengeFromAnyRemoval, LizardHandSizeSix) ++
        l.of[Trader].any.$(ExportToCommitments) ++
        l.of[Underground].any.$(MayorAssemblyAction) ++
        l.of[Mischief].any.$(ThreeOfEachPlot, EmbeddedAgentsBeforeBattle, DiversionPlot, BrutalHonesty) ++
        l.of[Expedition].any.$(ColumnBonusCardVP) ++
        l.of[Horde].any.$(SeparateItemsInRuins) ++
        $,
        $(NoHirelings, ThreeDemotedHirelings, TwoDemotedOneNormalHirelings, OneDemotedTwoNormalHirelings, ThreeNormalHirelings, ThreeRandomHirelings, SelectedHirelings) ++
        $(HirelingHirelingClash, FactionHirelingClash) ++
        hirelings./(IncludeHireling) ++
        $,
    )

    override def optionsFor(n : Int, l : $[F]) =
        hiddenOptions ++
        $(AutumnMap, WinterMap, LakeMap, MountainMap, TidalMap, TundraMap, GloomMap) ++
        $(DefaultClearings, NoClustersClearings, AllRandomClearings, SuitPairsClearings, ConnectedClearings, ThreeFourFiveClearings) ++
        $(SetupTypeCorners, SetupTypeHomelands) ++
        $(StandardDeck, ExilesDeck, MixedDeck, DuskDeck) ++
        $(NonBirdPartisans, UnthematicCoffinMakers, UnthematicPropagandaBureau) ++
        l.of[Mischief].any.$(TunnelsIgnoreRaid) ++
        l.of[Trader].any.$(TunnelsIgnoreTradePosts) ++
        $(AutoHitsAssignmentMode, AutoAmbushSkippingMode, ForcedAsyncMode) ++
        (n > 2).?(TotalWarDominance) ++
        (l.of[Feline].num > 1).$(TheWoodMustFlow) ++
        (l.of[Aviary].num > 1).$(OneTrueEmpire) ++
        (l.of[Insurgent].num > 1).$(Imprinting) ++
        (l.of[Hero].num > 1).$(RootingForTheUnderdog) ++
        (l.of[Fanatic].num > 1).$(DeathToTheInfidels) ++
        (l.of[Trader].num > 1).$(HostileTakeover) ++
        (l.of[Mischief].num > 1).$(DoubleAgents) ++
        (l.of[Underground].num > 1).$(OldBoys) ++
        (l.of[Horde].num > 1).$(PeerPressure) ++
        (l.of[Expedition].num > 1).$(SharedHistory) ++
        (l.of[Feline].any || l.of[Fanatic].any || l.of[Mischief].any).$(AdSetBuffOn, AdSetBuffOff) ++
        l.of[Feline].any.$(UntargetableKeep, IndestructibleKeep, ThreeHitKeep, InstantRebuildKeep, KeepExtraBuildingSlot, SawmillMassOverwork, WorkshopEveningCraft, WorkshopActionBonuses, Catapults) ++
        l.of[Hero].any.$(RandomCharacter, FolkHeroCharacter) ++
        l.of[Hero].any.$(WarriorsOnlyInfamy, LimitedInfamy, RevisedQuests) ++
        l.of[Hero].any.$(TinkerNoBag, HarrierNoCoins, ScoundrelTeapotNoBoot, ArbiterScoreCard, RoninSwiftTorch) ++
        l.of[Fanatic].any.$(LimitedTrampleDiscard, RevengeFromAnyRemoval, LizardHandSizeSix) ++
        l.of[Trader].any.$(ExportToCommitments) ++
        l.of[Underground].any.$(MayorAssemblyAction) ++
        l.of[Mischief].any.$(ThreeOfEachPlot, EmbeddedAgentsBeforeBattle, DiversionPlot, BrutalHonesty) ++
        l.of[Expedition].any.$(ColumnBonusCardVP) ++
        l.of[Horde].any.$(SeparateItemsInRuins) ++
        $(MapDefaultLandmarks, NoLandmarks, FerryLandmark, LostCityLandmark, TowerLandmark) ++
        $(NoHirelings, ThreeDemotedHirelings, TwoDemotedOneNormalHirelings, OneDemotedTwoNormalHirelings, ThreeNormalHirelings, ThreeRandomHirelings, SelectedHirelings) ++
        $(HirelingHirelingClash, FactionHirelingClash) ++
        hirelings./(IncludeHireling)

    override val hiddenOptions =
        factions./(IncludeFaction) ++
        $(SeatingGiven, SeatingRandom) ++
        $(FactionSeatingGiven, FactionSeatingRandom) ++
        $(SetupOrderPriority, SetupOrderReverse) ++
        $(CardDraftStandard, CardDraftFive)

    val options = optionsFor(4, factions) ++ hiddenOptions ++ factions./(IncludeFaction)

    override def defaultsFor(n : Int, l : $[F]) = $(
        SeatingGiven,
        FactionSeatingGiven,
        AutumnMap,
        DefaultClearings, NoClustersClearings,
        MixedDeck,
        AutoHitsAssignmentMode,
        FolkHeroCharacter,
        AdSetBuffOn,
        SetupTypeCorners,
        SetupOrderPriority,
        CardDraftStandard,
        MapDefaultLandmarks,
        NoHirelings
    ) ++ l./(IncludeFaction) ++ hirelings./(IncludeHireling)

    override def presetsFor(n : Int, l : $[F]) = $(
        ("Reset Options".spn, hirelings./(IncludeHireling), $),
        ("Official".hl ~ " Rules", $(
            AllRandomClearings,
            StandardDeck,
            NonBirdPartisans,
            UnthematicCoffinMakers,
            UnthematicPropagandaBureau,
            TunnelsIgnoreRaid,
            TunnelsIgnoreTradePosts,
            AutoHitsAssignmentMode,
            AdSetBuffOff
        ), $),
        ("Official".hl ~ " | " ~ "Hirelings".styled(BanditGangs), $(
            AllRandomClearings,
            StandardDeck,
            NonBirdPartisans,
            UnthematicCoffinMakers,
            UnthematicPropagandaBureau,
            TunnelsIgnoreRaid,
            TunnelsIgnoreTradePosts,
            AutoHitsAssignmentMode,
            AdSetBuffOff
        ) :+ (n @@ {
            case 2 => ThreeNormalHirelings
            case 3 => OneDemotedTwoNormalHirelings
            case 4 => TwoDemotedOneNormalHirelings
            case _ => ThreeDemotedHirelings
        }), $),
        ("Sunday".styled(Rabbit) ~ " Games", $(
            RandomCharacter,
            WarriorsOnlyInfamy, RevisedQuests,
            TinkerNoBag, HarrierNoCoins, ScoundrelTeapotNoBoot, ArbiterScoreCard, RoninSwiftTorch,
            LizardHandSizeSix,
            ExportToCommitments,
            ThreeOfEachPlot, DiversionPlot, BrutalHonesty,
            MayorAssemblyAction,
            InstantRebuildKeep,
            TotalWarDominance,
            SetupTypeHomelands,
            ThreeRandomHirelings,
            IncludeHireling(StreetBand),
            IncludeHireling(RatSmugglers),
            IncludeHireling(ForestPatrol),
            IncludeHireling(SpringUprising),
            IncludeHireling(RiverfolkFlotilla),
        ), hirelings./(IncludeHireling))
    )

    val quickMin = 4
    val quickMax = 4

    override val quickFactions = official ++ $(XC) ++ $(CUv2) ++ $(FH)

    def randomGameName() = {
        val n = $("Power", "Victory", "Glory", "Destiny", "Might", "Fight", "Right", "Betrayal", "Wood", "Land", "Air", "Ground").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def reach(factions : $[Faction]) =
        factions.of[Feline].num * 10 +
        factions.of[Aviary].num * 7 +
        factions.of[Insurgent].num * 3 +
        factions.of[Trader].num * 5 +
        factions.of[Fanatic].num * 2 +
        factions.of[Underground].num * 8 +
        factions.of[Mischief].num * 3 +
        factions.of[Expedition].num * 8 +
        factions.of[Horde].num * 9 +
        (0 :: 5 :: 7 :: 9)(factions.of[Hero].num)

    def validateFactionCombination(factions : $[Faction]) = {
        if (factions.num < 2)
            ErrorResult("Select at least two factions")
        else
        if (factions.num > 8)
            ErrorResult("Max eight factions")
        else
        if (reach(factions) < factions.num.of(0 :: 0 :: 17 :: 18 :: 21 :: 25 :: 28 :: 28 :: 28))
            WarningResult("Reach " + reach(factions))
        else
            InfoResult("Reach " + reach(factions))
    }

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = {
        val ff = factions.of[Hero]./(h => (factions.dropWhile(_ != h) ++ factions.takeWhile(_ != h)).drop(1).takeWhile(_.is[Hero].not).num)

        validateFactionCombination(factions) && (
            if (factions.of[Feline].num + factions.of[Aviary].num + factions.of[Fanatic].num + factions.of[Underground].num + factions.of[Horde].num > 4 && options.has(SetupTypeCorners))
                ErrorResult("Too many corner factions")
            else
            if (ff.maxOr(0) - ff.minOr(0) > 1)
                WarningResult("Vagabonds seated unevenly")
            else
                InfoResult("")
        )
    }

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.elem
    override def factionNote(f : Faction) : Elem = f.note

    override def glyph(g : G) : |[String] = g.current.but(Neutral)./(_.style + "-glyph")
    override def glyph(f : F) : |[String] = |(f.style + "-glyph")
    override def glyph(g : G, f : F) : |[String] = glyph(f).%!(_ => g.highlightFaction.has(f) && hrf.HRF.uptime() / 1000 % 2 == 1)

    def createGame(factions : $[Faction], options : $[O]) = new Game(factions, factions, options)

    def getBots(f : Faction) = f match {
        case _ : Feline => $("Easy")
        case _ : Aviary => $("Easy")
        case _ : Insurgent => $("Easy")
        case _ : Hero => $("Easy")
        case _ => $("None")
    }

    override def defaultBots = $("Easy", "None")

    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Faction, "Easy") => new BotXX(f)
        case (f : Faction, "None") => new BotXX(f)
        case (f : Faction, _) => new BotXX(f)
        case (f : Faction, "Normal") => new BotTT(f, 1, 6, 3, o => new BotXX(o))
    }

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) = (factions ++ hirelings).%(_.short == s).single || (s == "XCMDM").?(Mudmen(XC)) || hirelings.%(_.toString == s).single

    def writeOption(o : O) = Serialize.write(o)
    def parseOption(s : String) = $(options.find(o => writeOption(o) == s) || options.find(o => o.toString == s) | (s @@ {
        case "AllHirelings" => SelectedHirelings
        case _ => UnknownOption(s)
    }))

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    override def bodyFont = Some("luminari")

    val assets =
    ConditionalAssetsList((factions, options) => true, "transport")(
        ImageAsset("riverboat"            ) ::
        ImageAsset("swimmers"             ) ::
        ImageAsset("tunnel"               ) ::
        ImageAsset("balloon"              ) ::
        ImageAsset("forest"               ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "feature")(
        ImageAsset("ruins"                ) ::
        ImageAsset("ferry"                ) ::
        ImageAsset("tower"                ) ::
        ImageAsset("lost-city"            ) ::


        ImageAsset("clearing-highlight-placement" ) ::
        ImageAsset("clearing-highlight-battle"    ) ::
        ImageAsset("clearing-highlight-nuke"      ) ::

        ImageAsset("clearing-suit-fox"    ) ::
        ImageAsset("clearing-suit-rabbit" ) ::
        ImageAsset("clearing-suit-mouse"  ) ::
        ImageAsset("clearing-suit-frog"   ) ::

        ImageAsset("clearing-suit-fox-off"    ) ::
        ImageAsset("clearing-suit-rabbit-off" ) ::
        ImageAsset("clearing-suit-mouse-off"  ) ::

        ImageAsset("scorched-earth-1"  ) ::
        ImageAsset("scorched-earth-2"  ) ::
        ImageAsset("scorched-earth-3"  ) ::
        ImageAsset("scorched-earth-4"  ) ::
        ImageAsset("scorched-earth-5"  ) ::
        ImageAsset("scorched-earth-6"  ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "feature/building")(
        ImageAsset("cross"                ) ::
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
    $) ::
    ConditionalAssetsList((factions, options) => true, "feature/token")(
        ImageAsset("empty-token"          ) ::
        ImageAsset("empty-token-card"     ) ::
        ImageAsset("empty-token-1"        ) ::
        ImageAsset("empty-token-2"        ) ::
        ImageAsset("empty-token-3"        ) ::
        ImageAsset("empty-token-4"        ) ::
        ImageAsset("empty-token-5"        ) ::
        ImageAsset("empty-token-2-card"   ) ::
        ImageAsset("empty-token-cost-1"   ) ::
        ImageAsset("empty-token-cost-2"   ) ::
        ImageAsset("empty-token-cost-3"   ) ::
        ImageAsset("empty-token-fox"      ) ::
        ImageAsset("empty-token-rabbit"   ) ::
        ImageAsset("empty-token-mouse"    ) ::
        ImageAsset("empty-token-bird"     ) ::
        ImageAsset("empty-token-anysuit"  ) ::
        ImageAsset("empty-token-militant"      ) ::
        ImageAsset("empty-token-militant-card" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "feature/text", "text-tint:")(
        ImageAsset("fox") ::
        ImageAsset("rabbit") ::
        ImageAsset("mouse") ::
        ImageAsset("frog") ::
        ImageAsset("fox-frog") ::
        ImageAsset("rabbit-frog") ::
        ImageAsset("mouse-frog") ::
        ImageAsset("fox-rabbit-mouse") ::
        ImageAsset("fox-rabbit-mouse-frog") ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "icon/craft")(
        ImageAsset("craft-suit-anysuit"  ) ::
        ImageAsset("craft-suit-fox"      ) ::
        ImageAsset("craft-suit-rabbit"   ) ::
        ImageAsset("craft-suit-mouse"    ) ::
        ImageAsset("craft-suit-frog"     ) ::
        ImageAsset("craft-suit-fox-frog"      ) ::
        ImageAsset("craft-suit-rabbit-frog"   ) ::
        ImageAsset("craft-suit-mouse-frog"    ) ::
        ImageAsset("craft-suit-fox-rabbit-mouse"       ) ::
        ImageAsset("craft-suit-fox-rabbit-mouse-frog"  ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "icon/card")(
        ImageAsset("card-back"             ) ::
        ImageAsset("card-back-frog"        ) ::
        ImageAsset("card-empty"            ) ::
        ImageAsset("card-separator"        ) ::

        ImageAsset("card-suit-bird-gray"    ) ::
        ImageAsset("card-suit-fox-gray"     ) ::
        ImageAsset("card-suit-rabbit-gray"  ) ::
        ImageAsset("card-suit-mouse-gray"   ) ::
        ImageAsset("card-suit-frog-gray"    ) ::
        ImageAsset("card-suit-anysuit-gray" ) ::

        ImageAsset("deck" ) ::
        ImageAsset("deck-frog" ) ::

        ImageAsset("pile-empty"  ) ::
        ImageAsset("pile-bird"   ) ::
        ImageAsset("pile-fox"    ) ::
        ImageAsset("pile-rabbit" ) ::
        ImageAsset("pile-mouse"  ) ::
        ImageAsset("pile-frog"   ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "faction")(
        ImageAsset("action-black" ) ::
        ImageAsset("action-bird" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "item")(
        ImageAsset("item-x-spacer"     ) ::
        ImageAsset("item-x-placeholder" ) ::
        ImageAsset("item-x-placeholder-0p" ) ::
        ImageAsset("item-x-placeholder-1p" ) ::
        ImageAsset("item-x-placeholder-2p" ) ::
        ImageAsset("item-x-placeholder-3p" ) ::
        ImageAsset("item-x-placeholder-4p" ) ::

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

        ImageAsset("item-club"                      ) ::
        ImageAsset("item-club-exhausted"            ) ::
        ImageAsset("item-club-damaged"              ) ::
        ImageAsset("item-club-damaged-exhausted"    ) ::
        ImageAsset("item-club-empty"    ) ::

        ImageAsset("item-any"         ) ::
        ImageAsset("item-any-damaged" ) ::

    $) ::
    ConditionalAssetsList((factions, options) => true, "icon")(
        ImageAsset("swap"    ) ::
        ImageAsset("move"    ) ::
        ImageAsset("battle"  ) ::
        ImageAsset("remove"  ) ::
        ImageAsset("income"  ) ::
        ImageAsset("current"  ) ::

        ImageAsset("cog") ::

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
    $) ::
    ConditionalAssetsList((factions, options) => true, "card")(
        ImageAsset("card-back-art") ::

    $) ::
    ConditionalAssetsList((factions, options) => true, "faction/hireling")(
        ImageAsset("stp-deer"              ) ::
        ImageAsset("stp-deer-empty"        ) ::

        ImageAsset("stb-weasel"            ) ::
        ImageAsset("stb-weasel-playing"    ) ::
        ImageAsset("stb-weasel-empty"      ) ::

        ImageAsset("bdg-porcupine"         ) ::
        ImageAsset("bdg-porcupine-empty"   ) ::

        ImageAsset("plb-rock-star"         ) ::
        ImageAsset("plb-rock-star-playing"   ) ::
        ImageAsset("plb-rock-star-empty"  , "stb-weasel-empty"     ) ::

        ImageAsset("fpr-elk"               ) ::

        ImageAsset("hwb-bandit"            ) ::

        ImageAsset("frp-stray-cat"         ) ::
        ImageAsset("frp-stray-cat-empty"   ) ::

        ImageAsset("ldn-vulture"           ) ::
        ImageAsset("ldn-vulture-empty"     ) ::

        ImageAsset("sup-animal"            ) ::
        ImageAsset("sup-animal-empty"      ) ::

        ImageAsset("exl-bear"              ) ::

        ImageAsset("rff-flotilla"          ) ::

        ImageAsset("wsp-chameleon"         ) ::
        ImageAsset("wsp-chameleon-empty"   ) ::

        ImageAsset("csp-magpie"            ) ::
        ImageAsset("csp-magpie-empty"      ) ::

        ImageAsset("swe-mouldwarp"         ) ::
        ImageAsset("swe-mouldwarp-empty"   ) ::
        ImageAsset("swe-foothold"          ) ::

        ImageAsset("vlk-vole"              ) ::
        ImageAsset("vlk-vole-empty"        ) ::
        ImageAsset("vlk-vault"             ) ::

        ImageAsset("fmb-varmint"           ) ::
        ImageAsset("fmb-varmint-empty"     ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(AutumnMap), "autumn", "autumn:")(
        ImageAsset("map"          ,    "map-bright-new-expand" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-beach"           ) ::
        ImageAsset("clearing-name-creek"           ) ::
        ImageAsset("clearing-name-dune"            ) ::
        ImageAsset("clearing-name-glade"           ) ::
        ImageAsset("clearing-name-haven"           ) ::
        ImageAsset("clearing-name-hill"            ) ::
        ImageAsset("clearing-name-meadow"          ) ::
        ImageAsset("clearing-name-mountain"        ) ::
        ImageAsset("clearing-name-pond"            ) ::
        ImageAsset("clearing-name-quarry"          ) ::
        ImageAsset("clearing-name-waterfall"       ) ::
        ImageAsset("clearing-name-weald"           ) ::

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
    $) ::
    ConditionalAssetsList((factions, options) => options.has(WinterMap), "winter", "winter:")(
        ImageAsset("map"          ,    "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-black") ::

        ImageAsset("clearing-name-bend"         ) ::
        ImageAsset("clearing-name-dale"         ) ::
        ImageAsset("clearing-name-drift"        ) ::
        ImageAsset("clearing-name-ford"         ) ::
        ImageAsset("clearing-name-hedge"        ) ::
        ImageAsset("clearing-name-moor"         ) ::
        ImageAsset("clearing-name-mound"        ) ::
        ImageAsset("clearing-name-pit"          ) ::
        ImageAsset("clearing-name-rock"         ) ::
        ImageAsset("clearing-name-spire"        ) ::
        ImageAsset("clearing-name-trench"       ) ::
        ImageAsset("clearing-name-wade"         ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(LakeMap), "lake", "lake:")(
        ImageAsset("map"          ,    "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-alley"   ) ::
        ImageAsset("clearing-name-bay"     ) ::
        ImageAsset("clearing-name-den"     ) ::
        ImageAsset("clearing-name-grove"   ) ::
        ImageAsset("clearing-name-gulf"    ) ::
        ImageAsset("clearing-name-lawn"    ) ::
        ImageAsset("clearing-name-marsh"   ) ::
        ImageAsset("clearing-name-prairie" ) ::
        ImageAsset("clearing-name-shade"   ) ::
        ImageAsset("clearing-name-shoal"   ) ::
        ImageAsset("clearing-name-vert"    ) ::
        ImageAsset("clearing-name-yard"    ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(MountainMap), "mountain", "mountain:")(
        ImageAsset("map"          ,    "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-brim"       ) ::
        ImageAsset("clearing-name-cliff"      ) ::
        ImageAsset("clearing-name-crest"      ) ::
        ImageAsset("clearing-name-drain"      ) ::
        ImageAsset("clearing-name-ledge"      ) ::
        ImageAsset("clearing-name-mine"       ) ::
        ImageAsset("clearing-name-pass"       ) ::
        ImageAsset("clearing-name-peak"       ) ::
        ImageAsset("clearing-name-ramp"       ) ::
        ImageAsset("clearing-name-ridge"      ) ::
        ImageAsset("clearing-name-slope"      ) ::
        ImageAsset("clearing-name-valley"     ) ::

        ImageAsset("rubble-mine-peak"  ) ::
        ImageAsset("rubble-ledge-mine"  ) ::
        ImageAsset("rubble-brim-ledge"  ) ::
        ImageAsset("rubble-ramp-valley"  ) ::
        ImageAsset("rubble-valley-ridge"  ) ::
        ImageAsset("rubble-cliff-crest"  ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(TidalMap), "tidal", "tidal:")(
        ImageAsset("map"          ,    "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("flood"  ) ::
        ImageAsset("flood-north"  ) ::
        ImageAsset("flood-south-east"  ) ::
        ImageAsset("flood-south-west"  ) ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-barn" ) ::
        ImageAsset("clearing-name-big-tree" ) ::
        ImageAsset("clearing-name-cottage" ) ::
        ImageAsset("clearing-name-house-boat" ) ::
        ImageAsset("clearing-name-inn" ) ::
        ImageAsset("clearing-name-north-glen" ) ::
        ImageAsset("clearing-name-old-dock" ) ::
        ImageAsset("clearing-name-shanty-town" ) ::
        ImageAsset("clearing-name-stilts" ) ::
        ImageAsset("clearing-name-tree-house" ) ::
        ImageAsset("clearing-name-twin-pines" ) ::
        ImageAsset("clearing-name-underpass" ) ::
        ImageAsset("clearing-name-wetlands" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(TundraMap), "tundra", "tundra:")(
        ImageAsset("map"          ,    "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("blizzard"  ) ::
        ImageAsset("blizzard-mark"  ) ::

        ImageAsset("building-slot", "empty-building-black") ::

        ImageAsset("clearing-name-cliffside" ) ::
        ImageAsset("clearing-name-glacial-vale" ) ::
        ImageAsset("clearing-name-mountain-pass" ) ::
        ImageAsset("clearing-name-frozen-hollow" ) ::
        ImageAsset("clearing-name-cold-creek" ) ::
        ImageAsset("clearing-name-lone-cabin" ) ::
        ImageAsset("clearing-name-mansion" ) ::
        ImageAsset("clearing-name-river-bend" ) ::
        ImageAsset("clearing-name-old-farm" ) ::
        ImageAsset("clearing-name-seaside" ) ::
        ImageAsset("clearing-name-hovel" ) ::
        ImageAsset("clearing-name-deep-woods" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(GloomMap), "gloom", "gloom:")(
        ImageAsset("map"          , "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-woods"    ) ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-lower-jaw" ) ::
        ImageAsset("clearing-name-upper-jaw" ) ::
        ImageAsset("clearing-name-cork-isle" ) ::
        ImageAsset("clearing-name-auberge" ) ::
        ImageAsset("clearing-name-deadwing" ) ::
        ImageAsset("clearing-name-cemetery" ) ::
        ImageAsset("clearing-name-runestone" ) ::
        ImageAsset("clearing-name-haunted" ) ::
        ImageAsset("clearing-name-depot" ) ::
        ImageAsset("clearing-name-bog" ) ::
        ImageAsset("clearing-name-tailbone" ) ::
        ImageAsset("clearing-name-effigy" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Feline].any, "faction/feline")(
        ImageAsset("castle"            ) ::
        ImageAsset("castle-m"          ) ::

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
        ImageAsset("mc-catapult"       ) ::
        ImageAsset("mc-action"         ) ::

        ImageAsset("mc-skip"           ) ::
        ImageAsset("mc-skip-done"      ) ::
        ImageAsset("mc-ask"            ) ::
        ImageAsset("mc-ask-done"       ) ::
        ImageAsset("mc-one-plus"       ) ::
        ImageAsset("mc-one-plus-done"  ) ::
        ImageAsset("mc-two-plus"       ) ::
        ImageAsset("mc-two-plus-done"  ) ::

        ImageAsset("bk-glyph"          ) ::
        ImageAsset("bk-title"          ) ::
        ImageAsset("bk-char"           , "mc-char") ::
        ImageAsset("bk-cat"            ) ::
        ImageAsset("bk-cat-x5"         ) ::
        ImageAsset("bk-cat-empty"      , "mc-cat-empty") ::
        ImageAsset("bk-cat-x5-empty"   , "mc-cat-x5-empty") ::
        ImageAsset("bk-sawmill"        ) ::
        ImageAsset("bk-workshop"       ) ::
        ImageAsset("bk-recruiter"      , "bk-recruiter-new") ::
        ImageAsset("bk-wood"           ) ::
        ImageAsset("bk-keep"           , "mc-keep") ::
        ImageAsset("bk-catapult"       ) ::
        ImageAsset("bk-action"         ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Aviary].any, "card", scale = 50)(
        ImageAsset("loyal-vizier") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Aviary].any, "faction/aviary")(
        ImageAsset("ed-glyph"          ) ::
        ImageAsset("ed-title"          ) ::
        ImageAsset("ed-char"           ) ::
        ImageAsset("ed-hawk"           ) ::
        ImageAsset("ed-hawk-x5"        ) ::
        ImageAsset("ed-hawk-empty"     ) ::
        ImageAsset("ed-hawk-x5-empty"  ) ::
        ImageAsset("ed-roost"          ) ::

        ImageAsset("ed-recruit-bird"           ) ::
        ImageAsset("ed-recruit-fox"            ) ::
        ImageAsset("ed-recruit-rabbit"         ) ::
        ImageAsset("ed-recruit-mouse"          ) ::
        ImageAsset("ed-recruit-frog"           ) ::

        ImageAsset("ed-double-recruit-bird"      ) ::
        ImageAsset("ed-double-recruit-fox"       ) ::
        ImageAsset("ed-double-recruit-rabbit"    ) ::
        ImageAsset("ed-double-recruit-mouse"     ) ::
        ImageAsset("ed-double-recruit-frog"      ) ::

        ImageAsset("ed-move-bird"              ) ::
        ImageAsset("ed-move-fox"               ) ::
        ImageAsset("ed-move-rabbit"            ) ::
        ImageAsset("ed-move-mouse"             ) ::
        ImageAsset("ed-move-frog"              ) ::

        ImageAsset("ed-battle-bird"            ) ::
        ImageAsset("ed-battle-fox"             ) ::
        ImageAsset("ed-battle-rabbit"          ) ::
        ImageAsset("ed-battle-mouse"           ) ::
        ImageAsset("ed-battle-frog"            ) ::

        ImageAsset("ed-build-bird"             ) ::
        ImageAsset("ed-build-fox"              ) ::
        ImageAsset("ed-build-rabbit"           ) ::
        ImageAsset("ed-build-mouse"            ) ::
        ImageAsset("ed-build-frog"            ) ::

        ImageAsset("ed-recruit-bird-done"      ) ::
        ImageAsset("ed-recruit-fox-done"       ) ::
        ImageAsset("ed-recruit-rabbit-done"    ) ::
        ImageAsset("ed-recruit-mouse-done"     ) ::
        ImageAsset("ed-recruit-frog-done"      ) ::

        ImageAsset("ed-double-recruit-bird-done"      ) ::
        ImageAsset("ed-double-recruit-fox-done"       ) ::
        ImageAsset("ed-double-recruit-rabbit-done"    ) ::
        ImageAsset("ed-double-recruit-mouse-done"     ) ::
        ImageAsset("ed-double-recruit-frog-done"      ) ::

        ImageAsset("ed-move-bird-done"         ) ::
        ImageAsset("ed-move-fox-done"          ) ::
        ImageAsset("ed-move-rabbit-done"       ) ::
        ImageAsset("ed-move-mouse-done"        ) ::
        ImageAsset("ed-move-frog-done"         ) ::

        ImageAsset("ed-battle-bird-done"       ) ::
        ImageAsset("ed-battle-fox-done"        ) ::
        ImageAsset("ed-battle-rabbit-done"     ) ::
        ImageAsset("ed-battle-mouse-done"      ) ::
        ImageAsset("ed-battle-frog-done"       ) ::

        ImageAsset("ed-build-bird-done"        ) ::
        ImageAsset("ed-build-fox-done"         ) ::
        ImageAsset("ed-build-rabbit-done"      ) ::
        ImageAsset("ed-build-mouse-done"       ) ::
        ImageAsset("ed-build-frog-done"        ) ::


        ImageAsset("ed-builder"        ) ::
        ImageAsset("ed-charismatic"    ) ::
        ImageAsset("ed-commander"      ) ::
        ImageAsset("ed-despot"         ) ::

        ImageAsset("pe-glyph"          ) ::
        ImageAsset("pe-title"          ) ::
        ImageAsset("pe-char"           , "ed-char"          ) ::
        ImageAsset("pe-hawk"           ) ::
        ImageAsset("pe-hawk-x5"        ) ::
        ImageAsset("pe-hawk-empty"     , "ed-hawk-empty"    ) ::
        ImageAsset("pe-hawk-x5-empty"  , "ed-hawk-x5-empty" ) ::
        ImageAsset("pe-roost"          ) ::

        ImageAsset("pe-recruit-bird"               , "ed-recruit-bird"               ) ::
        ImageAsset("pe-recruit-fox"                , "ed-recruit-fox"                ) ::
        ImageAsset("pe-recruit-rabbit"             , "ed-recruit-rabbit"             ) ::
        ImageAsset("pe-recruit-mouse"              , "ed-recruit-mouse"              ) ::
        ImageAsset("pe-double-recruit-bird"        , "ed-double-recruit-bird"        ) ::
        ImageAsset("pe-double-recruit-fox"         , "ed-double-recruit-fox"         ) ::
        ImageAsset("pe-double-recruit-rabbit"      , "ed-double-recruit-rabbit"      ) ::
        ImageAsset("pe-double-recruit-mouse"       , "ed-double-recruit-mouse"       ) ::
        ImageAsset("pe-move-bird"                  , "ed-move-bird"                  ) ::
        ImageAsset("pe-move-fox"                   , "ed-move-fox"                   ) ::
        ImageAsset("pe-move-rabbit"                , "ed-move-rabbit"                ) ::
        ImageAsset("pe-move-mouse"                 , "ed-move-mouse"                 ) ::
        ImageAsset("pe-battle-bird"                , "ed-battle-bird"                ) ::
        ImageAsset("pe-battle-fox"                 , "ed-battle-fox"                 ) ::
        ImageAsset("pe-battle-rabbit"              , "ed-battle-rabbit"              ) ::
        ImageAsset("pe-battle-mouse"               , "ed-battle-mouse"               ) ::
        ImageAsset("pe-build-bird"                 , "ed-build-bird"                 ) ::
        ImageAsset("pe-build-fox"                  , "ed-build-fox"                  ) ::
        ImageAsset("pe-build-rabbit"               , "ed-build-rabbit"               ) ::
        ImageAsset("pe-build-mouse"                , "ed-build-mouse"                ) ::

        ImageAsset("pe-recruit-bird-done"          , "ed-recruit-bird-done"          ) ::
        ImageAsset("pe-recruit-fox-done"           , "ed-recruit-fox-done"           ) ::
        ImageAsset("pe-recruit-rabbit-done"        , "ed-recruit-rabbit-done"        ) ::
        ImageAsset("pe-recruit-mouse-done"         , "ed-recruit-mouse-done"         ) ::
        ImageAsset("pe-recruit-frog-done"          , "ed-recruit-frog-done"          ) ::
        ImageAsset("pe-double-recruit-bird-done"   , "ed-double-recruit-bird-done"   ) ::
        ImageAsset("pe-double-recruit-fox-done"    , "ed-double-recruit-fox-done"    ) ::
        ImageAsset("pe-double-recruit-rabbit-done" , "ed-double-recruit-rabbit-done" ) ::
        ImageAsset("pe-double-recruit-mouse-done"  , "ed-double-recruit-mouse-done"  ) ::
        ImageAsset("pe-double-recruit-frog-done"   , "ed-double-recruit-frog-done"   ) ::
        ImageAsset("pe-move-bird-done"             , "ed-move-bird-done"             ) ::
        ImageAsset("pe-move-fox-done"              , "ed-move-fox-done"              ) ::
        ImageAsset("pe-move-rabbit-done"           , "ed-move-rabbit-done"           ) ::
        ImageAsset("pe-move-mouse-done"            , "ed-move-mouse-done"            ) ::
        ImageAsset("pe-move-frog-done"             , "ed-move-frog-done"             ) ::
        ImageAsset("pe-battle-bird-done"           , "ed-battle-bird-done"           ) ::
        ImageAsset("pe-battle-fox-done"            , "ed-battle-fox-done"            ) ::
        ImageAsset("pe-battle-rabbit-done"         , "ed-battle-rabbit-done"         ) ::
        ImageAsset("pe-battle-mouse-done"          , "ed-battle-mouse-done"          ) ::
        ImageAsset("pe-battle-frog-done"           , "ed-battle-frog-done"           ) ::
        ImageAsset("pe-build-bird-done"            , "ed-build-bird-done"            ) ::
        ImageAsset("pe-build-fox-done"             , "ed-build-fox-done"             ) ::
        ImageAsset("pe-build-rabbit-done"          , "ed-build-rabbit-done"          ) ::
        ImageAsset("pe-build-mouse-done"           , "ed-build-mouse-done"           ) ::
        ImageAsset("pe-build-frog-done"            , "ed-build-frog-done"            ) ::

        ImageAsset("pe-builder"       , "ed-builder"        ) ::
        ImageAsset("pe-charismatic"   , "ed-charismatic"    ) ::
        ImageAsset("pe-commander"     , "ed-commander"      ) ::
        ImageAsset("pe-despot"        , "ed-despot"         ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Insurgent].any, "faction/insurgent")(
        ImageAsset("wa-glyph"         ) ::
        ImageAsset("wa-title"         ) ::
        ImageAsset("wa-char"          ) ::
        ImageAsset("wa-critter"       ) ::
        ImageAsset("wa-critter-empty" ) ::
        ImageAsset("wa-officer"       ) ::
        ImageAsset("wa-supporter"     ) ::
        ImageAsset("wa-sympathy"      ) ::
        ImageAsset("wa-base-fox"      ) ::
        ImageAsset("wa-base-rabbit"   ) ::
        ImageAsset("wa-base-mouse"    ) ::
        ImageAsset("wa-action"        ) ::

        ImageAsset("af-glyph"         ) ::
        ImageAsset("af-title"         ) ::
        ImageAsset("af-char"          , "wa-char"          ) ::
        ImageAsset("af-critter"       ) ::
        ImageAsset("af-critter-empty" , "wa-critter-empty" ) ::
        ImageAsset("af-officer"       , "wa-officer"       ) ::
        ImageAsset("af-supporter"     ) ::
        ImageAsset("af-sympathy"      ) ::
        ImageAsset("af-base-fox"      ) ::
        ImageAsset("af-base-rabbit"   ) ::
        ImageAsset("af-base-mouse"    ) ::
        ImageAsset("af-action"        ) ::

        ImageAsset("fu-glyph"         , "af-glyph"         ) ::
        ImageAsset("fu-title"         , "af-title"         ) ::
        ImageAsset("fu-char"          , "wa-char"          ) ::
        ImageAsset("fu-critter"       , "af-critter"       ) ::
        ImageAsset("fu-critter-empty" , "wa-critter-empty" ) ::
        ImageAsset("fu-officer"       , "wa-officer"       ) ::
        ImageAsset("fu-supporter"     , "af-supporter"     ) ::
        ImageAsset("fu-sympathy"      , "af-sympathy"      ) ::
        ImageAsset("fu-base-fox"      , "af-base-fox"      ) ::
        ImageAsset("fu-base-rabbit"   , "af-base-rabbit"   ) ::
        ImageAsset("fu-base-mouse"    , "af-base-mouse"    ) ::
        ImageAsset("fu-action"        , "af-action"        ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Trader].any, "faction/trader")(
        ImageAsset("rf-glyph"           ) ::
        ImageAsset("rf-title"           ) ::
        ImageAsset("rf-char"            ) ::
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
    $) ::
    ConditionalAssetsList((factions, options) => factions.has(SF), "faction/trader")(
        ImageAsset("sf-glyph") ::
        ImageAsset("sf-title") ::
        ImageAsset("sf-char", "../hero/vb-char-harrier") ::

        ImageAsset("sf-squirrel"           ) ::
        ImageAsset("sf-squirrel-x5"        ) ::
        ImageAsset("sf-squirrel-empty"     ) ::
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
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Fanatic].any, "faction/fanatic")(
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

        ImageAsset("cm-glyph"            ) ::
        ImageAsset("cm-title"            ) ::
        ImageAsset("cm-char"             , "lc-char"             ) ::
        ImageAsset("cm-lizard"           ) ::
        ImageAsset("cm-lizard-x5"        ) ::
        ImageAsset("cm-lizard-empty"     , "lc-lizard-empty"     ) ::
        ImageAsset("cm-lizard-x5-empty"  , "lc-lizard-x5-empty"  ) ::
        ImageAsset("cm-acolyte"          , "lc-acolyte"          ) ::
        ImageAsset("cm-acolyte-x5"       , "lc-acolyte-x5"       ) ::
        ImageAsset("cm-garden-fox"       ) ::
        ImageAsset("cm-garden-rabbit"    ) ::
        ImageAsset("cm-garden-mouse"     ) ::

        ImageAsset("outcast-fox"         ) ::
        ImageAsset("outcast-fox-hated"   ) ::
        ImageAsset("outcast-rabbit"      ) ::
        ImageAsset("outcast-rabbit-hated") ::
        ImageAsset("outcast-mouse"       ) ::
        ImageAsset("outcast-mouse-hated" ) ::
        ImageAsset("building-any"        ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Underground].any, "faction/underground")(
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
        ImageAsset("ud-crown-2"        ) ::
        ImageAsset("ud-crown-3"        ) ::
        ImageAsset("ud-crown-4"        ) ::
        ImageAsset("ud-crown-empty"    ) ::
        ImageAsset("ud-action"         ) ::
        ImageAsset("ud-building"       ) ::

        ImageAsset("dr-glyph"          ) ::
        ImageAsset("dr-title"          ) ::
        ImageAsset("dr-char"           , "ud-char"           ) ::
        ImageAsset("dr-mole"           ) ::
        ImageAsset("dr-mole-x5"        ) ::
        ImageAsset("dr-mole-empty"     , "ud-mole-empty"     ) ::
        ImageAsset("dr-mole-x5-empty"  , "ud-mole-x5-empty"  ) ::
        ImageAsset("dr-young"          , "ud-young"          ) ::
        ImageAsset("dr-young-x5"       , "ud-young-x5"       ) ::
        ImageAsset("dr-citadel"        ) ::
        ImageAsset("dr-market"         ) ::
        ImageAsset("dr-tunnel"         ) ::
        ImageAsset("dr-crown"          , "ud-crown"          ) ::
        ImageAsset("dr-crown-2"        , "ud-crown-2"        ) ::
        ImageAsset("dr-crown-3"        , "ud-crown-3"        ) ::
        ImageAsset("dr-crown-4"        , "ud-crown-4"        ) ::
        ImageAsset("dr-crown-empty"    , "ud-crown-empty"    ) ::
        ImageAsset("dr-action"         ) ::
        ImageAsset("dr-building"       ) ::

        ImageAsset("empty-building-mole-1") ::
        ImageAsset("empty-building-mole-2") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Underground].any, "info", "info:", lzy = Laziness.OnDemand)(
        ImageAsset("minister-foremole"        ) ::
        ImageAsset("minister-marshal"         ) ::
        ImageAsset("minister-captain"         ) ::
        ImageAsset("minister-brigadier"       ) ::
        ImageAsset("minister-mayor"           ) ::
        ImageAsset("minister-banker"          ) ::
        ImageAsset("minister-duchess"   , "minister-duchess-of-mud"  ) ::
        ImageAsset("minister-baron"     , "minister-baron-of-dirt"   ) ::
        ImageAsset("minister-earl"      , "minister-earl-of-stone"   ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Mischief].any, "faction/mischief")(
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
        ImageAsset("cc-diversion"      , "cc-plot" ) ::
        ImageAsset("cc-action"         ) ::
        ImageAsset("ri-glyph"           ) ::
        ImageAsset("ri-title"           ) ::
        ImageAsset("ri-char"            , "cc-char"           ) ::
        ImageAsset("ri-raven"           ) ::
        ImageAsset("ri-raven-x5"        ) ::
        ImageAsset("ri-raven-empty"     , "cc-raven-empty"    ) ::
        ImageAsset("ri-raven-x5-empty"  , "cc-raven-x5-empty" ) ::
        ImageAsset("ri-bomb"            ) ::
        ImageAsset("ri-snare"           ) ::
        ImageAsset("ri-extortion"       ) ::
        ImageAsset("ri-raid"            ) ::
        ImageAsset("ri-plot"            ) ::
        ImageAsset("ri-diversion"       , "ri-plot"           ) ::
        ImageAsset("ri-action"          ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Horde].any, "faction/horde")(
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

        ImageAsset("lk-glyph"          ) ::
        ImageAsset("lk-title"          ) ::
        ImageAsset("lk-char"           , "lh-char"           ) ::
        ImageAsset("lk-rat"            ) ::
        ImageAsset("lk-rat-x5"         ) ::
        ImageAsset("lk-rat-empty"      , "lh-rat-empty"      ) ::
        ImageAsset("lk-rat-x5-empty"   , "lh-rat-x5-empty"   ) ::
        ImageAsset("lk-warlord"        ) ::
        ImageAsset("lk-warlord-empty"  , "lh-warlord-empty"  ) ::
        ImageAsset("lk-stronghold"     ) ::
        ImageAsset("lk-mob"            ) ::
        ImageAsset("lk-mob-q"          ) ::
        ImageAsset("lk-move"           , "lh-move"           ) ::
        ImageAsset("lk-move-done"      , "lh-move-done"      ) ::
        ImageAsset("lk-battle"         , "lh-battle"         ) ::
        ImageAsset("lk-battle-done"    , "lh-battle-done"    ) ::
        ImageAsset("lk-action"         , "lh-action"         ) ::

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

        ImageAsset("satchel-left-1-lh"     ) ::
        ImageAsset("satchel-top-2-lh"      ) ::
        ImageAsset("satchel-top-3-lh"      ) ::
        ImageAsset("satchel-top-4-lh"      ) ::
        ImageAsset("satchel-bottom-2-lh"   ) ::
        ImageAsset("satchel-bottom-3-lh"   ) ::
        ImageAsset("satchel-bottom-4-lh"   ) ::

        ImageAsset("satchel-left-1-lk"     , "satchel-left-1-lh"   ) ::
        ImageAsset("satchel-top-2-lk"      , "satchel-top-2-lh"    ) ::
        ImageAsset("satchel-top-3-lk"      , "satchel-top-3-lh"    ) ::
        ImageAsset("satchel-top-4-lk"      , "satchel-top-4-lh"    ) ::
        ImageAsset("satchel-bottom-2-lk"   , "satchel-bottom-2-lh" ) ::
        ImageAsset("satchel-bottom-3-lk"   , "satchel-bottom-3-lh" ) ::
        ImageAsset("satchel-bottom-4-lk"   , "satchel-bottom-4-lh" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Horde].any, "info", "info:", lzy = Laziness.OnDemand)(
        ImageAsset("lord-of-the-hundreds-1" ) ::
        ImageAsset("lord-of-the-hundreds-2" ) ::
        ImageAsset("lord-of-the-hundreds-3" ) ::
        ImageAsset("mood-bitter"            ) ::
        ImageAsset("mood-grandiose"         ) ::
        ImageAsset("mood-jubilant"          ) ::
        ImageAsset("mood-lavish"            ) ::
        ImageAsset("mood-relentless"        ) ::
        ImageAsset("mood-rowdy"             ) ::
        ImageAsset("mood-stubborn"          ) ::
        ImageAsset("mood-wrathful"          ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Expedition].any, "card", scale = 50)(
        ImageAsset("faithful-retainer") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Expedition].any, "faction/aviary")(
        ImageAsset("ki-move-bird-done"     ,  "ed-move-bird-done"     ) ::
        ImageAsset("ki-move-fox-done"      ,  "ed-move-fox-done"      ) ::
        ImageAsset("ki-move-rabbit-done"   ,  "ed-move-rabbit-done"   ) ::
        ImageAsset("ki-move-mouse-done"    ,  "ed-move-mouse-done"    ) ::
        ImageAsset("ki-move-frog-done"     ,  "ed-move-frog-done"     ) ::

        ImageAsset("ki-battle-bird-done"   ,  "ed-battle-bird-done"   ) ::
        ImageAsset("ki-battle-fox-done"    ,  "ed-battle-fox-done"    ) ::
        ImageAsset("ki-battle-rabbit-done" ,  "ed-battle-rabbit-done" ) ::
        ImageAsset("ki-battle-mouse-done"  ,  "ed-battle-mouse-done"  ) ::
        ImageAsset("ki-battle-frog-done"   ,  "ed-battle-frog-done"   ) ::

        ImageAsset("ki-move-bird"          ,  "ed-move-bird"          ) ::
        ImageAsset("ki-move-fox"           ,  "ed-move-fox"           ) ::
        ImageAsset("ki-move-rabbit"        ,  "ed-move-rabbit"        ) ::
        ImageAsset("ki-move-mouse"         ,  "ed-move-mouse"         ) ::
        ImageAsset("ki-move-frog"          ,  "ed-move-frog"          ) ::

        ImageAsset("ki-battle-bird"        ,  "ed-battle-bird"        ) ::
        ImageAsset("ki-battle-fox"         ,  "ed-battle-fox"         ) ::
        ImageAsset("ki-battle-rabbit"      ,  "ed-battle-rabbit"      ) ::
        ImageAsset("ki-battle-mouse"       ,  "ed-battle-mouse"       ) ::
        ImageAsset("ki-battle-frog"        ,  "ed-battle-frog"        ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Expedition].any, "faction/expedition")(
        ImageAsset("ki-glyph"          ) ::
        ImageAsset("ki-title"          ) ::
        ImageAsset("ki-char"           ) ::
        ImageAsset("ki-badger-empty") ::
        ImageAsset("ki-badger-x5-empty") ::
        ImageAsset("ki-badger-x5") ::
        ImageAsset("ki-badger") ::
        ImageAsset("ki-idol-1") ::
        ImageAsset("ki-idol-2") ::
        ImageAsset("ki-idol-3") ::
        ImageAsset("ki-idol-hidden") ::
        ImageAsset("ki-idol-empty") ::
        ImageAsset("ki-jewel-1") ::
        ImageAsset("ki-jewel-2") ::
        ImageAsset("ki-jewel-3") ::
        ImageAsset("ki-jewel-hidden") ::
        ImageAsset("ki-jewel-empty") ::
        ImageAsset("ki-tablet-1") ::
        ImageAsset("ki-tablet-2") ::
        ImageAsset("ki-tablet-3") ::
        ImageAsset("ki-tablet-hidden") ::
        ImageAsset("ki-tablet-empty") ::

        ImageAsset("ki-waystation-idol-jewel") ::
        ImageAsset("ki-waystation-idol-tablet") ::
        ImageAsset("ki-waystation-jewel-idol") ::
        ImageAsset("ki-waystation-jewel-tablet") ::
        ImageAsset("ki-waystation-tablet-idol") ::
        ImageAsset("ki-waystation-tablet-jewel") ::

        ImageAsset("ki-recover-bird-done"    ) ::
        ImageAsset("ki-recover-fox-done"     ) ::
        ImageAsset("ki-recover-rabbit-done"  ) ::
        ImageAsset("ki-recover-mouse-done"   ) ::
        ImageAsset("ki-recover-frog-done"    ) ::

        ImageAsset("ki-recover-bird"    ) ::
        ImageAsset("ki-recover-fox"     ) ::
        ImageAsset("ki-recover-rabbit"  ) ::
        ImageAsset("ki-recover-mouse"   ) ::
        ImageAsset("ki-recover-frog"    ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[OldExpedition].any, "faction/old-expedition")(
        ImageAsset("ok-glyph"           ) ::
        ImageAsset("ok-title"           ) ::
        ImageAsset("ok-char"            , "../expedition/ki-char") ::
        ImageAsset("ok-board"           ).scaled(50) ::
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
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any, "faction/hero")(
        ImageAsset("vb-glyph"          ) ::
        ImageAsset("nb-glyph"          ) ::
        ImageAsset("mb-glyph"          ) ::
        ImageAsset("vb-vagabond"       ) ::
        ImageAsset("nb-vagabond"       ) ::
        ImageAsset("mb-vagabond"       ) ::

        ImageAsset("quest-fox"         ) ::
        ImageAsset("quest-rabbit"      ) ::
        ImageAsset("quest-mouse"       ) ::

        ImageAsset("attitude-heart-empty" ) ::
        ImageAsset("attitude-heart-full"  ) ::
        ImageAsset("attitude-hostile"     ) ::

        ImageAsset("satchel-top",    "../horde/satchel-top"        ) ::
        ImageAsset("satchel-bottom", "../horde/satchel-bottom"     ) ::

        ImageAsset("satchel-thin-top-3", "../horde/satchel-thin-top-3" ) ::
        ImageAsset("satchel-thin-top-4", "../horde/satchel-thin-top-4" ) ::
        ImageAsset("satchel-thin-top-5", "../horde/satchel-thin-top-5" ) ::
        ImageAsset("satchel-thin-top-6", "../horde/satchel-thin-top-6" ) ::

        ImageAsset("satchel-thin-bottom-3", "../horde/satchel-thin-bottom-3" ) ::
        ImageAsset("satchel-thin-bottom-4", "../horde/satchel-thin-bottom-4" ) ::
        ImageAsset("satchel-thin-bottom-5", "../horde/satchel-thin-bottom-5" ) ::
        ImageAsset("satchel-thin-bottom-6", "../horde/satchel-thin-bottom-6" ) ::

        ImageAsset("vb-char-thief") ::
        ImageAsset("vb-char-tinker") ::
        ImageAsset("vb-char-ranger") ::
        ImageAsset("vb-char-arbiter") ::
        ImageAsset("vb-char-vagrant") ::
        ImageAsset("vb-char-scoundrel") ::
        ImageAsset("vb-char-adventurer") ::
        ImageAsset("vb-char-ronin") ::
        ImageAsset("vb-char-harrier") ::

        ImageAsset("vb-char-folk-hero").scaled(50) ::

        ImageAsset("vb-char", "vb-char-thief") ::
        ImageAsset("nb-char", "vb-char-thief") ::
        ImageAsset("mb-char", "vb-char-thief") ::

        ImageAsset("vb-title-thief") ::
        ImageAsset("vb-title-tinker") ::
        ImageAsset("vb-title-ranger") ::
        ImageAsset("vb-title-arbiter") ::
        ImageAsset("vb-title-vagrant") ::
        ImageAsset("vb-title-scoundrel") ::
        ImageAsset("vb-title-adventurer") ::
        ImageAsset("vb-title-ronin") ::
        ImageAsset("vb-title-harrier") ::

        ImageAsset("vb-title-folk-hero") ::

        ImageAsset("vb-title") ::
        ImageAsset("nb-title") ::
        ImageAsset("mb-title") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any && options.has(RevisedQuests).not, "card/quest", "quest:", lzy = Laziness.OnDemand)(
        ImageAsset("errand-fox"             ) ::
        ImageAsset("errand-rabbit"          ) ::
        ImageAsset("escort-mouse"           ) ::
        ImageAsset("expel-bandits-mouse"    ) ::
        ImageAsset("expel-bandits-rabbit"   ) ::
        ImageAsset("fend-off-a-bear-mouse"  ) ::
        ImageAsset("fend-off-a-bear-rabbit" ) ::
        ImageAsset("fundraising-fox"        ) ::
        ImageAsset("give-a-speech-fox"      ) ::
        ImageAsset("give-a-speech-rabbit"   ) ::
        ImageAsset("guard-duty-mouse"       ) ::
        ImageAsset("guard-duty-rabbit"      ) ::
        ImageAsset("logistic-help-fox"      ) ::
        ImageAsset("logistic-help-mouse"    ) ::
        ImageAsset("repair-a-shed-fox"      ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any && options.has(RevisedQuests), "card/quest", "quest:", lzy = Laziness.OnDemand)(
        ImageAsset("revised-errand-fox"            , "revised/errand-fox"           ) ::
        ImageAsset("revised-errand-rabbit"         , "revised/errand-rabbit"        ) ::
        ImageAsset("revised-escort-mouse"          , "revised/escort-mouse"         ) ::
        ImageAsset("revised-expel-bandits-mouse"   , "expel-bandits-mouse"          ) ::
        ImageAsset("revised-expel-bandits-rabbit"  , "expel-bandits-rabbit"         ) ::
        ImageAsset("revised-fend-off-a-bear-mouse" , "fend-off-a-bear-mouse"        ) ::
        ImageAsset("revised-repair-a-shed-rabbit"  , "revised/repair-a-shed-rabbit" ) ::
        ImageAsset("revised-fundraising-fox"       , "revised/fundraising-fox"      ) ::
        ImageAsset("revised-give-a-speech-fox"     , "give-a-speech-fox"            ) ::
        ImageAsset("revised-give-a-speech-rabbit"  , "give-a-speech-rabbit"         ) ::
        ImageAsset("revised-guard-duty-mouse"      , "guard-duty-mouse"             ) ::
        ImageAsset("revised-guard-duty-rabbit"     , "guard-duty-rabbit"            ) ::
        ImageAsset("revised-logistic-help-fox"     , "logistic-help-fox"            ) ::
        ImageAsset("revised-logistic-help-mouse"   , "logistic-help-mouse"          ) ::
        ImageAsset("revised-escort-fox"            , "revised/escort-fox"           ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "card/deck", lzy = Laziness.Later, scale = 50)(
        ImageAsset("bird-ambush"                ) ::
        ImageAsset("fox-ambush"                 ) ::
        ImageAsset("mouse-ambush"               ) ::
        ImageAsset("rabbit-ambush"              ) ::

        ImageAsset("bird-dominance"             ) ::
        ImageAsset("fox-dominance"              ) ::
        ImageAsset("mouse-dominance"            ) ::
        ImageAsset("rabbit-dominance"           ) ::

        ImageAsset("a-visit-to-friends"         ) ::
        ImageAsset("anvil"                      ) ::
        ImageAsset("arms-trader"                ) ::
        ImageAsset("bake-sale"                  ) ::
        ImageAsset("bird-crossbow"              ) ::
        ImageAsset("birdy-bindle"               ) ::
        ImageAsset("fox-root-tea"               ) ::
        ImageAsset("fox-travel-gear"            ) ::
        ImageAsset("foxfolk-steel"              ) ::
        ImageAsset("gently-used-knapsack"       ) ::
        ImageAsset("investments"                ) ::
        ImageAsset("mouse-crossbow"             ) ::
        ImageAsset("mouse-in-a-sack"            ) ::
        ImageAsset("mouse-root-tea"             ) ::
        ImageAsset("mouse-travel-gear"          ) ::
        ImageAsset("protection-racket"          ) ::
        ImageAsset("rabbit-root-tea"            ) ::
        ImageAsset("smugglers-trail"            ) ::
        ImageAsset("sword"                      ) ::
        ImageAsset("woodland-runners"           ) ::

        ImageAsset("armorers"                   ) ::
        ImageAsset("better-burrow-bank"         ) ::
        ImageAsset("brutal-tactics"             ) ::
        ImageAsset("cobbler"                    ) ::
        ImageAsset("codebreakers"               ) ::
        ImageAsset("command-warren"             ) ::
        ImageAsset("favor-of-the-foxes"         ) ::
        ImageAsset("favor-of-the-mice"          ) ::
        ImageAsset("favor-of-the-rabbits"       ) ::
        ImageAsset("royal-claim"                ) ::
        ImageAsset("sappers"                    ) ::
        ImageAsset("scouting-party"             ) ::
        ImageAsset("stand-and-deliver"          ) ::
        ImageAsset("tax-collector"              ) ::

        ImageAsset("boat-builders"              ) ::
        ImageAsset("charm-offensive"            ) ::
        ImageAsset("coffin-makers"              ) ::
        ImageAsset("corvid-planners"            ) ::
        ImageAsset("eyrie-emigre"               ) ::
        ImageAsset("false-orders"               ) ::
        ImageAsset("fox-partisans"              ) ::
        ImageAsset("informants"                 ) ::
        ImageAsset("league-of-adventurous-mice" ) ::
        ImageAsset("master-engravers"           ) ::
        ImageAsset("mouse-partisans"            ) ::
        ImageAsset("murine-broker"              ) ::
        ImageAsset("propaganda-bureau"          ) ::
        ImageAsset("rabbit-partisans"           ) ::
        ImageAsset("saboteurs"                  ) ::
        ImageAsset("soup-kitchens"              ) ::
        ImageAsset("borscht-kitchens"           ) ::
        ImageAsset("swap-meet"                  ) ::
        ImageAsset("tunnels"                    ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(DuskDeck), "card/dusk", lzy = Laziness.Later)(
        ImageAsset("adventurers"                ) ::
        ImageAsset("breaking-dawn"              ) ::
        ImageAsset("dusk-awakening"             ) ::
        ImageAsset("forest-vigil"               ) ::
        ImageAsset("fowl-play"                  ) ::
        ImageAsset("fox-guard"                  ) ::
        ImageAsset("high-noon"                  ) ::
        ImageAsset("hoarders"                   ) ::
        ImageAsset("merchants"                  ) ::
        ImageAsset("military-supplies"          ) ::
        ImageAsset("mouse-guard"                ) ::
        ImageAsset("night-terrors"              ) ::
        ImageAsset("off-trail"                  ) ::
        ImageAsset("old-friend"                 ) ::
        ImageAsset("public-relations"           ) ::
        ImageAsset("rabbit-guard"               ) ::
        ImageAsset("service-of-the-foxes"       ) ::
        ImageAsset("service-of-the-mice"        ) ::
        ImageAsset("service-of-the-rabbits"     ) ::
        ImageAsset("sly-dog"                    ) ::
        ImageAsset("spoil"                      ) ::
        ImageAsset("tricky-claws"               ) ::
        ImageAsset("tricky-ears"                ) ::
        ImageAsset("tricky-tails"               ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "info", "info:", lzy = Laziness.OnDemand)(
        ImageAsset("street-band-1"          ) ::
        ImageAsset("street-band-2"          ) ::
        ImageAsset("bandit-gangs-1"         ) ::
        ImageAsset("bandit-gangs-2"         ) ::
        ImageAsset("rat-smugglers"          ) ::
        ImageAsset("forest-patrol"          ) ::
        ImageAsset("last-dynasty"           ) ::
        ImageAsset("spring-uprising"        ) ::
        ImageAsset("the-exile"              ) ::
        ImageAsset("mole-artisians"         ) ::
        ImageAsset("flotilla"               ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "artwork", "artwork:", lzy = Laziness.OnDemand)(
        ImageAsset("bird-ambush"                ) ::
        ImageAsset("fox-ambush"                 ) ::
        ImageAsset("mouse-ambush"               ) ::
        ImageAsset("rabbit-ambush"              ) ::

        ImageAsset("bird-dominance"             ) ::
        ImageAsset("fox-dominance"              ) ::
        ImageAsset("mouse-dominance"            ) ::
        ImageAsset("rabbit-dominance"           ) ::

        ImageAsset("a-visit-to-friends"         ) ::
        ImageAsset("anvil"                      ) ::
        ImageAsset("arms-trader"                ) ::
        ImageAsset("bake-sale"                  ) ::
        ImageAsset("bird-crossbow"              ) ::
        ImageAsset("birdy-bindle"               ) ::
        ImageAsset("fox-root-tea"               ) ::
        ImageAsset("fox-travel-gear"            ) ::
        ImageAsset("foxfolk-steel"              ) ::
        ImageAsset("gently-used-knapsack"       ) ::
        ImageAsset("investments"                ) ::
        ImageAsset("mouse-crossbow"             ) ::
        ImageAsset("mouse-in-a-sack"            ) ::
        ImageAsset("mouse-root-tea"             ) ::
        ImageAsset("mouse-travel-gear"          ) ::
        ImageAsset("protection-racket"          ) ::
        ImageAsset("rabbit-root-tea"            ) ::
        ImageAsset("smugglers-trail"            ) ::
        ImageAsset("sword"                      ) ::
        ImageAsset("woodland-runners"           ) ::

        ImageAsset("armorers"                   ) ::
        ImageAsset("better-burrow-bank"         ) ::
        ImageAsset("brutal-tactics"             ) ::
        ImageAsset("cobbler"                    ) ::
        ImageAsset("codebreakers"               ) ::
        ImageAsset("command-warren"             ) ::
        ImageAsset("favor-of-the-foxes"         ) ::
        ImageAsset("favor-of-the-mice"          ) ::
        ImageAsset("favor-of-the-rabbits"       ) ::
        ImageAsset("royal-claim"                ) ::
        ImageAsset("sappers"                    ) ::
        ImageAsset("scouting-party"             ) ::
        ImageAsset("stand-and-deliver"          ) ::
        ImageAsset("tax-collector"              ) ::

        ImageAsset("boat-builders"              ) ::
        ImageAsset("charm-offensive"            ) ::
        ImageAsset("coffin-makers"              ) ::
        ImageAsset("corvid-planners"            ) ::
        ImageAsset("eyrie-emigre"               ) ::
        ImageAsset("false-orders"               ) ::
        ImageAsset("fox-partisans"              ) ::
        ImageAsset("informants"                 ) ::
        ImageAsset("league-of-adventurous-mice" ) ::
        ImageAsset("master-engravers"           ) ::
        ImageAsset("mouse-partisans"            ) ::
        ImageAsset("murine-broker"              ) ::
        ImageAsset("propaganda-bureau"          ) ::
        ImageAsset("rabbit-partisans"           ) ::
        ImageAsset("saboteurs"                  ) ::
        ImageAsset("soup-kitchens"              ) ::
        ImageAsset("borscht-kitchens"           ) ::
        ImageAsset("swap-meet"                  ) ::
        ImageAsset("tunnels"                    ) ::

        ImageAsset("faithful-retainer"          ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any && options.has(RevisedQuests).not, "artwork/quest", "artwork:", lzy = Laziness.OnDemand)(
        ImageAsset("errand-fox"             ) ::
        ImageAsset("errand-rabbit"          ) ::
        ImageAsset("escort-mouse"           ) ::
        ImageAsset("expel-bandits-mouse"    ) ::
        ImageAsset("expel-bandits-rabbit"   ) ::
        ImageAsset("fend-off-a-bear-mouse"  ) ::
        ImageAsset("fend-off-a-bear-rabbit" ) ::
        ImageAsset("fundraising-fox"        ) ::
        ImageAsset("give-a-speech-fox"      ) ::
        ImageAsset("give-a-speech-rabbit"   ) ::
        ImageAsset("guard-duty-mouse"       ) ::
        ImageAsset("guard-duty-rabbit"      ) ::
        ImageAsset("logistic-help-fox"      ) ::
        ImageAsset("logistic-help-mouse"    ) ::
        ImageAsset("repair-a-shed-fox"      ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any && options.has(RevisedQuests), "artwork/quest", "artwork:", lzy = Laziness.OnDemand)(
        ImageAsset("errand-fox"             , "revised/errand-fox"            ) ::
        ImageAsset("errand-rabbit"          , "revised/errand-rabbit"         ) ::
        ImageAsset("escort-mouse"           , "revised/escort-mouse"          ) ::
        ImageAsset("expel-bandits-mouse"    ) ::
        ImageAsset("expel-bandits-rabbit"   ) ::
        ImageAsset("fend-off-a-bear-mouse"  ) ::
        ImageAsset("repair-a-shed-rabbit"   , "revised/repair-a-shed-rabbit"  ) ::
        ImageAsset("fundraising-fox"        , "revised/fundraising-fox"       ) ::
        ImageAsset("give-a-speech-fox"      ) ::
        ImageAsset("give-a-speech-rabbit"   ) ::
        ImageAsset("guard-duty-mouse"       ) ::
        ImageAsset("guard-duty-rabbit"      ) ::
        ImageAsset("logistic-help-fox"      ) ::
        ImageAsset("logistic-help-mouse"    ) ::
        ImageAsset("escort-fox"             , "revised/escort-fox"            ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Utopia].any, "faction/utopia")(
        ImageAsset("cu-glyph"          ) ::
        ImageAsset("cu-title"          ) ::
        ImageAsset("cu-char"           ) ::
        ImageAsset("cu-foxy"           ) ::
        ImageAsset("cu-foxy-empty"     ) ::
        ImageAsset("cu-foxy-x5"        ) ::
        ImageAsset("cu-foxy-x5-empty"  ) ::
        ImageAsset("cu-palace"         ) ::
        ImageAsset("cu-living-shield"  ) ::
        ImageAsset("cu-action"         ) ::

        ImageAsset("cu-board-v1"       ) ::
        ImageAsset("cu-board-v2"       ) ::
        ImageAsset("cu-board-v3"       ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Caster].any, "faction/caster")(
        ImageAsset("xc-glyph"          ) ::
        ImageAsset("xc-title"          ) ::
        ImageAsset("xc-char"           ) ::
        ImageAsset("xc-frog"           ) ::
        ImageAsset("xc-frog-empty"     ) ::
        ImageAsset("xc-frog-x5"        ) ::
        ImageAsset("xc-frog-x5-empty"  ) ::
        ImageAsset("xc-school"         ) ::
        ImageAsset("xc-statue"         ) ::
        ImageAsset("xc-growth"         ) ::
        ImageAsset("xc-mudman"         ) ::
        ImageAsset("xc-action"         ) ::
        ImageAsset("xc-magic"          ) ::
        ImageAsset("xc-magic-q"        ) ::
        ImageAsset("xc-board"          ) ::

        ImageAsset("xc-spell-petrify"      ) ::
        ImageAsset("xc-spell-dazzle"       ) ::
        ImageAsset("xc-spell-regrowth"     ) ::
        ImageAsset("xc-spell-fireball"     ) ::
        ImageAsset("xc-spell-summon"       ) ::
        ImageAsset("xc-spell-resurrection" ) ::
        ImageAsset("xc-spell-transform"    ) ::
        ImageAsset("xc-spell-teleport"     ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Farmer].any, "faction/farmer")(
        ImageAsset("fh-glyph"            ) ::
        ImageAsset("fh-title"            ) ::
        ImageAsset("fh-char"             ) ::
        ImageAsset("fh-hamster"          ) ::
        ImageAsset("fh-hamster-empty"    ) ::
        ImageAsset("fh-hamster-x5"       ) ::
        ImageAsset("fh-hamster-x5-empty" ) ::
        ImageAsset("fh-sheriff"          ) ::
        ImageAsset("fh-sheriff-empty"    ) ::
        ImageAsset("fh-farm"             ) ::
        ImageAsset("fh-windmill"         ) ::
        ImageAsset("fh-grain"            ) ::
        ImageAsset("fh-action"           ) ::
        ImageAsset("fh-board-v2"         ) ::

        ImageAsset("attitude-heart-empty" , "../hero/attitude-heart-empty" ) ::
        ImageAsset("attitude-heart-full"  , "../hero/attitude-heart-full"  ) ::
        ImageAsset("attitude-hostile"     , "../hero/attitude-hostile"     ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveAAA].any, "faction/invasive/card", lzy = Laziness.Later, scale = 50)(
        ImageAsset("frog-ambush") ::
        ImageAsset("frog-engineers") ::
        ImageAsset("favor-of-the-frogs") ::
        ImageAsset("frog-dominance") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveAAA].any, "faction/invasive")(
        ImageAsset("td-glyph"            ) ::
        ImageAsset("td-title"            ) ::
        ImageAsset("td-char"             ) ::
        ImageAsset("td-board"            ) ::

        ImageAsset("td-frog"          ) ::
        ImageAsset("td-frog-empty"    ) ::
        ImageAsset("td-frog-x5"       ) ::
        ImageAsset("td-frog-x5-empty" ) ::
        ImageAsset("td-peaceful"      ) ::
        ImageAsset("td-militant"      ) ::

        ImageAsset("td-action"           ) ::
        ImageAsset("td-action-done"      ) ::

        ImageAsset("outcast-fox"         , "../fanatic/outcast-fox") ::
        ImageAsset("outcast-rabbit"      , "../fanatic/outcast-rabbit") ::
        ImageAsset("outcast-mouse"       , "../fanatic/outcast-mouse") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveBBB].any, "faction/invasive/card", lzy = Laziness.Later, scale = 50)(
        ImageAsset("frog-ambush") ::
        ImageAsset("favor-of-the-frogs-half") ::
        ImageAsset("frog-dominance-half") ::
        ImageAsset("imitation") ::
        ImageAsset("rabbit-integration") ::
        ImageAsset("mouse-integration") ::
        ImageAsset("fox-integration") ::
        ImageAsset("card-frog-back-art") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveBBB].any, "faction/invasive")(
        ImageAsset("ld-glyph"            , "td-glyph"            ) ::
        ImageAsset("ld-title"                                    ) ::
        ImageAsset("ld-char"             , "td-char"             ) ::
        ImageAsset("ld-board"                                    ).scaled(50) ::

        ImageAsset("ld-frog"          , "td-frog"          ) ::
        ImageAsset("ld-frog-empty"    , "td-frog-empty"    ) ::
        ImageAsset("ld-frog-x5"       , "td-frog-x5"       ) ::
        ImageAsset("ld-frog-x5-empty" , "td-frog-x5-empty" ) ::
        ImageAsset("ld-peaceful"      , "td-peaceful"      ) ::
        ImageAsset("ld-militant"      , "td-militant"      ) ::

        ImageAsset("ld-action"        , "td-action"        ) ::
        ImageAsset("ld-action-done"   , "td-action-done"   ) ::

        ImageAsset("outcast-fox"         , "../fanatic/outcast-fox") ::
        ImageAsset("outcast-rabbit"      , "../fanatic/outcast-rabbit") ::
        ImageAsset("outcast-mouse"       , "../fanatic/outcast-mouse") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveCCC].any, "faction/invasive/card", lzy = Laziness.Later, scale = 50)(
        ImageAsset("frog-ambush") ::
        ImageAsset("favor-of-the-frogs-half") ::
        ImageAsset("frog-dominance-half") ::
        ImageAsset("imitation") ::
        ImageAsset("laborers") ::
        ImageAsset("peace-talks") ::
        ImageAsset("card-frog-back-art") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveCCC].any, "faction/invasive")(
        ImageAsset("ld-glyph"            , "td-glyph"            ) ::
        ImageAsset("ld-title"                                    ) ::
        ImageAsset("ld-char"             , "td-char"             ) ::
        ImageAsset("ld-board-c"                                  ) ::

        ImageAsset("ld-frog"          , "td-frog"          ) ::
        ImageAsset("ld-frog-empty"    , "td-frog-empty"    ) ::
        ImageAsset("ld-frog-x5"       , "td-frog-x5"       ) ::
        ImageAsset("ld-frog-x5-empty" , "td-frog-x5-empty" ) ::
        ImageAsset("ld-peaceful"      , "td-peaceful"      ) ::
        ImageAsset("ld-militant"      , "td-militant"      ) ::

        ImageAsset("ld-action"        , "td-action"        ) ::
        ImageAsset("ld-action-done"   , "td-action-done"   ) ::

        ImageAsset("outcast-fox"         , "../fanatic/outcast-fox") ::
        ImageAsset("outcast-rabbit"      , "../fanatic/outcast-rabbit") ::
        ImageAsset("outcast-mouse"       , "../fanatic/outcast-mouse") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveDDD].any, "faction/invasive/card", lzy = Laziness.Later, scale = 50)(
        ImageAsset("card-frog-back-art") ::
        ImageAsset("frog-ambush") ::
        ImageAsset("frog-dominance-half") ::
        ImageAsset("fox-laborers") ::
        ImageAsset("rabbit-laborers") ::
        ImageAsset("mouse-laborers") ::
        ImageAsset("peace-talks-flip") ::
        ImageAsset("incite-conflict") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveDDD].any, "faction/invasive")(
        ImageAsset("ld-glyph"            , "td-glyph"            ) ::
        ImageAsset("ld-title"                                    ) ::
        ImageAsset("ld-char"             , "td-char"             ) ::
        ImageAsset("ld-board-d"                                  ) ::

        ImageAsset("ld-frog"          , "td-frog"          ) ::
        ImageAsset("ld-frog-empty"    , "td-frog-empty"    ) ::
        ImageAsset("ld-frog-x5"       , "td-frog-x5"       ) ::
        ImageAsset("ld-frog-x5-empty" , "td-frog-x5-empty" ) ::
        ImageAsset("ld-peaceful"      , "td-peaceful"      ) ::
        ImageAsset("ld-militant"      , "td-militant"      ) ::

        ImageAsset("ld-action"        , "td-action"        ) ::
        ImageAsset("ld-action-done"   , "td-action-done"   ) ::

        ImageAsset("outcast-fox"         , "../fanatic/outcast-fox") ::
        ImageAsset("outcast-rabbit"      , "../fanatic/outcast-rabbit") ::
        ImageAsset("outcast-mouse"       , "../fanatic/outcast-mouse") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[LegalAAA].any, "faction/legal")(
        ImageAsset("tc-glyph"            ) ::
        ImageAsset("tc-title"            ) ::
        ImageAsset("tc-char"             ) ::
        ImageAsset("tc-board-a"          ).scaled(50) ::
        ImageAsset("tc-board-a-back"     ).scaled(50) ::

        ImageAsset("tc-bat"          ) ::
        ImageAsset("tc-bat-empty"    ) ::
        ImageAsset("tc-bat-x5"       ) ::
        ImageAsset("tc-bat-x5-empty" ) ::

        ImageAsset("tc-commune"      ) ::
        ImageAsset("tc-assembly"     ) ::
        ImageAsset("tc-convened"     ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[LegalAAA].any, "faction/legal/card", lzy = Laziness.Later)(
        ImageAsset("rouse-loyalists") ::
        ImageAsset("canvass-support") ::
        ImageAsset("overtime-debates") ::
        ImageAsset("pacificy-threats") ::
        ImageAsset("awaken-the-people") ::
        ImageAsset("might-makes-right") ::
        ImageAsset("intensify-pacifism") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[AbductAAA].any, "faction/abduct")(
        ImageAsset("kd-glyph"            ) ::
        ImageAsset("kd-title"            ) ::
        ImageAsset("kd-char"             ).scaled(20) ::
        ImageAsset("kd-board-a"          ) ::

        ImageAsset("kd-skunk"          ) ::
        ImageAsset("kd-skunk-empty"    ) ::
        ImageAsset("kd-skunk-x5"       ) ::
        ImageAsset("kd-skunk-x5-empty" ) ::

        ImageAsset("kd-captain-birdsong" ) ::
        ImageAsset("kd-captain-daylight" ) ::
        ImageAsset("kd-captain-evening"  ) ::

        ImageAsset("kd-captain-birdsong-empty" , "kd-captain-empty" ) ::
        ImageAsset("kd-captain-daylight-empty" , "kd-captain-empty" ) ::
        ImageAsset("kd-captain-evening-empty"  , "kd-captain-empty" ) ::
    $)

    override def intLinks = $(("Root: Advanced Setup".spn -> "root-adset"), ("Root: Mirror".spn -> "root-mirror"))
}
