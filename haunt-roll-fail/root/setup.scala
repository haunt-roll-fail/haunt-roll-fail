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
import root.elem._

case class StartAction(version : String) extends StartGameAction with GameVersion with JumpTo {
    def desc(l : $[Action]) = "Once upon a time...".hl
}

case class OptionsAction(players : $[Player], setup : $[Faction], options : $[Meta.O]) extends ForcedAction
case object SetupMapAction extends ForcedAction
case object DraftFactionsAction extends ForcedAction with Soft with OutOfTurn
case class FloodedAction(random : Clearing, then : ForcedAction) extends RandomAction[Clearing]
case class BlizzardAction(a : Clearing, b : Clearing, then : ForcedAction) extends RandomAction[(Clearing, Clearing)] { def random = (a, b) }
case class SaveMappingAction(shuffled : $[Clearing]) extends ShuffledAction[Clearing]
case class ThreeFourFiveMappingAction(shuffled : $[Clearing]) extends ShuffledAction[Clearing]
case class ThreeFourFiveSuitMappingAction(shuffled : $[BaseSuit], clearings : $[Clearing]) extends ShuffledAction[BaseSuit]

case object ReportMappingAction extends ForcedAction
case object MappingDoneAction extends ForcedAction
case object DraftCardsAction extends ForcedAction
case object DraftCardsDoneAction extends ForcedAction

case class DraftLimitAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class DraftLimitDiscardAction(f : Faction, l : $[DeckCard], then : ForcedAction) extends ForcedAction with Retry
case class ShuffleDeckAction(shuffled : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]

case object SetupLandmarksAction extends ForcedAction
case object SetupNextLandmarkAction extends ForcedAction
case class SetupLandmarkAction(self : Player, l : Landmark, more : Boolean) extends BaseAction(self, "sets up")(l) with Soft
case class SetupLandmarkClearingAction(self : Player, l : Landmark, c : Clearing) extends BaseAction("Place", l, "in")(c)

case object SetupHirelingsAction extends ForcedAction
case class ShuffleHirelingsAction(shuffled : $[Hireling]) extends ShuffledAction[Hireling]
case object SetupNextHirelingAction extends ForcedAction
case class SetupHirelingAction(self : Player, h : Hireling) extends BaseAction(self, "sets up")(h)

case object SetupFactionsAction extends ForcedAction
case class SetupNextFactionAction(indices : $[Int], remaining : $[Faction]) extends ForcedAction
case class SetupFactionIndexAction(f : Faction, n : Int) extends ForcedAction
case class SetupRandomFactionIndexAction(random : Faction, n : Int) extends RandomAction[Faction]
case class SetupFactionRandomIndexAction(f : Faction, random : Int) extends RandomAction[Int]

case class AddRuinItemsAction(then : ForcedAction) extends ForcedAction
case class ShuffleRuinItemsAction(shuffled : $[Item], then : ForcedAction) extends ShuffledAction[Item]

case class SelectFactionAction(self : Player, n : Int, f : PlayChoice) extends ForcedAction

case class CreatePlayerAction(f : Faction) extends ForcedAction
case class FactionInitAction(f : Faction) extends ForcedAction
case class FactionSetupAction(f : Faction) extends ForcedAction
case class StartingCornerAction(f : Faction) extends ForcedAction
case class ChooseCornerAction(self : Faction, r : Clearing) extends BaseAction(self, "starts in")(r)
case class StartingRegionAction(f : Faction) extends ForcedAction
case class StartingClearingAction(f : Faction, r : Clearing) extends ForcedAction
case class StartingForestAction(self : Faction, r : Forest) extends BaseAction(self, "starts in")(implicit g => board.forestName(r))


case class PlacePieceAction(self : Faction, r : Region, p : Piece, then : ForcedAction) extends BaseAction(self, "places", p.of(self), p.img(self), "in")(r)
case class PlacePiecesAction(self : Faction, r : Region, l : $[Piece], then : ForcedAction) extends BaseAction(self, "places", l./(_.img(self)), "in")(r)
case class PlacePieceClearingsAction(self : Faction, l : $[Clearing], p : Piece, then : ForcedAction) extends BaseAction(self, "places", l.num.times(p.img(self)), "in")(l.comma)


case class AfterSetupAction(f : Faction, then : ForcedAction) extends ForcedAction
case object SetupDoneAction extends ForcedAction
case object InitDoneAction extends ForcedAction


trait PlayChoice extends Record with Elementary {
    def faction : Faction
    def desc(implicit game : Game) : Elem
}

case class FactionChoice(faction : Faction) extends PlayChoice {
    def elem = faction.elem
    def desc(implicit game : Game) = game.desc(
        Image(faction.style + "-title")(styles.chartitle),
        Break,
        Image(faction.style + "-char")(styles.quest)(charstyles.get(faction)).div(styles.charblock) ~ faction.advertising,
        Break,
        faction.motto
    )
}

case class FactionCharacterChoice(faction : Hero, c : Character) extends PlayChoice {
    def elem = faction.elem ~ " " ~ c.name
    def desc(implicit game : Game) = game.desc(
        Image(c.title)(styles.chartitle),
        Break,
        Image(c.img)(styles.quest)((c == Ranger).?(styles.ranger))((c == Adventurer).?(styles.adventurer)).div(styles.charblock) ~
            c.starting(options)./(_.img).merge,
        Break,
        c.special./(_.of(faction))
    )
}

object PlayChoice {
    implicit def convertPlayChoice(f : PlayChoice)(implicit game : Game) = f.desc
}

case class PlayChoiceDescAction(obj : PlayChoice, remaining : $[Faction]) extends BaseInfo(Break ~ "Factions")(implicit g => obj.desc) with ViewObject[PlayChoice] with Selectable {
    val self = Neutral
    def selected = remaining.has(obj.faction).not
}


object SetupExpansion extends MandatoryExpansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // INIT
        case StartAction(version) =>
            log("HRF".hl, "version", gaming.version.hlb)
            log(Header(1, "Log", $(ExternalStyle("screen-reader"))) ~ "Root: A Game of Woodland Might and Right".hlb)

            if (version != gaming.version)
                log("Saved game version", version.hlb)

            Milestone(OptionsAction(game.players, game.candidates, game.options))

        case OptionsAction(players, candidates, options) =>
            if (players.of[Faction].any)
                log("Factions", players.of[Faction]./(_.elem).comma)

            options.foreach { o =>
                log(o.group, o.valueOn)
            }

            if (options.has(SeatingGiven))
                Force(SeatingOrderAction(players))
            else
                Shuffle(players, SeatingOrderAction)

        case SeatingOrderAction(t) =>
            game.seating = t

            log("Seating", game.seating./(_.elem).comma)

            Milestone(SetupMapAction)

        case SetupMapAction if board == TidalBoard && game.flooded.none =>
            Random[Clearing](board.inner, c => FloodedAction(c, SetupMapAction))

        case FloodedAction(c, then) =>
            log(c.name.styled(ED), "was", "flooded".hl ~ ",", "water connected", board.connected(c).dropRight(1)./(_.name.hl).comma, "and", board.connected(c).last.name.hl)

            game.flooded :+= c

            then

        case SetupMapAction if options.has(DefaultClearings) && board == AutumnBoard =>
            game.original = AutumnBoard.defaultMapping
            game.mapping = game.original

            game.ruins = board.ruins.intersect(clearings)./(_ -> Ruins($)).toMap

            MappingDoneAction

        case SetupMapAction if options.has(AllRandomClearings) =>
            Shuffle[Clearing](clearings, SaveMappingAction(_))

        case SetupMapAction if options.has(NoClustersClearings) =>
            ShuffleUntil[Clearing](clearings, SaveMappingAction(_), l => {
                val mapping = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)).toMap

                def z(c : Clearing) = game.connected(c).%(mapping(_) == mapping(c)).num

                clearings./(z).max == 0
            })

        case SetupMapAction if options.has(SuitPairsClearings) =>
            ShuffleUntil[Clearing](clearings, SaveMappingAction(_), l => {
                val mapping = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)).toMap

                def z(c : Clearing) = game.connected(c).%(mapping(_) == mapping(c)).num

                clearings./(z).%(_ != 1).none
            })

        case SetupMapAction if options.has(ConnectedClearings) =>
            ShuffleUntil[Clearing](clearings, SaveMappingAction(_), l => {
                val mapping = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)).toMap

                def z(c : Clearing) = game.connected(c).%(mapping(_) == mapping(c)).num

                clearings./(z).min > 0 && FoxRabbitMouse.%(s => board.clearings.%(mapping(_) == s)./(z).max < 2).none
            })

        case SetupMapAction if options.has(ThreeFourFiveClearings) =>
            Shuffle[Clearing](clearings, ThreeFourFiveMappingAction(_))

        case ThreeFourFiveMappingAction(l) =>
            Shuffle[BaseSuit](FoxRabbitMouse, ThreeFourFiveSuitMappingAction(_, l))

        case ThreeFourFiveSuitMappingAction(s, l) =>
            game.original = l.sortBy(c => c.capacity * 2 - board.ruins.has(c).??(1)).zip(5.times($(s(0))) ++ 4.times($(s(1))) ++ 3.times($(s(2)))).toMap
            game.mapping = game.original

            ReportMappingAction

        case SaveMappingAction(l) =>
            game.original = l.zip($(Fox, Fox, Fox, Fox, Rabbit, Rabbit, Rabbit, Rabbit, Mouse, Mouse, Mouse, Mouse)./($)).toMap
            game.mapping = game.original

            ReportMappingAction

        case ReportMappingAction =>
            clearings.foreach { c =>
                log(c, "was", game.mapping(c))
            }

            game.ruins = board.ruins.intersect(clearings)./(_ -> Ruins($)).toMap

            MappingDoneAction

        case MappingDoneAction if board == TundraBoard && game.blizzard.none =>
            Random[(Clearing, Clearing)](board.blizzard, ab => BlizzardAction(ab._1, ab._2, MappingDoneAction))

        case BlizzardAction(a, b, then) =>
            log("Path between", a, "and", b, "was blocked by", "blizzard".hl)

            game.blizzard = $((a, b))

            then

        case MappingDoneAction =>
            SetupLandmarksAction

        case SetupLandmarksAction =>
            if (options.of[Landmark].any)
                SetupNextLandmarkAction
            else
            {
                if (options.has(MapDefaultLandmarks)) {
                    game.ferry = board.ferry
                    game.tower = board.tower
                }

                SetupHirelingsAction
            }

        case SetupNextLandmarkAction =>
            val landmarks = options.of[Landmark]
            val done = landmarks.%{
                case FerryLandmark => game.ferry.any
                case LostCityLandmark => game.lostCity.any
                case TowerLandmark => game.tower.any
            }

            val remaining = landmarks.diff(done)

            if (remaining.any) {
                val f = landmarks./~(_ => game.seating.reverse).drop(done.num)(0)

                Ask(f)
                    .each(landmarks)(l => SetupLandmarkAction(f, l, remaining.num > 1).!(done.has(l)))
                    .needOk
            }
            else
                SetupHirelingsAction

        case SetupLandmarkAction(f, FerryLandmark, more) =>
            val l = game.riverside.diff(game.landmarks)

            Ask(f).each(l.diff(game.landmarks./~(game.connected)).some.|(l))(c => SetupLandmarkClearingAction(f, FerryLandmark, c)).cancelIf(more)

        case SetupLandmarkClearingAction(f, FerryLandmark, c) =>
            game.ferry :+= c

            log(f, "placed", FerryLandmark, "in", c)

            SetupNextLandmarkAction

        case SetupLandmarkAction(f, TowerLandmark, more) =>
            Ask(f).each(game.ruins.keys.$.diff(game.landmarks))(c => SetupLandmarkClearingAction(f, TowerLandmark, c)).cancelIf(more)

        case SetupLandmarkClearingAction(f, TowerLandmark, c) =>
            game.tower :+= c

            log(f, "placed", TowerLandmark, "in", c)

            SetupNextLandmarkAction

        case SetupLandmarkAction(f, LostCityLandmark, more) =>
            Ask(f).each(game.riverside.diff(game.landmarks).diff(game.landmarks./~(board.connected)))(c => SetupLandmarkClearingAction(f, LostCityLandmark, c)).cancelIf(more)

        case SetupLandmarkClearingAction(f, LostCityLandmark, c) =>
            game.lostCity :+= c

            game.mapping += c -> FoxRabbitMouse
            game.original = game.mapping

            log(f, "placed", LostCityLandmark, "in", c)

            SetupNextLandmarkAction

        case SetupHirelingsAction =>
            if (options.has(NoHirelings))
                DraftCardsAction
            else
                Shuffle[Hireling](options.of[IncludeHireling]./(_.hireling), ShuffleHirelingsAction(_))

        case ShuffleHirelingsAction(l) =>
            val (normal, demoted, total) = options.of[HirelingsOption].only @@ {
                case ThreeDemotedHirelings => (0, 3, 3)
                case TwoDemotedOneNormalHirelings => (1, 2, 3)
                case OneDemotedTwoNormalHirelings => (2, 1, 3)
                case ThreeNormalHirelings => (3, 0, 3)
                case ThreeRandomHirelings => (3, 3, 3)
                case SelectedHirelings => (999, 999, 999)
            }

            l.foreach { h =>
                if (game.unhired.num < total) {
                    if ((h.demoted && game.unhired.%(_.demoted).num < demoted) || (h.demoted.not && game.unhired.%(_.demoted.not).num < normal)) {
                        if (options.has(HirelingHirelingClash) || options.has(FactionHirelingClash) || game.unhired./(_.clashKey).has(h.clashKey).not) {
                            if (options.has(FactionHirelingClash) || game.candidates.%(_.clashKey == h.clashKey).none || game.candidates.%(_.clashKey.in(h.clashKey, game.unhired./(_.clashKey)).not).num > players.num) {
                                game.unhired :+= h
                            }
                        }
                    }
                }
            }

            if (game.unhired.any)
                log("Hirelings drawn", game.unhired./(_.elem).comma)
            else
                log("No Hirelings drawn")

            game.thresholds = 1.to(game.unhired.num)./(_ * 4)

            if (game.unhired.any)
                log("Thresholds for hire", game.thresholds./(_.vp).comma)

            SetupNextHirelingAction

        case SetupNextHirelingAction =>
            val l = game.unhired.%(h => game.states.contains(h).not)

            if (l.any) {
                val f = game.unhired./~(_ => game.seating).reverse.drop(game.unhired.num - l.num)(0)

                if (l.%(_.setup).any)
                    Ask(f)(l.%(_.setup)./(SetupHirelingAction(f, _))).needOk
                else
                    Force(SetupHirelingAction(f, l(0)))
            }
            else
                DraftCardsAction

        case DraftCardsAction if options.has(CardDraftFive) =>
            if (deck.num < game.arity * 5 && pile.any)
                Shuffle[DeckCard](pile, ShufflePileAction(_, DraftCardsAction))
            else {
                val pp = players.%(p => game.predrafts(p).none)

                if (pp.any) {
                    val p = pp(0)

                    val l = deck.get.take(5)

                    deck --> l --> game.predrafts(p)

                    log(p, "drew", 5.of("card"))

                    p.notify(l./(d => ViewCardInfoAction(p, p.elem ~ " drew " ~ 5.cards, d)))

                    DraftCardsAction
                }
                else
                    DraftCardsDoneAction
            }

        case DraftCardsAction =>
            DraftCardsDoneAction

        case DraftCardsDoneAction =>
            DraftFactionsAction

        case DraftFactionsAction =>
            val factions =
                if (options.has(FactionHirelingClash))
                    game.candidates
                else
                    game.candidates.%(_.clashKey.in(game.unhired./(_.clashKey)).not)

            val militant = factions.of[Feline] ++ factions.of[Aviary] ++ factions.of[Underground] ++ factions.of[Horde] ++ factions.of[Expedition]

            val combinations =
                if (factions.num == players.num)
                    $(game.candidates)
                else
                if (militant.any)
                    militant./~(m => factions.but(m)./~(l => factions.but(m).but(l).combinations(players.num - 1)./(c => m +: (c :+ l))))
                else
                    factions.combinations(players.num + 1).$

            Random[$[Faction]](combinations, t => FactionsCombinationAction(t))

        case FactionsCombinationAction(t) =>
            game.drawn = t

            if (options.has(RandomCharacter))
                Shuffle[Character](HeroExpansion.characters(options), t => CharactersCombinationAction(t))
            else
                Milestone(SetupFactionsAction)

        case CharactersCombinationAction(t) =>
            game.drawn.of[Hero].lazyZip(t).foreach((h, c) => game.chars += h -> c)

            log("Drawn factions", game.playChoices./(_.elem).comma)

            Milestone(SetupFactionsAction)

        case SetupFactionsAction =>
            SetupNextFactionAction(0.until(players.num).%!(game.ordering.contains).$, game.candidates.%!(game.states.contains))

        case SetupNextFactionAction(Nil, _) =>
            game.factions = 0.until(players.num)./(game.ordering)

            SetupDoneAction

        case SetupNextFactionAction(indices, remaining) if options.has(FactionSeatingGiven) && options.has(SetupOrderPriority) =>
            val f = remaining.minBy(_.priority)
            val n = game.candidates.indexOf(f)

            SetupFactionIndexAction(f, n)

        case SetupNextFactionAction(indices, remaining) if options.has(FactionSeatingGiven) && options.has(SetupOrderReverse) =>
            val n = indices.max
            val f = game.candidates(n)

            SetupFactionIndexAction(f, n)

        case SetupNextFactionAction(indices, remaining) if options.has(FactionSeatingRandom) && options.has(SetupOrderPriority) =>
            val f = remaining.minBy(_.priority)

            Random(indices, SetupFactionRandomIndexAction(f, _))

        case SetupNextFactionAction(indices, remaining) if options.has(FactionSeatingRandom) && options.has(SetupOrderReverse) =>
            val n = indices.max

            val l = game.drawn./(f => f.as[Hero]./(h => FactionCharacterChoice(h, game.chars(h))).|(FactionChoice(f)))

            YYSelectObjectsAction(game.seating(n), l)
                .withGroup("Choose faction")
                .withRule(l => remaining.has(l.faction))
                .withThen(f => SelectFactionAction(game.seating(n), n, f))("Play as " ~ _.elem)("~~~")

        case SetupNextFactionAction(indices, remaining) =>
            +++(indices)
            +++(remaining)
            +++(options)

            throw new Error("hmm")

        case SelectFactionAction(p, n, f) =>
            game.ftp += f.faction -> p
            game.ptf += p -> f.faction

            log(p, "played", f)

            SetupFactionIndexAction(f.faction, n)

        case SetupRandomFactionIndexAction(f, n) =>
            SetupFactionIndexAction(f, n)

        case SetupFactionRandomIndexAction(f, n) =>
            SetupFactionIndexAction(f, n)

        case SetupFactionIndexAction(f, n) =>
            game.ordering += n -> f

            Milestone(CreatePlayerAction(f))

        case InitDoneAction =>
            game.expansions = game.expansionsFor(factions)

            StartPlayerTurnAction(factions.first)

        case FactionInitAction(f) =>
            game.factions :+= f

            if (game.predrafts.get(game.ftp(f))./~(_.get).any) {
                game.predrafts(game.ftp(f)) --> f.hand

                FactionSetupAction(f)
            }
            else
                DrawCardsAction(f, 3, NotInLog(OnSetup), AddCardsAction(f, FactionSetupAction(f)))

        case StartingCornerAction(f) =>
            val cc = game.corners./~(board.opposite).diff(game.corners).some.|(board.corners.diff(game.corners))

            Ask(f).each(cc)(ChooseCornerAction(f, _)).needOk

        case ChooseCornerAction(f, r) =>
            game.corners :+= r
            StartingClearingAction(f, r)

        case AfterSetupAction(f : Hero, then) =>
            AddRuinItemsAction(then)

        case AfterSetupAction(f : Horde, then) if options.has(SeparateItemsInRuins) =>
            AddRuinItemsAction(then)

        case AfterSetupAction(f : Horde, then) if factions.of[Hero].none && factions.of[Horde].starting.has(f) =>
            AddRuinItemsAction(then)

        case AfterSetupAction(f : RF.type, then) if options.has(MixedDeck) && deck.$.of[CraftEffectCard].exists(_.effect == BoatBuilders) =>
            deck --> deck.$.of[CraftEffectCard].%(_.effect == BoatBuilders) --> game.outOfGameCards

            then

        case AfterSetupAction(f : CommonInvasive, then) if options.has(MixedDeck) && deck.$.of[CraftEffectCard].exists(_.effect == BoatBuilders) =>
            deck --> deck.$.of[CraftEffectCard].%(_.effect == BoatBuilders) --> game.outOfGameCards

            then

        case AfterSetupAction(f : Mischief, then) if options.has(MixedDeck) && deck.$.of[CraftEffectCard].exists(_.effect == CorvidPlanners) =>
            deck --> deck.$.of[CraftEffectCard].%(_.effect == CorvidPlanners) --> game.outOfGameCards

            then

        case AfterSetupAction(f, then) =>
            then

        case AddRuinItemsAction(then) =>
            Shuffle[Item]($(Boots, Sword, Hammer, Bag), ShuffleRuinItemsAction(_, then))

        case ShuffleRuinItemsAction(l, then) =>
            game.ruins.keys.lazyZip(l).foreach { (r, i) =>
                game.ruins += r -> Ruins(game.ruins(r).items :+ i)
            }

            then

        case SetupDoneAction =>
            val n = 3

            val remaining = factions.%(_.hand.num > n)

            if (remaining.any) {
                MultiAsk(remaining./(f =>
                    XXSelectObjectsAction(f, f.hand)
                        .withGroup(f.elem ~ " keeps " ~ n.cards)
                        .withRule(_.num(n))
                        .withThen(l => DraftLimitDiscardAction(f, f.hand.diff(l), SetupDoneAction))(l => "Keep".hl ~ l./(" " ~ _.elem))
                        .withExtra($(NoHand))
                        .perform(soft)
                ))
            }
            else
                (factions ++ hirelings).foldRight(InitDoneAction : ForcedAction)((f, q) => AfterSetupAction(f, q))

        case DraftLimitDiscardAction(f, l, then) =>
            f.hand --> l --> deck

            Shuffle(deck, ShuffleDeckAction(_, then))

        case ShuffleDeckAction(l, then) =>
            deck --> l --> deck

            then

        case PlacePieceAction(f, r, p, then) =>
            f.reserve --> p --> r

            f.log("placed", p.of(f), "in", r)

            then

        case PlacePiecesAction(f, r, l, then) =>
            l.foreach(p => f.reserve --> p --> r)

            f.log("placed", l./(_.of(f)).comma, "in", r)

            then

        case PlacePieceClearingsAction(f, l, p, then) =>
            l.foreach(r => f.reserve --> p --> r)

            l.distinct.foreach(r => f.log("placed", l.count(r).times(p.of(f)).comma, "in", r))

            then

        case _ => UnknownContinue
    }

}
