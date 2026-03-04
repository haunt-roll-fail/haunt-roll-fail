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

import hrf.tracker4._
import hrf.tracker4.implicits._
import hrf.elem._

import arcs.elem._

import scala.collection.immutable.ListMap


case class ArcsStartAction(version : String, factions : $[Faction], options : $[hrf.meta.GameOption]) extends ForcedAction
case class ArcsBlightedReachStartAction(version : String, factions : $[Faction], options : $[hrf.meta.GameOption]) extends ForcedAction with SkipValidate
case class ArcsBlightedReachFromActTwoAction(version : String, factions : $[Faction], options : $[hrf.meta.GameOption]) extends ForcedAction with SkipValidate

case class GameOverAction(winner : Faction) extends ForcedAction // OBSOLETE
case class ArcsGameOverAction(version : String, factions : $[Faction], options : $[hrf.meta.GameOption], winner : Faction, powers : ListMap[Faction, Int], chapter : Int) extends ForcedAction
case class ArcsBlightedReachGameOverAction(version : String, factions : $[Faction], options : $[hrf.meta.GameOption], winner : Faction, underdog : Boolean, chapter : Int, powers : ListMap[Faction, Int], fates : ListMap[Faction, $[Fate]]) extends ForcedAction

case class StartAction(version : String) extends StartGameAction with GameVersion
case class PlayOrderAction(random : $[Faction]) extends RandomAction[$[Faction]]
case object StartSetupAction extends ForcedAction
case object RandomizeAction extends ForcedAction
case object RandomizeRerollAction extends ForcedAction
case object RandomizePlanetResourcesAction extends ForcedAction
case class PlanetResourcesAction(shuffled : $[Resource], then : ForcedAction) extends ShuffledAction[Resource]
case object RandomizeStartingSystemsAction extends ForcedAction
case class StartingSystemsAction(shuffled : $[System], then : ForcedAction) extends ShuffledAction[System]
case object CourtSetupAction extends ForcedAction
case class ShuffleCourtDiscardAction(then : ForcedAction) extends ForcedAction
case class ShuffleCourtDeckAction(then : ForcedAction) extends ForcedAction
case class ShuffledCourtDeckAction(shuffled : $[CourtCard], then : ForcedAction) extends ShuffledAction[CourtCard]
case class CheckCourtScrapAction(then : ForcedAction) extends ForcedAction
case class ReplenishMarketAction(then : ForcedAction) extends ForcedAction
case object FactionsSetupAction extends ForcedAction
case object BaseFactionsSetupAction extends ForcedAction
case class BaseFactionSetupAction(f : Faction) extends ForcedAction
case object StartChapterAction extends ForcedAction
case object CheckWinAction extends ForcedAction
case object DealCardsAction extends ForcedAction
case object ShuffleDeckAction extends ForcedAction
case class ShuffledDeckCardsAction(shuffled : $[DeckCard]) extends ShuffledAction[DeckCard]
case object StartRoundAction extends ForcedAction
case class LeadMainAction(self : Faction) extends ForcedAction with Soft
case class LeadAction(self : Faction, d : DeckCard, suit : Suit) extends ForcedAction
case class PassAction(self : Faction) extends ForcedAction
case class CheckAmbitionAction(self : Faction, d : ActionCard) extends ForcedAction with Soft
case class FollowAction(self : Faction) extends ForcedAction with Soft
case class SurpassAction(self : Faction, d : DeckCard, suit : Suit) extends ForcedAction
case class CopyAction(self : Faction, d : DeckCard, suit : Suit) extends ForcedAction
case class PivotAction(self : Faction, d : DeckCard, suit : Suit) extends ForcedAction
case class MirrorAction(self : Faction, d : DeckCard, suit : Suit) extends ForcedAction
case class CheckSeizeAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class SeizeAction(self : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class DeclareAmbitionMainAction(self : Faction, effect : |[Effect], ambitions : $[Ambition], m : AmbitionMarker, zero : Boolean, faithful : Boolean, extra : $[UserAction], then : ForcedAction) extends ForcedAction with Soft
case class DeclareAmbitionAction(self : Faction, ambition : Ambition, m : AmbitionMarker, zero : Boolean, faithful : Boolean, then : ForcedAction) extends ForcedAction
case class AmbitionDeclaredAction(self : Faction, ambition : Ambition, used : $[Effect], then : ForcedAction) extends ForcedAction with Soft
case class PrePreludeActionAction(self : Faction, suit : Suit, pips : Int) extends ForcedAction
case class PreludeActionAction(self : Faction, suit : Suit, pips : Int) extends ForcedAction
case class GiveGuildCardAction(self : Faction, e : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction

case class TryHarmAction(self : Faction, e : Color, s : System, reserved : Cost, then : ForcedAction) extends ForcedAction with Soft
case class HarmAction(self : Faction, e : Color, s : System, then : ForcedAction) extends ForcedAction

case class StealResourceAction(self : Faction, e : Faction, x : ResourceLike, k : ResourceSlot, then : ForcedAction) extends ForcedAction
case class AdjustResourcesAction(then : ForcedAction) extends ForcedAction
case class ContinueAdjustResourcesAction(then : ForcedAction) extends ForcedAction with Soft

case class AdjustingResourcesAction(self : Faction, l : $[|[ResourceLike]], then : ForcedAction) extends HiddenChoice with Soft with NoExplode with SkipValidate // ???
case class ExplodeReorderResourcesAction(self : Faction, slots : $[ResourceSlot], list : $[|[ResourceLike]], then : ForcedAction) extends HiddenChoice with SelfExplode with SelfValidate {
    def validate(target : Action) = target @@ {
        case AdjustingResourcesAction(f, m, t) => self == f && then == t && m.toSet.equals(list.toSet)
        case ReorderResourcesAction(f, m, t) => self == f && then == t && m.toSet.equals(list.toSet)
        case _ => false
    }

    def explode(withSoft : Boolean)(implicit game : Game) = {
        val n = slots.notOf[Overflow].num

        val multi = slots.distinct.num < slots.num

        val keys = slots./(s => slots.indexOf(s) * 10000)

        val c = {
            var l : Set[$[|[ResourceLike]]] = Set(list)
            var ii = 0

            while (ii < 2 * n) {
                val i = ii /↓ 2
                // val i = ii % n

                val m = (i + 1).until(list.num)./~(j => l./~(c => (slots(i) != slots(j) && self.canExchangeSlotContents((c(i), slots(i)), (c(j), slots(j)))).?(c.updated(i, c(j)).updated(j, c(i)))))

                l = l ++ m.useIf(multi)(_./(_.zipWithIndex.sortBy { case (r, i) => keys(i) + r./(_.order).|(9999) }.lefts)/*.distinct*/)

                // l = l.distinct

                ii += 1
            }

            l
        }

        val pp = c./(_.zip(slots))
            .%! { pairs =>
                pairs.drop(n).exists(a => pairs.take(n).exists(b => a._1.any && b._1.none && self.canExchangeSlotContents(a, b)))
            }

        val cc = pp./(_.lefts)

        cc.$./(ReorderResourcesAction(self, _, then).as("Done")) ++ withSoft.??(c.$./(AdjustingResourcesAction(self, _, then)))
    }
}
case class ReorderResourcesAction(self : Faction, l : $[|[ResourceLike]], then : ForcedAction) extends ForcedAction

case class PayCostAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction

case class TaxMainAction(self : Faction, cost : Cost, effect : |[Effect], then : ForcedAction) extends ForcedAction with Soft
case class TaxAction(self : Faction, cost : Cost, effect : |[Effect], s : System, c : |[Figure], loyal : Boolean, then : ForcedAction) extends ForcedAction
case class TaxGainAction(self : Faction, r : |[Resource], along : Boolean, then : ForcedAction) extends ForcedAction
case class TaxTakeAction(self : Faction, r : ResourceLike, e : Faction, then : ForcedAction) extends ForcedAction
case class PostTaxAction(self : Faction, s : System, c : |[Figure], loyal : Boolean, then : ForcedAction) extends ForcedAction

case class OutrageAction(self : Faction, r : Resource, effect : |[Effect], then : ForcedAction) extends ForcedAction
case class ClearOutrageAction(self : Faction, l : $[Resource], then : ForcedAction) extends ForcedAction

case class DiscardResourcesMainAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class DiscardResourceNoEffectAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction

case class DiscardGuildCardsMainAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class DiscardGuildCardNoEffectAction(self : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction

case class BuildMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class BuildCityAction(self : Faction, cost : Cost, s : System, effect : |[Effect], then : ForcedAction) extends ForcedAction
case class BuildFreeCityAction(self : Faction, cost : Cost, s : System, effect : |[Effect], then : ForcedAction) extends ForcedAction
case class BuildStarportAction(self : Faction, cost : Cost, s : System, then : ForcedAction) extends ForcedAction
case class BuildFreeStarportAction(self : Faction, cost : Cost, s : System, then : ForcedAction) extends ForcedAction
case class BuildShipAction(self : Faction, cost : Cost, s : System, b : Figure, effect : |[Effect], then : ForcedAction) extends ForcedAction
case class BuildFlagshipAction(self : Faction, cost : Cost, u : FlagshipUpgrade, p : Building, then : ForcedAction) extends ForcedAction

case class RepairMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class RepairAction(self : Faction, cost : Cost, s : System, u : Figure, then : ForcedAction) extends ForcedAction
case class RepairFlagshipAction(self : Faction, cost : Cost, q : FlagshipUpgrade, u : Figure, then : ForcedAction) extends ForcedAction

case class InfluenceMainAction(self : Faction, cost : Cost, effect : |[Effect], skip : Boolean, cancel : Boolean, then : ForcedAction) extends ForcedAction with Soft
case class InfluenceAction(self : Faction, cost : Cost, index : Int, effect : |[Effect], then : ForcedAction) extends ForcedAction
case class MayInfluenceAction(self : Faction, effect : |[Effect], then : ForcedAction) extends ForcedAction with ThenDesc { def desc = "(then may " ~ "Influence".hh ~ ")" }

case class SecureMainAction(self : Faction, cost : Cost, effect : |[Effect], skip : Boolean, cancel : Boolean, then : ForcedAction) extends ForcedAction with Soft
case class SecureAction(self : Faction, cost : Cost, index : Int, effect : |[Effect], then : ForcedAction) extends ForcedAction
case class MustSecureAction(self : Faction, effect : |[Effect], then : ForcedAction) extends ForcedAction with ThenDesc { def desc = "(then must " ~ "Secure".hh ~ ")" }
case class MaySecureAction(self : Faction, effect : |[Effect], then : ForcedAction) extends ForcedAction with ThenDesc { def desc = "(then may " ~ "Secure".hh ~ ")" }

case class AddBattleOptionAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction

case class ReserveCardMainAction(self : Faction, c : GuildCard, l : $[DeckCard], then : ForcedAction) extends ForcedAction with Soft
case class ReserveCardAction(self : Faction, c : GuildCard, d : DeckCard, then : ForcedAction) extends ForcedAction

case class LatticeSeizeAction(self : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction

case class FreeCityAction(self : Faction, s : System, u : Figure, then : ForcedAction) extends ForcedAction
case class FreeCitySeizeAskAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class FreeCitySeizeAction(self : Faction, then : ForcedAction) extends ForcedAction

case class OutrageSpreadsAction(self : Faction, r : Resource, then : ForcedAction) extends ForcedAction

case class GainCourtCardAction(self : Faction, c : CourtCard, lane : Int, main : Boolean, then : ForcedAction) extends ForcedAction
case class CaptureAgentsCourtCardAction(self : Faction, lane : Int, then : ForcedAction) extends ForcedAction
case class ExecuteAgentsCourtCardAction(self : Faction, lane : Int, then : ForcedAction) extends ForcedAction
case class ReturnAgentsCourtCardAction(lane : Int, then : ForcedAction) extends ForcedAction
case class GiveCourtCardAction(self : Faction, c : CourtCard, from : Faction, then : ForcedAction) extends ForcedAction
case class DiscardVoxCardAction(self : Faction, c : VoxCard, then : ForcedAction) extends ForcedAction
case class DiscardPreludeGuildCardAction(self : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction
case class DiscardGuildCardAction(self : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction
case class BuryVoxCardAction(self : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction

case class UseEffectAction(self : Faction, c : Effect, then : ForcedAction) extends ForcedAction
case class ClearEffectAction(self : Faction, c : Effect, then : ForcedAction) extends ForcedAction

case class EndPreludeAction(self : Faction, suit : Suit, done : Int, total : Int) extends ForcedAction
case class MainTurnAction(self : Faction, suit : Suit, done : Int, total : Int) extends ForcedAction
case class EndTurnAction(self : Faction) extends ForcedAction
case class CheckNoFleetAction(self : Faction, then : ForcedAction) extends ForcedAction

case object EndRoundAction extends ForcedAction
case class TransferInitiativeAction(f : Faction) extends ForcedAction
case object ContinueRoundsAction extends ForcedAction
case object EndChapterAction extends ForcedAction
case object ScoreChapterAction extends ForcedAction
case object ScoreAmbitionsAction extends ForcedAction
case class CheckGrandAmbitionsAction(self : Faction, then : ForcedAction) extends ForcedAction
case object ScoreGrandAmbitionsAction extends ForcedAction
case class CleanUpChapterAction(then : ForcedAction) extends ForcedAction
case class CartelCleanUpAction(then : ForcedAction) extends ForcedAction

case class SecureWith(e : |[Effect]) extends Message {
    def elem(implicit game : Game) = game.desc("Secure".hl, e./("with" -> _))
}

case class BattleWith(e : |[Effect]) extends Message {
    def elem(implicit game : Game) = game.desc("Battle".hl, e./("with" -> _))
}

case class DeadlockAction(self : Faction, message : Message, then : ForcedAction) extends BaseAction(Empty)(Empty)
case class CheatAction(self : Faction, message : Message, then : ForcedAction) extends BaseAction(self, "made an illegal move, must", message, "but cannot", Break, "Please", "UNDO".hlb, "your move")(Empty) with Choice


case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won", "(" ~ NameReference(f.name, f).hl ~ ")")


object CommonExpansion extends Expansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // INIT
        case StartAction(version) =>
            log("HRF".hl, "version", gaming.version.hlb)
            log("Arcs: Conflict and Collapse in the Reach".hlb.styled(styles.title))

            if (version != gaming.version)
                log("Saved game version", version.hlb)

            options.foreach { o =>
                log(o.group, o.valueOn)
            }

            if (campaign)
                if (options.has(SetupActTwo))
                    Then(ArcsBlightedReachFromActTwoAction(version, game.seating, options))
                else
                    Then(ArcsBlightedReachStartAction(version, game.seating, options))
            else
                Then(ArcsStartAction(version, game.seating, options))

        case ArcsStartAction(_, _, _) =>
            game.setup.foreach { f =>
                game.states += f -> new FactionState(f)

                f.recalculateSlots()
            }

            if (true) {
                game.states += Blights -> new BlightsState(Blights)
                game.states += Empire -> new EmpireState(Empire)
                game.states += Free -> new FreeState(Free)
                game.states += Neutrals -> new NeutralsState(Neutrals)
            }

            if (options.has(RandomPlayerOrder))
                Random[$[Faction]](game.setup.permutations.$, PlayOrderAction(_))
            else
                Random[$[Faction]](game.setup./(f => game.setup.dropWhile(_ != f) ++ game.setup.takeWhile(_ != f)), PlayOrderAction(_))

        case ArcsBlightedReachStartAction(_, _, _) =>
            game.setup.foreach { f =>
                game.states += f -> new FactionState(f)

                f.recalculateSlots()
            }

            game.states += Blights -> new BlightsState(Blights)
            game.states += Empire -> new EmpireState(Empire)
            game.states += Free -> new FreeState(Free)
            game.states += Neutrals -> new NeutralsState(Neutrals)

            if (options.has(RandomPlayerOrder))
                Random[$[Faction]](game.setup.permutations.$, PlayOrderAction(_))
            else
                Random[$[Faction]](game.setup./(f => game.setup.dropWhile(_ != f) ++ game.setup.takeWhile(_ != f)), PlayOrderAction(_))

        case PlayOrderAction(l) =>
            game.seating = l

            game.factions = game.seating

            game.current = l.starting

            l.first.log("randomly took initiative")

            log("Play order", l.comma)

            RandomizeAction

        case RandomizeAction =>
            val rpr = options.has(RandomizePlanetResources)
            val rss = options.has(RandomizeStartingSystems)

            if (rpr && game.overridesSoft.none)
                RandomizePlanetResourcesAction
            else
            if (rss && game.starting.none)
                RandomizeStartingSystemsAction
            else
            if (rpr || rss)
                MultiAsk(factions./(f => Ask(f).group("Randomize Setup").add(StartSetupAction.as("Confirm".hh)).add(RandomizeRerollAction.as("Re-Roll".hh))))
            else
                Milestone(StartSetupAction)

        case RandomizeRerollAction =>
            log("Re-Roll")

            game.overridesSoft = Map()
            game.starting = $

            RandomizeAction

        case RandomizePlanetResourcesAction =>
            ShuffleTakeUntil[Resource](Resources.all./~(max(game.setup.num, board.systems.%!(_.gate).num /↑ 5).times), board.systems.%!(_.gate).num, l => (l ++ l.take(2)).sliding(3).forall(_.distinct.num == 3), PlanetResourcesAction(_, RandomizeAction))

        case PlanetResourcesAction(l, then) =>
            board.systems.%!(_.gate).lazyZip(l).foreach { case (s, r) =>
                game.overridesSoft += s -> r

                log(s, "was", r)
            }

            then

        case RandomizeStartingSystemsAction =>
            Shuffle[System](board.systems, StartingSystemsAction(_, RandomizeAction))

        case StartingSystemsAction(l, then) =>
            val aa = l.%(_.gate.not).take(game.setup.num)
            val bb = l.%(_.gate.not).reverse.take(game.setup.num)
            val cc = l.%(_.gate)

            game.starting = game.seating.lazyZip(aa).lazyZip(bb).lazyZip(cc).map { (f, a, b, c) =>
                log(f, "started in", a, Comma, b, Comma, c)

                (a, b, $(c))
            }

            then

        case CourtSetupAction =>
            ShuffleCourtDiscardAction(ReplenishMarketAction(FactionsSetupAction))

        case ShuffleCourtDiscardAction(then) =>
            game.discourt --> game.discourt.of[GuildCard] --> game.court

            if (game.chapter > 0)
                log("Guild cards from court discard were added to the court deck")

            ShuffleCourtDeckAction(then)

        case ShuffleCourtDeckAction(then) =>
            Shuffle[CourtCard](game.court, ShuffledCourtDeckAction(_, then))

        case ShuffledCourtDeckAction(l, then) =>
            game.court --> l --> game.court

            log("The court deck was shuffled")

            Then(then)

        case ReplenishMarketAction(then) =>
            game.market.foreach { m =>
                if (m.none) {
                    Influence(m.index).$.some./ { l =>
                        l.foreach { u =>
                            u --> u.faction.reserve
                        }

                        log("Returned agents", l.comma, "from the empty slot")
                    }
                }
            }

            game.market.foreach { m =>
                if (m.none) {
                    if (game.court.any) {
                        game.court.take(1) --> m

                        if (game.chapter > 0)
                            log("A card was added to the market")
                    }
                    else
                        log("No cards to refill the court")
                }
            }

            if (game.chapter == 0)
                log("Cards were added to the market")

            CheckCourtScrapAction(then)

        case FactionsSetupAction if options.has(LeadersAndLore).not =>
            BaseFactionsSetupAction

        case FactionsSetupAction =>
            val leaders = options.has(LeadersAndLore).??(Leaders.all)

            var lores = options.has(LeadersAndLore).??(Lores.all.notOf[UnofficialLore])

            Shuffle2[Leader, Lore](leaders, lores, (l1, l2) => LeadersLoresShuffledAction(l1, l2))

        // DEADLOCK
        case DeadlockAction(f, message, then) =>
            f.log("could not", message)

            Ask(f)
                .add(CheatAction(f, message, then))
                .needOk

        case CheatAction(f, message, then) =>
            f.log("cheated and continued playing")

            then

        // ADJUST
        case AdjustResourcesAction(then) =>
            factions.%(_.adjust).foreach { f =>
                f.unavailable.content.foreach { r =>
                    r --> f.overflow
                }

                if (f.spendable.content.none && f.overflow.none)
                    f.adjust = false
            }

            factions.foreach(_.taken = $)

            ContinueAdjustResourcesAction(then)

        case ContinueAdjustResourcesAction(then) =>
            val l = factions.%(_.adjust)

            if (l.any)
                MultiAsk(l./~(f => game.internalPerform(AdjustingResourcesAction(f, f.adjustable./~(s => 0.until(s.capacity(f))./(s.$.lift)), AdjustResourcesAction(then)), NoVoid, 0).as[Ask]))
            else
                Then(then)

        case AdjustingResourcesAction(f, l, then) =>
            val slots = f.adjustable./~(s => s.capacity(f).times(s))
            val n = slots.notOf[Overflow].num

            val keys = slots./{
                case GolemHearthSlots => Image("keys-" + 2 + "-golem", styles.token3x).spn(styles.card0)(styles.circle)
                case PirateHoardSlots => Image("keys-" + 2 + "-hoard", styles.token3x).spn(styles.card0)(styles.circle)
                case WellOfEmpathySlots => Image("keys-" + 2 + "-empathy", styles.token3x).spn(styles.card0)(styles.circle)
                case s if s.raidable.any => Image("keys-" + s.raidable.get, styles.token3x).spn(styles.card0)(styles.circle)
                case _ : Overflow => Image("discard-resource", styles.token3x).spn(styles.card0)(styles.circle)
            }

            implicit def convert(p : (|[ResourceLike], ResourceSlot)) = Image(p._1./(_.id).|("nothingness"), styles.token3x)

            def rule(l : $[(|[ResourceLike], ResourceSlot)]) : Boolean = l @@ {
                case Nil | List(_) => true
                case List(a, b) => f.canExchangeSlotContents(a, b)
            }

            val pairs = l.zip(slots)

            XXSelectObjectsAction(f, pairs)
                .withGroup("Adjust resources" ~ Break)
                .withSplit($(f.displayable.num))
                .withBreak({
                    case 0 => HorizontalBreak ~ HGap ~ HGap ~ keys.take(f.displayable.num) ~ HorizontalBreak
                    case _ => HorizontalBreak ~ HGap ~ HGap ~ HGap ~ HGap ~ keys.drop(f.displayable.num) ~ HorizontalBreak
                })
                .withRule(_.num(2)
                    .all(rule)
                )
                .withAutoIndex(ii => AdjustingResourcesAction(f, 0.until(l.num)./{
                    case i if i == ii(0) => l(ii(1))
                    case i if i == ii(1) => l(ii(0))
                    case i => l(i)
                }, then))
                .withExtra($(
                    ReorderResourcesAction(f, slots.zip(l).sortBy { case (s, r) => (slots.indexOf(s), r./(_.order).|(9999)) }.rights, then).as("Done"
                        // ~ slots.zip(l).sortBy { case (s, r) => (slots.indexOf(s), r./(_.order).|(9999)) }.toString
                    ).!(pairs.drop(n).exists(a => pairs.take(n).exists(b => a._1.any && b._1.none && rule($(a, b))))),
                    ExplodeReorderResourcesAction(f, slots, l, then)
                ))
                .ask

        case ReorderResourcesAction(f, l, then) =>
            val slots = f.adjustable./~(s => s.capacity(f).times(s))

            slots.lazyZip(l).foreach { (s, r) =>
                r.foreach { r =>
                    if (s.has(r).not) {
                        r --> s

                        if (f.adjust) {
                            f.adjust = false

                            f.log("reordered resources")
                        }
                    }
                }
            }

            f.adjust = false

            var golems : $[GolemToken] = $

            f.overflow.foreach {
                case r : ResourceToken =>
                    (r : ResourceLike) --> Supply(r.resource)

                    f.log("discarded", r)

                case r : GolemToken =>
                    golems :+= r

                    f.log("discarded", r)
            }

            golems.foldLeft(then)((q, g) => GiveGolemAwayMainAction(f, g, f.overflow, q))

        // COST
        case PayCostAction(f, cost, then) =>
            f.pay(cost)

            then

        // RESOURCES
        case StealResourceAction(f, e, r, k, then) =>
            f.steal(r)

            f.log("stole", r -> k)

            then

        // OUTRAGE
        case OutrageAction(f, r, effect, then) =>
            if (f.outraged.has(r).not) {
                f.outraged :+= r

                f.log("provoked", r, "outrage", effect./("with" -> _))
            }
            else
                f.log("provoked", r, "outrage again", effect./("with" -> _))

            val discarded = f.spendable.but(PirateHoardSlots).resources.lefts.%(_.is(r))

            if (discarded.any)
                f.log("discarded", discarded./(_.elem).comma)

            discarded.foreach { r =>
                r --> r.supply
            }

            if (f.hasLore(GuildLoyaltyLL).not)
                f.loyal.of[GuildCard].%!(_.effect.is[LoyalGuild]).foreach { c =>
                    if (c.suit == r) {
                        f.loyal --> c --> game.discourt

                        f.log("discarded", c)

                        c.effect @@ {
                            case ArsenalKeepers =>
                                ArsenalKeepersSlots.$.some.foreach { l =>
                                    l.of[ResourceToken].foreach(r => r --> r.supply)

                                    log(c, "returned", l)
                                }
                            case MerchantLeague =>
                                MerchantLeagueSlots.$.some.foreach { l =>
                                    l.of[ResourceToken].foreach(r => r --> r.supply)

                                    log(c, "returned", l)
                                }
                            case _ =>
                        }
                    }
                }

            if (f.hasGuild(Sycophants)) {
                Ask(f).group(Sycophants, "place", Ship.sof(f), "in")
                    .each(systems.%!(_.gate).%(game.resources(_).has(r)))(s => ShipsInSystemsAction(f, $(s, s), UseEffectAction(f, Sycophants, then)).as(s).!(f.pool(Ship).not))
                    .skip(then)
                    .needOk
            }
            else
                then

        case ClearOutrageAction(f, l, then) =>
            l.foreach { r =>
                f.outraged :-= r

                f.log("cleared", r, "outrage")
            }

            then

        // COURT CARDS
        case GainCourtCardAction(f, c : GuildCard, lane, main, then) =>
            (c : CourtCard) --> f.loyal

            f.recalculateSlots()

            then

        case StealGuildCardAction(f, e, c @ GuildCard(_, SwornGuardians), then) =>
            (c : CourtCard) --> game.court

            f.log("stole and buried", c)

            then

        case StealGuildCardAction(f, e, c, then) =>
            e.loyal --> c --> f.loyal

            f.recalculateSlots()
            e.recalculateSlots()

            f.log("stole", c)

            then

        case GiveCourtCardAction(f, _, e, then) =>
            f.recalculateSlots()
            e.recalculateSlots()

            then

        case DiscardVoxCardAction(f, c, then) =>
            (c : CourtCard) --> game.discourt

            f.recalculateSlots()

            f.log("discarded", c)

            then

        case DiscardPreludeGuildCardAction(f, c, then) =>
            f.loyal --> c --> game.discourt

            f.recalculateSlots()

            f.log("discarded", c)

            if (f.objective.has(ProveYourselfToOverseers))
                if (f.hasGuild(GuildOverseers))
                    f.advance(3, $("discarding a guild card"))

            then

        case DiscardGuildCardAction(f, c, then) =>
            f.loyal --> c --> game.discourt

            f.recalculateSlots()

            f.log("discarded", c)

            then

        case BuryVoxCardAction(f, v, then) =>
            v --> game.court

            f.recalculateSlots()

            f.log("buried", v)

            then

        case UseEffectAction(f, c, then) =>
            f.used :+= c

            then

        case ClearEffectAction(f, c, then) =>
            f.used :-= c

            then

        // TAX
        case TaxMainAction(f, x, effect, then) =>
            val chosen = f.abilities.has(JudgesChosen).?(f.rivals.%(_.fates.has(Judge)).only)

            var loyal = systems./~(s => f.at(s).cities./(_ -> s))
            var rival = systems.%(s => f.rules(s) || f.isLordIn(s.cluster))./~(s => f.rivals.%(e => (e.regent && f.regent && Empire.at(s).any && f.hasGuild(RogueAdmirals).not).not)./~(e => e.at(s).cities./(_ -> s)))
            var smugg = systems./~(s => $[Figure]()./(_ -> s))
            var free  = systems.%(s => f.rules(s) || f.isLordIn(s.cluster))./~(s => Free.at(s).cities./(_ -> s))
            var slots = systems.%(_ => false)
            var array = f.flagship./(_.system).%(_.gate).%(_ => ControlArray(f).any)./~(g => systems.%(_.cluster == g.cluster).%!(_.gate)./~(s => s.$.cities./(_ -> s))).diff(loyal).diff(free)
            var jaray = chosen./~(j => j.flagship./(_.system).%(_.gate).%(_ => ControlArray(j).any)./~(g => systems.%(_.cluster == g.cluster).%!(_.gate)./~(s => s.$.cities./(_ -> s)))).diff(loyal).diff(free).diff(array)
            var taxed = f.taxed.cities

            if (f.hasLore(ImperialAuthority))
                free ++= systems.%(Empire.rules)./~(s => f.rivals.appended(Free)./~(e => e.at(s).cities./(_ -> s)))

            if (f.hasLore(EmpathsBond) && game.declared.contains(Empath))
                free = systems./~(s => f.rivals.appended(Free)./~(e => e.at(s).cities./(_ -> s)))

            if (f.hasTrait(Inspiring)) {
                rival = systems.%(f.at(_).ships.any)./~(s => f.rivals./~(e => e.at(s).cities./(_ -> s)))

                slots = systems.%(f.at(_).ships.any)./~(s => game.freeSlots(s).times(s)).diff(f.taxed.slots)
            }

            if (f.hasTrait(Principled))
                loyal = $

            if (f.hasGuild(PirateSmugglers))
                smugg = systems.%(f.at(_).shiplikes.any).%!(f.rules)./~(s => f.rivals./~(e => e.at(s).cities./(_ -> s)))

            if (f.hasTrait(Callow))
                loyal = loyal.%>(f.rules)

            if (effect.has(LivingStructures)) {
                rival = rival.%<(_.faction == f)
                free = free.%<(_.faction == f)
                array = array.%<(_.faction == f)
                slots = $
            }

            if (f.hasLore(IreOfTheKeepers))
                loyal = loyal.%>(s => game.resources(s).has(Relic).not || f.rules(s))

            if (f.hasLore(IreOfTheTycoons))
                loyal = loyal.%>(s => (game.resources(s).has(Material).not && game.resources(s).has(Fuel).not) || f.rules(s))

            if (f.hasLore(WarlordsCruelty) && game.declared.contains(Warlord))
                taxed = $

            def res(s : System) = game.resources(s).distinct.%(game.available).some./(l => ("for", l./(r => (r, Image(r.name, styles.token))).intersperse("or")))

            Ask(f).group("Tax".hl, effect./("with" -> _), x)
                .each(loyal) { case (c, s) => TaxAction(f, x, effect, s, |(c), true,  then).as(c, game.showFigure(c), "in", s, res(s)).!(taxed.has(c), "taxed") }
                .each(rival) { case (c, s) => TaxAction(f, x, effect, s, |(c), false, then).as("Rival", c, game.showFigure(c), "in", s, res(s), c.faction.as[Faction].?(_.pool(Agent)).$("and capture")).!(taxed.has(c), "taxed").!(f.regent && c.faction.regent && Empire.at(s).any, "truce") }
                .each(smugg) { case (c, s) => TaxAction(f, x, effect, s, |(c), true,  then).as("Rival", c, game.showFigure(c), "in", s, res(s)                                                         ).!(taxed.has(c), "taxed").!(f.regent && c.faction.regent && Empire.at(s).any, "truce") }
                .each(free ) { case (c, s) => TaxAction(f, x, effect, s, |(c), true,  then).as(c, game.showFigure(c), "in", s, res(s)).!(taxed.has(c), "taxed") }
                .each(array) { case (c, s) => TaxAction(f, x, effect, s, |(c), true,  then).as(c, game.showFigure(c), "in", s, res(s), "with", ControlArray(f)).!(taxed.has(c), "taxed") }
                .each(jaray) { case (c, s) => TaxAction(f, x, effect, s, |(c), true,  then).as(c, game.showFigure(c), "in", s, res(s), "with", ControlArray(chosen.get)).!(taxed.has(c), "taxed") }
                .each(slots) { s =>           TaxAction(f, x, effect, s, None, true,  then).as("Empty Slot".hh, game.showFigure(Figure(f, City, 0), 2), "in", s, res(s)).!(f.taxed.slots.count(s) >= game.freeSlots(s), "taxed") }
                .cancel

        case TaxAction(f, x, effect, s, c, loyal, then) =>
            f.pay(x)

            f.log("taxed", c, "in", s, x)

            f.taxed.cities ++= c

            if (c.none)
                f.taxed.slots :+= s

            val e = c./~(_.faction.as[Faction]).but(f)

            if (loyal.not)
                e.foreach { e =>
                    if (e.pool(Agent)) {
                        e.reserve --> Agent --> f.captives

                        f.log("captured", Agent.of(e))
                    }
                    else {
                        e.log("had no", Agent.sof(e))
                    }
                }

            val res = game.resources(s).distinct
            val l = (f.hasGuild(TaxCollectors) && loyal.not).??(e./~(_.boardable).desc.%<(x => res.exists(x.is)))

            Ask(f)
                .each(game.resources(s).distinct./(|(_)).some.|($(None)))(r => TaxGainAction(f, r, x == Pip && (f.copy || f.pivot), PostTaxAction(f, s, c, loyal, then)).as("Gain", r./(ResourceRef(_, None)))("Tax", s))
                .each(l) { case (r, s) => TaxTakeAction(f, r, e.get, then).as("Take", r -> s) }

        case TaxGainAction(f, r, along, then) =>
            r.foreach { r =>
                f.gain(r, $)
            }

            if (along && f.hasTrait(Insatiable))
                f.gain(Fuel, $("from", Insatiable))

            if (along && f.hasTrait(Firebrand))
                f.gain(Weapon, $("from", Firebrand))

            if (along && f.hasTrait(Attuned))
                f.gain(Psionic, $("from", Attuned))

            AdjustResourcesAction(then)

        case TaxTakeAction(f, t, e, then) =>
            f.take(t)

            f.log("took", t, "from", e)

            AdjustResourcesAction(then)

        case PostTaxAction(f, s, c, loyal, then) =>
            implicit val ask = builder

            if (f.hasTrait(Mythic) && game.overridesHard.contains(s).not && board.slots(s) > 0) {
                f.spendable.resources./ { (r, k) =>
                    + MythicAction(f, s, r, PostTaxAction(f, s, c, loyal, then)).as("Change planet type with", r -> k)(Mythic, "in", s)
                }
            }

            if (f.hasTrait(Ruthless)) {
                c.$.cities.foreach { c =>
                    c.faction.as[Faction].foreach { e =>
                        + UseEffectAction(f, Ruthless, DealHitsAction(f, s, f, e, $(c), 0, |(Ruthless), $, TaxAction(f, NoCost, |(Ruthless), s, |(c), loyal, then))).as("Damage to Tax again")(Ruthless, "in", s)
                    }
                }
            }

            ask(f).done(then)

        // BUILD
        case BuildMainAction(f, x, then) =>
            val officers = f.officers
            val chosen = f.abilities.has(JudgesChosen).?(f.rivals.%(_.fates.has(Judge)).only)

            var present = systems.%(s => f.present(s) || officers && Empire.present(s) || chosen.?(j => j.at(s).shiplikes.any))

            val slots = present.%(s => game.freeSlots(s) > 0)

            var cities = slots
            var starports = slots
            var gateCities : $[System] = $
            var gateStarports : $[System] = $
            var freeCities : $[System] = $
            var freeClouds : $[System] = $
            var freeStarports : $[System] = $

            var clouds = f.hasLore(CloudCities).??(present.%!(_.gate).%(_.$.cities.intersect(game.unslotted).none))

            var upgrades = f.flagship./(_.system).%(_.gate.not)./(game.resources)./~( l =>
                (l.has(Material) && ShipCrane(f).none).$(ShipCrane(f)) ++
                (l.has(Fuel) && SlipstreamDrive(f).none).$(SlipstreamDrive(f)) ++
                (l.has(Weapon) && DefenseArray(f).none).$(DefenseArray(f)) ++
                (l.has(Relic) && TractorBeam(f).none).$(TractorBeam(f)) ++
                (l.has(Psionic) && ControlArray(f).none).$(ControlArray(f)) ++
                (Hull(f).none).$(Hull(f)) ++
                (l.has(Material) && ControlArray(f).armor.none && ControlArray(f).$.fresh.any).$(ControlArray(f).armor) ++
                (l.has(Fuel) && ShipCrane(f).armor.none && ShipCrane(f).$.fresh.any).$(ShipCrane(f).armor) ++
                (l.has(Weapon) && TractorBeam(f).armor.none && TractorBeam(f).$.fresh.any).$(TractorBeam(f).armor) ++
                (l.has(Relic) && SlipstreamDrive(f).armor.none && SlipstreamDrive(f).$.fresh.any).$(SlipstreamDrive(f).armor) ++
                (l.has(Psionic) && DefenseArray(f).armor.none && DefenseArray(f).$.fresh.any).$(DefenseArray(f).armor) ++
                (Hull(f).armor.none && Hull(f).$.fresh.any).$(Hull(f).armor) ++
                $
            )

            if (f.hasLore(GateStations))
                gateCities ++= present.%(_.gate).%(f.at(_).cities.none)

            if (f.hasLore(GatePorts))
                gateStarports ++= present.%(_.gate).%(f.at(_).starports.none)

            if (f.hasLore(BookOfLiberation)) {
                freeCities = (cities ++ gateCities).%(Empire.at(_).fresh.ships.none)
                freeClouds = clouds.%(Empire.at(_).fresh.ships.none)
                freeStarports = (starports ++ gateStarports).%(Empire.at(_).fresh.ships.none)
            }

            if (campaign && f.flagship.any) {
                cities = $
                starports = $
            }

            cities ++= gateCities
            starports ++= gateStarports

            var yards = present./~(s => f.at(s).starports./(_ -> s))

            if (campaign)
                yards ++= systems./~(s => Free.at(s).starports./(_ -> s).some.%(_ => f.rules(s)).|($))

            if (f.hasLore(EmpathsBond) && game.declared.contains(Empath))
                yards = systems./~(s => s.$.starports./(_ -> s))

            if (f.hasLore(ToolPriests))
                if (f.worked.cities.none)
                    yards ++= present.%(f.rules)./~(s => s.$.cities./(_ -> s))

            if (f.flagship.any)
                yards :+= f.flagship.get -> f.flagship.get.system

            chosen.foreach { j =>
                if (j.flagship.any)
                    yards :+= j.flagship.get -> j.flagship.get.system
            }

            if (f.pool(City).not)
                cities = $

            if (f.pool(City).not)
                clouds = $

            if (f.pool(Starport).not)
                starports = $

            val ruthless = (f.hasTrait(Ruthless) && f.used.has(Ruthless).not && f.pool(Ship).not).??(yards)

            if (f.pool(Ship).not)
                yards = $

            val prefix = f.short + "-"
            def suffix(s : System) = (f.rivals.exists(_.rules(s)) || campaign.&&(f.regent.not && Empire.rules(s))).??("-damaged")

            val resources = f.spendable.resources.%<!(x.as[PayResource]./(_.resource).has)

            Ask(f)
                .group("Build".hl, x)
                .each(cities)(s => BuildCityAction(f, x, s, None, then).as(s.gate.?("Station".styled(f)), City.of(f), Image(prefix + "city" + suffix(s), styles.qbuilding), "in", s))
                .some(clouds)(s => resources.%((r, k) => game.resources(s).has(r.resource))./((r, k) => BuildCityAction(f, MultiCost(x, PayResource(r, k)), s, |(CloudCities), then).as("Cloud".styled(f), City.of(f), Image(prefix + "city" + suffix(s), styles.qbuilding), "in", s, "with", r -> k)))
                .each(starports)(s => BuildStarportAction(f, x, s, then).as(s.gate.?("Port".styled(f)), Starport.of(f), Image(prefix + "starport" + suffix(s), styles.qbuilding), "in", s))
                .each(freeCities)(s => BuildFreeCityAction(f, x, s, None, then).as(s.gate.?("Station".styled(Free)), City.of(Free), Image("free-" + "city" + suffix(s), styles.qbuilding), "in", s))
                .some(freeClouds)(s => resources.%((r, k) => game.resources(s).has(r.resource))./((r, k) => BuildFreeCityAction(f, MultiCost(x, PayResource(r, k)), s, |(CloudCities), then).as("Cloud".styled(Free), City.of(Free), Image("free-" + "city" + suffix(s), styles.qbuilding), "out of slot in", s, "with", r -> k)))
                .each(freeStarports)(s => BuildFreeStarportAction(f, x, s, then).as(s.gate.?("Port".styled(Free)), Starport.of(Free), Image("free-" + "starport" + suffix(s), styles.qbuilding), "in", s))
                .each(yards) { case (b, s) =>
                    BuildShipAction(f, x, s, b, None, then).as(Ship.of(f), Image(prefix + "ship" + f.hasLore(HiddenHarbors).not.??(suffix(s)), styles.qship), "in", s, "with", b)
                        .!(f.worked.count(b) > (b.piece == Flagship && ShipCrane(b.faction.as[Faction].get).any).??(1), "built this turn")
                }
                .each(ruthless) { case (b, s) =>
                    PayCostAction(f, x, UseEffectAction(f, Ruthless, DealHitsAction(f, s, f, f, $(b), 0, |(Ruthless), $, then))).as("Damage", b, "with no Ships in supply to build")
                }
                .some(upgrades) { u =>
                    val s = f.flagship.get.system

                    f.pool(City).$(BuildFlagshipAction(f, x, u, City, then).as(City.of(f), Image(prefix + "city" + suffix(s), styles.qbuilding), "as", u)) ++
                    f.pool(Starport).$(BuildFlagshipAction(f, x, u, Starport, then).as(Starport.of(f), Image(prefix + "starport" + suffix(s), styles.qbuilding), "as", u))
                }
                .cancel

        case BuildCityAction(f, x, s, effect, then) =>
            f.pay(x)

            val u = f.reserve --> City

            if (f.rivals.exists(_.rules(s)) || campaign.&&(f.regent.not && Empire.rules(s)))
                f.damaged :+= u

            u --> s

            if (effect.has(CloudCities))
                game.unslotted :+= u
            else
            if (f.taxed.slots.has(s))
                f.taxed.slots :-= s

            f.log("built", u, "in", s, effect./("with" -> _), x)

            f.recalculateSlots()

            then

        case BuildFreeCityAction(f, x, s, effect, then) =>
            f.pay(x)

            val u = Free.reserve --> City

            if (f.rivals.exists(_.rules(s)))
                f.damaged :+= u

            u --> s

            if (effect.has(CloudCities))
                game.unslotted :+= u
            else
            if (f.taxed.slots.has(s))
                f.taxed.slots :-= s

            f.log("built", u, "in", s, effect./("with" -> _), x)

            f.recalculateSlots()

            ClearOutrageAction(f, s.gate.not.??(game.resources(s)).%(f.outraged.has), then)

        case BuildStarportAction(f, x, s, then) =>
            f.pay(x)

            val u = f.reserve --> Starport

            if (f.rivals.exists(_.rules(s)) || campaign.&&(f.regent.not && Empire.rules(s)))
                f.damaged :+= u

            u --> s

            if (f.taxed.slots.has(s))
                f.taxed.slots :-= s

            f.log("built", u, "in", s, x)

            then

        case BuildFreeStarportAction(f, x, s, then) =>
            f.pay(x)

            val u = Free.reserve --> Starport

            if (f.rivals.exists(_.rules(s)))
                f.damaged :+= u

            u --> s

            if (f.taxed.slots.has(s))
                f.taxed.slots :-= s

            f.log("built", u, "in", s, x)

            ClearOutrageAction(f, s.gate.not.??(game.resources(s)).%(f.outraged.has), then)

        case BuildShipAction(f, x, s, b, effect, then) =>
            f.pay(x)

            val u = f.reserve --> Ship.of(f)

            if (f.rivals.exists(_.rules(s)) || campaign.&&(f.regent.not && Empire.rules(s)))
                if (f.hasLore(HiddenHarbors).not)
                    f.damaged :+= u

            u --> s

            if (then.is[PirateSpentHoardMainAction].not)
                f.worked :+= b

            f.log("built", u, "in", s, effect./("with" -> _), x)

            implicit val ask = builder

            if (f.hasTrait(Ruthless) && f.used.has(Ruthless).not) {
                if (f.pool(Ship))
                    $(b).starports.foreach { b =>
                        b.faction.as[Faction].foreach { e =>
                            + UseEffectAction(f, Ruthless, DealHitsAction(f, s, f, e, $(b), 0, |(Ruthless), $, BuildShipAction(f, NoCost, s, b, |(Ruthless), then))).as("Damage", b, "to Build again")(Ruthless, "in", s)
                        }
                    }
                else
                    $(b).starports.foreach { b =>
                        b.faction.as[Faction].foreach { e =>
                            + UseEffectAction(f, Ruthless, DealHitsAction(f, s, f, e, $(b), 0, |(Ruthless), $, then)).as("Damage", b, "with no Ships in supply to build")(Ruthless, "in", s)
                        }
                    }
            }

            ask(f).done(then)

        // REPAIR
        case RepairMainAction(f, x, then) =>
            val officers = f.officers
            val chosen = f.abilities.has(JudgesChosen).?(f.rivals.%(_.fates.has(Judge)).only)

            val loyal = systems.%(f.present)
            val allied = chosen./~(j => systems.%(j.present))
            val imperial = f.regent.??(officers.?(systems.%(Empire.present))|((loyal ++ allied).distinct))
            val free = campaign.??(systems.%(f.rules))
            val blights = campaign.??((f.hasLore(BlightFury) || f.hasLore(BlightSociety)).??(free))
            val upgrades = campaign.??(f.flagship.any.??(Flagship.scheme(f)).%(_.$.damaged.any)) ++ chosen./~(j => j.flagship.any.??(Flagship.scheme(j)).%(_.$.damaged.any))

            Ask(f)
                .group("Repair".hl, x)
                .some(loyal)(s => f.at(s).damaged./(u => RepairAction(f, x, s, u, then).as(u, game.showFigure(u), "in", s)))
                .some(imperial)(s => Empire.at(s).damaged./(u => RepairAction(f, x, s, u, then).as(u, game.showFigure(u), "in", s)))
                .some(allied)(s => chosen.get.at(s).ships.damaged./(u => RepairAction(f, x, s, u, then).as(u, game.showFigure(u), "in", s)))
                .some(free)(s => Free.at(s).damaged./(u => RepairAction(f, x, s, u, then).as(u, game.showFigure(u), "in", s)))
                .some(blights)(s => Blights.at(s).damaged./(u => RepairAction(f, x, s, u, then).as(u, game.showFigure(u), "in", s)))
                .some(upgrades)(q => q.$.damaged./(u => RepairFlagshipAction(f, x, q, u, then).as(u, game.showFigure(u), "as", q)))
                .cancel

        case RepairAction(f, x, s, u, then) =>
            f.pay(x)

            u.faction.damaged :-= u

            f.log("repaired", u, "in", s, x)

            then

        // INFLUENCE
        case InfluenceMainAction(f, x, effect, skip, cancel, then) =>
            Ask(f).group("Influence".hl, effect./("with" -> _), x)
                .each(game.market) { m =>
                    InfluenceAction(f, x, m.index, effect, then).as(m.$.intersperse("|"))
                        .!(m.none)
                        .!(m.has(ImperialCouncilDecided))
                        .!(game.feudal.get(m.index).?(i => f.isLordIn(i).not && f.isVassalIn(i).not), "feudal court")
                }
                .skipIf(skip)(then)
                .cancelIf(cancel)
                .needOk

        case InfluenceAction(f, x, n, effect, then) =>
            f.pay(x)

            val feudal = n > 0 && game.laws.has(FeudalCourts) && game.feudal.contains(n).not && Influence(n).none

            f.reserve --> Agent --> Influence(n)

            f.log("influenced", Market(n).$.starting, effect./("with" -> _), x)

            if (feudal)
                FeudalCourtClaimMainAction(f, n, then)
            else
                then

        case MayInfluenceAction(f, effect, then) =>
            if (f.pool(Agent).not) {
                f.log("had no", "Agents".styled(f), "for", effect)

                then
            }
            else
                InfluenceMainAction(f, NoCost, effect, true, false, then)

        case CaptureAgentsCourtCardAction(f, lane, then) =>
            Influence(lane).foreach { u =>
                if (u.faction != f) {
                    if (game.feudal.get(lane).?(i => f.isLordIn(i) && u.faction.as[Faction].?(_.isVassalIn(i)))) {
                        u --> u.faction.reserve

                        f.log("returned", u)
                    }
                    else {
                        u --> f.captives

                        f.log("captured", u)
                    }
                }
                else
                    u --> f.reserve
            }

            if (game.feudal.get(lane).?(i => f.isLordIn(i))) {
                game.feudal -= lane

                f.log("returned feudal claim")
            }

            Then(then)

        case ExecuteAgentsCourtCardAction(f, lane, then) =>
            Influence(lane).foreach { u =>
                if (u.faction == f)
                    u --> f.reserve
                else {
                    u --> f.trophies

                    f.log("executed", u)
                }
            }

            Then(then)

        case ReturnAgentsCourtCardAction(lane, then) =>
            Influence(lane).groupBy(_.faction).$./ { (f, l) =>
                l --> f.reserve

                f.log("returned", l.intersperse(Comma))
            }

            Then(then)

        // SECURE
        case MustSecureAction(f, effect, then) =>
            Ask(f)
                .add(SecureMainAction(f, NoCost, effect, false, false, then).as("Secure").!!!)
                .add(DeadlockAction(f, SecureWith(effect), then))

        case MaySecureAction(f, effect, then) =>
            SecureMainAction(f, NoCost, effect, true, false, then)

        case SecureMainAction(f, x, effect, skip, cancel, then) =>
            Ask(f).group("Secure".hl, effect./("with" -> _), x)
                .each(game.market)(m => SecureAction(f, x, m.index, effect, then).as(m.$.intersperse("|"))
                    .!(Influence(m.index).$.use(l => l.%(_.faction == f).num <= f.rivals./(e => l.%(_.faction == e).num).max))
                    .!(f.hasTrait(Paranoid) && m.first.is[GuildCard] && Influence(m.index).%(_.faction == f).num <= 1, "Paranoid")
                    .!(game.feudal.get(m.index).?(i => f.isLordIn(i).not && f.isVassalIn(i).not), "feudal court")
                )
                .skipIf(skip)(then)
                .cancelIf(cancel)
                .needOk

        case SecureAction(f, x, n, effect, then) =>
            f.pay(x)

            val l = Market(n).$

            f.secured ++= l.of[GuildCard]

            f.log("secured", l.first, x, effect./("with" -> _))

            l.drop(1).foreach { c =>
                f.log("also secured", c)
            }

            if (f.abilities.has(Conspiracies))
                f.used :+= Conspiracies

            val after : ForcedAction = CaptureAgentsCourtCardAction(f, n, ReplenishMarketAction(then))

            val next = l.foldLeft(after)((q, c) => GainCourtCardAction(f, c, n, c == l.first, q))

            if (game.conspired.any) {
                val e = factions.%(_.fates.has(Conspirator)).only
                val guesses = l.drop(1).of[VoxCard].num + (l.first.is[VoxCard] || Influence(n).$.ofc(e).any).??(1)

                if (e != f && guesses > 0)
                    MayGuessConspiracyAction(f, e, guesses, next)
                else
                    next
            }
            else
                next

        // WEAPON
        case AddBattleOptionAction(f, x, then) =>
            f.pay(x)

            f.anyBattle = true

            f.log("could use any card action as ", "Battle".styled(f), x)

            then

        // DISCARD
        case DiscardResourcesMainAction(f, then) =>
            Ask(f).group("Discard Resources with No Effect".spn(xstyles.error))
                .each(f.spendable.resources)((r, k) =>
                    DiscardResourceNoEffectAction(f, PayResource(r, k), then).as("Discard", r -> k)
                )
                .cancel

        case DiscardResourceNoEffectAction(f, x, then) =>
            f.pay(x)

            f.log("discarded", x, "with no effect")

            then

        case DiscardGuildCardsMainAction(f, then) =>
            implicit val ask = builder

            def discardable(e : GuildEffect) : GuildCard = f.loyal./~(c => c.as[GuildCard].%(c => c.effect == e && f.secured.has(c).not)).first

            $(MiningInterest, MaterialCartel, ShippingInterest, FuelCartel, Gatekeepers, PrisonWardens, Skirmishers, CourtEnforcers, LoyalMarines, ElderBroker, HunterSquads, LesserRegent, RogueAdmirals, Sycophants).foreach { e =>
                if (f.canPrelude(e))
                    + DiscardGuildCardNoEffectAction(f, discardable(e), then).as("Discard", e)("Discard Guild Cards with No Effect".spn(xstyles.error))
            }

            ask(f).cancel

        case DiscardGuildCardNoEffectAction(f, c, then) =>
            f.loyal --> c --> game.discourt

            f.log("discarded", c, "with no effect")

            if (f.objective.has(ProveYourselfToOverseers))
                if (f.hasGuild(GuildOverseers))
                    f.advance(3, $("discarding a guild card"))

            then

        // TURN
        case StartChapterAction =>
            log(DoubleLine)
            log(SingleLine)
            log(DoubleLine)

            game.ambitionable = game.markers.drop(game.chapter + campaign.??(game.act - 1)).take(3).sortBy(_.high)
            game.declared = Map()
            game.revealed = Map()

            game.chapter += 1
            game.round = 0

            game.current = factions.starting

            if (game.act > 0)
                log(("Act".hl ~ " " ~ game.act.times("I".hlb)).styled(styles.title))

            log(("Chapter".hl ~ " " ~ game.chapter.hlb).styled(styles.title))

            MultiAsk(factions./(f => Ask(f).add(ShuffleDeckAction.as("Deal Cards")("Chapter", game.chapter.hlb)).needOk))

        case ShuffleDeckAction =>
            Shuffle[DeckCard](game.deck, ShuffledDeckCardsAction)

        case ShuffledDeckCardsAction(l) =>
            l --> game.deck

            log("The action deck was shuffled")

            Then(DealCardsAction)

        case DealCardsAction =>
            factions.foreach { f =>
                game.deck.$.take(6) --> f.hand

                f.log("drew", 6.cards)
            }

            Then(StartRoundAction)

        case StartRoundAction =>
            log(DoubleLine)

            game.round += 1

            LeadMainAction(factions.first)

        case LeadMainAction(f) =>
            if (f.hand.notOf[EventCard].none)
                Ask(f).add(PassAction(f).as("Pass".hh)).needOk
            else
                YYSelectObjectsAction(f, f.hand)
                    .withGroup(f.elem ~ " leads")
                    .withRule(_.is[EventCard].not)
                    .withThens(d => d.suit @@ {
                        case Faithful => $(
                            LeadAction(f, d, Zeal).as("Play", d.elem, "as", Zeal),
                            LeadAction(f, d, Wisdom).as("Play", d.elem, "as", Wisdom),
                        )
                        case _ => $(LeadAction(f, d, d.suit).as("Play", d.elem))
                    })
                    .withExtra($(NoHand, PassAction(f).as("Pass".hh)) ++
                        options.has(DebugInterface).$(
                            DebugFailAction(f, PassAction(f)).as("DEBUG: Pass and Discard Hand")("Debug Menu"),
                            DebugSucceedAction(f, PassAction(f)).as("DEBUG: Pass and Complete Objective")("Debug Menu")
                        )
                    )

        case PassAction(f) =>
            if (f.hand.any)
                game.passed += 1

            if (game.passed >= factions.%(_.hand.any).num) {
                f.log("passed")

                game.passed = 0

                Milestone(EndChapterAction)
            }
            else {
                game.factions = factions.drop(1).dropWhile(_.hand.none) ++ factions.take(1) ++ factions.drop(1).takeWhile(_.hand.none)

                game.current = |(factions.first)

                f.log("passed initative to", factions.first)

                if (factions.first.objective.has(SowDivision))
                    factions.first.advance(1, $("getting initative"))

                game.round -= 1

                Milestone(StartRoundAction)
            }

        case LeadAction(f, d, suit) =>
            game.passed = 0

            f.hand --> d --> f.played

            game.seen :+= (game.round, f, |(d))

            val q = d.as[ActionCard]./(_.copy(suit = suit))

            game.lead = q

            f.displayed = q

            f.log("led with", q)

            f.lead = true

            CheckAmbitionAction(f, q.get)

        case CheckAmbitionAction(f, d) =>
            if (game.ambitionable.none)
                PrePreludeActionAction(f, d.suit, d.pips)
            else {
                val faithful = d.suit.in(Zeal, Wisdom)

                val ambitions = d.strength @@ {
                    case x if x == 7 || faithful => game.ambitions
                    case n => game.ambitions.%(_.strength == n)
                }

                val next = PrePreludeActionAction(f, d.suit, d.pips)

                DeclareAmbitionMainAction(f, None, ambitions, game.ambitionable.last, true, faithful && f.hasGuild(FaithfulDisciples), $(SkipAction(next)), next)
            }

        case DeclareAmbitionMainAction(f, effect, ambitions, marker, zero, faithful, extra, then) =>
            Ask(f).group("Declare Ambition".hl, effect./("with" -> _))
                .each(ambitions) { a =>
                    DeclareAmbitionAction(f, a, marker, zero, faithful, then).as(a)
                        .!(a == Tycoon && f.hasLore(IreOfTheTycoons))
                        .!(a == Tyrant && f.hasLore(VowOfSurvival))
                        .!(a == Warlord && f.hasLore(OathOfPeace))
                        .!(a == Keeper && f.hasLore(IreOfTheKeepers))
                }
                .add(extra)

        case DeclareAmbitionAction(f, a, marker, zero, faithful, then) if f.hasTrait(Generous) && f.used.has(Generous).not =>
            val l = factions.but(f).%(_.power == factions.but(f)./(_.power).min)

            YYSelectObjectsAction(f, f.loyal.of[GuildCard])
                .withGroup("Give a card with", Generous)
                .withThensInfo(c => l./(e => UseEffectAction(f, Generous, GiveGuildCardAction(f, e, c, DeclareAmbitionAction(f, a, marker, zero, faithful, ClearEffectAction(f, Generous, then)))).as("Give", c, "to", e)))(l./(e => Info("Give to", e)))
                .withExtras(then.as("Forfeit declaring", a))

        case GiveGuildCardAction(f, e, c, then) =>
            f.loyal --> c --> e.loyal

            f.log("gave", c, "to", e)

            GiveCourtCardAction(e, c, f, then)

        case DeclareAmbitionAction(f, a, marker, zero, faithful, then) =>
            f.log("declared", a, "ambition")

            game.declared += a -> game.declared.get(a).|($).appended(marker)

            game.ambitionable :-= marker

            if (zero && (f.hasGuild(SecretOrder) && a.in(Keeper, Empath)).not)
                f.zeroed = true

            f.declared = true

            val next = then match {
                case then : PrePreludeActionAction if faithful && f.ambitionValue(a) > f.rivals./(_.ambitionValue(a)).max =>
                    f.log("gained an extra action with", FaithfulDisciples)

                    then.copy(pips = then.pips + 1)

                case then => then
            }

            AmbitionDeclaredAction(f, a, $, next)

        case AmbitionDeclaredAction(f, a, used, then) =>
            var ask = Ask(f)

            if (f.hasTrait(Bold) && used.has(Bold).not)
                ask = ask.add(BoldMainAction(f, $, AmbitionDeclaredAction(f, a, used :+ Bold, then)).as("Influence each card in court".hh)(Bold).!(f.pool(Agent).not, "no agents"))

            if (f.hasTrait(Ambitious) && used.has(Ambitious).not)
                Resources.all.foreach { r =>
                    ask = ask.add(GainResourcesAction(f, $(r), AmbitionDeclaredAction(f, a, used :+ Ambitious, then)).as("Gain".hh, r)(Ambitious).!(game.available(r).not, "not in supply"))
                }

            val disableNobleFarseers = f.hasGuild(Farseers).not.$(Farseers) // make a game option?

            if (f.hasTrait(Connected) && used.has(Connected).not && game.declared.get(a).|($).num == 1)
                ask = ask.add(ConnectedAction(f, AmbitionDeclaredAction(f, a, (used :+ Connected) ++ disableNobleFarseers, then)).as("Draw and secure a court card".hh)(Connected).!(game.court.none, "deck empty")).needOk

            if (f.hasGuild(Farseers) && used.has(Farseers).not)
                ask = ask.each(f.rivals)(e => FarseersMainAction(f, e, AmbitionDeclaredAction(f, a, used :+ Farseers, then)).as(e)(Farseers))

            if (f.hasGuild(ParadeFleets) && used.has(ParadeFleets).not)
                ask = ask.add(ParadeFleetsMainAction(f, AmbitionDeclaredAction(f, a, used :+ ParadeFleets, then)).as("Place Ships".hh)(ParadeFleets)/*.!(f.pool(Ship).not, "no ships")*/)

            if (f.hasGuild(MerchantLeague) && used.has(MerchantLeague).not)
                ask = ask.add(MerchantLeagueMainAction(f, AmbitionDeclaredAction(f, a, used :+ MerchantLeague, then)).as("Merchant League".hh)(MerchantLeague))

            if (f.hasGuild(StoneSpeakers) && used.has(StoneSpeakers).not && game.current.has(f))
                ask = ask.add(StoneSpeakersMainAction(f, AmbitionDeclaredAction(f, a, used :+ StoneSpeakers, then)).as("Take Golems".hh, "(ends turn)")(StoneSpeakers).!(current.has(f).not, "not player's turn"))

            ask.add(then.as("Done"))

        case FollowAction(f) =>
            val leading = game.lead.get.suit

            YYSelectObjectsAction(f, f.hand)
                .withGroup(f.elem ~ " follows " ~ game.lead.get.zeroed(factions.first.zeroed))
                .withThens {
                    case d : ActionCard => $(
                        SurpassAction(f, d, leading).as("Surpass".styled(leading).styled(xstyles.bold), "with", d).!(d.suit.sub.has(leading).not, "wrong suit").!(d.strength < game.lead.get.strength && factions.first.zeroed.not, "low strength"),
                        CopyAction(f, d, leading).as("Copy".styled(leading).styled(xstyles.bold), "with", d),
                    ) ++ d.suit @@ {
                        case Faithful => $(Zeal, Wisdom)./(suit =>
                            PivotAction(f, d, suit).as("Pivot".styled(suit).styled(xstyles.bold), "with", d, "as", suit).!(suit == leading, "same suit"),
                        )
                        case _ =>
                            $(PivotAction(f, d, d.suit).as("Pivot".styled(d.suit).styled(xstyles.bold), "with", d).!(d.suit == leading, "same suit"))
                    }
                    case d : EventCard => $(
                        MirrorAction(f, d, leading).as((factions.but(f).%(_.mirror).none && game.decided.none).?("Mirror").|("Zero").styled(leading).styled(xstyles.bold), "with", d),
                    )
                }
                .withExtras(NoHand)

        case SurpassAction(f, d, suit) =>
            f.surpass = true

            f.hand --> d --> f.played

            val q = d.as[ActionCard]./(_.copy(suit = suit))

            game.seen :+= (game.round, f, |(d))

            f.log("surpassed with", q)

            f.displayed = q

            if (game.seized.none && d.strength == 7 && d.suit != Faithful) {
                game.seized = |(f)

                f.log("seized the initative")

                if (f.objective.has(SowDivision))
                    f.advance(1, $("seizing initative"))
            }

            CheckSeizeAction(f, PrePreludeActionAction(f, game.lead.get.suit, d.pips))

        case CopyAction(f, d, suit) =>
            f.log("copied with a", (d.suit == Faithful).?(Faithful), "card")

            f.hand --> d --> f.blind

            f.seen :+= d

            game.seen :+= (game.round, f, None)

            f.copy = true

            CheckSeizeAction(f, PrePreludeActionAction(f, game.lead.get.suit, 1))

        case PivotAction(f, d, suit) =>
            f.displayed = d.as[ActionCard]./(_.copy(suit = suit))

            f.log("pivoted with", f.displayed)

            f.hand --> d --> f.played

            game.seen :+= (game.round, f, |(d))

            f.pivot = true

            CheckSeizeAction(f, PrePreludeActionAction(f, suit, 1))

        case MirrorAction(f, d, suit) =>
            f.log("mirrored with", d)

            f.hand --> d --> f.played

            game.seen :+= (game.round, f, |(d))

            f.mirror = true

            CheckSeizeAction(f, PrePreludeActionAction(f, suit, (factions.but(f).%(_.mirror).none && game.decided.none).??(game.lead.get.pips)))

        case CheckSeizeAction(f, then) =>
            if (factions.exists(_.declared).not && game.ambitionable.any && f.played.any && f.hasGuild(GalacticBards) && f.used.has(GalacticBards).not) {
                val next = UseEffectAction(f, GalacticBards, CheckSeizeAction(f, then))
                val card = f.played.single.get
                val any = card.strength == 7 || card.suit == Faithful

                DeclareAmbitionMainAction(f, |(GalacticBards), game.ambitions.%(_.strength == card.strength || any), game.ambitionable.last, false, false, $(SkipAction(next)), next)
            }
            else
            if (game.seized.none) {
                val ls = f.hasGuild(LatticeSpies).?(
                    LatticeSeizeAction(f, f.loyal.of[GuildCard].%(_.effect == LatticeSpies).first, then)
                        .as("Seize with", LatticeSpies)
                        .!(f.canUseGuildActions.not, "guild overseers")
                )

                if (f.abilities.has(PartisanSeizing)) {
                    Ask(f).group(f, "can seize initiative")
                        .each(Resources.all)(r => PartisanSeizeAction(f, r, then).as("Seize outraging", ResourceToken(r, 0).elem).!(f.outraged.has(r)))
                        .when(ls.any)(ls.get)
                        .skip(then)
                }
                else {
                    if (f.hand.notOf[EventCard].any || ls.any) {
                        YYSelectObjectsAction(f, f.hand)
                            .withGroup(f.elem ~ " can seize initiative")
                            .withRule(_.suit != Event)
                            .withThen(SeizeAction(f, _, then))("Seize with " ~ _.elem)("Seize")
                            .withExtras(NoHand, ls.|(NoHand), then.as("Skip".hh))
                    }
                    else
                        Then(then)
                }
            }
            else
                Then(then)

        case SeizeAction(f, d, then) =>
            f.log("seized initiative with a", (d.suit == Faithful).?(Faithful), "card")

            f.hand --> d --> f.blind

            f.seen :+= d

            game.seen :+= (game.round, f, None)

            game.seized = |(f)

            if (f.objective.has(SowDivision))
                f.advance(1, $("seizing initative"))

            then

        case LatticeSeizeAction(f, c, then) =>
            f.log("seized initiative with", c)

            f.loyal --> c --> game.discourt

            game.seized = |(f)

            if (f.objective.has(SowDivision))
                f.advance(1, $("seizing initative"))

            if (f.objective.has(ProveYourselfToOverseers))
                if (f.hasGuild(GuildOverseers))
                    f.advance(3, $("discarding a guild card"))

            then

        case PrePreludeActionAction(f, suit, pips) =>
            log(DottedLine)

            val next = PreludeActionAction(f, suit, pips)

            if (game.declared.contains(Tycoon) && f.hasLore(TycoonsAmbition) && game.ambitionable.any) {
                val resources = f.spendable.resources.%<(r => r.is(Material) || r.is(Fuel))
                val cost = resources./((r, s) => PayResource(r, s) : Cost).some./(_.reduceLeft((a, b) => MultiCost(a, b))).|(NoCost)

                DeclareAmbitionMainAction(f, |(TycoonsAmbition), game.ambitions.%!(game.declared.contains), game.ambitionable.last, false, false, $(SkipAction(next)), PayCostAction(f, cost, next))
            }
            else
                Then(next)

        case PreludeActionAction(f, suit, pips) =>
            val then = PreludeActionAction(f, suit, pips)

            implicit val ask = builder
            implicit val group = f.elem ~ " " ~ "Prelude".hh

            val guilds = f.canUseGuildActions

            if (f.flagship.any && SlipstreamDrive(f).any && f.used.has(Slipstream).not) {
                val s = f.flagship.get.system

                + SlipstreamDriveFlagshipMainAction(f, s, f.flagship.get, UseEffectAction(f, Slipstream, then)).as("Flagship".styled(f))(Slipstream)

                if (MovementExpansion.movable(f, s, f.regent && f.officers, f.hasLore(BlightFury) || f.hasLore(BlightSociety)).but(f.flagship).any)
                    + SlipstreamDriveOtherMainAction(f, s, f.flagship.get, UseEffectAction(f, Slipstream, then)).as("Other Ships")(Slipstream).!!!

                if (f.abilities.has(BreakingGates))
                    + BreakGateSlipstreamMainAction(f, UseEffectAction(f, Slipstream, then)).as("Break Gate".styled(f))(Slipstream).!!!
            }

            if (f.abilities.has(JudgesChosen)) {
                val j = f.rivals.%(_.fates.has(Judge)).only

                if (j.flagship.any && SlipstreamDrive(j).any && j.used.has(Slipstream).not) {
                    val s = j.flagship.get.system

                    + SlipstreamDriveFlagshipMainAction(f, s, j.flagship.get, UseEffectAction(j, Slipstream, then)).as("Flagship".styled(j))(Slipstream)

                    if (MovementExpansion.movable(f, s, f.regent && f.officers, f.hasLore(BlightFury) || f.hasLore(BlightSociety)).but(j.flagship).any)
                        + SlipstreamDriveOtherMainAction(f, s, j.flagship.get, UseEffectAction(j, Slipstream, then)).as("Other Ships")(Slipstream).!!!
                }
            }

            f.spendable.resources./ { (r, k) =>
                val cost = PayResource(r, k)

                if (f.abilities.has(UncoveringClues)) {
                    val fs = f.flagship.get.system

                    if (fs.gate.not && fs.$.use(l => l.hasA(ClueRight).not && l.hasA(ClueWrong).not)) {
                        val l = systems.%(_.cluster == fs.cluster).%!(_.gate)./~(game.resources)

                        if (l.has(r.resource).not ||
                            (l.has(Material).not && f.hasGuild(LoyalEngineers)) ||
                            (l.has(Fuel).not && f.hasGuild(LoyalPilots)) ||
                            (l.has(Weapon).not && f.hasGuild(LoyalMarines)) ||
                            (l.has(Relic).not && f.hasGuild(LoyalKeepers)) ||
                            (l.has(Psionic).not && f.hasGuild(LoyalEmpaths))
                        )
                            + UncoverClueAction(f, cost, fs, then).as("Uncover", "Clue".hl, "in", fs, cost)(group)
                    }
                }

                // TAX
                if ((r.is(Psionic) && f.outraged.has(Psionic).not) || f.hasGuild(LoyalEmpaths)) {
                    factions.first.displayed./(_.suit)./{
                        case Administration | Zeal =>
                            game.tax(f, cost, then)
                            game.taxAlt(f, cost, guilds, then)
                        case _ =>
                    }
                }

                // BUILD
                if ((r.is(Material) && f.outraged.has(Material).not) || f.hasGuild(LoyalEngineers)) {
                    game.build(f, cost, then)
                    game.buildAlt(f, cost, guilds, then)
                }
                else
                if ((r.is(Psionic) && f.outraged.has(Psionic).not) || f.hasGuild(LoyalEmpaths)) {
                    factions.first.displayed./(_.suit)./{
                        case Construction | Wisdom =>
                            game.build(f, cost, then)
                            game.buildAlt(f, cost, guilds, then)
                        case _ =>
                    }
                }

                // REPAIR
                if ((r.is(Material) && f.outraged.has(Material).not) || f.hasGuild(LoyalEngineers)) {
                    game.repair(f, cost, then)
                    game.repairAlt(f, cost, guilds, then)
                }
                else
                if ((r.is(Psionic) && f.outraged.has(Psionic).not) || f.hasGuild(LoyalEmpaths)) {
                    factions.first.displayed./(_.suit)./{
                        case Administration | Construction | Wisdom =>
                            game.repair(f, cost, then)
                            game.repairAlt(f, cost, guilds, then)
                        case _ =>
                    }
                }

                // MOVE
                if ((r.is(Fuel) && f.outraged.has(Fuel).not) || f.hasGuild(LoyalPilots)) {
                    game.move(f, cost, then)
                    game.moveAlt(f, cost, guilds, then)
                }
                else
                if ((r.is(Psionic) && f.outraged.has(Psionic).not) || f.hasGuild(LoyalEmpaths)) {
                    factions.first.displayed./(_.suit)./{
                        case Aggression | Mobilization | Zeal =>
                            game.move(f, cost, then)
                            game.moveAlt(f, cost, guilds, then)
                        case _ =>
                    }
                }

                // BATTLE
                if (((r.is(Weapon) && f.outraged.has(Weapon).not) || f.hasGuild(LoyalMarines)) && suit != Aggression && suit != Zeal)
                    + AddBattleOptionAction(f, cost, then).as("Add", "Battle".styled(f), "option", cost)(group).!(f.anyBattle)

                if ((r.is(Psionic) && f.outraged.has(Psionic).not) || f.hasGuild(LoyalEmpaths)) {
                    factions.first.displayed./(_.suit)./{
                        case Aggression | Zeal =>
                            game.battle(f, cost, then)
                            game.battleAlt(f, cost, guilds, then)
                        case _ =>
                    }
                }

                // INFLUENCE
                if ((r.is(Psionic) && f.outraged.has(Psionic).not) || f.hasGuild(LoyalEmpaths)) {
                    factions.first.displayed./(_.suit)./{
                        case Administration | Mobilization | Zeal =>
                            game.influence(f, cost, then)
                            game.influenceAlt(f, cost, guilds, then)
                        case _ =>
                    }
                }

                // SECURE
                if ((r.is(Relic) && f.outraged.has(Relic).not) || f.hasGuild(LoyalKeepers)) {
                    game.secure(f, cost, then)
                    game.secureAlt(f, cost, guilds, then)
                }
                else
                if ((r.is(Psionic) && f.outraged.has(Psionic).not) || f.hasGuild(LoyalEmpaths)) {
                    factions.first.displayed./(_.suit)./{
                        case Aggression | Wisdom =>
                            game.secure(f, cost, then)
                            game.secureAlt(f, cost, guilds, then)
                        case _ =>
                    }
                }

            }

            f.spendable.golems./ { (r, k) =>
                if (r.awake)
                    + SpendGolemMainAction(f, r, k, then).as("Activate", r)("Golems")
            }

            if (f.abilities.has(PlantingBanners))
                if (f.pool(Banner) && f.flagship.get.system.use(s => s.gate.not && s.$.use(l => l.hasA(Banner).not && l.buildings.forall(_.faction == f) && l.shiplikes.forall(_.faction == f))))
                    + PlantBannerAction(f, f.flagship.get.system, then).as("Plant", Banner.of(f), "in", f.flagship.get.system)(PlantingBanners)

            if (f.abilities.has(Witnesses))
                systems.%!(_.gate).%(game.resources(_).has(Psionic)).%(f.at(_).piece(Witness).any).%(f.rules).foreach { s =>
                    f.at(s).piece(Witness).foreach { _ =>
                        + WitnessAction(f, s, then).as("Take", Witness.of(f), "in", s)(Witnesses)
                    }
                }

            if (f.abilities.has(Pilgrims) && f.used.has(Pilgrims).not && FatePieces(Pathfinder).$.piece(Pilgrim).num < 6) {
                + PilgrimMoveInitAction(f, then).as("Move", "Pilgrims".styled(f)).!!!
            }

            if (guilds) {
                def discardable(e : GuildEffect) : GuildCard = f.loyal./~(c => c.as[GuildCard].%(c => c.effect == e && f.secured.has(c).not)).first
                def discardEffect(e : GuildEffect) : ForcedAction = DiscardPreludeGuildCardAction(f, discardable(e), then)
                def discardEffectReorder(e : GuildEffect) : ForcedAction = DiscardPreludeGuildCardAction(f, discardable(e), AdjustResourcesAction(then))

                if (f.canPrelude(MiningInterest))
                    + FillSlotsMainAction(f, Material, discardEffect(MiningInterest)).as("Fill up", ResourceRef(Material, None))(MiningInterest)

                if (f.canPrelude(ShippingInterest))
                    + FillSlotsMainAction(f, Fuel, discardEffect(ShippingInterest)).as("Fill up", ResourceRef(Fuel, None))(ShippingInterest)

                if (f.canPrelude(AdminUnion))
                    if (factions.exists(_.played.exists(_.suit == Administration)))
                        + ReserveCardMainAction(f, discardable(AdminUnion), factions./~(_.played.%(_.suit == Administration)), then).as("Take", Administration, "card")(AdminUnion)

                if (f.canPrelude(ConstructionUnion))
                    if (factions.exists(_.played.exists(_.suit == Construction)))
                        + ReserveCardMainAction(f, discardable(ConstructionUnion), factions./~(_.played.%(_.suit == Construction)), then).as("Take", Construction, "card")(ConstructionUnion)

                if (f.canPrelude(SpacingUnion))
                    if (factions.exists(_.played.exists(_.suit == Mobilization)))
                        + ReserveCardMainAction(f, discardable(SpacingUnion), factions./~(_.played.%(_.suit == Mobilization)), then).as("Take", Mobilization, "card")(SpacingUnion)

                if (f.canPrelude(ArmsUnion))
                    if (factions.exists(_.played.exists(_.suit == Aggression)))
                        + ReserveCardMainAction(f, discardable(ArmsUnion), factions./~(_.played.%(_.suit == Aggression)), then).as("Take", Aggression, "card")(ArmsUnion)

                $(TheProdigalOne, TheYoungLight, TheProphet).foreach { c =>
                    if (f.hand.exists(_.strength > 0))
                        if (f.canPrelude(c))
                            if (factions.exists(_.played.exists(_.suit == Faithful)) || factions.exists(_.blind.exists(_.suit == Faithful)))
                                + ReserveFaithfulMainAction(f, discardable(c), then).as("Take", Faithful, "card")(c)
                }

                if (f.canPrelude(MaterialCartel))
                    + StealResourceMainAction(f, |(Material), $(CancelAction), discardEffectReorder(MaterialCartel)).as("Steal", ResourceRef(Material, None))(MaterialCartel)

                if (f.canPrelude(FuelCartel))
                    + StealResourceMainAction(f, |(Fuel), $(CancelAction), discardEffectReorder(FuelCartel)).as("Steal", ResourceRef(Fuel, None))(FuelCartel)

                if (f.canPrelude(WeaponCartel))
                    + StealResourceMainAction(f, |(Weapon), $(CancelAction), discardEffectReorder(WeaponCartel)).as("Steal", ResourceRef(Weapon, None))(WeaponCartel)

                if (f.canPrelude(RelicCartel))
                    + StealResourceMainAction(f, |(Relic), $(CancelAction), discardEffectReorder(RelicCartel)).as("Steal", ResourceRef(Relic, None))(RelicCartel)

                if (f.canPrelude(PsionicCartel))
                    + StealResourceMainAction(f, |(Psionic), $(CancelAction), discardEffectReorder(PsionicCartel)).as("Steal", ResourceRef(Psionic, None))(PsionicCartel)

                if (f.canPrelude(Gatekeepers))
                    + ShipAtEachGateMainAction(f, discardEffect(Gatekeepers)).as("Place", Ship.of(f), "at each gate")(Gatekeepers).!(f.pool(Ship).not, "no ships")

                systems.%(r => f.rules(r)).some.foreach { l =>
                    $(PrisonWardens, Skirmishers, CourtEnforcers, LoyalMarines, HunterSquads, Sycophants).foreach { c =>
                        if (f.canPrelude(c))
                            + ShipsInSystemMainAction(f, l, discardEffect(c)).as("Place", 3.hlb, Ship.sof(f), "in a controlled system")(c).!(f.pool(Ship).not, "no ships")
                    }
                }

                systems.%(r => f.rules(r)).some.foreach { l =>
                    $(RogueAdmirals).foreach { c =>
                        if (f.canPrelude(c))
                            + ImperialShipsInSystemMainAction(f, l, discardEffect(c)).as("Place", 3.hlb, Ship.sof(Empire), "in a controlled system")(c).!(Empire.pool(Ship).not, "no ships")
                    }
                }

                if (f.canPrelude(Farseers))
                    + FarseersRedrawMainAction(f, discardEffect(Farseers)).as("Redraw cards")(Farseers)

                if (f.canPrelude(SilverTongues)) {
                    + StealResourceMainAction(f, None, $(CancelAction), discardEffectReorder(SilverTongues)).as("Steal a Resource")(SilverTongues)

                    + StealGuildCardMainAction(f, $(CancelAction), discardEffect(SilverTongues)).as("Steal a Guild Card")(SilverTongues)
                }

                if (f.canPrelude(RelicFence) && game.available(Relic)) {
                    f.spendable.resources./ { (r, k) =>
                        + FenceResourceAction(f, Relic, PayResource(r, k), UseEffectAction(f, RelicFence, then)).as("Gain", ResourceRef(Relic, None), "with", r -> k)(RelicFence)
                    }
                }

                if (f.canPrelude(ElderBroker) && (game.available(Material) || game.available(Fuel) || game.available(Weapon))) {
                    + GainResourcesAction(f, $(Material, Fuel, Weapon), discardEffect(ElderBroker)).as("Gain", ResourceRef(Material, None), ResourceRef(Fuel, None), ResourceRef(Weapon, None))(ElderBroker)
                }

                if (f.canPrelude(LesserRegent) && game.ambitionable.any)
                    + LesserRegentMainAction(f, discardEffect(LesserRegent)).as("Declare Ambition")(LesserRegent)

                if (f.canPrelude(MaterialLiaisons) && game.ambitionable.any) {
                    + LiaisonsMainAction(f, Tycoon, discardable(MaterialLiaisons), then).as("Declare", Tycoon, "Ambition")(MaterialLiaisons).!!!

                    if (game.ambitions.has(Edenguard))
                        + LiaisonsMainAction(f, Edenguard, discardable(MaterialLiaisons), then).as("Declare", Edenguard, "Ambition")(MaterialLiaisons).!!!
                }

                if (f.canPrelude(FuelLiaisons) && game.ambitionable.any) {
                    + LiaisonsMainAction(f, Tycoon, discardable(FuelLiaisons), then).as("Declare", Tycoon, "Ambition")(FuelLiaisons).!!!

                    if (game.ambitions.has(Edenguard))
                        + LiaisonsMainAction(f, Edenguard, discardable(FuelLiaisons), then).as("Declare", Edenguard, "Ambition")(FuelLiaisons).!!!
                }

                if (f.canPrelude(WeaponLiaisons) && game.ambitionable.any)
                    + LiaisonsMainAction(f, Warlord, discardable(WeaponLiaisons), then).as("Declare", Warlord, "Ambition")(WeaponLiaisons).!!!

                if (f.canPrelude(RelicLiaisons) && game.ambitionable.any)
                    + LiaisonsMainAction(f, Keeper, discardable(RelicLiaisons), then).as("Declare", Keeper, "Ambition")(RelicLiaisons).!!!

                if (f.canPrelude(PsionicLiaisons) && game.ambitionable.any) {
                    + LiaisonsMainAction(f, Empath, discardable(PsionicLiaisons), then).as("Declare", Empath, "Ambition")(PsionicLiaisons).!!!

                    if (game.ambitions.has(Blightkin))
                        + LiaisonsMainAction(f, Blightkin, discardable(PsionicLiaisons), then).as("Declare", Blightkin, "Ambition")(PsionicLiaisons).!!!
                }
            }

            if (game.declared.contains(Tyrant) && f.hasLore(TyrantsEgo) && f.captives.any) {
                + TyrantsEgoMainAction(f, then).as("Secure")(TyrantsEgo).!!!
            }

            if (game.declared.contains(Warlord) && f.hasLore(WarlordsTerror) && f.trophies.any && f.pool(Agent)) {
                + WarlordsTerrorMainAction(f, then).as("Influence")(WarlordsTerror).!!!
            }

            if (game.declared.contains(Tycoon) && f.hasLore(TycoonsCharm)) {
                f.spendable.resources./ { (r, k) =>
                    if (r.is(Material) || r.is(Fuel)) {
                        if (game.available(Material) && r.is(Material).not)
                            + FenceResourceAction(f, Material, PayResource(r, k), then).as("Gain", ResourceRef(Material, None), r)(TycoonsCharm)

                        if (game.available(Fuel) && r.is(Fuel).not)
                            + FenceResourceAction(f, Fuel, PayResource(r, k), then).as("Gain", ResourceRef(Fuel, None), r)(TycoonsCharm)

                        if (game.available(Weapon))
                            + FenceResourceAction(f, Weapon, PayResource(r, k), then).as("Gain", ResourceRef(Weapon, None), r)(TycoonsCharm)

                        if (game.available(Relic))
                            + FenceResourceAction(f, Relic, PayResource(r, k), then).as("Gain", ResourceRef(Relic, None), r)(TycoonsCharm)

                        if (game.available(Psionic))
                            + FenceResourceAction(f, Psionic, PayResource(r, k), then).as("Gain", ResourceRef(Psionic, None), r)(TycoonsCharm)
                    }
                }
            }

            $(
                (WarlordsCruelty, $(Weapon))        ,
                (TyrantsEgo, $(Weapon))             ,
                (KeepersTrust, $(Relic))            ,
                (EmpathsVision, $(Psionic))         ,
                (TycoonsCharm, $(Material, Fuel))   ,
            ).foreach { case (l, r) =>
                if (f.hasLore(l)) {
                    + ClearOutrageAction(f, r, DiscardLoreCardAction(f, l, then)).as("Clear", r, "Outrage")(l).!(f.outraged.intersect(r).none)
                }
            }

            + EndPreludeAction(f, suit, 0, pips).as("End Prelude".hh)(" ")

            if (f.spendable.resources.any)
                + DiscardResourcesMainAction(f, then).as("Discard Resources with No Effect")("  ")

            if (guilds)
                DiscardGuildCardsMainAction(f, then).as("Discard Guild Cards with No Effect")("  ").!!!.addIfAvailable

            if (campaign && options.has(DebugInterface)) {
                + DebugGainResourceAction(f, Material, then).as("DEBUG: Gain", Material)("Debug Menu").noClear
                + DebugGainResourceAction(f, Fuel, then).as("DEBUG: Gain", Fuel)("Debug Menu").noClear
                + DebugGainResourceAction(f, Weapon, then).as("DEBUG: Gain", Weapon)("Debug Menu").noClear
                + DebugGainResourceAction(f, Relic, then).as("DEBUG: Gain", Relic)("Debug Menu").noClear
                + DebugGainResourceAction(f, Psionic, then).as("DEBUG: Gain", Psionic)("Debug Menu").noClear
                + DebugRedealCourtAction(f, then).as("DEBUG: Redeal Court")("Debug Menu").noClear
                + DebugPipsAction(f, EndPreludeAction(f, suit, 0, 20)).as("DEBUG:", 20.hlb, "Pips")("Debug Menu").noClear
                + DebugCrisesAction(f, then).as("DEBUG:", "Crises".styled(Blights))("Debug Menu").noClear
                + DebugEdictsAction(f, then).as("DEBUG:", "Edicts".styled(Empire))("Debug Menu").noClear
                + DebugSummitAction(f, then).as("DEBUG:", "Summit".hl)("Debug Menu").noClear
                + DebugUnlockAction(f, then).as("DEBUG:", "Unlock Secured Cards".hl)("Debug Menu").noClear
                + DebugSuitAction(f, Administration, then).as("DEBUG:", Administration)("Debug Menu").noClear
                + DebugSuitAction(f, Mobilization, then).as("DEBUG:", Mobilization)("Debug Menu").noClear
                + DebugSuitAction(f, Construction, then).as("DEBUG:", Construction)("Debug Menu").noClear
                + DebugSuitAction(f, Aggression, then).as("DEBUG:", Aggression)("Debug Menu").noClear
            }

            ask(f)

        case EndPreludeAction(f, s, i, n) =>
            Resources.all.foreach { r =>
                PreludeHold(r).$ --> Supply(r)
            }

            log(DottedLine)

            MainTurnAction(f, s, i, n)

        case MainTurnAction(f, s, i, n) if i >= n =>
            Ask(f)
                .add(CheckNoFleetAction(f, EndTurnAction(f)).as("End Turn")(f.elem))
                .needOk

        case MainTurnAction(f, s, i, n) =>
            soft()

            val then = MainTurnAction(f, s, i + 1, n)

            implicit val ask = builder
            implicit val group = f.elem ~ " " ~ "Actions" ~ " " ~ (n - i).times(0x2726.toChar.toString.styled(s))

            val guilds = f.canUseGuildActions

            val cost = Pip
            val one = f.copy || f.pivot

            def build() {
                if (f.copy && f.hasGuild(MaterialLiaisons))
                    game.build(f, cost, MayGainResourceAction(f, Material, MaterialLiaisons, then))
                else
                    game.build(f, cost, then)

                game.buildAlt(f, cost, guilds, then)
            }

            def repair() {
                game.repair(f, cost, then)
                game.repairAlt(f, cost, guilds, then)
            }

            def tax() {
                game.tax(f, cost, then)
                game.taxAlt(f, cost, guilds, then)
            }

            def influence() {
                if (one && f.hasTrait(Influential))
                    game.influence(f, cost, MayInfluenceAction(f, |(Influential), then))
                else
                if (f.copy && f.hasGuild(PsionicLiaisons))
                    game.influence(f, cost, MayGainResourceAction(f, Psionic, PsionicLiaisons, then))
                else
                    game.influence(f, cost, then)

                game.influenceAlt(f, cost, guilds, then)
            }

            def move(canBattle : Boolean) {
                if (one && canBattle && f.hasTrait(Tactical))
                    game.move(f, cost, MayBattleAction(f, |(Tactical), then))
                else
                if (f.copy && f.hasGuild(FuelLiaisons))
                    game.move(f, cost, MayGainResourceAction(f, Fuel, FuelLiaisons, then))
                else
                    game.move(f, cost, then)

                game.moveAlt(f, cost, guilds, then)
            }

            def battle(canMove : Boolean) {
                if (one && f.hasTrait(Tactical)) {
                    game.battle(f, cost, MayMoveAction(f, |(Tactical), then))

                    if (canMove.not)
                        game.move(f, cost, MustBattleAction(f, |(Tactical), then))
                }
                else
                if (f.copy && f.hasGuild(WeaponLiaisons))
                    game.battle(f, cost, MayGainResourceAction(f, Weapon, WeaponLiaisons, then))
                else
                    game.battle(f, cost, then)

                game.battleAlt(f, cost, guilds, then)
            }

            def secure() {
                if (one && f.hasTrait(Charismatic)) {
                    game.secure(f, cost, MayInfluenceAction(f, |(Charismatic), then))
                    game.influence(f, cost, MustSecureAction(f, |(Charismatic), then))
                }
                else
                if (f.copy && f.hasGuild(RelicLiaisons))
                    game.secure(f, cost, MayGainResourceAction(f, Relic, RelicLiaisons, then))
                else
                    game.secure(f, cost, then)

                game.secureAlt(f, cost, guilds, then)
            }

            s @@ {
                case Construction =>
                    build()
                    repair()

                case Administration =>
                    tax()
                    repair()
                    influence()

                case Mobilization =>
                    move(f.anyBattle)
                    influence()

                case Aggression =>
                    battle(true)
                    move(true)
                    secure()

                case Zeal =>
                    move(true)
                    influence()
                    battle(true)
                    tax()

                case Wisdom =>
                    build()
                    repair()
                    secure()
            }

            if (s != Aggression && s != Zeal && f.anyBattle)
                battle(s == Mobilization)

            + CheckNoFleetAction(f, EndTurnAction(f)).as("End Turn and Forfeit", (n - i).hlb, "Action".s(n - i))(group)

            if (campaign && options.has(DebugInterface)) {
                + DebugPipsAction(f, MainTurnAction(f, s, 0, 20)).as("DEBUG: More Pips")("Debug Menu")
            }

            ask(f).needOk

        case CheckNoFleetAction(f, then) =>
            if (systems.exists(s => f.at(s).use(l => l.shiplikes.any || l.starports.any)).not && f.pool(Ship)) {
                log(DottedLine)

                f.log("had no", "Ships".hh, "or", "Starports".hh)

                Ask(f).group("Place", min(3, f.pooled(Ship)).hlb, Ship.sof(f), "in")
                    .each(systems.%(_.gate))(s => ShipsInSystemsAction(f, $(s, s, s), then).as(s))
                    .needOk
            }
            else
                Then(then)

        case EndTurnAction(f) =>
            factions.foreach { e =>
                e.used = $
                e.taxed.cities = $
                e.taxed.slots = $
                e.worked = $
                e.secured = $
                e.anyBattle = false
            }

            game.revealed = Map()

            log(SingleLine)

            val next = factions.dropWhile(_ != f).drop(1).%(_.hand.any).starting

            if (next.any) {
                game.current = next

                FollowAction(next.get)
            }
            else
                EndRoundAction

        case EndRoundAction =>
            val next =
                if (game.seized.any)
                    game.seized.get
                else
                    factions.sortBy(f => f.displayed.%(_.suit == game.lead.get.suit).%(_ => f.zeroed.not)./(-_.strength).|(0)).first

            if (factions.starting.has(next))
                next.log("held the initiative")
            else {
                next.log("took the initiative")

                game.seized.none.$(next).foreach { f =>
                    if (f.objective.has(SowDivision))
                        f.advance(1, $("seizing initative"))
                }
            }

            if (game.decided.none)
                if (factions.exists(_.played.exists(_.is[EventCard])))
                    game.decided = |(Blights)

            factions.foreach { f =>
                f.taking.foreach { d =>
                    d --> f.hand

                    f.log("took", d)
                }

                f.taking = $

                f.takingBlind.foreach { d =>
                    d --> f.hand

                    f.log("took a", Faithful, "card")
                }

                f.takingBlind = $

                f.discardAfterRound.foreach { c =>
                    c --> game.discourt

                    f.log("discarded", c)

                    if (f.objective.has(ProveYourselfToOverseers))
                        if (f.hasGuild(GuildOverseers))
                            f.advance(3, $("discarding a guild card"))
                }

                f.reclaimAfterRound.foreach { c =>
                    c --> f.loyal

                    f.log("reclaimed", c)
                }
            }

            game.lead = None
            // game.seized = None

            factions.foreach { f =>
                f.played --> game.deck
                f.blind --> game.deck
                f.displayed = None
                f.lead = false
                f.zeroed = false
                f.declared = false
                f.surpass = false
                f.copy = false
                f.pivot = false
                f.mirror = false
            }

            Milestone(TransferInitiativeAction(next))

        case TransferInitiativeAction(f) =>
            game.seized = None

            game.factions = factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f)

            game.current = |(f)

            if (game.decided.has(Blights))
                ResolveEventAction(f)
            else
            if (game.decided.any)
                ResolveCouncilAction(game.decided.of[Faction].get)
            else
                ContinueRoundsAction

        case ContinueRoundsAction =>
            game.decided = None

            if (factions.exists(_.hand.any))
                MultiAsk(factions./(f =>
                    Ask(f)
                        .add(StartRoundAction.as("Start Round")(("Chapter".hh ~ " " ~ game.chapter.hlb ~ " / " ~ "Round".hh ~ " " ~ (game.round + 1).hlb).styled(styles.title)))
                        .needOk
                ))
            else
                Milestone(EndChapterAction)

        case EndChapterAction =>
            log(("Chapter".hl ~ " " ~ game.chapter.hlb).styled(styles.title), "had ended")

            MultiAsk(factions./(f =>
                Ask(f)
                    .add(ScoreChapterAction.as("Score Ambitions")(("Chapter".hh ~ " " ~ game.chapter.hlb).styled(styles.title)))
                    .needOk
            ))

        case ScoreChapterAction =>
            factions.foreach { f =>
                f.hand --> game.deck
            }

            game.seen = $
            factions.foreach { f =>
                f.seen = $
            }

            factions.foreach { f =>
                if (f.objective.has(InspireConfidence) && f.regent.not) {
                    val l = systems.%(f.rulesAOE)

                    if (l.any) {
                        f.log("controlled systems as", "Outlaw".styled(Free))

                        f.advance(l.num, $("for controlled systems"))
                    }
                }

                if (f.objective.has(PlantBanners))
                    f.advance(systems.%(_.$.hasA(Banner)).num, $("for each banner"))

                if (f.objective.has(GrowYourHoard)) {
                    if (f.hasLore(PirateHoard)) {
                        f.advance(PirateHoardSlots.num, $("for resources in", PirateHoard))

                        f.advance(Resources.all.count(r => PirateHoardSlots.$.count(_.is(r)) > f.rivals./(_.countableResources(r)).max) * 2, $("for resources type in", PirateHoard, "in excess of each rival"))
                    }
                }

                if (f.objective.has(CommitToPacifism)) {
                    f.advance(f.trophies.none.??(2), $("having no trophies"))
                    f.advance(f.captives.none.??(1), $("having no captives"))
                }

                if (f.objective.has(RestrictTheWeaponSupply)) {
                    f.advance(f.countableResources(Weapon), $("for weapons held"))
                    f.advance(systems.%(game.resources(_).has(Weapon)).%(f.rulesAOE).num, $("for weapon planets controlled"))
                }

                if (f.objective.has(ControlTheFiefsSeats)) {
                    f.advance(game.seats.values.$./(_.system).count(f.rulesAOE) * 2, $("controlling seats"))
                    f.advance(game.seats.filter(_._2.faction == f).map(_._1).$.use(l => systems.%(_.cluster.in(l))./(_.$.cities.num)).sum, $("for cities in fiefs"))
                }

                if (f.objective.has(CollapseTheGates))
                    if (f.rulesAOE(GateWraithExpansion.Passage))
                        f.advance(game.broken.num, $("controlling the passage"))

                if (f.objective.has(RuleByFearAlone)) {
                    val lr = f.outraged.%(f.hasCountableResource)

                    f.advance(lr.num, $("ruling by fear alone"))

                    val ls = f.outraged.intersect(systems.%!(_.gate).%(f.rulesAOE)./~(game.resources))

                    f.advance(ls.num * 2, $("controlling by fear alone"))
                }
            }

            Then(ScoreAmbitionsAction)

        case ScoreAmbitionsAction =>
            game.winners = Map()

            game.ambitions.foreach { ambition =>
                val records = game.declared.contains(ambition).??(factions./(f => f -> f.ambitionValue(ambition))).%>(_ > 0).toMap

                if (records.isEmpty) {
                    if (game.declared.contains(ambition))
                        log("No one scored", ambition)

                    if (ambition == Keeper)
                        factions.foreach { f =>
                            if (f.objective.has(SubdueTheKeepers))
                                f.advance(3, $("subduing the keepers"))
                        }

                    if (ambition == Blightkin)
                        factions.foreach { f =>
                            if (f.objective.has(LiveInTheGarden))
                                f.advance(systems.%(s => Blights.at(s).fresh.any).%(f.rulesAOE).num, $("living in the garden"))
                        }

                    if (ambition == Tyrant)
                        factions.foreach { f =>
                            if (f.objective.has(GiveBackToTheLand))
                                f.advance(GreenVaultSlots.$.num, $("giving back to the land"))
                        }

                    if (ambition == Edenguard)
                        factions.foreach { f =>
                            if (f.objective.has(GiveBackToTheLand))
                                f.advance(systems.%!(_.gate).%(s => game.resources(s).use(l => l.has(Material) || l.has(Fuel))).%(f.rulesAOE).num, $("guarding"))
                        }


                    if (game.conspired.contains(ambition)) {
                        val l = game.conspired(ambition)./(_.faction)

                        val f = factions.%(_.objective.has(ControlTheProceedings)).only

                        f.advance(l.num, $("with unrevealed", ambition, "conspiracies"))
                    }
                }
                else {
                    val high = game.declared(ambition)./(_.high).sum
                    val low = game.declared(ambition)./(_.low).sum

                    val max = records.values.max
                    var ff = records.keys.%(f => records(f) == max).$
                    var ss = records.keys.%(f => records(f) == records.values.$.but(max).maxOr(0)).$

                    val first = (ff.num == 1).?(ff).|($)
                    val second = (ff.num == 1).?(ss.single.$).|(ff)

                    first.foreach { f =>
                        game.winners += ambition -> f

                        var p = high + (f.pooled(City) < 2).??(2) + (f.pooled(City) < 1).??(3)

                        if (ambition == Tycoon && f.hasTrait(Academic))
                            p = low

                        if (ambition == Tyrant && f.hasTrait(Just))
                            p = low

                        if (ambition == Empath && f.hasTrait(Violent))
                            p = low

                        if (ambition == Empath && f.hasTrait(Violent))
                            p = low

                        if (f.hasLore(VowOfSurvival))
                            p = low

                        if (ambition == Tycoon && f.hasLore(IreOfTheTycoons))
                            p = 0

                        if (ambition == Warlord && f.hasLore(OathOfPeace))
                            p = 0

                        f.power += p

                        f.log("scored first place", ambition, "for", p.power)

                        if (f.objective.has(InspireConfidence) && f.regent.not)
                            f.advance(3, $("winning ambition"))

                        if (f.objective.has(ConsolidateImperialPower) && f.primus)
                            f.advance(p, $("winning ambition"))

                        if (f.objective.has(SpreadTheGoodWord))
                            f.advance(BelieverCourtDeck.$.of[BelieverCourtCard].starting./(_.card.pips).|(10), $("winning ambition"))

                        if (f.objective.has(FindGolems))
                            f.advance(f.spendable.golems.num * 2, $("winning ambition"))

                        if (f.objective.has(SowDivision))
                            if (ambition @@ {
                                case Tycoon => f.outraged.has(Material).not && f.outraged.has(Fuel).not
                                case Tyrant => true
                                case Warlord => f.outraged.has(Weapon).not
                                case Keeper => f.outraged.has(Relic).not
                                case Empath => f.outraged.has(Psionic).not
                            })
                                f.advance(2, $("winning non-outraged ambition"))

                        if (f.objective.has(ContactAnAncientIntelligence))
                            f.advance(systems./(s => f.at(s).shiplikes.any.??(Blights.at(s).num)).sum, $("contacting"))

                        if (f.objective.has(PlantBanners))
                            log(f, PlantBanners, ">", game.winners.values.$, "<", game.winners.values.$.count(f))

                        if (f.objective.has(PlantBanners) && game.winners.values.$.count(f) == 1)
                            f.advance(systems.%(_.$.hasA(Banner)).num, $("for each banner winning ambition"))

                        if (f.objective.has(KeepThePeopleSafe))
                            f.advance(min(f.captives.num, systems./(f.at(_).bunkers.num).sum), $("keeping the people safe"))

                        if (f.objective.has(CollapseTheGates))
                            f.advance(game.broken.num, $("collapsing the gates"))

                        if (f.objective.has(SubdueTheKeepers)) {
                            if (ambition == Keeper)
                                f.advance(3, $("subduing the keepers"))

                            f.used :+= SubdueTheKeepers
                        }

                        if (ambition == Blightkin)
                            if (f.objective.has(LiveInTheGarden))
                                f.advance(systems.%(s => Blights.at(s).fresh.any).%(f.rulesAOE).num, $("living in the garden"))

                        if (ambition == Tyrant)
                            if (f.objective.has(GiveBackToTheLand))
                                f.advance(GreenVaultSlots.$.num, $("giving back to the land"))

                        if (ambition == Edenguard)
                            if (f.objective.has(GiveBackToTheLand))
                                f.advance(systems.%!(_.gate).%(s => game.resources(s).use(l => l.has(Material) || l.has(Fuel))).num, $("guarding"))
                    }

                    if (low > 0)
                    second.foreach { f =>
                        var p = low

                        if (ambition == Tycoon && f.hasTrait(Academic))
                            p = 0

                        if (ambition == Tyrant && f.hasTrait(Just))
                            p = 0

                        if (ambition == Empath && f.hasTrait(Violent))
                            p = 0

                        if (f.hasTrait(Proud))
                            p = 0

                        if (ambition == Tycoon && f.hasLore(IreOfTheTycoons))
                            p = 0

                        if (ambition == Warlord && f.hasLore(OathOfPeace))
                            p = 0

                        f.power += p

                        f.log("scored second place", ambition, "for", p.power)
                    }

                    if (ambition == Warlord)
                        factions.foreach { f =>
                            val n = f.trophies.$.piece(Banner).num
                            val p = n * 2

                            if (p > 0) {
                                f.power += p

                                f.log("scored extra", p.power, "for banners")
                            }
                        }

                    if (game.conspired.contains(ambition)) {
                        val l = game.conspired(ambition)./(_.faction)
                        game.revealed += ambition -> l./(Revealed)
                        game.conspired -= ambition

                        val f = factions.%(_.objective.has(ControlTheProceedings)).only

                        f.log("conspired", ambition, "for", l.intersperse("and"))

                        l.foreach { e =>
                            if (ff.has(e))
                                f.advance(1, $("plotting for", e))
                            else
                                f.advance(-1, $("plotting for", e))
                        }

                    }

                    if (second.num > 1) {
                        factions.foreach { f =>
                            if (f.objective.has(BalanceTheReach))
                                f.advance(1, $("balancing the reach"))
                        }
                    }

                }
            }

            if (game.act == 3)
                Then(factions.reverse.foldLeft(ScoreGrandAmbitionsAction : ForcedAction)((q, f) => CheckGrandAmbitionsAction(f, q)))
            else
                Milestone(CheckWinAction)

        case CheckGrandAmbitionsAction(f, then) =>
            then

        case ScoreGrandAmbitionsAction =>
            game.portal.foreach { s =>
                factions.foreach { f =>
                    if (f.hasGuild(PortalSeekers))
                        if (f.rulesAOE(s)) {
                            f.grand += 1

                            f.log("fulfilled an extra grand ambition with", PortalSeekers)
                        }
                }
            }

            if (game.act == 3) {
                factions.foreach { f =>
                    f.grand @@ {
                        case 0 =>
                        case 1 =>
                            val p = game.chapter @@ {
                                case 1 => 2
                                case 2 => 3
                                case 3 => 4
                                case 4 => 5
                            }

                            f.power += p

                            f.log("scored", p.power, "fulfilling a grand ambition")
                        case _ =>
                            val p = game.chapter @@ {
                                case 1 => 4
                                case 2 => 8
                                case 3 => 14
                                case 4 => 20
                            }

                            f.power += p

                            f.log("scored", p.power, "fulfilling two grand ambitions")
                    }

                    f.grand = 0
                }
            }

            Milestone(CheckWinAction)

        case CartelCleanUpAction(then) =>
            $(MaterialCartel, FuelCartel, WeaponCartel, RelicCartel, PsionicCartel).foreach { c =>
                factions.%(_.hasGuild(c)).some.foreach { f =>
                    factions.diff(f).foreach { e =>
                        val l = e.spendable.resources.content.%(_.is(c.suit))

                        if (l.any) {
                            l.foreach(_ --> Supply(c.suit))

                            e.log("discarded", l.comma, "due to", c)
                        }
                    }
                }
            }

            then

        case CleanUpChapterAction(then) =>
            if (game.declared.contains(Tyrant)) {
                factions.foreach { f =>
                    if (f.hasLore(VowOfSurvival).not && f.hasGuild(HappyHosts).not)
                        f.captives.$.some./ { l =>
                            l.foreach { u =>
                                u --> u.faction.reserve
                            }

                            f.log("returned captives", l.comma)
                        }

                    if (f.hasLore(GreenVault)) {
                        GreenVaultSlots.$.some./ { l =>
                            l.of[ResourceToken].foreach { r =>
                                r --> r.supply
                            }

                            f.log("returned captive resources", l.comma)
                        }
                    }
                }
            }

            if (game.laws.has(TheDeadLive)) {
                (TheDeadLive.$ ++ factions./~(_.trophies.$.ships)).some./ { l =>
                    l.foreach { u =>
                        u --> GateWraithExpansion.Passage
                    }

                    log(TheDeadLive, "revived", l.comma)
                }
            }

            if (factions.%(_.hasLore(BlightHunger)).any) {
                BlightHunger.$.some./ { l =>
                    l.foreach { u =>
                        u --> u.faction.reserve
                    }

                    log(BlightHunger, "returned", l.comma)
                }
            }

            if (game.declared.contains(Warlord)) {
                factions.foreach { f =>
                    f.trophies.$.some./ { l =>
                        l.foreach { u =>
                            u --> u.faction.reserve
                        }

                        f.log("returned trophies", l.comma)
                    }
                }
            }

            if (game.declared.contains(Tycoon)) {
                factions.foreach { f =>
                    if (f.hasTrait(Lavish)) {
                        val l = f.spendable.resources.lefts.%(_.is(Fuel))

                        if (l.any) {
                            l.foreach(_ --> Supply(Fuel))

                            f.log("discarded", l.comma, "due to", Lavish)
                        }
                    }
                }
            }

            game.winners = Map()

            factions.foreach(_.used = $)

            factions.foreach(_.recalculateSlots())

            AdjustResourcesAction(then)


        // ...
        case _ => UnknownContinue
    }
}
