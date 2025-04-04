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


case class StartAction(version : String) extends StartGameAction with GameVersion
case class PlayOrderAction(random : $[Faction]) extends RandomAction[$[Faction]]
case object StartSetupAction extends ForcedAction
case object RandomizeAction extends ForcedAction
case object RandomizePlanetResourcesAction extends ForcedAction
case class PlanetResourcesAction(shuffled : $[Resource], then : ForcedAction) extends ShuffledAction[Resource]
case object RandomizeStartingSystemsAction extends ForcedAction
case class StartingSystemsAction(shuffled : $[System], then : ForcedAction) extends ShuffledAction[System]
case object CourtSetupAction extends ForcedAction
case class ShuffleCourtDiscardAction(then : ForcedAction) extends ForcedAction
case class ShuffleCourtDeckAction(then : ForcedAction) extends ForcedAction
case class ShuffledCourtDeckAction(shuffled : $[CourtCard], then : ForcedAction) extends ShuffledAction[CourtCard]
case class ReplenishMarketAction(then : ForcedAction) extends ForcedAction
case object FactionsSetupAction extends ForcedAction
case object BaseFactionsSetupAction extends ForcedAction
case object StartChapterAction extends ForcedAction
case object CheckWinAction extends ForcedAction
case object DealCardsAction extends ForcedAction
case class ShuffleDeckCardsAction(shuffled : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]
case object StartRoundAction extends ForcedAction
case class LeadMainAction(self : Faction) extends ForcedAction with Soft
case class LeadAction(self : Faction, d : DeckCard) extends ForcedAction
case class PassAction(self : Faction) extends ForcedAction
case class CheckAmbitionAction(self : Faction, d : DeckCard) extends ForcedAction with Soft
case class FollowAction(self : Faction) extends ForcedAction with Soft
case class SurpassAction(self : Faction, d : DeckCard) extends ForcedAction
case class CopyAction(self : Faction, d : DeckCard) extends ForcedAction
case class PivotAction(self : Faction, d : DeckCard) extends ForcedAction
case class MirrorAction(self : Faction, d : DeckCard) extends ForcedAction
case class CheckSeizeAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class SeizeAction(self : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class DeclareAmbitionAction(self : Faction, ambition : Ambition, zero : Boolean, then : ForcedAction) extends ForcedAction
case class AmbitionDeclaredAction(self : Faction, ambition : Ambition, used : $[Effect], then : ForcedAction) extends ForcedAction with Soft
case class PreludeActionAction(self : Faction, suit : Suit, pips : Int) extends ForcedAction

case class AdjustResourcesAction(then : ForcedAction) extends ForcedAction with Soft

case class AdjustingResourcesAction(self : Faction, l : $[Resource], then : ForcedAction) extends HiddenChoice with Soft with NoExplode
case class ExplodeReorderResourcesAction(self : Faction, slots : Int, l : $[Resource], then : ForcedAction) extends HiddenChoice with SelfExplode with SelfValidate {
    def validate(target : Action) = target @@ {
        case AdjustingResourcesAction(f, m, t) => self == f && then == t && m.toSet.equals(l.toSet)
        case ReorderResourcesAction(f, m, t) => self == f && then == t && m.toSet.equals(l.toSet)
        case _ => false
    }

    def explode(withSoft : Boolean) = {
        val c = l.permutations.$
        val cc = c.%(l => l.drop(slots).but(Nothingness).none || l.take(slots).has(Nothingness).not)

        cc./(ReorderResourcesAction(self, _, then).as("Done")) ++ withSoft.??(c./(AdjustingResourcesAction(self, _, then)))
    }
}
case class ReorderResourcesAction(self : Faction, l : $[Resource], then : ForcedAction) extends ForcedAction


case class TaxMainAction(self : Faction, cost : Cost, effect : |[Effect], then : ForcedAction) extends ForcedAction with Soft
case class TaxAction(self : Faction, cost : Cost, effect : |[Effect], s : System, c : |[Figure], loyal : Boolean, then : ForcedAction) extends ForcedAction
case class TaxGainAction(self : Faction, r : |[Resource], along : Boolean, then : ForcedAction) extends ForcedAction
case class PostTaxAction(self : Faction, s : System, c : |[Figure], loyal : Boolean, then : ForcedAction) extends ForcedAction


case class MayMoveAction(self : Faction, effect : |[Effect], then : ForcedAction) extends ForcedAction with ThenDesc { def desc = "(then may " ~ "Move".hh ~ ")" }

case class MoveMainAction(self : Faction, cost : Cost, effect : |[Effect], skip : Boolean, cancel : Boolean, then : ForcedAction) extends ForcedAction with Soft
case class MoveFromAction(self : Faction, r : System, l : $[Figure], cascade : Boolean, x : Cost, effect : |[Effect], alt : UserAction, then : ForcedAction) extends ForcedAction with Soft
case class MoveToAction(self : Faction, r : System, d : System, l : $[Figure], cascade : Boolean, x : Cost, effect : |[Effect], then : ForcedAction) extends ForcedAction with Soft
case class MoveListAction(self : Faction, r : System, d : System, l : $[Figure], cascade : Boolean, x : Cost, effect : |[Effect], then : ForcedAction) extends ForcedAction

case class MayBattleAction(self : Faction, effect : |[Effect], then : ForcedAction) extends ForcedAction with ThenDesc { def desc = "(then may " ~ "Battle".hh ~ ")" }
case class MustBattleAction(self : Faction, effect : |[Effect], then : ForcedAction) extends ForcedAction with ThenDesc { def desc = "(then must " ~ "Battle".hh ~ ")" }
case class BattleMainAction(self : Faction, cost : Cost, effect : |[Effect], skip : Boolean, cancel : Boolean, then : ForcedAction) extends ForcedAction with Soft
case class BattleSystemAction(self : Faction, cost : Cost, effect : |[Effect], s : System, then : ForcedAction) extends ForcedAction with Soft
case class BattleFactionAction(self : Faction, cost : Cost, effect : |[Effect], s : System, e : Faction, then : ForcedAction) extends ForcedAction
case class BattleStartAction(self : Faction, s : System, e : Faction, used : $[Effect], then : ForcedAction) extends ForcedAction
case class BattleDiceAction(self : Faction, s : System, e : Faction, skirmish : Int, assault : Int, raid : Int, used : $[Effect], then : ForcedAction) extends ForcedAction
case class BattleReRollAction(self : Faction, s : System, e : Faction, skirmish : Rolled, assault : Rolled, raid : Rolled, rolled1 : Int, rolled2 : Int, rolled3 : Int, used : $[Effect], then : ForcedAction) extends ForcedAction
case class BattleRolledAction(self : Faction, s : System, e : Faction, skirmish : Rolled, assault : Rolled, raid : Rolled, rolled1 : Rolled, rolled2 : Rolled, rolled3 : Rolled, used : $[Effect], then : ForcedAction) extends Rolled3Action[$[BattleResult], $[BattleResult], $[BattleResult]]
case class SkirmishersAction(self : Faction, s : System, e : Faction, skirmish : Rolled, assault : Rolled, raid : Rolled, reroll : Rolled, used : $[Effect], then : ForcedAction) extends ForcedAction
case class SeekerTorpedoesAction(self : Faction, s : System, e : Faction, skirmish : Rolled, assault : Rolled, raid : Rolled, reroll : Rolled, used : $[Effect], then : ForcedAction) extends ForcedAction
case class TrickyAction(self : Faction, s : System, e : Faction, skirmish : Rolled, assault : Rolled, raid : Rolled, reroll : Rolled, used : $[Effect], then : ForcedAction) extends ForcedAction
case class BattleProcessAction(self : Faction, s : System, e : Faction, skirmish : Rolled, assault : Rolled, raid : Rolled, used : $[Effect], then : ForcedAction) extends ForcedAction
case class BattleRaidAction(self : Faction, s : System, e : Faction, raid : Int, then : ForcedAction) extends ForcedAction with Soft
case class BattleRaidResourceAction(self : Faction, e : Faction, r : Resource, keys : Int, then : ForcedAction) extends ForcedAction
case class BattleRaidCourtCardAction(self : Faction, e : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction
case class PostBattleAction(self : Faction, s : System, e : Faction, used : $[Effect], then : ForcedAction) extends ForcedAction

case class PromptRepairsAction(self : Faction, s : System, n : Int, effect : |[Effect], then : ForcedAction) extends ForcedAction with Soft
case class RepairsAction(self : Faction, s : System, l : $[Figure], effect : |[Effect], then : ForcedAction) extends ForcedAction



case class AssignHitsAction(self : Faction, s : System, f : Faction, e : Faction, l : $[Figure], hits : Int, bombardments : Int, raid : Int, effect : |[Effect], used : $[Effect], then : ForcedAction) extends ForcedAction with Soft
case class DealHitsAction(self : Faction, s : System, f : Faction, e : Faction, l : $[Figure], raid : Int, effect : |[Effect], used : $[Effect], then : ForcedAction) extends ForcedAction
case class OutrageAction(self : Faction, r : Resource, then : ForcedAction) extends ForcedAction
case class ClearOutrageAction(self : Faction, r : $[Resource], then : ForcedAction) extends ForcedAction
case class RansackMainAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction with Soft
case class RansackAction(self : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction

case class DiscardResourcesMainAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class DiscardResourceNoEffectAction(self : Faction, cost : PayResource, then : ForcedAction) extends ForcedAction

case class DiscardGuildCardsMainAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class DiscardGuildCardNoEffectAction(self : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction

case class BuildMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class BuildCityAction(self : Faction, cost : Cost, s : System, effect : |[Effect], then : ForcedAction) extends ForcedAction
case class BuildStarportAction(self : Faction, cost : Cost, s : System, then : ForcedAction) extends ForcedAction
case class BuildShipAction(self : Faction, cost : Cost, s : System, b : Figure, effect : |[Effect], then : ForcedAction) extends ForcedAction

case class RepairMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class RepairAction(self : Faction, cost : Cost, r : System, u : Figure, then : ForcedAction) extends ForcedAction

case class InfluenceMainAction(self : Faction, cost : Cost, effect : |[Effect], skip : Boolean, cancel : Boolean, then : ForcedAction) extends ForcedAction with Soft
case class InfluenceAction(self : Faction, cost : Cost, c : CourtCard, effect : |[Effect], then : ForcedAction) extends ForcedAction
case class MayInfluenceAction(self : Faction, effect : |[Effect], then : ForcedAction) extends ForcedAction with ThenDesc { def desc = "(then may " ~ "Influence".hh ~ ")" }

case class SecureMainAction(self : Faction, cost : Cost, effect : |[Effect], skip : Boolean, cancel : Boolean, then : ForcedAction) extends ForcedAction with Soft
case class SecureAction(self : Faction, cost : Cost, effect : |[Effect], c : CourtCard, then : ForcedAction) extends ForcedAction
case class MustSecureAction(self : Faction, effect : |[Effect], then : ForcedAction) extends ForcedAction with ThenDesc { def desc = "(then must " ~ "Secure".hh ~ ")" }

case class AddBattleOptionAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction

case class ManufactureMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction
case class SynthesizeMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction

case class ReserveCardMainAction(self : Faction, c : GuildCard, l : $[DeckCard], then : ForcedAction) extends ForcedAction with Soft
case class ReserveCardAction(self : Faction, c : GuildCard, d : DeckCard, then : ForcedAction) extends ForcedAction

case class FillSlotsMainAction(self : Faction, x : Resource, then : ForcedAction) extends ForcedAction
case class StealResourceMainAction(self : Faction, x : |[Resource], extra : $[UserAction], then : ForcedAction) extends ForcedAction with Soft
case class StealResourceAction(self : Faction, e : Faction, x : Resource, k : Int, then : ForcedAction) extends ForcedAction

case class StealGuildCardMainAction(self : Faction, alt : $[UserAction], then : ForcedAction) extends ForcedAction with Soft
case class StealGuildCardAction(self : Faction, e : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction

case class ShipAtEachGateMainAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class ShipsInSystemsAction(self : Faction, l : $[System], then : ForcedAction) extends ForcedAction

case class ShipsInSystemMainAction(self : Faction, l : $[System], then : ForcedAction) extends ForcedAction with Soft
case class ShipsInSystemAction(self : Faction, r : System, then : ForcedAction) extends ForcedAction

case class PressgangMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class PressganAction(self : Faction, u : Figure, r : Resource, cost : Cost, then : ForcedAction) extends ForcedAction

case class ExecuteMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class ExecuteAction(self : Faction, l : $[Figure], cost : Cost, then : ForcedAction) extends ForcedAction

case class AbductMainAction(self : Faction, l : $[CourtCard], cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class AbductAction(self : Faction, c : CourtCard, cost : Cost, then : ForcedAction) extends ForcedAction

case class FarseersMainAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction with Soft
case class FarseersAction(self : Faction, e : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class FarseersBackAction(self : Faction, e : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction

case class FarseersRedrawMainAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class FarseersRedrawAction(self : Faction, l : $[DeckCard], then : ForcedAction) extends ForcedAction

case class FenceResourceAction(self : Faction, r : Resource, cost : Cost, then : ForcedAction) extends ForcedAction
case class GainResourcesAction(self : Faction, r : $[Resource], then : ForcedAction) extends ForcedAction

case class TradeMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class TakeResourceAction(self : Faction, e : Faction, r : Resource, k : Int, cost : Cost, then : ForcedAction) extends ForcedAction
case class GiveBackResourceMainAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction with Soft
case class GiveBackResourceAction(self : Faction, e : Faction, r : Resource, then : ForcedAction) extends ForcedAction


case class LatticeSeizeAction(self : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction

case class FreeCityAction(self : Faction, r : System, u : Figure, then : ForcedAction) extends ForcedAction
case class FreeCitySeizeAskAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class FreeCitySeizeAction(self : Faction, then : ForcedAction) extends ForcedAction

case class OutrageSpreadsAction(self : Faction, r : Resource, then : ForcedAction) extends ForcedAction

case class GainCourtCardAction(self : Faction, c : CourtCard, from : |[Faction], then : ForcedAction) extends ForcedAction
case class DiscardCourtCardAction(self : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction
case class BuryCourtCardAction(self : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction

case class UseEffectAction(self : Faction, c : Effect, then : ForcedAction) extends ForcedAction
case class ClearEffectAction(self : Faction, c : Effect, then : ForcedAction) extends ForcedAction

case class EndPreludeAction(self : Faction, suit : Suit, done : Int, total : Int) extends ForcedAction
case class MainTurnAction(self : Faction, suit : Suit, done : Int, total : Int) extends ForcedAction
case class EndTurnAction(self : Faction) extends ForcedAction
case object EndRoundAction extends ForcedAction
case class TransferInitiativeAction(f : Faction) extends ForcedAction
case object ContinueRoundsAction extends ForcedAction
case object EndChapterAction extends ForcedAction
case object ScoreChapterAction extends ForcedAction
case object CleanUpChapterAction extends ForcedAction

case class SecureWith(e : |[Effect]) extends Message {
    def elem(implicit game : Game) = game.desc("Secure".hl, e./("with" -> _))
}

case class BattleWith(e : |[Effect]) extends Message {
    def elem(implicit game : Game) = game.desc("Battle".hl, e./("with" -> _))
}

case class DeadlockAction(self : Faction, message : Message, then : ForcedAction) extends BaseAction(Empty)(Empty)
case class CheatAction(self : Faction, message : Message, then : ForcedAction) extends BaseAction(self, "made an illegal move, must", message, "but cannot", Break, "Please", "UNDO".hlb, "your move")(Empty) with Choice


case class GameOverAction(winner : Faction) extends ForcedAction
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

            game.setup.foreach { f =>
                game.states += f -> new FactionState(f)
            }

            if (campaign) {
                game.states += Blights -> new BlightsState(Blights)
                game.states += Empire -> new EmpireState(Empire)
                game.states += Free -> new FreeState(Free)
            }

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
            if (options.has(RandomizePlanetResources) && game.overridesSoft.none)
                RandomizePlanetResourcesAction
            else
            if (options.has(RandomizeStartingSystems) && game.starting.none)
                RandomizeStartingSystemsAction
            else
                Milestone(StartSetupAction)

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

            if (chapter > 0)
                log("Guild cards from court discard were added to the court deck")

            ShuffleCourtDeckAction(then)

        case ShuffleCourtDeckAction(then) =>
            Shuffle[CourtCard](game.court, ShuffledCourtDeckAction(_, then))

        case ShuffledCourtDeckAction(l, then) =>
            game.court --> l --> game.court

            log("The court deck was shuffled")

            then

        case ReplenishMarketAction(then) =>
            while (game.court.any && market.num < 4 + campaign.??(1)) {
                game.court.take(1) --> market

                if (chapter > 0)
                    log("A card was added to the market")
            }

            if (chapter == 0)
                log("Cards were added to the market")

            then

        case FactionsSetupAction =>
            val leaders =
                options.has(LeadersAndLorePreset1).??(Leaders.preset1) ++
                options.has(LeadersAndLorePreset2).??(Leaders.preset2) ++
                options.has(LeadersAndLorePreset3).??(Leaders.preset3) ++
                options.has(LeadersAndLorePreset4).??(Leaders.preset4)

            val lores =
                options.has(LeadersAndLorePreset1).??(Lores.preset1) ++
                options.has(LeadersAndLorePreset2).??(Lores.preset2) ++
                options.has(LeadersAndLorePreset3).??(Lores.preset3) ++
                options.has(LeadersAndLorePreset4).??(Lores.preset4)

            if (leaders.num >= factions.num && lores.num >= factions.num)
                Shuffle2[Leader, Lore](leaders, lores, (l1, l2) => LeadersLoresShuffledAction(l1, l2))
            else
                BaseFactionsSetupAction

        case BaseFactionsSetupAction =>
            factions.lazyZip(game.starting).foreach { case (f, (city, port, fleets)) =>
                f.reserve --> City.of(f) --> city
                f.reserve --> Ship.of(f) --> city
                f.reserve --> Ship.of(f) --> city
                f.reserve --> Ship.of(f) --> city
                f.log("placed", City.of(f), "and", Ship.sof(f), "in", city)

                f.reserve --> Starport.of(f) --> port
                f.reserve --> Ship.of(f) --> port
                f.reserve --> Ship.of(f) --> port
                f.reserve --> Ship.of(f) --> port
                f.log("placed", Starport.of(f), "and", Ship.sof(f), "in", port)

                fleets.foreach { fleet =>
                    f.reserve --> Ship.of(f) --> fleet
                    f.reserve --> Ship.of(f) --> fleet
                    f.log("placed", Ship.sof(f), "in", fleet)
                }

                f.resources :+= board.resource(city)
                f.resources :+= board.resource(port)

                f.log("took", board.resource(city), "and", board.resource(port))
            }

            StartChapterAction

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
            if (factions.exists(_.adjust))
                MultiAsk(factions.%(_.adjust)./~(f => game.internalPerform(AdjustingResourcesAction(f, f.resources, AdjustResourcesAction(then)), NoVoid).as[Ask]))
            else
                Then(then)

        case AdjustingResourcesAction(f, ll, then) =>
            val kk = f.resourceSlots

            val keys = f.keys.take(kk)./(n => Image("keys-" + n, styles.token3x).spn(styles.card0)(styles.circle)) ~ (f.resources.num - kk).times(Image("discard-resource", styles.token3x).spn(styles.card0)(styles.circle))

            val l = ll ++ (kk - ll.num).times(Nothingness)

            implicit def convert(r : Resource) = Image(r.name, styles.token3x)

            if (l.but(Nothingness).any)
                XXSelectObjectsAction(f, l)
                    .withGroup("Adjust resources" ~ Break ~ keys)
                    .withSplit($(l.num))
                    .withBreak({
                        case 0 => Empty
                        case _ => HorizontalBreak
                    })
                    .withRule(rule => rule.num(2).all(ss => ss.num < 2 || ss(0) != ss(1)))
                    .withAutoIndex(ii => AdjustingResourcesAction(f, 0.until(l.num)./{
                        case i if i == ii(0) => l(ii(1))
                        case i if i == ii(1) => l(ii(0))
                        case i => l(i)
                    }, then))
                    .withExtra($(ReorderResourcesAction(f, l, then).as("Done").!(l.drop(kk).but(Nothingness).any && l.take(kk).has(Nothingness)), ExplodeReorderResourcesAction(f, kk, l, then)))
                    .ask
            else
                NoAsk(f)(ReorderResourcesAction(f, f.resources, then))

        case ReorderResourcesAction(f, l, then) =>
            if (l != f.resources) {
                f.resources = l

                f.log("reordered resources")
            }

            f.resources.drop(f.resourceSlots).but(Nothingness).foreach { r =>
                f.log("discarded", r)
            }

            f.resources = f.resources.take(f.resourceSlots)

            f.adjust = false

            then

//[[ GREENER
        // COURT
        case FillSlotsMainAction(f, r, then) =>
            val adjust = then.as[AdjustResourcesAction].|(AdjustResourcesAction(then))

            if (f.resources.has(Nothingness) || (f.resourceSlots > f.resources.num)) {
                if (game.available(r)) {
                    f.gain(r, $)

                    FillSlotsMainAction(f, r, adjust)
                }
                else
                if (f.rivals.exists(_.stealable(r)))
                    StealResourceMainAction(f, |(r), $(), FillSlotsMainAction(f, r, adjust))
                else
                    then
            }
            else
                then

        case StealResourceMainAction(f, x, extra, then) =>
            Ask(f).group("Steal", x.|("Resource"))
                .some(f.rivals.%(e => x./(e.resources.has).|(e.resources.but(Nothingness).any))) { e =>
                    e.resKeys.%<(r => x.but(r).none)./ { case (x, k) =>
                        StealResourceAction(f, e, x, k, then).as(ResourceRef(x, |(k)), "from", e)
                            .!(e.loyal.exists(_.as[GuildCard].?(_.effect == SwornGuardians)))
                    }
                }
                .needOk
                .add(extra)

        case StealResourceAction(f, e, r, k, then) =>
            e.remove(ResourceRef(r, |(k)))

            f.gain(r)

            f.log("stole", ResourceRef(r, |(k)))

            AdjustResourcesAction(then)

        case StealGuildCardMainAction(f, alt, then) =>
            Ask(f).group("Steal".hl, "a", "Guild Card".hh)
                .some(f.rivals)(e => e.loyal.of[GuildCard]./ { c =>
                    StealGuildCardAction(f, e, c, then).as(c, "from", e)
                        .!(e.loyal.but(c).exists(_.as[GuildCard].?(_.effect == SwornGuardians)))
                })
                .add(alt)

        case StealGuildCardAction(f, e, c, then) =>
            e.loyal --> c --> f.loyal

            f.log("stole", c)

            GainCourtCardAction(f, c, |(e), then)

        case ReserveCardMainAction(f, c, l, then) =>
            Ask(f).group("Take played card")
                .each(l)(d => ReserveCardAction(f, c, d, then).view(d)(_.img))
                .cancel

        case ReserveCardAction(f, c, d, then) =>
            f.log("reserved", d, "with", c)

            f.loyal --> c --> f.discardAfterRound

            f.taking :+= d

            then

        case ShipAtEachGateMainAction(f, then) =>
            val ss = systems.%(_.gate)
            val pp = min(ss.num, f.pooled(Ship))

            Ask(f).group("Place", Ship.of(f), "at gates")
                .each(ss.combinations(pp).$)(l => ShipsInSystemsAction(f, l, then).as(l.comma))
                .needOk
                .cancel

        case ShipsInSystemsAction(f, l, then) =>
            l.foreach { r =>
                f.reserve --> Ship --> r

                f.log("placed", Ship.of(f), "in", r)
            }

            then

        case ShipsInSystemMainAction(f, l, then) =>
            Ask(f).group("Place", min(3, f.pooled(Ship)).hlb, Ship.sof(f), "in")
                .each(l)(r => ShipsInSystemAction(f, r, then).as(r))
                .needOk
                .cancel

        case ShipsInSystemAction(f, r, then) =>
            val l = f.reserve.$.ships.take(3)

            l --> r

            f.log("placed", l.comma, "in", r)

            then

        case PressgangMainAction(f, x, then) =>
            val done = (x == AlreadyPaid).?(AdjustResourcesAction(then).as("Done")).|(CancelAction)

            implicit def convert(u : Figure, selected : Boolean) = game.showFigure(u, selected.??(2))

            if (f.captives.any)
                YYSelectObjectsAction(f, f.captives)
                    .withGroup("Pressgang captives", x)
                    .withThens(u => Resources.all.%(game.available)./(r => PressganAction(f, u, r, x, then).as(ResourceRef(r, None))))
                    .withExtras(done)
            else
                Ask(f)(done)

        case PressganAction(f, u, r, x, then) =>
            f.pay(x)

            u --> u.faction.reserve

            f.gain(r, $("releasing", u, x))

            PressgangMainAction(f, AlreadyPaid, then)

        case ExecuteMainAction(f, x, then) =>
            implicit def convert(u : Figure, selected : Boolean) = game.showFigure(u, selected.??(2))

            XXSelectObjectsAction(f, f.captives)
                .withGroup("Execute captives", x)
                .withThen(l => ExecuteAction(f, l, x, then))(l => ("Execute", l))
                .withExtras(CancelAction)

        case ExecuteAction(f, l, x, then) =>
            f.pay(x)

            f.captives --> l --> f.trophies

            f.log("executed", l, x)

            then

        case AbductMainAction(f, l, x, then) =>
            Ask(f).group("Abduct".hl, "Agents", x)
                .each(market)(c => AbductAction(f, c, x, then).as(c).!(l.has(c).not))
                .cancel

        case AbductAction(f, c, x, then) =>
            f.pay(x)

            val l = Influence(c).%(_.faction != f)

            l --> f.captives

            f.log("abducted", l.comma, x)

            then

        case FarseersMainAction(f, e, then) =>
            YYSelectObjectsAction(f, e.hand)
                .withGroup(Farseers, "look at", e, "cards")
                .withThen(d => FarseersAction(f, e, d, then))(d => ("Take", d, "from", e))("Take a card from", e)
                .withExtras(then.as("Skip"))

        case FarseersAction(f, e, d, then) =>
            e.hand --> d --> f.hand

            f.log("took a card from", e, "with", Farseers)

            YYSelectObjectsAction(f, f.hand)
                .withGroup(Farseers, "give back", e, "a card")
                .withThen(d => FarseersBackAction(f, e, d, then))(d => ("Give", d, "to", e))("Give a card back to", e)
                .withExtras(NoHand)

        case FarseersBackAction(f, e, d, then) =>
            f.hand --> d --> e.hand

            f.log("gave back a card to", e)

            then

        case FarseersRedrawMainAction(f, then) =>
            XXSelectObjectsAction(f, f.hand)
                .withGroup(Farseers, "discard cards to redraw")
                .withRule(_.upTo(deck.any.?(deck.num).|(discard.num) - 1))
                .withThen(l => FarseersRedrawAction(f, l, then))(l => ("Discard", Farseers, "and", l.num.cards, "to draw", (l.num + 1).cards))
                .withExtras(NoHand, FarseersRedrawAction(f, $, then).as("Discard", Farseers, "only to draw", 1.cards), CancelAction)

        case FarseersRedrawAction(f, l, then) =>
            l.foreach { d =>
                if (f.hand.has(d))
                    d --> deck
            }

            if (l.num + 1 > deck.num && discard.any) {
                log("Face up discard added to face down discard")

                Shuffle[DeckCard](deck.$ ++ discard.$, ShuffleDeckCardsAction(_, FarseersRedrawAction(f, l, then)))
            } else {
                val r = deck.take(l.num + 1)

                r --> f.hand

                f.log("discarded", l.num.cards, "and drew", r.num.cards)

                then
            }

        case FenceResourceAction(f, r, x, then) =>
            f.pay(x)

            f.gain("fenced", Relic, $(x))

            AdjustResourcesAction(then)

        case GainResourcesAction(f, l, then) =>
            l.foreach { r => f.gain(r, $) }

            AdjustResourcesAction(then)
//]]

//[[ PINKER
        // ON SECURE
        case GainCourtCardAction(f, c @ VoxCard(_, CallToAction), e, then) =>
            deck.take(1) --> f.hand

            f.log("drew", 1.cards)

            DiscardCourtCardAction(f, c, then)

        case GainCourtCardAction(f, c @ VoxCard(_, PopulistDemands), e, then) =>
            val next = DiscardCourtCardAction(f, c, then)

            if (game.ambitionable.any)
                Ask(f).group("Declare Ambition".hl, "with", c)
                    .add(DeclareAmbitionAction(f, Tycoon,  false, next).use(a => a.as(a.ambition)))
                    .add(DeclareAmbitionAction(f, Tyrant,  false, next).use(a => a.as(a.ambition)))
                    .add(DeclareAmbitionAction(f, Warlord, false, next).use(a => a.as(a.ambition)))
                    .add(DeclareAmbitionAction(f, Keeper,  false, next).use(a => a.as(a.ambition)))
                    .add(DeclareAmbitionAction(f, Empath,  false, next).use(a => a.as(a.ambition)))
                    .add(next.as("Skip"))
            else
                Ask(f).group("Declare Ambition".hl, "with", c).add(next.as("No Ambition Markers")).needOk

        case GainCourtCardAction(f, c @ VoxCard(_, MassUprising), e, then) =>
            val next = DiscardCourtCardAction(f, c, then)

            val nn = systems./(_.cluster).distinct
            val pp = min(4, f.pooled(Ship))

            if (pp > 0)
                Ask(f).group("Place", Ship.sof(f), "in a cluster")
                    .some(nn)(n => systems.%(_.cluster == n).combinations(pp)./(l => ShipsInSystemsAction(f, l, next).as(l.comma)))
                    .needOk
                    .cancel
            else
                NoAsk(f)(next)

        case GainCourtCardAction(f, c @ VoxCard(_, GuildStruggle), e, then) =>
            val next = ShuffleCourtDiscardAction(DiscardCourtCardAction(f, c, then))

            StealGuildCardMainAction(f, $(next.as("Skip")), next)

        case GainCourtCardAction(f, c @ VoxCard(_, SongOfFreedom), e, then) =>
            val next = BuryCourtCardAction(f, c, ShuffleCourtDeckAction(then))

            val l = systems.%(f.rules)

            Ask(f).group(c, "frees a", "City".hl)
                .some(l)(r => factions./~(_.at(r).cities)./(u => FreeCityAction(f, r, u, next).as(u, "in", r)))
                .skip(next)

        case FreeCityAction(f, r, u, then) =>
            u --> u.faction.reserve

            u.faction.damaged :-= u

            f.log("freed", u, "in", r)

            u.faction.as[Faction].foreach { e =>
                if (e.pooled(City) > 2)
                    e.adjust = true
            }

            AdjustResourcesAction(FreeCitySeizeAskAction(f, then))

        case FreeCitySeizeAskAction(f, then) =>
            if (game.seized.none && factions.first != f)
                Ask(f).group("Initiative")
                    .add(FreeCitySeizeAction(f, then).as("Seize".hl))
                    .skip(then)
            else
                NoAsk(f)(then)

        case FreeCitySeizeAction(f, then) =>
            game.seized = |(f)

            f.log("seized the initative")

            then

        case GainCourtCardAction(f, c @ VoxCard(_, OutrageSpreads), e, then) =>
            val next = DiscardCourtCardAction(f, c, then)

            Ask(f).group(c)
                .each(Resources.all)(r => OutrageSpreadsAction(f, r, next).as(ResourceRef(r, None)))
                .skip(next)

        case OutrageSpreadsAction(f, r, then) =>
            val l = factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f)

            l.foldLeft(then)((q, e) => OutrageAction(e, r, q))

        case GainCourtCardAction(f, c @ GuildCard(_, SwornGuardians), Some(e), then) =>
            BuryCourtCardAction(f, c, then)
//]]

        // COURT CARDS
        case GainCourtCardAction(f, c : GuildCard, e, then) =>
            then

        case DiscardCourtCardAction(f, c, then) =>
            f.loyal --> c --> game.discourt

            f.log("discarded", c)

            then

        case BuryCourtCardAction(f, c, then) =>
            f.log("buried", c)

            f.loyal --> c --> game.court

            then

        case UseEffectAction(f, c, then) =>
            f.used :+= c

            then

        case ClearEffectAction(f, c, then) =>
            f.used :-= c

            then

        // TAX
        case TaxMainAction(f, x, effect, then) =>
            def res(s : System) = game.resources(s).distinct.%(game.available).some./(l => ("for", l./(r => (r, Image(r.name, styles.token))).intersperse("or")))

            var loyal = systems./~(s => f.at(s).cities./(_ -> s))
            var rival = systems.%(f.rules)./~(s => f.rivals./~(e => e.at(s).cities./(_ -> s)))
            var slots = systems.%(_ => false)
            var taxed = f.taxed.cities

            if (f.can(Inspiring)) {
                rival = systems.%(f.at(_).ships.any)./~(s => f.rivals./~(e => e.at(s).cities./(_ -> s)))

                slots = systems.%(f.at(_).ships.any)./~(s => game.freeSlots(s).times(s)).diff(f.taxed.slots)
            }

            if (f.can(Principled))
                loyal = $

            if (f.can(Callow))
                loyal = loyal.%>(f.rules)

            if (f.can(WarlordsCruelty) && game.declared.contains(Warlord))
                taxed = $

            if (effect.has(LivingStructures))
                rival = $

            Ask(f).group("Tax".hl, x)
                .each(loyal) { case (c, s) => TaxAction(f, x, effect, s, |(c), true,  then).as(c, "in", s, res(s)).!(taxed.has(c), "taxed") }
                .each(rival) { case (c, s) => TaxAction(f, x, effect, s, |(c), false, then).as(c, "in", s, res(s)).!(taxed.has(c), "taxed").!(f.regent && c.faction.regent && Empire.at(s).any, "truce") }
                .each(slots) { s =>           TaxAction(f, x, effect, s, None, true,  then).as(s,          res(s)).!(f.taxed.slots.count(s) >= game.freeSlots(s), "taxed") }
                .cancel

        case TaxAction(f, x, effect, s, c, loyal, then) =>
            f.pay(x)

            f.log("taxed", c, "in", s, x)

            f.taxed.cities ++= c

            if (c.none)
                f.taxed.slots :+= s

            if (loyal.not)
                c./~(_.faction.as[Faction]).but(f).foreach { e =>
                    if (e.pool(Agent)) {
                        e.reserve --> Agent --> f.captives

                        f.log("captured", Agent.of(e))
                    }
                    else {
                        e.log("had no", Agent.sof(e))
                    }
                }

            Ask(f).each(game.resources(s).distinct./(|(_)).some.|($(None)))(r => TaxGainAction(f, r, x == Pip && (f.copy || f.pivot), PostTaxAction(f, s, c, loyal, then)).as("Gain", r./(ResourceRef(_, None)))("Tax", s))

        case TaxGainAction(f, r, along, then) =>
            r.foreach { r =>
                f.gain(r, $)
            }

            if (along)
                $((Attuned, Psionic), (Insatiable, Fuel), (Firebrand, Weapon))./ { (t, r) =>
                    if (f.can(t))
                        f.gain(r, $("from", t))
                }

            AdjustResourcesAction(then)

        case PostTaxAction(f, s, c, loyal, then) =>
            implicit val ask = builder

            if (f.can(Mythic) && game.overridesHard.contains(s).not && board.slots(s) > 0) {
                f.resKeys./ { (r, k) =>
                    + MythicAction(f, s, r, k, PostTaxAction(f, s, c, loyal, then)).as("Change planet type with", PayResource(r, |(k)))(Mythic, "in", s)
                }
            }

            if (f.can(Ruthless)) {
                c.$.cities.foreach { c =>
                    c.faction.as[Faction].foreach { e =>
                        + UseEffectAction(f, Ruthless, DealHitsAction(f, s, f, e, $(c), 0, |(Ruthless), $, TaxAction(f, NoCost, |(Ruthless), s, |(c), loyal, then))).as("Damage to Tax again")(Ruthless, "in", s)
                    }
                }
            }

            ask(f).done(then)

        // MOVE
        case MayMoveAction(f, effect, then) =>
            MoveMainAction(f, NoCost, effect, true, false, then)

        case MoveMainAction(f, x, effect, skip, cancel, then) =>
            var pp = systems.%(f.at(_).hasA(Starport))
            val ss = systems./(s => s -> f.at(s).ships).%>(_.any).toMap

            def empire(s : System) = (campaign && f.regent).??(Empire.at(s).ships)

            if (f.lores.has(CatapultOverdriveLL))
                pp ++= systems.%(f.rules).%(s => f.rivals.exists(_.at(s).starports.any))

            if (f.can(Ancient))
                pp = systems.%(_.gate)

            Ask(f).group("Move", x, effect./("with" -> _), "from")
                .each(ss.keys.% (pp.has).$)(s => MoveFromAction(f, s, ss(s) ++ empire(s), true,  x, effect, CancelAction, then).as(s))
                .each(ss.keys.%!(pp.has).$)(s => MoveFromAction(f, s, ss(s) ++ empire(s), false, x, effect, CancelAction, then).as(s))
                .skip(skip.?(then))
                .cancelIf(cancel)
                .needOk

        case MoveFromAction(f, r, l, cascade, x, effect, alt, then) =>
            Ask(f).group("Move", effect./("with" -> _), "from", r, "to")
                .each(board.connected(r))(d => {
                    val cat = cascade && (d.gate || f.lores.has(CatapultOverdriveLL)) && f.rivals.exists(_.rules(d)).not
                    MoveToAction(f, r, d, l, cat, x, effect, then).as(d, cat.?("and further"))
                })
                .add(alt)

        case MoveToAction(self, r, d, l, cascade, x, effect, then) =>
            val combinations = l.groupBy(_.faction).$./{ (f, l) =>
                val n = self.can(Disorganized).?(min(2, l.num)).|(l.num)
                val (damaged, fresh) = l.partition(f.damaged.has)
                val combinations = 0.to(n)./~(k => max(0, k - fresh.num).to(min(k, damaged.num))./(i => fresh.take(k - i) ++ damaged.take(i)))
                combinations
            }.reduceLeft((aa, bb) => aa./~(a => bb./(b => a ++ b))).%!(_.none)

            // val n = f.can(Disorganized).?(min(2, l.num)).|(l.num)
            // val (damaged, fresh) = l.partition(f.damaged.has)
            // val combinations = 1.to(n)./~(k => max(0, k - fresh.num).to(min(k, damaged.num))./(i => fresh.take(k - i) ++ damaged.take(i)))

            implicit def convert(u : Figure) = game.showFigure(u, u.faction.damaged.has(u).??(1))

            Ask(self).group("Move", effect./("with" -> _), "from", r, "to", d/*, l.num, n, combinations.toString*/)
                .each(combinations)(l => MoveListAction(self, r, d, l, cascade, x, effect, then).as(l./(u => convert(u))))
                .cancel

        case MoveListAction(f, r, d, l, cascade, x, effect, then) =>
            f.pay(x)

            f.log("moved", l.comma, "from", r, "to", d, x, effect./("with" -> _))

            if (d.gate)
                f.rivals.%(_.can(GatePorts))./ { e =>
                    if (e.at(d).starports.fresh.any && e.rules(d)) {
                        if (f.pool(Agent)) {
                            f.reserve --> Agent --> e.captives

                            e.log("captured", Agent.of(f), "with", GatePorts)
                        }
                        else {
                            f.log("had no", Agent.sof(f))
                        }
                    }
                }

            l --> d

            if (cascade)
                Force(MoveFromAction(f, d, l, true, NoCost, effect, then.as("Done"), then))
            else
            if (f.can(SprinterDrives) && l.fresh.any)
                Force(UseEffectAction(f, SprinterDrives, MoveFromAction(f, d, l.fresh, false, NoCost, |(SprinterDrives), ClearEffectAction(f, SprinterDrives, then).as("Done"), ClearEffectAction(f, SprinterDrives, then))))
            else
                Force(then)

        // BATTLE
        case MustBattleAction(f, effect, then) =>
            Ask(f)
                .add(BattleMainAction(f, NoCost, effect, false, false, then).as("Battle").!!!)
                .add(DeadlockAction(f, BattleWith(effect), then))

        case MayBattleAction(f, effect, then) =>
            BattleMainAction(f, NoCost, effect, true, false, then)

        case BattleMainAction(f, x, effect, skip, cancel, then) =>
            Ask(f).group(f, "battles", x, effect./("with" -> _), "in")
                .each(systems.%(f.at(_).hasA(Ship)).%(s => f.rivals.exists(_.present(s))))(s => BattleSystemAction(f, x, effect, s, then).as(s))
                .skip(skip.?(then))
                .cancelIf(cancel)
                .needOk

        case BattleSystemAction(f, x, effect, s, then) =>
            Ask(f).group(f, "battles in", s, effect./("with" -> _))
                .each(f.rivals.%(_.present(s)))(e =>
                    BattleFactionAction(f, x, effect, s, e, then).as(e)
                        .!(f.regent && e.regent && Empire.at(s).any, "truce")
                )
                .cancel
                .needOk

        case BattleFactionAction(f, x, effect, s, e, then) =>
            f.pay(x)

            f.log("battled", e, "in", s, x, effect./("with" -> _))

            BattleStartAction(f, s, e, $, then)

        case BattleStartAction(f, s, e, used, then) =>
            if (f.at(s).ships.none) {
                f.log("had no ships remaining")

                then
            }
            else
            if (f.lores.has(SignalBreaker) && used.has(SignalBreaker).not && f.at(s).ships.damaged.none)
                BattleStartAction(f, s, e, used :+ SignalBreaker, then)
            else
            if (e.lores.has(PredictiveSensors) && used.has(PredictiveSensors).not) {
                Ask(e).group(PredictiveSensors, "move to", s)
                    .some(board.connected(s)) { t =>
                        e.at(t).ships.fresh./(u => PredictiveSensorsAction(f, t, u, s, BattleStartAction(f, s, e, used, then)).as(u, "from", t))
                    }
                    .done(BattleStartAction(f, s, e, used :+ PredictiveSensors, then))
            }
            else
            if (e.lores.has(RailgunArrays) && used.has(RailgunArrays).not && e.at(s).ships.fresh.any)
                AssignHitsAction(f, s, e, f, f.at(s).ships, 1, 0, 0, |(RailgunArrays), used, BattleStartAction(f, s, e, used :+ RailgunArrays, then))
            else {
                val ships = f.at(s).count(Ship) + s.gate.??(f.can(Gatekeepers).??(2)) + f.can(Committed).??(2)
                val canRaid = (e.at(s).buildings.any || systems.exists(e.at(_).buildings.any).not) && (e.lores.has(HiddenHarbors) && e.at(s).starports.fresh.any).not
                val maxRaid = max(canRaid.??(6), f.lores.has(RaiderExosuits).??(e.at(s).buildings.none.??(1)))

                val combinations : $[(Int, Int, Int)] = 1.to(min(18, ships)).reverse./~(n =>
                    max(0, n - 12).to(min(maxRaid, n))./~(raid =>
                        max(0, n - raid - 6).to(min(6, n - raid))./~(assault =>
                            |(n - raid - assault).%(_ <= 6).%(_ >= f.can(Wary).??(assault))./(skirmish =>
                                (skirmish, assault, raid)
                            )
                        )
                    )
                )

                Ask(f).group(f, "battles", e, "in", s)
                    .each(combinations) { case (skirmish, assault, raid) =>
                        BattleDiceAction(f, s, e, skirmish, assault, raid, used, then)
                            .as(skirmish.times(Image("skirmish-die", styles.token)), assault.times(Image("assault-die", styles.token)), raid.times(Image("raid-die", styles.token)))
                    }
            }

        case BattleDiceAction(f, s, e, n1, n2, n3, used, then) =>
            BattleReRollAction(f, s, e, $, $, $, n1, n2, n3, used, then)

        case BattleReRollAction(f, r, e, o1, o2, o3, n1, n2, n3, used, then) =>
            Roll3[$[BattleResult], $[BattleResult], $[BattleResult]](n1.times(Skirmish.die), n2.times(Assault.die), n3.times(Raid.die), (l1, l2, l3) => BattleRolledAction(f, r, e, o1, o2, o3, l1, l2, l3, used, then))

        case BattleRolledAction(f, s, e, o1, o2, o3, l1, l2, l3, used, then) =>
            f.log("rolled",
                l1./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)) ~
                l2./(x => Image("assault-die-" + (Assault.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)) ~
                l3./(x => Image("raid-die-" + (Raid.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token))
            )

            var ask = Ask(f)

            if (l1.any && f.can(Skirmishers) && used.has(Skirmishers).not) {
                val limit = f.resources.count(Weapon) + f.loyal.of[GuildCard].count(_.suit == Weapon)
                val miss = $()
                val hit = $(HitShip)
                val misses = l1.count(miss)
                val hits = l1.count(hit)
                val rerollable = 1.to(min(limit, misses)).reverse./(_.times(miss)) ++ 1.to(min(limit, hits))./(_.times(hit))

                ask = ask
                    .group(Skirmishers)
                    .each(rerollable)(q => SkirmishersAction(f, s, e, l1, l2, l3, q, used, then).as("Reroll", q./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token))))
            }

            if (l2.any && f.can(SeekerTorpedoes) && used.has(SeekerTorpedoes).not) {
                val limit = f.at(s).ships.fresh.num
                val rerollable = 1.to(limit).reverse./~(n => l2.combinations(n).$)

                ask = ask
                    .group(SeekerTorpedoes)
                    .each(rerollable)(q => SeekerTorpedoesAction(f, s, e, l1, l2, l3, q, used, then).as("Reroll", q./(x => Image("assault-die-" + (Assault.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token))))
            }

            if (l3.any && f.can(Tricky) && used.has(Tricky).not) {
                val limit = f.resources.but(Nothingness).distinct.num
                val rerollable = 1.to(limit).reverse./~(n => l3.combinations(n).$)

                ask = ask
                    .group(Tricky)
                    .each(rerollable)(q => TrickyAction(f, s, e, l1, l2, l3, q, used, then).as("Reroll", q./(x => Image("raid-die-" + (Raid.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token))))
            }

            ask.skip(BattleProcessAction(f, s, e, o1 ++ l1, o2 ++ l2, o3 ++ l3, used, then))

        case SkirmishersAction(f, s, e, l1, l2, l3, q, used, then) =>
            f.log("rerolled", q./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)), "with", Skirmishers)

            BattleReRollAction(f, s, e, l1.diff(q), l2, l3, q.num, 0, 0, used :+ Skirmishers, then)

        case SeekerTorpedoesAction(f, s, e, l1, l2, l3, q, used, then) =>
            f.log("rerolled", q./(x => Image("assault-die-" + (Assault.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)), "with", SeekerTorpedoes)

            BattleReRollAction(f, s, e, l1, l2.diff(q), l3, 0, q.num, 0, used :+ SeekerTorpedoes, then)

        case TrickyAction(f, s, e, l1, l2, l3, q, used, then) =>
            f.log("rerolled", q./(x => Image("raid-die-" + (Raid.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)), "with", Tricky)

            BattleReRollAction(f, s, e, l1, l2, l3.diff(q), 0, 0, q.num, used :+ Tricky, then)

        case BattleProcessAction(f, s, e, l1, l2, l3, used, then) =>
            val ll = (l1 ++ l2 ++ l3).flatten

            val sb = used.has(SignalBreaker).$(Intercept)

            if (ll.intersect(sb).any)
                f.log("used", SignalBreaker, "to cancel", (ll.count(Intercept) > 1).?("just"), "one", "Intercept".hl)

            val mp = (e.lores.has(MirrorPlating) && l2.any).$(Intercept)

            if (mp.any)
                e.log("used", MirrorPlating, "to add one", "Intercept".hl)

            val l = ll.diff(sb) ++ mp

            val sd = l.count(OwnDamage)
            var ic = l.has(Intercept).??(e.at(s).ships.fresh.num)
            val hs = l.count(HitShip)
            val bb = l.count(HitBuilding)
            val rd = l.count(RaidKey)

            if (l.has(Intercept) && e.can(Irregular)) {
                ic = e.resources.count(Weapon) + e.loyal.of[GuildCard].count(_.suit == Weapon)

                if (e.resources.has(Weapon)) {
                    e.remove(ResourceRef(Weapon, None))
                    e.log("discarded", Weapon, "due to", Irregular)
                }
            }

            if (sd > 0)
                f.log("suffered", sd.hit)

            if (ic > 0)
                f.log("got intercepted for", ic.hit)

            if (hs > 0)
                f.log("dealt", hs.hit)

            if (bb > 0)
                f.log("bombarded for", bb.hit)

            if (rd > 0)
                f.log("raided with", rd.hl, "keys")

            AssignHitsAction(f, s, e, f, f.at(s).ships, sd + ic, 0, 0, None, used, AssignHitsAction(f, s, f, e, e.at(s), hs, bb, rd, None, used, PostBattleAction(f, s, e, used, then)))

        case BattleRaidAction(f, r, e, raid, then) =>
            def spend(k : Int) = (k >= raid).?(then).|(BattleRaidAction(f, r, e, raid - k, then))

            if (f.at(r).ships.any && raid > 0)
                Ask(f).group("Raid", e, "with", raid.times(Image("raid-key", styles.token)).merge)
                    .each(e.resKeys) { case (x, k) =>
                        BattleRaidResourceAction(f, e, x, k, spend(k)).as(ResourceRef(x, |(k)))
                            .!(k > raid)
                            .!(e.loyal.exists(_.as[GuildCard].?(_.effect == SwornGuardians)))
                    }
                    .each(e.loyal.of[GuildCard]) { c =>
                        BattleRaidCourtCardAction(f, e, c, spend(c.keys)).as(c, |(c.keys)./(n => Image("keys-" + n, styles.token)))
                            .!(c.keys > raid)
                            .!(e.loyal.but(c).exists(_.as[GuildCard].?(_.effect == SwornGuardians)))
                    }
                    .needOk
                    .done(then)
            else
                NoAsk(f)(then)

        case BattleRaidResourceAction(f, e, x, k, then) =>
            e.remove(ResourceRef(x, |(k)))

            f.gain(x)

            f.log("stole", ResourceRef(x, |(k)))

            AdjustResourcesAction(then)

        case BattleRaidCourtCardAction(f, e, c, then) =>
            e.loyal --> c --> f.loyal

            f.log("stole", c)

            GainCourtCardAction(f, c, |(e), then)

        case AssignHitsAction(self, r, f, e, l, hits, bombardments, raid, effect, used, then) =>
            implicit def convert(u : Figure, k : Int) = game.showFigure(u, u.damaged.??(1) + k)

            var h = hits
            var b = bombardments

            val ships = l.ships./(u => u.fresh.?(2).|(1)).sum
            val buildings = l.buildings./(u => u.fresh.?(2).|(1)).sum

            if (h > ships) {
                b = b + (h - ships)
                h = ships
            }

            if (b > buildings)
                b = buildings

            if (h + b > 0)
                XXSelectObjectsAction(self, l.sortBy(_.piece.is[Building].??(1)))
                    .withGroup(f, "dealt", hits.hit, (bombardments > 0).?(("and", bombardments.hlb), "Bombardment".s(bombardments).styled(styles.hit)), "to", e, "in", r,
                        $(convert(Figure(e, City, 0), 1), convert(Figure(e, Starport, 0), 1), convert(Figure(e, Ship, 0), 1), convert(Figure(e, City, 0), 2), convert(Figure(e, Starport, 0), 2), convert(Figure(e, Ship, 0), 2)).merge.div(xstyles.displayNone))
                    .withRule(_.num(h + b).all(d => d.ships.num <= h && d.buildings.num <= b))
                    .withMultipleSelects(_.fresh.?(2).|(1))
                    .withThen(d => DealHitsAction(self, r, f, e, d, raid, effect, used, then))(_ => "Damage".hl)
                    .ask
            else
            if (raid > 0 && f.at(r).ships.any)
                NoAsk(self)(BattleRaidAction(f, r, e, raid, then))
            else
                NoAsk(self)(then)

        case DealHitsAction(self, s, f, e, l, k, effect, used, then) =>
            val dd = e.damaged ++ l
            val destroyed = dd.diff(dd.distinct)

            if (destroyed.any)
                f.log("destroyed", destroyed.comma, effect./("with" -> _))

            e.damaged = e.damaged.diff(destroyed)

            val damaged = l.distinct.diff(destroyed)

            if (damaged.any)
                f.log("damaged", damaged.comma, effect./("with" -> _))

            e.damaged ++= damaged

            destroyed.foreach { u =>
                if (u.faction != f)
                    u --> f.trophies
                else
                    u --> u.faction.reserve
            }

            var next = then

            if (destroyed.any && e.can(Beloved) && e != self) {
                e.log("triggered", Beloved)

                next = BelovedAction(e, next)
            }

            if (k > 0 && f.at(s).ships.any)
                next = BattleRaidAction(f, s, e, k, next)

            destroyed.cities.foreach { u =>
                if (game.unslotted.has(u))
                    game.unslotted :-= u

                if (u.faction != f)
                    if (e.can(Beloved).not)
                        next = RansackMainAction(f, e, next)

                game.resources(s).foreach { r =>
                    next = OutrageAction(f, r, next)
                }
            }

            next

        case PostBattleAction(f, s, e, used, then) =>
            if (f.can(RepairDrones) && used.has(RepairDrones).not && f.at(s).ships.damaged.any)
                PromptRepairsAction(f, s, 1, |(RepairDrones), PostBattleAction(f, s, e, used :+ RepairDrones, then))
            else
            if (f.can(Resilient) && used.has(AttackerResilient).not && f.at(s).ships.damaged.any)
                PromptRepairsAction(f, s, systems.%(f.rules)./(_.$.starports.num).sum, |(Resilient), PostBattleAction(f, s, e, used :+ AttackerResilient, then))
            else
            if (e.can(Resilient) && used.has(DefenderResilient).not && e.at(s).ships.damaged.any)
                PromptRepairsAction(e, s, systems.%(e.rules)./(_.$.starports.num).sum, |(Resilient), PostBattleAction(f, s, e, used :+ DefenderResilient, then))
            else
                then

        case PromptRepairsAction(f, s, 0, effect, then) =>
            NoAsk(f)(then)

        case PromptRepairsAction(f, s, n, effect, then) =>
            val ll = f.at(s).ships

            implicit def convert(u : Figure, selected : Boolean) = game.showFigure(u, u.faction.damaged.has(u).??(1) - selected.??(1))

            if (ll.damaged.any)
                XXSelectObjectsAction(f, ll)
                    .withGroup(f, "repairs", n.hl, "ships in", s, $(convert(Figure(f, Ship, 0), false)).merge.div(xstyles.displayNone))
                    .withRule(_.num(min(ll.damaged.num, n)).each(_.damaged))
                    .withThen(l => RepairsAction(f, s, l, effect, then))(_ => "Repair")
                    .withExtras(then.as("Skip"))
                    .ask
            else
                NoAsk(f)(then)

       case RepairsAction(f, s, l, effect, then) =>
            l.foreach { u =>
                f.damaged :-= u
            }

            f.log("repaired", l.comma, "in", s, effect./("with" -> _))

            then

        case OutrageAction(f, r, then) =>
            if (f.outraged.has(r).not) {
                f.outraged :+= r

                f.log("suffered", r, "outrage")
            }

            val discarded = f.resources.%(_ == r)

            if (discarded.any)
                f.log("discarded", discarded./(_.elem).comma)

            f.resources = f.resources./{
                case x if x == r => Nothingness
                case x => x
            }

            if (f.lores.has(GuildLoyaltyLL).not)
                f.loyal.of[GuildCard].%!(_.effect.is[LoyalGuild]).foreach { c =>
                    if (c.suit == r) {
                        f.loyal --> c --> game.discourt

                        f.log("discarded", c)
                    }
                }

            then

        case ClearOutrageAction(f, r, then) =>
            r.foreach { c =>
                f.outraged :-= c

                f.log("cleared", c, "outrage")
            }

            then

        case RansackMainAction(f, e, then) =>
            Ask(f).group("Ransack".hl)
                .each(market.%(c => Influence(c).exists(_.faction == e)))(c => RansackAction(f, c, then).as(c))
                .needOk
                .bailout(then.as("No cards in court with", Agent.sof(e)))

        case RansackAction(f, c, then) =>
            market --> c --> f.loyal

            f.log("ransacked", c)

            Influence(c).foreach { u =>
                if (u.faction == f)
                    u --> f.reserve
                else {
                    u --> f.trophies

                    f.log("executed", u)
                }
            }

            GainCourtCardAction(f, c, None, ReplenishMarketAction(then))

        // BUILD
        case BuildMainAction(f, x, then) =>
            val present = systems.%(f.present)

            val slots = present.%(s => game.freeSlots(s) > 0)

            var cities = slots
            var starports = slots
            var clouds = f.can(CloudCities).??(present.%(s => board.slots(s) > 0 && f.at(s).cities.intersect(game.unslotted).none))

            if (f.can(GateStations))
                cities ++= present.%(_.gate).%(f.at(_).cities.none)

            if (f.can(GatePorts))
                starports ++= present.%(_.gate).%(f.at(_).starports.none)

            var yards = present./~(s => f.at(s).starports./(_ -> s))

            if (f.can(ToolPriests))
                if (f.worked.cities.none)
                    yards ++= present.%(f.rules)./~(s => s.$.cities./(_ -> s))

            if (f.pool(City).not)
                cities = $

            if (f.pool(City).not)
                clouds = $

            if (f.pool(Starport).not)
                starports = $

            if (f.pool(Ship).not)
                yards = $

            val prefix = f.short + "-"
            def suffix(s : System) = f.rivals.exists(_.rules(s)).??("-damaged")

            val resources = f.resKeys./((r, k) => PayResource(r, |(k))).diff(x.as[PayResource].$)

            Ask(f)
                .group("Build".hl, x)
                .each(cities)(s => BuildCityAction(f, x, s, None, then).as(City.of(f), Image(prefix + "city" + suffix(s), styles.qbuilding), "in", s))
                .some(clouds)(s => resources.%(pr => game.resources(s).has(pr.resource))./(pr => BuildCityAction(f, MultiCost(x, pr), s, |(CloudCities), then).as("Cloud".styled(f), City.of(f), Image(prefix + "city" + suffix(s), styles.qbuilding), "in", s, pr)))
                .each(starports)(s => BuildStarportAction(f, x, s, then).as(Starport.of(f), Image(prefix + "starport" + suffix(s), styles.qbuilding), "in", s))
                .each(yards) { case (b, s) =>
                    BuildShipAction(f, x, s, b, None, then).as(Ship.of(f), Image(prefix + "ship" + f.can(HiddenHarbors).not.??(suffix(s)), styles.qship), "in", s, "with", b)
                        .!(f.worked.has(b), "built this turn")
                }
                .cancel

        case BuildCityAction(f, x, s, effect, then) =>
            f.pay(x)

            val u = f.reserve --> City.of(f)

            if (f.rivals.exists(_.rules(s)))
                f.damaged :+= u

            u --> s

            if (effect.has(CloudCities))
                game.unslotted :+= u

            if (f.taxed.slots.has(s))
                f.taxed.slots :-= s

            f.log("built", u, "in", s, effect./("with" -> _), x)

            then

        case BuildStarportAction(f, x, s, then) =>
            f.pay(x)

            val u = f.reserve --> Starport.of(f)

            if (f.rivals.exists(_.rules(s)))
                f.damaged :+= u

            u --> s

            if (f.taxed.slots.has(s))
                f.taxed.slots :-= s

            f.log("built", u, "in", s, x)

            then

        case BuildShipAction(f, x, s, b, effect, then) =>
            f.pay(x)

            val u = f.reserve --> Ship.of(f)

            if (f.rivals.exists(_.rules(s)) && f.lores.has(HiddenHarbors).not)
                f.damaged :+= u

            u --> s

            f.worked :+= b

            f.log("built", u, "in", s, effect./("with" -> _), x)

            implicit val ask = builder

            if (f.can(Ruthless) && f.pool(Ship)) {
                $(b).starports.foreach { b =>
                    b.faction.as[Faction].foreach { e =>
                        + UseEffectAction(f, Ruthless, DealHitsAction(f, s, f, e, $(b), 0, |(Ruthless), $, BuildShipAction(f, NoCost, s, b, |(Ruthless), then))).as("Damage", b, "to Build again")(Ruthless, "in", s)
                    }
                }
            }

            ask(f).done(then)

        // REPAIR
        case RepairMainAction(f, x, then) =>
            val ll = systems.%(f.present)

            Ask(f)
                .group("Repair".hl, x)
                .some(ll)(r => f.at(r).damaged./(u => RepairAction(f, x, r, u, then).as(u.piece.of(f), Image(u.faction.short + "-" + u.piece.name + "-damaged", u.piece.is[Building].?(styles.qbuilding).|(styles.qship)), "in", r)))
                .cancel

        case RepairAction(f, x, s, u, then) =>
            f.pay(x)

            f.damaged :-= u

            f.log("repaired", u, "in", s, x)

            then

        // INFLUENCE
        case InfluenceMainAction(f, x, effect, skip, cancel, then) =>
            Ask(f).group("Influence".hl, effect./("with" -> _), x)
                .each(market) { c =>
                    InfluenceAction(f, x, c, effect, then).as(c)
                        .!(c == ImperialCouncilDecided)
                }
                .skip(skip.?(then))
                .cancelIf(cancel)
                .needOk

        case InfluenceAction(f, x, c, effect, then) =>
            f.pay(x)

            f.reserve --> Agent --> Influence(c)

            f.log("influenced", c, effect./("with" -> _), x)

            then

        case MayInfluenceAction(f, effect, then) =>
            if (f.pool(Agent).not) {
                f.log("had no", "Agents".styled(f), "for", effect)

                then
            }
            else
                InfluenceMainAction(f, NoCost, effect, true, false, then)

        // SECURE
        case SecureMainAction(f, x, effect, skip, cancel, then) =>
            Ask(f).group("Secure".hl, effect./("with" -> _), x)
                .each(market)(c => SecureAction(f, x, effect, c, then).as(c)
                    .!(Influence(c).$.use(l => l.%(_.faction == f).num <= f.rivals./(e => l.%(_.faction == e).num).max))
                    .!(f.can(Paranoid) && c.is[GuildCard] && Influence(c).%(_.faction == f).num <= 1, "Paranoid")
                )
                .skip(skip.?(then))
                .cancelIf(cancel)
                .needOk

        case SecureAction(f, x, effect, c, then) =>
            f.pay(x)

            market --> c --> f.loyal

            f.secured ++= c.as[GuildCard]

            f.log("secured", c, x, effect./("with" -> _))

            Influence(c).foreach { u =>
                if (u.faction == f)
                    u --> f.reserve
                else {
                    u --> f.captives

                    f.log("captured", u)
                }
            }

            GainCourtCardAction(f, c, None, ReplenishMarketAction(then))

        case MustSecureAction(f, effect, then) =>
            Ask(f)
                .add(SecureMainAction(f, NoCost, effect, false, false, then).as("Secure").!!!)
                .add(DeadlockAction(f, SecureWith(effect), then))

        // MANUFACTURE
        case ManufactureMainAction(f, x, then) =>
            f.pay(x)

            f.gain("manufactured", Material, $(x))

            AdjustResourcesAction(then)

        // SYNTHESIZE
        case SynthesizeMainAction(f, x, then) =>
            f.pay(x)

            f.gain("synthesized", Fuel, $(x))

            AdjustResourcesAction(then)

        // TRADE
        case TradeMainAction(f, x, then) =>
            Ask(f).group("Trade".hl)
                .some(f.rivals.%(_.resources.but(Nothingness).any)) { e =>
                    e.resources.lazyZip(e.keys).toList.%<(_ != Nothingness).%<(r => systems.exists(s => e.at(s).cities.any && game.resources(s).has(r) && f.rules(s)))./ { case (r, k) =>
                        TakeResourceAction(f, e, r, k, x, GiveBackResourceMainAction(f, e, then)).as(e, ResourceRef(r, None))
                    }
                }
                .cancel

        case TakeResourceAction(f, e, r, k, x, then) =>
            f.pay(x)

            e.remove(ResourceRef(r, |(k)))

            f.gain("took in trade", ResourceRef(r, |(k)), $(x))

            then

        case GiveBackResourceMainAction(f, e, then) =>
            Ask(f).group("Give to", e)
                .each(f.resources.but(Nothingness).distinct)(r => GiveBackResourceAction(f, e, r, then).as("Give back", ResourceRef(r, None)))
                .needOk

        case GiveBackResourceAction(f, e, r, then) =>
            f.remove(ResourceRef(r, None))

            e.gain("got back", r, $)

            AdjustResourcesAction(then)

        // WEAPON
        case AddBattleOptionAction(f, x, then) =>
            f.pay(x)

            f.anyBattle = true

            f.log("could use any card action as ", "Battle".styled(f), x)

            then

        // DISCARD
        case DiscardResourcesMainAction(f, then) =>
            Ask(f)
                .each(f.resKeys) { case (r, k) =>
                    DiscardResourceNoEffectAction(f, PayResource(r, |(k)), then)
                        .as("Discard", ResourceRef(r, |(k)))("Discard Resources with No Effect".spn(xstyles.error))
                }
                .cancel

        case DiscardResourceNoEffectAction(f, x, then) =>
            f.pay(x)

            f.log("discarded", x.ref.elem, "with no effect")

            then

        case DiscardGuildCardsMainAction(f, then) =>
            implicit val ask = builder

            def discardable(e : GuildEffect) : GuildCard = f.loyal./~(c => c.as[GuildCard].%(c => c.effect == e && f.secured.has(c).not)).first

            $(MiningInterest, ShippingInterest, Gatekeepers, PrisonWardens, Skirmishers, CourtEnforcers, LoyalMarines, ElderBroker).foreach { e =>
                if (f.canPrelude(e))
                    + DiscardGuildCardNoEffectAction(f, discardable(e), then).as("Discard", e)("Discard Guild Cards with No Effect".spn(xstyles.error))
            }

            ask(f).cancel

        case DiscardGuildCardNoEffectAction(f, c, then) =>
            f.loyal --> c --> game.discourt

            f.log("discarded", c, "with no effect")

            then

        // TURN
        case StartChapterAction =>
            log(DoubleLine)
            log(SingleLine)
            log(DoubleLine)

            game.ambitionable = game.markers.drop(chapter).take(3).sortBy(_.high)
            game.declared = Map()

            game.chapter += 1
            game.round = 0

            log(("Chapter".hl ~ " " ~ chapter.hlb).styled(styles.title))

            Shuffle[DeckCard](deck, ShuffleDeckCardsAction(_, DealCardsAction))

        case ShuffleDeckCardsAction(l, then) =>
            l --> deck

            log("The action deck was shuffled")

            then

        case DealCardsAction =>
            factions.foreach { f =>
                deck.$.take(6) --> f.hand

                f.log("drew", 6.cards)
            }

            Milestone(StartRoundAction)

        case StartRoundAction =>
            log(DoubleLine)

            game.round += 1

            LeadMainAction(factions.first)

        case LeadMainAction(f) =>
            if (f.hand.notOf[EventCard].none)
                NoAsk(f)(PassAction(f))
            else
                YYSelectObjectsAction(f, f.hand)
                    .withGroup(f.elem ~ " leads")
                    .withRule(_.is[EventCard].not)
                    .withThen(LeadAction(f, _))("Play " ~ _.elem)("Play")
                    .withExtras(NoHand, PassAction(f).as("Pass".hh))

        case PassAction(f) =>
            if (f.hand.any)
                game.passed += 1

            if (game.passed >= factions.%(_.hand.any).num) {
                f.log("passed")

                Milestone(EndChapterAction)
            }
            else {
                game.factions = factions.drop(1).dropWhile(_.hand.none) ++ factions.take(1) ++ factions.drop(1).takeWhile(_.hand.none)

                game.current = |(factions.first)

                f.log("passed initative to", factions.first)

                game.round -= 1

                Milestone(StartRoundAction)
            }

        case LeadAction(f, d) =>
            game.passed = 0

            game.lead = d.as[ActionCard]

            f.log("led with", d)

            f.hand --> d --> f.played

            game.seen :+= (round, f, |(d))

            f.lead = true

            CheckAmbitionAction(f, d)

        case CheckAmbitionAction(f, d) =>
            val s = game.ambitionable.any.??(d.strength)

            val next = PreludeActionAction(f, d.suit, d.pips)

            Ask(f).group("Declare Ambition".hl)
                .add(s.in(2, 7).?(DeclareAmbitionAction(f, Tycoon,  true, next))./(a => a.as(a.ambition)))
                .add(s.in(3, 7).?(DeclareAmbitionAction(f, Tyrant,  true, next))./(a => a.as(a.ambition)))
                .add(s.in(4, 7).?(DeclareAmbitionAction(f, Warlord, true, next))./(a => a.as(a.ambition)))
                .add(s.in(5, 7).?(DeclareAmbitionAction(f, Keeper,  true, next))./(a => a.as(a.ambition)))
                .add(s.in(6, 7).?(DeclareAmbitionAction(f, Empath,  true, next))./(a => a.as(a.ambition)))
                .add(next.as("Skip"))

        case DeclareAmbitionAction(f, a, zero, then) if f.can(Generous) =>
            val l = factions.but(f).%(_.power == factions.but(f)./(_.power).min)

            implicit def convert(c : GuildCard) = c.img

            YYSelectObjectsAction(f, f.loyal.of[GuildCard])
                .withGroup("Give a card with", Generous)
                .withThensInfo(c => l./(e => UseEffectAction(f, Generous, GiveGuildCardAction(f, e, c, DeclareAmbitionAction(f, a, zero, ClearEffectAction(f, Generous, then)))).as("Give", c, "to", e)))(l./(e => Info("Give to", e)))
                .withExtras(then.as("Forfeit declaring", a))

        case DeclareAmbitionAction(f, a, zero, then) =>
            f.log("declared", a, "ambition")

            game.declared += a -> (game.declared.get(a).|($) ++ $(game.ambitionable.last))

            game.ambitionable = game.ambitionable.dropRight(1)

            if (zero && (f.can(SecretOrder) && a.in(Keeper, Empath)).not)
                f.zeroed = true

            f.declared = true

            AmbitionDeclaredAction(f, a, $, then)

        case AmbitionDeclaredAction(f, a, used, then) =>
            var ask = Ask(f)

            if (f.can(Bold) && used.has(Bold).not)
                ask = ask.add(BoldMainAction(f, $, AmbitionDeclaredAction(f, a, used :+ Bold, then)).as("Influence each card in court".hh)(Bold).!(f.pool(Agent).not, "no agents"))

            if (f.can(Ambitious) && used.has(Ambitious).not)
                Resources.all.foreach { r =>
                    ask = ask.add(GainResourcesAction(f, $(r), AmbitionDeclaredAction(f, a, used :+ Ambitious, then)).as("Gain".hh, r)(Ambitious).!(game.available(r).not, "not in supply"))
                }

            if (f.can(Connected) && used.has(Connected).not && game.declared.get(a).|($).num == 1)
                ask = ask.add(ConnectedAction(f, AmbitionDeclaredAction(f, a, used :+ Connected, then)).as("Draw and secure a court card".hh)(Connected).!(game.court.none, "deck empty")).needOk

            if (f.can(Farseers) && used.has(Farseers).not)
                ask = ask.each(f.rivals)(e => FarseersMainAction(f, e, AmbitionDeclaredAction(f, a, used :+ Farseers, then)).as(e)(Farseers))

            ask.add(then.as("Done"))

        case FollowAction(f) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup(f.elem ~ " follows " ~ lead.get.zeroed(factions.first.zeroed))
                .withThens {
                    case d : ActionCard => $(
                        SurpassAction(f, d).as("Surpass".styled(lead.get.suit).styled(xstyles.bold), "with", d).!(d.suit != lead.get.suit, "wrong suit").!(d.strength < lead.get.strength && factions.first.zeroed.not, "low strength"),
                        CopyAction(f, d).as("Copy".styled(lead.get.suit).styled(xstyles.bold), "with", d),
                        PivotAction(f, d).as("Pivot".styled(d.suit).styled(xstyles.bold), "with", d).!(d.suit == lead.get.suit, "same suit"),
                    )
                    case d : EventCard => $(
                        MirrorAction(f, d).as("Mirror".styled(lead.get.suit).styled(xstyles.bold), "with", d),
                    )
                }
                .withExtras(NoHand)

        case SurpassAction(f, d) =>
            f.log("surpassed with", d)

            f.hand --> d --> f.played

            game.seen :+= (round, f, |(d))

            f.surpass = true

            if (game.seized.none && d.strength == 7) {
                f.log("seized the initative")

                game.seized = |(f)
            }

            CheckSeizeAction(f, PreludeActionAction(f, d.suit, d.pips))

        case CopyAction(f, d) =>
            f.log("copied with a card")

            f.hand --> d --> f.blind

            f.seen :+= d

            game.seen :+= (round, f, None)

            f.copy = true

            CheckSeizeAction(f, PreludeActionAction(f, lead.get.suit, 1))

        case PivotAction(f, d) =>
            f.log("pivoted with", d)

            f.hand --> d --> f.played

            game.seen :+= (round, f, |(d))

            f.pivot = true

            CheckSeizeAction(f, PreludeActionAction(f, d.suit, 1))

        case MirrorAction(f, d) =>
            f.log("mirrored with", d)

            f.hand --> d --> f.played

            game.seen :+= (round, f, |(d))

            f.mirror = true

            CheckSeizeAction(f, PreludeActionAction(f, lead.get.suit, factions.but(f).%(_.mirror).none.??(lead.get.pips)))

        case CheckSeizeAction(f, then) =>
            if (factions.exists(_.declared).not && game.ambitionable.any && f.played.any && f.can(GalacticBards)) {
                val next = UseEffectAction(f, GalacticBards, CheckSeizeAction(f, then))
                val s = f.played.single.get.strength

                Ask(f).group("Declare Ambition".hl)
                    .add(s.in(2, 7).?(DeclareAmbitionAction(f, Tycoon,  false, next))./(a => a.as(a.ambition)))
                    .add(s.in(3, 7).?(DeclareAmbitionAction(f, Tyrant,  false, next))./(a => a.as(a.ambition)))
                    .add(s.in(4, 7).?(DeclareAmbitionAction(f, Warlord, false, next))./(a => a.as(a.ambition)))
                    .add(s.in(5, 7).?(DeclareAmbitionAction(f, Keeper,  false, next))./(a => a.as(a.ambition)))
                    .add(s.in(6, 7).?(DeclareAmbitionAction(f, Empath,  false, next))./(a => a.as(a.ambition)))
                    .add(next.as("Skip"))
            }
            else
            if (seized.none && (f.hand.notOf[EventCard].any || f.can(LatticeSpies))) {
                YYSelectObjectsAction(f, f.hand)
                    .withGroup(f.elem ~ " can seize initiative")
                    .withRule(_.suit != Event)
                    .withThen(SeizeAction(f, _, then))("Seize with " ~ _.elem)("Seize")
                    .withExtras(NoHand, f.can(LatticeSpies).?(LatticeSeizeAction(f, f.loyal.of[GuildCard].%(_.effect == LatticeSpies).first, then).as("Seize with", LatticeSpies)).|(NoHand), then.as("Skip".hh))
            }
            else
                NoAsk(f)(then)

        case SeizeAction(f, d, then) =>
            f.log("seized initiative with a card")

            f.hand --> d --> f.blind

            f.seen :+= d

            game.seen :+= (round, f, None)

            game.seized = |(f)

            then

        case LatticeSeizeAction(f, c, then) =>
            f.log("seized initiative with", c)

            f.loyal --> c --> game.discourt

            game.seized = |(f)

            then

        case PreludeActionAction(f, s, p) =>
            val then = PreludeActionAction(f, s, p)

            implicit val ask = builder
            implicit val group = f.elem ~ " " ~ "Prelude".hh

            f.resKeys.distinct./ { (r, k) =>
                val cost = PayResource(r, |(k))

                // TAX
                if ((r == Psionic && f.outraged.has(Psionic).not) || f.can(LoyalEmpaths)) {
                    factions.first.played./(_.suit)./{
                        case Administration =>
                            game.tax(f, cost, then)
                            game.taxAlt(f, cost, then)
                        case Aggression =>
                        case Construction =>
                        case Mobilization =>
                    }
                }

                // BUILD
                if ((r == Material && f.outraged.has(Material).not) || f.can(LoyalEngineers)) {
                    game.build(f, cost, then)
                    game.buildAlt(f, cost, then)
                }
                else
                if ((r == Psionic && f.outraged.has(Psionic).not) || f.can(LoyalEmpaths)) {
                    factions.first.played./(_.suit)./{
                        case Administration =>
                        case Aggression =>
                        case Construction =>
                            game.build(f, cost, then)
                            game.buildAlt(f, cost, then)
                        case Mobilization =>
                    }
                }

                // REPAIR
                if ((r == Material && f.outraged.has(Material).not) || f.can(LoyalEngineers)) {
                    game.repair(f, cost, then)
                    game.repairAlt(f, cost, then)
                }
                else
                if ((r == Psionic && f.outraged.has(Psionic).not) || f.can(LoyalEmpaths)) {
                    factions.first.played./(_.suit)./{
                        case Administration =>
                            game.repair(f, cost, then)
                            game.repairAlt(f, cost, then)
                        case Aggression =>
                        case Construction =>
                            game.repair(f, cost, then)
                            game.repairAlt(f, cost, then)
                        case Mobilization =>
                    }
                }

                // MOVE
                if ((r == Fuel && f.outraged.has(Fuel).not) || f.can(LoyalPilots)) {
                    game.move(f, cost, then)
                    game.moveAlt(f, cost, then)
                }
                else
                if ((r == Psionic && f.outraged.has(Psionic).not) || f.can(LoyalEmpaths)) {
                    factions.first.played./(_.suit)./{
                        case Administration =>
                        case Aggression =>
                            game.move(f, cost, then)
                            game.moveAlt(f, cost, then)
                        case Construction =>
                        case Mobilization =>
                            game.move(f, cost, then)
                            game.moveAlt(f, cost, then)
                    }
                }

                // BATTLE
                if ((r == Weapon && s != Aggression && f.outraged.has(Weapon).not) || f.can(LoyalMarines))
                    + AddBattleOptionAction(f, cost, then).as("Add", "Battle".styled(f), "option", cost)(group).!(f.anyBattle)

                if ((r == Psionic && f.outraged.has(Psionic).not) || f.can(LoyalEmpaths)) {
                    factions.first.played./(_.suit)./{
                        case Administration =>
                        case Aggression =>
                            game.battle(f, cost, then)
                            game.battleAlt(f, cost, then)
                        case Construction =>
                        case Mobilization =>
                    }
                }

                // INFLUENCE
                if ((r == Psionic && f.outraged.has(Psionic).not) || f.can(LoyalEmpaths)) {
                    factions.first.played./(_.suit)./{
                        case Administration =>
                            game.influence(f, cost, then)
                            game.influenceAlt(f, cost, then)
                        case Aggression =>
                        case Construction =>
                        case Mobilization =>
                            game.influence(f, cost, then)
                            game.influenceAlt(f, cost, then)
                    }
                }

                // SECURE
                if ((r == Relic && f.outraged.has(Relic).not) || f.can(LoyalKeepers)) {
                    game.secure(f, cost, then)
                    game.secureAlt(f, cost, then)
                }
                else
                if ((r == Psionic && f.outraged.has(Psionic).not) || f.can(LoyalEmpaths)) {
                    factions.first.played./(_.suit)./{
                        case Administration =>
                        case Aggression =>
                            game.secure(f, cost, then)
                            game.secureAlt(f, cost, then)
                        case Construction =>
                        case Mobilization =>
                    }
                }

            }

            def discardable(e : GuildEffect) : GuildCard = f.loyal./~(c => c.as[GuildCard].%(c => c.effect == e && f.secured.has(c).not)).first
            def discardEffect(e : GuildEffect) : ForcedAction = DiscardCourtCardAction(f, discardable(e), then)

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

            if (f.canPrelude(MaterialCartel))
                + StealResourceMainAction(f, |(Material), $(CancelAction), discardEffect(MaterialCartel)).as("Steal", ResourceRef(Material, None))(MaterialCartel)

            if (f.canPrelude(FuelCartel))
                + StealResourceMainAction(f, |(Fuel), $(CancelAction), discardEffect(FuelCartel)).as("Steal", ResourceRef(Fuel, None))(FuelCartel)

            if (f.canPrelude(Gatekeepers))
                + ShipAtEachGateMainAction(f, discardEffect(Gatekeepers)).as("Place", Ship.of(f), "at each gate")(Gatekeepers).!(f.pool(Ship).not, "no ships")

            systems.%(r => f.rules(r)).some.foreach { l =>
                $(PrisonWardens, Skirmishers, CourtEnforcers, LoyalMarines).foreach { c =>
                    if (f.canPrelude(c))
                        + ShipsInSystemMainAction(f, l, discardEffect(c)).as("Place", 3.hlb, Ship.sof(f), "in a controlled system")(c).!(f.pool(Ship).not, "no ships")
                }
            }

            if (f.canPrelude(Farseers))
                + FarseersRedrawMainAction(f, discardEffect(Farseers)).as("Redraw cards")(Farseers)

            if (f.canPrelude(SilverTongues)) {
                + StealResourceMainAction(f, None, $(CancelAction), discardEffect(SilverTongues)).as("Steal any resource")(SilverTongues)

                + StealGuildCardMainAction(f, $(CancelAction), discardEffect(SilverTongues)).as("Steal a Guild Card")(SilverTongues)
            }

            if (f.canPrelude(RelicFence) && game.available(Relic)) {
                f.resources.lazyZip(f.keys).foreach { (r, k) =>
                    if (r != Nothingness) {
                        val cost = PayResource(r, |(k))

                        + FenceResourceAction(f, Relic, cost, UseEffectAction(f, RelicFence, then)).as("Gain", ResourceRef(Relic, None), cost)(RelicFence)
                    }
                }
            }

            if (f.canPrelude(ElderBroker) && (game.available(Material) || game.available(Fuel) || game.available(Weapon))) {
                + GainResourcesAction(f, $(Material, Fuel, Weapon), discardEffect(ElderBroker)).as("Gain", ResourceRef(Material, None), ResourceRef(Fuel, None), ResourceRef(Weapon, None))(ElderBroker)
            }

            $(
                (WarlordsCruelty, $(Weapon))        ,
                (TyrantsEgo, $(Weapon))             ,
                (KeepersTrust, $(Relic))            ,
                (EmpathsVision, $(Psionic))         ,
                (TycoonsCharm, $(Material, Fuel))   ,
            ).foreach { case (l, r) =>
                if (f.lores.has(l)) {
                    + ClearOutrageAction(f, r, DiscardLoreCardAction(f, l, then)).as("Clear", r, "Outrage")(l).!(f.outraged.intersect(r).none)
                }
            }

            + EndPreludeAction(f, s, 0, p).as("End Prelude".hh)(" ")

            if (f.resKeys.any)
                + DiscardResourcesMainAction(f, then).as("Discard Resources with No Effect")("  ")

            DiscardGuildCardsMainAction(f, then).as("Discard Guild Cards with No Effect")("  ").!!!.addIfAvailable

            ask(f)

        case EndPreludeAction(f, s, i, n) =>
            f.spent = $

            log(DottedLine)

            MainTurnAction(f, s, i, n)

        case MainTurnAction(f, s, i, n) if i >= n =>
            Ask(f)
                .add(EndTurnAction(f).as("End Turn")(f.elem))
                .needOk

        case MainTurnAction(f, s, i, n) =>
            soft()

            val then = MainTurnAction(f, s, i + 1, n)

            implicit val ask = builder
            implicit val group = f.elem ~ " " ~ "Actions" ~ " " ~ (n - i).times(0x2726.toChar.toString.styled(s))

            val cost = Pip
            val one = f.copy || f.pivot

            s @@ {
                case Administration =>
                    game.tax(f, cost, then)
                    game.taxAlt(f, cost, then)

                    game.repair(f, cost, then)
                    game.repairAlt(f, cost, then)

                    if (one && f.can(Influential))
                        game.influence(f, cost, MayInfluenceAction(f, |(Influential), then))
                    else
                        game.influence(f, cost, then)

                    game.influenceAlt(f, cost, then)

                case Aggression =>
                    if (one && f.can(Tactical)) {
                        game.battle(f, cost, MayMoveAction(f, |(Tactical), then))
                        game.battleAlt(f, cost, then)
                        game.move(f, cost, MayBattleAction(f, |(Tactical), then))
                        game.moveAlt(f, cost, then)
                    }
                    else {
                        game.battle(f, cost, then)
                        game.battleAlt(f, cost, then)
                        game.move(f, cost, then)
                        game.moveAlt(f, cost, then)
                    }

                    if (one && f.can(Charismatic)) {
                        game.secure(f, cost, MayInfluenceAction(f, |(Charismatic), then))
                        game.influence(f, cost, MustSecureAction(f, |(Charismatic), then))
                    }
                    else
                        game.secure(f, cost, then)

                    game.secureAlt(f, cost, then)

                case Construction =>
                    game.build(f, cost, then)
                    game.buildAlt(f, cost, then)

                    game.repair(f, cost, then)
                    game.repairAlt(f, cost, then)

                case Mobilization =>
                    if (one && f.anyBattle && f.can(Tactical))
                        game.move(f, cost, MayBattleAction(f, |(Tactical), then))
                    else
                        game.move(f, cost, then)

                    game.moveAlt(f, cost, then)

                    if (one && f.can(Influential))
                        game.influence(f, cost, MayInfluenceAction(f, |(Influential), then))
                    else
                        game.influence(f, cost, then)

                    game.influenceAlt(f, cost, then)

            }

            if (s != Aggression && f.anyBattle) {
                if (one && f.can(Tactical)) {
                    game.battle(f, cost, MayMoveAction(f, |(Tactical), then))

                    if (s != Mobilization)
                        game.move(f, cost, MustBattleAction(f, |(Tactical), then))
                }
                else
                    game.battle(f, cost, then)

                game.battleAlt(f, cost, then)
            }

            + EndTurnAction(f).as("End Turn and Forfeit", (n - i).hlb, "Action".s(n - i))(group)

            ask(f).needOk

        case EndTurnAction(f) if systems.exists(s => f.at(s).ships.any || f.at(s).starports.any).not && f.pool(Ship) =>
            log(DottedLine)

            f.log("had no", "Ships".hh, "or", "Starports".hh)

            Ask(f).group("Place", min(3, f.pooled(Ship)).hlb, Ship.sof(f), "in")
                .each(systems.%(_.gate))(r => ShipsInSystemAction(f, r, EndTurnAction(f)).as(r))
                .needOk

        case EndTurnAction(f) =>
            f.taxed.cities = $
            f.taxed.slots = $
            f.worked = $
            f.secured = $
            f.used = $
            f.anyBattle = false

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
                    factions.sortBy(f => f.played.single.%(_.suit == lead.get.suit).%(_ => f.zeroed.not)./(-_.strength).|(0)).first

            if (factions.starting.has(next))
                next.log("held the initiative")
            else
                next.log("took the initiative")

            if (game.decided.none)
                if (factions.exists(_.played.exists(_.is[EventCard])))
                    game.decided = |(Blights)

            factions.foreach { f =>
                f.taking.foreach { d =>
                    d --> f.hand

                    f.log("took", d)
                }

                f.taking = $

                f.discardAfterRound.foreach { c =>
                    c --> game.discourt

                    f.log("discarded", c)
                }
            }

            game.lead = None
            game.seized = None

            factions.foreach { f =>
                f.played --> options.has(SplitDiscardPile).?(discard).|(deck)
                f.blind --> deck
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
            game.factions = factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f)

            game.current = |(f)

            if (game.decided.has(Blights))
                ResolveEventAction(f)
            else
            if (game.decided.any)
                ResolveCouncilAction(f)
            else
                ContinueRoundsAction

        case ContinueRoundsAction =>
            game.decided = None

            if (factions.exists(_.hand.any))
                Milestone(StartRoundAction)
            else
                Milestone(EndChapterAction)

        case EndChapterAction =>
            log(("Chapter".hl ~ " " ~ chapter.hlb).styled(styles.title), "had ended")

            MultiAsk(factions./(f =>
                Ask(f)
                    .add(ScoreChapterAction.as("Score Ambitions")(("Chapter".hl ~ " " ~ chapter.hlb).styled(styles.title)))
                    .needOk
            ))

        case ScoreChapterAction =>
            factions.foreach { f =>
                f.hand --> deck
            }
            discard --> deck

            game.seen = $
            factions.foreach { f =>
                f.seen = $
            }

            $(Tycoon, Tyrant, Warlord, Keeper, Empath).foreach { ambition =>
                if (game.declared.contains(ambition)) {
                    val l = game.declared(ambition)
                    val high = l./(_.high).sum
                    val low = l./(_.low).sum

                    val records = factions./(f => f -> ambition @@ {
                        case Tycoon =>
                            f.resources.count(Material) +
                            f.resources.count(Fuel) +
                            f.loyal.of[GuildCard].count(_.suit == Material) +
                            f.loyal.of[GuildCard].count(_.suit == Fuel) +
                            f.can(MaterialCartel).??(game.availableNum(Material)) +
                            f.can(FuelCartel).??(game.availableNum(Fuel))
                        case Tyrant => f.captives.num
                        case Warlord => f.trophies.num
                        case Keeper => f.resources.count(Relic) + f.loyal.of[GuildCard].count(_.suit == Relic)
                        case Empath => f.resources.count(Psionic) + f.loyal.of[GuildCard].count(_.suit == Psionic)
                    }).%>(_ > 0).toMap

                    if (records.isEmpty) {
                        log("No one scored", ambition)
                    }
                    else {
                        val max = records.values.max
                        var ff = records.keys.%(f => records(f) == max).$
                        var ss = records.keys.%(f => records(f) == records.values.$.but(max).maxOr(0)).$

                        val first = (ff.num == 1).?(ff).|($)
                        val second = (ff.num == 1).?(ss.single.$).|(ff)

                        first.foreach { f =>
                            var p = high + (f.pooled(City) < 2).??(2) + (f.pooled(City) < 1).??(3)

                            $(
                                (Just, Tyrant),
                                (Academic, Tycoon),
                                (Violent, Empath),
                            ).foreach { case (t, a) =>
                                if (f.can(t) && ambition == a)
                                    p = low
                            }

                            f.power += p
                            f.log("scored first place", ambition, "for", p.power)

                            if (f.objective.has(InspireConfidence) && f.regent.not)
                                f.advance(3)

                            if (f.objective.has(ConsolidateImperialPower) && f.primus)
                                f.advance(p)
                        }

                        if (low > 0)
                        second.foreach { f =>
                            var p = low

                            $(
                                (Just, Tyrant),
                                (Academic, Tycoon),
                                (Violent, Empath),
                                (Proud, Tycoon),
                                (Proud, Tyrant),
                                (Proud, Warlord),
                                (Proud, Keeper),
                                (Proud, Empath),
                            ).foreach { case (t, a) =>
                                if (f.can(t) && ambition == a)
                                    p = 0
                            }

                            f.power += p
                            f.log("scored second place", ambition, "for", p.power)
                        }
                    }
                }
            }

            Milestone(CleanUpChapterAction)

        case CleanUpChapterAction =>
            if (game.declared.contains(Tycoon)) {
                factions.foreach { f =>
                    if (f.can(Lavish)) {
                        while (f.resources.has(Fuel)) {
                            f.remove(ResourceRef(Fuel, None))

                            f.log("discarded", Fuel, "due to", Lavish)
                        }
                    }
                }
            }

            if (game.declared.contains(Tyrant)) {
                factions.foreach { f =>
                    f.captives.foreach { u =>
                        u --> u.faction.reserve

                        f.log("returned", u)
                    }
                }
            }

            if (game.declared.contains(Warlord)) {
                factions.foreach { f =>
                    f.trophies.foreach { u =>
                        u --> u.faction.reserve

                        if (u.piece == City)
                            if (u.faction.pooled(City) > 2)
                                u.faction.as[Faction].foreach { f =>
                                    f.adjust = true
                                }

                        f.log("returned", u)
                    }
                }
            }

            factions.%(_.can(MaterialCartel)).some.foreach { mc =>
                factions.diff(mc).foreach { f =>
                    while (f.resources.has(Material)) {
                        f.remove(ResourceRef(Material, None))

                        f.log("discarded", Material, "due to", MaterialCartel)
                    }
                }
            }

            factions.%(_.can(FuelCartel)).some.foreach { fc =>
                factions.diff(fc).foreach { f =>
                    while (f.resources.has(Fuel)) {
                        f.remove(ResourceRef(Fuel, None))

                        f.log("discarded", Fuel, "due to", FuelCartel)
                    }
                }
            }

            AdjustResourcesAction(CheckWinAction)

        case GameOverAction(winner) =>
            val winners = $(winner)

            game.seized = |(winner)
            game.current = |(winner)
            game.isOver = true

            winners.foreach(f => f.log("won"))

            GameOver(winners, "Game Over", winners./~(f => $(GameOverWonAction(null, f))))

        case _ => UnknownContinue
    }
}
