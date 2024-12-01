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
case class StartingInitiativeAction(random : Faction) extends RandomAction[Faction] with SkipValidate
case class PlayOrderAction(random : $[Faction]) extends RandomAction[$[Faction]]
case object StartSetupAction extends ForcedAction
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
case class CheckSeizeAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class SeizeAction(self : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class DeclareAmbitionAction(self : Faction, ambition : Ambition, zero : Boolean, then : ForcedAction) extends ForcedAction
case class AmbitionDeclaredAction(self : Faction, ambition : Ambition, used : $[Effect], then : ForcedAction) extends ForcedAction with Soft
case class PreludeActionAction(self : Faction, suit : Suit, pips : Int) extends ForcedAction

case class AdjustResourcesAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class MultiAdjustResourcesAction(l : $[Faction],  then : ForcedAction) extends ForcedAction
case class ContinueMultiAdjustResourcesAction(then : ForcedAction) extends ForcedAction with Soft

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


case class TaxMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class TaxLoyalAction(self : Faction, cost : Cost, r : System, c : Figure, then : ForcedAction) extends ForcedAction
case class TaxRivalAction(self : Faction, cost : Cost, r : System, e : Faction, c : Figure, then : ForcedAction) extends ForcedAction
case class TaxBonusAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction

case class MoveMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class MoveFromAction(self : Faction, r : System, l : $[Figure], cascade : Boolean, x : Cost, alt : UserAction, then : ForcedAction) extends ForcedAction with Soft
case class MoveToAction(self : Faction, r : System, d : System, l : $[Figure], cascade : Boolean, x : Cost, then : ForcedAction) extends ForcedAction with Soft
case class MoveListAction(self : Faction, r : System, d : System, l : $[Figure], cascade : Boolean, x : Cost, then : ForcedAction) extends ForcedAction

case class BattleMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class BattleSystemAction(self : Faction, cost : Cost, r : System, then : ForcedAction) extends ForcedAction with Soft
case class BattleFactionAction(self : Faction, cost : Cost, r : System, e : Faction, then : ForcedAction) extends ForcedAction with Soft
case class BattleDiceAction(self : Faction, cost : Cost, r : System, e : Faction, skirmish : Int, assault : Int, raid : Int, then : ForcedAction) extends ForcedAction
case class BattleRolledAction(self : Faction, r : System, e : Faction, rolled1 : $[$[BattleResult]], rolled2 : $[$[BattleResult]], rolled3 : $[$[BattleResult]], then : ForcedAction) extends Rolled3Action[$[BattleResult], $[BattleResult], $[BattleResult]]
case class SkirmishersAction(self : Faction, r : System, e : Faction, results : $[BattleResult], reroll : $[$[BattleResult]], then : ForcedAction) extends ForcedAction
case class SkirmishersRolledAction(self : Faction, r : System, e : Faction, results : $[BattleResult], old : $[$[BattleResult]], rolled : $[$[BattleResult]], then : ForcedAction) extends RolledAction[$[BattleResult]]
case class BattleProcessAction(self : Faction, r : System, e : Faction, results : $[BattleResult], then : ForcedAction) extends ForcedAction
case class BattleRaidAction(self : Faction, r : System, e : Faction, raid : Int, then : ForcedAction) extends ForcedAction with Soft
case class BattleRaidResourceAction(self : Faction, e : Faction, r : Resource, keys : Int, then : ForcedAction) extends ForcedAction
case class BattleRaidCourtCardAction(self : Faction, e : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction


case class AssignHitsAction(self : Faction, r : System, f : Faction, e : Faction, l : $[Figure], hits : Int, bombardments : Int, raid : Int, then : ForcedAction) extends ForcedAction with Soft
case class DealHitsAction(self : Faction, r : System, f : Faction, e : Faction, l : $[Figure], raid : Int, then : ForcedAction) extends ForcedAction
case class OutrageAction(self : Faction, r : Resource, then : ForcedAction) extends ForcedAction
case class RansackMainAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction with Soft
case class RansackAction(self : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction

case class BuildMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class BuildCityAction(self : Faction, cost : Cost, r : System, then : ForcedAction) extends ForcedAction
case class BuildStarportAction(self : Faction, cost : Cost, r : System, then : ForcedAction) extends ForcedAction
case class BuildShipAction(self : Faction, cost : Cost, r : System, then : ForcedAction) extends ForcedAction

case class RepairMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class RepairAction(self : Faction, cost : Cost, r : System, u : Figure, then : ForcedAction) extends ForcedAction

case class InfluenceMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class InfluenceAction(self : Faction, cost : Cost, c : CourtCard, then : ForcedAction) extends ForcedAction

case class SecureMainAction(self : Faction, cost : Cost, then : ForcedAction) extends ForcedAction with Soft
case class SecureAction(self : Faction, cost : Cost, c : CourtCard, then : ForcedAction) extends ForcedAction

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
case class TakeResourceAction(self : Faction, e : Faction, x : Resource, k : Int, then : ForcedAction) extends ForcedAction
case class GiveBackResourceMainAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction with Soft
case class GiveBackResourceAction(self : Faction, e : Faction, r : Resource, then : ForcedAction) extends ForcedAction


case class LatticeSeizeAction(self : Faction, c : GuildCard, then : ForcedAction) extends ForcedAction

case class FreeCityAction(self : Faction, r : System, u : Figure, then : ForcedAction) extends ForcedAction
case class FreeCitySeazeAskAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class FreeCitySeazeAction(self : Faction, then : ForcedAction) extends ForcedAction

case class OutrageSpreadsAction(self : Faction, r : Resource, then : ForcedAction) extends ForcedAction

case class GainCourtCardAction(self : Faction, c : CourtCard, from : |[Faction], then : ForcedAction) extends ForcedAction
case class DiscardCourtCardAction(self : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction
case class BuryCourtCardAction(self : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction

case class UsedEffectCardAction(self : Faction, c : Effect, then : ForcedAction) extends ForcedAction

case class EndPreludeAction(self : Faction, suit : Suit, done : Int, total : Int) extends ForcedAction
case class MainTurnAction(self : Faction, suit : Suit, done : Int, total : Int) extends ForcedAction with Soft
case class EndTurnAction(self : Faction) extends ForcedAction
case object EndRoundAction extends ForcedAction
case object TransferRoundAction extends ForcedAction
case object EndChapterAction extends ForcedAction
case object CleanUpChapterAction extends ForcedAction

case class ViewCardInfoAction(self : Faction, d : DeckCard) extends BaseInfo(Break ~ "Hand")(d.img) with ViewCard with OnClickInfo { def param = d }


case class GameOverAction(winner : Faction) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won", "(" ~ NameReference(f.name, f).hl ~ ")")

case object NoHand extends HiddenInfo

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

        case StartingInitiativeAction(f) =>
            game.current = f

            log("Old initiative roll")

            game.seating = game.setup.dropWhile(_ != f) ++ game.setup.takeWhile(_ != f)

            game.factions = game.seating

            f.log("randomly took initiative")

            log("Play order", factions.comma)

            StartSetupAction

        case PlayOrderAction(l) =>
            game.seating = l

            game.factions = game.seating

            game.current = l.first

            l.first.log("randomly took initiative")

            log("Play order", l.comma)

            StartSetupAction

        case CourtSetupAction =>
            ShuffleCourtDiscardAction(ReplenishMarketAction(FactionsSetupAction))

        case ShuffleCourtDiscardAction(then) =>
            discourt --> discourt.of[GuildCard] --> court

            if (options.has(BadGuildStruggle))
                discourt --> court

            if (chapter > 0)
                log("Guild cards from court discard were added to the court deck")

            ShuffleCourtDeckAction(then)

        case ShuffleCourtDeckAction(then) =>
            Shuffle[CourtCard](court, ShuffledCourtDeckAction(_, then))

        case ShuffledCourtDeckAction(l, then) =>
            l.diff(court.$).foreach { c => game.log(c, "was not found in court deck".styled(xstyles.warning)) }

            court --> l.intersect(court.$) --> court

            log("The court deck was shuffled")

            then

        case ReplenishMarketAction(then) =>
            while (market.num < 4 && court.any) {
                court.take(1) --> market

                if (chapter > 0)
                    log("A card was added to the market")
            }

            if (chapter == 0)
                log("Cards were added to the market")

            then

        case FactionsSetupAction =>
            if (options.has(LeadersAndLorePreset1))
                Shuffle2[Leader, Lore](Leaders.preset1, Lores.preset1, (l1, l2) => LeadersLoresShuffledAction(l1, l2))
            else
                BaseFactionsSetupAction

        case BaseFactionsSetupAction =>
            factions.lazyZip(board.starting).foreach { case (f, (city, port, fleets)) =>
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

        // ADJUST
        case AdjustResourcesAction(f, then) =>
            Force(AdjustingResourcesAction(f, f.resources, then))

        case MultiAdjustResourcesAction(Nil, then) =>
            then

        case MultiAdjustResourcesAction(l, then) =>
            l.foreach { _.adjust = true }

            ContinueMultiAdjustResourcesAction(then)

        case ContinueMultiAdjustResourcesAction(then) =>
            if (factions.exists(_.adjust))
                MultiAsk(factions.%(_.adjust)./~(f => game.internalPerform(AdjustingResourcesAction(f, f.resources, ContinueMultiAdjustResourcesAction(then)), NoVoid).as[Ask]))
            else
                Milestone(then)

        case AdjustingResourcesAction(f, ll, then) =>
            val kk = f.resourceSlots

            val keys = f.keys.take(kk)./(n => Image("keys-" + n, styles.token3x).spn(styles.card0)(styles.circle)) ~ (f.resources.num - kk).times(Image("discard-resource", styles.token3x).spn(styles.card0)(styles.circle))

            val l = ll ++ (kk - ll.num).times(Nothingness)

            implicit val convert = (r : Resource) => Image(r.name, styles.token3x)

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
            if (f.resources.has(Nothingness)) {
                if (f.add(r))
                    FillSlotsMainAction(f, r, then)
                else
                if (factions.but(f).exists(_.stealable(r)))
                    StealResourceMainAction(f, |(r), $(), FillSlotsMainAction(f, r, then))
                else
                    then
            }
            else
                then

        case StealResourceMainAction(f, x, extra, then) =>
            Ask(f).group("Steal", x.|("Resource"))
                .some(factions.but(f).%(e => x./(e.resources.has).|(e.resources.but(Nothingness).any))) { e =>
                    e.resources.lazyZip(e.keys).toList.%<(_ != Nothingness).%<(r => x.but(r).none)./ { case (x, k) =>
                        StealResourceAction(f, e, x, k, then).as(ResourceRef(x, |(k)))
                            .!(e.loyal.has(SwornGuardians))
                    }
                }
                .needOk
                .add(extra)

        case StealResourceAction(f, e, x, k, then) =>
            e.remove(ResourceRef(x, |(k)))

            if (f.add(x)) {
                f.log("stole", ResourceRef(x, |(k)))

                AdjustResourcesAction(f, then)
            }
            else
                then

        case StealGuildCardMainAction(f, alt, then) =>
            Ask(f).group("Steal".hl, "a", "Guild Card".hh)
                .some(factions.but(f))(e => e.loyal.of[GuildCard]./ { c =>
                    StealGuildCardAction(f, e, c, then).as(c, "from", e)
                        .!(e.loyal.has(SwornGuardians) && c != SwornGuardians)
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
            val ss = systems.%(_.symbol == Gate)
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

            f.log("placed", l, "in", r)

            then

        case PressgangMainAction(f, x, then) =>
            val done = (x == AlreadyPaid).?(AdjustResourcesAction(f, then).as("Done")).|(CancelAction)

            implicit val convert = (u : Figure, selected : Boolean) => {
                val prefix = selected.not.??(u.faction.?./(_.short.toLowerCase + "-").|(""))
                val suffix = selected.??("-empty")

                Image(prefix + "agent" + suffix, styles.qship)
            }

            if (f.captives.any)
                YYSelectObjectsAction(f, f.captives)
                    .withGroup("Pressgang captives", x)
                    .withThens(u => Resources.all.%(game.available)./(r => PressganAction(f, u, r, x, then).as(ResourceRef(r, None))))
                    .withExtras(done)
            else
                Ask(f)(done)

        case PressganAction(f, u, r, x, then) =>
            f.pay(x)

            f.add(r)

            u --> u.faction.reserve

            f.log("released", u, "and took", r, x)

            PressgangMainAction(f, AlreadyPaid, then)

        case ExecuteMainAction(f, x, then) =>
            implicit val convert = (u : Figure, selected : Boolean) => {
                val prefix = selected.not.??(u.faction.?./(_.short.toLowerCase + "-").|(""))
                val suffix = selected.??("-empty")

                Image(prefix + "agent" + suffix, styles.qship)
            }

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

            f.log("abducted", l, x)

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
                .withThen(l => FarseersRedrawAction(f, l, then))(l => ("Discard to draw", (l.num + 1).cards))
                .withExtras(NoHand, FarseersRedrawAction(f, $, then).as("Discard", Farseers, "to draw", 1.cards), CancelAction)

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

            if (f.add(r)) {
                f.log("fenced", Relic, x)

                AdjustResourcesAction(f, then)
            }
            else
                then

        case GainResourcesAction(f, l, then) =>
            var any = false

            l.foreach { r =>
                if (f.add(r)) {
                    any = true

                    f.log("gained", r)
                }
            }

            if (any)
                AdjustResourcesAction(f, then)
            else
                then
//]]

//[[ PINKER
        // ON SECURE
        case GainCourtCardAction(f, c @ CallToAction, e, then) =>
            deck.take(1) --> f.hand

            f.log("drew", 1.cards)

            DiscardCourtCardAction(f, c, then)

        case GainCourtCardAction(f, c @ PopulistDemands, e, then) =>
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

        case GainCourtCardAction(f, c @ MassUprising, e, then) =>
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

        case GainCourtCardAction(f, c @ GuildStruggle, e, then) =>
            val next = ShuffleCourtDiscardAction(DiscardCourtCardAction(f, c, then))

            StealGuildCardMainAction(f, $(next.as("Skip")), next)

        case GainCourtCardAction(f, c @ SongOfFreedom, e, then) =>
            val next = BuryCourtCardAction(f, c, ShuffleCourtDeckAction(then))

            val l = systems.%(f.rules)

            Ask(f).group(SongOfFreedom, "frees a", "City".hl)
                .some(l)(r => factions./~(_.at(r).cities)./(u => FreeCityAction(f, r, u, next).as(u, "in", r)))
                .skip(next)

        case FreeCityAction(f, r, u, then) =>
            u --> u.faction.reserve

            f.log("freed", u, "in", r)

            AdjustResourcesAction(u.faction.as[Faction].get, FreeCitySeazeAskAction(f, then))

        case FreeCitySeazeAskAction(f, then) =>
            if (game.seized.none && factions.first != f)
                Ask(f).group("Initiative")
                    .add(FreeCitySeazeAction(f, then).as("Seize".hl))
                    .skip(then)
            else
                NoAsk(f)(then)

        case FreeCitySeazeAction(f, then) =>
            game.seized = |(f)

            f.log("seized the initative")

            then

        case GainCourtCardAction(f, c @ OutrageSpreads, e, then) =>
            val next = DiscardCourtCardAction(f, c, then)

            Ask(f).group(c)
                .each(Resources.all)(r => OutrageSpreadsAction(f, r, next).as(ResourceRef(r, None)))
                .skip(next)

        case OutrageSpreadsAction(f, r, then) =>
            val l = factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f)

            l.foldLeft(then)((q, e) => OutrageAction(e, r, q))

        case GainCourtCardAction(f, c @ SwornGuardians, Some(e), then) =>
            BuryCourtCardAction(f, c, then)
//]]

        // COURT CARDS
        case GainCourtCardAction(f, c : GuildCard, e, then) =>
            then

        case DiscardCourtCardAction(f, c, then) =>
            f.log("discarded", c)

            f.loyal --> c --> discourt

            then

        case BuryCourtCardAction(f, c, then) =>
            f.log("buried", c)

            f.loyal --> c --> court

            then

        case UsedEffectCardAction(f, c, then) =>
            f.used :+= c

            then

        // TAX
        case TaxMainAction(f, x, then) =>
            val g = "Tax".hl

            Ask(f).group(g)
                .some(systems)(s => f.at(s).cities./(c => TaxLoyalAction(f, x, s, c, TaxBonusAction(f, x, then)).as(c, "in", s, |(board.resource(s)).%(game.available)./(r => ("for", r, Image(r.name, styles.token))))(g).!(f.taxed.has(c), "taxed")))
                .some(systems.%(f.rules))(s => factions.but(f)./~(e => e.at(s).cities./(c => TaxRivalAction(f, x, s, e, c, TaxBonusAction(f, x, then)).as(c, "in", s, |(board.resource(s)).%(game.available)./(r => ("for", r, Image(r.name, styles.token))))(g).!(f.taxed.has(c), "taxed"))))
                .cancel

        case TaxLoyalAction(f, x, r, c, then) =>
            f.pay(x)

            f.log("taxed", c, "in", r, x.elemLog)

            f.taxed :+= c

            if (f.add(board.resource(r))) {
                f.log("gained", board.resource(r))

                AdjustResourcesAction(f, then)
            }
            else
                then

        case TaxRivalAction(f, x, r, e, c, then) =>
            f.pay(x)

            f.log("taxed", c, "in", r, x.elemLog)

            f.taxed :+= c

            if (e.pooled(Agent) > e.outraged.num) {
                e.reserve --> Agent --> f.captives

                f.log("captured", Agent.of(e))
            }

            if (f.add(board.resource(r))) {
                f.log("gained", board.resource(r))

                AdjustResourcesAction(f, then)
            }
            else
                then

        case TaxBonusAction(f, x, then) =>
            var next = then

            if (x == Pip && (f.copy || f.pivot)) {
                if (f.can(Attuned) && f.add(Psionic)) {
                    f.log("gained", Psionic, "from", Attuned)
                    next = AdjustResourcesAction(f, next)
                }

                if (f.can(Insatiable) && f.add(Fuel)) {
                    f.log("gained", Fuel, "from", Insatiable)
                    next = AdjustResourcesAction(f, next)
                }

                if (f.can(Firebrand) && f.add(Weapon)) {
                    f.log("gained", Weapon, "from", Firebrand)
                    next = AdjustResourcesAction(f, next)
                }
            }

            next

        // MOVE
        case MoveMainAction(f, x, then) =>
            val pp = systems.%(f.at(_).hasA(Starport))
            val ss = systems./(r => r -> f.at(r).ships).%>(_.any).toMap

            Ask(f).group("Move from")
                .each(ss.keys.% (pp.has).$)(r => MoveFromAction(f, r, ss(r), true,  x, CancelAction, then).as(r))
                .each(ss.keys.%!(pp.has).$)(r => MoveFromAction(f, r, ss(r), false, x, CancelAction, then).as(r))
                .cancel

        case MoveFromAction(f, r, l, cascade, x, alt, then) =>
            Ask(f).group("Move from", r, "to")
                .each(board.connected(r))(d => MoveToAction(f, r, d, l, cascade && d.symbol == Gate && factions.but(f).exists(_.rules(d)).not, x, then).as(d))
                .add(alt)

        case MoveToAction(f, r, d, l, cascade, x, then) =>
            val n = f.can(Disorganized).?(min(2, l.num)).|(l.num)
            val (damaged, fresh) = l.partition(f.damaged.has)
            val combinations = 1.to(n)./~(k => max(0, k - fresh.num).to(min(k, damaged.num))./(i => fresh.take(k - i) ++ damaged.take(i)))

            implicit val convert = (u : Figure, k : Int) => {
                val status = u.faction.as[Faction].?(_.damaged.has(u)).??(1) + k

                val prefix = (k < 2).??(u.faction.?./(_.short.toLowerCase + "-").|(""))
                val suffix = (k == 1).??("-damaged") + (k == 2).??("-empty")

                u.piece match {
                    case City => Image(prefix + "city" + suffix, styles.qbuilding)
                    case Starport => Image(prefix + "starport" + suffix, styles.qbuilding)
                    case Ship => Image(prefix + "ship" + suffix, styles.qship)
                }
            }

            Ask(f).group("Move from", r, "to", d)
                .each(combinations)(l => MoveListAction(f, r, d, l, cascade, x, then).as(l./(u => convert(u, damaged.has(u).??(1)))))
                .cancel

        case MoveListAction(f, r, d, l, cascade, x, then) =>
            f.pay(x)

            f.log("moved", l.comma, "from", r, "to", d, x.elemLog)

            l --> d

            if (cascade)
                Force(MoveFromAction(f, d, l, true, NoCost, then.as("Done"), then))
            else
                Force(then)

        // BATTLE
        case BattleMainAction(f, x, then) =>
            Ask(f).group(f, "battles in", x)
                .each(systems.%(f.at(_).hasA(Ship)).%(r => factions.but(f).exists(_.present(r))))(r => BattleSystemAction(f, x, r, then).as(r))
                .cancel

        case BattleSystemAction(f, x, r, then) =>
            Ask(f).group(f, "battles in", r, x)
                .each(factions.but(f).%(_.present(r)))(e => BattleFactionAction(f, x, r, e, then).as(e))
                .cancel

        case BattleFactionAction(f, x, r, e, then) =>
            val ships = f.at(r).count(Ship) + (r.symbol == Gate).??(f.loyal.has(Gatekeepers).??(2)) + f.can(Committed).??(2)
            val canRaid = e.at(r).hasBuilding || systems.exists(e.at(_).hasBuilding).not
            val combinations : $[(Int, Int, Int)] = 1.to(ships).reverse./~(n => 0.to(min(canRaid.??(6), n))./~(raid => 0.to(min(6, n - raid))./~(assault => |(n - raid - assault).%(_ <= 6)./((_, assault, raid)))))

            Ask(f).group(f, "battles", e, "in", r, x)
                .each(combinations){ case (skirmish, assault, raid) => BattleDiceAction(f, x, r, e, skirmish, assault, raid, then).as(skirmish.times(Image("skirmish-die", styles.token)), assault.times(Image("assault-die", styles.token)), raid.times(Image("raid-die", styles.token))) }
                .cancel

        case BattleDiceAction(f, x, r, e, skirmish, assault, raid, then) =>
            f.pay(x)

            f.log("battled", e, "in", r, x.elemLog)

            Roll3[$[BattleResult], $[BattleResult], $[BattleResult]](skirmish.times(Skirmish.die), assault.times(Assault.die), raid.times(Raid.die), (l1, l2, l3) => BattleRolledAction(f, r, e, l1, l2, l3, then))

        case BattleRolledAction(f, r, e, l1, l2, l3, then) =>
            f.log("rolled",
                l1./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)) ~
                l2./(x => Image("assault-die-" + (Assault.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)) ~
                l3./(x => Image("raid-die-" + (Raid.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token))
            )

            val next = BattleProcessAction(f, r, e, (l1 ++ l2 ++ l3).flatten, then)

            if (l1.any && f.loyal.has(Skirmishers)) {
                val limit = f.resources.count(Weapon) + f.loyal.of[GuildCard].count(_.suit == Weapon)
                val miss = $()
                val hit = $(HitShip)
                val misses = l1.count(miss)
                val hits = l1.count(hit)
                val rerollable = 1.to(min(limit, misses)).reverse./(_.times(miss)) ++ 1.to(min(limit, hits))./(_.times(hit))

                Ask(f).group(Skirmishers)
                    .each(rerollable)(q => SkirmishersAction(f, r, e, (l1.diff(q) ++ l2 ++ l3).flatten, q, then).as("Reroll", q./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token))))
                    .skip(next)
            }
            else
                NoAsk(f)(next)

        case SkirmishersAction(f, r, e, l, q, then) =>
            Roll[$[BattleResult]](q.num.times(Skirmish.die), n => SkirmishersRolledAction(f, r, e, l, q, n, then))

        case SkirmishersRolledAction(f, r, e, l, q, n, then) =>
            f.log("rerolled",
                q./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)),
                "to",
                n./(x => Image("skirmish-die-" + (Skirmish.die.values.indexed.%(_ == x).indices.shuffle(0) + 1), styles.token)),
                "with", Skirmishers)

            BattleProcessAction(f, r, e, l ++ n.flatten, then)

        case BattleProcessAction(f, r, e, l, then) =>
            val sd = l.count(OwnDamage)
            val ic = l.has(Intersept).??(e.at(r).ships.diff(e.damaged).num)
            val hs = l.count(HitShip)
            val bb = l.count(HitBuilding)
            val rd = l.count(RaidKey)

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

            AssignHitsAction(f, r, e, f, f.at(r).ships, sd + ic, 0, 0, AssignHitsAction(f, r, f, e, e.at(r), hs, bb, rd, then))

        case BattleRaidAction(f, r, e, raid, then) =>
            def spend(k : Int) = (k >= raid).?(then).|(BattleRaidAction(f, r, e, raid - k, then))

            if (f.at(r).ships.any && raid > 0)
                Ask(f).group("Raid", e, "with", raid.times(Image("raid-key", styles.token)).merge)
                    .each(e.resources.lazyZip(e.keys).toList.%<(_ != Nothingness)) { case (x, k) =>
                        BattleRaidResourceAction(f, e, x, k, spend(k)).as(ResourceRef(x, |(k)))
                            .!(k > raid)
                            .!(e.loyal.has(SwornGuardians))
                    }
                    .each(e.loyal.of[GuildCard]) { c =>
                        BattleRaidCourtCardAction(f, e, c, spend(c.keys)).as(c)
                            .!(c.keys > raid)
                            .!(e.loyal.has(SwornGuardians) && c != SwornGuardians)
                    }
                    .needOk
                    .done(then)
            else
                NoAsk(f)(then)

        case BattleRaidResourceAction(f, e, x, k, then) =>
            e.remove(ResourceRef(x, |(k)))

            if (f.add(x)) {
                f.log("stole", ResourceRef(x, |(k)))

                AdjustResourcesAction(f, then)
            }
            else
                then

        case BattleRaidCourtCardAction(f, e, c, then) =>
            e.loyal --> c --> f.loyal

            f.log("stole", c)

            GainCourtCardAction(f, c, |(e), then)

        case AssignHitsAction(self, r, f, e, l, hits, bombardments, raid, then) =>
            implicit val convert = (u : Figure, k : Int) => {
                val status = u.faction.as[Faction].?(_.damaged.has(u)).??(1) + k

                val prefix = (status < 2).??(u.faction.?./(_.short.toLowerCase + "-").|(""))
                val suffix = (status == 1).??("-damaged") + (status == 2).??("-empty")

                u.piece match {
                    case City => Image(prefix + "city" + suffix, styles.qbuilding)
                    case Starport => Image(prefix + "starport" + suffix, styles.qbuilding)
                    case Ship => Image(prefix + "ship" + suffix, styles.qship)
                }
            }

            var h = hits
            var b = bombardments

            val ships = l.ships./(u => u.faction.as[Faction].?(_.damaged.has(u)).?(1).|(2)).sum
            val buildings = l.buildings./(u => u.faction.as[Faction].?(_.damaged.has(u)).?(1).|(2)).sum

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
                    .withMultipleSelects(u => 2 - e.damaged.has(u).??(1))
                    .withThen(d => DealHitsAction(self, r, f, e, d, raid, then))(_ => "Damage".hl)
                    .ask
            else
            if (raid > 0 && f.at(r).ships.any)
                NoAsk(self)(BattleRaidAction(f, r, e, raid, then))
            else
                NoAsk(self)(then)

        case DealHitsAction(self, r, f, e, l, k, then) =>
            val dd = e.damaged ++ l
            val destroyed = dd.diff(dd.distinct)

            if (destroyed.any)
                f.log("destroyed", destroyed.comma)

            e.damaged = e.damaged.diff(destroyed)

            val damaged = l.distinct.diff(destroyed)

            if (damaged.any)
                f.log("damaged", damaged.comma)

            e.damaged ++= damaged

            destroyed --> f.trophies

            var next = then

            if (destroyed.any && e.can(Beloved)) {
                e.log("triggered", Beloved)
                next = BelovedAction(e, next)
            }

            if (k > 0 && f.at(r).ships.any)
                next = BattleRaidAction(f, r, e, k, next)

            destroyed.cities.foreach { u =>
                if (e.can(Beloved).not)
                    next = RansackMainAction(f, e, next)

                next = OutrageAction(f, board.resource(r), next)
            }

            next

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

            f.loyal.of[GuildCard].notOf[LoyalGuild].foreach { c =>
                if (c.suit == r) {
                    f.loyal --> c --> discourt

                    f.log("discarded", c)
                }
            }

            then

        case RansackMainAction(f, e, then) =>
            Ask(f).group("Ransack".hl)
                .each(market.%(c => Influence(c).exists(_.faction == e)))(c => RansackAction(f, c, then).as(c))
                .needOk
                .bail(then)

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
            val ll = systems.%(f.present)
            val bb = ll.%(c => game.freeSlots(c) > 0)
            val ss = ll.%(f.at(_).hasA(Starport))
            val prefix = f.short + "-"
            def suffix(s : System) = factions.but(f).exists(_.rules(s)).??("-damaged")

            Ask(f)
                .group("Build".hl)
                .each(f.pool(City).??(bb))(s => BuildCityAction(f, x, s, then).as(City.of(f), Image(prefix + "city" + suffix(s), styles.qbuilding), "in", s))
                .each(f.pool(Starport).??(bb))(s => BuildStarportAction(f, x, s, then).as(Starport.of(f), Image(prefix + "starport" + suffix(s), styles.qbuilding), "in", s))
                .each(f.pool(Ship).??(ss))(s => BuildShipAction(f, x, s, then).as(Ship.of(f), Image(prefix + "ship" + suffix(s), styles.qship), "in", s).!(f.at(s).exists(u => u.piece == Starport && u.faction == f && f.built.has(u).not).not, "built"))
                .cancel

        case BuildCityAction(f, x, r, then) =>
            f.pay(x)

            val u = f.reserve --> City.of(f)

            if (factions.but(f).exists(_.rules(r)))
                f.damaged :+= u

            u --> r

            f.log("built", u, "in", r, x.elemLog)

            then

        case BuildStarportAction(f, x, r, then) =>
            f.pay(x)

            val u = f.reserve --> Starport.of(f)

            if (factions.but(f).exists(_.rules(r)))
                f.damaged :+= u

            u --> r

            f.log("built", u, "in", r, x.elemLog)

            then

        case BuildShipAction(f, x, r, then) =>
            f.pay(x)

            val u = f.reserve --> Ship.of(f)

            if (factions.but(f).exists(_.rules(r)))
                f.damaged :+= u

            u --> r

            f.built :+= f.at(r).%(u => u.piece == Starport && f.built.has(u).not).first

            f.log("built", u, "in", r, x.elemLog)

            then

        // REPAIR
        case RepairMainAction(f, x, then) =>
            val ll = systems.%(f.present)

            Ask(f)
                .group("Repair".hl)
                .some(ll)(r => f.at(r).intersect(f.damaged)./(u => RepairAction(f, x, r, u, then).as(u.piece.of(f), Image(u.faction.short + "-" + u.piece.name + "-damaged", u.piece.is[Building].?(styles.qbuilding).|(styles.qship)), "in", r)))
                .cancel

        case RepairAction(f, x, r, u, then) =>
            f.pay(x)

            f.damaged :-= u

            f.log("repaired", u, "in", r, x.elemLog)

            then

        // INFLUENCE
        case InfluenceMainAction(f, x, then) =>
            Ask(f).group("Influence".hl)
                .each(market)(c => InfluenceAction(f, x, c, then).as(c))
                .cancel

        case InfluenceAction(f, x, c, then) =>
            f.pay(x)

            f.reserve --> Agent --> Influence(c)

            f.log("influenced", c, x.elemLog)

            then

        case SecureMainAction(f, x, then) =>
            Ask(f).group("Secure".hl)
                .each(market)(c => SecureAction(f, x, c, then).as(c)
                    .!(Influence(c).$.use(l => l.%(_.faction == f).num <= factions.but(f)./(e => l.%(_.faction == e).num).max))
                    .!(f.can(Paranoid) && Influence(c).%(_.faction == f).num <= 1, "Paranoid")
                )
                .cancel

        case SecureAction(f, x, c, then) =>
            f.pay(x)

            market --> c --> f.loyal

            f.used :+= c

            f.log("secured", c, x.elemLog)

            Influence(c).foreach { u =>
                if (u.faction == f)
                    u --> f.reserve
                else {
                    u --> f.captives

                    f.log("captured", u)
                }
            }

            GainCourtCardAction(f, c, None, ReplenishMarketAction(then))

        // MANUFACTURE
        case ManufactureMainAction(f, x, then) =>
            f.pay(x)

            f.log("manufactured", x.elemLog)

            if (f.add(Material)) {
                f.log("gained", Material)

                AdjustResourcesAction(f, then)
            }
            else
                then

        // SYNTHESIZE
        case SynthesizeMainAction(f, x, then) =>
            f.pay(x)

            f.log("synthesize", x.elemLog)

            if (f.add(Fuel)) {
                f.log("gained", Fuel)

                AdjustResourcesAction(f, then)
            }
            else
                then

        // TRADE
        case TradeMainAction(f, x, then) =>
            Ask(f).group("Trade".hl)
                .some(factions.but(f).%(_.resources.but(Nothingness).any)) { e =>
                    e.resources.lazyZip(e.keys).toList.%<(_ != Nothingness).%<(r => systems.exists(s => e.at(s).cities.any && board.resource(s) == r && f.rules(s)))./ { case (r, k) =>
                        TakeResourceAction(f, e, r, k, GiveBackResourceMainAction(f, e, then)).as(e, ResourceRef(r, None))
                    }
                }
                .cancel

        case TakeResourceAction(f, e, x, k, then) =>
            e.remove(ResourceRef(x, |(k)))

            f.add(x)

            f.log("took in trade", ResourceRef(x, |(k)))

            then

        case GiveBackResourceMainAction(f, e, then) =>
            Ask(f).group("Give to", e)
                .each(f.resources.but(Nothingness).distinct)(r => GiveBackResourceAction(f, e, r, then).as("Give back", ResourceRef(r, None)))
                .needOk

        case GiveBackResourceAction(f, e, r, then) =>
            f.remove(ResourceRef(r, None))

            e.add(r)

            f.log("gave back", r)

            AdjustResourcesAction(f, AdjustResourcesAction(e, then))

        // WEAPON
        case AddBattleOptionAction(f, x, then) =>
            f.pay(x)

            f.anyBattle = true

            f.log("could use any card action as ", "Battle".styled(f), x.elemLog)

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

            StartRoundAction

        case StartRoundAction =>
            log(DoubleLine)

            game.round += 1

            LeadMainAction(factions.first)

        case LeadMainAction(f) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup(f.elem ~ " leads")
                .withThen(LeadAction(f, _))("Play " ~ _.elem)("Play")
                .withExtras(NoHand, PassAction(f).as("Pass".hh))

        case PassAction(f) =>
            if (f.hand.any)
                game.passed += 1

            if (game.passed >= factions.%(_.hand.any).num) {
                f.log("passed")

                EndChapterAction
            }
            else {
                game.factions = factions.drop(1).dropWhile(_.hand.none) ++ factions.take(1) ++ factions.drop(1).takeWhile(_.hand.none)

                f.log("passed initative to", factions.first)

                StartRoundAction
            }

        case LeadAction(f, d) =>
            game.passed = 0

            game.lead = |(d)

            f.log("led with", d)

            f.hand --> d --> f.played
            game.seen :+= d
            game.seenX :+= (round, f, |(d))

            f.lead = true

            CheckAmbitionAction(f, d)

        case CheckAmbitionAction(f, d) =>
            val s = game.ambitionable.any.??(d.strength)

            val next = PreludeActionAction(f, d.suit, d.pips)

            Ask(f).group("Declare Ambition".hl)
                .add(s.in(2, 7).?(DeclareAmbitionAction(f, Tycoon,  true, next))./(a => a.as(a.ambition)))
                .add(s.in(3, 7).?(DeclareAmbitionAction(f, Tyrant,  true, next))./(a => a.as(a.ambition)))
                .add(s.in(4, 7).?(DeclareAmbitionAction(f, Warlord, true, next))./(a => a.as(a.ambition)))
                .add(s.in(5, 7).?(DeclareAmbitionAction(f, Keeper,  f.loyal.has(SecretOrder).not, next))./(a => a.as(a.ambition)))
                .add(s.in(6, 7).?(DeclareAmbitionAction(f, Empath,  f.loyal.has(SecretOrder).not, next))./(a => a.as(a.ambition)))
                .add(next.as("Skip"))

        case DeclareAmbitionAction(f, a, zero, then) =>
            f.log("declared", a, "ambition")

            game.declared += a -> (game.declared.get(a).|($) ++ $(game.ambitionable.last))

            game.ambitionable = game.ambitionable.dropRight(1)

            if (zero)
                game.zeroed = true

            AmbitionDeclaredAction(f, a, $, then)

        case AmbitionDeclaredAction(f, a, used, then) =>
            var ask = Ask(f)

            if (f.can(Farseers) && used.has(Farseers).not)
                ask = ask.each(factions.but(f))(e => FarseersMainAction(f, e, AmbitionDeclaredAction(f, a, used :+ Farseers, then)).as(e)(Farseers))

            if (f.can(Bold) && used.has(Bold).not)
                ask = ask.add(BoldMainAction(f, $, AmbitionDeclaredAction(f, a, used :+ Bold, then)).as("Influence each card in court".hh)(Bold))

            ask.add(then.as("Done"))

        case FollowAction(f) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup(f.elem ~ " follows " ~ lead.get.elem)
                .withThens(d => $(
                    SurpassAction(f, d).as("Surpass".styled(lead.get.suit).styled(xstyles.bold), "with", d).!(d.suit != lead.get.suit, "wrong suit").!(d.strength < lead.get.strength && zeroed.not, "low strength"),
                    CopyAction(f, d).as("Copy".styled(lead.get.suit).styled(xstyles.bold), "with", d),
                    PivotAction(f, d).as("Pivot".styled(d.suit).styled(xstyles.bold), "with", d).!(d.suit == lead.get.suit, "same suit"),
                ))
                .withExtras(NoHand)

        case SurpassAction(f, d) =>
            log(SingleLine)

            f.log("surpassed with", d)

            f.hand --> d --> f.played
            game.seen :+= d
            game.seenX :+= (round, f, |(d))

            f.surpass = true

            if (game.seized.none && d.strength == 7) {
                f.log("seized the initative")

                game.seized = |(f)
            }

            CheckSeizeAction(f, PreludeActionAction(f, d.suit, d.pips))

        case CopyAction(f, d) =>
            log(SingleLine)

            f.log("copied with a card")

            f.hand --> d --> f.blind
            game.seenX :+= (round, f, None)

            f.copy = true

            CheckSeizeAction(f, PreludeActionAction(f, lead.get.suit, 1))

        case PivotAction(f, d) =>
            log(SingleLine)

            f.log("pivoted with", d)

            f.hand --> d --> f.played
            game.seen :+= d
            game.seenX :+= (round, f, |(d))

            f.pivot = true

            CheckSeizeAction(f, PreludeActionAction(f, d.suit, 1))

        case CheckSeizeAction(f, then) =>
            if (zeroed.not && game.ambitionable.any && f.played.any && f.can(GalacticBards)) {
                val next = UsedEffectCardAction(f, GalacticBards, CheckSeizeAction(f, then))
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
            if (seized.none && (f.hand.any || f.loyal.has(LatticeSpies))) {
                YYSelectObjectsAction(f, f.hand)
                    .withGroup(f.elem ~ " can seize initiative")
                    .withThen(SeizeAction(f, _, then))("Seize with " ~ _.elem)("Seize")
                    .withExtras(NoHand, f.loyal.has(LatticeSpies).?(LatticeSeizeAction(f, LatticeSpies, then).as("Seize with", LatticeSpies)).|(NoHand), then.as("Skip".hh))
            }
            else
                NoAsk(f)(then)

        case SeizeAction(f, d, then) =>
            f.log("seized initiative with a card")

            f.hand --> d --> f.blind
            game.seenX :+= (round, f, None)

            game.seized = |(f)

            then

        case LatticeSeizeAction(f, c, then) =>
            f.log("seized initiative with", c)

            f.loyal --> c --> discourt

            game.seized = |(f)

            then

        case PreludeActionAction(f, s, p) =>
            implicit val ask = builder
            implicit val repeat = PreludeActionAction(f, s, p)
            implicit val group = f.elem ~ " " ~ "Prelude".hh

            f.resources.lazyZip(f.keys).foreach { (r, k) =>
                if (r != Nothingness) {
                    val cost = PayResource(r, |(k))

                    if ((r == Material && f.outraged.has(Material).not) || f.loyal.has(LoyalEngineers)) {
                        game.build(f, cost)
                        game.repair(f, cost)
                    }

                    if ((r == Weapon && s != Aggression && f.outraged.has(Weapon).not) || f.loyal.has(LoyalMarines))
                        + AddBattleOptionAction(f, cost, repeat).as("Add", "Battle".styled(f), "option", cost)(group).!(f.anyBattle)

                    if ((r == Fuel && f.outraged.has(Fuel).not) || f.loyal.has(LoyalPilots))
                        game.move(f, cost)

                    if ((r == Relic && f.outraged.has(Relic).not) || f.loyal.has(LoyalKeepers))
                        game.secure(f, cost)

                    if ((r == Psionic && f.outraged.has(Psionic).not) || f.loyal.has(LoyalEmpaths)) {
                        factions.first.played./(_.suit)./{
                            case Administration =>
                                game.tax(f, cost)
                                game.repair(f, cost)
                                game.influence(f, cost)
                            case Aggression =>
                                game.battle(f, cost)
                                game.move(f, cost)
                                game.secure(f, cost)
                            case Construction =>
                                game.build(f, cost)
                                game.repair(f, cost)
                            case Mobilization =>
                                game.move(f, cost)
                                game.influence(f, cost)
                        }
                    }
                }
            }

            if (f.can(MiningInterest))
                + FillSlotsMainAction(f, Material, DiscardCourtCardAction(f, MiningInterest, repeat)).as("Fill up", ResourceRef(Material, None))(MiningInterest)

            if (f.can(ShippingInterest))
                + FillSlotsMainAction(f, Fuel, DiscardCourtCardAction(f, ShippingInterest, repeat)).as("Fill up", ResourceRef(Fuel, None))(ShippingInterest)

            if (f.can(AdminUnion))
                if (factions.exists(_.played.exists(_.suit == Administration)))
                    + ReserveCardMainAction(f, AdminUnion, factions./~(_.played.%(_.suit == Administration)), repeat).as("Take", Administration, "card")(AdminUnion)

            if (f.can(ConstructionUnion))
                if (factions.exists(_.played.exists(_.suit == Construction)))
                    + ReserveCardMainAction(f, ConstructionUnion, factions./~(_.played.%(_.suit == Construction)), repeat).as("Take", Construction, "card")(ConstructionUnion)

            if (f.can(SpacingUnion))
                if (factions.exists(_.played.exists(_.suit == Mobilization)))
                    + ReserveCardMainAction(f, SpacingUnion, factions./~(_.played.%(_.suit == Mobilization)), repeat).as("Take", Mobilization, "card")(SpacingUnion)

            if (f.can(ArmsUnion))
                if (factions.exists(_.played.exists(_.suit == Aggression)))
                    + ReserveCardMainAction(f, ArmsUnion, factions./~(_.played.%(_.suit == Aggression)), repeat).as("Take", Aggression, "card")(ArmsUnion)

            if (f.can(MaterialCartel))
                + StealResourceMainAction(f, |(Material), $(CancelAction), DiscardCourtCardAction(f, MaterialCartel, repeat)).as("Steal", ResourceRef(Material, None))(MaterialCartel)

            if (f.can(FuelCartel))
                + StealResourceMainAction(f, |(Fuel), $(CancelAction), DiscardCourtCardAction(f, FuelCartel, repeat)).as("Steal", ResourceRef(Fuel, None))(FuelCartel)

            if (f.reserve.$.ships.any) {
                if (f.can(Gatekeepers))
                    + ShipAtEachGateMainAction(f, DiscardCourtCardAction(f, Gatekeepers, repeat)).as("Place", Ship.of(f), "at each gate")(Gatekeepers)

                systems.%(r => f.rules(r)).some.foreach { l =>
                    $(PrisonWardens, Skirmishers, CourtEnforcers, LoyalMarines).foreach { c =>
                        if (f.can(c))
                            + ShipsInSystemMainAction(f, l, DiscardCourtCardAction(f, c, repeat)).as("Place", 3.hlb, Ship.sof(f), "in a controlled system")(c)
                    }
                }
            }

            if (f.can(Farseers)) {
                + FarseersRedrawMainAction(f, DiscardCourtCardAction(f, Farseers, repeat)).as("Redraw cards")(Farseers)
            }

            if (f.can(SilverTongues)) {
                + StealResourceMainAction(f, None, $(CancelAction), DiscardCourtCardAction(f, SilverTongues, repeat)).as("Steal any resource")(SilverTongues)

                + StealGuildCardMainAction(f, $(CancelAction), DiscardCourtCardAction(f, SilverTongues, repeat)).as("Steal a Guild Card")(SilverTongues)
            }

            if (f.can(RelicFence) && game.available(Relic)) {
                f.resources.lazyZip(f.keys).foreach { (r, k) =>
                    if (r != Nothingness) {
                        val cost = PayResource(r, |(k))

                        + FenceResourceAction(f, Relic, cost, UsedEffectCardAction(f, RelicFence, repeat)).as("Gain", ResourceRef(Relic, None), cost)(RelicFence)
                    }
                }
            }

            if (f.can(ElderBroker) && (game.available(Material) || game.available(Fuel) || game.available(Weapon))) {
                + GainResourcesAction(f, $(Material, Fuel, Weapon), DiscardCourtCardAction(f, ElderBroker, repeat)).as("Gain", ResourceRef(Material, None), ResourceRef(Fuel, None), ResourceRef(Weapon, None))(ElderBroker)
            }

            + EndPreludeAction(f, s, 0, p).as("End Prelude")(" ")

            ask(f)

        case EndPreludeAction(f, s, i, n) =>
            f.spent = $

            log(DottedLine)

            MainTurnAction(f, s, i, n)

        case MainTurnAction(f, s, i, n) if i >= n =>
            NoAsk(f)(EndTurnAction(f))

        case MainTurnAction(f, s, i, n) =>
            implicit val ask = builder
            implicit val repeat = MainTurnAction(f, s, i + 1, n)
            implicit val group = f.elem ~ " " ~ "Actions" ~ " " ~ (n - i).times(0x2726.toChar.toString.styled(s))

            val cost = Pip

            s @@ {
                case Administration =>
                    game.tax(f, cost)
                    game.repair(f, cost)
                    game.influence(f, cost)
                case Aggression =>
                    game.battle(f, cost)
                    game.move(f, cost)
                    game.secure(f, cost)
                case Construction =>
                    game.build(f, cost)
                    game.repair(f, cost)
                case Mobilization =>
                    game.move(f, cost)
                    game.influence(f, cost)
            }

            if (s != Aggression && f.anyBattle)
                game.battle(f, cost)

            + EndTurnAction(f).as("Forfeit", (n - i).hl, "actions")(group)

            ask(f).needOk

        case EndTurnAction(f) =>
            f.taxed = $
            f.built = $
            f.used = $
            f.anyBattle = false

            val next = factions.dropWhile(_ != f).drop(1).%(_.hand.any).starting

            if (next.any) {
                game.current = next.get

                FollowAction(next.get)
            }
            else
                EndRoundAction

        case EndRoundAction =>
            factions.foreach { f =>
                f.taking.foreach { d =>
                    d --> f.hand

                    f.log("took", d)
                }

                f.taking = $

                f.discardAfterRound.foreach { c =>
                    c --> discourt

                    f.log("discarded", c)
                }
            }

            Milestone(TransferRoundAction)

            TransferRoundAction

        case TransferRoundAction =>
            val next =
                if (game.seized.any)
                    game.seized.get
                else
                if (options.has(BadInitiativeTransferChapter(chapter))) // !!!! TODO REMOVE !!!!
                    factions.%(_.played.single.exists(d => d.suit == lead.get.suit && d.strength >= zeroed.not.??(lead.get.strength))).starting.|(current)
                else
                    factions.sortBy(f => f.played.single.%(_.suit == lead.get.suit).%(_ => zeroed.not || f.lead.not)./(-_.strength).|(0)).first

            log(DoubleLine)

            if (next != current)
                next.log("took the initiative")
            else
                next.log("held the initiative")

            factions.foreach { f =>
                f.lead = false
                f.surpass = false
                f.copy = false
                f.pivot = false
            }

            game.current = next

            game.factions = factions.dropWhile(_ != current) ++ factions.takeWhile(_ != current)

            factions.foreach { f =>
                f.played --> (if (options.has(SplitDiscardPile)) discard else deck)
                f.blind --> deck
            }

            game.lead = None
            game.zeroed = false
            game.seized = None

            if (factions.exists(_.hand.any))
                StartRoundAction
            else
                EndChapterAction

        case EndChapterAction =>
            factions.foreach { f =>
                f.hand --> deck
            }
            discard --> deck
            game.seen = $
            game.seenX = $

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
                            f.loyal.has(MaterialCartel).??(game.availableNum(Material)) +
                            f.loyal.has(FuelCartel).??(game.availableNum(Fuel))
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

                            if (f.can(Just) && ambition == Tyrant)
                                p = low

                            f.power += p
                            f.log("scored first place", ambition, "for", p.power)
                        }

                        if (low > 0)
                        second.foreach { f =>
                            var p = low

                            if (f.can(Just) && ambition == Tyrant)
                                p = 0

                            f.power += p
                            f.log("scored second place", ambition, "for", p.power)
                        }
                    }
                }
            }

            Milestone(CleanUpChapterAction)

        case CleanUpChapterAction =>
            var adjust : $[Faction] = $

            if (game.declared.contains(Tyrant)) {
                factions.foreach { f =>
                    f.captives.foreach { u =>
                        u --> u.faction.as[Faction].get.reserve

                        f.log("returned", u)
                    }
                }
            }

            if (game.declared.contains(Warlord)) {
                factions.foreach { f =>
                    f.trophies.foreach { u =>
                        u --> u.faction.as[Faction].get.reserve

                        if (u.piece == City)
                            if (u.faction.pooled(City) >= 2)
                                adjust ++= u.faction.as[Faction]

                        f.log("returned", u)
                    }
                }
            }

            factions.%(_.loyal.has(MaterialCartel)).some.foreach { mc =>
                factions.diff(mc).foreach { f =>
                    while (f.resources.has(Material)) {
                        f.remove(ResourceRef(Material, None))

                        f.log("discarded", Material, "due to", MaterialCartel)
                    }
                }
            }

            factions.%(_.loyal.has(FuelCartel)).some.foreach { fc =>
                factions.diff(fc).foreach { f =>
                    while (f.resources.has(Fuel)) {
                        f.remove(ResourceRef(Fuel, None))

                        f.log("discarded", Fuel, "due to", FuelCartel)
                    }
                }
            }

            MultiAdjustResourcesAction(factions.intersect(adjust), CheckWinAction)

        case GameOverAction(winner) =>
            val winners = $(winner)

            game.seized = |(winner)
            game.current = winner
            game.isOver = true

            winners.foreach(f => f.log("won"))

            GameOver(winners, "Game Over", winners./~(f => $(GameOverWonAction(null, f))))

        case _ => UnknownContinue
    }
}
