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

import scala.collection.immutable.SortedMap


trait Trader extends WarriorFaction {
    val clashKey = RF

    val services : $[Service]

    def recruitIn(implicit game : Game) : $[Clearing]

    def pieces(options : $[Meta.O]) = warrior *** 15 ++ TradePost(Fox) *** 3 ++ TradePost(Rabbit) *** 3 ++ TradePost(Mouse) *** 3

    override def initialDamagePriorities(options : $[Meta.O]) = $(Otter, TradePost(Fox), TradePost(Rabbit), TradePost(Mouse))

    def advertising = TradePost(Fox).img(this) ~ TradePost(Rabbit).img(this) ~ TradePost(Mouse).img(this)
    def motto = "Trade".styled(this)
}

object Trader {
    case object Funds extends SpecialRegion
    case object Payments extends SpecialRegion
    case object Commited extends SpecialRegion
    case object Gone extends SpecialRegion
}

case object RF extends Trader {
    val name = "Riverfolk Company"
    override def funName = NameReference(name, this) ~ " Company"
    val short = "RF"
    val style = "rf"
    val priority = "G"

    def abilities(options : $[Meta.O]) = $(Swimmers, Protectionism, Dividents, TradepostHalvesFunds)

    val warrior = Otter

    def recruitIn(implicit game : Game) : $[Clearing] = game.riverside

    override val transports : $[$[Transport]] = $($(RuledMove), $(Waterway))

    val services : $[Service] = $(BuyCard, Riverboats, Mercenaries(this))
}

case object WC extends Trader {
    val name = "Watergate Corporation"
    override def funName = NameReference(name, this) ~ " Corporation"
    val short = "WC"
    val style = "WC"
    val priority = "G'"

    def abilities(options : $[Meta.O]) = $(Swimmers, Protectionism, Dividents, TradepostHalvesFunds)

    val warrior = Otter

    def recruitIn(implicit game : Game) : $[Clearing] = game.riverside

    override val transports : $[$[Transport]] = $($(RuledMove), $(Waterway))

    val services : $[Service] = $(BuyCard, Riverboats, Mercenaries(this))
}

case object SF extends Trader {
    val name = "Squirrel Firm"
    override def funName = NameReference(name, this) ~ " Firm"
    val short = "SF"
    val style = "sf"
    val priority = "G''"

    def abilities(options : $[Meta.O]) = $(Boycott, Protectionism, Surplus, FundsDefense, TradepostProgressiveVP)

    val warrior = Squirrel

    def recruitIn(implicit game : Game) : $[Clearing] = game.board.inner

    override val transports : $[$[Transport]] = $($(RuledMove))

    val services : $[Service] = $(BuyCard, Balloon(this), TalentScout(this), Outsourcing(this), Shares(this), Peacekeepers(this))

    override def note : Elem = SpacedDash ~ "unfinished " ~ "RF".styled(RF) ~ " variant"
}

case object Boycott extends Effect
case object TradepostProgressiveVP extends Effect
case object TradepostHalvesFunds extends Effect
case object Protectionism extends Effect
case object Dividents extends Effect
case object Surplus extends Effect
case object FundsDefense extends Effect


case class TradePost(suit : BaseSuit) extends Token  {
    override def id = "trade-post"
    override val name = "Trade Post"
    override def imgid(f : Faction) = f.style + "-" + id + "-" + suit.name
}

case object Otter extends Warrior
case object Squirrel extends Warrior

trait Fund {
    def textfund = this @@ {
        case NoFund(f) => ???
        case f : WarriorFaction => f.warrior.name.styled(f)
    }
    def imgwd = this @@ {
        case NoFund(f) => f.warrior.imged(f)
        case f : WarriorFaction => f.warrior.imgd(f)
    }
    def imgw = this @@ {
        case NoFund(f) => ???
        case f : WarriorFaction => f.warrior.img(f)
    }
}

case class NoFund(f : WarriorFaction) extends Fund

trait ByWater extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = allows(f) && o @@ {
        case o : Clearing => game.riverside.has(o)
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = allows(f, o) && (o, d) @@ {
        case (o : Clearing, d : Clearing) => game.byRiver(o).has(d)
        case _ => false
    }
}

case object Swimmers extends ByWater with Effect {
    override def allows(f : Faction)(implicit game : Game) = f.has(Swimmers)

    override def img = Image("Swimmers", styles.widepiece)
}

case object Waterway extends ByWater {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = super.allows(f, o, d) && (o, d) @@ {
        case (o : Clearing, d : Clearing) => f.rules(o).not && f.rules(d).not
        case _ => false
    }
}

case object Riverboat extends ByWater {
    override def img = Image("Riverboat", styles.widepiece)

    override def allows(f : Faction)(implicit game : Game) = f.has(Riverboats) || f.has(BoatBuilders)
}


trait Service extends Record {
    def name : String
    def id : String
    def index : Int
    def img(f : Trader) = Image(f.style + "-service-" + id)
}

object Service {
    implicit val ordering : Ordering[Service] = Ordering.by(_.index)
}

trait Purchase extends FactionEffect {
    def img(f : Trader) : Image
}

case class Offer(seller : Trader, purchase : Purchase, cost : Int, available : Boolean) extends Record


case object BuyCard extends Service {
    val id = "cards"
    val index = 1
    override val name = "Buy Card"
}

case class BuyCardN(d : DeckCard) extends Purchase {
    def img(f : Trader) = d.img
    val name = d.name
}


case object Riverboats extends Service with Purchase with DisplayEffect {
    val id = "boats"
    val index = 2
    val name = "Riverboats"
}

case class Mercenaries(f : Trader) extends Service with Purchase with DisplayEffect {
    val id = "mercs"
    val index = 3
    val name = "Mercenaries"
}

case class Balloon(f : Trader) extends Service with Purchase with DisplayEffect {
    val id = "balloon"
    val index = 4
    val name = "Balloon"
}

case object Balloon extends Transport with Elementary {
    override def img = Image("Balloon", styles.warrior)

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Clearing, d : Clearing) => true
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && ({
        l.num <= 3
    })

    def elem = "Balloon".hl
}

case class Shares(f : Trader) extends Service with Purchase with DisplayEffect {
    val id = "shares"
    val index = 5
    val name = "Shares"
}

case class TalentScout(f : Trader) extends Service with Purchase with DisplayEffect {
    val id = "talent-scout"
    val index = 6
    val name = "Talent Scout"
}

case class Peacekeepers(f : Trader) extends Service with Purchase with DisplayEffect {
    val id = "peacekeepers"
    val index = 7
    val name = "Peacekeepers"
}

case class Outsourcing(f : Trader) extends Service with Purchase with DisplayEffect {
    val id = "outsourcing"
    val index = 8
    val name = "Outsourcing"
}


class TraderPlayer(val faction : Trader)(implicit val game : Game) extends FactionState {
    val payments = location(Trader.Payments, _.piece.is[Warrior])
    val funds    = location(Trader.Funds   , _.piece.is[Warrior])
    val commited = location(Trader.Commited, _.piece.is[Warrior])

    def ffunds = funds.$./(_.faction).of[WarriorFaction]

    val gone = location(Trader.Gone, u => u.piece.is[TradePost] && u.faction == faction)

    var prices : Map[Service, Int] = faction.services./(_ -> 0).toMap

    var limit = 0

    var current : $[WarriorFaction] = $

    def pricelist(e : Faction) =
        faction.services./~(s => s @@ {
            case BuyCard => hand./(d => Offer(faction, BuyCardN(d), prices(s), true))
            case Riverboats => $(Offer(faction, Riverboats, prices(s), e.has(Riverboats).not && e.has(Swimmers).not))
            case Mercenaries(_) => $(Offer(faction, Mercenaries(faction), prices(s), e.is[Hero].not))
            case Shares(_) => $(Offer(faction, Shares(faction), prices(s), faction.vp > 0))
            case TalentScout(_) => $(Offer(faction, TalentScout(faction), prices(s), e.is[Hero].not && tradeposts.any))
            case Balloon(_) => $(Offer(faction, Balloon(faction), prices(s), tradeposts.intersect(e.movable).any))
            case Outsourcing(_) => $(Offer(faction, Outsourcing(faction), prices(s), tradeposts.%(e.present).any))
            case Peacekeepers(_) => $(Offer(faction, Peacekeepers(faction), prices(s), all(faction.warrior).any))

            case s : Purchase => $(Offer(faction, s, prices(s), false))
        })

    def craft = FoxRabbitMouse./~(s => (gone.count(TradePost(s)) + all(TradePost(s)).num).times(s))

    def procure(l : $[Fund]) : $[Figure] = {
        val m = l.distinct./~(o => funds.$.%(_.faction == o).take(l.count(o)))

        if (m.num != l.num)
            throw new Error("unaccounted commit funds: " + l)

         m
    }

    def commit(l : $[Fund]) {
        val m = procure(l)

        m.foreach { z => z --> commited }

        faction.logtemp("commited", m./(_.elem).comma)
    }

    def spend(l : $[Fund]) {
        val m = procure(l)

        m --> game.recycle

        faction.logtemp("returned", m./(_.elem).comma)
    }

    def tradeposts = (all(TradePost(Fox)) ++ all(TradePost(Rabbit)) ++ all(TradePost(Mouse)))
}


case class LeaveFunds(n : Int) extends ForfeitMessage {
    def elem(implicit game : Game) = "Leave " ~ n.hl ~ " funds?"
    def real = n > 0
}

case class WithFunds(zz : $[Fund]) extends Message {
    def elem(implicit game : Game) = (dt.Swap ~ zz./(_.imgwd)).spn(xlo.nowrap)
}

case class WithFundsText(zz : $[Fund]) extends Message {
    def elem(implicit game : Game) = ("(commited " ~ zz./(_.textfund).comma ~ ")").spn(xlo.nowrap)
}

case class ReturnFundsText(zz : $[Fund]) extends Message {
    def elem(implicit game : Game) = ("(returned " ~ zz./(_.textfund).comma ~ ")").spn(xlo.nowrap)
}


trait ViewService extends ViewObject[Service] { self : UserAction =>
    def s : Service
    def obj = s
}


case class UdjustPricesMainAction(f : Trader, l : $[Service], values : $[Int], m : SortedMap[Service, Int], then : ForcedAction) extends ForcedAction with Soft with SelfPerform {
    def perform(soft : Void)(implicit game : Game) = {
        Ask(f)(f.services./(s => UdjustServicePriceAction(f, l, values, m, then, s))
            ++ $(UdjustPricesAction(f, l, m, then).x(m.values.%(_ == 0).any))
            ++ $(UdjustPricesExplodeAction(f, l, values, then)))
    }
}

case class UdjustServicePriceAction(self : Trader, l : $[Service], values : $[Int], m : SortedMap[Service, Int], then : ForcedAction, s : Service) extends BaseAction("Adjust prices")(s.img(self)(styles.card), s.name.hl) with Soft with ViewService with Selectable with NoClear with NoExplode with SelfPerform with LimitedExtra[Int] with ExtraInt with ElemWrap {
    def wrap(g : Game)(e : Elem) = Div(values./ { x =>
        val selected = m(s) == x
        val p = self.style + "-price-" + x + selected.??("-selected")
        OnClick(x, Image(p, styles.action, xlo.pointer)(selected.?(styles.selected)))
    }.merge ~ Break ~ e, styles.inline, styles.margined)

    def update(p : Int) = this.copy(m = m + (s -> p))

    def selected = m(s) > 0

    def perform(soft : Void)(implicit game : Game) = UdjustPricesMainAction(self, l, values, m, then)
}

case class UdjustPricesExplodeAction(f : Trader, l : $[Service], values : $[Int], then : ForcedAction) extends HiddenChoice with SelfExplode {
    def explode(withSoft : Boolean) = {
        val mm = l.foldLeft($(SortedMap[Service, Int]()))((l, s) => l./~(m => (0 :: values)./(n => m + (s -> n))))

        mm.%(_.values.%(_ == 0).none)./(m => UdjustPricesAction(f, l, m, then)) ++ withSoft.??(mm./~(m => l./(s => UdjustServicePriceAction(f, l, values, m, then, s))))
    }
}

case class UdjustPricesAction(self : Trader, l : $[Service], m : Map[Service, Int], then : ForcedAction) extends BaseAction(None)("Done")

case class BuyServicesFromAction(self : Faction, f : Trader) extends ForcedAction
case class BuyServicesAction(self : Faction, f : Trader, l : $[Offer]) extends ForcedAction with Soft
case class GetServicesAction(self : Faction, f : Trader, l : $[Offer]) extends ForcedAction

case class BuyServicesMainAction(self : Faction, f : Trader) extends BaseAction("Birdsong")("Buy", f, "services", "(max", implicit g => f.limit.hl ~ ")")

case class PayForServicesAction(self : Faction, f : Trader, b : $[Offer], then : ForcedAction) extends ForcedAction with Soft
case class PayGiveWarriorsAction(self : WarriorFaction, f : Trader, n : Int, l : $[Warrior], then : ForcedAction) extends BaseAction()()
case class PayExhaustItemsAction(self : Hero, f : Trader, n : Int, l : $[Item], then : ForcedAction) extends ForcedAction


trait RFDaylightQuestion extends FactionAction {
    override def self : Trader
    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem
}

case class TraderMainCurrencyAction(self : Trader, currency : $[WarriorFaction]) extends BaseAction()() with DontRecord
case class TraderMainAction(self : Trader, currency : $[WarriorFaction]) extends ForcedAction with Soft

case class TraderCurrencyAction(self : Trader, oldc : $[WarriorFaction], ll : $[$[WarriorFaction]], c : $[WarriorFaction]) extends OptionAction() with RFDaylightQuestion with Soft with NoClear with HalfExplode with SelfPerform with LimitedExtra[$[WarriorFaction]] with ElemWrap {
    def wrap(g : Game)(e : Elem) = {
        ll./(l => l./(f => f.warrior.img(f)).merge.div(styles.fundbox)((l == c).?(styles.selected)).onClick.param((l != c).??(l)))
    }.merge.div(xstyles.xx)(xlo.fullwidth)

    def update(p : $[WarriorFaction]) = this.copy(c = p)

    def values = $ +: ll

    def fromAny(s : Any) = s @@ {
        case l : $[WarriorFaction] => Some(l)
        case _ => None
    }

    def perform(soft : Void)(implicit game : Game) = Ask(self)(TraderMainCurrencyAction(self, c))

    def expand(target : |[Action]) = values./(TraderMainCurrencyAction(self, _)) ++ (c == oldc).?(values./(TraderCurrencyAction(self, oldc, ll, _))).|($(DoAction(TraderMainAction(self, c))))
}


case class TraderAttackAction(self : Trader, l : $[Clearing], zz : $[Fund]) extends OptionAction(dt.Swap ~ zz./(_.imgwd), dt.Arrow, "Battle".styled(self), dt.Battle) with RFDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class TraderMoveAction(self : Trader, l : $[Clearing], zz : $[Fund]) extends OptionAction(dt.Swap ~ zz./(_.imgwd), dt.Arrow, "Move".styled(self), dt.Move) with RFDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class TraderRecruitAction(self : Trader, l : $[Clearing], zz : $[Fund]) extends OptionAction(dt.Remove ~ zz./(_.imgwd), dt.Arrow, "Recruit".styled(self), self.imgwd) with RFDaylightQuestion with Soft with Only[TraderRecruitClearingAction] { def tag = implicitly }
case class TraderRecruitClearingAction(self : Trader, c : Clearing, zz : $[Fund]) extends BaseAction(dt.Remove ~ zz./(_.imgwd), dt.Arrow, "Recruit".styled(self), self.imgwd, "in")(c)
case class TraderDrawAction(self : Trader, zz : $[Fund]) extends OptionAction(dt.Swap ~ zz./(_.imgwd), dt.Arrow, "Draw Card", dt.CardBack) with RFDaylightQuestion
case class TraderEstablishTradePostAction(self : Trader, l : $[Clearing], s : $[BaseSuit], zz : $[Fund]) extends OptionAction(dt.Remove ~ zz./(_.imgwd), dt.Arrow, "Trade Post".styled(self), s./(TradePost)./(_.imgd(self))) with RFDaylightQuestion with Soft
case class TraderEstablishTradePostClearingAction(self : Trader, c : Clearing, s : BaseSuit, zz : $[Fund]) extends BaseAction(dt.Remove ~ zz./(_.imgwd), dt.Arrow, "Trade Post".styled(self), "in")(c, implicit g => TradePost(s).imgd(self))
case class TraderExportMainAction(self : Trader) extends OptionAction("Export".styled(self)) with RFDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }

case class ExportAction(self : Trader, d : CraftCard, m : Message) extends ForcedAction

case class CommitFundsAction(self : Trader, zz : $[Fund], then : ForcedAction) extends ForcedAction

case class DestroyTradePostAction(f : Trader, c : Clearing, s : BaseSuit, then : ForcedAction) extends ForcedAction

case class DiscardLostFundsAction(self : Trader, zz : $[Fund], then : ForcedAction) extends BaseAction("Discard lost funds")(dt.Remove ~ zz./(_.imgw))


case class BalloonMainAction(self : Faction, s : Trader, l : $[Clearing], then : ForcedAction) extends BaseAction(None)(Balloon(s)) with Soft
case class BalloonAction(self : Faction, s : Trader) extends ForcedAction

case class OutsourcingAction(self : Faction, s : Trader, c : Clearing, x : BaseSuit) extends BaseAction(Outsourcing(s))(dt.CraftSuit(x), TradePost(x).of(s), TradePost(x).imgd(s), "in", c)

case class TalentScoutMainAction(self : WarriorFaction, s : Trader, l : $[Clearing], then : ForcedAction) extends BaseAction(None)(TalentScout(s)) with Soft
case class TalentScoutAction(self : WarriorFaction, s : Trader, c : Clearing, p : Piece, then : ForcedAction) extends BaseAction("Recruit", p.of(self), "with", TalentScout(s), "in")(c)


object TraderExpansion extends FactionExpansion[Trader] {

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        f.as[WarriorFaction].foreach { f =>
            traders.foreach { s =>
                if (f.can(TalentScout(s))) {
                    val cc = s.tradeposts.%(f.canPlace)
                    + TalentScoutMainAction(f, s, cc, Repeat).x(cc.none, "no clearings").x(f.pool(f.warrior).not, "no warriors")
                }
            }
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Trader) =>
            game.states += f -> new TraderPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Trader) =>
            if (f.all(f.warrior).num < 4) {
                Ask(f)(f.recruitIn(game).%(f.canPlace)./(c => PlacePieceAction(f, c, f.warrior, FactionSetupAction(f))))
            }
            else {
                f.reserve --> 3.times(f.warrior) --> f.payments
                f.log("gained", 3.hl, "funds")
                UdjustPricesMainAction(f, f.services, $(1, 2, 3, 4), f.prices.to(SortedMap), SetupFactionsAction)
            }

        // HELPER
        case MoveListAction(self, f, t, m, from, to, l, CommitFundsAction(ff, zz, then)) =>
            ff.commit(zz)

            Force(MoveListAction(self, f, t, m, from, to, l, then))

        case BattleStartAction(self, f, a, m, c, o, i, CommitFundsAction(ff, zz, then)) =>
            ff.commit(zz)

            BattleStartAction(self, f, a, m, c, o, i, then)

        case LegalAAAConveneAction(f, h, c, e, CommitFundsAction(ff, zz, then)) =>
            ff.commit(zz)

            LegalAAAConveneAction(f, h, c, e, then)

        case BattlePostHitInAction(b, e, f : Trader, Otter, then) =>
            e.log("poisoned", Otter.of(f))

            then

        case BattlePostHitInAction(b, e, f : Trader, Squirrel, then) =>
            e.log("pinched", Squirrel.of(f))

            then

        case BattlePostHitInAction(b, e, f : Trader, TradePost(s), then) =>
            e.log("pillaged", TradePost(s).of(f))

            then

        case ForcedRemoveTargetEffectAction(e, c, f : Trader, TradePost(s), then) =>
            DestroyTradePostAction(f, c, s, then)

        case DestroyTradePostAction(f, c, s, then) =>
            f.limbo(c) --> TradePost(s) --> f.gone

            if (f.has(TradepostHalvesFunds) && f.ffunds.any) {
                val a = f.ffunds
                val n = a.num /↑ 2

                val c = a.combinations(n).$

                Ask(f)(c./(zz => DiscardLostFundsAction(f, zz, then)))
            }
            else
                then

        case DiscardLostFundsAction(f, zz, then) =>
            f.spend(zz)

            f.log("lost", zz./(_.textfund).comma)

            then

        // TURN
        case BirdsongNAction(40, f : Trader) =>
            if (f.payments.$.none) {
                if (f.has(Boycott)) {
                    f.oscore(-factions.%!(f.friends).num)("due to", "Boycott".hl)
                }

                if (f.has(Protectionism)) {
                    if (f.pool(f.warrior)) {
                        val n = min(2, f.pooled(f.warrior))
                        f.reserve --> n.times(f.warrior) --> f.payments
                        f.log("got", n.hl, "payments due to", "Protectionism".styled(f))
                    }
                    else
                        f.log("got", "no", "payments due to", "Protectionism".styled(f))
                }
            }

            if (f.has(Dividents))
                f.oscore(f.tradeposts.any.??(f.funds.$.num /↓ 2))("as", "Dividents".styled(f))

            if (f.has(Surplus))
                f.oscore(min(f.tradeposts.num, f.funds.$.num))("from", "Surplus".styled(f))

            f.payments --> f.funds
            f.commited --> f.funds

            f.log("gathered", f.funds.$.num.hl, "funds")

            Next

        case BirdsongNAction(60, f : Trader) =>
            Ask(f).birdsong(f).done(Next)

        case DaylightNAction(50, f : Trader) =>
            soft()

            f.current = f.ffunds.intersect(f.current)

            if (f.current.none)
                if (f.ffunds.any)
                    f.current = f.ffunds.groupBy(x => x).values.$.maxBy(_.num)

            TraderMainAction(f, f.current)

        case TraderMainCurrencyAction(f, currency) =>
            f.current = currency

            TraderMainAction(f, currency)

        case TraderMainAction(f, currency) if f.funds.$.num == 0 =>
            Ask(f)(Next.as("End Turn".hl)).daylight(f)

        case TraderMainAction(f, currency) =>
            implicit val ask = builder

            var cc = currency ++ $(NoFund(f), NoFund(f), NoFund(f), NoFund(f))

            + TraderCurrencyAction(f, f.current, f.ffunds.groupBy(x => x).values.$, currency)

            val att = clearings.%(f.canAttackIn)
            + TraderAttackAction(f, att, cc.take(1)).x(currency.num < 1).x(att.none)

            val mvv = f.moveFrom.of[Clearing]
            + TraderMoveAction(f, mvv, cc.take(1)).x(currency.num < 1).x(mvv.none)

            + TraderRecruitAction(f, f.recruitIn(game).%(f.canPlace), cc.take(1)).x(currency.num < 1).x(f.pool(f.warrior).not && cc.take(1).has(f).not && f.totalWar.not, "maximum")

            val spec = f.funds
            val l = clearings.%(f.canPlace).%(c => c.suits.exists(s => f.pool(TradePost(s)))).diff(f.tradeposts).%(c => factions.%(_.rules(c)).%(f => currency.%(_.faction == f).num > 1).any)
            + TraderEstablishTradePostAction(f, l, l./~(_.suits.%(s => f.pool(TradePost(s)))).distinct, cc.take(2)).x(currency.num < 2).x(l.none)

            + TraderDrawAction(f, cc.take(1)).x(currency.num < 1).x(deck.none && pile.none)

            val c = f.hand.%(f.craftable)
            + NiceCraftMainAction(f, f.craft ++ f.frogCraft ++ f.extraCraft, f.crafted, Empty).x(f.hand.none).x(c.none, "nothing craftable")
            + TraderExportMainAction(f).x(c.none).x(f.pool(f.warrior).not, "maximum")

            + EndTurnSoftAction(f, "Turn", LeaveFunds(f.funds.$.num))

            ask(f).daylight(f)

        case TraderAttackAction(f, l, zz) =>
            BattleInitAction(f, f, AltInLog(WithFunds(zz), WithFundsText(zz)), l, $(CancelAction), CommitFundsAction(f, zz, Repeat))

        case TraderMoveAction(f, l, zz) =>
            MoveInitAction(f, f, $, AltInLog(WithFunds(zz), WithFundsText(zz)), l, f.movable, $(CancelAction), CommitFundsAction(f, zz, Repeat))

        case TraderRecruitAction(f, l, zz) =>
            Ask(f)(l./(TraderRecruitClearingAction(f, _, zz))).cancel

        case TraderRecruitClearingAction(f, c, zz) =>
            game.highlights :+= PlaceHighlight($(c))

            f.spend(zz)

            if (f.pool(f.warrior)) {
                f.reserve --> f.warrior --> c

                f.log("recruited", f.warrior.of(f), "in", c, ReturnFundsText(zz))
            }
            else
            if (f.totalWar)
                f.nscore(1)("recruiting")(f, ScoredVP, "recruiting", ReturnFundsText(zz))

            Repeat

        case TraderDrawAction(f, zz) =>
            f.commit(zz)

            DrawCardsAction(f, 1, AltInLog(OnTurn(game.turn), WithFundsText(zz)), AddCardsAction(f, Repeat))

        case TraderEstablishTradePostAction(f, l, _, zz) =>
            Ask(f).some(l)(c => c.suits.%(s => f.pool(TradePost(s)))./(s => TraderEstablishTradePostClearingAction(f, c, s, zz))).cancel

        case TraderEstablishTradePostClearingAction(f, c, s, zz) =>
            game.highlights :+= PlaceHighlight($(c))

            f.spend(zz)

            if (f.pool(f.warrior))
                f.reserve --> f.warrior --> c

            f.reserve --> TradePost(s) --> c

            f.nscore(f.has(TradepostProgressiveVP).?(f.all(TradePost(s)).num).|(2))("establishing", "Trade Post".styled(f))(f, "established", "Trade Post".styled(f), "in", c, ForVP, ReturnFundsText(zz))

            Repeat

        case CraftMenuAction(f : Trader) =>
            val l = f.craft
            val u = f.crafted
            val s = f.current.intersect(f.ffunds) ++ f.ffunds.diff(f.current)

            def fix(l : $[Fund]) = f.ffunds.intersect(l)

            YYSelectObjectsAction(f, f.hand)
                .withGroup("Craft".styled(f) ~ " with " ~ l.diff(u)./(dt.CraftSuit).merge ~ u.diff(l).any.?(" " ~ "-".hl ~ " " ~ u.diff(l)./(dt.CraftSuit).merge))
                .withRule(f.craftable)
                .withThens(c => {
                    val d = c.asInstanceOf[CraftCard]
                    Info("Craft", d, "with", d.cost./(dt.CraftSuit).merge) +: s.combinations(d.cost.num).$./(zz => CraftAction(f, d, WithFundsText(fix(zz)), CommitFundsAction(f, fix(zz), CraftPerformAction(f, d, WithFundsText(fix(zz))))).as((dt.Swap ~ zz./(_.imgw)).div(styles.card)))
                })
                .withExtra($(NoHand, CancelAction))

        case TraderExportMainAction(f) =>
            val l = f.craft
            val u = f.crafted
            val s = f.current.intersect(f.ffunds) ++ f.ffunds.diff(f.current)

            def fix(l : $[Fund]) = f.ffunds.intersect(l)

            YYSelectObjectsAction(f, f.hand)
                .withGroup("Export".styled(f) ~ " with " ~ l.diff(u)./(dt.CraftSuit).merge ~ u.diff(l).any.?(" " ~ "-".hl ~ " " ~ u.diff(l)./(dt.CraftSuit).merge))
                .withRule(f.craftable)
                .withThens(c => {
                    val d = c.asInstanceOf[CraftCard]
                    Info("Export", d, "with", d.cost./(dt.CraftSuit).merge) +: s.combinations(d.cost.num).$./(zz => CraftAction(f, d, WithFundsText(fix(zz)), CommitFundsAction(f, fix(zz), ExportAction(f, d, WithFundsText(fix(zz))))).as((dt.Swap ~ zz./(_.imgw)).div(styles.card)))
                })
                .withExtra($(NoHand, CancelAction))

        case CommitFundsAction(f, zz, then) =>
            f.commit(zz)

            then

        case ExportAction(f, d, m) =>
            f.hand --> d --> discard.quiet

            f.reserve --> f.warrior --> options.has(ExportToCommitments).?(f.commited).|(f.payments)

            f.log("exported", d, m)

            Repeat

        case NightStartAction(f : Trader) =>
            EveningHandLimitAction(f)

        case EveningEndAction(f : Trader) =>
            UdjustPricesMainAction(f, f.services, $(1, 2, 3, 4), f.prices.to(SortedMap), CleanUpAction(f))

        case UdjustPricesAction(f, l, m, then) =>
            f.prices = m

            l.foreach(s => f.log("set price for", s.name.styled(f), "to", m(s).hl))

            then

        case FactionCleanUpAction(f : Trader) =>
            f.crafted = $
            f.current = $

            CleanUpAction(f)

        // BIRDSONG SERVICES
        case BirdsongNAction(20, f) =>
            factions.but(f).of[Trader].foreach { s =>
                s.limit = 1 + s.tradeposts.%(f.present).num
            }

            Next

        case BirdsongNAction(22, f) =>
            val sellers = factions.but(f).of[Trader].%(_.limit > 0)

            if (sellers.num > 1)
                Ask(f).each(sellers)(BuyServicesMainAction(f, _)).done(Next)
            else
            if (sellers.any)
                BuyServicesFromAction(f, sellers(0))
            else
                Next

        case BirdsongNAction(24, f) =>
            val actions = traders./~(x =>
                f.can(Outsourcing(x)).?? {
                    val cc = x.tradeposts
                    cc./~(c => FoxRabbitMouse.%(s => f.at(c).has(TradePost(s)))./(s => OutsourcingAction(f, x, c, s).!(f.at(c).none)))
                })

            if (actions.any)
                Ask(f)(actions ++ f.expose).bail(Next)
            else
                Next

        case OutsourcingAction(f, s, c, x) =>
            f.used :+= Outsourcing(s)
            f.extraCraft :+= x

            f.log("outsourced to", s, "in", c)

            Repeat

        case BirdsongNAction(26, f) =>
            val ss = traders.%(s => f.can(Balloon(s)))
            val sss = ss.%(_.tradeposts.intersect(f.movable).any)

            val actions = ss./{ s =>
                if (sss.has(s)) {
                    val cc = s.tradeposts
                    val l = f.movable

                    val extra = if (sss.num == 1)
                        Next.as("Skip", Balloon) :: f.expose
                    else
                        $(CancelAction)

                    MoveInitAction(f, f, f.transports./(_.but(RuledMove)) ** $($(Balloon)), OnBalloon, l.intersect(cc), cc, extra, BalloonAction(f, s)).as(Balloon(s))
                }
                else
                    MoveInitAction(f, f, $, OnBalloon, $, $, $, BalloonAction(f, s)).as(Balloon(s)).x(true, "no units at a tradepost")
            }

            if (sss.num > 1)
                Ask(f)(actions)(f.expose)(Next.as("Skip", Balloon))
            else
            if (sss.num == 1)
                Ask(f)(actions)
            else
                Next

        case BalloonAction(f, s) =>
            f.used :+= Balloon(s)

            Repeat

        case TalentScoutMainAction(f, s, l, then) =>
            Ask(f)(l./(c => TalentScoutAction(f, s, c, f.warrior, then))).cancel

        case TalentScoutAction(f, s, c, p, then) =>
            f.used :+= TalentScout(s)
            f.reserve --> p --> c
            f.log("recruited", p.of(f), "in", c, "with", TalentScout(s))
            then

        case BuyServicesMainAction(f, s) =>
            BuyServicesFromAction(f, s)

        case BuyServicesFromAction(f, s) =>
            val n = f @@ {
                case f : WarriorFaction => f.pooled(f.warrior)
                case f : Hero => f.inv.ready.num
            }

            val c = f @@ {
                case f : WarriorFaction => f.warrior.imgd(f)
                case f : Hero => dt.ItemAny
            }

            implicit val conv = (o : Offer) => o.purchase @@ {
                case BuyCardN(d : DeckCard) => d.img ~ Gap ~ Gap ~ Gap
                case p : Service => p.img(s)(styles.card) ~ p.name.styled(s) ~ Break
            } ~ o.cost.times(c)

            XXSelectObjectsAction(f, s.pricelist(f))
                .withGroup("Buy " ~ s.elem ~ " service" ~ (s.limit > 1).??("s"))
                .withRule(_.upTo(s.limit).each(_.available).all(_./(_.cost).sum <= n))
                .withSplit($(s.hand.num))
                .withThen(BuyServicesAction(f, s, _))(l => "Buy".hl ~ l.some./(l => " " ~ l./(_.purchase.of(s)).comma ~ (" for " ~ l./(_.cost).sum.times(c)).styled(xlo.flexhcenter)))
                .withExtra($(Next.as("Don't buy, " ~ s.warrior.name ~ "s cry")))

        case BuyServicesAction(f, s, l) =>
            PayForServicesAction(f, s, l, GetServicesAction(f, s, l))

        case GetServicesAction(f, s, l) =>
            l./(_.purchase).foreach {
                case BuyCardN(d : DeckCard) =>
                    s.hand --> d --> f.hand
                    f.log("bought", d, "from", s)

                case p @ Riverboats =>
                    f.services :+= p
                    f.log("rented", p.of(s), "from", s)

                case p : Mercenaries =>
                    f.services :+= p
                    f.log("hired", p.of(s), "from", s)

                case p : Peacekeepers =>
                    f.services :+= p
                    f.log("hired", p.of(s), "from", s)

                case p : TalentScout =>
                    f.services :+= p
                    f.log("got help of", p.of(s), "from", s)

                case p : Shares =>
                    s.oscore(-1)("selling", "Shares".styled(s))
                    f.oscore(1)("buying", "Shares".styled(s))

                case p : Balloon =>
                    f.services :+= p
                    f.log("rented a", p.of(s), "from", s)

                case p : Outsourcing =>
                    f.services :+= p

                case p =>
                    f.services :+= p
                    f.log("got some", p.of(s), "from", s)
            }

            l./(_.purchase).foreach {
                case p : Outsourcing =>
                    f.logtemp("outsourced to", s)

                case p =>
            }

            s.limit -= l.num

            s.limit = 0

            Repeat

        case PayForServicesAction(f : WarriorFaction, s, l, then) =>
            val n = l./(_.cost).sum
            Ask(f)(PayGiveWarriorsAction(f, s, n, n.times(f.warrior), then))

        case PayForServicesAction(f : Hero, s, l, then) =>
            val n = l./(_.cost).sum

            SelectHeroItemsAction(f, "Exhaust " ~ n.hl ~ " items to get " ~ l./(_.purchase.of(s)).comma, $(CancelAction))(_.num(n))(ToExhaust.apply)(l => PayExhaustItemsAction(f, s, n, l./(_.item), then))

        case PayGiveWarriorsAction(f, s, n, l, then) =>
            f.reserve --> l --> s.payments

            f.log("paid", l./(_.of(f)).comma)

            then


        case PayExhaustItemsAction(f, s, n, l, then) =>
            s.reserve --?> l.num.times(s.warrior) --> s.payments

            f.use(l, "commited")

            then

        case _ => UnknownContinue
    }

}
