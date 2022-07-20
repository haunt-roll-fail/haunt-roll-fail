package root

import root.gaming._

import colmat._

import hrf.elem._
import root.elem._

import scala.collection.immutable.SortedMap


trait Trader extends WarriorFaction {
    val expansion = TraderExpansion

    val services : List[Service]
    
    def recruitIn(g : Game) : List[Clearing]

    lazy val pieces = warrior ** 15 ++ TradePost(Fox) ** 3 ++ TradePost(Rabbit) ** 3 ++ TradePost(Mouse) ** 3

    def advertising = TradePost(Fox).img(this) ~ TradePost(Rabbit).img(this) ~ TradePost(Mouse).img(this)
    def motto = "Trade".styled(this)
}

case object RF extends Trader {
    val name = "Riverfolk Company"
    override def funName = NameReference(name, this) ~ " Company"
    val short = "RF"
    val style = "rf"
    val priority = "G"

    val abilities = $(Swimmers, Protectionism, Dividents, TradepostHalvesFunds)

    val warrior = Otter
    
    def recruitIn(g : Game) : List[Clearing] = g.board.riverside

    override val transports : List[List[Transport]] = $($(RuledMove), $(Waterway))
    
    val services : List[Service] = $(BuyCard, Riverboats, Mercenaries(this))
}

case object SF extends Trader {
    val name = "Squirrel Firm"
    override def funName = NameReference(name, this) ~ " Firm"
    val short = "SF"
    val style = "sf"
    val priority = "G'"
    
    val abilities = $(Boycott, Protectionism, Surplus, FundsDefense, TradepostProgressiveVP)
    
    val warrior = Squirrel
    
    def recruitIn(g : Game) : List[Clearing] = g.board.inner

    override val transports : List[List[Transport]] = $($(RuledMove))
    
    val services : List[Service] = $(BuyCard, Balloon(this), TalentScout(this), Outsourcing(this), Shares(this), Peacekeepers(this))
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
    override def name = "Trade Post"
    override def imgid(f : Faction) = f.short + "-" + id + "-" + suit.name
}

case object Otter extends Warrior
case object Squirrel extends Warrior

trait Fund {
    def textfund = this @@ {
        case AnyFund => ???
        case NoFund(f) => ???
        case f : WarriorFaction => f.warrior.name.styled(f)
    }
    def imgwd = this @@ {
        case AnyFund => ???
        case NoFund(f) => f.warrior.imged(f)
        case f : WarriorFaction => f.warrior.imgd(f)
    }
    def imgw = this @@ {
        case AnyFund => ???
        case NoFund(f) => ???
        case f : WarriorFaction => f.warrior.img(f)
    }
}

case object AnyFund extends Fund

case class NoFund(f : WarriorFaction) extends Fund

trait ByWater extends Transport {
    override def allows(g : Game, f : Faction, o : Region) = allows(g, f) && o @@ {
        case o : Clearing => g.board.riverside.has(o)
        case _ => false
    }

    override def allows(g : Game, f : Faction, o : Region, d : Region) = allows(g, f, o) && (o, d) @@ {
        case (o : Clearing, d : Clearing) => g.board.byRiver(o).has(d)
        case _ => false
    }
}

case object Swimmers extends ByWater with Effect {
    override def allows(g : Game, f : Faction) = g.of(f).has(Swimmers)

    override def img = Image("Swimmers", styles.widepiece)
}

case object Waterway extends ByWater {
    override def allows(g : Game, f : Faction, o : Region, d : Region) = super.allows(g, f, o, d) && (o, d) @@ {
        case (o : Clearing, d : Clearing) => g.rule(f)(o).not && g.rule(f)(d).not
        case _ => false
    }
}

case object Riverboat extends ByWater {
    override def img = Image("Riverboat", styles.widepiece)
    
    override def allows(g : Game, f : Faction) = g.of(f).has(Riverboats) || g.of(f).has(BoatBuilders)
}


trait Service extends Record {
    def name : String
    def id : String
    def index : Int
    def img(f : Trader) = Image(f.short + "-service-" + id)
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
    override def name = "Buy Card"
}

case class BuyCardN(d : DeckCard) extends Purchase {
    def img(f : Trader) = d.img
    override def elem = d.elem
}   


case object Riverboats extends Service with Purchase {
    val id = "boats"
    val index = 2
    override def elem = name.styled(RF)
}

case class Mercenaries(f : Trader) extends Service with Purchase {
    val id = "mercs"
    val index = 3
    override def name = "Mercenaries"
    override def elem = name.styled(f)
}   

case class Balloon(f : Trader) extends Service with Purchase {
    val id = "balloon"
    val index = 4
    override def name = "Balloon"
    override def elem = name.styled(f)
}   

case object Balloon extends Transport with Elementary {
    override def img = Image("Balloon", styles.warrior)

    override def allows(g : Game, f : Faction, o : Region, d : Region) = (o, d) @@ {
        case (o : Clearing, d : Clearing) => true
        case _ => false
    }

    override def allows(g : Game, f : Faction, o : Region, d : Region, l : List[Movable]) = allows(g, f, o, d) && ({
        l.num <= 3
    })

    def elem = "Balloon".hl
}

case class Shares(f : Trader) extends Service with Purchase {
    val id = "shares"
    val index = 5
    override def name = "Shares"
    override def elem = name.styled(f)
}   

case class TalentScout(f : Trader) extends Service with Purchase {
    val id = "talent-scout"
    val index = 6
    override def name = "Talent Scout"
    override def elem = name.styled(f)
}   

case class Peacekeepers(f : Trader) extends Service with Purchase {
    val id = "peacekeepers"
    val index = 7
    override def name = "Peacekeepers"
    override def elem = name.styled(f)
}   

case class Outsourcing(f : Trader) extends Service with Purchase {
    val id = "outsourcing"
    val index = 8
    override def name = "Outsourcing"
    override def elem = name.styled(f)
}   



class TraderPlayer(val game : Game, val faction : Trader) extends PlayerState {
    import game._

    var prices : Map[Service, Int] = faction.services./(_ -> (0 + hrf.HRF.flag("fastsetup").??(2))).toMap

    var limit = 0

    var current : List[WarriorFaction] = Nil
    
    val payments = figures("payments", Nil, _.piece.warrior.any)
    val funds = figures("funds", Nil, _.piece.warrior.any)
    def ffunds = funds./(_.faction.asInstanceOf[WarriorFaction])
    val commited = figures("commited", Nil, _.piece.warrior.any)
    
    val graveyard = figures("graveyard", Nil, _.piece match { case TradePost(_) => true; case _ => false })
    
    def pricelist(e : Faction) =
        faction.services./~(s => s @@ {
            case BuyCard => hand./(d => Offer(faction, BuyCardN(d), prices(s), true))
            case Riverboats => $(Offer(faction, Riverboats, prices(s), e.has(Riverboats).not && e.has(Swimmers).not))
            case Mercenaries(_) => $(Offer(faction, Mercenaries(faction), prices(s), e.hero.any.not))
            case Shares(_) => $(Offer(faction, Shares(faction), prices(s), faction.vp > 0))
            case TalentScout(_) => $(Offer(faction, TalentScout(faction), prices(s), e.hero.any.not && tradeposts.any))
            case Balloon(_) => $(Offer(faction, Balloon(faction), prices(s), tradeposts.intersect(movable(e)).any))
            case Outsourcing(_) => $(Offer(faction, Outsourcing(faction), prices(s), tradeposts.%(t => e.at(t).any).any))
            case Peacekeepers(_) => $(Offer(faction, Peacekeepers(faction), prices(s), all(faction.warrior).any))

            case s : Purchase => $(Offer(faction, s, prices(s), false))
        })
    
    def craft = faction.figures.diff(pool)./(_.piece)./~ {
        case TradePost(s) => Some(s)
        case _ => None
    }

    def commit(l : List[Fund]) = {
        val m = l.distinct./~ { o =>
            funds.%(_.faction == o).take(l.count(o))
        }

        if (m.num != l.num)
            throw new Error("unaccounted commit funds: " + l)

        m.foreach { z => z --> commited }

        faction.logtemp("commited", m./(_.elem).join(", "))
    }

    def spend(l : List[Fund]) = {
        val m = l.distinct./~ { o =>
            funds.%(_.faction == o).take(l.count(o))
        }

        if (m.num != l.num)
            throw new Error("unaccounted spend funds: " + l)

        m.foreach { z => z --> z.faction.pool }

        faction.logtemp("returned", m./(_.elem).join(", "))
    }

    def tradeposts = (all(TradePost(Fox)) ++ all(TradePost(Rabbit)) ++ all(TradePost(Mouse)))
}

case class LeaveFunds(n : Int) extends Message {
    def elem(g : Game) = "Leave " ~ n.hl ~ " funds?"
}

case class WithFunds(zz : List[Fund]) extends Message {
    def elem(g : Game) = (dt.Swap ~ zz./(_.imgwd)).spn(xlo.nowrap)
}

case class WithFundsText(zz : List[Fund]) extends Message {
    def elem(g : Game) = ("(commited " ~ zz./(_.textfund).join(", ") ~ ")").spn(xlo.nowrap)
}

case class ReturnFundsText(zz : List[Fund]) extends Message {
    def elem(g : Game) = ("(returned " ~ zz./(_.textfund).join(", ") ~ ")").spn(xlo.nowrap)
}


trait ViewService extends ViewObject[Service] { self : UserAction => 
    def s : Service
    def obj = s
}


case class UdjustPricesMainAction(f : Trader, l : List[Service], values : List[Int], m : SortedMap[Service, Int], then : ForcedAction) extends ForcedAction with Soft with SelfPerform {
    def perform(g : Game) = {
        Ask(f, f.services./(s => UdjustServicePriceAction(f, l, values, m, then, s))
            ++ $(UdjustPricesAction(f, l, m, then).x(m.values.%(_ == 0).any))
            ++ $(UdjustPricesExplodeAction(f, l, values, then)))
    }
}

case class UdjustServicePriceAction(self : Trader, l : List[Service], values : List[Int], m : SortedMap[Service, Int], then : ForcedAction, s : Service) extends BaseAction("Adjust prices")(s.img(self)(styles.card), s.name.hl) with Soft with ViewService with Selectable with NoClear with NoExplode with SelfPerform with LimitedExtra[Int] with ExtraInt with ElemWrap {
    def wrap(g : Game)(e : Elem) = Div(values./ { x =>
        val selected = m(s) == x
        val p = self.style + "-price-" + x + selected.??("-selected")
        OnClick(x, Image(p, styles.action, xstyles.pointer)(selected.?(styles.selected)))
    }.merge ~ Break ~ e, styles.inline, styles.margined)
    
    def update(p : Int) = this.copy(m = m + (s -> p))

    def selected = m(s) > 0
    
    def perform(g : Game) = UdjustPricesMainAction(self, l, values, m, then)
}
    
case class UdjustPricesExplodeAction(f : Trader, l : List[Service], values : List[Int], then : ForcedAction) extends HiddenChoice with SelfExplode {
    def explode(withSoft : Boolean) = {
        val mm = l.foldLeft(List(SortedMap[Service, Int]()))((l, s) => l./~(m => (0 :: values)./(n => m + (s -> n))))
        
        mm.%(_.values.%(_ == 0).none)./(m => UdjustPricesAction(f, l, m, then)) ++ withSoft.??(mm./~(m => l./(s => UdjustServicePriceAction(f, l, values, m, then, s))))
    }
}

case class UdjustPricesAction(self : Trader, l : List[Service], m : Map[Service, Int], then : ForcedAction) extends BaseAction(None)("Done")


case class BuyServicesFromAction(self : Faction, f : Trader) extends ForcedAction
case class BuyServicesAction(self : Faction, f : Trader, l : List[Offer]) extends ForcedAction with Soft
case class GetServicesAction(self : Faction, f : Trader, l : List[Offer]) extends ForcedAction

case class BuyServicesMainAction(self : Faction, f : Trader) extends BaseAction("Birdsong")("Buy", f, "services", "(max", g => g.rf(f).limit.hl ~ ")")

case class PayForServicesAction(self : Faction, f : Trader, b : List[Offer], then : ForcedAction) extends ForcedAction with Soft
case class PayGiveWarriorsAction(self : WarriorFaction, f : Trader, n : Int, l : List[Warrior], then : ForcedAction) extends BaseAction()()
case class PayExhaustItemsAction(self : Hero, f : Trader, n : Int, l : List[Item], then : ForcedAction) extends ForcedAction


trait RFDaylightQuestion extends FactionAction {
    override def self : Trader
    def question(g : Game) = self.elem ~ " (" ~ "Daylight".styled(styles.phase) ~ ")"
}

case class OtterMainCurrencyAction(self : Trader, currency : List[WarriorFaction]) extends BaseAction()() with DontRecord
case class OtterMainAction(self : Trader, currency : List[WarriorFaction]) extends ForcedAction with Soft

case class OtterCurrencyAction(self : Trader, oldc : List[WarriorFaction], ll : List[List[WarriorFaction]], c : List[WarriorFaction]) extends OptionAction() with RFDaylightQuestion with Soft with NoClear with HalfExplode with SelfPerform with LimitedExtra[List[WarriorFaction]] with ElemWrap {
    def wrap(g : Game)(e : Elem) = {
        ll./(l => l./(f => f.warrior.img(f)).merge.div(styles.fundbox)((l == c).?(styles.selected)).onClick((l != c).??(l)))
    }.merge.div(xstyles.xx)(xlo.fullwidth)
    
    def update(p : List[WarriorFaction]) = this.copy(c = p)
    
    def values = Nil +: ll
    
    def fromAny(s : Any) = s @@ {
        case l : List[WarriorFaction] => Some(l)
        case _ => None
    }
    
    def perform(g : Game) = Ask(self)(OtterMainCurrencyAction(self, c))
    
    def expand(withSoft : Boolean) = values./(OtterMainCurrencyAction(self, _)) ++ (c == oldc).?(values./(OtterCurrencyAction(self, oldc, ll, _))).|($(DoAction(OtterMainAction(self, c))))
}


case class OtterAttackAction(self : Trader, l : List[Clearing], zz : List[Fund]) extends OptionAction(dt.Swap ~ zz./(_.imgwd), dt.Arrow, "Battle".styled(self), dt.Battle) with RFDaylightQuestion with Soft
case class OtterMoveAction(self : Trader, l : List[Clearing], zz : List[Fund]) extends OptionAction(dt.Swap ~ zz./(_.imgwd), dt.Arrow, "Move".styled(self)) with RFDaylightQuestion with Soft
case class OtterRecruitAction(self : Trader, l : List[Clearing], zz : List[Fund]) extends OptionAction(dt.Remove ~ zz./(_.imgwd), dt.Arrow, "Recruit".styled(self), self.imgwd) with RFDaylightQuestion with Soft
case class OtterRecruitClearingAction(self : Trader, c : Clearing, zz : List[Fund]) extends BaseAction(dt.Remove ~ zz./(_.imgwd), dt.Arrow, "Recruit".styled(self), self.imgwd, "in")(c)
case class OtterDrawAction(self : Trader, zz : List[Fund]) extends OptionAction(dt.Swap ~ zz./(_.imgwd), dt.Arrow, "Draw Card", dt.CardBack) with RFDaylightQuestion
case class OtterEstablishTradePostAction(self : Trader, l : List[Clearing], s : List[BaseSuit], zz : List[Fund]) extends OptionAction(dt.Remove ~ zz./(_.imgwd), dt.Arrow, "Trade Post".styled(self), s./(TradePost)./(_.imgd(self))) with RFDaylightQuestion with Soft
case class OtterEstablishTradePostClearingAction(self : Trader, c : Clearing, zz : List[Fund]) extends BaseAction(dt.Remove ~ zz./(_.imgwd), dt.Arrow, "Trade Post".styled(self), "in")(c, g => g.mapping.get(c)./(TradePost)./(_.imgd(self)).|(Empty))
case class OtterExportMainAction(self : Trader) extends OptionAction("Export".styled(self)) with RFDaylightQuestion with Soft

case class ExportAction(self : Trader, d : CraftCard, m : Message) extends ForcedAction

case class CommitFundsAction(self : Trader, zz : List[Fund], then : ForcedAction) extends ForcedAction

case class DestroyTradePostAction(f : Trader, c : Clearing, then : ForcedAction) extends ForcedAction

case class DiscardLostFundsAction(self : Trader, zz : List[Fund], then : ForcedAction) extends BaseAction("Discard lost funds")(dt.Remove ~ zz./(_.imgw))


case class BalloonMainAction(self : Faction, s : Trader, l : List[Clearing], then : ForcedAction) extends BaseAction(None)(Balloon(s)) with Soft
case class BalloonAction(self : Faction, s : Trader) extends ForcedAction

case class OutsourcingAction(self : Faction, s : Trader, c : Clearing, x : BaseSuit) extends BaseAction(Outsourcing(s))(dt.CraftSuit(x), TradePost(x).of(s), TradePost(x).imgd(s), "in", c)



object TraderExpansion extends Expansion {
    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            // SETUP
            case CreatePlayerAction(f : Trader) =>
                pstates += f -> new TraderPlayer(game, f)
                FactionInitAction(f)
                
            case FactionSetupAction(f : Trader) =>
                if (f.all(f.warrior).num < 4) {
                    if (hrf.HRF.flag("fastsetup"))
                        Ask(f, f.recruitIn(game).%(canPlace(f)).take(1)./(PlaceStartingWarriorAction(f, _, f.warrior)))
                    else
                        Ask(f, f.recruitIn(game).%(canPlace(f))./(PlaceStartingWarriorAction(f, _, f.warrior)))
                }
                else {
                    rf(f).pool.sub(3, f.warrior) --> f.payments
                    f.log("gained", 3.hl, "funds")
                    UdjustPricesMainAction(f, f.services, $(1, 2, 3, 4), f.prices.to(SortedMap), SetupNextAction)
                }
                
            case PlaceStartingWarriorAction(f : Trader, r, p) =>
                f.pool.one(p) --> r
    
                f.log("placed", p.of(f), "in", r)
                
                FactionSetupAction(f)

            // HELPER
            case MoveListAction(f, t, m, from, to, l, CommitFundsAction(ff, zz, then)) =>
                ff.commit(zz)
                
                Force(MoveListAction(f, t, m, from, to, l, then))
            
            case BattleStartAction(f, a, m, c, o, i, CommitFundsAction(ff, zz, then)) =>
                ff.commit(zz)

                Force(BattleStartAction(f, a, m, c, o, i, then))

            case BattlePostHitInAction(b, e, f : Trader, Otter, then) =>
                e.log("poisoned", Otter.of(f))

                then

            case BattlePostHitInAction(b, e, f : Trader, Squirrel, then) =>
                e.log("pinched", Squirrel.of(f))

                then

            case BattlePostHitInAction(b, e, f : Trader, TradePost(s), then) =>
                e.log("pillaged", TradePost(s).of(f))

                then

            case ForcedRemoveEffectAction(e, c, f : Trader, TradePost(_), then) =>
                DestroyTradePostAction(f, c, then)
                
            case DestroyTradePostAction(f, c, then) =>
                f.limbo.one(TradePost(c.suit)) --> f.graveyard
            
                if (f.has(TradepostHalvesFunds) && f.ffunds.any) {
                    val a = f.ffunds
                    val t = a.num
                    val n = t - t / 2
                
                    val c = a.combinations(n).toList
                    
                    Ask(f, c./(zz => DiscardLostFundsAction(f, zz, then)))
                }
                else
                    then
                
            case DiscardLostFundsAction(f, zz, then) =>
                f.spend(zz)
                
                f.log("lost", zz./(_.textfund).join(", "))

                then


            // TURN    
            case BirdsongNAction(40, f : Trader) =>
                if (rf(f).payments.none) {
                    if (f.has(Boycott)) {
                        f.oscore(-factions.%!(f.friends).num)("due to", "Boycott".hl)
                    }

                    if (f.has(Protectionism)) {
                        f.pool.sub(2, f.warrior) --> rf(f).payments
                        f.log("got", 2.hl, "payments due to", "Protectionism".styled(f))
                    }
                }
                
                if (f.has(Dividents))
                    f.oscore(f.tradeposts.any.??(f.funds.num / 2))("as", "Dividents".styled(f))
                
                if (f.has(Surplus))
                    f.oscore(min(f.tradeposts.num, f.funds.num))("from", "Surplus".styled(f))

                f.payments.get --> f.funds
                f.commited.get --> f.funds
                
                f.log("gathered", f.funds.num.hl, "funds")

                Next

            case BirdsongNAction(60, f : Trader) =>
                Ask(f, Nil.birdsong(f).done(Next))
                
            case DaylightNAction(50, f : Trader) =>
                f.current = f.ffunds.intersect(f.current)

                if (f.current.none)
                    if (f.ffunds.any)
                        f.current = f.ffunds.groupBy(x => x).values.toList.maxBy(_.num)

                OtterMainAction(f, f.current)

            case OtterMainCurrencyAction(f, currency) =>
                f.current = currency
                
                OtterMainAction(f, currency)

            case OtterMainAction(f, currency) =>
                var actions : List[UserAction] = Nil
                
                var cc = currency ++ $(NoFund(f), NoFund(f), NoFund(f), NoFund(f))

                if (f.funds.num > 0) {
                    actions :+= OtterCurrencyAction(f, f.current, f.ffunds.groupBy(x => x).values.toList, currency)
                
                    val att = clearings.%(c => attack(f)(c).any)
                    actions :+= OtterAttackAction(f, att, cc.take(1)).x(currency.num < 1).x(att.none)
                        
                    val mvv = moveFrom(f)
                    actions :+= OtterMoveAction(f, mvv, cc.take(1)).x(currency.num < 1).x(mvv.none)

                    actions :+= OtterRecruitAction(f, f.recruitIn(game).%(canPlace(f)), cc.take(1)).x(currency.num < 1).x(f.pooled(f.warrior) == 0 && cc.take(1).has(f).not && f.totalWar.not, "maximum")
                        
                    val spec = f.funds
                    val l = clearings.%(canPlace(f)).%(c => f.pool./(_.piece).contains(TradePost(c.suit))).diff(f.tradeposts).%(c => factions.%(rule(_)(c)).%(f => currency.%(_.faction == f).num > 1).any)
                    actions :+= OtterEstablishTradePostAction(f, l, l./(_.suit).distinct, cc.take(2)).x(currency.num < 2).x(l.none)
                    
                    actions :+= OtterDrawAction(f, cc.take(1)).x(currency.num < 1).x(deck.none && pile.none)
                        
                    val c = f.hand.%(craftable(f))
                    actions :+= NiceCraftMainAction(f, f.craft ++ f.extracraft, f.crafted).x(f.hand.none).x(c.none, "nothing craftable")
                    actions :+= OtterExportMainAction(f).x(c.none).x(f.pooled(f.warrior) == 0, "maximum")
                }
                
                if (f.funds.none)
                    actions :+= EndTurnAction(f)
                else
                    actions :+= EndTurnSoftAction(f, LeaveFunds(f.funds.num))
                
                Ask(f, actions.daylight(f))
                
            case OtterAttackAction(f, l, zz) =>
                BattleInitAction(f, AltInLog(WithFunds(zz), WithFundsText(zz)), l, $(CancelAction), CommitFundsAction(f, zz, Repeat))
                
            case OtterMoveAction(f, l, zz) =>
                MoveInitAction(f, Nil, AltInLog(WithFunds(zz), WithFundsText(zz)), l, movable(f), $(CancelAction), CommitFundsAction(f, zz, Repeat))
                
            case OtterRecruitAction(f, l, zz) =>
                Ask(f, l./(OtterRecruitClearingAction(f, _, zz)).cancel)
    
            case OtterRecruitClearingAction(f, c, zz) =>
                highlights :+= PlaceHighlight($(c))

                f.spend(zz)

                if (f.pooled(f.warrior) > 0) {
                    f.pool.one(f.warrior) --> c
        
                    f.log("recruited", f.warrior.of(f), "in", c, ReturnFundsText(zz))
                }
                else
                if (f.totalWar)
                    f.nscore(1)("recruiting")(f, ScoredVP, "recruiting", ReturnFundsText(zz))
                
                Repeat

            case OtterDrawAction(f, zz) =>
                f.commit(zz)
                
                DrawCardsAction(f, 1, AltInLog(OnTurn(turn), WithFundsText(zz)), AddCardsAction(f, Repeat))
                
            case OtterEstablishTradePostAction(f, l, _, zz) =>
                Ask(f, l./(OtterEstablishTradePostClearingAction(f, _, zz)).cancel)
    
            case OtterEstablishTradePostClearingAction(f, c, zz) =>
                highlights :+= PlaceHighlight(List(c))

                f.spend(zz)

                if (f.pooled(f.warrior) > 0)
                    f.pool.one(f.warrior) --> c

                f.pool.one(TradePost(c.suit)) --> c
                
                f.nscore(f.has(TradepostProgressiveVP).?(f.all(TradePost(c.suit)).num).|(2))("establishing", "Trade Post".styled(f))(f, "established", "Trade Post".styled(f), "in", c, ForVP, ReturnFundsText(zz))

                Repeat

            case CraftMenuAction(f : Trader) =>
                val l = f.craft
                val u = f.crafted
                val s = f.current.intersect(f.ffunds) ++ f.ffunds.diff(f.current)

                YYSelectObjectsAction(f, f.hand)
                    .withGroup("Craft".styled(f) ~ " with " ~ l.diff(u)./(dt.CraftSuit).merge ~ u.diff(l).any.??(" " ~ "-".hl ~ " " ~ u.diff(l)./(dt.CraftSuit).merge))
                    .withRule(craftable(f))
                    .withThens(c => { 
                        val d = c.asInstanceOf[CraftCard]
                        Info("Craft", d, "with", d.cost./(dt.CraftSuit).merge) +: s.combinations(d.cost.num).toList./(zz => CraftAction(f, d, WithFundsText(zz), CommitFundsAction(f, zz, CraftPerformAction(f, d, WithFundsText(zz)))).as((dt.Swap ~ zz./(_.imgw)).div(styles.card)))
                    })
                    .withExtra($(NoHand, CancelAction))
                    
            case OtterExportMainAction(f) =>
                val l = f.craft
                val u = f.crafted
                val s = f.current.intersect(f.ffunds) ++ f.ffunds.diff(f.current)

                YYSelectObjectsAction(f, f.hand)
                    .withGroup("Export".styled(f) ~ " with " ~ l.diff(u)./(dt.CraftSuit).merge ~ u.diff(l).any.??(" " ~ "-".hl ~ " " ~ u.diff(l)./(dt.CraftSuit).merge))
                    .withRule(craftable(f))
                    .withThens(c => { 
                        val d = c.asInstanceOf[CraftCard]
                        Info("Export", d, "with", d.cost./(dt.CraftSuit).merge) +: s.combinations(d.cost.num).toList./(zz => CraftAction(f, d, WithFundsText(zz), CommitFundsAction(f, zz, ExportAction(f, d, WithFundsText(zz)))).as((dt.Swap ~ zz./(_.imgw)).div(styles.card)))
                    })
                    .withExtra($(NoHand, CancelAction))
                    
            case CommitFundsAction(f, zz, then) =>
                f.commit(zz)

                then
            
            case ExportAction(f, d, m) =>
                f.hand --> d --> discard
                f.pool.one(f.warrior) --> f.payments
                
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
                f.crafted = Nil
                f.current = Nil

                EndPlayerTurnAction(f)
                
            // BIRDSONG SERVICES
            case BirdsongNAction(20, f) =>
                factions.but(f)./~(_.trader).foreach { s =>
                    s.limit = 1 + s.tradeposts.%(f.at(_).any).num
                }

                Next

            case BirdsongNAction(22, f) =>
                val sellers = factions.but(f)./~(_.trader).%(_.limit > 0)
                
                if (sellers.num > 1)
                    Ask(f, sellers./(BuyServicesMainAction(f, _)).done(Next))
                else
                if (sellers.any)
                    BuyServicesFromAction(f, sellers(0))
                else
                    Next

            case BirdsongNAction(24, f) =>
                val actions = traders./~(s =>
                    f.can(Outsourcing(s)).?? {
                        val cc = s.tradeposts
                        cc./(c => OutsourcingAction(f, s, c, c.suit).x(f.at(c).none))
                    })
                
                if (actions.any)
                    Ask(f, actions ++ f.expose).bail(Next)
                else
                    Next
                    
            case OutsourcingAction(f, s, c, x) =>
                f.used :+= Outsourcing(s)
                f.extracraft :+= x

                f.log("outsourced to", s, "in", c)

                Repeat

            case BirdsongNAction(26, f) =>
                val ss = traders.%(s => f.can(Balloon(s)))
                val sss = ss.%(_.tradeposts.intersect(movable(f)).any)
                
                val actions = ss./{ s =>
                    if (sss.has(s)) {
                        val cc = s.tradeposts
                        val l = movable(f)

                        val extra = if (sss.num == 1)                        
                            Next.as("Skip", Balloon) :: f.expose
                        else
                            $(CancelAction)

                        MoveInitAction(f, f.transports./(_.but(RuledMove)) ** $($(Balloon)), OnBalloon, l.intersect(cc), cc, extra, BalloonAction(f, s)).as(Balloon(s))
                    }
                    else
                        MoveInitAction(f, Nil, OnBalloon, Nil, Nil, Nil, BalloonAction(f, s)).as(Balloon(s)).x(true, "no units at a tradepost")
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
                } ~ c.repeat(o.cost)

                XXSelectObjectsAction(f, s.pricelist(f))
                    .withGroup("Buy " ~ s.elem ~ " service" ~ (s.limit > 1).??("s"))
                    .withRule(_.upTo(s.limit).each(_.available).all(_./(_.cost).sum <= n))
                    .withSplit($(s.hand.num))
                    .withThen(BuyServicesAction(f, s, _))(l => "Buy".hl ~ l.some./(l => " " ~ l./(_.purchase.elem).join(", ") ~ (" for " ~ c.repeat(l./(_.cost).sum)).styled(xlo.flexhcenter)))
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
                        f.log("rented", p, "from", s)

                    case p : Mercenaries =>
                        f.services :+= p
                        f.log("hired", p, "from", s)
                        
                    case p : Peacekeepers =>
                        f.services :+= p
                        f.log("hired", p, "from", s)
                        
                    case p : TalentScout =>
                        f.services :+= p
                        f.log("got help of", p, "from", s)
                        
                    case p : Shares =>
                        s.oscore(-1)("selling", "Shares".styled(s))
                        f.oscore(1)("buying", "Shares".styled(s))

                    case p : Balloon =>
                        f.services :+= p
                        f.log("rented a", p, "from", s)

                    case p : Shares =>
                        f.services :+= p
                        f.log("got help of", p, "from", s)
 
                    case p : Outsourcing =>
                        f.services :+= p

                    case p =>
                        f.services :+= p
                        f.log("got some", p, "from", s)
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
                Ask(f)(PayGiveWarriorsAction(f, s, n, f.warrior.repeat(n), then))

            case PayForServicesAction(f : Hero, s, l, then) =>
                val n = l./(_.cost).sum

                SelectHeroItemsAction(f, "Exhaust " ~ n.hl ~ " items to get " ~ l./(_.purchase.elem).join(", "), $(CancelAction))(_.num(n))(ToExhaust.apply)(l => PayExhaustItemsAction(f, s, n, l./(_.item), then))
    
            case PayGiveWarriorsAction(f, s, n, l, then) =>
                f.pool.sublist(l) --> s.payments

                f.log("paid", l./(_.of(f)).comma)

                then
                

            case PayExhaustItemsAction(f, s, n, l, then) =>
                s.pool.sub(l.num, s.warrior) --> s.payments
                    
                f.use(l, "commited")
    
                then

            case _ => UnknownContinue
        }
    }
}

