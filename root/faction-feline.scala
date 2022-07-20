package root

import root.gaming._

import colmat._

import hrf.elem._
import root.elem._

trait Feline extends WarriorFaction {
    val expansion = FelineExpansion
    
    val warrior = Cat

    val abilities = $(MassRecriut, FieldHospitals, DaylightCraft)

    val pieces = Cat ** 25 ++ Keep ** 1 ++ Sawmill ** 6 ++ Workshop ** 6 ++ Recruiter ** 6 ++ Wood ** 8
 
    def cost(b : Building) = $(999, 4, 3, 3, 2, 1, 0)

    def reward(b : Building) = b match {
        case Sawmill   => $(999, 5, 4, 3, 2, 1, 0)
        case Workshop  => $(999, 5, 4, 3, 2, 2, 0)
        case Recruiter => $(999, 4, 3, 3, 2, 1, 0)
    }

    def bonus(b : Building) : List[Option[FelineBonus]] = b match {
        case Sawmill   => $(None, None, None, None, None, None, None)
        case Workshop  => $(None, None, None, None, None, None, None)
        case Recruiter => $(None, None, Some(BonusDraw), None, Some(BonusDraw), None, None)
    }

    def basic : List[FelineBonus] = List(BonusBattle, BonusDraw, BonusMove, BonusMove)
}

case object Keep extends Token
case object Wood extends Token
case object Sawmill extends Building
case object Workshop extends Building
case object Recruiter extends Building
case object Cat extends Warrior

case object MassRecriut extends FactionEffect
case object FieldHospitals extends FactionEffect
case object MassOverwork extends FactionEffect

case object MC extends Feline {
    val name = "Marquise de Cat"
    override def funName = NameReference(name, this) ~ " " ~ Span("de Cat", styles.block)
    val short = "MC"
    val style = "mc"
    val priority = "A"

    def advertising = Keep.img(this) ~ Sawmill.img(this) ~ Workshop.img(this) ~ Recruiter.img(this)
    def motto = "Build".styled(this)
}

case object BK extends Feline {
    val name = "Workshop Marquise" // "Baron von Katze"
    override def funName = NameReference(name, this) ~ " von Katze"
    val short = "BK"
    val style = "bk"
    val priority = "A'"

    override val abilities = $(MassRecriut, FieldHospitals, MassOverwork, EveningCraft)

    override def reward(b : Building) = b match {
        case Sawmill   => $(999, 5, 4, 3, 2, 1, 0)
        case Workshop  => $(999, 4, 3, 3, 2, 1, 0)
        case Recruiter => $(999, 4, 3, 3, 2, 1, 0)
    }

    override def bonus(b : Building) = b match {
        case Sawmill   => $(None, None, None, None, None, None, None)
        case Workshop  => $(None, Some(BonusHit), Some(BonusBattle), Some(BonusDraw), Some(BonusMove), Some(BonusBattle), Some(BonusMove))
        case Recruiter => $(None, None, Some(BonusDraw), None, Some(BonusDraw), None, None)
    }

    override def basic = $(BonusBattle, BonusDraw, BonusMove)
    
    def advertising = Keep.img(this) ~ Workshop.img(this) ~ Workshop.img(this) ~ Workshop.img(this)
    def motto = "Build".styled(this)
}

trait FelineBonus extends FactionEffect
case object BonusDraw extends FelineBonus
case object BonusMove extends FelineBonus
case object BonusBattle extends FelineBonus
case object BonusHit extends FelineBonus {
    override def name = "Paw Crossbow"
}

class FelinePlayer(val game : Game, val faction : Feline) extends PlayerState {
    var acted = 0
    var extra = 0
    val hospitals = game.FoxRabbitMouse./(s => s -> figures("hospital-" + s, Nil, _.piece == Cat)).toMap
    
    def craft = all(Workshop)./(game.mapping)
    
    def bonuses(b : FelineBonus) = (faction.basic./(b => Some(b)) ++ 
        faction.bonus(Sawmill).drop(pooled(Sawmill) + 1) ++ 
        faction.bonus(Workshop).drop(pooled(Workshop) + 1) ++ 
        faction.bonus(Recruiter).drop(pooled(Recruiter) + 1)
    ).%(_ == Some(b)).num
}

case class Heals(f : Feline, n : Int) extends Message {
    def elem(g : Game) = "to heal " ~ (f.warrior.img(f).repeat(n)).merge
}

case object GetExtra extends Message {
    def elem(g : Game) = Text("to get an extra action")
}

case class OverworksIn(c : Clearing) extends Message {
    def elem(g : Game) = "to overwork in " ~ c.elem(g)
}




trait MCDaylightQuestion extends FactionAction {
    override def self : Feline

    def question(g : Game) = self.elem ~ " (" ~ "Daylight".styled(styles.phase) ~ ")" ~ Break ~
        Div( 
            1.to(3 + g.mc(self).extra)./(_ => Image("action-black", styles.action)).take(g.mc(self).acted) ~ 
            (1.to(3)./(_ => Image(self.style + "-action", styles.action)) ++ (0.until(g.mc(self).extra)./(_ => Image("action-bird", styles.action)))).drop(g.mc(self).acted),
        styles.margined)
}

case class StartingBuildingsAction(f : Feline) extends ForcedAction
case class StartingBuildingMainAction(self : Feline, l : List[Clearing], p : Building) extends BaseAction(self, "starting buildings")(p.of(self), p.imgd(self)) with Soft
case class StartingBuildingAction(self : Feline, c : Clearing, p : Building) extends BaseAction(self, "places", p.of(self), p.imgd(self), "in")(c)

case class FieldHospitalsMainAction(f : Feline, s : BaseSuit, then : ForcedAction) extends ForcedAction
case class FieldHospitalsAction(f : Feline, s : BaseSuit, then : ForcedAction) extends ForcedAction
case class FieldHospitalsIgnoreAction(f : Feline, s : BaseSuit, then : ForcedAction) extends ForcedAction

case class ProduceWoodAction(f : Faction) extends ForcedAction
case class ProduceMultiWoodAction(self : Faction, l : List[Clearing]) extends BaseAction("Place", Wood.of(self), "in")(l)

case class ExtraMainAction(self : Feline, n : Int) extends OptionAction("Extra action".hl, dt.CardSuit(Bird).repeat(n)) with MCDaylightQuestion with Soft
case class ExtraAction(f : Feline) extends ForcedAction

case class CatDoneAction(f : Feline, then : ForcedAction) extends ForcedAction

case class WageWarMainAction(self : Feline, total : Int) extends OptionAction((total > 1).?("Wage War".styled(self), dt.Battle.repeat(total).merge).|("Battle".styled(self), dt.Battle)) with MCDaylightQuestion with Soft

case class RecruitCatsMainAction(self : Feline, n : Int) extends OptionAction("Recruit".styled(self), self.warrior.imgd(self).repeat(n).merge) with MCDaylightQuestion with Soft
case class RecruitCatsAction(self : Feline, l : List[Clearing], r : List[Clearing]) extends BaseAction("Recruit in")(l)

case class MarchMainAction(self : Feline, total : Int) extends OptionAction((total > 1).?("March".styled(self), dt.Move.repeat(total).merge).|("Move".styled(self), dt.Move)) with MCDaylightQuestion with Soft



case class BuildMainAction(self : Feline, b : Building, cost : Int, vp : Int, bonus : String, l : List[List[Clearing]]) extends OptionAction("Build", b.of(self), (cost < 999).??((cost > 0).?(Wood.imgd(self).repeat(cost).merge).|(Text("free")), dt.Arrow, b.imgd(self), vp.vp, (bonus != "").??("|", bonus))) with MCDaylightQuestion with Soft
case class BuildAction(self : Feline, b : Building, cost : Int, vp : Int, c : Clearing, l : List[Clearing]) extends BaseAction("Build", b.of(self), "in")(c) with Soft
case class BuildPayAction(self : Feline, b : Building, vp : Int, c : Clearing, l : List[Clearing]) extends BaseAction("Build", b.of(self), "in", c, "with", Wood.of(self), "from")(l)

case class OverworkMainAction(self : Feline, l : List[Clearing], s : List[Suit]) extends OptionAction("Overwork".styled(self), dt.CardSuit(s.single.|(AnySuit)), dt.Arrow, Wood.imgd(self)) with MCDaylightQuestion with Soft
case class OverworkAction(self : Feline, l : List[Clearing], z : List[Clearing]) extends ForcedAction with Soft
case class OverworkDiscardAction(self : Feline, c : Clearing, l : List[Clearing], z : List[Clearing]) extends BaseAction("Overwork in")(c) with Soft
case class OverworkWoodAction(self : Feline, c : Clearing, l : List[Clearing], z : List[Clearing]) extends ForcedAction

object FelineExpansion extends Expansion {
    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            case CreatePlayerAction(f : Feline) =>
                pstates += f -> new FelinePlayer(game, f)
                FactionInitAction(f)
                
            // SETUP
            case FactionSetupAction(f : Feline) if options.has(SetupTypeHomelands) =>
                val clusters = {
                    var b = board.clearings.diff(homelands).%(canBuild(f))./(c => $(c))
                    var a = b.take(0)
                    
                    while (a.num > b.num || a./(_.num).sum < b./(_.num).sum) {
                        a = b
                        b = b./(l => (l ++ l./~(board.connected).distinct.diff(homelands).%(canBuild(f))).distinct.sortBy(_.name)).distinct
                    }
    
                    b
                }
                
                val l = clusters.%(_.num >= 3)./~(x => x)

                Ask(f)(StartingBuildingMainAction(f, l, Sawmill))(StartingBuildingMainAction(f, l, Workshop))(StartingBuildingMainAction(f, l, Recruiter))
                
            case StartingBuildingMainAction(f : Feline, l, p) =>
                Ask(f)(l./(StartingBuildingAction(f, _, p))).cancel
                
            case StartingBuildingAction(f : Feline, c, p) =>
                homelands :+= c

                factions.but(f).foreach { e =>
                    e.keep.foreach { k =>
                        e.at(c) --> k
                    }
                }
    
                if (game.options.has(AdSetBuffOn)) {
                    f.pool.one(p) --> c
                    f.pool.one(Cat) --> c

                    f.log("placed", p.of(f), "and", Cat.of(f), "in", c)
                }
                else {
                    f.pool.one(p) --> c

                    f.log("placed", p.of(f), "in", c)
                }
                
                StartingBuildingsAction(f)
                
            case StartingBuildingsAction(f) =>
                val s = f.all(Sawmill)
                val w = f.all(Workshop) 
                val r = f.all(Recruiter)

                if (s.none || w.none || r.none) {
                    val l = (s ++ w ++ r)./~(board.connected).diff(homelands).%(canBuild(f))
                
                    Ask(f)(s.none.?(StartingBuildingMainAction(f, l, Sawmill)))(w.none.?(StartingBuildingMainAction(f, l, Workshop)))(r.none.?(StartingBuildingMainAction(f, l, Recruiter)))
                }
                else {
                    val l = s ++ w ++ r
                    val ll = l.diff(homelands.diff(l)./~(board.connected)).some.|(l)

                    Ask(f)(ll./(c => StartingClearingAction(f, c).as(c)(f, "places", Keep.of(f), Keep.imgd(f), "in"))).needOk
                }
                    
            case StartingClearingAction(f : Feline, c) if options.has(SetupTypeHomelands) =>
                f.pool.one(Keep) --> c
                
                f.log("placed", Keep.of(f), "in", c)

                factions.but(f).foreach { e =>
                    e.keep.foreach { k =>
                        e.at(c) --> k
                    }
                }

                val l = (game.options.has(AdSetBuffOn) || options.has(SetupTypeHomelands)).?(clearings).|(clearings.but(board.opposite(c)))
                
                l.foreach { x =>
                    val o = factions.but(f).%(_.keep.any)./~(_.at(x))
                    if (o.building.any || o.token.any)
                        f.pool.one(Cat) --> c
                    else
                        f.pool.one(Cat) --> x
                }

                SetupNextAction

            case FactionSetupAction(f : Feline) =>
                StartingCornerAction(f)

            case StartingClearingAction(f : Feline, r) =>
                f.pool.one(Keep) --> r
                
                factions.but(f).foreach { e =>
                    e.keep.foreach { k =>
                        e.at(r) --> k
                    }
                }

                val l = game.options.has(AdSetBuffOn).?(clearings).|(clearings.but(board.opposite(r)))

                l.foreach { c =>
                    val o = factions.but(f).%(_.keep.any)./~(_.at(c))
                    if (o.building.any || o.token.any)
                        f.pool.one(Cat) --> r
                    else
                        f.pool.one(Cat) --> c
                }

                f.log("started in", r)
         
                StartingBuildingsNextAction(f, r, Sawmill)
            
            case StartingBuildingsNextAction(f : Feline, k, p) =>
                Ask(f, (k +: board.connected(k)).%(canBuild(f))./(PlaceStartingBuildingAction(f, k, _, p)))
                
            case PlaceStartingBuildingAction(f : Feline, k, r, p) =>
                f.pool.one(p) --> r
    
                factions.but(f).foreach { e =>
                    e.keep.foreach { k =>
                        e.at(r) --> k
                    }
                }

                f.log("placed", p.of(f), "in", r)
                
                if (game.options.has(AdSetBuffOn))
                    f.pool.one(Cat) --> r

                p match {
                    case Sawmill => StartingBuildingsNextAction(f, k, Workshop)
                    case Workshop => StartingBuildingsNextAction(f, k, Recruiter)
                    case Recruiter => SetupNextAction
                }
    
            // HELPER
            case MoveListAction(f, t, m, from, to, l, CatDoneAction(ff, then)) =>
                CatDoneAction(ff, ForceAction(MoveListAction(f, t, m, from, to, l, then)))
            
            case MoveListAction(f, t, m, from, to, l, MarchAction(ff, i, n, CatDoneAction(fff, then))) =>
                CatDoneAction(fff, ForceAction(MoveListAction(f, t, m, from, to, l, MarchAction(ff, i, n, then))))
            
            case BattleStartAction(f, a, m, c, o, i, CatDoneAction(ff, then)) =>
                CatDoneAction(ff, ForceAction(BattleStartAction(f, a, m, c, o, i, then)))

            case BattlePostHitInAction(b, e : Feline, f : Feline, p : Scoring, then) if e.pooled(p) > 0 =>
                e.pool.one(p) --> b.clearing
                e.log("captured", p.of(f))
                then

            case BattlePostHitInAction(b, e, f : Feline, Cat, then) =>
                e.log("wounded", Cat.of(f))
                then

            case BattlePostHitInAction(b, e, f : Feline, Keep, then) =>
                e.log("obliterated", Keep.of(f))
                then

            case BattlePostHitInAction(b, e, f : Feline, p : Scoring, then) =>
                e.log("burned", p.of(f))
                then

            case ForcedRemoveEffectAction(e, c, f : Feline, Cat, then) =>
                f.limbo.one(Cat) --> f.hospitals(c.suit)
                then
                
            case ForcedRemoveCleanupAction(f : Feline, then) =>
                coffins(f.limbo.get)

                val s = game.FoxRabbitMouse.maxBy(s => f.hospitals(s).num)
                if (f.hospitals(s).num > 0) {
                    val q = ForcedRemoveCleanupAction(f, then)
                    if (f.keep.any)
                        OpportunityDiscardCardAction(f, Heals(f, f.hospitals(s).get.num), s, FieldHospitalsAction(f, s, q), FieldHospitalsIgnoreAction(f, s, q))
                    else
                        FieldHospitalsIgnoreAction(f, s, q)
                }
                else
                    then

            case FieldHospitalsAction(f, s, then) =>
                val l = f.hospitals(s).get
                f.log("healed", l./(_.elem).comma, "with", f.drawn.get)
                f.drawn --> discard(f).quiet
                l --> f.keep.get
                then
    
            case FieldHospitalsIgnoreAction(f, s, then) =>
                coffins(f.hospitals(s).get)
                then
    
            // TURN
            case BirdsongNAction(50, f : Feline) =>
                ProduceWoodAction(f)
    
            case ProduceWoodAction(f) =>
                val r = f.all(Sawmill).%(canPlace(f))
                val t = f.pooled(Wood)
                val x = (t >= r.num).?($(r)).|(r.combinations(t).toList)
                
                Ask(f, x./(ProduceMultiWoodAction(f, _)).obirdsong(f))
                    
            case ProduceMultiWoodAction(f : Feline, l) =>
                highlights :+= PlaceHighlight(l.distinct)

                l.foreach(c => f.pool.one(Wood) --> c)
                l.distinct.foreach { c =>
                    log(c, "produced", l.%(_ == c)./(_ => Wood.of(f)).comma)
                }

                Next
                
            case DaylightNAction(30, f : Feline) =>
                if (f.has(DaylightCraft))
                    XCraftMainAction(f)
                else
                    Next

            case DaylightNAction(60, f : Feline) =>
                val dominated = clearings.%(rule(f))
                
                val clusters = {
                    var b = dominated./(c => $(c))
                    var a = b.take(0)
                    
                    while (a.num > b.num || a./(_.num).sum < b./(_.num).sum) {
                        a = b
                        b = b./(l => (l ++ l./~(connected(f)(_).of[Clearing])).filter(dominated.contains).distinct.sortBy(_.name)).distinctBy(_.mkString("|"))
                    }
    
                    b
                }
                
                val free = clusters.%(_.%(canBuild(f)).any)
                val att = clearings.%(attack(f)(_).any)
    
                var actions : List[UserAction] = Nil
                
                actions :+= WageWarMainAction(f, f.bonuses(BonusBattle)).x(att.none)
    
                actions :+= RecruitCatsMainAction(f, f.can(MassRecriut).??(min(f.pooled(Cat), f.all(Recruiter).%(canPlace(f)).num))).x(f.can(MassRecriut).not, "once per turn").x(f.pooled(Cat) == 0 && f.totalWar.not, "maximum").x(f.all(Recruiter).none, "no recruiters").x(f.all(Recruiter).%(canPlace(f)).none, "can't place")
    
                actions :+= MarchMainAction(f, f.bonuses(BonusMove)).x(moveFrom(f).none)
     
                $(Sawmill, Workshop, Recruiter).foreach { b =>
                    val n = f.pooled(b)
                    val cost = f.cost(b)(n)
                    val rw = f.reward(b)(n)
                    val bonus = f.bonus(b)(n)./{
                        case BonusDraw => "extra card"
                        case BonusMove => "extra move"
                        case BonusBattle => "extra battle"
                        case BonusHit => "extra hit"
                    }.|("")
    
                    val cc = free.%(_./(f.at(_).count(Wood)).sum >= cost)
                    
                    actions :+= BuildMainAction(f, b, cost, rw, bonus, cc).x(cost == 999, "maximum").x(clusters.%(_.%(canBuild(f)).any).none, "no place").x(cc.none, "not enough wood")
                }
                
                val r = f.all(Sawmill).distinct
                val o = r.%(c => f.hand.%(_.suit.m(c.suit)).any)
    
                actions :+= OverworkMainAction(f, r, r./(_.suit).distinct).x(f.pooled(Wood) == 0, "maximum").x(r.none, "no sawmills").x(o.none, "no matching card")

                actions :+= ExtraMainAction(f, f.hand.%(_.suit == Bird).num).x(f.hand.%(_.suit == Bird).none, "no bird cards")

                if (f.acted >= 3 + f.extra) {
                    actions = actions.reverse.take(1)
    
                    actions :+= EndTurnAction(f)
                }
                else
                    actions :+= EndTurnSoftAction(f, ForfeitActions(3 + f.extra - f.acted))
                
                Ask(f, actions.daylight(f))
                
            case CatDoneAction(f, then) =>
                f.acted += 1

                then
    
            case ExtraMainAction(f, _) =>
                OptionalDiscardCardAction(f, GetExtra, Bird, ExtraAction(f))
                
            case ExtraAction(f) =>
                f.log("got an", "extra action".hl, "with", f.drawn.get)

                f.drawn --> discard(f).quiet

                f.extra += 1

                Repeat
                
            case WageWarMainAction(f, total) =>
                WageWarAction(f, 1, total, CatDoneAction(f, Repeat))
                
            case MarchMainAction(f, total) =>
                MarchAction(f, 1, total, CatDoneAction(f, Repeat))
    
            case RecruitCatsMainAction(f, _) =>
                val t = f.pooled(Cat)
                val r = f.all(Recruiter).%(canPlace(f))
                
                if (t >= r.num)
                    Ask(f)(RecruitCatsAction(f, r, r))
                else
                if (t == 0)
                    Ask(f)(RecruitCatsAction(f, Nil, r))
                else
                    Ask(f)(r.combinations(t).toList./(RecruitCatsAction(f, _, r))).cancel
                
            case RecruitCatsAction(f, l, r) =>
                highlights :+= PlaceHighlight(l.distinct)

                l.distinct.foreach { c => 
                    val ll = l.%(_ == c)
                    ll.foreach(r => f.pool.one(Cat) --> r)
                    f.log("recruited", ll./(_ => Cat)./(_.of(f)).comma, "in", c)
                }
     
                if (r.num > l.num && f.totalWar)
                    f.oscore(r.num - l.num)("recruiting") 

                f.used :+= MassRecriut
    
                CatDoneAction(f, Repeat)
                    
            case BuildMainAction(f, b, cost, vp, _, cc) =>
                Ask(f, cc./~(l => l.%(canBuild(f))./(BuildAction(f, b, cost, vp, _, l))).cancel)
                
            case BuildAction(f, b, cost, vp, c, l) =>
                val logs = l./~(c => List.fill(f.at(c).count(Wood))(c))
            
                val cm = logs.combinations(cost).toList

                if (cm.num > 1)
                    Ask(f, cm./(BuildPayAction(f, b, vp, c, _)).cancel)
                else
                    Ask(f, cm./(BuildPayAction(f, b, vp, c, _)))
    
            case BuildPayAction(f, b, vp, c, l) =>
                highlights :+= PlaceHighlight($(c))

                l.foreach(c => f.at(c).one(Wood) --> f.pool)
         
                f.pool.one(b) --> c
                
                f.nscore(vp)("building", b.of(f))(f, "built", b.of(f), "in", c, "with", l.num.hl, Wood.of(f), VPOn, ForVP)
    
                CatDoneAction(f, Repeat)
    
            case OverworkMainAction(f, l, _) =>
                highlights :+= PlaceHighlight(l)

                OverworkAction(f, l, Nil)
                
            case OverworkAction(f, l, z) =>
                val r = l.diff(z)
                val a = r./(c => OverworkDiscardAction(f, c, l, z).x(f.hand.%(_.suit.m(c.suit)).none))

                if (a.none)
                    CatDoneAction(f, Repeat)
                else
                if (z.none)
                    Ask(f, a.cancel)
                else     
                    Ask(f, a.done(CatDoneAction(f, Repeat)))
                
            case OverworkDiscardAction(f, c, l, z) =>
                OptionalDiscardCardAction(f, OverworksIn(c), c.suit, OverworkWoodAction(f, c, l, z))
    
            case OverworkWoodAction(f, c, l, z) =>
                f.drawn --> discard(f)

                f.pool.one(Wood) --> c
                
                f.log("overworked in", c)
    
                if (f.pooled(Wood) > 0 && f.has(MassOverwork))
                    OverworkAction(f, l, z :+ c)
                else
                    CatDoneAction(f, Repeat)
                    
            case EveningNAction(50, f : Feline) =>
                if (f.has(EveningCraft))
                    XCraftMainAction(f)
                else
                    Next
                    
            case NightStartAction(f : Feline) =>
                EveningDrawAction(f, f.bonuses(BonusDraw))
                
            case FactionCleanUpAction(f : Feline) =>
                f.acted = 0
                f.extra = 0
                EndPlayerTurnAction(f)

            case _ => UnknownContinue
        }
    }
}

