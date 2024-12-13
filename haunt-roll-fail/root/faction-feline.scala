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

import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedMap


trait Feline extends WarriorFaction {
    val clashKey = MC

    val warrior = Cat

    def abilities(options : $[Meta.O]) = $(MassRecriut, FieldHospitals) ++
        options.has(SawmillMassOverwork).$(MassOverwork) ++
        options.has(WorkshopEveningCraft).?($(EveningCraft)).|($(DaylightCraft)) ++
        options.has(WorkshopActionBonuses).$(FancyWorkshops) ++
        options.has(Catapults).$(Siege)

    def pieces(options : $[Meta.O]) = Cat *** 25 ++ Keep *** 1 ++ Sawmill *** 6 ++ Workshop *** 6 ++ Recruiter *** 6 ++ Wood *** 8 ++ options.has(Catapults).??(Catapult *** 3)

    override val transports : $[$[Transport]] = $($(RuledMove, CatapultMove))

    def basic : $[FelineBonus] = $(BonusBattle, BonusDraw, BonusMove, BonusMove)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Cat, Workshop, Sawmill, Recruiter, Wood) ++ options.has(Catapults).$(Catapult) ++ $(Keep)

    def advertising = Keep.img(this) ~ Sawmill.img(this) ~ Workshop.img(this) ~ Recruiter.img(this)
    def motto = "Build".styled(this)
}

case object Keep extends Token
case object Wood extends Token
case object Sawmill extends Building
case object Workshop extends Building
case object Recruiter extends Building
case object Cat extends Warrior

case object Catapult extends Token with Movable

case object Siege extends BattleEffect

case object MassRecriut extends HiddenEffect
case object FieldHospitals extends HiddenEffect with RemoveEffect

case class FieldHospital(c : Clearing) extends SpecialRegion

case object MassOverwork extends HiddenEffect
case object FancyWorkshops extends HiddenEffect

case object MC extends Feline {
    val name = "Marquise de Cat"
    override def funName = NameReference(name, this) ~ " " ~ Span("de Cat", styles.block)
    val short = "MC"
    val style = "mc"
    val priority = "A"
}

case object BK extends Feline {
    val name = "Baron von Katze"
    override def funName = NameReference(name, this) ~ " von Katze"
    val short = "BK"
    val style = "bk"
    val priority = "A'"
}

trait FelineBonus extends Effect
case object BonusDraw extends FelineBonus
case object BonusMove extends FelineBonus
case object BonusBattle extends FelineBonus
case object BonusHit extends FelineBonus with FactionEffect with BattleEffect {
    override val name = "Paw Crossbow"
}


case object CatapultMove extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = (f, o) @@ {
        case (f : Feline, o : Clearing) => f.at(o).of[Warrior].any
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && (l.count(Cat) >= l.count(Catapult) * 4)

    override def order(l : $[Movable]) : Int = l.count(Catapult) * 900 + l.count(Cat)
    override def sortBy(m : Movable) : Int = m @@ {
        case Cat => 1
        case Catapult => 2
    }
}


class FelinePlayer(val faction : Feline)(implicit val game : Game) extends FactionState {
    var acted = 0
    var extra = 0

    var keep : |[Clearing] = None

    var keepDamage = 0

    var plan : $[(DeckCard, HospitalPlan)] = $

    def craft = all(Workshop)./(_.asset)

    def cost(b : Building) = $(999, 4, 3, 3, 2, 1, 0)

    def reward(b : Building) = b match {
        case Sawmill                                  => $(999, 5, 4, 3, 2, 1, 0)
        case Workshop if core.has(FancyWorkshops).not => $(999, 5, 4, 3, 2, 2, 0)
        case Workshop if core.has(FancyWorkshops)     => $(999, 4, 3, 3, 2, 1, 0)
        case Recruiter                                => $(999, 4, 3, 3, 2, 1, 0)
    }

    def bonus(b : Building) : $[$[FelineBonus]] = b match {
        case Sawmill                                  => $($, $,           $,              $,            $,            $,              $)
        case Workshop if core.has(FancyWorkshops).not => $($, $,           $,              $,            $,            $,              $,            $(BonusMove))
        case Workshop if core.has(FancyWorkshops)     => $($, $(BonusHit), $(BonusBattle), $(BonusDraw), $(BonusMove), $(BonusBattle), $(BonusMove))
        case Recruiter                                => $($, $,           $(BonusDraw),   $,            $(BonusDraw), $,              $)
    }

    def basic = $[FelineBonus](BonusBattle, BonusDraw, BonusMove)

    def bonuses(b : FelineBonus) = basic.count(b) +
        bonus(Sawmill).drop(pooled(Sawmill) + 1).flatten.count(b) +
        bonus(Workshop).drop(pooled(Workshop) + 1).flatten.count(b) +
        bonus(Recruiter).drop(pooled(Recruiter) + 1).flatten.count(b)
}

case class ToHeal(f : WarriorFaction, n : Int) extends Message {
    def elem(implicit game : Game) = "to heal " ~ n.times(f.warrior.img(f)).merge
}

case class ToRebuild(f : Faction, p : Piece) extends Message {
    def elem(implicit game : Game) = "to rebuild " ~ p.of(f)
}

case object GetExtra extends Message {
    def elem(implicit game : Game) = "to get an extra action".txt
}

case class OverworksIn(c : Clearing) extends Message {
    def elem(implicit game : Game) = "to overwork in " ~ c.elem
}

trait MCDaylightQuestion extends FactionAction {
    override def self : Feline

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            1.to(3 + self.extra)./(_ => Image("action-black", styles.action, "")).take(self.acted) ~
            (1.to(3)./(_ => Image(self.style + "-action", styles.action)) ++ (0.until(self.extra)./(_ => Image("action-bird", styles.action)))).drop(self.acted),
        styles.margined)
}

case class StartingBuildingsAction(f : Feline) extends ForcedAction
case class StartingBuildingMainAction(self : Feline, l : $[Clearing], p : Building) extends BaseAction(self, "starting buildings")(p.of(self), p.imgd(self)) with Soft
case class StartingBuildingAction(self : Feline, c : Clearing, p : Building) extends BaseAction(self, "places", p.of(self), p.imgd(self), "in")(c)
case class PlaceStartingBuildingAction(self : Faction, k : Clearing, r : Clearing, p : Building) extends BaseAction(self, "places", p.of(self), "in")(r)
case class StartingBuildingsNextAction(self : Faction, r : Clearing, p : Building) extends ForcedAction

case class FieldHospitalsAction(f : Feline, c : Clearing, then : ForcedAction) extends ForcedAction
case class FieldHospitalsIgnoreAction(f : Feline, c : Clearing, then : ForcedAction) extends ForcedAction

case class ProduceMultiWoodAction(self : Faction, l : $[Clearing]) extends BaseAction("Place", Wood.of(self), "in")(l.comma)

case class ExtraMainAction(self : Feline, n : Int) extends OptionAction("Extra action".hl, n.times(dt.CardSuit(Bird))) with MCDaylightQuestion with Soft
case class ExtraAction(f : Feline) extends ForcedAction

case class CatDoneAction(f : Feline, then : ForcedAction) extends ForcedAction

case class WageWarMainAction(self : Feline, total : Int) extends OptionAction((total > 1).?("Wage War".styled(self), total.times(dt.Battle).merge).|("Battle".styled(self), dt.Battle)) with MCDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }

case class RecruitCatsMainAction(self : Feline, n : Int) extends OptionAction("Recruit".styled(self), n.times(self.warrior.imgd(self)).merge) with MCDaylightQuestion with Soft with Only[RecruitCatsAction] { def tag = implicitly }
case class RecruitCatsAction(self : Feline, l : $[Clearing], r : $[Clearing]) extends BaseAction("Recruit in")(l.comma)

case class MarchMainAction(self : Feline, total : Int) extends OptionAction((total > 1).?("March".styled(self), total.times(dt.Move).merge).|("Move".styled(self), dt.Move)) with MCDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }

case class RebuildKeepLocationAction(self : Feline, c : Clearing, from : Clearing, then : ForcedAction) extends BaseAction("Rebuild", Keep.of(self), "in")(c) with Soft
case class RebuildKeepAction(self : Feline, c : Clearing, from : Clearing, then : ForcedAction) extends ForcedAction

case class BuildMainAction(self : Feline, b : Building, cost : Int, vp : Int, bonus : String, supply : $[$[Clearing]]) extends OptionAction("Build", b.of(self), (cost < 999).$((cost > 0).?(cost.times(Wood.imgd(self)).merge).|(Text("free")), dt.Arrow, b.imgd(self), vp.vp, (bonus != "").$("|", bonus))) with MCDaylightQuestion with Soft with Only[BuildPayAction] { def tag = implicitly }
case class BuildCatapultAction(self : Feline, cost : Int, l : $[Clearing], supply : $[$[Clearing]]) extends OptionAction("Build", Catapult.of(self), (cost < 999).$((cost > 0).?(cost.times(Wood.imgd(self)).merge).|(Text("free")), dt.Arrow, Catapult.imgd(self))) with MCDaylightQuestion with Soft with Only[BuildPayAction] { def tag = implicitly }

case class BuildAction(self : Feline, b : Piece, cost : Int, vp : Int, c : Clearing, l : $[Clearing]) extends BaseAction("Build", b.of(self), "in")(c) with Soft
case class BuildPayAction(self : Feline, b : Piece, vp : Int, c : Clearing, l : $[Clearing]) extends BaseAction("Build", b.of(self), "in", c, "with", Wood.of(self), "from")(l.comma)


case class OverworkMainAction(self : Feline, l : $[Clearing], s : $[Suit]) extends OptionAction("Overwork".styled(self), dt.CardSuit(s.single.|(AnySuit)), dt.Arrow, Wood.imgd(self)) with MCDaylightQuestion with Soft
case class OverworkAction(self : Feline, l : $[Clearing], z : $[Clearing]) extends ForcedAction with Soft
case class OverworkDiscardAction(self : Feline, c : Clearing, l : $[Clearing], z : $[Clearing]) extends BaseAction("Overwork", z.any.??("next"), "in")(c) with Soft
case class OverworkWoodAction(self : Feline, c : Clearing, l : $[Clearing], z : $[Clearing]) extends ForcedAction

case class BattleSiegeAction(self : Faction, e : Faction, b : Battle, l : $[Piece]) extends BaseAction("Siege")("Siege".hh, "with", l./(_.img(self)).merge)
case class BattleNoSiegeAction(self : Faction, e : Faction, b : Battle) extends BaseAction(None)("Cancel")


trait HospitalPlan extends Record with Elementary {
    def id : String
}

object HospitalPlan {
    val all = $[HospitalPlan](HospitalSkip, HospitalAsk, HospitalOnePlus, HospitalTwoPlus)

    def of(f : Feline) = "Hospital Plan".styled(f)
}

case object HospitalSkip extends HospitalPlan {
    val id = "skip"
    val elem = "Skip".hl
}
case object HospitalAsk extends HospitalPlan {
    val id = "ask"
    val elem = "Ask".hl
}
case object HospitalOnePlus extends HospitalPlan {
    val id = "one-plus"
    val elem = "One".hl ~ " or " ~ "More".hl
}
case object HospitalTwoPlus extends HospitalPlan {
    val id = "two-plus"
    val elem = "Two".hl ~ " or " ~ "More".hl
}

case class HospitalPlanMainAction(f : Feline, h : $[DeckCard], q : $[Elem], o : $[Option[HospitalPlan]], max : Int, m : SortedMap[Int, HospitalPlan]) extends ForcedAction with Soft
case class HospitalPlanCardAction(self : Feline, h : $[DeckCard], q : $[Elem], o : $[Option[HospitalPlan]], max : Int, m : SortedMap[Int, HospitalPlan], n : Int) extends BaseAction(q(n))(h(n).img) with Soft with ViewCard with Selectable with NoClear with NoExplode with LimitedExtra[HospitalPlan] with SkipValidate with ElemWrap {
    val d = h(n)

    def wrap(g : Game)(e : Elem) = Div(values./ { c =>
        val x = (m.contains(n) || m.num < max) && o(n) != Some(c)
        val s = m.get(n) == Some(c) || m.contains(n).not && o(n) == Some(c)
        val p = self.style + "-" + c.id + s.not.??("-done")
        x.?(OnClick(c, s.?(Image(p, styles.action, styles.selected, xlo.pointer)).|(Image(p, styles.action, xlo.pointer)))).|(Image(p, styles.action))
    }.merge ~ Break ~ m.get(n).any.?(OnClick(m(n), e.styled(xlo.pointer))).|(e), styles.inline, styles.margined)

    def selected = m.contains(n)

    def fromAny(s : Any) = s @@ {
        case c : HospitalPlan => Some(c)
        case _ => None
    }

    def values = HospitalPlan.all

    def update(c : HospitalPlan) = this.copy(m = (m.get(n) == Some(c)).?(m - n).|(m + (n -> c)))
}

case class HospitalPlanExplodeAction(self : Feline, h : $[DeckCard], o : $[Option[HospitalPlan]], max : Int) extends HiddenChoice with SelfExplode {
    def explode(withSoft : Boolean) = {
        val iii = 1.to(max)./~(h.indices.combinations).flatMap { l =>
            l.foldLeft($(SortedMap[Int, HospitalPlan]()))((mm, n) => mm./~(m => HospitalPlan.all./(c => m + (n -> c))))
        }

        val ii = iii.%(_.any)

        ii./(m => HospitalPlanAction(self, h, m.to(ListMap), o))
    }
}

case class HospitalPlanAction(self : Feline, h : $[DeckCard], m : ListMap[Int, HospitalPlan], o : $[Option[HospitalPlan]]) extends BaseAction()("Done")


object FelineExpansion extends FactionExpansion[Feline] {
    def cardValue(f : Feline)(d : DeckCard)(implicit game : Game) : Int = d @@ {
        case c : Favor if f.keep.?(_.cost.matched(c.suit)) => 1200
        case Ambush(Bird) => 1000
        case _ if d.suit == Bird => 800
        case c : Favor => 700
        case _ : Ambush => 600
        case c : CraftItemCard if game.uncrafted.has(c.item).not => 0
        case c : CraftItemCard if c.item == Hammer => 400
        case c : CraftItemCard => f.craftableWith(f.craft, $)(c).?(c.vp * 100).|(c.vp * 10)
        case c : CraftEffectCard => f.craftableWith(f.craft, $)(c).?(200).|(50)
        case d : Dominance => 1
        case _ => throw new Error("card with no value " + d)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        case CreatePlayerAction(f : Feline) =>
            game.states += f -> new FelinePlayer(f)

            FactionInitAction(f)

        // SETUP
        case FactionSetupAction(f : Feline) if options.has(SetupTypeHomelands) =>
            soft()

            val clusters = {
                var b = board.clearings.diff(game.homelands).%(f.canBuild)./(c => $(c))
                var a = b.take(0)

                while (a.num > b.num || a./(_.num).sum < b./(_.num).sum) {
                    a = b
                    b = b./(l => (l ++ l./~(game.connected).distinct.diff(game.homelands).%(f.canBuild)).distinct.sortBy(_.name)).distinct
                }

                b
            }

            val l = clusters.%(_.num >= 3)./~(x => x)

            Ask(f)(StartingBuildingMainAction(f, l, Sawmill))(StartingBuildingMainAction(f, l, Workshop))(StartingBuildingMainAction(f, l, Recruiter))

        case StartingBuildingMainAction(f : Feline, l, p) =>
            Ask(f)(l./(StartingBuildingAction(f, _, p))).cancel

        case StartingBuildingAction(f : Feline, c, p) =>
            game.homelands :+= c

            factions.but(f).of[Feline].foreach { e =>
                e.keep.foreach { k =>
                    e.from(c) --> k
                }
            }

            if (game.options.has(AdSetBuffOn)) {
                f.reserve --> p --> c
                f.reserve --> Cat --> c

                f.log("placed", p.of(f), "and", Cat.of(f), "in", c)
            }
            else {
                f.reserve --> p --> c

                f.log("placed", p.of(f), "in", c)
            }

            StartingBuildingsAction(f)

        case StartingBuildingsAction(f) =>
            soft()

            val s = f.all(Sawmill)
            val w = f.all(Workshop)
            val r = f.all(Recruiter)

            if (s.none || w.none || r.none) {
                val l = (s ++ w ++ r)./~(game.connected).distinct.diff(game.homelands).%(f.canBuild)

                Ask(f)(s.none.?(StartingBuildingMainAction(f, l, Sawmill)))(w.none.?(StartingBuildingMainAction(f, l, Workshop)))(r.none.?(StartingBuildingMainAction(f, l, Recruiter)))
            }
            else {
                val l = s ++ w ++ r
                val ll = l.diff(game.homelands.diff(l)./~(game.connected)).some.|(l)

                Ask(f)(ll./(c => StartingClearingAction(f, c).as(c)(f, "places", Keep.of(f), Keep.imgd(f), "in"))).needOk
            }

        case StartingClearingAction(f : Feline, c) if options.has(SetupTypeHomelands) =>
            if (!options.has(UntargetableKeep))
                f.reserve --> Keep --> c

            f.keep = Some(c)

            f.log("placed", Keep.of(f), "in", c)

            factions.but(f).of[Feline].foreach { e =>
                e.keep.foreach { k =>
                    e.from(c) --> k
                }
            }

            val l = (game.options.has(AdSetBuffOn) || options.has(SetupTypeHomelands)).?(clearings).|(clearings.but(board.opposite(c)))

            l.foreach { x =>
                val o = factions.but(f).of[Feline]./~(_.at(x))
                if (o.of[Building].any || o.of[Token].any)
                    f.reserve --> Cat --> c
                else
                    f.reserve --> Cat --> x
            }

            SetupFactionsAction

        case FactionSetupAction(f : Feline) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : Feline, r) =>
            if (!options.has(UntargetableKeep))
                f.reserve --> Keep --> r

            f.keep = Some(r)

            factions.but(f).of[Feline].foreach { e =>
                e.keep.foreach { k =>
                    e.from(r) --> k
                }
            }

            val l = game.options.has(AdSetBuffOn).?(clearings).|(clearings.but(board.opposite(r)))

            l.foreach { c =>
                val o = factions.but(f).of[Feline]./~(_.at(c))
                if (o.of[Building].any || o.of[Token].any)
                    f.reserve --> Cat --> r
                else
                    f.reserve --> Cat --> c
            }

            f.log("placed", Keep.of(f), "in", r)
            f.log("placed", Cat.sof(f), "through the forest")

            StartingBuildingsNextAction(f, r, Sawmill)

        case StartingBuildingsNextAction(f : Feline, k, p) =>
            soft()

            Ask(f)((k +: game.connected(k)).%(f.canBuild)./(PlaceStartingBuildingAction(f, k, _, p)))

        case PlaceStartingBuildingAction(f : Feline, k, r, p) =>
            f.reserve --> p --> r

            factions.but(f).of[Feline].foreach { e =>
                e.keep.foreach { k =>
                    e.from(r) --> k
                }
            }

            if (game.options.has(AdSetBuffOn))
                f.reserve --> Cat --> r

            f.log("placed", p.of(f), game.options.has(AdSetBuffOn).?(("and", Cat.of(f))), "in", r)

            p match {
                case Sawmill => StartingBuildingsNextAction(f, k, Workshop)
                case Workshop => StartingBuildingsNextAction(f, k, Recruiter)
                case Recruiter => SetupFactionsAction
            }

        // HELPER
        case MoveListAction(self, f, t, m, from, to, l, CatDoneAction(ff, then)) =>
            CatDoneAction(ff, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case MoveListAction(self, f, t, m, from, to, l, MarchAction(ff, i, n, CatDoneAction(fff, then))) =>
            CatDoneAction(fff, ForceAction(MoveListAction(self, f, t, m, from, to, l, MarchAction(ff, i, n, then))))

        case BattleStartAction(self, f, a, m, c, o, i, CatDoneAction(ff, then)) =>
            CatDoneAction(ff, BattleStartAction(self, f, a, m, c, o, i, then))

        case LegalAAAConveneAction(f, h, c, e, CatDoneAction(ff, then)) =>
            CatDoneAction(ff, LegalAAAConveneAction(f, h, c, e, then))


        case BattlePostHitInAction(b, e : Feline, f : Feline, p : Building, then @ BattlePostHitOutAction(_, _, _, _, remove : TryForcedRemoveAction))
        if options.has(TheWoodMustFlow) && e.used.has(Siege).not && e.pool(p) =>
            e.reserve --> p --> b.clearing
            e.log("captured", p.of(f))
            then.copy(then = remove.copy(n = 0))

        case BattlePostHitInAction(b, e : Feline, f : Feline, p : Wood.type, then @ BattlePostHitOutAction(_, _, _, _, remove : TryForcedRemoveAction))
        if options.has(TheWoodMustFlow) && e.used.has(Siege).not && e.pool(p) =>
            e.reserve --> p --> b.clearing
            e.log("captured", p.of(f))
            then.copy(then = remove.copy(n = 0))

        case BattlePostHitInAction(b, e : Feline, f : Feline, p : Keep.type, then @ BattlePostHitOutAction(_, _, _, _, remove : TryForcedRemoveAction))
        if options.has(TheWoodMustFlow) && e.used.has(Siege).not && e.pool(p)
        && options.has(IndestructibleKeep).not && (options.has(ThreeHitKeep).not || f.keepDamage == 2) =>
            e.reserve --> p --> b.clearing
            e.log("captured", p.of(f))
            then.copy(then = remove.copy(n = 0))

        case BattlePostHitInAction(b, e, f : Feline, Cat, then) =>
            e.log("wounded", Cat.of(f))
            then

        case BattlePostHitInAction(b, e, f : Feline, Keep, then) =>
            e.log("obliterated", Keep.of(f))
            then

        case BattlePostHitInAction(b, e, f : Feline, p : Scoring, then) =>
            e.log("burned", p.of(f))
            then

        case ForcedRemoveTargetEffectAction(e, c, f : Feline, Keep, then) if options.has(IndestructibleKeep) =>
            f.limbo(c) --> Keep --> c

            then

        case ForcedRemoveTargetEffectAction(e, c, f : Feline, Keep, then) if options.has(ThreeHitKeep) && f.keepDamage < 2 =>
            f.limbo(c) --> Keep --> c

            f.keepDamage += 1

            then

        case ForcedRemoveTargetEffectAction(e, c, f : Feline, Keep, then) if options.has(InstantRebuildKeep) =>
            f.keep = None

            val sites = clearings.but(c).%(f.rules)
            val valid = sites.%(f.canPlace).%(c => f.hand.exists(_.matches(c.cost)))

            if (valid.any)
                Ask(f).each(sites)(t => RebuildKeepLocationAction(f, t, c, then).!(valid.has(t).not)).skip(then)
            else
                then

        case ForcedRemoveTargetEffectAction(e, c, f : Feline, Keep, then) =>
            f.keep = None

            then

        case RebuildKeepLocationAction(f, t, c, then) =>
            OptionalDiscardCardAction(f, ToRebuild(f, Keep), c.cost, RebuildKeepAction(f, t, c, then))

        case RebuildKeepAction(f, t, c, then) =>
            f.limbo(c) --> Keep --> t

            f.keep = Some(t)

            f.log("rebuilt", Keep.of(f), "with", f.drawn.get, "in", t)

            f.drawn --> discard.quiet

            f.keepDamage = 0

            then

        case ForcedRemoveProcessAction(f : Feline, then) if f.has(FelinePhysicians).not && f.keep.any && f.ignored.has(FieldHospitals).not && f.ignored.has(IgnoreWarriorRemovalEffects).not =>
            val c = board.clearings.diff(f.ignored.of[SkipHeal]./(_.c)).maxBy(c => f.limbo(c).$.%(_.piece.is[Warrior]).num)
            val n = f.limbo(c).$.%(_.piece.is[Warrior]).num

            if (n > 0) {
                val q = ForcedRemoveProcessAction(f, then)

                val l = f.hand.%(_.matches(c.cost))

                val plan = (game.current != f).??(f.plan).toMap
                val m1 = l.%(d => plan.get(d).has(HospitalOnePlus)).sortBy(cardValue(f))
                val m2 = (n > 1).??(l.%(d => plan.get(d).has(HospitalTwoPlus))).sortBy(cardValue(f))
                val mm = m1 ++ m2
                val ask = l.%(d => plan.get(d).but(HospitalAsk).none)

                if (mm.any) {
                    if (n > 1 && f.plan.has(mm.head -> HospitalTwoPlus))
                        f.plan :-= mm.head -> HospitalTwoPlus
                    else
                    if (f.plan.has(mm.head -> HospitalOnePlus))
                        f.plan :-= mm.head -> HospitalOnePlus

                    f.hand --> mm.head --> f.drawn

                    Ask(f)(DoAction(FieldHospitalsAction(f, c, q)))
                }
                else
                if (ask.none) {
                    Ask(f)(DoAction(FieldHospitalsIgnoreAction(f, c, q)))
                }
                else
                    OpportunityDiscardCardAction(f, ToHeal(f, n), c.cost, FieldHospitalsAction(f, c, q), FieldHospitalsIgnoreAction(f, c, q))
            }
            else {
                f.ignored :+= FieldHospitals

                ForcedRemoveProcessAction(f, then)
            }

        case FieldHospitalsAction(f, c, then) =>
            val l = f.limbo(c).$.%(_.piece.is[Warrior])

            f.log("healed", l./(_.elem).comma, "with", f.drawn.get)

            f.drawn --> discard.quiet

            l --> f.keep.get

            then

        case FieldHospitalsIgnoreAction(f, c, then) =>
            f.ignored :+= SkipHeal(c)

            then

        case BattleStartedAction(b) if b.attacker.can(Siege) && b.attacker.at(b.clearing).has(Catapult) && b.attacker.rules(b.clearing) && b.defender.at(b.clearing).of[Scoring].any =>
            val f = b.attacker
            val e = b.defender
            Ask(f)((BattleSiegeAction(f, e, b, b.attacker.at(b.clearing).of[Catapult.type]) :: BattleNoSiegeAction(f, e, b)))

        case BattleSiegeAction(f, e, b, l) =>
            f.used :+= Siege

            f.log("sieged with", l./(_.of(f)).comma)

            BattleStartedAction(b)

        case BattleNoSiegeAction(f, e, b) =>
            f.ignored :+= Siege

            BattleStartedAction(b)

        // TURN
        case BirdsongNAction(50, f : Feline) if soft =>
            val t = f.pooled(Wood)
            val r = f.all(Sawmill).%(f.canPlace)

            if (t == 0)
                Ask(f).done(Next).birdsong(f)
            else
            if (t >= r.num)
                Ask(f)(ProduceMultiWoodAction(f, r))
            else
                Ask(f).each(r.combinations(t).$)(ProduceMultiWoodAction(f, _)).birdsong(f)

        case ProduceMultiWoodAction(f : Feline, l) =>
            game.highlights :+= PlaceHighlight(l.distinct)

            l.foreach(c => f.reserve --> Wood --> c)

            l.distinct.foreach { c =>
                log(c, "produced", l.%(_ == c)./(_ => Wood.of(f)).comma)
            }

            Next

        case DaylightNAction(30, f : Feline) =>
            if (f.has(DaylightCraft))
                XCraftMainAction(f)
            else
                Next

        case DaylightNAction(60, f : Feline) if soft =>
            implicit val ask = builder

            if (f.acted < 3 + f.extra) {
                + WageWarMainAction(f, f.bonuses(BonusBattle))
                    .!(clearings.%(f.canAttackIn).none)

                + RecruitCatsMainAction(f, f.can(MassRecriut).??(min(f.pooled(Cat), f.all(Recruiter).%(f.canPlace).num)))
                    .!(f.can(MassRecriut).not, "once per turn")
                    .!(f.pool(Cat).not && f.totalWar.not, "maximum")
                    .!(f.all(Recruiter).none, "no recruiters")
                    .!(f.all(Recruiter).%(f.canPlace).none, "can't place")

                + MarchMainAction(f, f.bonuses(BonusMove))
                    .!(f.moveFrom.none)

                val dominated = clearings.%(f.rules)

                val clusters = {
                    var b = dominated./(c => $(c))
                    var a = b.take(0)

                    while (a.num > b.num || a./(_.num).sum < b./(_.num).sum) {
                        a = b
                        b = b./(l => clearings.intersect((l ++ l./~(f.connectedFor).of[Clearing]).%(dominated.contains))).distinct
                    }

                    b
                }

                val free = clusters.%(_.%(f.canBuild).any)

                $(Sawmill, Workshop, Recruiter).foreach { b =>
                    val n = f.pooled(b)
                    val cost = f.cost(b)(n)
                    val rw = f.reward(b)(n)
                    val bonus = f.bonus(b)(n).single./{
                        case BonusDraw => "extra card"
                        case BonusMove => "extra move"
                        case BonusBattle => "extra battle"
                        case BonusHit => "extra hit"
                    }.|("")

                    val supplied = free.%(_./(f.at(_).count(Wood)).sum >= cost)

                    + BuildMainAction(f, b, cost, rw, bonus, supplied)
                        .!(n == 0, "maximum")
                        .!(free.none, "no place")
                        .!(supplied.none, "not enough wood")
                }

                if (options.has(Catapults)) {
                    val cost = 1
                    val l = f.all(Workshop).distinct
                    val supplied = clusters.%(_.exists(l.has)).%(_./(f.at(_).count(Wood)).sum >= cost)

                    + BuildCatapultAction(f, cost, l, supplied)
                        .!(f.pool(Catapult).not, "maximum")
                        .!(l.none, "no workshops")
                        .!(supplied.none, "not enough wood")
                }

                val r = f.all(Sawmill).distinct
                val o = r.%(c => f.hand.%(_.matches(c.cost)).any)

                + OverworkMainAction(f, r, r./~(_.suits).distinct)
                    .!(f.pool(Wood).not, "maximum")
                    .!(r.none, "no sawmills")
                    .!(o.none, "no matching card")
            }

            + ExtraMainAction(f, f.hand.%(_.suit == Bird).num)
                .!(f.hand.%(_.suit == Bird).none, "no bird cards")

            + EndTurnSoftAction(f, "Turn", ForfeitActions(3 + f.extra - f.acted))

            ask(f).daylight(f)

        case CatDoneAction(f, then) =>
            f.acted += 1

            then

        case ExtraMainAction(f, _) =>
            OptionalDiscardCardAction(f, GetExtra, Bird, ExtraAction(f))

        case ExtraAction(f) =>
            f.log("got an", "extra action".hl, "with", f.drawn.get)

            f.drawn --> discard.quiet

            f.extra += 1

            Repeat

        case WageWarMainAction(f, total) =>
            WageWarAction(f, 1, total, CatDoneAction(f, Repeat))

        case MarchMainAction(f, total) =>
            MarchAction(f, 1, total, CatDoneAction(f, Repeat))

        case RecruitCatsMainAction(f, _) =>
            val t = f.pooled(Cat)
            val r = f.all(Recruiter).%(f.canPlace)

            if (t >= r.num)
                Ask(f)(RecruitCatsAction(f, r, r))
            else
            if (t == 0)
                Ask(f)(RecruitCatsAction(f, $, r))
            else
                Ask(f)(r.combinations(t).$./(RecruitCatsAction(f, _, r))).cancel

        case RecruitCatsAction(f, l, r) =>
            game.highlights :+= PlaceHighlight(l.distinct)

            l.distinct.foreach { c =>
                val n = l.count(c)

                f.reserve --> n.times(Cat) --> c

                f.log("recruited", n.times(Cat.of(f)).comma, "in", c)
            }

            if (r.num > l.num && f.totalWar)
                f.oscore(r.num - l.num)("recruiting")

            f.used :+= MassRecriut

            CatDoneAction(f, Repeat)

        case BuildMainAction(f, b, cost, vp, _, cc) =>
            Ask(f)(cc./~(l => l.%(f.canBuild)./(BuildAction(f, b, cost, vp, _, l)))).cancel

        case BuildCatapultAction(f, cost, workshops, supply) =>
            Ask(f).some(supply)(l => l.%(workshops.has)./(BuildAction(f, Catapult, cost, 0, _, l))).cancel

        case BuildAction(f, b, cost, vp, c, l) =>
            val logs = l./~(c => f.at(c).count(Wood).times(c))

            val cm = logs.combinations(cost).$

            Ask(f)(cm./(BuildPayAction(f, b, vp, c, _))).cancelIf(cm.num > 1)

        case BuildPayAction(f, b, vp, c, l) =>
            game.highlights :+= PlaceHighlight($(c))

            l.foreach(c => f.from(c) --> Wood --> f.reserve)

            f.reserve --> b --> c

            f.nscore(vp)("building", b.of(f))(f, "built", b.of(f), "in", c, "with", l.num.hl, Wood.of(f), ForVP)

            CatDoneAction(f, Repeat)

        case OverworkMainAction(f, l, _) =>
            game.highlights :+= PlaceHighlight(l)

            OverworkAction(f, l, $)

        case OverworkAction(f, l, z) =>
            val r = l.diff(z)
            val a = r./(c => OverworkDiscardAction(f, c, l, z).!(f.hand.%(_.matches(c.cost)).none))

            if (a.none)
                CatDoneAction(f, Repeat)
            else
            if (z.none)
                Ask(f)(a).cancel
            else
                Ask(f)(a).done(CatDoneAction(f, Repeat))

        case OverworkDiscardAction(f, c, l, z) =>
            OptionalDiscardCardAction(f, OverworksIn(c), c.cost, OverworkWoodAction(f, c, l, z))

        case OverworkWoodAction(f, c, l, z) =>
            f.log("overworked in", c, "with", f.drawn.get)

            f.drawn --> discard.quiet

            f.reserve --> Wood --> c

            if (f.pool(Wood) && f.has(MassOverwork))
                OverworkAction(f, l, z :+ c)
            else
                CatDoneAction(f, Repeat)

        case EveningNAction(50, f : Feline) if f.has(EveningCraft) =>
            XCraftMainAction(f)

        case EveningNAction(60, f : Feline) if f.has(EveningCraft) =>
            Ask(f).evening(f).done(Next)

        case NightStartAction(f : Feline) =>
            EveningDrawAction(f, f.bonuses(BonusDraw))

        case FactionCleanUpAction(f : Feline) =>
            f.acted = 0
            f.extra = 0

            CleanUpAction(f)

        case AfterTurnAction(f : Feline) if options.has(ForcedAsyncMode) && f.keep.any && f.hand.any && f.used.has(FieldHospitals).not =>
            val h = f.hand.$
            val q = f.hand.$.indices./(_ => "Adjust " ~ HospitalPlan.of(f))
            val o = f.hand.$.indices./(_ => None)
            val max = 999

            var m : $[(Int, HospitalPlan)] = $

            f.hand.$.indexed.foreach { (d, i) =>
                if (f.plan.toMap.contains(d)) {
                    val p = f.plan.toMap.apply(d)
                    m :+= i -> p
                    f.plan :-= d -> p
                }
                else {
                    m :+= i -> d @@ {
                        case _ if d.suit == Bird => HospitalSkip
                        case _ : Ambush => HospitalSkip
                        case c : CraftItemCard if game.uncrafted.has(c.item).not => HospitalOnePlus
                        case c : CraftItemCard if c.item == Hammer => HospitalSkip
                        case c : CraftItemCard => f.craftableWith(f.craft, $)(c).?((c.vp > 1).?(HospitalSkip).|(HospitalTwoPlus)).|(HospitalOnePlus)
                        case c : CraftEffectCard => f.craftableWith(f.craft, $)(c).?(HospitalTwoPlus).|(HospitalOnePlus)
                        case c : Favor => HospitalSkip
                        case d : Dominance => HospitalOnePlus
                        case _ => HospitalAsk
                    }
                }
            }

            f.plan = $

            HospitalPlanMainAction(f, h, q, o, max, m.to(SortedMap))

        case HospitalPlanMainAction(f, h, q, o, max, m) =>
            Ask(f)
              .each(0.until(h.num).$)(n => HospitalPlanCardAction(f, h, q, o, max, m, n))
              .add(HospitalPlanAction(f, h, m.to(ListMap), o).x(m.none))
              .add((HospitalPlanExplodeAction(f, h, o, max)))

        case HospitalPlanCardAction(f, h, q, o, max, m, _) =>
            HospitalPlanMainAction(f, h, q, o, max, m)

        case HospitalPlanAction(f, h, m, o) =>
            f.plan = m.$./((i, p) => h(i) -> p)

            f.used :+= FieldHospitals

            AfterTurnAction(f)

        case _ => UnknownContinue
    }

}
