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

case object Wizard extends Warrior {
    override def id = "Frog"
}

trait Caster extends WarriorFaction {
    val clashKey = XC

    val warrior = Wizard

    val spellbooks = $[Spell](Petrify, Dazzle, Regrowth, Fireball, Summon, Resurrection, Transform, Teleport)

    def abilities(options : $[Meta.O]) = $(MorningCraft)

    def pieces(options : $[Meta.O]) = Wizard *** 15 ++ Statue *** 3 ++ School *** 3

    override def note : Elem = SpacedDash ~ "V" ~ "1.5".hh ~ " by " ~ "slugfacekillah".hl

    def advertising = 4.times(Image(this.short + "-magic", styles.piece)).merge

    def motto = "Cast".styled(this)
}

case object Statue extends Token

case object School extends Building

case object Growth extends Pawn

trait Spell extends Effect {
    val cost : |[Int]
    val name = toString
    def img = Image("xc-spell-" + name, styles.card)
    def of(f : Faction) = name.styled(f)
}

case object BattleSpell extends BattleEffect



case class SkipAllResurrection(f : Caster) extends RemoveEffect


case object Petrify extends Spell { val cost = Some(1) }
case object Dazzle extends Spell { val cost = Some(1) }
case object Regrowth extends Spell { val cost = Some(2) }
case object Fireball extends Spell { val cost = Some(2) }
case object Summon extends Spell { val cost = Some(3) }
case object Resurrection extends Spell { val cost = Some(3) }
case object Transform extends Spell { val cost = None }
case object Teleport extends Spell { val cost = None }


case object XC extends Caster {
    val name = "Croakers Coven"
    override def funName = NameReference(name, this) ~ " Coven"
    val short = "XC"
    val style = "xc"
    val priority = "S"
}

trait XCDaylightQuestion extends FactionAction {
    override def self : Caster

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            1.to(self.acted)./(_ => Image("action-black", styles.action)) ~ (self.acted.until(4)./(_ => Image(self.style + "-action", styles.action))),
        styles.margined)
}

trait XCSpellQuestion extends FactionAction {
    override def self : Caster

    def question(implicit game : Game) = "Repertoire".styled(self)
}



case class Mudmen(f : Caster) extends PawnHireling {
    val name = "Mudman"
    val short = f.short + "" + "MDM"
    val style = f.style

    val demoted = false
    val setup = false

    val clashKey = XC

    override val transports : $[$[Transport]] = $($(FreeMove))

    def pieces(options : $[Meta.O]) = Mudman *** 1
}

case object Mudman extends Pawn with Movable with Attacking with Tenacious


case class CasterAttackAction(self : Caster, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with XCDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class CasterMoveAction(self : Caster, l : $[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with XCDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class CasterBuildAction(self : Caster, l : $[Clearing]) extends OptionAction("Build", School.of(self), School.imgd(self)) with XCDaylightQuestion with Soft with Only[BuildSchoolClearingAction] { def tag = implicitly }
case class CasterRecruitAction(self : Caster, n : Int, l : $[Clearing]) extends OptionAction("Recruit", l.num.times(self.warrior.imgd(self))) with XCDaylightQuestion with Soft with Only[PlacePieceClearingsAction] { def tag = implicitly }

case class BuildSchoolClearingAction(self : Caster, c : Clearing) extends BaseAction("Build", School.of(self), "in")(c)

case class BattleSpellAction(self : Caster, then : ForcedAction) extends ForcedAction

case class DazzleAction(self : Caster, then : ForcedAction) extends OptionAction(Dazzle.of(self), Image(self.short + "-magic", styles.dt.token)) with XCSpellQuestion

case class PetrifyMainAction(self : Caster, l : $[Clearing], then : ForcedAction) extends OptionAction(Petrify.of(self), Image(self.short + "-magic", styles.dt.token)) with XCSpellQuestion with Soft with Only[PetrifyAction] { def tag = implicitly }
case class PetrifyAction(self : Caster, c : Clearing, e : Faction, p : Piece, then : ForcedAction) extends ForcedAction
case class PetrifyPlaceAction(self : Caster, c : Clearing, then : ForcedAction) extends ForcedAction

case class RegrowthMainAction(self : Caster, l : $[Clearing], then : ForcedAction) extends OptionAction(Regrowth.of(self), 2.times(Image(self.short + "-magic", styles.dt.token))) with XCSpellQuestion with Soft with Only[RegrowthAction] { def tag = implicitly }
case class RegrowthAction(self : Caster, a : Clearing, b : Clearing, then : ForcedAction) extends BaseAction(Regrowth.of(self))(a, dt.Arrow, b)

case class FireballMainAction(self : Caster, l : $[Clearing], dazzle : Boolean, then : ForcedAction) extends OptionAction(Fireball.of(self), dazzle.$("with", Dazzle.of(self)), (2 + dazzle.??(1)).times(Image(self.short + "-magic", styles.dt.token))) with XCSpellQuestion with Soft with Only[FireballAction] { def tag = implicitly }
case class FireballAction(self : Caster, c : Clearing, dazzle : Boolean, then : ForcedAction) extends BaseAction(Fireball.of(self), dazzle.$("with", Dazzle.of(self)))(c)
case class FireballRolledAction(f : Caster, c : Clearing, hi : Int, lo : Int, dazzle : Boolean, then : ForcedAction) extends RolledAction[Int] { def rolled = $(hi, lo) }
case class FireballDamageAction(self : Caster, dealer : Faction, c : Clearing, l : $[Faction], then : ForcedAction) extends BaseAction(Fireball.of(self), "damages")(l.but(self)./(_.elem).comma)

case class TransformMainAction(self : Caster, l : $[Clearing], then : ForcedAction) extends OptionAction(Transform.of(self), Image(self.short + "-magic-q", styles.dt.token)) with XCSpellQuestion with Soft with Only[TransformAction] { def tag = implicitly }
case class TransformClearingAction(self : Caster, c : Clearing, then : ForcedAction) extends BaseAction(Transform.of(self))(c) with Soft
case class TransformTargetAction(self : Caster, c : Clearing, e : Faction, pp : $[Piece], then : ForcedAction) extends BaseAction(Transform.of(self), "in", c)(pp./(_.of(e)).comma) with Soft
case class TransformAction(self : Caster, c : Clearing, e : Faction, pp : $[Piece], l : $[Figure], then : ForcedAction) extends BaseAction(Transform.of(self), pp./(_.of(e)).comma, "from", c, "to")(l./(x => x.piece.of(x.faction)).comma)
case class TransformPlaceAction(self : Caster, c : Clearing, l : $[Figure], then : ForcedAction) extends ForcedAction

case class TeleportMainAction(self : Caster, l : $[Clearing], then : ForcedAction) extends OptionAction(Teleport.of(self), Image(self.short + "-magic-q", styles.dt.token)) with XCSpellQuestion with Soft with Only[TeleportAction] { def tag = implicitly }
case class TeleportFromAction(self : Caster, from : Clearing, then : ForcedAction) extends BaseAction(Teleport.of(self), "from")(from) with Soft
case class TeleportToAction(self : Caster, from : Clearing, to : Clearing, then : ForcedAction) extends BaseAction(Teleport.of(self), "from", from, "to")(to) with Soft
case class TeleportAction(self : Caster, from : Clearing, to : Clearing, e : Faction, l : $[Figure], then : ForcedAction) extends ForcedAction
case class TeleportConsentAction(self : Caster, from : Clearing, to : Clearing, e : Faction, l : $[Figure], then : ForcedAction) extends ForcedAction
case class TeleportNoConsentAction(self : Caster, from : Clearing, to : Clearing, e : Faction, l : $[Figure], then : ForcedAction) extends ForcedAction

case class SummonMainAction(self : Caster, l : $[Clearing], then : ForcedAction) extends OptionAction(Summon.of(self), 3.times(Image(self.short + "-magic", styles.dt.token))) with XCSpellQuestion with Soft with Only[SummonAction] { def tag = implicitly }
case class SummonAction(self : Caster, c : Clearing, then : ForcedAction) extends BaseAction(Summon.of(self), "in")(c)

case class ResurrectAction(self : Caster, c : Clearing, e : Faction, l : $[Piece], then : ForcedAction) extends BaseAction(Resurrection.of(self), 3.times(Image(self.short + "-magic", styles.dt.token)))(l./(_.of(e)).comma, "in", c)
case class ResurrectIgnoreAction(self : Caster, e : Faction, then : ForcedAction) extends BaseAction()("Skip")

case class RollMudmanAction(self : Caster, dazzle : Boolean) extends BaseAction(Mudman.of(self))("Roll".hl, dazzle.$("with", Dazzle.of(self), Image(self.short + "-magic", styles.dt.token)))
case class RolledMudmanSuitAction(self : Caster, dazzle : Boolean, s : BaseSuit) extends RandomAction[BaseSuit] { def random = s }
case class RolledMudmanHitAction(self : Caster, dazzle : Boolean, s : BaseSuit, n : Int) extends RandomAction[Int] { def random = n }
case class MoveMudmanAction(self : Caster, dazzle : Boolean, s : BaseSuit, n : Int, from : Clearing, to : Clearing) extends BaseAction(Mudman.of(self), (n > 1).?(("deals", n.hits, "in")), (n == 0).?("moves to"))(to)
case class MudmanHitAction(self : Caster, dazzle : Boolean, s : BaseSuit, n : Int, c : Clearing) extends ForcedAction

case class BrewMainAction(self : Caster, c : Clearing) extends BaseAction("Brew")(c) with Soft with Only[BrewAction] { def tag = implicitly }
case class BrewAction(self : Caster, c : Clearing, d : DeckCard) extends BaseAction("Discard", implicit g => c.cost.elem, "card")(d.img) with ViewCard

case class ResearchAction(self : Caster, s : Spell) extends BaseAction("Research")(s.img) with ViewSpell


case class CasterDoneAction(f : Caster, then : ForcedAction) extends ForcedAction

case class EveningExtraDrawMainAction(f : Caster, then : ForcedAction) extends ForcedAction with Soft
case class EveningExtraDrawAction(f : Caster, l : $[Clearing], then : ForcedAction) extends ForcedAction

case object Extra extends Message {
    def elem(implicit game : Game) = "extra".txt
}


class CasterPlayer(val faction : Caster)(implicit val game : Game) extends FactionState {
    var growth : $[(Clearing, Clearing)] = $

    var acted = 0

    var mana = 0

    var brewed : $[Clearing] = $

    var refused : $[Faction] = $

    var spells = $[Spell](Petrify)

    def craft = all(School)./(_.asset)
}


object CasterExpansion extends FactionExpansion[Caster] {
    def spells(f : Caster, l : $[Clearing], then : ForcedAction)(implicit game : Game, ask : ActionCollector) {
        if (f.spells.has(Dazzle)) {
            then match {
                case BattleSpellAction(_, BattleStartedAction(b)) if b.defender == f || b.attacker == f =>
                    + DazzleAction(f, then)
                        .!(f.mana < 1, "no mana")
                case _ =>
            }
        }

        if (f.spells.has(Petrify)) {
            val l0 = l
            val l1 = f.pool(Statue).??(l0)
            val l2 = l1.%(f.at(_).use(l => l.has(School).not && l.has(Statue).not))
            val l3 = l2.%(c => f.enemies.exists(e => e.at(c).of[Warrior].notOf[Tenacious].any))
            val l4 = l3.%(f.canPlace)

            + PetrifyMainAction(f, l4, then)
                .!(l0.none, "no presence")
                .!(l1.none, "max")
                .!(l2.none, "schools")
                .!(l3.none, "no enemies")
                .!(l4.none, "can't place")
                .!(f.mana < 1, "no mana")
        }

        if (f.spells.has(Regrowth)) {
            val l0 = l.%(f.at(_).has(f.warrior))
            val l1 = (f.growth.num < 3).??(l0)

            + RegrowthMainAction(f, l1, then)
                .!(l0.none, "no presence")
                .!(l1.none, "max")
                .!(f.mana < 2, "no mana")
        }

        if (f.spells.has(Fireball) && game.current == f) {
            val ee = f.enemies
            val l0 = l.%(f.at(_).has(f.warrior))
            val l1 = l0.%(c => ee.exists(e => e.present(c)))
            val l2 = l1.%(c => ee.exists(e => f.canAttack(c)(e) && f.canRemove(c)(e)))

            + FireballMainAction(f, l2, false, then)
                .!(l0.none, "no warriors")
                .!(l1.none, "no enemies")
                .!(l2.none, "can't attack")
                .!(f.mana < 2, "no mana")

            if (f.spells.has(Dazzle)) {
                + FireballMainAction(f, l2, true, then)
                    .!(l0.none, "no warriors")
                    .!(l1.none, "no enemies")
                    .!(l2.none, "can't attack")
                    .!(f.mana < 3, "no mana")
            }
        }

        if (f.spells.has(Summon)) {
            val l0 = l
            val l1 = l0.%(f.canPlace)

            + SummonMainAction(f, l1, then)
                .!(Mudmen(f).presence.of[Clearing].any, "summoned")
                .!(l0.none, "no presence")
                .!(l1.none, "can't place")
                .!(f.mana < 3, "no mana")
        }

        if (f.spells.has(Transform)) {
            val ee = f.enemies
            val l0 = l
            val l1 = l0.%(f.canPlace)
            val l2 = l1.%(c => ee.exists(e => e.at(c).of[Warrior].any))
            val l3 = l2.%(c => ee.exists(e => f.canRemove(c)(e)))

            + TransformMainAction(f, l3, then)
                .!(f.used.has(Transform), "once per turn")
                .!(l0.none, "no presence")
                .!(l1.none, "can't place")
                .!(l2.none, "no enemies")
                .!(l3.none, "can't remove")
                .!(f.mana < 1, "no mana")
        }

        if (f.spells.has(Teleport)) {
            val l0 = l

            + TeleportMainAction(f, l0, then)
                .!(l0.none, "no presence")
                .!(f.mana < 1, "no mana")
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Caster) =>
            game.states += f -> new CasterPlayer(f)

            game.states += Mudmen(f) -> new PawnHirelingState(Mudmen(f))

            game.hirelings :+= Mudmen(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Caster) =>
            SetupFactionsAction

        // HELPER
        case BattlePostHitInAction(b, e, f : Caster, Statue, then) =>
            e.log("toppled", Statue.of(f))
            then

        case BattlePostHitInAction(b, e, f : Caster, School, then) =>
            e.log("disaccredited", School.of(f))
            then

        case BattlePostHitInAction(b, e, f : Caster, Wizard, then) =>
            e.log("blew up", Wizard.of(f))
            then

        case BattleSpellAction(f, then) =>
            f.used :+= BattleSpell

            then

        case ForcedRemoveProcessAction(f : Caster, then) if f.mana >= 3 && f.spells.has(Resurrection) && f.used.has(Resurrection).not =>
            Ask(f)
                .some(board.clearings.%(f.canPlace).%(f.present))(c => factions./~(e => e.limbo(c).$.pieces.of[Warrior].use(l => l.combinations(1).$ ++ l.combinations(2).$)./(ResurrectAction(f, c, e, _, then))))
                .skip(then)

        case ResurrectAction(h, c, f, l, then) =>
            h.mana -= 3

            f.limbo(c) --> l --> c

            h.used :+= Resurrection

            h.log("resurrected", l./(_.of(f)), "in", c)

            then

        // TURN
        case BirdsongNAction(30, f : Caster) =>
            f.growth = $

            Mudmen(f).presence.foreach { r => Mudmen(f).from(r) --> Mudmen(f).reserve }

            Next

        case BirdsongNAction(50, f : Caster) =>
            f.oscore(f.all(Statue).num)("for", f.all(Statue).num.hl, Statue.nof(f.all(Statue).num)(f))

            f.oscore(f.all(School).num /↓ 3)("for", f.all(School).num.hl, School.nof(f.all(School).num)(f))

            Next

        case BirdsongNAction(70, f : Caster) =>
            XCraftMainAction(f)

        case DaylightNAction(40, f : Caster) if f.all(School).none =>
            f.mana += 1

            f.log("gained", 1.hlb, "magic")

            Ask(f).each(clearings.%(f.canPlace))(PlacePiecesAction(f, _, min(3, f.pooled(Wizard)).times(Wizard), Next))

        case DaylightNAction(60, f : Caster) =>
            implicit val ask = builder

            if (f.acted < 4) {
                val att = clearings.%(f.canAttackIn)
                + CasterAttackAction(f, att).!(att.none)

                val mvv = f.moveFrom.of[Clearing]
                + CasterMoveAction(f, mvv).!(mvv.none)

                val sch = f.all(School).distinct.%(f.canPlace)
                + CasterRecruitAction(f, min(sch.num, f.pooled(f.warrior)), sch).!(f.pool(f.warrior).not, "max").!(sch.none, "nowhere")

                val b0 = clearings
                val b1 = b0.%(f.at(_).has(Statue).not)
                val b2 = b1.%(f.rules)
                val b3 = b2.%(f.canPlace)
                val b4 = b3.%(f.canBuild)

                + CasterBuildAction(f, b4)
                    .!(f.pool(School).not, "max")
                    .!(b1.none, "no place")
                    .!(b2.none, "no rule")
                    .!(b3.none, "can't place")
                    .!(b4.none, "no slot")
            }

            spells(f, f.presence.of[Clearing], Repeat)

            + EndTurnSoftAction(f, "Turn", ForfeitActions(4 - f.acted))

            ask(f).daylight(f)

        case CasterAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), CasterDoneAction(f, Repeat))

        case CasterMoveAction(f, l) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), CasterDoneAction(f, Repeat))

        case CasterBuildAction(f, l) =>
            Ask(f).each(l)(c => BuildSchoolClearingAction(f, c)).cancel

        case CasterRecruitAction(f, n, l) =>
            val ll = l.combinations(n).$.%(x => l.diff(x).distinct.num == l.diff(x).num)

            Ask(f).each(ll)(l => PlacePieceClearingsAction(f, l, f.warrior, CasterDoneAction(f, Repeat))).ocancel

        case BuildSchoolClearingAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.log("built", School.of(f), "in", c)

            f.reserve --> School --> c

            CasterDoneAction(f, Repeat)

        case CasterDoneAction(f, then) =>
            f.acted += 1

            then

        // DAZZLE
        case DazzleAction(f, then) =>
            f.mana -= 1

            f.log("cast", Dazzle.of(f))

            f.used :+= Dazzle

            then

        // PETRIFY
        case PetrifyMainAction(f, l, then) =>
            implicit def convert(x : Figure, s : Boolean) = FigureRemove(x).elem(s)(styles.iii)

            val enemies = f.enemies.of[WarriorFaction]

            val ll = l./{ c =>
                c -> enemies.%(f.canRemove(c))./~{ e => e.from(c).$.%(_.piece.is[Warrior]).%(_.piece.is[Tenacious].not) }
            }

            val cc = ll./~((c, xx) => xx./(_ -> c)).toMap

            val xx = ll.rights./~(x => x)

            YYSelectObjectsAction(f, xx)
                .withGroup(f.elem ~ " casts " ~ Petrify.of(f))
                .withBreak(n => (n < xx.num && (n == 0 || cc(xx(n)) != cc(xx(n - 1)))).?(HorizontalBreak ~ Gap ~ HorizontalBreak ~ Gap ~ HorizontalBreak ~ Gap ~ HorizontalBreak ~ cc(xx(n)).elem ~ HorizontalBreak))
                .withSplit(xx./(_ => 1))
                .withThen(x => PetrifyAction(f, cc(x), x.faction, x.piece, then))(x => "Petrify " ~ x.piece.of(x.faction) ~ " in " ~ cc(x).elem)("Petrify")
                .withExtra($(CancelAction))

        case PetrifyAction(f, c, e, p, then) =>
            f.mana -= 1

            f.log("petrified", p.of(e), "in", c)

            TryForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), Sanctifying, PetrifyPlaceAction(f, c, ForcedRemoveFinishedAction(e, then)), then)

        case PetrifyPlaceAction(f, c, then) =>
            f.reserve --> Statue --> c

            then

        // REGROWTH
        case RegrowthMainAction(f, l, then) =>
            Ask(f).each(clearings.combinations(2).$.%(_.intersect(l).any)./(l => l(0) -> l(1)).%((a, b) => game.connected(a).has(b)))({ case (a, b) => RegrowthAction(f, a, b, then) }).needOk.cancel

        case RegrowthAction(f, a, b, then) =>
            f.mana -= 2

            f.growth +:= (a, b)

            f.log("placed", Growth.of(f), "on the path between", a, "and", b)

            then

        // FIREBALL
        case FireballMainAction(f, l, dazzle, then) =>
            Ask(f).each(l)(c => FireballAction(f, c, dazzle, then)).needOk.cancel

        case FireballAction(f, c, dazzle, then) =>
            f.mana -= 2

            f.log("cast", Fireball.of(f), dazzle.$("with", Dazzle.of(f)), "in", c)

            Roll[Int](D4 :: D4, l => FireballRolledAction(f, c, l.max, l.min, dazzle, then), Fireball.of(f) ~ " in " ~ c.elem)

        case FireballRolledAction(f, c, hi, lo, dazzle, then) =>
            f.log("rolled", hi.roll, lo.roll)

            if (hi > 0) {
                val ee = f.enemies.%(_.present(c)).%(f.canAttack(c)).%(f.canRemove(c))

                val t = ee./~(e => e.from(c).$./(_ => e) ++ e.is[Hero].$(e, e))

                Ask(f).each(t.combinations(min(t.num, hi)).$)(l => FireballDamageAction(f, f, c, l ++ (lo - dazzle.??(1)).times(f), then))
            }
            else
                then

        case FireballDamageAction(f, d, c, e :: rest, then) =>
            BattleAssignHitsAction(e, Battle(c, d, None, e, Some(f), None, None, 0, 0, 0, DummyAction), 1 + rest.count(e), 0, ForceAction(FireballDamageAction(f, d, c, rest.but(e), ForcedRemoveFinishedAction(e, then))))

        case FireballDamageAction(f, d, c, Nil, then) =>
            then

        // TRANSFORM
        case TransformMainAction(f, l, then) =>
            Ask(f).each(l)(TransformClearingAction(f, _, then)).cancel

        case TransformClearingAction(f, c, then) =>
            val ee = f.enemies.%(_.at(c).of[Warrior].notOf[Tenacious].any).%(f.canRemove(c))

            Ask(f).some(ee)(e => {
                val l = e.at(c).of[Warrior].notOf[Tenacious]
                val ll = l.intersect(l.distinct./~(f.mana.times))
                val limit = min(f.mana, factions.but(f).but(e)./(_.reserve.$.%(_.piece.is[Warrior]).%(_.piece.is[Tenacious].not).num).sum)
                1.to(limit)./~(n => ll.combinations(n)).map(TransformTargetAction(f, c, e, _, then))
            }).cancel

        case TransformTargetAction(f, c, e, pp, then) =>
            val rr = factions.but(f).but(e)

            val tt = rr.combinations(2).$./(ff => ff(0) -> ff(1))

            val ww = rr./(r => r -> r.reserve.$.%(_.piece.is[Warrior]).%(_.piece.is[Tenacious].not)./(x => (x.faction, x.piece))).toMap

            val nn = rr./(r => r -> ww(r).num).toMap

            val ll = rr./~(r => ww(r)).combinations(pp.num).$.%(l => l.lefts.use(l => tt.all((a, b) => (l.count(a) - l.count(b)).abs <= 1 || l.count(a) == min(nn(a), nn(b)) || l.count(b) == min(nn(a), nn(b)))))

            Ask(f).each(ll./(_.map(x => Figure.fromTuple(x))))(l => TransformAction(f, c, e, pp, l, then)).cancel

        case TransformAction(f, c, e, pp, ll, then) =>
            f.mana -= pp.num

            f.used :+= Transform

            f.log("cast", Transform.of(f))

            f.log("swapped", pp./(_.of(e)).comma)

            pp.foldLeft[ForcedAction](TransformPlaceAction(f, c, ll, then))((q, p) => TryForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), NoMessage, q, ForcedRemoveFinishedAction(e, then)))

        case TransformPlaceAction(f, c, l, then) =>
            l.foreach { x =>
                x.faction.reserve --> x.piece --> c
            }

            f.log("placed", l./(x => x.piece.of(x.faction)).comma, "in", c)

            then

        // TELEPORT
        case TeleportMainAction(f, l, then) =>
            Ask(f).each(l)(TeleportFromAction(f, _, then)).cancel

        case TeleportFromAction(f, from, then) =>
            Ask(f).each(clearings.but(from))(TeleportToAction(f, from, _, then)).cancel

        case TeleportToAction(f, from, to, then) =>
            implicit def convert(x : Figure, s : Boolean) = FigureRemove(x).elem(s)(styles.iii)

            val ee = f.refused.diff(f.refused.distinct).diff(f.refused.distinct)

            val l = factions.diff(ee).%(_.canPlace(to))./~(_.from(from).$)

            XXSelectObjectsAction(f, l)
                .withGroup(f.elem, "teleports from", from, "to", to)
                .withGrouping(_.faction)(_.faction.elem)
                .withRule(_
                    .upTo(f.mana)
                    .all(_./(_.faction).distinct.num <= 1)
                    .each(_.piece.is[Tenacious].not)
                    .each(x => l.takeWhile(_ != x).%(_.piece == x.piece).%(_.faction == x.faction).num < 8)
                    .all(l => l.none || {
                        val e = l./(_.faction).distinct.only
                        val o = e.at(to)
                        val n = l./(_.piece)
                        val u = o ++ n

                        l.%(_.piece.is[Building]).num <= game.freeSlots(to) &&
                        n.of[Base].all(_.suit.matches(to.cost)) &&
                        n.of[Garden].all(_.suit.matches(to.cost)) &&
                        n.of[TradePost].all(_.suit.matches(to.cost)) &&
                        u.of[Keep.type].num <= 1 &&
                        u.of[Roost.type].num <= 1 &&
                        u.of[Sympathy.type].num <= 1 &&
                        u.of[TradePost].num <= 1 &&
                        u.of[Plot].num <= 1 &&
                        u.of[Tunnel.type].num <= 1 &&
                        u.of[Mob.type].num <= 1 &&
                        u.of[Palace.type].num <= 1 &&
                        n.of[PeacefulDDD].all(_.suit.matches(to.cost)) &&
                        n.of[MilitantDDD].all(_.suit.matches(to.cost)) &&
                        u.of[MilitantDDD].num + u.of[PeacefulDDD].num <= 1 &&
                        n.of[PeacefulCCC].all(_.suit.matches(to.cost)) &&
                        n.of[MilitantCCC].all(_.suit.matches(to.cost)) &&
                        u.of[MilitantCCC].num + u.of[PeacefulCCC].num <= 1 &&
                        n.of[PeacefulBBB].all(_.suit.matches(to.cost)) &&
                        n.of[MilitantBBB].all(_.suit.matches(to.cost)) &&
                        u.of[MilitantBBB].num + u.of[PeacefulBBB].num <= 1 &&
                        n.of[PeacefulAAA].all(_.suit.matches(to.cost)) &&
                        n.of[MilitantAAA].all(_.suit.matches(to.cost)) &&
                        u.of[MilitantAAA].num + u.of[PeacefulAAA].num <= 1 &&
                        true
                    })
                )
                .withThen(l => TeleportAction(f, from, to, l./(_.faction).distinct.only, l, then))(l => game.desc("Teleport".hl, l./(_.elem).comma))
                .withExtra($(CancelAction))

        case TeleportAction(f, from, to, e, l, then) =>
            if (l.all(_.faction == f))
                TeleportConsentAction(f, from, to, e, l, then)
            else
                Ask(e)(TeleportConsentAction(f, from, to, e, l, then).as("Teleport".styled(styles.hit))("Teleport", l./(_.elem).comma, "from", from, "to", to))(TeleportNoConsentAction(f, from, to, e, l, then).as("Refuse".hl))

        case TeleportConsentAction(f, from, to, e, l, then) =>
            f.mana -= l.num

            l.foreach { x =>
                if (x.piece == HiddenPlot) {
                    x.faction.as[Mischief].foreach { e =>
                        e.secret += to -> e.secret(from)
                        e.secret -= from
                    }
                }
                if (x.piece == Keep) {
                    x.faction.as[Feline].foreach { e =>
                        e.keep = None
                        e.keep = Some(to)
                    }
                }
            }

            l --> to

            f.log("teleported", l./(_.elem).comma, "from", from, "to", to)

            then

        case TeleportNoConsentAction(f, from, to, e, l, then) =>
            e.log("refused teleporting", l./(_.elem).comma, "from", from, "to", to)

            f.refused :+= e

            then

        // SUMMON
        case SummonMainAction(f, l, then) =>
            Ask(f).each(l)(SummonAction(f, _, then)).cancel

        case SummonAction(f, c, then) =>
            f.mana -= 3

            Mudmen(f).reserve --> c

            f.log("summoned", Mudman.of(f))

            then

        case BetweenTurnAction if factions.of[Caster].%(_.spells.has(Summon)).%(f => game.used.has(Mudmen(f)).not).%(f => Mudmen(f).reserve.none).any =>
            val f = factions.of[Caster].%(_.spells.has(Summon)).%(f => game.used.has(Mudmen(f)).not).%(f => Mudmen(f).reserve.none).head

            game.used :+= Mudmen(f)

            Ask(f)
                .add(RollMudmanAction(f, false))
                .add((f.mana >= 1 && f.spells.has(Dazzle)).?(RollMudmanAction(f, true)))
                .needOk

        case RollMudmanAction(f, dazzle) =>
            if (dazzle) {
                f.mana -= 1

                f.log("cast", Dazzle.of(f))
            }

            Random[BaseSuit](FoxRabbitMouse, s => RolledMudmanSuitAction(f, dazzle, s))

        case RolledMudmanSuitAction(f, dazzle, s) =>
            Random[Int]($(0, 1, 1, 1, 2, 2), n => RolledMudmanHitAction(f, dazzle, s, n))

        case RolledMudmanHitAction(f, dazzle, s, n) =>
            f.log("rolled", s, "and", n.roll)

            val from = Mudmen(f).presence.of[Clearing].only
            val l0 = game.connected(from)
            val l1 = l0.%(_.cost.matched(s))
            val pp = l1./(c => c -> (factions ++ hirelings)./(_.from(c).num).sum).toMap
            val l2 = l1.%(c => l1.all(pp(_) <= pp(c)))

            Ask(f).each(l0)(c => MoveMudmanAction(f, dazzle, s, n, from, c).!(l1.has(c).not).!(l2.has(c).not, "less pieces")).bailout(MudmanHitAction(f, dazzle, s, n, from).as(from)).needOk

        case MoveMudmanAction(f, dazzle, s, n, from, to) =>
            Force(MoveListAction(f, Mudmen(f), $(Roads), NoMessage, from, to, $(Mudman), MudmanHitAction(f, dazzle, s, n, to)))

        case MudmanHitAction(f, dazzle, s, 0, c) =>
            BetweenTurnAction

        case MudmanHitAction(f, dazzle, s, n, c) =>
            val ee = factions.%(_.present(c)).%(Mudmen(f).canAttack(c)).%(Mudmen(f).canRemove(c))

            Force(FireballDamageAction(f, Mudmen(f), c, ee./~(e => (n - (e == f && dazzle).??(1)).times(e)), BetweenTurnAction))

        // EVENING
        case EveningNAction(40, f : Caster) =>
            implicit def descCard(d : Spell) = d.img

            YYSelectObjectsAction(f, f.spellbooks.diff(f.spells))
                .withGroup(f.elem ~ " researches")
                .withRule(s => (s.cost.none && f.pool(Statue).not) || (s.cost.any && s.cost.?(_ <= f.all(School).num)))
                .withThen(s => ForceAction(ResearchAction(f, s)))(s => "Research " ~ s.of(f))("Research")
                .withBail($(Next.as("Skip")))

        case ResearchAction(f, s) =>
            f.spells :+= s

            f.log("researched", s.of(f))

            Next

        case EveningNAction(60, f : Caster) =>
            Ask(f).each(f.presence.of[Clearing])(c => BrewMainAction(f, c).!(f.brewed.has(c), "").!(f.hand.none, "no cards").!(f.hand.%(_.matches(c.cost)).none, "no matching cards").!(f.mana >= 8, "max")).done(Next).evening(f)

        case BrewMainAction(f, c) =>
            Ask(f).each(f.hand)(d => BrewAction(f, c, d).!(d.matches(c.cost).not)).cancel

        case BrewAction(f, c, d) =>
            game.highlights :+= PlaceHighlight($(c))

            f.hand --> d --> discard.quiet

            f.mana += 1

            f.brewed :+= c

            f.log("brewed", "Magic".styled(f), "in", c, "with", d)

            Repeat

        case NightStartAction(f : Caster) =>
            EveningDrawAction(f, 1 + min(2, f.all(Statue).num))

        case EveningHandLimitAction(f : Caster) =>
            HandLimitAction(f, 5, EveningExtraDrawMainAction(f, EveningAfterHandLimitAction(f)))

        case EveningExtraDrawMainAction(f, then) =>
            implicit def convert(c : Clearing, s : Boolean) = FigureRemove(Figure(f, f.warrior, 0)).elem(s)(styles.iii)

            val l = f.all(f.warrior)

            XXSelectObjectsAction(f, l)
                .withGroup(f.elem ~ " removes " ~ Wizard.sof(f) ~ " to draw up to " ~ 3.cards ~ " over limit")
                .withBreak(n => (n < l.num && (n == 0 || l(n) != l(n - 1))).?(HorizontalBreak ~ Gap ~ HorizontalBreak ~ Gap ~ HorizontalBreak ~ Gap ~ HorizontalBreak ~ l(n).elem ~ HorizontalBreak))
                .withSplit(l./(_ => 1))
                .withRule(_.upTo(3))
                .withThen(l => EveningExtraDrawAction(f, l, then))(l => "Draw Cards".hl ~ " " ~ l.num.times(dt.CardBack).merge)
                .withExtra($(then.as("Skip")))

        case EveningExtraDrawAction(f, l, then) =>
            l.distinct.foreach { c =>
                f.from(c) --> l.count(c).times(f.warrior) --> game.recycle

                f.log("removed", l.count(c).times(f.warrior.of(f)).comma, "from", c)
            }

            DrawCardsAction(f, l.num, Extra, AddCardsAction(f, then))

        case FactionCleanUpAction(f : Caster) =>
            f.acted = 0

            f.brewed = $

            f.refused = $

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
