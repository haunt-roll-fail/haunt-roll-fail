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

trait Expedition extends WarriorFaction {
    val clashKey = KI

    val warrior = Badger

    def abilities(options : $[Meta.O]) = $(DevoutKnights, DaylightCraft)

    override val transports : $[$[Transport]] = $($(RuledMove, RelicMove))

    val wst = $(WayStation(Tablet, Jewel), WayStation(Jewel, Tablet), WayStation(Jewel, Idol), WayStation(Idol, Jewel), WayStation(Idol, Tablet), WayStation(Tablet, Idol))

    def pieces(options : $[Meta.O]) = Badger *** 15 ++
        wst ++
        Relic.types./~(rtype => HiddenRelic(rtype) *** 4 ++ $(1, 2, 3, 3)./(n => Relic(rtype, n)))

    val relics = 4.times(Tablet) ++ 4.times(Jewel) ++ 4.times(Idol)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Badger) ++ wst ++ $(1, 2, 3)./~(n => Relic.types./(rtype => Relic(rtype, n)))
}

case object DevoutKnights extends BattleEffect with FactionEffect {
    val name = "Devout Knights"
}

case object Badger extends Warrior {
    def of(f : Expedition, n : Int) = ("Badger" + (n != 1).??("s")).styled(f)
}

case class WayStation(active : RelicType, passive : RelicType) extends Building {
    override def id = "waystation"
    override val name = "Waystation"
    override def of(f : Faction) = name.styled(f) ~ " " ~ active.name.hl ~ "/" ~ passive.name.spn(styles.darken)
    override def imgid(f : Faction) = f.style + "-" + id + "-" + active + "-" + passive
}

trait RelicType extends Record with Elementary {
    val name = toString
    def elem = name.spn
}

case object Tablet extends RelicType
case object Jewel extends RelicType
case object Idol extends RelicType

trait AnyRelic extends Token {
    val rtype : RelicType
}

case class Relic(rtype : RelicType, value : Int) extends AnyRelic with Token with Movable {
    override def of(f : Faction) = rtype.name.styled(f) ~ " " ~ "I".repeat(value).hl
    override def imgid(f : Faction) = f.style + "-" + rtype + "-" + value
}

case class HiddenRelic(rtype : RelicType) extends AnyRelic with Token {
    override def of(f : Faction) = rtype.name.styled(f)
    override def imgid(f : Faction) = f.style + "-" + rtype + "-" + "hidden"
}

object Relic {
    def types = $[RelicType](Tablet, Jewel, Idol)
}

case class Recovered(rtype : RelicType) extends SpecialRegion

case object Enigma extends SpecialRegion


trait Retinue extends Record with Elementary {
    def id : String
}

object Retinue {
    val all = $[Retinue](RetinueMove, BattleThenDelve, MoveOrRecover)

    def of(f : Expedition) = "Retinue".styled(f)
}

case object RetinueMove extends Retinue {
    val id = "move"
    val elem = "Move".hl
}
case object BattleThenDelve extends Retinue {
    val id = "battle"
    val elem = "Battle".hl ~ " then " ~ "Delve".hl
}
case object MoveOrRecover extends Retinue {
    val id = "recover"
    val elem = "Move".hl ~ " or " ~ "Recover".hl
}



case object FaithfulRetainer extends Card with Record {
    def suit = Bird
    def name = "Faithful Retainer"
    def img = Image("faithful-retainer", styles.card)
}

case object KI extends Expedition {
    val name = "Keepers in Iron"
    override def funName = NameReference(name, this) ~ " in Iron"
    val short = "KI"
    val style = "ki"
    val priority = "K"

    def advertising = HiddenRelic(Tablet).img(this) ~ HiddenRelic(Jewel).img(this) ~ HiddenRelic(Idol).img(this)
    def motto = "Delve".styled(this)
}

class ExpeditionPlayer(val faction : Expedition)(implicit val game : Game) extends FactionState {
    board.forests.foreach(c => location(c))

    var encamped : $[Clearing] = $
    var decamped : $[Clearing] = $

    val retinue = Retinue.all./(t => t -> game.cards.another[Card]("retinue-" + t, $(FaithfulRetainer))).toMap
    val complete = Retinue.all./(t => t -> game.cards.another[Card]("complete-" + t, $)).toMap

    val retired = game.cards.another[Card]("retired", $)

    val recovered = Relic.types./(t => t -> location(Recovered(t), u => u.faction == faction && u.piece.as[Relic].?(_.rtype == t))).toMap

    val enigmas = location(Enigma, u => u.faction == faction && u.piece.is[HiddenRelic])

    var values : $[Relic] = $

    def craft = faction.wst./~(w => all(w))./(_.asset)
}

case object RelicMove extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = (f, o) @@ {
        case (f : Expedition, o : Clearing) => f.at(o).of[Warrior].any
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && ({
        val warriors = l.count(Badger)
        val relics = l./~(_.as[Relic]).num

        relics <= warriors
    })

    override def order(l : $[Movable]) : Int = l.count(Badger) * 9 + l.num
    override def sortBy(m : Movable) : Int = (m == Badger).??(-1) + m.as[Relic]./(_.value).|(0)
}


case object Recovering extends Message {
    def elem(implicit game : Game) = "recovering"
}

case object RecoveringSet extends Message {
    def elem(implicit game : Game) = "recovering a set"
}


case class StartingClearingsAction(f : Faction, l : $[Clearing]) extends ForcedAction
case class PlaceStartingPiecesAction(self : Faction, k : Clearing, r : Clearing, l : $[Piece]) extends BaseAction(self, "places", l./(_.of(self)).comma, "in")(r)
case class RelicRandomnessAction(f : Expedition, k : Clearing, r : Clearing, rolled : $[Int]) extends RolledAction[Int]

case class ReinforcementsAction(self : Expedition, c : Clearing) extends BaseAction("Reinforcements", 2.times(self.warrior.imgd(self)).merge)(c)

case class EncampClearingAction(self : Expedition, c : Clearing, l : $[WayStation]) extends BaseAction("Encamp".hl)(c) with Soft
case class EncampAction(self : Expedition, c : Clearing, w : WayStation) extends BaseAction("Encamp".hl, "in", c)(w.of(self), self.warrior.imgd(self), dt.Arrow, w.imgd(self))
case class DecampAction(self : Expedition, c : Clearing, w : WayStation) extends BaseAction("Decamp".hl)(w.of(self), "in", c, w.imgd(self), dt.Arrow, self.warrior.imgd(self))

case class ExpeditionRevealCardMainAction(self : Expedition, s : BaseSuit, then : ForcedAction, alt : UserAction) extends ForcedAction with Soft
case class ExpeditionRevealCardAction(self : Expedition, s : BaseSuit, d : DeckCard, then : ForcedAction) extends BaseAction("Reveal", s.elem, "card")(d.img) with ViewCard

case class ExpeditionRecruitClearingAction(self : Expedition, r : Clearing) extends BaseAction("Recruit in")(r) with Soft
case class ExpeditionRecruitAction(f : Expedition, r : Clearing, l : $[DeckCard]) extends ForcedAction

case class RetinueMoveAction(self : Expedition, d : Card) extends BaseAction("Move".hl)(d.img) with Soft with ViewCard
case class RetinueMoveDoneAction(self : Expedition, d : Card, then : ForcedAction) extends ForcedAction

case class RetinueBattleThenDelveAction(self : Expedition, d : Card) extends BaseAction("Battle".hl, "then", "Delve".hl)(d.img) with Soft with ViewCard
case class RetinueBattleThenDelveDoneAction(self : Expedition, d : Card, then : ForcedAction) extends ForcedAction

case class RetinueDelveAction(self : Expedition, d : Card, c : Clearing, cancel : Boolean, then : ForcedAction) extends BaseAction("Delve".hl, "with", d)(c) with SoftOptional {
    def soft = cancel
}
case class RetinueDelveRelicAction(self : Expedition, d : Card, c : Clearing, r : Forest, x : AnyRelic, cancel : Boolean, then : ForcedAction) extends BaseAction("Delve".hl, "from", c)(x.of(self), x.imgd(self), "in", r)

case class RetinueMoveOrRecoverAction(self : Expedition, d : Card) extends BaseAction("Move".hl, "or", "Recover".hl)(d.img) with Soft with ViewCard
case class RetinueMoveOrRecoverDoneAction(self : Expedition, d : Card, then : ForcedAction) extends ForcedAction
case class RetinueRecoverAction(self : Expedition, d : Card, c : Clearing, then : ForcedAction) extends BaseAction("Recover".hl, "with", d, "in")(c) with Soft with PreMove
case class RetinueRecoverContinueAction(self : Expedition, d : Card, c : Clearing, then : ForcedAction) extends ForcedAction with Soft
case class RetinueRecoverDiscardAction(self : Expedition, d : Card, then : ForcedAction) extends ForcedAction
case class RetinueRecoverRelicAction(self : Expedition, d : Card, c : Clearing, r : Relic, v : Int, first : Boolean, then : ForcedAction)
    extends BaseAction("Recover".hl, "in", c, "(" ~ (v == 0).?("no".spn).|(v.hl) ~ " ruled clearing" ~ (v != 1).??("s") ~ ")")(Image(r.imgid(self))(styles.iii))
    with ViewObject[Figure] { def obj = Figure(self, r, -1) }

case class RetinueAddAction(self : Expedition) extends BaseAction("Retinue".hl)("Add cards".hl) with Soft
case class RetinueShiftAction(self : Expedition) extends BaseAction("Retinue".hl)("Shift a card".hl) with Soft

case class RetinueMainAction(f : Expedition, h : $[Card], q : $[Elem], o : $[Option[Retinue]], max : Int, m : SortedMap[Int, Retinue]) extends ForcedAction with Soft
case class RetinueCardAction(self : Expedition, h : $[Card], q : $[Elem], o : $[Option[Retinue]], max : Int, m : SortedMap[Int, Retinue], n : Int) extends BaseAction(q(n))(h(n).img) with Soft with ViewCard with Selectable with NoClear with NoExplode with LimitedExtra[Retinue] with SkipValidate with ElemWrap {
    val d = h(n)

    def wrap(g : Game)(e : Elem) = Div(values./ { c =>
        val x = (m.contains(n) || m.num < max) && o(n) != Some(c)
        val s = m.get(n) == Some(c) || m.contains(n).not && o(n) == Some(c)
        val p = self.style + "-" + c.id + "-" + h(n).suit.name + s.not.??("-done")
        x.?(OnClick(c, s.?(Image(p, styles.action, styles.selected, xlo.pointer)).|(Image(p, styles.action, xlo.pointer)))).|(Image(p, styles.action))
    }.merge ~ Break ~ m.get(n).any.?(OnClick(m(n), e.styled(xlo.pointer))).|(e), styles.inline, styles.margined)

    def selected = m.contains(n)

    def fromAny(s : Any) = s @@ {
        case c : Retinue => Some(c)
        case _ => None
    }

    def values = Retinue.all

    def update(c : Retinue) = this.copy(m = (m.get(n) == Some(c)).?(m - n).|(m + (n -> c)))
}

case class RetinueExplodeAction(self : Expedition, h : $[Card], o : $[Option[Retinue]], max : Int) extends HiddenChoice with SelfExplode {
    def explode(withSoft : Boolean) = {
        val iii = 1.to(max)./~(h.indices.combinations).flatMap { l =>
            l.foldLeft($(SortedMap[Int, Retinue]()))((mm, n) => mm./~(m => Retinue.all./(c => m + (n -> c))))
        }

        val ii = iii.%(_.any)

        ii./(m => RetinueAction(self, h, m.to(ListMap), o))
    }
}

case class RetinueAction(self : Expedition, h : $[Card], m : ListMap[Int, Retinue], o : $[Option[Retinue]]) extends BaseAction()(o./~(x => x).any.?("Shift").|("Add").hl)

case class ReturnToWoodAction(self : Faction, c : Clearing, e : Expedition, p : Relic, q : Forest, then : ForcedAction) extends BaseAction("Return", p.of(e), "to forest")(implicit g => board.forestName(q))


object ExpeditionExpansion extends FactionExpansion[Expedition] {
    val OrderingDie = Die.range(0 -> 999999)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Expedition) =>
            game.states += f -> new ExpeditionPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Expedition) if options.has(SetupTypeHomelands) =>
            def hasPair(l : $[Clearing]) = l.intersect(l./~(game.connected)).any
            val h = game.homelands
            val hh = h./~(game.connected)
            val hhh = hh./~(game.connected)

            val l = clearings.diff(h)
            val ll = l.diff(board.inner).some.%(hasPair).|(l)
            val lll = ll.diff(hh).diff(hhh).some.%(hasPair).||(ll.diff(hh).some.%(hasPair)).|(ll)

            Ask(f)(lll.combinations(2).$.%(p => game.connected(p(0)).has(p(1)))./(p => StartingClearingsAction(f, p).as(p.comma)(f, "starts in"))).needOk

        case FactionSetupAction(f : Expedition) =>
            StartingCornerAction(f)

        case StartingClearingsAction(f : Expedition, l) =>
            l.foreach { r =>
                game.homelands :+= r

                f.reserve --> 4.times(Badger) --> r

                f.log("placed", 4.times(Badger.of(f)).comma, "in", r)
            }

            Roll[Int](0.until(36)./(_ => OrderingDie), x => RelicRandomnessAction(f, l(0), l(1), x))

        case StartingClearingAction(f : Expedition, c) =>
            Force(PlaceStartingPiecesAction(f, c, c, 4.times(Badger)))

        case PlaceStartingPiecesAction(f : Expedition, k, r, l) =>
            f.reserve --> 4.times(Badger) --> r

            f.log("placed", 4.times(Badger.of(f)).comma, "in", r)

            if (k == r)
                Ask(f)(game.connected(k).diff(board.inner)./(PlaceStartingPiecesAction(f, k, _, 4.times(Badger))))
            else
                Roll[Int](0.until(36)./(_ => OrderingDie), x => RelicRandomnessAction(f, k, r, x))

        case RelicRandomnessAction(f, k, r, l) =>
            val randomness = new Randomness(l)

            f.values = f.pieces(options).of[Relic].shuffleWith(randomness)

            val near = board.forests.%(q => game.fromForest(q).has(r) || game.fromForest(q).has(k)).shuffleWith(randomness)
            val far = board.forests.diff(near).shuffleWith(randomness)

            val forests = near ++ 1.to(12)./~(_ => far)

            var relics = f.pieces(options).of[HiddenRelic].shuffleWith(randomness)

            relics.lazyZip(forests).foreach { case (x, r) =>
                f.reserve --> x --> r
            }

            SetupFactionsAction

        // HELPER
        case BattlePostHitInAction(b, e, f : Expedition, p : Relic, then) =>
            then

        case BattlePostHitInAction(b, e, f : Expedition, p @ Badger, then) =>
            e.log("pinched", p.of(f))
            then

        case BattlePostHitInAction(b, e, f : Expedition, p : WayStation, then) =>
            e.log("marauded", p.of(f))
            then

        case ForcedRemoveAction(e, c, f : Expedition, p : Relic, x, Removing, then) =>
            ForcedRemoveAction(e, c, f, p, x, Recovering, then)

        case ForcedRemoveTargetEffectAction(e, c, f : Expedition, p : Relic, then) =>
            Ask(e, board.forests./(q => ReturnToWoodAction(e, c, f, p, q, then)))

        case ReturnToWoodAction(e, c, f, p, q, then) =>
            e.nscore(1)("returning the relic")(f, ScoredVP, "returning the relic", p.of(f), "to", q)

            f.limbo(c) --> p --> q

            then

        // TURN

        // RESPAWN
        case BirdsongNAction(30, f : Expedition) =>
            if (f.all(f.warrior).none && f.wst./~(w => f.all(w)).none)
                Ask(f)
                    .each(board.clearings.diff(board.inner).%(f.canBuild))(ReinforcementsAction(f, _).!(f.pool(f.warrior).not))
                    .bailw(Next) { f.log("could not get reinforcements") }
            else
                Next

        case ReinforcementsAction(f, c) =>
            val n = min(2, f.pooled(f.warrior))

            f.reserve --> n.times(f.warrior) --> c

            f.log("placed", n.times(f.warrior.of(f)).comma, "in", c)

            Next

        // ENCAMP
        case BirdsongNAction(40, f : Expedition) =>
            val w = f.reserve.pieces.of[WayStation]

            val ww = w.%(ws => w.has(WayStation(ws.passive, ws.active)))

            if (ww.any)
                Ask(f)(f.all(f.warrior).distinct.%(f.canBuild)./(c => EncampClearingAction(f, c, ww).x(f.encamped.has(c))))(f.birdsong)(Next.as("Done"))
            else
                Next

        case EncampClearingAction(f, c, l) =>
            Ask(f)(l./(w => EncampAction(f, c, w)))(f.birdsong).cancel

        case EncampAction(f, c, w) =>
            f.from(c) --> f.warrior --> game.recycle

            f.reserve --> w --> c

            f.encamped :+= c

            f.log("encamped", w.of(f), "in", c)

            Repeat

        // DECAMP
        case BirdsongNAction(50, f : Expedition) =>
            Ask(f)(f.wst./~(w => f.all(w).%(f.canPlace)./(c => DecampAction(f, c, w).x(f.decamped.has(c)).x(f.pool(f.warrior).not, "no warriors"))))(f.birdsong)(Next.as("Done"))

        case DecampAction(f, c, w) =>
            f.from(c) --> w --> f.reserve

            f.reserve --> f.warrior --> c

            f.decamped :+= c

            f.log("decamped", w.of(f), "in", c)

            Repeat

        case BirdsongNAction(60, f : Expedition) =>
            f.encamped = $
            f.decamped = $

            Next

        // RECRUIT
        case BirdsongNAction(70, f : Expedition) =>
            if (f.hand.any && (f.pool(f.warrior) || f.totalWar)) {
                val l = f.wst./~(w => f.all(w)).distinct
                Ask(f)(l./(c => ExpeditionRecruitClearingAction(f, c).!(f.canPlace(c).not, "can't place").!(f.hand.%(_.matches(c.cost)).none))).done(Next).birdsong(f)
            }
            else
                Next

        case ExpeditionRecruitClearingAction(f, c) =>
            val l = f.hand.%(d => d.matches(c.cost))
            val max = min(f.pooled(f.warrior) + f.totalWar.??(999), l.num * 2)
            XXSelectObjectsAction(f, f.hand)
                .withGroup(f.elem ~ " recruits in " ~ c.elem(game) ~ " " ~ (max == 1).?("a ").||((max < l.num).?("up to " ~ max.hl ~ " ")).|("up to " ~ (l.num * 2).hl ~ " ") ~ Badger.of(f, max))
                .withRule(_.upTo(max /↑ 2).each(d => d.matches(c.cost)))
                .withThen(ExpeditionRecruitAction(f, c, _))(l => "Recruit".hl ~ l.any.?(" " ~ min(max, l.num * 2).times(Badger.of(f)).comma))
                .withExtra($(CancelAction, NoHand))

        case ExpeditionRecruitAction(f, c, l) =>
            val a = min(f.pooled(f.warrior), l.num * 2)
            val e = f.pooled(f.warrior) - a

            f.reserve --> a.times(f.warrior) --> c

            f.hand --> l --> discard.quiet

            if (a > 0)
                f.log("recruited", a.times(f.warrior.of(f)).comma, "in", c, "with", l)

            if (f.totalWar)
                f.oscore(e)("recruiting")

            Repeat

        // CRAFT
        case DaylightNAction(30, f : Expedition) =>
            XCraftMainAction(f)

        // MOVE
        case DaylightNAction(40, f : Expedition) =>
            if (f.retinue(RetinueMove).any)
                Ask(f)(f.retinue(RetinueMove)./(RetinueMoveAction(f, _)))(EndTurnSoftAction(f, RetinueMove.elem, ForfeitActions(f.retinue(RetinueMove).num)))(f.daylight)
            else
                Next

        case RetinueMoveAction(f, d) =>
            MoveInitAction(f, f, $, WithCard(d), f.moveFrom.of[Clearing].%(c => d.matches(c.cost)), f.movable, $(CancelAction), RetinueMoveDoneAction(f, d, Repeat))

        case MoveListAction(self, f : Aviary, t, m, from : Clearing, to : Clearing, l, RetinueMoveDoneAction(ff, d, then)) =>
            RetinueMoveDoneAction(ff, d, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case RetinueMoveDoneAction(f, d, then) =>
            f.retinue(RetinueMove) --> d --> f.complete(RetinueMove)

            then

        // DELVE
        case DaylightNAction(50, f : Expedition) =>
            if (f.retinue(BattleThenDelve).any)
                Ask(f)(f.retinue(BattleThenDelve)./(RetinueBattleThenDelveAction(f, _)))(EndTurnSoftAction(f, BattleThenDelve.elem, ForfeitActions(f.retinue(BattleThenDelve).num)))(f.daylight)
            else
                Next

        case RetinueBattleThenDelveAction(f, d) =>
            val l = clearings.%(c => d.matches(c.cost))
            val la = l.%(f.canAttackIn)
            val ld = l.diff(la).%(c => f.rules(c) && f.at(c).of[Warrior].any && board.forests.%(game.fromForest(_).has(c)).%(f.at(_).of[AnyRelic].any).any)

            BattleInitAction(f, f, WithCard(d), la, ld./(RetinueDelveAction(f, d, _, true, Repeat)) ++ $(CancelAction), RetinueBattleThenDelveDoneAction(f, d, Repeat))

        case BattleStartAction(self, f, a, m, c, o, i, RetinueBattleThenDelveDoneAction(ff, d, then)) =>
            RetinueBattleThenDelveDoneAction(ff, d, BattleStartAction(self, f, a, m, c, o, i, ForceAction(RetinueDelveAction(ff, d, c, false, then))))

        case LegalAAAConveneAction(f, h, c, e, RetinueBattleThenDelveDoneAction(ff, d, then)) =>
            RetinueBattleThenDelveDoneAction(ff, d, LegalAAAConveneAction(f, h, c, e, then))

        case RetinueBattleThenDelveDoneAction(f, d, then) =>
            f.retinue(BattleThenDelve) --> d --> f.complete(BattleThenDelve)

            then

        case RetinueDelveAction(f, d, c, cancel, then) =>
            if (f.rules(c) && f.at(c).of[Warrior].any)
                Ask(f)(board.forests.%(game.fromForest(_).has(c))./~(r => f.at(r).of[AnyRelic]./(RetinueDelveRelicAction(f, d, c, r, _, cancel, then))))(cancel.?(CancelAction).|(then.as("Done")))
            else
                then

        case RetinueDelveRelicAction(f, d, c, r, x, cancel, then) =>
            if (cancel)
                f.retinue(BattleThenDelve) --> d --> f.complete(BattleThenDelve)

            f.from(r) --> x --> c

            f.log("delved from", c, "to", r, "for", x.of(f))

            val v = x @@ {
                case x : Relic => x
                case x : HiddenRelic =>
                    val v = f.values.%(_.rtype == x.rtype)(0)
                    f.values :-= v

                    f.from(c) --> x --> f.enigmas
                    f.reserve --> v --> c

                    f.log("found", v.of(f))

                    v
            }

            if (game.fromForest(r).%(f.rules).num < v.value)
                if (d == FaithfulRetainer) {
                    f.complete(BattleThenDelve) --> d --> f.retired
                    f.log("discarded", d)
                }
                else
                    f.complete(BattleThenDelve) --> d --> discard(f)

            then

        // RECOVER
        case DaylightNAction(60, f : Expedition) =>
            if (f.retinue(MoveOrRecover).any)
                Ask(f)(f.retinue(MoveOrRecover)./(RetinueMoveOrRecoverAction(f, _)))(EndTurnSoftAction(f, MoveOrRecover.elem, ForfeitActions(f.retinue(MoveOrRecover).num)))(f.daylight)
            else
                Next

        case RetinueMoveOrRecoverAction(f, d) =>
            val l = f.wst./~(w => f.all(w).%(c => d.matches(c.cost)).%(c => f.at(c).of[Relic].%(_.rtype == w.active).any)).distinct

            MoveInitAction(f, f, $, WithCard(d), f.moveFrom.of[Clearing].%(c => d.matches(c.cost)), f.movable, l./(RetinueRecoverAction(f, d, _, Repeat)) ++ $(CancelAction), RetinueMoveOrRecoverDoneAction(f, d, Repeat))

        case MoveListAction(self, f : Aviary, t, m, from : Clearing, to : Clearing, l, RetinueMoveOrRecoverDoneAction(ff, d, then)) =>
            RetinueMoveOrRecoverDoneAction(ff, d, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case RetinueMoveOrRecoverDoneAction(f, d, then) =>
            f.retinue(MoveOrRecover) --> d --> f.complete(MoveOrRecover)

            then

        case RetinueRecoverAction(f, d, c, then) =>
            val q = clearings.%(o => c.asset.matches(o.cost)).%(f.rules).num

            val t = f.at(c).of[WayStation]./(_.active)
            val r = f.at(c).of[Relic].%(r => t.has(r.rtype))

            Ask(f).each(r)(RetinueRecoverRelicAction(f, d, c, _, q, true, then)).cancel

        case RetinueRecoverContinueAction(f, d, c, then) =>
            val q = clearings.%(o => c.asset.matches(o.cost)).%(f.rules).num

            val t = f.at(c).of[WayStation]./(_.active)
            val r = f.at(c).of[Relic].%(r => t.has(r.rtype))

            Ask(f).each(r)(RetinueRecoverRelicAction(f, d, c, _, q, false, then)).done(Repeat)

        case RetinueRecoverRelicAction(f, d, c, r, v, first, then) =>
            if (first)
                f.retinue(MoveOrRecover) --> d --> f.complete(MoveOrRecover)

            f.from(c) --> r --> f.recovered(r.rtype)

            f.oscore(r.value)("recovering", r.of(f))

            var q : ForcedAction = then

            q = if (r.value > v)
                RetinueRecoverDiscardAction(f, d, q)
            else
                RetinueRecoverContinueAction(f, d, c, q)

            val column = Relic.types.but(r.rtype)./(t => f.recovered(t).num).min >= f.recovered(r.rtype).num

            if (column) {
                var n = 2

                if (options.has(ColumnBonusCardVP)) {
                    q = DrawCardsAction(f, 1, RecoveringSet, AddCardsAction(f, q))
                    n -= 1
                }

                f.oscore(n)("recovering a set")
            }

            q

        case RetinueRecoverDiscardAction(f, d, then) =>
            if (d == FaithfulRetainer) {
                f.complete(MoveOrRecover) --> d --> f.retired

                f.log("let go of", d)
            }
            else
                f.complete(MoveOrRecover) --> d --> discard(f)

            then

        // END
        case DaylightNAction(70, f : Expedition) =>
            f.complete(RetinueMove) --> f.retinue(RetinueMove)
            f.complete(BattleThenDelve) --> f.retinue(BattleThenDelve)
            f.complete(MoveOrRecover) --> f.retinue(MoveOrRecover)

            Next

        case EveningNAction(20, f : Expedition) =>
            clearings.foreach { c =>
                if (f.at(c).of[Warrior].num > 3) {
                    f.from(c) --> f.warrior --> game.recycle
                    f.log("dispatched", f.warrior.of(f), "in", c, "for supplies")
                }
            }

            Next

        case EveningNAction(40, f : Expedition) =>
            Ask(f)
              .add(RetinueAddAction(f).x(Retinue.all./~(f.retinue.apply).num >= 10, "max").x(f.hand.none, "no cards"))
              .add(RetinueShiftAction(f).x(Retinue.all./~(f.retinue.apply).num == 0, "no retinue"))
              .add(Next.as("Skip"))
              .evening(f)

        case RetinueAddAction(f) =>
            RetinueMainAction(f, f.hand.get, f.hand.$.indices./(_ => "Add to " ~ Retinue.of(f)), f.hand.$.indices./(_ => None), 10 - Retinue.all./~(f.retinue.apply).num, SortedMap())

        case RetinueMainAction(f, h, q, o, max, m) =>
            Ask(f)
              .each(0.until(h.num).$)(n => RetinueCardAction(f, h, q, o, max, m, n))
              .add(RetinueAction(f, h, m.to(ListMap), o).x(m.none))
              .add((RetinueExplodeAction(f, h, o, max)))
              .evening(f)
              .cancel

        case RetinueCardAction(f, h, q, o, max, m, _) =>
            RetinueMainAction(f, h, q, o, max, m)

        case RetinueAction(f, h, m, o) =>
            Retinue.all.foreach { r =>
                m.foreach { case (i, rr) =>
                    if (r == rr) {
                        o(i) @@ {
                            case None =>
                                f.hand --> h(i) --> f.retinue(r)
                                f.log("added", h(i), "to", r)
                            case Some(o) =>
                                f.retinue(o) --> h(i) --> f.retinue(r)
                                f.log("shifted", h(i), "from", o, "to", r)
                        }
                    }
                }
            }

            Next

        case RetinueShiftAction(f) =>
            RetinueMainAction(f, Retinue.all./~(r => f.retinue(r)), Retinue.all./~(r => f.retinue(r).indices./(_ => r.elem)), Retinue.all./~(r => f.retinue(r).indices./(_ => Some(r))), 1, SortedMap())

        case NightStartAction(f : Expedition) =>
            val n = 1 + f.wst./~(w => f.all(w)).num

            EveningDrawAction(f, n)

        case _ => UnknownContinue
    }

}
