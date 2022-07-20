package root

import root.gaming._

import colmat._

import hrf.elem._
import root.elem._

import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedMap

trait Expedition extends WarriorFaction {
    val expansion = ExpeditionExpansion

    val warrior = Badger

    val abilities = List(DevoutKnights, DaylightCraft)

    override val transports : List[List[Transport]] = $($(RuledMove, RelicMove))

    val wst = $(WayStation(Tablet, Jewel), WayStation(Jewel, Tablet), WayStation(Jewel, Idol), WayStation(Idol, Jewel), WayStation(Idol, Tablet), WayStation(Tablet, Idol))

    val pieces = Badger * 15 ++ 
        wst ++ 
        Relic.types./~(rt => Relic(rt, None) * 4 ++ $(1, 2, 3, 3)./(n => Relic(rt, Some(n))))
    
    val relics = Tablet * 4 ++ Jewel * 4 ++ Idol * 4
}

case object DevoutKnights extends BattleEffect {
    override def name = "Devout Knights"
}

case object Badger extends Warrior {
    def of(f : Expedition, n : Int) = ("Badger" + (n != 1).??("s")).styled(f)
}

case class WayStation(active : RelicType, passive : RelicType) extends Building {
    override def id = "waystation"
    override def name = "Waystation"
    override def of(f : Faction) = name.styled(f) ~ " " ~ active.name.hl ~ "/" ~ passive.name.take(1111)
    override def imgid(f : Faction) = f.short + "-" + id + "-" + active + "-" + passive
}

trait RelicType extends Record with Elementary {
    val name = toString
    def elem = name.spn
}

case object Tablet extends RelicType
case object Jewel extends RelicType
case object Idol extends RelicType

case class Relic(rt : RelicType, value : Option[Int]) extends Token with Movable {
    override def of(f : Faction) = rt.name.styled(f) ~ value./(n => " " ~ "I".repeat(n).join("").hl)
    override def imgid(f : Faction) = f.short + "-" + rt + "-" + value.|("hidden")
}

object Relic {
    val types = $[RelicType](Tablet, Jewel, Idol)
}

trait Retinue extends Record with Elementary {
    def id : String
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

object Retinue {

    val all = $[Retinue](RetinueMove, BattleThenDelve, MoveOrRecover)

    def of(f : Expedition) = "Retinue".styled(f)
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

    def advertising = Relic(Tablet, None).img(this) ~ Relic(Jewel, None).img(this) ~ Relic(Idol, None).img(this)
    def motto = "Delve".styled(this)
}

class ExpeditionPlayer(val game : Game, val faction : Expedition) extends PlayerState {
    var encamped : List[Clearing] = Nil
    var decamped : List[Clearing] = Nil

    val retinue = Retinue.all./(t => t -> game.cards.another[Card]("retinue-" + t, $(FaithfulRetainer))).toMap
    val complete = Retinue.all./(t => t -> game.cards.another[Card]("complete-" + t, Nil)).toMap
    
    val retired = game.cards.another[Card]("retired", Nil)
    
    val recovered = Relic.types./(t => t -> figures("recovered-" + t, Nil)).toMap

    val enigmas = figures("enigmas", Nil)
    
    var values : List[Relic] = Nil

    def craft = faction.wst./~(w => all(w))./(game.mapping)
}

case object RelicMove extends Transport {
    override def allows(g : Game, f : Faction, o : Region) = (f, o) @@ {
        case (f : Expedition, o : Clearing) => 
            import g._

            f.at(o).warrior.any
        case _ => false
    }
    
    override def allows(g : Game, f : Faction, o : Region, d : Region, l : List[Movable]) = allows(g, f, o, d) && ({
        val warriors = l.count(Badger)
        val relics = l./~(_.relic).num
        
        relics <= warriors
    })
    
    override def order(l : List[Movable]) : Int = l.count(Badger) * 9 + l.num
    override def sortBy(m : Movable) : Int = (m == Badger).??(-1) + m.relic./~(_.value).|(0)
}



object D1000 extends Die[Int](0.until(1000).toList)

case class StartingClearingsAction(f : Faction, l : List[Clearing]) extends ForcedAction
case class RelicRandomnessAction(f : Expedition, k : Clearing, r : Clearing, rolled : List[Int]) extends RolledAction[Int]

case class EncampClearingAction(self : Expedition, c : Clearing, l : List[WayStation]) extends BaseAction("Encamp".hl)(c) with Soft
case class EncampAction(self : Expedition, c : Clearing, w : WayStation) extends BaseAction("Encamp".hl, "in", c)(w.of(self), self.warrior.imgd(self), dt.Arrow, w.imgd(self))
case class DecampAction(self : Expedition, c : Clearing, w : WayStation) extends BaseAction("Decamp".hl)(w.of(self), "in", c, w.imgd(self), dt.Arrow, self.warrior.imgd(self))

case class BadgerRevealCardMainAction(self : Expedition, s : BaseSuit, then : ForcedAction, alt : UserAction) extends ForcedAction with Soft
case class BadgerRevealCardAction(self : Expedition, s : BaseSuit, d : DeckCard, then : ForcedAction) extends BaseAction("Reveal", s.elem, "card")(d.img) with ViewCard

case class BadgerRecruitClearingAction(self : Expedition, r : Clearing) extends BaseAction("Recruit in")(r) with Soft
case class BadgerRecruitAction(f : Expedition, r : Clearing, l : List[DeckCard]) extends ForcedAction

case class RetinueMoveAction(self : Expedition, d : Card) extends BaseAction("Move".hl)(d.img) with Soft with ViewCard
case class RetinueMoveDoneAction(self : Expedition, d : Card, then : ForcedAction) extends ForcedAction

case class RetinueBattleThenDelveAction(self : Expedition, d : Card) extends BaseAction("Battle".hl, "then", "Delve".hl)(d.img) with Soft with ViewCard
case class RetinueBattleThenDelveDoneAction(self : Expedition, d : Card, then : ForcedAction) extends ForcedAction

case class RetinueDelveAction(self : Expedition, d : Card, c : Clearing, cancel : Boolean, then : ForcedAction) extends BaseAction("Delve".hl, "with", d)(c) with Soft
case class RetinueDelveRelicAction(self : Expedition, d : Card, c : Clearing, r : Forest, x : Relic, cancel : Boolean, then : ForcedAction) extends BaseAction("Delve".hl, "from", c)(x.of(self), x.imgd(self), "in", r)

case class RetinueMoveOrRecoverAction(self : Expedition, d : Card) extends BaseAction("Move".hl, "or", "Recover".hl)(d.img) with Soft with ViewCard
case class RetinueMoveOrRecoverDoneAction(self : Expedition, d : Card, then : ForcedAction) extends ForcedAction
case class RetinueRecoverAction(self : Expedition, d : Card, c : Clearing, cancel : Boolean, then : ForcedAction) extends BaseAction("Recover".hl)(c) with Soft
case class RetinueRecoverRelicAction(self : Expedition, d : Card, c : Clearing, r : Relic, q : Int, first : Boolean, then : ForcedAction) extends BaseAction("Recover".hl, "in", c, "(" ~ (q == 0).?("no".spn).|(q.hl) ~ " ruled clearing" ~ (q != 1).??("s") ~ ")")(Image(r.imgid(self))(styles.iii)) with ViewObject[Figure] { def obj = Figure(self, r, -1) }



case class RetinueAddAction(self : Expedition) extends BaseAction("Retinue".hl)("Add cards") with Soft
case class RetinueShiftAction(self : Expedition) extends BaseAction("Retinue".hl)("Shift a card") with Soft


case class RetinueMainAction(f : Expedition, h : List[Card], q : List[Elem], o : List[Option[Retinue]], max : Int, m : SortedMap[Int, Retinue]) extends ForcedAction with Soft
case class RetinueCardAction(self : Expedition, h : List[Card], q : List[Elem], o : List[Option[Retinue]], max : Int, m : SortedMap[Int, Retinue], n : Int) extends BaseAction(q(n))(h(n).img) with Soft with ViewCard with Selectable with NoClear with NoExplode with LimitedExtra[Retinue] with SkipValidate with ElemWrap {
    def d = h(n)

    def wrap(g : Game)(e : Elem) = Div(values./ { c =>
        val x = (m.contains(n) || m.num < max) && o(n) != Some(c)
        val s = m.get(n) == Some(c) || m.contains(n).not && o(n) == Some(c)
        val p = self.style + "-" + c.id + "-" + h(n).suit.name + s.not.??("-done")
        x.?(OnClick(c, s.?(Image(p, styles.action, styles.selected, xstyles.pointer)).|(Image(p, styles.action, xstyles.pointer)))).|(Image(p, styles.action))
    }.merge ~ Break ~ m.get(n).any.?(OnClick(m(n), e.styled(xstyles.pointer))).|(e), styles.inline, styles.margined)
    
    def selected = m.contains(n)
    
    def fromAny(s : Any) = s match {
        case c : Retinue => Some(c)
        case _ => None
    }

    def values = Retinue.all

    def update(c : Retinue) = this.copy(m = (m.get(n) == Some(c)).?(m - n).|(m + (n -> c)))
}
    
case class RetinueExplodeAction(self : Expedition, h : List[Card], o : List[Option[Retinue]], max : Int) extends HiddenChoice with SelfExplode {
    def explode(withSoft : Boolean) = {
        val iii = 1.to(max)./~(h.indices.combinations).flatMap { l =>
            l.foldLeft($(SortedMap[Int, Retinue]()))((mm, n) => mm./~(m => Retinue.all./(c => m + (n -> c))))
        }
        
        val ii = iii.%(_.any)
        
        ii./(m => RetinueAction(self, h, m.to(ListMap), o))
    }
}

case class RetinueAction(self : Expedition, h : List[Card], m : ListMap[Int, Retinue], o : List[Option[Retinue]]) extends BaseAction()(o./~(x => x).any.?("Shift").|("Add").hl)
    
case class EndRetinueSoftAction(self : Expedition, r : Retinue, m : Message) extends BaseAction(None)("End", r) with Soft

case class ReturnToWoodAction(self : Faction, c : Clearing, e : Expedition, p : Relic, q : Forest, then : ForcedAction) extends BaseAction("Return", p.of(e), "to forest")(g => g.board.forestName(q, g))


object ExpeditionExpansion extends Expansion {
    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            // SETUP
            case CreatePlayerAction(f : Expedition) =>
                pstates += f -> new ExpeditionPlayer(game, f)
                FactionInitAction(f)
                
            case FactionSetupAction(f : Expedition) if options.has(SetupTypeHomelands) =>
                def hasPair(l : List[Clearing]) = l.intersect(l./~(board.connected)).any
                val h = homelands
                val hh = h./~(board.connected)
                val hhh = hh./~(board.connected)

                val l = board.clearings.diff(h)
                val ll = l.diff(board.inner).some.%(hasPair).|(l)
                val lll = ll.diff(hh).diff(hhh).some.%(hasPair).||(ll.diff(hh).some.%(hasPair)).|(ll)

                Ask(f)(lll.combinations(2).toList.%(p => board.connected(p(0)).has(p(1)))./(p => StartingClearingsAction(f, p).as(p)(f, "starts in"))).needOk

            case FactionSetupAction(f : Expedition) =>
                StartingCornerAction(f)

            case StartingClearingsAction(f : Expedition, l) =>
                l.foreach { r =>
                    homelands :+= r

                    f.pool.sub(4, Badger) --> r
            
                    f.log("placed", Badger.of(f).repeat(4).comma, "in", r)
                }

                Roll[Int](0.until(36)./(_ => D1000).toList, x => RelicRandomnessAction(f, l(0), l(1), x))
                
            case StartingClearingAction(f : Expedition, c) =>
                Force(PlaceStartingPiecesAction(f, c, c, Badger.repeat(4)))
                
            case PlaceStartingPiecesAction(f : Expedition, k, r, l) =>
                f.pool.sub(4, Badger) --> r
            
                f.log("placed", Badger.of(f).repeat(4).comma, "in", r)
                
                if (k == r) 
                    Ask(f, board.connected(k).diff(board.inner)./(PlaceStartingPiecesAction(f, k, _, Badger.repeat(4))))
                else
                    Roll[Int](0.until(36)./(_ => D1000).toList, x => RelicRandomnessAction(f, k, r, x))

            case RelicRandomnessAction(f, k, r, l) =>
                val randomness = new Randomness(l)
                
                f.values = f.pieces./~(_.relic).%(_.value.any).shuffleWith(randomness)
                
                val near = board.forests.%(q => board.fromForest(q).has(r) || board.fromForest(q).has(k)).shuffleWith(randomness)
                val far = board.forests.diff(near).shuffleWith(randomness)

                val forests = near ++ 1.to(12)./~(_ => far)
                
                var relics = f.pieces./~(_.relic).%(_.value.none).shuffleWith(randomness)
 
                relics.lazyZip(forests).foreach { case (x, r) =>
                    f.pool.one(x) --> r
                }
                
                SetupNextAction

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
                
            case ForcedRemoveEffectAction(e, c, f : Expedition, p : Relic, then) =>
                Ask(e, board.forests./(q => ReturnToWoodAction(e, c, f, p, q, then)))
            
            case ReturnToWoodAction(e, c, f, p, q, then) =>
                e.nscore(1)("returning the relic")(f, ScoredVP, "returning the relic", p.of(f), "to", q)

                f.limbo.one(p) --> q
                then

            // TURN

            // ENCAMP
            case BirdsongNAction(40, f : Expedition) =>
                val w = f.pool./(_.piece)./~{
                    case ws : WayStation => Some(ws)
                    case _ => None
                }
                
                val ww = w.%(ws => w.has(WayStation(ws.passive, ws.active)))

                if (ww.any)
                    Ask(f)(f.all(f.warrior).distinct.%(canBuild(f))./(c => EncampClearingAction(f, c, ww).x(f.encamped.has(c))))(f.birdsong)(Next.as("Done"))
                else
                    Next

            case EncampClearingAction(f, c, l) =>
                Ask(f)(l./(w => EncampAction(f, c, w)))(f.birdsong).cancel

            case EncampAction(f, c, w) =>
                f.at(c).one(f.warrior) --> f.pool

                f.pool.one(w) --> c
                
                f.encamped :+= c

                f.log("encamped", w, "in", c)

                Repeat
                
            // DECAMP
            case BirdsongNAction(50, f : Expedition) =>
                Ask(f)(f.wst./~(w => f.all(w).%(canPlace(f))./(c => DecampAction(f, c, w).x(f.decamped.has(c)).x(f.encamped.has(c), "just camped").x(f.pooled(f.warrior) == 0, "no warriors"))))(f.birdsong)(Next.as("Done"))
                
            case DecampAction(f, c, w) =>
                f.at(c).one(w) --> f.pool

                f.pool.one(f.warrior) --> c
                
                f.decamped :+= c

                f.log("decamped", w, "in", c)
                
                Repeat
                
            case BirdsongNAction(60, f : Expedition) =>
                f.encamped = Nil
                f.decamped = Nil

                Next
            
            // RECRUIT
            case BirdsongNAction(70, f : Expedition) =>
                if (f.hand.any && (f.pooled(f.warrior) > 0 || f.totalWar)) {
                    val l = f.wst./~(w => f.all(w)).distinct
                    Ask(f)(l./(c => BadgerRecruitClearingAction(f, c).x(canPlace(f)(c).not, "can't place").x(f.hand.%(_.suit.m(c.suit)).none)))(f.birdsong)(Next.as("Done"))
                }
                else
                    Next
                    
            case BadgerRecruitClearingAction(f, c) =>
                val l = f.hand.%(d => d.suit.m(c.suit))
                val max = min(f.pooled(f.warrior) + f.totalWar.??(999), l.num * 2)
                XXSelectObjectsAction(f, f.hand)
                    .withGroup(f.elem ~ " recruits in " ~ c.elem(game) ~ " " ~ (max == 1).?("a ").||((max < l.num).?("up to " ~ max.hl ~ " ")).|("up to " ~ (l.num * 2).hl ~ " ") ~ Badger.of(f, max))
                    .withRule(_.upTo((max + 1) / 2).each(d => d.suit.m(c.suit)))
                    .withThen(BadgerRecruitAction(f, c, _))(l => "Recruit".hl ~ l.any.??(" " ~ Badger.of(f).repeat(min(max, l.num * 2)).comma))
                    .withExtra($(CancelAction, NoHand))
                
            case BadgerRecruitAction(f, c, l) =>
                val a = min(f.pooled(f.warrior), l.num * 2)
                val e = f.pooled(f.warrior) - a

                f.pool.sub(a, f.warrior) --> c
                
                f.hand --> l --> discard(f).quiet

                if (a > 0)
                    f.log("recruited", f.warrior.of(f).repeat(a).comma, "in", c, "with", l)

                if (f.totalWar)
                    f.oscore(e)("recruiting")

                Repeat

            // CRAFT
            case DaylightNAction(30, f : Expedition) =>
                XCraftMainAction(f)

            // MOVE
            case DaylightNAction(40, f : Expedition) =>
                if (f.retinue(RetinueMove).any)
                    Ask(f)(f.retinue(RetinueMove)./(RetinueMoveAction(f, _)))(EndRetinueSoftAction(f, RetinueMove, ForfeitActions(f.retinue(RetinueMove).num)))(f.daylight)
                else
                    Next

            case RetinueMoveAction(f, d) =>
                MoveInitAction(f, Nil, WithCard(d), moveFrom(f).%(c => d.suit.m(c.suit)), movable(f), $(CancelAction), RetinueMoveDoneAction(f, d, Repeat))
                
            case MoveListAction(f : Aviary, t, m, from : Clearing, to : Clearing, l, RetinueMoveDoneAction(ff, d, then)) =>
                RetinueMoveDoneAction(ff, d, ForceAction(MoveListAction(f, t, m, from, to, l, then)))
            
            case RetinueMoveDoneAction(f, d, then) =>
                f.retinue(RetinueMove) --> d --> f.complete(RetinueMove)

                then

            // DELVE
            case DaylightNAction(50, f : Expedition) =>
                if (f.retinue(BattleThenDelve).any)
                    Ask(f)(f.retinue(BattleThenDelve)./(RetinueBattleThenDelveAction(f, _)))(EndRetinueSoftAction(f, BattleThenDelve, ForfeitActions(f.retinue(BattleThenDelve).num)))(f.daylight)
                else
                    Next
                    
            case RetinueBattleThenDelveAction(f, d) =>
                val l = clearings.%(c => d.suit.m(c.suit))
                val la = l.%(c => attack(f)(c).any)
                val ld = l.diff(la).%(c => rule(f)(c) && f.at(c).warrior.any && board.forests.%(board.fromForest(_).has(c)).%(f.at(_)./~(_.piece.relic).any).any)
                
                BattleInitAction(f, WithCard(d), la, ld./(RetinueDelveAction(f, d, _, true, Repeat)) ++ $(CancelAction), RetinueBattleThenDelveDoneAction(f, d, Repeat))
                
            case BattleStartAction(f, a, m, c, o, i, RetinueBattleThenDelveDoneAction(ff, d, then)) =>
                RetinueBattleThenDelveDoneAction(ff, d, ForceAction(BattleStartAction(f, a, m, c, o, i, ForceAction(RetinueDelveAction(ff, d, c, false, then)))))

            case RetinueBattleThenDelveDoneAction(f, d, then) =>
                f.retinue(BattleThenDelve) --> d --> f.complete(BattleThenDelve)

                then

            case RetinueDelveAction(f, d, c, cancel, then) =>
                if (rule(f)(c) && f.at(c).warrior.any)
                    Ask(f)(board.forests.%(board.fromForest(_).has(c))./~(r => f.at(r)./~(_.piece.relic)./(RetinueDelveRelicAction(f, d, c, r, _, cancel, then))))(cancel.?(CancelAction).|(then.as("Done")))
                else
                    then

            case RetinueDelveRelicAction(f, d, c, r, x, cancel, then) =>
                if (cancel) 
                    f.retinue(BattleThenDelve) --> d --> f.complete(BattleThenDelve)

                f.at(r).one(x) --> c

                f.log("delved from", c, "to", r, "for", x.of(f))
                
                val v = if (x.value.none) {
                    val v = f.values.%(_.rt == x.rt)(0)
                    f.values :-= v

                    f.at(c).one(x) --> f.enigmas
                    f.pool.one(v) --> c
                    
                    f.log("found", v.of(f))
                    
                    v
                }
                else
                    x

                if (board.fromForest(r).%(rule(f)).num < v.value.get)
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
                    Ask(f)(f.retinue(MoveOrRecover)./(RetinueMoveOrRecoverAction(f, _)))(EndRetinueSoftAction(f, MoveOrRecover, ForfeitActions(f.retinue(MoveOrRecover).num)))(f.daylight)
                else
                    Next
                    
            case RetinueMoveOrRecoverAction(f, d) =>
                val l = f.wst./~(w => f.all(w).%(c => d.suit.m(c.suit)).%(c => f.at(c)./~(_.piece.relic).%(_.rt == w.active).any)).distinct
                
                MoveInitAction(f, Nil, WithCard(d), moveFrom(f).%(c => d.suit.m(c.suit)), movable(f), l./(RetinueRecoverAction(f, d, _, true, Repeat)) ++ $(CancelAction), RetinueMoveOrRecoverDoneAction(f, d, Repeat))
                
            case MoveListAction(f : Aviary, t, m, from : Clearing, to : Clearing, l, RetinueMoveOrRecoverDoneAction(ff, d, then)) =>
                RetinueMoveOrRecoverDoneAction(ff, d, ForceAction(MoveListAction(f, t, m, from, to, l, then)))
            
            case RetinueMoveOrRecoverDoneAction(f, d, then) =>
                f.retinue(MoveOrRecover) --> d --> f.complete(MoveOrRecover)

                then
 
            case RetinueRecoverAction(f, d, c, cancel, then) =>
                val q = clearings.%(_.suit.m(c.suit)).%(rule(f)).num

                val t = f.at(c)./(_.piece)./~{
                    case WayStation(a, _) => Some(a)
                    case _ => None
                }

                val r = f.at(c)./~(_.piece.relic).%(r => t.has(r.rt))

                Ask(f)(r./(RetinueRecoverRelicAction(f, d, c, _, q, cancel, then)))(cancel.?(CancelAction).|(Repeat.as("Done")))
                                                                         
            case RetinueRecoverRelicAction(f, d, c, r, q, first, then) =>
                if (first)
                    f.retinue(MoveOrRecover) --> d --> f.complete(MoveOrRecover)

                val column = f.recovered.view.filterKeys(_ != r.rt).values./(_.num).min > f.recovered(r.rt).num
                
                f.at(c).one(r) --> f.recovered(r.rt)

                f.oscore(r.value.get)("recovering", r.of(f))
                
                if (column)
                    f.oscore(2)("recovering a set")

                if (r.value.get <= q)
                    Force(RetinueRecoverAction(f, d, c, false, then))
                else {
                    if (d == FaithfulRetainer) {
                        f.complete(MoveOrRecover) --> d --> f.retired
                        f.log("discarded", d)
                    }
                    else
                        f.complete(MoveOrRecover) --> d --> discard(f)
                        
                    then
                }

            // END
            case EndRetinueSoftAction(f, r, m) =>
                if (hrf.HRF.flag("fastsetup") && false)
                    Ask(f)(Next.as("End"))
                else
                    WarnAction(f, Next, ForfeitActions(f.retinue(r).num).elem(game), "End".styled(styles.hit), "Continue" ~ " " ~ r.elem)

            case DaylightNAction(70, f : Expedition) =>
                f.complete(RetinueMove) --> f.retinue(RetinueMove)
                f.complete(BattleThenDelve) --> f.retinue(BattleThenDelve)
                f.complete(MoveOrRecover) --> f.retinue(MoveOrRecover)
            
                Next
                
            case EveningNAction(20, f : Expedition) =>
                clearings.foreach { c =>
                    if (f.at(c).warrior.num > 3) {
                        f.at(c).one(f.warrior) --> f.pool
                        f.log("removed", f.warrior.of(f), "in", c)
                    }
                }
 
                Next
    
            case EveningNAction(40, f : Expedition) =>
                Ask(f)(RetinueAddAction(f).x(Retinue.all./~(f.retinue.apply).num >= 10, "max").x(f.hand.none, "no cards"))(RetinueShiftAction(f).x(Retinue.all./~(f.retinue.apply).num == 0, "no retinue"))(f.evening)(Next.as("Skip"))

            case RetinueAddAction(f) =>
                RetinueMainAction(f, f.hand.get, f.hand.get.indices./(_ => "Add to " ~ Retinue.of(f)), f.hand.get.indices./(_ => None), 10 - Retinue.all./~(f.retinue.apply).num, SortedMap())
    
            case RetinueMainAction(f, h, q, o, max, m) =>
                Ask(f, (0.until(h.num)./(n => RetinueCardAction(f, h, q, o, max, m, n)) :+ RetinueAction(f, h, m.to(ListMap), o).x(m.none) :+ RetinueExplodeAction(f, h, o, max)).evening(f).cancel)
                
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
                
            case FactionCleanUpAction(f : Expedition) =>
                EndPlayerTurnAction(f)
                
            case _ => UnknownContinue
        }
    }
}

