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

trait CommonAbduct extends WarriorFaction
trait CommonSkunkWarrior { self : Warrior => }
trait CommonSkunkCaptain

trait AbductAAA extends WarriorFaction with CommonAbduct {
    val clashKey = KDvA

    val warrior = SkunkAAA

    def abilities(options : $[Meta.O]) = $(EveningCraft)

    def pieces(options : $[Meta.O]) = SkunkAAA *** 10 ++ SkunkAAACaptain(Birdsong) *** 1 ++ SkunkAAACaptain(Daylight) *** 1 ++ SkunkAAACaptain(Evening) *** 1

    override val transports : $[$[Transport]] = $($(ForestClearingMove, CaptainFirst, CaptainTurf), $(RuledMove, CaptainFirst, CaptainTurf))

    override def getElem : Elem = super.getElem

    override def note : Elem = HorizontalBreak ~ "Version from " ~ "2024-10-22".hh

    def advertising = $(Bag, Bag, Bag)./(_.img).merge

    def motto = "Abduct".styled(this)
}

trait SkunkAAAWarrior extends Warrior with CommonSkunkWarrior
case class SkunkAAACaptain(phase : Phase) extends Warrior with Scoring with CommonSkunkWarrior with CommonSkunkCaptain {
    override def ofg(f : Faction)(implicit g : Game) : Elem = f.as[AbductAAA]./(of).|(super.ofg(f))
    def of(f : AbductAAA)(implicit g : Game) : Elem = f.characters(phase).name.styled(f)

    override def id = "captain-" + phase.toString
    override def name = phase.toString + " Captain"
}

case class CaptainMove(phase : Phase) extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = f @@ {
        case f : AbductAAA => f.find(phase) == o
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && l.has(SkunkAAACaptain(phase))
}

case object CaptainFirst extends Transport {
    override def order(l : $[Movable]) : Int = l.of[SkunkAAACaptain].none.??(999) + l.num
    override def sortBy(m : Movable) : Int = m.is[SkunkAAACaptain].??(-1)
}

case object CaptainTurf extends Transport {
    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = (f, o, d, l) @@ {
        case (f : Faction, o : Region, d : Clearing, l : $[Movable]) => l.of[SkunkAAACaptain].none || f.at(d).of[SkunkAAACaptain].none
        case _ => true
    }
}

case object SkunkAAA extends Warrior with SkunkAAAWarrior {
    override def id = "Skunk"
    override def name = "Skunk"
}

case object KDvA extends AbductAAA {
    val name = "Knaves of the Deepwood"
    override def funName = "Knaves of the " ~ NameReference(name, this)
    val short = "KDvA"
    val style = "KD"
    val priority = "U"
}


case class Cage(region : Forest) extends SpecialRegion

class AbductAAAPlayer(val faction : AbductAAA)(implicit val game : Game) extends FactionState {
    board.forests.foreach(r => location(r))
    board.forests.foreach(r => location(Cage(r), u => u.piece.is[Warrior] && u.piece.is[Tenacious].not))

    val phases : $[Phase] = $(Birdsong, Daylight, Evening)

    var preparing : $[Phase] = $

    val captains : Map[Phase, Figure] = phases./(phase => phase -> reserve.$.%(_.piece == SkunkAAACaptain(phase)).only).toMap

    def find(phase : Phase) : Region = game.pieces.find(captains(phase)).get._2

    var characters : Map[Phase, Character] = Map(Birdsong -> Thief, Daylight -> Tinker, Evening -> Scoundrel)

    var items : Map[Phase, $[Item]] = phases./(phase => phase -> $(Boots, Teapot, Sword, Boots, Bag, Crossbow, Torch, Coins, Hammer).take(0)).toMap

    var incoming : Map[Phase, $[Item]] = phases./(phase => phase -> $()).toMap

    var bag : $[Item] = $

    def allr(p : Piece) : $[Region] = (clearings ++ board.forests)./~(c => at(c).count(p).times(c))

    def craft = $(Birdsong, Daylight, Evening)./~(p => find(p).as[Clearing].%(faction.rules)./~(c => (1 + items(p).count(Hammer)).times(c.asset)))

    def plog(phase : Phase)(s : Any*) = game.log(faction.characters(phase).name.styled(faction), s.$)
}


case object Burning extends Message {
    def elem(implicit game : Game) = "burning"
}

trait AbductDaylightQuestion extends FactionAction {
    override def self : AbductAAA

    def question(implicit game : Game) = Empty
}

case class AbductAAAStartingForestAction(self : AbductAAA, phase : Phase, r : Forest) extends BaseAction(implicit g => SkunkAAACaptain(phase).of(self), "starts in")(implicit g => board.forestName(r))
case class AbductAAAStartingItemAction(self : AbductAAA, phase : Phase, i : Item) extends ForcedAction

case class AbductAAAMainAction(self : AbductAAA, phase : Phase) extends ForcedAction with Soft

case class AbductAAAPrepareMainAction(self : AbductAAA, phase : Phase) extends BaseAction(implicit g => SkunkAAACaptain(phase).of(self), "acts")("Prepare".hl)
case class AbductAAAActMainAction(self : AbductAAA, phase : Phase) extends BaseAction(implicit g => SkunkAAACaptain(phase).of(self), "acts")("Strike".hl) with Soft


case class AbductAAADrawItemAction(self : AbductAAA, phase : Phase, then : ForcedAction) extends ForcedAction
case class AbductAAAItemDrawnAction(self : AbductAAA, phase : Phase, random : Item, then : ForcedAction) extends RandomAction[Item]

case class AbductAAARecruitAction(self : AbductAAA, phase : Phase, then : ForcedAction) extends ForcedAction
case class AbductAAARecruitMainAction(self : AbductAAA, phase : Phase, then : ForcedAction) extends ForcedAction with Soft
case class AbductAAARecruitMoreAction(self : AbductAAA, phase : Phase, l : $[DeckCard], then : ForcedAction) extends ForcedAction

case class AbductAAAAttackMainAction(self : AbductAAA, phase : Phase) extends ForcedAction with Soft


case class AbductAAAItemsMainAction(self : AbductAAA, phase : Phase) extends ForcedAction with Soft

case class AbductAAAUseItemAction(self : AbductAAA, phase : Phase, i : Item) extends ForcedAction

case class AbductAAAStrikeMainAction(self : AbductAAA, c : Clearing, l : $[Faction], then : ForcedAction) extends ForcedAction with Soft
case class AbductAAAStrikeAction(self : AbductAAA, c : Clearing, e : Faction, p : Piece, then : ForcedAction) extends ForcedAction

case class AbductAAABurnMainAction(self : AbductAAA, c : Clearing, l : $[Faction], then : ForcedAction) extends ForcedAction with Soft
case class AbductAAABurnAction(self : AbductAAA, c : Clearing, e : Faction, p : Piece, then : ForcedAction) extends ForcedAction

case class AbductAAAStealMainAction(self : AbductAAA, phase : Phase, l : $[Faction], then : ForcedAction) extends ForcedAction with Soft
case class AbductAAAStealAction(self : AbductAAA, phase : Phase, e : Faction, then : ForcedAction) extends BaseAction(Steal.of(self), "from")(e)

case class AbductAAADayLaborMainAction(self : AbductAAA, phase : Phase, c : Clearing, then : ForcedAction) extends ForcedAction with Soft
case class AbductAAADayLaborAction(self : AbductAAA, phase : Phase, d : DeckCard, then : ForcedAction) extends BaseAction(DayLabor.of(self))(d.img) with ViewCard

case class AbductAAAScorchedEarthMainAction(self : AbductAAA, phase : Phase, c : Clearing) extends ForcedAction with Soft
case class AbductAAAScorchedEarthAction(self : AbductAAA, phase : Phase, c : Clearing) extends BaseAction("Burn", c, "to the ground")(ScorchedEarth.name.styled(styles.hit))
case class AbductAAAScorchedEarthDoneAction(self : AbductAAA, phase : Phase, c : Clearing) extends ForcedAction



case class AbductAAARansomMainAction(self : AbductAAA) extends ForcedAction with Soft
case class AbductAAARansomAction(self : AbductAAA, f : Faction, n : Int) extends ForcedAction
case class AbductAAARansomRolledAction(self : AbductAAA, f : Faction, n : Int, high : Int, low : Int) extends RolledAction[Int] { def rolled = $(high, low) }
case class AbductAAARansomReleaseAction(self : AbductAAA, f : Faction, l : $[Figure]) extends ForcedAction
case class AbductAAARansomPositionAction(self : AbductAAA, f : Faction, l : $[Figure], c : Clearing, d : DeckCard) extends ForcedAction
case class AbductAAARansomReserveAction(self : AbductAAA, f : Faction, l : $[Figure]) extends ForcedAction




case class AbductAAAUseHammersAction(self : AbductAAA, l : $[Phase], then : ForcedAction) extends ForcedAction
case class AbductAAAAcquireAction(self : AbductAAA, p : Phase, i : Item, then : ForcedAction) extends ForcedAction

case class AbductAAAHostagesAction(self : AbductAAA, e : Faction, c : Clearing, l : $[Piece], r : Forest, then : ForcedAction) extends ForcedAction

case class AbductAAARoutAction(e : Faction, f : AbductAAA, c : Clearing, p : SkunkAAACaptain, r : Forest, then : ForcedAction) extends ForcedAction
case class AbductAAAResqueAction(e : Faction, f : AbductAAA, r : Forest, c : Clearing, then : ForcedAction) extends ForcedAction








object AbductAAAExpansion extends FactionExpansion[AbductAAA] {
    override def extraMoveFrom(f : Faction)(implicit game : Game) = f @@ {
        case f : AbductAAA => board.forests
        case _ => $()
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : AbductAAA) =>
            game.states += f -> new AbductAAAPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : AbductAAA) =>
            val l = f.phases.%(p => f.pool(SkunkAAACaptain(p)))

            if (l.any)
                Ask(f)(board.forests.%(f.at(_).none)./(AbductAAAStartingForestAction(f, l.first, _)))
            else {
                val pp = f.phases.%(p => f.items(p).none)

                if (pp.any)
                    Ask(f).each(f.bag)(i => AbductAAAStartingItemAction(f, pp(0), i).as(i, i.img)("Starting item for", SkunkAAACaptain(pp(0)).ofg(f)))
                else
                    SetupFactionsAction
            }

        case AbductAAAStartingItemAction(f, phase, i) =>
            f.bag :-= i

            f.items += phase -> (f.items(phase) :+ i)

            f.plog(phase)("drew", i)

            FactionSetupAction(f)

        case AbductAAAStartingForestAction(f, p, r) =>
            f.reserve --> SkunkAAACaptain(p) --> r
            f.reserve --> f.warrior --> r

            f.log("placed", SkunkAAACaptain(p).of(f), "and", f.warrior.of(f), "in", r)

            f.bag ++= f.characters(p).starting(options)

            FactionSetupAction(f)

        // HELPER
        case BattlePostHitInAction(b, e, f : AbductAAA, SkunkAAA, then) =>
            e.log("isolated", SkunkAAA.of(f))

            then

        case BattlePostHitInAction(b, e, f : AbductAAA, p : SkunkAAACaptain, then) =>
            then

        case ForcedRemoveTargetEffectAction(e, c, f : AbductAAA, p : SkunkAAACaptain, then) =>
            f.limbo(c) --> p --> c

            f.bag ++= f.items(p.phase)
            f.bag ++= f.incoming(p.phase)

            f.items += p.phase -> $
            f.incoming += p.phase -> $

            Ask(e).each(board.forests.%(board.fromForest(_).has(c)))(r => AbductAAARoutAction(e, f, c, p, r, then).as(r)("Send", p.ofg(f), "to"))

        case AbductAAARoutAction(e, f, c, p, r, then) =>
            f.from(c) --> p --> r

            e.log("routed", p.ofg(f), "to", r)

            soft()

            val l = f.from(Cage(r)).$.%(_.faction == e)

            if (l.any) {
                val cc = board.fromForest(r).%(e.rules)

                Ask(e).each(cc)(c => AbductAAAResqueAction(e, f, r, c, then).as(c)("Place resqued " ~ l./(_.elem).comma ~ " in").!(e.canPlace(c).not)).add(then.as("Don't resque"))
            }
            else
                NoAsk(e)(then)

        case AbductAAAResqueAction(e, f, r, c, then) =>
            val l = f.from(Cage(r)).$.%(_.faction == e)

            l --> c

            e.log("resqued", l, "to", c)

            then



        case AbductAAADrawItemAction(f, phase, then) =>
            if (f.bag.any)
                Random[Item](f.bag, AbductAAAItemDrawnAction(f, phase, _, then))
            else {
                log("No more items in the bag")

                then
            }

        case AbductAAAItemDrawnAction(f, phase, i, then) =>
            f.bag :-= i

            f.incoming += phase -> (f.incoming(phase) :+ i)

            f.plog(phase)("drew", i)

            then

        case BattleCleanupAttackerAction(b, f : AbductAAA) =>
            if (f.at(b.clearing).of[SkunkAAACaptain].any) {
                val l = b.defender.from(Limbo(b.clearing)).$.pieces.of[Warrior].notOf[Tenacious]

                if (l.any)
                    Ask(f)
                        .each(board.forests.%(board.fromForest(_).has(b.clearing)))(r => AbductAAAHostagesAction(f, b.defender, b.clearing, l, r, BattleForcedRemoveAction(b)).as(board.forestName(r))("Take", l./(_.of(b.defender)).comma, "hostage to"))
                        .add(BattleForcedRemoveAction(b).as("Don't take hostages"))
                else
                    BattleForcedRemoveAction(b)
            }
            else
                BattleForcedRemoveAction(b)

        case AbductAAAHostagesAction(f, e, c, l, r, then) =>
            e.from(Limbo(c)) --> l --> (f, Cage(r))

            f.log("took hostages to", r)

            then

        case CraftAssignAction(f : AbductAAA, d, all, used, m, then : CraftPerformAction) =>
            soft()

            var u : $[SuitAsset] = used

            val x = (f.frogCraft ++ f.extraCraft).intersect(u)

            if (x.any) {
                f.extraCraft = f.extraCraft.diff(x)

                u = u.diff(x)
            }

            val native = f.phases./~(p => f.find(p).as[Clearing].%(f.rules)./(_.asset)).diff(f.crafted).intersect(u)

            f.crafted ++= native

            u = u.diff(native)

            if (u.any) {
                val q = then.copy(m = WithItemsOutsourcing(u./(_ => Hammer), x))

                val available : $[(Phase, SuitAsset)] = f.phases./~(p => f.find(p).as[Clearing].%(f.rules)./~(c => f.items(p).count(Hammer).times(p -> c.asset)))
                val combinations : Iterator[$[(Phase, SuitAsset)]] = available.combinations(u.num)
                val hammers : $[$[Phase]] = combinations.filter { (l : $[(Phase, SuitAsset)]) =>
                    l.rights.lazyZip(u).forall { case (asset, cost) => asset == cost }
                }.$./(_.lefts).distinct

                if (hammers.none) {
                    warn("Not enough hammers")

                    NoAsk(f)(then)
                }
                else
                    Ask(f).each(hammers)(l => AbductAAAUseHammersAction(f, l, q).as(l.distinct./(p => SkunkAAACaptain(p).ofg(f) ~ " uses " ~ l.count(p).times(Hammer.img)).comma)("Craft using"))
            }
            else
                NoAsk(f)(then)

        case AbductAAAUseHammersAction(f, l, then) =>
            l.foreach { phase =>
                f.items += phase -> (f.items(phase) :- Hammer)

                f.bag :+= Hammer

                f.plog(phase)("used", Hammer.exhaust)
            }

            then

        case AcquireItemsAction(f : AbductAAA, then) =>
            Ask(f).some(f.forTrade)(i => f.phases./(p => AbductAAAAcquireAction(f, p, i.item, then).as("Give", SkunkAAACaptain(p).ofg(f))("Crafted", i, i.img)))

        case AbductAAAAcquireAction(f, phase, i, then) =>
            f.incoming += phase -> (f.incoming(phase) :+ i)

            f.forTrade = f.forTrade :- i.pristine

            f.plog(phase)("got", i)

            if (f.forTrade.any)
                AcquireItemsAction(f, then)
            else
                then

        // MAIN
        case AbductAAAMainAction(f, phase) =>
            Ask(f)
                .add(AbductAAAPrepareMainAction(f, phase))
                .add(AbductAAAActMainAction(f, phase))
                .use(a => (phase == Birdsong).?(a.birdsong(f)).|(a))
                .use(a => (phase == Daylight).?(a.daylight(f)).|(a))
                .use(a => (phase == Evening ).?(a.evening (f)).|(a))

        case AbductAAAPrepareMainAction(f, phase) =>
            f.preparing :+= phase

            AbductAAADrawItemAction(f, phase, AbductAAARecruitAction(f, phase, Next))

        case AbductAAARecruitAction(f, phase, then) =>
            val r = f.find(phase)

            if (f.pool(SkunkAAA)) {
                if (f.canPlace(r)) {
                    f.reserve --> SkunkAAA --> r

                    f.plog(phase)("recuited", SkunkAAA.of(f))

                    if (r.is[Clearing] && then == Next)
                        AbductAAARecruitMainAction(f, phase, then)
                    else
                        then
                }
                else {
                    f.plog(phase)("could not recruit")

                    then
                }
            }
            else {
                if (f.totalWar)
                    f.oscore(1)("recruiting")

                then
            }

        case AbductAAARecruitMainAction(f, phase, then) =>
            val r = f.find(phase).as[Clearing].get

            XXSelectObjectsAction(f, f.hand)
                .withGroup("Recruit more")
                .withRule(_.each(_.matches(r.cost)).upTo(f.pooled(f.warrior) + f.totalWar.??(999)))
                .withThen(AbductAAARecruitMoreAction(f, phase, _, then))(l => game.desc("Recruit", l.num.times(SkunkAAA.of(f)).comma, "with", l))
                .withExtras(then.as("Done"), NoHand)

        case AbductAAARecruitMoreAction(f, phase, l, then) =>
            f.hand --> l --> discard.quiet

            val r = f.find(phase).as[Clearing].get

            var n = 0

            l.foreach { d =>
                if (f.pool(f.warrior))
                    f.reserve --> SkunkAAA --> r
                else
                    n += 1
            }

            f.plog(phase)("recuited", n.times(f.warrior.of(f)).comma, "with", l)

            if (f.totalWar && n < l.num)
                f.oscore(l.num - n)("recruiting")

            then

        case AbductAAAActMainAction(f, phase) =>
            val r = f.find(phase)

            MoveInitAction(f, f, game.transports./($) ** f.transports ** $($(CaptainMove(phase))), NoMessage, $(r), $(r), $(CancelAction, AbductAAAAttackMainAction(f, phase).as("Stay in", r)), AbductAAAAttackMainAction(f, phase))

        case AbductAAAAttackMainAction(f, phase) =>
            val r = f.find(phase)

            BattleInitAction(f, f, NoMessage, $(r).of[Clearing], $(Next.as("Skip")), Next)



        case AbductAAAItemsMainAction(f, phase) =>
            implicit val ask = builder

            val l = f.items(phase)
            val g = f.characters(phase).name.styled(f)

            def use(i : Item) = AbductAAAUseItemAction(f, phase, i)

            if (l.any) {
                val r = f.find(phase)

                val repeat = AbductAAAItemsMainAction(f, phase)

                if (l.has(Boots)) {
                    + MoveInitAction(f, f, game.transports./($) ** f.transports ** $($(CaptainMove(phase))), NoMessage, $(r), $(r), $(CancelAction), use(Boots)).as(Boots.img, dt.Arrow, "Move".styled(f), dt.Move)(g)
                }

                r.as[Clearing].foreach { c =>
                    if (l.has(Sword)) {
                        val att = f.canAttackList(c)
                        + BattleInitAction(f, f, NoMessage, $(c), $(CancelAction), use(Sword)).as(Sword.img, dt.Arrow, "Battle".styled(f), dt.Battle)(g).!(att.none)
                    }

                    val ee = factions.but(f).%(_.present(c))
                    val eeh = ee ++ hirelings.%(_.present(c))
                    val rmm = eeh.%!(f.friends).%(f.canRemove(c))

                    if (l.has(Crossbow)) {
                        + AbductAAAStrikeMainAction(f, c, rmm, use(Crossbow)).as(Crossbow.img, dt.Arrow, "Strike".styled(f))(g).!(rmm.none, "no target")
                    }

                    if (l.has(Torch) && f.characters(phase).special.has(Steal)) {
                        + AbductAAAStealMainAction(f, phase, ee.%(_.hand.any), use(Torch)).as(Torch.img, dt.Arrow, Steal.of(f))(g).!(ee.none, "no target").!(ee.%(_.hand.any).none, "no cards")
                    }

                    if (l.has(Torch) && f.characters(phase).special.has(DayLabor)) {
                        + AbductAAADayLaborMainAction(f, phase, c, use(Torch)).as(Torch.img, dt.Arrow, DayLabor.of(f))(g).!(pile.exists(_.matches(c.cost)).not, "no matching cards in discard")
                    }

                    if (l.has(Torch) && f.characters(phase).special.has(ScorchedEarth)) {
                        + AbductAAAScorchedEarthMainAction(f, phase, c).as(Torch.damage.img, dt.Arrow, ScorchedEarth.of(f))(g).!(!f.canPlace(c), "forbidden").!((factions ++ hirelings).%(_.present(c)).%!(f.canRemove(c)).any, "protector")
                    }

                    if (l.has(Torch)) {
                        val brn = rmm.%(_.at(c).of[Building].any)
                        + AbductAAABurnMainAction(f, c, brn, use(Torch)).as(Torch.img, dt.Arrow, "Burn".styled(f))(g).!(f.rules(c).not).!(brn.none, "no target")
                    }
                }

                if (l.has(Teapot)) {
                    + AbductAAADrawItemAction(f, phase, AbductAAADrawItemAction(f, phase, use(Teapot))).as(Teapot.img, dt.Arrow, "Draw".styled(f), 2.times(Image("item-any", styles.piece)).merge)(g).!(f.bag.none)
                }

                if (l.has(Bag)) {
                    + AbductAAARecruitAction(f, phase, use(Bag)).as(Bag.img, dt.Arrow, "Recruit".styled(f), SkunkAAA.imgd(f))(g).!(f.canPlace(r).not)
                }

                if (l.has(Coins)) {
                    + DrawCardsAction(f, 1, AltInLog(OnTurn(game.turn), NoMessage), AddCardsAction(f, use(Coins))).as(Coins.img, dt.Arrow, "Draw".styled(f), dt.CardBack)(g).!(game.deck.none && game.pile.none)
                }
            }

            ask(f)
                .add(Next.as("Done")(g))
                .use(a => (phase == Birdsong).?(a.birdsong(f)).|(a))
                .use(a => (phase == Daylight).?(a.daylight(f)).|(a))
                .use(a => (phase == Evening ).?(a.evening (f)).|(a))

        case AbductAAAUseItemAction(f, phase, i) =>
            f.items += phase -> (f.items(phase) :- i)

            f.bag :+= i

            f.plog(phase)("used", i.exhaust)

            AbductAAAItemsMainAction(f, phase)

        case AbductAAAStrikeMainAction(f, c, ee, then) =>
            val xx = ee./~(_.from(c).$)

            SelectFiguresAction(f, "Strike".styled(f), xx, $(CancelAction))(_
                .num(1)
                .each(u => u.piece.is[Warrior] || u.faction.at(c).of[Warrior].none)
                .each(u => u.piece == Vagabond || u.piece.is[Tenacious].not)
            )(x => AbductAAAStrikeAction(f, c, x.only.faction, x.only.piece, then))

        case AbductAAAStrikeAction(f, c, e, p, then) =>
            if (e.is[Hero]) {
                BattleAssignHitsAction(e, Battle(c, f, None, e, None, None, None, 0, 0, 0, DummyAction), 1, 0, then)
            }
            else {
                log(p.of(e), "was removed from", c)

                TryForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), Striking, ForcedRemoveFinishedAction(e, then), then)
            }

        case AbductAAABurnMainAction(f, c, ee, then) =>
            val xx = ee./~(_.from(c).$)

            SelectFiguresAction(f, "Burn".styled(f), xx, $(CancelAction))(_
                .num(1)
                .each(u => u.piece.is[Building])
            )(x => AbductAAABurnAction(f, c, x.only.faction, x.only.piece, then))

        case AbductAAABurnAction(f, c, e, p, then) =>
            if (e.is[Hero]) {
                BattleAssignHitsAction(e, Battle(c, f, None, e, None, None, None, 0, 0, 0, DummyAction), 1, 0, then)
            }
            else {
                log(p.of(e), "was removed from", c)

                TryForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), Burning, ForcedRemoveFinishedAction(e, then), then)
            }

        // STEAL
        case AbductAAAStealMainAction(f, phase, l, then) =>
            Ask(f)(l./(AbductAAAStealAction(f, phase, _, then))).cancel

        case AbductAAAStealAction(f, phase, e, then) =>
            f.log("stole a card from", e, "with", Torch.exhaust.elem)

            StealCardAction(f, e, then)

        case AbductAAADayLaborMainAction(f, phase, c, then) =>
            Ask(f).each(pile)(d => AbductAAADayLaborAction(f, phase, d, then).!(d.matches(c.cost).not)).cancel

        case AbductAAADayLaborAction(f, phase, d, then) =>
            pile --> d --> f.hand

            f.log("took", d, "from the dicard pile", "with", Torch.exhaust.elem)

            then

        // SCORCHED EARTH
        case AbductAAAScorchedEarthMainAction(f, phase, c) =>
            Ask(f)(AbductAAAScorchedEarthAction(f, phase, c)).cancel

        case AbductAAAScorchedEarthAction(f, phase, c) =>
            f.items += phase -> (f.items(phase) :- Torch)

            f.plog(phase)("dropped", Torch, "and scorched", c)

            game.ruins -= c

            NukeAction(f, factions.but(f) ++ hirelings, $(c), NukeType.TotalAnnihilation, AbductAAAScorchedEarthDoneAction(f, phase, c))

        case AbductAAAScorchedEarthDoneAction(f, phase, c) =>
            game.scorched :+= c

            AbductAAAItemsMainAction(f, phase)


        // TURN
        case BirdsongNAction(40, f : AbductAAA) =>
            AbductAAAMainAction(f, Birdsong)

        case BirdsongNAction(42, f : AbductAAA) if f.preparing.has(Birdsong).not =>
            AbductAAAItemsMainAction(f, Birdsong)

        // DAYLIGHT
        case DaylightNAction(40, f : AbductAAA) =>
            AbductAAAMainAction(f, Daylight)

        case DaylightNAction(42, f : AbductAAA) if f.preparing.has(Daylight).not =>
            AbductAAAItemsMainAction(f, Daylight)

        // EVENING
        case EveningNAction(40, f : AbductAAA) =>
            AbductAAAMainAction(f, Evening)

        case EveningNAction(42, f : AbductAAA) if f.preparing.has(Evening).not =>
            AbductAAAItemsMainAction(f, Evening)

        case EveningNAction(50, f : AbductAAA) =>
            XCraftMainAction(f)

        case EveningNAction(60, f : AbductAAA) =>
            AbductAAARansomMainAction(f)

        case AbductAAARansomMainAction(f) =>
            val l = board.forests./~(r => f.from(Cage(r)))

            val ee = l./(_.faction).distinct

            if (ee.any)
                Ask(f).each(ee)(e => AbductAAARansomAction(f, e, l.%(_.faction == e).num).as(e)("Ransom Hostages".hh)).needOk
            else
                NoAsk(f)(Next)


        case AbductAAARansomAction(f, e, n) =>
            f.log("ransomed", e)

            Roll[Int](D4 :: D4, l => AbductAAARansomRolledAction(f, e, n, l.max, l.min), f.elem ~ " ransomed " ~ e.elem)

        case AbductAAARansomRolledAction(f, e, n, hi, lo) =>
            f.log("rolled", hi.roll)
            e.log("rolled", lo.roll)

            f.oscore(min(n, hi))("ransoming", e)

            soft()

            if (lo > 0) {
                val l : $[(Forest, Figure)] = board.forests./~(r => f.from(Cage(r)).$.%(_.faction == e)./(r -> _))

                val ll = l.combinations(min(lo, l.num)).distinctBy(l => l./((r, u) => (r, u.piece))).$

                Ask(f).each(ll)(l => AbductAAARansomReleaseAction(f, e, l.rights).as(l./((r, u) => u.elem ~ " in " ~ r.elem).comma)("Release".hh)).needOk
            }
            else {
                NoAsk(f)(Next)
            }

        case AbductAAARansomReleaseAction(f, e, l) =>
            l.foreach { u =>
                f.log("released", u, "from", game.pieces.find(u)./(_._2.as[Cage]./(_.region)))
            }

            if (e.hand.any) {
                val cc = clearings.%(e.rules)

                if (cc.any) {
                   YYSelectObjectsAction(e, e.hand)
                        .withGroup("Place released " ~ l./(_.elem).comma ~ " with")
                        .withRule(d => cc.exists(c => d.matches(c.cost)))
                        .withThensInfo(d => cc./(c => AbductAAARansomPositionAction(f, e, l, c, d).as(l./(_.elem).comma, "in", c, "with", d).!(d.matches(c.cost).not).!(e.canPlace(c).not)))(cc./(c => Info("In", c)))
                        .withExtras(AbductAAARansomReserveAction(f, e, l).as("Don't place"), NoHand)
                }
                else
                    NoAsk(e)(AbductAAARansomReserveAction(f, e, l))
            }
            else
                NoAsk(e)(AbductAAARansomReserveAction(f, e, l))

        case AbductAAARansomPositionAction(f, e, l, c, d) =>
            l.foreach { u =>
                u --> c
            }

            e.hand --> d --> f.hand

            e.log("placed", l, "in", c, "and gave a card to", f)

            f.notify($(ViewCardInfoAction(f, Gave(e, f).elem(game), d)))

            Next

        case AbductAAARansomReserveAction(f, e, l) =>
            l.foreach { u =>
                u --> e.reserve
            }

            Next

        case NightStartAction(f : AbductAAA) =>
            EveningDrawAction(f, clearings.%(f.rules).num)

        case FactionCleanUpAction(f : AbductAAA) =>
            f.phases.foreach { p =>
                f.items += p -> (f.items(p) ++ f.incoming(p))
                f.incoming += p -> $
            }

            f.preparing = $

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
