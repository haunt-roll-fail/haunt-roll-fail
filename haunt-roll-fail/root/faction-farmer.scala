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

case object Hamster extends Warrior

case object Sheriff extends Warrior with Tenacious

trait Farmer extends WarriorFaction {
    val clashKey = FH

    val warrior = Hamster

    def abilities(options : $[Meta.O]) = $(MorningCraft, HamsterTactics)

    override val transports : $[$[Transport]] = $($(RuledMove, GrainMove))

    def pieces(options : $[Meta.O]) = Hamster *** 15 ++ Sheriff *** 1 ++ Farm *** 3 ++ Windmill *** 3 ++ Grain *** 9

    override def note : Elem = SpacedDash ~ "by " ~ "jgregory17".hl

    def advertising = 5.times(Image(this.short + "-grain", styles.piece)).merge

    def motto = "Grind".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Hamster, Sheriff, Farm, Windmill, Grain)
}

case object Grain extends Token with Movable

case object Farm extends Building
case object Windmill extends Building

case object GrainMove extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = (f, o) @@ {
        case (f : Farmer, o : Clearing) => f.at(o).of[Warrior].any
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && ({
        val warriors = l.of[Warrior].num
        val grains = l./~(_.as[Grain.type]).num

        grains <= warriors
    })

    override def order(l : $[Movable]) : Int = l.count(Hamster) * 9 + l.num
    override def sortBy(m : Movable) : Int = (m == Hamster).??(-1) + m.is[Grain.type].??(1)
}

case object HamsterTactics extends BattleEffect {
    val name = "Hamster Tactics"
}

case object FH extends Farmer {
    val name = "Farmers of Hamstershire"
    override def funName = "Farmers of " ~ NameReference(name, this)
    val short = "FH"
    val style = "fh"
    val priority = "K"
}

trait FHDaylightQuestion extends FactionAction {
    override def self : Farmer

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            1.to(self.acted)./(_ => Image("action-black", styles.action)) ~ (self.acted.until(self.all(Farm).num + 2)./(_ => Image(self.style + "-action", styles.action))),
        styles.margined)
}


case class ProduceMultiGrainAction(self : Farmer, l : $[Clearing]) extends BaseAction("Place", Grain.of(self), "in")(l.comma)

case class FarmerAttackAction(self : Farmer, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with FHDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class FarmerMoveAction(self : Farmer, l : $[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with FHDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class FarmerBuildAction(self : Farmer, p : Piece, l : $[Clearing]) extends OptionAction("Build", implicit g => dt.CardSuit(l./(_.cost).distinct.single.|(AnySuit)), dt.Arrow, p.of(self), p.imgd(self)) with FHDaylightQuestion with Soft with Only[FarmerBuildClearingCardAction] { def tag = implicitly }
case class FarmerRecruitAction(self : Farmer, n : Int, l : $[Clearing]) extends OptionAction("Recruit", l.num.times(self.warrior.imgd(self))) with FHDaylightQuestion with Soft with Only[PlacePieceClearingsAction] { def tag = implicitly }
case class FarmerGrindAction(self : Farmer, l : $[Clearing]) extends OptionAction("Grind", Grain.of(self), Grain.imgd(self), dt.Arrow, 2.vp) with FHDaylightQuestion with Soft with Only[FarmerGrindClearingAction] { def tag = implicitly }
case class FarmerElectAction(self : Farmer, p : Piece, l : $[Clearing]) extends OptionAction("Elect", p.of(self), p.imgd(self)) with FHDaylightQuestion with Soft with Only[FarmerElectClearingCardAction] { def tag = implicitly }

case class FarmerBuildClearingAction(self : Farmer, p : Piece, c : Clearing) extends BaseAction("Build", p.of(self), "in")(c) with Soft
case class FarmerBuildClearingCardAction(self : Farmer, p : Piece, c : Clearing, d : DeckCard) extends BaseAction("Build", p.of(self), "in", c, "with")(d.img) with ViewCard

case class FarmerElectClearingAction(self : Farmer, p : Piece, c : Clearing) extends BaseAction("Elect", p.of(self), "in")(c) with Soft
case class FarmerElectClearingCardAction(self : Farmer, p : Piece, c : Clearing, d : DeckCard) extends BaseAction("Elect", p.of(self), "in", c, "with")(d.img) with ViewCard

case class FarmerGrindClearingAction(self : Farmer, c : Clearing) extends BaseAction("Grind", Grain.of(self), "in")(c)
case class FarmerGrindRollAction(self : Farmer, l : $[Faction], random : BaseSuit) extends RandomAction[BaseSuit]
case class FarmerGrindPlaceAction(f : WarriorFaction, p : Piece, then : ForcedAction) extends ForcedAction
case class FarmerGrindRefreshAction(f : Hero, n : Int, then : ForcedAction) extends ForcedAction


case class FarmerDoneAction(f : Farmer, then : ForcedAction) extends ForcedAction


case class FromGrain(f : Farmer) extends Message {
    def elem(implicit game : Game) = "from " ~ Grain.of(f)
}


class FarmerPlayer(val faction : Farmer)(implicit val game : Game) extends FactionState {
    var acted = 0

    var foes : $[Faction] = $

    def craft = all(Farm)./(_.asset) ++ all(Windmill)./(_.asset)
}


object FarmerExpansion extends FactionExpansion[Farmer] {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Farmer) =>
            game.states += f -> new FarmerPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Farmer) =>
            val h = game.homelands

            val l = clearings.diff(h)
            val ll = l.%(f.canPlace)
            val lll = ll.%(c => factions.but(f)./~(_.at(c)).of[Scoring].none)

            Ask(f).each(lll)(c => StartingClearingAction(f, c).as(c)(f, "starts in")).needOk

        case StartingClearingAction(f : Farmer, c) =>
            game.homelands :+= c

            f.reserve --> Farm --> c
            f.reserve --> 3.times(Hamster) --> c

            f.log("placed", Farm.of(f), "and", Hamster.sof(f), "in", c)

            val n = game.connected(c).%(f.canPlace)

            val ll = n./(x => $(x, x, x))

            Ask(f).each(ll)(l => PlacePieceClearingsAction(f, l, f.warrior, SetupFactionsAction))

        // HELPER
        case ForcedRemoveSourceEffectAction(e : Farmer, c, f, p, then) if e.foes.has(f).not =>
            e.foes :+= f

            f.log("became", "foe".styled(styles.hit), "of", e)

            TryAgain

        case ForcedRemoveSourceEffectAction(e, c, f : Farmer, p, then) if f.foes.has(e).not =>
            f.foes :+= e

            e.log("became", "foe".styled(styles.hit), "of", f)

            TryAgain

        case BattlePostHitInAction(b, e, f : Farmer, Grain, then) =>
            e.log("squandered", Grain.of(f))
            then

        case BattlePostHitInAction(b, e, f : Farmer, Farm, then) =>
            e.log("sacked", Farm.of(f))
            then

        case BattlePostHitInAction(b, e, f : Farmer, Windmill, then) =>
            e.log("tilted", Windmill.of(f))
            then

        case BattlePostHitInAction(b, e, f : Farmer, Hamster, then) =>
            e.log("poked", Hamster.of(f))
            then

        case BattlePostHitInAction(b, e, f : Farmer, Sheriff, then) =>
            e.log("shot", Sheriff.of(f))
            then

        // TURN
        case BirdsongNAction(30, f : Farmer) if f.all(Farm).none && f.all(Hamster).none =>
            val l = clearings.%(f.canPlace)
            val ll = l.%(f.canBuild).some.|(l)

            val n = ll./(c => f.foes./(_.at(c).num).sum).minOr(0)

            val lll = ll.%(c => f.foes./(_.at(c).num).sum == n)

            Ask(f).each(lll)(PlacePiecesAction(f, _, $(Farm) ++ min(3, f.pooled(Hamster)).times(Hamster), Next))

        case BirdsongNAction(50, f : Farmer) if soft =>
            val t = f.pooled(Grain)
            val r = f.all(Farm).%(f.canPlace)

            if (t >= r.num)
                Ask(f)(ProduceMultiGrainAction(f, r))
            else
                Ask(f).each(r.combinations(t).$)(ProduceMultiGrainAction(f, _)).birdsong(f)

        case ProduceMultiGrainAction(f : Farmer, l) =>
            game.highlights :+= PlaceHighlight(l.distinct)

            l.foreach(c => f.reserve --> Grain --> c)

            l.distinct.foreach { c =>
                log(c, "produced", l.%(_ == c)./(_ => Grain.of(f)).comma)
            }

            Next

        case BirdsongNAction(70, f : Farmer) =>
            XCraftMainAction(f)

        case DaylightNAction(40, f : Farmer) =>
            Next

        case DaylightNAction(60, f : Farmer) =>
            implicit val ask = builder

            if (f.acted < f.all(Farm).num + 2) {
                val att = clearings.%(f.canAttackIn)
                + FarmerAttackAction(f, att).!(att.none)

                val mvv = f.moveFrom.of[Clearing]
                + FarmerMoveAction(f, mvv).!(mvv.none)

                val frm = f.all(Farm).%(f.canPlace)
                + FarmerRecruitAction(f, min(frm.num, f.pooled(f.warrior)), frm).!(f.pool(f.warrior).not, "max").!(frm.none, "nowhere")


                val b0 = clearings
                val b1 = b0.%(f.canPlace)
                val b2 = b1.%(f.rules)
                val b3 = b2.%(f.canBuild)
                val b4 = b3.%(c => f.hand.exists(_.matches(c.cost)))
                val e2 = b1.%(c => f.hand.exists(_.matches(c.cost)))

                val g1 = b0.%(f.at(_).has(Windmill))
                val g2 = g1.%(f.at(_).has(Grain))

                + FarmerBuildAction(f, Farm, b4)
                    .!(f.pool(Farm).not, "max")
                    .!(b1.none, "can't place")
                    .!(b2.none, "no rule")
                    .!(b3.none, "no slot")
                    .!(b4.none, "no card")

                + FarmerBuildAction(f, Windmill, b4)
                    .!(f.pool(Windmill).not, "max")
                    .!(b1.none, "can't place")
                    .!(b2.none, "no rule")
                    .!(b3.none, "no slot")
                    .!(b4.none, "no card")

                + FarmerGrindAction(f, g2)
                    .!(g1.none, "no windmill")
                    .!(g2.none, "no grain")

                + FarmerElectAction(f, Sheriff, e2.%(f.at(_).has(Hamster)))
                    .!(f.pool(Sheriff).not)
                    .!(b1.none, "can't place")
                    .!(e2.none, "no card")
            }

            + EndTurnSoftAction(f, "Turn", ForfeitActions(2 + f.all(Farm).num - f.acted))

            ask(f).daylight(f)

        case FarmerAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), FarmerDoneAction(f, Repeat))

        case FarmerMoveAction(f, l) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), FarmerDoneAction(f, Repeat))

        case FarmerRecruitAction(f, n, l) =>
            val ll = l.combinations(n).$.%(x => l.diff(x).distinct.num == l.diff(x).num)

            Ask(f).each(ll)(l => PlacePieceClearingsAction(f, l, f.warrior, FarmerDoneAction(f, Repeat))).ocancel

        case FarmerGrindAction(f, l) =>
            Ask(f).each(l)(c => FarmerGrindClearingAction(f, c)).cancel

        case FarmerGrindClearingAction(f, c) =>
            f.from(c) --> Grain --> f.reserve

            f.nscore(2)("grinding")(f, "ground", Grain.of(f), "in", c, ForVP)

            f.acted += 1

            Random[BaseSuit](FoxRabbitMouse, s => FarmerGrindRollAction(f, factions.diff(f.foes), s))

        case FarmerGrindRollAction(f, l, Mouse) =>
            l.foreach { o =>
                o.oscore(1)("from", Grain.of(f))
            }

            Repeat

        case FarmerGrindRollAction(f, l, Rabbit) =>
            (l.take(1) ++ l.drop(1).reverse.sortBy(_.vp.abs)).reverse.foldLeft[ForcedAction](Repeat)((q, o) => DrawCardsAction(o, 1, FromGrain(f), AddCardsAction(o, q)))

        case FarmerGrindRollAction(f, l, Fox) =>
            (l.take(1) ++ l.drop(1).reverse.sortBy(_.vp.abs)).foldLeft[ForcedAction](Repeat)((q, o) =>
                o.as[WarriorFaction].%(o => o.pool(o.warrior))./(o => FarmerGrindPlaceAction(o, o.warrior, q)) ||
                o.as[Hero].%(_.inv.exhausted.any)./(o => FarmerGrindRefreshAction(o, 1, q)) |
            q)

        case FarmerGrindPlaceAction(f, p, then) =>
            Ask(f).each(f.presence.%(f.canPlace))(c => PlacePieceAction(f, c, p, then)).skip(then)

        case FarmerGrindRefreshAction(f, n, then) =>
            SelectHeroItemsAction(f, "Ready " ~ n.hl ~ " items", $())(_.num(n))(ToRefresh(_))(l => ReadyItemsAction(f, l, then))

        case FarmerBuildAction(f, p, l) =>
            Ask(f).each(l)(c => FarmerBuildClearingAction(f, p, c)).cancel

        case FarmerBuildClearingAction(f, p, c) =>
            Ask(f).each(f.hand)(d => FarmerBuildClearingCardAction(f, p, c, d).!(d.matches(c.cost).not)).cancel

        case FarmerBuildClearingCardAction(f, p, c, d) =>
            game.highlights :+= PlaceHighlight($(c))

            f.hand --> d --> discard.quiet

            f.reserve --> p --> c

            f.log("built", p.of(f), "in", c, "with", d)

            FarmerDoneAction(f, Repeat)

        case FarmerElectAction(f, p, l) =>
            Ask(f).each(l)(c => FarmerElectClearingAction(f, p, c)).cancel

        case FarmerElectClearingAction(f, p, c) =>
            Ask(f).each(f.hand)(d => FarmerElectClearingCardAction(f, p, c, d).!(d.matches(c.cost).not)).cancel

        case FarmerElectClearingCardAction(f, p, c, d) =>
            game.highlights :+= PlaceHighlight($(c))

            f.hand --> d --> discard.quiet

            f.reserve --> p --> c

            f.from(c) --> f.warrior --> game.recycle

            f.log("elected", p.of(f), "in", c, "with", d)

            FarmerDoneAction(f, Repeat)

        case FarmerDoneAction(f, then) =>
            f.acted += 1

            then

        // EVENING
        case NightStartAction(f : Farmer) =>
            EveningDrawAction(f, 1 + f.all(Windmill).num)

        case FactionCleanUpAction(f : Farmer) =>
            f.acted = 0

            if (f.foes.any) {
                f.foes = $

                f.log("forgave all", "foes".styled(styles.hit))
            }

            factions.of[Hero].%(_.attitude.get(f).has(Hostile)).foreach { e =>
                e.attitude += f -> Indifferent
            }

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
