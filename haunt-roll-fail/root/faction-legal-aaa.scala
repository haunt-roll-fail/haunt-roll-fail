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

trait CommonLegal extends WarriorFaction
trait CommonBatWarrior { self : Warrior => }
trait CommonBatToken { self : Token => }
trait CommonBatBuilding { self : Building => }

trait DebateEffect extends Effect

case object ConveneInstead extends BattleEffect

case object IgnoreWarriorRemovalEffects extends DebateEffect

trait LegalAAA extends WarriorFaction with CommonLegal {
    val clashKey = TCvA

    val warrior = BatAAA

    def abilities(options : $[Meta.O]) = $(DaylightCraft)

    def pieces(options : $[Meta.O]) = BatAAA *** 15 ++ AssemblyAAA *** 12 ++ ConvenedAAA *** 12 ++ CommuneAAA *** 6

    override def getElem : Elem = super.getElem // ~ " " ~ "α".hh

    override def note : Elem = HorizontalBreak ~ "Version from " ~ "2024-10-30".hh

    def advertising = ConvenedAAA.img(this) ~ AssemblyAAA.img(this) ~ ConvenedAAA.img(this) ~ AssemblyAAA.img(this) ~ ConvenedAAA.img(this)

    def motto = "Debate".styled(this)
}

case object BatAAA extends Warrior with CommonBatWarrior {
    override def id = "Bat"
    override def name = "Bat"
}

trait BatTokenAAA extends Token with CommonBatToken

case object AssemblyAAA extends BatTokenAAA {
    override def id = "Assembly"
    override def imgid(f : Faction) = f.style + "-" + id
}

case object ConvenedAAA extends BatTokenAAA {
    override def id = "Convened"
    override def imgid(f : Faction) = f.style + "-" + id
}

case object CommuneAAA extends Building with CommonBatBuilding {
    override def id = "Commune"
    override def imgid(f : Faction) = f.style + "-" + id
}


trait EdictAAA extends FactionEffect {
    def id : String
}

object EdictsAAA {
    def deck = $(
        AwakenThePeople,
        CanvassSupport,
        IntensifyPacifism,
        MightMakesRight,
        OvertimeDebates,
        PacificyThreats,
        RouseLoyalists,
    )
}

case object AwakenThePeople extends EdictAAA {
    val id = "awaken-the-people"
    val name = "Awaken the People"
}

case object CanvassSupport extends EdictAAA with DebateEffect {
    val id = "canvass-support"
    val name = "Canvass Support"
}

case object IntensifyPacifism extends EdictAAA {
    val id = "intensify-pacifism"
    val name = "Intensify Pacifism"
}

case object MightMakesRight extends EdictAAA with DebateEffect {
    val id = "might-makes-right"
    val name = "Might Makes Right"
}

case object OvertimeDebates extends EdictAAA {
    val id = "overtime-debates"
    val name = "Overtime Debates"
}

case object PacificyThreats extends EdictAAA {
    val id = "pacificy-threats"
    val name = "Pacificy Threats"
}

case object RouseLoyalists extends EdictAAA with DebateEffect {
    val id = "rouse-loyalists"
    val name = "Rouse Loyalists"
}





case object TCvA extends LegalAAA {
    val name = "Twilight Council"
    override def funName = NameReference(name, this) ~ " Council"
    val short = "TCvA"
    val style = "TC"
    val priority = "S"
}


class LegalAAAPlayer(val faction : LegalAAA)(implicit val game : Game) extends FactionState {
    var edicts : $[EdictAAA] = $

    override def transient = edicts

    var suits : $[Suit] = $

    var loyalists = cards("tucked")

    def craft = all(CommuneAAA)./(_.asset)
}



case class LegalAAASetupWarriorsAction(f : LegalAAA) extends ForcedAction with Soft
case class LegalAAASetupAssembliesAction(f : LegalAAA) extends ForcedAction with Soft

case class LegalAAAPlaceAssembliesAction(f : LegalAAA, n : Int) extends ForcedAction with Soft

case class LegalAAARecruitMainAction(f : LegalAAA) extends ForcedAction with Soft
case class LegalAAARecruitSuitAction(self : LegalAAA, s : Suit, l : $[Clearing], n : Int) extends BaseAction("Recruit")(s, min(n, l.num).times(self.warrior.imgd(self)).merge) with Soft
case class LegalAAARecruitListAction(self : LegalAAA, s : Suit, l : $[Clearing], r : $[Clearing]) extends BaseAction("Recruit", l.num.times(self.warrior.img(self)).merge, "in")(l.comma) with Soft
case class LegalAAARecruitAction(f : LegalAAA, s : Suit, l : $[Clearing], r : $[Clearing]) extends ForcedAction

case class LegalAAAConveneAssembliesAction(f : LegalAAA) extends ForcedAction with Soft
case class LegalAAAScoreAssembliesAction(f : LegalAAA) extends ForcedAction with Soft
case class LegalAAAScoreConvenedAction(f : LegalAAA, c : Clearing) extends ForcedAction
case class LegalAAABuildConvenedAction(f : LegalAAA, c : Clearing) extends ForcedAction

case class LegalAAARedeployMainAction(f : LegalAAA) extends ForcedAction with Soft
case class LegalAAARedeployFromAction(f : LegalAAA, c : Clearing, l : $[Clearing]) extends ForcedAction with Soft
case class LegalAAARedeployAction(f : LegalAAA, c : Clearing, t : Clearing) extends ForcedAction

case class LegalAAAEdictsMainAction(f : LegalAAA) extends ForcedAction with Soft
case class LegalAAAEdictAction(f : LegalAAA, e : EdictAAA) extends ForcedAction with Soft
case class LegalAAAEdictCardAction(f : LegalAAA, e : EdictAAA, d : DeckCard) extends ForcedAction

case class LegalAAAOutrageAction(f : LegalAAA, o : Faction, c : Clearing, then : ForcedAction) extends ForcedAction
case class LegalAAAOutrageCardAction(f : LegalAAA, self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(self, "surrenders a card to", f)(d.img) with ViewCard







case class LegalAAAConveneMainAction(f : Faction, host : LegalAAA, c : Clearing, then : ForcedAction) extends ForcedAction with Soft
case class LegalAAAConveneAction(f : Faction, host : LegalAAA, c : Clearing, e : Faction, then : ForcedAction) extends ForcedAction
case class LegalAAAConveneRolledAction(f : Faction, host : LegalAAA, c : Clearing, e : Faction, fr : Int, er : Int, then : ForcedAction) extends RolledAction[Int] { def rolled = $(fr, er) }

case class LegalAAAConveneAttackerAdjustAction(f : Faction, host : LegalAAA, c : Clearing, e : Faction, fr : Int, er : Int, passed : Boolean, then : ForcedAction) extends ForcedAction
case class LegalAAAConveneAttackerCardAction  (f : Faction, host : LegalAAA, c : Clearing, e : Faction, fr : Int, er : Int, d : DeckCard,     then : ForcedAction) extends ForcedAction
case class LegalAAAConveneDefenderAdjustAction(f : Faction, host : LegalAAA, c : Clearing, e : Faction, fr : Int, er : Int, passed : Boolean, then : ForcedAction) extends ForcedAction
case class LegalAAAConveneDefenderCardAction  (f : Faction, host : LegalAAA, c : Clearing, e : Faction, fr : Int, er : Int, d : DeckCard,     then : ForcedAction) extends ForcedAction

case class LegalAAAConveneResolveAction(f : Faction, host : LegalAAA, c : Clearing, e : Faction, fr : Int, er : Int, then : ForcedAction) extends ForcedAction
case class LegalAAAConveneFinishedAction(f : Faction, host : LegalAAA, c : Clearing, e : Faction, then : ForcedAction) extends ForcedAction




object LegalAAAExpansion extends FactionExpansion[LegalAAA] {
    def delta(f : Faction)(d : DeckCard)(implicit game : Game) : Int = d @@ {
        case d : CraftCard => d.cost.num
        case _ => 0
    } + f.as[LegalAAA].?(_.loyalists./(_.suit).has(d.suit)).??(1)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : LegalAAA) =>
            game.states += f -> new LegalAAAPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : LegalAAA) =>
            LegalAAASetupWarriorsAction(f)

        case LegalAAASetupWarriorsAction(f) =>
            if (f.all(BatAAA).num < 6) {
                val existing = f.all(BatAAA)
                val costs = existing./(_.cost)

                Ask(f).each(clearings.diff(existing).%(f.canPlace))(c => PlacePieceAction(f, c, BatAAA, LegalAAASetupWarriorsAction(f)).!(
                    (FoxRabbitMouse ++ FoxRabbitMouse).permutations.exists(_.lazyZip(c.cost +: costs).forall((a, c) => a.matches(c))).not
                ))
            }
            else
                NoAsk(f)(LegalAAASetupAssembliesAction(f))

        case LegalAAASetupAssembliesAction(f) =>
            if (f.all(AssemblyAAA).num < 3) {
                val bats = f.all(BatAAA)
                val existing = f.all(AssemblyAAA)

                Ask(f).each(clearings.intersect(bats).diff(existing).%(f.canPlace))(c => PlacePieceAction(f, c, AssemblyAAA, LegalAAASetupAssembliesAction(f)))
            }
            else
                NoAsk(f)(SetupFactionsAction)

        case AfterSetupAction(f : LegalAAA, then) =>
            deck --> deck.$.take(2) --> f.hand

            f.log("drew", 2.of("card"), "extra")

            then

        // HELPER
        case LegalAAAOutrageAction(f : LegalAAA, o, c, then) =>
            o.log("angered", "Pacifism".styled(f), "in", c)

            val h = o.hand.%(_.matches(c.cost))

            if (h.any)
                Ask(o).each(o.hand)(d => LegalAAAOutrageCardAction(f, o, d, then).!(h.has(d).not)).needOk
            else {
                val draw = DrawCardsAction(f, 1, NoMessage, AddCardsAction(f, then))

                if (o.hand.any) {
                    f.log("looked at", o, "cards, no", c.suits, "cards")

                    if (f.has(IntensifyPacifism)) {
                        o.hand --> discard(o, "due", IntensifyPacifism.of(f))
                    }

                    NoAsk(o)(PeekCardsMainAction(f, Empty, FactionHand(o), o.hand, draw))
                }
                else {
                    o.log("had no cards")

                    NoAsk(o)(draw)
                }
            }

        case LegalAAAOutrageCardAction(f : LegalAAA, o, d, then) =>
            o.hand --> d --> f.hand

            o.log("gave card to", f)

            then


        case ForcedRemoveTargetEffectAction(e, c, f : LegalAAA, p : BatTokenAAA, then) =>
            then

        case BattlePostHitInAction(b, e, f : LegalAAA, p : BatAAA.type, then) =>
            e.log("deafened", p.of(f))

            then

        case BattlePostHitInAction(b, e, f : LegalAAA, p : AssemblyAAA.type, then) =>
            e.log("disassembled", p.of(f))

            then

        case BattlePostHitInAction(b, e, f : LegalAAA, p : ConvenedAAA.type, then) =>
            e.log("disconvenienced", p.of(f))

            then

        case BattlePostHitInAction(b, e, f : LegalAAA, p : CommuneAAA.type, then) =>
            e.log("discommunicated", p.of(f))

            then

        // BIRDSONG
        case BirdsongNAction(30, f : LegalAAA) =>
            LegalAAAPlaceAssembliesAction(f, 1 + (7 - f.pooled(CommuneAAA)) /↓ 2)

        case LegalAAAPlaceAssembliesAction(f, 0) =>
            NoAsk(f)(Next)

        case LegalAAAPlaceAssembliesAction(f, n) =>
            Ask(f)
                .each(clearings.diff(f.all(AssemblyAAA)).diff(f.all(ConvenedAAA)).%(f.canPlace))(c => PlacePieceAction(f, c, AssemblyAAA, LegalAAAPlaceAssembliesAction(f, n - 1)))
                .skip(Next)
                .birdsong(f)

        case BirdsongNAction(60, f : LegalAAA) =>
            if (f.pool(BatAAA) || f.totalWar)
                LegalAAARecruitMainAction(f)
            else
                Next

        case LegalAAARecruitMainAction(f) =>
            Ask(f)
                .each(FoxRabbitMouse ++ factions.of[CommonInvasive].any.$(Frog))(s => {
                    val d = f.hand.%(_.matches(s)).any
                    val l = d.??(clearings.%(_.cost.matched(s)).%(f.at(_).of[BatTokenAAA].any).%(f.canPlace))
                    LegalAAARecruitSuitAction(f, s, l, f.pooled(BatAAA)).!(d.not, "no cards").!(l.none, "can't place")
                })
                .skip(Next)
                .birdsong(f)

        case LegalAAARecruitSuitAction(f, s, l, n) =>
            if (f.pooled(BatAAA) >= l.num)
                Force(LegalAAARecruitListAction(f, s, l, l))
            else
                Ask(f)(l.combinations(f.pooled(BatAAA)).$./(LegalAAARecruitListAction(f, s, _, l))).cancel

        case LegalAAARecruitListAction(f, s, l, r) =>
            OptionalDiscardCardAction(f, RecruitsIn(l), s, LegalAAARecruitAction(f, s, l, r))

        case LegalAAARecruitAction(f, s, l, r) =>
            game.highlights :+= PlaceHighlight(l)

            l.foreach(c => f.reserve --> BatAAA --> c)

            if (l.any)
                f.log("recruited", "in", l./(_.elem(game)).comma, f.drawn.get.any.?("with"), f.drawn.get)

            f.drawn --> discard.quiet

            if (r.num > l.num && f.totalWar)
                f.oscore(r.num - l.num)("recruiting")

            Repeat

        // DAYLIGHT
        case DaylightNAction(0, f) if f.can(PacificyThreats) =>
            soft()

            val cw = f.can(CommandWarren).??(clearings.%(f.canAttackIn))

            Ask(f).group("Start of", Daylight)
                .add(MarchAction(f, 1, 2, WageWarAction(f, 1, 1, UsedEffectAction(f, PacificyThreats, Repeat))).as(PacificyThreats.of(f), dt.Move, dt.Move, dt.Battle))
                .add(cw.any.?(BattleInitAction(f, f, WithEffect(CommandWarren), cw, $(CancelAction), UsedEffectAction(f, CommandWarren, Repeat)).as(CommandWarren)))
                .skip(Next)

        case DaylightNAction(20, f : LegalAAA) =>
            XCraftMainAction(f)

        // EVENING
        case EveningNAction(20, f : LegalAAA) =>
            LegalAAAConveneAssembliesAction(f)

        case LegalAAAConveneAssembliesAction(f) =>
            val l = f.all(AssemblyAAA) ++ f.has(OvertimeDebates).??(f.all(ConvenedAAA))

            Ask(f).group("Convene at", "Assemblies".styled(f))
                .each(l)(c => LegalAAAConveneMainAction(f, f, c, Repeat).as("Convene in", c))
                .done(Next)

        case LegalAAAConveneMainAction(f, h, c, then) =>
            Ask(f).group("Convene in", c, "against")
                .each(f.enemies.%(_.present(c)))(e => LegalAAAConveneAction(f, h, c, e, then).as(e))
                .cancel

//[[ BLACK
        case LegalAAAConveneAction(f, h, c, e, then) =>
            if (h.at(c).has(AssemblyAAA)) {
                h.from(c) --> AssemblyAAA --> h.reserve
                h.reserve --> ConvenedAAA --> c
            }

            log(SingleLine)

            f.log("debated", e, "in", c)

            factions.foreach { f =>
                f.ignored :+= IgnoreWarriorRemovalEffects
            }

            Roll[Int](D4 :: D4, l => LegalAAAConveneRolledAction(f, h, c, e, l(0), l(1), then), f.elem ~ " in " ~ c.elem ~ " debates " ~ e.elem)

        case LegalAAAConveneRolledAction(f, h, c, e, fr, er, then) =>
            f.log("rolled", fr.roll)
            e.log("rolled", er.roll)

            LegalAAAConveneAttackerAdjustAction(f, h, c, e, fr, er, false, then)

        case LegalAAAConveneAttackerAdjustAction(f, h, c, e, fr, er, passed, then) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup("Adjust rolls", Break, f, fr.roll, Break, e, er.roll)
                .withRule(d => d.suit.matches(c.cost) || f.can(CanvassSupport))
                .withThens(d => {
                    val x = delta(f)(d)
                    val fnew = min(3, fr + x)
                    val enew = max(0, er - x)

                    LegalAAAConveneAttackerCardAction(f, h, c, e, fnew, er, d, then).as(f, fnew.roll, HorizontalBreak, e, er.roll, HorizontalBreak, "with", d).!(fr >= 3) ::
                    LegalAAAConveneAttackerCardAction(f, h, c, e, fr, enew, d, then).as(f, fr.roll, HorizontalBreak, e, enew.roll, HorizontalBreak, "with", d).!(er <= 0)
                })
                .withExtras(passed.not
                    .?(LegalAAAConveneDefenderAdjustAction(f, h, c, e, fr, er, true, then).as(f, fr.roll, HorizontalBreak, e, er.roll, HorizontalBreak, "Pass"))
                    .|(LegalAAAConveneResolveAction       (f, h, c, e, fr, er,       then).as(f, fr.roll, HorizontalBreak, e, er.roll, HorizontalBreak, "Done"))
                , NoHand)

        case LegalAAAConveneAttackerCardAction(f, h, c, e, fr, er, d, then) =>
            f.hand --> d --> discard.quiet

            e.ignored :+= CanvassSupport

            f.log("adjusted roll with", d)

            f.log("roll became", fr.roll)
            e.log("roll became", er.roll)

            DrawCardsAction(f, 1, NoMessage, AddCardsAction(f, LegalAAAConveneDefenderAdjustAction(f, h, c, e, fr, er, false, then)))

        case LegalAAAConveneDefenderAdjustAction(f, h, c, e, fr, er, passed, then) =>
            YYSelectObjectsAction(e, e.hand)
                .withGroup("Adjust rolls", Break, f, fr.roll, Break, e, er.roll)
                .withRule(d => d.suit.matches(c.cost) || e.can(CanvassSupport))
                .withThens(d => {
                    val x = delta(e)(d)
                    val fnew = max(0, fr - x)
                    val enew = min(3, er + x)

                    LegalAAAConveneDefenderCardAction(f, h, c, e, fr, enew, d, then).as(f, fr.roll, HorizontalBreak, e, enew.roll, HorizontalBreak, "with", d).!(er >= 3) ::
                    LegalAAAConveneDefenderCardAction(f, h, c, e, fnew, er, d, then).as(f, fnew.roll, HorizontalBreak, e, er.roll, HorizontalBreak, "with", d).!(fr <= 0)
                })
                .withExtras(passed.not
                    .?(LegalAAAConveneAttackerAdjustAction(f, h, c, e, fr, er, true, then).as(f, fr.roll, HorizontalBreak, e, er.roll, HorizontalBreak, "Pass"))
                    .|(LegalAAAConveneResolveAction       (f, h, c, e, fr, er,       then).as(f, fr.roll, HorizontalBreak, e, er.roll, HorizontalBreak, "Done"))
                , NoHand)

        case LegalAAAConveneDefenderCardAction(f, h, c, e, fr, er, d, then) =>
            e.hand --> d --> discard.quiet

            e.ignored :+= CanvassSupport

            e.log("adjusted roll with", d)

            f.log("roll became", fr.roll)
            e.log("roll became", er.roll)

            val might = f.as[LegalAAA].?(f => f.has(MightMakesRight) && f.rules(c))

            DrawCardsAction(e, 1 - might.??(1), NoMessage, AddCardsAction(e, LegalAAAConveneAttackerAdjustAction(f, h, c, e, fr, er, false, then)))

        case LegalAAAConveneResolveAction(f, h, c, e, fr, er, then) =>
            var q = then

            q = LegalAAAConveneFinishedAction(f, h, c, e, q)

            q = ForcedRemoveFinishedAction(f, q)

            if (fr > 0 && f.canRemove(c)(e))
                q = BattleAssignHitsAction(e, Battle(c, f, None, e, None, None, None, 0, 0, 0, DummyAction), fr, 0, q)

            if (er > 0 && e.canRemove(c)(f))
                q = BattleAssignHitsAction(f, Battle(c, f, None, e, None, None, None, 0, 0, 0, DummyAction), er, 0, q)

            q

        case LegalAAAConveneFinishedAction(f, h, c, e, then) =>
            factions.foreach { f =>
                f.used = f.used.notOf[DebateEffect]
                f.ignored = f.ignored.notOf[DebateEffect]
            }

            log("Debate ended in", c)

            log(SingleLine)

            then

//]]

        case EveningNAction(30, f : LegalAAA) =>
            LegalAAAScoreAssembliesAction(f)

        case LegalAAAScoreAssembliesAction(f) =>
            val l = f.all(ConvenedAAA)
            val b = l.%(_ => f.pool(CommuneAAA)).%(f.canBuild)

            if (b.any)
                Ask(f)
                    .group("Build with", "Convened".styled(f))
                    .each(b)(c => LegalAAABuildConvenedAction(f, c).as("Build", CommuneAAA.of(f), CommuneAAA.imgd(f), "in", c))
                    .group("Score with", "Convened".styled(f))
                    .each(l)(c => LegalAAAScoreConvenedAction(f, c).as("Score", 1.vp, "in", c))
            else
            if (l.any)
                NoAsk(f)(LegalAAAScoreConvenedAction(f, l(0)))
            else
                NoAsk(f)(Next)

        case LegalAAABuildConvenedAction(f, c) =>
            if (f.at(c).has(ConvenedAAA)) {
                f.from(c) --> ConvenedAAA --> f.reserve
                f.reserve --> AssemblyAAA --> c
            }

            f.reserve --> CommuneAAA --> c

            f.log("built", CommuneAAA.of(f), "in", c)

            Repeat

        case LegalAAAScoreConvenedAction(f, c) =>
            if (f.at(c).has(ConvenedAAA)) {
                f.from(c) --> ConvenedAAA --> f.reserve
                f.reserve --> AssemblyAAA --> c
            }

            f.oscore(1)("from", ConvenedAAA.of(f))

            Repeat

        case EveningNAction(40, f : LegalAAA) =>
            LegalAAARedeployMainAction(f)

        case LegalAAARedeployMainAction(f) =>
            val l = f.all(AssemblyAAA)
            val d = l.%(f.canPlace)
            val s = l.%(f.at(_).of[Warrior].any)

            Ask(f).group("Redeploy", s./(f.at(_).count(BatAAA)).sum.times(BatAAA.imgd(f)).merge, "from")
                .each(s)(c => LegalAAARedeployFromAction(f, c, d.but(c)).as(c).!(d.but(c).none))
                .done(Next)

        case LegalAAARedeployFromAction(f, c, l) =>
            Ask(f).group("Redeploy from", c, "to")
                .each(l)(t => LegalAAARedeployAction(f, c, t).as(t))
                .cancel

        case LegalAAARedeployAction(f, c, t) =>
            f.from(c) --> BatAAA --> t

            f.log("redeployed", BatAAA.of(f), "from", c, "to", t)

            Repeat

        case EveningNAction(50, f : LegalAAA) =>
            if (f.has(AwakenThePeople)) {
                val n = f.pooled(CommuneAAA) match {
                    case 0 => 4
                    case 1 => 3
                    case 2 | 3 => 2
                    case 4 | 5 => 1
                    case _ => 0
                }

                f.oscore(n)("awakening people".styled(f))
            }

            if (f.edicts.any) {
                f.edicts = $
                f.suits = $

                f.log("ended all", "Edicts".styled(f))

                f.loyalists --> discard(f)
            }

            Next

        case EveningNAction(60, f : LegalAAA) =>
            LegalAAAEdictsMainAction(f)

        case LegalAAAEdictsMainAction(f) =>
            implicit def descCard(e : EdictAAA) = Image(e.id, styles.card)

            if (f.hand.any)
                YYSelectObjectsAction(f, EdictsAAA.deck.diff(f.edicts))
                    .withGroup("Enact", "Edicts".styled(f))
                    .withRule(_ => f.hand.exists(_.suit.in(f.suits).not))
                    .withThen(e => LegalAAAEdictAction(f, e))(e => "Enact " ~ e.of(f))("Enact")
                    .withExtras(Next.as("Done"))
            else
                NoAsk(f)(Next)

        case LegalAAAEdictAction(f, e) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup("Enact", e.of(f))
                .withRule(d => d.suit.in(f.suits).not)
                .withThen(d => LegalAAAEdictCardAction(f, e, d))(d => ("Enact " ~ e.of(f) ~ " with " ~ d.elem))("Enact " ~ e.of(f))
                .withExtras(Next.as("Done"), NoHand)

            // YYSelectObjectsAction(f, f.hand)
            //     .withGroup("Enact", "Edicts".styled(f))
            //     .withRule(d => d.suit.in(f.suits).not)
            //     .withThensInfo(d => EdictsAAA.deck./(e => LegalAAAEdictAction(f, e, d).as(e.of(f), "with", d)))(EdictsAAA.deck./(e => Info(e.of(f))))
            //     .withExtras(Next.as("Done"), NoHand)

        case LegalAAAEdictCardAction(f, e, d) =>
            f.log("enacted", e.of(f), "with", d)

            f.edicts :+= e

            f.suits :+= d.suit

            if (e == RouseLoyalists)
                f.hand --> d --> f.loyalists
            else
                f.hand --> d --> discard.quiet

            Repeat

        case NightStartAction(f : LegalAAA) =>
            val n = 1 + (6 - f.pooled(CommuneAAA)) /↓ 2

            EveningDrawAction(f, n)

        case FactionCleanUpAction(f : LegalAAA) =>

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
