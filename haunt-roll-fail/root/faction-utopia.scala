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

trait Utopia extends WarriorFaction {
    val clashKey = CU

    val warrior = Foxy

    def abilities(options : $[Meta.O]) = $(ExcessiveForce, MorningCraft)

    override val transports : $[$[Transport]] = $($(RuledMove))

    def pieces(options : $[Meta.O]) = Foxy *** 25 ++ Palace *** 7 ++ LivingShield *** 7

    def getFreedom(corruption : Int) : Int

    def getDraw(corruption : Int) : Int
}

case object CU extends Utopia {
    val name = "Chancellor's Utopia"
    override def funName = NameReference(name, this) ~ " Utopia"
    val short = "CU"
    val style = "cu"
    val priority = "X"

    def advertising = 2.times(Palace.img(this)).merge
    def motto = "Compomise".styled(this)
    override def note : Elem = HorizontalBreak ~ "Original " ~ "RootJam".hh ~ " version by " ~ "blbyblby".hl ~ HorizontalBreak ~ "Bribery".hh ~ " works only for scoring and shields".txt

    def getFreedom(corruption : Int) = 1 + (corruption < 4).??(1) + (corruption < 6).??(1)

    def getDraw(corruption : Int) = 1 + (corruption < 2).??(1) + (corruption < 4).??(1) + (corruption < 6).??(1)
}

case object CUv2 extends Utopia {
    val name = "Chancellor's Utopia"
    override def funName = NameReference(name, this) ~ " Utopia"
    val short = "CUv2"
    val style = "cu"
    val priority = "X"

    def advertising = 2.times(Palace.img(this)).merge
    def motto = "Compomise".styled(this)

    override def note : Elem = HorizontalBreak ~ "Version from " ~ "2024-05-18".hh ~ " by " ~ "blbyblby".hl ~ HorizontalBreak ~ "Bribery".hh ~ " works only for scoring and shields".txt

    def getFreedom(corruption : Int) = 1 + (corruption < 4).??(1) + (corruption < 6).??(1)

    def getDraw(corruption : Int) = 1 + (corruption < 2).??(1) + (corruption < 3).??(1) + (corruption < 4).??(1)
}


case object ExcessiveForce extends BattleEffect

case object Palace extends Building with RemovalResistance
case object LivingShield extends Building with Tenacious {
    override def name = "Living Shield"
    override def id = "living-shield"
}

case object Foxy extends Warrior


class UtopiaPlayer(val faction : Utopia)(implicit val game : Game) extends FactionState {
    var acted = 0
    var extra = 0

    var scoredSuits : $[BaseSuit] = $
    var scored : $[Clearing] = $

    var corruption = 1

    def freedom = faction.getFreedom(corruption)

    def draw = faction.getDraw(corruption)

    val deserters = location(Acolytes, u => u.faction == faction && u.piece == faction.warrior)

    def craft = all(Palace)./(_.asset)./~{
        case a if corruption < 3 => $()
        case a if corruption < 6 => $(a)
        case a => $(a, a)
    }
}

trait CUDaylightQuestion extends FactionAction {
    override def self : Utopia

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            1.to(self.acted + self.extra)./(_ => Image("action-black", styles.action)) ~
            (1.to(self.corruption)./(_ => Image(self.style + "-action", styles.action))).drop(self.acted),
        styles.margined)
}


case class UtopiaRecruitAction(f : Utopia, n : Int, corrupt : Boolean, then : ForcedAction) extends ForcedAction

case class UtopiaDoneAction(f : Utopia, then : ForcedAction) extends ForcedAction
case class CorruptAction(f : Utopia, then : ForcedAction) extends ForcedAction

case class UtopiaMainAction(f : Utopia) extends ForcedAction

case class UtopiaAttackAction(self : Utopia, l : $[Clearing], corrupt : Boolean) extends OptionAction("Battle".styled(self), dt.Battle, corrupt.?("(" ~ "corrupt".styled(styles.hit) ~ ")")) with CUDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class UtopiaMoveAction(self : Utopia, l : $[Clearing], corrupt : Boolean) extends OptionAction("Move".styled(self), dt.Move, corrupt.?("(" ~ "corrupt".styled(styles.hit) ~ ")")) with CUDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class UtopiaBuildAction(self : Utopia, l : $[Clearing], corrupt : Boolean) extends OptionAction("Build", Palace.of(self), Palace.imgd(self), corrupt.?("(" ~ "corrupt".styled(styles.hit) ~ ")")) with CUDaylightQuestion with Soft with Only[BuildPalaceClearingAction] { def tag = implicitly }

case class BuildPalaceClearingAction(self : Utopia, c : Clearing) extends BaseAction("Build", Palace.of(self), "in")(c)

case class RemoveDesertersAction(self : Utopia, l : $[Clearing], then : ForcedAction) extends BaseAction("Remove deserted", "Foxies".styled(self), "from")(l.comma)


case class UtopiaScoreSuitMainAction(self : Utopia, s : BaseSuit, n : Int, vp : Int) extends BaseAction("Celebrate")(s.elem, "freedoms", dt.CardSuit(s), dt.Arrow, (n * vp).vp) with Soft with Only[UtopiaRevealDiscardCardAction] { def tag = implicitly }
case class UtopiaScoreSuitAction(self : Utopia, s : BaseSuit, n : Int, d : DeckCard) extends ForcedAction

case class UtopiaScoreMainAction(self : Utopia, c : Clearing, vp : Int) extends BaseAction("Celebrate")(c, "freedoms", implicit g => dt.CardSuit(c.cost), dt.Arrow, vp.vp) with Soft with Only[UtopiaRevealDiscardCardAction] { def tag = implicitly }
case class UtopiaScoreAction(self : Utopia, c : Clearing, d : DeckCard) extends ForcedAction

case class UtopiaRevealDiscardCardSuitMainAction(self : Utopia, s : BaseSuit, n : Int) extends ForcedAction with Soft
case class UtopiaRevealDiscardCardSuitAction(self : Utopia, s : BaseSuit, n : Int, d : DeckCard) extends BaseAction("Discard", s.elem, "card")(d.img, d.matches(s).not.?("(" ~ "corrupt".styled(styles.hit) ~ ")")) with ViewCard

case class UtopiaRevealDiscardCardMainAction(self : Utopia, c : Clearing, vp : Int) extends ForcedAction with Soft
case class UtopiaRevealDiscardCardAction(self : Utopia, c : Clearing, d : DeckCard, vp : Int) extends BaseAction("Discard", c, "card")(d.img, implicit g => d.matches(c.cost).not.?("(" ~ "corrupt".styled(styles.hit) ~ ")")) with ViewCard

case class UtopiaDrawAction(self : Utopia, n : Int) extends BaseAction(None)("Draw Cards", n.times(dt.CardBack).merge)
case class UtopiaTaxAction(self : Utopia, n : Int) extends BaseAction(None)("Tax", n.times(dt.CardBack).merge, "(" ~ "corrupt".styled(styles.hit) ~ ")")

case class CheckLivingShieldAction(self : Utopia, c : Clearing, yes : ForcedAction, no : ForcedAction) extends ForcedAction
case class ActivateLivingShieldAction(self : Utopia, c : Clearing, d : DeckCard, then : ForcedAction) extends BaseAction("Discard a card", ToActivateLivingShield(self, c))(d.img, implicit g => ("(" ~ "corrupt".styled(styles.hit) ~ d.matches(c.cost).not.?(", " ~ "corrupt".styled(styles.hit)) ~ ")")) with ViewCard

case class ExcessiveForceAction(self : Faction, b : Battle) extends ForcedAction
case class ExcessiveForceIgnoreAction(self : Faction, b : Battle) extends ForcedAction


case class ToActivateLivingShield(f : Utopia, c : Clearing) extends Message {
    def elem(implicit game : Game) = "to activate " ~ LivingShield.of(f) ~ " in " ~ c.elem
}



object UtopiaExpansion extends FactionExpansion[Utopia] {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Utopia) =>
            game.states += f -> new UtopiaPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Utopia) if options.has(SetupTypeHomelands) =>
            val h = game.homelands
            val hh = h./~(game.connected)

            val l = clearings.diff(h)
            val ll = l.diff(board.inner).some.|(l)

            Ask(f).each(ll)(c => StartingClearingAction(f, c).as(c)(f, "starts in")).needOk

        case FactionSetupAction(f : Utopia) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : Utopia, c) =>
            game.homelands :+= c

            f.reserve --> Palace --> c
            f.reserve --> 2.times(Foxy) --> c

            f.log("placed", Palace.of(f), "and", "Foxies".styled(f), "in", c)

            val n = game.connected(c).%(f.canPlace)

            val ll = n.combinations(2).$./(_./~(x => $(x, x)))

            Ask(f).each(ll)(l => PlacePieceClearingsAction(f, l, f.warrior, SetupFactionsAction))

        // HELPER
        case MoveListAction(self, f, t, m, from, to, l, UtopiaDoneAction(ff, then)) =>
            UtopiaDoneAction(ff, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case MoveListAction(self, f : Utopia, t, m, from, to, l, then) if l.diff(f.at(from)).any =>
            Force(MoveListAction(self, f, t, m, from, to, l.intersect(f.at(from)), then))

        case BattleStartAction(self, f, a, m, c, o, i, UtopiaDoneAction(ff, then)) =>
            UtopiaDoneAction(ff, BattleStartAction(self, f, a, m, c, o, i, then))

        case LegalAAAConveneAction(f, h, c, e, AssemblyDoneAction(ff, then)) =>
            AssemblyDoneAction(ff, LegalAAAConveneAction(f, h, c, e, then))

        case CorruptAction(f : CU.type, then) =>
            f.corruption += 1

            f.log("became more", "corrupt".hl)

            if (f.corruption % 2 == 1) {
                val n = min(3, f.pooled(f.warrior))

                if (n >= 1) {
                    f.reserve --> n.times(f.warrior) --> f.deserters

                    f.log("lost", n.hl, "potential", "Foxies".styled(f))
                }

                if (n < 3) {
                    val l = f.all(f.warrior)
                    val r = min(3 - n, l.num)

                    if (r > 0) {
                        Ask(f).each(l.combinations(r).$)(RemoveDesertersAction(f, _, then))
                    }
                    else
                        then
                }
                else
                    then
            }
            else {
                if (f.corruption == 8) {
                    f.corruption -= 1

                    f.oscore(-1)("exceeding corruption")
                }

                then
            }

        case CorruptAction(f : CUv2.type, then) =>
            f.corruption += 1

            f.log("became more", "corrupt".hl)

            if (f.corruption >= 5 && f.corruption <= 7) {
                val n = min(3, f.pooled(f.warrior))

                if (n >= 1) {
                    f.reserve --> n.times(f.warrior) --> f.deserters

                    f.log("lost", n.hl, "potential", "Foxies".styled(f))
                }

                if (n < 3) {
                    val l = f.all(f.warrior)
                    val r = min(3 - n, l.num)

                    if (r > 0) {
                        Ask(f).each(l.combinations(r).$)(RemoveDesertersAction(f, _, then))
                    }
                    else
                        then
                }
                else
                    then
            }
            else {
                if (f.corruption == 8) {
                    f.corruption -= 1

                    f.oscore(-1)("exceeding corruption")
                }

                then
            }

       case RemoveDesertersAction(f, l, then) =>
            l.distinct.foreach { c =>
                val n = l.count(c)
                f.from(c) --> n.times(f.warrior) --> f.deserters
                log(n.times(Foxy.of(f)).comma, "deserted from", c)
            }

            then


        case BattleCleanupAction(b) if b.attacker.can(ExcessiveForce) =>
            if (b.attacker.rules(b.clearing) && b.defender.present(b.clearing))
                Ask(b.attacker)(CorruptAction(b.attacker.as[Utopia].get, ExcessiveForceAction(b.attacker, b)).as("Apply " ~ "(" ~ "corrupt".styled(styles.hit) ~ ")")("Excessive Force".hl)).skip(ExcessiveForceIgnoreAction(b.attacker, b))
            else
                ExcessiveForceIgnoreAction(b.attacker, b)

        case ExcessiveForceIgnoreAction(f, b) =>
            f.used :+= ExcessiveForce

            BattleCleanupAction(b)

        case ExcessiveForceAction(f, b) =>
            f.used :+= ExcessiveForce

            f.log("applied", "Excessive Force".hl)

            BattleResolveHitsAction(b, f.used.has(Siege).not.??(b.ahits), 0, f.used.has(Siege).??(b.ahits))

        case TryForcedRemoveAction(e, c, f : Utopia, p @ LivingShield, n, m, then, fail) =>
            fail

        case TryForcedRemoveAction(e, c, f : Utopia, p @ Palace, n, m, then, fail) =>
            CheckLivingShieldAction(f, c, fail, ForcedRemoveAction(e, c, f, p, n, m, then))

        case CheckLivingShieldAction(f, c, yes, no) =>
            Ask(f).each(f.hand)(d => ActivateLivingShieldAction(f, c, d, yes)).skip(no)

        case ActivateLivingShieldAction(f, c, d, then) =>
            f.from(c) --> Palace --> f.reserve
            f.reserve --> LivingShield --> c

            f.log("activated", LivingShield.of(f), "in", c, "with", d)

            f.hand --> d --> discard.quiet

            if (d.matches(c.cost).not)
                CorruptAction(f, CorruptAction(f, then))
            else
                CorruptAction(f, then)

        case BattlePostHitInAction(b, e, f : Utopia, Palace, then) =>
            e.log("shattered", Palace.of(f))
            then

        case BattlePostHitInAction(b, e, f : Utopia, LivingShield, then) =>
            e.log("did nothing to", LivingShield.of(f))
            then

        case BattlePostHitInAction(b, e, f : Utopia, Foxy, then) =>
            e.log("disillusioned", Foxy.of(f))
            then

        // TURN
        case BirdsongNAction(30, f : Utopia) =>
            f.all(LivingShield).foreach { c =>
                f.from(c) --> LivingShield --> f.reserve
                f.reserve --> Palace --> c

                log(LivingShield.of(f), "expired in", c)
            }

            Next

        // CRAFT
        case BirdsongNAction(40, f : Utopia) =>
            XCraftMainAction(f)

        // RECRUIT
        case BirdsongNAction(60, f : Utopia) =>
            UtopiaRecruitAction(f, f.corruption, false, Next)

        case UtopiaRecruitAction(f, n, corrupt, then) =>
            val p = f.all(Palace).%(f.canPlace)
            val m = min(n, f.pooled(f.warrior))

            val a = if (p.any && m > 0) {
                var l = p

                while (l.num < m)
                    l ++= p

                val ll = l.combinations(m).$.%(x => l.diff(x).distinct.num == l.diff(x).num)

                Ask(f).each(ll)(l => PlacePieceClearingsAction(f, l, f.warrior, then))
            }
            else
                Ask(f).done(then)

            if (corrupt)
                a
            else
                a.birdsong(f)

        case BirdsongNAction(70, f : CU.type) =>
            Ask(f)(CorruptAction(f, UtopiaRecruitAction(f, min(7, f.corruption + 1), true, Next)).as("Recruit".hl, "(" ~ "corrupt".styled(styles.hit) ~ ")")("Recruit again")).skip(Next)

        case BirdsongNAction(70, f : CUv2.type) =>
            Ask(f)(CorruptAction(f, UtopiaRecruitAction(f, f.all(Palace).%(f.canPlace).num, true, Next)).as("Recruit".hl, "(" ~ "corrupt".styled(styles.hit) ~ ")")("Recruit at each", Palace.of(f))).skip(Next)

        // COMMAND
        case DaylightNAction(30, f : Utopia) =>
            f.acted = 0
            f.extra = 0

            Next

        case DaylightNAction(40, f : Utopia) =>
            var ask = Ask(f)

            if (f.extra < 3) {
                val corrupt = f.acted >= f.corruption || f.extra > 0

                val att = clearings.%(f.canAttackIn)
                ask += UtopiaAttackAction(f, att, corrupt).!(att.none)

                val mvv = f.moveFrom.of[Clearing]
                ask += UtopiaMoveAction(f, mvv, corrupt).!(mvv.none)

                val b0 = clearings
                val b1 = b0.%(f.at(_).has(Palace).not)
                val b2 = b1.%(f.at(_).has(LivingShield).not)
                val b3 = b2.%(f.rules)
                val b4 = b3.%(f.canPlace)
                val b5 = b4.%(f.canBuild)

                ask += UtopiaBuildAction(f, b5, corrupt)
                    .!(f.pooled(Palace) <= f.all(LivingShield).num, "max")
                    .!(b2.none, "no place")
                    .!(b3.none, "no rule")
                    .!(b4.none, "can't place")
                    .!(b5.none, "no slot")
            }

            ask += EndTurnSoftAction(f, "Daylight".styled(styles.phase), ForfeitActions((f.extra == 0).??(f.corruption - f.acted)))

            ask.daylight(f)

        case UtopiaAttackAction(f, l, _) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), UtopiaDoneAction(f, Repeat))

        case UtopiaMoveAction(f, l, _) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), UtopiaDoneAction(f, Repeat))

        case UtopiaBuildAction(f, l, _) =>
            Ask(f)(l./(c => BuildPalaceClearingAction(f, c))).cancel

        case BuildPalaceClearingAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.log("built", Palace.of(f), "in", c)

            f.reserve --> Palace --> c

            UtopiaDoneAction(f, Repeat)

        case UtopiaDoneAction(f, then) =>
            if (f.extra > 0 || f.acted >= f.corruption) {
                f.extra += 1

                CorruptAction(f, then)
            }
            else {
                f.acted += 1

                then
            }

        // EVENING
        case EveningNAction(40, f : CU.type) =>
            var actions : $[UserAction] = $

            FoxRabbitMouse.foreach { s =>
                val ll = f.all(Palace).%(_.cost.matched(s))
                val l = ll.%(f.rules)
                actions :+= UtopiaScoreSuitMainAction(f, s, l.num, f.freedom).!(ll.none, "no palaces").!(l.none, "not ruled").!(f.scoredSuits.contains(s), "once per turn")
            }

            Ask(f)(actions).done(Next).evening(f)

        case EveningNAction(40, f : CUv2.type) =>
            var actions : $[UserAction] = $

            f.all(Palace).diff(f.scored).foreach { c =>
                actions :+= UtopiaScoreMainAction(f, c, f.freedom).!(f.rules(c).not, "not ruled").!(f.hand.none)
            }

            Ask(f)(actions).done(Next).evening(f)

        case UtopiaScoreSuitMainAction(f, s, n, _) =>
            UtopiaRevealDiscardCardSuitMainAction(f, s, n)

        case UtopiaRevealDiscardCardSuitMainAction(f, s, n) =>
            Ask(f).each(f.hand)(d => UtopiaRevealDiscardCardSuitAction(f, s, n, d)).cancel

        case UtopiaRevealDiscardCardSuitAction(f, s, n, d) =>
            if (d.matches(s).not)
                CorruptAction(f, UtopiaScoreSuitAction(f, s, n, d))
            else
                UtopiaScoreSuitAction(f, s, n, d)

        case UtopiaScoreSuitAction(f, s, n, d) =>
            game.highlights :+= NothingHighlight

            f.hand --> d --> discard.quiet

            f.nscore(n * f.freedom)(s, "Freedoms".styled(f))(f, "celebrated", s, "freedoms", ForVP, "with", d)

            f.scoredSuits :+= s

            Repeat

        case UtopiaScoreMainAction(f, c, n) =>
            UtopiaRevealDiscardCardMainAction(f, c, n)

        case UtopiaRevealDiscardCardMainAction(f, c, vp) =>
            Ask(f).each(f.hand)(d => UtopiaRevealDiscardCardAction(f, c, d, vp)).cancel

        case UtopiaRevealDiscardCardAction(f, c, d, _) =>
            if (d.matches(c.cost).not)
                CorruptAction(f, UtopiaScoreAction(f, c, d))
            else
                UtopiaScoreAction(f, c, d)

        case UtopiaScoreAction(f, c, d) =>
            game.highlights :+= NothingHighlight

            f.hand --> d --> discard.quiet

            f.nscore(f.freedom)(c, "Freedoms".styled(f))(f, "celebrated", c, "freedoms", ForVP, "with", d)

            f.scored :+= c

            Repeat

        case NightStartAction(f : Utopia) =>
            val d = f.draw
            val t = f.all(Palace).%(f.rules).num

            Ask(f)(UtopiaDrawAction(f, d))(UtopiaTaxAction(f, t)).evening(f)

        case UtopiaDrawAction(f, d) =>
            EveningDrawAction(f, d)

        case UtopiaTaxAction(f, t) =>
            CorruptAction(f, EveningDrawAction(f, t))

        case FactionCleanUpAction(f : Utopia) =>
            f.scoredSuits = $
            f.scored = $
            f.acted = 0
            f.extra = 0

            CleanUpAction(f)

        case _ => UnknownContinue
    }
}
