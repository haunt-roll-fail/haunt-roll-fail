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

trait Fanatic extends WarriorFaction {
    val clashKey = LC

    val warrior = Lizard

    def abilities(options : $[Meta.O]) = $(LostSouls, EveningCraft)

    def pieces(options : $[Meta.O]) = Lizard *** 25 ++ Garden(Fox) *** 5 ++ Garden(Rabbit) *** 5 ++ Garden(Mouse) *** 5

    def advertising = Garden(Fox).img(this) ~ Garden(Rabbit).img(this) ~ Garden(Mouse).img(this)

    def motto = "Ritual".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Lizard, Garden(Fox), Garden(Rabbit), Garden(Mouse))
}

case object LC extends Fanatic {
    val name = "The Lizard Cult"
    override def funName = NameReference(name, this) ~ " Cult"
    val short = "LC"
    val style = "lc"
    val priority = "F"
}

case object CM extends Fanatic {
    val name = "Crocodile Monks"
    override def funName = NameReference(name, this) ~ " Monks"
    val short = "CM"
    val style = "cm"
    val priority = "F'"
}

case class Garden(suit : BaseSuit) extends Building {
    override def id = "Garden"
    override def imgid(f : Faction) = f.style + "-" + name + "-" + suit.name
}

case object Lizard extends Warrior

case object Revenge extends BattleEffect with RemoveEffect
case object Crusaders extends HiddenEffect
case object LostSouls extends HiddenEffect

case object Acolytes extends SpecialRegion {
    def of(f : Faction, n : Int) = (n == 1).?("Acolyte").|(name).styled(f)
    def img(f : Faction) = Image(f.style + "-acolyte", styles.piece)
    def imgd(f : Faction) = Image(f.style + "-acolyte", styles.fund)
}

class FanaticPlayer(val faction : Fanatic)(implicit val game : Game) extends FactionState {
    var outcast : |[BaseSuit] = None
    var hated = false

    var scored : $[BaseSuit] = $

    var lost = cards("lost-souls")

    var revealed = cards("revealed")

    val acolytes = location(Acolytes, u => u.faction == faction && u.piece == faction.warrior)

    val rubble = location(Rubble, u => u.faction == faction && u.piece.is[Garden])

    def craft = outcast./~(s => all(Garden(s))./(_ => s))
}


case object Sanctifying extends Message {
    def elem(implicit game : Game) = "sanctifying"
}

case class Crusade(s : Suit) extends Message {
    def elem(implicit game : Game) = "on " ~ "Crusade".styled(s)
}

case class WithAcolytes(f : Faction, n : Int) extends Message {
    def elem(implicit game : Game) = "with " ~ n.hl ~ " " ~ n.times(Acolytes.of(f, 1)).comma
}


trait LCBirdsongQuestion extends FactionAction {
    override def self : Fanatic
    def question(implicit game : Game) = self.elem ~ " (" ~ "Birdsong".styled(styles.phase) ~ ") " ~
        Div(
            Image("outcast-" + self.outcast.get + self.hated.??("-hated"), styles.outcast) ~
            self.acolytes.$.num.times(Acolytes.img(self)) ~
            Image("outcast-" + self.outcast.get + self.hated.??("-hated"), styles.outcast),
        styles.margined)
}

trait LCDaylightQuestion extends FactionAction {
    override def self : Fanatic
    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ " " ~ self.hand./(_.suit).ss
}

case class StartingOutcastAction(self : Fanatic) extends ForcedAction
case class ChooseOutcastAction(self : Fanatic, s : BaseSuit) extends BaseAction("Choose outcast")(s)

case class StartingClearingSuitAction(f : Faction, r : Clearing, s : BaseSuit) extends ForcedAction

case class SpendAcolytesAction(self : Fanatic, n : Int, then : ForcedAction) extends ForcedAction
case class FanaticAttackMainAction(self : Fanatic, cost : Int, l : $[Clearing]) extends OptionAction(implicit g => "Battle".styled(self.outcast), cost.times(Acolytes.imgd(self)).merge, dt.Battle) with LCBirdsongQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class FanaticCrusadeMainAction(self : Fanatic, cost : Int, l : $[Clearing]) extends OptionAction(implicit g => "Crusade".styled(self.outcast), cost.times(Acolytes.imgd(self)).merge, dt.Move ~ dt.Battle) with LCBirdsongQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class FanaticCrusadeAttackAction(self : Fanatic, l : $[Clearing]) extends ForcedAction
case class FanaticConvertMainAction(self : Fanatic, cost : Int, l : $[Clearing]) extends OptionAction(implicit g => "Convert".styled(self.outcast), cost.times(Acolytes.imgd(self)).merge, dt.Swap, self.warrior.imgd(self)) with LCBirdsongQuestion with Soft with Only[FanaticConvertAction] { def tag = implicitly }
case class FanaticConvertAction(self : Fanatic, cost : Int, c : Clearing, e : Faction, p : Warrior) extends BaseAction(implicit g => "Convert".styled(self.outcast), "(" ~ cost.hl, Acolytes.of(self, cost) ~ ")")(p.of(e), "in", c)
case class FanaticConvertPlaceAction(self : Fanatic, c : Clearing, p : Warrior, then : ForcedAction) extends ForcedAction
case class FanaticSanctifyMainAction(self : Fanatic, cost : Int, l : $[Clearing], s : BaseSuit) extends OptionAction(implicit g => "Sanctify".styled(self.outcast), cost.times(Acolytes.imgd(self)).merge, dt.Swap, Image("building-any", styles.piece)) with LCBirdsongQuestion with Soft with Only[FanaticSanctifyAction] { def tag = implicitly }
case class FanaticSanctifyAction(self : Fanatic, cost : Int, c : Clearing, s : BaseSuit, e : Faction, p : Building) extends BaseAction(implicit g => "Sanctify".styled(self.outcast), "(" ~ cost.hl, Acolytes.of(self, cost) ~ ")")(p.of(e), "in", c)
case class FanaticSanctifyPlaceAction(self : Fanatic, c : Clearing, p : Building, then : ForcedAction) extends ForcedAction

case class FanaticRecruitMainAction(self : Fanatic, s : $[BaseSuit], l : $[Clearing]) extends OptionAction("Recruit", s./(s => dt.CardSuit(s)).merge) with LCDaylightQuestion with Soft with Only[FanaticRecruitAction] { def tag = implicitly }
case class FanaticRecruitClearingAction(self : Fanatic, s : $[BaseSuit], c : Clearing) extends BaseAction("Recruit in")(c) with Soft
case class FanaticRecruitAction(self : Fanatic, c : Clearing) extends ForcedAction

case class FanaticBuildMainAction(self : Fanatic, s : $[BaseSuit], l : $[Clearing]) extends OptionAction("Build", s./(s => dt.CardSuit(s)).merge) with LCDaylightQuestion with Soft with Only[FanaticBuildAction] { def tag = implicitly }
case class FanaticBuildClearingAction(self : Fanatic, s : BaseSuit, c : Clearing) extends BaseAction("Build")(s, Garden(s).of(self), Garden(s).imgd(self), "in", c) with Soft
case class FanaticBuildAction(self : Fanatic, s : BaseSuit, c : Clearing) extends ForcedAction

case class FanaticScoreMainAction(self : Fanatic, s : BaseSuit, vp : Int) extends OptionAction("Ritual", s.elem, vp.vp, dt.CardSuit(s)) with LCDaylightQuestion with Soft with Only[FanaticRevealDiscardCardAction] { def tag = implicitly }
case class FanaticScoreAction(self : Fanatic, s : BaseSuit, vp : Int, d : DeckCard) extends ForcedAction

case class FanaticSacrificeMainAction(self : Fanatic, s : $[Suit]) extends OptionAction("Sacrifice", s./(s => dt.CardSuit(s)).merge) with LCDaylightQuestion with Soft with Only[FanaticSacrificeAction] { def tag = implicitly }
case class FanaticSacrificeAction(self : Fanatic) extends ForcedAction

case class FanaticRevealCardMainAction(self : Fanatic, s : $[Suit], then : ForcedAction) extends ForcedAction with Soft
case class FanaticRevealCardAction(self : Fanatic, s : $[Suit], d : DeckCard, then : ForcedAction) extends BaseAction("Reveal", s./(_.elem).commaOr, "card")(d.img) with ViewCard with ExpandThen

case class FanaticRevealDiscardCardMainAction(self : Fanatic, s : BaseSuit, vp : Int) extends ForcedAction with Soft
case class FanaticRevealDiscardCardAction(self : Fanatic, s : BaseSuit, d : DeckCard, vp : Int) extends BaseAction("Discard", s.elem, "card")(d.img) with ViewCard

object FanaticExpansion extends FactionExpansion[Fanatic] {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Fanatic) =>
            game.states += f -> new FanaticPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Fanatic) if options.has(SetupTypeHomelands) =>
            val l = clearings.diff(game.homelands)
            val ll = l.diff(game.homelands./~(game.connected)).some.|(l)
            Ask(f)
                .some(ll)(c => FoxRabbitMouse.intersect(c.suits)./(s => StartingClearingSuitAction(f, c, s).as(c, c.suits.but(s).any.?("(" ~ s.elem ~ ")"))(f, "starts in")))
                .needOk

        case StartingClearingSuitAction(f : Fanatic, c, s) if options.has(SetupTypeHomelands) =>
            game.homelands :+= c

            f.reserve --> Garden(s) --> c
            f.reserve --> 4.times(f.warrior) --> c

            f.log("placed", Garden(s).of(f), "and", f.warrior.sof(f), "in", c)

            val n = game.connected(c).%(f.canPlace)

            var l = n
            while (l.num < 3)
                l ++= n

            val ll = l.combinations(3).$.%(x => l.diff(x).distinct.num == l.diff(x).num)

            Ask(f).each(ll)(l => PlacePieceClearingsAction(f, l, f.warrior, StartingOutcastAction(f)))

        case FactionSetupAction(f : Fanatic) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : Fanatic, c) =>
            Ask(f).each(FoxRabbitMouse.intersect(c.suits))(s => StartingClearingSuitAction(f, c, s).as(c, c.suits.but(s).any.?("(" ~ s.elem ~ ")"))(f, "starts in"))

        case StartingClearingSuitAction(f : Fanatic, c, s) =>
            f.reserve --> Garden(s) --> c
            f.reserve --> 4.times(f.warrior) --> c

            f.log("placed", Garden(s).of(f), "and", Lizard.sof(f), "in", c)

            game.connected(c).foreach { x =>
                f.reserve --> f.warrior --> x

                f.log("placed", Lizard.of(f), "in", x)
            }

            StartingOutcastAction(f)

        case StartingOutcastAction(f) =>
            if (game.options.has(AdSetBuffOn)) {
                f.reserve --> 2.times(f.warrior) --> f.acolytes

                f.log("got", 2.hl, Acolytes.of(f, 2))
            }

            Ask(f)(FoxRabbitMouse./(ChooseOutcastAction(f, _)))

        case ChooseOutcastAction(f, s) =>
            f.outcast = Some(s)

            f.log("declared", s, "to be the", "Outcast".hl)

            SetupFactionsAction

        // HELPER
        case MoveListAction(self, f, t, m, from : Clearing, to : Clearing, l, SpendAcolytesAction(ff, nn, then)) =>
            SpendAcolytesAction(ff, nn, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case BattleStartAction(self, f, a, m, c, o, i, SpendAcolytesAction(ff, nn, then)) =>
            SpendAcolytesAction(ff, nn, BattleStartAction(self, f, a, m, c, o, i, then))

        case LegalAAAConveneAction(f, h, c, e, SpendAcolytesAction(ff, nn, then)) =>
            SpendAcolytesAction(ff, nn, LegalAAAConveneAction(f, h, c, e, then))

        case MoveFinishedAction(f, from, to : Clearing, FanaticCrusadeAttackAction(ff, Nil)) =>
            MoveFinishedAction(f, from, to, FanaticCrusadeAttackAction(ff, $(to)))

        // TRAMPLE
        case BattlePostHitInAction(b, e, f : Fanatic, Garden(s), then) =>
            e.log("trampled", Garden(s).of(f))

            then

        case ForcedRemoveTargetEffectAction(e, c, f : Fanatic, p : Garden, then) if options.has(LimitedTrampleDiscard).not =>
            DiscardRandomCardAction(f, then)

        case ForcedRemoveProcessAction(f : Fanatic, then) =>
            if ((f.used.has(Revenge) || (game.current != f && options.has(RevengeFromAnyRemoval))) && f.ignored.has(IgnoreWarriorRemovalEffects).not) {
                var w = 0

                board.clearings.foreach { c =>
                    val l = f.limbo(c).$.%(_.piece.is[Warrior])

                    l --> f.acolytes

                    w += l.num
                }

                if (f.used.has(Revenge).not && w > 0)
                    f.log("got", Acolytes.of(f, w))
            }

            var g = 0

            board.clearings.foreach { c =>
                val l = f.limbo(c).$.%(_.piece.is[Garden])

                g += l.num
            }

            if (g > 0 && options.has(LimitedTrampleDiscard)) {
                DiscardRandomCardAction(f, then)
            }
            else
                then

        // REVENGE
        case BattlePostHitInAction(b, e, f : Fanatic, Lizard, then) =>
            if (f == b.defender && e == b.attacker && e.is[CommonAbduct]) {
                e.log("smashed", Lizard.of(f))

                f.used :+= Revenge
            }
            else
            if (f.ignored.has(IgnoreWarriorRemovalEffects)) {
                e.log("smashed", Lizard.of(f))

                f.used :+= Revenge
            }
            else
            if (f == b.defender) {
                e.log("smashed", Lizard.of(f) ~ ",", "who became", Acolytes.of(f, 1))

                f.used :+= Revenge
            }
            else
                e.log("smashed", Lizard.of(f))

            then

        // TURN
        case BirdsongNAction(40, f : Fanatic) =>
            f.log("inspected lost souls", f.lost./(_.suit).ss)

            val ss = f.lost./(_.suit).but(Bird)
            val nf = ss.count(Fox)
            val nr = ss.count(Rabbit)
            val nm = ss.count(Mouse)

            val s =
                if (nf > nr && nf > nm)
                    Fox
                else
                if (nr > nf && nr > nm)
                    Rabbit
                else
                if (nm > nf && nm > nr)
                    Mouse
                else
                    Bird

            if (s != Bird && s != f.outcast.get) {
                f.outcast = s.as[BaseSuit]
                f.hated = false

                log(f.outcast, "became the", "Outcast".hl)
            }
            else
            if (!f.hated) {
                f.hated = true

                log(f.outcast, "became the", "Hated Outcast".styled(styles.hit))
            }
            else
                log(f.outcast, "remained the", "Hated Outcast".styled(styles.hit))

            f.lost --> discard.direct

            Next

        case BirdsongNAction(60, f : Fanatic) =>
            var actions : $[UserAction] = $

            val n = f.acolytes.$.num
            val d = f.hated.??(1)

            if (n >= 0) {
                val outcast = f.outcast.get
                val att = clearings.%(f.canAttackIn).%(_.cost.matched(outcast))
                actions :+= FanaticAttackMainAction(f, 2 - d, att).!(att.none, "no targets").!(n < 2 - d, "not enough acolytes")

                val mvv = f.moveFrom.of[Clearing].%(_.cost.matched(outcast))
                actions :+= FanaticCrusadeMainAction(f, 2 - d, mvv).!(mvv.none, "can't move").!(n < 2 - d, "not enough acolytes")

                val cnv = clearings.%(_.cost.matched(outcast)).%(f.canPlace).%(c => f.enemies.%(f.canRemove(c)).%(_.at(c).of[Warrior].notOf[Tenacious].any).any)
                actions :+= FanaticConvertMainAction(f, 2 - d, cnv).!(cnv.none, "no targets").!(n < 2 - d, "not enough acolytes")

                val snf = clearings.%(_.cost.matched(outcast)).%(f.canPlace).%(c => f.enemies.%(f.canRemove(c)).%(_.at(c).of[Building].any).any)
                actions :+= FanaticSanctifyMainAction(f, 3 - d, snf, outcast).!(snf.none, "no targets").!(f.pool(Garden(outcast)).not, "maximum").!(n < 3 - d, "not enough acolytes")
            }

            Ask(f)(actions).done(Next).birdsong(f)

        case SpendAcolytesAction(f, n, then) =>
            f.acolytes --> n.times(f.warrior) --> game.recycle

            then

        case FanaticAttackMainAction(f, n, l) =>
            BattleInitAction(f, f, WithAcolytes(f, n), l, $(CancelAction), SpendAcolytesAction(f, n, Repeat))

        case FanaticCrusadeMainAction(f, n, l) =>
            MoveInitAction(f, f, $, Crusade(f.outcast.get), l, f.movable, CancelAction +: f.birdsong, SpendAcolytesAction(f, n, FanaticCrusadeAttackAction(f, $)))

        case FanaticCrusadeAttackAction(f, l) =>
            BattleInitAction(f, f, Crusade(f.outcast.get), l, $(DoneAction(Repeat)), Repeat)

        case FanaticConvertMainAction(f, n, l) =>
            Ask(f)(l./~(c => f.enemies./~(e => e.at(c).of[Warrior].notOf[Tenacious].distinct./(FanaticConvertAction(f, n, c, e, _))))).cancel

        case FanaticConvertAction(f, n, c, e, p) =>
            game.highlights :+= BattleHighlight(c)

            var cost = n

            if (f.pool(f.warrior).not) {
                f.acolytes --?> f.warrior --> f.reserve
                cost -= f.pooled(f.warrior)
            }

            f.log("converted", p.of(e), "in", c, WithAcolytes(f, n))

            SpendAcolytesAction(f, cost, TryForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), NoMessage, FanaticConvertPlaceAction(f, c, f.warrior, ForcedRemoveFinishedAction(e, Repeat)), Repeat))

        case FanaticConvertPlaceAction(f, c, p, then) =>
            f.reserve --> p --> c

            then

        case FanaticSanctifyMainAction(f, n, l, s) =>
            val aa = l./~(c => factions.but(f)./~(e => e.at(c).of[Building]./(b => FanaticSanctifyAction(f, n, c, s, e, b))))
            Ask(f)(aa).cancel

        case FanaticSanctifyAction(f, n, c, s, e, p) =>
            game.highlights :+= BattleHighlight(c)

            f.log("sanctified", p.of(e), "in", c, WithAcolytes(f, n))

            SpendAcolytesAction(f, n, TryForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), Sanctifying, FanaticSanctifyPlaceAction(f, c, Garden(s), ForcedRemoveFinishedAction(e, Repeat)), Repeat))

        case FanaticSanctifyPlaceAction(f, c, p, then) =>
            f.reserve --> p --> c

            then

        case DaylightNAction(50, f : Fanatic) if f.hand.none =>
            Ask(f).done(Next).daylight(f)

        case DaylightNAction(50, f : Fanatic) =>
            var actions : $[UserAction] = $

            actions :+= FanaticSacrificeMainAction(f, f.hand.%(_.suit == Bird)./(_.suit)).!(f.pool(Lizard).not, "maximum").!(f.hand.%(_.suit == Bird).none, "no bird cards")

            FoxRabbitMouse.foreach { s =>
                val n = $(4, 3, 2, 2, 0, 0)(f.pooled(Garden(s)))
                actions :+= FanaticScoreMainAction(f, s, n).!(f.scored.contains(s), "once per turn").!(n == 0, "not enough gardens").!(f.hand.%(_.suit == s).none, "no matching cards")
            }

            val s = f.hand./(_.suit)./~{
                case s : BaseSuit => Some(s)
                case _ => None
            }.distinct

            val llll = clearings.%(f.canBuild)
            val lll = llll.%(f.rules)

            val bs = s.%(s => lll.exists(c => s.matches(c.cost) && FoxRabbitMouse.exists(s => f.pool(Garden(s)) && s.matches(c.cost))))

            val ll = lll.%(c => s.exists(_.matches(c.cost)))
            val l = ll.%(c => FoxRabbitMouse.exists(s => f.pool(Garden(s)) && s.matches(c.cost)))

            actions :+= FanaticBuildMainAction(f, bs, l).!(llll.none, "no place").!(lll.none, "no rule").!(ll.none, "no matching cards").!(l.none, "maximum")

            actions :+= FanaticRecruitMainAction(f, s, clearings.%(f.canPlace).%(c => s.exists(_.matches(c.cost)))).!(f.pool(Lizard).not, "maximum").!(s.none, "no matching cards")

            Ask(f)(actions).done(Next).daylight(f)

        case FanaticSacrificeMainAction(f, _) =>
            FanaticRevealCardMainAction(f, $(Bird), FanaticSacrificeAction(f))

        case FanaticSacrificeAction(f) =>
            game.highlights :+= NothingHighlight

            f.reserve --> f.warrior --> f.acolytes

            f.log("made a sacrifice with", f.drawn.get)

            f.drawn --> f.revealed

            Repeat

        case FanaticRecruitMainAction(f, s, l) =>
            Ask(f).each(l)(c => FanaticRecruitClearingAction(f, c.suits, c)).cancel

        case FanaticRecruitClearingAction(f, s, c) =>
            FanaticRevealCardMainAction(f, s, FanaticRecruitAction(f, c))

        case FanaticRecruitAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> f.warrior --> c

            f.log("recruited", Lizard.of(f), "in", c, "with", f.drawn.get)

            f.drawn --> f.revealed

            Repeat

        case FanaticBuildMainAction(f, s, l) =>
            Ask(f)
                .some(l)(c => FoxRabbitMouse.%(_.matches(c.cost)).%(s => f.pool(Garden(s)))./(s => FanaticBuildClearingAction(f, s, c)))
                .cancel

        case FanaticBuildClearingAction(f, s, c) =>
            FanaticRevealCardMainAction(f, c.cost @@ {
                case AnyOf(l) => l
                case s : BaseSuit => $(s)
            }, FanaticBuildAction(f, s, c))

        case FanaticBuildAction(f, s, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> Garden(s) --> c

            f.log("built", Garden(s).of(f), "in", c, "with", f.drawn.get)

            f.drawn --> f.revealed

            Repeat

        case FanaticScoreMainAction(f, s, n) =>
            FanaticRevealDiscardCardMainAction(f, s, n)

        case FanaticRevealDiscardCardMainAction(f, s, vp) =>
            Ask(f).each(f.hand)(d => FanaticRevealDiscardCardAction(f, s, d, vp).!(d.suit != s)).cancel

        case FanaticRevealDiscardCardAction(f, s, d, vp) =>
            FanaticScoreAction(f, s, vp, d)

        case FanaticScoreAction(f, s, n, d) =>
            game.highlights :+= NothingHighlight

            f.hand --> d --> discard.quiet

            f.nscore(n)(s, "Gardens".styled(f))(f, "performed ritual in", s, "Gardens".styled(f), ForVP, "with", d)

            f.scored :+= s

            Repeat

        case FanaticRevealCardMainAction(f, s, t) =>
            Ask(f).each(f.hand)(d => FanaticRevealCardAction(f, s, d, t).!(s.exists(s => d.matches(s) && (s == d.suit || d.suit != Bird)).not)).cancel

        case FanaticRevealCardAction(f, _, d, t) =>
            f.hand --> d --> f.drawn

            t

        case EveningNAction(40, f : Fanatic) =>
            f.log("got back", f.revealed.get)

            f.revealed --> f.hand

            Next

        case EveningNAction(50, f : Fanatic) =>
            XCraftMainAction(f)

        case EveningNAction(60, f : Fanatic) =>
            Ask(f).evening(f).done(Next)

        case NightStartAction(f : Fanatic) =>
            EveningDrawAction(f, 1 + FoxRabbitMouse.%(s => f.pooled(Garden(s)) < 4).num)

        case EveningHandLimitAction(f : Fanatic) if options.has(LizardHandSizeSix) =>
            HandLimitAction(f, 6, EveningAfterHandLimitAction(f))

        case FactionCleanUpAction(f : Fanatic) =>
            f.scored = $

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
