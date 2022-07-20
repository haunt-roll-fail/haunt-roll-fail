package root

import root.gaming._

import colmat._

import hrf.elem._
import root.elem._

trait Fanatic extends WarriorFaction {
    val expansion = FanaticExpansion

    val warrior = Lizard

    val abilities = List(LostSouls, EveningCraft)

    val pieces = Lizard ** 25 ++ Garden(Fox) ** 5 ++ Garden(Rabbit) ** 5 ++ Garden(Mouse) ** 5

    def advertising = Garden(Fox).img(this) ~ Garden(Rabbit).img(this) ~ Garden(Mouse).img(this)
    def motto = "Ritual".styled(this)
}

case object LC extends Fanatic {
    val name = "The Lizard Cult"
    override def funName = NameReference(name, this) ~ " Cult"
    val short = "LC"
    val style = "lc"
    val priority = "F"
}

case class Garden(suit : BaseSuit) extends Building {
    override def id = "Garden"
    override def imgid(f : Faction) = f.short + "-" + name + "-" + suit.name
}
    
case object Lizard extends Warrior

case object Crusaders extends FactionEffect
case object LostSouls extends FactionEffect
case object Acolytes extends FactionEffect {
    def of(f : Faction, n : Int) = (n == 1).?("Acolyte").|(name).styled(f)
    def img(f : Faction) = Image(f.short + "-acolyte", styles.piece)
    def imgd(f : Faction) = Image(f.short + "-acolyte", styles.fund)
}

class FanaticPlayer(val game : Game, val faction : Fanatic) extends PlayerState {
    var outcast : BaseSuit = null
    var hated = false

    var scored : List[BaseSuit] = Nil
    
    var lost = cards("lost-souls")
    
    var revealed = cards("revealed")

    val acolytes = figures("acolytes", Nil, _.piece == Lizard)
    
    def craft = faction.figures.diff(pool)./(_.piece)./~ {
        case Garden(s) if s == outcast => Some(s)
        case _ => None
    }
}

trait LCBirdsongQuestion extends FactionAction {
    override def self : Fanatic
    def question(g : Game) = self.elem ~ " (" ~ "Birdsong".styled(styles.phase) ~ ") " ~
        Div(
            Image("outcast-" + g.lc(self).outcast + g.lc(self).hated.??("-hated"), styles.outcast) ~ 
            Acolytes.img(self).repeat(g.lc(self).acolytes.num) ~
            Image("outcast-" + g.lc(self).outcast + g.lc(self).hated.??("-hated"), styles.outcast),
        styles.margined)
}

trait LCDaylightQuestion extends FactionAction {
    override def self : Fanatic
    def question(g : Game) = self.elem ~ " (" ~ "Daylight".styled(styles.phase) ~ ") " ~ g.lc(self).hand./(_.suit).ss
}


case class Crusade(s : Suit) extends Message {
    def elem(g : Game) = "(" ~ "Crusade".styled(s) ~ ") "
}

case class WithAcolytes(f : Faction, n : Int) extends Message {
    def elem(g : Game) = "with " ~ n.hl ~ " " ~ Acolytes.of(f, 1).repeat(n).join(", ")
}

case class StartingOutcastAction(self : Fanatic) extends ForcedAction
case class ChooseOutcastAction(self : Fanatic, s : BaseSuit) extends BaseAction("Choose outcast")(s)

case class SpendAcolytesAction(self : Fanatic, n : Int) extends ForcedAction
case class LizardAttackMainAction(self : Fanatic, cost : Int, l : List[Clearing]) extends OptionAction(g => "Battle".styled(g.lc(self).outcast), Acolytes.imgd(self).repeat(cost).merge, dt.Battle) with LCBirdsongQuestion with Soft
case class LizardCrusadeMainAction(self : Fanatic, cost : Int, l : List[Clearing]) extends OptionAction(g => "Crusade".styled(g.lc(self).outcast), Acolytes.imgd(self).repeat(cost).merge, dt.Move ~ dt.Battle) with LCBirdsongQuestion with Soft
case class LizardCrusadeAttackAction(self : Fanatic, cost : Int, l : List[Clearing]) extends ForcedAction
case class LizardConvertMainAction(self : Fanatic, cost : Int, l : List[Clearing]) extends OptionAction(g => "Convert".styled(g.lc(self).outcast), Acolytes.imgd(self).repeat(cost).merge, dt.Swap, self.warrior.imgd(self)) with LCBirdsongQuestion with Soft
case class LizardConvertAction(self : Fanatic, cost : Int, c : Clearing, e : Faction, p : Warrior) extends BaseAction(g => "Convert".styled(g.lc(self).outcast), "(" ~ cost.hl, Acolytes.of(self, cost) ~ ")")(p.of(e), "in", c)
case class LizardSanctifyMainAction(self : Fanatic, cost : Int, l : List[Clearing]) extends OptionAction(g => "Sanctify".styled(g.lc(self).outcast), Acolytes.imgd(self).repeat(cost).merge, dt.Swap, Image("building-any", styles.piece)) with LCBirdsongQuestion with Soft
case class LizardSanctifyAction(self : Fanatic, cost : Int, c : Clearing, e : Faction, p : Building) extends BaseAction(g => "Sanctify".styled(g.lc(self).outcast), "(" ~ cost.hl, Acolytes.of(self, cost) ~ ")")(p.of(e), "in", c)

case class LizardRecruitMainAction(self : Fanatic, s : List[BaseSuit], l : List[Clearing]) extends OptionAction("Recruit", s./(s => dt.CardSuit(s)).merge) with LCDaylightQuestion with Soft
case class LizardRecruitClearingAction(self : Fanatic, s : BaseSuit, c : Clearing) extends BaseAction("Recruit in")(c) with Soft
case class LizardRecruitAction(self : Fanatic, c : Clearing) extends ForcedAction

case class LizardBuildMainAction(self : Fanatic, s : List[BaseSuit], l : List[Clearing]) extends OptionAction("Build", s./(s => dt.CardSuit(s)).merge) with LCDaylightQuestion with Soft
case class LizardBuildClearingAction(self : Fanatic, s : BaseSuit, c : Clearing) extends BaseAction("Build in")(c) with Soft
case class LizardBuildAction(self : Fanatic, c : Clearing) extends ForcedAction

case class LizardScoreMainAction(self : Fanatic, s : BaseSuit, vp : Int) extends OptionAction("Ritual", s.elem, vp.vp, dt.CardSuit(s)) with LCDaylightQuestion with Soft
case class LizardScoreAction(self : Fanatic, s : BaseSuit, vp : Int, d : DeckCard) extends ForcedAction

case class LizardSacrificeMainAction(self : Fanatic, s : List[Suit]) extends OptionAction("Sacrifice", s./(s => dt.CardSuit(s)).merge) with LCDaylightQuestion with Soft
case class LizardSacrificeAction(self : Fanatic) extends ForcedAction

case class LizardRevealCardMainAction(self : Fanatic, s : Suit, then : ForcedAction) extends ForcedAction with Soft
case class LizardRevealCardAction(self : Fanatic, s : Suit, d : DeckCard, then : ForcedAction) extends BaseAction("Reveal", s.elem, "card")(d.img) with ViewCard

case class LizardRevealDiscardCardMainAction(self : Fanatic, s : BaseSuit, vp : Int) extends ForcedAction with Soft
case class LizardRevealDiscardCardAction(self : Fanatic, s : BaseSuit, d : DeckCard, vp : Int) extends BaseAction("Discard", s.elem, "card")(d.img) with ViewCard

object FanaticExpansion extends Expansion {
    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            // SETUP
            case CreatePlayerAction(f : Fanatic) =>
                pstates += f -> new FanaticPlayer(game, f)
                FactionInitAction(f)
                
            case FactionSetupAction(f : Fanatic) if options.has(SetupTypeHomelands) =>
                val l = board.clearings.diff(homelands)
                val ll = l.diff(homelands./~(board.connected)).some.|(l)
                Ask(f)(ll./(c => StartingClearingAction(f, c).as(c)(f, "starts in"))).needOk

            case StartingClearingAction(f : Fanatic, c) if options.has(SetupTypeHomelands) =>
                homelands :+= c

                f.pool.one(Garden(c.suit)) --> c
                f.pool.sub(4, Lizard) --> c
                
                f.log("started in", c)

                val n = board.connected(c).%(canPlace(f))

                var l = n
                while (l.num < 3)
                    l ++= n
                    
                val ll = l.combinations(3).toList.%(x => l.diff(x).distinct.num == l.diff(x).num)

                Ask(f)(ll./(l => PlaceStartingWarriorsAction(f, l, f.warrior, StartingOutcastAction(f))))

            case FactionSetupAction(f : Fanatic) =>
                StartingCornerAction(f)
                
            case StartingClearingAction(f : Fanatic, c) =>
                f.pool.one(Garden(c.suit)) --> c
                f.pool.sub(4, Lizard) --> c

                board.connected(c).foreach { x =>
                    f.pool.one(Lizard) --> x
                }
                
                f.log("started in", c)
         
                StartingOutcastAction(f)
                
            case StartingOutcastAction(f) =>
                if (game.options.has(AdSetBuffOn)) {
                    f.pool.sub(2, Lizard) --> f.acolytes

                    f.log("got", 2.hl, Acolytes.of(f, 2))
                }
         
                Ask(f, FoxRabbitMouse./(ChooseOutcastAction(f, _)))

            case ChooseOutcastAction(f, s) =>
                f.outcast = s

                f.log("declared", s, "to be the", "Outcast".hl)

                SetupNextAction
                
            // HELPER
            case MoveFinishedAction(f, from, to : Clearing, LizardCrusadeAttackAction(ff, n, Nil)) =>
                MoveFinishedAction(f, from, to, LizardCrusadeAttackAction(ff, n, $(to)))
            
            case BattlePostHitInAction(b, e, f : Fanatic, Garden(s), then) =>
                e.log("trampled", Garden(s).of(f))
                then

            case BattlePostHitInAction(b, e, f : Fanatic, Lizard, then) =>
                if (f == b.defender && f.pooled(Lizard) > 0) {
                    f.limbo.one(Lizard) --> f.acolytes
                    e.log("smashed", Lizard.of(f) ~ ",", f, "gained", Acolytes.of(f, 1))
                }
                else
                    e.log("smashed", Lizard.of(f))

                then

            case ForcedRemoveEffectAction(e, c, f : Fanatic, Garden(_), then) =>
                DiscardRandomCardAction(f, then)
                
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

                if (s != Bird && s != f.outcast) {
                    f.outcast = s.asInstanceOf[BaseSuit]
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
                    
                f.lost.foreach(moveToPile(f.lost, _))

                Next
                
            case BirdsongNAction(60, f : Fanatic) =>
                var actions : List[UserAction] = Nil

                val n = f.acolytes.num
                val d = f.hated.??(1)

                if (n >= 0) {
                    val att = clearings.%(c => attack(f)(c).any).%(_.suit == f.outcast)
                    actions :+= LizardAttackMainAction(f, 2 - d, att).x(att.none, "no targets").x(n < 2 - d, "not enough acolytes")
                        
                    val mvv = moveFrom(f).%(_.suit == f.outcast)
                    actions :+= LizardCrusadeMainAction(f, 2 - d, mvv).x(mvv.none, "can't move").x(n < 2 - d, "not enough acolytes")
                    
                    val cnv = clearings.%(_.suit == f.outcast).%(canPlace(f)).%(c => factions.but(f).%(_.at(c).warrior.%(_.piece.pawn.none).any).any)
                    actions :+= LizardConvertMainAction(f, 2 - d, cnv).x(cnv.none, "no targets").x(f.pooled(Lizard) == 0, "maximum").x(n < 2 - d, "not enough acolytes")
                    
                    val snf = clearings.%(_.suit == f.outcast).%(canPlace(f)).%(c => factions.but(f).%(_.at(c).building.any).any)
                    actions :+= LizardSanctifyMainAction(f, 3 - d, snf).x(snf.none, "no targets").x(f.pooled(Garden(f.outcast)) == 0, "maximum").x(n < 3 - d, "not enough acolytes")
                }
                
                Ask(f, actions.done(Next).birdsong(f))

            case SpendAcolytesAction(f, n) =>
                f.acolytes.sub(n, Lizard) --> f.pool

                Repeat
                
            case LizardAttackMainAction(f, n, l) =>
                BattleInitAction(f, WithAcolytes(f, n), l, $(CancelAction), SpendAcolytesAction(f, n))
                
            case LizardCrusadeMainAction(f, n, l) =>
                MoveInitAction(f, Nil, Crusade(f.outcast), l, movable(f), CancelAction +: f.birdsong, LizardCrusadeAttackAction(f, n, Nil))
                
            case LizardCrusadeAttackAction(f, n, l) =>
                BattleInitAction(f, Crusade(f.outcast), l, $(DoneAction(SpendAcolytesAction(f, n))), SpendAcolytesAction(f, n))
                
            case LizardConvertMainAction(f, n, l) =>
                val aa = l./~(c => factions.but(f)./~(e => e.at(c)./~(_.piece.warrior).%(_.pawn.none).distinct./(LizardConvertAction(f, n, c, e, _))))
                Ask(f, aa.cancel)
                
            case LizardConvertAction(f, n, c, e, p) =>
                highlights :+= BattleHighlight(c)

                e.at(c).one(p) --> e.limbo
                f.pool.one(Lizard) --> c

                f.log("converted", p.of(e), "in", c, WithAcolytes(f, n))

                ForcedRemoveAction(f, c, e, p, p.scoring.any.??(1), NoMessage, ForcedRemoveCleanupAction(e, SpendAcolytesAction(f, n)))
                
            case LizardSanctifyMainAction(f, n, l) =>
                val aa = l./~(c => factions.but(f)./~(e => e.at(c)./~(_.piece.building).distinct./(LizardSanctifyAction(f, n, c, e, _))))
                Ask(f, aa.cancel)
 
            case LizardSanctifyAction(f, n, c, e, p) =>
                highlights :+= BattleHighlight(c)

                e.at(c).one(p) --> e.limbo
                f.pool.one(Garden(c.suit)) --> c
                
                f.log("sanctified", p.of(e), "in", c, WithAcolytes(f, n))
                
                ForcedRemoveAction(f, c, e, p, p.scoring.any.??(1), Sanctifying, ForcedRemoveCleanupAction(e, SpendAcolytesAction(f, n)))

            case DaylightNAction(50, f : Fanatic) =>
                var actions : List[UserAction] = Nil
                
                if (f.hand.any) {
                    actions :+= LizardSacrificeMainAction(f, f.hand.%(_.suit == Bird)./(_.suit)).x(f.pooled(Lizard) == 0, "maximum").x(f.hand.%(_.suit == Bird).none, "no bird cards")

                    FoxRabbitMouse.foreach { s =>
                        val n = List(4, 3, 2, 2, 0, 0)(f.pooled(Garden(s)))
                        actions :+= LizardScoreMainAction(f, s, n).x(f.scored.contains(s), "once per turn").x(n == 0, "not enough gardens").x(f.hand.%(_.suit == s).none, "no matching cards")
                    }
                    
                    val s = f.hand./(_.suit)./~{
                        case s : BaseSuit => Some(s)
                        case _ => None
                    }

                    val llll = clearings.%(canBuild(f))
                    val lll = llll.%(rule(f))
                    val ll = lll.%(c => s.contains(c.suit))
                    val l = ll.%(c => f.pooled(Garden(c.suit)) > 0)
                    
                    actions :+= LizardBuildMainAction(f, s.%(l./(_.suit).contains), l).x(llll.none, "no place").x(lll.none, "no rule").x(ll.none, "no matching cards").x(l.none, "maximum")
                    
                    actions :+= LizardRecruitMainAction(f, s, clearings.%(canPlace(f)).%(c => s.contains(c.suit))).x(f.pooled(Lizard) == 0, "maximum").x(s.none, "no matching cards")
                }

                Ask(f, actions.done(Next).daylight(f))

            case LizardSacrificeMainAction(f, _) =>
                LizardRevealCardMainAction(f, Bird, LizardSacrificeAction(f))
                
            case LizardSacrificeAction(f) =>
                highlights :+= NothingHighlight

                f.pool.one(Lizard) --> f.acolytes
                
                f.log("made a sacrifice with", f.drawn.get)

                f.drawn --> f.revealed

                Repeat
               
            case LizardRecruitMainAction(f, s, l) =>
                Ask(f, l./(c => LizardRecruitClearingAction(f, c.suit, c)).cancel)

            case LizardRecruitClearingAction(f, s, c) =>
                LizardRevealCardMainAction(f, s, LizardRecruitAction(f, c))
                
            case LizardRecruitAction(f, c) =>
                highlights :+= PlaceHighlight(List(c))

                f.pool.one(Lizard) --> c
                
                f.log("recruited", Lizard.of(f), "in", c, "with", f.drawn.get)
                
                f.drawn --> f.revealed

                Repeat
                
            case LizardBuildMainAction(f, s, l) =>
                Ask(f, l./(c => LizardBuildClearingAction(f, c.suit, c)).cancel)

            case LizardBuildClearingAction(f, s, c) =>
                LizardRevealCardMainAction(f, s, LizardBuildAction(f, c))
                
            case LizardBuildAction(f, c) =>
                highlights :+= PlaceHighlight(List(c))

                f.pool.one(Garden(c.suit)) --> c
                
                f.log("built", Garden(c.suit).of(f), "in", c, "with", f.drawn.get)
                
                f.drawn --> f.revealed

                Repeat
                
            case LizardScoreMainAction(f, s, n) =>
                LizardRevealDiscardCardMainAction(f, s, n)
                
            case LizardRevealDiscardCardMainAction(f, s, vp) =>
                Ask(f, f.hand./(d => LizardRevealDiscardCardAction(f, s, d, vp).x(d.suit != s)).cancel)

            case LizardRevealDiscardCardAction(f, s, d, vp) =>
                LizardScoreAction(f, s, vp, d)

            case LizardScoreAction(f, s, n, d) =>
                highlights :+= NothingHighlight
                                                
                f.hand --> d --> discard(f).quiet
                
                f.nscore(n)(s, "Gardens".styled(f))(f, "performed ritual in", s, "Gardens".styled(f), ForVP, "with", d)

                f.scored :+= s

                Repeat
                
            case LizardRevealCardMainAction(f, s, t) =>
                Ask(f, f.hand./(d => LizardRevealCardAction(f, s, d, t).x(d.suit != s)).cancel)

            case LizardRevealCardAction(f, _, d, t) =>
                f.hand --> d --> f.drawn

                t
                
            case EveningNAction(40, f : Fanatic) =>
                f.log("got back", f.revealed.get)
                f.revealed --> f.hand
                Next

            case EveningNAction(50, f : Fanatic) =>
                XCraftMainAction(f)

            case NightStartAction(f : Fanatic) =>
                EveningDrawAction(f, 1 + FoxRabbitMouse.%(s => f.pooled(Garden(s)) < 4).num)
                
            case FactionCleanUpAction(f : Fanatic) =>
                f.scored = Nil
                EndPlayerTurnAction(f)
                
            case _ => UnknownContinue
        }
    }
}

