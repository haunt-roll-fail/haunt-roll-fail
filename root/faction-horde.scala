package root

import root.gaming._

import colmat._

import hrf.elem._
import root.elem._

trait Horde extends WarriorFaction {
    val expansion = HordeExpansion

    val warrior = Rat

    val abilities = List(Looters, DaylightCraft)

    override val transports : List[List[Transport]] = $($(RuledMove, WarlordFirst))

    val pieces = Warlord ** 1 ++ Rat ** 20 ++ Stronghold ** 6 ++ Mob ** 5

    val warlord = figures(0)
}

case object LH extends Horde {
    val name = "Lord of the Hundreds"
    override def funName = NameReference(name, this) ~ " of the Hundreds"
    val short = "LH"
    val style = "lh"
    val priority = "I"
    
    def advertising = Mob.img(this).repeat(5).merge
    def motto = "Oppress".styled(this)
}

case object Looters extends FactionEffect with BattleEffect

case object Stronghold extends Building    

case object Mob extends Token

case object Warlord extends Warrior with Pawn

case object Rat extends Warrior


abstract class Mood(val item : Item) extends Effect with Record {
    def desc(f : Horde) : Elem
}

case object Stubborn extends Mood(Crossbow) with BattleEffect {
    def desc(f : Horde) = Warlord.img(f) ~ " ignores " ~ 1.hit
}
case object Relentless extends Mood(Bag) {
    def desc(f : Horde) = Image(f.style + "-move", styles.piece) ~ Image(f.style + "-battle", styles.piece) ~ dt.ArrowWide ~ Image(f.style + "-action", styles.piece)
}
case object Bitter extends Mood(Hammer) with BattleEffect {
    def desc(f : Horde) = Mob.imgd(f) ~ dt.ArrowWide ~ f.warrior.img(f)
}
case object Grandiose extends Mood(Teapot) {
    def desc(f : Horde) = "Advance ".styled(styles.phase) ~ dt.Swap ~ " Command".styled(styles.phase)
}
case object Jubilant extends Mood(Boots) {
    def desc(f : Horde) = Mob.imgd(f) ~ dt.ArrowWide ~ Image(f.style + "-mob-q", styles.dt.token).repeat(4).merge
}
case object Rowdy extends Mood(Coins) {
    def desc(f : Horde) = dt.CardBack ~ " | " ~ dt.CardBack.repeat(2)
}
case object Wrathful extends Mood(Sword) with BattleEffect {
    def desc(f : Horde) = Warlord.img(f) ~ " deals extra " ~ 1.hit
}
case object Lavish extends Mood(Torch) {
    def desc(f : Horde) = Image("item-any", styles.piece) ~ dt.ArrowWide ~ f.warrior.img(f).repeat(2).merge
}

object Mood {
    def all = $(Stubborn, Relentless, Bitter, Grandiose, Jubilant, Rowdy, Wrathful, Lavish)
}


object BaseSuitDie extends Die[BaseSuit]($(Fox, Rabbit, Mouse))



class HordePlayer(val game : Game, val faction : Horde) extends PlayerState {
    var acted = 0

    var mood : Mood = Stubborn
    
    override def abilities = $(mood)

    var advancing = false
    var skipped = false

    object hoard {
        var command = $[Item]()
        var prowess = $[Item]()
    }

    var fromRuins = $[Item]()
    var wasted = $[Item]()

    def command = hoard.command.num match {
        case 0 => 1
        case 1 => 2
        case x => x
    }

    def prowess = hoard.prowess.num match {
        case 0 => 1
        case 1 => 2
        case x => x
    }
    
    def region = game.pieces.find(faction.warlord).key match {
        case r : Clearing => Some(r)
        case q => None
    }

    def craft = all(Stronghold)./(game.mapping)

    def actionInfo(style : Style, warlordStyle : Style) = {
        import game._

        val f = faction

        val cn = f.advancing.?(f.can(Grandiose).?(0).|(f.command)).|(f.acted)
        val an = f.can(Relentless).?(
            f.advancing.?((f.acted % 2) match {
                case 0 if f.skipped => f.acted / 2 * 3
                case 0 if f.skipped.not => f.acted / 2 * 3 - 1
                case 1 => (f.acted - 1) / 2 * 3 + 1
            }).|(0)
        ).|(f.advancing.?(f.acted).|(f.can(Grandiose).?(f.prowess * 2).|(0)))
        
        val c = 
            1.to(f.command)./(_ => Image("action-black", style)).take(cn) ~
            1.to(f.command)./(_ => Image(f.style + "-action", style)).drop(cn)  

        val a = Image(f.style + "-warlord", warlordStyle) ~
            1.to(f.prowess)./~(_ => (Image(f.style + "-move-done", style) :: Image(f.style + "-battle-done", style)) ++ f.can(Relentless).??(List(Image("action-black", style)))).take(an) ~ 
            1.to(f.prowess)./~(_ => (Image(f.style + "-move",      style) :: Image(f.style + "-battle",      style)) ++ f.can(Relentless).??(List(Image(f.style + "-action",  style)))).drop(an)
            
        f.can(Grandiose).?(a ~ "|".spn(style) ~ c).|(c ~ "|".spn(style) ~ a)
    }
}

case object WarlordMove extends Transport {
    override def allows(g : Game, f : Faction, o : Region) = f @@ {
        case f : Horde => 
            import g._

            f.region == Some(o)
        case _ => false
    }
    
    override def allows(g : Game, f : Faction, o : Region, d : Region, l : List[Movable]) = allows(g, f, o, d) && l.has(Warlord)
}

case object WarlordFirst extends Transport {
    override def order(l : List[Movable]) : Int = l.has(Warlord).not.??(999) + l.num
    override def sortBy(m : Movable) : Int = (m == Warlord).??(-1)
}

trait LHDaylightQuestion extends FactionAction {
    val zzz = styles.action
    override def self : Horde
    def question(g : Game) = {
        import g._

        val f = self

        val cn = f.advancing.?(f.can(Grandiose).?(0).|(f.command)).|(f.acted)
        val an = f.can(Relentless).?(
            f.advancing.?((f.acted % 2) match {
                case 0 if f.skipped => f.acted / 2 * 3
                case 0 if f.skipped.not => f.acted / 2 * 3 - 1
                case 1 => (f.acted - 1) / 2 * 3 + 1
            }).|(0)
        ).|(f.advancing.?(f.acted).|(f.can(Grandiose).?(f.prowess * 2).|(0)))
        
        val t = f.elem ~ " (" ~ f.advancing.?("Advance").|("Command").styled(styles.phase) ~ ") "

        val c = 
            1.to(f.command)./(_ => Image("action-black", zzz)).take(cn) ~
            1.to(f.command)./(_ => Image(f.style + "-action", zzz)).drop(cn)  

        val a = Image(f.style + "-warlord", styles.dt.building) ~
            1.to(f.prowess)./~(_ => (Image(f.style + "-move-done", zzz) :: Image(f.style + "-battle-done", zzz)) ++ f.can(Relentless).??(List(Image("action-black", zzz)))).take(an) ~ 
            1.to(f.prowess)./~(_ => (Image(f.style + "-move",      zzz) :: Image(f.style + "-battle",      zzz)) ++ f.can(Relentless).??(List(Image(f.style + "-action",  zzz)))).drop(an)

        Div(
            t ~ Break ~ f.actionInfo(styles.action, styles.dt.building),
        styles.margined)
    }
}

case class RazeClearingAction(self : Horde, c : Clearing, l : List[Clearing]) extends BaseAction("Raze".styled(self), "in")(c)
case class RazeRuinsAction(self : Horde, c : Clearing, l : List[Clearing]) extends ForcedAction
case class RazeTakeItemAction(self : Horde, i : Item, c : Clearing, l : List[Clearing]) extends BaseAction("Take", "in", c)(i, i.img)
case class RazeContinueAction(self : Horde, l : List[Clearing]) extends ForcedAction

case class RazeSpreadAction(self : Horde, then : ForcedAction) extends ForcedAction
case class RazeSpreadRolledAction(self : Horde, s : BaseSuit, then : ForcedAction) extends RolledAction[BaseSuit] {
    def rolled = List(s)
}
case class RazeSpreadSelectAction(self : Horde, s : BaseSuit, then : ForcedAction) extends ForcedAction
case class RazeSpreadClearingAction(self : Horde, s : BaseSuit, c : Clearing, then : ForcedAction) extends BaseAction("Place", Mob.of(self), Mob.img(self), "in")(c)

case class RecruitXRatsAction(self : Horde, l : List[Clearing], r : List[Clearing]) extends BaseAction("Recruit in")(l)
case class AnointWarlordAction(self : Horde, c : Clearing) extends BaseAction("Anoint", Warlord.of(self), "in")(c)

case class SelectMoodAction(self : Horde, m : Mood) extends BaseAction("Choose new", "Mood".styled(self))((m.item != Torch).??(m.item.img), m.name.hl, m.desc(self))

case class DiscardLavishItemAction(self : Horde, item : Item, c : Option[Clearing], n : Int) extends BaseAction("Discard an item", g => c.??(r => "to place " ~ Image("lh-rat", styles.action).*(n) ~ " in " ~ r.elem(g)))("Discard", Image("item-" + item.name, styles.piece))

case class BattleLootAction(self : Faction, e : Faction, b : Battle, l : List[ItemRef]) extends BaseAction("Loot", e)(l./(_.img))
case class BattleNoLootAction(self : Faction, e : Faction, b : Battle) extends BaseAction(None)("Cancel")

case class BattleLootItemAction(self : Faction, e : Faction, b : Battle, i : ItemRef) extends BaseAction("Loot")(i, i.img)

case class BattleMobAction(self : Faction, c : Clearing, m : Clearing, then : ForcedAction) extends BaseAction("Add", Rat.of(self), Rat.img(self), "to", c, "instead of", Mob.of(self), Mob.img(self), "from")(m)
case class BattleNoMobAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Done")

case class XRatDoneAction(f : Horde, then : ForcedAction) extends ForcedAction

case class XRatAttackAction(self : Horde, l : List[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with LHDaylightQuestion with Soft
case class XRatMoveAction(self : Horde, l : List[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with LHDaylightQuestion with Soft
case class XRatBuildAction(self : Horde, l : List[Clearing]) extends OptionAction("Build", Stronghold.of(self), dt.AnyCard, dt.Arrow, Stronghold.imgd(self)) with LHDaylightQuestion with Soft

case class BuildStrongholdClearingAction(self : Horde, c : Clearing) extends BaseAction("Build", Stronghold.of(self), "in")(c) with Soft
case class BuildStrongholdAction(self : Horde, c : Clearing) extends ForcedAction

case class XRatAdvanceMoveDoneAction(f : Horde, then : ForcedAction) extends ForcedAction
case class XRatAdvanceAttackDoneAction(f : Horde, then : ForcedAction) extends ForcedAction
case class XRatRelentlessAction(f : Horde, then : ForcedAction) extends ForcedAction

case class XRatAdvanceAttackAction(self : Horde, l : List[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with LHDaylightQuestion with Soft
case class XRatAdvanceMoveAction(self : Horde, l : List[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with LHDaylightQuestion with Soft


case class InciteMobClearingAction(self : Horde, c : Clearing) extends BaseAction("Incite", Mob.of(self), Mob.imgd(self), "in")(c) with Soft
case class InciteMobAction(self : Horde, c : Clearing) extends ForcedAction
case class JubilantAction(self : Horde) extends ForcedAction
case class JubilantContinueAction(self : Horde) extends BaseAction(Jubilant.name.hl)("Spread", Image(self.style + "-mob-q", styles.dt.token))
case class JubilantDoneAction(self : Horde) extends ForcedAction


case class CraftRewardItemAction(self : Horde, d : CraftItemCard, then : ForcedAction) extends BaseAction("Contempt for Trade".styled(self))("Get", d.item, d.item.img)
case class CraftRewardVPAction(self : Horde, d : CraftItemCard, then : ForcedAction) extends BaseAction("Contempt for Trade".styled(self))("Score", d.vp.vp)

case class HoardCapacityAction(self : Horde, then : ForcedAction) extends ForcedAction
case class DiscardHoardItemAction(self : Horde, i : Item, then : ForcedAction) extends BaseAction("Discard an item")("Discard", i.img)


case class ToBuildStronghold(f : Faction, c : Clearing) extends Message {
    def elem(g : Game) = " to build " ~ Stronghold.of(f) ~ " in " ~ c.elem(g)
}

case class ToInciteMob(f : Faction, c : Clearing) extends Message {
    def elem(g : Game) = " to incite " ~ Mob.of(f) ~ " in " ~ c.elem(g)
}

object HordeExpansion extends Expansion {
    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            // SETUP
            case CreatePlayerAction(f : Horde) =>
                pstates += f -> new HordePlayer(game, f)
                FactionInitAction(f)
                
            case FactionSetupAction(f : Horde) if options.has(SetupTypeHomelands) =>
                val h = homelands
                val hh = h./~(board.connected)
                val hhh = hh./~(board.connected)

                val l = board.clearings.diff(h)
                val ll = l.diff(board.inner).some.|(l)
                val lll = ll.diff(hh).diff(hhh).some.||(ll.diff(hh).some).|(ll)

                Ask(f)(lll./(c => StartingClearingAction(f, c).as(c)(f, "starts in"))).needOk

            case FactionSetupAction(f : Horde) =>
                StartingCornerAction(f)
                
            case StartingClearingAction(f : Horde, c) =>
                homelands :+= c

                f.pool.one(Stronghold) --> c
                f.pool.sub(4, Rat) --> c
                f.pool.one(Warlord) --> c
         
                SetupNextAction
                    
            // HELPER
            case MoveListAction(f, t, m, from, to, l, XRatDoneAction(ff, then)) =>
                XRatDoneAction(ff, ForceAction(MoveListAction(f, t, m, from, to, l, then)))
            
            case BattleStartAction(f, a, m, c, o, i, XRatDoneAction(ff, then)) =>
                XRatDoneAction(ff, ForceAction(BattleStartAction(f, a, m, c, o, i, then)))

            case MoveListAction(f, t, m, from, to, l, XRatAdvanceMoveDoneAction(ff, then)) =>
                XRatAdvanceMoveDoneAction(ff, ForceAction(MoveListAction(f, t, m, from, to, l, then)))
            
            case BattleStartAction(f, a, m, c, o, i, XRatAdvanceAttackDoneAction(ff, then)) =>
                XRatAdvanceAttackDoneAction(ff, ForceAction(BattleStartAction(f, a, m, c, o, i, then)))

            case BattleAttackerPreRollAction(b) 
                    if b.attacker.can(Bitter) 
                    && b.attacker.as[Horde]./~(_.region) == Some(b.clearing) 
                    && canPlace(b.attacker)(b.clearing) 
                    && b.attacker.all(Mob).%(c => c == b.clearing || connected(b.attacker)(b.clearing).has(c)).any =>

                val f = b.attacker.asInstanceOf[Horde]
                Ask(f, f.all(Mob).%(c => c == b.clearing || connected(b.attacker)(b.clearing).has(c))./(c => BattleMobAction(f, b.clearing, c, BattleAttackerPreRollAction(b))) :+ BattleNoMobAction(f, BattleAttackerPreRollAction(b)))

            case BattleDefenderPreRollAction(b) 
                    if b.defender.can(Bitter) 
                    && b.defender.as[Horde]./~(_.region) == Some(b.clearing) 
                    && canPlace(b.defender)(b.clearing) 
                    && b.defender.all(Mob).%(c => c == b.clearing || connected(b.defender)(b.clearing).has(c)).any =>

                val f = b.defender.asInstanceOf[Horde]
                Ask(f, f.all(Mob).%(c => c == b.clearing || connected(b.defender)(b.clearing).has(c))./(c => BattleMobAction(f, b.clearing, c, BattleDefenderPreRollAction(b))) :+ BattleNoMobAction(f, BattleDefenderPreRollAction(b)))

            case BattleMobAction(f, c, m, then) =>
                f.at(m).one(Mob) --> f.pool
                f.pool.sub(1, Rat) --> c

                f.log("being", Bitter.name.hl, "added", Rat.of(f), "to", c, "from", Mob.of(f), "in", m)

                then

            case BattleNoMobAction(f, then) =>
                f.ignored :+= Bitter

                then

            case BattleStartedAction(b) if b.attacker.can(Looters) && b.defender.forTrade.any =>
                val f = b.attacker
                val e = b.defender
                Ask(f, (BattleLootAction(f, e, b, b.defender.forTrade) :: BattleNoLootAction(f, e, b)))

            case BattleLootAction(f, e, b, _) =>
                f.used :+= Looters

                f.log("wanted to loot")

                BattleDefenderPreRollAction(b)

            case BattleResolvedAction(b) if b.attacker.used.has(Looters) =>
                b.attacker.used :-= Looters
                
                if (b.defender.forTrade.any && rule(b.attacker)(b.clearing))
                    Ask(b.attacker, b.defender.forTrade./(BattleLootItemAction(b.attacker, b.defender, b, _)))
                else {
                    b.attacker.log("failed to loot", b.defender)

                    BattleResolvedAction(b)
                }
                    
            case BattleLootItemAction(f, e, b, i) =>
                e.forTrade :-= i

                f.forTrade :+= i
                
                AcquireItemsAction(f, BattleResolvedAction(b))
                
            case BattleNoLootAction(f, e, b) =>
                f.ignored :+= Looters

                BattleDefenderPreRollAction(b)
            
            case BattleAssignHitsAction(f : Horde, b, n, then) if f.can(Stubborn) && f.region == Some(b.clearing) =>
                f.used :+= Stubborn
                
                f.log("ignored the first hit being", Stubborn.name.hl)

                BattleAssignHitsAction(f, b, n - 1, then)

            
            case BattlePostHitInAction(b, e, f : Horde, Stronghold, then) =>
                e.log("demolished", Stronghold.of(f))
                then

            case BattlePostHitInAction(b, e, f : Horde, Rat, then) =>
                e.log("squashed", Rat.of(f))
                then

            case BattlePostHitInAction(b, e, f : Horde, Warlord, then) =>
                e.log("slaughtered", Warlord.of(f))
                then

            case BattlePostHitInAction(b, e, f : Horde, Mob, then) =>
                e.log("dispersed", Mob.of(f))
                then
                
            case CraftScoreAction(f : Horde, d, n, m, then) if f.forTrade.any =>
                Ask(f, (CraftRewardItemAction(f, d, then) :: CraftRewardVPAction(f, d, CraftScoreAction(f, d, n, m, then))))

            case CraftRewardItemAction(f, d, then) =>
                f.log("crafted", d.item, d, "(" ~ d.cost.ss ~ ")", "for himself")
                AcquireItemsAction(f, then)
            
            case CraftRewardVPAction(f, d, then) =>
                f.forTrade :-= d.item.pristine
                f.wasted :+= d.item

                then

            case AcquireItemsAction(f : Horde, then) =>
                f.forTrade./(_.item).foreach {
                    case i if i.in(Boots, Coins, Bag) => f.hoard.command :+= i
                    case i if i.in(Hammer, Teapot, Sword, Crossbow) => f.hoard.prowess :+= i
                }

                f.forTrade = Nil
                
                HoardCapacityAction(f, then)

            case HoardCapacityAction(f, then) =>
                val d = (f.hoard.command.num > 4).??(f.hoard.command) ++ (f.hoard.prowess.num > 4).??(f.hoard.prowess)
                
                if (d.any)
                    Ask(f, d./(DiscardHoardItemAction(f, _, HoardCapacityAction(f, then))))
                else
                    then
                
            case DiscardHoardItemAction(f, i, then) =>
                f.wasted :+= i
                f.hoard.command :-= i
                f.hoard.prowess :-= i

                f.nscore(1)("item overflow")(f, "discarded", i, "and", ScoredVP)

                then
                
            // TURN

            // RAZE
            case BirdsongNAction(30, f : Horde) =>
                val l = f.all(Mob).%(c => ruins.contains(c) || factions.but(f).%!(f.friends).%(_.at(c)./~(_.piece.scoring).any).any)
            
                if (l.any)
                    Ask(f, l./(RazeClearingAction(f, _, l)).okNeeded.birdsong(f))
                else
                    RazeSpreadAction(f, Next)
                    
            case RazeClearingAction(f, c, l) =>
                NukeAction(f, factions.%(f.friends), List(c), AntiMaterial, RazeRuinsAction(f, c, l))

            case RazeRuinsAction(f, c, l) =>
                if (ruins.contains(c))
                    Ask(f, ruins(c).items./(i => RazeTakeItemAction(f, i, c, l).x(f.fromRuins.contains(i))).okNeeded ++ ruins(c).items.%!(f.fromRuins.contains).none.?(DoneAction(RazeContinueAction(f, l.but(c)))))
                else
                    RazeContinueAction(f, l.but(c))
                    
            case RazeTakeItemAction(f, i, c, l) =>
                val rest = ruins(c).items :- i

                if (rest.any)
                    ruins += c -> Ruins(rest)
                else
                    ruins -= c
                    
                f.forTrade :+= i.pristine
                f.fromRuins :+= i
                    
                f.log("found", i, "in", c, "ruins")

                RazeContinueAction(f, l.but(c))

            case RazeContinueAction(f, l) =>
                if (l.any)
                    Ask(f, l./(RazeClearingAction(f, _, l)).okNeeded)
                else
                    AcquireItemsAction(f, RazeSpreadAction(f, Next))

            case RazeSpreadAction(f, then) =>
                if (f.inpool(Mob))
                    Roll[BaseSuit]($(BaseSuitDie), l => RazeSpreadRolledAction(f, l(0), then), f.elem ~ " rolls raze dice")
                else
                    then

            case RazeSpreadRolledAction(f, s, then) =>
                f.logtemp("rolled", s)

                RazeSpreadSelectAction(f, s, then)
            
            case RazeSpreadSelectAction(f, s, then) =>
                val l = f.all(Mob)
                val ll = clearings.%(_.suit == s).diff(l).%(c => connected(f)(c).intersect(l).any).%(canPlace(f))

                if (ll.any)
                    Ask(f, ll./(RazeSpreadClearingAction(f, s, _, then)).okNeeded)
                else {
                    f.log("rolled", s.elem ~ ", could not spread", Mob.of(f))
                    then
                }

            case RazeSpreadClearingAction(f, s, c, then) =>
                highlights :+= PlaceHighlight(List(c))

                f.pool.one(Mob) --> c

                f.log("rolled", s, "and spread", Mob.of(f), "to", c)

                then
                
            // RECRUIT
            case BirdsongNAction(40, f : Horde) =>
                f.region.%(canPlace(f)).foreach { c =>
                    val n = f.prowess
                    val m = min(f.pooled(Rat), f.prowess)

                    if (m > 0) {
                        f.pool.sub(m, Rat) --> c
                        f.log("recruited", Rat.*(m)./(_.of(f)).comma ~ " in", c)
                    }

                    if (n > m && f.totalWar)
                        f.oscore(n - m)("recruiting")
                }
                
                val t = f.pooled(Rat)
                val r = f.all(Stronghold).%(canPlace(f))
                
                if (t >= r.num)
                    Ask(f, RecruitXRatsAction(f, r, r) :: Nil)
                else
                    Ask(f, r.combinations(t).toList./(RecruitXRatsAction(f, _, r)).birdsong(f))
                    
            case RecruitXRatsAction(f, l, r) =>
                highlights :+= PlaceHighlight(l.distinct)

                l.distinct.foreach { c => 
                    val n = l.count(c)
                    f.pool.sub(n, Rat) --> c
                    f.log("recruited", Rat.*(n)./(_.of(f)).comma ~ " in", c)
                }

                if (r.num > l.num && f.totalWar)
                    f.oscore(r.num - l.num)("recruiting") 
     
                Next

            
            // ANOINT
            case BirdsongNAction(60, f : Horde) =>
                if (f.region.none) {
                    val l = f.all(Rat).distinct.%(canPlace(f)).some.|(clearings.%(canPlace(f)))
                    
                    if (l.any)
                        Ask(f, l./(AnointWarlordAction(f, _)).birdsong(f))
                    else
                        Next
                }
                else
                    Next

            case AnointWarlordAction(f, c) =>
                highlights :+= PlaceHighlight(List(c))

                f.at(c).sub(1, Rat) --> f.pool
                f.pool.one(Warlord) --> c
                
                f.log("anointed", Warlord.of(f), "in", c)
                
                Next

            case BirdsongNAction(70, f : Horde) =>
                val available = Mood.all.%!(m => f.hoard.command.has(m.item) || f.hoard.prowess.has(m.item))

                Ask(f, Mood.all./(m => SelectMoodAction(f, m).x(available.has(m).not).x(available.but(m).any && m == f.mood, "old mood")).birdsong(f))

            case SelectMoodAction(f, m) =>
                f.mood = m
               
                f.log("mood became", m.name.hl)

                if (m == Grandiose) {
                    f.advancing = true
                    f.skipped = true
                }

                Next

            // LAVISH
            case BirdsongNAction(80, f : Horde) if f.can(Lavish) =>
                val d = f.hoard.command ++ f.hoard.prowess
                
                if (d.any && f.region.any)
                    Ask(f, d./(DiscardLavishItemAction(f, _, f.region.%(canPlace(f)).%(_ => f.inpool(Rat)), min(2, f.pooled(Rat)))).done(Next))
                else
                    Next

            case DiscardLavishItemAction(f, i, _, _) =>
                f.wasted :+= i
                f.hoard.command :-= i
                f.hoard.prowess :-= i

                f.log("discarded", i)

                f.region.%(canPlace(f)).foreach { c =>
                    highlights :+= PlaceHighlight(List(c))

                    val n = min(f.pooled(Rat), 2)
                    if (n > 0) {
                        f.pool.sub(n, Rat) --> c
                        f.log("recruited", Rat.*(n)./(_.of(f)).comma ~ " in", c)
                    }
                }

                Repeat

            // CRAFT
            case DaylightNAction(30, f : Horde) =>
                XCraftMainAction(f)
                
            // COMMAND
            case DaylightNAction(n, f : Horde) if n == f.can(Grandiose).not.?(40).|(60) =>
                f.advancing = false
                f.skipped = false
                f.acted = 0

                Next
                
            case DaylightNAction(n, f : Horde) if n == f.can(Grandiose).not.?(50).|(70) =>
                var actions : List[UserAction] = Nil
                
                val att = clearings.%(c => attack(f)(c).any)
                actions :+= XRatAttackAction(f, att).x(att.none)
                    
                val mvv = moveFrom(f)
                actions :+= XRatMoveAction(f, mvv).x(mvv.none)
                
                if (f.acted >= f.command)
                    actions = Nil

                val b0 = clearings
                val b1 = b0.%(rule(f))
                val b2 = b1.%(canBuild(f))
                val b3 = b2.%(canPlace(f))
                val b4 = b3.%(c => f.hand.%(_.suit.m(c.suit)).any)
                
                actions :+= XRatBuildAction(f, b4).x(f.inpool(Stronghold).not, "max").x(b1.none, "no rule").x(b2.none, "no place").x(b3.none, "can't place").x(b4.none, "no matching cards")
                
                if (f.acted >= f.command) {
                    actions = Nil
    
                    actions :+= EndTurnAction(f)
                }
                else
                    actions :+= EndTurnSoftAction(f, ForfeitActions(f.command - f.acted))

                if (f.can(Grandiose))    
                    Ask(f, actions.daylight(f))
                else
                    Ask(f, actions.odaylight(f))

                    
            case XRatAttackAction(f, l) =>
                BattleInitAction(f, NoMessage, l, $(CancelAction), XRatDoneAction(f, Repeat))
                
            case XRatMoveAction(f, l) =>
                MoveInitAction(f, Nil, NoMessage, l, movable(f), $(CancelAction), XRatDoneAction(f, Repeat))

            case XRatBuildAction(f, l) =>
                Ask(f, l./(c => BuildStrongholdClearingAction(f, c).x(f.hand.%(_.suit.m(c.suit)).none, "no matching card")).cancel)

            case BuildStrongholdClearingAction(f, c) =>
                OptionalDiscardCardAction(f, ToBuildStronghold(f, c), c.suit, BuildStrongholdAction(f, c))
                
            case BuildStrongholdAction(f, c) =>
                highlights :+= PlaceHighlight(List(c))

                f.log("built", Stronghold.of(f), "in", c, "with", f.drawn.get)

                f.drawn --> discard(f).quiet

                f.pool.one(Stronghold) --> c
                
                XRatDoneAction(f, Repeat)
                
            case XRatDoneAction(f, then) =>
                f.acted += 1

                then
                
            case DaylightNAction(n, f : Horde) if n == f.can(Grandiose).not.?(60).|(40) =>
                f.advancing = true
                f.skipped = true
                f.acted = 0

                Next

            // ADVANCE 
            case DaylightNAction(n, f : Horde) if n == f.can(Grandiose).not.?(70).|(50) =>
                if (f.region.any) {
                    val r = f.region.get

                    var actions : List[UserAction] = Nil
                
                    val bonus = f.acted % 2 == 0 && f.skipped.not && f.can(Relentless)

                    actions :+= XRatAdvanceAttackAction(f, List(r)).x(attack(f)(r).none)
                    
                    actions :+= XRatAdvanceMoveAction(f, List(r)).x(moveFrom(f).has(r).not).x(f.acted == f.prowess * 2 - 1 && bonus.not)
                
                    if (f.acted >= f.prowess * 2 && bonus.not) {
                        actions = Nil
    
                        actions :+= EndTurnAction(f)
                    }
                    else
                        actions :+= EndTurnSoftAction(f, ForfeitActions(f.prowess * 2 + bonus.??(1) - f.acted))
                        
                    if (f.can(Grandiose))    
                        Ask(f, actions.odaylight(f))
                    else
                        Ask(f, actions.daylight(f))
                }
                else
                    Next
                
            case XRatAdvanceAttackAction(f, l) =>
                BattleInitAction(f, NoMessage, l, $(CancelAction), XRatAdvanceAttackDoneAction(f, Repeat))
                
            case XRatAdvanceMoveAction(f, l) =>
                MoveInitAction(f, transports./($(_)) ** f.transports ** $($(WarlordMove)), NoMessage, l, l, $(CancelAction), XRatAdvanceMoveDoneAction(f, Repeat))
                
            case XRatAdvanceAttackDoneAction(f, then) =>
                if (f.acted % 2 == 0 && f.skipped.not && f.can(Relentless))
                    XRatRelentlessAction(f, then)
                else {
                    f.skipped = f.acted % 2 == 0

                    if (f.skipped)
                        f.acted += 1

                    f.acted += 1

                    then
                }

            case XRatAdvanceMoveDoneAction(f, then) =>
                if (f.acted % 2 == 0 && f.skipped.not && f.can(Relentless))
                    XRatRelentlessAction(f, then)
                else {
                    f.skipped = f.acted % 2 == 1

                    if (f.skipped)
                        f.acted += 1

                    f.acted += 1

                    then
                }
                
            case XRatRelentlessAction(f, then) =>
                f.skipped = true

                f.log("got an extra action being", Relentless.name.hl)

                then                    

            // INCITE    
            case EveningNAction(40, f : Horde) =>
                if (f.inpool(Mob)) {
                    val b0 = clearings
                    val b1 = b0.diff(f.all(Mob))
                    val b2 = b1.%(c => f.at(c).warrior.any)
                    val b3 = b2.%(canPlace(f))
                    val b4 = b3.%(c => f.hand.%(_.suit.m(c.suit)).any)
            
                    if (b3.any)
                        Ask(f, b3./(c => InciteMobClearingAction(f, c).x(f.hand.%(_.suit.m(c.suit)).none, "no matching card")).done(Next).evening(f))
                    else
                        Next
                }
                else
                    Next
                    
            case InciteMobClearingAction(f, c) =>
                OptionalDiscardCardAction(f, ToInciteMob(f, c), c.suit, InciteMobAction(f, c))
                
            case InciteMobAction(f, c) =>
                highlights :+= PlaceHighlight(List(c))

                f.log("incited", Mob.of(f), "in", c, "with", f.drawn.get)

                f.drawn --> discard(f).quiet
                
                f.pool.one(Mob) --> c
 
                if (f.can(Jubilant) && f.region == Some(c)) {
                    f.acted = 0
                    f.log("can spread", Mob.of(f), "as", Jubilant)
                    JubilantAction(f)
                }
                else
                    Repeat

            case JubilantAction(f) =>
                Ask(f, $(JubilantContinueAction(f)).done(Repeat))

            case JubilantContinueAction(f) =>
                RazeSpreadAction(f, JubilantDoneAction(f))

            case JubilantDoneAction(f) =>
                f.acted += 1
                
                if (f.acted < 4)
                    JubilantAction(f)
                else
                    Repeat


            // OPPRESS
            case NightStartAction(f : Horde) =>
                val r = clearings.%(rule(f)).%(c => factions.%!(_.friends(f)).%(_.at(c).any).none)
                val s = r.num match {
                    case 0 => 0
                    case 1 | 2 => 1
                    case 3 | 4 => 2
                    case 5 => 3
                    case _ => 4
                }
                
                f.oscore(s)("oppressing", r.num.hl, "clearing" + (r.num > 1).??("s"))

                val w = f.region
                
                EveningDrawAction(f, 1 + f.can(Rowdy).??(w.any.??(1) + (w./(c => factions.%!(f.friends)./(_.at(c).num).sum).|(0) >= 3).??(1)))
                
            case FactionCleanUpAction(f : Horde) =>
                f.acted = 0
                f.skipped = false
                f.advancing = false

                EndPlayerTurnAction(f)
                
            case _ => UnknownContinue
        }
    }
}

