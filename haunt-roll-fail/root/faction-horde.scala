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

trait Horde extends WarriorFaction {
    val clashKey = LH

    val warrior = Rat

    def abilities(options : $[Meta.O]) = $(Looters, DaylightCraft)

    override val transports : $[$[Transport]] = $($(RuledMove, WarlordFirst))

    def pieces(options : $[Meta.O]) = Warlord *** 1 ++ Rat *** 20 ++ Stronghold *** 6 ++ Mob *** 5

    object items {
      val command = $[Item](Boots, Coins, Bag)
      val prowess = $[Item](Hammer, Teapot, Sword, Crossbow)
    }

    override def initialDamagePriorities(options : $[Meta.O]) = $(Rat, Warlord, Mob, Stronghold)
}

case object LH extends Horde {
    val name = "Lord of the Hundreds"
    override def funName = NameReference(name, this) ~ " of the Hundreds"
    val short = "LH"
    val style = "lh"
    val priority = "I"

    def advertising = 5.times(Mob.img(this)).merge
    def motto = "Oppress".styled(this)
}

case object LK extends Horde {
    val name = "Longtail Kaliph"
    override def funName = NameReference(name, this) ~ " Kaliph"
    val short = "LK"
    val style = "lk"
    val priority = "I'"

    def advertising = 5.times(Mob.img(this)).merge
    def motto = "Oppress".styled(this)
}

case object Looters extends BattleEffect

case object PeerPressured extends Effect

case object Stronghold extends Building

case object Mob extends Token

case object Warlord extends Warrior with Tenacious

case object Rat extends Warrior


abstract class Mood(val item : Item) extends FactionEffect with Record {
    def desc(f : Horde) : Elem
    def long(f : Horde) : $[Elem]
}

case object Stubborn extends Mood(Crossbow) with BattleEffect {
    val name = "Stubborn"
    def desc(f : Horde) = Warlord.img(f) ~ " ignores " ~ 1.hit
    def long(f : Horde) = $("Ignores the first", "hit".styled(styles.hit), "taken in battle")
}

case object Relentless extends Mood(Bag) {
    val name = "Relentless"
    def desc(f : Horde) = Image(f.style + "-move", styles.piece) ~ Image(f.style + "-battle", styles.piece) ~ dt.ArrowWide ~ Image(f.style + "-action", styles.piece)
    def long(f : Horde) = $("Gains an extra", "action".hh, "after both", "move".hh, "and", "battle".hh)
}

case object Bitter extends Mood(Hammer) with BattleEffect {
    val name = "Bitter"
    def desc(f : Horde) = Mob.imgd(f) ~ dt.ArrowWide ~ f.warrior.img(f)
    def long(f : Horde) = $("Can convert", Mob.sof(f), "to", Rat.sof(f), "from neighbouring clearings before roll in battle")
}

case object Grandiose extends Mood(Teapot) {
    val name = "Grandiose"
    def desc(f : Horde) = "Advance ".styled(styles.phase) ~ dt.Swap ~ " Command".styled(styles.phase)
    def long(f : Horde) = $("Performs", "Advance".styled(styles.phase), "actions before", "Command".styled(styles.phase), "actions")
}

case object Jubilant extends Mood(Boots) {
    val name = "Jubilant"
    def desc(f : Horde) = Mob.imgd(f) ~ dt.ArrowWide ~ 4.times(Image(f.style + "-mob-q", styles.dt.token)).merge
    def long(f : Horde) = $("Can try to spread", Mob.of(f), "up to", 4.hl, "times after inciting")
}

case object Rowdy extends Mood(Coins) {
    val name = "Rowdy"
    def desc(f : Horde) = dt.CardBack ~ " | " ~ 2.times(dt.CardBack)
    def long(f : Horde) = $("Gains an extra", "card".hh, "at the turn end,", Break, "plus one more if in a clearing with at least", 3.hl, "enemy pieces")
}

case object Wrathful extends Mood(Sword) with BattleEffect {
    val name = "Wrathful"
    def desc(f : Horde) = Warlord.img(f) ~ " deals extra " ~ 1.hit
    def long(f : Horde) = $("Deals an extra", "hit".styled(styles.hit), "attacking")
}

case object Lavish extends Mood(Torch) {
    val name = "Lavish"
    def desc(f : Horde) = Image("item-any", styles.piece) ~ dt.ArrowWide ~ 2.times(f.warrior.img(f)).merge
    def long(f : Horde) = $("Can discard items to recruit", 2.hl, Rat.sof(f), "for each item")
}

object Mood {
    def all = $(Stubborn, Relentless, Bitter, Grandiose, Jubilant, Rowdy, Wrathful, Lavish)
}


class HordePlayer(val faction : Horde)(implicit val game : Game) extends FactionState {
    var acted = 0

    var mood : Mood = Stubborn

    override def transient = $(mood)

    val warlord = reserve.$.first

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

    def region = game.pieces.find(faction.warlord) match {
        case Some((_, r : Clearing)) => Some(r)
        case q => None
    }

    def craft = all(Stronghold)./(_.asset)

    def actionInfo(style : Style, warlordStyle : Style) = {
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
            1.to(f.prowess)./~(_ => (Image(f.style + "-move-done", style) :: Image(f.style + "-battle-done", style)) ++ f.can(Relentless).$(Image("action-black", style))).take(an) ~
            1.to(f.prowess)./~(_ => (Image(f.style + "-move",      style) :: Image(f.style + "-battle",      style)) ++ f.can(Relentless).$(Image(f.style + "-action",  style))).drop(an)

        f.can(Grandiose).?(a ~ "|".spn(style) ~ c).|(c ~ "|".spn(style) ~ a)
    }
}

case object WarlordMove extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = f @@ {
        case f : Horde => f.region == Some(o)
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && l.has(Warlord)
}

case object WarlordFirst extends Transport {
    override def order(l : $[Movable]) : Int = l.has(Warlord).not.??(999) + l.num
    override def sortBy(m : Movable) : Int = (m == Warlord).??(-1)
}

trait LHDaylightQuestion extends FactionAction {
    val zzz = styles.action
    override def self : Horde
    def question(implicit game : Game) = {
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
            1.to(f.prowess)./~(_ => (Image(f.style + "-move-done", zzz) :: Image(f.style + "-battle-done", zzz)) ++ f.can(Relentless).$(Image("action-black", zzz))).take(an) ~
            1.to(f.prowess)./~(_ => (Image(f.style + "-move",      zzz) :: Image(f.style + "-battle",      zzz)) ++ f.can(Relentless).$(Image(f.style + "-action",  zzz))).drop(an)

        Div(
            t ~ Break ~ f.actionInfo(styles.action, styles.dt.building),
        styles.margined)
    }
}

case class RazeClearingAction(self : Horde, c : Clearing, l : $[Clearing]) extends BaseAction("Raze".styled(self), "in")(c)
case class RazeRuinsAction(self : Horde, c : Clearing, l : $[Clearing]) extends ForcedAction
case class RazeTakeItemAction(self : Horde, i : Item, c : Clearing, l : $[Clearing]) extends BaseAction("Take", "in", c)(i, i.img)
case class RazeContinueAction(self : Horde, l : $[Clearing]) extends ForcedAction

case class RazeSpreadAction(self : Horde, then : ForcedAction) extends ForcedAction
case class RazeSpreadRolledAction(self : Horde, s : BaseSuit, then : ForcedAction) extends RolledAction[BaseSuit] {
    def rolled = $(s)
}
case class RazeSpreadSelectAction(self : Horde, s : BaseSuit, then : ForcedAction) extends ForcedAction
case class RazeSpreadClearingAction(self : Horde, s : BaseSuit, c : Clearing, then : ForcedAction) extends BaseAction("Place", Mob.of(self), Mob.img(self), "in")(c)

case class HordeRecruitAction(self : Horde, l : $[Clearing], r : $[Clearing]) extends BaseAction("Recruit in")(l)
case class AnointWarlordAction(self : Horde, c : Clearing) extends BaseAction("Anoint", Warlord.of(self), "in")(c)

case class SelectMoodAction(self : Horde, m : Mood) extends BaseAction("Choose new", "Mood".styled(self))((m.item != Torch).?(m.item.img), m.name.hl, m.desc(self))

case class DiscardLavishItemAction(self : Horde, item : Item, c : Option[Clearing], n : Int) extends BaseAction("Discard an item", implicit g => c./(r => "to place " ~ n.times(Image("lh-rat", styles.action)) ~ " in " ~ r.elem))("Discard", Image("item-" + item.name, styles.piece))

case class BattleLootAction(self : Faction, e : Faction, b : Battle, l : $[ItemRef]) extends BaseAction("Loot", e)(l./(_.img))
case class BattleNoLootAction(self : Faction, e : Faction, b : Battle) extends BaseAction(None)("Cancel")

case class BattleLootItemAction(self : Faction, e : Faction, b : Battle, i : ItemRef) extends BaseAction("Loot")(i, i.img)

case class BattleMobAction(self : Faction, c : Clearing, m : Clearing, then : ForcedAction) extends BaseAction("Add", Rat.of(self), Rat.img(self), "to", c, "instead of", Mob.of(self), Mob.img(self), "from")(m)
case class BattleNoMobAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Done")

case class HordeDoneAction(f : Horde, then : ForcedAction) extends ForcedAction

case class HordeMainAction(f : Horde) extends ForcedAction

case class HordeAdvanceAction(f : Horde, bonus : Boolean) extends ForcedAction


case class HordeAttackAction(self : Horde, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with LHDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class HordeMoveAction(self : Horde, l : $[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with LHDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class HordeBuildAction(self : Horde, l : $[Clearing]) extends OptionAction("Build", Stronghold.of(self), dt.AnyCard, dt.Arrow, Stronghold.imgd(self)) with LHDaylightQuestion with Soft with Only[BuildStrongholdAction] { def tag = implicitly }

case class BuildStrongholdClearingAction(self : Horde, c : Clearing) extends BaseAction("Build", Stronghold.of(self), "in")(c) with Soft
case class BuildStrongholdAction(self : Horde, c : Clearing) extends ForcedAction

case class HordeAdvanceMoveDoneAction(f : Horde, then : ForcedAction) extends ForcedAction
case class HordeAdvanceAttackDoneAction(f : Horde, then : ForcedAction) extends ForcedAction
case class HordeRelentlessAction(f : Horde, then : ForcedAction) extends ForcedAction

case class HordeAdvanceAttackAction(self : Horde, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with LHDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class HordeAdvanceMoveAction(self : Horde, l : $[Clearing]) extends OptionAction("Move".styled(self), dt.Move) with LHDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class HordeAdvanceSkipAction(self : Horde) extends OptionAction("Skip") with LHDaylightQuestion


case class InciteMobClearingAction(self : Horde, c : Clearing) extends BaseAction("Incite", Mob.of(self), Mob.imgd(self), "in")(c) with Soft with Only[PrepareDiscardCardsAction] { def tag = implicitly }
case class InciteMobAction(self : Horde, c : Clearing) extends ForcedAction
case class JubilantAction(self : Horde) extends ForcedAction
case class JubilantContinueAction(self : Horde) extends BaseAction(Jubilant.name.hl)("Spread", Image(self.style + "-mob-q", styles.dt.token))
case class JubilantDoneAction(self : Horde) extends ForcedAction


case class CraftRewardItemAction(self : Horde, d : CraftItemCard, then : ForcedAction) extends BaseAction("Contempt for Trade".styled(self))("Get", d.item, d.item.img)
case class CraftRewardVPAction(self : Horde, d : CraftItemCard, then : ForcedAction) extends BaseAction("Contempt for Trade".styled(self))("Score", d.vp.vp)

case class HoardCapacityAction(self : Horde, then : ForcedAction) extends ForcedAction
case class DiscardHoardItemAction(self : Horde, i : Item, then : ForcedAction) extends BaseAction("Discard an item")("Discard", i.img)

case class PeerPressureAction(c : Clearing, then : ForcedAction) extends ForcedAction


case class ToBuildStronghold(f : Faction, c : Clearing) extends Message {
    def elem(implicit game : Game) = " to build " ~ Stronghold.of(f) ~ " in " ~ c.elem
}

case class ToInciteMob(f : Faction, c : Clearing) extends Message {
    def elem(implicit game : Game) = " to incite " ~ Mob.of(f) ~ " in " ~ c.elem
}

object HordeExpansion extends FactionExpansion[Horde] {
    override def extraMoveFrom(f : Faction)(implicit game : Game) = f @@ {
        case f : Horde => f.region.$
        case _ => $()
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Horde) =>
            game.states += f -> new HordePlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Horde) if options.has(SetupTypeHomelands) =>
            val h = game.homelands
            val hh = h./~(game.connected)
            val hhh = hh./~(game.connected)

            val l = clearings.diff(h)
            val ll = l.diff(board.inner).some.|(l)
            val lll = ll.diff(hh).diff(hhh).some.||(ll.diff(hh).some).|(ll)

            Ask(f)(lll./(c => StartingClearingAction(f, c).as(c)(f, "starts in"))).needOk

        case FactionSetupAction(f : Horde) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : Horde, c) =>
            game.homelands :+= c

            f.reserve --> Stronghold --> c
            f.reserve --> Warlord --> c
            f.reserve --> 4.times(Rat) --> c

            f.log("placed", Stronghold.of(f) ~ ",", Rat.sof(f), "and", Warlord.of(f), "in", c)

            SetupFactionsAction

        // HELPER
        case MoveListAction(self, f, t, m, from, to, l, HordeDoneAction(ff, then)) =>
            HordeDoneAction(ff, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case BattleStartAction(self, f, a, m, c, o, i, HordeDoneAction(ff, then)) =>
            HordeDoneAction(ff, BattleStartAction(self, f, a, m, c, o, i, then))

        case LegalAAAConveneAction(f, h, c, e, HordeDoneAction(ff, then)) =>
            HordeDoneAction(ff, LegalAAAConveneAction(f, h, c, e, then))

        case MoveListAction(self, f, t, m, from, to, l, HordeAdvanceMoveDoneAction(ff, then)) =>
            HordeAdvanceMoveDoneAction(ff, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case BattleStartAction(self, f, a, m, c, o, i, HordeAdvanceAttackDoneAction(ff, then)) =>
            HordeAdvanceAttackDoneAction(ff, BattleStartAction(self, f, a, m, c, o, i, then))

        case LegalAAAConveneAction(f, h, c, e, HordeAdvanceAttackDoneAction(ff, then)) =>
            HordeAdvanceAttackDoneAction(ff, LegalAAAConveneAction(f, h, c, e, then))


        case BattleAttackerPreRollAction(b)
                if b.attacker.can(Bitter)
                && b.attacker.as[Horde]./~(_.region) == Some(b.clearing)
                && b.attacker.canPlace(b.clearing)
                && b.attacker.all(Mob).%(c => c == b.clearing || b.attacker.connectedFor(b.clearing).has(c)).any =>

            val f = b.attacker.asInstanceOf[Horde]
            Ask(f)(f.all(Mob).%(c => c == b.clearing || b.attacker.connectedFor(b.clearing).has(c))./(c => BattleMobAction(f, b.clearing, c, BattleAttackerPreRollAction(b))) :+ BattleNoMobAction(f, BattleAttackerPreRollAction(b)))

        case BattleDefenderPreRollAction(b)
                if b.defender.can(Bitter)
                && b.defender.as[Horde]./~(_.region) == Some(b.clearing)
                && b.defender.canPlace(b.clearing)
                && b.defender.all(Mob).%(c => c == b.clearing || b.defender.connectedFor(b.clearing).has(c)).any =>

            val f = b.defender.asInstanceOf[Horde]
            Ask(f)(f.all(Mob).%(c => c == b.clearing || b.defender.connectedFor(b.clearing).has(c))./(c => BattleMobAction(f, b.clearing, c, BattleDefenderPreRollAction(b))) :+ BattleNoMobAction(f, BattleDefenderPreRollAction(b)))

        case BattleMobAction(f, c, m, then) =>
            f.from(m) --> Mob --> f.reserve
            f.reserve --?> Rat --> c

            f.log("being", Bitter.name.hl, "added", Rat.of(f), "to", c, "from", Mob.of(f), "in", m)

            then

        case BattleNoMobAction(f, then) =>
            f.ignored :+= Bitter

            then

        case BattleStartedAction(b) if b.attacker.can(Looters) && b.defender.forTrade.any =>
            val f = b.attacker
            val e = b.defender
            Ask(f)((BattleLootAction(f, e, b, b.defender.forTrade) :: BattleNoLootAction(f, e, b)))

        case BattleLootAction(f, e, b, _) =>
            f.used :+= Looters

            f.log("wanted to loot")

            BattleStartedAction(b)

        case BattleResolvedAction(b) if b.attacker.used.has(Looters) =>
            b.attacker.used :-= Looters

            if (b.defender.forTrade.any && b.attacker.rules(b.clearing))
                Ask(b.attacker, b.defender.forTrade./(BattleLootItemAction(b.attacker, b.defender, b, _)))
            else {
                b.attacker.log("failed to loot", b.defender)

                BattleResolvedAction(b)
            }

        case BattleLootItemAction(f, e, b, i) =>
            e.forTrade :-= i

            f.forTrade :+= i

            f.log("looted", i, "from", e)

            AcquireItemsAction(f, BattleResolvedAction(b))

        case BattleNoLootAction(f, e, b) =>
            f.ignored :+= Looters

            BattleStartedAction(b)

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
            Ask(f)((CraftRewardItemAction(f, d, then) :: CraftRewardVPAction(f, d, CraftScoreAction(f, d, n, m, then))))

        case CraftRewardItemAction(f, d, then) =>
            f.log("crafted", d.item, d, "(" ~ d.cost.ss ~ ")", "for himself")
            AcquireItemsAction(f, then)

        case CraftRewardVPAction(f, d, then) =>
            f.forTrade :-= d.item.pristine
            f.wasted :+= d.item

            then

        case AcquireItemsAction(f : Horde, then) =>
            f.forTrade./(_.item).foreach {
                case i if i.in(f.items.command) => f.hoard.command :+= i
                case i if i.in(f.items.prowess) => f.hoard.prowess :+= i
            }

            f.forTrade = $

            HoardCapacityAction(f, then)

        case HoardCapacityAction(f, then) =>
            val d = (f.hoard.command.num > 4).??(f.hoard.command) ++ (f.hoard.prowess.num > 4).??(f.hoard.prowess)

            if (d.any)
                Ask(f)(d./(DiscardHoardItemAction(f, _, HoardCapacityAction(f, then))))
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
            val l = f.all(Mob).%(c => game.ruins.contains(c) || f.enemies.%(f.canRemove(c)).%(_.at(c).of[Scoring].any).any)

            if (l.any)
                Ask(f).each(l)(RazeClearingAction(f, _, l)).needOk.birdsong(f)
            else
                RazeSpreadAction(f, Next)

        case RazeClearingAction(f, c, l) =>
            NukeAction(f, f.enemies.%(f.canRemove(c)), $(c), NukeType.AntiMaterial, RazeRuinsAction(f, c, l))

        case RazeRuinsAction(f, c, l) =>
            val items = game.ruins.get(c)./~(_.items)

            if (items.any)
                Ask(f)
                  .add(items./(i => RazeTakeItemAction(f, i, c, l).!(f.fromRuins.has(i))))
                  .bailout(DoneAction(RazeContinueAction(f, l.but(c))))
                  .needOk
            else
                RazeContinueAction(f, l.but(c))

        case RazeTakeItemAction(f, i, c, l) =>
            val rest = game.ruins(c).items :- i

            if (rest.any)
                game.ruins += c -> Ruins(rest)
            else
                game.ruins -= c

            f.forTrade :+= i.pristine
            f.fromRuins :+= i

            f.log("found", i, "in", c, "ruins")

            RazeContinueAction(f, l.but(c))

        case RazeContinueAction(f, l) =>
            if (l.any)
                Ask(f).each(l)(RazeClearingAction(f, _, l)).needOk
            else
                AcquireItemsAction(f, RazeSpreadAction(f, Next))

        case RazeSpreadAction(f, then) =>
            if (f.pool(Mob))
                Roll[BaseSuit]($(BaseSuitDie), l => RazeSpreadRolledAction(f, l(0), then), f.elem ~ " rolls raze dice")
            else
                then

        case RazeSpreadRolledAction(f, s, then) =>
            f.logtemp("rolled", s)

            RazeSpreadSelectAction(f, s, then)

        case RazeSpreadSelectAction(f, s, then) =>
            val l = f.all(Mob)
            val ll = clearings.%(_.cost.matched(s)).diff(l).%(c => f.connectedFor(c).intersect(l).any).%(f.canPlace)

            if (ll.any)
                Ask(f).each(ll)(RazeSpreadClearingAction(f, s, _, then)).needOk
            else {
                f.log("rolled", s.elem ~ ", could not spread", Mob.of(f))
                then
            }

        case RazeSpreadClearingAction(f, s, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> Mob --> c

            f.log("rolled", s, "and spread", Mob.of(f), "to", c)

            PeerPressureAction(c, then)

        // RECRUIT
        case BirdsongNAction(40, f : Horde) =>
            f.region.%(f.canPlace).foreach { c =>
                val n = f.prowess
                val m = min(f.pooled(Rat), f.prowess)

                if (m > 0) {
                    f.reserve --> m.times(Rat) --> c
                    log(Warlord.of(f), "recruited", m.times(Rat)./(_.of(f)).comma ~ " in", c)
                }

                if (n > m && f.totalWar)
                    f.oscore(n - m)("recruiting")
            }

            val t = f.pooled(Rat)
            val r = f.all(Stronghold).%(f.canPlace)

            if (t >= r.num)
                Ask(f)(HordeRecruitAction(f, r, r))
            else
            if (t == 0)
                Next
            else
                Ask(f)
                  .each(r.combinations(t).$)(HordeRecruitAction(f, _, r))
                  .birdsong(f)

        case HordeRecruitAction(f, l, r) =>
            game.highlights :+= PlaceHighlight(l.distinct)

            l.distinct.foreach { c =>
                val n = l.count(c)
                f.reserve --> n.times(Rat) --> c
                log("Stronghold".s(n).styled(f), "recruited", n.times(Rat)./(_.of(f)).comma ~ " in", c)
            }

            if (r.num > l.num && f.totalWar)
                f.oscore(r.num - l.num)("recruiting")

            Next


        // ANOINT
        case BirdsongNAction(60, f : Horde) =>
            if (f.region.none) {
                val l = f.all(Rat).distinct.%(f.canPlace).some.|(clearings.%(f.canPlace))

                if (l.any)
                    Ask(f).each(l)(AnointWarlordAction(f, _)).birdsong(f)
                else
                    Next
            }
            else
                Next

        case AnointWarlordAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.from(c) --?> Rat --> game.recycle
            f.reserve --> Warlord --> c

            f.log("anointed", Warlord.of(f), "in", c)

            Next

        case BirdsongNAction(70, f : Horde) =>
            val available = Mood.all.%!(m => f.hoard.command.has(m.item) || f.hoard.prowess.has(m.item))

            Ask(f)
              .each(Mood.all)(m => SelectMoodAction(f, m).!(available.has(m).not).!(available.but(m).any && m == f.mood, "old mood"))
              .birdsong(f)

        case SelectMoodAction(f, m) =>
            f.mood = m

            log(Warlord.of(f), "mood became", m.name.hl)

            if (m == Grandiose) {
                f.advancing = true
                f.skipped = true
            }

            Next

        // LAVISH
        case BirdsongNAction(80, f : Horde) if f.can(Lavish) =>
            val d = f.hoard.command ++ f.hoard.prowess

            if (d.any && f.region.any)
                Ask(f)
                  .each(d)(DiscardLavishItemAction(f, _, f.region.%(f.canPlace).%(_ => f.pool(Rat)), min(2, f.pooled(Rat))))
                  .done(Next)
            else
                Next

        case DiscardLavishItemAction(f, i, _, _) =>
            f.wasted :+= i
            f.hoard.command :-= i
            f.hoard.prowess :-= i

            f.log("discarded", i)

            f.region.%(f.canPlace).foreach { c =>
                game.highlights :+= PlaceHighlight($(c))

                val n = min(f.pooled(Rat), 2)
                if (n > 0) {
                    f.reserve --> n.times(Rat) --> c
                    f.log("recruited", n.times(Rat)./(_.of(f)).comma ~ " in", c)
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
            HordeMainAction(f)

        case HordeMainAction(f) if f.acted >= f.command =>
            if (f.can(Grandiose))
                Ask(f)(Next.as("End Turn".hl)).daylight(f)
            else
                Next

        case HordeMainAction(f) =>
            var ask = Ask(f)

            val att = clearings.%(f.canAttackIn)
            ask += HordeAttackAction(f, att).!(att.none)

            val mvv = f.moveFrom.of[Clearing]
            ask += HordeMoveAction(f, mvv).!(mvv.none)

            val b0 = clearings
            val b1 = b0.%(f.rules)
            val b2 = b1.%(f.canBuild)
            val b3 = b2.%(f.canPlace)
            val b4 = b3.%(c => f.hand.%(_.matches(c.cost)).any)

            ask += HordeBuildAction(f, b4).!(f.pool(Stronghold).not, "max").!(b1.none, "no rule").!(b2.none, "no place").!(b3.none, "can't place").!(b4.none, "no matching cards")

            ask += EndTurnSoftAction(f, "Command".styled(f), ForfeitActions(f.command - f.acted))

            ask.daylight(f)

        case HordeAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), HordeDoneAction(f, Repeat))

        case HordeMoveAction(f, l) =>
            MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), HordeDoneAction(f, Repeat))

        case HordeBuildAction(f, l) =>
            Ask(f)(l./(c => BuildStrongholdClearingAction(f, c).!(f.hand.%(_.matches(c.cost)).none, "no matching card"))).cancel

        case BuildStrongholdClearingAction(f, c) =>
                OptionalDiscardCardAction(f, ToBuildStronghold(f, c), c.cost, BuildStrongholdAction(f, c))

        case BuildStrongholdAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.log("built", Stronghold.of(f), "in", c, "with", f.drawn.get)

            f.drawn --> discard.quiet

            f.reserve --> Stronghold --> c

            HordeDoneAction(f, Repeat)

        case HordeDoneAction(f, then) =>
            f.acted += 1

            then

        case DaylightNAction(n, f : Horde) if n == f.can(Grandiose).not.?(60).|(40) =>
            f.advancing = true
            f.skipped = true
            f.acted = 0

            Next

        case DaylightNAction(n, f : Horde) if n == f.can(Grandiose).not.?(70).|(50) =>
            val bonus = f.acted % 2 == 0 && f.skipped.not && f.can(Relentless)

            HordeAdvanceAction(f, bonus)

        // ADVANCE
        case HordeAdvanceAction(f, bonus) if f.region.none =>
            Ask(f)(Next.as("End Turn".hl)).daylight(f)

        case HordeAdvanceAction(f, bonus) if f.acted >= f.prowess * 2 && bonus.not =>
            if (f.can(Grandiose))
                Next
            else
                Ask(f)(Next.as("End Turn".hl)).daylight(f)

        case HordeAdvanceAction(f, bonus) =>
            val r = f.region.get

            var ask = Ask(f)

            ask += HordeAdvanceAttackAction(f, $(r)).!(f.canAttackIn(r).not)

            ask += HordeAdvanceMoveAction(f, $(r)).!(f.moveFrom.has(r).not).!(f.acted == f.prowess * 2 - 1 && bonus.not)

            if (bonus && f.prowess * 2 > f.acted)
                ask += HordeAdvanceSkipAction(f)

            ask += EndTurnSoftAction(f, "Advance".styled(f), ForfeitActions(f.prowess * 2 + bonus.??(1) - f.acted))

            if (f.acted % 2 == 0)
                ask.daylight(f)
            else
                ask

        case HordeAdvanceAttackAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), HordeAdvanceAttackDoneAction(f, Repeat))

        case HordeAdvanceMoveAction(f, l) =>
            MoveInitAction(f, f, game.transports./($) ** f.transports ** $($(WarlordMove)), NoMessage, l, l, $(CancelAction), HordeAdvanceMoveDoneAction(f, Repeat))

        case HordeAdvanceAttackDoneAction(f, then) =>
            if (f.acted % 2 == 0 && f.skipped.not && f.can(Relentless))
                HordeRelentlessAction(f, then)
            else {
                f.skipped = f.acted % 2 == 0

                if (f.skipped)
                    f.acted += 1

                f.acted += 1

                then
            }

        case HordeAdvanceMoveDoneAction(f, then) =>
            if (f.acted % 2 == 0 && f.skipped.not && f.can(Relentless))
                HordeRelentlessAction(f, then)
            else {
                f.skipped = f.acted % 2 == 1

                if (f.skipped)
                    f.acted += 1

                f.acted += 1

                then
            }

        case HordeRelentlessAction(f, then) =>
            f.skipped = true

            f.log("got an extra action being", Relentless.name.hl)

            then

        case HordeAdvanceSkipAction(f) =>
            f.skipped = true

            f.log("skipped an extra action")

            Repeat

        // INCITE
        case EveningNAction(40, f : Horde) =>
            if (f.pool(Mob)) {
                val b0 = clearings
                val b1 = b0.diff(f.all(Mob))
                val b2 = b1.%(c => f.at(c).of[Warrior].any)
                val b3 = b2.%(f.canPlace)
                val b4 = b3.%(c => f.hand.%(_.matches(c.cost)).any)

                if (b3.any)
                    Ask(f)
                      .each(b3)(c => InciteMobClearingAction(f, c).!(f.hand.%(_.matches(c.cost)).none, "no matching card"))
                      .done(Next)
                      .evening(f)
                else
                    Next
            }
            else
                Next

        case InciteMobClearingAction(f, c) =>
            OptionalDiscardCardAction(f, ToInciteMob(f, c), c.cost, InciteMobAction(f, c))

        case InciteMobAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.log("incited", Mob.of(f), "in", c, "with", f.drawn.get)

            f.drawn --> discard.quiet

            f.reserve --> Mob --> c

            if (f.can(Jubilant) && f.region.has(c)) {
                f.acted = 0
                f.log("can spread", Mob.of(f), "as", Jubilant.of(f))
                PeerPressureAction(c, JubilantAction(f))
            }
            else
                PeerPressureAction(c, Repeat)

        case JubilantAction(f) =>
            Ask(f)(JubilantContinueAction(f)).done(Repeat)

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
            val enemies = f.enemies
            val r = clearings.%(f.present).%(f.rules).%(c => enemies.forall(_.present(c).not))
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

            CleanUpAction(f)

        // PEER PRESSURE
        case PeerPressureAction(c, then) if options.has(PeerPressure) =>
            val hh = factions.but(game.current).appended(game.current).of[Horde]
            val mm = hh.%(_.at(c).has(Mob))

            if (mm.num > 1) {
                mm.foreach { f =>
                    f.from(c) --> Mob --> game.recycle
                }

                log(mm./(Mob.of).commaAnd, "dispersed")
            }
            else
                mm.foreach { f =>
                    val tt = hh.but(f).% { e =>
                        val l = e.at(c)
                        l.any && l.has(Stronghold).not && l.has(Warlord).not
                    }.sortBy(_.at(c).num)

                    tt.foreach { e =>
                        val n = min(e.at(c).count(e.warrior), f.pooled(f.warrior))
                        if (n > 0) {
                            e.from(c) --> n.times(e.warrior) --> game.recycle
                            f.reserve --> n.times(f.warrior) --> c
                            log(Mob.of(f), "replaced", n.times(e.warrior.of(e)).comma, "with", n.times(f.warrior.of(f)).comma)
                        }
                    }
                }

            then

        case PeerPressureAction(c, then) =>
            then

        case MoveCompleteAction(self, f, from, to : Clearing, l, e, then) if options.has(PeerPressure) && factions.but(f).diff(e).of[Horde].any =>
            PeerPressureAction(to, MoveCompleteAction(self, f, from, to : Clearing, l, factions.but(f).diff(e).of[Horde] ++ e, then))

        case _ => UnknownContinue
    }
}
