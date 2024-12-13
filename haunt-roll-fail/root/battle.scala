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


trait BattleEffect extends Effect

trait HitTarget

case class Battle(clearing : Clearing, attacker : Faction, ally : Option[Faction], defender : Faction, instigator : Option[Faction], arbiter : Option[Hero], codef : Option[WarriorFaction], balance : Int, ahits : Int, dhits : Int, then : ForcedAction) extends Record {
    def parties = $(attacker, defender) ++ ally ++ instigator ++ codef
}


case object Armorers extends CardEffect with BattleEffect {
    override val name = "Armorers"
}

case object Sappers extends CardEffect with BattleEffect {
    override val name = "Sappers"
}

case object BrutalTactics extends CardEffect with BattleEffect {
    override val name = "Brutal Tactics"
}

case object ScoutingParty extends CardEffect with BattleEffect {
    override val name = "Scouting Party"
}

case class Partisans(suit : BaseSuit) extends CardEffect with BattleEffect {
    override val name = suit.name + " Partisans"
}

case object Defenseless extends BattleEffect

case object Hatred extends BattleEffect with FactionEffect {
    val name = "Hatred"
}

case object IgnoreFirstHit extends BattleEffect


case class BattleInitAction(self : Faction, f : Faction, m : Message, l : $[Clearing], extra : $[UserAction], then : ForcedAction) extends ForcedAction with Soft
case class BattleImpossibleAction(self : Faction, f : Faction, l : $[Clearing], then : ForcedAction) extends BaseAction()(f, "can't battle in", l)
case class BattleFromAction(self : Faction, f : Faction, m : Message, c : Clearing, l : $[Faction], avoid : Boolean, then : ForcedAction) extends BaseAction(f, "battles", m, "in", implicit g => (m == NoMessage).?(self.as[Aviary]./(f => " [" ~ f.desc(Decree.Battle) ~ "]").??).??)(c) with Soft
case class BattleAllyAction(self : Faction, f : Faction, a : Faction, m : Message, c : Clearing, l : $[Faction], avoid : Boolean, then : ForcedAction) extends BaseAction(f, "battles", m, "in", c, "with ally")((self == a).?("Alone").|(a)) with Soft
case class BattleConfirmAction(self : Faction, f : Faction, a : Faction, m : Message, c : Clearing, o : Faction, i : Option[Hero], then : ForcedAction) extends BaseAction(f, (a != f).?("and"), (a != f).?(a), "battle" + (a == f).??("s"), m, "in", c)(o) with Soft
case class BattleStartAction(self : Faction, f : Faction, a : Faction, m : Message, c : Clearing, o : Faction, i : Option[Hero], then : ForcedAction) extends ForcedAction
case class BattleStartedAction(b : Battle) extends ForcedAction

case class BattleAskAmbushAction(b : Battle, then : ForcedAction) extends ForcedAction
case class BattleAskCounterAmbushAction(b : Battle, then : ForcedAction) extends ForcedAction

case class BattleAmbushAction(self : Faction, b : Battle, d : DeckCard, have : Boolean, available : Boolean, then : ForcedAction) extends BaseAction(self, have.?("can").|("can't"), "ambush", g => "(" ~ g.mapping(b.clearing)./(_.elem).spaceCommaOr ~ ")")(d.img ~ available.?(d.elem).|(have.?(" ".pre).|(Empty))) with ViewCard
case class BattleCounterAmbushAction(self : Faction, b : Battle, d : DeckCard, have : Boolean, available : Boolean, then : ForcedAction) extends BaseAction(self, have.?("can").|("can't"), "counter-ambush", g => "(" ~ g.mapping(b.clearing)./(_.elem).spaceCommaOr ~ ")")(d.img ~ available.?(d.elem).|(have.?(" ".pre).|(Empty))) with ViewCard
case class BattleAmbushHitsAction(b : Battle, n : Int, then : ForcedAction) extends ForcedAction

case class BattleBombAmbushAction(self : Mischief, b : Battle, then : ForcedAction) extends ForcedAction
case class BattleBombCounterAmbushAction(self : Mischief, b : Battle, then : ForcedAction) extends ForcedAction


case class BattleDefenderPreRollAction(b : Battle) extends ForcedAction
case class BattleEnlistArbiterAction(self : Faction, b : Battle, arbiter : Hero) extends BaseAction(self, "can enlist an", "Arbiter".hl)(arbiter)
case class BattleRewardArbiterAction(self : Faction, b : Battle, arbiter : Hero, then : ForcedAction) extends ForcedAction
case class BattleEnlistDefenderAction(self : Faction, b : Battle, defender : WarriorFaction) extends BaseAction(self, "can enlist a", "Defender".hl)(defender)
case class BattleEnlistScoutsAction(self : Faction, b : Battle) extends BaseAction(self, "can employ scouts")(RabbitScouts) with Soft
case class BattleEnlistScoutsCardAction(self : Faction, b : Battle, d : DeckCard, available : Boolean) extends BaseAction(self, "can employ", RabbitScouts)(d.img) with ViewCard

case class BattleAttackerPreRollAction(b : Battle) extends ForcedAction

case class BattleRollAction(b : Battle) extends ForcedAction
case class BattleRolledAction(b : Battle, fr : Int, or : Int) extends RolledAction[Int] { def rolled = $(fr, or) }
case class BattleAdjustRollAction(b : Battle, f : Faction, o : Faction, fs : Int, os : Int, fr : Int, or : Int) extends ForcedAction
case class BattleFinishRollAction(b : Battle, f : Faction, o : Faction, fs : Int, os : Int, fr : Int, or : Int, fh : Int, oh : Int) extends ForcedAction
case class BattleBonusAction(b : Battle, f : Faction, o : Faction, fs : Int, os : Int, fr : Int, or : Int, fh : Int, oh : Int, fe : Int, oe : Int) extends ForcedAction
case class BattlePostRollAction(b : Battle, fh : Int, oh : Int, fe : Int, oe : Int, ask : $[Faction]) extends ForcedAction

case class SappersAction(self : Faction, then : ForcedAction) extends BaseAction(self, "in battle")("Deal an extra", 1.hit, "and discard", Sappers)
case class PartisansAction(self : Faction, s : BaseSuit, l : $[DeckCard], then : ForcedAction) extends BaseAction(self, "in battle")("Deal an extra", 1.hit, "with", Partisans(s), "and discard", l)
case class SwiftStrikeAction(self : Hero, then : ForcedAction) extends BaseAction(self, "in battle")(SwiftStrike.of(self), implicit g => options.has(RoninSwiftTorch).?(Torch).|(Sword).img)
case class ArmorersAction(self : Faction, then : ForcedAction) extends BaseAction(self, "in battle")("Ignore all rolled", "Hits".styled(styles.hit), "and discard", Armorers)

case class GuardMainAction(self : Faction, s : BaseSuit, then : ForcedAction) extends BaseAction(self, "in battle")("Ignore one rolled", 1.hit, "and discard", Guard(s), "or a", s, "card") with Soft
case class GuardAction(self : Faction, s : BaseSuit, d : DeckCard, then : ForcedAction) extends BaseAction("Discard for", Guard(s))(d.img) with ViewCard
case class GuardSelfAction(self : Faction, s : BaseSuit, then : ForcedAction) extends BaseAction()("Discard", Guard(s), "itself")

case class HoardersMainAction(self : Faction, then : ForcedAction) extends BaseAction(self, "in battle")("Ignore one rolled", 1.hit, "and exhaust an item with", Hoarders) with Soft
case class HoardersAction(self : Faction, i : ItemRef, then : ForcedAction) extends ForcedAction

case class BrutalTacticsAction(self : Faction, o : Faction, then : ForcedAction) extends BaseAction(self, "in battle")("Deal an extra", 1.hit, BrutalTactics, "and give", o, 1.vp)
case class NightTerrorsAction(self : Faction, then : ForcedAction) extends BaseAction(self, "in battle")("Deal an extra", 1.hit, "and discard", NightTerrors)

case class BattleResolveHitsAction(b : Battle, fh : Int, oh : Int, fsh : Int) extends ForcedAction
case class BattleAssignHitsAction(f : Faction, b : Battle, n : Int, s : Int, then : ForcedAction) extends ForcedAction
case class BattleDealHitsAction(self : Faction, b : Battle, l : $[Figure], then : ForcedAction) extends ForcedAction
case class BattleDealHitAction(self : Faction, b : Battle, p : Figure, n : Int, then : ForcedAction) extends ForcedAction
case class BattlePostHitInAction(b : Battle, e : Faction, f : Faction, p : Piece, then : ForcedAction) extends ForcedAction
case class BattlePostHitOutAction(b : Battle, e : Faction, f : Faction, p : Piece, then : ForcedAction) extends ForcedAction
case class BattleCleanupAction(b : Battle) extends ForcedAction
case class BattleCleanupAllyAction(b : Battle, f : Faction) extends ForcedAction
case class BattleCleanupDefenderAction(b : Battle, f : Faction) extends ForcedAction
case class BattleCleanupAttackerAction(b : Battle, f : Faction) extends ForcedAction
case class BattleForcedRemoveAction(b : Battle) extends ForcedAction
case class BattleResolvedAction(b : Battle) extends ForcedAction
case class BattleFinishedAction(b : Battle) extends ForcedAction


case class WageWarAction(self : Faction, n : Int, total : Int, then : ForcedAction) extends ForcedAction with Soft


case class ToBattle(e : Faction) extends Message {
    def elem(implicit game : Game) = " to battle " ~ e.elem
}


object BattleExpansion extends MandatoryExpansion {
    def canAmbushWith(faction : Faction, clearing : Clearing, opponent : Faction)(d : DeckCard)(implicit game : Game) : Boolean = d @@ {
        case Ambush(Frog) if opponent.is[InvasiveCCC] => false
        case Ambush(Frog) if opponent.is[InvasiveBBB] => false
        case Ambush(Frog) if opponent.is[InvasiveAAA] => false
        case Ambush(s) => clearing.cost.matched(s)
        case CraftItemCard(_, _, _, Sword, _, _)  => faction.has(TrickyClaws) && clearing.cost.matched(Fox)
        case CraftItemCard(_, _, _, Coins, _, _)  => faction.has(TrickyEars)  && clearing.cost.matched(Rabbit)
        case CraftItemCard(_, _, _, Teapot, _, _) => faction.has(TrickyTails) && clearing.cost.matched(Mouse)
        case _ => false
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // BATTLE
        case BattleInitAction(self, f, m, from, extra, then) =>
            Ask(f)
                .each(from) { c =>
                    val l = f.canAttackList(c)
                    BattleFromAction(self, f, m, c, l, from.num > 1 || extra.any, then).!(l.none && f.enemies.%(_.present(c)).none, "no enemies").!(l.none, "no valid targets")
                 }
                .add(extra)
                .bailout(BattleImpossibleAction(self, f, from, then))

        case BattleImpossibleAction(self, f, from, then) =>
            f.log("could not battle", from.any.?("in"), from./(_.elem).commaOr)

            then

        case BattleFromAction(self, f, m, c, l, avoid, then) =>
            val pp = f.at(c)

            val ff = pp.of[Attacking].any.$(f) ++ f.is[TheExile.type].$(f)

            val aa = f.as[Hero].?(f => l.notOf[Hero].%(l.but(_).any).%(f.allied).%(_.at(c).of[Warrior].any))

            val mm = traders.%(s => f.has(Mercenaries(s)) && s.at(c).of[Warrior].any && l.but(s).any)

            val xx = (ff ++ aa ++ mm)./(a => BattleAllyAction(self, f, a, m, c, l.but(a), true, then))

            if (xx.num == 1)
                Ask(f)(xx.head.copy(avoid = avoid))
            else
                Ask(f)(xx).cancelIf(avoid)

        case BattleAllyAction(self, f, a, m, c, l, avoid, then) =>
            val ee = f.enemies.%(_.present(c)).diff(l)

            val ll = f.attacked.intersect(l) ++ l.diff(f.attacked)

            Ask(f)
                .each(ll)(BattleConfirmAction(self, f, a, m, c, _, None, then))
                .each(ee)(BattleConfirmAction(self, f, a, m, c, _, None, then).!(true, "can't attack"))
                .cancelIf(avoid)
                .needOk

        case BattleConfirmAction(self, f, a, m, c, o, i, then) if f.ignored.has(ConveneInstead).not && factions.of[LegalAAA].exists(_.at(c).has(AssemblyAAA)) && f.present(c) =>
            val ll = factions.of[LegalAAA].%(_.at(c).has(AssemblyAAA))

            Ask(self).group(f, "can", ll./(l => "debate".styled(l)).commaOr, o, "instead")
                .add(IgnoredEffectAction(f, ConveneInstead, ForceAction(BattleConfirmAction(self, f, a, m, c, o, i, then))).as("Battle"))
                .each(ll)(l => LegalAAAConveneAction(f, l, c, o, then).as("Debate".styled(l)))

        case BattleConfirmAction(self, f, a, m, c, o : SpringUprising.type, i, then) if f.used.has(SpringUprising).not =>
            OptionalDiscardCardAction(f, ToBattle(o), c.cost, DiscardDrawnAction(f, UsedEffectAction(f, SpringUprising, BattleStartAction(self, f, a, m, c, o, i, then))))

        case BattleConfirmAction(self, f, a, m, c, o, i, then) if o.can(SlyDog) =>
            NoAsk(f)(SlyDogMainAction(self, f, a, m, c, o, i, then))

        case SlyDogMainAction(self, f, a, m, c, o, i, then) =>
            if (f == a)
                f.logtemp("wanted to battle", o, "in", c, m)
            else
                f.logtemp("and", a, "wanted to battle", o, "in", c, m)

            Ask(o)
                .each(f.enemies.but(o).%(_.present(c)))(e => SlyDogAction(o, f, e, ForceAction(BattleConfirmAction(self, f, a, m, c, e, i, then))).as("Redirect", f, "attack to", e)(SlyDog))
                .add(IgnoredEffectAction(o, SlyDog, ForceAction(BattleConfirmAction(self, f, a, m, c, o, i, then))).as("Skip"))

        case SlyDogAction(o, f, e, then) =>
            o.log("redirected", f, "attack to", e, "with", SlyDog)

            o.removeStuckEffect(SlyDog)

            NoAsk(o)(then)

        case BattleConfirmAction(self, f, a, m, c, o, i, then) =>
            NoAsk(self)(BattleStartAction(self, f, a, m, c, o, i, then))

        // Embedded Agents with EmbeddedAgentsBeforeBattle - defender, pre-emptive hit
        // Loot - attacker, cancel own rolled hits

        // Enlist Arbiter - defender
        // Scouting Party - attacker, no Ambush
        // Bitter - attacker and defender, convert mobs to warriors

        // Devout Knight - attacker and defender, cancel first hit
        // Armored - attacker and defender, cancel first hit
        // Stubborn - attacker and defender, cancel first hit

        // Ambush - defender, 2 hits
        // Ambush - attacker, cancel Ambush

        // Wrathful - attacker, extra hit
        // Commander - attacker, extra hit
        // Defenseless - attacker, extra hit
        // Paw Crossbow - attacker, extra hit
        // Embedded Agents - defender, extra hit
        // Dazzle - attacker and defender, cancel first hit

        // Brutal Tactics - attacker, extra hit / enemy 1 vp
        // Armorers - attacker and defender, cancel rolled hits, discard
        // Sappers - defender, extra hit, discard
        // Partisans - attacker and defender, extra hit, discard hand cards
        // Swift Strike - attacker and defender, extra hit, exhaust sword

        // Despot - attacker and defender, 1 vp if buildings or tokens removed
        // Infamy - attacker, 1 vp for each piece
        // Loot - attacker, take item

        case BattleStartAction(self, f, a, m, c, o, i, then) =>
            game.highlights :+= BattleHighlight(c)

            f.attacked = (o :: f.attacked).distinct

            log(SingleLine)

            val b = Battle(c, f, (f != a).?(a), o, i, None, None, 0, 0, 0, then)

            if (f == a)
                f.log("battled", o, "in", c, m)
            else
                f.log("and", a, "battled", o, "in", c, m)

            val dl = o.as[Hero]./(_.swords).|(o.at(c).of[Warrior].num) == 0

            if (f == a) {
                log(f.as[TheExile.type]./(_.pawn) || f.as[Hero] | f.at(c)./(_.ofg(f)).comma, "versus", dl.??("defenseless"), o.as[Hero].|(o.at(c)./(_.ofg(o)).comma))
            }
            else {
                f.log("attacked with", f.at(c)./(_.of(f)).comma)
                a.log("helped with", a.at(c).of[Warrior]./(_.of(a)).comma)
                o.log(dl.?("was defenseless").|("defended"), (o.is[Hero].not).$("with", o.at(c)./(_.of(o)).comma))
            }

            var q : ForcedAction = BattleStartedAction(b)

            factions.but(f).of[Insurgent].%(_.at(c).has(Sympathy)).%(_.has(BattleOutrage)).foreach { outraged =>
                q = OutrageAction(outraged, f, c, q)
            }

            factions.but(f).of[LegalAAA].%(_.at(c).of[BatTokenAAA].any).foreach { outraged =>
                q = LegalAAAOutrageAction(outraged, f, c, q)
            }

            q

        case BattleStartedAction(b) if factions.of[Caster].%(_.mana > 0).%(_.used.has(BattleSpell).not).%(_.at(b.clearing).any).any =>
            val f =                    factions.of[Caster].%(_.mana > 0).%(_.used.has(BattleSpell).not).%(_.at(b.clearing).any).head

            val done = BattleSpellAction(f, BattleStartedAction(b))

            implicit val ask = builder

            CasterExpansion.spells(f, $(b.clearing), done)

            ask(f).skip(done)


        case BattleStartedAction(b) if options.has(EmbeddedAgentsBeforeBattle) && b.defender.as[Mischief].?(o => o.can(EmbeddedAgents) && o.hidden.has(b.clearing)) =>
            b.defender.used :+= EmbeddedAgents

            val n = 1 + (options.has(DoubleAgents) && b.attacker.is[Mischief]).??(1)

            b.defender.log("dealt a hindering", n.hit, "with", EmbeddedAgents.of(b.defender))

            BattleAmbushHitsAction(b, n, BattleStartedAction(b))

        case BattleStartedAction(b) =>
            if (b.attacker.at(b.clearing).of[Attacking].any || b.ally./~(_.at(b.clearing).of[Attacking]).any || b.attacker.as[TheExile.type].?(f => f.ready.any || f.exhausted.any)) {
                if (b.defender.present(b.clearing))
                    BattleAskAmbushAction(b, BattleDefenderPreRollAction(b))
                else
                    BattleCleanupAction(b)
            }
            else
                BattleCleanupAction(b)

        case BattleAskAmbushAction(b, then) =>
            def canAmbush(d : DeckCard) = canAmbushWith(b.defender, b.clearing, b.attacker)(d)
            def skip = Ask(b.defender)(then.as("No " ~ "Ambush".hl))

            val ask =
            if (b.attacker.has(ScoutingParty))
                skip
            else
            if (b.defender.hand.none)
                skip
            else
            if (b.defender.is[Trader] && b.defender.hand.%(canAmbush).none)
                skip
            else
            if (b.defender.canRemove(b.clearing)(b.attacker).not)
                skip
            else
            if (game.playingDeck.%(canAmbush)
                .diff(game.pile.$)
                .diff(factions.of[Aviary]./~(f => Decree.all./~(d => f.decree(d).$).but(LoyalVizier)))
                .diff(factions.of[Expedition]./~(f => Retinue.all./~(r => f.retinue(r).$).but(FaithfulRetainer)))
                .diff(factions.of[Fanatic]./~(_.lost.$))
                .diff(factions.of[Fanatic]./~(_.revealed.$))
                .diff(factions.of[Underground]./~(_.revealed.$))
                .diff(factions.but(b.defender).of[Trader]./~(_.hand.$))
                .none
            ) {
                // log("...deduced no ambush...")
                skip
            }
            else
            if (options.has(AutoAmbushSkippingMode) && b.defender.hand.%(canAmbush).none)
                skip
            else {
                val f = b.defender
                val q = f.hand.%(canAmbush)
                val h = f.hand./(d => BattleAmbushAction(f, b, d, q.any, q.contains(d), then).!(q.contains(d).not))

                if (q.any)
                    Ask(f)(h)(then.as("Skip " ~ "Ambush".hl))(NoHand)(HiddenAmbush)(HiddenClearing(b.clearing))
                else
                    Ask(f)(then.as("No " ~ "Ambush".hl))(h)(NoHand)(HiddenAmbush)(HiddenClearing(b.clearing)).needOk
            }

            if (options.has(BrutalHonesty) && b.defender.is[Mischief] && (b.defender.at(b.clearing).has(Bomb) || b.defender.at(b.clearing).has(HiddenPlot))) {
                val f = b.defender.as[Mischief].get
                ask.prepend(BattleBombAmbushAction(f, b, then).as("Ambush with", Bomb.of(f), Bomb.img(f))("Brutal Honesty".hl).!(f.at(b.clearing).has(Bomb).not && f.secret.get(b.clearing).has(Bomb).not)).needOk
            }
            else
                ask

        case BattleBombAmbushAction(f, b, then) =>
            f.from(b.clearing) --?> Bomb --> game.recycle
            f.from(b.clearing) --?> HiddenPlot --> game.recycle
            f.secret = f.secret - b.clearing

            f.log("exploded", Bomb.of(f), "as", "Ambush".hl)

            BattleAskCounterAmbushAction(b, then)

        case BattleAmbushAction(f, b, d, true, true, then) =>
            f.hand --> d --> discard.quiet

            f.log("played", d)

            BattleAskCounterAmbushAction(b, then)

        case BattleAskCounterAmbushAction(b, then) =>
            def canAmbush(d : DeckCard) = canAmbushWith(b.attacker, b.clearing, b.defender)(d)

            val pass = BattleAmbushHitsAction(b, 2, then)
            val skip = Ask(b.attacker)(pass.as("No " ~ "Ambush".hl))

            val ask =
            if (b.attacker.hand.none)
                skip
            else
            if (b.attacker.is[Trader] && b.attacker.hand.%(canAmbush).none)
                skip
            else
            if (game.playingDeck.%(canAmbush)
                .diff(game.pile.$)
                .diff(factions.of[Aviary]./~(f => Decree.all./~(d => f.decree(d).$).but(LoyalVizier)))
                .diff(factions.of[Expedition]./~(f => Retinue.all./~(r => f.retinue(r).$).but(FaithfulRetainer)))
                .diff(factions.of[Fanatic]./~(_.lost.$))
                .diff(factions.of[Fanatic]./~(_.revealed.$))
                .diff(factions.of[Underground]./~(_.revealed.$))
                .diff(factions.but(b.attacker).of[Trader]./~(_.hand.$))
                .none
            ) {
                // log("...deduced no ambush...")
                skip
            }
            else
            if (options.has(AutoAmbushSkippingMode) && b.attacker.hand.%(canAmbush).none)
                skip
            else {
                val f = b.attacker
                val z = f.hand.any
                val q = f.hand.%(d => d == Ambush(Bird) || b.clearing.suits.exists(s => d == Ambush(s)))
                val h = f.hand./(d => BattleCounterAmbushAction(f, b, d, q.any, q.contains(d), then).x(q.contains(d).not))

                if (q.any)
                    Ask(f)(h)(pass.as("Skip " ~ "Counter-Ambush".hl))(NoHand)(HiddenAmbush)(HiddenClearing(b.clearing))
                else
                    Ask(f)(pass.as("No " ~ "Counter-Ambush".hl))(h)(NoHand)(HiddenAmbush)(HiddenClearing(b.clearing)).needOk
            }

            if (options.has(BrutalHonesty) && b.attacker.is[Mischief] && (b.attacker.at(b.clearing).has(Bomb) || b.attacker.at(b.clearing).has(HiddenPlot))) {
                val f = b.attacker.as[Mischief].get
                ask.prepend(BattleBombCounterAmbushAction(f, b, then).as("Counter-Ambush with", Bomb.of(f), Bomb.img(f))("Brutal Honesty".hl).!(f.at(b.clearing).has(Bomb).not && f.secret.get(b.clearing).has(Bomb).not)).needOk
            }
            else
                ask

        case BattleCounterAmbushAction(f, c, d, true, true, then) =>
            f.hand --> d --> discard.quiet

            f.log("counter-played", d)

            then

        case BattleBombCounterAmbushAction(f, b, then) =>
            f.from(b.clearing) --?> Bomb --> game.recycle
            f.from(b.clearing) --?> HiddenPlot --> game.recycle
            f.secret = f.secret - b.clearing

            f.log("exploded", Bomb.of(f), "as", "Counter-Ambush".hl)

            then

        case BattleAmbushHitsAction(b, n, then) =>
            if (n > 0 && b.defender.canRemove(b.clearing)(b.attacker).not) {
                b.defender.log("could not remove pieces of", b.attacker)
                BattleAmbushHitsAction(b, 0, then)
            }
            else
            if (n > 0)
                BattleAssignHitsAction(b.attacker, b, n, 0, BattleAmbushHitsAction(b, 0, then))
            else
            if (b.attacker.at(b.clearing).of[Attacking].any || b.ally./~(_.at(b.clearing).of[Attacking]).any || b.attacker.as[TheExile.type].?(f => f.ready.any || f.exhausted.any))
                then
            else
                BattleCleanupAction(b)


        case BattleDefenderPreRollAction(b) =>
            val f = b.defender

            val arbiter = factions.but(b.attacker).but(b.defender).but(b.ally).but(b.arbiter).of[Hero].%(_.has(Protector)).%(_.at(b.clearing).any).%(_.swords > 0)

            val codefender = (b.codef.none && f.has(FundsDefense)).??(f @@ {
                case f : Trader => factions.but(b.attacker).but(b.defender).but(b.ally).of[WarriorFaction].%(d => f.funds.$.%(_.faction == d).any).%(_.at(b.clearing).of[Warrior].any)
                case _ => throw new Error("funds defense is for traders")
            })

            val scouts = (f.has(GuerillaTactics).not && f.can(RabbitScouts) && f.hand.%(_.matches(b.clearing.cost)).any).?(RabbitScouts)

            val e = arbiter./(BattleEnlistArbiterAction(f, b, _)) ++ codefender./(BattleEnlistDefenderAction(f, b, _)) ++ scouts./(_ => BattleEnlistScoutsAction(f, b))

            if (e.none)
                BattleAttackerPreRollAction(b)
            else
                Ask(f)(e).done(BattleAttackerPreRollAction(b))

        case BattleEnlistArbiterAction(f, b, a) =>
            b.defender.log("asked", a, "protection")

            BattleRewardArbiterAction(f, b, a, BattleDefenderPreRollAction(b.copy(arbiter = Some(a))))

        case BattleRewardArbiterAction(f, b, a, then) if options.has(ArbiterScoreCard) =>
            DrawCardsAction(a, 1, AsArbiter(a), AddCardsAction(a, then))

        case BattleRewardArbiterAction(f, b, a, then) =>
            a.oscore(1)("as", "Arbiter".styled(a))

            then

        case BattleEnlistDefenderAction(f : Trader, b, d) =>
            b.defender.log("asked", d, "protection")

            f.spend($(d))

            BattleDefenderPreRollAction(b.copy(codef = Some(d)))

        case BattleEnlistScoutsAction(f, b) =>
            val q = f.hand.%(d => d.matches(b.clearing.cost))
            val h = f.hand./(d => BattleEnlistScoutsCardAction(f, b, d, q.contains(d)).x(q.contains(d).not))

            Ask(f)(h).cancel

        case BattleEnlistScoutsCardAction(f, b, d, _) =>
            f.hand --> d --> discard.quiet

            b.defender.log("employed", RabbitScouts, "with", d)

            f.used :+= RabbitScouts

            BattleDefenderPreRollAction(b)

        case BattleAttackerPreRollAction(b) =>
            BattleRollAction(b)

        case BattleRollAction(b) =>
            Roll[Int](D4 :: D4, l => BattleRolledAction(b, l.max, l.min), b.attacker.elem ~ " in " ~ b.clearing.elem ~ " battles " ~ b.defender.elem)

        case BattleRolledAction(b, fr, or) =>
            val f = b.attacker
            val o = b.defender
            val c = b.clearing

            val as = b.ally @@ {
                case Some(a) => a.at(c).of[Warrior].num
                case _ => 0
            }

            val ds = b.arbiter @@ {
                case Some(a) => a.swords
                case _ => 0
            } + b.codef @@ {
                case Some(d) => d.at(c).of[Warrior].num
                case _ => 0
            }

            val fs = f.is[RiverfolkFlotilla.type].?(3) || f.as[TheExile.type]./(f => f.ready.num + f.exhausted.num) || f.as[Hero]./(_.swords) || f.used.has(Siege).?(f.at(c).count(Catapult)) | f.at(c).of[Warrior].num
            val os = o.is[RiverfolkFlotilla.type].?(3) || o.as[TheExile.type]./(o => o.ready.num + o.exhausted.num) || o.as[Hero]./(_.swords) || o.used.has(Siege).?(o.at(c).count(Catapult)) | o.at(c).of[Warrior].num

            BattleAdjustRollAction(b, b.attacker, b.defender, fs + as, os + ds, fr, or)

        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if fr > or && (
            b.defender.can(GuerillaTactics) ||
            b.defender.used.has(RabbitScouts) ||
            (b.attacker.has(HamsterTactics) && b.attacker.at(b.clearing).has(Sheriff).not)
        ) =>
            BattleAdjustRollAction(b, f, o, fs, os, or, fr)

        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if fs > 0 && b.attacker.used.has(Looters) =>
            BattleAdjustRollAction(b, f, o, 0, os, fr, or)

        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) =>
            var fh = min(fr, fs)
            var oh = min(or, os)

            BattleFinishRollAction(b, f, o, fs, os, fr, or, fh, oh)

        case BattleFinishRollAction(b, f, o, fs, os, fr, or, fh, oh) =>
            f.log("rolled", fr.roll, (fh > 0).?(("and can deal", fh.ehits(f.used.has(Siege).?(" siege ".hl).|(" ".txt)))))
            o.log("rolled", or.roll, (oh > 0).?(("and can deal", oh.hits)))

            Force(BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, 0, 0))

        case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) if os == 0 && o.used.has(Defenseless).not =>
            o.used :+= Defenseless

            f.log("dealt an extra", 1.hit, "to defenseless")

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)

        case BattleBonusAction(b, f : Aviary, o, fs, os, fr, or, fh, oh, fe, oe) if f.can(Commander) =>
            f.used :+= Commander

            f.log("dealt an extra", 1.hit, "as", Commander)

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)

        case BattleBonusAction(b, f : Fanatic, o : Fanatic, fs, os, fr, or, fh, oh, fe, oe) if options.has(DeathToTheInfidels) && f.used.has(Hatred).not =>
            f.used :+= Hatred

            f.log("dealt an extra", 1.hit, "with", Hatred.name.hl)

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)

        case BattleBonusAction(b, f : Feline, o, fs, os, fr, or, fh, oh, fe, oe) if f.can(BonusHit) =>
            f.used :+= BonusHit

            f.log("dealt an extra", 1.hit, "with", BonusHit.of(f))

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)

        case BattleBonusAction(b, f : Horde, o, fs, os, fr, or, fh, oh, fe, oe) if f.can(Wrathful) && f.region == Some(b.clearing) =>
            f.used :+= Wrathful

            f.log("dealt an extra", 1.hit, "as", Wrathful.of(f))

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)

        case BattleBonusAction(b, f, o : Mischief, fs, os, fr, or, fh, oh, fe, oe) if o.can(EmbeddedAgents) && o.hidden.has(b.clearing) =>
            o.used :+= EmbeddedAgents

            val n = 1 + (options.has(DoubleAgents) && f.is[Mischief]).??(1)

            o.log("dealt an extra", n.hit, "with", EmbeddedAgents.of(o))

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe + n)

        case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) if o.can(RavenSentries) && o.at(b.clearing).of[Scoring].any =>
            o.used :+= RavenSentries

            o.log("dealt an extra", 1.hit, "with", RavenSentries)

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe + 1)

        case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) if f.can(RavenSentries) && f.at(b.clearing).of[Scoring].any =>
            f.used :+= RavenSentries

            f.log("dealt an extra", 1.hit, "with", RavenSentries)

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)

        case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) =>
            BattlePostRollAction(b, fh, oh, fe, oe, $(f, o))

        case BattlePostRollAction(b, fh, oh, fe, oe, ask) =>
            if (ask.none)
                BattleResolveHitsAction(b.copy(ahits = fh + fe, dhits = oh + oe), b.attacker.used.has(Siege).not.??(fh) + fe, oh + oe, b.attacker.used.has(Siege).??(fh))
            else {
                val f = ask.head
                val o = $(b.attacker, b.defender).but(f).only

                lazy val ft = {
                    val all = b.attacker.at(b.clearing).but(Vagabond)
                    val items = b.attacker.as[Hero]./~(_.inv.notDamaged)
                    val merc = b.ally./~(_.from(b.clearing).$.%(_.piece.is[Warrior]))
                    all.num + items.num + merc.num
                }

                lazy val ot = {
                    val all = b.defender.at(b.clearing).but(Vagabond)
                    val items = b.defender.as[Hero]./~(_.inv.notDamaged)
                    val merc = b.codef./~(_.from(b.clearing).$.%(_.piece.is[Warrior]))
                    all.num + items.num + merc.num
                }

                val full = $(f) ++ $(b.attacker, b.defender).but(f)

                var actions : $[UserAction] = $

                if (f.has(Armorers)) {
                    if (f == b.attacker && oh > 0 && oe < ft)
                        actions :+= ArmorersAction(f, BattlePostRollAction(b, fh, 0, fe, oe, full))

                    if (f == b.defender && fh > 0 && fe < ot)
                        actions :+= ArmorersAction(f, BattlePostRollAction(b, 0, oh, fe, oe, full))
                }

                val guards = b.clearing.suits.%(s => f.can(Guard(s)))
                val hoarders = f.can(Hoarders) && f.forTrade.exists(_.exhausted.not)
                val maxNegate = guards.num + hoarders.??(1)

                guards.foreach { s =>
                    if (f == b.attacker && oh > 0 && oe + max(0, oh - maxNegate) < ft)
                        actions :+= GuardMainAction(f, s, BattlePostRollAction(b, fh, oh - 1, fe, oe, full))

                    if (f == b.defender && fh > 0 && fe + min(0, fh - maxNegate) < ot)
                        actions :+= GuardMainAction(f, s, BattlePostRollAction(b, fh - 1, oh, fe, oe, full))
                }

                if (hoarders) {
                    if (f == b.attacker && oh > 0 && oe + max(0, oh - maxNegate) < ft)
                        actions :+= HoardersMainAction(f, BattlePostRollAction(b, fh, oh - 1, fe, oe, full))

                    if (f == b.defender && fh > 0 && fe + min(0, fh - maxNegate) < ot)
                        actions :+= HoardersMainAction(f, BattlePostRollAction(b, fh - 1, oh, fe, oe, full))
                }

                if (f.canRemove(b.clearing)(o)) {
                    if (f.can(BrutalTactics))
                        if (f == b.attacker && fh + fe < ot)
                            actions :+= BrutalTacticsAction(f, b.defender, BattlePostRollAction(b, fh, oh, fe + 1, oe, full))

                    if (f.used.has(NightTerrors))
                        if (f == b.attacker && fh + fe < ot)
                            actions :+= NightTerrorsAction(f, BattlePostRollAction(b, fh, oh, fe + 1, oe, full))

                    if (f.has(Sappers))
                        if (f == b.defender && oh + oe < ft)
                            actions :+= SappersAction(f, BattlePostRollAction(b, fh, oh, fe, oe + 1, full))

                    b.clearing.suits.%(s => f.can(Partisans(s))).foreach { s =>
                        def x = f.hand.%!(d => d.matches(s) && (d.suit != Bird || options.has(NonBirdPartisans).not))

                        if (f == b.attacker && fh + fe < ot)
                            actions :+= PartisansAction(f, s, x, BattlePostRollAction(b, fh, oh, fe + 1, oe, full))

                        if (f == b.defender && oh + oe < ft)
                            actions :+= PartisansAction(f, s, x, BattlePostRollAction(b, fh, oh, fe, oe + 1, full))
                    }


                    f match {
                        case f : Hero if f.can(SwiftStrike) && f.ready(options.has(RoninSwiftTorch).?(Torch).|(Sword)) > 0 =>
                            if (f == b.attacker && fh + fe < ot)
                                actions :+= SwiftStrikeAction(f, BattlePostRollAction(b, fh, oh, fe + 1, oe, full))

                            if (f == b.defender && oh + oe < ft)
                                actions :+= SwiftStrikeAction(f, BattlePostRollAction(b, fh, oh, fe, oe + 1, full))
                        case _ =>
                    }
                }

                val auto = (game.current != f && options.has(ForcedAsyncMode)).??(actions.of[PartisansAction].%(_.l.none))

                if (auto.any)
                    Ask(f)(auto.head)
                else
                    Ask(f)(actions).done(BattlePostRollAction(b, fh, oh, fe, oe, ask.drop(1)))
            }

        case ArmorersAction(f, then) =>
            f.removeStuckEffect(Armorers)
            f.log("used", Armorers, "to cancel rolled", "Hits".styled(styles.hit))
            then

        case SappersAction(f, then) =>
            f.removeStuckEffect(Sappers)
            f.log("used", Sappers, "to deal an extra", 1.hit)
            then

        case PartisansAction(f, s, l, then) =>
            f.used :+= Partisans(s)

            f.hand --> l --> discard.quiet

            f.log("enlisted", Partisans(s), "for an extra", 1.hit, l.any.$("and discarded", l))

            then

        case SwiftStrikeAction(f, then) =>
            val item = options.has(RoninSwiftTorch).?(Torch).|(Sword)
            f.exhaust(item)
            f.log("made a", SwiftStrike.of(f), "dealing an extra", 1.hit, "for", item.exhaust)
            then

        case BrutalTacticsAction(f, o, then) =>
            f.used :+= BrutalTactics
            f.log("employed", BrutalTactics, "to deal an extra", 1.hit)
            o.oscore(1)("from", BrutalTactics)
            then

        case NightTerrorsAction(f, then) =>
            f.removeStuckEffect(NightTerrors)

            f.used :-= NightTerrors

            f.log("sacrificed", NightTerrors, "to deal an extra", 1.hit)

            then

        case GuardMainAction(f, s, then) =>
            Ask(f).each(f.hand)(d => GuardAction(f, s, d, then).!(d.matches(s).not))(GuardSelfAction(f, s, then)).cancel.needOk

        case GuardAction(f, s, d, then) =>
            f.hand --> d --> discard.quiet

            f.log("discarded", d, "to ignore one rolled", 1.hit)

            then

        case GuardSelfAction(f, s, then) =>
            f.removeStuckEffect(Guard(s))

            f.log("discarded", Guard(s), "to ignore one rolled", 1.hit)

            then

        case HoardersMainAction(f, then) =>
            YYSelectObjectsAction(f, f.forTrade./(ToExhaust))
                .withGroup(Merchants.elem)
                .withRule(_.ref.exhausted.not)
                .withThen(i => HoardersAction(f, i.ref, then))(i => "Ignore " ~ 1.hit ~ " with " ~ i.ref.elem)("Ignore " ~ 1.hit)
                .withExtras(CancelAction)

        case HoardersAction(f, i, then) =>
            f.used :+= Hoarders

            f.forTrade :-= i
            f.forTrade :+= i.exhaust

            f.log("exhausted", i.exhaust, "with", Hoarders, "to ignore one rolled", 1.hit)

            then

        case BattleResolveHitsAction(b, fh, oh, fsh) =>
            if (fh + fsh > 0 && b.attacker.canRemove(b.clearing)(b.defender).not) {
                b.attacker.log("could not remove pieces of", b.defender)
                BattleResolveHitsAction(b, 0, oh, 0)
            }
            else
            if (oh > 0 && b.defender.canRemove(b.clearing)(b.attacker).not) {
                b.defender.log("could not remove pieces of", b.attacker)
                BattleResolveHitsAction(b, fh, 0, fsh)
            }
            else
            if (fh + fsh > 0)
                BattleAssignHitsAction(b.defender, b, fh, fsh, BattleResolveHitsAction(b, 0, oh, 0))
            else
            if (oh > 0)
                BattleAssignHitsAction(b.attacker, b, oh, 0, BattleResolveHitsAction(b, fh, 0, fsh))
            else
                BattleCleanupAction(b)

        case BattleAssignHitsAction(_, _, 0, 0, then) =>
            then

        case BattleAssignHitsAction(f, b, n, s, then) if f.used.has(IgnoreFirstHit).not && f.can(Armored) && f.at(b.clearing).of[Warrior].any =>
            f.used :+= Armored
            f.used :+= IgnoreFirstHit

            f.log("ignored the first hit being", Armored.of(f))

            BattleAssignHitsAction(f, b, n - (n > 0).??(1), s - (n == 0).??(1), then)

        case BattleAssignHitsAction(f : Horde, b, n, s, then) if f.used.has(IgnoreFirstHit).not && f.can(Stubborn) && f.region == Some(b.clearing) =>
            f.used :+= Stubborn
            f.used :+= IgnoreFirstHit

            f.log("ignored the first hit being", Stubborn.name.hl)

            BattleAssignHitsAction(f, b, n - (n > 0).??(1), s - (n == 0).??(1), then)

        case BattleAssignHitsAction(f, b, n, s, then) if f.used.has(IgnoreFirstHit).not && f.can(DevoutKnights) && f.at(b.clearing).of[Warrior].any && f.at(b.clearing).of[Relic].any =>
            f.used :+= DevoutKnights
            f.used :+= IgnoreFirstHit

            f.log("ignored the first hit being", DevoutKnights.of(f))

            BattleAssignHitsAction(f, b, n - (n > 0).??(1), s - (n == 0).??(1), then)

        case BattleAssignHitsAction(f, b, n, s, then) if f.used.has(IgnoreFirstHit).not && f.can(BadgerBodyguards) =>
            f.used :+= BadgerBodyguards
            f.used :+= IgnoreFirstHit

            f.log("ignored the first hit with", BadgerBodyguards)

            BattleAssignHitsAction(f, b, n - (n > 0).??(1), s - (n == 0).??(1), then)

        case BattleAssignHitsAction(f, b, n, s, then) if f.used.has(IgnoreFirstHit).not && f.used.has(Dazzle) =>
            f.used :-= Dazzle
            f.used :+= IgnoreFirstHit

            f.log("ignored the first hit with", Dazzle.of(f))

            BattleAssignHitsAction(f, b, n - (n > 0).??(1), s - (n == 0).??(1), then)

        case BattleAssignHitsAction(f, b, n, s, then) if n > 0 && soft =>
            val all = f.from(b.clearing).sortBy(_.piece.priority)
            val own = all.%(_.piece.is[Warrior]).%(_.piece.is[Scoring].not)
            val merc = ((f == b.attacker).??(b.ally./~(_.from(b.clearing).$.%(_.piece.is[Warrior]))) ++ (f == b.defender).??(b.codef./~(_.from(b.clearing).$.%(_.piece.is[Warrior])))).sortBy(_.piece.priority)
            val rear = all.diff(own)

            val ww = own ++ merc
            val wo = rear.none || ww.num >= n
            val nn = min(n, ww.num + rear.num)

            val q =
                if (s > 0)
                    BattleAssignHitsAction(f, b, 0, s, then)
                else
                    then

            val ask = SelectFiguresAction(
                f, "Remove " ~ nn.ofb(wo.?("warrior").|("piece")), ww ++ rear, $(HiddenAssignHits, HiddenClearing(b.clearing))
            )(
                _.num(nn).all(l => l.intersect(rear).none || ww.diff(l).none).all(l => l.intersect(own).num >= l.intersect(merc).num || own.diff(l).none).all(l => l.intersect(merc).num + 1 >= l.intersect(own).num || merc.diff(l).none)
            )(
                l => BattleDealHitsAction(f, b, l, q)
            )

            if (options.has(ForcedAsyncMode)) {
                val explode = ask.perform(soft).actions.of[HalfExplode]
                val variants = explode./~(_.expand(None)).of[XXObjectsSelectedAction[Figure]]

                val priorities : $[Piece] = f.damagePriorities

                Ask(f)(variants.minBy(_.values.indexed./((f, n) => priorities.indexOf(f.piece) * 100 * 100 + pow((priorities.indexOf(f.piece) - n).abs, 2)).sum))(explode)
            }
            else
            if (options.has(AutoHitsAssignmentMode)) {
                val explode = ask.perform(soft).actions.of[HalfExplode]
                val variants = explode./~(_.expand(None)).of[XXObjectsSelectedAction[Figure]]

                val d = variants.distinctBy(_.values./(u => u.piece -> u.faction))

                if (d.num == 1)
                    Ask(f)(d)(explode)
                else
                    ask
            }
            else
                ask


        case BattleAssignHitsAction(f, b, 0, s, then) if soft =>
            val all = f.from(b.clearing).sortBy(_.piece.priority)
            val own = all.%(_.piece.is[Warrior])
            val merc = ((f == b.attacker).??(b.ally./~(_.from(b.clearing).$.%(_.piece.is[Warrior]))) ++ (f == b.defender).??(b.codef./~(_.from(b.clearing).$.%(_.piece.is[Warrior])))).sortBy(_.piece.priority)
            val rear = all.diff(own)

            val ww = own ++ merc
            val wo = rear.none || ww.num >= s
            val nn = min(s, rear.num)

            SelectFiguresAction(
                f, "Remove " ~ nn.ofb("building") ~ " or " ~ nn.ofb("tokens"), ww ++ rear, $(HiddenAssignHits, HiddenClearing(b.clearing))
            )(
                _.num(nn).all(l => l.diff(rear).none)
            )(
                l => BattleDealHitsAction(f, b, l, then)
            )

        case BattleDealHitsAction(f, b, Nil, then) =>
            then

        case BattleDealHitsAction(f, b, p :: l, then) =>
            Force(BattleDealHitAction(f, b, p, 0, BattleDealHitsAction(f, b, l, then)))

        case BattleDealHitAction(f, b, x : Figure, _, then) =>
            val a = x.faction
            val p = x.piece

            val o = (f == b.defender).?(b.attacker).|(b.defender)

            val s = b.instigator.|(o)

            BattlePostHitInAction(b, o, a, p, BattlePostHitOutAction(b, o, a, p, TryForcedRemoveAction(s, b.clearing, a, p, (p.is[Scoring] && s != a).??(1), Removing, then, then)))

        case BattlePostHitInAction(b, e, f, p, then) =>
            e.log("HIT IN", p.name.styled(f))

            then

        case BattlePostHitOutAction(b, e, f, p, then) =>
            then

        case BattleCleanupAction(b) =>
            if (b.ally.any)
                BattleCleanupAllyAction(b, b.ally.get)
            else
                BattleCleanupDefenderAction(b, b.defender)

        case BattleCleanupAllyAction(b, f) =>
            BattleCleanupDefenderAction(b, b.defender)

        case BattleCleanupDefenderAction(b, f) =>
            BattleCleanupAttackerAction(b, b.attacker)

        case BattleCleanupAttackerAction(b, f) =>
            BattleForcedRemoveAction(b)

        case BattleForcedRemoveAction(b) =>
            var q : ForcedAction = BattleResolvedAction(b)

            ForcedRemoveFinishedAction(b.instigator | b.attacker, q)

        case BattleResolvedAction(b) =>
            BattleFinishedAction(b)

        case BattleFinishedAction(b) =>
            factions.foreach { f =>
                f.used = f.used.notOf[BattleEffect]
                f.ignored = f.ignored.notOf[BattleEffect]
            }

            log("Battle ended in", b.clearing)
            log(SingleLine)

            b.then

        case WageWarAction(f, n, total, then) =>
            val cc = clearings.%(f.canAttackIn)

            if (n > total || cc.none)
                NoAsk(f)(then)
            else
                BattleInitAction(f, f, (total > 1).?(WageWar(n, total)).|(NoMessage), cc, $((n == 1).?(CancelAction).|(then.as("Forfeit " ~ (total - n + 1).of("battle")))), WageWarAction(f, n + 1, total, then))

        case _ => UnknownContinue
    }

}
