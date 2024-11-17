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

case object Next extends ForcedAction
case object Repeat extends ForcedAction

trait TopLevelAction extends ForcedAction {
    def next : ForcedAction
}

case class StartPlayerTurnAction(f : Faction) extends ForcedAction

case class BirdsongStartAction(f : Faction) extends ForcedAction

object BirdsongNActionXXX {
    var done = false
}

case class BirdsongNAction(n : Int, f : Faction) extends TopLevelAction {
    if (n == 40 && f == CC) {
        if (BirdsongNActionXXX.done)
            throw new Error("BirdsongNActionXXX")
        else
            BirdsongNActionXXX.done = true
    }

    val next = (n >= 99).?(DaylightStartAction(f)).|(BirdsongNAction(n + 1, f))
}

case class DaylightStartAction(f : Faction) extends ForcedAction

case class DaylightNAction(n : Int, f : Faction) extends TopLevelAction {
    val next = (n >= 99).?(EveningStartAction(f)).|(DaylightNAction(n + 1, f))
}

case class EveningStartAction(f : Faction) extends ForcedAction

case class EveningNAction(n : Int, f : Faction) extends TopLevelAction {
    val next = (n >= 99).?(NightStartAction(f)).|(EveningNAction(n + 1, f))
}

case class NightStartAction(f : Faction) extends ForcedAction

case class FactionCleanUpAction(f : Faction) extends ForcedAction
case class CleanUpAction(f : Faction) extends ForcedAction

case class EveningDrawAction(f : Faction, n : Int) extends ForcedAction
case class EveningHandLimitAction(f : Faction) extends ForcedAction
case class EveningAfterHandLimitAction(f : Faction) extends ForcedAction with TopLevelAction {
    def next = this
}
case class EveningTowerScoreAction(f : Faction) extends ForcedAction
case class EveningEndAction(f : Faction) extends ForcedAction

case class EndTurnAction(self : Faction) extends BaseAction(None)("End Turn".hl) with WrappedAction {
    def then = Next
}

case class EndTurnSoftAction(self : Faction, e : Elem, m : ForfeitMessage) extends BaseAction(None)("End", e) with Soft

case class WarnAction(self : Faction, then : ForcedAction, q : Elem, proceed : Elem, cancel : Elem) extends ForcedAction with Soft

case class UsedEffectAction(f : Faction, e : Effect, then : ForcedAction) extends ForcedAction with ExpandThen
case class IgnoredEffectAction(f : Faction, e : Effect, then : ForcedAction) extends ForcedAction with ExpandThen

case class EndPlayerTurnAction(f : Faction) extends ForcedAction
case class AfterTurnAction(f : Faction) extends ForcedAction

case class AdjustDamagePrioritiesAction(self : Faction, l : $[Piece]) extends HiddenChoice with NoExplode
case class ExplodeDamagePriorityAction(self : Faction, l : $[Piece]) extends HiddenChoice /*with SelfExplode*/ with SelfValidate {
    def validate(target : Action) = target @@ {
        case SetDamagePriorityAction(f, m) => self == f && m.toSet.equals(l.toSet)
        case AdjustDamagePrioritiesAction(f, m) => self == f && m.toSet.equals(l.toSet)
        case _ => false
    }

    def explode(withSoft : Boolean) = {
        val c = l.permutations.$

        c./(SetDamagePriorityAction(self, _).as("Done")) ++ withSoft.??(c./(AdjustDamagePrioritiesAction(self, _)))
    }
}
case class SetDamagePriorityAction(f : Faction, l : $[Piece]) extends ForcedAction with SkipValidate


case object TransferTurnAction extends ForcedAction
case object BetweenTurnAction extends ForcedAction


object TurnHelperExpansion extends MandatoryExpansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        case a : TopLevelAction if game.repeat != Some(a) =>
            game.repeat = Some(a)

            TryAgain

        case Repeat =>
            game.repeat.get

        case Next =>
            game.repeat.get.next

        case _ =>
            UnknownContinue
    }
}

object TurnExpansion extends MandatoryExpansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // START
        case StartPlayerTurnAction(f) =>
            game.highlights :+= NothingHighlight

            log(DoubleLine)

            game.turn += 1
            game.current = f

            factions.foreach { e =>
                e.used = $
                e.ignored = $
            }

            log(Header(2, "Turn #" + game.turn + "-" + f.name, $(ExternalStyle("screen-reader"))) ~ "Turn", ("#" + game.turn).hl, "-", f)

            Milestone("start turn " + game.turn + " of " + f + ", " + f.vp + " vp, " + f.hand.num + " cards", CheckDominanceAction(f, BirdsongStartAction(f)))

        // BIRDSONG
        case BirdsongStartAction(f) =>
            game.phase = Birdsong

            log(Birdsong)

            f.birdsong0

        case BirdsongNAction(10, f) =>
            soft()

            val mandatory =
                FoxRabbitMouse.%(s => f.can(ServiceOfThe(s)))./(s => ServiceOfMainAction(f, s)) ++
                f.can(BetterBurrowBank).??(factions.but(f)./(BetterBurrowBankAction(f, _))) ++
                f.can(StoicProtector).?? {
                    val h = StoicProtector
                    val from = h.all(Deer)
                    val plans = h.movePlans(from.%(h.canMoveFrom), game.transports./($) ** h.transports)
                    from./(o => plans.get(o) @@ {
                        case Some((t, l)) => MoveFromAction(f, h, t, JustElem(StoicProtector), o, l, UsedEffectAction(f, StoicProtector, Repeat))
                        case None => MoveFromAction(f, h, $, JustElem(StoicProtector), o, $, Repeat).!(f.canMoveFrom(o).not, "snared").!(true)
                    })
                } ++
                f.can(BluebirdNobles).$(BluebirdNoblesAction(f, clearings.%(f.ruleSelf).num).!(clearings.%(f.ruleSelf).num < 3)) ++
                f.can(SpringUprising).?(SpringUprisingMainAction(f, SpringUprising)) ++
                f.can(RiverfolkFlotilla).$(RiverfolkFlotillaAction(f, RiverfolkFlotilla))

            val optional = $ ++
                f.can(LizardEnvoys).?(LizardEnvoysMainAction(f).!(pile.none)) ++
                f.can(BreakingDawn).?(BreakingDawnMainAction(f, f.as[WarriorFaction].?(f => f.pool(f.warrior)), game.dawn.num, Repeat).as(BreakingDawn)) ++
                f.can(MilitarySupplies).?(WageWarAction(f, 1, 2, UsedEffectAction(f, MilitarySupplies, Repeat)).as(MilitarySupplies)) ++
                f.can(Saboteurs).??(factions.but(f)./~(e => e.stuck.$.of[CraftEffectCard]./(d => SaboteurAction(f, e, d))))

            val ask = Ask(f)(RawElemGroup("Start of " ~ Birdsong.elem))(mandatory)

            if (ask.available)
                ask(optional).needOk
            else
                ask(optional)(Next.as("Skip"))

        case BirdsongNAction(98, f) =>
            soft()

            if (f.birdsongOnly)
                Ask(f)(f.birdsong)(Next.as("End " ~ Birdsong.elem))
            else
                NoAsk(f)(Next)

        // TURN - DAYLIGHT
        case DaylightStartAction(f) =>
            game.phase = Daylight

            log(Daylight)

            DaylightNAction(0, f)

        case DaylightNAction(0, f) =>
            soft()

            val cw = f.has(CommandWarren).??(clearings.%(f.canAttackIn))

            if (cw.any)
                BattleInitAction(f, f, WithEffect(CommandWarren), cw, $(Next.as("Skip " ~ CommandWarren.elem)), Next)
            else
                NoAsk(f)(Next)

        // TURN - EVENING
        case EveningStartAction(f) =>
            game.phase = Evening

            log(Evening)

            EveningNAction(0, f)

        case EveningNAction(0, f) =>
            soft()

            if (f.can(Cobbler) || f.can(CharmOffensive)) {
                Ask(f)
                    .add(f.can(Cobbler).?(MoveInitAction(f, f, $, WithEffect(Cobbler), f.moveFrom, f.movable, $(CancelAction), UsedEffectAction(f, Cobbler, Repeat)).as(Cobbler)(Evening)))
                    .add(f.can(CharmOffensive).?(CharmOffensiveMainAction(f, factions.but(f), Repeat).as(CharmOffensive)(Evening)))
                    .add(Next.as("Skip"))
                    .evening(f)
            }
            else
                NoAsk(f)(Next)

        case UsedEffectAction(f, e, then) =>
            f.used :+= e

            then

        case IgnoredEffectAction(f, e, then) =>
            f.ignored :+= e

            then

        // TURN - EVENING END
        case EveningDrawAction(f, n) =>
            DrawCardsAction(f, n, NotInLog(OnTurn(game.turn)), AddCardsAction(f, EveningHandLimitAction(f)))

        case EveningHandLimitAction(f) =>
            HandLimitAction(f, 5, EveningAfterHandLimitAction(f))

        case EveningAfterHandLimitAction(f) =>
            implicit val ask = builder

            val group = "End of " ~ Evening.elem
            var skip = true

            if (f.can(NightTerrors)) {
                val l = clearings.%(f.canAttackIn)

                if (l.any) {
                    skip = false

                    + BattleInitAction(f, f, WithEffect(NightTerrors), l, $, UsedEffectAction(f, NightTerrors, Repeat)).as("Battle with", NightTerrors)(group)
                }
            }

            if (f.can(DuskAwakening)) {
                val feed = f.as[WarriorFaction].?(f => f.pool(f.warrior))
                val milk = game.dusk.num

                + DuskAwakeningMainAction(f, feed, milk, UsedEffectAction(f, DuskAwakening, Repeat)).as(DuskAwakening)(group)
            }

            if (f.has(MilitarySupplies) && f.ignored.has(MilitarySupplies).not) {
                skip = false

                + MilitarySuppliesPayAction(f, IgnoredEffectAction(f, MilitarySupplies, Repeat)).as(MilitarySupplies)(group)
            }

            ask(f).done(skip.?(EveningTowerScoreAction(f))).needOkIf(skip.not)

        case EveningAfterHandLimitAction(f) =>
            EveningTowerScoreAction(f)

        case EveningTowerScoreAction(f) =>
            game.tower.foreach { c =>
                if (f.rules(c))
                    f.oscore(1)("controlling", "Tower".hl)
            }

            EveningEndAction(f)

        case EveningEndAction(f) =>
            FactionCleanUpAction(f)

        case FactionCleanUpAction(f) =>
            CleanUpAction(f)

        case CleanUpAction(f) if hirelings.has(StreetBand) && StreetBand.enchanted.any =>
            StreetBand.enchanted = $

            CleanUpAction(f)

        case CleanUpAction(f) if f.has(MoleArtisians) && MoleArtisians.revealed.any =>
            f.log("got back", MoleArtisians.revealed.get, "from", MoleArtisians)

            MoleArtisians.revealed --> f.hand

            CleanUpAction(f)

        case CleanUpAction(f) =>
            f.crafted = $
            f.extraCraft = $
            f.services = $

            f.effects.of[TillEndOfTurn].foreach { e =>
                f.removeStuckEffect(e)

                f.log("discarded", e)
            }

            EndPlayerTurnAction(f)

        case EndPlayerTurnAction(f) =>
            game.phase = Night

            f.contracts = f.contracts.diff(f.contracts.distinct)

            HirelingTransfersAction(f)

        case HirelingTransfersAction(f) =>
            val going = f.effects.of[Hireling].%!(f.contracts.has)

            if (going.any)
                Ask(f).each(going)(GiveAwayHirelingAction(f, going, _)).needOk
            else
                HirelingPurchasesAction(f)

        case GiveAwayHirelingAction(f, going, h) =>
            Ask(f)(factions.but(f)./(GiveAwayHirelingToAction(f, h, _))).cancelIf(going.num > 1 && factions.but(f).num > 1)

        case GiveAwayHirelingToAction(f, h, e) =>
            f.log("gave", h, "to", e)

            f.contracts :-= h
            f.effects :-= h

            TakeHirelingAction(e, h, HirelingTransfersAction(f))

        case TakeHirelingAction(f, h, then) =>
            f.effects :+= h

            val die = (f.vp >= factions./(_.vp).max).?(GoldenDie).|(SilverDie)

            if (die == GoldenDie)
                f.log("rolled the", "golden die".styled(styles.gold))
            else
                f.log("rolled the", "silver die".hl)

            Roll[Int]($(die), r => ControlRolledAction(f, h, r(0), then))

        case ControlRolledAction(f, h, n, then) =>
            f.contracts ++= n.times(h)

            f.log("took", h, "for", n.of("turn"))

            OnTakeHirelingAction(f, h, then)

        case HirelingPurchasesAction(f) =>
            if (f.effects.has(NewHireling))
                Ask(f).each(game.unhired)(HireHirelingAction(f, _)).needOk
            else
                AfterTurnAction(f)

        case AfterTurnAction(f) if options.has(ForcedAsyncMode) =>
            if (clearings.exists { c =>
                val l = f.at(c)
                l.of[Scoring].distinct.num > 1 || l.notOf[Scoring].distinct.num > 1
            })
                Force(AdjustDamagePrioritiesAction(f, f.damagePriorities))
            else
                BetweenTurnAction

        case AfterTurnAction(f) =>
            BetweenTurnAction

        case AdjustDamagePrioritiesAction(f, p) =>
            implicit val convF = (x : Figure, s : Boolean) => FigureSelect(x).elem(s)(styles.iii)

            XXSelectObjectsAction(f, p./(Figure(f, _, 0)))
                .withGroup("Adjust priorities")
                .withSplit($(p.num))
                .withBreak({
                    case 0 => Empty
                    case _ => HorizontalBreak
                })
                .withRule(rule => rule.num(2).all(l => l.num < 2 || (l(0).piece.is[Scoring] == l(1).piece.is[Scoring] && l(0).piece.is[Tenacious] == l(1).piece.is[Tenacious])))
                .withAuto(l => AdjustDamagePrioritiesAction(f, p./{
                    case x if x == l(0).piece => l(1).piece
                    case x if x == l(1).piece => l(0).piece
                    case x => x
                }))
                .withExtra($(SetDamagePriorityAction(f, p).as("Done"), ExplodeDamagePriorityAction(f, p)))
                .ask

        case SetDamagePriorityAction(f, p) =>
            f.damagePriorities = p

            BetweenTurnAction

        case BetweenTurnAction if board.blizzard.any && game.turn % factions.num == 0 && game.used.has(Blizzard).not =>
            game.used :+= Blizzard

            Random[(Clearing, Clearing)](board.blizzard, ab => BlizzardAction(ab._1, ab._2, BetweenTurnAction))

        case BetweenTurnAction =>
            game.used = $

            TransferTurnAction

        case TransferTurnAction =>
            game.factions = factions.drop(1) ++ factions.take(1)

            Milestone(StartPlayerTurnAction(factions.first.get))

        // END
        case WarnAction(f, then, q, proceed, cancel) =>
            if (hrf.HRF.flag("fastsetup"))
                Ask(f)(then.as(proceed)(q))
            else
                Ask(f)(Cancel.as(cancel)(q))(then.as(proceed)(q))

        case EndTurnAction(f) =>
            Next

        case EndTurnSoftAction(f, e, m) =>
            if (m.real.not)
                Ask(f)(Next.as(e))
            else
                WarnAction(f, Next, m.elem, ("End Anyway").styled(styles.hit), "Continue" ~ " " ~ e)

        case a : TopLevelAction =>
            a.next

        case _ => UnknownContinue
    }

}
