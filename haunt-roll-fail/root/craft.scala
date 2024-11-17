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


case class XCraftMainAction(f : Faction) extends ForcedAction with Soft
case class NiceCraftMainAction(self : Faction, l : $[SuitAsset], u : $[SuitAsset], title : Elem) extends BaseAction(title)("Craft".styled(self), l.diff(u)./(dt.CraftSuit).merge ~ u.diff(l).any.?(" " ~ "-".hl ~ " " ~ u.diff(l)./(dt.CraftSuit).merge)) with Soft with Only[CraftAssignAction] { def tag = implicitly }
case class CraftMenuAction(f : Faction) extends ForcedAction with Soft
case class CraftAction(f : Faction, d : DeckCard, m : Message, then : ForcedAction) extends ForcedAction with Soft
case class CraftAssignAction(self : Faction, d : DeckCard, all : $[SuitAsset], used : $[SuitAsset], m : Message, then : ForcedAction) extends ForcedAction
case class CraftPerformAction(f : Faction, d : DeckCard, m : Message) extends ForcedAction
case class CraftScoreAction(f : Faction, d : CraftItemCard, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class CraftEffectAction(f : Faction, d : CraftEffectCard, m : Message, then : ForcedAction) extends ForcedAction
case class CraftFavorAction(f : Faction, d : Favor, m : Message, then : ForcedAction) extends ForcedAction
case class AnotherPlayerCraftsItemAction(f : Faction, then : ForcedAction) extends ForcedAction
case class AcquireItemsAction(self : Faction, then : ForcedAction) extends ForcedAction


object CraftExpansion extends MandatoryExpansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // CRAFT
        case XCraftMainAction(f) =>
            val c = f.hand.%(f.craftable)
            val a = NiceCraftMainAction(f, f.craft ++ f.frogCraft ++ f.extraCraft, f.crafted, f.elem ~ " crafts")
                .!(f.hand.none)
                .!(c.none, "nothing craftable")

            val extra =
                if (f.has(MorningCraft) && (c.any || f.birdsongNewCard))
                    f.birdsong
                else
                if (f.has(DaylightCraft) && (c.any || f.daylightNewCard))
                    f.daylight
                else
                if (f.has(EveningCraft) && (c.any || f.eveningNewCard))
                    f.evening
                else
                    $()

            Ask(f)(a)(Next.as("End Craft"))(extra)

        case NiceCraftMainAction(f, _, _, _) =>
            Force(CraftMenuAction(f))

        case CraftMenuAction(f) =>
            val l = f.craft ++ f.frogCraft ++ f.extraCraft
            val u = f.crafted

            YYSelectObjectsAction(f, f.hand)
                .withGroup("Craft".styled(f) ~ " with " ~ l.diff(u)./(dt.CraftSuit).merge ~ u.diff(l).any.?(" " ~ "-".hl ~ " " ~ u.diff(l)./(dt.CraftSuit).merge))
                .withRule(f.craftable)
                .withThen(d => CraftAction(f, d, NoMessage, CraftPerformAction(f, d, NoMessage)))(d => game.desc("Craft", d, "with", d.as[CraftCard]./~(_.cost./(dt.CraftSuit)).merge))("Craft")
                .withExtra($(NoHand, CancelAction))

        case CraftAction(f, d : CraftCard, m, then) =>
            val assets : $[SuitAsset] = (f.craft ++ f.frogCraft ++ f.extraCraft).diff(f.crafted)
            val costs : $[SuitCost] = d.cost

            if (costs.diff(assets).none)
                Ask(f).done(CraftAssignAction(f, d, assets, assets.intersect(costs), m, then))
            else
            if (assets.distinct.num == 1)
                Ask(f).done(CraftAssignAction(f, d, assets, assets.take(costs.num), m, then))
            else
                XXSelectObjectsAction(f, assets./(ToCraft))
                    .withGroup("Craft " ~ d.elem ~ " using")
                    .withRule(_.num(costs.num).all(l => costs.permutations.exists(_.lazyZip(l).forall((c, a) => a.ref.matches(c)))))
                    .withThenElem(l => CraftAssignAction(f, d, assets, l./(_.ref), m, then))("Craft".hh)
                    .withExtra($(CancelAction))

        case CraftAssignAction(f, d, a, used, m, then) =>
            f.crafted ++= used

            then

        case CraftPerformAction(f, d @ CraftItemCard(_, _, _, item, vp, _), m) =>
            game.uncrafted :-= item
            f.forTrade +:= item.pristine

            val q = CraftScoreAction(f, d, d.vp, m, AnotherPlayerCraftsItemAction(f, Repeat))

            if (f.has(MoleArtisians)) {
                f.hand --> d --> MoleArtisians.display

                Ask(f)(MoleArtisiansRevealAction(f, d, q))(MoleArtisiansDiscardAction(f, d, q))
            }
            else {
                f.hand --> d --> discard.quiet

                q
            }

        case AnotherPlayerCraftsItemAction(f, then) =>
            var q : ForcedAction = then

            factions.but(f).%(_.has(Adventurers)).foreach { e =>
                q = AdventurersAction(e, q)
            }

            factions.but(f).%(_.has(MurineBroker)).foreach { e =>
                q = DrawCardsAction(e, 1, WithEffect(MurineBroker), AddCardsAction(e, q))
            }

            q

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, effect), m) =>
            f.hand --> d --> f.stuck
            f.effects :+= effect

            CraftEffectAction(f, d, m, Repeat)

        case CraftPerformAction(f, d @ Favor(suit, _, _), m) =>
            f.hand --> d --> discard.quiet

            CraftFavorAction(f, d, m, NukeAction(f, f.enemies, clearings.%(c => suit.matches(c.cost)), NukeType.ClearSector, Repeat))

        case CraftScoreAction(f, d, n, m, then) =>
            f.nscore(n)("crafting", d.item)(f, "crafted", d.item, d, "(" ~ d.cost.ss ~ ")", ForVP, m)

            AcquireItemsAction(f, then)

        case CraftEffectAction(f, d, m, then) =>
            f.log("prepared", d, "(" ~ d.cost.ss ~ ")", m)

            then

        case CraftFavorAction(f, d, m, then) =>
            f.log("was granted", d, "(" ~ d.cost.ss ~ ")", m)

            then

        case AcquireItemsAction(f, then) =>
            then

        case _ => UnknownContinue
    }

}
