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

trait Hero extends Faction {
    val clashKey = VB

    def abilities(options : $[Meta.O]) = $(Nimble)

    override val transports : $[$[Transport]] = $($(FreeMove))

    def pieces(options : $[Meta.O]) = Vagabond *** 1

    def advertising = $(Teapot, Boots, Torch, Sword, Coins)./(_.img).merge
    def motto = "Quest".styled(this)
}

case object VB extends Hero {
    val name = "Vagabond"
    val short = "VB"
    val style = "vb"
    val priority = "E"
}

case object NB extends Hero {
    val name = "Negabond"
    val short = "NB"
    val style = "nb"
    val priority = "E'"
}

case object MB extends Hero {
    val name = "Meganagibond"
    val short = "MB"
    val style = "mb"
    val priority = "E''"

    override def note : Elem = SpacedDash ~ "the dreaded third " ~ "Vagabond".styled(VB)
}

case object Vagabond extends Pawn with Movable with Attacking with Tenacious

sealed trait Attitude extends Named with Styling {
    def name = toString
}
case object Hostile extends Attitude
case object Indifferent extends Attitude
case object Amiable extends Attitude
case object Friendly extends Attitude
case object Allied extends Attitude



case object Nimble extends Effect

case class Infamy(f : WarriorFaction) extends BattleEffect

trait Character extends Record {
    val name : String = toString
    def id = name.replace(" ", "-")
    def starting(options : $[Meta.O]) : $[Item]
    val special : $[FactionEffect]
    def img = "vb-char-" + id
    def title = "vb-title-" + id
}


case object Testabond extends Character {
    def starting(options : $[Meta.O]) = $(Boots, Boots, Boots, Boots, Torch, Torch, Teapot, Teapot, Teapot, Sword, Sword, Sword, Crossbow, Hammer, Hammer, Hammer, Hammer, Bag, Bag, Bag, Coins)
    val special = $(Steal, DayLabor, Hideout, Protector, ScorchedEarth, Instigate, Glide, SwiftStrike, Improvise)
}


case object Steal extends FactionEffect {
    val name = "Steal"
}

case object Thief extends Character {
    def starting(options : $[Meta.O]) = $(Boots, Torch, Teapot, Sword)
    val special = $(Steal)
}


case object DayLabor extends FactionEffect {
    val name = "Day Labor"
}

case object Tinker extends Character {
    def starting(options : $[Meta.O]) =
        if (options.has(TinkerNoBag))
            $(Boots, Torch, Hammer)
        else
            $(Boots, Torch, Bag, Hammer)
    val special = $(DayLabor)
}


case object Hideout extends FactionEffect {
    val name = "Hideout"
}

case object Ranger extends Character {
    def starting(options : $[Meta.O]) = $(Boots, Torch, Crossbow, Sword)
    val special = $(Hideout)
}


case object Protector extends FactionEffect with BattleEffect {
    val name = "Protector"
}

case object Arbiter extends Character {
    def starting(options : $[Meta.O]) = $(Boots, Torch, Sword, Sword)
    val special = $(Protector)
}


case object ScorchedEarth extends FactionEffect {
    val name = "Scorched Earth"
}

case class ScorchedEarthMarker(n : Int) extends Token

case object Scoundrel extends Character {
    def starting(options : $[Meta.O]) =
        if (options.has(ScoundrelTeapotNoBoot))
            $(Teapot, Boots, Torch, Crossbow)
        else
            $(Boots, Boots, Torch, Crossbow)
    val special = $(ScorchedEarth)
}


case object Instigate extends FactionEffect {
    val name = "Instigate"
}

case object Vagrant extends Character {
    def starting(options : $[Meta.O]) = $(Boots, Torch, Coins)
    val special = $(Instigate)
}


case object Glide extends FactionEffect {
    val name = "Glide"
}

case object Harrier extends Character {
    def starting(options : $[Meta.O]) =
        if (options.has(HarrierNoCoins).not)
            $(Coins, Torch, Sword, Crossbow)
        else
            $(Torch, Sword, Crossbow)
    val special = $(Glide)
}


case object SwiftStrike extends FactionEffect with BattleEffect {
    override val name = "Swift Strike"
}

case object Ronin extends Character {
    def starting(options : $[Meta.O]) = $(Boots, Boots, Torch, Sword)
    val special = $(SwiftStrike)
}


case object Improvise extends FactionEffect {
    val name = "Improvise"
}

case object Adventurer extends Character {
    def starting(options : $[Meta.O]) = $(Boots, Torch, Hammer)
    val special = $(Improvise)
}


case object FriendOfTheForest extends FactionEffect {
    val name = "Friend of the Forest"
}

case object FolkHero extends Character {
    override val name = "Folk Hero"
    def starting(options : $[Meta.O]) = $(Boots, Torch, Sword, Coins)
    val special = $(FriendOfTheForest)
}


case class Quest(suit : Suit, name : String, id : String, a : Item, b : Item) extends Elementary {
    val ab = $(a, b)
    def other(x : Item) = (x == a).?(b).|((x == b).?(a).|!("unknown other item for quest"))

    def img = Image("quest:" + id, styles.quest)
    def elem = name.styled(suit)
}

object Quest {
    val default = $(
        Quest(Fox, "Fundraising", "fundraising-fox", Teapot, Coins),
        Quest(Fox, "Errand", "errand-fox", Teapot, Boots),
        Quest(Fox, "Logistic Help", "logistic-help-fox", Boots, Bag),
        Quest(Fox, "Repair a Shed", "repair-a-shed-fox", Torch, Hammer),
        Quest(Fox, "Give a Speech", "give-a-speech-fox", Torch, Teapot),

        Quest(Rabbit, "Guard Duty", "guard-duty-rabbit", Torch, Sword),
        Quest(Rabbit, "Errand", "errand-rabbit", Teapot, Boots),
        Quest(Rabbit, "Give a Speech", "give-a-speech-rabbit", Torch, Teapot),
        Quest(Rabbit, "Fend off a Bear", "fend-off-a-bear-rabbit", Torch, Crossbow),
        Quest(Rabbit, "Expel Bandits", "expel-bandits-rabbit", Sword, Sword),

        Quest(Mouse, "Expel Bandits", "expel-bandits-mouse", Sword, Sword),
        Quest(Mouse, "Guard Duty", "guard-duty-mouse", Torch, Sword),
        Quest(Mouse, "Fend off a Bear", "fend-off-a-bear-mouse", Torch, Crossbow),
        Quest(Mouse, "Escort", "escort-mouse", Boots, Boots),
        Quest(Mouse, "Logistic Help", "logistic-help-mouse", Boots, Bag)
    )

    val revised = $(
        Quest(Fox, "Fundraising", "fundraising-fox", Hammer, Coins),
        Quest(Fox, "Errand", "errand-fox", Boots, Boots),
        Quest(Fox, "Logistic Help", "logistic-help-fox", Boots, Bag),
        Quest(Fox, "Escort", "escort-fox", Boots, Sword),
        Quest(Fox, "Give a Speech", "give-a-speech-fox", Torch, Teapot),

        Quest(Rabbit, "Guard Duty", "guard-duty-rabbit", Torch, Sword),
        Quest(Rabbit, "Errand", "errand-rabbit", Boots, Boots),
        Quest(Rabbit, "Give a Speech", "give-a-speech-rabbit", Torch, Teapot),
        Quest(Rabbit, "Repair a Shed", "repair-a-shed-rabbit", Bag, Hammer),
        Quest(Rabbit, "Expel Bandits", "expel-bandits-rabbit", Sword, Sword),

        Quest(Mouse, "Expel Bandits", "expel-bandits-mouse", Sword, Sword),
        Quest(Mouse, "Guard Duty", "guard-duty-mouse", Torch, Sword),
        Quest(Mouse, "Fend off a Bear", "fend-off-a-bear-mouse", Torch, Crossbow),
        Quest(Mouse, "Escort", "escort-mouse", Boots, Sword),
        Quest(Mouse, "Logistic Help", "logistic-help-mouse", Boots, Bag)
    )

    val all = default ++ revised
}

case object Effort extends SpecialRegion
case object Heart extends Warrior {
    override def imgid(f : Faction) = "attitude-heart-full"
}

class HeroPlayer(val faction : Hero)(implicit val game : Game) extends FactionState {
    board.forests.foreach(c => location(c))

    var character : Option[Character] = None

    override def transient = character./~(_.special)

    val pawn = reserve.$.only

    var inv : $[ItemRef] = $

    var attitude = Map[Faction, Attitude]()
    def allied(f : Faction) = attitude.get(f).has(Allied)
    def hostile(f : Faction) = attitude.get(f).has(Hostile)

    var aid = Map[Faction, Int]()

    var quests = $[Suit]()

    def region : Region = game.pieces.find(pawn) match {
        case Some((f, r)) => r
        case q => println(q); throw new Error("vagabond nowhere to be found")
    }

    var fromRuins : $[Item] = $
    var wasted : $[Item] = $

    def ready(i : Item) = inv.ready.count(i)
    def lacks(i : Item) = inv.ready.count(i) == 0

    def available = inv.sort.ready./(_.item)

    def use(i : Item, m : String = "exhausted") {
        use($(i), m)
    }

    def use(ii : $[Item], m : String) {
        ii.foreach(exhaust)

        game.log(faction, m, ii./(_.exhaust))
    }

    def exhaust(i : Item) {
        if (inv.ready.count(i) == 0)
            throw new RuntimeException("Item not found " + i.name.styled(styles.hit) + "!!!")
        else {
            inv :-= i.ref(false, false)
            inv :+= i.exhaust
        }
    }

    val dontCount = $(Teapot, Teapot, Teapot, Coins, Coins, Coins, Bag, Bag, Bag)./(_.ref(false, false))

    def swords = inv.notDamaged.count(Sword)

    def craft = region.as[Clearing]./(c => ready(Hammer).times(c.asset)).|($)

    var itemDamagePriorities : $[Item] = $
}

case object Slip extends Transport {
    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = l match {
        case $(p : Pawn) => true
        case _ => false
    }
}

case object ForestRoads extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Clearing, d : Clearing) => game.connected(o).has(d)
        case (o : Forest, d : Clearing) => game.fromForest(o).has(d)
        case (o : Clearing, d : Forest) => game.fromForest(d).has(o)
        case (o : Forest, d : Forest) => game.board.forestsConnected(o, d)
        case _ => false
    }
}

case object MoveBoots extends Transport {
    override def img = Boots.img

    override def allows(f : Faction)(implicit game : Game) = f @@ {
        case f : Hero => f.ready(Boots) >= 1
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = allows(f, o) && (f, d) @@ {
        case (f : Hero, d : Clearing) => factions.of[WarriorFaction].%(f.hostile).%(_.at(d).of[Warrior].any).none
        case _ => false
    }
}

case object MoveDoubleBoots extends Transport {
    override def img = Boots.img ~ Boots.img

    override def allows(f : Faction)(implicit game : Game) = f @@ {
        case f : Hero => f.ready(Boots) >= 2
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = allows(f, o) && (f, d) @@ {
        case (f : Hero, d : Clearing) => factions.of[WarriorFaction].%(f.hostile).%(_.at(d).of[Warrior].any).any && traders.%(s => f.has(Peacekeepers(s))).%(_.at(d).of[Warrior].any).none
        case _ => false
    }
}

case class AidedWith(f : Hero, e : Faction) extends Message {
    def elem(implicit game : Game) = f.elem ~ " aided " ~ e.elem ~ " with"
}

case object AsAReward extends Message {
    def elem(implicit game : Game) = Text(" as a reward")
}

case class AsArbiter(f : Hero) extends Message {
    def elem(implicit game : Game) = " as " ~ "Protector".styled(f)
}

case class WithItems(l : $[Item]) extends Message with GameModeElementary {
    def elem(implicit game : Game) = " with " ~ l./(_.img).merge
    def elem(g : Game, mode : ConvertMode) = mode match {
        case ModeLog => " with " ~ l./(_.exhaust.elem).spaced
        case _ => elem(g)
    }
}

case object SlipTo extends Message {
    def elem(implicit game : Game) = Text("Slip to")
}

object WithItems {
    def apply(i : Item) : WithItems = WithItems($(i))
    def apply(a : Item, b : Item) : WithItems = WithItems($(a, b))
}

case class WithItemsOutsourcing(l : $[Item], s : $[SuitAsset]) extends Message with GameModeElementary {
    def elem(implicit game : Game) = ("with" + l.any.??(" ")) ~ l./(_.img).merge ~ s./(_ => "Outsourcing".hl)./(" " ~ _).merge
    def elem(g : Game, mode : ConvertMode) = mode match {
        case ModeLog => "with" ~ l./(" " ~ _.exhaust.elem).merge ~ s./(_ => "Outsourcing".hl)./(" " ~ _).merge
        case _ => elem(g)
    }
}

case object Striking extends Message {
    def elem(implicit game : Game) = "striking"
}

case class SomeHostile(m : Message) extends Message {
    def elem(implicit game : Game) = m.elem ~ " hostile"
}


trait VBDaylightQuestion extends FactionAction {
    override def self : Hero
    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~ Div(self.inv.sort.ready./(_.img).merge, styles.margined)
}

case class CharacterRandomAction(f : Hero, random : Character) extends RandomAction[Character]
case class CharacterChooseAction(f : Hero, l : $[Character], except : $[Character]) extends ForcedAction
case class CharacterSelectAction(f : Hero, c : Character) extends ForcedAction

case class ShuffleQuestsAction(shuffled : $[Quest], then : ForcedAction) extends ShuffledAction[Quest]

case class ReadyAction(self : Hero) extends ForcedAction
case class ReadyItemsAction(self : Hero, l : $[ItemRef], then : ForcedAction) extends ForcedAction

case class VagabondMoveMainAction(self : Hero, to1b : Boolean, to2b : Boolean) extends OptionAction("Move".styled(self), $(to1b.?(Boots.img), to2b.?(Boots.img ~ Boots.img)).flatten.join("/")) with VBDaylightQuestion with Soft

case class VagabondDealHitsAction(self : Hero, l : $[ItemRef], then : ForcedAction) extends ForcedAction
case class VagabondDealHitsAlliesAction(self : Hero, b : Battle, l : $[ItemRef], a : $[Figure], then : ForcedAction) extends ForcedAction


case class VagabondRepairMainAction(self : Hero) extends OptionAction("Repair".styled(self), Hammer.img) with VBDaylightQuestion with Soft with Only[RepairItemAction] { def tag = implicitly }
case class RepairItemAction(self : Hero, i : ItemRef) extends ForcedAction

case class VagabondFriendOfTheForestMainAction(f : Hero) extends ForcedAction
case class FriendOfTheForestAction(self : Hero, i : ItemRef) extends ForcedAction

case class VagabondBattleMainAction(self : Hero, c : Clearing, l : $[Faction]) extends OptionAction("Battle".styled(self), Sword.img) with VBDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }

case class VagabondAidMainAction(self : Hero, s : SuitCost, l : $[Faction]) extends OptionAction("Aid".styled(self), dt.CardSuit(s) ~ "&".styled(styles.aidamp) ~ Image("item-any", styles.piece)) with VBDaylightQuestion with Soft with Only[AidExhaustAction] { def tag = implicitly }
case class AidFactionAction(self : Hero, s : SuitCost, e : Faction) extends BaseAction("Aid")(e) with Soft
case class AidCardAction(self : Hero, s : SuitCost, e : Faction, d : DeckCard) extends BaseAction("Aid", e)(d.img) with Soft with ViewCard
case class AidExhaustAction(self : Hero, e : Faction, d : DeckCard, i : Item) extends ForcedAction
case class AidTradeAction(self : Hero, i : Item, e : Faction) extends ForcedAction
case class AidTakeItemAction(self : Hero, i : Item, e : Faction, n : ItemRef) extends BaseAction("Take in trade from", e)(n, n.img)
case class AidIgnoreItemAction(self : Hero, i : Item, f : Faction) extends BaseAction(None)("Don't take an item".styled(styles.hit))
case class AidDoneAction(self : Hero, e : Faction) extends ForcedAction


case class VagabondQuestMainAction(self : Hero, c : Clearing, l : $[Quest]) extends OptionAction("Quests".styled(self), l.any.?(l./(q => q.a.img ~ q.b.img).join(" | ")).|(2.times(Image("item-any", styles.piece)).merge)) with VBDaylightQuestion with Soft with Only[QuestItemsAction] { def tag = implicitly }
case class QuestAction(self : Hero, c : Clearing, q : Quest, inv : $[ItemRef], vp : Int) extends BaseAction("Complete", "Quests".styled(self), "in", c)(q.img, Break, inv.sort.%(_.item == q.a).take(1).single./(_.imgD).|(q.a.imgEmptyD) ~ " " ~ inv.sort.%(_.item == q.b).drop((q.a == q.b).??(1)).take(1).single./(_.imgD).|(q.b.imgEmptyD) ~ Div(2.times(dt.CardBack) ~ " | " ~ vp.vp, styles.margined)) with ViewQuest with Soft
case class QuestItemsAction(self : Hero, q : Quest, l : $[ItemRef]) extends ForcedAction
case class QuestCompleteAction(self : Hero, q : Quest, l : $[ItemRef]) extends ForcedAction
case class QuestInfoAction(self : Hero, q : Quest, inv : $[ItemRef]) extends BaseInfo(Break ~ "Quests")(OnClick(q, Div(q.img, xlo.pointer)), inv.sort.%(_.item == q.a).take(1).single./(_.img).|(q.a.imgEmpty) ~ " " ~ inv.sort.%(_.item == q.b).drop((q.a == q.b).??(1)).take(1).single./(_.img).|(q.b.imgEmpty)) with Info with ViewQuest with OnClickInfo { def param = q }
case class QuestRewardCardsAction(self : Hero, q : Quest, n : Int) extends BaseAction(q, "reward")("Draw", n.hl, "cards")
case class QuestRewardVPAction(self : Hero, q : Quest, n : Int) extends BaseAction(q, "reward")("Score", n.vp)
case class NewQuestAction(self : Hero, q : Quest) extends ForcedAction

case class VagabondExploreMainAction(self : Hero, c : Clearing) extends OptionAction("Explore".styled(self), Torch.img) with VBDaylightQuestion
case class TakeRuinItemAction(self : Hero, c : Clearing, i : Item) extends BaseAction(self, "explores ruins and finds an item")("Take", i.img, i)
case class IgnoreRuinItemAction(self : Hero, c : Clearing) extends BaseAction(None)("Don't take an item".styled(styles.hit))

case class VagabondStealMainAction(self : Hero, l : $[Faction]) extends OptionAction(Steal.of(self), Torch.img) with VBDaylightQuestion with Soft with Only[StealAction] { def tag = implicitly }
case class StealAction(self : Hero, e : Faction) extends BaseAction(Steal.of(self), "from")(e)

case class VagabondDayLaborMainAction(self : Hero, c : Clearing) extends OptionAction(DayLabor.of(self), Torch.img) with VBDaylightQuestion with Soft with Only[DayLaborAction] { def tag = implicitly }
case class DayLaborAction(self : Hero, d : DeckCard) extends BaseAction(DayLabor.of(self))(d.img) with ViewCard

case class VagabondHideoutMainAction(self : Hero) extends OptionAction(Hideout.of(self), Torch.img) with VBDaylightQuestion with Soft with Only[HideoutAction] { def tag = implicitly }
case class HideoutAction(self : Hero, l : $[ItemRef]) extends ForcedAction

case class VagabondScorchedEarthMainAction(self : Hero, c : Clearing) extends OptionAction(ScorchedEarth.of(self), Torch.damage.img) with VBDaylightQuestion with Soft with Only[ScorchedEarthAction] { def tag = implicitly }
case class ScorchedEarthAction(self : Hero, c : Clearing) extends BaseAction("Burn", c, "to the ground")(ScorchedEarth.name.styled(styles.hit))
case class ScorchedEarthDoneAction(self : Hero, c : Clearing) extends ForcedAction

case class VagabondGlideMainAction(self : Hero, l : $[Clearing]) extends OptionAction(Glide.of(self), Torch.img) with VBDaylightQuestion with Soft with Only[GlideAction] { def tag = implicitly }
case class GlideAction(self : Hero, c : Clearing) extends BaseAction(Glide.of(self), "to")(c)

case class VagabondInstigateMainAction(self : Hero, c : Clearing, l : $[Faction]) extends OptionAction(Instigate.of(self), Torch.img) with VBDaylightQuestion with Soft with Only[InstigateAction] { def tag = implicitly }
case class InstigateAction(self : Hero, c : Clearing, a : Faction, d : Faction) extends BaseAction(Instigate.of(self), "a fight in", c)(a, "battles", d)


case class VagabondImproviseMainAction(self : Hero, c : Clearing) extends OptionAction(Improvise.of(self)) with VBDaylightQuestion with Soft with Only[ImproviseAction] { def tag = implicitly }
case class ImproviseQuestAction(self : Hero, c : Clearing, q : Quest, inv : $[ItemRef], vp : Int) extends BaseAction("Improvise", "Quests".styled(self), "in", c)(q.img, Break, inv.sort.%(_.item == q.a).take(1).single./(_.imgD).|(q.a.imgEmptyD) ~ " " ~ inv.sort.%(_.item == q.b).drop((q.a == q.b).??(1)).take(1).single./(_.imgD).|(q.b.imgEmptyD) ~ Div(2.times(dt.CardBack) ~ " | " ~ vp.vp, styles.margined)) with ViewQuest with Soft
case class ImproviseAction(self : Hero, q : Quest, l : $[ItemRef]) extends ForcedAction

case class VagabondStrikeMainAction(self : Hero, c : Clearing, l : $[Faction]) extends OptionAction("Strike".styled(self), Crossbow.img) with VBDaylightQuestion with Soft with Only[StrikeAction] { def tag = implicitly }
case class StrikeAction(self : Hero, c : Clearing, e : Faction, p : Piece) extends ForcedAction

case class VagabondCraftMainAction(self : Hero, n : Int) extends OptionAction("Craft".styled(self), max(n, 1).times(Hammer.img).merge) with VBDaylightQuestion with Soft with Only[CraftAssignAction] { def tag = implicitly }

case class ExhaustItemAction(f : Hero, i : Item, then : ForcedAction) extends ForcedAction
case class ItemsLimitAction(f : Hero, n : Int) extends ForcedAction
case class DiscardItemsAction(f : Hero, capacity : Int, then : ForcedAction) extends ForcedAction
case class DiscardItemsListAction(f : Hero, l : $[ItemRef], then : ForcedAction) extends ForcedAction

case class AdjustItemDamagePrioritiesAction(f : Hero, l : $[ItemRef], d : Int, e : Int, then : ForcedAction) extends ForcedAction with Soft

case class SetItemDamagePriorityAction(f : Hero, l : $[Item], then : ForcedAction) extends ForcedAction with SkipValidate


case class SelectHeroItemsAction(f : Hero, m : Elem, extra : $[UserAction], inner : Boolean = false, order : Boolean = false)(r : ObjectSetRule[QuasiItem] => ObjectSetRule[QuasiItem])(display : ItemRef => RealItem)(then : $[ItemRef] => ForcedAction, a : $[Figure] = $, athen : ($[ItemRef], $[Figure]) => ForcedAction = null, adone : ($[ItemRef], $[Figure]) => Elem = null) extends ForcedAction with Soft with SelfPerform {
    def perform(soft : Void)(implicit game : Game) = {
        val inv = f.inv

        val track = inv.intersect(Item.track).but(Bag.pristine).sort./(inner.?(ToShow).|(display))
        val satchel = inv.diff(Item.track).sort./(display)
        val columns = 3 + inv.intersect(Item.track).count(Bag)
        val (r1, r2) = satchel.splitAt(satchel.num /↑ 2)
        val row1 = r1 ++ (columns - r1.num).times(ItemPlaceholder)
        val row2 = r2 ++ (columns - r2.num).times(ItemPlaceholder)
        val bags = (columns > 3).??(3.times(ItemEmptySpace) ++ (columns - 3).times(Bag.pristine)./(inner.?(ToShow).|(display)))
        val allies = a./(FigureRemove)

        XXSelectObjectsAction(f, track ++ row1 ++ row2 ++ bags ++ allies, order)
            .withGroup(m)
            .withSplit($(track.num, row1.num, row2.num) ++ (columns > 3).?(columns))
            .withBreak({
                case 0 => Gap ~ Gap
                case 1 => Image("satchel-thin-top-"    + columns)(styles.columns(columns)).div(xlo.fullwidth)(xlo.flexhcenter)
                case 3 => Image("satchel-thin-bottom-" + columns)(styles.columns(columns)).div(xlo.fullwidth)(xlo.flexhcenter)
                case _ => HorizontalBreak
            })
            .withRule(rule => r(rule.each(_.available)))
            .withThen(l => (athen != null).?(athen(l./~(_.real), l./~(_.figure))).|(then(l./~(_.real))))(l => (adone != null).?(adone(l./~(_.real), l./~(_.figure))).|("Done".hl))
            .withExtra(extra)
    }
}


object HeroExpansion extends FactionExpansion[Hero] {
    def characters(implicit options : $[Meta.O]) : $[Character] =
        $(Thief, Tinker, Ranger, Arbiter, Scoundrel, Vagrant, Adventurer, Ronin, Harrier) ++
        options.has(FolkHeroCharacter).$(FolkHero)

    override def extraMoveFrom(f : Faction)(implicit game : Game) = f @@ {
        case f : Hero => $(f.region)
        case _ => $()
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Hero) =>
            game.states += f -> new HeroPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Hero) =>
            val x = factions.of[Hero].but(f).%(game.states.contains)./~(_.character)

            if (hrf.HRF.flag("testabond"))
                Force(CharacterSelectAction(f, Testabond))
            else
            if (game.chars.contains(f))
                Force(CharacterSelectAction(f, game.chars(f)))
            else
            if (options.has(RandomCharacter))
                Random(characters(options).diff(x), CharacterRandomAction(f, _))
            else
                CharacterChooseAction(f, characters(options), x)

        case CharacterRandomAction(f, c) =>
            CharacterChooseAction(f, $(c), $)

        case CharacterChooseAction(f, l, x) =>
            implicit def convert(c : Character) = {
                game.desc(
                    Image(c.title)(styles.chartitle),
                    Break,
                    Image(c.img)(styles.quest)((c == Ranger).?(styles.ranger))((c == Adventurer).?(styles.adventurer))((c == FolkHero).?(styles.folk)).div(styles.charblock) ~ c.starting(options)./(_.img).merge,
                    Break,
                    c.special./(_.of(f))
                )
            }

            YYSelectObjectsAction(f, l)
                .withGroup((l.num > 1).?("Choose character").|("Random character"))
                .withSplit($(3, 3, 3))
                .withRuleExcept(x)
                .withThen(CharacterSelectAction(f, _))("Play as " ~ _.name.hl)("~~~")

        case CharacterSelectAction(f, c) =>
            f.character = Some(c)
            f.inv = c.starting(options)./(_.ref(false, false))

            f.log("was", c.name.styled(f), "with", c.starting(options))

            if (hrf.HRF.flag("testabond"))
                f.attitude = game.candidates.notOf[Hero]./(_ -> Allied).toMap

            StartingRegionAction(f)

        case StartingRegionAction(f) =>
            Ask(f)(board.forests./(StartingForestAction(f, _)))

        case StartingForestAction(f : Hero, r) =>
            f.pawn --> r

            f.log("started in", board.forestName(r))

            if (game.quests.none)
                Shuffle[Quest](options.has(RevisedQuests).?(Quest.revised).|(Quest.default), ShuffleQuestsAction(_, SetupFactionsAction))
            else
                SetupFactionsAction

        case ShuffleQuestsAction(l, then) =>
            game.quests = l

            then

        // HELPER
        case MoveToAction(self, f : Hero, tt, m, from : Region, to : Region, then) =>
            val mv = f.at(from).of[Movable]
            val cm = 1.to(mv.num)./~(n => mv.combinations(n))
            val pp = tt./~(t => cm.%(l => t.forall(_.allows(f, from, to, l)))./(l => l.sortBy(m => t./(_.sortBy(m)).sum))./(Party(t, _))).sortBy(p => p.transport./(t => t.order(p.movable)).sum)

            val l = pp./(p => MoveListAction(self, f, p.transport, m, from, to, p.movable, then))

            val aa = tt.%(_.has(Slip).not).any.??(factions.of[WarriorFaction].%(f.allied).%(_.at(from).of[Warrior].notOf[Tenacious].any))

            val al = aa./~{ a =>
                val amv = a.at(from).of[Movable].notOf[Tenacious]
                val acm = 1.to(amv.num)./~(n => amv.combinations(n))
                val app = a.transports./~(t => acm.%(l => t.forall(_.allows(a, from, to, l)))./(l => l.sortBy(m => t./(_.sortBy(m)).sum))./(Party(t, _))).sortBy(p => p.transport./(t => t.order(p.movable)).sum)

                pp./~(p => app./~(ap => p.transport.forall(_.allows(f, from, to, p.movable ++ ap.movable)).?(MoveListAlliedAction(self, f, p.transport ++ ap.transport, m, from, to, p.movable, a, ap.movable, then))))
            }

            val ll = l ++ al

            Ask(self, ll ++ (ll.num != 1).?(CancelAction))

        case MoveListAlliedAction(self, f : Hero, t, m, from, to, List(p : Pawn), a, w, then) if t.has(MoveBoots) =>
            f.exhaust(Boots)

            Force(MoveListAlliedAction(self, f, t.but(MoveBoots), m, from, to, List(p : Pawn), a, w, then))

        case MoveListAlliedAction(self, f : Hero, t, m, from, to, List(p : Pawn), a, w, then) if t.has(MoveDoubleBoots) =>
            f.exhaust(Boots)
            f.exhaust(Boots)

            Force(MoveListAlliedAction(self, f, t.but(MoveDoubleBoots), m, from, to, List(p : Pawn), a, w, then))

        case MoveListAlliedAction(self, f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), a, w, then) if t.has(Ferry) =>
            f.log("moved on", Ferry.elem, "with", w./(_.of(a)).comma, "from", from, "to", to, m)

            Force(MoveListAlliedAction(self, f, $, NoMessage, from, to, $(p), a, w, FerryAction(f, from, to, then)))

        case MoveListAlliedAction(self, f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), a, w, then) if t.has(OffTrail) =>
            f.log("moved with", w./(_.of(a)).comma, "from", from, "to", to, m, "going", OffTrail)

            Force(MoveListAlliedAction(self, f, $, NoMessage, from, to, $(p), a, w, UsedEffectAction(f, OffTrail, then)))

        case MoveListAlliedAction(self, f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), a, w, then) if t.any =>
            f.log("moved with", w./(_.of(a)).comma, "from", from, "to", to, m)

            Force(MoveListAlliedAction(self, f, $, NoMessage, from, to, $(p), a, w, then))

        case MoveListAlliedAction(self, f : Hero, Nil, m, from : Clearing, to : Clearing, List(p : Pawn), a, w, then) =>
            game.highlights :+= MoveHighlight(from, to)

            f.pawn --> to
            a.from(from) --> w --> to

            MoveCompleteAction(self, f, from, to, $(p), $, MoveCompleteAction(self, a, from, to, w, $, then))

        case MoveListAction(self, f : Hero, t, m, from, to, List(p : Pawn), then) if t.has(MoveBoots) =>
            f.exhaust(Boots)

            Force(MoveListAction(self, f : Hero, t.but(MoveBoots), m, from, to, List(p : Pawn), then))

        case MoveListAction(self, f : Hero, t, m, from, to, List(p : Pawn), then) if t.has(MoveDoubleBoots) =>
            f.exhaust(Boots)
            f.exhaust(Boots)

            Force(MoveListAction(self, f : Hero, t.but(MoveDoubleBoots), m, from, to, List(p : Pawn), then))

        case MoveListAction(self, f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), then) if t.has(Ferry) && t.has(Slip) =>
            f.log("slipped on", Ferry.elem, "from", from, "to", to)

            Force(MoveListAction(self, f, $, NoMessage, from, to, $(p), FerryAction(f, from, to, then)))

        case MoveListAction(self, f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), then) if t.has(OffTrail) && t.has(Slip) =>
            f.log("slipped from", from, "to", to, "going", OffTrail)

            Force(MoveListAction(self, f, $, NoMessage, from, to, $(p), UsedEffectAction(f, OffTrail, then)))

        case MoveListAction(self, f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), then) if t.has(Ferry) =>
            f.log("moved on", Ferry.elem, "from", from, "to", to, m)

            Force(MoveListAction(self, f, $, NoMessage, from, to, $(p), FerryAction(f, from, to, then)))

        case MoveListAction(self, f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), then) if t.has(OffTrail) =>
            f.log("moved from", from, "to", to, m, "going", OffTrail)

            Force(MoveListAction(self, f, $, NoMessage, from, to, $(p), UsedEffectAction(f, OffTrail, then)))

        case MoveListAction(self, f : Hero, t, m, from : Forest, to : Clearing, List(p : Pawn), then) if t.has(Slip) =>
            f.log("slipped to", to, "from forest")

            Force(MoveListAction(self, f, $, NoMessage, from, to, $(p), then))

        case MoveListAction(self, f : Hero, t, m, from : Region, to : Region, List(p : Pawn), then) if t.has(Slip) =>
            f.log("slipped from", from, "to", to)

            Force(MoveListAction(self, f, $, NoMessage, from, to, $(p), then))

        case MoveListAction(self, f : Hero, t, m, from : Region, to : Region, List(p : Pawn), then) if t.any =>
            f.log("moved", "from", from, "to", to, m)

            Force(MoveListAction(self, f, $, NoMessage, from, to, $(p), then))

        case MoveListAction(self, f : Hero, Nil, m, from : Region, to : Region, l @ List(p : Pawn), then) =>
            game.highlights :+= MoveHighlight(from, to)

            f.pawn --> to

            MoveCompleteAction(f, f, from, to, l, $, then)


        case AcquireItemsAction(f : Hero, then) =>
            f.inv ++= f.forTrade./(_.pristine)
            f.forTrade = $

            then

        case CraftAssignAction(f : Hero, d, all, used, m, then : CraftPerformAction) =>
            var u = used

            val x = (f.frogCraft ++ f.extraCraft).intersect(u)

            if (x.any) {
                f.extraCraft = f.extraCraft.diff(x)

                u = u.diff(x)
            }

            u.foreach(_ => f.exhaust(Hammer))

            then.copy(m = WithItemsOutsourcing(u./(_ => Hammer), x))

        case ForcedRemoveSourceEffectAction(e : Hero, c, f : Hireling, p : Warrior, then) =>
            then

        case ForcedRemoveSourceEffectAction(e : Hero, c, f, p : Warrior, then) if e.hostile(f).not =>
            e.attitude += f -> Hostile
            f.log("became", Hostile, "to", e)

            TryAgain

        case BattleStartAction(self, f, a, m, c, o, i, ExhaustItemAction(hero, Sword, then)) =>
            hero.exhaust(Sword)

            BattleStartAction(self, f, a, m, c, o, i, then)

        case LegalAAAConveneAction(f, h, c, e, ExhaustItemAction(hero, Sword, then)) =>
            hero.exhaust(Sword)

            LegalAAAConveneAction(f, h, c, e, then)


        case BattleAssignHitsAction(f : Hero, b, n, s, then) =>
            val ambush = b.ally.any && then @@ {
                case BattleAmbushHitsAction(bb, ah, then) => true
                case _ => false
            }

            val l = f.inv.notDamaged
            val a = (b.attacker == f).??(b.ally.?(a => a.from(b.clearing).$.%(_.piece.is[Warrior])))
            if (l.none && a.none)
                then
            else
            if (l.num + a.num <= n)
                VagabondDealHitsAlliesAction(f, b, l, a, then)
            else
            if (game.current != f && options.has(ForcedAsyncMode)) {
                var ll = l
                var ss = $[ItemRef]()

                f.log(f.itemDamagePriorities)

                f.itemDamagePriorities.foreach { item =>
                    if (ss.num < n) {
                        if (ll.has(item.exhaust)) {
                            ll :-= item.exhaust
                            ss :+= item.exhaust
                        }
                        else
                        if (ll.has(item.pristine)) {
                            ll :-= item.pristine
                            ss :+= item.pristine
                        }
                    }
                }

                if (ss.num < n)
                    AdjustItemDamagePrioritiesAction(f, $, n, 0, BattleAssignHitsAction(f, b, n, s, then))
                else {
                    ss.foreach { ref =>
                        f.itemDamagePriorities :-= ref.item
                    }
                    Ask(f)(VagabondDealHitsAlliesAction(f, b, ss, $, then).as("Done".hl))
                }
            }
            else
                SelectHeroItemsAction(f, "Damage " ~ n.hl ~ " items" ~ a.any.??(" or remove warriors"), $())(_.num(n))(ToDamage.apply)(null, a, (l, u) => VagabondDealHitsAlliesAction(f, b, l, u, then), (l, u) => "Done".hl ~ (l.num - u.num + b.balance < 0).?(" (" ~ b.ally.get.elem ~ " " ~ ambush.?("may").|("will") ~ " become " ~ Hostile.elem ~ ")"))

        case VagabondDealHitsAlliesAction(f, b, l, a, BattleAmbushHitsAction(bb, ah, then)) =>
            VagabondDealHitsAction(f, l, BattleDealHitsAction(f, b, a, BattleAmbushHitsAction(bb.copy(balance = bb.balance + l.num - a.num), ah, then)))

        case VagabondDealHitsAlliesAction(f, b, l, a, BattleResolveHitsAction(bb, ah, dh, ash)) =>
            VagabondDealHitsAction(f, l, BattleDealHitsAction(f, b, a, BattleResolveHitsAction(bb.copy(balance = bb.balance + l.num - a.num), ah, dh, ash)))

        case VagabondDealHitsAlliesAction(f, b, l, a, then) =>
            VagabondDealHitsAction(f, l, BattleDealHitsAction(f, b, a, then))

        case VagabondDealHitsAction(f, l, then) =>
            val d = l./(_.damage)
            f.inv = f.inv.diff(l) ++ d

            f.log("damaged", d)

            then

        case BattlePostHitOutAction(b, _, _, _, TryForcedRemoveAction(e : Hero, c, f : WarriorFaction, p, x, m, then, fail)) if e.hostile(f) && (b.attacker == e || b.instigator.has(e)) =>
            val bonusA = p.is[Warrior] || options.has(WarriorsOnlyInfamy).not
            val bonusB = e.used.has(Infamy(f)).not || options.has(LimitedInfamy).not

            if (bonusA && bonusB)
                TryForcedRemoveAction(e, c, f, p, x + 1, SomeHostile(m), UsedEffectAction(e, Infamy(f), then), fail)
            else
                TryForcedRemoveAction(e, c, f, p, x, m, then, fail)

        case BattleCleanupAttackerAction(b, f : Hero) =>
            if (b.balance < 0) {
                f.attitude += b.ally.get -> Hostile
                log(b.ally.get, "became", Hostile, "to", f)
            }

            BattleForcedRemoveAction(b)

        // TURN
        case BirdsongNAction(40, f : Hero) =>
            val e = f.inv.exhausted.num
            val b = 3
            val t = min(3, f.ready(Teapot)) * 2
            val r = options.has(RootingForTheUnderdog).?? {
                val others = factions.but(f).of[Hero].%(_.vp >= 0)
                others.exists(_.vp > f.vp).??(1) - others.exists(_.vp < f.vp).??(1)
            }
            val n = b + t + r

            if (n < e)
                SelectHeroItemsAction(f, "Ready " ~ n.hl ~ " items", $())(_.num(n))(ToRefresh(_))(l => ReadyItemsAction(f, l, Next))
            else {
                if (e > 0) {
                    f.inv = f.inv./(_.copy(exhausted = false))
                    f.log("readied all items")
                }
                Next
            }

        case ReadyItemsAction(f, l, then) =>
            l.foreach { i =>
                val r = i.copy(exhausted = false)
                f.inv :-= i
                f.inv :+= r
            }

            f.log("readied", l)

            then

        case BirdsongNAction(60, f : Hero) =>
            soft()

            val o = f.region

            val (t, l) = f.movePlans($(o), $($(Slip)) ** game.transports.but(Roads).appended(ForestRoads)./($))(o)

            Ask(f)(l./(d => {
                val tt = t.%(_.forall(_.allows(f, o, d)))
                MoveToAction(f, f, tt, SlipTo, o, d, Next).x(tt.none, "forbidden")
            }))(Next.as("Stay in", o))(f.birdsong)

        case DaylightNAction(50, f : Hero) =>
            soft()

            var actions : $[UserAction] = $

            actions :+= f.movePlans($(f.region), game.transports./($) ** f.transports ** $($(MoveBoots), $(MoveDoubleBoots))).get(f.region) @@ {
                case Some((t, l)) => VagabondMoveMainAction(f, t.%(_.has(MoveBoots)).any, t.%(_.has(MoveDoubleBoots)).any).x(f.canMoveFrom(f.region).not, "can't move")
                case None => VagabondMoveMainAction(f, true, false).x(f.canMoveFrom(f.region).not, "can't move").x(f.lacks(Boots), "no boots").x(f.ready(Boots) == 1, "not enough boots")
            }

            f.region.as[Clearing].foreach { c =>
                val att = f.canAttackList(c)

                actions :+= VagabondBattleMainAction(f, c, att).!(f.lacks(Sword), "no sword").!(att.none, "no enemy")
            }

            f.region.as[Clearing].but(game.scorched).foreach { c =>
                val ee = factions.but(f).%(_.present(c))

                val eeh = ee ++ hirelings.%(_.present(c))

                actions :+= VagabondExploreMainAction(f, c).x(game.ruins.get(c).none, "no ruins").x(f.lacks(Torch), "no torch")

                if (f.can(Steal))
                    actions :+= VagabondStealMainAction(f, ee.%(_.hand.any)).x(ee.none, "no target").x(f.lacks(Torch), "no torch").x(ee.%(_.hand.any).none, "no cards")

                if (f.can(DayLabor))
                    actions :+= VagabondDayLaborMainAction(f, c).!(pile.exists(_.matches(c.cost)).not, "no matching cards in discard").x(f.lacks(Torch), "no torch")

                if (f.can(Hideout))
                    actions :+= VagabondHideoutMainAction(f).x(f.inv.damaged.none, "no damaged items").x(f.lacks(Torch), "no torch")

                if (f.can(ScorchedEarth))
                    actions :+= VagabondScorchedEarthMainAction(f, c).x(f.lacks(Torch), "no torch").x(!f.canPlace(c), "forbidden").x((factions ++ hirelings).%(_.present(c)).%!(f.canRemove(c)).any, "protector")

                if (f.can(Improvise)) {
                    val qq = game.quests.take(3).%(_.suit.matches(c.cost))
                    val inv = f.inv.ready./(_.item)
                    val ss = (inv.num > 1).??(qq.%(q => inv.has(q.a) || inv.has(q.b)).%(q => inv.diff(q.ab).any))
                    actions :+= VagabondImproviseMainAction(f, c).x(!f.can(Improvise), "once per turn").x(qq.none, "no matching quests").x(inv.num < 2, "not enough items").x(ss.none, "not a single matching item")
                }

                if (f.can(Instigate)) {
                    val l = ee ++ f.canAttackSelf(c).?(f)
                    actions :+= VagabondInstigateMainAction(f, c, l).x(f.lacks(Torch), "no torch").x(eeh.none, "empty").x(l.none, "no attacker")
                }

                actions :+= VagabondAidMainAction(f, c.cost, ee.notOf[Hero]).x(ee.notOf[Hero].none, "no target").x(f.inv.ready.none, "no items").x(f.hand.%(_.matches(c.cost)).none, "no matching cards")

                val qq0 = game.quests.take(3).%(_.suit.matches(c.cost))
                val qq = qq0.%(q => q.ab.diff(f.inv.ready./(_.item)).none)

                actions :+= VagabondQuestMainAction(f, c, qq).x(qq0.none, "wrong suit").x(qq.none, "no items")

                val rmm = eeh.%!(f.friends).%(f.canRemove(c))

                actions :+= VagabondStrikeMainAction(f, c, rmm).!(f.lacks(Crossbow), "no crossbow").!(rmm.none, "no target")

                actions :+= VagabondCraftMainAction(f, f.ready(Hammer)).x(f.lacks(Hammer), "no hammer").x(f.hand.none, "no cards").x(f.hand.%(f.craftable).none, "nothing craftable")
            }

            if (f.can(Glide))
                actions :+= VagabondGlideMainAction(f, clearings.but(f.region.as[Clearing])).x(f.lacks(Torch), "no torch")

            actions :+= VagabondRepairMainAction(f).x(f.inv.damaged.none).x(f.lacks(Hammer), "no hammer")

            Ask(f)(actions)(Next.as("End Turn"))(f.daylight)

        case ExhaustItemAction(f, i, then) =>
            f.exhaust(i)
            then

        case VagabondCraftMainAction(f, _) =>
            Force(CraftMenuAction(f))

        case VagabondMoveMainAction(f, l1b, l2b) =>
            MoveInitAction(f, f, game.transports./($) ** f.transports ** $($(MoveBoots), $(MoveDoubleBoots)), NoMessage, $(f.region), $(f.region), $(), Repeat)

        case VagabondBattleMainAction(f, c, l) =>
            BattleInitAction(f, f, WithItems(Sword), $(c), $(CancelAction), ExhaustItemAction(f, Sword, Repeat))

        case VagabondExploreMainAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.exhaust(Torch)

            f.logtemp("explored", c, "ruins", "with", Torch.exhaust.elem)

            Ask(f)
                .each(game.ruins(c).items)(i => TakeRuinItemAction(f, c, i).!(f.fromRuins.has(i)))
                .add(IgnoreRuinItemAction(f, c))
                .needOk

        case TakeRuinItemAction(f, c, i) =>
            val rest = game.ruins(c).items :- i

            if (rest.any)
                game.ruins += c -> Ruins(rest)
            else
                game.ruins -= c

            f.inv :+= i.ref(false, false)
            f.fromRuins :+= i

            f.nscore(1)("exploring ruins")(f, "explored", c, "ruins", "with", Torch.exhaust.elem ~ " found", i, "and", ScoredVP)

            Repeat

        case IgnoreRuinItemAction(f, c) =>
            f.log("explored", c, "ruins", "with", Torch.exhaust.elem, "but took nothing")

            Repeat

        case VagabondStealMainAction(f, l) =>
            Ask(f)(l./(StealAction(f, _))).cancel

        case StealAction(f, e) =>
            f.use(Torch)

            f.log("stole a card from", e, "with", Torch.exhaust.elem)

            StealCardAction(f, e, Repeat)

        case VagabondDayLaborMainAction(f, c) =>
            Ask(f).each(pile)(d => DayLaborAction(f, d).!(d.matches(c.cost).not)).cancel

        case DayLaborAction(f, d) =>
            f.exhaust(Torch)

            pile --> d --> f.hand

            f.log("took", d, "from the dicard pile", "with", Torch.exhaust.elem)

            Repeat

        case VagabondHideoutMainAction(f) =>
            val n = min(3, f.inv.damaged.num)
            SelectHeroItemsAction(f, "Repair " ~ n.hl ~ " item" ~ (n > 1).??("s"), $(CancelAction))(_.num(n))(ToRepair(_))(l => HideoutAction(f, l))

        case HideoutAction(f, l) =>
            f.use(Torch)

            f.log("went into", Hideout.of(f))

            l.foreach { i =>
                val r = i.copy(damaged = false)

                f.inv :-= i
                f.inv :+= r

                f.log("repaired", r)
            }

            EveningStartAction(f)

        case VagabondFriendOfTheForestMainAction(f) =>
            if (f.inv.damaged.any)
                SelectHeroItemsAction(f, "Repair an item", $)(_.num(1))(ToRepair(_))(l => FriendOfTheForestAction(f, l(0)))
            else
                Ask(f).done(Repeat)

        case FriendOfTheForestAction(f, i) =>
            val r = i.copy(damaged = false)

            f.inv :-= i
            f.inv :+= r

            f.log("repaired", r, "as", FriendOfTheForest.of(f))

            Repeat

        case VagabondScorchedEarthMainAction(f, c) =>
            Ask(f)(ScorchedEarthAction(f, c)).cancel

        case ScorchedEarthAction(f, c) =>
            val i = Torch.ref(false, false)
            f.inv :-= i
            f.wasted :+= i.item

            f.log("dropped", i, "and scorched", c)

            game.ruins -= c

            NukeAction(f, factions.but(f) ++ hirelings, $(c), NukeType.TotalAnnihilation, ScorchedEarthDoneAction(f, c))

        case ScorchedEarthDoneAction(f, c) =>
            game.scorched :+= c

            Repeat

        case VagabondGlideMainAction(f, l) =>
            Ask(f)(l./(GlideAction(f, _))).cancel

        case GlideAction(f, to) =>
            val from = f.region

            f.use(Torch)

            f.pawn --> to

            f.log("glided to", to)

            MoveCompleteAction(f, f, from, to, $(f.pawn.piece), $, Repeat)

        case VagabondInstigateMainAction(f, c, l) =>
            Ask(f)(l./~(a => a.canAttackList(c)./(d => InstigateAction(f, c, a, d)))).cancel

        case InstigateAction(f, c, a, d) =>
            f.use(Torch)

            f.log("tricked", a, "into attacking", d)

            Force(BattleStartAction(a, a, a, NoMessage, c, d, Some(f), Repeat))

        case VagabondImproviseMainAction(f, c) =>
            val qq = game.quests.take(3).%(_.suit.matches(c.cost))
            val ii = f.inv.ready.sort./(_.item)

            val iq = qq.%(q => q.ab.distinct./~(x => ii.has(x).??(ii.diff(q.ab).but(q.other(x)).distinct)).any)

            Ask(f)(game.quests.take(3)./(q => ImproviseQuestAction(f, c, q, f.inv, (f.quests.count(q.suit) + 1)).!(iq.has(q).not))).cancel

        case ImproviseQuestAction(f, _, q, _, _) =>
            def impr(l : $[Item]) = (l.num < 2) || (l.intersect(q.ab).any && l.diff(q.ab).any)

            SelectHeroItemsAction(f, "Exhaust and damage " ~ q.a.img ~ dt.ItemAnyDamaged ~ " or " ~ dt.ItemAnyDamaged ~ q.b.img ~ " to complete " ~ q.elem, $(CancelAction))(_.num(2).all(l => impr(l./~(_.real)./(_.item))))(ToExhaustDamage(q.ab, _))(l => ImproviseAction(f, q, l))

        case ImproviseAction(f, q, l) =>
            f.used :+= Improvise

            val ex = l./(_.item).intersect(q.ab)
            val ed = l./(_.item).diff(q.ab)

            val iex = ex./(_.exhaust)
            val ied = ed./(_.exhaust.damage)

            f.inv = f.inv.diff(l) ++ iex ++ ied

            ied.%(_.damaged).foreach(i => f.log("exhausted and damaged", i))

            QuestCompleteAction(f, q, iex ++ ied)

        case VagabondAidMainAction(f, s, ff) =>
            Ask(f)(ff./(AidFactionAction(f, s, _))).cancel

        case AidFactionAction(f, s, e) =>
            Ask(f).each(f.hand)(d => AidCardAction(f, s, e, d).!(d.matches(s).not)).cancel

        case AidCardAction(f, s, e, d) =>
            SelectHeroItemsAction(f, "Exhaust an item to aid " ~ e.elem ~ " with " ~ d.elem, $(CancelAction))(_.num(1))(ToExhaust(_))(l => AidExhaustAction(f, e, d, l(0).item))

        case AidExhaustAction(f, e, d, i) =>
            f.exhaust(i)

            f.hand --> d --> e.hand

            f.logtemp("aided", e, "with", i.exhaust)

            game.notify(e, $(ViewCardInfoAction(f, AidedWith(f, e).elem(game), d)))

            AidTradeAction(f, i, e)

        case AidTradeAction(f, i, e) =>
            Ask(f)(e.forTrade./(AidTakeItemAction(f, i, e, _)) :+ AidIgnoreItemAction(f, i, e))

        case AidTakeItemAction(f, i, e, n) =>
            f.inv :+= n.pristine
            e.forTrade :-= n

            f.log("aided", e, "with", i.exhaust, "and", "took", n, "in trade")

            AidDoneAction(f, e)

        case AidIgnoreItemAction(f, i, e) =>
            f.log("aided", e, "with", i.exhaust, e.forTrade.any.??("and took nothing"))

            AidDoneAction(f, e)

        case AidDoneAction(f, e) =>
            val att = f.attitude.get(e).|(Indifferent)
            val aid = f.aid.get(e).|(0) + 1

            f.aid += e -> 0

            (att, aid) match {
                case (Indifferent, 1) =>
                    f.attitude += e -> Amiable
                    f.nscore(1)("aiding", e)(e, "attitude became", Amiable, "and", f, ScoredVP)
                case (Amiable, 2) =>
                    f.attitude += e -> Friendly
                    f.nscore(2)("aiding", e)(e, "attitude became", Friendly, "and", f, ScoredVP)
                case (Friendly, 3) =>
                    f.attitude += e -> Allied
                    f.nscore(2)("aiding", e)(e, "attitude became", Allied, "and", f, ScoredVP)
                case (Allied, 1) =>
                    f.oscore(2)("aiding ally", e)
                case _ =>
                    f.aid += e -> aid
            }

            if (f.has(FriendOfTheForest) && att == Hostile) {
                f.attitude += e -> Indifferent

                e.log("became", Indifferent, "to", f)

                VagabondFriendOfTheForestMainAction(f)
            }
            else
                Repeat


        case VagabondQuestMainAction(f, c, _) =>
            Ask(f)
                .each(game.quests.take(3))(q => QuestAction(f, c, q, f.inv, f.quests.count(q.suit) + 1).!(q.suit.matches(c.cost).not).!(q.ab.diff(f.inv.ready./(_.item)).any))
                .cancel

        case QuestAction(f, c, q, _, _) =>
            SelectHeroItemsAction(f, "Exhaust " ~ q.a.img ~ q.b.img ~ " to complete " ~ q.elem, $(CancelAction))(_.num(2).all(_./~(_.real)./(_.item).diff(q.ab).none))(ToExhaust(_))(l => QuestItemsAction(f, q, l))

        case QuestItemsAction(f, q, l) =>
            l.foreach { i =>
                f.inv :-= i.pristine
                f.inv :+= i.exhaust
            }
            QuestCompleteAction(f, q, l./(_.exhaust))

        case QuestCompleteAction(f, q, l) =>
            game.highlights :+= PlaceHighlight($(f.region).of[Clearing])

            f.quests :+= q.suit
            f.log("completed quest", q, "with", l)
            Ask(f)((QuestRewardCardsAction(f, q, 2) :: QuestRewardVPAction(f, q, f.quests.count(q.suit))))

        case QuestRewardCardsAction(f, q, n) =>
            DrawCardsAction(f, n, AsAReward, AddCardsAction(f, NewQuestAction(f, q)))

        case QuestRewardVPAction(f, q, n) =>
            f.oscore(n)("as a quest reward")
            NewQuestAction(f, q)

        case NewQuestAction(f, q) =>
            game.quests :-= q

            if (game.quests.num >= 3)
                log("New quest available", game.quests(2))

            Repeat

        case VagabondStrikeMainAction(f, c, ee) =>
            val xx = ee./~(_.from(c).$)

            SelectFiguresAction(f, "Strike".styled(f), xx, $(CancelAction))(_
                .num(1)
                .each(u => u.piece.is[Warrior] || u.faction.at(c).of[Warrior].none)
                .each(u => u.piece == Vagabond || u.piece.is[Tenacious].not)
            )(x => StrikeAction(f, c, x.only.faction, x.only.piece))

        case StrikeAction(f, c, e, p) =>
            f.use(Crossbow, "shot with")

            if (e.is[Hero]) {
                BattleAssignHitsAction(e, Battle(c, f, None, e, None, None, None, 0, 0, 0, DummyAction), 1, 0, Repeat)
            }
            else {
                log(p.of(e), "was removed from", c)

                TryForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), Striking, ForcedRemoveFinishedAction(e, Repeat), Repeat)
            }

        case VagabondRepairMainAction(f) =>
            SelectHeroItemsAction(f, "Repair an item", $(CancelAction))(_.num(1))(ToRepair(_))(l => RepairItemAction(f, l(0)))

        case RepairItemAction(f, i) =>
            f.exhaust(Hammer)

            val r = i.copy(damaged = false)

            f.inv :-= i
            f.inv :+= r

            f.log("repaired", r, "with", Hammer.exhaust)

            Repeat

        case EveningNAction(40, f : Hero) =>
            f.aid = factions.but(f)./(_ -> 0).toMap

            if (f.region.is[Forest]) {
                if (f.inv.damaged.any || f.inv.exhausted.any) {
                    f.inv = f.inv./(_.copy(exhausted = false, damaged = false))

                    f.log("repaired all items")
                }
            }

            Next

        case NightStartAction(f : Hero) =>
            val n = 1 + min(3, f.ready(Coins))
            EveningDrawAction(f, n)

        case EveningEndAction(f : Hero) =>
            val n = 6 + min(3, f.ready(Bag)) * 2
            val k = f.inv.diff(f.dontCount).num

            if (k <= n)
                CleanUpAction(f)
            else
                SelectHeroItemsAction(f, "Discard " ~ (k - n).hl ~ " items", $(), true)(_.num(k - n))(ToDiscard(_))(DiscardItemsListAction(f, _, CleanUpAction(f)))

        case DiscardItemsListAction(f, l, then) =>
            l.foreach { i =>
                f.inv :-= i
                f.wasted :+= i.item
                f.log("dropped", i)
            }

            then

        case AfterTurnAction(f : Hero) if options.has(ForcedAsyncMode) =>
            if (f.region.is[Clearing] && f.inv.notDamaged.any)
                AdjustItemDamagePrioritiesAction(f, $, 0, f.inv.notDamaged.num /↑ 2, BetweenTurnAction)
            else
                BetweenTurnAction

        case AdjustItemDamagePrioritiesAction(f, l, d, e, then) =>
            val n = d + e
            val m1 = (e > 0).?("Queue items in case of damage, min " ~ (e).hl ~ " items")
            val m2 = (d > 0).?("Damage " ~ (d).hl ~ " " ~ ("item" + (d > 1).??("s")) ~ ", can queue other items for damage")
            SelectHeroItemsAction(f, m1 ~ m2, $, order = true)(_.atLeast(d + e))(ToDamage.apply)(null, $, (l, u) => SetItemDamagePriorityAction(f, l./(_.item), then), (l, u) => l.none.?("Done".hl) ~ l./(_.elem).join(" " ~ dt.Arrow ~ " "))

        case SetItemDamagePriorityAction(f, l, then) =>
            f.itemDamagePriorities = l

            then

        case _ => UnknownContinue
    }

}
