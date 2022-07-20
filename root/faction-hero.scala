package root
//
//
//
//
import logger._, colmat._
//
//
//
//

import root.gaming._

import hrf.elem._
import root.elem._

trait Hero extends Faction {
    val expansion = HeroExpansion

    val abilities = List(Nimble)

    override val transports : List[List[Transport]] = $($(FreeMove))
    
    val pieces = Vagabond ** 1
    val pawn = figures(0)

    def advertising = Empty
    def motto = Empty
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
}

case object Vagabond extends Pawn

sealed trait Attitude extends Named with Styling {
    def name = toString
}
case object Hostile extends Attitude
case object Indifferent extends Attitude
case object Amiable extends Attitude 
case object Friendly extends Attitude
case object Allied extends Attitude



case object Nimble extends FactionEffect

trait Character extends Record {
    val name : String = toString
    val starting : List[Item]
    val special : List[FactionEffect]
    val img = "vb-char-" + name
    val title = "vb-title-" + name
}


case object Testabond extends Character {
    val starting = List(Boots, Boots, Boots, Boots, Torch, Torch, Teapot, Teapot, Teapot, Sword, Sword, Sword, Crossbow, Hammer, Hammer, Hammer, Hammer, Bag, Bag, Bag, Coins)
    val special = $(Steal, DayLabor, Hideout, Protector, ScorchedEarth, Instigate, Glide, SwiftStrike, Improvise)
}


case object Steal extends FactionEffect

case object Thief extends Character {
    val starting = List(Boots, Torch, Teapot, Sword)
    val special = $(Steal)
}


case object DayLabor extends FactionEffect {
    override def name = "Day Labor"
}

case object Tinker extends Character {
    val starting = List(Boots, Torch, Bag, Hammer)
    val special = $(DayLabor)
}


case object Hideout extends FactionEffect

case object Ranger extends Character {
    val starting = List(Boots, Torch, Crossbow, Sword)
    val special = $(Hideout)
}


case object Protector extends FactionEffect

case object Arbiter extends Character {
    val starting = List(Boots, Torch, Sword, Sword)
    val special = $(Protector)
}


case object ScorchedEarth extends FactionEffect {
    override def name = "Scorched Earth"
}

case class ScorchedEarthMarker(n : Int) extends Token

case object Scoundrel extends Character {
    val starting = List(Boots, Boots, Torch, Crossbow)
    val special = $(ScorchedEarth)
}


case object Instigate extends FactionEffect

case object Vagrant extends Character {
    val starting = List(Boots, Torch, Coins)
    val special = $(Instigate)
}


case object Glide extends FactionEffect

case object Harrier extends Character {
    val starting = List(Coins, Torch, Sword, Crossbow)
    val special = $(Glide)
}


case object SwiftStrike extends FactionEffect with BattleEffect {
    override val name = "Swift Strike"
}

case object Ronin extends Character {
    val starting = List(Boots, Boots, Torch, Sword)
    val special = $(SwiftStrike)
}


case object Improvise extends FactionEffect

case object Adventurer extends Character {
    val starting = List(Boots, Torch, Hammer)
    val special = $(Improvise)
}


case class Quest(suit : Suit, name : String, id : String, a : Item, b : Item) extends Elementary {
    val ab = List(a, b)
    def other(x : Item) = (x == a).?(b).|((x == b).?(a).|!("unknown other item for quest"))

    def img = Image("quest:" + id, styles.quest)
    def elem = name.styled(suit)
}

object Quest {
    def all = List(
        Quest(Fox, "Fundraising", "fundraising", Teapot, Coins),
        Quest(Fox, "Errand", "errand-fox", Teapot, Boots),
        Quest(Fox, "Logistic Help", "logistic-help-fox", Boots, Bag),
        Quest(Fox, "Repair a Shed", "repair-a-shed", Torch, Hammer),
        Quest(Fox, "Give a Speech", "give-a-speech-fox", Torch, Teapot),
        
        Quest(Rabbit, "Guard Duty", "guard-duty-rabbit", Torch, Sword),
        Quest(Rabbit, "Errand", "errand-rabbit", Teapot, Boots),
        Quest(Rabbit, "Give a Speech", "give-a-speech-rabbit", Torch, Teapot),
        Quest(Rabbit, "Fend off a Bear", "fend-off-a-bear-rabbit", Torch, Crossbow),
        Quest(Rabbit, "Expel Bandits", "expel-bandits-rabbit", Sword, Sword),

        Quest(Mouse, "Expel Bandits", "expel-bandits-mouse", Sword, Sword),
        Quest(Mouse, "Guard Duty", "guard-duty-mouse", Torch, Sword),
        Quest(Mouse, "Fend off a Bear", "fend-off-a-bear-mouse", Torch, Crossbow),
        Quest(Mouse, "Escort", "escort", Boots, Boots),
        Quest(Mouse, "Logistic Help", "logistic-help-mouse", Boots, Bag)
    )
}

class HeroPlayer(val game : Game, val faction : Hero) extends PlayerState {
    var character : Character = null
    
    override def abilities = Option(character)./~(_.special)

    var inv : List[ItemRef] = Nil

    var attitude = Map[Faction, Attitude]()
    var aid = Map[Faction, Int]()

    var quests = List[Suit]()

    def region = game.pieces.find(faction.pawn).key match {
        case r : Region => r
        case q => null
    }

    var fromRuins : List[Item] = Nil
    var wasted : List[Item] = Nil

    def ready(i : Item) = inv.ready.count(i)

    def available = inv.sort.ready./(_.item)

    def use(i : Item, m : String = "exhausted") {
        use($(i), m)
    }
    
    def use(ii : List[Item], m : String) {
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

    val dontCount = List(Teapot, Teapot, Teapot, Coins, Coins, Coins, Bag, Bag, Bag)./(_.ref(false, false))
    
    def swords = inv.notDamaged.count(Sword)

    def craft = region.clearing./(c => List.fill(ready(Hammer))(game.mapping(c))).|(Nil)
}

case object Slip extends Transport {
    override def allows(g : Game, f : Faction, o : Region, d : Region, l : List[Movable]) = l match {
        case List(p : Pawn) => true
        case _ => false
    }
}

case object ForestRoads extends Transport {
    override def allows(g : Game, f : Faction, o : Region, d : Region) = (o, d) @@ {
        case (o : Clearing, d : Clearing) => g.board.connected(o).has(d) && g.scorched.has(d).not
        case (o : Forest, d : Clearing) => g.fromForest(o).has(d)
        case (o : Clearing, d : Forest) => g.fromForest(d).has(o)
        case (o : Forest, d : Forest) => {
            val cc = g.fromForest(o).intersect(g.fromForest(d))
            cc.num > 1 || cc.intersect(g.board.coastal).any
        }
        case _ => false
    }
}

case object MoveBoots extends Transport {
    override def img = Boots.img

    override def allows(g : Game, f : Faction) = f @@ {
        case f : Hero => 
            import g._

            f.ready(Boots) >= 1
        case _ => false
    }
    
    override def allows(g : Game, f : Faction, o : Region, d : Region) = allows(g, f, o) && (f, d) @@ {
        case (f : Hero, d : Clearing) => 
            import g._

            factions./~(_.warf).%(f.attitude(_) == Hostile).%(_.at(d).warrior.num > 0).none
        case _ => false
    }
}

case object MoveDoubleBoots extends Transport {
    override def img = Boots.img ~ Boots.img

    override def allows(g : Game, f : Faction) = f @@ {
        case f : Hero => 
            import g._

            f.ready(Boots) >= 2
        case _ => false
    }
    
    override def allows(g : Game, f : Faction, o : Region, d : Region) = allows(g, f, o) && (f, d) @@ {
        case (f : Hero, d : Clearing) => 
            import g._

            factions./~(_.warf).%(f.attitude(_) == Hostile).%(_.at(d).warrior.any).any && traders.%(s => f.has(Peacekeepers(s))).%(_.at(d).warrior.any).none
        case _ => false
    }
}

case class AidedWith(f : Hero, e : Faction) extends Message {
    def elem(g : Game) = f.elem ~ " aided " ~ e.elem ~ " with"
}

case object AsAReward extends Message {
    def elem(g : Game) = Text(" as a reward")
}

case class WithItems(l : List[Item]) extends Message with GameModeElementary {
    def elem(g : Game) = " with " ~ l./(_.img).merge
    def elem(g : Game, mode : ConvertMode) = mode match {
        case ModeLog => " with " ~ l./(_.exhaust.elem).join(" ")
        case _ => elem(g)
    }
}

case object SlipTo extends Message {
    def elem(g : Game) = Text("Slip to")
}

object WithItems {
    def apply(i : Item) : WithItems = WithItems(List(i))
    def apply(a : Item, b : Item) : WithItems = WithItems(List(a, b))
}

case class WithItemsOutsourcing(l : List[Item], s : List[CraftAsset]) extends Message with GameModeElementary {
    def elem(g : Game) = ("with" + l.any.??(" ")) ~ l./(_.img).merge ~ s./(_ => "Outsourcing".hl)./(" " ~ _).merge
    def elem(g : Game, mode : ConvertMode) = mode match {
        case ModeLog => "with" ~ l./(" " ~ _.exhaust.elem).merge ~ s./(_ => "Outsourcing".hl)./(" " ~ _).merge
        case _ => elem(g)
    }
}

trait VBDaylightQuestion extends FactionAction {
    override def self : Hero
    def question(g : Game) = self.elem ~ " (" ~ "Daylight".styled(styles.phase) ~ ")" ~ Break ~ Div(g.vb(self).inv.sort.ready./(_.img).merge, styles.margined)
}                                                      

// VB
case class CharacterRandomAction(f : Hero, random : Character) extends RandomAction[Character]
case class CharacterChooseAction(f : Hero, l : List[Character], except : List[Character]) extends ForcedAction
case class CharacterSelectAction(f : Hero, c : Character) extends ForcedAction

case class ShuffleQuestsAction(shuffled : List[Quest], then : ForcedAction) extends ShuffledAction[Quest]

case class ReadyAction(self : Hero) extends ForcedAction
case class ReadyItemsAction(self : Hero, l : List[ItemRef], then : ForcedAction) extends ForcedAction

case class VagabondMoveMainAction(self : Hero, to1b : Boolean, to2b : Boolean) extends OptionAction("Move".styled(self), $(to1b.??(Boots.img), to2b.??(Boots.img ~ Boots.img)).join("/")) with VBDaylightQuestion with Soft

case class VagabondDealHitsAction(self : Hero, l : List[ItemRef], then : ForcedAction) extends ForcedAction
case class VagabondDealHitsAlliesAction(self : Hero, b : Battle, l : List[ItemRef], a : List[Figure], then : ForcedAction) extends ForcedAction


case class VagabondRepairMainAction(self : Hero) extends OptionAction("Repair".styled(self), Hammer.img) with VBDaylightQuestion with Soft
case class RepairItemAction(self : Hero, i : ItemRef) extends ForcedAction

case class VagabondBattleMainAction(self : Hero, c : Clearing, l : List[Faction]) extends OptionAction("Battle".styled(self), Sword.img) with VBDaylightQuestion with Soft

case class VagabondAidMainAction(self : Hero, s : Suit, l : List[Faction]) extends OptionAction("Aid".styled(self), dt.CardSuit(s) ~ "&".styled(styles.aidamp) ~ Image("item-any", styles.piece)) with VBDaylightQuestion with Soft
case class AidFactionAction(self : Hero, s : Suit, e : Faction) extends BaseAction("Aid")(e) with Soft
case class AidCardAction(self : Hero, s : Suit, e : Faction, d : DeckCard, available : Boolean) extends BaseAction("Aid", e)(d.img) with Soft with ViewCard
case class AidExhaustAction(self : Hero, e : Faction, d : DeckCard, i : Item) extends ForcedAction
case class AidTradeAction(self : Hero, i : Item, e : Faction) extends ForcedAction
case class AidTakeItemAction(self : Hero, i : Item, e : Faction, n : ItemRef) extends BaseAction("Take in trade from", e)(n, n.img)
case class AidIgnoreItemAction(self : Hero, i : Item, f : Faction) extends BaseAction(None)("Don't take an item".styled(styles.hit))
case class AidDoneAction(self : Hero, e : Faction) extends ForcedAction


case class VagabondQuestMainAction(self : Hero, c : Clearing, l : List[Quest]) extends OptionAction("Quests".styled(self), l.any.?(l./(q => q.a.img ~ q.b.img).join(" | ")).|(Image("item-any", styles.piece).repeat(2).merge)) with VBDaylightQuestion with Soft
case class QuestAction(self : Hero, c : Clearing, q : Quest, inv : List[ItemRef], vp : Int) extends BaseAction("Complete", "Quests".styled(self), "in", c)(q.img, Break, inv.sort.%(_.item == q.a).take(1).single./(_.imgD).|(q.a.imgEmptyD) ~ " " ~ inv.sort.%(_.item == q.b).drop((q.a == q.b).??(1)).take(1).single./(_.imgD).|(q.b.imgEmptyD) ~ Div(dt.CardBack.repeat(2) ~ " | " ~ vp.vp, styles.margined)) with ViewQuest with Soft
case class QuestItemsAction(self : Hero, q : Quest, l : List[ItemRef]) extends ForcedAction
case class QuestCompleteAction(self : Hero, q : Quest, l : List[ItemRef]) extends ForcedAction
case class QuestInfoAction(self : Hero, q : Quest, inv : List[ItemRef]) extends BaseInfo(Break ~ "Quests")(OnClick(q, Div(q.img, xstyles.pointer)), inv.sort.%(_.item == q.a).take(1).single./(_.img).|(q.a.imgEmpty) ~ " " ~ inv.sort.%(_.item == q.b).drop((q.a == q.b).??(1)).take(1).single./(_.img).|(q.b.imgEmpty)) with Info with ViewQuest with OnClickInfo { def param = q }
case class QuestRewardCardsAction(self : Hero, q : Quest, n : Int) extends BaseAction(q, "reward")("Draw", n.hl, "cards")
case class QuestRewardVPAction(self : Hero, q : Quest, n : Int) extends BaseAction(q, "reward")("Score", n.vp)
case class NewQuestAction(self : Hero, q : Quest) extends ForcedAction

case class VagabondExploreMainAction(self : Hero, c : Clearing) extends OptionAction("Explore".styled(self), Torch.img) with VBDaylightQuestion
case class TakeRuinItemAction(self : Hero, c : Clearing, i : Item, available : Boolean) extends BaseAction(self, "explores ruins and finds an item")("Take", i.img, i)
case class IgnoreRuinItemAction(self : Hero, c : Clearing) extends BaseAction(None)("Don't take an item".styled(styles.hit))

case class VagabondStealMainAction(self : Hero, l : List[Faction]) extends OptionAction(Steal.of(self), Torch.img) with VBDaylightQuestion with Soft
case class StealAction(self : Hero, e : Faction) extends BaseAction(Steal.of(self), "from")(e)

case class VagabondDayLaborMainAction(self : Hero, s : Suit) extends OptionAction(DayLabor.of(self), Torch.img) with VBDaylightQuestion with Soft
case class DayLaborAction(self : Hero, d : DeckCard) extends BaseAction(DayLabor.of(self))(d.img) with ViewCard

case class VagabondHideoutMainAction(self : Hero) extends OptionAction(Hideout.of(self), Torch.img) with VBDaylightQuestion with Soft
case class HideoutAction(self : Hero, l : List[ItemRef]) extends ForcedAction

case class VagabondScorchedEarthMainAction(self : Hero, c : Clearing) extends OptionAction(ScorchedEarth.of(self), Torch.damage.img) with VBDaylightQuestion with Soft
case class ScorchedEarthAction(self : Hero, c : Clearing) extends BaseAction("Burn", c, "to the ground")(ScorchedEarth.name.styled(styles.hit))
case class ScorchedEarthDoneAction(self : Hero, c : Clearing) extends ForcedAction

case class VagabondGlideMainAction(self : Hero, l : List[Clearing]) extends OptionAction(Glide.of(self), Torch.img) with VBDaylightQuestion with Soft
case class GlideAction(self : Hero, c : Clearing) extends BaseAction(Glide.of(self), "to")(c)

case class VagabondInstigateMainAction(self : Hero, c : Clearing, l : List[Faction]) extends OptionAction(Instigate.of(self), Torch.img) with VBDaylightQuestion with Soft
case class InstigateAction(self : Hero, c : Clearing, a : Faction, d : Faction) extends BaseAction(Instigate.of(self), "a fight in", c)(a, "battles", d)


case class VagabondImproviseMainAction(self : Hero, c : Clearing) extends OptionAction(Improvise.of(self)) with VBDaylightQuestion with Soft
case class ImproviseQuestAction(self : Hero, c : Clearing, q : Quest, inv : List[ItemRef], vp : Int) extends BaseAction("Improvise", "Quests".styled(self), "in", c)(q.img, Break, inv.sort.%(_.item == q.a).take(1).single./(_.imgD).|(q.a.imgEmptyD) ~ " " ~ inv.sort.%(_.item == q.b).drop((q.a == q.b).??(1)).take(1).single./(_.imgD).|(q.b.imgEmptyD) ~ Div(dt.CardBack.repeat(2) ~ " | " ~ vp.vp, styles.margined)) with ViewQuest with Soft
case class ImproviseAction(self : Hero, q : Quest, l : List[ItemRef]) extends ForcedAction

case class VagabondStrikeMainAction(self : Hero, c : Clearing, l : List[Faction]) extends OptionAction("Strike".styled(self), Crossbow.img) with VBDaylightQuestion with Soft
case class StrikeAction(self : Hero, c : Clearing, e : Faction, p : Piece) extends BaseAction("Strike".styled(self))(p.img(e))

case class VagabondCraftMainAction(self : Hero, n : Int) extends OptionAction("Craft".styled(self), Hammer.img.repeat(max(n, 1)).merge) with VBDaylightQuestion with Soft

case class ExhaustItemAction(f : Hero, i : Item, then : ForcedAction) extends ForcedAction
case class ItemsLimitAction(f : Hero, n : Int) extends ForcedAction
case class DiscardItemsAction(f : Hero, capacity : Int, then : ForcedAction) extends ForcedAction
case class DiscardItemsListAction(f : Hero, l : List[ItemRef], then : ForcedAction) extends ForcedAction


case class SelectHeroItemsAction(f : Hero, m : Elem, extra : List[UserAction], inner : Boolean = false)(r : ObjectSetRule[QuasiItem] => ObjectSetRule[QuasiItem])(x : ItemRef => RealItem)(then : List[ItemRef] => ForcedAction, a : List[Figure] = Nil, athen : (List[ItemRef], List[Figure]) => ForcedAction = null, adone : (List[ItemRef], List[Figure]) => Elem = null) extends ForcedAction with Soft with SelfPerform {
    def perform(g : Game) = {
        import g._
        
        val track = f.inv.intersect(Item.track).but(Bag.pristine).sort./(inner.?(ToShow).|(x))
        val satchel = f.inv.diff(Item.track).sort./(x)
        val columns = 3 + f.inv.intersect(Item.track).count(Bag)
        val (r1, r2) = satchel.splitAt((satchel.num + 1) / 2)
        val row1 = r1 ++ ItemPlaceholder.repeat(columns - r1.num)
        val row2 = r2 ++ ItemPlaceholder.repeat(columns - r2.num)
        val bags = (columns > 3).??(ItemEmptySpace.repeat(3) ++ Bag.pristine.repeat(columns - 3)./(inner.?(ToShow).|(x)))
        val allies = a./(FigureRemove)
 
        XXSelectObjectsAction(f, track ++ row1 ++ row2 ++ bags ++ allies)
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

           
object HeroExpansion extends Expansion {
    val characters = $[Character](Thief, Tinker, Ranger, Arbiter, Scoundrel, Vagrant, Adventurer, Ronin, Harrier)

    override def extraMoveFrom(game : Game, faction : Faction) = faction match {
        case f : Hero => List(game.vb(f).region)
        case _ => Nil
    }

    def perform(game : Game, action : Action) : Continue = {
        import game._

        implicit val a = action

        action match {
            // SETUP
            case CreatePlayerAction(f : Hero) =>
                pstates += f -> new HeroPlayer(game, f)
                FactionInitAction(f)

            case FactionSetupAction(f : Hero) =>
                val x = factions./~(_.hero).but(f).%(pstates.contains)./(_.character)
                
                if (hrf.HRF.flag("fastsetup"))
                    Force(CharacterSelectAction(f, Testabond))
                else
                if (options.has(SelectCharacter))
                    CharacterChooseAction(f, characters, x)
                else
                if (chars.contains(f))
                    Force(CharacterSelectAction(f, chars(f)))
                else
                    Random(characters.diff(x), CharacterRandomAction(f, _))

            case CharacterRandomAction(f, c) =>
                CharacterChooseAction(f, $(c), Nil)

            case CharacterChooseAction(f, l, x) =>
                implicit def convert(c : Character) = {
                    desc(Image(c.title)(styles.chartitle), Break, Image(c.img)(styles.quest)((c == Ranger).?(styles.ranger))((c == Adventurer).?(styles.adventurer)).div(styles.charblock) ~ c.starting./(_.img).merge, Break, c.special)
                }
                
                YYSelectObjectsAction(f, l)
                    .withGroup((l.num > 1).?("Choose character").|("Random character"))
                    .withSplit($(3, 3, 3))
                    .withRuleExcept(x)
                    .withThen(CharacterSelectAction(f, _))("Play as " ~ _.name.hl)("~~~")

            case CharacterSelectAction(f, c) =>
                f.character = c
                f.inv = c.starting./(_.ref(false, false)) 
                
                f.log("started as", c.name.styled(f), "with", c.starting)

                f.attitude = setup.%!(_.hero.any)./(_ -> Indifferent).toMap
                f.aid = setup.%!(_.hero.any)./(_ -> 0).toMap

                if (hrf.HRF.flag("fastsetup"))
                    f.attitude = setup.%!(_.hero.any)./(_ -> Allied).toMap

                StartingRegionAction(f)

            case StartingRegionAction(f) =>
                if (hrf.HRF.flag("fastsetup"))
                    Force(StartingForestAction(f, board.forests.%(_.label != null)(0)))
                else
                    Ask(f, board.forests./(StartingForestAction(f, _)))
                
            case StartingForestAction(f, r) =>
                f.pool.one(Vagabond) --> r

                f.log("started in", board.forestName(r, game))
                
                if (quests == Nil)
                    Shuffle[Quest](Quest.all, ShuffleQuestsAction(_, SetupNextAction))
                else
                    SetupNextAction
                    
            case ShuffleQuestsAction(l, then) =>
                quests = l

                then

            // HELPER
            case MoveToAction(self, f : Hero, tt, m, from : Region, to : Region, then) =>
                val mv = f.at(from)./~(_.piece.movable)
                val cm = 1.to(mv.num)./~(n => mv.combinations(n))
                val pp = tt./~(t => cm.%(l => t.forall(_.allows(game, f, from, to, l)))./(l => l.sortBy(m => t./(_.sortBy(m)).sum))./(Party(t, _))).sortBy(p => p.transport./(t => t.order(p.movable)).sum)
                
                val l = pp./(p => MoveListAction(f, p.transport, m, from, to, p.movable, then))
                
                val aa = tt.%(_.has(Slip).not).any.??(factions./~(_.warf).%(f.attitude(_) == Allied).%(_.at(from).warrior.%(_.piece.pawn.none).any))
                
                val al = aa./~{ a =>
                    val amv = a.at(from)./~(_.piece.movable).%(_.pawn.none)
                    val acm = 1.to(amv.num)./~(n => amv.combinations(n))
                    val app = a.transports./~(t => acm.%(l => t.forall(_.allows(game, a, from, to, l)))./(l => l.sortBy(m => t./(_.sortBy(m)).sum))./(Party(t, _))).sortBy(p => p.transport./(t => t.order(p.movable)).sum)

                    pp./~(p => app./~(ap => p.transport.forall(_.allows(game, f, from, to, p.movable ++ ap.movable)).?(MoveListAlliedAction(f, p.transport ++ ap.transport, m, from, to, p.movable, a, ap.movable, then))))
                }
                
                val ll = l ++ al

                Ask(self, ll ++ (ll.num != 1).?(CancelAction))
                
            case MoveListAlliedAction(f : Hero, t, m, from, to, List(p : Pawn), a, w, then) if t.has(MoveBoots) =>
                f.exhaust(Boots)

                Force(MoveListAlliedAction(f, t.but(MoveBoots), m, from, to, List(p : Pawn), a, w, then))

            case MoveListAlliedAction(f : Hero, t, m, from, to, List(p : Pawn), a, w, then) if t.has(MoveDoubleBoots) =>
                f.exhaust(Boots)
                f.exhaust(Boots)
                
                Force(MoveListAlliedAction(f, t.but(MoveDoubleBoots), m, from, to, List(p : Pawn), a, w, then))

            case MoveListAlliedAction(f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), a, w, then) if t.has(Ferry) =>
                f.log("moved on", Ferry.elem, "with", w./(_.of(a)).comma, "from", from, "to", to, m)
                
                Force(MoveListAlliedAction(f, Nil, NoMessage, from, to, $(p), a, w, FerryAction(f, from, to, then)))
                
            case MoveListAlliedAction(f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), a, w, then) if t.any =>
                f.log("moved with", w./(_.of(a)).comma, "from", from, "to", to, m)

                Force(MoveListAlliedAction(f, Nil, NoMessage, from, to, $(p), a, w, then))

            case MoveListAlliedAction(f : Hero, Nil, m, from : Clearing, to : Clearing, List(p : Pawn), a, w, then) =>
                highlights :+= MoveHighlight(from, to)
                
                f.pawn --> to
                w.foreach(p => a.at(from).one(p) --> to)
                
                MoveWarriorsCompleteAction(f, from, to, then)
 
            case MoveListAction(f : Hero, t, m, from, to, List(p : Pawn), then) if t.has(MoveBoots) =>
                f.exhaust(Boots)

                Force(MoveListAction(f : Hero, t.but(MoveBoots), m, from, to, List(p : Pawn), then))

            case MoveListAction(f : Hero, t, m, from, to, List(p : Pawn), then) if t.has(MoveDoubleBoots) =>
                f.exhaust(Boots)
                f.exhaust(Boots)
                
                Force(MoveListAction(f : Hero, t.but(MoveDoubleBoots), m, from, to, List(p : Pawn), then))

            case MoveListAction(f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), then) if t.has(Ferry) && t.has(Slip) =>
                f.log("slipped on", Ferry.elem, "from", from, "to", to)
                
                Force(MoveListAction(f, Nil, NoMessage, from, to, $(p), FerryAction(f, from, to, then)))
                
            case MoveListAction(f : Hero, t, m, from : Clearing, to : Clearing, List(p : Pawn), then) if t.has(Ferry) =>
                f.log("moved on", Ferry.elem, "from", from, "to", to, m)
                
                Force(MoveListAction(f, Nil, NoMessage, from, to, $(p), FerryAction(f, from, to, then)))
                
            case MoveListAction(f : Hero, t, m, from : Forest, to : Clearing, List(p : Pawn), then) if t.has(Slip) =>
                f.log("slipped to", to, "from forest")

                Force(MoveListAction(f, Nil, NoMessage, from, to, $(p), then))
                
            case MoveListAction(f : Hero, t, m, from : Region, to : Region, List(p : Pawn), then) if t.has(Slip) =>
                f.log("slipped from", from, "to", to)
                
                Force(MoveListAction(f, Nil, NoMessage, from, to, $(p), then))
                
            case MoveListAction(f : Hero, t, m, from : Region, to : Region, List(p : Pawn), then) if t.any =>
                f.log("moved", "from", from, "to", to, m)
                
                Force(MoveListAction(f, Nil, NoMessage, from, to, $(p), then))
                
            case MoveListAction(f : Hero, Nil, m, from : Region, to : Region, List(p : Pawn), then) =>
                highlights :+= MoveHighlight(from, to)

                f.pawn --> to
                
                MoveFinishedAction(f, from, to, then)

                
            case AcquireItemsAction(f : Hero, then) =>
                f.inv ++= f.forTrade./(_.pristine)
                f.forTrade = Nil

                then
                
            case CraftAssignAction(f : Hero, d, all, used, m, then : CraftPerformAction) =>
                var u = used

                val x = f.extracraft.intersect(u)

                if (x.any) {
                    f.extracraft = f.extracraft.diff(x)
                    u = u.diff(x)
                }
                
                u.foreach(_ => f.exhaust(Hammer))

                then.copy(m = WithItemsOutsourcing(u./(_ => Hammer), x))
                
            case ForcedRemoveEffectAction(e : Hero, c, f, p : Warrior, then) =>
                if (e.attitude(f) != Hostile) {
                    e.attitude += f -> Hostile
                    f.log("became", Hostile, "to", e)
                }
                then
                
            case BattleStartAction(f, a, m, c, o, i, ExhaustItemAction(hero, Sword, then)) =>
                hero.exhaust(Sword)

                Force(BattleStartAction(f, a, m, c, o, i, then))
                
            case BattleAssignHitsAction(f : Hero, b, n, then) =>
                val ambush = b.ally.any && then @@ {
                    case BattleAmbushHitsAction(bb, ah) => true
                    case _ => false
                }

                val l = f.inv.notDamaged
                val a = (b.attacker == f).??(b.ally./?(a => a.at(b.clearing).warrior))
                if (l.none && a.none)
                    then
                else
                if (l.num + a.num <= n)
                    VagabondDealHitsAlliesAction(f, b, l, a, then)
                else
                    SelectHeroItemsAction(f, "Damage " ~ n.hl ~ " items" ~ a.any.??(" or remove warriors"), $())(_.num(n))(ToDamage.apply)(null, a, (l, u) => VagabondDealHitsAlliesAction(f, b, l, u, then), (l, u) => "Done".hl ~ (l.num - u.num + b.balance < 0).?(" (" ~ b.ally.get.elem ~ " " ~ ambush.?("may").|("will") ~ " become " ~ Hostile.elem ~ ")"))
                    
            case VagabondDealHitsAlliesAction(f, b, l, a, BattleAmbushHitsAction(bb, ah)) =>
                VagabondDealHitsAction(f, l, BattleDealHitsAction(f, b, a, BattleAmbushHitsAction(bb.copy(balance = bb.balance + l.num - a.num), ah)))

            case VagabondDealHitsAlliesAction(f, b, l, a, BattleResolveHitsAction(bb, ah, dh)) =>
                VagabondDealHitsAction(f, l, BattleDealHitsAction(f, b, a, BattleResolveHitsAction(bb.copy(balance = bb.balance + l.num - a.num), ah, dh)))
                
            case VagabondDealHitsAlliesAction(f, b, l, a, then) =>
                VagabondDealHitsAction(f, l, BattleDealHitsAction(f, b, a, then))
                
            case VagabondDealHitsAction(f, l, then) =>
                val d = l./(_.damage)
                f.inv = f.inv.diff(l) ++ d

                f.log("damaged", d)
                
                then
            
            case BattlePostHitOutAction(b, _, _, _, ForcedRemoveAction(e : Hero, c, f, p, x, m, then)) if e.attitude(f) == Hostile && (b.attacker == e || b.instigator == Some(e)) =>
                val bonus = p.warrior.any || options.has(WarriorsOnlyInfamy).not

                if (bonus)
                    ForcedRemoveAction(e : Hero, c, f, p, x + 1, SomeHostile(m), then)
                else
                    ForcedRemoveAction(e : Hero, c, f, p, x, m, then)
            
            case BattleCleanupAttackerAction(b, f : Hero) =>
                if (b.balance < 0) {
                    f.attitude += b.ally.get -> Hostile
                    log(b.ally.get, "became", Hostile, "to", f)
                }

                BattleForcedRemoveAction(b)

            // TURN
            case BirdsongNAction(40, f : Hero) =>
                val e = f.inv.exhausted.num
                val n = 3 + min(3, f.ready(Teapot)) * 2
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
                val o = f.region

                val (t, l) = movePlans(f, $(o), $($(Slip)) ** transports.but(Roads).appended(ForestRoads)./($(_)))(o)
                
                Ask(f, l./(d => {
                    val tt = t.%(_.forall(_.allows(game, f, o, d)))
                    MoveToAction(f, f, tt, SlipTo, o, d, Next).x(tt.none, "forbidden")
                }))(Next.as("Stay in", o))(f.birdsong)

            case DaylightNAction(50, f : Hero) =>
                var actions : List[UserAction] = Nil
                    
                actions :+= movePlans(f, $(f.region), transports./($(_)) ** f.transports ** $($(MoveBoots), $(MoveDoubleBoots))).get(f.region) @@ {
                    case Some((t, l)) => VagabondMoveMainAction(f, t.%(_.has(MoveBoots)).any, t.%(_.has(MoveDoubleBoots)).any)
                    case None => VagabondMoveMainAction(f, true, false).x(snaredFor(f)(f.region), "can't move").x(f.ready(Boots) == 0, "no boots").x(f.ready(Boots) == 1, "not enough boots")
                }

                if (f.region.clearing.any && scorched.has(f.region.clearing.get).not) {
                    val c = f.region.clearing.get

                    val ee = factions.but(f).%(_.at(c).any)

                    actions :+= VagabondBattleMainAction(f, c, ee).x(f.ready(Sword) == 0, "no sword").x(ee.none, "no enemy")
                    
                    actions :+= VagabondExploreMainAction(f, c).x(ruins.get(c).none, "no ruins").x(f.ready(Torch) == 0, "no torch")
                    
                    if (f.character.special.has(Steal))
                        actions :+= VagabondStealMainAction(f, ee.%(_.hand.any)).x(ee.none, "no target").x(f.ready(Torch) == 0, "no torch").x(ee.%(_.hand.any).none, "no cards")
                        
                    if (f.character.special.has(DayLabor))
                        actions :+= VagabondDayLaborMainAction(f, c.suit).x(pile.%(_.suit.m(c.suit)).none, "no cards in discard").x(f.ready(Torch) == 0, "no torch")
                        
                    if (f.character.special.has(Hideout))
                        actions :+= VagabondHideoutMainAction(f).x(f.inv.damaged.none, "no damaged items").x(f.ready(Torch) == 0, "no torch")

                    if (f.character.special.has(ScorchedEarth))
                        actions :+= VagabondScorchedEarthMainAction(f, c).x(f.ready(Torch) == 0, "no torch").x(!canPlace(f)(c), "forbidden")

                    if (f.character.special.has(Improvise)) {
                        val qq = quests.take(3).%(_.suit == c.suit)
                        val inv = f.inv.ready./(_.item)
                        val ss = (inv.num > 1).??(qq.%(q => inv.has(q.a) || inv.has(q.b)).%(q => inv.diff(q.ab).any))
                        actions :+= VagabondImproviseMainAction(f, c).x(!f.can(Improvise), "once per turn").x(qq.none, "no matching quests").x(inv.num < 2, "not enough items").x(ss.none, "not a single matching item")
                    }
                
                    if (f.character.special.has(Instigate)) {
                        val l = ee.:+(f).%(attackSelf(_)(c).any)
                        actions :+= VagabondInstigateMainAction(f, c, l).x(f.ready(Torch) == 0, "no torch").x(ee.none, "empty").x(l.none, "no attacker")
                    }

                    actions :+= VagabondAidMainAction(f, c.suit, ee.%!(_.hero.any)).x(ee.%!(_.hero.any).none, "no target").x(f.inv.ready.none, "no items").x(f.hand.%(_.suit.m(c.suit)).none, "no matching cards")
                
                    val qq0 = quests.take(3).%(q => q.suit == c.suit)
                    val qq = qq0.%(q => q.ab.diff(f.inv.ready./(_.item)).none)
                
                    actions :+= VagabondQuestMainAction(f, c, qq).x(qq0.none, "wrong suit").x(qq.none, "no items")
                
                    actions :+= VagabondStrikeMainAction(f, c, ee).x(f.ready(Crossbow) == 0, "no crossbow").x(ee.none, "no target")
                
                    actions :+= VagabondCraftMainAction(f, f.ready(Hammer)).x(f.ready(Hammer) == 0, "no hammer").x(f.hand.none, "no cards").x(f.hand.%(craftable(f)).none, "nothing craftable")
                }

                if (f.character.special.has(Glide))
                    actions :+= VagabondGlideMainAction(f, clearings.but(f.region.clearing)).x(f.ready(Torch) == 0, "no torch")
                
                actions :+= VagabondRepairMainAction(f).x(f.inv.damaged.none).x(f.ready(Hammer) == 0, "no hammer")
                                     
                Ask(f)(actions)(Next.as("End Turn"))(f.daylight)
            
            case ExhaustItemAction(f, i, then) =>
                f.exhaust(i)
                then

            case VagabondCraftMainAction(f, _) =>
                Force(CraftMenuAction(f))

            case VagabondMoveMainAction(f, l1b, l2b) =>
                MoveInitAction(f, transports./($(_)) ** f.transports ** $($(MoveBoots), $(MoveDoubleBoots)), NoMessage, $(f.region), $(f.region), $(), Repeat)

            case VagabondBattleMainAction(f, c, l) =>
                BattleInitAction(f, WithItems(Sword), $(c), $(CancelAction), ExhaustItemAction(f, Sword, Repeat))
                
            case VagabondExploreMainAction(f, c) =>
                highlights :+= PlaceHighlight($(c))

                f.exhaust(Torch)
                f.logtemp("explored", c, "ruins", "with", Torch.exhaust.elem)
                val aa = ruins(c).items./(i => TakeRuinItemAction(f, c, i, 1>0).x(f.fromRuins.contains(i)))
                Ask(f, aa :+ IgnoreRuinItemAction(f, c)).needOk
                
            case TakeRuinItemAction(f, c, i, true) =>
                val rest = ruins(c).items :- i

                if (rest.any)
                    ruins += c -> Ruins(rest)
                else
                    ruins -= c
                    
                f.inv :+= i.ref(false, false)
                f.fromRuins :+= i

                f.nscore(1)("exploring ruins")(f, "explored", c, "ruins", "with", Torch.exhaust.elem ~ ", found", i, "and", ScoredVP)

                Repeat

            case IgnoreRuinItemAction(f, c) =>
                f.log("explored", c, "ruins", "with", Torch.exhaust.elem, "but took nothing")

                Repeat

            case VagabondStealMainAction(f, l) =>
                Ask(f, l./(StealAction(f, _)).cancel)
                
            case StealAction(f, e) =>
                f.use(Torch)
                
                StealCardAction(f, e, Repeat)
                
            case VagabondDayLaborMainAction(f, s) =>
                Ask(f, pile./(d => DayLaborAction(f, d).x(d.suit.m(s).not)).cancel)

            case DayLaborAction(f, d) =>
                f.use(Torch)

                pile --> d --> f.hand
                
                f.log("took", d, "from the dicard pile")

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

            case VagabondScorchedEarthMainAction(f, c) =>
                Ask(f, List(ScorchedEarthAction(f, c)).cancel)
                
            case ScorchedEarthAction(f, c) =>
                val i = Torch.ref(false, false)
                f.inv :-= i
                f.wasted :+= i.item

                f.log("dropped", i, "and scorched", c)
                
                scorched :+= c
                ruins -= c

                NukeAction(f, Nil, List(c), TotalAnnihilation, ScorchedEarthDoneAction(f, c))

            case ScorchedEarthDoneAction(f, c) =>
                val fire = pieces.another[Figure]("fire", 0.to(11)./(n => Figure(null, ScorchedEarthMarker(n % 6 + 1), n)))
                
                fire.get --> c

                Repeat
                
            case VagabondGlideMainAction(f, l) =>
                Ask(f, l./(GlideAction(f, _)).cancel)
                
            case GlideAction(f, to) =>
                f.use(Torch)
                
                f.pawn --> to
                
                f.log("glided to", to)
                
                Repeat

            case VagabondInstigateMainAction(f, c, l) =>
                Ask(f, l./~(a => attack(a)(c)./(d => InstigateAction(f, c, a, d))).cancel)
                
            case InstigateAction(f, c, a, d) =>
                f.use(Torch)
                
                f.log("tricked", a, "into attacking", d)
                
                Force(BattleStartAction(a, a, NoMessage, c, d, Some(f), Repeat))
                
            case VagabondImproviseMainAction(f, c) =>
                val qq = quests.take(3).%(_.suit == c.suit)
                val ii = f.inv.ready.sort./(_.item)

                val iq = qq.%(q => q.ab.distinct./~(x => ii.has(x).??(ii.diff(q.ab).but(q.other(x)).distinct)).any)
                
                Ask(f, quests.take(3)./(q => ImproviseQuestAction(f, c, q, f.inv, (f.quests.count(c.suit) + 1)).x(iq.has(q).not)).cancel)

            case ImproviseQuestAction(f, _, q, _, _) =>
                def impr(l : List[Item]) = (l.num < 2) || (l.intersect(q.ab).any && l.diff(q.ab).any)
            
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
                Ask(f, ff./(AidFactionAction(f, s, _)).cancel)
                
            case AidFactionAction(f, s, e) =>
                Ask(f, f.hand./(d => AidCardAction(f, s, e, d, 1>0).x(d.suit.m(s).not)).cancel)

            case AidCardAction(f, s, e, d, true) =>
                SelectHeroItemsAction(f, "Exhaust an item to aid " ~ e.elem ~ " with " ~ d.elem, $(CancelAction))(_.num(1))(ToExhaust(_))(l => AidExhaustAction(f, e, d, l(0).item))
                
            case AidExhaustAction(f, e, d, i) =>
                f.exhaust(i)

                f.hand --> d --> e.hand
                
                f.logtemp("aided", e, "with", i.exhaust)

                Notify($(e), $(ViewCardInfoAction(f, AidedWith(f, e).elem(game), d)), AidTradeAction(f, i, e))
            
            case AidTradeAction(f, i, e) =>
                Ask(f, e.forTrade./(AidTakeItemAction(f, i, e, _)) :+ AidIgnoreItemAction(f, i, e))
                
            case AidTakeItemAction(f, i, e, n) =>
                f.inv :+= n.pristine
                e.forTrade :-= n

                f.log("aided", e, "with", i.exhaust, "and", "took", n, "in trade")

                AidDoneAction(f, e)
                
            case AidIgnoreItemAction(f, i, e) =>
                f.log("aided", e, "with", i.exhaust, e.forTrade.any.??("and took nothing"))

                AidDoneAction(f, e)

            case AidDoneAction(f, e) =>
                var a = f.aid(e) + 1
                f.aid += e -> 0
                
                (f.attitude(e), a) match {
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
                        f.aid += e -> a
                }

                Repeat
                

            case VagabondQuestMainAction(f, c, _) =>
                Ask(f, quests.take(3)./(q => QuestAction(f, c, q, f.inv, f.quests.count(q.suit) + 1).x(q.suit != c.suit).x(q.ab.diff(f.inv.ready./(_.item)).any)).cancel)
                
            case QuestAction(f, c, q, _, _) =>
                SelectHeroItemsAction(f, "Exhaust " ~ q.a.img ~ q.b.img ~ " to complete " ~ q.elem, $(CancelAction))(_.num(2).all(_./~(_.real)./(_.item).diff(q.ab).none))(ToExhaust(_))(l => QuestItemsAction(f, q, l))

            case QuestItemsAction(f, q, l) =>
                l.foreach { i =>
                    f.inv :-= i.pristine
                    f.inv :+= i.exhaust
                }
                QuestCompleteAction(f, q, l./(_.exhaust))

            case QuestCompleteAction(f, q, l) =>
                highlights :+= PlaceHighlight(List(f.region.clearing.get))

                f.quests :+= q.suit
                f.log("completed quest", q, "with", l)
                Ask(f, (QuestRewardCardsAction(f, q, 2) :: QuestRewardVPAction(f, q, f.quests.count(q.suit))))

            case QuestRewardCardsAction(f, q, n) =>
                DrawCardsAction(f, n, AsAReward, AddCardsAction(f, NewQuestAction(f, q)))
 
            case QuestRewardVPAction(f, q, n) =>
                f.oscore(n)("as a quest reward")
                NewQuestAction(f, q)

            case NewQuestAction(f, q) =>
                quests :-= q
                if (quests.num >= 3)
                    log("New quest available", quests(2))
                Repeat

            case VagabondStrikeMainAction(f, c, ee) =>
                val aa = ee./~{ e => 
                    val l = e.at(c)
                    l.warrior.some.|(l)./(_.piece)./(StrikeAction(f, c, e, _))
                }
                Ask(f, aa.cancel)
                
            case StrikeAction(f, c, e, p) =>
                f.use(Crossbow, "shot with")
                
                if (e.hero.any) {
                    BattleAssignHitsAction(e, Battle(c, f, None, e, None, false, None, None, 0, DummyAction), 1, Repeat)
                }
                else {
                    e.at(c).one(p) --> e.limbo
                    
                    log(p.of(e), "was removed from", c)
                    
                    ForcedRemoveAction(f, c, e, p, p.scoring.any.??(1), Striking, ForcedRemoveCleanupAction(e, Repeat))
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
            
                if (f.region.isInstanceOf[Forest]) {
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

            case FactionCleanUpAction(f : Hero) =>
                EndPlayerTurnAction(f)
                
            case _ => UnknownContinue
        }
    }
}

