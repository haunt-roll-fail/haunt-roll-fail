package arcs
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

import hrf.tracker4._
import hrf.tracker4.implicits._
import hrf.elem._

import arcs.elem._


trait LeaderEffect extends Effect with NamedToString with Elementary {
    override def elem = name.styled(styles.title).hl
}


case object Beloved extends LeaderEffect
case object Just extends LeaderEffect

case object Attuned extends LeaderEffect
case object Cryptic extends LeaderEffect

case object Insatiable extends LeaderEffect
case object Lavish extends LeaderEffect

case object Ambitious extends LeaderEffect
case object Callow extends LeaderEffect

case object Committed extends LeaderEffect
case object Disorganized extends LeaderEffect

case object Tactical extends LeaderEffect
case object Violent extends LeaderEffect

case object Charismatic extends LeaderEffect
case object Generous extends LeaderEffect

case object Bold extends LeaderEffect
case object Paranoid extends LeaderEffect

case object Learned extends LeaderEffect
case object Academic extends LeaderEffect

case object Ruthless extends LeaderEffect
case object Hated extends LeaderEffect

case object Tricky extends LeaderEffect
case object Wary extends LeaderEffect

case object Connected extends LeaderEffect
case object Influential extends LeaderEffect
case object Proud extends LeaderEffect

case object Decentralized extends LeaderEffect
case object Inspiring extends LeaderEffect
case object Principled extends LeaderEffect

case object Mythic extends LeaderEffect
case object Ancient extends LeaderEffect

case object Firebrand extends LeaderEffect
case object Irregular extends LeaderEffect

case object Resilient extends LeaderEffect
case object Greedy extends LeaderEffect

case object AttackerResilient extends LeaderEffect {
    override def name = "Resilient"
}
case object DefenderResilient extends LeaderEffect {
    override def name = "Resilient"
}

abstract class Leader(val id : String, val name : String, val effects : $[Effect], val resources : $[Resource], val setupA : $[Piece], val setupB : $[Piece], val setupC : $[Piece]) extends Record with Elementary {
    def img = Image(id, styles.leaderCard)
    def elem = name.styled(styles.title).hl
}


case object Elder         extends Leader("leader01", "Elder",         $(Beloved, Just), $(Relic, Material), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object Mystic        extends Leader("leader02", "Mystic",        $(Attuned, Cryptic), $(Psionic, Relic), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object FuelDrinker   extends Leader("leader03", "Fuel-Drinker",  $(Insatiable, Lavish), $(Fuel, Fuel), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship)) { override val name = "Fuel Drinker" }
case object Upstart       extends Leader("leader04", "Upstart",       $(Ambitious, Callow), $(Psionic, Material), $(City, Ship, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object Rebel         extends Leader("leader05", "Rebel",         $(Committed, Disorganized), $(Material, Weapon), $(Starport, Ship, Ship, Ship, Ship), $(Ship, Ship, Ship, Ship), $(Ship, Ship))
case object Warrior       extends Leader("leader06", "Warrior",       $(Tactical, Violent), $(Weapon, Material), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object Feastbringer  extends Leader("leader07", "Feastbringer",  $(Charismatic, Generous), $(Relic, Material), $(City, Ship, Ship, Ship), $(City, Ship, Ship, Ship), $(Ship, Ship, Ship))
case object Demagogue     extends Leader("leader08", "Demagogue",     $(Bold, Paranoid), $(Psionic, Weapon), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object Archivist     extends Leader("leader09", "Archivist",     $(Learned, Academic), $(Relic, Relic), $(City, Ship, Ship, Ship), $(City, Ship, Ship, Ship), $(Ship, Ship))
case object Overseer      extends Leader("leader10", "Overseer",      $(Ruthless, Hated), $(Fuel, Material), $(Starport, Ship, Ship, Ship), $(City, Ship, Ship, Ship), $(Ship, Ship))
case object Corsair       extends Leader("leader11", "Corsair",       $(Tricky, Wary), $(Fuel, Weapon), $(Starport, Ship, Ship, Ship, Ship), $(Ship, Ship, Ship), $(Ship, Ship))
case object Noble         extends Leader("leader12", "Noble",         $(Connected, Influential, Proud), $(Psionic, Psionic), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship), $(Ship, Ship))
case object Anarchist     extends Leader("leader13", "Anarchist",     $(Decentralized, Inspiring, Principled), $(Relic, Weapon), $(Ship, Ship, Ship, Ship), $(Ship, Ship, Ship), $(Ship, Ship))
case object Shaper        extends Leader("leader14", "Shaper",        $(Mythic, Ancient), $(Relic, Material), $(City, Ship, Ship, Ship), $(Ship, Ship, Ship), $(Ship, Ship, Ship))
case object Agitator      extends Leader("leader15", "Agitator",      $(Firebrand, Irregular), $(Fuel, Material), $(City, Ship, Ship, Ship), $(Starport, Ship, Ship, Ship, Ship), $(Ship, Ship))
case object Quartermaster extends Leader("leader16", "Quartermaster", $(Resilient, Greedy), $(Fuel, Weapon), $(Starport, Ship, Ship, Ship, Ship), $(Ship, Ship, Ship), $(Ship, Ship))


object Leaders {
    def all = $(
        Elder         ,
        Mystic        ,
        FuelDrinker   ,
        Upstart       ,
        Rebel         ,
        Warrior       ,
        Feastbringer  ,
        Demagogue     ,
        Archivist     ,
        Overseer      ,
        Corsair       ,
        Noble         ,
        Anarchist     ,
        Shaper        ,
        Agitator      ,
        Quartermaster ,
    )

    def preset1 = $(Elder, Mystic, FuelDrinker, Rebel, Demagogue)
    def preset2 = $(Agitator, Feastbringer, Warrior, Noble, Upstart)
    def preset3 = $(Overseer, Corsair, Anarchist, Shaper, Quartermaster)
    def preset4 = $(Archivist)
}


case class LeadersLoresShuffledAction(shuffled1 : $[Leader], shuffled2 : $[Lore]) extends Shuffled2Action[Leader, Lore]
case class DraftNextAction(f : Faction) extends ForcedAction
case class AssignLeaderAction(f : Faction, l : Leader, then : ForcedAction) extends ForcedAction
case class AssignLoreAction(f : Faction, l : Lore, then : ForcedAction) extends ForcedAction
case object LeadersFactionsSetupAction extends ForcedAction


case class BelovedAction(self : Faction, then : ForcedAction) extends ForcedAction

case class LearnedAction(self : Faction, l : $[Lore], then : ForcedAction) extends ForcedAction

case class ConnectedAction(self : Faction, then : ForcedAction) extends ForcedAction

case class MythicAction(self : Faction, s : System, r : Resource, k : Int, then : ForcedAction) extends ForcedAction

case class BoldMainAction(self : Faction, influenced : $[CourtCard], then : ForcedAction) extends ForcedAction with Soft
case class GiveGuildCardAction(self : Faction, e : Faction, c : CourtCard, then : ForcedAction) extends ForcedAction


object LeadersExpansion extends Expansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case LeadersLoresShuffledAction(l1, l2) =>
            game.leaders = l1.take(factions.num + 1)
            game.lores = l2.take(factions.num + 1)
            game.unusedLores = l2.drop(factions.num + 1)

            log("Leaders".hh, "and", "Lores".hh, "were shuffled")

            game.leaders.foreach { l =>
                log("Drew", l)
            }

            game.lores.foreach { l =>
                log("Drew", l)
            }

            DraftNextAction(factions.last)

        case DraftNextAction(f) =>
            if (game.leaders.num <= 1 && game.lores.num <= 1) {
                game.leaders = $
                game.lores = $

                Milestone(LeadersFactionsSetupAction)
            }
            else {
                val next = DraftNextAction((factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f)).last)

                implicit val convert = (l : Either[Leader, Lore]) => l @@ {
                    case Left(l) => l.img
                    case Right(l) => l.img
                }

                game.current = |(f)

                YYSelectObjectsAction(f, game.leaders./(Left(_)) ++ game.lores./(Right(_)))
                    .withGroup("Leaders and Lores".hl)
                    .withSplit($(game.leaders.num))
                    .withRule({
                        case Left(l) => f.leader.none
                        case Right(l) => f.lores.none
                    })
                    .withThen({
                        case Left(l) => AssignLeaderAction(f, l, next)
                        case Right(l) => AssignLoreAction(f, l, next)
                    })({
                        case Left(l) => "Take " ~ l.elem
                        case Right(l) => "Take " ~ l.elem
                    })("Take")
                    .withExtras(NoLeadersAndLores)
            }

        case AssignLeaderAction(f, l, then) =>
            game.leaders :-= l

            f.leader = |(l)

            f.log("took", l)

            then

        case AssignLoreAction(f, l, then) =>
            game.lores :-= l

            f.lores :+= l

            f.log("took", l)

            then

        case LeadersFactionsSetupAction =>
            var archivist: |[Faction] = None

            factions.lazyZip(board.starting).foreach { case (f, (a, b, cc)) =>
                val leader = f.leader.get

                leader.setupA.foreach { p =>
                    f.reserve --> p --> a
                }

                f.log("placed", leader.setupA./(_.of(f)).comma, "in", a)

                leader.setupB.foreach { p =>
                    f.reserve --> p --> b
                }

                f.log("placed", leader.setupB./(_.of(f)).comma, "in", b)

                cc.foreach { c =>
                    leader.setupC.foreach { p =>
                        f.reserve --> p --> c
                    }

                    f.log("placed", leader.setupC./(_.of(f)).comma, "in", c)
                }

                f.resources = leader.resources

                f.log("took", leader.resources.lift(0), "and", leader.resources.lift(1))

                if (f.can(Cryptic))
                    f.outraged ++= $(Material, Fuel)

                if (f.can(Greedy))
                    f.outraged ++= $(Material)

                if (f.can(AncientHoldings)) {
                    f.extraKeys = $(4) ++ f.extraKeys
                    f.resources = $(Nothingness) ++ f.resources
                }

                if (f.can(Decentralized)) {
                    f.reserve --> City.of(f) --> game.scrap
                    f.reserve --> City.of(f) --> game.scrap
                }

                if (f.can(Hated)) {
                    f.reserve --> Ship.of(f) --> game.scrap
                    f.reserve --> Ship.of(f) --> game.scrap
                    f.reserve --> Agent.of(f) --> game.scrap
                    f.reserve --> Agent.of(f) --> game.scrap
                    f.reserve --> Agent.of(f) --> game.scrap
                }

                if (f.can(Learned)) {
                    archivist = |(f)
                }
            }

            if (archivist.any) {
                implicit def convert(u : Lore) = u.img

                XXSelectObjectsAction(archivist.get, game.unusedLores.take(5))
                    .withGroup("Select", "2".hlb, "extra lore cards")
                    .withRule(_.num(2))
                    .withThen(l => LearnedAction(archivist.get, l, StartChapterAction).as("Keep", l.comma))
                    .ask
            }
            else {
                StartChapterAction
            }

        // ELDER
        case BelovedAction(f, then) =>
            MayInfluenceAction(f, |(Beloved), then)

        // ARCHIVIST
        case LearnedAction(f, l, then) =>
            l.foreach { u =>
                f.lores :+= u
                f.log("gained", u, "from", Learned)
            }

            then

        // NOBLE
        case ConnectedAction(f, then) =>
            val c = court.first

            c --> market

            SecureAction(f, NoCost, |(Connected), c, then)

        // DEMAGOGUE
        case BoldMainAction(f, influenced, then) =>
            Ask(f).group("Influence".hl)
                .each(market)(c => InfluenceAction(f, NoCost, c, |(Bold), BoldMainAction(f, influenced :+ c, then)).as(c)
                    .!(influenced.has(c), "influenced")
                    .!(f.pool(Agent).not, "no agents")
                )
                .cancelIf(influenced.none)
                .done(influenced.any.?(then))

        // FEASTBRINGER
        case GiveGuildCardAction(f, e, c, then) =>
            f.loyal --> c --> e.loyal

            f.log("gave", c, "to", e)

            GainCourtCardAction(e, c, None, then)

        // SHAPER
        case MythicAction(f, s, r, k, then) =>
            game.overrides += s -> r

            f.remove(ResourceRef(r, |(k)))

            f.log("changed", s, "type to", r, "with", Mythic)

            then

        case _ => UnknownContinue
    }
}
