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

import scala.collection.immutable.ListMap


// who where whose what, and what next
case class TryForcedRemoveAction(e : Faction, c : Clearing, f : Faction, p : Piece, n : Int, m : Message, then : ForcedAction, fail : ForcedAction) extends ForcedAction
case class ForcedRemoveAction(e : Faction, c : Clearing, f : Faction, p : Piece, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class ForcedRemoveTargetEffectAction(e : Faction, c : Clearing, f : Faction, p : Piece, then : ForcedAction) extends ForcedAction
case class ForcedRemoveSourceEffectAction(e : Faction, c : Clearing, f : Faction, p : Piece, then : ForcedAction) extends ForcedAction
case class ForcedRemoveScoreAction(e : Faction, c : Clearing, f : Faction, p : Piece, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class ForcedRemoveFinishedAction(e : Faction, then : ForcedAction) extends ForcedAction
case class ForcedRemoveProcessAction(e : Faction, then : ForcedAction) extends ForcedAction
case class ForcedRemoveCleanupAction(e : Faction, then : ForcedAction) extends ForcedAction

case object NeverAction extends ForcedAction

trait RemoveEffect extends Effect

trait RemovalResistance { self : Piece => }

trait NukeType extends Record

object NukeType {
    case object TotalAnnihilation extends NukeType
    case object ClearSector extends NukeType
    case object AntiMaterial extends NukeType
    case object AntiPersonnel extends NukeType
}


case class NukeAction(f : Faction, affects : $[Faction], l : $[Clearing], nuke : NukeType, then : ForcedAction) extends ForcedAction

case class EnemyPiece(clearing : Clearing, faction : Faction, piece : Piece)

object RemovalExpansion extends MandatoryExpansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // REMOVE
        case TryForcedRemoveAction(e, c, f, p, n, m, then, fail) =>
            ForcedRemoveAction(e, c, f, p, n, m, then)

        case ForcedRemoveAction(e, c, f, p, n, m, then) =>
            f.from(c) --> p --> f.limbo(c)

            game.graveyard += c -> (game.graveyard(c) :+ Figure(f, p, -1))

            ForcedRemoveSourceEffectAction(e, c, f, p, ForcedRemoveTargetEffectAction(e, c, f, p, ForcedRemoveScoreAction(e, c, f, p, n, m, then)))

        case ForcedRemoveSourceEffectAction(e, c, f, p, then) =>
            then

        case ForcedRemoveTargetEffectAction(e, c, f, p, then) =>
            then

        case ForcedRemoveScoreAction(e, c, f, p, n, m, then) =>
            e.oscore(n)(m, p.ofg(f))

            then

        case ForcedRemoveFinishedAction(f, then) =>
            (f +: factions.but(f)).foldLeft[ForcedAction](ForcedRemoveCleanupAction(f, then))((a, e) => ForcedRemoveProcessAction(e, a))

        case ForcedRemoveProcessAction(f, then) =>
            then

        case ForcedRemoveCleanupAction(f, then) =>
            (factions ++ hirelings).foreach { e =>
                board.clearings.foreach { c =>
                    e.limbo(c) --> game.dead
                }

                e.used = e.used.notOf[RemoveEffect]
                e.ignored = e.ignored.notOf[RemoveEffect]
            }

            then

        case NukeAction(f, affects, l, nuke, then) =>
            // TARGETS
            val damaged : $[(Hero, Clearing)] = (nuke.in(NukeType.ClearSector, NukeType.TotalAnnihilation)).??(affects.of[Hero])./~(e => l./~(c => e.at(c).has(Vagabond).?(e -> c)))

            val dd : $[EnemyPiece] = l./~(c => affects./~(e => e.at(c).%{
                case _ : Tenacious => false
                case _ if nuke == NukeType.TotalAnnihilation => true
                case _ if f.canRemove(c)(e).not => false
                case _ if nuke == NukeType.ClearSector => true
                case _ : Building if nuke == NukeType.AntiMaterial => true
                case _ : Token if nuke == NukeType.AntiMaterial => true
                case _ : Warrior if nuke == NukeType.AntiPersonnel => true
                case _ => false
            }./(p => EnemyPiece(c, e, p))))

            val clearings = l.intersect(dd.map(_.clearing) ++ damaged.rights).sortBy { c =>
                dd./{ case EnemyPiece(cc, ee, pp) => (c == cc).??(1 + pp.is[Scoring].??(10)) }.sum +
                damaged.rights.has(c).??(100)
            }

            val destroyed = dd.sortBy { case EnemyPiece(c, e, p) =>
                (clearings.indexOf(c) * 1000) +
                (p.is[RemovalResistance]).??(200) +
                (p == Keep).??(100) +
                (p.is[Relic]).??(80) +
                (p.is[Building]).??(50) +
                (p.is[Token]).??(20) +
                (p.is[Scoring]).??(10)
            }


            // SCORE
            game.highlights :+= NukeHighlight(clearings)

            destroyed.foreach(x => f.log("removed", x.piece.ofg(x.faction), "in", x.clearing))

            val n = destroyed./(_.piece).of[Scoring].notOf[Palace.type].num

            f.oscore(n)("laying waste on", clearings./(_.elem).comma)


            // PROCESS
            var q : ForcedAction = ForcedRemoveFinishedAction(f, then)

            damaged.foreach { case (e, c) =>
                q = BattleAssignHitsAction(e, Battle(c, f, None, e, None, None, None, 0, 0, 0, DummyAction), 3, 0, q)
            }

            destroyed.foreach { x =>
                q = TryForcedRemoveAction(f, x.clearing, x.faction, x.piece, x.piece.is[Palace.type].??(1), Removing, q, x.piece.is[RemovalResistance].?(q).|(NeverAction))
            }

            q

        case _ => UnknownContinue
    }

}
