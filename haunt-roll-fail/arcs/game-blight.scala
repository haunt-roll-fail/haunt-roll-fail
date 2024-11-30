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


trait Tough { self : Piece => }

case object Blight extends Piece with Tough

case object Blights extends Color
case object Empire extends Color
case object Free extends Color


trait Act extends Record
case object ActA extends Act
case object ActB extends Act
case object ActC extends Act

abstract class Fate(val name : String, val id : String, val act : Act) extends Record with Elementary {
    def img = Image(id, styles.fateCard)
    def elem = (id == "no-fate").?(name.styled(styles.title).hh.spn(styles.notDoneYet)).|(name.styled(styles.title).hl)
}

case object Steward       extends Fate("Steward",            "fate01",  ActA)
case object Founder       extends Fate("Founder",            "no-fate", ActA)
case object Magnate       extends Fate("Magnate",            "no-fate", ActA)
case object Advocate      extends Fate("Advocate",           "no-fate", ActA)
case object Caretaker     extends Fate("Caretaker",          "no-fate", ActA)
case object Partisan      extends Fate("Partisan",           "no-fate", ActA)
case object Admiral       extends Fate("Admiral",            "no-fate", ActA)
case object Believer      extends Fate("Believer",           "no-fate", ActA)

case object Pathfinder    extends Fate("Pathfinder",         "no-fate", ActB)
case object Hegemon       extends Fate("Hegemon",            "no-fate", ActB)
case object PlanetBreaker extends Fate("Planet Breaker",     "no-fate", ActB)
case object Pirate        extends Fate("Pirate",             "no-fate", ActB)
case object BlightSpeaker extends Fate("Blight Speaker",     "no-fate", ActB)
case object Pacifist      extends Fate("Pacifist",           "no-fate", ActB)
case object Peacekeeper   extends Fate("Peacekeeper",        "no-fate", ActB)
case object Warden        extends Fate("Warden",             "no-fate", ActB)

case object Overlord      extends Fate("Overlord",           "no-fate", ActC)
case object Survivalist   extends Fate("Survivalist",        "no-fate", ActC)
case object Redeemer      extends Fate("Redeemer",           "no-fate", ActC)
case object Guardian      extends Fate("Guardian",           "no-fate", ActC)
case object Naturalist    extends Fate("Naturalist",         "no-fate", ActC)
case object GateWraith    extends Fate("Gate Wraith",        "no-fate", ActC)
case object Conspirator   extends Fate("Conspirator",        "no-fate", ActC)
case object Judge         extends Fate("Judge",              "no-fate", ActC)

object Fates {
    val actA = $(
        Steward       ,
        Founder       ,
        Magnate       ,
        Advocate      ,
        Caretaker     ,
        Partisan      ,
        Admiral       ,
        Believer      ,
    )

    val actB = $(
        Pathfinder    ,
        Hegemon       ,
        PlanetBreaker ,
        Pirate        ,
        BlightSpeaker ,
        Pacifist      ,
        Peacekeeper   ,
        Warden        ,
    )

    val actC = $(
        Overlord      ,
        Survivalist   ,
        Redeemer      ,
        Guardian      ,
        Naturalist    ,
        GateWraith    ,
        Conspirator   ,
        Judge         ,
    )
}


case class DealFatesAction(then : ForcedAction) extends ForcedAction
case class ChooseFatesAction(then : ForcedAction) extends ForcedAction
case class ChooseFateAction(self : Faction, x : Fate, then : ForcedAction) extends ForcedAction
case class FatesShuffledAction(shuffled : $[Fate], then : ForcedAction) extends ShuffledAction[Fate]
case object SetupEmpireAction extends ForcedAction
case class EmpireClustersRandomAction(random : $[Int]) extends RandomAction[$[Int]]

object BlightExpansion extends Expansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        case StartSetupAction =>
            DealFatesAction(SetupEmpireAction)

        case DealFatesAction(then) =>
            Shuffle[Fate](Fates.actA, FatesShuffledAction(_, then))

        case FatesShuffledAction(l, then) =>
            var rest = l

            factions.foreach { f =>
                if (f.fates.none) {
                    f.fates ++= rest.take(1)
                    rest = rest.drop(1)
                }

                f.fates ++= rest.take(1)
                rest = rest.drop(1)
            }

            log("Fates were shuffled and dealt")

            ChooseFatesAction(then)

        case ChooseFatesAction(then) =>
            val l = factions.%(_.fates.num > 1)

            implicit val convert = (x : Fate) => x.img

            if (l.none)
                Milestone(then)
            else
                MultiAsk(l./(f =>
                    YYSelectObjectsAction(f, f.fates)
                        .withGroup("Choose Fate")
                        .withThen(x => ChooseFateAction(f, x, ChooseFatesAction(then)))(x => x)("~~~")
                        .ask
                ))

        case ChooseFateAction(f, x, then) =>
            f.fates = $(x)

            if (f.past.has(x).not)
                f.past :+= x

            then

        case SetupEmpireAction =>
            Random[$[Int]]($($(1, 2), $(2, 3), $(3, 4), $(4, 5), $(5, 6), $(6, 1)), EmpireClustersRandomAction(_))

        case EmpireClustersRandomAction(l) =>
            log("rolled", l)

            systems.foreach { s =>
                if (s.cluster.in(l))
                    Empire.reserve --> Ship --> s
                else {
                    val u = Blights.reserve --> Blight
                    u --> s
                    Blights.damaged :+= u
                }
            }

            CourtSetupAction

        case _ => UnknownContinue
    }
}
