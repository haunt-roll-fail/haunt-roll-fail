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


case object GateWraith extends Fate("Gate Wraith", "fate22", 3) {
    override val expansion = GateWraithExpansion
}


case object CollapseTheGates extends Objective("f22-01b", "Collapse the Gates of the Reach")

case object CatapultOverdrive extends Lore("f22-02", "Catapult Overdrive")
case object BreakingGates extends Ability("f22-03", "Breaking Gates" /*+ " & Placing Passages"*/)

case object TheDeadLive extends Law("f22-04", "The Dead Live") with SpecialRegion
case object Passages extends Law("f22-05", "Passages & The Twisted Passage")

case object PassageStorms extends VoxEffect("Passage Storms")


case class BreakGateMainAction(self : Faction, x : Cost, then : ForcedAction) extends ForcedAction with Soft
case class BreakGateFromAction(self : Faction, x : Cost, s : System, ss : $[Figure], l : $[Figure], then : ForcedAction) extends ForcedAction with Soft
case class BreakGateAction(self : Faction, x : Cost, i : Int, s : |[System], ss : $[Figure], l : $[Figure], then : ForcedAction) extends ForcedAction
case class ReRollPassageExitAction(then : ForcedAction) extends ForcedAction
case class PassageExitRolledAction(random : Symbol, then : ForcedAction) extends RandomAction[Symbol]

case class BreakGateSlipstreamMainAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft

case class PassageStormsMainAction(self : Faction, then : ForcedAction) extends ForcedAction


object GateWraithExpansion extends FateExpansion(GateWraith) {
    val deck = $(
        CatapultOverdrive,
        VoxCard("f22-06", PassageStorms),
    )

    val Passage = System(7, Gate)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // GATE WRAITH III
        case FateInitAction(f, `fate`, 3, then) =>
            f.objective = |(CollapseTheGates)

            f.progress = game.factions.num @@ {
                case 2 => 24
                case 3 => 21
                case 4 => 18
            }

            f.log("objective was set to", f.progress.hlb)

            FateDeck(fate) --> CatapultOverdrive --> f.lores

            f.log("got", f.lores.last)

            f.abilities :+= BreakingGates

            f.log("gained", f.abilities.last)

            game.laws :+= TheDeadLive

            f.log("set", game.laws.last)

            game.figures.register(TheDeadLive)
            game.figures.register(Passage)
            game.systems :+= Passage

            game.laws :+= Passages

            f.log("set", game.laws.last)

            FateDeck(fate).%(_.as[VoxCard]./(_.effect).has(PassageStorms)) ++ game.court.$ --> game.court

            f.log("placed", game.court.first, "on top of the court deck")

            Ask(f).group("Open Passage")
                .each($(1, 2, 3, 4, 5, 6))(i => BreakGateAction(f, NoCost, i, None, $, $, ReRollPassageExitAction(then)).as(System(i, Gate)))

        case BreakGateAction(f, x, n, s, ss, l, then) =>
            f.pay(x)

            game.broken :+= n

            val s = System(n, Gate)

            if (l.any) {
                l --> Passage

                f.log("moved", l.intersperse(Comma), "from", s, "to", Passage)
            }

            f.log("broke", s, ss.some./("and removed" -> _.intersperse(Comma)), x)

            ss.foreach { b =>
                b.faction.damaged :-= b

                b --> b.faction.reserve
            }

            val gg = s.$
            val bb = gg.buildings

            if (bb.any)
                f.log("removed", bb.intersperse(Comma), "from gate")

            bb.foreach { b =>
                b.faction.damaged :-= b

                b --> b.faction.reserve
            }

            gg.diff(bb) --> Passage

            f.log("moved", gg.diff(bb).intersperse(Comma), "to", Passage, "from gate")

            Then(then)

        case ReRollPassageExitAction(then) =>
            Random[Symbol]($(Arrow, Crescent, Hex), PassageExitRolledAction(_, then))

        case PassageExitRolledAction(s, then) =>
            game.exit = |(s)

            log(Passage, "exits shifted to", systems.%(_.symbol == s).intersperse(Comma))

            then

        case BreakGateSlipstreamMainAction(f, then) =>
            val regent = f.regent
            val officers = regent && f.officers

            val fs = f.flagship.get.system

            if (fs.gate.not && game.broken.contains(fs.cluster).not) {
                val ss = SlipstreamDrive(f).$ ++ SlipstreamDrive(f).armor.$
                val l = MovementExpansion.movable(f, fs, officers, false)

                if (l.any)
                    BreakGateFromAction(f, NoCost, fs, ss, l, then)
                else
                    then
            }
            else
                then

        case BreakGateMainAction(f, x, then) =>
            val regent = f.regent
            val officers = regent && f.officers

            var pp = systems./~(s => f.at(s).starports./(_ -> s))
            var ss = systems./(s => s -> MovementExpansion.movable(f, s, officers, false)).%>(_.any).toMap

            if (f.hasLore(EmpathsBond) && game.declared.contains(Empath))
                pp ++= systems./~(s => game.colors.but(f)./~(_.at(s).starports./(_ -> s)))
            else
            if (f.hasLore(CatapultOverdrive) || f.hasLore(CatapultOverdriveLL))
                pp ++= systems./~(s => game.colors.but(f)./~(_.at(s).starports./(_ -> s))).%>(f.rules)
            else
            if (campaign)
                pp ++= systems./~(s => Free.at(s).starports./(_ -> s)).%>(f.rules)

            pp = pp.%>(ss.contains).%>!(_.cluster.in(game.broken))

            Ask(f).group("Break Gate".hh, x, "from")
                .each(pp)((u, s) => BreakGateFromAction(f, x, s, $(u), MovementExpansion.movable(f, s, officers, false), then).as(game.showFigure(u), u, "in", s))
                .cancel

        case BreakGateFromAction(f, x, s, ss, l, then) =>
            val combinations = l.groupBy(u => (u.faction, u.piece)).$.sortBy(_._1._2 @@ {
                case Flagship => 1
                case Ship => 2
                case Witness => 3
                case Blight => 4
            })./{ case ((c, p), l) =>
                val n = f.hasTrait(Disorganized).?(l.num.hi(2)).|(l.num)
                val (damaged, fresh) = l.partition(c.damaged.has)
                val combinations = 0.to(n).reverse./~(k => max(0, k - fresh.num).to(min(k, damaged.num))./(i => fresh.take(k - i) ++ damaged.take(i)))
                combinations
            }.reduceLeft((aa, bb) => aa./~(a => bb./(b => a ++ b))).%(_.any).%(l => l.ofc(Empire).num <= l.ofc(f).num || f.officers)

            implicit def convert(u : Figure) = game.showFigure(u)

            Ask(f).group("Break Gate".hh, "from", s, "using", ss.intersperse(Comma), x)
                .each(combinations)(l => {
                    BreakGateAction(f, x, s.cluster, |(s), ss, l, then).as(l./(u => convert(u)))
                })
                .cancel

        case CourtCrisesPerformAction(cluster, symbol, lane, skip, main, v @ VoxCard(_, PassageStorms), then) =>
            val next : ForcedAction = UnderTopCrisisVoxCardAction(v, lane, CourtCrisesContinueAction(cluster, symbol, lane, skip + main.??(1), then))

            log("Crisis", v)

            val l = Passage.$ // System(cluster, symbol).$
            val ll = l.diff(l.blights).diff(l.bunkers)./~{
                case u @ Figure(f : Faction, Flagship, _) => Flagship.scheme(f).reverse./~(_.$).starting
                case u => |(u)
            }

            val (destroyed, damaged) = ll.partition(_.damaged)

            if (damaged.any) {
                log(v, "damaged", damaged.intersperse(Comma))

                damaged.foreach { u =>
                    u.faction.damaged :+= u
                }
            }

            if (destroyed.any) {
                log(v, "destroyed", destroyed.intersperse(Comma))

                destroyed.foreach { u =>
                    u.faction.damaged :-= u

                    u --> (u.piece == Ship && game.laws.has(TheDeadLive)).?(TheDeadLive).|(u.faction.reserve)
                }
            }

            next

        case GainCourtCardAction(f, v @ VoxCard(_, PassageStorms), lane, main, then) =>
            PassageStormsMainAction(f, then)

        case PassageStormsMainAction(f, then) =>
            val l = MovementExpansion.movable(f, Passage, f.regent && f.officers, f.hasLore(BlightFury) || f.hasLore(BlightSociety))

            if (l.any)
                MoveFromAction(f, Passage, l, false, NoCost, |(PassageStorms), then.as("Done"), PassageStormsMainAction(f, then))
            else
                Then(then)


        // ...
        case _ => UnknownContinue
    }
}
