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


trait Transport extends Record {
    def img : Elem = Empty
    def allows(f : Faction)(implicit game : Game) : Boolean = true
    def allows(f : Faction, o : Region)(implicit game : Game) : Boolean = allows(f)
    def allows(f : Faction, o : Region, d : Region)(implicit game : Game) : Boolean = allows(f, o)
    def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) : Boolean = allows(f, o, d)
    def order(l : $[Movable]) : Int = 0
    def sortBy(m : Movable) : Int = 0
}

object Transport {
    implicit class TransportEx(aa : $[$[Transport]]) {
        def **(bb : $[$[Transport]]) = aa./~(a => bb./(b => a ++ b))
    }
}

case object FreeMove extends Transport

case class Party(transport : $[Transport], movable : $[Movable])

case object Roads extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Clearing, d : Clearing) => board.connected(o).has(d) &&
            game.rubble.has((o, d)).not &&
            game.rubble.has((d, o)).not &&
            game.blizzard.has((o, d)).not &&
            game.blizzard.has((d, o)).not &&
            factions.of[Caster].%(game.states.contains).exists(f => f.growth.has((o, d)) || f.growth.has((d, o))).not

        case (o : Forest, d : Clearing) => game.fromForest(o).has(d)
        case (o : Clearing, d : Forest) => board.fromForest(d).has(o)
        case _ => false
    }
}

case object ForestClearingMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Forest, d : Clearing) => game.fromForest(o).has(d)
        case (o : Clearing, d : Forest) => board.fromForest(d).has(o)
        case _ => false
    }
}

case object BurrowRoads extends Transport {
    override def allows(f : Faction)(implicit game : Game) = f @@ {
        case f : Underground => true
        case _ => false
    }

    override def allows(f : Faction, o : Region)(implicit game : Game) : Boolean = (f, o) @@ {
        case (f : Underground, o : Burrow) => o == f.burrow
        case (f : Underground, o : Clearing) => f.at(o).%(_ == Tunnel).any
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (f, o, d) @@ {
        case (f : Underground, o : Burrow, d : Clearing) => o == f.burrow && f.at(d).%(_ == Tunnel).any
        case (f : Underground, o : Clearing, d : Burrow) => f.at(o).%(_ == Tunnel).any && d == f.burrow
        case _ => false
    }
}

case object TunnelRoads extends Transport {
    override def img = Image("tunnel", styles.widepiece)

    def crafting(f : Faction, o : Region)(implicit game : Game) = (f, o) @@ {
        case (f : Faction, o : Clearing) if f.has(FrogEngineers) && factions.of[InvasiveAAA].only.at(o).of[FrogTokenAAA].any => true
        case (f : Faction, o : Clearing) if factions.of[InvasiveDDD].any && f.has(SuitLaborers(factions.of[InvasiveDDD].only, Fox   )) && factions.of[InvasiveDDD].only.at(o).use(l => l.has(PeacefulDDD(Fox   ))) => true
        case (f : Faction, o : Clearing) if factions.of[InvasiveDDD].any && f.has(SuitLaborers(factions.of[InvasiveDDD].only, Rabbit)) && factions.of[InvasiveDDD].only.at(o).use(l => l.has(PeacefulDDD(Rabbit))) => true
        case (f : Faction, o : Clearing) if factions.of[InvasiveDDD].any && f.has(SuitLaborers(factions.of[InvasiveDDD].only, Mouse )) && factions.of[InvasiveDDD].only.at(o).use(l => l.has(PeacefulDDD(Mouse ))) => true
        case (f : Faction, o : Clearing) if factions.of[InvasiveCCC].any && f.has(Laborers(factions.of[InvasiveCCC].only)) && factions.of[InvasiveCCC].only.at(o).of[PeacefulCCC].any => true
        case (f : Faction, o : Clearing) if factions.of[InvasiveBBB].any && f.has(Integration(factions.of[InvasiveBBB].only, Fox   )) && factions.of[InvasiveBBB].only.at(o).use(l => l.has(PeacefulBBB(Fox   )) || l.has(MilitantBBB(Fox   ))) => true
        case (f : Faction, o : Clearing) if factions.of[InvasiveBBB].any && f.has(Integration(factions.of[InvasiveBBB].only, Rabbit)) && factions.of[InvasiveBBB].only.at(o).use(l => l.has(PeacefulBBB(Rabbit)) || l.has(MilitantBBB(Rabbit))) => true
        case (f : Faction, o : Clearing) if factions.of[InvasiveBBB].any && f.has(Integration(factions.of[InvasiveBBB].only, Mouse )) && factions.of[InvasiveBBB].only.at(o).use(l => l.has(PeacefulBBB(Mouse )) || l.has(MilitantBBB(Mouse ))) => true

        case (f : Feline, o : Clearing) => f.at(o).has(Workshop)
        case (f : Aviary, o : Clearing) => f.at(o).has(Roost)
        case (f : Insurgent, o : Clearing) => f.at(o).has(Sympathy)
        case (f : Trader, o : Clearing) => f.at(o).of[TradePost].any && options.has(TunnelsIgnoreTradePosts).not
        case (f : Fanatic, o : Clearing) => f.at(o).of[Garden].any
        case (f : Underground, o : Clearing) => f.at(o).of[Building].any
        case (f : Mischief, o : Clearing) => f.at(o).of[Plot].any
        case (f : Horde, o : Clearing) => f.at(o).has(Stronghold)
        case (f : Expedition, o : Clearing) => f.at(o).of[WayStation].any
        case (f : OldExpedition, o : Clearing) => f.at(o).has(Station)
        case (f : Utopia, o : Clearing) => f.at(o).has(Palace)
        case (f : Caster, o : Clearing) => f.at(o).has(School)
        case (f : InvasiveDDD, o : Clearing) => f.at(o).of[FrogTokenDDD].any
        case (f : InvasiveCCC, o : Clearing) => f.at(o).of[PeacefulCCC].any
        case (f : InvasiveBBB, o : Clearing) => f.at(o).use(l => l.of[PeacefulBBB].any || l.of[MilitantBBB].any)
        case (f : InvasiveAAA, o : Clearing) => f.at(o).of[PeacefulAAA].any || (f.has(FrogEngineers) && f.at(o).of[MilitantAAA].any)
        case _ => false
    }

    override def allows(f : Faction)(implicit game : Game) = f.has(Tunnels)

    override def allows(f : Faction, o : Region)(implicit game : Game) = allows(f) && crafting(f, o)

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = allows(f, o) && crafting(f, d)
}

case object RuledMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Rulable, d : Rulable) => f.rules(o) || f.rules(d) || f.has(CorvidPlanners) || ((game.riverside.contains(o) || game.riverside.contains(d)) && f.has(OtterDivers))
        case _ => false
    }
}

case object HalfWarriors extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = (f, o) @@ {
        case (f : Faction, o : Region) => f.at(o).of[Warrior].notOf[Tenacious].any
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = (f, o, l) @@ {
        case (f : Faction, o : Region, l : $[Movable]) => l.forall(_.is[Warrior]) && l.of[Tenacious].none && f.at(o).of[Warrior].num /↑ 2 == l.num
    }
}

case object AllWarriors extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = (f, o) @@ {
        case (f : Faction, o : Region) => f.at(o).of[Warrior].any
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = (f, o, l) @@ {
        case (f : Faction, o : Region, l : $[Movable]) => l.forall(_.is[Warrior]) && f.at(o).of[Warrior].num == l.num
    }
}

case object Ferry extends Transport with DisplayEffect with Pawn with Tenacious {
    override def img = Image("ferry", styles.widepiece)
    override val name = "Ferry"

    override def allows(f : Faction)(implicit game : Game) = game.ferry.any && f.as[Hireling]./~(_.owner).|(f).used.has(Ferry).not

    override def allows(f : Faction, o : Region)(implicit game : Game) = allows(f) && o @@ {
        case o : Clearing => game.ferry.has(o)
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = allows(f, o) && (o, d) @@ {
        case (o : Clearing, d : Clearing) => game.byRiver(o).has(d)
        case _ => false
    }
}

case object OffTrail extends Transport with CardEffect {
    override def img = Image("forest", styles.widepiece)
    override val name = "Off Trail"

    override def allows(f : Faction)(implicit game : Game) = f.can(OffTrail)

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = allows(f, o) && (o, d) @@ {
        case (o : Clearing, d : Clearing) => game.board.forests.exists(x => game.board.fromForest(x).use(l => l.has(o) && l.has(d)))
        case _ => false
    }

    override def order(l : $[Movable]) = 1
}




case class MoveInitAction(self : Faction, f : Faction, tt : $[$[Transport]], m : Message, l : $[Region], all : $[Region], extra : $[UserAction], then : ForcedAction) extends ForcedAction with Soft
case class MoveFromAction(self : Faction, f : Faction, tt : $[$[Transport]], m : Message, from : Region, to : $[Region], then : ForcedAction) extends BaseAction(implicit g => "Move " ~ m.elem(g) ~ " from" ~ (m == NoMessage).?(self.as[Aviary]./(f => " [" ~ f.desc(Decree.Move) ~ "]").|(Empty)))(from) with Soft
case class MoveToAction(self : Faction, f : Faction, tt : $[$[Transport]], m : Message, from : Region, to : Region, then : ForcedAction) extends BaseAction(g => (m == SlipTo).?(m.elem(g)).|("Move " ~ m.elem(g) ~ " from " ~ from.elem(g) ~ " to"))(tt./(_./(_.img).merge), to, g => Image("move-deg-" + g.board.dir(from, to), styles.token, "")) with Soft
case class MoveListAction(self : Faction, f : Faction, t : $[Transport], m : Message, from : Region, to : Region, l : $[Piece], then : ForcedAction) extends BaseAction(g => (m == SlipTo).?(m.elem(g)).|("Move " ~ m.elem(g) ~ " from " ~ from.elem(g) ~ " to") ~ " " ~ to.elem(g))(t./(_.img).merge, l./(_.img(f)).merge)
case class CantMoveAction(self : Faction, f : Faction, tt : $[$[Transport]], m : Message, from : Region, to : Region, then : ForcedAction) extends BaseAction(g => f.elem ~ "can't move " ~ m.elem(g) ~ " from " ~ from.elem(g) ~ " to" ~ " " ~ to.elem(g))("Ok")
case class MoveListAlliedAction(self : Faction, f : Faction, t : $[Transport], m : Message, from : Region, to : Region, l : $[Piece], a : Faction, al : $[Piece], then : ForcedAction) extends BaseAction(g => "Move " ~ m.elem(g) ~ " from", from.as[Clearing].|("forest"), "to", to)(t./(_.img).merge, l./(_.img(self)).merge, al./(p => p.img(a)).merge)
case class MoveCompleteAction(self : Faction, f : Faction, from : Region, to : Region, l : $[Piece], notified : $[Faction], then : ForcedAction) extends ForcedAction
case class MoveFinishedAction(f : Faction, from : Region, to : Region, then : ForcedAction) extends ForcedAction
case class FerryAction(f : Faction, from : Clearing, to : Clearing, then : ForcedAction) extends ForcedAction
case class MarchAction(self : Faction, n : Int, total : Int, then : ForcedAction) extends ForcedAction with Soft


trait PreMove { self : Action => }

object MovementExpansion extends MandatoryExpansion {
    def describe(self : Faction, f : Faction, t : $[Elementary], l : $[Piece])(implicit game : Game) : $[Elem] = {
        val prefix = f @@ {
            case f : AbductAAA if self == f && l.of[SkunkAAACaptain].single.any => $(f.characters(l.of[SkunkAAACaptain].only.phase).name.styled(f), "moved".txt)
            case f => $(f.elem, "moved".txt, t./(_.elem ~ ", ").merge)
        }

        val units =
            if (l.num > 7 && l.forall(_ == Otter))
                $("Otter Ball".styled(f))
            else
            if (l.forall(_ == Raven))
                $("Murder of Crows".styled(f))
            else
                l./{
                    case SkunkAAACaptain(_) if self == f => Empty
                    case SkunkAAACaptain(phase) => f.as[AbductAAA]./(_.characters(phase).name.styled(f)).|("Captain".styled(f))
                    case p => p.of(f)
                }.but(Empty).comma

        prefix ++ units
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // MOVE
        case MoveInitAction(self, f, ttt, m, from, allfrom, extra, then) =>
            val plans = f.movePlans(from.%(f.canMoveFrom), ttt.some.|(game.transports./($) ** f.transports))

            Ask(self)(extra.of[PreMove])(allfrom./(o => plans.get(o) @@ {
                case Some((t, l)) => MoveFromAction(self, f, t, m, o, l, then)
                case None => MoveFromAction(self, f, Nil, m, o, Nil, then).!(f.canMoveFrom(o).not, "snared").!(true)
            }))(extra.notOf[PreMove])

        case MoveFromAction(self, f, tt, m, from, to, then) =>
            Ask(self)
                .each(to)(d =>
                    MoveToAction(self, f, tt.%(_.forall(_.allows(f, from, d))), m, from, d, then)
                        .!(tt.exists(_.forall(_.allows(f, from, d))).not, "disallowed")
                        .!({
                            val mv = f.at(from).of[Movable]
                            val cm = 1.to(mv.num)./~(n => mv.combinations(n))
                            tt.exists(t => cm.exists(l => t.forall(_.allows(f, from, d, l)))).not
                        }, "disallowed")
                )
                .cancel

        case MoveToAction(self, f, tt, m, from : Region, to : Region, then) =>
            val mv = f.at(from).of[Movable]
            val cm = 1.to(mv.num)./~(n => mv.combinations(n))
            val pp = tt./~(t => cm.%(l => t.forall(_.allows(f, from, to, l)))./(l => l.sortBy(m => t./(_.sortBy(m)).sum))./(Party(t, _))).sortBy(p => p.transport./(t => t.order(p.movable)).sum)

            Ask(self)(pp./(p => MoveListAction(self, f, p.transport, m, from, to, p.movable, then))).ocancel.bailout(CantMoveAction(self, f, tt, m, from, to, then))

        case CantMoveAction(self, f, t, m, from, to, then) =>
            f.log("could not move from", from, "to", to, m)

            then

        case MoveListAction(self, f, t, m, from : Clearing, to : Clearing, l, FalseOrdersCompleteAction(ff, then)) if ff != f =>
            FalseOrdersCompleteAction(ff, ForceAction(MoveListAction(f, f, t, m, from : Clearing, to : Clearing, l, then)))

        case MoveListAction(self, f, t, m, from : Clearing, to : Clearing, l, then) if t.has(Ferry) =>
            log(describe(self, f, $(Ferry), l), "from", from, "to", to, m)

            Force(MoveListAction(self, f, $, NoMessage, from, to, l, FerryAction(f, from, to, then)))

        case MoveListAction(self, f, t, m, from : Clearing, to : Clearing, l, then) if t.has(OffTrail) =>
            log(describe(self, f, $, l), "from", from, "to", to, m, "going", OffTrail)

            Force(MoveListAction(self, f, $, NoMessage, from, to, l, UsedEffectAction(f, OffTrail, then)))

        case MoveListAction(self, f, t, m, from : Region, to : Region, l, then) if t.any =>
            log(describe(self, f, $, l), "from", from, "to", to, m)

            Force(MoveListAction(self, f, $, NoMessage, from, to, l, then))

        case MoveListAction(self, f, Nil, m, from : Region, to : Region, l, then) =>
            game.highlights :+= MoveHighlight(from, to)

            f.from(from) --> l --> to

            MoveCompleteAction(self, f, from, to, l, Nil, then)

        case FerryAction(f, from, to, then) =>
            val b = f.as[Hireling]./~(_.owner).|(f)

            b.used :+= Ferry

            game.ferry :-= from
            game.ferry :+= to

            DrawCardsAction(b, 1, WithEffect(Ferry), AddCardsAction(b, then))

        case MoveCompleteAction(self, f, from, to : Clearing, l, e, then) if hirelings.has(StreetBand) && e.has(StreetBand).not =>
            if (f == game.current)
                if (f.has(StreetBand).not)
                    if (StreetBand.at(to).any)
                        if (StreetBand.enchanted.contains(to).not) {
                            StreetBand.log("started playing in", to)

                            StreetBand.enchanted :+= to
                        }

            MoveCompleteAction(self, f, from, to, l, e :+ StreetBand, then)

        case MoveCompleteAction(self, f, from, to, l, e, then) =>
            MoveFinishedAction(f, from, to, then)

        case MoveFinishedAction(f, from, to, FerryAction(ff, ffrom, fto, fthen)) =>
            FerryAction(ff, ffrom, fto, MoveFinishedAction(f, from, to, fthen))

        case MoveFinishedAction(f, from, to, then) =>
            then

        case MarchAction(f, n, total, then) =>
            val cc = f.moveFrom
            if (n > total)
                then
            else
            if (cc.none)
                then
            else
                MoveInitAction(f, f, Nil, (total > 1).?(March(n, total)).|(NoMessage), cc, f.movable, $((n == 1).?(CancelAction).|(then.as("Forfeit " ~ (total - n + 1).of("move")))), MarchAction(f, n + 1, total, then))

        case _ => UnknownContinue
    }

}
