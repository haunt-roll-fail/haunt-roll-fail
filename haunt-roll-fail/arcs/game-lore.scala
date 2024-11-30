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


abstract class Lore(val id : String, val name : String) extends Record with Effect with Elementary {
    def img = Image(id, styles.card)
    def elem = name.styled(styles.title).hl
}

case object MirrorPlating     extends Lore("lore04", "Mirror Plating")
case object HiddenHarbors     extends Lore("lore05", "Hidden Harbors")
case object LivingStructures  extends Lore("lore10", "Living Structures")
case object SurvivalOverrides extends Lore("lore18", "Survival Overrides")
case object WarlordsCruelty   extends Lore("lore23", "Warlords Cruelty")

object Lores {
    def all = $(
        MirrorPlating    ,
        HiddenHarbors    ,
        LivingStructures ,
        SurvivalOverrides,
        WarlordsCruelty  ,
    )

    def preset1 = $(MirrorPlating, HiddenHarbors, LivingStructures, SurvivalOverrides, WarlordsCruelty)
}


object LoreExpansion extends Expansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        case _ => UnknownContinue
    }
}
