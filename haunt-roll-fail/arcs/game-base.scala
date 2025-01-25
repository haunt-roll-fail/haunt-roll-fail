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


object BaseExpansion extends Expansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        case StartSetupAction =>
            if (game.starting.none)
                game.starting = board.starting

            CourtSetupAction

        case CheckWinAction =>
            if (chapter >= 5 || factions./(_.power).max >= 39 - factions.num * 3) {
                val winner = factions.%(_.power == factions./(_.power).max).first

                Milestone(GameOverAction(winner))
            }
            else
                Milestone(StartChapterAction)

        case _ => UnknownContinue
    }
}
