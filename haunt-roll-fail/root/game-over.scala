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


case object GameOverTriggredAction extends ForcedAction
case class GameOverAction(winners : $[Faction], scores : ListMap[Faction, Int], seating : $[Faction], options : $[Meta.O]) extends ForcedAction

trait NoGameOverTrigger { self : Action => }

case object GameOverTopAction extends ForcedAction

trait GameOverBaseAction extends OutOfTurn with NoGameOverTrigger with Choice {
    def question(implicit game : Game) = "Game Over"
}

trait GameOverBaseInfo extends Info { self : UserAction =>
    def question(implicit game : Game) = "Game Over"
}

case class GameOverWonAction(self : Faction, f : Faction) extends OptionInfo(f, "won", "(" ~ NameReference(f.name, f).hl ~ ")") with GameOverBaseInfo
case class GameOverTextAction(self : Faction, t : Elem) extends OptionInfo(t) with GameOverBaseInfo
case object GameOverMapsAction extends OptionAction("Maps") with GameOverBaseAction with Soft with OutOfTurn

case class GameOverDoneAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Done") with Soft with OutOfTurn

case class GameOverMoveMapAction(self : Faction, v : Boolean) extends BaseAction("Maps")("Movements".hlIf(v)) with NoGameOverTrigger with Soft with OutOfTurn with NoClear
case class GameOverBattleMapAction(self : Faction, v : Boolean) extends BaseAction("Maps")("Battles".hlIf(v)) with NoGameOverTrigger with Soft with OutOfTurn with NoClear
case class GameOverNukeMapAction(self : Faction, v : Boolean) extends BaseAction("Maps")("Nukes".hlIf(v)) with NoGameOverTrigger with Soft with OutOfTurn with NoClear
case class GameOverGraveyardAction(self : Faction, v : Boolean) extends BaseAction("Maps")("Graveyard".hlIf(v)) with NoGameOverTrigger with Soft with OutOfTurn with NoClear


object GameOverExpansion extends MandatoryExpansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // GAME OVER
        case _ if game.over.any && action.is[NoGameOverTrigger].not =>
            game.over.get

        case GameOverAction(winners, scores, seating, options) =>
            game.highlights :+= NothingHighlight
            game.highlights :+= NothingHighlight
            game.highlights :+= NothingHighlight

            log(winners./(_.elem).commaAnd, "won")

            val over = GameOver(winners, "Game Over" ~ Break ~ winners./(f => f.elem ~ " won" ~ Break ~ (f.vp >= 0).?(f.scorelog.join(Break))).join(Break),
                winners./~(w =>
                    $(GameOverWonAction(Neutral, w)) ++ (w.vp >= 0).?(GameOverTextAction(Neutral, w.scorelog.join(HorizontalBreak)))
                ) ++ $(GameOverMapsAction)
            )

            game.over = Some(over)

            over

        case GameOverTriggredAction =>
            val ww = factions.%(f => f.vp >= 30)
            val cw = factions.%(f => ww.%(w => f.coalition == Some(w)).any)
            val winners = ww ++ cw

            Milestone(GameOverAction(ww ++ cw, factions./(f => f -> f.vp).to(ListMap), game.seating./(game.ptf), game.options))

        case _ if game.over.none && game.turn > 0 && factions.%(f => f.vp >= 30).any =>
            Milestone("game over triggred", GameOverTriggredAction)

        case GameOverDoneAction(f, then) =>
            then

        case GameOverMapsAction =>
            MultiAsk(factions./(f => Ask(f,
                GameOverMoveMapAction(f, game.ui.movements) ::
                GameOverBattleMapAction(f, game.ui.battles) ::
                GameOverNukeMapAction(f, game.ui.nukes) ::
                GameOverGraveyardAction(f, game.ui.graveyard) ::
                GameOverDoneAction(f, GameOverTopAction)
            )))

        case GameOverMoveMapAction(f, _) =>
            game.ui.movements = game.ui.movements.not

            Force(GameOverMapsAction)

        case GameOverBattleMapAction(f, _) =>
            game.ui.battles = game.ui.battles.not

            Force(GameOverMapsAction)

        case GameOverNukeMapAction(f, _) =>
            game.ui.nukes = game.ui.nukes.not

            Force(GameOverMapsAction)

        case GameOverGraveyardAction(f, _) =>
            game.ui.graveyard = game.ui.graveyard.not

            Force(GameOverMapsAction)

        case _ => UnknownContinue
    }

}
