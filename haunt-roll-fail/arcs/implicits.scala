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

trait GameImplicits {
    implicit def factionToPlayer(f : Faction)(implicit game : Game) = game.states(f)
    implicit def colorToPlayer(f : Color)(implicit game : Game) = f.as[Faction]./(f => game.states(f)).|!("no player of color")
    implicit def regionToContent(r : Region)(implicit game : Game) : $[Figure] = game.figures.get(r)
    implicit def cardLocationToContent(r : DeckCardLocation)(implicit game : Game) : $[DeckCard] = game.cards.get(r)
    implicit def courtLocationToContent(r : CourtLocation)(implicit game : Game) : $[CourtCard] = game.courtiers.get(r)
}
