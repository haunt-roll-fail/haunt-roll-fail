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
    implicit def factionToState(f : Faction)(implicit game : Game) : FactionState = game.states(f).as[FactionState].get
    implicit def blightsToState(f : Blights.type)(implicit game : Game) : BlightsState = game.states(f).as[BlightsState].get
    implicit def empireToState(f : Empire.type)(implicit game : Game) : EmpireState = game.states(f).as[EmpireState].get
    implicit def freeToState(f : Free.type)(implicit game : Game) : FreeState = game.states(f).as[FreeState].get
    implicit def colorToState(f : Color)(implicit game : Game) : ColorState = game.states(f)
    implicit def regionToContent(r : Region)(implicit game : Game) : $[Figure] = game.figures.get(r)
    implicit def cardLocationToContent(r : DeckCardLocation)(implicit game : Game) : $[DeckCard] = game.cards.get(r)
    implicit def courtLocationToContent(r : CourtLocation)(implicit game : Game) : $[CourtCard] = game.courtiers.get(r)

    def log(s : Any*)(implicit game : Game) {
        game.log(s : _*)
    }

    implicit class FactionEx(f : Color)(implicit game : Game) {
        def log(s : Any*) { if (game.logging) game.log((f +: s.$) : _*) }
    }

    implicit def descCard(g : Game, d : DeckCard) = d.img

    def options(implicit game : Game) = game.options
    def colors(implicit game : Game) = game.colors
    def factions(implicit game : Game) = game.factions
    def board(implicit game : Game) = game.board
    def systems(implicit game : Game) = game.board.systems
    def current(implicit game : Game) = game.current
    def campaign(implicit game : Game) = game.campaign
    def chapter(implicit game : Game) = game.chapter
    def round(implicit game : Game) = game.round
    def market(implicit game : Game) = game.market
    def court(implicit game : Game) = game.court
    def discourt(implicit game : Game) = game.discourt
    def deck(implicit game : Game) = game.deck
    def discard(implicit game : Game) = game.discard
    def lead(implicit game : Game) = game.lead
    def zeroed(implicit game : Game) = game.zeroed
    def seized(implicit game : Game) = game.seized


    implicit def cards(implicit game : Game) = game.cards
    implicit def courtiers(implicit game : Game) = game.courtiers
    implicit def figures(implicit game : Game) = game.figures



}
