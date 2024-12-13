package object arcs extends hrf.base.Gaming with hrf.bot.BotGaming with hrf.base.SelectSubset with hrf.ui.GreyMapUI with arcs.GameImplicits {
    type F = Faction
    type G = Game

    val gaming = this

    val styles = elem.styles
}
