package object dwam extends hrf.base.Gaming with hrf.bot.BotGaming with hrf.base.SelectSubset with hrf.ui.GreyMapUI {
    type F = Faction
    type G = Game

    val gaming = this

    val styles = elem.styles
}
