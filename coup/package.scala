package object coup extends hrf.base.Gaming with hrf.bot.BotGaming with hrf.base.SelectSubset {
    type F = Faction
    type G = Game

    val gaming = this
    
    val styles = elem.styles
}
