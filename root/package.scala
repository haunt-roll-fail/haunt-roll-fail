package object root extends hrf.base.Gaming with hrf.bot.BotGaming with hrf.base.SelectSubset {
    type F = Player
    type G = Game

    val gaming = this

    val version = "beta"
    
    val styles = elem.styles
}
