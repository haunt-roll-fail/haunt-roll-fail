package object root extends hrf.base.Gaming with hrf.bot.BotGaming with hrf.base.SelectSubset with hrf.ui.GreyUI with root.GameImplicits {
    type F = Player
    type G = Game

    val gaming = this

    val styles = elem.styles
}
