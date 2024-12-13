package object yarg extends hrf.base.Gaming with hrf.bot.BotGaming with hrf.ui.GreyUI {
    type F = Faction
    type G = Game

    val gaming = this

    val styles = elem.styles
}
