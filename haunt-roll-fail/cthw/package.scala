package object cthw extends hrf.base.Gaming with hrf.bot.BotGaming with hrf.base.SelectSubset with hrf.ui.GreyUI {
    type F = Faction
    type G = Game

    val gaming = this

    val styles = elem.styles
}
