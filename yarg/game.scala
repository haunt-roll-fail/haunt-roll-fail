package yarg

import colmat._

import hrf.tracker._

import hrf.elem._

object D6 extends Die[Int](List(0, 1, 2, 3, 4, 5))

trait Faction extends BasePlayer with Named with Styling {
    def rod : String
    def short : String
    def team : List[Piece]
}

abstract class Piece (
    val id : String,
    val k : Double,
    val name : String,
    val rod : String,
    val what : String,
    val obj : String,
    val maxHealth : Int,
    val maxMana : Int,
    val maxRage : Int,
    val initHealth : Int,
    val initMana : Int,
    val initRage : Int,
)

case class UnitRef(faction : Faction, index : Int) extends Record with GameElementary {
    def elem(g : Game) = g.of(faction).units(index).elem
}

class Figure(val faction : Faction, val unit : Piece, val index : Int, val rank : Option[Int]) extends Elementary {
    var health = unit.initHealth
    var mana = unit.initMana
    var rage = unit.initRage
    var reload = 0
    var stun = 0
    var dead = false

    def elem = unit.name.styled(faction) ~ rank./(n => " №" ~ (n + 1).elem)
    def ref = UnitRef(faction, index)
    def rod = unit.rod.styled(faction) ~ rank./(n => " №" ~ (n + 1).elem)
    def what = unit.what.styled(faction) ~ rank./(n => " №" ~ (n + 1).elem)
    def obj = unit.obj.styled(faction) ~ rank./(n => " №" ~ (n + 1).elem)
}

case object Captain extends Piece("captain", 0.6, "Капітан", "Капітана", "Капітана", "Капітаном", 8, 0, 7, 8, 0, 0)
case object Parrot extends Piece("parrot", 0.5, "Папуга", "Папуги", "Папугу", "Папугою", 3, 0, 7, 3, 0, 0)
case object Pirate extends Piece("pirate", 0.5, "Пірат", "Пірата", "Пірата", "Піратом", 10, 3, 0, 10, 3, 0)
case object CabinBoy extends Piece("cabin-boy", 0.7, "Юнга", "Юнги", "Юнгу", "Юнгою", 8, 6, 0, 8, 6, 0)

case object Pirates extends Faction {
    def name = "Пірати"
    def rod = "Піратів"
    def short = "DP"

    def team = List(Captain, Pirate, CabinBoy)
}

case object FireKnight extends Piece("fire-knight", 0.9, "Вогняний Лицар", "Вогняного Лицаря", "Вогняного Лицаря", "Вогняним Лицарем", 10, 10, 0, 10, 10, 0)

case object FireKnights extends Faction {
    def name = "Вогняні Лицарі"
    def rod = "Вогняних Лицарів"
    def short = "FK"

    def team = List(FireKnight, FireKnight)
}

case object Mage extends Piece("mage", 0.9, "Маг", "Мага", "Мага", "Магом", 8, 10, 0, 8, 10, 0)

case object Mages extends Faction {
    def name = "Маги"
    def rod = "Магів"
    def short = "MG"

    def team = List(Mage, Mage)
}

class Player(val f : Faction) {
    var units = f.team.zipWithIndex./{ case (p, n) => new Figure(f, p, n, (f.team.count(p) > 1).?(f.team.take(n).count(p))) }

}

object Desc {
    def apply(e : Elem) = HorizontalBreak ~ Div(e, xlo.nowrap, xstyles.smaller75)
}

case object StartAction extends StartGameAction
case class StartPlayerTurnAction(f : Faction) extends ForcedAction
case class StartUnitTurnAction(f : Faction) extends ForcedAction
case class UnitTurnAction(f : Faction, u : Piece, r : UnitRef) extends ForcedAction

case class CaptainHookMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Удар крюком".styled(self), "(" ~ "ціль " ~ elem.mod(-4) ~ " здоров'я".styled(styles.health) ~ ")") with Soft
case class CaptainHookAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Удар крюком".styled(self), "(" ~ "ціль " ~ elem.mod(-4) ~ " здоров'я".styled(styles.health) ~ ")")(t)
case class CaptainHookRollAction(self : Faction, r : UnitRef, t : UnitRef, roll : Int) extends OracleAction with RolledAction[Int] {
    def rolled = $(roll)
}

case class CaptainOrderMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Капітанський наказ".styled(self), Span("(всі супротивники " ~ elem.mod(-3) ~ " здоров'я".styled(styles.health) ~ ")", xlo.nowrap))


case class ParrotProvokeMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Дражнитися".styled(self), "(" ~ "+1".hl ~ " ".pre ~ "ярість".styled(styles.rage) ~ ", ціль " ~ "-1".hl ~ " здоров'я".styled(styles.health) ~ ")") with Soft
case class ParrotProvokeAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Дражнитися".styled(self), "(" ~ "+1".hl ~ " ".pre ~ "ярість".styled(styles.rage) ~ ", ціль " ~ "-1".hl ~ " здоров'я".styled(styles.health) ~ ")")(t)

case class ParrotTauntMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Кепкувати".styled(self), "(" ~ "+1".hl ~ " ".pre ~ "ярість".styled(styles.rage) ~ ", ціль " ~ "-1".hl ~ " мана".styled(styles.mana) ~ ")") with Soft
case class ParrotTauntAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Кепкувати".styled(self), "(" ~ "+1".hl ~ " ".pre ~ "ярість".styled(styles.rage) ~ ", ціль " ~ "-1".hl ~ " мана".styled(styles.mana) ~ ")")(t)

case class ParrotDistractMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Відволікати".styled(self), "(" ~ "+1".hl ~ " ".pre ~ "ярість".styled(styles.rage) ~ ", ціль " ~ "50%".hl ~ " пропуск ходу" ~ ")") with Soft
case class ParrotDistractAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Відволікати".styled(self), "(" ~ "+1".hl ~ " ".pre ~ "ярість".styled(styles.rage) ~ ", ціль " ~ "50%".hl ~ " пропуск ходу" ~ ")")(t)
case class ParrotDistractRollAction(self : Faction, r : UnitRef, t : UnitRef, roll : Int) extends OracleAction with RolledAction[Int] {
    def rolled = $(roll)
}

case class ParrotCurseMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Древнє Прокляття".styled(self), Span("(" ~ "-7".hl ~ " ".pre ~ "ярості".styled(styles.rage) ~ ",", xlo.nowrap) ~ " ".pre ~ "всі супротивники втрачають всю " ~ "ману".styled(styles.mana) ~ ")")

case class ParrotAvoidAttackRollAction(r : UnitRef, d : Int, then : ForcedAction, roll : Int) extends OracleAction with RolledAction[Int] {
    def rolled = $(roll)
}


case class PiratePistolMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Постріл з пістоля".styled(self), "(" ~ "ціль " ~ elem.mod(-1, 5) ~ " здоров'я".styled(styles.health) ~ ")") with Soft
case class PiratePistolAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Постріл з пістоля".styled(self), "(" ~ "ціль " ~ elem.mod(-1, 5) ~ " здоров'я".styled(styles.health) ~ ")")(t)
case class PiratePistolRollAction(self : Faction, r : UnitRef, t : UnitRef, roll : Int) extends OracleAction with RolledAction[Int] {
    def rolled = $(roll)
}

case class PirateReloadMainAction(self : Faction, r : UnitRef) extends BaseAction(r)("Перезарядити пістоль".styled(self))
case class PirateRumMainAction(self : Faction, r : UnitRef) extends BaseAction(r)("Випити ром".styled(self), "(" ~ elem.mod(4) ~ " здоров'я".styled(styles.health) ~ ", " ~ elem.mod(-1) ~ " ".pre ~ "мана".styled(styles.mana) ~ ")")

case class CabinBoySwordMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Удар шаблею".styled(self), "(" ~ "ціль " ~ elem.mod(-2) ~ " здоров'я".styled(styles.health) ~ ")") with Soft
case class CabinBoySwordAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Удар шаблею".styled(self), "(" ~ "ціль " ~ elem.mod(-2) ~ " здоров'я".styled(styles.health) ~ ")")(t)
case class CabinBoySwordRollAction(self : Faction, r : UnitRef, t : UnitRef, roll : Int) extends OracleAction with RolledAction[Int] {
    def rolled = $(roll)
}

case class CabinBoyPizzaMainAction(self : Faction, r : UnitRef) extends BaseAction(r)("З'їсти піццу".styled(self), "(" ~ elem.mod(-1) ~ " ".pre ~ "мана".styled(styles.mana) ~ ", " ~ elem.mod(2) ~ " здоров'я".styled(styles.health) ~ ")")

case class CabinBoyFeedPizzaMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Поділитися піццею".styled(self), "(" ~ elem.mod(-1) ~ " ".pre ~ "мана".styled(styles.mana) ~ ", ціль " ~ elem.mod(2) ~ " здоров'я".styled(styles.health) ~ ")") with Soft
case class CabinBoyFeedPizzaAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Поділитися піццею".styled(self), "(" ~ elem.mod(-1) ~ " ".pre ~ "мана".styled(styles.mana) ~ ", ціль " ~ elem.mod(2) ~ " здоров'я".styled(styles.health) ~ ")")(t)

case class FireKnightSwordMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Удар мечем".styled(self), "(" ~ elem.mod(-2) ~ " ".pre ~ "мани".styled(styles.mana) ~ ", ціль " ~ elem.mod(-1, 4) ~ " здоров'я".styled(styles.health) ~ ")") with Soft
case class FireKnightSwordAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Удар мечем".styled(self), "(" ~ elem.mod(-2) ~ " ".pre ~ "мани".styled(styles.mana) ~ ", ціль " ~ elem.mod(-1, 4) ~ " здоров'я".styled(styles.health) ~ ")")(t)
case class FireKnightSwordRollAction(self : Faction, r : UnitRef, t : UnitRef, roll : Int) extends OracleAction with RolledAction[Int] {
    def rolled = $(roll)
}

case class FireKnightFireballMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Вогняна куля".styled(self), "(" ~ elem.mod(-3) ~ " ".pre ~ "мани".styled(styles.mana) ~ ", ціль " ~ elem.mod(-3) ~ " здоров'я".styled(styles.health) ~ ")") with Soft
case class FireKnightFireballAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Вогняна куля".styled(self), "(" ~ elem.mod(-3) ~ " ".pre ~ "мани".styled(styles.mana) ~ ", ціль " ~ elem.mod(-3) ~ " здоров'я".styled(styles.health) ~ ")")(t)
case class FireKnightFireballRollAction(self : Faction, r : UnitRef, t : UnitRef, roll : Int) extends OracleAction with RolledAction[Int] {
    def rolled = $(roll)
}

case class FireKnightFirewallMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Стіна вогню".styled(self), Span("(" ~ elem.mod(-7) ~ " ".pre ~ "мани".styled(styles.mana) ~ ", всі супротивники " ~ elem.mod(-2) ~ " здоров'я".styled(styles.health) ~ ")", xlo.nowrap))

case class FireKnightSelfHealMainAction(self : Faction, r : UnitRef) extends BaseAction(r)("Вогняне Очищення".styled(self), "(" ~ elem.mod(3) ~ " здоров'я".styled(styles.health) ~ ", " ~ elem.mod(1) ~ " ".pre ~ "мани".styled(styles.mana) ~ ")")
case class FireKnightMeditateMainAction(self : Faction, r : UnitRef) extends BaseAction(r)("Вогняна Медитація".styled(self), "(" ~ elem.mod(1) ~ " здоров'я".styled(styles.health) ~ ", " ~ elem.mod(3) ~ " ".pre ~ "мани".styled(styles.mana) ~ ")")

case class MageFreezeMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Заморозити".styled(self), Desc("(" ~ elem.mod(-1) ~ " ".pre ~ "мана".styled(styles.mana) ~ ", ціль " ~ elem.mod(-1) ~ " здоров'я".styled(styles.health) ~ " та пропуск ходу)")) with Soft
case class MageFreezeAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Заморозити".styled(self), Desc("(" ~ elem.mod(-1) ~ " ".pre ~ "мана".styled(styles.mana) ~ ", ціль " ~ elem.mod(-1) ~ " здоров'я".styled(styles.health) ~ " та пропуск ходу)"))(t)
case class MageFreezeRollAction(self : Faction, r : UnitRef, t : UnitRef, roll : Int) extends OracleAction with RolledAction[Int] {
    def rolled = $(roll)
}

case class MageRayMainAction(self : Faction, r : UnitRef, l : List[UnitRef]) extends BaseAction(r)("Енерго-промінь".styled(self), Desc("(" ~ elem.mod(-2) ~ " ".pre ~ "мани".styled(styles.mana) ~ ", ціль " ~ elem.mod(-4) ~ " здоров'я".styled(styles.health) ~ ")")) with Soft
case class MageRayAction(self : Faction, r : UnitRef, t : UnitRef) extends BaseAction("Енерго-промінь".styled(self), Desc("(" ~ elem.mod(-2) ~ " ".pre ~ "мани".styled(styles.mana) ~ ", ціль " ~ elem.mod(-4) ~ " здоров'я".styled(styles.health) ~ ")"))(t)
case class MageRayRollAction(self : Faction, r : UnitRef, t : UnitRef, roll : Int) extends OracleAction with RolledAction[Int] {
    def rolled = $(roll)
}

case class MageSelfHealMainAction(self : Faction, r : UnitRef) extends BaseAction(r)("Воскресіння".styled(self), Desc("(" ~ elem.mod(-1) ~ " ".pre ~ "мана".styled(styles.mana) ~ ", " ~ "подвоїти".hl ~ " ".pre ~ "здоров'я".styled(styles.health) ~ ")"))

case class MageManaReserveMainAction(self : Faction, r : UnitRef) extends BaseAction(r)("Резерв мани".styled(self), Desc("(стає " ~ "1".hl ~ " " ~ "здоров'я".styled(styles.health) ~ ", " ~ "повна".hl ~ " " ~ "мана".styled(styles.mana) ~ ", повторний хід)"))

case class SkipUnitTurnAction(self : Faction, r : UnitRef) extends BaseAction(None)("Пропустити хід")

case class EndUnitTurnAction(f : Faction) extends ForcedAction
case class CleanUnitTurnAction(self : Faction) extends BaseAction(None)("Продовжити")

case class AttackDamageAction(r : UnitRef, d : Int, then : ForcedAction) extends ForcedAction
case class UnavoidableDamageAction(r : UnitRef, d : Int, then : ForcedAction) extends ForcedAction

case class AttackManaAction(r : UnitRef, d : Int, then : ForcedAction) extends ForcedAction
case class AttackStunAction(r : UnitRef, then : ForcedAction) extends ForcedAction

            
case class GameOverAction(l : List[Faction]) extends ForcedAction

trait GameOverBaseInfo extends Info { self : UserAction =>
    def question(g : Game) = "Кінець Гри"
}

case class GameOverWonAction(self : Faction, f : Faction) extends OptionInfo(f, "перемогли!!!", "(" ~ NameReference(f.name, f).hl ~ ")") with GameOverBaseInfo
case class GameOverDrawAction(self : Faction) extends OptionInfo("Нічия, ніхто не переміг") with GameOverBaseInfo



case object CancelAction extends ElemAction(Empty)("Назад") with Cancel

class Game(val setup : List[Faction], val logging : Boolean) extends BaseGame with ContinueGame with LoggedGame {
    type F = Faction

    var players = Map[Faction, Player]()
    var factions = List[Faction]()
    var current : Faction = null
    var active : Figure = null
    var damaged : List[Figure] = Nil

    
    var turn : Int = 0
    var unit : Int = 0

    var firewall : Int = 0
   
    def of(f : Faction) = players(f)

    implicit def faction2player(f : Faction) = of(f)
    implicit def unitref2figure(r : UnitRef) = of(r.faction).units(r.index)
    implicit def figure2unitref(r : Figure) = r.ref
    implicit def figures2unitrefs(l : List[Figure]) = l./(_.ref)

    implicit class UnitRefEx(val r : UnitRef) {
        def figure = of(r.faction).units(r.index)
    }

    implicit class ActionsEx(val l : List[UserAction]) {
        def cancel : List[UserAction] = l :+ CancelAction
    }

    

    def loggedPerform(action : Action) : Continue = {
        println("> " + action)
        
        val c = performZ(action)
        
        c match {
            case Ask(_, Nil, _) =>
                println("")
                println("")
                println("")
                println("WTF!!!")
                println("Empty Ask as a result of " + action)
            case _ =>
        }

        println("    < " + c)

        c
    }
    
    def info(waiting : List[Faction], self : Option[Faction], actions : List[UserAction]) : List[UserAction with Info] = {
        Nil
    }

    def preinfo(waiting : List[Faction], self : Option[Faction], actions : List[UserAction]) : List[UserAction with Info] = Nil

    def performZ(a : Action) : Continue = {
        println("action: " + a)

        implicit val action = a
        
        action match {
            // HELPERS
            case RefuseAction(then) =>
                then
    
            case DoneAction(then) =>
                then
                    
            // INIT
            case StartAction =>
                log(setup(0), " напали на", setup(1).rod.styled(setup(1)))

                setup.foreach { f =>
                    players += f -> new Player(f)
                    factions :+= f
                }

                StartPlayerTurnAction(factions(0))
                
            case StartPlayerTurnAction(f) =>
                log(DoubleLine)
                turn += 1
                unit = 0
                current = f
                log("Хід", ("#" + turn).hl, "-", f)
                
                StartUnitTurnAction(f)

            case StartUnitTurnAction(f) if unit >= f.units.num =>
                factions = factions.drop(1) ++ factions.take(1)

                StartPlayerTurnAction(factions(0))

            case StartUnitTurnAction(f) if f.units(unit).dead =>
                unit += 1
                
                StartUnitTurnAction(f)

            case StartUnitTurnAction(f) if f.units(unit).stun > 0 =>
                active = f.units(unit)

                log(active, "приходить до тями")
                
                EndUnitTurnAction(f)

            case StartUnitTurnAction(f) =>
                active = f.units(unit)

                logtemp(active, "ходить...")
                
                UnitTurnAction(f, active.unit, UnitRef(f, unit))
 
            // CAPTAIN
            case UnitTurnAction(f, Captain, r) =>
                val t = factions.but(f)./~(_.units).%!(_.dead)
                val a = f.units.%!(_.dead).but(r.figure)

                Ask(f, (
                    CaptainHookMainAction(f, r, t).x(t.none) :: 
                    CaptainOrderMainAction(f, r, t).x(t.none).x(r.reload % 2 == 1, "тільки один раз") ::
                    SkipUnitTurnAction(f, r)
                ).okNeeded)
            
            case CaptainHookMainAction(f, r, l) =>
                Ask(f, l./(CaptainHookAction(f, r, _)).cancel)
 
            case CaptainHookAction(f, r, t) =>
                log(r, "б'є крюком", t.what)
 
                Roll[Int]($(D6), l => CaptainHookRollAction(f, r, t, l.single.get))
                
            case CaptainHookRollAction(f, r, t, x) =>
                if (x > 2) {
                    AttackDamageAction(t, 4, EndUnitTurnAction(f))
                }
                else {
                    log(r, "промахнувся")
                    EndUnitTurnAction(f)
                }

            case CaptainOrderMainAction(f, r, l) =>
                log(r, "віддає", "Капітанський наказ".styled(f))

                r.reload += 1
                
                var q : ForcedAction = EndUnitTurnAction(f)
                
                l.foreach { t =>
                    q = AttackDamageAction(t, 3, q)
                }
                
                q
            
            case AttackDamageAction(r, d, then) if r.unit == Captain =>
                r.rage += 1

                log(r, "+1".hl ~ " ярість".styled(styles.rage))

                UnavoidableDamageAction(r, d, then)

            // PARROT
            case UnitTurnAction(f, Parrot, r) =>
                val t = factions.but(f)./~(_.units).%!(_.dead)

                Ask(f, (
                    ParrotProvokeMainAction(f, r, t).x(t.none) :: 
                    ParrotTauntMainAction(f, r, t).x(t.none) :: 
                    ParrotDistractMainAction(f, r, t).x(t.none) :: 
                    ParrotCurseMainAction(f, r, t).x(t.none).x(r.reload > 0, "тільки один раз").x(r.rage < 7, "недостатньо ярості") ::
                    SkipUnitTurnAction(f, r)
                ).okNeeded)
                
            case ParrotProvokeMainAction(f, r, l) =>
                Ask(f, l./(ParrotProvokeAction(f, r, _)).cancel)
 
            case ParrotProvokeAction(f, r, t) =>
                log(r, "дражнив", t.rod)

                r.rage += 1

                log(r, "+1".hl ~ " ярість".styled(styles.rage))
                
                AttackDamageAction(t, 1, EndUnitTurnAction(f))

            case ParrotTauntMainAction(f, r, l) =>
                Ask(f, l./(ParrotTauntAction(f, r, _)).cancel)
 
            case ParrotTauntAction(f, r, t) =>
                log(r, "кепкував з", t.rod)

                r.rage += 1

                log(r, "+1".hl ~ " ярість".styled(styles.rage))
                
                AttackManaAction(t, 1, EndUnitTurnAction(f))

            case ParrotDistractMainAction(f, r, l) =>
                Ask(f, l./(ParrotDistractAction(f, r, _)).cancel)
 
            case ParrotDistractAction(f, r, t) =>
                logtemp(r, "відволікає", t.what ~ "...")
 
                Roll[Int]($(D6), l => ParrotDistractRollAction(f, r, t, l.single.get))
                
            case ParrotDistractRollAction(f, r, t, x) =>
                r.rage += 1
                
                log(r, "+1".hl ~ " ярість".styled(styles.rage))
                
                if (x < 3) {
                    log(r, "не зумів відволікти", t.what)
                    EndUnitTurnAction(f)
                }
                else {
                    log(r, "відволік", t.what)

                    AttackStunAction(t, EndUnitTurnAction(f))
                }

            case ParrotCurseMainAction(f, r, l) =>
                log(r, "кричить", "Древнє Прокляття".styled(f))

                r.rage = 0
                r.reload = 1

                l.foreach { t =>
                    if (t.mana > 0) {
                        t.mana = 0
                        damaged :+= t
                        log(t, "втратив всю", "ману".styled(styles.mana))
                    }
                }
                
                EndUnitTurnAction(f)
 
            case AttackDamageAction(r, d, then) if r.unit == Parrot && r.stun == 0 =>
                Roll[Int]($(D6), l => ParrotAvoidAttackRollAction(r, d, then, l.single.get))
  
            case ParrotAvoidAttackRollAction(r, d, then, 0) =>
                UnavoidableDamageAction(r, d, then)
                
            case ParrotAvoidAttackRollAction(r, d, then, x) if x < 3 =>
                if (d > 1)
                    log(r, "злетів, тільки пір'я зачепило")

                UnavoidableDamageAction(r, 1, then)
                
            case ParrotAvoidAttackRollAction(r, d, then, _) =>
                log(r, "ухилився від удару")
                
                then match {
                    case AttackStunAction(_, then) => then
                    case _ => then
                }
                
            // PIRATE
            case UnitTurnAction(f, Pirate, r) =>
                val t = factions.but(f)./~(_.units).%!(_.dead)

                Ask(f, (
                    PiratePistolMainAction(f, r, t).x(t.none).x(r.reload > 0, "пістоль розряжений") :: 
                    PirateReloadMainAction(f, r).x(t.none).x(r.reload <= 0) :: 
                    PirateRumMainAction(f, r).x(r.mana <= 0, "замало мани").x(r.health >= r.unit.maxHealth, "повне здоров'я") :: 
                    SkipUnitTurnAction(f, r)
                ).okNeeded)
                
            case PiratePistolMainAction(f, r, l) =>
                Ask(f, l./(PiratePistolAction(f, r, _)).cancel)
 
            case PiratePistolAction(f, r, t) =>
                log(r, "стріляє в", t.what)
 
                r.reload += 1

                Roll[Int]($(D6), l => PiratePistolRollAction(f, r, t, l.single.get))
                
            case PiratePistolRollAction(f, r, t, x) =>
                val d = x

                if (d > 0) {
                    AttackDamageAction(t, d, EndUnitTurnAction(f))
                }
                else {
                    log(r, "промахнувся")
                    EndUnitTurnAction(f)
                }

            case PirateReloadMainAction(f, r) =>
                r.reload = 0

                log(r, "перезарядив пістоль")

                EndUnitTurnAction(f)

            case PirateRumMainAction(f, r) =>
                log(r, "випив ром")
                
                r.mana -= 1
                log(r, elem.mod(-1) ~ " мана".styled(styles.mana))

                r.health += 4
                log(r, elem.mod(+4) ~ " здоров'я".styled(styles.health))

                EndUnitTurnAction(f)

            // CABIN BOY
            case UnitTurnAction(f, CabinBoy, r) =>
                val t = factions.but(f)./~(_.units).%!(_.dead)
                val a = f.units.%!(_.dead).but(r.figure)

                Ask(f, (
                    CabinBoySwordMainAction(f, r, t).x(t.none) :: 
                    CabinBoyPizzaMainAction(f, r).x(r.mana <= 0, "замало мани").x(r.health >= r.unit.maxHealth, "повне здоров'я") :: 
                    CabinBoyFeedPizzaMainAction(f, r, a).x(a.none).x(r.mana <= 0, "замало мани").x(a.all(r => r.health >= r.unit.maxHealth), "повне здоров'я") :: 
                    SkipUnitTurnAction(f, r)
                ).okNeeded)
                
            case CabinBoySwordMainAction(f, r, l) =>
                Ask(f, l./(CabinBoySwordAction(f, r, _)).cancel)
 
            case CabinBoySwordAction(f, r, t) =>
                log(r, "б'є шаблею", t.what)
 
                Roll[Int]($(D6), l => CabinBoySwordRollAction(f, r, t, l.single.get))
                
            case CabinBoySwordRollAction(f, r, t, x) =>
                if (x > 2) {
                    AttackDamageAction(t, 2, EndUnitTurnAction(f))
                }
                else {
                    log(r, "промахнувся")
                    EndUnitTurnAction(f)
                }

            case CabinBoyPizzaMainAction(f, r) =>
                log(r, "з'їв шматочок піцци")
                
                r.mana -= 1
                log(r, elem.mod(-1) ~ " мана".styled(styles.mana))

                r.health += 2
                log(r, elem.mod(+2) ~ " здоров'я".styled(styles.health))

                EndUnitTurnAction(f)

            case CabinBoyFeedPizzaMainAction(f, r, l) =>
                Ask(f, l./(CabinBoyFeedPizzaAction(f, r, _)).cancel)

            case CabinBoyFeedPizzaAction(f, r, t) =>
                log(r, "поділився шматочком піцци з", t.obj)
                
                r.mana -= 1
                log(r, elem.mod(-1) ~ " мана".styled(styles.mana))

                t.health += 2
                log(t, elem.mod(+2) ~ " здоров'я".styled(styles.health))

                EndUnitTurnAction(f)

 
            // FIRE KNIGHT
            case UnitTurnAction(f, FireKnight, r) =>
                val t = factions.but(f)./~(_.units).%!(_.dead)

                Ask(f, (
                    FireKnightSwordMainAction(f, r, t).x(t.none).x(r.mana < 2, "замало мани") :: 
                    FireKnightFireballMainAction(f, r, t).x(t.none).x(r.mana < 3, "замало мани") ::
                    FireKnightFirewallMainAction(f, r, t).x(t.none).x(firewall == turn, "стіна вогню вже стоїть").x(r.mana < 7, "замало мани") ::
                    FireKnightSelfHealMainAction(f, r).x(r.mana >= r.unit.maxMana && r.health >= r.unit.maxHealth, "повна мана і здров'я") ::
                    FireKnightMeditateMainAction(f, r).x(r.mana >= r.unit.maxMana && r.health >= r.unit.maxHealth, "повна мана і здров'я") ::
                    SkipUnitTurnAction(f, r)
                ).okNeeded)
                
            case FireKnightSwordMainAction(f, r, l) =>
                Ask(f, l./(FireKnightSwordAction(f, r, _)).cancel)
 
            case FireKnightSwordAction(f, r, t) =>
                log(r, "б'є вогняним мечем", t.what)
 
                r.mana -= 2
                log(r, elem.mod(-2) ~ " мани".styled(styles.mana))
                
                Roll[Int]($(D6), l => FireKnightSwordRollAction(f, r, t, l.single.get))
                
            case FireKnightSwordRollAction(f, r, t, x) =>
                val d = x match {
                    case 5 => 4
                    case 0 => 0
                    case _ => 1
                }

                if (d > 0) {
                    AttackDamageAction(t, d, EndUnitTurnAction(f))
                }
                else {
                    log(r, "промахнувся")
                    EndUnitTurnAction(f)
                }

            case FireKnightFireballMainAction(f, r, l) =>
                Ask(f, l./(FireKnightFireballAction(f, r, _)).cancel)
 
            case FireKnightFireballAction(f, r, t) =>
                log(r, "запускає вогняний шар у", t.what)
 
                r.mana -= 3
                log(r, elem.mod(-3) ~ " мани".styled(styles.mana))

                Roll[Int]($(D6), l => FireKnightFireballRollAction(f, r, t, l.single.get))
                
            case FireKnightFireballRollAction(f, r, t, x) =>
                val d = (x > 1).??(3)

                if (d > 0) {
                    AttackDamageAction(t, d, EndUnitTurnAction(f))
                }
                else {
                    log(r, "промахнувся")
                    EndUnitTurnAction(f)
                }
                
            case FireKnightFirewallMainAction(f, r, l) =>
                log(r, "ставить", "Стіну Вогню".styled(f))

                r.mana -= 7
                log(r, elem.mod(-7) ~ " мани".styled(styles.mana))

                firewall = turn
                
                var q : ForcedAction = EndUnitTurnAction(f)
                
                l.foreach { t =>
                    q = AttackDamageAction(t, 2, q)
                }
                
                q

            case FireKnightSelfHealMainAction(f, r) =>
                r.health += 3
                r.mana += 1
                
                log(r, "очистився")
                
                log(r, elem.mod(+3) ~ " здоров'я".styled(styles.health))
                log(r, elem.mod(+1) ~ " мана".styled(styles.mana))

                EndUnitTurnAction(f)

            case FireKnightMeditateMainAction(f, r) =>
                r.health += 1
                r.mana += 3 

                log(r, "медитував")
                
                log(r, elem.mod(+1) ~ " здоров'я".styled(styles.health))
                log(r, elem.mod(+3) ~ " мана".styled(styles.mana))

                EndUnitTurnAction(f)

            // MAGE
            case UnitTurnAction(f, Mage, r) =>
                val t = factions.but(f)./~(_.units).%!(_.dead)

                Ask(f, (
                    MageFreezeMainAction(f, r, t).x(t.none).x(r.mana < 1, "замало мани") :: 
                    MageRayMainAction(f, r, t).x(t.none).x(r.mana < 2, "замало мани") :: 
                    MageSelfHealMainAction(f, r).x(r.mana < 1, "замало мани").x(r.health >= r.unit.maxHealth, "повне здров'я") ::
                    MageManaReserveMainAction(f, r).x(r.reload > 0, "тільки один раз").x(r.mana >= r.unit.maxMana, "повна мана") ::
                    SkipUnitTurnAction(f, r)
                ).okNeeded)
                
            case MageFreezeMainAction(f, r, l) =>
                Ask(f, l./(MageFreezeAction(f, r, _)).cancel)
 
            case MageFreezeAction(f, r, t) =>
                logtemp(r, "заморожує", t.what, "...")
 
                r.mana -= 1
                log(r, elem.mod(-1) ~ " мана".styled(styles.mana))

                Roll[Int]($(D6), l => MageFreezeRollAction(f, r, t, l.single.get))
                
            case MageFreezeRollAction(f, r, t, x) =>
                if (x < 1) {
                    log(r, "не зумів заморозити", t.what)
                    EndUnitTurnAction(f)
                }
                else {
                    log(r, "заморозив", t.what)

                    AttackDamageAction(t, 1, AttackStunAction(t, EndUnitTurnAction(f)))
                }

            case MageRayMainAction(f, r, l) =>
                Ask(f, l./(MageRayAction(f, r, _)).cancel)
 
            case MageRayAction(f, r, t) =>
                log(r, "б'є енергією", t.what)
 
                r.mana -= 2
                log(r, elem.mod(-2) ~ " мани".styled(styles.mana))

                Roll[Int]($(D6), l => MageRayRollAction(f, r, t, l.single.get))
                
            case MageRayRollAction(f, r, t, x) =>
                if (x < 2) {
                    log(r, "промахнувся")
                    EndUnitTurnAction(f)
                }
                else {
                    AttackDamageAction(t, 4, EndUnitTurnAction(f))
                }
                
            case MageSelfHealMainAction(f, r) =>
                log(r, "воскресає")
                
                r.mana -= 1
                log(r, elem.mod(-1) ~ " мана".styled(styles.mana))
 
                val d = r.health
                r.health += d
                
                log(r, elem.mod(+d) ~ " здоров'я".styled(styles.health))

                EndUnitTurnAction(f)
 
            case MageManaReserveMainAction(f, r) =>
                log(r, "черпає внутрішні резерви мани")

                r.health = 1
                log("У", r.rod, "стало", 1.hl, "здоров'я".styled(styles.health))

                r.mana = r.unit.maxMana
                log("У", r.rod, "стала", "повна", "мана".styled(styles.mana))

                r.reload += 1

                log(r, "ходить знов")

                StartUnitTurnAction(f)

            // DEFAULT
            case AttackDamageAction(r, d, then) =>
                UnavoidableDamageAction(r, d, then)
                
            case UnavoidableDamageAction(r, d, then) =>
                r.health -= d
                
                damaged :+= r
                
                log(r, elem.mod(-d) ~ " здоров'я".styled(styles.health))

                then

            case AttackManaAction(r, d, then) =>
                damaged :+= r
 
                r.mana -= d
                log(r, elem.mod(-d) ~ " " ~ (d == 1).?("мана").|("мани").styled(styles.health))

                then
                
            case AttackStunAction(r, then) =>
                r.stun += 1
                
                damaged :+= r
 
                log(r, "пропустить хід")
                
                then

            case UnitTurnAction(f, u, r) =>
                Ask(f, $(SkipUnitTurnAction(f, r)).okNeeded)

            case SkipUnitTurnAction(f, r) =>
                log(r, "пропустив хід")

                EndUnitTurnAction(f)

            case EndUnitTurnAction(f) =>
                factions.foreach { f =>
                    f.units.foreach { r =>
                        if (r.health <= 0 && r.dead.not) {
                            log(r.what, "здолано")
                            
                            if (r.unit == Captain) {
                                f.units = f.units./{
                                    case x if x == r => {
                                        val a = new Figure(r.faction, Parrot, r.index, r.rank)
                                        a.rage = r.rage
                                        log(a, "заміняє", r.what)
                                        a
                                    }
                                    case x => x
                                }
                            }

                            r.dead = true
                            r.health = 0
                            r.mana = 0
                            r.rage = 0
                        }
 
                        r.health = max(0, min(r.health, r.unit.maxHealth))
                        r.mana = max(0, min(r.mana, r.unit.maxMana))
                        r.rage = max(0, min(r.rage, r.unit.maxRage))
                    }
                }
                
                val alive = factions.%(_.units.%!(_.dead).any)

                if (alive.num <= 1) {
                    GameOverAction(alive)
                }
                else {
                    unit += 1
                
                    Ask(f, $(CleanUnitTurnAction(f)))
                }

            case CleanUnitTurnAction(f) =>
                if (active.stun > 0)
                    active.stun -= 1

                active = null
                damaged = Nil

                DelayedContinue(10, StartUnitTurnAction(f))

            case GameOverAction(winners) =>
                 GameOver(winners, "Game Over" ~ Break ~ winners./(f => f.elem ~ " перемогли" ~ Break).join(Break), (null +: Nil)./(f => {
                    Ask(f, winners./~(w => 
                        $(GameOverWonAction(f, w))
                        )
                    )
                }))
        }
    }
}