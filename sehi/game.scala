package sehi

import colmat._

import hrf.tracker._

import hrf.elem._

trait Party extends Record with Named with Styling {
    def name = toString
    def pl = name + "s"
    def img : Image
}

case object Fascist extends Party {
    def img = Image("party-fascist", styles.card)
}

case object Liberal extends Party {
    def img = Image("party-liberal", styles.card)
}

trait Role extends Record with Elementary {
    def party : Party
    def img : Image
}

case object Hitler extends Role {
    val party = Fascist
    def img = Image("hitler", styles.card)
    def elem = this.toString.styled(Fascist)
}

case class Citizen(party : Party, image : String) extends Role {
    def img = party match {
        case Fascist => Image("fascist-" + image, styles.card)
        case Liberal => Image("liberal-" + image, styles.card)
    }
    def elem = party.elem
}

object Roles {
    def fascists = 
        Citizen(Fascist, "a") :: 
        Citizen(Fascist, "b") :: 
        Citizen(Fascist, "c")
                  
    def liberals = 
        Citizen(Liberal, "1") :: 
        Citizen(Liberal, "2") :: 
        Citizen(Liberal, "3") :: 
        Citizen(Liberal, "4") :: 
        Citizen(Liberal, "5") :: 
        Citizen(Liberal, "6")
}

case class Card(party : Party, text : String, image : String) extends Record with Elementary {
    def img = party match {
        case Fascist => Image("fascist-article", styles.card)
        case Liberal => Image("liberal-article-" + image, styles.card)
    }
    def elem = text.styled(party)
}

object Deck {
    val full = 
        Card(Liberal, "Habeas Corpus", "habeas-corpus") ::
        Card(Liberal, "Right to Work", "right-to-work") ::
        Card(Liberal, "School Choice", "school-choice") ::
        Card(Liberal, "Jury Trial", "jury-trial") ::
        Card(Liberal, "Right to Bear Arms", "right-to-bear-arms") ::
        Card(Liberal, "Stand Your Ground", "stand-your-ground") ::
        Card(Fascist, "Mass Surveillance", "A" + "") :: 
        Card(Fascist, "Mandatory Medical Procedures", "B") :: 
        Card(Fascist, "Abortions Ban", "C") :: 
        Card(Fascist, "Asset Forfeiture", "D") :: 
        Card(Fascist, "Concentration Camps", "E") :: 
        Card(Fascist, "Affirmative Action", "F") :: 
        Card(Fascist, "Martial Law", "G") :: 
        Card(Fascist, "Extrajudicial Punishment", "H") :: 
        Card(Fascist, "Censorship", "I") :: 
        Card(Fascist, "Forced Disappearances", "J") :: 
        Card(Fascist, "Punitive Psychiatry", "K")
}

sealed trait Vote extends Record with Elementary {
    def wide : Elem
}

case object Yes extends Vote {
    def elem = " Yes ".pre.styled(styles.yes, styles.vote)
    def wide = Div(Text("Yes"), styles.yes, styles.widevote)
}

case object No extends Vote {
    def elem = " No ".pre.styled(styles.no, styles.vote)
    def wide = Div(Text("No"), styles.no, styles.widevote)
}

sealed trait Title extends Elementary

case object President extends Title {
    def elem = (" " + this + " ").pre(styles.president)(styles.title)
}

case object Chancellor extends Title {
    def elem = (" " + this + " ").pre(styles.chancellor)(styles.title)
}

trait Faction extends BasePlayer with Named with Styling {
    def name = toString
    def short = name.take(1)
    def style = name.toLowerCase
    def ss = name.take(2).styled(this)
}

case object Red extends Faction
case object Green extends Faction
case object Blue extends Faction
case object Yellow extends Faction
case object White extends Faction
case object Purple extends Faction
case object Azure extends Faction
case object Lime extends Faction
case object Orange extends Faction
case object Electric extends Faction



trait ViewCard { self : UserAction => }

case object StartAction extends StartGameAction
case class ShuffledLiberalsAction(shuffled : List[Citizen]) extends ShuffledAction[Citizen]
case class ShuffledFascistsAction(ll : List[Citizen], shuffled : List[Citizen]) extends ShuffledAction[Citizen]
case class ShuffledRolesAction(shuffled : List[Role]) extends ShuffledAction[Role]

case class CheckDeckAction(then : ForcedAction) extends ForcedAction
case class ShuffledDeckAction(shuffled : List[Card], then : ForcedAction) extends ShuffledAction[Card]
case object TurnStartAction extends ForcedAction

case class NominateChancellorAction(self : Faction, f : Faction) extends BaseAction("Nominate Chancellor")(f)

case class ViewRoleInfoAction(self : Faction, f : Faction, role : Role) extends BaseInfo("")(role.img ~ " " ~ f.elem ~ Break ~ (f == self).?("You").|("Ally")) with ViewCard

case class VoteAction(self : Faction, p : Faction, c : Faction, vote : Vote) extends BaseAction("Vote for", p, "and", c)(vote.wide) with Retry

case class VotingAction(p : Faction, c : Faction) extends ForcedAction
case class TallyAction(p : Faction, c : Faction) extends ForcedAction
case class LegislationAction(p : Faction, c : Faction) extends ForcedAction
case class SelectPolicyAction(self : Faction, a : Card, m : String, then : ForcedAction) extends BaseAction(m)(a.img) with ViewCard
case class PresidentialPolicyAction(p : Faction, c : Faction) extends ForcedAction
case class PresidentialSecondPolicyAction(p : Faction, c : Faction, a : Card) extends ForcedAction
case class PresidentialSelectedAction(p : Faction, c : Faction, a : Card, b : Card) extends ForcedAction
case class ChancellorPolicyAction(p : Faction, c : Faction) extends ForcedAction
case class ChancellorPolicyVetoAction(p : Faction, c : Faction) extends ForcedAction
case class ChancellorSelectedAction(p : Faction, c : Faction, a : Card) extends ForcedAction

case class ChancellorVetoAction(p : Faction, self : Faction) extends BaseAction("Veto power")("Veto")

case class PresidentMainVetoAction(p : Faction, c : Faction) extends ForcedAction
case class PresidentVetoAction(self : Faction, c : Faction) extends BaseAction("Veto power")("Confirm Veto")
case class PresidentDenyVetoAction(self : Faction, c : Faction) extends BaseAction("Veto power")("Deny Veto")


case class PolicyPeekAction(p : Faction) extends ForcedAction
case class SpecialElectionAction(p : Faction) extends ForcedAction
case class InvestigateAction(p : Faction) extends ForcedAction
case class ExecuteAction(p : Faction) extends ForcedAction

case class ViewPolicyAction(self : Faction, a : Card) extends BaseInfo("Next policies")(a.img) with ViewCard

case class ExecuteFactionAction(self : Faction, f : Faction) extends BaseAction("Execute")(f) with Soft
case class ExecuteFactionConfirmAction(self : Faction, f : Faction) extends BaseAction("Execute", f)("Kill".styled(styles.kill))

case class ExecutedAction(self : Faction, p : Faction) extends BaseInfo("You were executed by " ~ p.elem)("R.I.P.")


case class NominateNextPresidentAction(self : Faction, f : Faction) extends BaseAction("Nominate next", President)(f)

case class InvestigateFactionAction(self : Faction, f : Faction) extends BaseAction("Investigate")(f)

case class ViewPartyAction(self : Faction, f : Faction, party : Party) extends BaseInfo(f, "affiliation")(party.img) with ViewCard

case class GameOverAction(party : Party, m : Elem) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won", "(" ~ NameReference(f.name, f).hl ~ ")")

case object RandomPolicyAction extends ForcedAction

class Game(val setup : List[Faction], val logging : Boolean) extends BaseGame with ContinueGame with LoggedGame {
    type F = Faction

    var roles = Map[Faction, Role]()
    val factions = setup
    var alive = factions
    var candidates = factions ++ factions ++ factions ++ factions ++ factions ++ factions ++ factions

    var friends = factions./(f => f -> List[Faction]()).toMap
    var enemies = factions./(f => f -> List[Faction]()).toMap
    var karma = factions./(f => f -> 0).toMap
    
    val cards = new ValueTracker[Card]

    val pile = cards.another[Card]("pile", Deck.full)
    val drawn = cards.another[Card]("drawn")
    val deck = cards.another[Card]("deck")
    val liberal = cards.another[Card]("liberal")
    val fascist = cards.another[Card]("fascist")
    
    var frustration = 0
    var limited = factions.take(0)
    var investigated = factions.take(0)
    var votes = Map[Faction, Vote]()
    var over = false

    var president : Faction = null
    var chancellor : Faction = null
    
    implicit class SourceEx(source : Location[Card]) {
        def -->(l : List[Card]) = new SourceCardsEx(source, l)
        def -->(d : Card) = new SourceCardEx(source, d)
        def -->(dest : Location[Card]) = new SourceCardsEx(source, cards.get[Card](source)) --> dest
    }
      
    class SourceCardsEx(source : Location[Card], l : List[Card]) {
        def -->(dest : Location[Card]) {
            l.foreach(new SourceCardEx(source, _) --> dest)
        }
    }
      
    class SourceCardEx(source : Location[Card], d : Card) {
        def -->(dest : Location[Card]) {
            cards.move(source, d, dest)
        }
    }
      
    var turn : Int = 0
   
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

    def knows(f : Faction) = f +: (roles(f).party == Fascist && (setup.num <= 6 || roles(f) != Hitler)).??(setup.%(roles(_).party == Fascist)).but(f)
    def info(waiting : List[Faction], self : Option[Faction], actions : List[UserAction]) : List[UserAction with Info] = {
        roles.any.??(self./~{ self =>
            knows(self)./(f => ViewRoleInfoAction(self, f, roles(f)))
        })
    }

    def performZ(a : Action) : Continue = {
        println("action: " + a)

        implicit val action = a
        
        action match {
            // HELPERS
            case BackAction(then) =>
                then

            case DoneAction(then) =>
                then
                    
            // INIT
            case StartAction =>
                Shuffle(Roles.liberals, ll => ShuffledLiberalsAction(ll), "Shuffle liberals")
                
            case ShuffledLiberalsAction(ll) =>
                Shuffle(Roles.fascists, ff => ShuffledFascistsAction(ll, ff), "Shuffle fascists")

            case ShuffledFascistsAction(liberals, fascists) =>
                val roles = Hitler +: (factions.num match {
                    case 5 => liberals.take(3) ++ fascists.take(1)
                    case 6 => liberals.take(4) ++ fascists.take(1)
                    case 7 => liberals.take(4) ++ fascists.take(2)
                    case 8 => liberals.take(5) ++ fascists.take(2)
                    case 9 => liberals.take(5) ++ fascists.take(3)
                    case 10 => liberals.take(6) ++ fascists.take(3)
                    case _ => liberals.take(factions.num - 2) ++ fascists.take(1) 
                })
                
                Shuffle(roles, rr => ShuffledRolesAction(rr), "Shuffle roles")

            case ShuffledRolesAction(rr) =>
                log("Shuffled roles")
                
                factions.lazyZip(rr).foreach { (f, r) =>
                    roles += f -> r
                }

                factions.foreach { f =>
                    val role = roles(f)
                    
                    if (role.party == Fascist && (setup.num <= 6 || role != Hitler)) {
                        friends += f -> factions.%(roles(_).party == Fascist).but(f)
                        enemies += f -> factions.diff(friends(f)).but(f)
                    }
                }

                CheckDeckAction(TurnStartAction)
            
            case CheckDeckAction(then) =>
                if (deck.num < 3) {
                    cards.dump()
                    println("deck --> pile")
                    deck --> pile
                    cards.dump()
                    Shuffle[Card](pile, (aa) => ShuffledDeckAction(aa, then), "Shuffle deck")
                }
                else 
                    then
                
            case ShuffledDeckAction(aa, then) =>
                cards.dump()

                pile --> aa --> deck
                
                log("Shuffled deck")

                then
                
            case TurnStartAction =>
                if (frustration == 3)
                    DelayedContinue(50, Force(RandomPolicyAction))
                else
                if (liberal.num >= 5)
                    GameOverAction(Liberal, "Five " ~ Liberal.elem ~ " policies enacted")
                else
                if (fascist.num >= 6)
                    GameOverAction(Fascist, "Six " ~ Fascist.elem ~ " policies enacted")
                else {
                    log(DoubleLine)
                    turn += 1
                    log("Turn", ("#" + turn).hl)

                    val f = candidates.first
                    candidates = candidates.drop(1)

                    president = f
                    chancellor = null

                    log(f, "ran for", President)
                    Ask(f, alive.but(f).diff(limited)./(NominateChancellorAction(f, _)))
                }
                
            case NominateChancellorAction(f, c) =>
                chancellor = c

                log(f, "nominated", c, "as", Chancellor)
                
                votes = Map()
 
                VotingAction(f, c)
                
            case VotingAction(p, c) =>
                val voters = alive.diff(votes.keys.toList)

                if (voters.any)
                    MultiAsk(voters./(a => Ask(a, (VoteAction(a, p, c, Yes) :: VoteAction(a, p, c, No)))))
                else
                    TallyAction(p, c)
                    
            case VoteAction(f, p, c, v) =>
                votes += f -> v

                VotingAction(p, c)

            case TallyAction(p, c) =>
                var balance = 0
                alive.foreach { a =>
                    log(a, "voted", votes(a))
                    balance += (votes(a) == Yes).?(1).|(-1)
                }

                if (balance > 0) {
                    log(p, "was elected", President)
                    log(c, "was elected", Chancellor)

                    if (roles(c) == Hitler && fascist.num >= 3)
                        GameOverAction(Fascist, Hitler.elem ~ " became " ~ Chancellor.elem ~ " with three " ~ Fascist.elem ~ " policies enacted")
                    else
                        LegislationAction(p, c)
                }
                else {
                    log("Elections failed")
                    frustration += 1
                    CheckDeckAction(TurnStartAction)
                }
                
            case RandomPolicyAction =>
                log(SingleLine)

                val a = deck.first
 
                log("A random policy was enacted")
                log(a)

                alive.%(votes(_) == No).foreach { f =>
                    karma += f -> (karma(f) - 1)
                }

                a.party match {
                    case Liberal => deck --> a --> liberal
                    case Fascist => deck --> a --> fascist
                }
                
                limited = Nil

                frustration = 0

                CheckDeckAction(TurnStartAction)

            case SelectPolicyAction(self, a, m, then) =>
                then

            case LegislationAction(p, c) =>
                deck --> deck.take(3) --> drawn
                
                PresidentialPolicyAction(p, c)
                
            case PresidentialPolicyAction(p, c) =>
                Ask(p, drawn.get./(a => SelectPolicyAction(p, a, "Select policies", PresidentialSecondPolicyAction(p, c, a))))

            case PresidentialSecondPolicyAction(p, c, a) =>
                Ask(p, drawn.get.but(a)./(b => SelectPolicyAction(p, b, "Select policies", PresidentialSelectedAction(p, c, a, b))).back(PresidentialPolicyAction(p, c)).cancel)
                
            case PresidentialSelectedAction(p, c, a, b) =>
                drawn --> drawn.get.but(a).but(b) --> pile

                log(p, "selected two of three policies")
                
                if (fascist.num == 5)
                    ChancellorPolicyVetoAction(p, c)
                else
                    ChancellorPolicyAction(p, c)

            case ChancellorPolicyAction(p, c) =>
                Ask(c, drawn.get./(a => SelectPolicyAction(p, a, "Select policy", ChancellorSelectedAction(p, c, a))))
                
            case ChancellorPolicyVetoAction(p, c) =>
                Ask(c, (drawn.get./(a => SelectPolicyAction(p, a, "Select policy", ChancellorSelectedAction(p, c, a))) :+ ChancellorVetoAction(p, c)))
                
            case ChancellorVetoAction(p, c) =>
                log(c, "requested", "Veto".hl)

                PresidentMainVetoAction(p, c)

            case PresidentMainVetoAction(p, c) =>
                Ask(p, (PresidentVetoAction(p, c) :: PresidentDenyVetoAction(p, c)))

            case PresidentVetoAction(p, c) =>
                log(p, "agreed", "Veto".hl)

                drawn --> drawn.get --> pile
                
                frustration += 1

                CheckDeckAction(TurnStartAction)

            case PresidentDenyVetoAction(p, c) =>
                log(c, "denied", "Veto".hl)

                ChancellorPolicyAction(p, c)
                
            case ChancellorSelectedAction(p, c, a) =>
                if (drawn.get./(_.party).distinct.num > 1) {
                    if (a.party == Fascist && roles(p).party == Liberal)
                        enemies += p -> (enemies(p) :+ c)
                }

                a.party match {
                    case Fascist =>
                        karma += p -> (karma(p) - 10)
                        karma += c -> (karma(c) - 4)
                        alive.%(votes(_) == Yes).foreach { f =>
                            karma += f -> (karma(f) - 1)
                        }
                    case Liberal =>
                        karma += p -> (karma(p) + 10)
                        karma += c -> (karma(c) + 2)
                        alive.%(votes(_) == No).foreach { f =>
                            karma += f -> (karma(f) - 1)
                        }
                }

                
                a.party match {
                    case Liberal => drawn --> a --> liberal
                    case Fascist => drawn --> a --> fascist
                }
                
                drawn --> drawn.get --> pile

                log(c, "enacted", a)

                limited = $(c) ++ (alive.num > 5).??($(p))

                frustration = 0
                    
                CheckDeckAction(a.party match {
                    case Liberal => TurnStartAction
                    case Fascist =>
                        if (factions.num.between(5, 6) && fascist.num == 3)
                            PolicyPeekAction(p)
                        else
                        if (factions.num.between(7, 10) && fascist.num == 3)
                            SpecialElectionAction(p)
                        else
                        if (factions.num.between(9, 10) && fascist.num.between(1, 2))
                            InvestigateAction(p)
                        else
                        if (factions.num.between(7, 8) && fascist.num == 2)
                            InvestigateAction(p)
                        else
                        if (fascist.num.between(4, 5))
                            ExecuteAction(p)
                        else
                            TurnStartAction
                })
                
            case PolicyPeekAction(p) =>
                log(p, "viewed three next policies")
                Ask(p, deck.take(3)./(a => ViewPolicyAction(p, a)).done(TurnStartAction))
           
            case ExecuteAction(p) =>
                Ask(p, alive.but(p)./(f => ExecuteFactionAction(p, f)))
           
            case ExecuteFactionAction(p, f) =>
                Ask(p, $(f)./(f => ExecuteFactionConfirmAction(p, f)).cancel)
           
            case ExecuteFactionConfirmAction(p, f) =>
                log(p, "executed", f)
                
                alive :-= f
                
                candidates = candidates.but(f)

                factions.%(b => friends(b).has(f)).foreach { b =>
                    enemies += b -> (enemies(b) :+ p)
                }

                if (roles(f) == Hitler)
                    GameOverAction(Liberal, Hitler.elem ~ " was killed")
                else
                    Notify($(f), $(ExecutedAction(f, p)), TurnStartAction)

            case SpecialElectionAction(p) =>
                Ask(p, alive.but(p)./(f => NominateNextPresidentAction(p, f)))
           
            case NominateNextPresidentAction(p, f) =>
                log(p, "nominated", f, "as the next", "President")

                candidates = f +: candidates

                TurnStartAction
                
            case InvestigateAction(p) =>
                Ask(p, alive.but(p).diff(investigated)./(f => InvestigateFactionAction(p, f)))
           
            case InvestigateFactionAction(p, f) =>
                log(p, "investigated", f)
                
                investigated :+= f

                if (roles(p).party == roles(f).party)
                    friends += p -> (friends(p) :+ f)
                else
                    enemies += p -> (enemies(p) :+ f)

                Ask(p, $(ViewPartyAction(p, f, roles(f).party)).done(TurnStartAction)).needOk

            case GameOverAction(s, m) =>
                over = true

                log(m)
                log((s.toString + "s").styled(s), "won")

                val winners = factions.%(roles(_).party == s)

                GameOver(winners, "Game Over", $(
                    Ask(null, winners./(f => GameOverWonAction(null, f)))
                ))
                
            case a : SelfPerform =>
                a.perform(this)
        }
    }
}