package hrf.voice
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

import org.scalajs.dom
import org.scalajs.dom.html

import scalajs._
import scalajs.js.timers.setTimeout

@js.native
@js.annotation.JSGlobal("SpeechSynthesisUtterance")
class Utterance(text : String) extends js.Object {
    var pitch : Double = js.native
    var rate : Double = js.native
    var volume : Double = js.native
    var lang : String = js.native
    var voice : Voice = js.native
    var onend : js.Function1[dom.Event, Unit] = js.native
    var onerror : js.Function1[dom.Event, Unit] = js.native
}

@js.native
@js.annotation.JSGlobal("SpeechSynthesisVoice")
class Voice extends js.Object {
}

@js.native
@js.annotation.JSGlobal("speechSynthesis")
object SpeechSynthesis extends js.Object {
    def getVoices() : js.Array[Voice] = js.native
    def speak(text : Utterance) : Unit = js.native
    def cancel() : Unit = js.native
}

object Speech {
    var enabled = hrf.HRF.flag("voice")
    var ready = false

    var pitch = 1.0
    var rate = 1.6
    var volume = 0.8
    var voice = 0

    def onKey(key : String, ctrl : Boolean = false, alt : Boolean = false, shift : Boolean = false)(then : => Unit) {
        hrf.HRF.onKey(e => e.ctrlKey == ctrl && e.altKey == alt && e.shiftKey == shift && e.code == key) {
            if (enabled) {
                cancel()
                then
            }
        }
    }

    def say(text : String) {
        say(text, {}, {})
    }

    var reference : Utterance = null

    def initialize(then : => Unit) {
        if (ready.not) {
            say(".", { ready = true }, {})
            setTimeout(100) { initialize(then) }
        }
        else
            then
    }

    def cancel() {
        SpeechSynthesis.cancel()
    }

    def say(text : String, then : => Unit, fail : => Unit) {
        if (enabled.not)
            return

        val voices = SpeechSynthesis.getVoices()

        val s = text.replace("@", "").replace(0x2192.toChar.toString, "").replace(hrf.elem.DoubleLine.text, "").replace(hrf.elem.SingleLine.text, "")

        val u = new Utterance(s)
        u.pitch = pitch
        u.rate = rate
        u.volume = volume
        u.lang = "en-US"
        u.voice = voices(voice % voices.length)

        u.onend = e => { then }
        u.onerror = e => { fail }

        reference = u

        dom.window.console.log(u)
        dom.window.console.log(s)

        SpeechSynthesis.speak(u)
    }
}
