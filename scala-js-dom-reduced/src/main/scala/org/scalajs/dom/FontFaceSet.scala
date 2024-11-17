/** https://www.w3.org/TR/2016/CR-mediacapture-streams-20160519/ */
package org.scalajs.dom

import scala.scalajs.js

@js.native
trait FontFaceSet extends js.Object {
  var status: String = js.native

  def forEach(callback: js.Function1[FontFace, Unit]): Unit = js.native
  def load(font : String, text : String = js.native): Unit = js.native
}
