package hrf

import colmat._

import scala.scalajs.reflect._

package object reflect {
    def className(o : AnyRef) : String = o.getClass.getName.split('.').toList.last.split('$').toList.last

    type Contructor = List[Any] => Any

    def lookupClass(s : String, n : Int) : Option[List[Any] => Any] = Reflect.lookupInstantiatableClass(s).toList.flatMap(_.declaredConstructors).%(_.parameterTypes.num == n).single./(c => (l => c.newInstance(l : _*)))

    def lookupObject(s : String) : Option[Any] = Reflect.lookupLoadableModuleClass(s + "$").map(_.loadModule())
}
