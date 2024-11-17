package hrf
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

import scalajs.reflect._


package object reflect {
    def className(o : AnyRef) : String = o.getClass.getName.split('.').$.last.split('$').$.last

    type Contructor = $[Any] => Any

    def lookupClass(s : String, n : Int) : |[$[Any] => Any] = Reflect.lookupInstantiatableClass(s).$.flatMap(_.declaredConstructors).%(_.parameterTypes.num == n).single./(c => (l => c.newInstance(l : _*)))

    def lookupObject(s : String) : |[Any] = Reflect.lookupLoadableModuleClass(s + "$").map(_.loadModule())
}
