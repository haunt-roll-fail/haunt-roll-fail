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

package hrf {
    package object reflect {
        def className(o : AnyRef) : String = o.getClass.getName.split('.').$.last.split('$').$.last

        def lookupClass(s : String, n : Int) : |[Seq[Any] => Any] =
            util.Try({
                Class.forName(s).getConstructors().$.%(_.getParameterCount == n).single./(x => (o : Seq[Any]) => x.newInstance(o : _*))
            }).toOption.flatten

        def lookupObject(s : String) : |[Any] =
            util.Try({
                Class.forName(s + "$").getField("MODULE$").get(null)
            }).toOption
    }

}

package scala.scalajs.reflect.annotation {
    class EnableReflectiveInstantiation extends scala.annotation.StaticAnnotation
}
