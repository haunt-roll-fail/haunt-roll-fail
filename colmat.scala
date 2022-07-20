package object colmat {

    def abs(x : Int): Int = java.lang.Math.abs(x)    
    def random(): Double = java.lang.Math.random()
    def max(x : Int, y : Int) : Int = java.lang.Math.max(x, y)
    def min(x : Int, y : Int) : Int = java.lang.Math.min(x, y)
    def max(x : Double, y : Double) : Double = java.lang.Math.max(x, y)
    def min(x : Double, y : Double) : Double = java.lang.Math.min(x, y)
    def pow(b : Double, e : Double) : Double = java.lang.Math.pow(b, e)

    import scala.collection._
    import scala.reflect.ClassTag

    type $[T] = scala.List[T]

    def $[T](values : T*) = values.toList
    
    implicit class OptionNullLogic[T <: Any](val o : T) extends AnyVal {
        def ? = Option(o)
    }
    
    implicit class AnyMatch[T <: Any](val o : T) extends AnyVal {
        def @@[U](t : T => U) = t(o)
        def as[T : ClassTag] : Option[T] = Some(o).collect({ case m : T => m })
        def is[T : ClassTag] : Boolean = Some(o).collect({ case m : T => m }).any
    }

    implicit class ListUtilsT[T](val v : T) extends AnyVal {
        def repeat(n : Int) = List.fill(n)(v)
        def *(n : Int) = List.fill(n)(v)
        def in(l : List[T]) = l.contains(v)
        def in(l : Set[T]) = l.contains(v)
        def in(l : T*) = l.contains(v)
        def inlist[B](p : T => B) : B = p(v)
    }

    implicit class RangeUtils(val l : Range) extends AnyVal {
        def /[U](p : Int => U) = l.map(p).toList
        def /~[U](p : Int => IterableOnce[U]) = l.flatMap(p).toList
        def %(p : Int => Boolean) = l.filter(p)
        def %!(p : Int => Boolean) = l.filterNot(p)
    }

    implicit class IterableUtils[T](val l : Iterable[T]) extends AnyVal {
        def any = l.nonEmpty
        def none = l.isEmpty
        def num = l.size
        def some = if (l.isEmpty) None else Some(l.toList)
        def /[U](p : T => U) = l.map(p)
        def /~[U](p : T => IterableOnce[U]) = l.flatMap(p)
        def %(p : T => Boolean) = l.filter(p)
        def %!(p : T => Boolean) = l.filterNot(p)
    }
    
    implicit class IteratorUtils[T](val l : Iterator[T]) extends AnyVal {
        def /[U](p : T => U) = l.map(p).toList
    }
    
    implicit class ListUtils[T](val l : List[T]) extends AnyVal {
        def single = l match {
            case Seq(e) => Some(e)
            case _ => None
        }
        def any = l.nonEmpty
        def none = l.isEmpty
        def num = l.size
        def has(x : T) = l.contains(x)
        def but(x : T) = l.filter(_ != x)
        def but(ox : Option[T]) = ox.map(x => l.filter(_ != x)).getOrElse(l)
        def %(p : T => Boolean) = l.filter(p)
        def %!(p : T => Boolean) = l.filterNot(p)
        def all(p : T => Boolean) = l.forall(p)
        def /[U](p : T => U) = l.map(p)
        def /~[U](p : T => IterableOnce[U]) = l.flatMap(p)
        def maxOr(v : T)(implicit cmp : Ordering[T]) = if (l.none) v else l.max
        def minOr(v : T)(implicit cmp : Ordering[T]) = if (l.none) v else l.min
        def count(x : T) = l.count(_ == x)
        def some = if (l.isEmpty) None else Some(l)
        def intersperse[U >: T](s : U) : List[U] = l.take(1) ++ l.drop(1)./~($(s, _))
        def first = l.head 

        def shuffle = scala.util.Random.shuffle(l)
        def shuffleWith(r : Randomness) = l.zip(r.get(l.num)).sortBy(_._2)./(_._1)
        def occurrences[K](d : T => K): Map[K, Int] = l.groupMapReduce(d)(_ => 1)(_ + _)        
        
        def :-(x : T) = l.diff(List(x))
        def of[U : ClassTag] : List[U] = l.collect { case m : U => m }
    }

    implicit class StringListUtils(val l : Iterable[String]) extends AnyVal {
        def join(s : String) = l.mkString(s)
    }

    implicit class StringArrayUtils(val l : Array[String]) extends AnyVal {
        def join(s : String) = l.mkString(s)
    }

    implicit class ListUtilsN[T](val a : T) extends AnyVal {
        def ::[U, That](b : U)(implicit conv : BiConverter[T, U, That]) : List[That] = List[That](conv.fromUtoR(b), conv.fromTtoR(a))
        def doesntwork_::[U, That](b : Option[U])(implicit conv : BiConverter[T, U, That]) : List[That] = b match {
            case Some(b) => List[That](conv.fromUtoR(b), conv.fromTtoR(a))
            case None => List[That](conv.fromTtoR(a))
        }
    }
    
    implicit class ListUtilsNN[T](val a : List[T]) extends AnyVal {
        def !!++[U, That](b : U)(implicit conv : BiConverter[T, U, That]) : List[That] = a.map(conv.fromTtoR) :+ conv.fromUtoR(b)
        def !!::[U, That](b : U)(implicit conv : BiConverter[T, U, That]) : List[That] = a.map(conv.fromTtoR) :+ conv.fromUtoR(b)
    }

    implicit class Bool(val b : Boolean) {
        def not = !b
        def ?[T](f : => T) = if (b) Some(f) else None
        def ?~[T](f : => Option[T]) = if (b) f else None
        def ??~(f : => Option[String]) = (if (b) f else None).|("")
        def ??(f : => String) = if (b) f else ""
        def ??(f : => Int) = if (b) f else 0
        def ??[T](f : => List[T]) = if (b) f else Nil
        def ??[T](f : => () => Unit) = if (b) f else null
        def ??[T](f : => Unit) = if (b) f else null
        def ??[T](f0 : => T, f1 : => T) = if (b) List(f0, f1) else Nil
        def ??[T](f0 : => T, f1 : => T, f2 : => T) = if (b) List(f0, f1, f2) else Nil
        def ??[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T) = if (b) List(f0, f1, f2, f3) else Nil
        def ??[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T, f4 : => T) = if (b) List(f0, f1, f2, f3, f4) else Nil
        def ??[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T, f4 : => T, f5 : => T) = if (b) List(f0, f1, f2, f3, f4, f5) else Nil
        def ??[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T, f4 : => T, f5 : => T, f6 : => T) = if (b) List(f0, f1, f2, f3, f4, f5, f6) else Nil
        def ??[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T, f4 : => T, f5 : => T, f6 : => T, f7 : => T) = if (b) List(f0, f1, f2, f3, f4, f5, f6, f7) else Nil
    }

    class BiConverter[T, U, R](val fromTtoR : T => R, val fromUtoR : U => R)

    trait LowPriorityBiConverterImplicits {
        implicit def subtype[R, T <: R, U <: R] : BiConverter[T, U, R] = new BiConverter[T, U, R](identity[T], identity[U])
    }
    
    object BiConverter extends LowPriorityBiConverterImplicits {
        implicit def identityConverter[T] : BiConverter[T, T, T] = new BiConverter[T, T, T](identity, identity)
        implicit def firstAsSecond[T, U](implicit conv : T => U) : BiConverter[T, U, U] = new BiConverter[T, U, U](conv, identity)
        implicit def secondAsFirst[T, U](implicit conv : U => T) : BiConverter[T, U, T] = new BiConverter[T, U, T](identity, conv)
    }

    implicit class OptionLogic[T](val o : Option[T]) extends AnyVal {
        def of[U : ClassTag] : Option[U] = o.collect { case m : U => m }
        def any = o.isDefined
        def none = o.isEmpty
        def |[U, That](f : => U)(implicit conv : BiConverter[T, U, That]) : That = o.map(conv.fromTtoR).getOrElse(conv.fromUtoR(f))
        def ||[U, That](t : => Option[U])(implicit conv : BiConverter[T, U, That]) : Option[That] = o match {
            case x : Some[_] => o.map(conv.fromTtoR)
            case None => t.o.map(conv.fromUtoR)
        }
        
        def %(p : T => Boolean) = o.filter(p)
        def %!(p : T => Boolean) = o.filterNot(p)
        def /[U](p: T => U) = o.map(p)
        def /?(p: T => Boolean) = o.map(p).getOrElse(false)
        def /?(p: T => Int) = o.map(p).getOrElse(0)
        def /?[U](p: T => String) = o.map(p).getOrElse("")
        def /?[U](p: T => List[U]) = o.map(p).getOrElse(Nil)

        def but(x : T) = o.filter(_ != x)
        def but(ox : Option[T]) = ox.flatMap(x => o.filter(_ != x)).getOrElse(o)
        def has(x : T) = o.filter(_ == x).isDefined

        def /~[U](p: T => Option[U]) = o.flatMap(p)
        def /~[U](p: T => Array[U])(implicit d : DummyImplicit) = o.toList.flatMap(x => p(x))
        def /~[U](p: T => Seq[U]) = o.toList.flatMap(x => p(x))
        def |!(e : => String) = o.getOrElse {
            println(e)
            throw new Error(e)
        }
    }
    
    implicit class StringUtils[T](val s : String) extends AnyVal {
        def any = some.any
        def some = if (s == null || s == "") None else Some(s)
        def splt(d : String) = s.split(java.util.regex.Pattern.quote(d)).toList
    }

    implicit class IntUtils(val n : Int) extends AnyVal {
        def between(from : Int, to : Int) = n >= from && n <= to
    }

    class Randomness(l : List[Int]) {
        private val num = l.num
        private var p = 0

        def get(n : Int) = {
            if (p + n > num)
                throw new Error("randomness exhausted")
                
            p += n

            l.drop(p - n).take(n)
        }
    }
}
