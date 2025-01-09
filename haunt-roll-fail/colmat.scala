package hrf

import scala.collection._

object colmat {

    def random(): Double = java.lang.Math.random()
    def random(less : Int): Int = java.lang.Math.floor(java.lang.Math.random() * less).toInt
    def randomInRange(min : Int, max : Int): Int = java.lang.Math.floor(java.lang.Math.random() * (max - min + 1)).toInt + min
    def max(x : Int, y : Int) : Int = java.lang.Math.max(x, y)
    def min(x : Int, y : Int) : Int = java.lang.Math.min(x, y)
    def max(x : Double, y : Double) : Double = java.lang.Math.max(x, y)
    def min(x : Double, y : Double) : Double = java.lang.Math.min(x, y)
    def pow(b : Double, e : Double) : Double = java.lang.Math.pow(b, e)


    type ClassTag[T] = scala.reflect.ClassTag[T]


    type $[T] = List[T]

    object $ {
        def apply() = Nil
        def apply[T](values : T*) = values.toList
        def unapplySeq[T](l : List[T]) = List.unapplySeq(l)
    }

    implicit def nilToNil(v : $.type) : List[Nothing] = Nil
    implicit def nilToApply1[T](v : $.type) : T => List[T] = (a : T) => List(a)


    type |[T] = Option[T]

    object | {
        def unapply[T](l : Some[T]) = Some.unapply(l)
        def apply[T](v : T) = Some(v)
    }

    implicit class NoneUtils(val v : None.type) extends AnyVal {
        def apply[T](v : T) = Some(v)
    }

    implicit class OptionElemUtils[T <: Any](val o : T) extends AnyVal {
        def ? = Option(o)
        def but(v : T) = if (v == o) None else Some(o)
        def ?%(f : T => Boolean) = if (f(o)) Some(o) else None
    }


    trait GoodMatch

    trait MatchesGood[T]

    implicit def goodMatch[T <: GoodMatch] : MatchesGood[T] = null
    implicit val goodMatchUnit : MatchesGood[Unit] = null
    implicit val goodMatchInt : MatchesGood[Int] = null
    implicit val goodMatchBoolean : MatchesGood[Boolean] = null
    implicit val goodMatchString : MatchesGood[String] = null
    implicit val goodMatchNil : MatchesGood[Nil.type] = null
    implicit def goodMatchList[T](implicit gm : MatchesGood[T]) : MatchesGood[$[T]] = null
    implicit def goodMatchOption[T](implicit gm : MatchesGood[T]) : MatchesGood[|[T]] = null
    implicit def goodMatchTuple2[T, U](implicit gmt : MatchesGood[T], gmu : MatchesGood[U]) : MatchesGood[(T, U)] = null
    implicit def goodMatchTuple3[T, U, V](implicit gmt : MatchesGood[T], gmu : MatchesGood[U], gmv : MatchesGood[V]) : MatchesGood[(T, U, V)] = null

    implicit class AnyUtils[T <: Any](val o : T) extends AnyVal {
        def @@[U : MatchesGood](t : T => U) : U = t(o)
        def @@(t : T => Nothing) = t(o)
        def @@?[U](t : T => U) : U = t(o)
        def as[U : ClassTag] : Option[U] = Some(o).collect({ case m : U => m })
        def check[U : ClassTag](f : U => Boolean) : Boolean = Some(o).map({ case m : U => f(m) ; case _ => false }).getOrElse(false)
        def is[U : ClassTag] : Boolean = Some(o).collect({ case m : U => m }).any
        def tap[U](f : T => U) = { f(o) ; o }
        def use[U](f : T => U) = f(o)
        def mut[U, R <: T](u : Option[U])(f : (T, U) => R) = u.map(u => f(o, u)).getOrElse(o)
    }

    implicit class ListElemUtils[T](val v : T) extends AnyVal {
        def ***(n : Int) = List.fill(n)(v)
        def in(l : Set[T]) = l.contains(v)
        def in(l : Seq[T]) = l.contains(v)
        def in(a : T, l : Seq[T]) = a == v || l.contains(v)
        def in(l : T*) = l.contains(v)
    }

    implicit class RangeUtils(val l : Range) extends AnyVal {
        def /[U](p : Int => U) = l.map(p).toList
        def /~[U](p : Int => IterableOnce[U]) = l.flatMap(p).toList
        def %(p : Int => Boolean) = l.filter(p)
        def %!(p : Int => Boolean) = l.filterNot(p)
    }

    implicit class IterableUtils[T](val l : Iterable[T]) extends AnyVal {
        def $ = l.toList
        def any = l.nonEmpty
        def none = l.isEmpty
        def num = l.size
        def some = if (l.isEmpty) None else Some(l.toList)
        def /[U](p : T => U) = l.map(p)
        def /~[U](p : T => IterableOnce[U]) = l.flatMap(p)
        def %(p : T => Boolean) = l.filter(p)
        def %!(p : T => Boolean) = l.filterNot(p)
        def maxOr(v : T)(implicit cmp : Ordering[T]) = if (l.none) v else l.max
        def minOr(v : T)(implicit cmp : Ordering[T]) = if (l.none) v else l.min
    }

    implicit class IteratorUtils[T](val l : Iterator[T]) extends AnyVal {
        def $ = l.toList
        def /[U](p : T => U) = l.map(p).toList
    }

    implicit class ListUtilsError1[T](val l : List[T]) extends AnyVal {
        def $ = ???
    }

    implicit class ListUtilsError2[T](val l : List[T]) extends AnyVal {
        def $ = ???
    }

    implicit class ListLikeUtils[S, T](val ll : S)(implicit conv : S => List[T]) {
        private def l = conv(ll)
        def $ = l

        def single : |[T] = $ match {
            case Seq(e) => Some(e)
            case _ => None
        }
        def only = single.get
        def any = l.nonEmpty
        def none = l.isEmpty
        def num = l.size
        def has(x : T) = l.contains(x)
        def but(x : T) = l.filter(_ != x)
        def --(x : T) = l.filter(_ != x)
        def add(x : T) = l :+ x
        def %(p : T => Boolean) = l.filter(p)
        def %!(p : T => Boolean) = l.filterNot(p)
        def %~[U](t : T => U)(p : U => Boolean) = l.filter(x => p(t(x)))
        def all(p : T => Boolean) = l.forall(p)
        def /[U](p : T => U) = l.map(p)
        def /~[U](p : T => IterableOnce[U]) = l.flatMap(p)
        def count(x : T) = l.count(_ == x)
        def count(p : T => Boolean) = l.count(p)
        def some = if (l.isEmpty) None else Some(l)
        def intersperse[U >: T](s : U) : List[U] = l.take(1) ++ l.drop(1)./~(List(s, _))
        def first = l.head
        def starting = l.headOption
        def ending = l.lastOption
        def dropFirst = l.drop(1)
        def dropLast = l.dropRight(1)
        def dropEnd(n : Int) = l.dropRight(n)

        def shuffle = scala.util.Random.shuffle(l)
        def shuffleWith(r : Randomness) = l.zip(r.get(l.num)).sortBy(_._2).map(_._1)
        def shuffleSeed(s : Int) = new scala.util.Random(s).shuffle(l)
        def occurrences[K](d : T => K): Map[K, Int] = l.groupMapReduce(d)(_ => 1)(_ + _)

        def :-(x : T) = l.diff(List(x))
        def of[U : ClassTag] : List[T with U] = l.collect { case m : U => m.asInstanceOf[T with U] }
        def notOf[U : ClassTag] : List[T] = l.filter { case m : U => false ; case _ => true }

        def indexed = IndexedList(l.zipWithIndex)

    }

    implicit class ListUtils[T](val l : List[T]) extends AnyVal {
        def single = l match {
            case Seq(e) => Some(e)
            case _ => None
        }
        def only = single.get
        def any = l.nonEmpty
        def none = l.isEmpty
        def num = l.size
        def has(x : T) = l.contains(x)
        def but(x : T) = l.filter(_ != x)
        def --(x : T) = l.filter(_ != x)
        def but(ox : Option[T]) = ox.map(x => l.filterNot(_ == x)).getOrElse(l)
        def add(x : T) = l :+ x
        def %(p : T => Boolean) = l.filter(p)
        def %!(p : T => Boolean) = l.filterNot(p)
        def %~[U](t : T => U)(p : U => Boolean) = l.filter(x => p(t(x)))
        def all(p : T => Boolean) = l.forall(p)
        def /[U](p : T => U) = l.map(p)
        def /~[U](p : T => IterableOnce[U]) = l.flatMap(p)
        def count(x : T) = l.count(_ == x)
        def some = if (l.isEmpty) None else Some(l)
        def intersperse[U >: T](s : U) : List[U] = l.take(1) ++ l.drop(1)./~($(s, _))
        def first = l.head
        def starting = l.headOption
        def ending = l.lastOption
        def dropFirst = l.drop(1)
        def dropLast = l.dropRight(1)
        def dropEnd(n : Int) = l.dropRight(n)

        def shuffle = scala.util.Random.shuffle(l)
        def shuffleWith(r : Randomness) = l.zip(r.get(l.num)).sortBy(_._2).map(_._1)
        def shuffleSeed(s : Int) = new scala.util.Random(s).shuffle(l)
        def occurrences[K](d : T => K): Map[K, Int] = l.groupMapReduce(d)(_ => 1)(_ + _)

        def :-(x : T) = l.diff(List(x))
        def of[U : ClassTag] : List[T with U] = l.collect { case m : U => m.asInstanceOf[T with U] }
        def notOf[U : ClassTag] : List[T] = l.filter { case m : U => false ; case _ => true }

        def indexed = IndexedList(l.zipWithIndex)
    }

    implicit class ListTuplesUtils[A, B](val l : List[(A, B)]) extends AnyVal {
        def lefts = l.map(_._1)
        def rights = l.map(_._2)
        def forany(p : (A, B) => Boolean) = l.exists { case (a, b) => p(a, b) }
        def %(p : (A, B) => Boolean) = l.filter { case (a, b) => p(a, b) }
        def %!(p : (A, B) => Boolean) = l.filterNot { case (a, b) => p(a, b) }
        def %<(p : (A) => Boolean) = l.filter { case (a, b) => p(a) }
        def %>(p : (B) => Boolean) = l.filter { case (a, b) => p(b) }
        def %<!(p : (A) => Boolean) = l.filterNot { case (a, b) => p(a) }
        def %>!(p : (B) => Boolean) = l.filterNot { case (a, b) => p(b) }
        def /[U](p : (A, B) => U) = l.map { case (a, b) => p(a, b) }
        def /<[U](p : A => U) = l.map { case (a, b) => (p(a), b) }
        def />[U](p : B => U) = l.map { case (a, b) => (a, p(b)) }
        def /~[U](p : (A, B) => IterableOnce[U]) = l.flatMap { case (a, b) => p(a, b) }
        def all(p : (A, B) => Boolean) = l.forall { case (a, b) => p(a, b) }
    }

    implicit class OptionTupleUtils[A, B](val l : Option[(A, B)]) extends AnyVal {
        def left = l.map(_._1)
        def right = l.map(_._2)
        def /< = l.map(_._1)
        def /> = l.map(_._2)
    }

    case class IndexedList[T](l : List[(T, Int)]) {
        def foreach(f : (T, Int) => Unit) = l.foreach { case (e, i) => f(e, i) }
        def reverse = IndexedList(l.reverse)
        def %(p : T => Boolean) = IndexedList(l.filter { case (e, i) => p(e) })
        def %!(p : T => Boolean) = IndexedList(l.filterNot { case (e, i) => p(e) })
        def /[U](p : (T, Int) => U) = l.map { case (e, i) => p(e, i) }
        def /~[U](p : (T, Int) => IterableOnce[U]) = l.flatMap { case (e, i) => p(e, i) }
        def indices = l.map { case (e, i) => i }
        def values = l.map { case (e, i) => e }
    }

    implicit class ArrayUtils[T](val array : Array[T]) extends AnyVal {
        def $ = array.toList
    }

    implicit class StringIterableUtils(val l : Iterable[String]) extends AnyVal {
        def join(s : String) = l.mkString(s)
    }

    implicit class StringArrayUtils(val l : Array[String]) extends AnyVal {
        def $ = l.toList
        def join(s : String) = l.mkString(s)
    }

    implicit class ListComposeUtils[T](val a : T) extends AnyVal {
        def ::[U, That](b : U)(implicit conv : BiConverter[T, U, That]) : List[That] = List[That](conv.fromUtoR(b), conv.fromTtoR(a))
    }

    implicit class BooleanUtils(val b : Boolean) {
        def not = !b
        def ?[T](f : => T) = if (b) Some(f) else None
        def ??[T](f : => Option[T]) = if (b) f else None
        def ??(f : => String) = if (b) f else ""
        def ??(f : => Int) = if (b) f else 0
        def ??[T](f : => List[T]) = if (b) f else Nil
        def ??[T](f : => () => Unit) = if (b) f else null
        def ??[T](f : => Any => Unit) = if (b) f else null
        def ??[T](f : => Unit) = if (b) f else ()
        def $[T](f0 : => T) = if (b) List(f0) else Nil
        def $[T](f0 : => T, f1 : => T) = if (b) List(f0, f1) else Nil
        def $[T](f0 : => T, f1 : => T, f2 : => T) = if (b) List(f0, f1, f2) else Nil
        def $[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T) = if (b) List(f0, f1, f2, f3) else Nil
        def $[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T, f4 : => T) = if (b) List(f0, f1, f2, f3, f4) else Nil
        def $[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T, f4 : => T, f5 : => T) = if (b) List(f0, f1, f2, f3, f4, f5) else Nil
        def $[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T, f4 : => T, f5 : => T, f6 : => T) = if (b) List(f0, f1, f2, f3, f4, f5, f6) else Nil
        def $[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T, f4 : => T, f5 : => T, f6 : => T, f7 : => T) = if (b) List(f0, f1, f2, f3, f4, f5, f6, f7) else Nil
        def $[T](f0 : => T, f1 : => T, f2 : => T, f3 : => T, f4 : => T, f5 : => T, f6 : => T, f7 : => T, f8 : => T) = if (b) List(f0, f1, f2, f3, f4, f5, f6, f7, f8) else Nil
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

    implicit class OptionStringUtils[T](val o : Option[String]) extends AnyVal {
        def ?? = o.getOrElse("")
    }

    implicit class OptionUtils[T](val o : Option[T]) extends AnyVal {
        def |[U, That](f : => U)(implicit conv : BiConverter[T, U, That]) : That = o.map(conv.fromTtoR).getOrElse(conv.fromUtoR(f))
        def ||[U, That](t : => Option[U])(implicit conv : BiConverter[T, U, That]) : Option[That] = o match {
            case x : Some[_] => o.map(conv.fromTtoR)
            case None => t.o.map(conv.fromUtoR)
        }

        def $ = o.toList
        def of[U : ClassTag] : Option[U] = o.collect { case m : U => m }
        def any = o.isDefined
        def none = o.isEmpty

        def %(p : T => Boolean) = o.filter(p)
        def %!(p : T => Boolean) = o.filterNot(p)
        def /[U](p : T => U) = o.map(p)
        def ?(p : T => Boolean) = o.map(p).getOrElse(false)
        def ?(p : T => Int) = o.map(p).getOrElse(0)
        def ?(p : T => String) = o.map(p).getOrElse("")
        def ?[U](p : T => List[U]) = o.map(p).getOrElse(Nil)

        def but(x : T) = o.filterNot(_ == x)
        def but(ox : Option[T]) : Option[T] = ox.flatMap(x => o.filterNot(_ == x)).orElse(o)
        def but(l : $[T]) = o.filterNot(x => l.contains(x))
        def has(x : T) = o.contains(x)

        def /~[U](p: T => Option[U]) = o.flatMap(p)
        def /~[U](p: T => Array[U])(implicit d : DummyImplicit) = o.toList.flatMap(x => p(x))
        def /~[U](p: T => Seq[U]) = o.toList.flatMap(x => p(x))
        def |!(e : => String) = o.getOrElse { throw new Error(e) }
    }

    implicit class StringUtils[T](val str : String) extends AnyVal {
        def some = if (str == null || str == "") None else Some(str)
        def any = some.any
        def splt(d : String) = str.split(java.util.regex.Pattern.quote(d)).toList
        def s(n : Int) = if (n != 1) str + "s" else str
        def repeat(n : Int) = List.fill(n)(str).join("")
    }

    implicit class IntUtils(val n : Int) extends AnyVal {
        def /\/(d : Int) = n / d
        def /:/(d : Int) = n / d.toDouble
        def ÷(d : Int) = n / d.toDouble
        def /~/(d : Int) = (n /:/ d).~
        def up_/(d : Int) = (n /:/ d).ceil.toInt
        def down_/(d : Int) = (n /:/ d).floor.toInt
        def /↑(d : Int) = (n /:/ d).ceil.toInt
        def /↓(d : Int) = (n /:/ d).floor.toInt
        def ÷↑(d : Int) = (n /:/ d).ceil.toInt
        def ÷↓(d : Int) = (n /:/ d).floor.toInt

        def formatted(s : String) : String = s.format(n)
        def times[T](v : T) : List[T] = List.fill(n)(v)
        def timesDo(v : () => Unit) : Unit = 1.to(n).foreach(_ => v())
        def between(from : Int, to : Int) : Boolean = n >= from && n <= to
        def clamp(from : Int, to : Int) : Int = if (n < from) from else if (n > to) to else n
        def of[T](p : List[T]) : T = p.apply(n)
    }

    implicit class IntDisable1(val n : Int) extends AnyVal {
        def ~ = ???
    }

    implicit class IntDisable2(val n : Int) extends AnyVal {
        def ~ = ???
    }

    implicit class DoubleUtils(val r : Double) extends AnyVal {
        def /:/(d : Int) = r / d.toDouble
        def /~/(d : Int) = (r /:/ d).~
        def ~ = r.round.toInt

        def clamp(from : Double, to : Double) : Double = if (from > to) (from + to) / 2 else if (r < from) from else if (r > to) to else r
        def formatted(s : String) = s.format(r)
    }

    trait Void {
        def apply() : true
    }

    class BreakVoid(active : Boolean, break : () => Nothing) extends Void {
        def apply() : true = if (active) break() else true
    }

    class ThrowVoid(active : Boolean) extends Void {
        def apply() : true = throw new Error("VOID")
    }

    class CallbackBreakVoid(active : Boolean, break : () => Nothing, callback : () => Unit) extends Void {
        override def apply() : true = { callback(); if (active) break() else true }
    }

    object NoVoid extends Void {
        def apply() = true
    }

    object Void {
        implicit def voidToTrue(v : Void) : true = v.apply()
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
