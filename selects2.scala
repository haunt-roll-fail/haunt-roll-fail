package hrf.base
//
//
//
//
import logger._, colmat._
//
//
//
//

import hrf.elem._

trait SelectSubset { self : Gaming =>
    trait ObjectOneRule[T] { rule =>
        def validOne(o : T) : Boolean

        abstract class Same extends ObjectOneRule[T] {
            final def validOne(o : T) = rule.validOne(o) && valid(o)
            def valid(o : T) = true
        }
        
        def only(x : List[T]) : ObjectOneRule[T] = new Same {
            override def valid(o : T) = x.has(o)
        }
        
        def except(x : List[T]) : ObjectOneRule[T] = new Same {
            override def valid(o : T) = x.has(o).not
        }

        def each(f : T => Boolean) : ObjectOneRule[T] = new Same {
            override def valid(o : T) = f(o)
        }
    }
    
    class OfOne[T] extends ObjectOneRule[T] {
        def validOne(o : T) = true
    }

    trait ObjectSetRule[T] { rule =>
        def validPrefix(l : List[T]) : Boolean
        def validSet(l : List[T]) : Boolean
        
        abstract class Same extends ObjectSetRule[T] {
            final def validPrefix(l : List[T]) = rule.validPrefix(l) && prefix(l)
            final def validSet(l : List[T]) = rule.validSet(l) && set(l)
            def prefix(l : List[T]) = true
            def set(l : List[T]) = true
        }
 
        def each(f : T => Boolean) : ObjectSetRule[T] = new Same {
            override def prefix(l : List[T]) = l.forall(f)
        }
 
        def all(f : List[T] => Boolean) = new Same {
            override def prefix(l : List[T]) = f(l)
        }
 
        def except(x : List[T]) : ObjectSetRule[T] = new Same {
            override def prefix(l : List[T]) = l.intersect(x).none
        }

        def only(x : List[T]) : ObjectSetRule[T] = new Same {
            override def prefix(l : List[T]) = l.diff(x).none
        }

        def num(n : Int) = new Same {
            override def prefix(l : List[T]) = l.num <= n
            override def set(l : List[T]) = l.num == n
        }
        
        def upTo(n : Int) = new Same {
            override def prefix(l : List[T]) = l.num <= n
        }
        
        def min(n : Int) = new Same {
            override def set(l : List[T]) = l.num >= n
        }
    }

    class OfSet[T] extends ObjectSetRule[T] {
        def validPrefix(l : List[T]) = true
        def validSet(l : List[T]) = l.num > 0
    }
    
    
    case class Convert[T](convert : (G, List[T], Int, List[Int]) => Elem)

    object Convert {
        implicit def convertToTransformObjectsGVNS[T](implicit convert : (G, List[T], Int, List[Int]) => Elem) = Convert(convert)
        implicit def convertToConvertVNS[T](implicit convert : (List[T], Int, List[Int]) => Elem) =              Convert((g : G, l : List[T], i : Int, selected : List[Int]) => convert(l, i, selected))
        implicit def convertToConvertGTB[T](implicit convert : (G, T, Boolean) => Elem) =                        Convert((g : G, l : List[T], i : Int, selected : List[Int]) => convert(g, l(i), selected.has(i)))
        implicit def convertToConvertGT[T](implicit convert : (G, T) => Elem) =                                  Convert((g : G, l : List[T], i : Int, selected : List[Int]) => convert(g, l(i)))
        implicit def convertToConvertTB[T](implicit convert : (T, Boolean) => Elem) =                            Convert((g : G, l : List[T], i : Int, selected : List[Int]) => convert(l(i), selected.has(i)))
        implicit def convertToConvertT[T](implicit convert : T => Elem) =                                        Convert((g : G, l : List[T], i : Int, selected : List[Int]) => convert(l(i)))
    }

    case class XXSelectConfig[T](extra : List[UserAction] = Nil, 
        rule : ObjectSetRule[T] = new OfSet[T],
        single : Boolean = false,
        group : (G, List[T]) => Elem = (g : G, s : List[T]) => Empty,
        splitAt : List[Int] = Nil, 
        break : Int => Elem = (n : Int) => HorizontalBreak,
        thens : (List[T], List[Int], Boolean) => List[UserAction] = (l : List[T], s : List[Int], v : Boolean) => Nil)
            
    object XXSelectObjectsAction {
        def apply[T](self : F, values : List[T], order : Boolean)(implicit convert : Convert[T]) : XXSelectObjectsAction[T] = XXSelectObjectsAction(self, values, Nil, order)(new XXSelectConfig[T]())
        def apply[T](self : F, values : List[T]                 )(implicit convert : Convert[T]) : XXSelectObjectsAction[T] = XXSelectObjectsAction(self, values, Nil, false)(new XXSelectConfig[T]())
    }
    
    case class XXSelectObjectsAction[T](self : F, values : List[T], selecting : List[Int], order : Boolean)(config : XXSelectConfig[T] = new XXSelectConfig[T]())(implicit convert : Convert[T]) extends ForcedAction with Soft with SelfPerform {
        import config._

        def withExtra(l : List[UserAction]) = copy()(config.copy(extra = l))
        def withRule(r : ObjectSetRule[T]) = copy()(config.copy(rule = r))
        def withRule(f : ObjectSetRule[T] => ObjectSetRule[T]) = copy()(config.copy(rule = f(rule)))
        def withGroup(e : Elem) = copy()(config.copy(group = (_, _ : List[T]) => e))
        def withGroupDyn(f : List[T] => Elem) = copy()(config.copy(group = (_, l : List[T]) => f(l)))
        def withSplit(l : List[Int]) = copy()(config.copy(splitAt = l.scanLeft(-1)(_ + _)))
        def withSplitAt(l : List[Int]) = copy()(config.copy(splitAt = l))
        def withBreak(e : Elem) = copy()(config.copy(break = _ => e))
        def withBreak(f : Int => Elem) = copy()(config.copy(break = f))
        def withThenDone(f : List[T] => ForcedAction) = withThenElem(f)("Done".hl)
        def withThenElem(f : List[T] => ForcedAction)(e : Elem) = copy()(config.copy(thens = (value : List[T], selecting : List[Int], valid : Boolean) => $(XXObjectsSelectedAction(self, e, valid.?(f(selecting./(values.apply))).|(null)))))
        def withThen(f : List[T] => ForcedAction)(m : List[T] => Elem) = withThenAlt(f)(m)(m(Nil))
        def withThenAlt(f : List[T] => ForcedAction)(m : List[T] => Elem)(a : Elem) = copy()(config.copy(thens = (value : List[T], selecting : List[Int], valid : Boolean) => {
            val r = selecting./(values.apply)
            $(XXObjectsSelectedAction(self, r.any.?(m(r)).|(a), valid.?(f(r)).|(null)))
        }))
  
        def perform(g : G) = {
            val r = selecting./(values.apply)
            val q = group(g, r)
            val v = rule.validPrefix(r) && rule.validSet(r)
            
            Ask(self, $(XXSelectObjectsExplodeAction(self, values, order)(config)) ++ 
                $(RawElemInfo(q)(Empty)) ++
                splitAt.zipWithIndex.%(_._1 == -1)./(_._2)./(i => RawElemInfo(q)(break(i))) ++
                values.zipWithIndex./~{ case (d, n) => {
                    $(if (selecting.has(n)) {
                        val z = selecting.but(n)
                        val o = XXDeselectObjectAction(self, values, order, d, n, selecting, z)(config)
                        o.x(rule.validPrefix(z./(values.apply)).not)
                    }
                    else {
                        val z = order.?(selecting :+ n).|((selecting :+ n).sorted)
                        val o = XXSelectObjectAction(self, values, order, d, n, selecting, z)(config)
                        o.x(rule.validPrefix(z./(values.apply)).not)
                    }) ++ splitAt.zipWithIndex.%(_._1 == n)./(_._2)./(i => RawElemInfo(Empty)(break(i)))
                }} ++
                thens(values, selecting, v)./(then => then.x(v.not)) ++
                extra)
        }
    }
        
    case class   XXSelectObjectAction[T](self : F, values : List[T], order : Boolean, obj : T, n : Int, selecting : List[Int], z : List[Int])(config : XXSelectConfig[T])(implicit convert : Convert[T]) extends BaseAction()(g => convert.convert(g, values, n, selecting)) with ViewObject[T] with SkipValidate with NoClear with SelfPerform with Soft with NoExplode {
        def perform(g : G) = XXSelectObjectsAction(self, values, z, order)(config)
    }
 
    case class XXDeselectObjectAction[T](self : F, values : List[T], order : Boolean, obj : T, n : Int, selecting : List[Int], z : List[Int])(config : XXSelectConfig[T])(implicit convert : Convert[T]) extends BaseAction()(g => convert.convert(g, values, n, selecting)) with ViewObject[T] with SkipValidate with NoClear with SelfPerform with Soft with NoExplode with Selected {
        def perform(g : G) = XXSelectObjectsAction(self, values, z, order)(config)
    }
    
    case class XXObjectsSelectedAction[T](self : F, o : Elem, then : ForcedAction) extends BaseAction()(o) with ThenAction
    
    case class XXSelectObjectsExplodeAction[T](self : F, values : List[T], order : Boolean)(config : XXSelectConfig[T])(implicit convert : Convert[T]) extends HiddenChoice with HalfExplode {
        import config._

        lazy val (prefixes, combinations) = {
            var result = Nil
        
            var prefixes = $(List[Int]())
        
            if (order) {
                var current = $(List[Int]())
                while (current.any) {
                    prefixes ++= current
                    current = 0.until(values.num)./~(i => current.%!(_.has(i))./(_ :+ i)).%(s => rule.validPrefix(s./(values.apply)))
                }
            }
            else
                0.until(values.num).foreach { i =>
                    prefixes = prefixes ++ prefixes./(_ :+ i).%(s => rule.validPrefix(s./(values.apply)))
                }
        
            val combinations = prefixes.%(s => rule.validPrefix(s./(values.apply))).%(s => rule.validSet(s./(values.apply)))
        
            (prefixes, combinations)
        }
        
        def expand(withBack : Boolean) = combinations./~(c => thens(values, c, true))
    }
 

    case class YYSelectConfig[T](extra : List[UserAction] = Nil, 
        rule : ObjectOneRule[T] = new OfOne[T],
        single : Boolean = false,
        group : (G, Option[T]) => Elem = (g : G, s : Option[T]) => Empty,
        splitAt : List[Int] = Nil, 
        break : Int => Elem = (n : Int) => HorizontalBreak,
        thens : (List[T], Option[Int]) => List[UserAction] = (l : List[T], s : Option[Int]) => Nil)
            
    object YYSelectObjectsAction {
        def apply[T](self : F, values : List[T])(implicit convert : Convert[T]) : YYSelectObjectsAction[T] = YYSelectObjectsAction(self, values, None)(new YYSelectConfig[T]())
    }
    
    case class YYSelectObjectsAction[T](self : F, values : List[T], selecting : Option[Int])(config : YYSelectConfig[T] = new YYSelectConfig[T]())(implicit convert : Convert[T]) extends ForcedAction with Soft with SelfPerform {
        import config._

        def withExtra(l : List[UserAction]) = copy()(config.copy(extra = l))
        def withRule(r : ObjectOneRule[T]) = copy()(config.copy(rule = r))
        def withRuleT(f : ObjectOneRule[T] => ObjectOneRule[T]) = copy()(config.copy(rule = f(rule)))
        def withRuleOnly(l : List[T]) = copy()(config.copy(rule = rule.only(l)))
        def withRuleExcept(l : List[T]) = copy()(config.copy(rule = rule.except(l)))
        def withRule(f : T => Boolean) = copy()(config.copy(rule = rule.each(f)))
        def withGroup(e : Elem) = copy()(config.copy(group = (_, _ : Option[T]) => e))
        def withSplit(l : List[Int]) = copy()(config.copy(splitAt = l.scanLeft(-1)(_ + _)))
        def withSplitAt(l : List[Int]) = copy()(config.copy(splitAt = l))
        def withBreak(e : Elem) = copy()(config.copy(break = _ => e))
        def withBreak(f : Int => Elem) = copy()(config.copy(break = f))
        def withThenDone(f : T => ForcedAction) = withThenElem(f)("Done".hl)
        def withThenElem(f : T => ForcedAction)(e : Elem) = withThen(f)(_ => e)(e)
        def withThen(f : T => ForcedAction)(m : T => Elem)(a : Elem) = copy()(config.copy(thens = (value : List[T], selecting : Option[Int]) => {
            val o = selecting./(values.apply)
            $(YYObjectsSelectedAction(self, o./(m).|(a), o./(f).|(null)))
        }))
        def withThens(f : T => List[UserAction]) = copy()(config.copy(thens = (value : List[T], selecting : Option[Int]) => selecting.any.??(f(values(selecting.get)))))
  
        def perform(g : G) = {
            val r = selecting./(values.apply)
            val q = group(g, r)
            val v = selecting./(values.apply)./(rule.validOne).has(true)
            
            Ask(self, $(YYSelectObjectsExplodeAction(self, values)(config)) ++ 
                $(RawElemInfo(q)(Empty)) ++
                splitAt.zipWithIndex.%(_._1 == -1)./(_._2)./(i => RawElemInfo(q)(break(i))) ++
                values.zipWithIndex./~{ case (d, n) => {
                    $(if (selecting.has(n)) {
                        val o = YYDeselectObjectAction(self, values, d, n, selecting)(config)
                        o
                    }
                    else {
                        val o = YYSelectObjectAction(self, values, d, n, selecting)(config)
                        o.x(rule.validOne(values(n)).not)
                    }) ++ splitAt.zipWithIndex.%(_._1 == n)./(_._2)./(i => RawElemInfo(Empty)(break(i)))
                }} ++
                thens(values, selecting)./(then => then.x(v.not)) ++
                extra)
        }
    }
        
    case class   YYSelectObjectAction[T](self : F, values : List[T], obj : T, n : Int, selecting : Option[Int])(config : YYSelectConfig[T])(implicit convert : Convert[T]) extends     BaseAction()(g => convert.convert(g, values, n, selecting.toList)) with ViewObject[T] with Soft with NoExplode with NoClear with SelfPerform {
        def perform(g : G) = YYSelectObjectsAction(self, values, Some(n))(config)
    }
 
    case class YYDeselectObjectAction[T](self : F, values : List[T], obj : T, n : Int, selecting : Option[Int])(config : YYSelectConfig[T])(implicit convert : Convert[T]) extends BaseUserAction()(g => convert.convert(g, values, n, selecting.toList)) with ViewObject[T] with Info with Selected with OnClickInfo {
        def param = obj
    }
    
    case class YYObjectsSelectedAction[T](self : F, o : Elem, then : ForcedAction) extends BaseAction()(o) with ThenAction
    
    case class YYSelectObjectsExplodeAction[T](self : F, values : List[T])(config : YYSelectConfig[T])(implicit convert : Convert[T]) extends HiddenChoice with HalfExplode {
        import config._

        lazy val combinations = 0.until(values.num).%(s => rule.validOne(values(s))).toList
        
        def expand(withBack : Boolean) =
            combinations./~(c => thens(values, Some(c))) ++ {
                combinations./~(p => 
                    combinations./(i => 
                        YYSelectObjectAction(self, values, values(i), i, Some(p).but(i))(config) 
                    )
                )
            }
    }
    
}

