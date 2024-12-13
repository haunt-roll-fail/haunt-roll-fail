package hrf.base
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

import hrf.elem._


trait SelectSubset { self : Gaming =>

    trait ObjectOneRule[T] { rule =>
        def validOne(o : T) : Boolean

        abstract class Same extends ObjectOneRule[T] {
            final def validOne(o : T) = rule.validOne(o) && valid(o)
            def valid(o : T) = true
        }

        def only(x : $[T]) : ObjectOneRule[T] = new Same {
            override def valid(o : T) = x.has(o)
        }

        def except(x : $[T]) : ObjectOneRule[T] = new Same {
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
        def validPrefix(l : $[T]) : Boolean
        def validSet(l : $[T]) : Boolean

        abstract class Same extends ObjectSetRule[T] {
            final def validPrefix(l : $[T]) = rule.validPrefix(l) && prefix(l)
            final def validSet(l : $[T]) = rule.validSet(l) && set(l)
            def prefix(l : $[T]) = true
            def set(l : $[T]) = true
        }

        def each(f : T => Boolean) : ObjectSetRule[T] = new Same {
            override def prefix(l : $[T]) = l.forall(f)
        }

        def all(f : $[T] => Boolean) = new Same {
            override def prefix(l : $[T]) = f(l)
        }

        def matches(f : $[T] => Boolean) = new Same {
            override def set(l : $[T]) = f(l)
        }

        def except(x : $[T]) : ObjectSetRule[T] = new Same {
            override def prefix(l : $[T]) = l.intersect(x).none
        }

        def only(x : $[T]) : ObjectSetRule[T] = new Same {
            override def prefix(l : $[T]) = l.diff(x).none
        }

        def num(n : Int) = new Same {
            override def prefix(l : $[T]) = l.num <= n
            override def set(l : $[T]) = l.num == n
        }

        def upTo(n : Int) = new Same {
            override def prefix(l : $[T]) = l.num <= n
        }

        def atLeast(n : Int) = new Same {
            override def set(l : $[T]) = l.num >= n
        }
    }

    class OfSet[T](minimum : Int) extends ObjectSetRule[T] {
        def validPrefix(l : $[T]) = true
        def validSet(l : $[T]) = l.num >= minimum
    }


    case class Convert[T](convert : (G, $[T], Int, $[Int]) => Elem)

    object Convert {
        implicit def convertToConvertGVNS[T](implicit convert : (G, $[T], Int, $[Int]) => Elem) = Convert(convert)
        implicit def convertToConvertVNS [T](implicit convert : (   $[T], Int, $[Int]) => Elem) = Convert((g : G, l : $[T], i : Int, selected : $[Int]) => convert(l, i, selected))
        implicit def convertToConvertGTBM[T](implicit convert : (G,   T,         Int ) => Elem) = Convert((g : G, l : $[T], i : Int, selected : $[Int]) => convert(g, l(i), selected.count(i)))
        implicit def convertToConvertGTB [T](implicit convert : (G,   T, Boolean     ) => Elem) = Convert((g : G, l : $[T], i : Int, selected : $[Int]) => convert(g, l(i), selected.has(i)))
        implicit def convertToConvertGT  [T](implicit convert : (G,   T              ) => Elem) = Convert((g : G, l : $[T], i : Int, selected : $[Int]) => convert(g, l(i)))
        implicit def convertToConvertTM  [T](implicit convert : (     T,         Int ) => Elem) = Convert((g : G, l : $[T], i : Int, selected : $[Int]) => convert(l(i), selected.count(i)))
        implicit def convertToConvertTB  [T](implicit convert : (     T, Boolean     ) => Elem) = Convert((g : G, l : $[T], i : Int, selected : $[Int]) => convert(l(i), selected.has(i)))
        implicit def convertToConvertT   [T](implicit convert :       T                => Elem) = Convert((g : G, l : $[T], i : Int, selected : $[Int]) => convert(l(i)))
    }

    case class XXSelectConfig[T](extra : $[UserAction] = $,
        rule : ObjectSetRule[T] = new OfSet[T](1),
        multipleSelects : Int => Int = _ => 1,
        group : (G, $[T]) => Any = (g : G, s : $[T]) => Empty,
        splitAt : $[Int] = $,
        break : Int => Any = (n : Int) => HorizontalBreak,
        thens : ($[T], $[Int], Boolean) => $[UserAction] = (selecting : $[T], indices : $[Int], valid : Boolean) => Nil,
        auto : ($[T], $[Int]) => |[UserAction] = (selecting : $[T], indices : $[Int]) => None)

    object XXSelectObjectsAction {
        def apply[T]  (self : F, values : $[T]                 )(implicit convert : Convert[T]) : XXSelectObjectsAction[T] = XXSelectObjectsAction(self, values, $, false)(new XXSelectConfig[T]())
        def apply[T]  (self : F, values : $[T], order : Boolean)(implicit convert : Convert[T]) : XXSelectObjectsAction[T] = XXSelectObjectsAction(self, values, $, order)(new XXSelectConfig[T]())
    }

    case class XXSelectObjectsAction[T](self : F, values : $[T], selecting : $[Int], order : Boolean)(config : XXSelectConfig[T] = new XXSelectConfig[T]())(implicit convert : Convert[T]) extends ForcedAction with Soft with SelfPerform {
        import config._

        def withExtra(l : $[UserAction]) = copy()(config.copy(extra = l))
        def withExtras(l : UserAction*) = copy()(config.copy(extra = l.$))
        def withRule(r : ObjectSetRule[T]) = copy()(config.copy(rule = r))
        def withRule(f : ObjectSetRule[T] => ObjectSetRule[T]) = copy()(config.copy(rule = f(rule)))
        def withRuleNone(f : ObjectSetRule[T] => ObjectSetRule[T]) = copy()(config.copy(rule = f(new OfSet[T](0))))

        def withMultipleSelects(f : T => Int) = copy()(config.copy(multipleSelects = i => f(values(i))))

        def withGroup(a : Any*) = copy()(config.copy(group = (_, _ : $[T]) => a.$))
        def withGroupDyn(f : $[T] => Any) = copy()(config.copy(group = (_, l : $[T]) => f(l)))


        def withGrouping[K](key : T => K)(desc : T => Elem) = withSplitAt(values.indices.map(_ - 1).$).withBreak(n => (n < values.num && (n == 0 || key(values(n)) != key(values(n - 1)))).?(HorizontalBreak ~ HGap ~ HorizontalBreak ~ HGap ~ HorizontalBreak ~ HGap ~ HorizontalBreak ~ desc(values(n)) ~ HorizontalBreak))

        def withSplit(l : $[Int]) = copy()(config.copy(splitAt = l.scanLeft(-1)(_ + _)))
        def withSplitAt(l : $[Int]) = copy()(config.copy(splitAt = l))
        def withBreak(e : Elem) = copy()(config.copy(break = _ => e))
        def withBreak(f : Int => Elem) = copy()(config.copy(break = f))
        def withAuto(f : $[T] => UserAction) = copy()(config.copy(auto = (selecting : $[T], indices : $[Int]) => |(f(selecting))))
        def withAutoIndex(f : $[Int] => UserAction) = copy()(config.copy(auto = (selecting : $[T], indices : $[Int]) => |(f(indices))))
        def withThenDone(f : $[T] => ForcedAction) = withThenElem(f)("Done".hl)
        def withThenElem(f : $[T] => ForcedAction)(e : Elem) = copy()(config.copy(thens = (selecting : $[T], indices : $[Int], valid : Boolean) => $(XXObjectsSelectedAction(self, e, selecting, valid.?(f(selecting)).|(null)))))
        def withThen(f : $[T] => ForcedAction)(m : $[T] => Any) = withThenAlt(f)(m)(m($))
        def withThenAlt(f : $[T] => ForcedAction)(m : $[T] => Any)(a : Any) = copy()(config.copy(thens = (selecting : $[T], indices : $[Int], valid : Boolean) => {
            $(XXObjectsSelectedAction(self, selecting.any.?(m(selecting)).|(a), selecting, valid.?(f(selecting)).|(null)))
        }))
        def withThenList[S](l : $[S])(f : (S, $[T]) => ForcedAction)(m : (S, $[T]) => Elem) = copy()(config.copy(thens = (selecting : $[T], indices : $[Int], valid : Boolean) => {
            l./(s => XXObjectsSelectedAction(self, selecting.any.?(m(s, selecting)).|(m(s, $)), selecting, valid.?(f(s, selecting)).|(null)))
        }))
        def withThens(f : $[T] => $[UserAction]) = copy()(config.copy(thens = (selecting : $[T], indices : $[Int], valid : Boolean) => f(selecting)))

        def perform(soft : Void)(implicit g : G) : Ask = ask(g)

        def ask(implicit game : G) : Ask = {
            val r = selecting./(values.apply)
            val q = game.desc(group(game, r))
            val v = rule.validPrefix(r) && rule.validSet(r)

            val a = v.??(config.auto(values, selecting))

            if (a.any)
                Ask(self)(a.get)
            else
                Ask(self)
                    .add(XXSelectObjectsExplodeAction(self, values, order)(config))
                    .add(RawElemInfo(q)(Empty))
                    .add(splitAt.indexed.%(_ == -1).indices./(i => RawElemInfo(q)(game.desc(break(i)))))
                    .add(values.indexed./~{ (d, n) =>
                        var deselect = selecting.has(n)

                        if (deselect) {
                            val m = multipleSelects(n)
                            if (m > 1 && m > selecting.count(n)) {
                                val z = order.?(selecting :+ n).|((selecting :+ n).sorted)
                                if (rule.validPrefix(z./(values.apply))) {
                                    deselect = false
                                }
                            }
                        }

                        $(if (deselect) {
                            val z = selecting.but(n)
                            val o = XXDeselectObjectAction(self, values, order, d, n, selecting, z)(config)
                            o.!(rule.validPrefix(z./(values.apply)).not)
                        }
                        else {
                            val z = order.?(selecting :+ n).|((selecting :+ n).sorted)
                            val o = XXSelectObjectAction(self, values, order, d, n, selecting, z)(config)
                            o.!(rule.validPrefix(z./(values.apply)).not)
                        }) ++ splitAt.indexed.%(_ == n).indices./(i => RawElemInfo(Empty)(game.desc(break(i))))
                    })
                    .add(thens(r, selecting, v)./(then => then.!(v.not)))
                    .add(extra)
        }
    }

    case class   XXSelectObjectAction[T](self : F, values : $[T], order : Boolean, obj : T, n : Int, selecting : $[Int], z : $[Int])(config : XXSelectConfig[T])(implicit convert : Convert[T]) extends BaseAction()(g => convert.convert(g, values, n, selecting)) with ViewObject[T] with SkipValidate with NoClear with SelfPerform with Soft with NoExplode {
        def perform(soft : Void)(implicit g : G) = XXSelectObjectsAction(self, values, z, order)(config)
    }

    case class XXDeselectObjectAction[T](self : F, values : $[T], order : Boolean, obj : T, n : Int, selecting : $[Int], z : $[Int])(config : XXSelectConfig[T])(implicit convert : Convert[T]) extends BaseAction()(g => convert.convert(g, values, n, selecting)) with ViewObject[T] with SkipValidate with NoClear with SelfPerform with Soft with NoExplode with Selected {
        def perform(soft : Void)(implicit g : G) = XXSelectObjectsAction(self, values, z, order)(config)
    }

    case class XXObjectsSelectedAction[T](self : F, o : Any, values : $[T], then : ForcedAction) extends BaseAction()(o) with WrappedAction

    trait XXSelected {
        def indices : $[Int]
    }

    case class XXSelectObjectsExplodeAction[T](self : F, values : $[T], order : Boolean)(config : XXSelectConfig[T])(implicit convert : Convert[T]) extends HiddenChoice with HalfExplode {
        import config._

        lazy val (prefixes, combinations) = {
            var result = $

            var prefixes = $($[Int]())

            if (order) {
                var current = $($[Int]())
                while (current.any) {
                    prefixes ++= current
                    current = 0.until(values.num)./~(i => current.%!(_.has(i))./(_ :+ i)).%(s => rule.validPrefix(s./(values.apply)))
                }
            }
            else
                0.until(values.num).foreach { i =>
                    var j = multipleSelects(i)
                    while ({
                        val pp = prefixes./(_ :+ i).%(s => rule.validPrefix(s./(values.apply)))
                        prefixes ++= pp
                        j -= 1
                        j > 0 && pp.any
                    }) {}
                }

            val combinations = prefixes.%(s => rule.validPrefix(s./(values.apply))).%(s => rule.validSet(s./(values.apply)))

            (prefixes, combinations)
        }

        def expand(target : |[Action]) = target./~(_.unwrap.as[XXSelected])./(s => $(s.indices)).|(combinations)./~(s => auto(s./(values.apply), s)./(a => $(a)).|(thens(s./(values.apply), s, true)))
    }


    case class YYSelectConfig[T](extra : $[UserAction] = $,
        rule : ObjectOneRule[T] = new OfOne[T],
        group : (G, |[T]) => Any = (g : G, s : |[T]) => Empty,
        splitAt : $[Int] = Nil,
        break : Int => Elem = (n : Int) => HorizontalBreak,
        thens : ($[T], |[Int]) => $[UserAction] = (l : $[T], s : |[Int]) => Nil,
        bail : $[UserAction] = Nil)

    object YYSelectObjectsAction {
        def apply[T](self : F, values : $[T])(implicit convert : Convert[T]) : YYSelectObjectsAction[T] = YYSelectObjectsAction(self, values, None)(new YYSelectConfig[T]())
    }

    case class YYSelectObjectsAction[T](self : F, values : $[T], selecting : |[Int])(config : YYSelectConfig[T] = new YYSelectConfig[T]())(implicit convert : Convert[T]) extends ForcedAction with Soft with SelfPerform {
        import config._

        def withExtra(l : $[UserAction]) = copy()(config.copy(extra = l))
        def withExtras(l : UserAction*) = copy()(config.copy(extra = l.$))
        def withBail(l : $[UserAction]) = copy()(config.copy(bail = l))
        def withRule(r : ObjectOneRule[T]) = copy()(config.copy(rule = r))
        def withRuleT(f : ObjectOneRule[T] => ObjectOneRule[T]) = copy()(config.copy(rule = f(rule)))
        def withRuleOnly(l : $[T]) = copy()(config.copy(rule = rule.only(l)))
        def withRuleExcept(l : $[T]) = copy()(config.copy(rule = rule.except(l)))
        def withRule(f : T => Boolean) = copy()(config.copy(rule = rule.each(f)))
        def withGroup(a : Any*) = copy()(config.copy(group = (_, _ : |[T]) => a.$))
        def withSplit(l : $[Int]) = copy()(config.copy(splitAt = l.scanLeft(-1)(_ + _)))
        def withSplitAt(l : $[Int]) = copy()(config.copy(splitAt = l))
        def withBreak(e : Elem) = copy()(config.copy(break = _ => e))
        def withBreak(f : Int => Elem) = copy()(config.copy(break = f))
        def withThenDone(f : T => ForcedAction) = withThenElem(f)("Done".hl)
        def withThenElem(f : T => ForcedAction)(e : Elem) = withThen(f)(_ => e)(e)
        def withThen(f : T => ForcedAction)(m : T => Any)(a : Any*) = copy()(config.copy(thens = (value : $[T], selecting : |[Int]) => {
            val o = selecting./(values.apply)
            $(YYObjectsSelectedAction(self, o./(m).|(a.$), o./(f).|(null)))
        }))
        def withThens(f : T => $[UserAction]) = copy()(config.copy(thens = (value : $[T], selecting : |[Int]) => selecting.any.??(f(values(selecting.get)))))
        def withThensInfo(f : T => $[UserAction])(info : $[Info]) = copy()(config.copy(thens = (value : $[T], selecting : |[Int]) => selecting.any.?(f(values(selecting.get))).|(info)))

        def perform(soft : Void)(implicit g : G) : Ask = ask(g)

        def ask(implicit game : G) : Ask = {
            val r = selecting./(values.apply)
            val q = game.desc(group(game, r))
            val v = selecting./(values.apply)./(rule.validOne).has(true)

            Ask(self, $(YYSelectObjectsExplodeAction(self, values)(config)) ++
                $(RawElemInfo(q)(Empty)) ++
                splitAt.indexed.%(_ == -1).indices./(i => RawElemInfo(q)(break(i))) ++
                values.indexed./~{ (d, n) => {
                    $(if (selecting.has(n)) {
                        val o = YYDeselectObjectAction(self, values, d, n, selecting)(config)
                        o
                    }
                    else {
                        val o = YYSelectObjectAction(self, values, d, n, selecting)(config)
                        o.x(rule.validOne(values(n)).not)
                    }) ++ splitAt.indexed.%(_ == n).indices./(i => RawElemInfo(Empty)(break(i)))
                }} ++
                thens(values, selecting)./(then => then.x(v.not)) ++
                extra ++
                values.exists(rule.validOne).not.??(bail)
            )
        }

    }

    case class   YYSelectObjectAction[T](self : F, values : $[T], obj : T, n : Int, selecting : |[Int])(config : YYSelectConfig[T])(implicit convert : Convert[T]) extends     BaseAction()(g => convert.convert(g, values, n, selecting.$)) with ViewObject[T] with Soft with NoExplode with NoClear with SelfPerform {
        def perform(soft : Void)(implicit g : G) = YYSelectObjectsAction(self, values, Some(n))(config)
    }

    case class YYDeselectObjectAction[T](self : F, values : $[T], obj : T, n : Int, selecting : |[Int])(config : YYSelectConfig[T])(implicit convert : Convert[T]) extends BaseUserAction()(g => convert.convert(g, values, n, selecting.$)) with ViewObject[T] with Info with Selected with OnClickInfo {
        def param = obj
    }

    case class YYObjectsSelectedAction[T](self : F, o : Any, then : ForcedAction) extends BaseAction()(o) with WrappedAction

    case class YYSelectObjectsExplodeAction[T](self : F, values : $[T])(config : YYSelectConfig[T])(implicit convert : Convert[T]) extends HiddenChoice with HalfExplode {
        import config._

        lazy val combinations = 0.until(values.num).%(s => rule.validOne(values(s))).$

        def expand(target : |[Action]) =
            combinations./~(c => thens(values, Some(c))) ++ {
                combinations./~(p =>
                    combinations./(i =>
                        YYSelectObjectAction(self, values, values(i), i, Some(p).but(i))(config)
                    )
                )
            }
    }

}
