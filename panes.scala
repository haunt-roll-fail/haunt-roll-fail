package hrf.ui
//
//
//
//
import logger._, colmat._
//
//
//
//

import org.scalajs.dom

import colmat._

import hrf.web._
import hrf.elem._

import scala.collection.mutable

trait Pane {
    def dim(x : Int, y : Int, w : Int, h : Int) : Unit
    var s : Double = 0.0
}

class SplitHor(a : Pane, b : Pane) extends Pane {
    val cached = mutable.Map[Int, Int]()
    
    def dim(x : Int, y : Int, w : Int, h : Int) {
        val bi = cached.getOrElseUpdate(w * 10000 + h, {
            var bi = 0
            var bs = 0.0
            var bt = 0.0
            0.to(w).%(_ % 81 == 0).foreach { i =>
                a.dim(x, y, i, h)
                b.dim(x + i, y, w - i, h)
                val s = min(a.s, b.s)
                val t = max(a.s, b.s)
                if (s > bs) {
                    bs = s
                    bt = t
                    bi = i
                }
                else 
                if (s == bs && t > bt) {
                    bs = s
                    bt = t
                    bi = i
                }

            }
            (bi - 80).to(bi + 80).%(_ >= 0).%(_ <= w).%(_ % 27 == 0).foreach { i =>
                a.dim(x, y, i, h)
                b.dim(x + i, y, w - i, h)
                val s = min(a.s, b.s)
                val t = max(a.s, b.s)
                if (s > bs) {
                    bs = s
                    bt = t
                    bi = i
                }
                else 
                if (s == bs && t > bt) {
                    bs = s
                    bt = t
                    bi = i
                }
            }
            (bi - 26).to(bi + 26).%(_ >= 0).%(_ <= w).%(_ % 9 == 0).foreach { i =>
                a.dim(x, y, i, h)
                b.dim(x + i, y, w - i, h)
                val s = min(a.s, b.s)
                val t = max(a.s, b.s)
                if (s > bs) {
                    bs = s
                    bt = t
                    bi = i
                }
                else 
                if (s == bs && t > bt) {
                    bs = s
                    bt = t
                    bi = i
                }
            }
            (bi - 8).to(bi + 8).%(_ >= 0).%(_ <= w).%(_ % 3 == 0).foreach { i =>
                a.dim(x, y, i, h)
                b.dim(x + i, y, w - i, h)
                val s = min(a.s, b.s)
                val t = max(a.s, b.s)
                if (s > bs) {
                    bs = s
                    bt = t
                    bi = i
                }
                else 
                if (s == bs && t > bt) {
                    bs = s
                    bt = t
                    bi = i
                }
            }
            (bi - 2).to(bi + 2).%(_ >= 0).%(_ <= w).foreach { i =>
                a.dim(x, y, i, h)
                b.dim(x + i, y, w - i, h)
                val s = min(a.s, b.s)
                val t = max(a.s, b.s)
                if (s > bs) {
                    bs = s
                    bt = t
                    bi = i
                }
                else 
                if (s == bs && t > bt) {
                    bs = s
                    bt = t
                    bi = i
                }
            }
        
            bi
        })
        
        val bi000 = cached.getOrElseUpdate(w * 10000 + h, { ===("WTF"); 0 })

        a.dim(x, y, bi, h)
        b.dim(x + bi, y, w - bi, h)
        s = min(a.s, b.s)
    }
}

object SplitHor {
    val cached = mutable.Map[(Pane, Seq[Pane]), SplitHor]()

    def apply(a : Pane, panes : Pane*) : Pane = panes.none.?(a).|(cached.getOrElseUpdate((a, panes), { new SplitHor(a, SplitHor(panes.head, panes.drop(1) : _*)) }))
}

class SplitVer(a : Pane, b : Pane) extends Pane {
    var cached = mutable.Map[Int, Int]()
    
    def dim(x : Int, y : Int, w : Int, h : Int) {
        val bj = cached.getOrElseUpdate(w * 10000 + h, {
            var bj = 0
            var bs = 0.0
            var bt = 0.0
            0.to(h).%(_ % 81 == 0).foreach { j =>
                a.dim(x, y, w, j)
                b.dim(x, y + j, w, h - j)
                val s = min(a.s, b.s)
                val t = max(a.s, b.s)
                if (s > bs) {
                    bs = s
                    bt = t
                    bj = j
                }
                else 
                if (s == bs && t > bt) {
                    bs = s
                    bt = t
                    bj = j
                }
            }
            (bj - 80).to(bj + 80).%(_ >= 0).%(_ <= h).%(_ % 27 == 0).foreach { j =>
                a.dim(x, y, w, j)
                b.dim(x, y + j, w, h - j)
                val s = min(a.s, b.s)
                val t = max(a.s, b.s)
                if (s > bs) {
                    bs = s
                    bt = t
                    bj = j
                }
                else 
                if (s == bs && t > bt) {
                    bs = s
                    bt = t
                    bj = j
                }
            }
            (bj - 26).to(bj + 26).%(_ >= 0).%(_ <= h).%(_ % 9 == 0).foreach { j =>
                a.dim(x, y, w, j)
                b.dim(x, y + j, w, h - j)
                val s = min(a.s, b.s)
                val t = max(a.s, b.s)
                if (s > bs) {
                    bs = s
                    bt = t
                    bj = j
                }
                else 
                if (s == bs && t > bt) {
                    bs = s
                    bt = t
                    bj = j
                }
            }
            (bj - 8).to(bj + 8).%(_ >= 0).%(_ <= h).%(_ % 3 == 0).foreach { j =>
                a.dim(x, y, w, j)
                b.dim(x, y + j, w, h - j)
                val s = min(a.s, b.s)
                val t = max(a.s, b.s)
                if (s > bs) {
                    bs = s
                    bt = t
                    bj = j
                }
                else 
                if (s == bs && t > bt) {
                    bs = s
                    bt = t
                    bj = j
                }
            }
            (bj - 2).to(bj + 2).%(_ >= 0).%(_ <= h).foreach { j =>
                a.dim(x, y, w, j)
                b.dim(x, y + j, w, h - j)
                val s = min(a.s, b.s)
                val t = max(a.s, b.s)
                if (s > bs) {
                    bs = s
                    bt = t
                    bj = j
                }
                else 
                if (s == bs && t > bt) {
                    bs = s
                    bt = t
                    bj = j
                }
            }

            bj
        })

        a.dim(x, y, w, bj)
        b.dim(x, y + bj, w, h - bj)
        s = min(a.s, b.s)
    }
}

object SplitVer {
    val cached = mutable.Map[(Pane, Seq[Pane]), SplitVer]()

    def apply(a : Pane, panes : Pane*) : Pane = panes.none.?(a).|(cached.getOrElseUpdate((a, panes), { new SplitVer(a, SplitVer(panes.head, panes.drop(1) : _*)) }))
}

class SplitHorEven(panes : List[Pane]) extends Pane {
    def dim(x : Int, y : Int, w : Int, h : Int) {
        var n = x
        s = 0.until(panes.num)./{ i => 
            val ww = (w + i) / panes.num
            n += ww
            panes(i).dim(n - ww, y, ww, h)
            panes(i).s
        }.min
    }
}

object SplitHorEven {
    def apply(panes : List[Pane]) = new SplitHorEven(panes)
}

class SplitVerEven(panes : List[Pane]) extends Pane {
    def dim(x : Int, y : Int, w : Int, h : Int) {
        var n = y
        s = 0.until(panes.num)./{ j => 
            val hh = (h + j) / panes.num
            n += hh
            panes(j).dim(x, n - hh, w, hh)
            panes(j).s
        }.min
    }
}

object SplitVerEven {
    def apply(panes : List[Pane]) = new SplitVerEven(panes)
}

class SplitEither(a : Pane, b : Pane) extends Pane {
    def dim(x : Int, y : Int, w : Int, h : Int) {
        a.dim(x, y, w, h)
        b.dim(x, y, w, h)

        if (a.s >= b.s) {
            a.dim(x, y, w, h)
            s = a.s
        }
        else {
            b.dim(x, y, w, h)
            s = b.s
        }
    }
}

object SplitEither {
    val cached = mutable.Map[(Pane, Pane), SplitEither]()
    
    def apply(a : Pane, b : Pane) = cached.getOrElseUpdate((a, b), new SplitEither(a, b))
}


class SplitEither3(a : Pane, b : Pane, c : Pane) extends Pane {
    def dim(x : Int, y : Int, w : Int, h : Int) {
        a.dim(x, y, w, h)
        b.dim(x, y, w, h)
        c.dim(x, y, w, h)

        if (a.s >= b.s && a.s >= c.s) {
            a.dim(x, y, w, h)
            s = a.s
        }
        else
        if (b.s >= a.s && b.s >= c.s) {
            b.dim(x, y, w, h)
            s = b.s 
        }
        else {
            c.dim(x, y, w, h)
            s = c.s
        }
    }
}

object SplitEven {
    def apply(panes : List[Pane]) = SplitEither(SplitVerEven(panes), SplitHorEven(panes))
}

object SplitX {
    def apply(a : Pane, panes : Pane*) : Pane = panes.none.?(a).|(SplitEither(SplitVer(a, panes : _*), SplitHor(a, panes : _*)))
}

object SplitY {
    def apply(a : Pane, b : Pane, c : Pane) : Pane = SplitEither(SplitX(a, SplitX(b, c)), SplitX(b, SplitX(a, c)))
    def apply(a : Pane, b : Pane, c : Pane, d : Pane) : Pane = new SplitEither3(SplitY(a, b, SplitX(c, d)), SplitY(a, c, SplitX(b, d)), SplitY(b, c, SplitX(a, d)))
}

class SplitZ(panes : List[Pane]) extends Pane {
    var cached = mutable.Map[Int, List[Pane]]()

    def dim(x : Int, y : Int, w : Int, h : Int) {
        val c = cached.getOrElseUpdate(w * 10000 + h, {
            val ss = 1.to(panes.num - 1)./~(panes.combinations)

            var bc = ss.head
            var bs = 0.0
            var bt = 0.0
            
            ss.foreach { p =>
                val q = panes.diff(p)

                if (p.contains(panes(0))) {
                    val pp = SplitZ(p : _*)
                    val qq = SplitZ(q : _*)

                    val v = SplitX(pp, qq).dim(x, y, w, h)

                    val ss = panes./(_.s).sorted
                    val s = ss(0)
                    val t = ss(1)

                    if (panes.num == 4) {
                        ===(p./(panes.indexOf).mkString(" | ") + " <<< " + q./(panes.indexOf).mkString(" | "))
                        ===("s = " + s)
                        ===("t = " + t)
                    }

                    if (s > bs) {
                        bs = s
                        bt = t
                        bc = p
                    }
                    else 
                    if (s == bs && t > bt) {
                        bs = s
                        bt = t
                        bc = p
                    }
                }
            }
            
            bc
        })
     
        val p = SplitX(SplitZ(c : _*), SplitZ(panes.diff(c) : _*))
        
        p.dim(x, y, w, h)

        s = p.s
    }
}

object SplitZ {
    val cached = mutable.Map[List[Pane], SplitZ]()

    def apply(panes : Pane*) = 
        if (panes.num == 1) 
            panes(0)
        else
        if (panes.num == 2) 
            SplitEither(SplitVer(panes(0), panes(1)), SplitHor(panes(0), panes(1)))
        else {
            val l = panes.toList
            cached.getOrElseUpdate(l, { new SplitZ(l) })
        }
}

class ImagePane(d : Int, val iw : Double, val ih : Double) extends Pane {
    var x = 0
    var y = 0
    var w = 0
    var h = 0
    
    def dim(x : Int, y : Int, w : Int, h : Int) {
        this.x = x
        this.y = y
        this.w = w
        this.h = h

        val sw = (w - 2 * d) / iw
        val sh = (h - 2 * d) / ih

        this.s = min(sw, sh)
    }

    override def toString = x + " " + y + " " + w + " " + h
}

class FontSizePane(d : Int, fw : Double, fh : Double, cw : Double, ch : Double) extends ImagePane(d, fw*cw, fh*ch)

class TextPane(d : Int, f : FontDimensionInfo, sz : Double, cw : Double, ch : Double) extends ImagePane(d, f.w*cw*sz/f.s, f.h*ch*sz/f.s) {
    def fontSize = min((w - 2 * d) * f.s / f.w / cw, (h - 2 * d) * f.s / f.h / ch)
}

case class FontDimensionInfo(s : Double, w : Double, h : Double)

