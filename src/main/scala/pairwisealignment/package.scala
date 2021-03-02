/*
 * The MIT License
 *
 * Copyright (c) 2015 ECOLE POLYTECHNIQUE FEDERALE DE LAUSANNE, Switzerland,
 * Group Fellay
 * Copyright (c) 2020 Istvan Bartha for changes made after 2015
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the Software
 * is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package pairwisealignment

import scala.collection.mutable.{StringBuilder => SB}

private[pairwisealignment] case class MutableMatrix(
    rows: Int,
    cols: Int,
    data: Array[Int]
) {
  def apply(row: Int, col: Int): Int = data(row * cols + col)
  def update(row: Int, col: Int, t: Int): Unit = {
    data(row * cols + col) = t
  }

  def copy = {
    val d2 = Array.ofDim[Int](rows * cols)
    System.arraycopy(data, 0, d2, 0, data.length)
    MutableMatrix(rows, cols, d2)
  }

  def maxLoc = {
    var i = 0
    val n = data.size
    var maxi = 0
    var max = Int.MinValue
    while (i < n) {
      val c = data(i)
      if (c > max) {
        max = c
        maxi = i
      }
      i += 1
    }
    val r = maxi / cols
    val c = maxi % cols
    (r, c, max)
  }
  def maxInRow(row: Int) = {
    var i = row * cols
    val n = i + cols
    var maxi = 0
    var max = Int.MinValue
    while (i < n) {
      val c = data(i)
      if (c > max) {
        max = c
        maxi = i
      }
      i += 1
    }
    (maxi % cols, max)
  }
  def maxInCol(col: Int) = {
    var i = 0
    val n = rows
    var maxi = 0
    var max = Int.MinValue
    while (i < n) {
      val c = data(i * cols + col)
      if (c > max) {
        max = c
        maxi = i
      }
      i += 1
    }
    (maxi, max)
  }

}
private[pairwisealignment] object MutableMatrix {
  def apply(r: Int, c: Int): MutableMatrix =
    MutableMatrix(r, c, Array.fill(r * c)(0))

}

object OverlapPairwiseAlignment {

  private def overlapAlignmentBacktrack(
      v: String,
      w: String,
      scores: Map[(Char, Char), Int],
      indelpenalty: Int
  ): (MutableMatrix, Int, Int, Int) = {
    val n = v.size
    val m = w.size
    val s = MutableMatrix(n + 1, m + 1)
    val b = MutableMatrix(n, m)
    for (i <- 0 to n) {
      s(i, 0) = 0
    }
    for (j <- 0 to m) {
      s(0, j) = -1 * j * indelpenalty
    }

    val ar = Array.ofDim[Int](3)
    var i = 1
    while (i <= n) {
      var j = 1
      while (j <= m) {

        val move = {
          val cost0 = (s(i - 1, j) - indelpenalty)
          val cost1 = (s(i, j - 1) - indelpenalty)
          val cost2 =
            (s(i - 1, j - 1) + scores((v.charAt(i - 1), w.charAt(j - 1))))

          ar(0) = cost0
          ar(1) = cost1
          ar(2) = cost2

          var k = 0
          var maxi = 0
          var max = Int.MinValue
          while (k < 3) {
            val c = ar(k)
            if (c > max) {
              maxi = k
              max = c
            }
            k += 1
          }

          (maxi, max)

        }

        b(i - 1, j - 1) = move._1
        s(i, j) = move._2

        j += 1
      }
      i += 1
    }

    val (maxCol, max) = s.maxInRow(n)

    (b, max, n - 1, maxCol - 1)
  }

  private def overlapAlignmentEmit(
      v: String,
      w: String,
      backtrack: MutableMatrix,
      mi: Int,
      mj: Int
  ): (String, String) = {
    def loop(i: Int, j: Int, acc1: SB, acc2: SB): (SB, SB) = {
      if (i == -1 && j == -1) (acc1, acc2)
      else if (i == -1) (acc1.append('-'), acc2.append(w.charAt(j)))
      else if (j == -1) (acc1, acc2)
      else if (i == v.size - 1 && j == w.size - 1 && (mi != i || mj != j))
        loop(mi, mj, acc1, acc2)
      else {
        if (backtrack(i, j) == 0)
          loop(i - 1, j, acc1.append(v.charAt(i)), acc2.append('-'))
        else if (backtrack(i, j) == 1)
          loop(i, j - 1, acc1.append('-'), acc2.append(w.charAt(j)))
        else if (backtrack(i, j) == 2)
          loop(i - 1, j - 1, acc1.append(v.charAt(i)), acc2.append(w.charAt(j)))
        else loop(i - 1, j, acc1, acc2.append(w.charAt(j)))
      }
    }

    val (a, b) = loop(v.size - 1, w.size - 1, new SB, new SB)
    (a.reverse.toString, b.reverse.toString)
  }

  def overlapAlignment(
      v: String,
      w: String,
      scores: Map[(Char, Char), Int],
      indelpenalty: Int
  ): (Int, String, String) = {

    val (backtrack, maxScore, mi, mj) =
      overlapAlignmentBacktrack(v, w, scores, indelpenalty)
    val (s1, s2) = overlapAlignmentEmit(v, w, backtrack, mi, mj)
    (maxScore, s1, s2)
  }

}

object FittingPairwiseAlignment {

  private def fittingAffineAlignmentBacktrack(
      v: String,
      w: String,
      scores: Map[(Char, Char), Int],
      gapopen: Int,
      gapextension: Int
  ) = {

    val (vInt, wInt, scoresInt, _) = PenaltyHelper(v, w, scores)

    val n = v.size
    val m = w.size
    val lower = MutableMatrix(n + 1, m + 1)

    val blower = MutableMatrix(n, m)
    val bmiddle = blower.copy
    val bupper = blower.copy
    for (i <- 1 to n) {
      lower(i, 0) = 0
    }
    for (j <- 1 to m) {
      lower(0, j) = -1 * math.max(0, j - 1) * gapextension - gapopen
    }

    val middle = lower.copy
    val upper = lower.copy

    var i = 1
    while (i <= n) {
      var j = 1
      while (j <= m) {
        {

          {
            val cost0 = (lower(i - 1, j) - gapextension)
            val cost3 = (middle(i - 1, j) - gapopen)
            if (cost0 >= cost3) {
              blower(i - 1, j - 1) = 0
              lower(i, j) = cost0
            } else {
              blower(i - 1, j - 1) = 3
              lower(i, j) = cost3
            }
          }

          {
            val cost1 = (upper(i, j - 1) - gapextension)
            val cost4 = (middle(i, j - 1) - gapopen)
            if (cost1 >= cost4) {
              bupper(i - 1, j - 1) = 1
              upper(i, j) = cost1
            } else {
              bupper(i - 1, j - 1) = 4
              upper(i, j) = cost4
            }
          }

          {
            val cost2 =
              (middle(i - 1, j - 1) + scoresInt(vInt(i - 1), wInt(j - 1)))
            val cost5 = (lower(i, j))
            val cost6 = (upper(i, j))
            if (cost2 >= cost5 && cost2 >= cost6) {
              bmiddle(i - 1, j - 1) = 2
              middle(i, j) = cost2
            } else if (cost5 >= cost2 && cost5 >= cost6) {
              bmiddle(i - 1, j - 1) = 5
              middle(i, j) = cost5
            } else {
              bmiddle(i - 1, j - 1) = 6
              middle(i, j) = cost6
            }
          }

        }
        j += 1
      }
      i += 1
    }

    val (maxLoc, max) = middle.maxInCol(m)

    (blower, bmiddle, bupper, max, maxLoc - 1, m - 1)
  }

  private def fittingAffineAlignmentEmit(
      v: String,
      w: String,
      backtracklower: MutableMatrix,
      backtrackmiddle: MutableMatrix,
      backtrackupper: MutableMatrix,
      mi: Int,
      mj: Int
  ): (String, String) = {
    def loop(
        i: Int,
        j: Int,
        k: Int,
        acc1: SB,
        acc2: SB
    ): (SB, SB) = {
      val mat =
        if (k == 0) backtracklower
        else if (k == 2) backtrackmiddle
        else backtrackupper
      if (i == -1 && j == -1) (acc1, acc2)
      else if (i == -1)
        loop(i, j - 1, k, acc1.append('-'), acc2.append(w.charAt(j)))
      else if (j == -1) loop(i - 1, j, k, acc1, acc2)
      else if (i == v.size - 1 && j == w.size - 1 && (mi != i || mj != j))
        loop(mi, mj, k, acc1, acc2)
      else {
        if (mat(i, j) == 0)
          loop(i - 1, j, 0, acc1.append(v.charAt(i)), acc2.append('-'))
        else if (mat(i, j) == 3)
          loop(i - 1, j, 2, acc1.append(v.charAt(i)), acc2.append('-'))
        else if (mat(i, j) == 1)
          loop(i, j - 1, 1, acc1.append('-'), acc2.append(w.charAt(j)))
        else if (mat(i, j) == 4)
          loop(i, j - 1, 2, acc1.append('-'), acc2.append(w.charAt(j)))
        else if (mat(i, j) == 2)
          loop(
            i - 1,
            j - 1,
            2,
            acc1.append(v.charAt(i)),
            acc2.append(w.charAt(j))
          )
        else if (mat(i, j) == 5) loop(i, j, 0, acc1, acc2)
        else loop(i, j, 1, acc1, acc2)
      }
    }
    val (a, b) = loop(v.size - 1, w.size - 1, 2, new SB, new SB)
    a.reverse.toString -> b.reverse.toString
  }

  private def fittingAlignmentBacktrack(
      v: String,
      w: String,
      scores: Map[(Char, Char), Int],
      indelpenalty: Int
  ): (MutableMatrix, Int, Int, Int) = {
    val n = v.size
    val m = w.size

    val (vInt, wInt, scoresInt, _) = PenaltyHelper(v, w, scores)

    val s = MutableMatrix(n + 1, m + 1)
    val b = MutableMatrix(n, m)
    for (i <- 0 to n) {
      s(i, 0) = 0
    }
    for (j <- 0 to m) {
      s(0, j) = -1 * j * indelpenalty
    }
    var i = 1
    val ar = Array.ofDim[Int](3)
    while (i <= n) {
      var j = 1
      while (j <= m) {
        val move = {
          val cost0 = (s(i - 1, j) - indelpenalty)
          val cost1 = (s(i, j - 1) - indelpenalty)
          val cost2 = (s(i - 1, j - 1) + scoresInt(vInt(i - 1), wInt(j - 1)))
          ar(0) = cost0
          ar(1) = cost1
          ar(2) = cost2

          var k = 0
          var maxi = 0
          var max = Int.MinValue
          while (k < 3) {
            val c = ar(k)
            if (c > max) {
              maxi = k
              max = c
            }
            k += 1
          }

          (maxi, max)

        }
        b(i - 1, j - 1) = move._1
        s(i, j) = move._2
        j += 1
      }
      i += 1
    }

    val (maxLoc, max) = s.maxInCol(m)

    (b, max, maxLoc - 1, m - 1)
  }

  private def fittingAlignmentEmit(
      v: String,
      w: String,
      backtrack: MutableMatrix,
      mi: Int,
      mj: Int
  ): (String, String) = {
    def loop(i: Int, j: Int, acc1: SB, acc2: SB): (SB, SB) = {
      if (i == -1 && j == -1) (acc1, acc2)
      else if (i == -1)
        loop(i, j - 1, acc1.append('-'), acc2.append(w.charAt(j)))
      else if (j == -1) (acc1, acc2)
      else if (i == v.size - 1 && j == w.size - 1 && (mi != i || mj != j))
        loop(mi, mj, acc1, acc2)
      else {
        if (backtrack(i, j) == 0)
          loop(i - 1, j, acc1.append(v.charAt(i)), acc2.append('-'))
        else if (backtrack(i, j) == 1)
          loop(i, j - 1, acc1.append('-'), acc2.append(w.charAt(j)))
        else if (backtrack(i, j) == 2)
          loop(i - 1, j - 1, acc1.append(v.charAt(i)), acc2.append(w.charAt(j)))
        else loop(i - 1, j, acc1, acc2.append(w.charAt(j)))
      }
    }

    val (a, b) = loop(v.size - 1, w.size - 1, new SB, new SB)
    (a.reverse.toString(), b.reverse.toString())
  }

  def fittingAffineAlignment(
      ref: String,
      query: String,
      scores: Map[(Char, Char), Int],
      indelpenalty: Int,
      gapextension: Int
  ): (Int, String, String) = {

    val (blower, bmiddle, bupper, maxScore, mi, mj) =
      fittingAffineAlignmentBacktrack(
        ref,
        query,
        scores,
        indelpenalty,
        gapextension
      )
    val (s1, s2) =
      fittingAffineAlignmentEmit(ref, query, blower, bmiddle, bupper, mi, mj)
    (maxScore, s1, s2)
  }

  def fittingAlignment(
      ref: String,
      query: String,
      scores: Map[(Char, Char), Int],
      indelpenalty: Int
  ): (Int, String, String) = {

    val (backtrack, maxScore, mi, mj) =
      fittingAlignmentBacktrack(ref, query, scores, indelpenalty)

    val (s1, s2) = fittingAlignmentEmit(ref, query, backtrack, mi, mj)
    (maxScore, s1, s2)
  }

  def fittingAlignment(
      v: String,
      w: String,
      scores: Map[(Char, Char), Int],
      indelpenalty: Int,
      gapextension: Int
  ): (Int, String, String) =
    if (indelpenalty == gapextension)
      fittingAlignment(v, w, scores, indelpenalty)
    else fittingAffineAlignment(v, w, scores, indelpenalty, gapextension)

}

object LocalPairwiseAlignment {

  private def localAlignmentBacktrack(
      v: String,
      w: String,
      scores: Map[(Char, Char), Int],
      indelpenalty: Int
  ): (MutableMatrix, Int, Int, Int, MutableMatrix) = {
    val n = v.size
    val m = w.size
    val s = MutableMatrix(n + 1, m + 1)
    val b = MutableMatrix(n, m)
    for (i <- 0 to n) {
      s(i, 0) = 0
    }
    for (j <- 0 to m) {
      s(0, j) = 0
    }
    val ar = Array.ofDim[Int](4)
    var i = 1
    while (i <= n) {
      var j = 1
      while (j <= m) {

        val move = {
          val cost0 = math.max(0, (s(i - 1, j) - indelpenalty))
          val cost1 = math.max(0, (s(i, j - 1) - indelpenalty))
          val cost2 = (s(i - 1, j - 1) + math
            .max(0, scores((v.charAt(i - 1), w.charAt(j - 1)))))
          val cost3 = s(i - 1, j - 1)
          ar(0) = cost0
          ar(1) = cost1
          ar(2) = cost2
          ar(3) = cost3

          var k = 0
          var maxi = 0
          var max = Int.MinValue
          while (k < 4) {
            val c = ar(k)
            if (c > max) {
              maxi = k
              max = c
            }
            k += 1
          }

          (maxi, max)
        }
        b(i - 1, j - 1) = move._1
        s(i, j) = move._2

        j += 1
      }
      i += 1
    }

    val (mi, mj, max) = s.maxLoc

    (b, max, mi - 1, mj - 1, s)
  }

  private def localAlignmentEmit(
      v: String,
      w: String,
      backtrack: MutableMatrix,
      score: MutableMatrix,
      mi: Int,
      mj: Int
  ): (String, String) = {
    def loop(i: Int, j: Int, acc1: String, acc2: String): (String, String) = {
      if (i == -1 && j == -1) (acc1, acc2)
      else if (i == -1) ('-' +: acc1, w.charAt(j) +: acc2)
      else if (j == -1) (v.charAt(i) +: acc1, '-' +: acc2)
      else if (i == v.size - 1 && j == w.size - 1 && (mi != i || mj != j))
        loop(mi, mj, acc1, acc2)
      else if (score(i, j) == 0) (v.charAt(i) +: acc1, w.charAt(j) +: acc2)
      else {
        if (backtrack(i, j) == 0)
          loop(i - 1, j, v.charAt(i) +: acc1, '-' +: acc2)
        else if (backtrack(i, j) == 1)
          loop(i, j - 1, '-' +: acc1, w.charAt(j) +: acc2)
        else if (backtrack(i, j) == 2)
          loop(i - 1, j - 1, v.charAt(i) +: acc1, w.charAt(j) +: acc2)
        else (acc1, acc2)
      }
    }
    loop(v.size - 1, w.size - 1, "", "")
  }

  def localAlignment(
      v: String,
      w: String,
      score: Map[(Char, Char), Int],
      indelpenalty: Int
  ): (Int, String, String) = {
    val (backtrack, maxScore, mi, mj, scores) =
      localAlignmentBacktrack(v, w, score, indelpenalty)
    val (s1, s2) = localAlignmentEmit(v, w, backtrack, scores, mi, mj)
    (maxScore, s1, s2)
  }

}

private[pairwisealignment] object PenaltyHelper {
  def apply(
      s1: String,
      s2: String,
      scores: Map[(Char, Char), Int]
  ): (Array[Int], Array[Int], MutableMatrix, Map[Char, Int]) = {
    val map: Map[Char, Int] = scores.keys
      .flatMap(x => x._1 :: x._2 :: Nil)
      .toSeq
      .distinct
      .sorted
      .zipWithIndex
      .toMap
    val mm = MutableMatrix(map.keys.size, map.keys.size)
    scores.foreach { case ((i, j), k) => mm.update(map(i), map(j), k) }
    (s1.map(map).toArray, s2.map(map).toArray, mm, map)
  }

}

object GlobalPairwiseAlignment {

  private def globalAlignmentBacktrack(
      v: String,
      w: String,
      scores: Map[(Char, Char), Int],
      indelpenalty: Int
  ): (MutableMatrix, Int) = {
    val (vInt, wInt, scoresInt, _) = PenaltyHelper(v, w, scores)

    val n = v.size
    val m = w.size
    val s = MutableMatrix(n + 1, m + 1)
    val b = MutableMatrix(n, m)
    for (i <- 0 to n) {
      s(i, 0) = -1 * i * indelpenalty
    }
    for (j <- 0 to m) {
      s(0, j) = -1 * j * indelpenalty
    }
    var i = 1
    while (i <= n) {
      var j = 1
      while (j <= m) {
        {
          {
            s(i, j) = math.max(
              math.max(s(i - 1, j) - indelpenalty, s(i, j - 1) - indelpenalty),
              s(i - 1, j - 1) + scoresInt(vInt(i - 1), wInt(j - 1))
            )
            if (s(i, j) == s(i - 1, j) - indelpenalty) {
              b(i - 1, j - 1) = 0
            } else if (s(i, j) == s(i, j - 1) - indelpenalty) {
              b(i - 1, j - 1) = 1
            } else if (s(i, j) == s(i - 1, j - 1) + scoresInt(
                         vInt(i - 1),
                         wInt(j - 1)
                       )) {
              b(i - 1, j - 1) = 2
            }
          }

        }
        j += 1
      }
      i += 1
    }
    b -> s(n, m)
  }

  private def globalAffineAlignmentBacktrack(
      v: String,
      w: String,
      scores: Map[(Char, Char), Int],
      gapopen: Int,
      gapextension: Int
  ) = {

    val (vInt, wInt, scoresInt, _) = PenaltyHelper(v, w, scores)

    val n = v.size
    val m = w.size
    val lower = MutableMatrix(n + 1, m + 1)

    val blower = MutableMatrix(n, m)
    val bmiddle = blower.copy
    val bupper = blower.copy
    for (i <- 1 to n) {
      lower(i, 0) = -1 * math.max(0, i - 1) * gapextension - gapopen
    }
    for (j <- 1 to m) {
      lower(0, j) = -1 * math.max(0, j - 1) * gapextension - gapopen
    }

    val middle = lower.copy
    val upper = lower.copy

    var i = 1
    while (i <= n) {
      var j = 1
      while (j <= m) {
        {

          {
            val cost0 = (lower(i - 1, j) - gapextension)
            val cost3 = (middle(i - 1, j) - gapopen)
            if (cost0 >= cost3) {
              blower(i - 1, j - 1) = 0
              lower(i, j) = cost0
            } else {
              blower(i - 1, j - 1) = 3
              lower(i, j) = cost3
            }
          }

          {
            val cost1 = (upper(i, j - 1) - gapextension)
            val cost4 = (middle(i, j - 1) - gapopen)
            if (cost1 >= cost4) {
              bupper(i - 1, j - 1) = 1
              upper(i, j) = cost1
            } else {
              bupper(i - 1, j - 1) = 4
              upper(i, j) = cost4
            }
          }

          {
            val cost2 =
              (middle(i - 1, j - 1) + scoresInt(vInt(i - 1), wInt(j - 1)))
            val cost5 = (lower(i, j))
            val cost6 = (upper(i, j))
            if (cost2 >= cost5 && cost2 >= cost6) {
              bmiddle(i - 1, j - 1) = 2
              middle(i, j) = cost2
            } else if (cost5 >= cost2 && cost5 >= cost6) {
              bmiddle(i - 1, j - 1) = 5
              middle(i, j) = cost5
            } else {
              bmiddle(i - 1, j - 1) = 6
              middle(i, j) = cost6
            }
          }

        }
        j += 1
      }
      i += 1
    }

    (blower, bmiddle, bupper) -> middle(n, m)
  }

  private def globalAffineAlignmentEmit(
      v: String,
      w: String,
      backtracklower: MutableMatrix,
      backtrackmiddle: MutableMatrix,
      backtrackupper: MutableMatrix
  ): (String, String) = {
    def loop(
        i: Int,
        j: Int,
        k: Int,
        acc1: SB,
        acc2: SB
    ): (SB, SB) = {
      val mat =
        if (k == 0) backtracklower
        else if (k == 2) backtrackmiddle
        else backtrackupper
      if (i == -1 && j == -1) (acc1, acc2)
      else if (i == -1)
        loop(i, j - 1, k, acc1.append('-'), acc2.append(w.charAt(j)))
      else if (j == -1)
        loop(i - 1, j, k, acc1.append(v.charAt(i)), acc2.append('-'))
      else {
        if (mat(i, j) == 0)
          loop(i - 1, j, 0, acc1.append(v.charAt(i)), acc2.append('-'))
        else if (mat(i, j) == 3)
          loop(i - 1, j, 2, acc1.append(v.charAt(i)), acc2.append('-'))
        else if (mat(i, j) == 1)
          loop(i, j - 1, 1, acc1.append('-'), acc2.append(w.charAt(j)))
        else if (mat(i, j) == 4)
          loop(i, j - 1, 2, acc1.append('-'), acc2.append(w.charAt(j)))
        else if (mat(i, j) == 2)
          loop(
            i - 1,
            j - 1,
            2,
            acc1.append(v.charAt(i)),
            acc2.append(w.charAt(j))
          )
        else if (mat(i, j) == 5) loop(i, j, 0, acc1, acc2)
        else loop(i, j, 1, acc1, acc2)
      }
    }
    val (a, b) = loop(v.size - 1, w.size - 1, 2, new SB, new SB)
    (a.reverse.toString, b.reverse.toString)
  }

  private def globalAlignmentEmit(
      v: String,
      w: String,
      backtrack: MutableMatrix
  ): (String, String) = {
    def loop(
        i: Int,
        j: Int,
        acc1: SB,
        acc2: SB
    ): (SB, SB) = {
      if (i == -1 && j == -1) (acc1, acc2)
      else if (i == -1)
        loop(i, j - 1, acc1.append('-'), acc2.append(w.charAt(j)))
      else if (j == -1)
        loop(i - 1, j, acc1.append(v.charAt(i)), acc2.append('-'))
      else {
        if (backtrack(i, j) == 0)
          loop(i - 1, j, acc1.append(v.charAt(i)), acc2.append('-'))
        else if (backtrack(i, j) == 1)
          loop(i, j - 1, acc1.append('-'), acc2.append(w.charAt(j)))
        else
          loop(i - 1, j - 1, acc1.append(v.charAt(i)), acc2.append(w.charAt(j)))
      }
    }
    val (a, b) = loop(v.size - 1, w.size - 1, new SB, new SB)
    (a.reverse.toString, b.reverse.toString)
  }

  def globalAffineAlignment(
      v: String,
      w: String,
      score: Map[(Char, Char), Int],
      gapopen: Int,
      gapextension: Int
  ): (Int, String, String) = {
    val ((lower, middle, upper), maxScore) =
      globalAffineAlignmentBacktrack(v, w, score, gapopen, gapextension)
    val (s1, s2) = globalAffineAlignmentEmit(v, w, lower, middle, upper)
    (maxScore, s1, s2)
  }

  def globalAlignment(
      v: String,
      w: String,
      score: Map[(Char, Char), Int],
      indelpenalty: Int,
      gapextension: Int
  ): (Int, String, String) = {
    if (indelpenalty == gapextension) {
      val (backtrack, maxScore) =
        globalAlignmentBacktrack(v, w, score, indelpenalty)
      val (s1, s2) = globalAlignmentEmit(v, w, backtrack)
      (maxScore, s1, s2)
    } else globalAffineAlignment(v, w, score, indelpenalty, gapextension)
  }

  def editDistance(v: String, w: String): Int = {
    val score = {
      val l = (v.toSet ++ w.toSet).toList
      l.flatMap { c1 =>
        l.map { c2 =>
          (c1, c2) -> (if (c1 == c2) 0
                       else -1)
        }
      }
    }.toMap
    val (_, maxScore) = globalAlignmentBacktrack(v, w, score, 1)

    maxScore * -1
  }

}

object LongestCommonSubstring {

  private def longestCommonSubstringBacktrack(
      v: String,
      w: String
  ): MutableMatrix = {
    val n = v.size
    val m = w.size
    val s = MutableMatrix(n + 1, m + 1)
    val b = MutableMatrix(n, m)
    for (i <- 0 to n) {
      s(i, 0) = 0
    }
    for (j <- 0 to m) {
      s(0, j) = 0
    }
    for (i <- 1 to n; j <- 1 to m) {
      s(i, j) = math.max(
        math.max(s(i - 1, j), s(i, j - 1)),
        s(i - 1, j - 1) + (if (v.charAt(i - 1) == w.charAt(j - 1)) 1 else 0)
      )
      if (s(i, j) == s(i - 1, j)) {
        b(i - 1, j - 1) = 0
      } else if (s(i, j) == s(i, j - 1)) {
        b(i - 1, j - 1) = 1
      } else if (s(i, j) == s(i - 1, j - 1) + 1) {
        b(i - 1, j - 1) = 2
      }
    }
    b
  }

  private def longestCommonSubstringEmit(
      v: String,
      w: String,
      backtrack: MutableMatrix
  ): String = {
    def loop(i: Int, j: Int, acc: String): String = {
      if (i == -1 || j == -1) acc
      else {
        if (backtrack(i, j) == 0) loop(i - 1, j, acc)
        else if (backtrack(i, j) == 1) loop(i, j - 1, acc)
        else loop(i - 1, j - 1, v.charAt(i) +: acc)
      }
    }
    loop(v.size - 1, w.size - 1, "")
  }

  def longestCommonSubstring(v: String, w: String) = {
    val backtrack = longestCommonSubstringBacktrack(v, w)
    longestCommonSubstringEmit(v, w, backtrack)
  }

}
