/*
 * The MIT License
 *
 * Copyright (c) 2020 Istvan Bartha
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
package pairwisealignment.wfa

import java.{util => ju}

sealed trait Op
case class MatchOrMismatch(n: Int) extends Op
case class Insertion(n: Int) extends Op
case class Deletion(n: Int) extends Op

object WFA {

  def editDistance(s1: String, s2: String) =
    globalAffineAlignment(s1, s2, 1, 0, 1)._1

  /** Global affine pairwise alignment with the Wavefront Alignment algorithm
    * https://doi.org/10.1093/bioinformatics/btaa777
    *
    * @param qString
    *   input string, ascii
    * @param tString
    *   input string, ascii
    * @param x
    *   mismatch penalty, positive integer
    * @param o
    *   gap open penalty, positive integer
    * @param e
    *   gap extension penalty, positive integer
    * @return
    *   (score, cigar, aligned Q, aligned T)
    */
  def globalAffineAlignment(
      qString: String,
      tString: String,
      x: Int,
      o: Int,
      e: Int
  ) = {
    require(x > 0)
    require(e > 0)
    val q = qString.getBytes("US-ASCII")
    val t = tString.getBytes("US-ASCII")
    require(q.length == qString.length)
    require(t.length == tString.length)

    val n = q.length
    val m = t.length
    val mainDiagonal = m - n
    val mainDiagonalLength = m
    val wfM = WF(qLength = n, tLength = m, x = x, o = o, e = e)
    val wfI = WF(qLength = n, tLength = m, x = x, o = o, e = e)
    val wfD = WF(qLength = n, tLength = m, x = x, o = o, e = e)

    {
      val wfc = WFC.empty(0, 0, n, m)
      wfc.updateDiagonal(0, 0)
      wfM.addWavefront(0, wfc)
    }

    var s = 0
    var break = false
    while (!break) {
      val w = wfM.getWavefront(s)
      if (w != null) {
        wfExtend(w, q, n, t, m)
      }
      val offset = wfM.getOffset(s, mainDiagonal)
      if (offset != Int.MinValue && offset >= mainDiagonalLength) {
        break = true
      } else {
        s += 1
        wfNext(wfM, wfI, wfD, n, m, s, x, o, e)
      }
    }
    val emitter = wfBacktrack(
      wfM = wfM,
      wfI = wfI,
      wfD = wfD,
      startOffset = mainDiagonalLength,
      startScore = s,
      mainDiagonal = mainDiagonal,
      x = x,
      o = o,
      e = e
    )

    val qa = emitter.alignQuery(qString)
    val ta = emitter.alignText(tString)
    val cg = emitter.cigar
    (s, cg, qa, ta)
  }

  private class Emitter(
      ops: scala.collection.mutable.ArrayBuffer[(Int, Int)]
  ) {
    def `match`(n: Int) = { if (n > 0) { ops += (0 -> n) } }
    def insert(n: Int) = { if (n > 0) { ops += (1 -> n) } }
    def delete(n: Int) = { if (n > 0) { ops += (2 -> n) } }

    def alignQuery(q: String): String = {
      val sb = new StringBuilder
      var i = 0
      ops.reverseIterator.foreach {
        case (0, n) =>
          sb.append(q.substring(i, i + n))
          i += n
        case (2, n) =>
          sb.append(q.substring(i, i + n))
          i += n
        case (1, n) =>
          sb.append(0 until n map (_ => '-') mkString)
        case _ =>
      }
      sb.toString
    }
    def alignText(t: String): String = {
      val sb = new StringBuilder
      var i = 0
      ops.reverseIterator.foreach {
        case (0, n) =>
          sb.append(t.substring(i, i + n))
          i += n
        case (1, n) =>
          sb.append(t.substring(i, i + n))
          i += n
        case (2, n) =>
          sb.append(0 until n map (_ => '-') mkString)
        case _ =>
      }
      sb.toString
    }

    def cigar = ops.reverse.toList.map {
      case (0, n) =>
        MatchOrMismatch(n)
      case (1, n) =>
        Insertion(n)
      case (2, n) =>
        Deletion(n)
      case _ => ???
    }

  }

  private case class WFC(
      low: Int,
      high: Int,
      mid: Int,
      private val values: Array[Int]
  ) {
    def getDiagonal(k: Int): Int = {
      val o = mid + k
      if (o >= 0 && o < values.length) values(o) else Int.MinValue
    }
    def updateDiagonal(k: Int, value: Int): Unit = {
      val o = mid + k
      if (o >= 0 && o < values.length) {
        values(o) = value
      }
    }
  }
  private object WFC {
    def empty(low: Int, high: Int, qLength: Int, tLength: Int) = {
      val ar = Array.ofDim[Int](qLength + tLength + 1)
      ju.Arrays.fill(ar, Int.MinValue)
      WFC(low, high, qLength, ar)
    }
  }
  private case class WF(
      private val values: Array[WFC]
  ) {
    def getWavefront(score: Int): WFC =
      if (score >= 0 && score < values.length)
        values(score)
      else null
    def getOffset(score: Int, diagonal: Int): Int = {
      if (score >= 0 && score < values.length) {
        val wfc = values(score)
        if (wfc != null) wfc.getDiagonal(diagonal)
        else Int.MinValue
      } else Int.MinValue
    }
    def addWavefront(score: Int, wf: WFC): Unit =
      if (score >= 0 && score < values.length) { values.update(score, wf) }
  }
  private object WF {
    def apply(qLength: Int, tLength: Int, x: Int, o: Int, e: Int): WF = {
      val maxScoreMismatch = math.min(qLength, tLength) * x
      val maxScoreGap = o + math.abs(qLength - tLength) * e
      val maxScore = maxScoreMismatch + maxScoreGap
      val ar = Array.ofDim[WFC](maxScore)
      WF(ar)
    }
  }

  private def which(a: Int, b: Int, c: Int, q: Int) =
    if (a == q) 0 else if (b == q) 1 else if (c == q) 2 else ???
  private def max(a: Int, b: Int, c: Int, d: Int) = {
    math.max(math.max(math.max(a, b), c), d)
  }
  private def max(a: Int, b: Int, c: Int) = {
    math.max(math.max(a, b), c)
  }

  private def min(a: Int, b: Int, c: Int, d: Int) = {
    math.min(math.min(math.min(a, b), c), d)
  }
  private def wfNext(
      wfM: WF,
      wfI: WF,
      wfD: WF,
      qLength: Int,
      tLength: Int,
      s: Int,
      x: Int,
      o: Int,
      e: Int
  ): Unit = {
    val mMismatch = wfM.getWavefront(s - x)
    val mOpenInsert = wfM.getWavefront(s - o - e)
    val iExtend = wfI.getWavefront(s - e)
    val dExtend = wfD.getWavefront(s - e)
    val hi1 = max(
      if (mMismatch == null) Int.MinValue else mMismatch.high,
      if (mOpenInsert == null) Int.MinValue else mOpenInsert.high,
      if (iExtend == null) Int.MinValue else iExtend.high,
      if (dExtend == null) Int.MinValue else dExtend.high
    )
    val low1 = min(
      if (mMismatch == null) Int.MaxValue else mMismatch.low,
      if (mOpenInsert == null) Int.MaxValue else mOpenInsert.low,
      if (iExtend == null) Int.MaxValue else iExtend.low,
      if (dExtend == null) Int.MaxValue else dExtend.low
    )
    if (hi1 != Int.MinValue && low1 != Int.MaxValue) {
      val hi = hi1 + 1
      val low = low1 - 1
      var nM: WFC = null
      var nI: WFC = null
      var nD: WFC = null
      var k = low
      while (k <= hi) {
        val i = {
          val mx = math.max(
            if (mOpenInsert == null) Int.MinValue
            else mOpenInsert.getDiagonal(k - 1),
            if (iExtend == null) Int.MinValue else iExtend.getDiagonal(k - 1)
          )
          if (mx != Int.MinValue) mx + 1 else mx
        }
        val d = math.max(
          if (mOpenInsert == null) Int.MinValue
          else mOpenInsert.getDiagonal(k + 1),
          if (dExtend == null) Int.MinValue else dExtend.getDiagonal(k + 1)
        )
        val m = max(
          if (mMismatch == null) Int.MinValue
          else {
            val t = mMismatch.getDiagonal(k)
            if (t == Int.MinValue) t else t + 1
          },
          i,
          d
        )
        if (m != Int.MinValue) {
          if (nM == null) {
            nM = WFC.empty(
              low = low,
              high = hi,
              qLength = qLength,
              tLength = tLength
            )
          }
          nM.updateDiagonal(k, m)
        }
        if (i != Int.MinValue) {
          if (nI == null) {
            nI = WFC.empty(
              low = low,
              high = hi,
              qLength = qLength,
              tLength = tLength
            )
          }
          nI.updateDiagonal(k, i)
        }
        if (d != Int.MinValue) {
          if (nD == null) {
            nD = WFC.empty(
              low = low,
              high = hi,
              qLength = qLength,
              tLength = tLength
            )
          }
          nD.updateDiagonal(k, d)
        }
        k += 1
      }
      if (nM != null) { wfM.addWavefront(s, nM) }
      if (nI != null) { wfI.addWavefront(s, nI) }
      if (nD != null) { wfD.addWavefront(s, nD) }
    }
  }

  private def wfExtend(
      wf: WFC,
      q: Array[Byte],
      qLength: Int,
      t: Array[Byte],
      tLength: Int
  ): Unit = {
    var k = wf.low
    val n = wf.high
    while (k <= n) {
      val offset = wf.getDiagonal(k)
      if (offset != Int.MinValue) {
        var v = offset - k
        var h = offset
        while (v < qLength && h < tLength && q(v) == t(h)) {
          v += 1
          h += 1
        }

        wf.updateDiagonal(k, h)
      }
      k += 1
    }
  }
  private def wfBacktrack(
      wfM: WF,
      wfI: WF,
      wfD: WF,
      startOffset: Int,
      startScore: Int,
      mainDiagonal: Int,
      x: Int,
      o: Int,
      e: Int
  ) = {
    val emitter = new Emitter(scala.collection.mutable.ArrayBuffer())
    var state = 0

    var offset = startOffset
    var s = startScore
    var k = mainDiagonal

    while (offset > 0 || s > 0 || k != 0) {
      if (state == 2) {
        val open = wfM
          .getOffset(s - e - o, k + 1)
        val extend = wfD
          .getOffset(s - e, k + 1)

        val oldOffset = math.max(open, extend)
        assert(oldOffset != Int.MinValue)
        val w = which(open, extend, Int.MinValue, oldOffset)
        if (w == 0) {
          assert(oldOffset == offset)
          state = 0
          k = k + 1
          s = s - e - o
          offset = oldOffset
          emitter.delete(1)
        } else {
          assert(oldOffset == offset)
          state = 2
          k = k + 1
          s = s - e
          offset = oldOffset
          emitter.delete(1)
        }

      } else if (state == 1) {
        val open = wfM
          .getOffset(s - e - o, k - 1)
        val extend = wfI
          .getOffset(s - e, k - 1)

        val oldOffset = math.max(open, extend)
        assert(oldOffset != Int.MinValue)
        val w = which(open, extend, Int.MinValue, oldOffset)

        if (w == 1) {
          assert(oldOffset + 1 == offset)
          state = 1
          k = k - 1
          s = s - e
          offset -= 1
          emitter.insert(1)
        } else {
          assert(oldOffset + 1 == offset)
          state = 0
          k = k - 1
          s = s - e - o
          offset -= 1
          emitter.insert(1)
        }
      } else {
        val insertion = wfI
          .getOffset(s, k)

        val deletion = wfD
          .getOffset(s, k)

        val misMatch = {
          val t = wfM
            .getOffset(s - x, k)
          if (t == Int.MinValue) t else t + 1
        }

        val oldOffset = max(insertion, deletion, misMatch)

        if (oldOffset == Int.MinValue && k == 0 && s == 0) {
          emitter.`match`(offset)
          offset = 0
        } else {
          val w = which(insertion, deletion, misMatch, oldOffset)
          if (w == 2) {
            state = 0
            val off = oldOffset - 1
            val diff = offset - off
            assert(diff > 0)
            offset = off
            k = k
            s = s - x
            emitter.`match`(diff)
          } else if (w == 0) {
            val diff = offset - oldOffset
            state = 1
            offset = oldOffset
            emitter.`match`(diff)
          } else {
            val diff = offset - oldOffset
            state = 2
            offset = oldOffset
            emitter.`match`(diff)
          }
        }

      }
    }
    if (offset > 0) {
      emitter.`match`(offset)
    }
    emitter
  }

}
