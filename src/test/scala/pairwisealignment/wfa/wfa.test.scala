package pairwisealignment.wfa

import pairwisealignment.GlobalPairwiseAlignment

import munit.FunSuite

class WFASpec extends FunSuite {

  def testEditDistance(q: String, t: String) = {
    val t1 = System.nanoTime
    val score1 =
      WFA.editDistance(q, t)
    val wfaElapsed = System.nanoTime - t1

    val t2 = System.nanoTime
    val score2 = GlobalPairwiseAlignment.editDistance(
      q,
      t
    )
    val dpElapsed = System.nanoTime - t2

    assertEquals(score1, score2)
    wfaElapsed.toDouble / dpElapsed
  }
  def testAgainstDP(q: String, t: String) = {
    val t1 = System.nanoTime
    val (score1, _, qa1, ta1) =
      WFA.globalAffineAlignment(q, t, x = 4, o = 6, e = 2)
    val wfaElapsed = System.nanoTime - t1
    val scores = {
      val c = (q.toSeq ++ t.toSeq).distinct
      for {
        i <- c; j <- c
      } yield (i, j) -> (if (i != j) -4 else 0)
    }
    val t2 = System.nanoTime
    val (score2, _, _) = GlobalPairwiseAlignment.globalAffineAlignment(
      q,
      t,
      score = scores.toMap,
      gapopen = 8,
      gapextension = 2
    )
    val dpElapsed = System.nanoTime - t2

    val score3 = {
      val (s, o) = (qa1 zip ta1)
        .foldLeft((0, 0)) { case ((accScore, openGap), (a, b)) =>
          if (a == b && b != '-')
            (accScore + openGap * 2 + (if (openGap > 0) 6 else 0), 0)
          else if (a != b && b != '-' && a != '-')
            (accScore + 4 + openGap * 2 + (if (openGap > 0) 6 else 0), 0)
          else (accScore, openGap + 1)
        }
      s + o * 2 + (if (o > 0) 6 else 0)
    }
    assertEquals(score1, score3)
    assertEquals(score1, -score2)
    assertEquals(qa1.filterNot(_ == '-'), q)
    assertEquals(ta1.filterNot(_ == '-'), t)
    wfaElapsed.toDouble / dpElapsed
  }

  val rng = new scala.util.Random(43)

  def testRandom() = {
    val alphabet = "ATGC".toSeq
    val length = 200

    val s1 = 0 until length map (_ => alphabet(rng.nextInt(4))) mkString
    val s2 =
      s1.map(c =>
        if (rng.nextDouble() < 0.1) alphabet(rng.nextInt(4)) else c
      ) mkString

    val s1d = s1.flatMap { c =>
      if (rng.nextDouble() < 0.05) Nil
      else if (rng.nextDouble() < 0.05) List(alphabet(rng.nextInt(4)))
      else List(c)
    } mkString
    val s2d = s2.flatMap { c =>
      if (rng.nextDouble() < 0.05) Nil
      else if (rng.nextDouble() < 0.05) List(alphabet(rng.nextInt(4)))
      else List(c)
    } mkString

    testAgainstDP(s1d, s2d)
    testEditDistance(s1d, s2d)
  }

  println(
    "Avg WFA/DP runtime ratio: " + (1 to 20000 map { _ =>
      testRandom()
    }).sum / 20000d
  )

  test("empty input") {
    intercept[java.lang.IllegalArgumentException](
      WFA
        .globalAffineAlignment(
          "",
          "",
          x = 4,
          o = 6,
          e = 2
        )
    )
  }

  test("global affine alignment") {
    assertEquals(
      {
        val (s, _, qa, ta) = WFA
          .globalAffineAlignment(
            "GTCCCGATGGTACAAGGCT",
            "GTCCCGTCTAGTCCATGGCT",
            x = 4,
            o = 6,
            e = 2
          )
        (s, qa, ta)
      },
      (
        24,
        "GTCCCGA-TGGTACAAGGCT",
        "GTCCCGTCTAGTCCATGGCT"
      )
    )
    assertEquals(
      {
        val (s, _, qa, ta) = WFA
          .globalAffineAlignment(
            "GAGATCTCGCTTACTCC",
            "GAGAGCTGTATCTTAGACCG",
            x = 4,
            o = 6,
            e = 2
          )
        (s, qa, ta)
      },
      (
        38,
        "GAGATCTCG--CTTACTCC-",
        "GAGAGCTGTATCTTAGACCG"
      )
    )
    assertEquals(
      WFA
        .globalAffineAlignment("GATACA", "ATAC", x = 4, o = 6, e = 2),
      (
        16,
        List(Deletion(1), MatchOrMismatch(4), Deletion(1)),
        "GATACA",
        "-ATAC-"
      )
    )
    assertEquals(
      WFA
        .globalAffineAlignment(
          "AGAT",
          "GAGATA",
          x = 4,
          o = 6,
          e = 2
        ),
      (
        16,
        List(Insertion(1), MatchOrMismatch(4), Insertion(1)),
        "-AGAT-",
        "GAGATA"
      )
    )

    assertEquals(
      WFA
        .globalAffineAlignment("GATACA", "GAGTA", x = 4, o = 6, e = 2),
      (
        16,
        List(
          MatchOrMismatch(2),
          MatchOrMismatch(1),
          MatchOrMismatch(1),
          Deletion(1),
          MatchOrMismatch(1)
        ),
        "GATACA",
        "GAGT-A"
      )
    )

  }
}
