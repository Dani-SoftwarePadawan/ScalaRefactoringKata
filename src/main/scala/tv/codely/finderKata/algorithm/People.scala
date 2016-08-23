package tv.codely.finderKata.algorithm

import java.util
import java.util.ArrayList

import scala.collection.JavaConverters._

import tv.codely.finderKata.algorithm.AgeDiffOption.AgeDiffOption

class People(private val _people: List[Person]) {

  def pairByAgeDiffOption(option: AgeDiffOption): PairAgeDiff = {
    val pairs: List[PairAgeDiff] = _pairs match {
      case Left(emptyPair) => return emptyPair
      case Right(result) => result
    }

    option match {
      case AgeDiffOption.MinimumAgeDiff => pairs.minBy(p => p.AgeDiff)
      case AgeDiffOption.MaximumAgeDiff => pairs.maxBy(p => p.AgeDiff)
    }
  }

  private def _pairs: Either[PairAgeDiff, List[PairAgeDiff]] = {
    val pairs = for {
      p1 <- _people
      p2 <- _people
      if !p1.eq(p2)
    } yield {
      val pair: PairAgeDiff = new PairAgeDiff()

      if (p1.birthDate.getMillis < p2.birthDate.getMillis) {
        pair.Younger = p1
        pair.Older = p2
      } else {
        pair.Younger = p2
        pair.Older = p1
      }

      pair.AgeDiff = pair.Older.birthDate.getMillis - pair.Younger.birthDate.getMillis

      pair
    }

    if (pairs.size < 1) {
      return Left(new PairAgeDiff())
    }

    Right(pairs)
  }
}
