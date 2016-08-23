package tv.codely.finderKata.algorithm

import java.util.{ArrayList, Date}

import com.github.nscala_time.time.Imports._
import org.scalatest._
import org.scalatest.Matchers._

final class PeopleTest extends WordSpec with BeforeAndAfterEach {

  val sue: Person = new Person("Sue", DateTime.parse("1950-01-01"))
  val greg: Person = new Person("Greg", DateTime.parse("1952-05-01"))
  val sarah: Person = new Person("Sarah", DateTime.parse("1982-01-01"))
  val mike: Person = new Person("Mike", DateTime.parse("1979-01-01"))

  "Finder" should {
    "Return empty results when given empty list" in {
      val list = List.empty

      val finder = new People(list)

      val result = finder.pairByAgeDiffOption(AgeDiffOption.MinimumAgeDiff)

      result.Younger shouldBe null
      result.Older shouldBe null
    }

    "Return empty results when given one person" in {
      val finder = new People(List(sue))

      val result = finder.pairByAgeDiffOption(AgeDiffOption.MinimumAgeDiff)

      result.Younger shouldBe null
      result.Older shouldBe null
    }

    "Return closest two for two people" in {
      val finder = new People(List(sue, greg))

      val result = finder.pairByAgeDiffOption(AgeDiffOption.MinimumAgeDiff)

      result.Younger shouldBe sue
      result.Older shouldBe greg
    }

    "Return furthest two for two people" in {
      val finder = new People(List(mike, greg))

      val result = finder.pairByAgeDiffOption(AgeDiffOption.MaximumAgeDiff)

      result.Younger shouldBe greg
      result.Older shouldBe mike
    }

    "Return furthest two for four people" in {
      val finder = new People(List(sue, sarah, mike, greg))

      val result = finder.pairByAgeDiffOption(AgeDiffOption.MaximumAgeDiff)

      result.Younger shouldBe sue
      result.Older shouldBe sarah
    }

    "Return closest two for four people" in {
      val finder = new People(List(sue, sarah, mike, greg))

      val result = finder.pairByAgeDiffOption(AgeDiffOption.MinimumAgeDiff)


      result.Younger shouldBe sue
      result.Older shouldBe greg
    }
  }
}

