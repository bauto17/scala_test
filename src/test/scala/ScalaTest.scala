import org.scalatest.FlatSpec

import scala.util.Try

class ScalaTest extends FlatSpec {

  "An empty Set" should "have size 0" in {
    assert(Set.empty.isEmpty)
  }

  it should "produce NoSuchElementException when head is invoked" in {
    assertThrows[NoSuchElementException] {
      Set.empty.head
    }
  }

  "opcion" should "produce specific element" in {
    val optional: Option[Int] = Some(1)
    assertResult(1){
      optional.get
    }
  }

  "opcion" should "produce specific element with getOrElse" in {
    val optional = toInt("1").getOrElse(0)
    assertResult(1){
      optional
    }
  }

  "opcion" should "produce default element with getOrElse in none value" in {
    val optional = toInt("1x").getOrElse(0)
    assertResult(0){
      optional
    }
  }

  "opcion" should "produce isEmpty response" in {
    val optional = toInt("1x")
    assert(optional.isEmpty)
  }

  "opcion" should "produce response with filter" in {
    val optional = toInt("50")
    val spectedValue = optional.filter(_%10==0)
    assert(spectedValue.isDefined)
  }

  "opcion" should "produce empty response with filter" in {
    val optional = toInt("51")
    val spectedValue = optional.filter(_%10==0)
    assert(spectedValue.isEmpty)
  }

  "opcion" should "produce response with exist condition" in {
    val optional = toInt("51")
    assert(!optional.exists(_%10 == 0))
  }

  "opcion" should "produce response dinamic response" in {
    val optional = toInt("51")
    optional.map(_*5).orElse(toInt("1"))
  }

  "opcion" should "produce response with match condition" in {
    val result = toInt("51") match {
      case Some(thing) => 43
      case None => 0
    }
    assertResult(43){
      result
    }
  }

  "opcion" should "produce response of validation stream" in {
    val result = Stream.from(20)
      .map(_.toString)
      .flatMap(toInt)
      .filter(_ % 2 == 0).head
    assertResult(20) {
      result
    }
  }

  "Try" should "produce default element with getOrElse in Try" in {
    val optional = tryToInt("1x").getOrElse(0)
    assertResult(0){
      optional
    }
  }

  "Try" should "produce default element with orElse" in {
    val optional = tryToInt("1x").orElse(tryToInt("24"))
    assertResult(24){
      optional.get.get
    }
  }

  "Try" should "be filter with Opcional value" in {
    val optional = tryToInt("1x").filter(_.exists(_*2==4))
    assert(optional.isFailure)
  }

  "Try" should "use fold for manage error" in {
    val optional = tryToInt("1x").fold("NumberFormatException "+_, "Valid int: " + _)
    assert(optional.contains("NumberFormatException"))
  }

  "Try" should "use fold in correct case" in {
    val optional = tryToInt("1").fold("NumberFormatException for " + _, "Valid int: " + _.get)
    assertResult("Valid int: 1"){
      optional
    }
  }

  "Try" should "Identify an error correctly" in {
    assert(!tryToInt("1").isFailure)
  }

  "opcion" should "Try recover in Exception case" in {
    tryToInt("1X") recover {
      case ex => println("error"); throw ex
    }
  }

  def toInt(s: String): Option[Int] = {
    try {
      Some(Integer.parseInt(s.trim))
    } catch {
      case e: Exception => None
    }
  }

  def getList(s: Int): Option[Iterable[Int]] = {
    try {
      Some(Stream.from(20))
    } catch {
      case e: Exception => None
    }
  }

  def tryToInt(s: String) : Try[Option[Int]] = {
    Try(Some(Integer.parseInt(s.trim)))
  }
}