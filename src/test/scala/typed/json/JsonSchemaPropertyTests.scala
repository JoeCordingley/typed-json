package typed.json

import org.scalacheck.{Properties, Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.language.adhocExtensions

object JsonSchemaPropertyTests extends Properties("JsonSchema") {

  sealed trait UntypedJson:
    type T
    val typed: JsonType[T]

  object UntypedJson:
    def gen: Gen[UntypedJson] = Gen.oneOf(
      Gen.const(JsonType.StringType),
      Gen.lzy(gen).map(_.typed).map(JsonType.ArrayType(_))
    )
    def length: UntypedJson => Int = _.typed match {
      case JsonType.StringType   => 1
      case JsonType.ArrayType(l) => length(l) + 1
    }
    given Arbitrary[UntypedJson] = Arbitrary(gen)

  object JsonType:
    case object StringType extends JsonType[String]
    case class ArrayType[A](t: JsonType[A]) extends JsonType[JsonArray[A]]

  sealed trait JsonType[A] extends UntypedJson {
    type T = A
    val typed: JsonType[T] = this
  }

  def genJson[A](jsonType: JsonType[A]): Gen[A] = jsonType match {
    case JsonType.StringType => arbitrary[String]
    case JsonType.ArrayType(a) =>
      Gen.listOf(Gen.lzy(genJson(a))).map(JsonArray(_))
  }

  enum Tree[A]:
    case Leaf(a: A) extends Tree[A]
    case Node(l: Tree[A], r: Tree[A]) extends Tree[A]

  property("startsWith") = forAll { (a: String, b: String) =>
    (a + b).startsWith(a)
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a + b + c).substring(a.length, a.length + b.length) == b
  }

  property("not massive") = forAll { (a: UntypedJson) =>
    UntypedJson.length(a) <= 10
  }

}
