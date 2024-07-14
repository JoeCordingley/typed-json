package typed.json

import org.scalacheck.{Properties, Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.language.adhocExtensions
import io.circe.Encoder

object JsonPropertyTests extends Properties("JsonSchema") {

  enum Tree[A]:
    case Leaf(a: A) extends Tree[A]
    case Node(l: Tree[A], r: Tree[A]) extends Tree[A]

  object Tree:
    def size[A]: Tree[A] => Int = {
      case Tree.Leaf(_)    => 1
      case Tree.Node(l, r) => size(l) + size(r) + 1
    }
    def genTree[A: Arbitrary]: Gen[Tree[A]] = Gen.sized { size =>
      if (size <= 0) genLeaf else Gen.oneOf(genLeaf, genNode)
    }
    def genNode[A: Arbitrary]: Gen[Tree[A]] = for {
      l <- resizeGen(_ / 2, genTree)
      r <- resizeGen(_ / 2, genTree)
    } yield Tree.Node(l, r)
    def genLeaf[A: Arbitrary]: Gen[Tree[A]] = arbitrary[A].map(Tree.Leaf(_))
    given [A: Arbitrary]: Arbitrary[Tree[A]] = Arbitrary(genTree)

  def resizeGen[A](f: Int => Int, gen: Gen[A]): Gen[A] =
    Gen.sized(i => Gen.resize(f(i), gen))

  type JsonBranchUnfixed[T] = JsonObject[(("left", T), ("right", T))]
  type JsonTree[A] = Fix[[T] =>> Either[JsonBranchUnfixed[T], A]]

  def convertTree[A, B]: Tree[A] => JsonTree[A] = {
    case Tree.Node(left, right) =>
      Fix(
        Left(
          JsonObject(("left", convertTree(left)), ("right", convertTree(right)))
        )
      )
    case Tree.Leaf(value) => Fix(Right(value))
  }

  property("trees not so big") = forAll { (a: Tree[Unit]) =>
    Tree.size(a) <= 100
  }

  opaque type Name = String

  object Name {
    def encoder: Encoder[Name] = Encoder[String]
    given Encoder[Name] = encoder
    def apply(s: String): Name = s
    extension (n: Name) def value: String = n
  }

}
