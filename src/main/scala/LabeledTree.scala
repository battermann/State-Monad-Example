import cats.data._
import cats.implicits._
import pprint._

sealed trait Tree[+A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](a: A) extends Tree[A]

object LabeledTreeExampleWithState {
  object LabeledTree {

    type S = Int
    type LabeledTree[A] = Tree[(S, A)]

    def fromTree[A](tree: Tree[A]): State[S, LabeledTree[A]] = {
      tree match {
        case Leaf(a) =>
          for {
            state <- State.get[S]
            _ <- State.modify[S](s => s + 1)
          } yield Leaf(state, a)
        case Branch(left, right) =>
          for {
            l <- fromTree(left)
            r <- fromTree(right)
          } yield Branch(l, r)
      }
    }
  }
}

object LabeledTreeExampleWithStateAndEither {
  object LabeledTree {

    type S = Int
    type Error = String
    type ErrorOr[A] = Either[Error, A]
    type EitherState[A] = StateT[ErrorOr, S, A]

    type LabeledTree[A] = Tree[(S, A)]

    def fromTree[A](tree: Tree[A]): EitherState[LabeledTree[A]] = {
      tree match {
        case Leaf(a) =>
          for {
            state <- StateT.get[ErrorOr, S]
            _ <- StateT.modify[ErrorOr, S](s => s + 1)
          } yield Leaf(state, a): LabeledTree[A]
        case Branch(left, right) =>
          for {
            l <- fromTree(left)
            r <- fromTree(right)
          } yield Branch(l, r)
      }
    }
  }
}

object LabeledTreeMain extends App {

  val tree: Tree[Char] = Branch(Leaf('a'), Branch(Branch(Leaf('b'), Leaf('c')), Leaf('d')))

  pprintln(tree)

  pprintln(LabeledTreeExampleWithState.LabeledTree.fromTree(tree).runA(0).value)
  pprintln(LabeledTreeExampleWithStateAndEither.LabeledTree.fromTree(tree).runA(0))
}
