import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import fetch._
import scala.concurrent.duration._
import cats.effect.concurrent.Semaphore
import higherkindness.droste._
import higherkindness.droste.data._
import UserDB._

sealed trait TreeF[A, B]
case class LeafF[A](value: A) extends TreeF[A, Nothing]
case class NodeF[A, B](value: A, left: B, right: B) extends TreeF[A, B]
object TreeF {
  type Tree[A] = Fix[TreeF[A, *]]

  implicit def treeFTraverse[V]: Traverse[TreeF[V, *]] =
    new higherkindness.droste.util.DefaultTraverse[TreeF[V, *]] {
      override def traverse[G[_]: Applicative, A, B](
          fa: TreeF[V, A]
      )(f: A => G[B]): G[TreeF[V, B]] =
        fa match {
          case leaf @ LeafF(_) => leaf.asInstanceOf[TreeF[V, B]].pure[G]
          case NodeF(value, left, right) =>
            (f(left), f(right)).mapN { (leftResult, rightResult) =>
              NodeF(value, leftResult, rightResult)
            }
        }
    }

  def buildTreeAlg[F[_]: Applicative](userAlg: UserAlg[F]) =
    CoalgebraM[F, TreeF[User, *], UserId] { userId =>
      userAlg.getUser(userId).map {
        case user @ User(_, _, Some(Parents(fatherId, motherId))) =>
          NodeF(user, fatherId, motherId)
        case user => LeafF(user).asInstanceOf[TreeF[User, UserId]]
      }
    }

  def buildTree[F[_]: Monad](userAlg: UserAlg[F]): UserId => F[Tree[User]] = {
    scheme.anaM[F, TreeF[User, *], UserId, Tree[User]](buildTreeAlg[F](userAlg))
  }

  def buildTreePar[F[_]: Monad: Parallel](
      userAlg: UserAlg[F]
  ): UserId => F[Tree[User]] = {
    parAna.anaMP[F, TreeF[User, *], UserId, Tree[User]](
      buildTreeAlg[F](userAlg)
    )
  }
}
