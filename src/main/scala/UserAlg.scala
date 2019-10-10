import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import fetch._
import UserDB._

trait UserAlg[F[_]] {
  def getUser(id: UserId): F[User]
}
