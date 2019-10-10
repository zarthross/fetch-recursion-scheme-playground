import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import fetch._
import scala.concurrent.duration._
import cats.effect.concurrent.Semaphore

object UserDB {
  type UserId = String
  case class Parents(father: UserId, mother: UserId)
  case class User(
      id: UserId,
      username: String,
      parents: Option[Parents] = Option.empty
  )
}
class UserDB(
    implicit ce: ConcurrentEffect[IO],
    cs: ContextShift[IO],
    t: Timer[IO]
) {
  import UserDB._
  val semapore = Semaphore[IO](2).unsafeRunSync()

  def latency[F[_]: Concurrent: Timer](msg: String): F[Unit] = {
    for {
      _ <- semapore.acquire.to[F]
      _ <- Sync[F].delay(println(s"--> [${Thread.currentThread.getId}] $msg"))
      _ <- Timer[F].sleep(1000.millis)
      _ <- Sync[F].delay(println(s"<-- [${Thread.currentThread.getId}] $msg"))
      _ <- semapore.release.to[F]
    } yield ()
  }

  val child = "c"
  val userDatabase: Map[UserId, User] = Map(
    "c" -> User("c", "child", Parents("f", "m").some),
    "m" -> User("f", "father", Parents("ff", "mf").some),
    "f" -> User("m", "mother", Parents("fm", "mm").some),
    "ff" -> User("ff", "mother's father", Parents("fff", "ffm").some),
    "mm" -> User("mm", "mother's mother", Parents("mmf", "mmm").some),
    "fm" -> User("fm", "father's mother", Parents("fmf", "fmm").some),
    "mf" -> User("mf", "mother's father", Parents("mff", "mfm").some),
    "fff" -> User("fff", "mother's father's father"),
    "mmf" -> User("mmf", "mother's mother's father"),
    "fmf" -> User("fmf", "father's mother's father"),
    "mff" -> User("mff", "mother's father's father"),
    "ffm" -> User("ffm", "mother's father's mother"),
    "mmm" -> User("mmm", "mother's mother's mother"),
    "fmm" -> User("fmm", "father's mother's mother"),
    "mfm" -> User("mfm", "mother's father's mother")
  )

  def getSingleUser[F[_]: Concurrent: Timer](id: UserId): F[Option[User]] =
    latency[F](s"One User $id") >> userDatabase.get(id).pure[F]

  def batch[F[_]: Concurrent: Timer](
      ids: NonEmptyList[UserId]
  ): F[Map[UserId, User]] =
    latency[F](s"Batch Users $ids") >>
      userDatabase.filterKeys(ids.toList.toSet).pure[F]
}
