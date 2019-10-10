import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import fetch._
import scala.concurrent.duration._
import cats.effect.concurrent.Semaphore

import UserDB._
object Main extends IOApp {
  val userDB = new UserDB()
  import userDB.child
  def log[F[_]: Sync](msg: String) = Sync[F].delay(println(msg))

  object Users extends Data[UserId, User] {
    def name = "Users"

    def source[F[_]: Concurrent: Timer]: DataSource[F, UserId, User] =
      new DataSource[F, UserId, User] {
        override def data = Users

        override def CF = Concurrent[F]

        override def fetch(id: UserId): F[Option[User]] =
          userDB.getSingleUser[F](id)

        override def batch(ids: NonEmptyList[UserId]): F[Map[UserId, User]] =
          userDB.batch[F](ids)
      }
  }

  object NormalUserAlg extends UserAlg[IO] {
    def getUser(id: UserId): IO[User] = userDB.getSingleUser[IO](id).map(_.get)
  }
  object FetchUserAlg extends UserAlg[Fetch[IO, *]] {
    def getUser(id: UserId): Fetch[IO, User] = Fetch(id, Users.source)
  }

  import TreeF._
  val doItSeq = buildTree(NormalUserAlg).apply(child)
  val doItPar = buildTreePar(NormalUserAlg).apply(child)
  val doItFetch = Fetch.run(buildTree(FetchUserAlg).apply(child))

  def doIt(i: IO[Tree[User]], name: String) = {
    val time = Clock[IO].realTime(scala.concurrent.duration.MILLISECONDS)
    time.flatMap { startTime =>
      log[IO](s"Starting: $name") *>
        i.flatMap(s => log[IO](s.toString())) <*
        time.flatMap { endTime =>
          log[IO](s"Ending (${endTime - startTime}ms): $name")
        }
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    doIt(doItSeq, "Seq\n") *>
      doIt(doItPar, "Par\n") *>
      doIt(doItFetch, "Fetch\n").as(ExitCode.Success)
}
