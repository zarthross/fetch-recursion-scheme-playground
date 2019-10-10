import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import fetch._
import scala.concurrent.duration._
import cats.effect.concurrent.Semaphore

object Main extends IOApp {
  def log[F[_]: Sync](msg: String) = Sync[F].delay(println(msg))
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

  type UserId = String
  case class Parents(father: UserId, mother: UserId)
  case class User(
      id: UserId,
      username: String,
      parents: Option[Parents] = Option.empty
  )
  val child = "1"
  val userDatabase: Map[UserId, User] = Map(
    "1" -> User("1", "@one", Parents("2", "3").some),
    "2" -> User("2", "@two", Parents("6", "7").some),
    "3" -> User("3", "@three", Parents("4", "5").some),
    "4" -> User("4", "@four"),
    "5" -> User("5", "@five"),
    "6" -> User("6", "@six"),
    "7" -> User("7", "@seven")
  )

  object Users extends Data[UserId, User] {
    def name = "Users"

    def source[F[_]: Concurrent: Timer]: DataSource[F, UserId, User] =
      new DataSource[F, UserId, User] {
        override def data = Users

        override def CF = Concurrent[F]

        override def fetch(id: UserId): F[Option[User]] =
          latency[F](s"One User $id") >> CF.pure(userDatabase.get(id))

        override def batch(ids: NonEmptyList[UserId]): F[Map[UserId, User]] =
          latency[F](s"Batch Users $ids") >> CF.pure(
            userDatabase.filterKeys(ids.toList.toSet)
          )
      }
  }

  def getUserFetch[F[_]: Concurrent: Timer](id: UserId): Fetch[F, User] =
    Fetch(id, Users.source)

  import higherkindness.droste._
  import higherkindness.droste.data._

  sealed trait TreeF[A, B]
  case class LeafF[A](value: A) extends TreeF[A, Nothing]
  case class NodeF[A, B](value: A, left: B, right: B) extends TreeF[A, B]
  type Tree[A] = Fix[TreeF[A, *]]
  implicit def treeFTraverse[V]: Traverse[TreeF[V, *]] =
    new higherkindness.droste.util.DefaultTraverse[TreeF[V, *]] {
      override def traverse[G[_]: Applicative, A, B](
          fa: TreeF[V, A]
      )(f: A => G[B]): G[TreeF[V, B]] =
        fa match {
          case leaf@LeafF(_) => leaf.asInstanceOf[TreeF[V, B]].pure[G]
          case NodeF(value, left, right) =>
            (f(left), f(right)).mapN { (leftResult, rightResult) =>
              NodeF(value, leftResult, rightResult)
            }
        }
    }

  trait UserAlg[F[_]] {
    def getUser(id: UserId): F[User]
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

  def buildTreePar[F[_]: Monad: Parallel](userAlg: UserAlg[F]): UserId => F[Tree[User]] = {
    parAna.anaMP[F, TreeF[User, *], UserId, Tree[User]](buildTreeAlg[F](userAlg))
  }

  object NormalUserAlg extends UserAlg[IO] {
    def getUser(id: UserId): IO[User] = Fetch.run[IO](getUserFetch(id))
  }
  object FetchUserAlg extends UserAlg[Fetch[IO, *]] {
    def getUser(id: UserId): Fetch[IO, User] = getUserFetch[IO](id)
  }

  val doItSeq = buildTree(NormalUserAlg).apply(child)
  val doItPar = buildTreePar(NormalUserAlg).apply(child)
  val doItFetch = Fetch.run(buildTree(FetchUserAlg).apply(child))

  def doIt(i: IO[Tree[User]], name: String) = { 
    val time = Clock[IO].realTime(scala.concurrent.duration.MILLISECONDS)
    time.flatMap { startTime => 
      val thing = log[IO](s"Starting: $name") *> i.flatMap(s => log[IO](s.toString())) 
      thing <* { 
        time.flatMap {endTime => 
        log[IO](s"Ending (${endTime - startTime}ms): $name")
        }
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    doIt(doItSeq, "Seq\n") *> doIt(doItPar, "Par\n") *> doIt(doItFetch, "Fetch\n")
      .as(ExitCode.Success)
}
