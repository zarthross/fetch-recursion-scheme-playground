import higherkindness.droste
import higherkindness.droste._
import higherkindness.droste.data._
import cats._
import cats.implicits._

object parAna {
  def anaMP[M[_]: Monad: Parallel, F[_]: Traverse, A, R](
      coalgebraM: CoalgebraM[M, F, A]
  )(implicit embed: Embed[F, R]): A => M[R] =
    hyloMP(embed.algebra.lift[M].run, coalgebraM.run)

  def hyloMP[M[_]: Monad: Parallel, F[_]: Traverse, A, B](
      algebra: F[B] => M[B],
      coalgebra: A => M[F[A]]
  ): A => M[B] =
    droste.kernel.hyloC[M, F, A, M[B]](
      _.flatMap(_.parSequence.flatMap(algebra)),
      coalgebra
    )
}
