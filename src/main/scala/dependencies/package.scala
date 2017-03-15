import cats.data.{EitherT, WriterT}
import cats.free.Free
import cats.{Monad, MonadError}
import github4s.GithubResponses._
import github4s.free.interpreters.{Capture, Interpreters}
import github4s.jvm.Implicits._

import scala.util.Try
import scalaj.http.HttpResponse
import cats.implicits._

package object dependencies {

  type Log                 = List[String]
  type Github4sResponse[A] = EitherT[GHIO, GHException, GHResult[A]]
  type GithubOpsLog[A]     = WriterT[Github4sResponse, Log, A]

  implicit val ghResponseMonad: Monad[Github4sResponse] = new Monad[Github4sResponse] {

    override def flatMap[A, B](fa: Github4sResponse[A])(
        f: (A) => Github4sResponse[B]): Github4sResponse[B] = {
      fa.flatMap(ghResult => f(ghResult.result))
    }

    override def tailRecM[A, B](a: A)(
        f: (A) => Github4sResponse[Either[A, B]]): Github4sResponse[B] = {
      f(a).flatMap { ghResult =>
        ghResult.result match {
          case Right(v) =>
            val ghio: GHIO[GHResponse[B]] =
              Free.pure(Right(GHResult(v, ghResult.statusCode, ghResult.headers)))
            EitherT(ghio)
          case Left(e) => tailRecM(e)(f)
        }
      }
    }

    override def pure[A](x: A): Github4sResponse[A] = EitherT.pure(GHResult(x, 200, Map.empty))
  }

  implicit val tryMonadError: MonadError[Try, Throwable] =
    cats.instances.try_.catsStdInstancesForTry

  implicit val tryCaptureInstance: Capture[Try] = new Capture[Try] {
    override def capture[A](a: ⇒ A): Try[A] = Try(a)
  }

  implicit val tryInterpreter: Interpreters[Try, HttpResponse[String]] =
    new Interpreters[Try, HttpResponse[String]]

  def liftLog[A](ghr: Github4sResponse[A]): GithubOpsLog[A] =
    WriterT.lift[Github4sResponse, Log, A](ghr)

  def liftResponse[A](fa: GHIO[GHResponse[A]]): Github4sResponse[A] =
    EitherT(fa)

  def logW(v: String): GithubOpsLog[Unit] =
    WriterT.put[Github4sResponse, Log, Unit](())(v :: Nil)

}
