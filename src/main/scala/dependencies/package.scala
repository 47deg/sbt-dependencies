import cats.data.{EitherT, WriterT}
import cats.{Monad, MonadError}
import github4s.GithubResponses.{GHException, GHIO, GHResponse, GHResult}
import github4s.free.interpreters.{Capture, Interpreters}
import github4s.jvm.Implicits._
import scala.util.Try
import scalaj.http.HttpResponse
import cats.implicits._

package object dependencies {

  type Log                 = List[String]
  type Github4sResponse[A] = EitherT[GHIO, GHException, GHResult[A]]
  type GithubOpsLog[A]     = WriterT[Github4sResponse, Log, A]

  implicit val ghResponseApplicative: Monad[Github4sResponse] = implicitly[Monad[Github4sResponse]]

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
