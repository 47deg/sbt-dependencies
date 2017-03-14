import cats.data.{EitherT, WriterT}
import cats.{Monad, MonadError}
import github4s.GithubResponses.{GHException, GHIO, GHResult}
import github4s.free.interpreters.{Capture, Interpreters}

package object dependencies {

  import github4s.jvm.Implicits._

  import scala.util.Try
  import scalaj.http.HttpResponse

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

}
