import cats.data.{EitherT, WriterT}
import github4s.GithubResponses._
import sbtorgpolicies.github.instances._

import cats.implicits._

package object dependencies {

  type Log             = List[String]
  type GithubOpsLog[A] = WriterT[Github4sResponse, Log, A]

  def liftLog[A](ghr: Github4sResponse[A]): GithubOpsLog[A] =
    WriterT.lift[Github4sResponse, Log, A](ghr)

  def liftResponse[A](fa: GHIO[GHResponse[A]]): Github4sResponse[A] =
    EitherT(fa)

  def logW(v: String): GithubOpsLog[Unit] =
    WriterT.put[Github4sResponse, Log, Unit](())(v :: Nil)

}
