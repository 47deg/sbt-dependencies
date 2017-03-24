/*
 * Copyright 2017 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
