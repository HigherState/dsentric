package dsentric

import cats.data.NonEmptyList

package object failure {
  type ValidResult[+T] = Either[NonEmptyList[Failure], T]

}
