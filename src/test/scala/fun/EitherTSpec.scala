package fun

import org.scalatest.FreeSpec
import org.scalatest.concurrent.ScalaFutures

import scalaz._
import std.scalaFuture._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

class EitherTSpec extends FreeSpec with ScalaFutures {

  case class ErrorMsg(msg: String)

  type Result = Either[ErrorMsg,String]

  def maybeFind(a:String): Future[\/[ErrorMsg,String]] = Future.successful(-\/(ErrorMsg("couldnt find "+a)))

  val result = for {
    a <- EitherT(maybeFind("a"))
    b <- EitherT(maybeFind(a+"a"))
  } yield b

  val out: Future[\/[ErrorMsg, String]] = result.run

  "EitherT" in {
    whenReady(out) { found =>
      assertResult(-\/(ErrorMsg("couldnt find a")))(found)
    }
  }
}
