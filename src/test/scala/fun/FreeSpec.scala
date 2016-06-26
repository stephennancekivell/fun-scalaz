package fun

import org.scalatest
import scalaz._
import Scalaz._

class FreeSpec extends scalatest.FreeSpec {
  sealed trait Action[A]
  case class Create[A](a: A) extends Action[A]
  case class Delete[A](a: A) extends Action[A]
  case class List[A](a:A) extends Action[A]

  def create[A](a: A): Free[Action, A] = Free.liftF(Create(a))
  def delete[A](a: A): Free[Action, A] = Free.liftF(Delete(a))
  def list[A](a: A): Free[Action, A] = Free.liftF(List(a))

  object Interpreter extends (Action ~> Option) {
    var state:Seq[Any] = Nil

    def apply[A](in: Action[A]): Option[A] = {
      in match {
        case Create(a) =>
          state = state :+ a
          Some(a)
        case Delete(a) =>
          state = state.filter(_ != a)
          Some(a)
        case List(a) =>
          state.find(_ == a).map(_.asInstanceOf[A])
      }
    }
  }

  "should find" in {
    val prog = for {
        _ <- create("aa")
        found <- list("aa")
      } yield found

    val result = prog.foldMap(Interpreter)

    assertResult(Some("aa"))(result)
  }

  "should found none" in {
    val prog = for {
      _ <- create("aa")
      _ <- delete("aa")
      found <- list("aa")
    } yield found

    val result = prog.foldMap(Interpreter)

    assertResult(None)(result)
  }
}
