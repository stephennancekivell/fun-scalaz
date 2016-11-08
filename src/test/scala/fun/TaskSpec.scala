package fun

import org.scalatest

import scala.concurrent.Future
import scalaz.concurrent.Task

class TaskSpec extends scalatest.FreeSpec {

  // sequence in batches

  def find(i:Int): Future[String] = {
    println("starting find "+i)
    Future.successful(i.toString)
  }

  def findTask(i: Int): Task[String] = {
    Task {
      i.toString
    }
  }

  val finds: Seq[Task[String]] = (0 to 10).map(findTask)

  val finds2: Task[Seq[String]] = Task.gatherUnordered(finds)



  val found2: Task[String] = findTask(1).flatMap(_ => findTask(2))

  finds2.unsafePerformSyncAttempt.toEither.right.get








  "Task" -> {

  }
}
