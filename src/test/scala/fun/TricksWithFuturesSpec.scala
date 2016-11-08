package fun

import org.scalatest
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._

class TricksWithFuturesSpec extends scalatest.FreeSpec with ScalaFutures {

  def futureWithDelay(delayMs: Long):Future[Unit] = Future {
    println("starting delay "+delayMs)
    Thread.sleep(delayMs)
    println("ending delay "+delayMs)
  }

  // sequence

  "sequence" in {

    val data = Seq(
      futureWithDelay(1),
      futureWithDelay(2),
      futureWithDelay(3)
    )

    def sequence(xs: Seq[Future[Unit]], acc: Seq[Unit] = Nil): Future[Seq[Unit]] = {
      xs match {
        case Nil =>
          Future.successful(acc)
        case head :: tail =>
          head.flatMap { a =>
            sequence(tail, acc :+ a)
          }
      }
    }

    val found = sequence(data)

    found.map {assertResult(Seq((),(),()))(_)}
  }

  // map 2

  // sequence in batch, perform only some concurrently, 2 at a time

  "sequence in batch" in {
    val data = Seq(
      futureWithDelay(1),
      futureWithDelay(2),
      futureWithDelay(3)
    )

    def sequenceInBatches(xs: Seq[() => Future[Unit]], batchSize: Int): Future[Seq[Unit]] = {
      val batched = xs.grouped(batchSize)


      def go(rem: Seq[Seq[() => Future[Unit]]], acc: Seq[Unit] = Nil): Future[Seq[Unit]] = {
        rem match {
          case Nil => Future.successful(acc)
          case head :: tail =>
            val executedHead = head.map(f => f())
            Future.sequence(executedHead).flatMap { found =>
              go(tail, acc ++ found)
            }
        }
      }

      go(batched.toSeq)
    }
  }

  // sequence in batch without waiting using mutable, concurrent list

  // sequence in batch without waiting using pure FP

  // retry with timeout, but race timeouts.

  def delay(delayMs: Long): Future[Unit] = {
    val f = Future {
      util.Try(Await.ready(Promise().future, delayMs.millisecond))
    }

    f.map(_ => ())
  }


  "retry with timeout, but race timeouts." in {

    def getFuture() = futureWithDelay(1)

    val retryInterval = 10

    sealed trait RetryResult[T]
    case class RetryOk[T](a:T) extends RetryResult[T]
    case object RetryTimeout extends RetryResult[Nothing]

    def retryWithRacingTimeout[T](f: () => Future[T], concurrentAttempts: Seq[Future[RetryResult[T]]] = Nil): Future[T] = {
      val waiting = concurrentAttempts :+ f().map(RetryOk(_))
      val d = delay(retryInterval).map(_ => RetryTimeout)

      Future.firstCompletedOf(waiting :+ d).flatMap {
        case RetryOk(a) => Future.successful(a)
        case RetryTimeout => retryWithRacingTimeout(f, waiting)
      }
    }

    retryWithRacingTimeout(() => getFuture())
  }


}
