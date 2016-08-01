package utilities.cache

import scala.concurrent.Future.fromTry
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scalaz.Free._
import scalaz._
trait CanBeStored[-T]

sealed trait Store[+Key, +A]

case class Get[+Key, +T : CanBeStored](k: Key) extends Store[Key, Option[T]]
case class Put[+Key, +T: CanBeStored](k: Key, v: T) extends Store[Key, T]
case class Del[+Key, +T: CanBeStored](k: Key) extends Store[Key, T]

trait LiftStore {
  type Key
  type FreeStore[A] = Store[LiftStore.this.Key, A]

  def get[T : CanBeStored](k: Key) = liftFC[FreeStore, Option[T]](Get[Key, T](k))

  def put[T : CanBeStored](k: Key, t: T) = liftFC[FreeStore, T](Put[Key, T](k, t))

  def del[T : CanBeStored](k: Key) = liftFC[FreeStore, T](Del[Key, T](k))
}

trait InMemoryStore { self: LiftStore =>
  type InMemory[T]  = StateT[Future, Map[Key, Any], T]

  def toMemory(implicit ctx: ExecutionContext) : (FreeStore ~> InMemory) = new (FreeStore ~> InMemory) {
    override def apply[A](fa: Store[Key, A]): InMemory[A] = fa match {
      case Get(k)    => StateT[Future, Map[Key, Any], A]{ m => fromTry(Try((m, m.get(k).map(_.asInstanceOf[A]))))}
      case Put(k, v) => StateT[Future, Map[Key, Any], A]{ m => fromTry(Try((m + (k -> v), v))) }
      case Del(k)    => StateT[Future, Map[Key, Any], A]{ m => fromTry(Try((m - k, m(k).asInstanceOf[A]))) }
    }
  }
}

import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.Free

object KVS extends LiftStore with InMemoryStore {
  import scalaz.Scalaz._
  type Key = String
  implicit val storeLong = new CanBeStored[Long] {}

  def prg(k: String) = for {
    _ <- put[Long](k, 7L)
    a <- get[Long](k)
  } yield a

  def main(args: Array[String]) {
    Free.runFC(prg("key"))(toMemory).apply(Map.empty).foreach { println(_) }
  }
}
