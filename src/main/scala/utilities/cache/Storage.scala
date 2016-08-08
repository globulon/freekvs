package utilities.cache

import redis.RedisCommands

import scala.concurrent.Future.fromTry
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.Try
import scalaz.Scalaz._
import scalaz._

trait CanBeStored[+T, +C <: CanBeStored[T, C]] { self: C => }

trait Storable[+T] extends CanBeStored[T, Storable[T]]

sealed trait Storage[+K, +V, +C <: CanBeStored[V, C], +S <: Storage[K, V, C, S]] { self: S => }

sealed trait KVS[+K, +V] extends Storage[K, V, Storable[V], KVS[K, V]]

case class Get[+K, +V](k: K) extends KVS[K, Option[V]]
case class Put[+K, +V](k: K, v: V) extends KVS[K, V]

trait LiftStore {
  type K
  type Store[V] = KVS[K, V]
  type Request[V] = Coyoneda[Store, V]

  def get[V](k: K): Free[Request, Option[V]] = Free.liftFC[Store, Option[V]](Get[K, V](k))

  def put[V](k: K, v: V): Free[Request, V] = Free.liftFC[Store, V](Put[K, V](k, v))
}

trait InMemoryStorage {
  self: LiftStore =>
  type InMemory[T] = StateT[Future, Map[K, Any], T]

  def toMemory(implicit ctx: ExecutionContext): (Store ~> InMemory) = new (Store ~> InMemory) {
    override def apply[A](fa: Store[A]): InMemory[A] = fa match {
      case Get(k) => StateT[Future, Map[K, Any], A] { m => fromTry(Try((m, m.get(k).map(_.asInstanceOf[A])))) }
      case Put(k, v) => StateT[Future, Map[K, Any], A] { m => fromTry(Try((m + (k -> v), v))) }
    }
  }
}

trait RedisStorage {
  self: LiftStore =>
  type K = String
  type Persistent[T] = StateT[Future, RedisCommands, T]

  def toRedis(implicit ctx: ExecutionContext) : (Store ~> Persistent) = new (Store ~> Persistent) {
   override def apply[A](fa: Store[A]): Persistent[A] = fa match {
     case Get(k)    => StateT[Future, RedisCommands, A] { c => (c, c.get[A](k)) }
     case Put(k, v) => StateT[Future, RedisCommands, A] { c => (c, c.set[A](k, v).map(_ => v)) }
   }
  }
}

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends LiftStore with InMemoryStorage {
 type K = String
 implicit val storeLong = new Storable[Long] {}

 def prg(k: String) = for {
   _ <- put[Long](k, 7L)
   a <- get[Long](k)
 } yield a

 def main(args: Array[String]) {
   Free.runFC(prg("key"))(toMemory).apply(Map.empty).foreach { println(_) }
 }
}
