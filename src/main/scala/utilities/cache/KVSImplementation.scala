package utilities.cache

import redis.RedisCommands

import scala.concurrent.Future.fromTry
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, reflectiveCalls}
import scala.util.Try
import scalaz.Scalaz._
import scalaz._

trait CanBeStored[+T, +C <: CanBeStored[T, C]] { self: C => }

sealed trait Storage[+K, +V, +C <: CanBeStored[V, C], +S <: Storage[K, V, C, S]] { self: S => }

sealed trait KVS[+K, +V, +C <: CanBeStored[V, C]] extends Storage[K, V, C, KVS[K, V, C]]

case class Get[+K, +V, +C <: CanBeStored[V, C]](k: K) extends KVS[K, V, C]
case class Put[+K, +V, +C <: CanBeStored[V, C]](k: K, v: V) extends KVS[K, V, C]

trait LiftKVS {
  type K
  type StoreValue[+T] <: CanBeStored[T, StoreValue[T]]
  type Store[V] = KVS[K, V, StoreValue[V]]
  type Request[V] = Free[({type L[A] = Coyoneda[Store, A]})#L, V]

  def get[V: StoreValue](k: K): Request[V] = Free.liftFC[Store, V](Get[K, V, StoreValue[V]](k))

  def put[V: StoreValue](k: K, v: V): Request[V] = Free.liftFC[Store, V](Put[K, V, StoreValue[V]](k, v))
}


trait KVSImplementation {
  self: LiftKVS =>

  type Implementation[T]
  type Interpreter = Store ~> Implementation
  type ValueProjection[+V]
  type ProjectValue[A] = StoreValue[A] => ValueProjection[A]

}

trait InMemoryKVS extends KVSImplementation {
  self: LiftKVS =>
  override type Implementation[T] = StateT[Future, Map[K, Any], T]

  def toMemory(implicit ctx: ExecutionContext): Interpreter = new (Store ~> Implementation) {
    override def apply[A](fa: Store[A]): Implementation[A] = fa match {
      case Get(k) => StateT[Future, Map[K, Any], A] { m => fromTry(Try((m, m(k).asInstanceOf[A]))) }
      case Put(k, v) => StateT[Future, Map[K, Any], A] { m => fromTry(Try((m + (k -> v), v))) }
    }
  }
}

//trait RedisStorage {
//  self: LiftStore =>
//  type K = String
//  type Persistent[T] = StateT[Future, RedisCommands, T]
//
//  def toRedis(implicit ctx: ExecutionContext) : (Store ~> Persistent) = new (Store ~> Persistent) {
//   override def apply[A](fa: Store[A]): Persistent[A] = fa match {
//     case Get(k)    => StateT[Future, RedisCommands, A] { c => (c, c.get[A](k)) }
//     case Put(k, v) => StateT[Future, RedisCommands, A] { c => (c, c.set[A](k, v).map(_ => v)) }
//   }
//  }
//}

import scala.concurrent.ExecutionContext.Implicits.global

trait Storables {

  sealed trait StorePrimitive[+T] extends CanBeStored[T, StorePrimitive[T]]
  object StorePrimitive {
    def apply[A](): StorePrimitive[A] = new StorePrimitive[A] {}
  }
  implicit val storeLong = StorePrimitive[Long]()

}

//object Main extends LiftKVS with InMemoryKVS with Storables {
//  type K = String
//  override type Storable[T] = StorePrimitive[T]
//
//  def prg(k: String) = for {
//    _ <- put[Long](k, 7L)
//    a <- get[Long](k)
//  } yield a
//
//  def main(args: Array[String]) {
//    Free.runFC(prg("key"))(toMemory).apply(Map.empty).foreach {
//      println(_)
//    }
//  }
//}
