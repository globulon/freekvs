package utilities.cache

import redis.{ByteStringDeserializer, ByteStringSerializer, RedisCommands}

import scala.concurrent.Future.fromTry
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, reflectiveCalls}
import scala.util.Try
import scalaz._

trait CanBeStored[+T, +C <: CanBeStored[T, C]] { self: C => }

sealed trait Storage[+K, +V, +C <: CanBeStored[V, C], +S <: Storage[K, V, C, S]] { self: S => }
sealed trait KVS[+K, +V, +C <: CanBeStored[V, C]] extends Storage[K, V, C, KVS[K, V, C]]

case class Get[+K, +V, +C <: CanBeStored[V, C]](k: K, rule: C) extends KVS[K, V, C]
case class Put[+K, +V, +C <: CanBeStored[V, C]](k: K, v: V, rule: C) extends KVS[K, V, C]

trait LiftKVS {
  type K
  type StoreValue[+T] <: CanBeStored[T, StoreValue[T]]
  type Store[V] = KVS[K, V, StoreValue[V]]
  type Request[V] = Free[({type L[A] = Coyoneda[Store, A]})#L, V]

  def get[V: StoreValue](k: K): Request[V] = Free.liftFC[Store, V](Get[K, V, StoreValue[V]](k, implicitly[StoreValue[V]]))
  def put[V: StoreValue](k: K, v: V): Request[V] = Free.liftFC[Store, V](Put[K, V, StoreValue[V]](k, v, implicitly[StoreValue[V]]))
}

trait KVSImplementation {
  self: LiftKVS =>

  type Implementation[T]
  type Interpreter = Store ~> Implementation
  type ValueProjection[V]

  protected def project: StoreValue ~> ValueProjection
}

trait InMemoryKVS extends KVSImplementation {
  self: LiftKVS =>
  override type Implementation[T] = StateT[Future, Map[K, Any], T]
  override type ValueProjection[V] = Any => V

  override def project: StoreValue ~> ValueProjection = new (StoreValue ~> ValueProjection) {
    override def apply[A](fa: StoreValue[A]): (Any) => A = _.asInstanceOf[A]
  }

  def toMemory(implicit ctx: ExecutionContext): Interpreter = new (Store ~> Implementation) {
    override def apply[A](fa: Store[A]): Implementation[A] = fa match {
      case Get(k, rule)    => StateT[Future, Map[K, Any], A] { m => fromTry(Try((m, project(rule)(m(k))))) }
      case Put(k, v, rule) => StateT[Future, Map[K, Any], A] { m => fromTry(Try((m + (k -> v), v))) }
    }
  }
}

trait RedisStorage extends KVSImplementation {
  self: LiftKVS =>
  type K = String
  override type Implementation[T] = StateT[Future, RedisCommands, T]
  override type ValueProjection[V] = {
    def deserializer: ByteStringDeserializer[V]
    def serializer: ByteStringSerializer[V]
  }

  override def project: StoreValue ~> ValueProjection

  def toRedis(implicit ctx: ExecutionContext) : (Store ~> Implementation) = new (Store ~> Implementation) {
   override def apply[A](fa: Store[A]): Implementation[A] = fa match {
     case Get(k, rule)    => StateT[Future, RedisCommands, A] { c => c.get[A](k)(project(rule).deserializer).map(_.get).map((c, _)) }
     case Put(k, v, rule) => StateT[Future, RedisCommands, A] { c => c.set[A](k, v)(project(rule).serializer).map(_ => (c, v)) }
   }
  }
}

trait Storables {
  sealed trait StorePrimitive[+T] extends CanBeStored[T, StorePrimitive[T]]
  object StorePrimitive {
    def apply[A](): StorePrimitive[A] = new StorePrimitive[A] {}
  }

  implicit val storeLong = StorePrimitive[Long]()
}

//object Main extends LiftKVS with InMemoryKVS with Storables {
//  type K = String
//  override type StoreValue[T] = StorePrimitive[T]
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
