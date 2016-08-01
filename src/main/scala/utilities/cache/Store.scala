package utilities.cache

import scalaz.  Free._
import scalaz.{Coyoneda, Functor, ~>}

trait CanBeStored[-T]

sealed trait Store[+Key, +A]

case class Get[+Key, +T : CanBeStored](k: Key) extends Store[Key, T]
case class Put[+Key, +T: CanBeStored](k: Key, v: T) extends Store[Key, T]
case class Del[+Key](k: Key) extends Store[Key, Unit]


trait LiftStores {
//  type FreeStore = ({type λ[+α] = Store[Key, α]})#λ
//  type StoreF[A] = Coyoneda[FreeStore, A]

  def get[Key, T : CanBeStored](k: Key) = liftFC[({type λ[+α] = Store[Key, α]})#λ, T](Get[Key, T](k))

  def put[Key, T : CanBeStored](k: Key, t: T) = liftFC[({type λ[+α] = Store[Key, α]})#λ, T](Put[Key, T](k, t))

  def del[Key](k: Key) = liftFC[({type λ[+α] = Store[Key, α]})#λ, Unit](Del[Key](k))
}

trait StoreRuntimes {
  trait InMemory[T]

  def  toMemory : (({type λ[+α] = Store[String, α]})#λ ~> InMemory) = new (({type λ[+α] = Store[String, α]})#λ ~> InMemory) {
    override def apply[A](fa: Store[String, A]): InMemory[A] = ???
  }
}



object Store extends LiftStores with StoreRuntimes {
  implicit val storeLong = new CanBeStored[Long] {}

  def prg[Key](k: Key) = for {
    _ <- put[Key, Long](k, 7L)
    a <- get[Key, Long](k)
  } yield ()

//  prg[String]("key")
}