package Part2.Chapter11

import Print.PrintSyntax._
import cats.Apply
import cats.implicits._
import cats.kernel.CommutativeMonoid

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intBoundedSemiLattice: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
    override def combine(a1: Int, a2: Int): Int = a1 max a2

    override def empty: Int = 0
  }

  implicit def setBoundedSemiLattice[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
    override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2

    override def empty: Set[A] = Set.empty[A]
  }
}

final case class GCounterMap[A](counters: Map[String, A])(implicit ma: BoundedSemiLattice[A]) {
  def increment(machine: String, amount: A): GCounterMap[A] = {
    val newAmount = ma.combine(counters.getOrElse(machine, ma.empty), amount)
    GCounterMap(counters.updated(machine, newAmount))
  }
  def merge(that: GCounterMap[A]): GCounterMap[A] = GCounterMap(
    counters.map2(that.counters)(ma.combine)
  )
  def total: A = ma.combineAll(counters.values)
}

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}
object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) =
    counter

  import KeyValueStore.KvsOps

  implicit def gcounterInstance[F[_, _], K, V](
      implicit kvs: KeyValueStore[F],
      smf: Apply[Lambda[x => F[K, x]]]
  ): GCounter[F, K, V] =
    new GCounter[F, K, V] {
      def increment(f: F[K, V])(key: K, value: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
        val total = m.combine(f.getOrElse(key, m.empty), value)
        f.put(key, total)
      }
      def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] =
        smf.map2(f1, f2)(b.combine)
      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.combineAll
    }
}

object GCounterInstances {
//  implicit def mapGCounter[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
//    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
//      val newAmount = f.getOrElse(k, m.empty) combine v
//      f.updated(k, newAmount)
//    }
//
//    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
//      f1.map2(f2)(b.combine)
//
//    override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V = m.combineAll(f.values)
//  }
}

trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)
  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)
    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(key)
    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)
    def values(implicit kvs: KeyValueStore[F]): List[V] =
      kvs.values(f)
  }

  implicit val mapKeyValueStore: KeyValueStore[Map] = new KeyValueStore[Map] {
    override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f.updated(k, v)

    override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }
}

object CRDT extends App {
  val g1      = Map("a" -> 7, "b" -> 3)
  val g2      = Map("a" -> 2, "b" -> 5)
  val counter = GCounter[Map, String, Int]
  val merged  = counter.merge(g1, g2)
  merged.print
  // merged: Map[String, Int] = Map("a" -> 7, "b" -> 5)
  val total = counter.total(merged).print
  // total: Int = 12
}
