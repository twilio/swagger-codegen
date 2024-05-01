package dev.guardrail.core

import _root_.io.swagger.v3.oas.models.OpenAPI
import cats._
import cats.data._
import cats.syntax.all._
import dev.guardrail.{ Target, UserError }
import scala.jdk.CollectionConverters._
import cats.Functor

/** Tracker is a class to marshal access into potentially nullable fields in Java classes, while maintaining a human-readable history of which properties were
  * accessed.
  *
  * Tracker heavily utilizes syntax from IndexedFunctor and IndexedDistributive. These classes are similar to Functor and Traversable, except they expose the
  * index into the structure they are walking over. This is used to automatically build the history while walking structures.
  *
  * ```scala
  * val tracker: Tracker[OpenAPI]               = Tracker(openAPI)
  * val servers: Tracker[List[Server]]          = tracker.downField("servers", _.getServers)
  * val firstServer: Tracker[Option[Server]]    = servers.map(_.headOption)
  * val firstServerUrl: Tracker[Option[String]] = firstServer.flatDownField("url", _.getUrl)
  *
  * val trackedUrl: Tracker[Option[URL]] = firstServerUrl.map(_.map(new URL(_)))
  *
  * // Examples of extracting:
  * val firstServerUrl: Option[URL] = trackedUrl.unwrapTracker                             // Throw away history
  * val firstServerUrl: Target[URL] = trackedUrl.raiseErrorIfEmpty("No Server URL found!") // Append history to the end of the error message
  * ```
  */
class Tracker[+A] private[core] (private[core] val get: A, private[core] val history: Vector[String]) {
  override def toString(): String = s"Tracker($get, $history)"
}

trait LowPriorityTrackerEvidence {
  implicit def arbConvincer[A]: Tracker.Convincer[A, A]                                          = Tracker.Convincer(identity _)
  implicit def juListConvincer[A]: Tracker.Convincer[Option[java.util.List[A]], Option[List[A]]] = Tracker.Convincer(_.map(_.asScala.toList))
}

trait HighPriorityTrackerEvidence extends LowPriorityTrackerEvidence {
  implicit def jlBooleanConvincer: Tracker.Convincer[Option[java.lang.Boolean], Option[Boolean]] = Tracker.Convincer(_.map(x => x))
  implicit def optionalArrayConvincer[A]: Tracker.Convincer[Option[Array[A]], List[A]]           = Tracker.Convincer(_.fold(List.empty[A])(_.toList))
  implicit def optionaljuListConvincer[A]: Tracker.Convincer[Option[java.util.List[A]], List[A]] = Tracker.Convincer(_.fold(List.empty[A])(_.asScala.toList))
  implicit def optionalListConvincer[A]: Tracker.Convincer[Option[List[A]], List[A]]             = Tracker.Convincer(_.getOrElse(List.empty[A]))
  implicit def optionaljuCollectionConvincer[A]: Tracker.Convincer[Option[java.util.Collection[A]], List[A]] =
    Tracker.Convincer(_.fold(List.empty[A])(_.asScala.toList))
  implicit def optionalOptionConvincer[A]: Tracker.Convincer[Option[Option[A]], Option[A]] = Tracker.Convincer(_.flatten)
  implicit def optionaljuMapConvincer[K, V]: Tracker.Convincer[Option[java.util.Map[K, V]], Mappish[List, K, V]] =
    Tracker.Convincer(_.fold(Mappish(List.empty[(K, V)]))(x => Mappish(x.asScala.toList)))
  implicit def optionalMapConvincer[K, V]: Tracker.Convincer[Option[Map[K, V]], Mappish[List, K, V]] =
    Tracker.Convincer(_.fold(Mappish(List.empty[(K, V)]))(x => Mappish(x.toList)))
}

trait LowestPriorityTrackerInstances {
  implicit object distributiveTracker extends IndexedDistributive[Tracker] {
    def indexedDistribute[G[_], A](value: Tracker[G[A]])(implicit G: IndexedFunctor[G]): G[Tracker[A]] =
      G.map(value.unwrapTracker) { case (i, a) =>
        new Tracker(a, value.history ++ G.label(i))
      }
  }

  implicit def trackerFunctor: Functor[Tracker] = new cats.Functor[Tracker] {
    def map[A, B](fa: Tracker[A])(f: A => B): Tracker[B] = new Tracker(f(fa.unwrapTracker), fa.history)
  }
}

trait LowPriorityTrackerSyntax extends LowestPriorityTrackerInstances {
  implicit class ListSyntax[A](tracker: Tracker[List[A]]) {
    def toNel: Tracker[Option[NonEmptyList[A]]] = tracker.map(NonEmptyList.fromList _)
  }

  implicit class MappishSyntax[A, B](tracker: Tracker[Mappish[List, A, B]]) {
    def toNel: Tracker[Option[Mappish[NonEmptyList, A, B]]] = tracker.map(x => NonEmptyList.fromList(x.value).map(Mappish(_)))
  }
}

trait HighPriorityTrackerSyntax extends LowPriorityTrackerSyntax {
  implicit class StringyEitherSyntax[B](tracker: Tracker[Either[String, B]]) {
    def raiseErrorIfLeft: Target[Tracker[B]] = tracker.fold(err => Target.raiseUserError(s"${err.get} (${err.showHistory})"), Target.pure _)
  }

  implicit class EitherSyntax[A, B](tracker: Tracker[Either[A, B]]) {
    def fold[C](a: Tracker[A] => C, b: Tracker[B] => C): C =
      tracker.unwrapTracker.fold(x => a(Tracker.cloneHistory(tracker, x)), x => b(Tracker.cloneHistory(tracker, x)))
  }

  implicit class FlatSyntax[F[_]: FlatMap: Applicative, A](tracker: Tracker[F[A]]) {
    class FlatDownFieldPartiallyApplied[C](val dummy: Boolean = true) {
      def apply[B](label: String, f: A => B)(implicit ev: Tracker.Convincer[F[B], C], M: MonoidK[F]): Tracker[C] =
        new Tracker(ev(tracker.unwrapTracker.flatMap(x => Option(f(x)).fold(M.empty[B])(_.pure[F]))), tracker.history :+ s".${label}")
    }
    def flatDownField[C]: FlatDownFieldPartiallyApplied[C] = new FlatDownFieldPartiallyApplied()
  }

  implicit class OptionSyntax[A](tracker: Tracker[Option[A]]) {
    def fold[B](default: => B)(f: Tracker[A] => B): B =
      tracker.unwrapTracker.fold(default)(x => f(Tracker.cloneHistory(tracker, x)))
    def orHistory: Either[Vector[String], Tracker[A]]      = tracker.indexedCosequence.toRight(tracker.history)
    def raiseErrorIfEmpty(err: String): Target[Tracker[A]] = Target.fromOption(tracker.indexedCosequence, UserError(s"${err} (${tracker.showHistory})"))
  }

  implicit class RefineSyntax[A](tracker: Tracker[A]) {
    class RefinePartiallyApplied[C](val dummy: Boolean = true) {
      def apply[B1](r: PartialFunction[A, B1])(f: Tracker[B1] => C): Either[Tracker[A], C] =
        r.andThen(value => f(Tracker.cloneHistory(tracker, value)))
          .andThen(Right(_))
          .applyOrElse(tracker.get, (_: A) => Left(tracker))
    }
    def refine[C]: RefinePartiallyApplied[C] = new RefinePartiallyApplied[C]()
  }

  implicit class RefineEitherSyntax[A, C](value: Either[Tracker[A], C]) {
    def orRefine[B1](r: PartialFunction[A, B1])(f: Tracker[B1] => C): Either[Tracker[A], C] =
      value.fold(
        tracker =>
          r.andThen(value => f(Tracker.cloneHistory(tracker, value)))
            .andThen(Right(_))
            .applyOrElse(tracker.get, (_: A) => Left(tracker)),
        Right(_)
      )

    def orRefineFallback(f: Tracker[A] => C): C =
      value.fold(f, identity _)
  }

  implicit class Syntax[A](tracker: Tracker[A]) {
    class DownFieldPartiallyApplied[C](val dummy: Boolean = true) {
      def apply[B](label: String, f: A => B)(implicit ev: Tracker.Convincer[Option[B], C]): Tracker[C] =
        new Tracker(ev(Option(f(tracker.get))), tracker.history :+ s".${label}")
    }
    def downField[C]: DownFieldPartiallyApplied[C] = new DownFieldPartiallyApplied()

    def showHistory: String = tracker.history.mkString("")

    def unwrapTracker: A = tracker.get

    def forceConvince[C](implicit ev: Tracker.Convincer[Option[A], C]): Tracker[C] =
      tracker.map(x => ev(Option(x)))
  }
}

object Tracker extends HighPriorityTrackerEvidence with HighPriorityTrackerSyntax {
  object Convincer {
    def apply[A, B](f: A => B): Convincer[A, B] = new Convincer[A, B] {
      def apply(a: A): B = f(a)
    }
  }
  sealed trait Convincer[-A, +B] {
    def apply(a: A): B
  }

  def apply(spec: OpenAPI): Tracker[OpenAPI]                        = new Tracker(spec, Vector.empty)
  def cloneHistory[A, B](tracker: Tracker[A], value: B): Tracker[B] = new Tracker(value, tracker.history)
  def unapply[A](instance: Tracker[A]): Some[(String, A)]           = Some((instance.showHistory, instance.unwrapTracker))

  def unsafe[T](value: T, history: Vector[String]): Tracker[T] = new Tracker(value, history)
}
