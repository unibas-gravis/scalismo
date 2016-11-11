/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.utils

import scala.collection.mutable

/** represents a metric to be used with the Vantage Point tree */
trait Metric[A] {
  /** calculates distance */
  def distance(x: A, y: A): Double

  /** convenience interface through apply */
  def apply(x: A, y: A): Double = distance(x, y)
}

object Metric {
  def apply[A](f: (A, A) => Double) = new Metric[A] {
    override def distance(x: A, y: A) = f(x, y)
  }
}

/**
 * Recursive, immutable Vantage Point tree
 * Partitions metric spaces for efficient neighbour searches
 * Key concept: split a point set at a node into an inner and an outer set which satisfy:
 *  - inner: all points are closer to the pivot/center than the radius
 *  - outer: all points are further away from the the pivot than radius
 *
 *  WARNING: the tree only works with a metric (positive, symmetric, triangle inequality)
 */
sealed trait VantagePointTree[A] extends Traversable[A] {
  /** metric of the space */
  def metric: Metric[A]

  /** find closest point in tree */
  def findClosestPoint(point: A): A = findKNearestNeighbours(point, k = 1).head

  /** return true of the tree contains the point */
  def contains(point: A): Boolean

  /** find k nearest neighbours */
  def findKNearestNeighbours(point: A, k: Int): IndexedSeq[A] = {
    val candidates = new CandidatesKNN[A](k)
    findNN(point, candidates)
    candidates.points
  }

  /** find all neighbours within an epsilon region around point */
  def findEpsilonNeighbours(point: A, maxDistance: Double): IndexedSeq[A] = {
    val candidates = new CandidatesEpsRegion[A](maxDistance)
    findNN(point, candidates)
    candidates.points
  }

  /** main implementation of neighbour searches */
  protected[utils] def findNN(point: A, candidates: CandidateSet[A]): Unit
}

/** mutable candidate set for neighbour searches (internal use) */
private trait CandidateSet[A] {
  protected case class DistPoint(p: A, dist: Double) extends Ordered[DistPoint] {
    override def compare(that: DistPoint): Int = if (dist < that.dist) -1 else if (dist > that.dist) 1 else 0
  }

  def points: IndexedSeq[A]

  def addCandidate(point: A, dist: Double): Unit

  def maxDistance: Double
}

/** mutable candidate set for k neighbour searches (internal use) */
private class CandidatesKNN[A](size: Int) extends CandidateSet[A] {
  private val q = mutable.SortedSet.empty[DistPoint]

  /** all points in this candidate set */
  override def points: IndexedSeq[A] = q.iterator.map(_.p).toIndexedSeq

  /** add a new candidate to consider */
  override def addCandidate(point: A, dist: Double): Unit = {
    if (dist <= maxDistance || q.size < size) {
      q.add(DistPoint(point, dist))
      while (q.size > size) {
        q.remove(q.last)
      }
    }
  }

  /** distance of candidate which is farthest */
  override def maxDistance = {
    if (q.size >= size)
      q.lastOption.map(_.dist).getOrElse(Double.PositiveInfinity)
    else
      Double.PositiveInfinity
  }
}

/** mutable candidate set for epsilon neighbour searches (internal use) */
private class CandidatesEpsRegion[A](override val maxDistance: Double) extends CandidateSet[A] {
  private val q = mutable.SortedSet.empty[DistPoint]

  /** all points in this candidate set */
  override def points: IndexedSeq[A] = q.iterator.map(_.p).toIndexedSeq

  /** add a new candidate to consider */
  override def addCandidate(point: A, dist: Double): Unit = {
    if (dist <= maxDistance) {
      q.add(DistPoint(point, dist))
    }
  }
}

object VantagePointTree {
  /** build a Vantage Point tree with given metric (must be a metric!), uses random pivot elements */
  def apply[A](data: Iterable[A], metric: Metric[A]): VantagePointTree[A] = recursiveTreeBuilder(data, metric, randomPivotSelector[A])

  /** build a Vantage Point tree with given metric (must be a metric!) and pivot selector */
  def apply[A](data: Iterable[A], metric: Metric[A], pivotSelector: Iterable[A] => A): VantagePointTree[A] = recursiveTreeBuilder(data, metric, pivotSelector)

  /** select a random element as pivot */
  def randomPivotSelector[A](points: Iterable[A])(implicit random: Random): A = {
    random.scalaRandom.shuffle(points).head
  }

  /** select first element as pivot */
  def firstPivotSelector[A](points: Iterable[A]): A = points.head

  /** select central element as pivot */
  def centralPivotSelector[A](metric: Metric[A], samples: Int)(points: Iterable[A])(implicit rand: Random): A = points.toSeq match {
    case Seq() => throw new RuntimeException("cannot select from empty seq!")
    case head +: Seq() => head
    case first +: second +: Seq() => first
    case _ =>
      // draw candidates, select the most central
      val trials = rand.scalaRandom.shuffle(points).take(math.min(samples, points.size))
      def spread(pivot: A) = {
        val dists = points.toSeq map { p => metric(pivot, p) }
        val medianDistance = median(dists)
        dists.map({ d => math.pow(d - medianDistance, 2) }).sum / dists.size
      }
      trials.minBy(spread)
  }

  private def median(s: Seq[Double]): Double = {
    val (lower, upper) = s.sortWith(_ < _).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
  }

  private def recursiveTreeBuilder[A](data: Iterable[A], metric: Metric[A], pivotSelector: Iterable[A] => A): VantagePointTree[A] = {
    // recursive tree builder
    def recursiveBuilder(points: Seq[A]): VantagePointTree[A] = points match {
      // small cases
      case Seq() => EmptyVP(metric)
      case point +: Seq() => VPLeaf(metric, point)
      case first +: second +: Seq() if first == second => VPLeaf[A](metric, first)
      case first +: second +: Seq() => VPLink(metric, first, VPLeaf[A](metric, second))
      case _ => // general case
        assert(points.size >= 3)
        val pivot = pivotSelector(points)
        // find distance to each point in the set
        val distances = points map { point => metric(point, pivot) }
        // pick median as node radius
        val radius = median(distances.toIndexedSeq)
        // all <= --> left, > --> right (remember to remove the Vantage point)
        val inside = points filter { point => metric(point, pivot) <= radius && metric(point, pivot) > 0.0 }
        val outside = points filter { point => metric(point, pivot) > radius }

        // construct the node and build the tree recursively
        if (inside.nonEmpty && outside.nonEmpty)
          VPNode(metric, pivot, radius,
            recursiveBuilder(inside),
            recursiveBuilder(outside)
          )
        else if (inside.nonEmpty)
          VPLink(metric, pivot,
            recursiveBuilder(inside)
          )
        else if (outside.nonEmpty)
          VPLink(metric, pivot,
            recursiveBuilder(outside)
          )
        else
          VPLeaf(metric, pivot)
    }
    // build with all points
    recursiveBuilder(data.toIndexedSeq)
  }
}

/** empty VP tree node */
private case class EmptyVP[A](metric: Metric[A]) extends VantagePointTree[A] {

  override def contains(point: A): Boolean = false

  override def foreach[U](f: (A) => U): Unit = {}

  override def findNN(point: A, candidates: CandidateSet[A]): Unit = {}
}

/** leaf node of VP tree */
private case class VPLeaf[A](metric: Metric[A], center: A) extends VantagePointTree[A] {

  override def contains(point: A): Boolean = point == center

  override def foreach[U](f: (A) => U): Unit = f(center)

  override def findNN(point: A, candidates: CandidateSet[A]): Unit = candidates.addCandidate(center, metric(center, point))
}

/** link node: list element, only a single child - should only appear before a leaf */
private case class VPLink[A](metric: Metric[A], center: A, next: VantagePointTree[A]) extends VantagePointTree[A] {

  override def findNN(point: A, candidates: CandidateSet[A]): Unit = {
    candidates.addCandidate(center, metric(center, point))
    next.findNN(point, candidates)
  }

  override def contains(point: A): Boolean = point == this.center || next.contains(point)

  override def foreach[U](f: (A) => U): Unit = {
    f(center)
    next.foreach(f)
  }
}

/** regular VP tree node with inner and outer children, inner contains all points which are closer to center than radius (inclusive) */
private case class VPNode[A](metric: Metric[A], center: A, radius: Double, inner: VantagePointTree[A], outer: VantagePointTree[A]) extends VantagePointTree[A] {
  // smart part of the VP tree, space partition according to distance to current pivot element
  override def findNN(point: A, candidates: CandidateSet[A]): Unit = {
    // distance to center point decides on first lookup
    val dist = metric(point, center)

    // this node as candidate
    candidates.addCandidate(center, metric(point, center))

    // children
    if (dist < radius) {
      if (dist - candidates.maxDistance < radius) {
        inner.findNN(point, candidates)
      }
      if (dist + candidates.maxDistance > radius) {
        outer.findNN(point, candidates)
      }
    } else {
      if (dist + candidates.maxDistance > radius) {
        outer.findNN(point, candidates)
      }
      if (dist - candidates.maxDistance < radius) {
        inner.findNN(point, candidates)
      }
    }
  }

  override def contains(point: A): Boolean = {
    if (point == center) true
    else {
      val d = metric(point, center)
      // can efficiently choose sub tree for further search, inside or outside radius
      if (d > radius) outer.contains(point) else inner.contains(point)
    }
  }

  /** to satisfy traversable */
  override def foreach[U](f: (A) => U): Unit = {
    f(center)
    inner.foreach(f)
    outer.foreach(f)
  }
}
