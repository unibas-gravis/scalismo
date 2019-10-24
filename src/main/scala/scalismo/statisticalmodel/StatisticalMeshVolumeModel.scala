package scalismo.statisticalmodel

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.linalg.svd.SVD
import breeze.numerics.sqrt
import scalismo.common.{DiscreteField, Field, PointId, UnstructuredPointsDomain}
import scalismo.geometry.{EuclideanVector, Point, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.numerics.{FixedPointsUniformMeshSampler3D, FixedPointsUniformMeshVolumeSampler3D}
import scalismo.statisticalmodel.dataset.DataCollectionOfMeshVolume
import scalismo.statisticalmodel.{DiscreteLowRankGaussianProcess, GaussianProcess, LowRankGaussianProcess, StatisticalMeshModel}
import scalismo.tetramesh.{TetrahedralCell, TetrahedralList, TetrahedralMesh, TetrahedralMesh3D}
import scalismo.utils.Random

import scala.util.{Failure, Success, Try}

case class StatisticalMeshVolumeModel private (referenceMeshVolume: TetrahedralMesh[_3D], gp: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], EuclideanVector[_3D]]) {

  /** @see [[scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.rank]] */
  val rank = gp.rank

  /**
    * The mean shape
    * @see [[DiscreteLowRankGaussianProcess.mean]]
    */
  lazy val mean: TetrahedralMesh[_3D] = warpReference(gp.mean)

  /**
    * The covariance between two points of the  mesh with given point id.
    * @see [[DiscreteLowRankGaussianProcess.cov]]
    */
  def cov(ptId1: PointId, ptId2: PointId) = gp.cov(ptId1, ptId2)

  /**
    * draws a random shape.
    * @see [[DiscreteLowRankGaussianProcess.sample]]
    */
  def sample()(implicit rand: Random) = warpReference(gp.sample())

  /**
    * returns the probability density for an instance of the model
    * @param instanceCoefficients coefficients of the instance in the model. For shapes in correspondence, these can be obtained using the coefficients method
    *
    */

  def pdf(instanceCoefficients: DenseVector[Double]): Double = {
    val disVecField = gp.instance(instanceCoefficients)
    gp.pdf(disVecField)
  }

  /**
    * returns a shape that corresponds to a linear combination of the basis functions with the given coefficients c.
    *  @see [[DiscreteLowRankGaussianProcess.instance]]
    */
  def instance(c: DenseVector[Double]): TetrahedralMesh[_3D] = warpReference(gp.instance(c))


  /**
    *  Returns a marginal StatisticalMeshVolumeModel, modelling deformations only on the chosen points of the reference
    *
    *  This method proceeds by clipping the reference mesh to keep only the indicated point identifiers, and then marginalizing the
    *  GP over those points. Notice that when clipping, not all indicated point ids will be part of the clipped mesh volume, as some points may not belong
    *  to any cells anymore. Therefore 2 behaviours are supported by this method :
    *
    *  1- in case some of the indicated pointIds remain after clipping and do form a mesh, a marginal model is returned only for those points
    *  2- in case none of the indicated points remain (they are not meshed), a reference mesh voulme with all indicated point Ids and no cells is constructed and a marginal
    *  over this new reference is returned
    *
    * @see [[DiscreteLowRankGaussianProcess.marginal]]
    */




  def marginal(ptIds: IndexedSeq[PointId]) = {
    val clippedReference = referenceMeshVolume.operations.clip(p => { !ptIds.contains(referenceMeshVolume.pointSet.findClosestPoint(p).id) })
    // not all of the ptIds remain in the reference after clipping, since their cells might disappear
    val remainingPtIds = clippedReference.pointSet.points.map(p => referenceMeshVolume.pointSet.findClosestPoint(p).id).toIndexedSeq
    if (remainingPtIds.isEmpty) {
      val newRef = TetrahedralMesh3D(UnstructuredPointsDomain(ptIds.map(id => referenceMeshVolume.pointSet.point(id)).toIndexedSeq), TetrahedralList(IndexedSeq[TetrahedralCell]()))
      val marginalGP = gp.marginal(ptIds.toIndexedSeq)
      StatisticalMeshVolumeModel(newRef, marginalGP)
    } else {
      val marginalGP = gp.marginal(remainingPtIds)
      StatisticalMeshVolumeModel(clippedReference, marginalGP)
    }
  }






  private def warpReference(vectorPointData: DiscreteField[_3D, UnstructuredPointsDomain[_3D], EuclideanVector[_3D]]) = {
    val newPoints = vectorPointData.pointsWithValues.map { case (pt, v) => pt + v }
    TetrahedralMesh3D(UnstructuredPointsDomain(newPoints.toIndexedSeq), referenceMeshVolume.tetrahedralization)
  }
}

object StatisticalMeshVolumeModel {

  /**
    * creates a StatisticalMeshModel by discretizing the given Gaussian Process on the points of the reference mesh.
    */
  def apply(referenceMesh: TetrahedralMesh[_3D], gp: LowRankGaussianProcess[_3D, EuclideanVector[_3D]]): StatisticalMeshVolumeModel = {
    val discreteGp = DiscreteLowRankGaussianProcess(referenceMesh.pointSet, gp)
    new StatisticalMeshVolumeModel(referenceMesh, discreteGp)
  }

  /**
    * creates a StatisticalMeshModel from vector/matrix representation of the mean, variance and basis matrix.
    *
    * @see [[DiscreteLowRankGaussianProcess.apply(FiniteDiscreteDomain, DenseVector[Double], DenseVector[Double], DenseMatrix[Double]]
    */
  private[scalismo] def apply(referenceMesh: TetrahedralMesh[_3D],
                              meanVector: DenseVector[Double],
                              variance: DenseVector[Double],
                              basisMatrix: DenseMatrix[Double]) = {
    val gp = new DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], EuclideanVector[_3D]](referenceMesh.pointSet, meanVector, variance, basisMatrix)
    new StatisticalMeshVolumeModel(referenceMesh, gp)
  }

  /**
    * Creates a new DiscreteLowRankGaussianProcess, where the mean and covariance matrix are estimated from the given transformations.
    *
    */
  def createUsingPCA(referenceMesh: TetrahedralMesh[_3D], fields: Seq[Field[_3D, EuclideanVector[_3D]]]): StatisticalMeshVolumeModel = {
    val dgp: DiscreteLowRankGaussianProcess[_3D, UnstructuredPointsDomain[_3D], EuclideanVector[_3D]] = DiscreteLowRankGaussianProcess.createUsingPCA(referenceMesh.pointSet, fields)
    new StatisticalMeshVolumeModel(referenceMesh, dgp)
  }

  /**
    *  @deprecated
    *  Biasmodel can now be approximated before and the augment method is just an addition of two lowrank Gaussian processes.
    *  Please use approximate the biasModel before as a LowRankGaussianProcess and use the new method to create the bias model.
    */

  @deprecated("Please use the new method augmentModel(model,biasModel : LowRankGaussianProcess)", "20-04-2016")
  def augmentModel(model: StatisticalMeshVolumeModel, biasModel: GaussianProcess[_3D, EuclideanVector[_3D]], numBasisFunctions: Int)(implicit rand: Random): StatisticalMeshVolumeModel = {

    val modelGP = model.gp.interpolateNearestNeighbor
    // TODO: check if there is a better alternative (move method to Field?)
    val newMean = Field[_3D, EuclideanVector[_3D]](modelGP.domain,
      (p: Point[_3D]) => modelGP.mean(p) + biasModel.mean(p)
    )
    val newCov = modelGP.cov + biasModel.cov
    val newGP = GaussianProcess(newMean, newCov)
    val sampler = FixedPointsUniformMeshVolumeSampler3D(model.referenceMeshVolume, 2 * numBasisFunctions)
    val newLowRankGP = LowRankGaussianProcess.approximateGPNystrom(newGP, sampler, numBasisFunctions)
    StatisticalMeshVolumeModel(model.referenceMeshVolume, newLowRankGP)
  }

  /**
    *  Adds a bias model to the given statistical shape model
    */

  def augmentModel(model: StatisticalMeshVolumeModel, biasModel: LowRankGaussianProcess[_3D, EuclideanVector[_3D]]) = {

    val discretizedBiasModel = biasModel.discretize(model.referenceMeshVolume.pointSet)
    val eigenvalues = DenseVector.vertcat(model.gp.variance, discretizedBiasModel.variance).map(sqrt(_))
    val eigenvectors = DenseMatrix.horzcat(model.gp.basisMatrix, discretizedBiasModel.basisMatrix)

    for (i <- 0 until eigenvalues.length) {
      eigenvectors(::, i) :*= eigenvalues(i)
    }

    val l: DenseMatrix[Double] = eigenvectors.t * eigenvectors
    val SVD(v, _, _) = breeze.linalg.svd(l)
    val U: DenseMatrix[Double] = eigenvectors * v
    val d: DenseVector[Double] = DenseVector.zeros(U.cols)
    for (i <- 0 until U.cols) {
      d(i) = breeze.linalg.norm(U(::, i))
      U(::, i) := U(::, i) * (1.0 / d(i))
    }

    val r = model.gp.copy[_3D, UnstructuredPointsDomain[_3D], EuclideanVector[_3D]](meanVector = model.gp.meanVector + discretizedBiasModel.meanVector, variance = breeze.numerics.pow(d, 2), basisMatrix = U)
    StatisticalMeshVolumeModel(model.referenceMeshVolume, r)
  }

  /**
    * Returns a PCA model with given reference mesh and a set of items in correspondence.
    * All points of the reference mesh are considered for computing the PCA
    */
  def createUsingPCA(dc: DataCollectionOfMeshVolume): Try[StatisticalMeshVolumeModel] = {
    if (dc.size < 3) return Failure(new Throwable(s"A data collection with at least 3 transformations is required to build a PCA Model (only ${dc.size} were provided)"))

    val fields = dc.dataItems.map { i =>
      Field[_3D, EuclideanVector[_3D]](i.transformation.domain, p => i.transformation(p) - p)
    }
    Success(createUsingPCA(dc.reference, fields))
  }

}

