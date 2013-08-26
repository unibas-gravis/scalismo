import org.statismo.stk.core.registration.RegistrationConfiguration

import org.statismo.stk.core.geometry._
import org.statismo.stk.core.numerics.{ GradientDescentOptimizer, GradientDescentConfiguration, Integrator, IntegratorConfiguration, UniformDistributionRandomSampler2D }
import org.statismo.stk.core.numerics.{ GradientDescentOptimizer, GradientDescentConfiguration, Integrator, IntegratorConfiguration, UniformDistributionRandomSampler2D }
import org.statismo.stk.core.registration.{ MeanSquaresMetric2D, RKHSNormRegularizer, RotationSpace2D, TranslationSpace2D }
import java.io.File
import breeze.linalg.DenseVector
import org.statismo.stk.core.io.ImageIO._
import org.statismo.stk.core.common.BoxedDomain2D


val space = RotationSpace2D(Point2D(0,0))
val domain = BoxedDomain2D(Point2D(0,0),Point2D(10,10))

val regConf1 = new RegistrationConfiguration[TwoD](optimizer = GradientDescentOptimizer(GradientDescentConfiguration(200, 0.00001, false, true, 0.602)),
  integrator = Integrator[TwoD](IntegratorConfiguration(UniformDistributionRandomSampler2D(domain), 1000)),
       
  metric = MeanSquaresMetric2D(),
  transformationSpace = space,
  regularizer = RKHSNormRegularizer,
  regularizationWeight = 0.0,
  initialParametersOrNone = None)
  
  
val regConf2 = new RegistrationConfiguration[TwoD](optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.00001, false, true, 0.602)),
  integrator = Integrator[TwoD](IntegratorConfiguration(UniformDistributionRandomSampler2D(domain), 1000)),       
  metric = MeanSquaresMetric2D(),
  transformationSpace = TranslationSpace2D(),
  regularizer = RKHSNormRegularizer,
  regularizationWeight = 0.0,
  initialParametersOrNone = None)
  
  List(regConf1,regConf2)