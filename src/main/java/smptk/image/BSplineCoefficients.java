/*
 * This code just canibalized the code by Philippe Thevenaz, made publically available. 
 * See copyright information below
 */

/*=====================================================================
 | Version: July 6, 2011
 \=====================================================================*/

/*=====================================================================
 | EPFL/STI/IMT/LIB
 | Philippe Thevenaz
 | BM-Ecublens 4.137
 | CH-1015 Lausanne VD
 | Switzerland
 |
 | phone (CET): +41(21)693.51.61
 | fax: +41(21)693.68.10
 | RFC-822: philippe.thevenaz@epfl.ch
 | X-400: /C=ch/A=400net/P=switch/O=epfl/S=thevenaz/G=philippe/
 | URL: http://bigwww.epfl.ch/
 \=====================================================================*/

/*===================================================================
 | This work is based on the following paper:
 |
 | Michael Unser
 | Splines: A Perfect Fit for Signal and Image Processing
 | IEEE Signal Processing Magazine
 | vol. 16, no. 6, pp. 22-38, November 1999
 |
 | This paper is available on-line at
 | http://bigwww.epfl.ch/publications/unser9902.html
 |
 | Other relevant on-line publications are available at
 | http://bigwww.epfl.ch/publications/
 \==================================================================*/

/*====================================================================
 | See the companion file "Differentials_.html" for additional help
 |
 | You'll be free to use this software for research purposes, but you should not
 | redistribute it without our consent. In addition, we expect you to include a
 | citation or acknowlegment whenever you present or publish results that are
 | based on it.
 \===================================================================*/

package smptk.image;

public class BSplineCoefficients {

	
	private static double getInitialAntiCausalCoefficientMirrorOnBounds(double[] c,
			double z, double tolerance) {
		return ((z * c[c.length - 2] + c[c.length - 1]) * z / (z * z - 1.0));
	} /* end getInitialAntiCausalCoefficientMirrorOnBounds */

	/*------------------------------------------------------------------*/
	static double getInitialCausalCoefficientMirrorOnBounds(double[] c, double z,
			double tolerance) {
		double z1 = z, zn = Math.pow(z, c.length - 1);
		double sum = c[0] + zn * c[c.length - 1];
		int horizon = c.length;

		if (0.0 < tolerance) {
			horizon = 2 + (int) (Math.log(tolerance) / Math.log(Math.abs(z)));
			horizon = (horizon < c.length) ? (horizon) : (c.length);
		}
		zn = zn * zn;
		for (int n = 1; (n < (horizon - 1)); n++) {
			zn = zn / z;
			sum = sum + (z1 + zn) * c[n];
			z1 = z1 * z;
		}
		return (sum / (1.0 - Math.pow(z, 2 * c.length - 2)));
	} /* end getInitialCausalCoefficientMirrorOnBounds */


	
	static public void getSplineInterpolationCoefficients(int degree, double[] c) throws Exception {
		
		double z[] = null;
		if (degree == 0 || degree == 1) { 
			z = new double[0];			
		}
		else if (degree == 2) { 
			z = new double[1];
			z[0] = Math.sqrt(8.0) - 3.0;
		}
		else if(degree == 3) { 
			z = new double[1];
			z[0] = Math.sqrt(3.0) - 2.0;
		}
		else {
			throw new Exception("SplineOrder must be between 0 and 3");
		}
		
		double tolerance = Float.intBitsToFloat((int)0x33FFFFFF);
		

		double lambda = 1.0;

		if (c.length == 1) {
			return;
		}
		for (int k = 0; (k < z.length); k++) {
			lambda = lambda * (1.0 - z[k]) * (1.0 - 1.0 / z[k]);
		}
		for (int n = 0; (n < c.length); n++) {
			c[n] = c[n] * lambda;
		}
		for (int k = 0; (k < z.length); k++) {
			c[0] = getInitialCausalCoefficientMirrorOnBounds(c, z[k], tolerance);
			for (int n = 1; (n < c.length); n++) {
				c[n] = c[n] + z[k] * c[n - 1];
			}
			c[c.length - 1] = getInitialAntiCausalCoefficientMirrorOnBounds(c,
					z[k], tolerance);
			for (int n = c.length - 2; (0 <= n); n--) {
				c[n] = z[k] * (c[n + 1] - c[n]);
			}
		}
	} /* end getSplineInterpolation */
}
