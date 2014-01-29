package factorygame.util

/**
 * PRNG that returns the same thing for any call with the same seed and position
 * Passes most dieharder tests
 */
class PositionalRandom(seed: Int = 0) {

	import scala.util.hashing.MurmurHash3._

	def get(x: Int, y: Int): Int = {
		var h = seed
		h = mix(h, x)
		h = mixLast(h, y)
		finalizeHash(h, 2)
	}

	def get(x: Int, y: Int, z: Int): Int = {
		var h = seed
		h = mix(h, x)
		h = mix(h, y)
		h = mixLast(h, z)
		finalizeHash(h, 3)
	}

	def get(x: Int, y: Int, z: Int, w: Int): Int = {
		var h = seed
		h = mix(h, x)
		h = mix(h, y)
		h = mix(h, z)
		h = mixLast(h, w)
		finalizeHash(h, 4)
	}

	/* Dieharder output:
	 * #==============================================================================#
	 * #             dieharder version 3.31.1 Copyright 2003 Robert G. Brown          #
	 * #==============================================================================#
	 * #        test_name   |ntup| tsamples |psamples|  p-value |Assessment           #
	 * #====================|====|==========|========|==========|=====================#
	 * #   diehard_birthdays|   0|       100|     100|0.40880874|  PASSED             #
	 * #      diehard_operm5|   0|   1000000|     100|0.72452476|  PASSED             #
	 * #  diehard_rank_32x32|   0|     40000|     100|0.78335969|  PASSED             #
	 * #    diehard_rank_6x8|   0|    100000|     100|0.75337552|  PASSED             #
	 * #   diehard_bitstream|   0|   2097152|     100|0.72333358|  PASSED             #
	 * #        diehard_opso|   0|   2097152|     100|0.25713198|  PASSED             #
	 * #        diehard_oqso|   0|   2097152|     100|0.49800982|  PASSED             #
	 * #         diehard_dna|   0|   2097152|     100|0.08237114|  PASSED             #
	 * #diehard_count_1s_str|   0|    256000|     100|0.34973925|  PASSED             #
	 * #diehard_count_1s_byt|   0|    256000|     100|0.72467821|  PASSED             #
	 * # diehard_parking_lot|   0|     12000|     100|0.59664629|  PASSED             #
	 * #    diehard_2dsphere|   2|      8000|     100|0.63668889|  PASSED             #
	 * #    diehard_3dsphere|   3|      4000|     100|0.34782166|  PASSED             #
	 * #     diehard_squeeze|   0|    100000|     100|0.00593673|  PASSED             #
	 * #        diehard_sums|   0|       100|     100|0.79630100|  PASSED             #
	 * #        diehard_runs|   0|    100000|     100|0.42867351|  PASSED             #
	 * #        diehard_runs|   0|    100000|     100|0.46821230|  PASSED             #
	 * #       diehard_craps|   0|    200000|     100|0.43589180|  PASSED             #
	 * #       diehard_craps|   0|    200000|     100|0.68189355|  PASSED             #
	 * # marsaglia_tsang_gcd|   0|  10000000|     100|0.00141310|   WEAK              #
	 * # marsaglia_tsang_gcd|   0|  10000000|     100|0.00000012|  FAILED             #
	 * #         sts_monobit|   1|    100000|     100|0.86143815|  PASSED             #
	 * #            sts_runs|   2|    100000|     100|0.60921817|  PASSED             #
	 * #          sts_serial|   1|    100000|     100|0.42383004|  PASSED             #
	 * #          sts_serial|   2|    100000|     100|0.15616099|  PASSED             #
	 * #          sts_serial|   3|    100000|     100|0.48029954|  PASSED             #
	 * #          sts_serial|   3|    100000|     100|0.98087638|  PASSED             #
	 * #          sts_serial|   4|    100000|     100|0.31608443|  PASSED             #
	 * #          sts_serial|   4|    100000|     100|0.27249792|  PASSED             #
	 * #          sts_serial|   5|    100000|     100|0.46622510|  PASSED             #
	 * #          sts_serial|   5|    100000|     100|0.34657385|  PASSED             #
	 * #          sts_serial|   6|    100000|     100|0.17003731|  PASSED             #
	 * #          sts_serial|   6|    100000|     100|0.34707886|  PASSED             #
	 * #          sts_serial|   7|    100000|     100|0.44757248|  PASSED             #
	 * #          sts_serial|   7|    100000|     100|0.63221159|  PASSED             #
	 * #          sts_serial|   8|    100000|     100|0.63197063|  PASSED             #
	 * #          sts_serial|   8|    100000|     100|0.84584490|  PASSED             #
	 * #          sts_serial|   9|    100000|     100|0.55886134|  PASSED             #
	 * #          sts_serial|   9|    100000|     100|0.99815529|   WEAK              #
	 * #          sts_serial|  10|    100000|     100|0.26324996|  PASSED             #
	 * #          sts_serial|  10|    100000|     100|0.31887488|  PASSED             #
	 * #          sts_serial|  11|    100000|     100|0.46708031|  PASSED             #
	 * #          sts_serial|  11|    100000|     100|0.81074853|  PASSED             #
	 * #          sts_serial|  12|    100000|     100|0.36470932|  PASSED             #
	 * #          sts_serial|  12|    100000|     100|0.65880322|  PASSED             #
	 * #          sts_serial|  13|    100000|     100|0.34500497|  PASSED             #
	 * #          sts_serial|  13|    100000|     100|0.24357216|  PASSED             #
	 * #          sts_serial|  14|    100000|     100|0.50091315|  PASSED             #
	 * #          sts_serial|  14|    100000|     100|0.58706647|  PASSED             #
	 * #          sts_serial|  15|    100000|     100|0.89167530|  PASSED             #
	 * #          sts_serial|  15|    100000|     100|0.07006410|  PASSED             #
	 * #          sts_serial|  16|    100000|     100|0.85137192|  PASSED             #
	 * #          sts_serial|  16|    100000|     100|0.40764625|  PASSED             #
	 * #         rgb_bitdist|   1|    100000|     100|0.67687671|  PASSED             #
	 * #         rgb_bitdist|   2|    100000|     100|0.29779794|  PASSED             #
	 * #         rgb_bitdist|   3|    100000|     100|0.92556392|  PASSED             #
	 * #         rgb_bitdist|   4|    100000|     100|0.97578153|  PASSED             #
	 * #         rgb_bitdist|   5|    100000|     100|0.85255867|  PASSED             #
	 * #         rgb_bitdist|   6|    100000|     100|0.13090987|  PASSED             #
	 * #         rgb_bitdist|   7|    100000|     100|0.81852517|  PASSED             #
	 * #         rgb_bitdist|   8|    100000|     100|0.76788669|  PASSED             #
	 * #         rgb_bitdist|   9|    100000|     100|0.60482221|  PASSED             #
	 * #         rgb_bitdist|  10|    100000|     100|0.98125647|  PASSED             #
	 * #         rgb_bitdist|  11|    100000|     100|0.71426592|  PASSED             #
	 * #         rgb_bitdist|  12|    100000|     100|0.75571344|  PASSED             #
	 * #rgb_minimum_distance|   2|     10000|    1000|0.46311147|  PASSED             #
	 * #rgb_minimum_distance|   3|     10000|    1000|0.90862213|  PASSED             #
	 * #rgb_minimum_distance|   4|     10000|    1000|0.65559541|  PASSED             #
	 * #rgb_minimum_distance|   5|     10000|    1000|0.16565253|  PASSED             #
	 * #    rgb_permutations|   2|    100000|     100|0.48099984|  PASSED             #
	 * #    rgb_permutations|   3|    100000|     100|0.66428982|  PASSED             #
	 * #    rgb_permutations|   4|    100000|     100|0.97728368|  PASSED             #
	 * #    rgb_permutations|   5|    100000|     100|0.52846309|  PASSED             #
	 * #      rgb_lagged_sum|   0|   1000000|     100|0.09195222|  PASSED             #
	 * #      rgb_lagged_sum|   1|   1000000|     100|0.01693758|  PASSED             #
	 * #      rgb_lagged_sum|   2|   1000000|     100|0.07422078|  PASSED             #
	 * #      rgb_lagged_sum|   3|   1000000|     100|0.00000042|  FAILED             #
	 * #      rgb_lagged_sum|   4|   1000000|     100|0.28501636|  PASSED             #
	 * #      rgb_lagged_sum|   5|   1000000|     100|0.00541635|  PASSED             #
	 * #      rgb_lagged_sum|   6|   1000000|     100|0.14085317|  PASSED             #
	 * #      rgb_lagged_sum|   7|   1000000|     100|0.00001639|   WEAK              #
	 * #      rgb_lagged_sum|   8|   1000000|     100|0.07674376|  PASSED             #
	 * #      rgb_lagged_sum|   9|   1000000|     100|0.00764132|  PASSED             #
	 * #      rgb_lagged_sum|  10|   1000000|     100|0.05339300|  PASSED             #
	 * #      rgb_lagged_sum|  11|   1000000|     100|0.00000000|  FAILED             #
	 * #      rgb_lagged_sum|  12|   1000000|     100|0.00544162|  PASSED             #
	 * #      rgb_lagged_sum|  13|   1000000|     100|0.00665530|  PASSED             #
	 * #      rgb_lagged_sum|  14|   1000000|     100|0.21547609|  PASSED             #
	 * #      rgb_lagged_sum|  15|   1000000|     100|0.00005538|   WEAK              #
	 * #      rgb_lagged_sum|  16|   1000000|     100|0.03563740|  PASSED             #
	 * #      rgb_lagged_sum|  17|   1000000|     100|0.00863607|  PASSED             #
	 * #      rgb_lagged_sum|  18|   1000000|     100|0.10989656|  PASSED             #
	 * #      rgb_lagged_sum|  19|   1000000|     100|0.00000000|  FAILED             #
	 * #      rgb_lagged_sum|  20|   1000000|     100|0.02861136|  PASSED             #
	 * #      rgb_lagged_sum|  21|   1000000|     100|0.00551620|  PASSED             #
	 * #      rgb_lagged_sum|  22|   1000000|     100|0.00607141|  PASSED             #
	 * #      rgb_lagged_sum|  23|   1000000|     100|0.00000008|  FAILED             #
	 * #      rgb_lagged_sum|  24|   1000000|     100|0.07240627|  PASSED             #
	 * #      rgb_lagged_sum|  25|   1000000|     100|0.00070920|   WEAK              #
	 * #      rgb_lagged_sum|  26|   1000000|     100|0.18582527|  PASSED             #
	 * #      rgb_lagged_sum|  27|   1000000|     100|0.00000001|  FAILED             #
	 * #      rgb_lagged_sum|  28|   1000000|     100|0.42734407|  PASSED             #
	 * #      rgb_lagged_sum|  29|   1000000|     100|0.00776284|  PASSED             #
	 * #      rgb_lagged_sum|  30|   1000000|     100|0.28195880|  PASSED             #
	 * #      rgb_lagged_sum|  31|   1000000|     100|0.00000000|  FAILED             #
	 * #      rgb_lagged_sum|  32|   1000000|     100|0.22208246|  PASSED             #
	 * #     rgb_kstest_test|   0|     10000|    1000|0.55171675|  PASSED             #
	 * #     dab_bytedistrib|   0|  51200000|       1|0.00872171|  PASSED             #
	 * #             dab_dct| 256|     50000|       1|0.72116312|  PASSED             #
	 * #        dab_filltree|  32|  15000000|       1|0.48026690|  PASSED             #
	 * #        dab_filltree|  32|  15000000|       1|0.55679223|  PASSED             #
	 * #       dab_filltree2|   0|   5000000|       1|0.23468126|  PASSED             #
	 * #       dab_filltree2|   1|   5000000|       1|0.09744176|  PASSED             #
	 * #        dab_monobit2|  12|  65000000|       1|0.93173064|  PASSED             #
	 * #==============================================================================#
	 */
}
