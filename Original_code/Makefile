main: test1.4

####################################
# Phase 0
test0.1: Rcode_phase0/phase0_LayoutExamples_test.R
	echo "Running $<"
	R --vanilla < $<

test0.2: Rcode_phase0/phase0_LayoutVisualisation_test.R
	echo "Running $<"
	R --vanilla < $<

test0.3: Rcode_phase0/phase0_LayoutPixel_test.R
	echo "Running $<"
	R --vanilla < $<

####################################
# Phase 1
test1.1: Rcode_phase1/phase1_PixelReading_test.R
	echo "Running $<"
	R --vanilla < $<

test1.2: Rcode_phase1/phase1_LogRegression_test.R
	echo "Running $<"
	R --vanilla < $<

test1.3: Rcode_phase1/phase1_CountsAngles_test.R
	echo "Running $<"
	R --vanilla < $<

test1.4: Rcode_phase1/phase1_KFG_test.R
	echo "Running $<"
	R --vanilla < $<

# Not all dependencies yet expressed here.
Rcode_phase0/phase0_LayoutExamples_test.R Rcode_phase0/phase0_LayoutVisualisation_test.R Rcode_phase0/phase0_LayoutPixel_test.R: paths.R
Rcode_phase1/phase1_LogRegression_test.R: Rcode_phase1/phase1_PixelReading_test.R
Rcode_phase0/phase0_LayoutExamples_test.R: Rcode_phase0/phase0_LayoutExamples_body.R
