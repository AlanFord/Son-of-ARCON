# Son-of-ARCON
Bringing the ARCON96 code back to life.

ARCON96 is an old program (from the 90s) used to simulate downwind concentrations of airborne materials in the vicinity of buildings.  It converts source release rates into downwind receptor concentrations.  While the documentation ([NUREG/CR-6331 Rev. 1](http://www.osti.gov/scitech/biblio/481848)) indicates some of ARCON96 is 32-bit, significant portions are older, 16-bit ANSI graphics.

ARCON96 has gotten long in the tooth.  The source code for the compute module (found in Appendix A of the public document NUREG/CR-6331 Rev. 1) is written in FORTRAN 77.  Son-of-ARCON will update the FORTRAN to FORTRAN 95 (as implemented by GNU Fortran) and fix a bug or two.

The source code for the user interface of ARCON96 was written in some sort of Microsoft Visual Basic and was never made publicly available.  This front-end will be rewritten as part of the ARCON-greasepaint project.

The original ARCON96 software was written by J. V. Ramsdell Jr. and C. A. Simonen at the Pacific Northwest National Laboratory, prepared for the U.S. Nuclear Regulatory Commission.  Much more detail can be found in the document NUREG/CR-6331 Rev. 1, "Atomospheric Relative Concentrations in Building Wakes."
