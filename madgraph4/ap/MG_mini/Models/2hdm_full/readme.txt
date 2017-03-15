****************************************************************************
**                                                                        **
**       A SHORT TUTORIAL FOR NEW MODEL IMPLEMENTATION IN MG/ME           **
**                                                                        **
**                   CP3 DVLPT TEAM - APRIL 06                            **
**                                                                        **
****************************************************************************

1. MODEL DIRECTORY STRUCTURE
============================

	In order to implement new models in MadGraphII/MadEvent, you ONLY need
	to modify files in the Models/you_model_name directory. Here is the content
	of these files, please stick to these conventions!

	PARTICLES.DAT: A text file containing:
		a) An extensive list of ALL particles in the model (including SM ones)
		   in the format:
			Name anti_Name  Spin    Linetype Mass Width Color Label PDG Code
			str  str        S,F,V   W,S,D,C  num  num   S,T,O str   num
		b) Prescriptions for groups of particles to sum over. Notice that these line
		   are modified if MG_ME is executed from the web interface

	INTERACTIONS.DAT: A text file containing an extensive list of all possible 3 and 4
		vertices in the model, with the corresponding variable names containing the numerical
		value of these vertices.


	COUPL.INC: A Fortran include file containing variable declarations for all variables NEEDED BY MG/ME
		(and only them). In particular all variables defined in particles.dat (masses, widths)
		and interactions.dat (couplings) must be here.

	INPUT.INC: A Fortran include file containing variable declarations for all variables that
		are needed to compute couplings but not by MG_ME directly.

	COUPLINGS.F: A Fortran file containing
		a) A extensive list of algebraic formulas to compute all couplings defined 
		in interactions.dat. 
		b) Eventually, user declaration/definition of intermediate variables/routines needed
		to compute these couplings

	RW_PARAM.F: A Fortran file containg a single subroutine called "setpara". This subroutine call
		the load_lha_para subroutine which read the LHA parameter file param_card.dat and
		fill in the corresponding variables declared in coupl.inc and input.inc. 

	IDENTNAMEFILE.DAT: A text file where user-defined variable names can be matched with the corresponding
		LHA code. By default, the reading routine will create a variable name in the format BLOCK_ID1_ID2_...
		but one can modify here this default behaviour. Do not forget to reflect changes in the setpara 
		routine.

	RUNNING.F : Fortran file where the routines used by madgraph to run parameter with energy
		are defined. You can modify them to take into account the new particles in your model. 

	LHA_READING.F: A Fortran file containing all the LHA format reading routines. This file should be only
		modified if new conventions in the LHA format have to be taken into account.
	