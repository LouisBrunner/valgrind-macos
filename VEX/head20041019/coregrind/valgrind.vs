VALGRIND_2.1 {
	global:
		vgPlain_*;
		vgSkin_*;
		vgProf_*;
                vgOff_*;
                vgArch_*;
		ppIRBB;
		emptyIR*;
		*IRTypeEnv;
		*IRExpr*;
		*IRStmt*;
		*IRBB*;
		*IRDirty*;
		LibVEX_Alloc;

	local:
		*;		# default to hidden
};
