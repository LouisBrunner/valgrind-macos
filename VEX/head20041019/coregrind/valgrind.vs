VALGRIND_2.1 {
	global:
		vgPlain_*;
		vgSkin_*;
		vgProf_*;
                vgOff_*;
                vgArch_*;
		*IROp*;
		*IRExpr*;
		*IRStmt*;
		*IRBB*;
		*IRDirty*;
		*IRType*;
		*IRTemp*;
		*IRConst*;
		*IRCallee*;
		LibVEX_Alloc;

	local:
		*;		# default to hidden
};
