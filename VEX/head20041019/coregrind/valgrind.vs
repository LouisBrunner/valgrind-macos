VALGRIND_2.1 {
	global:
		vgPlain_*;
		vgSkin_*;
		vgProf_*;
                vgOff_*;
                vgArch_*;
		*IRExpr*;
		*IRStmt*;
		*IRBB*;
		*IRDirty*;
		*IRType*;
		*IRCallee*;
		LibVEX_Alloc;

	local:
		*;		# default to hidden
};
