VALGRIND_2.1 {
	global:
		vgPlain_*;
		vgTool_*;
		vgProf_*;
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
		*IRArray*;
		*IRAtom*;
		LibVEX_Alloc;
	local:
		*;		# default to hidden
};
