VALGRIND_2.1 {
	global:
		vgPlain_*;
		vgTool_*;
		vgProf_*;
                vgOff_*;
                vgArch_*;

	local:
		*;		# default to hidden
};
