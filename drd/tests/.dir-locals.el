;; Emacs settings for drd tests.

(
 ;; Standard settings for editing C source files.
 (c-mode . (
	    ;; Apply the Linux style as a base.
	    (c-file-style . "linux")
	    ;; Use spaces instead of tabs for indentation.
	    (indent-tabs-mode . nil)
	    ;; Indent 2 columns per level.  Note that this differs from
	    ;; the usual Valgrind style.
	    (c-basic-offset . 2)
	    ;; Lines should be limited to 80 columns.
	    (fill-column . 80)
	   ))
)
