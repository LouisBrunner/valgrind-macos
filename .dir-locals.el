;; Emacs settings for Valgrind.

(
 ;; Format used to turn a bug number into a URL (when displaying it).
 (nil . ((bug-reference-url-format . "http://bugs.kde.org/show_bug.cgi?id=%s")))

 ;; Standard settings for editing C source files.
 (c-mode . (
	    ;; Apply the Linux style as a base.
	    (c-file-style . "linux")
	    ;; Use spaces instead of tabs for indentation.
	    (indent-tabs-mode . nil)
	    ;; Indent 3 columns per level.
	    (c-basic-offset . 3)
	    ;; Lines should be limited to 80 columns.
	    (fill-column . 80)
	   ))
)
