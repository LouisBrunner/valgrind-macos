
Our long term goal is to move to structure Valgrind's top level as a
set of well-defined modules.  Much of the difficulty in maintaining
the beast is caused by the lack of clear boundaries, definitions and
semantics for subsystems (modules), and in particular a lack of
clarity about which modules may depend on which others.  The ongoing
modularisation activities are aimed at dealing with this problem.

Architecture dependent stuff will be chopped up and placed into the
relevant modules.  Since the system's top level is now to be
structured as modules with clearly delimited areas of functionality,
directories such as 'amd64', 'amd64-linux', etc, cannot continue to
exist long-term.  These trees contain mish-mashes of functionality
from multiple different modules, and so make no sense as top-level
entities in a scheme where all top-level entities are modules.

This process is ongoing.  Consequently some of the code in coregrind/
has been bought into the module structure, but much hasn't.  A naming
scheme distinguishes the done vs not-done stuff:

  Consider a module of name 'foo'.  

  If 'foo' is implemented in a single C file, and requires no other
  files, it will live in coregrind/m_foo.c.

  Otherwise (if 'foo' requires more than one C file, or more than
  zero private header files, or any other kind of auxiliary stuff)
  then it will live in the directory coregrind/m_foo.

Each module 'foo' must have two associated header files which describe
its public (exported) interface:

  include/pub_tool_foo.h
  coregrind/pub_core_foo.h

pub_tool_foo.h describes that part of the module's functionality that
is visible to tools.  Hopefully this can be minimal or zero.  If there
is nothing to visible to tool, pub_tool_foo.h can be omitted.

pub_core_foo.h describes functionality that is visible to other
modules in the core.  This is a strict superset of the visible-to-tool
functionality.  Consequently, pub_core_foo.h *must* #include
pub_tool_foo.h, if it exists.  pub_tool_foo.h *must not* #include
pub_core_foo.h, nor any other pub_core_ header for that matter.

Module-private headers are named "priv_foo.h".

No module may include the private headers of any other module.  If a
type/enum/function/struct/whatever is stated in neither
include/pub_tool_foo.h nor coregrind/pub_core_foo.h then module 'foo'
DOES NOT EXPORT IT.

Over time it is hoped to develop some simple Perl scripts to scan
source files for #includes so as to mechanically enforce these rules.
One of the most infuriating aspects of C is the total lack of support
for building properly abstracted subsystems.  This is in sharp
comparison to languages such as Modula3, Haskell, ML, all of which
have support for modules built into the language, and hence such
boundaries are enforceable by the compiler.
