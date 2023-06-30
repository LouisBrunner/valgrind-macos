#!/usr/bin/env python3
import os
import subprocess
import sys
try:
  import lief
except ImportError:
  print("this version of valgrind is not ready for production yet (and depends on an unreleased version of lief)")
  sys.exit(1)
# lief has trouble adding a command at the end of the list,
# so we try to add them where they would naturally be

def main():
  if len(sys.argv) != 3:
    print("Usage: python3 fixup_dynamic_binary_darwin.py <binary> <image_base>")
    sys.exit(1)

  image_base = int(sys.argv[2], 16)

  toolpath = sys.argv[1]
  toolpath_fixed = f"{toolpath}.fixed"
  tool = lief.parse(toolpath)

  # Remove signature if present as it will be invalidated by our modifications
  tool.remove_signature()
  # Remove non-required stuff
  tool.header.remove(lief._lief.MachO.HEADER_FLAGS.TWOLEVEL)
  tool.remove(lief._lief.MachO.LOAD_COMMAND_TYPES.LOAD_DYLIB) # libSystem.B.dylib
  tool.remove(lief._lief.MachO.LOAD_COMMAND_TYPES.BUILD_VERSION)
  tool.remove(lief._lief.MachO.LOAD_COMMAND_TYPES.DYLD_INFO_ONLY)
  tool.remove_section("__unwind_info")
  tool.remove_section("__stubs")
  tool.remove_section("__stub_helper")
  tool.remove_section("__got")
  tool.remove_section("__la_symbol_ptr")
  tool.remove(lief._lief.MachO.LOAD_COMMAND_TYPES.DATA_IN_CODE)
  tool.remove(lief._lief.MachO.LOAD_COMMAND_TYPES.FUNCTION_STARTS)
  # offset all segments and sections so the load address is correct
  text = tool.get_segment("__TEXT")
  data = tool.get_segment("__DATA")
  offset = image_base - text.virtual_address
  text.virtual_address = image_base
  text.get_section("__text").virtual_address += offset
  text.get_section("__const").virtual_address += offset
  text.get_section("__cstring").virtual_address += offset
  data.virtual_address += offset
  data.get_section("__const").virtual_address += offset
  data.get_section("__data").virtual_address += offset
  data.get_section("__common").virtual_address += offset
  data.get_section("__bss").virtual_address += offset
  tool.get_segment("__LINKEDIT").virtual_address += offset

  # Commit the changes and make the binary executable
  tool.write(toolpath_fixed)
  os.system(f"cp {toolpath} {toolpath}.orig")
  os.replace(toolpath_fixed, toolpath)
  os.chmod(toolpath, 0o755)

  # Now we need to resign the binary
  subprocess.run(["codesign", "-f", "-s", "-", toolpath], check=True)

if __name__ == "__main__":
  main()
