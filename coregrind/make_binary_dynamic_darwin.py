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
  if len(sys.argv) != 2:
    print("Usage: python3 make_binary_dynamic_darwin.py <binary>")
    sys.exit(1)

  toolpath = sys.argv[1]
  toolpath_dyn = f"{toolpath}.dynamic"
  tool = lief.parse(toolpath)

  # Remove signature if present as it will be invalidated by our modifications
  tool.remove_signature()
  # Make the binary dynamic
  tool.header.add(lief._lief.MachO.HEADER_FLAGS.DYLDLINK)
  tool.header.add(lief._lief.MachO.HEADER_FLAGS.PIE)
  tool.add(lief.MachO.DynamicSymbolCommand(), 5)
  tool.add(lief.MachO.DylinkerCommand("/usr/lib/dyld"), 6)
  # dyld doesn't dynamic library without LC_MAIN and you can't have both LC_MAIN and LC_UNIXTHREAD
  image_base = tool.get_segment("__TEXT").virtual_address
  stack_size = tool.get_segment("__UNIXSTACK").virtual_size
  entry_address = tool.thread_command.pc
  offset_from_text = entry_address - image_base
  tool.add(lief.MachO.MainCommand(offset_from_text, stack_size), 8)
  tool.remove(lief._lief.MachO.LOAD_COMMAND_TYPES.UNIXTHREAD)

  # Commit the changes and make the binary executable
  tool.write(toolpath_dyn)
  os.system(f"cp {toolpath} {toolpath}.orig")
  os.replace(toolpath_dyn, toolpath)
  os.chmod(toolpath, 0o755)

  # Now we need to resign the binary
  subprocess.run(["codesign", "-f", "-s", "-", toolpath], check=True)

if __name__ == "__main__":
  main()
