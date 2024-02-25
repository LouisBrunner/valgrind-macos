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
    print("Usage: python3 fixup_dynamic_binary_darwin.py <binary>")
    sys.exit(1)

  toolpath = sys.argv[1]
  toolpath_fixed = f"{toolpath}.fixed"
  tool = lief.parse(toolpath)

  # Remove signature if present as it will be invalidated by our modifications
  tool.remove_signature()
  # libSystem.B.dylib has we don't need it and it might mess up our memory layout
  # and initaalize things we don't want to be initialized (yet)
  tool.remove(lief._lief.MachO.LOAD_COMMAND_TYPES.LOAD_DYLIB) # libSystem.B.dylib

  # Commit the changes and make the binary executable
  tool.write(toolpath_fixed)
  os.system(f"cp {toolpath} {toolpath}.orig")
  os.replace(toolpath_fixed, toolpath)
  os.chmod(toolpath, 0o755)

  # Now we need to resign the binary
  subprocess.run(["codesign", "-f", "-s", "-", toolpath], check=True)

if __name__ == "__main__":
  main()
