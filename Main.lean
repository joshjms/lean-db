/-
Copyright (c) 2026 LeanDB Authors. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: LeanDB Authors
-/

import LeanDB.Cli.Commands

def main (args : List String) : IO UInt32 := do
  ldbCmd.validate args
