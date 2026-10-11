/-
Copyright (c) 2026 LeanDB Authors. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: LeanDB Authors
-/

import LeanDB.Cli.Handlers

import Cli
open Cli

def upCmd : Cmd := `[Cli|
  up VIA runUp;
  "Start the LeanDB HTTP Server."

  FLAGS:
    p, port : Nat; "Listening port. Default: 6767"
]

def ldbCmd : Cmd := `[Cli|
  ldb VIA printHelp; ["0.1.0"]
  "LeanDB - world's first formally verified database made using Lean 4."

  SUBCOMMANDS:
    upCmd
]
