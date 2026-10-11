/-
Copyright (c) 2026 LeanDB Authors. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: LeanDB Authors
-/

import LeanDB.Server.Server

import Cli
open Cli

/--
`ldb up`
Starts the LeanDB HTTP Server
-/
def runUp (p : Parsed) : IO UInt32 := do
  let port : Nat := (p.flag! "port").as! Nat

  if port == 0 || port > 65535 then
    IO.eprintln s!"invalid port: {port}"
    return 1

  let server <- DBServer.new { port := port.toUInt16 }
  server.up
  return 0

/--
`ldb`
Prints the help message
-/
def printHelp (p : Parsed) : IO UInt32 := do
  p.printHelp
  return 0
