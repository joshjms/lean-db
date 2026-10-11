/-
Copyright (c) 2026 LeanDB Authors. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: LeanDB Authors
-/

import Std.Data.HashMap
import Std.Http.Server

import LeanDB.BPTree.Tree

open Std Async Http Net

structure ServerOptions where
  port : UInt16 := 6767

structure DBServer where
  opts : ServerOptions
  databases : IO.Ref (HashMap String Tree)

def DBServer.new (opts : ServerOptions := {}) : IO DBServer := do
  return { opts := opts, databases := <- IO.mkRef {} }

instance : Server.Handler DBServer where
  onRequest _self _req := do
    Std.Http.Response.ok |>.text "Buy LeanCoin™"

def DBServer.up (s : DBServer) : IO Unit :=
  Std.Async.Async.block do
    let addr : Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, s.opts.port⟩
    let server <- Server.serve addr s
    IO.println s!"LeanDB starting on http://127.0.0.1:{s.opts.port}"
    server.waitShutdown
