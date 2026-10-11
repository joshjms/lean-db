/-
Copyright (c) 2026 LeanDB Authors. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: LeanDB Authors
-/

def concatNats (xs : Array Nat) : String :=
  String.intercalate " " (xs.toList.map toString)
