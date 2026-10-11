/-
Copyright (c) 2026 LeanDB Authors. All rights reserved.
Released under MIT license as described in the file LICENSE.
Authors: LeanDB Authors
-/

import LeanDB.Utils.Helpers

structure Config where
  name : String := "leandb"
  order : Nat := 3

structure Entry where
  key : Nat
  val : String
  deriving BEq, Repr, Inhabited

inductive Node where
  | leaf (entries : Array Entry)
  | internal (keys : Array Nat) (children : Array Node)
  deriving Repr, Inhabited

structure Tree where
  config : Config
  root : Node

--- BPTree visualization

def Node.shape (u : Node) (indent : String) : String :=
  match u with
  | .leaf entries => indent ++ "(" ++ concatNats (entries.map (fun e => e.key)) ++ ")\n"
  | .internal keys children =>
    let below := children.map (fun c => c.shape (indent ++ "  "))
    indent ++ "[" ++ concatNats keys ++ "]\n" ++ String.join below.toList

def Tree.shape (t : Tree) : String :=
  t.root.shape ""

def Node.height (u : Node) : Nat :=
  match u with
  | .leaf _ => 1
  | .internal _ children => 1 + (children.attach.map (fun ⟨e, _h⟩ => e.height)).foldl max 0
termination_by u

-- BPTree methods

/-- `childIndex` finds the first index in an array of keys where keys[index] > k -/
def childIndex (keys : Array Nat) (k : Nat) : Nat :=
  binarySearch 0 keys.size
where binarySearch (l r : Nat) : Nat :=
    if h : l < r then
      let mid := (l + r) / 2
      if keys[mid]! <= k then binarySearch (mid + 1) r else binarySearch l mid
    else l
  termination_by r - l

-- `put`

def insertOrUpdateEntry (entries : Array Entry) (k : Nat) (v : String) : Array Entry :=
  entries.filter (fun e => e.key < k) ++ #[{ key := k, val := v }] ++
    entries.filter (fun e => e.key > k)

inductive PutResult where
  | split (l : Node) (m : Nat) (r : Node)
  | done (u : Node)

def splitLeaf (entries : Array Entry) : PutResult :=
  let mid := entries.size / 2
  let l := entries[:mid]
  let r := entries[mid:]
  .split (.leaf l) r[0]!.key (.leaf r)

def splitInternal (keys : Array Nat) (children : Array Node) : PutResult :=
  let mid := keys.size / 2
  let l := Node.internal keys[:mid] children[:(mid + 1)]
  let r := Node.internal keys[(mid + 1):] children[(mid + 1):]
  .split l keys[mid]! r

def Node.put (u : Node) (k : Nat) (v : String) (cfg : Config) : PutResult :=
  match u with
  | .leaf entries =>
    let entries := insertOrUpdateEntry entries k v
    if entries.size < cfg.order then .done (.leaf entries) else splitLeaf entries
  | .internal keys children =>
    let idx := childIndex keys k
    if h : idx < children.size then
      -- put on the children
      let childPutResult := children[idx].put k v cfg
      match childPutResult with
      | .done u => .done (.internal keys (children.set! idx u))
      | .split l mid r =>
        -- Add mid to keys and add l and r around that index
        let keys := keys.insertIdx! idx mid
        let children := (children.set! idx l).insertIdx! (idx + 1) r
        if keys.size < cfg.order then .done (.internal keys children)
        else splitInternal keys children
    else .done u

def Tree.put (t : Tree) (k : Nat) (v : String) : Tree :=
  match t.root.put k v t.config with
  | .done u => { t with root := u }
  | .split l mid r =>
    let keys := #[mid]
    let children := #[l, r]
    let newRoot := Node.internal keys children
    { t with root := newRoot }

-- `get`

def Node.get? (u : Node) (k : Nat) : Option String :=
  match u with
  | .leaf entries =>
    match entries.find? (fun e => e.key == k) with
    | some e => some e.val
    | none => none
  | .internal keys children =>
    let idx := childIndex keys k
    if h : idx < children.size then children[idx].get? k else none

def Tree.get? (t : Tree) (k : Nat) : Option String :=
  t.root.get? k

-- `range`

def Node.range (u : Node) (lk : Nat) (rk : Nat) : Array Entry :=
  match u with
  | .leaf entries => entries.filter (fun e => (e.key >= lk) && (e.key <= rk))
  | .internal keys children =>
    let leftIdx := childIndex keys lk
    let rightIdx := childIndex keys rk
    (children.attach.extract leftIdx (rightIdx + 1)).flatMap (fun ⟨c, _h⟩ => c.range lk rk)
termination_by u

def Tree.range (t : Tree) (lk : Nat) (rk : Nat) : Array Entry :=
  t.root.range lk rk

-- `delete`

def Tree.delete (t : Tree) (_k : Nat) : Tree :=
  t
