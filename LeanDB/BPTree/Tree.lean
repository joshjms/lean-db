import LeanDB.Utils.Helpers

structure Config where
  name : String := "leandb"
  order : Nat := 3

structure Entry where
  key : Nat
  val : String
  deriving Repr, Inhabited

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
  | .leaf entries =>
    indent ++ "(" ++ concatNats (entries.map (fun e => e.key)) ++ ")\n"
  | .internal keys children =>
    let below := children.map (fun c => c.shape (indent ++ "  "))
    indent ++ "[" ++ concatNats keys ++ "]\n" ++ String.join below.toList

def Tree.shape (t : Tree) : String := t.root.shape ""

-- BPTree methods

/-- `childIndex` finds the first index in an array of keys where keys[index] > k -/
def childIndex (keys : Array Nat) (k : Nat) : Nat :=
  binarySearch 0 keys.size
where
  binarySearch (l r : Nat) : Nat :=
    if h : l < r then
      let mid := (l + r) / 2
      if keys[mid]! <= k then binarySearch (mid + 1) r else binarySearch l mid
    else
      l
  termination_by r - l

-- `put`

def Tree.put (t : Tree) (_k : Nat) (_v : String) : Tree :=
  t

-- `get`

def Node.get? (u : Node) (k : Nat) : Option String :=
  match u with
  | .leaf entries =>
    match entries.find? (fun e => e.key == k) with
    | some e => some e.val
    | none => none
  | .internal keys children =>
    let i := childIndex keys k
    if h : i < children.size then
      children[i].get? k
    else
      none

def Tree.get? (t : Tree) (k : Nat) : Option String :=
  t.root.get? k

-- `delete`

def Tree.delete (t : Tree) (_k : Nat) : Tree :=
  t
