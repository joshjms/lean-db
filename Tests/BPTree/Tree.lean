import LeanDB.BPTree.Tree

-- The entries are based on Unova Regional Pokedex
def sampleTree : Tree := {
  config := {
    name := "test-db",
    order := 4,
  }
  root :=
    .internal #[20, 40] #[
      .leaf #[{ key := 5, val := "pignite" }, { key := 10, val := "patrat" }],
      .leaf #[{ key := 20, val := "simisear" }, { key := 30, val := "roggenrola" }],
      .leaf #[{ key := 40, val := "conkeldurr" }, { key := 50, val := "whirlipede" }],
    ]
}

#guard childIndex #[10, 20, 30, 40, 50] 5 == 0
#guard childIndex #[10, 20, 30, 40, 50] 15 == 1
#guard childIndex #[10, 20, 30, 40, 50] 40 == 4
#guard childIndex #[10, 20, 30, 40, 50] 55 == 5

#guard sampleTree.get? 5 == "pignite"
#guard sampleTree.get? 10 == "patrat"
#guard sampleTree.get? 15 == none
#guard sampleTree.get? 20 == "simisear"
#guard sampleTree.get? 30 == "roggenrola"
#guard sampleTree.get? 40 == "conkeldurr"
#guard sampleTree.get? 50 == "whirlipede"
