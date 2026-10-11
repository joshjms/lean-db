import LeanDB.BPTree.Tree

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


def main : IO Unit :=
  IO.print sampleTree.shape
