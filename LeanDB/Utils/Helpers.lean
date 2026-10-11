def concatNats (xs : Array Nat) : String :=
  String.intercalate " " (xs.toList.map toString)
