val K = "\nval _ = print (concat [\"val K = \\\"\", String.translate (fn #\"\\n\" => \"\\\\n\" | #\"\\\\\" => \"\\\\\\\\\" | #\"\\\"\" => \"\\\\\\\"\" | c => str c) K, \"\\\"\", K, \"\\n\"])"
val _ = print (concat ["val K = \"", String.translate (fn #"\n" => "\\n" | #"\\" => "\\\\" | #"\"" => "\\\"" | c => str c) K, "\"", K, "\n"])
