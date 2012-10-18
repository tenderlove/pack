(include "../lib/pack")
(import pack)
(use test)

(test "A" (pack "C" (list 65)))
(test "AB" (pack "CC" (list 65 66)))
