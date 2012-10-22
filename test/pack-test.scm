(include "../lib/pack")
(import pack)
(use test)

(define (pack-unpack format input)
  (test input (unpack format (pack format input))))

(test "A" (pack "C" (list 65)))
(test "AB" (pack "CC" (list 65 66)))
(test "AB" (pack "C2" (list 65 66)))
(test "ABC" (pack "C3" (list 65 66 67)))

(test (list 65) (with-input-from-port (open-input-string "A") (make-unpacker "C")))
(test (list 65) (unpack "C" "A"))
(test (list 65 66 67) (unpack "C3" "ABC"))

(pack-unpack "C" (list 65))
(pack-unpack "N" (list #xABCDFFFF))
(pack-unpack "N2" (list #xABCDFFFF #xDEADBEEF))
(pack-unpack "n4" (list #xABCD #xFFFF #xDEAD #xBEEF))
