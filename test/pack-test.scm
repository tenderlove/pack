(include "../lib/pack")
(import pack)
(use test)

(test "A" (pack "C" (list 65)))
(test "AB" (pack "CC" (list 65 66)))
(test "AB" (pack "C2" (list 65 66)))
(test "ABC" (pack "C3" (list 65 66 67)))

(test (list 65) (with-input-from-port (open-input-string "A") (make-unpacker "C")))
(test (list 65) (unpack "C" "A"))

;(test (list 65) (unpack "C" "A"))
;(test "\xAB\xCD\xFF\xFF" (pack "N" (list #xABCDFFFF)))
;(test '(#\C #\C) (make-packer "CC"))
;(test '(#\C 2) (make-packer "C2"))
;(test '(#\C 21) (make-packer "C21"))
;(test '(#\C 21 #\A) (make-packer "C21A"))
