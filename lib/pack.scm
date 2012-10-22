(module pack

(export pack make-packer)
(import scheme chicken ports)
(use srfi-1)

;;; HELPERS

(define << arithmetic-shift)
(define (>> n count) (arithmetic-shift n (- count)))
(define & bitwise-and)

(define (scan-number char)
  (let scan ((ack (list char)))
    (let ((c (peek-char)))
      (if (or (eof-object? c) (not (char-numeric? c)))
          (string->number (list->string (reverse ack)))
          (scan (cons (read-char) ack))))))

(define (make-apply-num num cmd acc)
  (if (= num 0)
      acc
      (make-apply-num (- num 1) cmd (cons cmd acc))))

(define (parse-format command)
  (reverse (port-fold (lambda (char acc)
    (cond ((eof-object? char) acc)
          ((char-numeric? char)
           (make-apply-num (- (scan-number char) 1) (car acc) acc))
          (else (cons (command char) acc))))
                                  '()
                                  read-char)))

;;; PACK

(define (apply-pack-insns insns bytes)
  (for-each (lambda (fn byte) (fn byte)) insns bytes))

(define (make-pack-command command)
  (lambda (byte) (apply-pack command byte)))

(define (compile-pack)
  (let ((insns (parse-format make-pack-command)))
    (lambda (bytes) (apply-pack-insns insns bytes))))

(define (apply-pack command byte)
  (cond ((char=? #\C command)
         (write-char (integer->char byte)))
        ((char=? #\N command)
         (begin
           (write-char (integer->char (& (>> byte 24) #xFF)))
           (write-char (integer->char (& (>> byte 16) #xFF)))
           (write-char (integer->char (& (>> byte 8)  #xFF)))
           (write-char (integer->char (& byte #xFF)))))
        (else (error "Unknown command: " command))))

(define (make-packer format)
  (with-input-from-port (open-input-string format) compile-pack))

(define (pack format bytes)
  (let ((packer (make-packer format))
        (output (open-output-string)))
    (with-output-to-port output (lambda () (packer bytes)))
    (get-output-string output)))

)
