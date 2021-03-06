(module pack

(export pack make-packer unpack make-unpacker)
(import scheme chicken ports)
(use srfi-69)

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

(define (parse-format table)
  (let ((lookup (lambda (key)
                  (hash-table-ref table key
                                  (lambda () (error "Unknown cmd: " key))))))
    (reverse (port-fold (lambda (char acc)
      (cond ((eof-object? char) acc)
            ((char-numeric? char)
             (make-apply-num (- (scan-number char) 1) (car acc) acc))
            (else (cons (lookup char) acc))))
                        '()
                        read-char))))

(define (compile-format format compiler)
  (with-input-from-port (open-input-string format) compiler))

;;; PACK

(define pack-commands (alist->hash-table
  (list (cons #\C
              (lambda (byte)
                (write-char (integer->char byte))))
        (cons #\N
              (lambda (byte)
                (write-char (integer->char (& (>> byte 24) #xFF)))
                (write-char (integer->char (& (>> byte 16) #xFF)))
                (write-char (integer->char (& (>> byte 8)  #xFF)))
                (write-char (integer->char (& byte #xFF)))))
        (cons #\n
              (lambda (byte)
                (write-char (integer->char (& (>> byte 8)  #xFF)))
                (write-char (integer->char (& byte #xFF))))))))

(define (compile-pack)
  (let ((insns (parse-format pack-commands)))
    (lambda (bytes)
      (for-each (lambda (fn byte) (fn byte)) insns bytes))))

(define (make-packer format) (compile-format format compile-pack))

(define (pack format bytes)
  (let ((packer (make-packer format))
        (output (open-output-string)))
    (with-output-to-port output (lambda () (packer bytes)))
    (get-output-string output)))

;;; UNPACK

(define unpack-commands (alist->hash-table
  (list (cons #\C
              (lambda () (char->integer (read-char))))
        (cons #\N
              (lambda ()
                (+ (<< (char->integer (read-char)) 24)
                   (<< (char->integer (read-char)) 16)
                   (<< (char->integer (read-char)) 8)
                   (char->integer (read-char)))))
        (cons #\n
              (lambda ()
                (+ (<< (char->integer (read-char)) 8)
                   (char->integer (read-char))))))))

(define (compile-unpack)
  (let ((insns (parse-format unpack-commands)))
    (lambda ()
      (map (lambda (fn) (fn)) insns))))

(define (make-unpacker format) (compile-format format compile-unpack))

(define (unpack format str)
  (let ((unpacker (make-unpacker format))
        (input (open-input-string str)))
    (with-input-from-port input unpacker)))

)
