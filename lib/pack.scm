(module pack

(export pack make-packer)
(import scheme chicken ports)

(define (translate-input insns bytes)
  (if (null? insns)
      '()
      (translate-input (cdr insns) ((car insns) bytes))))

(define (scan-number char)
  (let scan ((ack (list char)))
    (let ((c (peek-char)))
      (if (or (eof-object? c) (not (char-numeric? c)))
          (string->number (list->string (reverse ack)))
          (scan (cons (read-char) ack))))))

(define (make-apply-char char)
  (lambda (bytes) (apply-cmd char bytes)))

(define (make-apply-num num cmd acc)
  (if (= num 0)
      acc
      (make-apply-num (- num 1) cmd (cons cmd acc))))

(define (parse-format)
  (reverse (port-fold (lambda (char acc)
    (cond ((eof-object? char) acc)
          ((char-numeric? char)
           (make-apply-num (- (scan-number char) 1) (car acc) acc))
          (else (cons (make-apply-char char) acc))))
                                  '()
                                  read-char)))

(define (compile-format)
  (let ((insns (parse-format)))
    (lambda (bytes) (translate-input insns bytes))))

(define (make-packer format)
  (with-input-from-port (open-input-string format) compile-format))

(define (apply-cmd command bytes)
  (let ((byte (car bytes)))
    (cond ((char=? #\C command) (write-char (integer->char byte))))
    (cdr bytes)))

(define (pack format bytes)
  (let ((packer (make-packer format))
        (output (open-output-string)))
    (with-output-to-port output (lambda () (packer bytes)))
    (get-output-string output)))
)
