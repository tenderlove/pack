(module pack
        (export pack)
        (import scheme chicken ports srfi-14)

        (define (next-command) (read-char))

        (define (process-command bytes command)
          (let ((byte (car bytes)))
            (cond ((char=? #\C command) (write-char (integer->char byte))))
            (cdr bytes)))

        (define (process bytes)
          (let ((command (next-command)))
            (if (eof-object? command)
                '()
                (process (process-command bytes command)))))

        (define (pack-stream bytes format output)
          (with-output-to-port output (lambda ()
            (with-input-from-port format (lambda ()
              (process bytes))))))

        (define (pack format bytes)
          (let ((byte-format (open-input-string format))
                (output (open-output-string)))
            (pack-stream bytes byte-format output)
            (get-output-string output)))
)
