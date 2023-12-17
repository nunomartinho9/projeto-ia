(defun escrever-tabuleiros (data)
  (with-open-file (stream "problemas.dat"
                          :direction :output
                          :if-exists :supersede)
    (dolist (item data)
      (write item :stream stream)
      (terpri stream))
    (finish-output stream) ; Flushes the stream
    (close stream)))       ; Closes the file handle explicitly
