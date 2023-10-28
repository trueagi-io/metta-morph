(import (chicken platform) (chicken foreign) (chicken string))

(define-external (mettamorph (c-string arg1)) c-string
                 (let ((result (eval (read (open-input-string arg1)))))
                      (if (symbol? result)
                          (string-append "'" (->string result))
                          (if (boolean? result)
                              (if result "True" "False")
                              (if (string? result)
                                  (string-append (string-append "\"" result) "\"")
                                  (->string result))))))

(return-to-host)
