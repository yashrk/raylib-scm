;;; Macros to define foreign functions

; Converts signatures like
; ((c-pointer (struct StructType)) structName) to "*structName"
; Converts signatures like
; (int name) or ((c-pointer void) name) to "name"
(define-for-syntax (get-argument signature)
  (let ((name (symbol->string (cadr signature))))
    (if (and (list? (car signature))
             (list? (cadar signature)))
        (string-append "*" name)
        name)))

; Creates foreign-lambda for C function with struct arguments passed by value.
; Arguments:
; - name of C function (string);
; - return type in standard Chicken FFI format;
; - argument list in standard Chicken FFI format.
(define-syntax foreign-lambda-with-struct
  (er-macro-transformer
   (lambda (exp rename compare)
     (let* ((args (drop exp 3))
            (foreign-function-name (list-ref exp 1))
            (return-type (list-ref exp 2))
            (c-names (map get-argument (car args)))
            (c-names (string-join c-names ", "))
            (c-function (string-join (list foreign-function-name "(" c-names ")") ""))
            (c-call (if (eq? return-type 'void)
                        (string-join (list c-function ";") "")
                        (string-join (list "C_return(" c-function ");") ""))))
       `(foreign-lambda* ,return-type ,@args
          ,c-call)))))

; Creates named Scheme function for C function with struct arguments passed by value.
; Arguments:
; - Scheme function name
; - name of C function (string);
; - return type in standard Chicken FFI format;
; - argument list in standard Chicken FFI format.
(define-syntax foreign-define-with-struct
  (er-macro-transformer
   (lambda (exp rename compare)
     (let* ((to-lambda (drop exp 2))
            (args (drop exp 4))
            (function-name (list-ref exp 1))
            (names (map cadr (car args))))
       `(define (,function-name ,@names)
          ((foreign-lambda-with-struct ,@to-lambda)
           ,@names))))))

; Creates boolean Scheme function (predicate) for C function returning 0 for false and
; something else for true.
; Arguments:
; - Scheme function name
; - name of C function (string);
; - return type in standard Chicken FFI format;
; - argument list in standard Chicken FFI format.
(define-syntax foreign-predicate
  (er-macro-transformer
   (lambda (exp rename compare)
     (let* ((to-lambda (drop exp 2))
            (args (drop exp 4))
            (function-name (list-ref exp 1))
            (names (map cadr (car args))))
       `(define (,function-name ,@names)
          (not (= ((foreign-lambda-with-struct ,@to-lambda)
                   ,@names)
                  0)))))))

; Creates foreign-lambda* that creates newly allocated C structure
; Arguments:
; - Scheme function name
; - name of C function (string);
; - return type in Scheme format;
; - return type in standard Chicken FFI format;
; - list of arguments in standard Chicken FFI format.
(define-syntax foreign-constructor
  (er-macro-transformer
   (lambda (exp rename compare)
     (let* ((rest (drop exp 5))
            (args (if (eq? rest '())
                      '()
                      (car rest)))
            (function-name (list-ref exp 1))
            (foreign-function-name (list-ref exp 2))
            (return-type (list-ref exp 3))
            (c-type (symbol->string (cadadr (list-ref exp 4))))
            (names (map cadr args))
            (c-names (map get-argument args))
            (c-names (string-join c-names ", "))
            (c-function (string-join (list foreign-function-name "(" c-names ")") ""))
            (allocation-code
             (format #f
                     "~a* new_object = (~a*)malloc(sizeof(~a));
                      *new_object = ~a;
                      C_return(new_object);"
                     c-type
                     c-type
                     c-type
                     c-function)))
       `(define (,function-name ,@names)
          (let ((new-object
                 ((foreign-lambda* ,return-type ,args
                    ,allocation-code)
                  ,@names)))
            (set-finalizer! new-object free)
            new-object))))))

; Creates foreign-lambda* that allocates C structure
; and fills its fields with given values.
; Argumens:
; - Scheme function name
; - return type in Scheme format;
; - string with C type of structure;
; - list of fields in standard Chicken FFI format
(define-syntax foreign-constructor*
  (er-macro-transformer
   (lambda (exp rename compare)
     (let* ((args (car (drop exp 4)))
            (function-name (list-ref exp 1))
            (return-type (list-ref exp 2))
            (c-type (list-ref exp 3))
            (names (map cadr args))
            (c-names (map get-argument args))
            (init-field (lambda (field)
                          (format #f
                                  "new_object->~a = ~a;"
                                  (string-delete #\* field)
                                  field)))
            (init-strings (map init-field c-names))
            (allocation-code (format #f
                                     "~a* new_object = (~a*)malloc(sizeof(~a));
                                      ~a
                                      C_return(new_object);"
                                     c-type
                                     c-type
                                     c-type
                                     (string-join init-strings ""))))
       `(define (,function-name ,@names)
          (let ((new-object
                 ((foreign-lambda* ,return-type ,args
                    ,allocation-code)
                  ,@names)))
            (set-finalizer! new-object free)
            new-object))))))
