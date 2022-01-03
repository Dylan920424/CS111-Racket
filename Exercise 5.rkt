;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "./file_operations.rkt")

; Part 1

; backup! : Path Path -> Void
;
; Recursively copies all the files and subdirectories in the `from`
; directory to the `to` directory. This is a modified version of the
; `copy-tree` procedure we discussed in class.
;
; EFFECT: `to` and all its contents now exist
; EFFECT: may overwrite existing files at `to`
(define (backup! from to) 
  (begin
    ; create the destination directory if it doesn't already exist
    (unless (directory-exists? to)
      (make-directory! to))

    ; for each file (leaf node) in the origin directory,
    ; copy it over to the destination directory
    (for-each (λ (file)
                ; print the name of the file being copied into the REPL
                ; for more on how `printf` works, see Appendix 1 in the pdf
                (when (or (not (file-exists? (build-path to (path-filename file))))
                             (> (file-or-directory-modify-seconds file)
                                (file-or-directory-modify-seconds (build-path to (path-filename file)))
                                )
                             )
                  (begin
                    (printf "Copying file ~A to ~A~n" file to)
                    (copy-file! file
                                (build-path to (path-filename file))
                                #true))))
              (directory-files from))

    ; for each folder (recursive child node) in the origin directory,
    ; recursively `backup!` its contents
    (for-each (λ (subdir)
                (backup! subdir
                         ; add the subdirectory's name to the
                         ; end of the original destination path
                         (build-path to (path-filename subdir))))
              (directory-subdirectories from))))

; Part 2

; count-files: path -> number

;(define (count-files path)
;  (local [(define count 0)]
;    (begin
;      (for-each (lambda (file)
;                  (set! count (+ count 1))
;                  )
;                (directory-files path)
;                )
;      (for-each (lambda (sub)
;                  (set! count (+ count (count-files sub))))
;                (directory-subdirectories path)
;                )
;       count
;      )
;    )
;  )

(define (count-files path)
  (local [(define count 0)]
    (begin
      (for-each (lambda (file)
                  (set! count (+ count 1))
                  )
                (directory-files path)
                )
      (set! count (+ count
                     (apply + (map count-files
                                   (directory-subdirectories path)
                                   )
                            )
                     ))
      count
      )
    )
  )

; directory-size: path -> number
(define (directory-size path)
  (local [(define ans 0)]
    (begin
      (for-each (lambda (file)
                  (set! ans (+ ans (file-size file)))
                  )
                (directory-files path)
                )
      (set! ans (+ ans
                     (apply + (map directory-size
                                   (directory-subdirectories path)
                                   )
                            )
                     ))
      ans
      )
    ))

; search-directory: string path -> (listof path)
(define (search-directory name path)
  (local [(define ans '())]
    (begin
      (for-each (lambda (file)
                  (when (string-contains? name (path->string (path-filename file)))
                    (set! ans (cons file ans))
                    )
                  )
                (directory-files path)
                )
      (set! ans (append ans
                        (apply append (map (lambda (n)
                                             (search-directory name n))
                                           (directory-subdirectories path)
                                           )
                               )
                        ))
      ans
      )
    )
  )


; filter-directory: (path -> boolean) path -> (listof path)
(define (filter-directory predicate path)
  (local [(define ans '())]
    (begin
      (for-each (lambda (file)
                  (when (predicate file)
                    (set! ans (cons file ans))
                    )
                  )
                (directory-files path)
                )
      (set! ans (append ans
                        (apply append (map (lambda (n)
                                             (filter-directory predicate n))
                                           (directory-subdirectories path)
                                           )
                               )
                        ))
      ans
      )
    ))

; find-file-type: string path -> (listof path)
(define (find-file-type extension path)
  (filter-directory (lambda (n) (path-has-extension? n extension)) path)
  )

; file-type-disk-usage: string path -> number
(define (file-type-disk-usage extension path)
  (apply + (map file-size
                (find-file-type extension path)
                )
         )
  )