;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |exercise 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./remove_duplicates.rkt")

; This defines the basic album datatype.
(define-struct album (title artist genre))
; define-struct automatically creates the following functions for you:
;
; `make-<struct-name>` (in this case `make-album`)
;   a function to create an instance of the struct
;   this function takes arguments for each of the fields listed, so for example
;   (make-struct 'Sway' 'Tove Styrke' 'Pop') will create an album struct
;    with title 'Sway', artist 'Tove Styrke' & genre 'Pop
;
; `<struct-name>-<field-name>` (for each field)
;    functions for accessing values of each field in the struct
;    for album this would mean we'd have the following functions:
;    `album-title`, `album-artist`, `album-genre`
;    the following examples creates an album and then accesses its fields
;    ```
;    (define sway (make-album 'Sway' 'Tove Styrke' 'Pop')
;    (album-title sway) ; returns 'Sway'
;    (album-artist sway) ; returns 'Tove Styrke'
;    (album-genre sway) ; returns 'Pop'
;    ```
;
; `<struct-name>?` (in this case `album?`)
;   a predicate (function which returns a boolean) that checks a value and
;   returns true if it's an instance of the struct, false otherwise
;   using the `sway` album defined in the previous example
;   ```
;   (album? sway) ; returns true
;   (album? 1) ; returns false
;   (album? 'hi') ; returns false
;   ```

;;; Enter a list of albums below
;;; They need not be the actual albums you own.
;;; But you should include enough variety to adequately
;;; test your code.
;;;
;;; Here's what we mean.  One of the questions involves
;;; writing a procedure that finds all the albums of a
;;; given genre.  If all the albums in the library are
;;; in the rock genre, then there's only one genre and
;;; when you ask for all the rock albums and it gives
;;; back all the albums, you don't know whether that's
;;; because the code really works, or because it's
;;; not even paying attention to the genre.  So you want
;;; to make sure there are multiple artists and genres,
;;; some artists with only one album or genre, others
;;; with multiple artists or genres, etc.

(define testing-library-1
  ;; Fill in the info below
  (list (make-album "title1" "as" "country")
        (make-album "title2" "wer" "rock")
        (make-album "title3" "done" "rock")
        (make-album "title4" "happy" "jazz")
        (make-album "title5" "artist" "pop")
        (make-album "title6" "yo" "pop")
        (make-album "badname" "yo" "country")
        (make-album "goodname" "ma" "NA")
        ))

;;; Now enter a *different* list here so you can test your code on
;;; two different libraries to be sure it really works
(define testing-library-2
  ;; Fill in the info below
  (list (make-album "titlea" "art" "classical")
       (make-album "element" "art" "rock")
        (make-album "index" "art" "classical")
        (make-album "young" "art" "jazz")
        (make-album "list" "am" "jazz")
        (make-album "0index" "am" "jazz")
        (make-album "friend" "oy" "country")
        (make-album "zoner" "goo" "rock")
        ))

;;; Add the procedures you write (e.g. all-genres, versatile-artists)
;;; below.  Be sure to test your procedures to make sure they work.
;;; We are not providing test cases this time, so it's up to you
;;; to make sure your code works.  We will use our own test cases
;;; when grading and assign you a grade based on the number of
;;; test cases that passed.


;; all-titles : (listof album) -> (listof string)
(define all-titles (lambda (x)
                     (map (lambda (album)
                            (album-title album))
                          x)
                     )
  )

(check-expect (all-titles (list (make-album "a" "somebody" "rock")
 (make-album "b" "somebody 2" "country")))
 (list "a" "b"))

(all-titles testing-library-1)
(all-titles testing-library-2)

;; all-artists: (listof album) -> (listof string)
(define all-artists (lambda (x)
                      (remove-duplicates (map (lambda (album)
                                                (album-artist album))
                                              x)
                                         )
                      )
  )

(all-artists testing-library-1)
(all-artists testing-library-2)

;; all-genres: (listof album) -> (listof string)
(define all-genres (lambda (x)
                      (remove-duplicates (map (lambda (album)
                                                (album-genre album))
                                              x)
                                         )
                      )
  )

(all-genres testing-library-1)
(all-genres testing-library-2)



;; artist-albums : string, (listof album) -> (listof album)
(define artist-albums (lambda (artist lib)
                        (filter (lambda (album) (string=? (album-artist album)
                                                          artist)
                                  )
                                lib)
                        )
  )

(artist-albums "art" testing-library-2)
(artist-albums "yo" testing-library-1)

;; artist-genres: string, (listof album) -> (listof string)
(define (artist-genres artist lib) (remove-duplicates (all-genres (artist-albums artist lib
                                                                                   )
                                                                    )
                                                      )
  )

(artist-genres "art" testing-library-2)
(artist-genres "am" testing-library-2)
(artist-genres "yo" testing-library-1)

;; artist-is-versatile?: string, (listof album) -> boolean

(define (artist-is-versatile? artist lib) (> (length (artist-genres artist lib)) 1))

(artist-is-versatile? "art" testing-library-2)
(artist-is-versatile? "am" testing-library-2)
(artist-is-versatile? "yo" testing-library-1)

;; versatile-artists: (listof album) -> (listof string)

(define (versatile-artists lib) (remove-duplicates
                                 (all-artists (filter (lambda (album)
                                                        (artist-is-versatile? (album-artist album) lib))
                                                      lib)
                                              )
                                 )
  )

(versatile-artists testing-library-1)
(versatile-artists testing-library-2)

;; artist-album-counts: (listof album) -> (listof (list string number))

(define (artist-album-count artist lib) (length (artist-albums artist lib)))

(define (artist-album-count-list artist lib) (list artist (artist-album-count artist lib)))

(define (artist-album-counts lib) (remove-duplicates (map (lambda (album)
                                                            (artist-album-count-list (album-artist album) lib)
                                                            )lib
                                                             )
                                                     )
  )


(artist-album-counts testing-library-2)
(artist-album-counts testing-library-1)

;; genre-album-counts: (listof album) -> (listof (list string number))

(define genre-albums (lambda (genre lib)
                        (filter (lambda (album) (string=? (album-genre album)
                                                          genre)
                                  )
                                lib)
                        )
  )


(define (genre-album-count genre lib) (length (genre-albums genre lib)))

(define (genre-album-count-list genre lib) (list genre (genre-album-count genre lib)))

(define (genre-album-counts lib) (remove-duplicates (map (lambda (album)
                                                            (genre-album-count-list (album-genre album) lib)
                                                            )lib
                                                             )
                                                     )
  )

(genre-album-counts testing-library-2)
(genre-album-counts testing-library-1)