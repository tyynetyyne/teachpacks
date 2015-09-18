;; Display-read-utility v.0.5
;; =================
;; display-read displays one image and shows a text editor, returns the string entered in the text buffer
;; display-no-read displays one image but shows no text editor, returns empty string
;; You can move coursor with arrowkeys and delete characters with backspace-key 
;; The editor shows (BUFFER-SIZE - 1) characters at the time, but the textbuffer can contain a lot more
;; =================
;; Constants:
#lang racket                         ; comment out in WeScheme
(require 2htdp/image)                ; comment out in WeScheme
(require 2htdp/universe)             ; comment out in WeScheme
(require "big-crunch.rkt")           ; comment out in WeScheme

(provide display-read
         display-no-read
         ; empty-image               ;needed in WeScheme
         )

;; needed in WeScheme:
;(define empty-image (rectangle 0 0 0 "white"))

(define BUFFER-SIZE 30)
(define TEXT-X 8)    

;; default color and size for the text editor field
(define COLOR "blue")
(define SIZE 20)

;; default color and size for the text user wants to display
;; default line-lenght for the displayed string
(define TEXT-SIZE 30)
(define TEXT-COLOR "black")
(define LINE-LENGTH 30)

;; render-text : String -> Image
;; converts a string into an image (defines font, size, color and style)
(define (render-text s)
  (text/font s SIZE COLOR "Courier New" 'modern 'normal 'normal #f))

;; editor window dimensions are set according to used font SIZE and BUFFER-SIZE
(define HEIGHT (+ (image-height (render-text "m")) 5))
(define WIDTH (+ (* (sub1 BUFFER-SIZE) (image-width (render-text "m"))) (* 2 TEXT-X))) 

(define TEXT-Y (/ HEIGHT 2))

(define CURSOR-IMG (rectangle 1 (+ SIZE 3) "solid" "red"))
(define MTS (empty-scene WIDTH HEIGHT))

(define MARGIN 15)

(define MARGIN-IMG (rectangle WIDTH MARGIN "solid" "white"))

;; =================
;; Data definitions:

;; Cursor is Natural [0, BUFFER-SIZE)
;; interp. the position of the cursor in the text buffer, which is visible on the screen
;; 0 is the first possible position (before any characters)
(define C1 0)     ; first position before any characters on the screen
(define C2 1)     ; after 1st character
(define C3 49)    ; last position if BUFFER-SIZE is 50

#;
(define (fn-for-cursor c)
  (... c))

(define-struct editor (textbuf curpos screenpos))
;; Editor is (make-editor String Cursor Natural)
;; interp. stores editor state:
;; - textbuf (String) stores the characters that user has entered
;; - curpos (Cursor) is the cursor position between the displayed characters
;; - screenpos (Natural) stores the index of the first displayed character from the textbuf (indexing starts from zero)

(define E1 (make-editor "" 0 0  ))                           ; empty editor, cursor in the beginning
(define E2 (make-editor "testing" 7 0  ))                    ; one word in the textbuffer with cursor in the end of the word
(define E3 (make-editor "testing that this works" 10 0  ))   ; multiple words and cursor after the 10th character
(define E4 (make-editor "testing that this works also with longer strings like this" 10 8  ))   
                                                           ; multiple words and cursor after the 10th character, 
                                                           ; first displayed word is "that"
#;
(define (fn-for-editor e)
  (... (editor-textbuf e)     ;String
       (editor-curpos e)      ;Cursor
       (editor-screenpos e)   ;Natural
       (editor-img e)))        ;Image

(define-struct window (editor img active?))
;; Window is (make-window Editor Image Boolean)
;; interp. stores UI window state:
;; - editor (Editor), editor struct containing buffer and cursor info
;; - img (Image), user provided image to be displayed in the UI window
;; - active? (Boolean), when #true window is displayed, when set to #false window closes
;; - display-mode? (Boolean), when #true no editor shown

(define W1 (make-window (make-editor "" 0 0) (circle 100 "solid" "red") #t)) ; normal window with image and empty editor

#;
(define (fn-for-window w)
  (... (window-editor w)     ;Editor
       (window-img w)        ;Image
       (window-active? w)))   ;Boolean

;; =================
;; Functions:

;; string-split : String -> list-of-String
;; NOTE: needed in WeScheme (comment out in DrRacket)
;(define (string-split str)
;  (rec-string-split '() "" str))

;; rec-string-split : list-of-String String String -> list-of-string
;; splitting a string into a list of strings at each whitespace
;; NOTE: needed in WeScheme (comment out in DrRacket)
;(define (rec-string-split str-list curstr str)
;      (cond [(and (<= (string-length str) 0)
;                  (not (<= (string-length curstr) 0)))
;             (append str-list (list curstr))]
;            [(<= (string-length str) 0)
;             str-list]
;            [(and (string=? (substring str 0 1) " ")
;                  (> (string-length curstr) 0))
;             (rec-string-split (append str-list (list curstr)) "" (substring str 1))]
;            [(string=? (substring str 0 1) " ")
;             (rec-string-split str-list curstr (substring str 1))]
;            [else
;             (rec-string-split str-list (string-append curstr (substring str 0 1)) (substring str 1))]))

;; print-text : String Number -> Image
;; splits the given string into list of strings and calls merge-words
(define (print-text text line-length)
  (text->image (merge-words '() (string-split text) "" line-length)
               line-length
               empty-image))

;; merge-words : list-of-Strings list-of-Strings String Number -> list-of-Strings
;; combines words into lines
(define (merge-words word-list-ok word-list c-line line-length)
  (if (empty? word-list)
      (append word-list-ok (list c-line))
      (cond [(< (+ (string-length c-line)(string-length (first word-list))) line-length)
             (merge-words word-list-ok (rest word-list) (string-append c-line (first word-list) " ") line-length)]
            [(> (string-length (first word-list)) line-length)
             (if (= (string-length c-line) 0)
                 (merge-words (append word-list-ok (list (substring (first word-list) 0 (sub1 line-length))))
                              (cons (substring (first word-list) (sub1 line-length))(rest word-list))
                              c-line
                              line-length)
                 (merge-words (append word-list-ok
                                      (list c-line)
                                      (list (substring (first word-list) 0 (sub1 line-length))))
                              (cons (substring (first word-list) (sub1 line-length))(rest word-list))
                              ""
                              line-length))
                 ]
            [else
             (merge-words (append word-list-ok (list c-line)) word-list "" line-length)])))

;; text->image : list-of-Strings Number Image -> Image
;; converts a list of line strings into one image
(define (text->image lines line-length image)
  (cond [(empty? lines)
         image]
        [(string? (first lines)) ;(<= (add1 line-length) (string-length (first lines))))
         (text->image (rest lines)
                      line-length
                      (above/align "left" image (text (first lines) TEXT-SIZE TEXT-COLOR)))]
        [ else
          (text->image (rest lines)
                       line-length
                       (above/align "left" image empty-image))]))

;; item->image : Any -> Image
(define (item->image item)
  (cond [(number? item)
         (print-text (number->string item) LINE-LENGTH)]
        [(string? item)
         (print-text item LINE-LENGTH)]
        [(image? item)
         item]
        [else
         empty-image]))
         

;; display-read : Image or String or Number -> String
;; a function for displaying user provided image and opening editor for user input
(define (display-read item)
         (editor-textbuf (window-editor (big-bang/big-crunch (make-window (make-editor "" 0 0) (item->image item) #t)                       
                                                             (to-draw render-whole)      
                                                             (on-key handle-key-whole)
                                                             (stop-when stop?)))))

;; display-no-read :  Image or String or Number -> String
;; a function for displaying user provided image and opening editor for user input
(define (display-no-read item)
  (editor-textbuf (window-editor (big-bang/big-crunch (make-window (make-editor "" 0 0) (item->image item) #t)                       
                                             (to-draw render-img)      
                                             (on-key handle-key-img)
                                             (stop-when stop?)))))

;; stop? : Window -> Boolean
;; tests if the window should be closed
(define (stop? w)
  (not (window-active? w)))

;; =================
;; render : Editor -> Image
;; displays the visible part of the textbuffer on the screen

(define (render e)
  (place-image/align (beside (render-text (left-of-cursor (fit-to-screen e) (editor-curpos e)))
                             CURSOR-IMG
                             (render-text (right-of-cursor (fit-to-screen e)(editor-curpos e))))
                     TEXT-X 
                     TEXT-Y
                     "left" 
                     "center"
                     MTS))

;; fit-image : Image -> Image
;; scales the user image to fit the editor width
(define (fit-image img)
  (cond [(= 0 (image-width img))
         img]
        [(> (image-width img) WIDTH)
         (scale (/ (- WIDTH (* 2 MARGIN)) (image-width img)) img)]
        [else img]))

;; render-whole : Window -> Image
;; render the editor and image in the window
(define (render-whole w)
  (above (render-img w)
         (render (window-editor w))))
  
;; render-img : Window -> Image
;; render the editor and image in the window
(define (render-img w)
  (above MARGIN-IMG
         (fit-image (window-img w))
         MARGIN-IMG))

;; Helper-functions:
;; =================
;; cut-left-side : Editor -> String
;; cuts off the characters that will not fit into the screen on the left side

(define (cut-left-side e)
  (if (> (string-length (editor-textbuf e)) 0) 
      (substring (editor-textbuf e)
                 (editor-screenpos e))
      (editor-textbuf e)))

;; cut-right-side : String -> String
;; cuts of the characters that will not fit on the screen on the right side

(define (cut-right-side s)
  (if (> (string-length s) BUFFER-SIZE)
      (substring s 0 (sub1 BUFFER-SIZE))
      s))
   
;; fit-to-screen : Editor -> String
;; cuts off the the characters, which will not fit on the screen (left and right sides)

(define (fit-to-screen e)
  (cut-right-side (cut-left-side e))) 

;; left-of-cursor : String Cursor -> String
;; returns the part of string which is on the left side of the cursor

(define (left-of-cursor str pos)
  (if (> (string-length str) pos)
      (substring str 0 pos)
      str))

;; right-of-cursor : String Cursor -> String
;; returns the part of string which is on the right side of the cursor

(define (right-of-cursor str pos)
  (if (> (string-length str) pos)
      (substring str pos)
      ""))

;; =================
;; handle-key : Editor Key -> Editor
;; move cursor to left with left arrow, to right with right arrow, remove characters with DEL

(define (handle-key e k)
  (cond [(key=? k "left")
         (move-cursor-to-left e)]
        [(key=? k "right")
         (move-cursor-to-right e)]
        [(key=? k "\b")
         (if (> (string-length (editor-textbuf e)) 0)
             (delete-one-chr e)
             e)]
        [(key=? k "\r") e]
        [(displayable-character? k)
         (add-to-buffer e k)]
        [else e]))

;; handle-key-whole : Window Key -> Window
;; checks if return/enter has been pressed

(define (handle-key-whole w k)
  (cond [(key=? k "\r")
         (make-window (window-editor w) (window-img w) #f)]
        [else
         (make-window (handle-key (window-editor w) k)
                      (window-img w)
                      (window-active? w))]))

;; handle-key-img : Window Key -> Window
;; checks if return/enter has been pressed

(define (handle-key-img w k)
  (cond [(key=? k "\r")
         (make-window (window-editor w) (window-img w) #f)]
        [else w]))

;; helper functions:
;; =================
;; delete-one-chr : Editor -> Editor
;; takes off one character to the left of the current cursor position, if there is a character

(define (delete-one-chr e)
  (cond [(and (> (characters-left-of-cursor e) 0)(> (editor-screenpos e) 0)) 
         (make-editor (delete-one-chr-left-of-cursor e)      
                      (editor-curpos e)      
                   (sub1 (editor-screenpos e)))]
        [(> (characters-left-of-cursor e) 0)
         (make-editor (delete-one-chr-left-of-cursor e)      
                   (sub1 (editor-curpos e))      
                   (editor-screenpos e))]
        [else e]))  

;; delete-one-chr-left-of-cursor : Editor -> String
;; takes off one character to the left of the current cursor position, if there is a character
(define (delete-one-chr-left-of-cursor e)
  (string-append (delete-chr 
                  (left-of-cursor (editor-textbuf e) (absolute-curpos e)))
                 (right-of-cursor (editor-textbuf e) (absolute-curpos e))))

;; delete-chr : String -> String
;; takes off the last character of the given string

(define (delete-chr s)
      (substring s 0 (sub1 (string-length s))))
  
;; move-cursor-to-left : Editor -> Editor
;; moves cursor to the left one step, if there are more characters

(define (move-cursor-to-left e)
  (cond [(> (editor-curpos e) 0)
         (set-cursor-position e (sub1 (editor-curpos e)))]
        [(and (= (editor-curpos e) 0)(> (characters-left-of-cursor e) 0))
         (set-screen-position e (sub1 (editor-screenpos e)))]
        [else e]))

;; characters-left-of-cursor : Editor -> Natural
;; tells how many characters there are on the left side of the cursor in the text buffer

(define (characters-left-of-cursor e)
  (string-length (left-of-cursor (editor-textbuf e) (absolute-curpos e))))

;; characters-right-of-cursor : Editor -> Natural
;; tels how many characters there are on the right side of the cursor in the text buffer

(define (characters-right-of-cursor e)
  (string-length (right-of-cursor (editor-textbuf e) (absolute-curpos e))))

;; absolute-curpos : Editor -> Natural
;; absolute position of the cursor in the text buffer 

(define (absolute-curpos e)
  (+ (editor-screenpos e)(editor-curpos e)))
                                                       
;; move-cursor-to-right : Editor -> Editor
;; moves cursor to the right one step, if there are more characters 

(define (move-cursor-to-right e)
  (cond [(and (< (add1 (editor-curpos e)) BUFFER-SIZE)(> (characters-right-of-cursor e) 0))
         (set-cursor-position e (add1 (editor-curpos e)))]
        [(and (= (add1 (editor-curpos e)) BUFFER-SIZE)(> (characters-right-of-cursor e) 0))
         (set-screen-position e (add1 (editor-screenpos e)))] 
        [else e]))
  
;; set-cursor-position : Editor Cursor -> Editor
;; setter for cursor position
(define (set-cursor-position e c)
    (make-editor (editor-textbuf e)     
                 c      
                 (editor-screenpos e)))
  
;; set-screen-position : Editor Natural -> Editor
;; setter for screen position
(define (set-screen-position e sp)
    (make-editor (editor-textbuf e)     
                 (editor-curpos e)  
                 sp))  

;; add-to-buffer : Editor -> Editor
;; adds one character in the cursor position

(define (add-to-buffer e c)
  (if (< (add1 (editor-curpos e)) BUFFER-SIZE)
      (make-editor (string-append (left-of-cursor (editor-textbuf e) (absolute-curpos e)) 
                                  c 
                                  (right-of-cursor (editor-textbuf e)(absolute-curpos e)))
                   (add1 (editor-curpos e))
                   (editor-screenpos e))
      (make-editor (string-append (editor-textbuf e) c)
                   (editor-curpos e)
                   (add1 (editor-screenpos e)))))

;; displayable-character? : String -> Boolean
;; checks if the key that was pressed is a single character

(define (displayable-character? k)
         (= (string-length k) 1))
