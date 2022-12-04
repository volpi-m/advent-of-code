(defvar priority "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun read-file (file lines)
    (with-open-file (in file)
        (loop repeat lines
            collect (read-line in))))

(defvar lines (read-file "input" 300))

(defun take (nb lst)
    (if (> nb 0)
        (cons (first lst) (take (- nb 1) (cdr lst)))
        ()))

(defun get-ltr (str nb) (subseq str nb (+ nb 1)))

(defun fst-half (str) (subseq str 0 (/ (length str) 2)))
(defun snd-half (str) (subseq str (/ (length str) 2)))

(defun is-letter-there (letter str)
    (if (= (length str) 0)
        nil
        (if (string= letter (subseq str 0 1))
            t
            (is-letter-there letter (subseq str 1)))))

(defun find-common-letter (half1 half2)
    (setq letter (get-ltr half1 0))
    (if (is-letter-there letter half2)
        letter
        (find-common-letter (subseq half1 1) half2)))

(defun get-priority (letter p)
    (if (string= letter (subseq p 0 1))
        1
        (+ 1 (get-priority letter (subseq p 1)))))

(defun sum-priority (lines)
    (if (= 0 (length lines))
        0
        (+
            (get-priority (find-common-letter (fst-half (first lines)) (snd-half (first lines))) priority)
            (sum-priority (subseq lines 1)))))

;(print (fst-half (first lines)))
;(print (snd-half (first lines)))

;(print (get-priority (find-common-letter (fst-half (first lines)) (snd-half (first lines))) priority))

(print (sum-priority lines))
