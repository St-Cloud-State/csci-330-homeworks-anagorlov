(defun str-to-chars (str)
  (coerce str 'list))

(defun parse-input (input)
  (let* ((chars (str-to-chars input))
         (res (parse-I chars 0)))
    (if (and (first res) (null (second res)))
        (format t "String \"~A\" is valid.~%" input)
        (format t "String \"~A\" is invalid. Error: ~A~%" input (third res)))))

(defun parse-I (txt pos)
  (if (and txt (char= (first txt) #\i))
      (let ((e-res (parse-E (rest txt) (1+ pos))))
        (if (first e-res)
            (let ((s-res (parse-S (second e-res) pos)))
              (if (first s-res)
                  (list t (second s-res) nil)
                  (list nil nil (third s-res))))
            (list nil nil (third e-res))))
      (list nil nil (format nil "Expected 'i' at pos ~D." pos))))


(defun parse-E (str pos)
  (let ((g-res (parse-G str pos)))
    (if (first g-res)
        (parse-E-prime (second g-res) pos)
        (list nil nil (third g-res)))))


(defun parse-E-prime (inp pos)
  (if (and inp (char= (first inp) #\o))
      (let ((g-res (parse-G (rest inp) (1+ pos))))
        (if (first g-res)
            (parse-E-prime (second g-res) pos)
            (list nil nil (third g-res))))
      (list t inp nil)))

(defun parse-G (input pos)
  (if (and input (member (first input) '(#\x #\y #\z #\w)))
      (list t (rest input) nil)
      (list nil nil (format nil "Expected x/y/z/w at pos ~D." pos))))



  (if (and items (char= (first items) #\s))
      (list t (rest items) nil)
      (if (and items (char= (first items) #\d))
          (let ((l-res (parse-L (rest items) (1+ pos))))
            (if (and (first l-res) (second l-res) (char= (first (second l-res)) #\b))
                (list t (rest (second l-res)) nil)
                (list nil nil "Expected 'b'.")))
          (list nil nil "Expected 's' or 'd'."))))


(defun parse-L (input pos)
  (if (and input (char= (first input) #\s))
      (parse-L-prime (rest input) pos)
      (list nil nil "Expected 's'.")))


(defun parse-L-prime (input pos)
  (if (or (null input) (char= (first input) #\b))
      (list t input nil)
      (parse-L input pos)))

(defun run-tests ()
  (mapc #'parse-input '("ixoys" "iydssb" "ixoydsbs" "ixoydssbes" "ixoyowdssbess" "ixozoweyeydsbsbs"))
  (mapc #'parse-input '("xoys" "iydszb" "ixoydcbs" "ixoydssbea" "ixoyowdssbasse" "ixozoweyeydsbsb1")))


(run-tests)
