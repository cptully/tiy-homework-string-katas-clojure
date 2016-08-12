(ns tiy-homework-string-katas-clojure.katas
  (:require [clojure.string :as s]))
;  (:require [clojure.math.numeric-tower :as math]))


;The parameter weekday is true if it is a weekday, and the parameter vacation is
;true if we are on vacation. We sleep in if it is not a weekday or we're on
;vacation. Return true if we sleep in.
(defn sleep-in [week-day vacation]
  (or (not week-day)  vacation))

;We have two monkeys, a and b, and the parameters aSmile and bSmile indicate if
;each is smiling. We are in trouble if they are both smiling or if neither of
;them is smiling. Return true if we are in trouble.
(defn monkey-trouble [aSmile, bSmile]
  (or (and aSmile bSmile) (not (and aSmile bSmile))))

;Given two int values, return their sum. Unless the two values are the
;same, then return double their sum.
(defn sum-double [a, b]
  (if (= a b)
    (* 2 (+ a b))
    (+ a b)))


;Given an int n, return the absolute difference between n and 21, except
;return double the absolute difference if n is over 21.
(defn diff21 [n]
  (if (> n  21)
    (* 2 (- n 21))
    (- 21 n)))

;We have a loud talking parrot. The "hour" parameter is the current hour
;time in the range 0..23. We are in trouble if the parrot is talking
;and the hour is before 7 or after 20. Return true if we are in trouble.
(defn parrot-trouble [talking hour]
  (and talking (or (< hour 7) (> hour 20))))

;Given 2 ints, a and b, return true if one if them is 10 or if their sum is 10.
(defn makes10 [a b]
  (or (= a 10) (= b 10) (= (+ a b) 10)))


;Given an int n, return true if it is within 10 of 100 or 200.
;Note: Math.abs(num) computes the absolute value of a number.
(defn abs [n]
  (max n (- n)))

(defn near-hundred [n]
  (or (<= (abs (- 100 n)) 10) (<= (abs (- 200 n)) 10)))

;Given 2 int values, return true if one is negative and one is positive. Except
;if the parameter "negative" is true, then return true only if both are negative.
(defn pos-neg [a b negative]
  (and (not negative)
       (or
         (and (< a 0) (> b 0))
         (and (> a 0) (< b 0)))))


;Given a string, return a new string where "not " has been added to the front.
;However, if the string already begins with "not", return the string unchanged.
;Note: use .equals() to compare 2 strings.
(defn not-string [str-test]
  (if (<= (count str-test) 3)
    (str "not " str-test)
    (if (not (= (subs str-test 0 3) "not"))
     (str "not " str-test)
     str-test)))


(defn missing-char [str-test n]
  (if (< (count str-test) n)
    ("error!")
    (if (= n 0)
      (subs str-test (+ n 1));
      (if (= n (- (count str-test) 1))
        (subs str-test 0 (+ n 1))
        (str (subs str-test 0 n) (subs str-test (+ n 1)))))))

;Given a string, return a new string where the first and last chars have been exchanged.
(defn front-back [str-test]
  (if (<= (count str-test) 1)
    (str)
    (str (.charAt str-test (- (count str-test) 1)) (subs str-test 1 (- (count str-test) 1))  (.charAt str-test 0))))


;Given a string, we'll say that the front is the first 3 chars of the string. If the
;string length is less than 3, the front is whatever is there. Return a new string
;which is 3 copies of the front.
(defn front3 [str-test]
  (if (< (count str-test) 3)
    (str str-test str-test str-test)
    (let [first3 (subs str-test 0 3)]
      (str first3 first3 first3))))

;Given a string, take the last char and return a new string with the last char added
;at the front and back, so "cat" yields "tcatt". The original string will be length 1 or more.*/
(defn back-around [str-test]
  (let [last  (.charAt str-test (- (count str-test) 1))]
     (str last str-test last)))


;Return true if the given non-negative number is a multiple of 3 or a multiple of 5.
;Use the % "mod" operator
(defn or35 [n]
  (or (= (mod n  3) 0) (= (mod n  5) 0)))


;Given a string, take the first 2 chars and return the string with the 2 chars added
;at both the front and back, so "kitten" yields"kikittenki". If the string length is
;less than 2, use whatever chars are there.*/
(defn front22 [str-test]
  (if (<= (count str-test) 2)
    (str str-test str-test str-test)
    (str (subs str-test 0,2) str-test (subs str-test 0,2))))

;Given a string, return true if the string starts with "hi" and false otherwise.
(defn start-hi [str-test]
   (if (< (count str-test) 2)
     false
     (if (= (subs str-test 0 2) "hi")
       true
       false)))

;Given two temperatures, return true if one is less than 0 and the other is greater than 100.
(defn icy-hot [temp1 temp2]
  (or (and (> temp1 100) (< temp2 0)) (and (< temp1 0) (> temp2 100))))

;Given 2 int values, return true if either of them is in the range 10..20 inclusive.
(defn in1020 [a b]
  (or (and (< a 21) (> a 9)) (and (> b 9) (< b 21))))


;We'll say that a number is "teen" if it is in the range 13..19 inclusive.
;Given 3 int values, return true if 1 or more of them are teen.
(defn has-teen [a b c]
  (or (and (>= a 13) (<= a 19))
    (and (>= b 13) (<= b 19))
    (and (>= c 13) (<= c 19))))


;We'll say that a number is "teen" if it is in the range 13..19 inclusive.
;Given 2 int values, return true if one or the other is teen, but not both.
(defn lone-teen [a b]
  (and (or (and (>= a 13) (<= a 19)) (>= b 13) (<= b 19))
      (not (and (>= a 13) (<= a 19) (>= b 13) (<= b 19)))))


;Given a string, if the string "del" appears starting at index 1,
;return a string where that "del" has been deleted. Otherwise, return the
;string unchanged.
(defn del-del [str-test]
  (if (and (> (count str-test) 3) (= (subs str-test 1 4) "del"))
    (str (subs str-test 0 1) (subs str-test 4))
    str-test))


;Return true if the given string begins with "mix", except the 'm'
;can be anything, so "pix", "9ix" .. all count.
(defn mix-start [str-test]
  (and (>= (count str-test) 3) (= (subs str-test 1, 3) "ix")))


;Given a string, return a string made of the first 2 chars (if present),
;however include first char only if it is 'o' and include the second only
;if it is 'z', so "ozymandias" yields "oz".
(defn start-oz [str-test]
  (if (>= (count str-test) 2)
    (let [o (subs str-test 0 1)
          z (subs str-test 1 2)]
      (if (and (= o "o") (= z "z"))
        (str o z)
        (if (= o "o")
          (str o)
          (if (= z "z")
            (str z)
            (str "")))))
    (if (= str-test "o")
      str-test
      "")))



;Given three int values, a b c, return the largest.
(defn int-max [a b c]
  (max a b c))


;Given 2 int values, return whichever value is nearest to the value 10,
;or return 0 in the event of a tie. Note that Math.abs(n) returns the
;absolute value of a number.
(defn close10 [a b]
  (if (= (abs (- 10 a)) (abs (- 10 b)))
    0
    (if ( > (abs (- 10 a)) (abs (- 10 b)))
      b
      a)))


;Given 2 int values, return true if they are both in the range 30..40
;'inclusive, or they are both in the range 40..50 inclusive.
(defn in3050 [a b]
  (or (and (>= a 30) (<= a 40) (>= b 30) (<= b 40))
      (and (>= a 40) (<= a 50) (>= b 40) (<= b 50))))


;Given 2 positive int values, return the larger value that is in the
;range 10..20 inclusive, or return 0 if neither is in that range.
(defn max1020 [a b]
  (if (and (>= a 10) (<= a 20) (>= b 10) (<= b 20))
    (if (> a b)
      a
      b)
    (if (and (>= a 10) (<= a 20))
      a
      (if (and (>= b 10) (<= b 20))
        b
        0))))


;Return true if the given string contains between 1 and 3 'e' chars.
(defn string-e [str-test]
  (if (.contains str-test "e")
    (<= (get (frequencies str-test) \e) 3)
    false))

;Given two non-negative int values, return true if they have the same last
;digit, such as with 27 and 57. Note that the % "mod" operator computes
;remainders, so 17 % 10 is 7.
(defn last-digit [a b]
  (= (mod a 10) (mod b 10)))


;Given a string, return a new string where the last 3 chars are now in upper
;case. If the string has less than 3 chars, uppercase whatever is there. Note
;that str-test.toUpperCase() returns the uppercase version of a string.
(defn end-up [str-test]
  (if (< (count str-test) 4)
    (s/upper-case str-test)
    (let [last3 (subs str-test (- (count str-test) 3))]
        (str (subs str-test 0 (- (count str-test) 3)) (s/upper-case last3)))))

;Given a non-empty string and an int N, return the string made starting with
;char 0, and then every Nth char of the string. So if N is 3, use char 0, 3,
;6, ... and so on. N is 1 or more.
(defn every-nth [str-test n]
  (for [i (range 0 (count str-test) n)]
       (str (subs str-test i (+ i 1)))))


;Given a string and a non-negative int n, return a larger string that is
;n copies of the original string.
(defn string-times [str-test cnt]
;  (def buffer str-test)
  (for [x (range 0 cnt)]
    (str str-test)))



(defn explode [str-test]
  (if (< (count str-test) 2)
    (str str-test)
    (for [i (range 0 (count str-test))]
         (str (nth str-test i)  " "))))
