(ns advent-of-code-2019.day-4
  (:require [clojure.test :refer :all]))

;; You arrive at the Venus fuel depot only to discover it's protected by a
;; password. The Elves had written the password on a sticky note, but someone
;; threw it out.

;; However, they do remember a few key facts about the password:
;; - It is a six-digit number.
;; - The value is within the range given in your puzzle input.
;; - Two adjacent digits are the same (like 22 in 122345).
;; - Going from left to right, the digits never decrease; they only ever
;;   increase or stay the same (like 111123 or 135679).

;; Other than the range rule, the following are true:
;; - 111111 meets these criteria (double 11, never decreases).
;; - 223450 does not meet these criteria (decreasing pair of digits 50).
;; - 123789 does not meet these criteria (no double).

;; How many different passwords within the range given in your puzzle input meet
;; these criteria?

;; Your puzzle input is 138307-654504.

(def lowest 138307)
(def highest 654504)

(defn digits-not-decreasing? [password]
  (apply <= (map int password)))

(defn split-string-on-repeating-chars [s]
  (reduce (fn [acc ch]
            (let [[group & groups] acc]
              (if (= ch (first group))
                (concat [(conj group ch)] groups)
                (concat [[ch]] acc))))
          []
          s))

(defn exactly-two-adjacent-digits? [password]
  (some #(= 2 (count %)) (split-string-on-repeating-chars password)))

(deftest adjacent-digit-examples
  (is (not (exactly-two-adjacent-digits? "111111")))
  (is (exactly-two-adjacent-digits? "124524452")))

(defn valid-password? [password]
  (and (exactly-two-adjacent-digits? password)
       (digits-not-decreasing? password)))

(deftest valid-passwords
  (is (not (valid-password? "111111")))
  (is (not (valid-password? "223450")))
  (is (not (valid-password? "123789"))))

(defn valid-passwords-in-range [low high]
  (for [password (range low (inc high))
        :when (valid-password? (str password))]
    password))


;; An Elf just remembered one more important detail: the two adjacent matching
;; digits are not part of a larger group of matching digits.

;; Given this additional criterion, but still ignoring the range rule, the
;; following are now true:
;; - 112233 meets these criteria because the digits never decrease and all
;;   repeated digits are exactly two digits long.
;; - 123444 no longer meets the criteria
;;   (the repeated 44 is part of a larger group of 444).
;; - 111122 meets the criteria
;;   (even though 1 is repeated more than twice, it still contains a double 22).

;; How many different passwords within the range given in your puzzle input meet
;; all of the criteria?

(deftest new-password-criteria
  (is (valid-password? "112233"))
  (is (not (valid-password? "123444")))
  (is (valid-password? "111122")))
