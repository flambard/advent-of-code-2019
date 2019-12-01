(ns advent-of-code-2019.day-1)

;;;
;;; PROBLEM 1
;;;

;; The Elves quickly load you into a spacecraft and prepare to launch.

;; At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper.
;; They haven't determined the amount of fuel required yet.

;; Fuel required to launch a given module is based on its mass. Specifically, to
;; find the fuel required for a module, take its mass, divide by three, round
;; down, and subtract 2.

;; For example:

;; - For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to
;; get 2.
;; - For a mass of 14, dividing by 3 and rounding down still yields 4, so the
;; fuel required is also 2.
;; - For a mass of 1969, the fuel required is 654.
;; - For a mass of 100756, the fuel required is 33583.
;; - The Fuel Counter-Upper needs to know the total fuel requirement. To find
;; it, individually calculate the fuel needed for the mass of each module
;; (your puzzle input), then add together all the fuel values.

;; What is the sum of the fuel requirements for all of the modules on your
;; spacecraft?

;; Puzzle input:
(def module-masses
  [148623 147663 67990 108663 62204 140999 123277 52459 143331 71135 76282 69509
   72977 120407 62278 136882 131667 146225 112216 108600 127267 149149 72977
   149639 101527 70059 124825 69539 141444 64138 71145 68178 134752 79431 126342
   134161 135424 95647 54507 104852 100164 118799 57387 93136 133359 144942
   89337 60441 131825 93943 98142 108306 55355 115813 83431 125883 101115 107938
   103484 61174 123502 73670 91619 136860 93268 149648 105328 53194 115351
   130953 85611 71134 141663 106590 56437 147797 98484 60851 121252 66898 56502
   103796 86497 121534 70914 122642 53151 131939 108394 128239 103490 122304
   113810 141469 79176 108002 91942 84400 101217 116287])

(defn fuel-required [mass]
  (- (quot mass 3) 2))

(assert (= (fuel-required 12) 2))
(assert (= (fuel-required 14) 2))
(assert (= (fuel-required 1969) 654))
(assert (= (fuel-required 100756) 33583))

(defn fuel-required-of-all-modules [module-masses]
  (reduce + (map fuel-required module-masses)))


;;;
;;; PROBLEM 2
;;;

;; During the second Go / No Go poll, the Elf in charge of the Rocket Equation
;; Double-Checker stops the launch sequence. Apparently, you forgot to include
;; additional fuel for the fuel you just added.

;; Fuel itself requires fuel just like a module - take its mass, divide by
;; three, round down, and subtract 2. However, that fuel also requires fuel, and
;; that fuel requires fuel, and so on. Any mass that would require negative fuel
;; should instead be treated as if it requires zero fuel; the remaining mass, if
;; any, is instead handled by wishing really hard, which has no mass and is
;; outside the scope of this calculation.

;; So, for each module mass, calculate its fuel and add it to the total. Then,
;; treat the fuel amount you just calculated as the input mass and repeat the
;; process, continuing until a fuel requirement is zero or negative.
;; For example:

;; A module of mass 14 requires 2 fuel. This fuel requires no further fuel (2
;; divided by 3 and rounded down is 0, which would call for a negative fuel), so
;; the total fuel required is still just 2.

;; At first, a module of mass 1969 requires 654 fuel. Then, this fuel requires
;; 216 more fuel (654 / 3 - 2). 216 then requires 70 more fuel, which requires
;; 21 fuel, which requires 5 fuel, which requires no further fuel. So, the total
;; fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966.

;; The fuel required by a module of mass 100756 and its fuel is:
;; 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346.

;; What is the sum of the fuel requirements for all of the modules on your
;; spacecraft when also taking into account the mass of the added fuel?
;; (Calculate the fuel requirements for each module separately, then add them
;; all up at the end.)

(defn total-fuel-required [mass]
  (let [fuel (fuel-required mass)]
    (if (>= 0 fuel)
      0
      (+ fuel (total-fuel-required fuel)))))

(assert (= (total-fuel-required 1969) 966))
(assert (= (total-fuel-required 100756) 50346))

(defn total-fuel-required-of-all-modules [module-masses]
  (reduce + (map total-fuel-required module-masses)))
