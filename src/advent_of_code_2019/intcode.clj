(ns advent-of-code-2019.intcode
  (:require [clojure.test :refer :all]))


;;;
;;; PARAMETER MODES
;;;

(defn- get-referenced-value [position memory]
  (memory (memory position)))

(defn- get-immediate-value [position memory]
  (memory position))

(def parameter-modes
  {0 :position
   1 :immediate})

(defn- read-parameter [position memory parameter-mode]
  (case parameter-mode
    :position (get-referenced-value position memory)
    :immediate (get-immediate-value position memory)
    (get-referenced-value position memory)))


;;;
;;; INSTRUCTIONS
;;;

(defn add [{position :instruction-pointer, memory :memory} parameter-modes]
  (let [term1 (read-parameter (+ 1 position) memory (get parameter-modes 0))
        term2 (read-parameter (+ 2 position) memory (get parameter-modes 1))
        result-position (get-immediate-value (+ 3 position) memory)]
    {:instruction-pointer (+ 4 position)
     :memory (assoc memory result-position (+ term1 term2))}))

(defn multiply [{position :instruction-pointer, memory :memory} parameter-modes]
  (let [factor1 (read-parameter (+ 1 position) memory (get parameter-modes 0))
        factor2 (read-parameter (+ 2 position) memory (get parameter-modes 1))
        result-position (get-immediate-value (+ 3 position) memory)]
    {:instruction-pointer (+ 4 position)
     :memory (assoc memory result-position (* factor1 factor2))}))

(defn store-value [{pos :instruction-pointer, memory :memory} parameter-modes]
  (let [address (get-immediate-value (+ 1 pos) memory)
        input (read-string (read-line))]
    {:instruction-pointer (+ 2 pos)
     :memory (assoc memory address input)}))

(defn output-value [{pos :instruction-pointer, memory :memory} parameter-modes]
  (let [value (read-parameter (+ 1 pos) memory (get parameter-modes 0))]
    (println value)
    {:instruction-pointer (+ 2 pos)
     :memory memory}))

(defn- jump-if-true [{pos :instruction-pointer, memory :memory} parameter-modes]
  (let [predicate (read-parameter (+ 1 pos) memory (get parameter-modes 0))
        address (read-parameter (+ 2 pos) memory (get parameter-modes 1))]
    {:memory memory
     :instruction-pointer (if (= 0 predicate) (+ 3 pos) address)}))

(defn- jump-if-false [{pos :instruction-pointer, mem :memory} parameter-modes]
  (let [predicate (read-parameter (+ 1 pos) mem (get parameter-modes 0))
        address (read-parameter (+ 2 pos) mem (get parameter-modes 1))]
    {:memory mem
     :instruction-pointer (if (= 0 predicate) address (+ 3 pos))}))

(defn- less-than [{pos :instruction-pointer, memory :memory} parameter-modes]
  (let [frst (read-parameter (+ 1 pos) memory (get parameter-modes 0))
        scnd (read-parameter (+ 2 pos) memory (get parameter-modes 1))
        result-position (get-immediate-value (+ 3 pos) memory)]
    {:memory (assoc memory result-position (if (< frst scnd) 1 0))
     :instruction-pointer (+ 4 pos)}))

(defn- equals [{pos :instruction-pointer, memory :memory} parameter-modes]
  (let [frst (read-parameter (+ 1 pos) memory (get parameter-modes 0))
        scnd (read-parameter (+ 2 pos) memory (get parameter-modes 1))
        result-position (get-immediate-value (+ 3 pos) memory)]
    {:memory (assoc memory result-position (if (= frst scnd) 1 0))
     :instruction-pointer (+ 4 pos)}))

(defn stop [{memory :memory} parameter-modes]
  {:instruction-pointer nil
   :memory memory})

(def operations
  {1 add
   2 multiply
   3 store-value
   4 output-value
   5 jump-if-true
   6 jump-if-false
   7 less-than
   8 equals
   99 stop})


;;;
;;; PROGRAM RUNNER
;;;

(defn- parameter-modes-vector [parameter-modes-string]
  (mapv #(-> % (str) (read-string) (parameter-modes))
        (reverse (str parameter-modes-string))))

(defn- split-opcode-and-parameter-modes [opcode-with-parameter-modes]
  (let [opcode (rem opcode-with-parameter-modes 100)]
    {:opcode opcode
     :parameter-modes (parameter-modes-vector
                       (/ (- opcode-with-parameter-modes opcode) 100))}))

(defn- execute-instruction [program-state]
  (let [{position :instruction-pointer, memory :memory} program-state
        {parameter-modes :parameter-modes
         opcode :opcode} (split-opcode-and-parameter-modes (memory position))
        operation (operations opcode)]
    (if (nil? operation)
      (throw (ex-info "Unknown operation" program-state))
      (operation program-state parameter-modes))))

(defn- run-until-stop-instruction [program-state]
  (let [new-state (execute-instruction program-state)]
    (if (nil? (new-state :instruction-pointer))
      new-state
      (recur new-state))))

(defn run-program [program]
  (run-until-stop-instruction {:instruction-pointer 0, :memory program}))

(deftest examples
  (is (= (:memory (run-program [1 9 10 3 2 3 11 0 99 30 40 50]))
         [3500 9 10 70 2 3 11 0 99 30 40 50]))
  (is (= (:memory (run-program [1 9 10 3 2 3 11 0 99 30 40 50]))
         [3500 9 10 70 2 3 11 0 99 30 40 50]))
  (is (= (:memory (run-program [1 0 0 0 99]))
         [2 0 0 0 99]))
  (is (= (:memory (run-program [2 3 0 3 99]))
         [2 3 0 6 99]))
  (is (= (:memory (run-program [2 4 4 5 99 0]))
         [2 4 4 5 99 9801]))
  (is (= (:memory (run-program [1 1 1 4 99 5 6 0 99]))
         [30 1 1 4 2 5 6 0 99])))
