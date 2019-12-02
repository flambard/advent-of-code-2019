(ns advent-of-code-2019.day-2
  (:require [clojure.test :refer :all]))

(def program
  [1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 6 1 19 2 19 13 23 1 23 10 27 1 13 27 31 2
   31 10 35 1 35 9 39 1 39 13 43 1 13 43 47 1 47 13 51 1 13 51 55 1 5 55 59 2 10
   59 63 1 9 63 67 1 6 67 71 2 71 13 75 2 75 13 79 1 79 9 83 2 83 10 87 1 9 87
   91 1 6 91 95 1 95 10 99 1 99 13 103 1 13 103 107 2 13 107 111 1 111 9 115 2
   115 10 119 1 119 5 123 1 123 2 127 1 127 5 0 99 2 14 0 0])

(def desired-output 19690720)

(defn add [{position :instruction-pointer, memory :memory}]
  (let [term1  (memory (memory (+ 1 position)))
        term2  (memory (memory (+ 2 position)))
        result-position (memory (+ 3 position))]
    {:instruction-pointer (+ 4 position)
     :memory (assoc memory result-position (+ term1 term2))}))

(defn multiply [{position :instruction-pointer, memory :memory}]
  (let [factor1 (memory (memory (+ 1 position)))
        factor2 (memory (memory (+ 2 position)))
        result-position  (memory (+ 3 position))]
    {:instruction-pointer (+ 4 position)
     :memory (assoc memory result-position (* factor1 factor2))}))

(defn stop [{memory :memory}]
  {:instruction-pointer nil
   :memory memory})

(def operations
  {1 add
   2 multiply
   99 stop})

(defn execute-instruction [program-state]
  (let [{position :instruction-pointer, memory :memory} program-state
        opcode (memory position)
        operation (operations opcode)]
    (if (nil? operation)
      (throw (ex-info "Unknown operation" program-state))
      (let [new-state (operation program-state)]
        (if (nil? (new-state :instruction-pointer))
          new-state
          (recur new-state))))))

(defn run-program [program]
  (execute-instruction {:instruction-pointer 0
                        :memory program}))

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

(defn run-gravity-assist-program [noun verb]
  (let [modified-program (assoc program 1 noun 2 verb)]
    ((:memory (run-program modified-program)) 0)))

(defn run-modified-program []
  (run-gravity-assist-program 12 2))

(defn find-noun-and-verb-for-desired-output []
  (for [noun (range 0 100)
        verb (range 0 100)
        :when (= desired-output (run-gravity-assist-program noun verb))]
    {:noun noun :verb verb}))
