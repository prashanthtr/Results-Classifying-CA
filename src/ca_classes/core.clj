(ns ca-classes.core
  (:require [clojure.math.combinatorics :as comb])
  (:gen-class)
  )

;; rule is an list '(0 0 0 0 0 0 0 0)
(defn caRule [rule]
  ;;(println "carule" rule)
  (let [
        ruleExpansion (reverse (comb/selections '(0 1) 3))
        caR     (reduce
                 merge
                 (map
                  (fn [r RE]
                    {RE r}
                    )
                  rule ruleExpansion)
                 )
        ]
    ;;(println ruleExpansion)
    ;;(println caR)
    caR
    )
  )

;;; rule: { '(0 0 0) 0, '(0 0 1) 1, etc etc}
;; curState list of values '(0 1 0)
(defn nextState [rule curState]

  ;;(println rule)
  (let [
        cnt (count curState)
        nState (map-indexed
                (fn [ind c]

                  (cond
                    (= ind 0) (get rule (list (nth curState (- cnt 1))
                                              c
                                              (nth curState (+ ind 1))
                                              ))
                    (= ind (- (count curState) 1))
                    (get rule (list (nth curState (- ind 1))
                                    c
                                    (nth curState 0)
                                    ))
                    :else (get rule (list (nth curState (- ind 1))
                                          c
                                          (nth curState (+ ind 1))
                                          ))
                    )

                  )
                curState)
        ]
    ;;(println nState)
    nState
    )

  )



(defn isFixed [initState rule]
  (cond
    (= initState (nextState rule initState)) true
    :else false
    )
  )


;; part of a periodic rule
(defn isPeriodic [initState state rule memory cnt & args]

  ;;(println args)
  ;;(println initState state memory)
  (cond
    (>= cnt (Math/pow 2 (count initState))) nil
    (= (count (distinct memory)) (count memory) ) ;;initially true
    (isPeriodic
     initState
     (nextState rule state)
     rule
     (concat memory (list state))
     (+ cnt 1)
     (first args)
     )
    (< (count (distinct memory)) (count memory))
    (cond
      (= (first memory) (last memory)) (cond
                                         (= (first args) 'periodic)
                                         (do
                                           ;;(println "here")
                                           memory
                                           )
                                         :else
                                         (do
                                           ;;(println "heresay")
                                           (- (count memory) 1)
                                           )
                                         )
      :else nil
      )
    :else false
    )
  )


;; takes a memory of an init state, and gives the transient number of
;; that state

(defn findTransientNumber [memory ind]

  (cond
    (>= ind (count memory)) nil ;;no period after transient
    (= (nth memory ind) (last memory)) ind
    :else (findTransientNumber memory (+ ind 1))
    )

  )

;; part of a periodic rule
(defn isTransient [initState state rule memory cnt & args]

  ;;(println initState state memory)
  (cond
    (>= cnt (Math/pow 2 (count initState))) nil
    (= (count (distinct memory)) (count memory) ) ;;initially true
    (isTransient
     initState
     (nextState rule state)
     rule
     (concat memory (list state))
     (+ cnt 1)
     (first args)
     )
    (< (count (distinct memory)) (count memory))
    (cond
      (= (first memory) (last memory)) nil ;;not a transient
      :else (cond
              (= (first args) 'transient) (do
                                            ;;(println args)
                                            memory
                                            )
              :else (findTransientNumber memory 0)
              )
      )
    :else false
    )
  )

;; part of a sequence
(defn partSequence [initState state rule memory cnt]

  (cond
    (>= cnt (Math/pow 2 (count initState))) memory
    (= (count (distinct memory)) (count memory) ) ;;initially true
    (partSequence
     initState
     (nextState rule state)
     rule
     (concat memory (list state))
     (+ cnt 1))
    (< (count (distinct memory)) (count memory)) memory ;;cycle
    :else nil
    )
  )



(defn countOnes [state]
  (reduce + state)
  )

(defn sLeft [state n]
  (cond
    (= n 0) state
    :else
    (let [
          f (first state)
          shifted  (map-indexed
                    (fn [ind el]
                      (cond
                        (< ind (- (count state) 1)) (nth state (+ ind 1))
                        :else f
                        )
                      )
                    state)
          ]
      (sLeft shifted (- n 1))
      )
    )
  )

;; belongs to a class if it matches one of the members in the class
;; within n steps
(defn belongsToClass [state cl]
  (let [
        cnt (count state)
        shifts (range cnt)
        shiftCheck     (filter identity
                               (map
                                (fn [n]
                                  (cond
                                    (= (sLeft state n) cl) true
                                    :else nil
                                    )
                                  )
                                shifts)
                               )
        ]

    (cond
      (empty? shiftCheck) nil
      :else true
      )
    )
  )


(defn checkExistingClass [state classList]

  (let [
        classId (filter identity
                        (map
                         (fn [cl]
                           (belongsToClass state cl)
                           )
                         classList))
        ]
    (cond
      (empty? classId) (do
                         ;;(println "new class created")
                         (concat classList (list state))
                         )
      :else (do
              ;;(println "belongs to old class")
              classList
              )
      )
    )

  )

;;generates shift invariant classes based on operational closure of CA
(defn genClasses [states classes]

  (cond
    (empty? states) classes
    :else (genClasses (rest states) (checkExistingClass (first states) classes))
    )
  )

(defn expandClassGen [states]

  (let [
        classes (genClasses states '())
        ]
    (map
     (fn [c]
       (let [
             classList '()
             ]
         (filter identity
                 (map
                  (fn [s]
                    (cond
                      (= (belongsToClass s c) true) (concat classList s)
                      :else nil
                      )
                    )
                  states))
         )
       )
     classes)
    )
  )

;; returns the classes that it is member of
(defn checkClassMember [state]

  (let [
        n (count state)
        states (comb/selections [0 1] n)
        classes (expandClassGen states)
        ]
    ;;(println classes)
    (first (filter identity (map
                      (fn [c]
                        (cond
                          (= (belongsToClass state (first c)) true) c
                          :else nil
                          )
                        )
                      classes)))
    )
  )


(def join (fn [x & args]
            (cond
              (> (count args) 0) (#(clojure.string/join (first args) x))
              :else (#(clojure.string/join x))
              )
            )
  )

;; generates the table output form
(defn characterizeState [state rule filename]
  ;;(println "characterizing")
  (let [
        caClass (checkClassMember state)
        seq (partSequence state state rule '() 0)
        trNo (isTransient state state rule '() 0)
        phNo (isPeriodic state state rule '() 0)
        phase (cond
                (= nil phNo) "nil"
                :else phNo
                )
        transientNumber (cond
                          (= nil trNo) "nil"
                          :else trNo
                          )
        ]
    ;;(println "writing")
    (println           (str "|" (join state)
                            "|" (join (map join caClass) ",")
                            "|" (join (map join seq) "-->")
                            "|" phase
                            "|" transientNumber "|\n" ))
    (spit filename
          (str "|" (join state)
               "|" (join (map join caClass) ",")
               "|" (join (map join seq) "-->")
               "|" phase
               "|" transientNumber "|\n" )
          :append true
          )
    true
    )
  )

(defn getIntRep [string]
  (let [
        arsplit  (#(clojure.string/split string #""))
        intRule (map (fn [s]
                       ;;(println s)
                       (Integer. s)
                       )
                     arsplit)
        ]
    ;;(println intRule)
    intRule
    )
  )

;; n -> number
;; rule -> wolfram representation of rule
;; output characterization of all states
(defn tableGen [n rule]
  (let [
        filename (str n "cellCARule" rule ".org")
        states (comb/selections [0 1] (#(Integer/parseInt n)))
        rule (getIntRep rule)
        carule (caRule rule)
        ]
    (spit filename "")
    (spit filename
          (str "#+TABLE: Characterization of a " n " cell CA with Rule " (join rule) "\n")
          :append true
          )
    (spit filename "#+ATTR_LATEX: :align |c|c|c|c|c|\n" :append true)
    (spit filename "|state|Shift invariant class|Sequence|Phase|Transient|\n" :append true)

    (println "writing to" filename)
    (doall (map
         (fn [s]
           ;;(println "mapping" s)
           (characterizeState s carule filename)
           )
         states))
    (println "done writing the file" filename)
    )
  )

(defn -main [n rule]
  (tableGen n rule)
  )


;; we want to se if seq2 is contained in seq1
(defn longestSubseq [seq1 seq2]
  ;;(println seq1 seq2)
  (cond
    (or
     (and (empty? seq2) (empty? seq1))
     (and (empty? seq2) (not (empty? seq1)))
     ) true
    (and (empty? seq1) (not (empty? seq2))) nil
    (= (first seq1) (first seq2))
    (longestSubseq (rest seq1) (rest seq2) )
    :else (longestSubseq (rest seq1) seq2)
    )

  )

 ;; (concat seq (list (first seq2)))

;; a state is in the basin of the initial state when it gets to the
;; initial state in a certain number of iterations,. The maximum
;; numnber of iterations is 2^n.

;;generates the sequences froom a state and rule

(defn basins [state rule sequences cnt]

  ;;(println cnt)
  ;;(println initState prevState state)
  (cond
    (>= cnt (Math/pow 2 (count state))) '()  ;;avoid infinite
    ;;recursion, this should only be the number of classes actually
    (= (type (isPeriodic state state rule '() 0 'periodic))  clojure.lang.LazySeq)
    (do
      ;;(println "periodic")
      (concat
       sequences
       (isPeriodic state state rule '() 0 'periodic))
      )

    (= (type (isTransient state state rule '() 0 'transient)) clojure.lang.LazySeq)
    (concat
     sequences
     (isTransient state state rule '() 0 'transient)
     )

    ;; then its a periodic state
    (isFixed state rule) (concat sequences (list state)) ;;no more reachable states
    ;; then it is a fixed sate
    :else
    (basins
     (nextState rule state) ; make the next state current state
     rule
     (concat sequences (list state)) ;;add transients to sequence
     (inc cnt)
     )

    )
  )


;; checks if it is a part of the sequence as well as returns a longer
;; subsequence if it is a part of that.

(defn checkPartSequence [state rule sequences & args]

  ;;(println "in check part seq" state)
  (cond
    (empty? sequences) (do
                         ;;trivial case, build up a sequence
                         (list (basins state rule '() 0))
                         )
    :else
    (let [
          ;;checks if state is fixed, is a part of bigger sequence, or
          ;;is part of a sequence that is part of bigger sequence
          partof  (map-indexed
                   (fn [ind seq]
                     (cond
                       (= (isFixed state rule) true) (do
                                                       ;;(println "fixed")
                                                       (basins state rule '() 0)
                                                       )
                       (and
                        (=  (longestSubseq (basins state rule '() 0) seq ) true)
                        (>  (count (basins state rule '() 0))
                            (count seq)
                            )
                        )
                       (do
                         ;;(println "found longer seq" state seq)
                         (basins state rule '() 0) ;;longer seq
                         )
                       (.contains seq state) (do
                                               ;;(println "part of longer sequence")
                                               seq)
                       ;;same seq
                       :else nil ;;nothing
                       )
                     )
                   sequences)
          partofRefined (filter identity partof)
          ]
      ;;contains the current update sequence
      ;;(println partof)
      (cond
        (empty? partofRefined) (do
                          ;;(println "creating sequence")
                          (concat
                           sequences
                           (basins state rule '() 0)
                           )
                          ;;create a new sequence wih state
                          )
        (= (count sequences) (count partofRefined)) (do
                                               ;;(println "adding fixed point")
                                       (concat
                                        sequences
                                        (distinct partofRefined)
                                        )
                                       )
        :else (map
               (fn [p s]
                 (cond
                   (= nil p) s
                   (= p s) s
                   :else p
                   )
                 )
               partof sequences) ;;returns the sequence it is a part of
        )
      )
    )
  )


;; need to check if output correct
(defn sequenceNumbers [states rule sequences]

  ;;(println (first states) sequences)
  (cond
    (= nil (first states)) sequences
    :else (sequenceNumbers (rest states) rule
                           (checkPartSequence (first states) rule sequences)
                           )
                           ;; directly return the sequences
                           ;; (concat
                            ;;  sequences)

    )

  )

(defn findSequence [n state rule]

  (let [
        sequences (sequenceNumbers (comb/selections [0 1] n)
                                   rule
                                   '()
                                   )
        ]
    ;;(println sequences)
    (filter identity
     (map-indexed
      (fn [ind seq]
        (cond
          (.contains seq state) (str "S" ind " " (reverse (into () seq)))
          :else nil
          )
        )
      sequences))
    )
  )

(defn findClass [state]

  (let [
        n (count state)
        classes (expandClassGen (comb/selections [0 1] n))
        ]
    (filter identity
            (map-indexed
             (fn [ind c]
               (cond
                 (.contains c state) (str "C" ind " " (reverse (into () state)))
                 :else nil
                 )
               )
             classes)
            )
    )
  )

(defn classToSeq [state rule]
  (let [
        classId (findClass state)
        n (count state)
        seqId (findSequence n state rule)
        ]
    (list (first classId) " occurs in " (first seqId) )

    )
  )


(defn characterizeClassSequences [n rule]
  (let [
        states (comb/selections [0 1] n)
        rule (caRule rule)
        ]
    ; for every state, get the class and the sequence it belongs to
    (map
     (fn [s]
       (classToSeq s rule)
       )
     states)
    )
  )
