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

(defn replaceSequences [seq newSeq rule]

  (cond
    (empty? seq) (list newSeq)
    :else   (let [
                  needNewSeq (filter identity
                                     (map
                                      (fn [s]
                                        (cond
                                          (and
                                           (> (count newSeq)  (count s))
                                           (not (isFixed (first s) rule))
                                           ;;because fixed points are
                                           ;;always parts of
                                           ;;converging sequences
                                           (longestSubseq newSeq s)
                                           )
                                          newSeq
                                          :else nil
                                          )
                                        )
                                      seq))
                  ]
              (cond
                (empty? needNewSeq) (concat seq (list newSeq)) ;;not
                ;;in list
                ;;in list but need a longer subseq
                :else (map
                       (fn [s]
                         (cond
                           (and
                            (> (count newSeq)  (count s))
                            (longestSubseq newSeq s)
                            ) newSeq
                           :else s
                           )
                         )
                       seq)
                )
              )
    )
  )

;; builds a new sequence
;; or returns an existing sequence
;; or creates a longer sequence
;; or creates a new sequence and adds it

(defn buildSequences [state rule sequences]

  (cond

    ;; if it is a fixed point
    (isFixed state rule) (concat sequences (list (list state state)))
    ;;periodic point
    (isPeriodic state state rule '() 0)
    (do
      ;;(println "periodic")
      (concat
       sequences
       (list (isPeriodic state state rule '() 0 'periodic))
       ) ;;returns the period sequence

      )

    ;; ;;transient point
    (isTransient state state rule '() 0)
    ;;replaces only the transient sequences
    (replaceSequences
     sequences
     (isTransient state state rule '() 0 'transient)
     rule
     )

    ;; ;;replace with a longer sequence
    ;; (do
    ;;   ;;(println "transient")
    ;;   (replaceSequences
    ;;    sequences
    ;;    (isTransient state state rule '() 0 'transient)
    ;;    )
    ;;   )

    :else nil ;;there is no point like that
    )

  )

;; need to check if output correct
(defn sequenceNumbers [states rule sequences]

  ;;(println (first states) sequences)
  (cond
    (= nil (first states)) sequences
    :else (sequenceNumbers (rest states) rule
                           (buildSequences (first states) rule sequences)
                           )
    ;;checkPartSequence
                           ;; directly return the sequences
                           ;; (concat
                            ;;  sequences)

    )

  )

(defn findSequence [state rule]

  (let [
        sequences (sequenceNumbers (comb/selections [0 1] (count state))
                                   rule
                                   '()
                                   )
        charcSeq (filter identity
                         (map-indexed
                          (fn [ind seq]
                            (cond
                              ;;distinguishing between transient and
                              ;;fixed points, both occuring in many sequences
                              (and
                               (not (isFixed state rule))
                               (.contains seq state))
                              (str "S" ind)
                              (and (isFixed state rule )
                                   (= state (first seq)) ;;seq is
                                   ;;twice the fixed point
                                   )
                              (str "S" ind)
                              :else nil
                              )
                            )
                          sequences))
        ]
    ;;(str "S" ind " " (reverse (into () seq)))
    ;;(println charcSeq)
    (cond
      (empty? charcSeq) nil
      (= (count charcSeq) 1) (first charcSeq)  ;; converging sequence
      ;;(isFixed state rule) (first charcSeq)
      :else (join charcSeq ",")
      )
    ;;(println sequences)

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
                 (.contains c state) (str "C" ind " ")
                 :else nil
                 )
               )
             classes)
            )
    )
  )

;;(reverse (into () state))

(defn classToSeq [state rule]
  (let [
        classId (findClass state)
        n (count state)
        seqId (findSequence state rule)
        ]
    (list (first classId) " --> " seqId)
    )
  )

;; (defn characterizeClassSequences [n rule]
;;   (let [
;;         states (comb/selections [0 1] n)
;;         rule (caRule rule)
;;         ]
;;     ; for every state, get the class and the sequence it belongs to
;;     (map
;;      (fn [s]
;;        (join (classToSeq s rule) " ")
;;        )
;;      states)
;;     )
;;   )

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
        classNo (findClass state)
        seqNo (findSequence state rule)
        ]
    ;;(println "writing")
    ;; (println           (str "|" (join state)
    ;;                         "|" (join (map join caClass) ",")
    ;;                         "|" (join (map join seq) "-->")
    ;;                         "|" phase
    ;;                         "|" transientNumber
    ;;                         "|" transientNumber
    ;;                         "|" (apply str classNo)
    ;;                         "|" seqNo
    ;;                         "|\n" ))
    (spit filename
          (str "|" (join state)
               "|" (join (map join caClass) ",")
               "|" (join (map join seq) "-->")
               "|" phase
               "|" transientNumber
               "|" (apply str classNo)
               "|" seqNo
               "\n" )
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
        filename (str n "cellCARule.org")
        states (comb/selections [0 1] n)
        ;;rule (getIntRep rule)
        carule (caRule rule)
        ]
    (spit filename "\n\n" :append true)
    (spit filename
          (str "#+TABLE: Characterization of a " n " cell CA with Rule " (join rule) "\n")
          :append true
          )
    (spit filename "#+ATTR_LATEX: :align |c|c|c|c|c|\n" :append true)
    (spit filename "|state|Shift invariant class|Sequence|Phase|Transient|classNo|SeqNo|\n" :append true)

    (println "writing" rule " to " filename)
    (doall (map
         (fn [s]
           ;;(println "mapping" s)
           (characterizeState s carule filename)
           )
         states))
    (println "done writing" rule " to " filename)
    )
  )

(defn nCellRule [n rule]
  (tableGen n rule)
  )

(defn -main [nCell]

  (let [
        nC (#(Integer/parseInt nCell))
        ;;nR (#(Integer/parseInt nRule))
        rules (comb/selections [0 1] 8)
        ]
    ;;for each rule, each cell generate tables
    (doall
     (map
     (fn [r]
       (nCellRule nC r)
       )
     rules))
    )
  )
