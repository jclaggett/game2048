(ns game2048.core-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [game2048.core :refer :all]))

(def gen-idx (gen/choose 0 15))

(defspec left-works
  10
  (prop/for-all [idx gen-idx]
                (or (nil? (left idx)) (= (left idx) (dec idx)))))

(defspec right-works
  10
  (prop/for-all [idx gen-idx]
                (or (nil? (right idx)) (= (right idx) (inc idx)))))

(defspec up-works
  10
  (prop/for-all [idx gen-idx]
                (or (nil? (up idx)) (= (up idx) (- idx 4)))))

(defspec down-works
  10
  (prop/for-all [idx gen-idx]
                (or (nil? (down idx)) (= (down idx) (+ idx 4)))))
