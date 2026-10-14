(ns dk.simongray.id3.hostile-test
  "Tags built to cost a reader: tags with random bytes changed or cut
  short, and more frames, strings and tags than the limits allow."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [dk.simongray.id3 :as id3]
            [dk.simongray.id3.chapters :as chapters]
            [dk.simongray.id3.round-trip-test :as round-trip]
            [dk.simongray.id3.test-support :refer [ascii bs frame tag]]
            [dk.simongray.webcrypto.bytes :as bytes]))

(def mangled
  "A generator of the bytes of a written tag with random bytes changed, the
  unsynchronisation of the tag switched on, or the end cut off."
  (gen/let [t      round-trip/tag
            edits  (gen/vector (gen/tuple (gen/choose 0 100000)
                                          (gen/choose 0 255))
                               0 8)
            cut    (gen/one-of [(gen/return nil) (gen/choose 0 2000)])
            unsync gen/boolean]
    (let [b  (id3/write t)
          n  (bytes/length b)
          xs (cond-> (mapv #(bytes/at b %) (range n))
               unsync (update 5 bit-or 0x80))
          xs (reduce (fn [xs [i x]] (assoc xs (mod i n) x)) xs edits)]
      (bytes/of (cond->> xs
                  (and cut (< cut n)) (take cut))))))

(defspec mangled-tags-read-without-throwing 300
  (prop/for-all [b mangled]
    (let [tags (id3/read-all b)]
      (doseq [t tags]
        (id3/report t)
        (chapters/ordered (:frames t)))
      (id3/header b)
      (id3/read b)
      true)))

(deftest limits
  (testing "frames"
    (let [t (id3/read (bs (apply tag 4 (repeat 30 (frame 4 "PRIV" []))))
                      {:max-frames 20})]
      (is (= 20 (count (:frames t))))
      (is (= [[:id3/too-many-frames 20]]
             (map (juxt :id :limit) (id3/report t))))))
  (testing "frames inside chapters count"
    (let [chap (frame 4 "CHAP" (concat [0x61 0] (repeat 16 0)
                                       (apply concat
                                              (repeat 5 (frame 4 "PRIV" [])))))
          t    (id3/read (bs (tag 4 chap chap)) {:max-frames 8})]
      (is (= [5 1] (map (comp count :frames) (:frames t)))
          "the first chapter and its five, then the second and one")))
  (testing "strings"
    (let [xs (frame 4 "TPE1" (concat [0] (take 59 (cycle [0x41 0]))))
          t  (id3/read (bs (tag 4 xs xs)) {:max-strings 20})]
      (is (= [21 1] (map #(count (:values % [(:text %)])) (:frames t)))
          "the first of each frame, and 20 after them")
      (is (= [:id3/too-many-strings] (map :id (id3/report t))))))
  (testing "tags in a row"
    (let [ts (id3/read-all (bs (apply concat (repeat 5 (tag 4))))
                           {:max-tags 3})]
      (is (= 3 (count ts)))
      (is (= [:id3/second-tag :id3/too-many-tags]
             (map :id (id3/report (peek ts)))))))
  (is (= {:max-frames 10000 :max-strings 10000 :max-tags 16}
         id3/default-limits)))

(deftest the-same-diagnostic-once
  (let [xs (frame 4 "TIT2" (concat [1] (ascii "Hi")))
        t  (id3/read (bs (apply tag 4 (repeat 50 xs))))]
    (is (= [:id3/utf16-bom] (map :id (id3/report t))))))
