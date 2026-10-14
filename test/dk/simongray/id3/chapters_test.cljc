(ns dk.simongray.id3.chapters-test
  "CHAP and CTOC frames, read and written, the order of the chapters, and
  tables of contents built to hurt a reader."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.id3 :as id3]
            [dk.simongray.id3.chapters :as chapters]
            [dk.simongray.id3.test-support
             :refer [ascii apic be bs chap ctoc ctoc-of failure frame jpeg
                     nested-chaps tag text]]))

(def sample
  "A tag of chapters, two in a table of contents, out of order, and one
  that it doesn't list."
  (bs (tag 3
           (ctoc 3 "toc" ["ch1" "ch0"])
           (chap 3 "ch0" 0 60000 (text 3 "TIT2" "Intro"))
           (chap 3 "ch1" 60000 150500
                 (text 3 "TIT2" "The interview")
                 (frame 3 "WXXX" (concat [0 0] (ascii "https://example.org")))
                 (apic 3 "image/jpeg" 0 jpeg))
           (chap 3 "ch2" 150500 160000 (text 3 "TIT2" "Hidden")))))

(deftest frames
  (let [[toc ch0 ch1] (:frames (id3/read sample))]
    (is (= {:id "CTOC" :element-id "toc" :top-level? true :ordered? true
            :children ["ch1" "ch0"] :frames []}
           toc))
    (is (= {:id "CHAP" :element-id "ch0" :start-time 0 :end-time 60000
            :frames [{:id "TIT2" :encoding :latin1 :text "Intro"}]}
           ch0))
    (is (= ["TIT2" "WXXX" "APIC"] (map :id (:frames ch1)))
        "the frames inside a chapter"))
  (testing "the offsets, when a chapter gives them"
    (let [xs (frame 4 "CHAP" (concat (ascii "c") [0] (be 4 0) (be 4 1000)
                                     (be 4 417) (be 4 0xFFFFFFFF)))]
      (is (= {:id "CHAP" :element-id "c" :start-time 0 :end-time 1000
              :start-offset 417 :frames []}
             (first (:frames (id3/read (bs (tag 4 xs))))))))))

(deftest order
  (let [frames (:frames (id3/read sample))]
    (is (= ["ch1" "ch0"] (chapters/listed frames)))
    (is (= ["ch1" "ch0" "ch2"] (map :element-id (chapters/ordered frames)))
        "in the order of the table, and what it leaves out by start, last"))
  (testing "a table that lists another"
    (let [part   (fn [id children]
                   (frame 3 "CTOC" (concat (ascii id) [0 1 (count children)]
                                           (mapcat #(concat (ascii %) [0])
                                                   children))))
          frames (:frames (id3/read (bs (tag 3
                                             (ctoc 3 "top" ["part2" "part1"])
                                             (part "part1" ["a"])
                                             (part "part2" ["b" "c"])
                                             (chap 3 "a" 0 1)
                                             (chap 3 "b" 1 2)
                                             (chap 3 "c" 2 3)))))]
      (is (= ["b" "c" "a"] (chapters/listed frames)))))
  (testing "no table"
    (let [frames (:frames (id3/read (bs (tag 3 (chap 3 "b" 1000 2000)
                                             (chap 3 "a" 0 1000)))))]
      (is (nil? (chapters/listed frames)))
      (is (= ["a" "b"] (map :element-id (chapters/ordered frames)))
          "by start"))))

(deftest chapter-end-quirk
  (let [xs     (bs (tag 3 (chap 3 "c" 216000 190041)))
        t      (id3/read xs)
        strict (id3/read xs {:except #{:id3/chapter-end}})]
    (is (= {:id "CHAP" :element-id "c" :start-time 216000 :frames []}
           (first (:frames t))))
    (is (= [[:id3/chapter-end "c"]]
           (map (juxt :id :element-id) (id3/report t))))
    (is (= 190041 (:end-time (first (:frames strict))))
        "without the quirk, the end as it is")))

(deftest hurtful-tables
  (testing "chapters inside chapters inside chapters, 2000 deep"
    (let [t (id3/read (bs (tag 3 (nested-chaps 2000))))]
      (is (= ["CHAP"] (map :id (:frames (first (:frames t)))))
          "a chapter inside a chapter is kept as it is")
      (is (contains? (first (:frames (first (:frames t)))) :data))))
  (testing "a chain of 20000 tables of contents, each listing the next"
    (let [link   #(ctoc-of (str "t" %) [(str "t" (inc %))] (zero? %))
          tables (into [] (mapcat link) (range 20000))
          frames (:frames (id3/read (bs (tag 3 tables (nested-chaps 1)))
                                    {:max-frames 30000}))]
      (is (= ["t20000"] (chapters/listed frames)))
      (is (= 1 (count (chapters/ordered frames))))))
  (testing "tables that list the same tables, 2^40 ways through them"
    (let [after  #(str "a" (inc %))
          level  (fn [i]
                   (concat (ctoc-of (str "a" i) [(str "b" i) (str "c" i)]
                                    (zero? i))
                           (ctoc-of (str "b" i) [(after i)] false)
                           (ctoc-of (str "c" i) [(after i)] false)))
          tables (into [] (mapcat level) (range 40))
          frames (:frames (id3/read (bs (tag 3 tables))))]
      (is (= ["a40"] (chapters/listed frames))))))

(deftest writing
  (let [chaps  [{:id         "CHAP"
                 :element-id "b"
                 :start-time 5000
                 :end-time   9000
                 :frames     [{:id "TIT2" :text "Second"}]}
                {:id "CHAP" :element-id "a" :start-time 0 :end-time 5000}]
        frames (:frames (id3/read (id3/write {:frames (into [(chapters/toc
                                                              chaps)]
                                                            chaps)})))]
    (is (= {:id "CTOC" :element-id "toc" :top-level? true :ordered? true
            :children ["b" "a"] :frames []}
           (first frames)))
    (is (= ["b" "a"] (map :element-id (chapters/ordered frames)))
        "in the order of the table")
    (is (= [[{:id "TIT2" :encoding :latin1 :text "Second"}] []]
           (map :frames (rest frames)))))
  (is (= "toc2" (:element-id (chapters/toc [{:id "CHAP" :element-id "toc"}])))
      "an element id that no chapter has")
  (testing "a chapter without an end ends at its start, which reads back as
            no end"
    (let [t (id3/read (id3/write {:frames [{:id         "CHAP"
                                            :element-id "c"
                                            :start-time 1000}]}))]
      (is (= {:id "CHAP" :element-id "c" :start-time 1000 :frames []}
             (first (:frames t))))
      (is (= [:id3/chapter-end] (map :id (id3/report t))))))
  (testing "what can't be written"
    (doseq [[why frame] [["a table of no chapters"
                          {:id "CTOC" :element-id "t" :children []}]
                         ["a chapter inside a chapter"
                          {:id         "CHAP"
                           :element-id "a"
                           :start-time 0
                           :frames     [{:id         "CHAP"
                                         :element-id "b"
                                         :start-time 0}]}]
                         ["a time that isn't whole"
                          {:id "CHAP" :element-id "a" :start-time 0.5}]
                         ["a time past 32 bits"
                          {:id "CHAP" :element-id "a" :start-time 0x100000000}]
                         ["an element id that ISO-8859-1 can't hold"
                          {:id "CHAP" :element-id "Ł" :start-time 0}]]]
      (is (= :dk.simongray.id3/unwritable
             (:type (failure #(id3/write {:frames [frame]}))))
          why))))
