(ns dk.simongray.id3-test
  "The reading of tags, built here byte by byte: each version with its
  header, extended header, unsynchronisation and footer, tags cut short,
  tags in a row, and the quirks of tags with and without them."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.id3 :as id3]
            [dk.simongray.id3.test-support
             :refer [ascii apic bs frame frame2 jpeg syncsafe tag text
                     utf16le]]
            [dk.simongray.webcrypto.bytes :as bytes]))

(deftest headers
  (is (= {:version 4 :revision 0 :flags #{} :size 100 :end 110}
         (id3/header (bs (concat (ascii "ID3") [4 0 0] (syncsafe 100)))))
      "the size is that after the header, and the end that of the tag")
  (is (= {:version 4 :revision 0 :flags #{:footer} :size 100 :end 120}
         (id3/header (bs (concat (ascii "ID3") [4 0 0x10] (syncsafe 100)))))
      "after the footer")
  (is (= #{:unsynchronisation :extended-header :experimental}
         (:flags (id3/header (bs (concat (ascii "ID3") [3 0 0xE0]
                                         (syncsafe 0)))))))
  (is (= #{:unsynchronisation :compression}
         (:flags (id3/header (bs (concat (ascii "ID3") [2 0 0xC0]
                                         (syncsafe 0)))))))
  (testing "no header"
    (is (nil? (id3/header (bs (ascii "ID3")))) "too short")
    (is (nil? (id3/header (bs (concat (ascii "RIFF") (repeat 10 0))))))
    (is (nil? (id3/header (bs (concat (ascii "ID3") [5 0 0] (syncsafe 0)))))
        "a version that isn't 2.2, 2.3 or 2.4")
    (is (nil? (id3/header (bs (concat (ascii "ID3") [4 0 0] [0 0 0 0x80]))))
        "a size that isn't syncsafe")))

(deftest tags
  (doseq [version [3 4]]
    (testing (str "ID3v2." version)
      (let [t (id3/read (bs (tag version
                                 (text version "TIT2" "An episode")
                                 (frame version "TPE1"
                                        (concat [1 0xFF 0xFE] (utf16le "Host")))
                                 (apic version "image/jpeg" 3 jpeg)
                                 (text version "TLEN" "3600500")
                                 [0 0 0 0 0 0 0 0])))]
        (is (= version (:version t)))
        (is (= [] (id3/report t)))
        (is (= #{:version :revision :flags :size :end :frames} (set (keys t)))
            "the report is in the metadata")
        (is (= [{:id "TIT2" :encoding :latin1 :text "An episode"}
                {:id "TPE1" :encoding :utf16 :text "Host"}]
               (take 2 (:frames t)))
            "ISO-8859-1, and UTF-16 by its byte order mark")
        (is (= ["image/jpeg" 3 "Cover"]
               ((juxt :mime-type :picture-type :description)
                (nth (:frames t) 2))))
        (is (= (bytes/hex (bs jpeg))
               (bytes/hex (:data (nth (:frames t) 2))))))))
  (testing "ID3v2.2, with three-letter ids and an image format"
    (let [tt2 (frame2 "TT2" (concat [0] (ascii "Old tag   ")))
          pic (frame2 "PIC" (concat [0] (ascii "JPG") [3 0] jpeg))
          t   (id3/read (bs (concat (ascii "ID3") [2 0 0]
                                    (syncsafe (+ (count tt2) (count pic)))
                                    tt2
                                    pic)))]
      (is (= [{:id "TT2" :encoding :latin1 :text "Old tag   "}]
             (take 1 (:frames t))))
      (is (= ["PIC" "JPG" 3] ((juxt :id :image-format :picture-type)
                              (second (:frames t)))))
      (is (= [] (id3/report t)))))
  (is (nil? (id3/read (bs (concat (ascii "RIFF") (repeat 10 0)))))
      "no tag")
  (testing "the bytes after the tag are no part of it"
    (let [xs (concat (tag 4 (text 4 "TIT2" "A")) [0xFF 0xFB 0x90 0x00])
          t  (id3/read (bs xs))]
      (is (= ["A"] (map :text (:frames t))))
      (is (= (- (count xs) 4) (:end t))))))

(deftest unsynchronisation
  (testing "undone over the whole of a 2.3 tag"
    (let [body (text 3 "TIT2" "A")
          t    (id3/read (bs (concat (ascii "ID3") [3 0 0x80]
                                     (syncsafe (+ (count body) 2))
                                     body [0xFF 0x00])))]
      (is (= ["A"] (map :text (:frames t))))))
  (testing "undone in each frame of a 2.4 tag that says so"
    (let [content [0 0x41 0xFF 0x00 0xE0]
          t       (id3/read (bs (tag 4 (frame 4 "PRIV" content :syncsafe
                                              [0 2]))))]
      (is (= [0 0x41 0xFF 0xE0]
             (map #(bytes/at (:data (first (:frames t))) %) (range 4))))))
  (testing "undone in each frame of a 2.4 tag whose header says so"
    (let [body (frame 4 "PRIV" [0xFF 0x00 0xE0])
          t    (id3/read (bs (concat (ascii "ID3") [4 0 0x80]
                                     (syncsafe (count body)) body)))]
      (is (= 2 (bytes/length (:data (first (:frames t)))))))))

(deftest extended-headers
  (testing "ID3v2.3, whose size leaves out the size itself"
    (let [extended [0 0 0 6 0 0 0 0 0 0]
          body     (concat extended (text 3 "TIT2" "A"))
          t        (id3/read (bs (concat (ascii "ID3") [3 0 0x40]
                                         (syncsafe (count body)) body)))]
      (is (= ["A"] (map :text (:frames t))))))
  (testing "ID3v2.4, whose size is syncsafe and counts itself"
    (let [extended [0 0 0 6 1 0]
          body     (concat extended (text 4 "TIT2" "A"))
          t        (id3/read (bs (concat (ascii "ID3") [4 0 0x40]
                                         (syncsafe (count body)) body)))]
      (is (= ["A"] (map :text (:frames t)))))))

(deftest footers
  (let [body (text 4 "TIT2" "A")
        xs   (concat (ascii "ID3") [4 0 0x10] (syncsafe (count body)) body
                     (ascii "3DI") [4 0 0x10] (syncsafe (count body)))
        t    (id3/read (bs xs))]
    (is (= ["A"] (map :text (:frames t))))
    (is (= (count xs) (:end t)))
    (is (= [] (id3/report t)) "the footer is no padding")))

(deftest frame-flags
  (testing "status flags and a group id are kept"
    (let [t (id3/read (bs (tag 4 (frame 4 "TIT2" (concat [7 0] (ascii "A"))
                                        :syncsafe [0x50 0x40]))))]
      (is (= [{:id "TIT2" :encoding :latin1 :text "A"
               :flags #{:tag-alter-preservation :read-only} :group 7}]
             (:frames t)))))
  (testing "ID3v2.3, with the bits where that version has them"
    (let [t (id3/read (bs (tag 3 (frame 3 "TIT2" (concat [9] [0] (ascii "A"))
                                        :plain [0xC0 0x20]))))]
      (is (= [#{:tag-alter-preservation :file-alter-preservation} 9]
             ((juxt :flags :group) (first (:frames t)))))))
  (testing "a compressed or encrypted frame is skipped, and the rest read"
    (doseq [[version flags] [[3 [0 0x80]] [3 [0 0x40]]
                             [4 [0 0x09]] [4 [0 0x04]]]]
      (let [t (id3/read (bs (tag version
                                 (frame version "TIT2" [0 0 0 0 9 0 0x41]
                                        (if (= 4 version) :syncsafe :plain)
                                        flags)
                                 (text version "TALB" "Kept"))))]
        (is (= ["Kept"] (map :text (:frames t))))
        (is (= [[:id3/skipped-frame "TIT2"]]
               (map (juxt :id :frame) (id3/report t))))))))

(deftest truncated-tags
  (let [xs (vec (tag 4 (text 4 "TIT2" "Kept") (text 4 "TALB" "Lost")))
        t  (id3/read (bs (subvec xs 0 (- (count xs) 3))))]
    (is (= ["Kept"] (map :text (:frames t))))
    (is (= [:id3/truncated] (map :id (id3/report t))))
    (is (= (count xs) (:end t)) "the end of the whole tag")))

(deftest malformed-frames
  (testing "a frame too short for its kind is left out, and the rest read"
    (let [t (id3/read (bs (tag 3 (frame 3 "COMM" [0 0x65])
                               (text 3 "TIT2" "Kept"))))]
      (is (= ["Kept"] (map :text (:frames t))))
      (is (= [[:id3/malformed "COMM"]] (map (juxt :id :frame) (id3/report t))))
      (is (every? :message (id3/report t)) "each diagnostic with its message")))
  (testing "a frame whose size runs past the tag ends the frames"
    (let [t (id3/read (bs (tag 4 (text 4 "TIT2" "Kept")
                               (concat (ascii "TALB") (syncsafe 1000) [0 0]
                                       [0 0x41]))))]
      (is (= ["Kept"] (map :text (:frames t))))
      (is (= [:id3/malformed] (map :id (id3/report t)))))))

(deftest quirks
  (testing "a 2.4 frame whose size is a plain number, as some writers put it"
    (let [big (concat (range 0 256) (range 0 256))
          xs  (bs (tag 4 (frame 4 "PRIV" big :plain) (text 4 "TIT2" "After")))]
      (is (= "After" (:text (second (:frames (id3/read xs))))))
      (is (= [:id3/frame-size] (map :id (id3/report (id3/read xs)))))
      (is (= [:id3/padding-junk]
             (map :id (id3/report (id3/read xs {:except #{:id3/frame-size}}))))
          "without the quirk, the frames end where the size leads, which
           seems padding with junk in it")))
  (testing "a second tag right after the first"
    (let [xs (bs (concat (tag 4 (text 4 "TIT2" "New"))
                         (tag 3 (text 3 "TALB" "Old"))
                         [0xFF 0xFB 0x90 0x00]))
          ts (id3/read-all xs)]
      (is (= [4 3] (map :version ts)))
      (is (= [24 48] (map :end ts)) "each ends where it ends in the bytes")
      (is (= [[] [:id3/second-tag]] (map #(map :id (id3/report %)) ts)))
      (is (= [4] (map :version (id3/read-all xs {:except #{:id3/second-tag}}))))
      (is (= [] (id3/read-all (bs (ascii "RIFF")))) "no tag"))))

(deftest kinds-of-your-own
  (let [popm {:read (fn [_ b] {:rating (bytes/at b 2)})}
        xs   (bs (tag 4 (frame 4 "POPM" [0x61 0 196 0 0 0 1])))]
    (is (= [{:id "POPM" :rating 196}]
           (:frames (id3/read xs {:kinds {"POPM" popm}}))))
    (is (= #{:id :data} (set (keys (first (:frames (id3/read xs))))))
        "without it, the content as it is")))
