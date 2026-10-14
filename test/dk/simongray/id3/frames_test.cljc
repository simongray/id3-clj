(ns dk.simongray.id3.frames-test
  "The content of each kind of frame, and the quirks of frames with and
  without them."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.id3 :as id3]
            [dk.simongray.id3.frames :as frames]
            [dk.simongray.id3.test-support
             :refer [ascii apic bs frame jpeg png tag utf16le utf8]]
            [dk.simongray.webcrypto.bytes :as bytes]))

(defn- frames-of
  "The frames of a tag of `version` around the frame `xs`, read with
  `opts`."
  ([version xs]
   (frames-of version xs {}))
  ([version xs opts]
   (:frames (id3/read (bs (tag version xs)) opts))))

(defn- first-frame
  "The first frame of a tag of ID3v2.4 around the frame `xs`, read with
  `opts`, without its :data, which the tests compare as hex."
  ([xs]
   (first-frame xs {}))
  ([xs opts]
   (first (frames-of 4 xs opts))))

(deftest kinds
  (testing "text frames, with more than one string in ID3v2.4"
    (is (= {:id "TPE1" :encoding :utf8 :text "Ærø"}
           (first-frame (frame 4 "TPE1" (concat [3] (utf8 "Ærø"))))))
    (is (= {:id "TPE1" :encoding :latin1 :text "A" :values ["A" "B"]}
           (first-frame (frame 4 "TPE1" (concat [0] (ascii "A") [0]
                                                (ascii "B") [0 0])))))
    (is (= {:id "TPE1" :encoding :utf16 :text "A" :values ["A" "B"]}
           (first-frame (frame 4 "TPE1" (concat [1 0xFF 0xFE] (utf16le "A")
                                                [0 0 0xFF 0xFE]
                                                (utf16le "B")))))
        "each string of UTF-16 with its byte order mark")
    (is (= {:id "TIT2" :encoding :latin1 :text ""}
           (first-frame (frame 4 "TIT2" [0])))
        "an empty one"))
  (is (= {:id "TXXX" :encoding :latin1 :description "Mood" :value "Calm"}
         (first-frame (frame 4 "TXXX" (concat [0] (ascii "Mood") [0]
                                              (ascii "Calm"))))))
  (is (= {:id "WOAF" :url "https://example.org"}
         (first-frame (frame 4 "WOAF" (ascii "https://example.org")))))
  (is (= {:id "WXXX" :encoding :latin1 :description "" :url "https://a.b"}
         (first-frame (frame 4 "WXXX" (concat [0 0] (ascii "https://a.b"))))))
  (doseq [id ["COMM" "USLT"]]
    (is (= {:id id :encoding :latin1 :language "eng" :description "Note"
            :text "Some words"}
           (first-frame (frame 4 id (concat [0] (ascii "eng") (ascii "Note")
                                            [0] (ascii "Some words")))))))
  (testing "pictures"
    (let [f (first-frame (apic 4 "image/png" 3 png))]
      (is (= {:id "APIC" :encoding :latin1 :mime-type "image/png"
              :picture-type 3 :description "Cover"}
             (dissoc f :data)))
      (is (= (bytes/hex (bs png)) (bytes/hex (:data f)))))
    (is (= {:id "APIC" :encoding :latin1 :mime-type "-->" :picture-type 0
            :description "" :url "https://a.b/c.jpg"}
           (first-frame (frame 4 "APIC" (concat [0] (ascii "-->") [0 0 0]
                                                (ascii "https://a.b/c.jpg")))))
        "a link")
    (is (= "Cover (front)" (frames/picture-types 3)))
    (is (= 21 (count frames/picture-types))))
  (testing "other frames keep their content"
    (let [f (first-frame (frame 4 "PRIV" (concat (ascii "owner") [0 1 2 3])))]
      (is (= #{:id :data} (set (keys f))))
      (is (= "6f776e657200010203" (bytes/hex (:data f)))))))

(deftest text-quirks
  (testing "text declared ISO-8859-1 whose bytes are UTF-8"
    (let [xs (frame 3 "TIT2" (concat [0] (utf8 "Ærø")))]
      (is (= "Ærø" (:text (first (frames-of 3 xs)))))
      (is (= (apply str (map char (utf8 "Ærø")))
             (:text (first (frames-of 3 xs {:except #{:id3/utf8-latin1}}))))
          "without the quirk, the bytes read as declared")))
  (testing "UTF-16 without its byte order mark"
    (let [xs     (frame 3 "TIT2" (concat [1] (utf16le "Hi")))
          t      (id3/read (bs (tag 3 xs)))
          strict (frames-of 3 xs {:except #{:id3/utf16-bom}})]
      (is (= "Hi" (:text (first (:frames t)))))
      (is (= [:id3/utf16-bom] (map :id (id3/report t))))
      (is (not= "Hi" (:text (first strict))) "big-endian, as Unicode says")))
  (testing "a text encoding that ID3 does not define"
    (let [xs (concat (frame 3 "TIT2" (concat [9] (ascii "Hi")))
                     (frame 3 "TALB" (concat [0] (ascii "Kept"))))
          t  (id3/read (bs (tag 3 xs)))]
      (is (= [["Hi" :latin1] ["Kept" :latin1]]
             (map (juxt :text :encoding) (:frames t)))
          "read as ISO-8859-1")
      (is (= [[:id3/text-encoding 9]]
             (map (juxt :id :encoding) (id3/report t))))
      (let [strict (id3/read (bs (tag 3 xs)) {:except #{:id3/text-encoding}})]
        (is (= ["Kept"] (map :text (:frames strict)))
            "without the quirk, the frame is left out")
        (is (= [:id3/malformed] (map :id (id3/report strict))))))))

(deftest image-type-quirk
  (doseq [declared ["image/jpg" "JPG" ""]]
    (let [xs     (bs (tag 3 (apic 3 declared 3 jpeg)))
          t      (id3/read xs)
          strict (id3/read xs {:except #{:id3/image-type}})]
      (is (= "image/jpeg" (:mime-type (first (:frames t)))) declared)
      (is (= [[:id3/image-type declared]]
             (map (juxt :id :declared) (id3/report t))))
      (is (= declared (:mime-type (first (:frames strict))))
          "without the quirk, the type as declared")))
  (is (= "image/png" (:mime-type (first-frame (apic 4 "image/png" 3 jpeg))))
      "a type that isn't loose stays, whatever the bytes")
  (is (= "JPG" (:mime-type (first-frame (apic 4 "JPG" 3 [1 2 3]))))
      "bytes that start like no image format the quirk checks for"))

(deftest images
  (is (= "image/jpeg" (frames/sniff-image (bs jpeg))))
  (is (= "image/png" (frames/sniff-image (bs png))))
  (is (= "image/webp" (frames/sniff-image (bs (concat (ascii "RIFF") [1 2 3 4]
                                                      (ascii "WEBPVP8 "))))))
  (is (nil? (frames/sniff-image (bs [0xFF 0xD8])))))
