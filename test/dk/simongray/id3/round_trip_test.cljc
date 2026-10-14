(ns dk.simongray.id3.round-trip-test
  "Tags of every kind of frame, made up at random, which read back as they
  were written."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.walk :as walk]
            [dk.simongray.id3 :as id3]
            [dk.simongray.webcrypto.bytes :as bytes]))

(defn- code-point->string
  "The text of the code point `cp`."
  [cp]
  #?(:clj  (String. (Character/toChars cp))
     :cljs (js/String.fromCodePoint cp)))

(def latin1-string
  "Text that ISO-8859-1 can hold, without a null character."
  (gen/fmap #(apply str (map char %)) (gen/vector (gen/choose 1 255) 0 12)))

(def unicode-string
  "Text of any characters from all planes, but a null character."
  (gen/fmap #(apply str (map code-point->string %))
            (gen/vector (gen/one-of [(gen/choose 1 0xD7FF)
                                     (gen/choose 0xE000 0xFFFF)
                                     (gen/choose 0x10000 0x10FFFF)])
                        0 12)))

(def data
  "Bytes of up to 40."
  (gen/fmap bytes/of (gen/vector (gen/choose 0 255) 0 40)))

(defn- encoded
  "A generator of an :encoding of `version`, with the generator of strings
  that it can hold, as a pair."
  [version]
  (gen/elements (cond-> [[:latin1 latin1-string] [:utf16 unicode-string]]
                  (= 4 version) (conj [:utf16be unicode-string]
                                      [:utf8 unicode-string]))))

(defn- frame-of
  "A generator of the frame that `f` makes of an id from `ids`, and of the
  :encoding of `version` and its generator of strings."
  [version ids f]
  (gen/let [id              (gen/elements ids)
            [encoding text] (encoded version)
            fields          (f encoding text)]
    (into {:id id} fields)))

(defn- text-frame
  "A generator of a text frame of `version`, with more than one string in
  ID3v2.4."
  [version]
  (frame-of version ["TIT2" "TPE1" "TALB" "TCON" "TLEN" "TDRC"]
            (fn [encoding text]
              (gen/let [values (if (= 4 version)
                                 (gen/vector (gen/such-that seq text) 1 3)
                                 (gen/fmap vector text))]
                (cond-> {:encoding encoding :text (first values)}
                  (next values) (assoc :values values))))))

(def url
  "A URL, as ISO-8859-1 text."
  (gen/fmap #(str "https://example.org/" %) latin1-string))

(defn- simple-frame
  "A generator of a frame of `version` of any kind but CHAP and CTOC."
  [version]
  (gen/one-of
   [(text-frame version)
    (frame-of version ["TXXX"]
              (fn [encoding text]
                (gen/hash-map :encoding    (gen/return encoding)
                              :description text
                              :value       text)))
    (gen/hash-map :id (gen/elements ["WOAF" "WOAR" "WPUB"]) :url url)
    (frame-of version ["WXXX"]
              (fn [encoding text]
                (gen/hash-map :encoding    (gen/return encoding)
                              :description text
                              :url         url)))
    (frame-of version ["COMM" "USLT"]
              (fn [encoding text]
                (gen/hash-map :encoding    (gen/return encoding)
                              :language    (gen/elements ["eng" "dan" "und"])
                              :description text
                              :text        text)))
    (frame-of version ["APIC"]
              (fn [encoding text]
                (gen/one-of
                 [(gen/hash-map :encoding     (gen/return encoding)
                                :mime-type    (gen/elements ["image/png"
                                                             "image/jpeg"
                                                             "image/webp"])
                                :picture-type (gen/choose 0 20)
                                :description  text
                                :data         data)
                  (gen/hash-map :encoding     (gen/return encoding)
                                :mime-type    (gen/return "-->")
                                :picture-type (gen/choose 0 20)
                                :description  text
                                :url          url)])))
    (gen/hash-map :id (gen/elements ["PRIV" "GEOB" "PCNT" "UFID"])
                  :data data)]))

(defn- flagged
  "A generator of the frames of `frame-gen`, some with status flags and a
  group id."
  [frame-gen]
  (gen/let [frame frame-gen
            flags (gen/set (gen/elements [:tag-alter-preservation
                                          :file-alter-preservation
                                          :read-only]))
            group (gen/one-of [(gen/return nil) (gen/choose 0 255)])]
    (cond-> frame
      (seq flags) (assoc :flags flags)
      group       (assoc :group group))))

(def element-id
  "An element id of a chapter or a table of contents."
  (gen/such-that seq latin1-string))

(def milliseconds
  "A time or an offset that a chapter can hold."
  (gen/choose 0 0xFFFFFFFE))

(defn- chapter
  "A generator of a CHAP frame of `version`, which ends after its start."
  [version]
  (gen/let [id           element-id
            start        (gen/choose 0 0xFFFF0000)
            length       (gen/choose 1 0xFFFF)
            start-offset (gen/one-of [(gen/return nil) milliseconds])
            end-offset   (gen/one-of [(gen/return nil) milliseconds])
            inner        (gen/vector (flagged (simple-frame version)) 0 3)]
    (cond-> {:id "CHAP" :element-id id :start-time start}
      :always      (assoc :end-time (+ start length))
      start-offset (assoc :start-offset start-offset)
      end-offset   (assoc :end-offset end-offset)
      :always      (assoc :frames inner))))

(defn- table-of-contents
  "A generator of a CTOC frame of `version`."
  [version]
  (gen/hash-map :id         (gen/return "CTOC")
                :element-id element-id
                :top-level? gen/boolean
                :ordered?   gen/boolean
                :children   (gen/vector element-id 1 5)
                :frames     (gen/vector (simple-frame version) 0 2)))

(def tag
  "A generator of a tag of ID3v2.3 or 2.4 with frames of every kind."
  (gen/let [version (gen/elements [3 4])
            frames  (gen/vector (flagged (gen/one-of [(simple-frame version)
                                                      (chapter version)
                                                      (table-of-contents
                                                       version)]))
                                0 8)]
    {:version version :frames frames}))

(defn- comparable
  "The tag `t` with each of its bytes as a vector, which = can compare."
  [t]
  (walk/postwalk #(if (bytes/bytes? %)
                    (mapv (fn [i] (bytes/at % i)) (range (bytes/length %)))
                    %)
                 t))

(def strict-text
  "The options of a reading that takes ISO-8859-1 as it's written, since
  text such as Ã¦ is also UTF-8."
  {:except #{:id3/utf8-latin1}})

(defspec written-tags-read-back 300
  (prop/for-all [t tag]
    (let [read-back (id3/read (id3/write t) strict-text)]
      (and (= (comparable t)
              (comparable (select-keys read-back [:version :frames])))
           (empty? (id3/report read-back))))))

(defspec padding-reads-as-padding 100
  (prop/for-all [t       tag
                 padding (gen/choose 0 2000)]
    (let [b         (id3/write t {:padding padding})
          read-back (id3/read b strict-text)]
      (and (= (comparable (:frames t)) (comparable (:frames read-back)))
           (= (bytes/length b) (:end read-back))
           (empty? (id3/report read-back))))))

(defspec tags-in-front-of-audio 100
  (prop/for-all [old   (gen/vector tag 0 2)
                 new   tag
                 audio (gen/fmap #(bytes/of (concat [0xFF 0xFB] %))
                                 (gen/vector (gen/choose 0 255) 0 40))]
    (let [file (apply bytes/concat (concat (map id3/write old) [audio]))]
      (= (comparable (bytes/concat (id3/write new) audio))
         (comparable (id3/with-tag file new))))))
