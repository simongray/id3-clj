(ns dk.simongray.id3.test-support
  "The tags that the tests build byte by byte. Each builder returns a
  sequence of numbers from 0 to 255, which bs turns into bytes."
  (:require [dk.simongray.webcrypto.bytes :as bytes]))

(defn bs
  "The bytes of the numbers in `xs`."
  [xs]
  (bytes/of xs))

(defn code
  "The code of the character `c`, on both platforms."
  [c]
  #?(:clj (int c) :cljs (.charCodeAt c 0)))

(defn ascii
  "The bytes of `s`, which holds only ASCII."
  [s]
  (map code s))

(defn be
  "The `n` bytes of the number `x`, big-endian."
  [n x]
  (for [i (range (dec n) -1 -1)]
    (mod (quot x (long (Math/pow 256 i))) 256)))

(defn syncsafe
  "The four syncsafe bytes of the number `x`, as ID3 writes a size."
  [x]
  (for [i [21 14 7 0]] (bit-and (quot x (long (Math/pow 2 i))) 0x7F)))

(defn utf16le
  "The UTF-16LE bytes of `s`, which holds only characters of the BMP."
  [s]
  (mapcat #(reverse (be 2 (code %))) s))

(defn utf16be
  "The UTF-16BE bytes of `s`, which holds only characters of the BMP."
  [s]
  (mapcat #(be 2 (code %)) s))

(defn utf8
  "The UTF-8 bytes of `s`."
  [s]
  #?(:clj  (map #(bit-and % 0xFF) (.getBytes ^String s "UTF-8"))
     :cljs (array-seq (.encode (js/TextEncoder.) s))))

(defn failure
  "The data of the exception that calling `f` throws, or nil when it
  returns."
  [f]
  (try
    (f)
    nil
    (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
      (ex-data e))))

(def jpeg
  "The start of a JPEG image."
  [0xFF 0xD8 0xFF 0xE0 0 16 74 70 73 70])

(def png
  "The start of a PNG image."
  [0x89 0x50 0x4E 0x47 0x0D 0x0A 0x1A 0x0A])

(defn frame
  "A frame of `version` 3 or 4 with the `id` and the `body` bytes, its size
  written as `size-form` (:syncsafe or :plain) or by the version, and the
  two bytes of `flags`."
  ([version id body]
   (frame version id body (if (= 4 version) :syncsafe :plain)))
  ([version id body size-form]
   (frame version id body size-form [0 0]))
  ([_ id body size-form flags]
   (concat (ascii id)
           (if (= :syncsafe size-form)
             (syncsafe (count body))
             (be 4 (count body)))
           flags
           body)))

(defn frame2
  "A frame of ID3v2.2 with the `id` and the `body` bytes."
  [id body]
  (concat (ascii id) (be 3 (count body)) body))

(defn tag
  "An ID3v2 tag of `version` around the `frames`."
  [version & frames]
  (let [body (apply concat frames)]
    (concat (ascii "ID3") [version 0 0] (syncsafe (count body)) body)))

(defn text
  "A text frame of `version` with the `id` and the ISO-8859-1 text `s`."
  [version id s]
  (frame version id (concat [0] (ascii s))))

(defn chap
  "A CHAP frame of `version` with the element `id`, from `start` to `end` in
  milliseconds, around the `subframes`."
  [version id start end & subframes]
  (frame version "CHAP" (concat (ascii id) [0] (be 4 start) (be 4 end)
                                (be 4 0xFFFFFFFF) (be 4 0xFFFFFFFF)
                                (apply concat subframes))))

(defn ctoc
  "A top-level, ordered CTOC frame of `version` with the element `id`,
  which lists the `children`."
  [version id children]
  (frame version "CTOC" (concat (ascii id) [0 3 (count children)]
                                (mapcat #(concat (ascii %) [0]) children))))

(defn apic
  "An APIC frame of `version` with a picture of the `mime` type and the
  `picture-type`, whose bytes are `data`."
  [version mime picture-type data]
  (frame version "APIC" (concat [0] (ascii mime) [0 picture-type]
                                (ascii "Cover") [0] data)))

(defn frame-of
  "The bytes of an ID3v2.3 frame `id` with the bytes `body`, as a vector."
  [id body]
  (-> (vec (ascii id)) (into (be 4 (count body))) (into [0 0]) (into body)))

(defn nested-chaps
  "CHAP frames, each inside the one before, `depth` deep."
  [depth]
  (let [head (-> (vec (ascii "a"))
                 (into [0])
                 (into (be 4 0))
                 (into (be 4 1000))
                 (into (be 4 0xFFFFFFFF))
                 (into (be 4 0xFFFFFFFF)))]
    (nth (iterate #(frame-of "CHAP" (into head %)) []) depth)))

(defn ctoc-of
  "A CTOC frame `id` of ID3v2.3 that lists `children`, top-level when
  `top?`."
  [id children top?]
  (frame-of "CTOC" (-> (vec (ascii id))
                       (into [0 (if top? 3 1) (count children)])
                       (into (mapcat #(concat (ascii %) [0])) children))))
