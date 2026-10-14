(ns dk.simongray.id3.text
  "The text in ID3v2 frames: its four encodings, and strings that end in a
  terminator.

  A frame with text names its encoding in its first byte. ID3v2.4 has four
  encodings: ISO-8859-1, UTF-16 with a byte order mark, UTF-16BE and
  UTF-8. ID3v2.3 has only the first two. A string ends at a terminator,
  which is a zero byte, or two for UTF-16, or else at the end of the frame.

  The functions that read a frame take a reading context from
  diagnostics/context, for the quirks of text that breaks the spec. The
  functions that write one throw for text that the frame can't hold."
  (:require [dk.simongray.id3.diagnostics :as diagnostics]
            [dk.simongray.webcrypto.bytes :as bytes])
  #?(:clj (:import [java.nio.charset StandardCharsets])))

;; ID3v2.4.0 structure, section 4; ID3v2.3.0, section 3.3
(def encodings
  "The text encoding of each encoding byte."
  {0 :latin1
   1 :utf16
   2 :utf16be
   3 :utf8})

(defn width
  "The width in bytes of the terminator of a string in `encoding`."
  [encoding]
  (if (#{:utf16 :utf16be} encoding) 2 1))

#?(:cljs
   (defn- binary-string
     "The bytes `b` as a string of one character for each byte, in pieces so
     that a long text never overflows the stack of `apply`."
     [b]
     (let [n (alength b)]
       (apply str (for [i (range 0 n 32768)]
                    (.apply js/String.fromCharCode nil
                            (.subarray b i (min n (+ i 32768)))))))))

(defn decode
  "The text of the bytes `b` in `encoding`: :latin1, :utf8, :utf16le or
  :utf16be. A byte order mark stays, and bytes that aren't valid become
  U+FFFD, on both platforms."
  [b encoding]
  (case encoding
    :utf8    (bytes/decode b)
    #?(:clj  (String. ^bytes b
                      (case encoding
                        :latin1  StandardCharsets/ISO_8859_1
                        :utf16le StandardCharsets/UTF_16LE
                        :utf16be StandardCharsets/UTF_16BE))
       :cljs (if (= :latin1 encoding)
               (binary-string b)
               (let [label (case encoding
                             :utf16le "utf-16le"
                             :utf16be "utf-16be")]
                 (.decode (js/TextDecoder. label #js {:ignoreBOM true})
                          b))))))

(defn- code-units
  "The UTF-16 code units of the text `s`, as numbers."
  [s]
  #?(:clj  (map int s)
     :cljs (map #(.charCodeAt s %) (range (count s)))))

(defn latin1?
  "Whether ISO-8859-1 can hold each character of the text `s`."
  [s]
  (every? #(< % 0x100) (code-units s)))

#?(:cljs
   (defn- utf16-bytes
     "The bytes of the code `units` of a text in UTF-16, with the high byte
     first when `big-endian?`."
     [units big-endian?]
     (bytes/of (mapcat (fn [u]
                         (let [high (quot u 256)
                               low  (rem u 256)]
                           (if big-endian? [high low] [low high])))
                       units))))

(defn encode
  "The bytes of the text `s` in `encoding`: :latin1, :utf8, :utf16le,
  :utf16be, or :utf16, which is UTF-16LE after a byte order mark. It
  throws when the encoding is ISO-8859-1 and `s` doesn't fit it."
  [s encoding]
  (case encoding
    :utf8    (bytes/encode s)
    :latin1  (if (latin1? s)
               (bytes/of (code-units s))
               (throw (diagnostics/unwritable
                       (str "ISO-8859-1 can't hold the text " s)
                       {})))
    :utf16le #?(:clj  (.getBytes ^String s StandardCharsets/UTF_16LE)
                :cljs (utf16-bytes (code-units s) false))
    :utf16be #?(:clj  (.getBytes ^String s StandardCharsets/UTF_16BE)
                :cljs (utf16-bytes (code-units s) true))
    :utf16   (bytes/concat (bytes/of [0xFF 0xFE]) (encode s :utf16le))))

;; RFC 3629, section 3
(defn utf8?
  "Whether the bytes of `b` from `start` to `end` are UTF-8 with one or more
  characters outside ASCII.

  Bytes like these are almost certainly UTF-8, even when a frame declares
  another encoding."
  [b start end]
  (let [continued? (fn [i n]
                     (every? #(= 0x80 (bit-and (bytes/at b (+ i %)) 0xC0))
                             (range 1 (inc n))))]
    (loop [i start wide? false]
      (if (>= i end)
        wide?
        (let [x (bytes/at b i)
              ;; how many bytes follow the first of a character, or -1 for
              ;; a byte that starts none
              n (cond
                  (< x 0x80) 0
                  (< x 0xC2) -1
                  (< x 0xE0) 1
                  (< x 0xF0) 2
                  (< x 0xF5) 3
                  :else      -1)]
          (cond
            (neg? n)          false
            (zero? n)         (recur (inc i) wide?)
            (> (+ i n 1) end) false
            (continued? i n)  (recur (+ i n 1) true)
            :else             false))))))

(defn text-encoding
  "The text encoding named by the first byte of `b`, the content of a
  frame, read in the context `ctx`.

  With the quirk :id3/text-encoding, a byte that names none of ID3's
  encodings means ISO-8859-1. Without the quirk it throws, as it does for
  a frame without content."
  [ctx b]
  (when (zero? (bytes/length b))
    (throw (ex-info "A frame without content" {})))
  (let [x (bytes/at b 0)]
    (cond
      (contains? encodings x)
      (encodings x)

      (diagnostics/quirk? ctx :id3/text-encoding)
      (do (diagnostics/note! ctx :id3/text-encoding {:encoding x})
          :latin1)

      :else
      (throw (ex-info "A text encoding that ID3 does not define"
                      {:encoding x})))))

(defn terminator
  "Where the string at `start` in `b` ends: at its first terminator of
  `width` zero bytes, aligned for UTF-16, or at `end` when there's none
  before it."
  [b start end width]
  (loop [p start]
    (cond
      (> (+ p width) end)
      end

      (every? #(zero? (bytes/at b (+ p %))) (range width))
      p

      :else
      (recur (+ p width)))))

(defn- utf16-order
  "The byte order of the UTF-16 string in `b` from `start` to `end`, read in
  the context `ctx`, and where the string starts, as a pair.

  A byte order mark gives the order, and the string starts after it.
  Without one, the order is big-endian, as Unicode says. With the quirk
  :id3/utf16-bom, the order follows from where the zero bytes are instead,
  since the high byte of a Latin letter is zero."
  [ctx b start end]
  (let [room? (< (inc start) end)
        x     (when room? (bytes/at b start))
        y     (when room? (bytes/at b (inc start)))
        zeros (fn [k]
                (count (filter #(zero? (bytes/at b %))
                               (range (+ start k) end 2))))]
    (cond
      (and (= 0xFF x) (= 0xFE y))
      [:utf16le (+ start 2)]

      (and (= 0xFE x) (= 0xFF y))
      [:utf16be (+ start 2)]

      (or (>= start end)
          (not (diagnostics/quirk? ctx :id3/utf16-bom)))
      [:utf16be start]

      :else
      (do (diagnostics/note! ctx :id3/utf16-bom)
          [(if (> (zeros 1) (zeros 0)) :utf16le :utf16be) start]))))

(defn- decoded
  "The string in `b` from `start` to `end` in the ID3 `encoding`, read in
  the context `ctx`."
  [ctx b start end encoding]
  (case encoding
    :latin1 (if (and (diagnostics/quirk? ctx :id3/utf8-latin1)
                     (utf8? b start end))
              (do (diagnostics/note! ctx :id3/utf8-latin1)
                  (decode (bytes/slice b start end) :utf8))
              (decode (bytes/slice b start end) :latin1))
    :utf16  (let [[order start] (utf16-order ctx b start end)]
              (decode (bytes/slice b start end) order))
    (decode (bytes/slice b start end) encoding)))

(defn string-at
  "The string in the ID3 `encoding` at `start` in `b`, up to its terminator
  or `end`, read in the context `ctx`. It returns a pair of the string and
  where what follows it starts."
  [ctx b start end encoding]
  (let [n    (width encoding)
        stop (terminator b start end n)]
    [(decoded ctx b start stop encoding) (min end (+ stop n))]))

(defn strings
  "The strings in the ID3 `encoding` in `b` from `start` to `end`, read in
  the context `ctx`, which ID3v2.4 separates with terminators.

  It leaves out empty strings at the end, since some writers end the last
  string with one or more terminators. When that leaves none, it returns
  one empty string."
  [ctx b start end encoding]
  (loop [p start acc []]
    (if (>= p end)
      (let [n (count (take-while empty? (rseq acc)))]
        (or (not-empty (subvec acc 0 (- (count acc) n)))
            [""]))
      (let [[s next] (string-at ctx b p end encoding)]
        (recur next (conj acc s))))))

;; ID3v2.3.0, section 3.3; ID3v2.4.0 structure, section 4
(def version-encodings
  "The text encodings of each version that a writer can use."
  {3 #{:latin1 :utf16}
   4 #{:latin1 :utf16 :utf16be :utf8}})

(defn encoding-byte
  "The byte that names `encoding` in a frame."
  [encoding]
  (some (fn [[x e]] (when (= e encoding) x)) encodings))

(defn- looks-utf8?
  "Whether the text `s`, written in ISO-8859-1, would read as UTF-8 by the
  quirk :id3/utf8-latin1."
  [s]
  (let [b (encode s :latin1)]
    (utf8? b 0 (bytes/length b))))

(defn encoding-for
  "The encoding to write the `strings` of a frame of `version` in: the
  `requested` one, when the version has it and it can hold the strings.

  Otherwise, it's ISO-8859-1 when that can hold the strings, unless one of
  them would look like UTF-8 to a reader. Failing that, it's UTF-8 for
  ID3v2.4 and UTF-16 for 2.3."
  [version requested strings]
  (let [fits? (every? latin1? strings)]
    (cond
      (and (contains? (version-encodings version) requested)
           (or (not= :latin1 requested) fits?))
      requested

      (and fits? (not-any? looks-utf8? strings))
      :latin1

      (= 4 version)
      :utf8

      :else
      :utf16)))

(defn encode-string
  "The bytes of the string `s` in the ID3 `encoding`, without a terminator.
  It throws when `s` isn't a string, when it holds a null character, which
  would end it, or when the encoding can't hold it."
  [s encoding]
  (when-not (string? s)
    (throw (diagnostics/unwritable (str "Not a string: " (pr-str s)) {})))
  (when (some zero? (code-units s))
    (throw (diagnostics/unwritable
            (str "A null character would end the string " s)
            {})))
  (encode s encoding))

(defn encode-terminated
  "The bytes of the string `s` in the ID3 `encoding`, with its terminator."
  [s encoding]
  (bytes/concat (encode-string s encoding)
                (bytes/of (repeat (width encoding) 0))))
