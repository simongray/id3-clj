(ns dk.simongray.id3.frames
  "The frames of an ID3v2 tag: their headers, and the layout of each kind
  of frame.

  A frame is a map of its :id and the fields of its kind:

      a text frame     :encoding and :text, and :values when it holds more
                       than one string
      TXXX             :encoding, :description and :value
      a URL frame      :url
      WXXX             :encoding, :description and :url
      COMM, USLT       :encoding, :language, :description and :text
      APIC             :encoding, :mime-type, :picture-type, :description
                       and :data, or :url when the picture is a link
      PIC              the same, with an :image-format such as JPG in place
                       of the MIME type
      any other frame  :data, its content

  The three-letter ids of ID3v2.2, such as TT2 and COM, have the same
  fields. A frame can also have :flags, the set of status flags that are
  set, and :group, its group id.

  A kind is a map of two functions. Its :read takes a reading context and
  the content of a frame, and returns the frame's fields. Its :write does
  the reverse: it takes a context and the frame, and returns the content.
  A writing context holds the :version and the :kinds. The kinds table
  holds the kinds by frame id, and the chapters namespace adds CHAP and
  CTOC."
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as str]
            [dk.simongray.id3.diagnostics :as diagnostics]
            [dk.simongray.id3.text :as text]
            [dk.simongray.webcrypto.bytes :as bytes]))

(defn u8
  "The byte at `p` in `b`, from 0 to 255. It throws when `p` is past the
  end of `b`, so that a frame too short for its kind fails to parse."
  [b p]
  (when-not (< -1 p (bytes/length b))
    (throw (ex-info "The bytes end before the field" {:offset p})))
  (bytes/at b p))

(defn number
  "The big-endian number of the `n` bytes at `p` in `b`."
  [b p n]
  (reduce (fn [acc i] (+ (* 256 acc) (u8 b (+ p i)))) 0 (range n)))

(defn- digits
  "The `n` digits of the whole number `x` in the `base`, the most
  significant first, or nil when `x` isn't a whole number that they can
  hold."
  [x n base]
  (when (and (number? x)
             (== x (Math/floor x))
             (<= 0 x)
             (< x (reduce * (repeat n base))))
    (loop [x (long x) acc ()]
      (if (= n (count acc))
        acc
        (recur (quot x base) (conj acc (rem x base)))))))

(defn number-bytes
  "The `n` bytes of the whole number `x`, big-endian. It throws when `x`
  isn't a whole number that they can hold."
  [x n]
  (or (some-> (digits x n 256) bytes/of)
      (throw (diagnostics/unwritable
              (str "The number " (pr-str x) " doesn't fit in " n " bytes")
              {}))))

;; ID3v2.4.0 structure, section 6.2
(defn syncsafe
  "The syncsafe integer in the four bytes at `p` in `b`, or nil when one of
  them has its top bit set, which a syncsafe integer never has."
  [b p]
  (let [xs (mapv #(u8 b (+ p %)) (range 4))]
    (when (every? #(< % 0x80) xs)
      (reduce #(+ (* 128 %1) %2) 0 xs))))

(defn syncsafe-bytes
  "The four bytes of `x` as a syncsafe integer. It throws when `x` doesn't
  fit in its 28 bits."
  [x]
  (or (some-> (digits x 4 128) bytes/of)
      (throw (diagnostics/unwritable
              (str "The size " x " doesn't fit in 28 bits")
              {}))))

;; ID3v2.4.0 structure, section 6.1
(defn resync
  "The bytes `b` with unsynchronisation undone, i.e. without the zero byte
  after each 0xFF."
  [b]
  (let [n   (bytes/length b)
        out #?(:clj (byte-array n) :cljs (js/Uint8Array. n))]
    (loop [i 0 j 0]
      (if (>= i n)
        (bytes/slice out 0 j)
        (let [x         (bytes/at b i)
              inserted? (and (= 0xFF x)
                             (< (inc i) n)
                             (zero? (bytes/at b (inc i))))]
          #?(:clj  (aset-byte out j (unchecked-byte x))
             :cljs (aset out j x))
          (recur (if inserted? (+ i 2) (inc i))
                 (inc j)))))))

(defn code
  "The `n` bytes at `p` in `b` as ISO-8859-1 text, which is how frame ids
  and the languages of comments are written."
  [b p n]
  (text/decode (bytes/slice b p (+ p n)) :latin1))

(defn id?
  "Whether `s` is the id of a frame of `version`: three capitals or digits
  for ID3v2.2, four for 2.3 and 2.4."
  [version s]
  (boolean (re-matches (if (= 2 version) #"[A-Z0-9]{3}" #"[A-Z0-9]{4}") s)))

(defn plausible-end?
  "Whether a frame of `version` that ends at `p` in `b` is followed by
  something that can follow a frame before `end`:

  - nothing
  - the header of another frame
  - padding, which is zeros up to the end of the tag, of which it checks
    the first kilobyte"
  [b version p end]
  (or (= p end)
      (and (<= (+ p (if (= 2 version) 6 10)) end)
           (id? version (code b p (if (= 2 version) 3 4))))
      (and (< p end)
           (every? #(zero? (bytes/at b %))
                   (range p (min end (+ p 1024)))))))

;; ID3v2.4.0 structure, section 4.1; ID3v2.3.0, section 3.3
(defn- frame-size
  "The size of the frame of `version` 2.3 or 2.4 whose header is at `p` in
  `b`, read in the context `ctx`, or nil when no size fits within `end`.

  The size is a plain number in 2.3 and a syncsafe one in 2.4, but some
  writers use the other form. With the quirk :id3/frame-size, it takes the
  other form when only that one ends the frame in a plausible place."
  [ctx b version p end]
  (let [plain       (number b (+ p 4) 4)
        safe        (syncsafe b (+ p 4))
        [own other] (if (= 4 version) [safe plain] [plain safe])
        fits?       #(and % (plausible-end? b version (+ p 10 %) end))]
    (cond
      (fits? own)
      own

      (and (diagnostics/quirk? ctx :id3/frame-size) (fits? other))
      (do (diagnostics/note! ctx :id3/frame-size {:offset p})
          other)

      (and own (<= (+ p 10 own) end))
      own)))

;; ID3v2.4.0 structure, section 4.1; ID3v2.3.0, section 3.3.1
(def flag-bits
  "Where each flag is in the two flag bytes of a frame header, by version,
  as pairs of the byte and the bit."
  {3 {:tag-alter-preservation  [0 7]
      :file-alter-preservation [0 6]
      :read-only               [0 5]
      :compression             [1 7]
      :encryption              [1 6]
      :grouping-identity       [1 5]}
   4 {:tag-alter-preservation  [0 6]
      :file-alter-preservation [0 5]
      :read-only               [0 4]
      :grouping-identity       [1 6]
      :compression             [1 3]
      :encryption              [1 2]
      :unsynchronisation       [1 1]
      :data-length-indicator   [1 0]}})

(def status-flags
  "The flags that a frame keeps in its :flags. They say what to do with the
  frame, not how to parse it."
  #{:tag-alter-preservation :file-alter-preservation :read-only})

;; ID3v2.4.0 structure, section 4.1.2; ID3v2.3.0, section 3.3.1
(def added-fields
  "The fields that frame flags add after the frame header, by version, in
  order, as pairs of the flag and the size of its field."
  {3 [[:compression 4] [:encryption 1] [:grouping-identity 1]]
   4 [[:grouping-identity 1] [:encryption 1] [:data-length-indicator 4]]})

(defn- frame-flags
  "The flags of the frame of `version` whose header is at `p` in `b`, as a
  set."
  [b version p]
  (into #{}
        (keep (fn [[flag [i bit]]]
                (when (bit-test (u8 b (+ p 8 i)) bit)
                  flag)))
        (get flag-bits version)))

(defn- field-starts
  "Where each field that the `flags` add starts, by flag, in a frame of
  `version` whose header ends at `p`. Its :content is where the content
  of the frame starts."
  [version flags p]
  (reduce (fn [starts [flag n]]
            (if (contains? flags flag)
              (-> starts
                  (assoc flag (:content starts))
                  (update :content + n))
              starts))
          {:content p}
          (get added-fields version)))

(defn- frame-header
  "The header of the frame at `p` in `b`, read in the context `ctx`, or nil
  when there's no frame there that ends by `end`. It's a map of the :id,
  :flags and :group, with the :start of the content and the :stop of the
  frame."
  [ctx b p end]
  (let [version (:version ctx)
        v2?     (= 2 version)
        header  (if v2? 6 10)
        id      (code b p (if v2? 3 4))
        size    (when (id? version id)
                  (if v2?
                    (number b (+ p 3) 3)
                    (frame-size ctx b version p end)))]
    (when (and size (<= (+ p header size) end))
      (let [flags  (cond-> (frame-flags b version p)
                     (:unsynchronisation? ctx) (conj :unsynchronisation))
            starts (field-starts version flags (+ p header))
            stop   (+ p header size)]
        {:id    id
         :flags flags
         :group (when-let [q (:grouping-identity starts)]
                  (when (< q stop)
                    (u8 b q)))
         :start (:content starts)
         :stop  stop}))))

;; WHATWG MIME Sniffing Standard, section 6.1
(def image-signatures
  "The MIME type of each image format, with the bytes that its files start
  with, where nil stands for any byte."
  [["image/jpeg" [0xFF 0xD8 0xFF]]
   ["image/png" [0x89 0x50 0x4E 0x47 0x0D 0x0A 0x1A 0x0A]]
   ["image/gif" [0x47 0x49 0x46 0x38]]
   ["image/webp" [0x52 0x49 0x46 0x46 nil nil nil nil 0x57 0x45 0x42 0x50]]])

(defn sniff-image
  "The MIME type of the image in `b` when it starts like a JPEG, PNG, GIF or
  WebP file, or else nil."
  [b]
  (some (fn [[mime-type signature]]
          (when (and (>= (bytes/length b) (count signature))
                     (every? (fn [[i x]] (or (nil? x) (= x (bytes/at b i))))
                             (map-indexed vector signature)))
            mime-type))
        image-signatures))

(defn loose-type?
  "Whether the `mime-type` of a picture frame is loose, such as JPG or
  image/jpg, or missing."
  [mime-type]
  (let [t (str/lower-case (str/trim (str mime-type)))]
    (not (and (re-matches #"image/[a-z0-9.+-]+" t)
              (not= "image/jpg" t)))))

(defn- image-type
  "The MIME type of a picture whose frame declares `declared` and whose
  image is `data`, read in the context `ctx`. When the declared type is
  loose, it takes the type from the start of the image instead, by the
  quirk :id3/image-type."
  [ctx declared data]
  (if-let [sniffed (and (loose-type? declared)
                        (diagnostics/quirk? ctx :id3/image-type)
                        (sniff-image data))]
    (do (diagnostics/note! ctx :id3/image-type {:declared declared})
        sniffed)
    declared))

;; ID3v2.4.0 frames, section 4.2
(defn read-text
  "The fields of the text frame whose content is `b`, read in the context
  `ctx`: its encoding and one string, or more in ID3v2.4."
  [ctx b]
  (let [encoding (text/text-encoding ctx b)
        values   (text/strings ctx b 1 (bytes/length b) encoding)]
    (cond-> {:encoding encoding :text (first values)}
      (next values) (assoc :values values))))

(defn- encoding-byte
  "The one byte that names `encoding`, as bytes."
  [encoding]
  (bytes/of [(text/encoding-byte encoding)]))

(defn write-text
  "The content of the text frame `frame`, written in the context `ctx`:
  its :values, or else its :text.

  ID3v2.3 has room for only one string, so there it joins the values with
  slashes, as ID3v2.3 does for the people in TPE1 and the like."
  [ctx {:keys [encoding text values] :as frame}]
  (let [version  (:version ctx)
        values   (or values [text])
        values   (if (and (= 3 version) (next values))
                   [(str/join "/" values)]
                   values)
        encoding (text/encoding-for version encoding values)]
    (apply bytes/concat
           (encoding-byte encoding)
           (interpose (bytes/of (repeat (text/width encoding) 0))
                      (map #(text/encode-string % encoding) values)))))

;; ID3v2.4.0 frames, section 4.2.6
(defn read-user-text
  "The fields of the TXXX frame whose content is `b`, read in the context
  `ctx`: its encoding, a description and a value."
  [ctx b]
  (let [end              (bytes/length b)
        encoding         (text/text-encoding ctx b)
        [description p]  (text/string-at ctx b 1 end encoding)
        [value _]        (text/string-at ctx b p end encoding)]
    {:encoding encoding :description description :value value}))

(defn write-user-text
  "The content of the TXXX frame `frame`, written in the context `ctx`."
  [ctx {:keys [encoding description value] :or {description ""} :as frame}]
  (let [encoding (text/encoding-for (:version ctx)
                                    encoding
                                    [description value])]
    (bytes/concat (encoding-byte encoding)
                  (text/encode-terminated description encoding)
                  (text/encode-string value encoding))))

;; ID3v2.4.0 frames, section 4.3.1
(defn read-url
  "The fields of the URL frame whose content is `b`, read in the context
  `ctx`: its URL."
  [ctx b]
  {:url (first (text/string-at ctx b 0 (bytes/length b) :latin1))})

(defn write-url
  "The content of the URL frame `frame`, written in the context `ctx`."
  [_ {:keys [url] :as frame}]
  (text/encode-string url :latin1))

;; ID3v2.4.0 frames, section 4.3.2
(defn read-user-url
  "The fields of the WXXX frame whose content is `b`, read in the context
  `ctx`: its encoding, a description and a URL."
  [ctx b]
  (let [end             (bytes/length b)
        encoding        (text/text-encoding ctx b)
        [description p] (text/string-at ctx b 1 end encoding)]
    {:encoding    encoding
     :description description
     :url         (first (text/string-at ctx b p end :latin1))}))

(defn write-user-url
  "The content of the WXXX frame `frame`, written in the context `ctx`."
  [ctx {:keys [encoding description url] :or {description ""} :as frame}]
  (let [encoding (text/encoding-for (:version ctx) encoding [description])]
    (bytes/concat (encoding-byte encoding)
                  (text/encode-terminated description encoding)
                  (text/encode-string url :latin1))))

;; ID3v2.4.0 frames, sections 4.8 and 4.10
(defn read-comment
  "The fields of the COMM or USLT frame whose content is `b`, read in the
  context `ctx`: its encoding, a language, a description and a text."
  [ctx b]
  (let [end             (bytes/length b)
        encoding        (text/text-encoding ctx b)
        language        (code b 1 3)
        [description p] (text/string-at ctx b 4 end encoding)]
    {:encoding    encoding
     :language    language
     :description description
     :text        (first (text/string-at ctx b p end encoding))}))

;; ISO 639-2: und, an undetermined language
(defn write-comment
  "The content of the COMM or USLT frame `frame`, written in the context
  `ctx`. Without a :language, it writes und, the code for a language that
  isn't known."
  [ctx {:keys [encoding language description text]
        :or   {language "und" description ""}
        :as   frame}]
  (when-not (and (string? language) (= 3 (count language)))
    (throw (diagnostics/unwritable
            (str "A language of three letters, not " (pr-str language))
            {})))
  (let [encoding (text/encoding-for (:version ctx)
                                    encoding
                                    [description text])]
    (bytes/concat (encoding-byte encoding)
                  (text/encode-string language :latin1)
                  (text/encode-terminated description encoding)
                  (text/encode-string text encoding))))

(defn- picture
  "The `fields` of a picture frame whose content is `b`, plus those from
  the picture type at `p` on, read in the context `ctx`. The picture is a
  link when its `format` is -->."
  [ctx b p format fields]
  (let [end              (bytes/length b)
        encoding         (:encoding fields)
        [description q]  (text/string-at ctx b (inc p) end encoding)
        fields           (assoc fields
                                :picture-type (u8 b p)
                                :description description)]
    (if (= "-->" format)
      (assoc fields :url (text/decode (bytes/slice b q end) :latin1))
      (assoc fields :data (bytes/slice b q end)))))

;; ID3v2.4.0 frames, section 4.14
(defn read-picture
  "The fields of the APIC frame whose content is `b`, read in the context
  `ctx`: its encoding, a MIME type, a picture type, a description, and the
  image or a link to it."
  [ctx b]
  (let [encoding      (text/text-encoding ctx b)
        [mime-type p] (text/string-at ctx b 1 (bytes/length b) :latin1)
        fields        (picture ctx b p mime-type {:encoding  encoding
                                                  :mime-type mime-type})]
    (cond-> fields
      (:data fields) (update :mime-type #(image-type ctx % (:data fields))))))

(defn write-picture
  "The content of the APIC frame `frame`, written in the context `ctx`: a
  link when it has a :url, or else its :data.

  Without a :picture-type, it writes 0, other. Without a :mime-type, it
  writes an empty one, which ID3 takes as image/."
  [ctx {:keys [encoding mime-type picture-type description data url]
        :or   {mime-type "" picture-type 0 description ""}
        :as   frame}]
  (when-not (or url (bytes/bytes? data))
    (throw (diagnostics/unwritable "A picture without :data or :url" {})))
  (let [encoding (text/encoding-for (:version ctx) encoding [description])]
    (bytes/concat (encoding-byte encoding)
                  (text/encode-terminated (if url "-->" mime-type) :latin1)
                  (number-bytes picture-type 1)
                  (text/encode-terminated description encoding)
                  (if url (text/encode-string url :latin1) data))))

;; ID3v2.2 (id3v2-00), section 4.15
(defn read-pic
  "The fields of the PIC frame of ID3v2.2 whose content is `b`, read in the
  context `ctx`: its encoding, an image format, a picture type, a
  description, and the image or a link to it."
  [ctx b]
  (let [encoding     (text/text-encoding ctx b)
        image-format (code b 1 3)]
    (picture ctx b 4 image-format {:encoding     encoding
                                   :image-format image-format})))

(defn read-other
  "The fields of a frame of a kind without a reader, whose content is `b`:
  the content as it is."
  [_ b]
  {:data b})

(defn write-other
  "The content of the frame `frame`, of a kind without a writer, written in
  the context `ctx`: its :data as it is."
  [_ {:keys [data] :as frame}]
  (if (bytes/bytes? data)
    data
    (throw (diagnostics/unwritable
            "A frame of a kind without a writer needs its :data"
            {}))))

;; ID3v2.4.0 frames, section 4.14
(def picture-types
  "The name of each picture type of an APIC or PIC frame, by its number."
  ["Other"
   "32x32 pixels 'file icon' (PNG only)"
   "Other file icon"
   "Cover (front)"
   "Cover (back)"
   "Leaflet page"
   "Media (e.g. label side of CD)"
   "Lead artist/lead performer/soloist"
   "Artist/performer"
   "Conductor"
   "Band/Orchestra"
   "Composer"
   "Lyricist/text writer"
   "Recording Location"
   "During recording"
   "During performance"
   "Movie/video screen capture"
   "A bright coloured fish"
   "Illustration"
   "Band/artist logotype"
   "Publisher/Studio logotype"])

(def text-frame
  "The kind of the text frames, whose ids start with T."
  {:read read-text :write write-text})

(def url-frame
  "The kind of the URL frames, whose ids start with W."
  {:read read-url :write write-url})

(def other-frame
  "The kind of a frame without a kind of its own, which keeps its content
  as it is."
  {:read read-other :write write-other})

(def kinds
  "The kinds of frames that have a layout of their own, by id, including
  the ids of ID3v2.2, which can be read but not written."
  (let [user-text {:read read-user-text :write write-user-text}
        user-url  {:read read-user-url :write write-user-url}
        remark    {:read read-comment :write write-comment}]
    {"TXXX" user-text
     "TXX"  user-text
     "WXXX" user-url
     "WXX"  user-url
     "COMM" remark
     "COM"  remark
     "USLT" remark
     "ULT"  remark
     "APIC" {:read read-picture :write write-picture}
     "PIC"  {:read read-pic}}))

(defn kind
  "The kind of the frame `id`: its entry in `kinds`, or else the kind of
  text or URL frames when `id` starts with T or W, or else other-frame."
  [kinds id]
  (or (get kinds id)
      (cond
        (re-matches #"T[A-Z0-9]{2,3}" id) text-frame
        (re-matches #"W[A-Z0-9]{2,3}" id) url-frame
        :else                             other-frame)))

(defn- frame-at
  "The frame whose `header` is at `p` in `b`, read by its kind in the
  context `ctx`. It's nil when the content can't be parsed, and the report
  says so."
  [ctx b p {:keys [id flags group start stop] :as header}]
  (try
    (let [content (cond-> (bytes/slice b start stop)
                    (contains? flags :unsynchronisation) resync)
          status  (into #{} (filter status-flags) flags)]
      (cond-> (into {:id id} ((:read (kind (:kinds ctx) id)) ctx content))
        (seq status) (assoc :flags status)
        group        (assoc :group group)))
    (catch #?(:clj Exception :cljs :default) _
      (diagnostics/note! ctx :id3/malformed {:offset p :frame id})
      nil)))

(defn read
  "The frames in `b` from `start` up to the padding or `end`, read in the
  context `ctx`.

  The context holds the :version of the tag and the :kinds of frames. It
  skips a frame that is compressed or encrypted, and leaves out one whose
  content can't be parsed. A header that can't be parsed ends the frames,
  and so does the :max-frames limit of the context. The report notes each
  of these, except a header that was cut off in a tag that is
  :truncated?."
  [ctx b start end]
  (let [header (if (= 2 (:version ctx)) 6 10)]
    (loop [p start acc []]
      (cond
        (or (>= p end)
            (zero? (u8 b p))
            (> (+ p header) end))
        (do (when (and (< p end)
                       (not (plausible-end? b (:version ctx) p end)))
              (diagnostics/note! ctx :id3/padding-junk {:offset p}))
            acc)

        (not (diagnostics/within-limit! ctx :max-frames))
        acc

        :else
        (if-let [{:keys [id flags start stop] :as h}
                 (frame-header ctx b p end)]
          (if (or (contains? flags :compression)
                  (contains? flags :encryption)
                  (> start stop))
            (do (diagnostics/note! ctx :id3/skipped-frame {:frame id})
                (recur stop acc))
            (let [frame (frame-at ctx b p h)]
              (recur stop (cond-> acc frame (conj frame)))))
          (do (when-not (:truncated? ctx)
                (diagnostics/note! ctx :id3/malformed {:offset p}))
              acc))))))

(defn- flag-bytes
  "The two flag bytes of a frame header of `version` with the status
  `flags`, and with the flag of a group id when `grouped?`."
  [version flags grouped?]
  (let [bits (flag-bits version)]
    (bytes/of (reduce (fn [acc flag]
                        (let [[i bit] (bits flag)]
                          (update acc i bit-set bit)))
                      [0 0]
                      (cond-> (vec flags)
                        grouped? (conj :grouping-identity))))))

;; ID3v2.4.0 structure, section 4.1; ID3v2.3.0, section 3.3
(defn- frame-bytes
  "The bytes of the frame `frame`, written by its kind in the context
  `ctx`, with its header and its group id."
  [ctx {:keys [id flags group] :as frame}]
  (let [version (:version ctx)
        write   (:write (kind (:kinds ctx) (str id)))]
    (when-not (and (string? id) (id? version id))
      (throw (diagnostics/unwritable
              (str "Not the id of a frame of ID3v2." version ": "
                   (pr-str id))
              {})))
    (when-not (every? status-flags flags)
      (throw (diagnostics/unwritable
              (str "Flags that a frame can't keep: " (pr-str flags))
              {})))
    (when-not write
      (throw (diagnostics/unwritable (str "No writer for the frame " id)
                                     {})))
    (let [content (cond->> (write ctx frame)
                    group (bytes/concat (number-bytes group 1)))
          size    (bytes/length content)]
      (bytes/concat (text/encode id :latin1)
                    (if (= 4 version)
                      (syncsafe-bytes size)
                      (number-bytes size 4))
                    (flag-bytes version flags (some? group))
                    content))))

(defn write
  "The bytes of the `frames`, one after another, written in the context
  `ctx`, which holds the :version of the tag and the :kinds of frames.

  It throws when a frame can't be written, with the id of the frame as
  the :frame of the exception's data."
  [ctx frames]
  (apply bytes/concat
         (for [frame frames]
           (try
             (frame-bytes ctx frame)
             (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
               (throw (ex-info (ex-message e)
                               (update (ex-data e) :frame #(or % (:id frame)))
                               e)))))))
