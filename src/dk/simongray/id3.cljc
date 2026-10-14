(ns dk.simongray.id3
  "ID3v2 tags, the metadata at the start of MP3 and other audio files.

  It reads ID3v2.2, 2.3 and 2.4 as id3.org specifies them, along with the
  chapters of the Chapter Frame Addendum, and writes 2.3 and 2.4. A tag is
  a map of its header fields and its :frames. The frames namespace lists
  the fields of each kind of frame.

  Reading is strict. Each accommodation for a tag that breaks the spec is
  a quirk with an id, and the diagnostics namespace lists them all. The
  report of a reading names each quirk that applied. To turn quirks off,
  name them in the :except option."
  (:refer-clojure :exclude [read])
  (:require [dk.simongray.id3.chapters :as chapters]
            [dk.simongray.id3.diagnostics :as diagnostics]
            [dk.simongray.id3.frames :as frames]
            [dk.simongray.webcrypto.bytes :as bytes]))

;; ID3v2.4.0 structure, section 3.1; ID3v2.3.0, section 3.1; ID3v2.2
;; (id3v2-00), section 3.1
(def tag-flags
  "The bit of each flag in a tag header, by version."
  {2 {:unsynchronisation 7 :compression 6}
   3 {:unsynchronisation 7 :extended-header 6 :experimental 5}
   4 {:unsynchronisation 7 :extended-header 6 :experimental 5 :footer 4}})

(def kinds
  "The kinds of frames that have a layout of their own, by id: those in the
  frames namespace, plus CHAP and CTOC."
  (merge frames/kinds chapters/kinds))

(defn- header-at
  "The header of the tag at `p` in `b`, or nil when no tag starts there."
  [b p]
  (when (<= (+ p 10) (bytes/length b))
    (let [version (bytes/at b (+ p 3))
          flags   (bytes/at b (+ p 5))
          size    (frames/syncsafe b (+ p 6))]
      (when (and (= "ID3" (frames/code b p 3))
                 (contains? tag-flags version)
                 (< (bytes/at b (+ p 4)) 0xFF)
                 size)
        (let [flags (into #{}
                          (keep (fn [[flag bit]]
                                  (when (bit-test flags bit) flag)))
                          (tag-flags version))]
          {:version  version
           :revision (bytes/at b (+ p 4))
           :flags    flags
           :size     size
           :end      (+ p 10 size (if (:footer flags) 10 0))})))))

(defn header
  "The header of the ID3v2 tag at the start of `b`, or nil when `b` doesn't
  start with one. It only looks at the first ten bytes.

  The header is a map of:

  - :version, e.g. 4 for ID3v2.4, and :revision
  - :flags, the set of flags that are set
  - :size, the size of the tag without its header and footer
  - :end, where the tag ends, after its footer if it has one, which is
    how many bytes read needs"
  [b]
  (header-at b 0))

;; ID3v2.4.0 structure, section 3.2; ID3v2.3.0, section 3.2
(defn- extended-size
  "The size of the extended header at the start of the `body` of a tag of
  `version` with the `flags`, or 0 when it has none."
  [body version flags]
  (cond
    (not (contains? flags :extended-header)) 0
    (= 4 version)                            (or (frames/syncsafe body 0) 0)
    :else                                    (+ 4 (frames/number body 0 4))))

(defn- tag-at
  "The tag at `p` in `b`, read in the context `ctx`, or nil when no tag
  starts there.

  When `b` ends before the tag does, it reads the frames that fit and
  reports that the tag was cut short. When the extended header can't be
  parsed, the tag has no frames, and the report says why."
  [ctx b p]
  (when-let [{:keys [version flags end] :as h} (header-at b p)]
    (let [total  (bytes/length b)
          stop   (min total (- end (if (:footer flags) 10 0)))
          unsync (contains? flags :unsynchronisation)
          body   (cond-> (bytes/slice b (+ p 10) stop)
                   (and unsync (#{2 3} version)) frames/resync)
          ctx    (assoc ctx
                        :version version
                        :unsynchronisation? (and unsync (= 4 version))
                        :truncated? (> end total))]
      (when (:truncated? ctx)
        (diagnostics/note! ctx :id3/truncated {:end end :length total}))
      (assoc h :frames (try
                         (frames/read ctx
                                      body
                                      (extended-size body version flags)
                                      (bytes/length body))
                         (catch #?(:clj Exception :cljs :default) _
                           (diagnostics/note! ctx :id3/malformed {:offset p})
                           []))))))

(defn- context
  "A reading context for `opts`, whose frame kinds are those of kinds plus
  the :kinds of `opts`."
  [opts]
  (assoc (diagnostics/context opts) :kinds (merge kinds (:kinds opts))))

(defn- reported
  "The `tag` with the diagnostics noted in the reading context `ctx` as its
  report."
  [tag ctx]
  (some-> tag (vary-meta assoc ::report (diagnostics/noted ctx))))

(defn report
  "The diagnostics from reading `tag`, each with its level and message."
  [tag]
  (mapv diagnostics/describe (::report (meta tag))))

(defn read
  "The ID3v2 tag at the start of `b`, read with the `opts` below, or nil
  when `b` doesn't start with one. The tag is its header plus its :frames.

  The bytes should hold the whole tag, up to the :end that header
  returns. If they hold less, it reads the frames that fit. The `opts`
  are:

  - :except, the ids of the quirks to turn off
  - :kinds, frame kinds by id, which add to kinds or replace its entries,
    as the frames namespace describes"
  ([b]
   (read b {}))
  ([b opts]
   {:pre [(map? opts)]}
   (let [ctx (context opts)]
     (reported (tag-at ctx b 0) ctx))))

(defn read-all
  "The ID3v2 tags in a row at the start of `b`, each read with `opts` as
  read takes them, or an empty vector when `b` doesn't start with a tag.
  Each tag's :end is where it ends in `b`, which is where the next one
  starts.

  Some writers put a new tag in front of the old one and leave the old one
  in place. With the quirk :id3/second-tag, read-all also reads each tag
  that follows right after the one before."
  ([b]
   (read-all b {}))
  ([b opts]
   {:pre [(map? opts)]}
   (let [more? (diagnostics/quirk? (context opts) :id3/second-tag)]
     (loop [acc [] p 0]
       (let [ctx (context opts)]
         (when (pos? p)
           (diagnostics/note! ctx :id3/second-tag {:offset p}))
         (if-let [tag (and (or (zero? p) more?)
                           (tag-at ctx b p))]
           (recur (conj acc (reported tag ctx)) (:end tag))
           acc))))))

;; ID3v2.4.0 structure, sections 3.1 and 3.3; ID3v2.3.0, section 3.1
(defn write
  "The bytes of `tag`, a map of a :version and :frames as read returns it,
  written with the `opts` below.

  The :version is 3 for ID3v2.3, or 4 for ID3v2.4, which is the default.
  The frames namespace describes the fields of each kind of frame. A frame
  of a kind that the library doesn't parse is written from its :data. The
  `opts` are:

  - :padding, the number of zero bytes after the frames, 0 by default.
    Padding leaves room to change the tag later without moving the audio.
  - :kinds, frame kinds by id, which add to kinds or replace its entries

  It throws when the tag can't be written, e.g. for an id of ID3v2.2 or
  a string with a null character. The data of the exception has the
  :type :dk.simongray.id3/unwritable, and the id of the :frame."
  ([tag]
   (write tag {}))
  ([{:keys [version frames] :or {version 4} :as tag}
    {:keys [padding] :or {padding 0} :as opts}]
   (when-not (contains? #{3 4} version)
     (throw (diagnostics/unwritable
             (str "ID3v2.3 and 2.4 can be written, not " (pr-str version))
             {})))
   (when-not (and (int? padding) (<= 0 padding))
     (throw (diagnostics/unwritable
             (str "The padding is a number of bytes, not " (pr-str padding))
             {})))
   (let [ctx  {:version version :kinds (merge kinds (:kinds opts))}
         body (frames/write ctx frames)]
     (bytes/concat (bytes/of [0x49 0x44 0x33 version 0 0])
                   (frames/syncsafe-bytes (+ (bytes/length body) padding))
                   body
                   (bytes/of (repeat padding 0))))))

(defn- audio-start
  "Where the ID3v2 tags in a row at the start of `b` end, or 0 when `b`
  doesn't start with a tag."
  [b]
  (loop [p 0]
    (if-let [{:keys [end]} (header-at b p)]
      (recur (min end (bytes/length b)))
      p)))

(defn with-tag
  "The bytes of the file `b`, with its ID3v2 tags replaced by `tag`, written
  with `opts` as write takes them. The audio stays as it is, and so does an
  ID3v1 tag at the end of the file."
  ([b tag]
   (with-tag b tag {}))
  ([b tag opts]
   (bytes/concat (write tag opts) (bytes/slice b (audio-start b)))))

(comment
  ;; a tag of ID3v2.4 with a title, TIT2, and a length in milliseconds, TLEN
  (def sample
    (let [header [0x49 0x44 0x33 4 0 0 0 0 0 28]
          tit2   [0x54 0x49 0x54 0x32 0 0 0 3 0 0 3 0x48 0x69]
          tlen   [0x54 0x4C 0x45 0x4E 0 0 0 5 0 0 0 0x31 0x35 0x30 0x30]]
      (bytes/of (concat header tit2 tlen))))

  (header sample)
  (read sample)
  (report (read sample))

  ;; a text encoding that ID3 does not define, with the quirk and without it
  (def loose
    (let [header [0x49 0x44 0x33 4 0 0 0 0 0 13]
          tit2   [0x54 0x49 0x54 0x32 0 0 0 3 0 0 9 0x48 0x69]]
      (bytes/of (concat header tit2))))

  (map :id (report (read loose)))
  (map :id (report (read loose {:except #{:id3/text-encoding}})))
  #_.)
