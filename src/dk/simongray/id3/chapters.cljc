(ns dk.simongray.id3.chapters
  "Chapters, as the ID3v2 Chapter Frame Addendum defines them: CHAP and
  CTOC frames, and the order that a table of contents puts chapters in.

  A CHAP frame is a chapter, a map of these fields:

  - :element-id, its id in the tag
  - :start-time and :end-time, in milliseconds
  - :start-offset and :end-offset, the byte offsets in the file where it
    starts and ends, if it gives them
  - :frames, the frames inside it, such as its title in a TIT2 frame

  A CTOC frame is a table of contents: a map of its :element-id, the
  element ids of its :children, its :frames and two flags. It's
  :top-level? when it's the root of the tables, and :ordered? when its
  children are in order.

  A CHAP or CTOC frame inside another one keeps its content as bytes, as
  frames of unknown kinds do, so nesting can't go deeper than one level.
  To write chapters, put the table of contents that toc makes for them
  first."
  (:require [dk.simongray.id3.diagnostics :as diagnostics]
            [dk.simongray.id3.frames :as frames]
            [dk.simongray.id3.text :as text]
            [dk.simongray.webcrypto.bytes :as bytes]))

(def unused
  "The value of an offset that a chapter doesn't give."
  0xFFFFFFFF)

(defn- embedded
  "The reading context `ctx` for the frames inside a CHAP or CTOC frame,
  in which CHAP and CTOC have no kind."
  [ctx]
  (update ctx :kinds dissoc "CHAP" "CTOC"))

(defn- element-id
  "The element id at `p` in `b` and where what follows it starts, as a
  pair."
  [b p]
  (let [stop (text/terminator b p (bytes/length b) 1)]
    [(text/decode (bytes/slice b p stop) :latin1) (inc stop)]))

;; Chapter Frame Addendum, section 3.1
(defn read-chapter
  "The fields of the CHAP frame whose content is `b`, read in the context
  `ctx`.

  With the quirk :id3/chapter-end, a chapter that ends at or before its
  start gets no end."
  [ctx b]
  (let [[id p]       (element-id b 0)
        start-time   (frames/number b p 4)
        end-time     (frames/number b (+ p 4) 4)
        start-offset (frames/number b (+ p 8) 4)
        end-offset   (frames/number b (+ p 12) 4)
        endless?     (and (<= end-time start-time)
                          (diagnostics/quirk? ctx :id3/chapter-end))]
    (when endless?
      (diagnostics/note! ctx :id3/chapter-end {:element-id id}))
    (let [inner (frames/read (embedded ctx) b (+ p 16) (bytes/length b))]
      (cond-> {:element-id id :start-time start-time}
        (not endless?)             (assoc :end-time end-time)
        (not= unused start-offset) (assoc :start-offset start-offset)
        (not= unused end-offset)   (assoc :end-offset end-offset)
        :always                    (assoc :frames inner)))))

(defn write-chapter
  "The content of the CHAP frame `frame`, written in the context `ctx`.

  Without an :end-time, the chapter ends at its start, which reads back as
  no end by the quirk :id3/chapter-end. Missing offsets are written as
  unused."
  [ctx {:keys [element-id start-time end-time start-offset end-offset frames]
        :as   frame}]
  (bytes/concat (text/encode-terminated element-id :latin1)
                (frames/number-bytes start-time 4)
                (frames/number-bytes (or end-time start-time) 4)
                (frames/number-bytes (or start-offset unused) 4)
                (frames/number-bytes (or end-offset unused) 4)
                (frames/write (embedded ctx) frames)))

(defn- children
  "The `n` element ids at `p` in `b`, and where they end, as a pair. It
  throws when `b` holds fewer than `n`."
  [b p n]
  (loop [p p acc []]
    (cond
      (= n (count acc))
      [acc p]

      (>= p (bytes/length b))
      (throw (ex-info (str "A table of contents with fewer children than it"
                           " counts")
                      {:count n}))

      :else
      (let [[id next] (element-id b p)]
        (recur next (conj acc id))))))

;; Chapter Frame Addendum, section 3.2
(defn read-toc
  "The fields of the CTOC frame whose content is `b`, read in the context
  `ctx`."
  [ctx b]
  (let [[id p]         (element-id b 0)
        flags          (frames/u8 b p)
        [ids q]        (children b (+ p 2) (frames/u8 b (inc p)))]
    {:element-id id
     :top-level? (bit-test flags 1)
     :ordered?   (bit-test flags 0)
     :children   ids
     :frames     (frames/read (embedded ctx) b q (bytes/length b))}))

(defn write-toc
  "The content of the CTOC frame `frame`, written in the context `ctx`. It
  throws when the table lists no children, or more than 255."
  [ctx {:keys [element-id top-level? ordered? children frames] :as frame}]
  (when-not (<= 1 (count children) 255)
    (throw (diagnostics/unwritable
            (str "A table of contents lists from 1 to 255 children, not "
                 (count children))
            {})))
  (apply bytes/concat
         (text/encode-terminated element-id :latin1)
         (bytes/of [(+ (if top-level? 2 0) (if ordered? 1 0))
                    (count children)])
         (concat (map #(text/encode-terminated % :latin1) children)
                 [(frames/write (embedded ctx) frames)])))

(def kinds
  "The kinds of the CHAP and CTOC frames, by id."
  {"CHAP" {:read read-chapter :write write-chapter}
   "CTOC" {:read read-toc :write write-toc}})

(defn toc
  "A top-level, ordered CTOC frame that lists the CHAP frames `chapters` in
  their order, with an element id that none of them has."
  [chapters]
  (let [taken (set (map :element-id chapters))
        id    (->> (iterate inc 2)
                   (map #(str "toc" %))
                   (cons "toc")
                   (remove taken)
                   first)]
    {:id         "CTOC"
     :element-id id
     :top-level? true
     :ordered?   true
     :children   (mapv :element-id chapters)
     :frames     []}))

(defn- top-level
  "The top-level table of contents among `tables`, or else the first of
  them, or nil when there are none."
  [tables]
  (or (some #(when (:top-level? %) %) tables)
      (first tables)))

(defn listed
  "The element ids that the top-level table of contents in `frames` lists,
  in order, with the ids of each table it lists in that table's place, or
  nil when there's no table. An id listed more than once comes where it's
  first listed.

  It follows each table once, however many tables list it, so chains of
  tables or repeated listings can't make the walk deep or long."
  [frames]
  (let [tables (filterv #(= "CTOC" (:id %)) frames)
        by-id  (into {} (map (juxt :element-id identity)) tables)]
    (when-let [top (top-level tables)]
      (loop [todo  (seq (:children top))
             stack ()
             seen  #{(:element-id top)}
             acc   []]
        (cond
          todo
          (let [[child & more] todo]
            (cond
              (seen child)
              (recur more stack seen acc)

              (contains? by-id child)
              (recur (seq (:children (by-id child)))
                     (conj stack more)
                     (conj seen child)
                     acc)

              :else
              (recur more stack (conj seen child) (conj acc child))))

          (seq stack)
          (recur (first stack) (rest stack) seen acc)

          :else
          acc)))))

(defn ordered
  "The CHAP frames in `frames`, in the order that the top-level table of
  contents lists them, followed by the ones it doesn't list, by start
  time."
  [frames]
  (let [places (into {} (map-indexed (fn [i id] [id i])) (listed frames))]
    (->> (filter #(= "CHAP" (:id %)) frames)
         (sort-by (fn [{:keys [element-id start-time]}]
                    [(get places element-id ##Inf) start-time]))
         vec)))
