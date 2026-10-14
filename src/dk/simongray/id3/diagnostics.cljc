(ns dk.simongray.id3.diagnostics
  "Every diagnostic that reading an ID3v2 tag can report, by id, with its
  level and meaning.

  A diagnostic is a map of an :id, a :level and a :message, plus details
  such as the id of the :frame. The ids are part of the API, since apps
  match on them. An app with a catalogue of its own can add this one to
  it.

  Most diagnostics are quirks: accommodations for tags that break the
  spec. A reading applies every quirk that its :except option doesn't
  name. The others say what couldn't be parsed, and where a reading
  stopped at one of its limits. A reading notes the same diagnostic
  once."
  (:require [clojure.string :as str]))

(defn- unwrap
  "The text `s` on one line, without the line breaks and the indentation
  that wrap it in the source."
  [s]
  (str/replace s #"\s*\n\s*" " "))

(def catalogue
  "The diagnostics of a reading, in the order they tend to arise."
  (mapv
   #(update % :doc unwrap)
   [{:id :id3/truncated :level :warning
     :doc "The bytes end before the tag does, so only the frames that fit were
           read."}
    {:id :id3/frame-size :level :info
     :doc "A frame gave its size in the other version's form, a plain number
           in ID3v2.4 or a syncsafe one in ID3v2.3, so it was read that way."}
    {:id :id3/text-encoding :level :info
     :doc "A frame names a text encoding that ID3 doesn't define, so its text
           was read as ISO-8859-1."}
    {:id :id3/utf8-latin1 :level :info
     :doc "A frame declared its text as ISO-8859-1, but the bytes are UTF-8, so
           the text was read as UTF-8."}
    {:id :id3/utf16-bom :level :info
     :doc "A frame declared its text as UTF-16 without a byte order mark, so
           the byte order was worked out from where the zero bytes are."}
    {:id :id3/image-type :level :info
     :doc "A picture gave its MIME type loosely, such as JPG or image/jpg, or
           gave none, so the type was taken from the bytes of the image."}
    {:id :id3/chapter-end :level :info
     :doc "A chapter ends at or before its start, so its end was left out."}
    {:id :id3/second-tag :level :info
     :doc "A second tag follows the first, as when a writer adds a tag in front
           of an existing one, and both tags were read."}
    {:id :id3/skipped-frame :level :info
     :doc "A frame is compressed or encrypted, and wasn't read."}
    {:id :id3/malformed :level :warning
     :doc "A frame couldn't be parsed and was left out. When its header
           couldn't be parsed, the frames after it were left out too."}
    {:id :id3/padding-junk :level :info
     :doc "The padding after the frames holds bytes that aren't zeros, because
           a writer didn't clear it or a frame gave a wrong size."}
    {:id :id3/too-many-frames :level :warning
     :doc "The tag holds more frames than the :max-frames option allows,
           counting those inside chapters, so the frames after the limit were
           left out."}
    {:id :id3/too-many-strings :level :warning
     :doc "The text frames of the tag hold more strings than the :max-strings
           option allows, so from the limit on, each text frame kept only its
           first string."}
    {:id :id3/too-many-tags :level :warning
     :doc "More tags follow each other than the :max-tags option allows, so
           the tags after the limit were left out."}]))

(def by-id
  "The catalogue by id."
  (into {} (map (juxt :id identity)) catalogue))

(defn describe
  "The `diagnostic` with a :level and a :message: its own, or else those
  that the catalogue gives for its id."
  [diagnostic]
  (let [{:keys [doc level]} (by-id (:id diagnostic))]
    (cond-> diagnostic
      (and level (not (:level diagnostic)))  (assoc :level level)
      (and doc (not (:message diagnostic))) (assoc :message doc))))

(def limit-diagnostics
  "The diagnostic that a reading notes when it reaches each limit of its
  options."
  {:max-frames  :id3/too-many-frames
   :max-strings :id3/too-many-strings
   :max-tags    :id3/too-many-tags})

(defn context
  "A reading context for the `opts` of a reading. It collects the
  diagnostics of the reading, holds the quirks that the :except of `opts`
  turns off, and counts what the limits of `opts` limit."
  [{:keys [except] :as opts}]
  {:except (set except)
   :limits (select-keys opts (keys limit-diagnostics))
   :counts (volatile! {})
   :noted  (volatile! #{})
   :report (volatile! [])})

(defn quirk?
  "Whether the quirk `id` is on in the reading context `ctx`, i.e. its
  :except doesn't name it."
  [ctx id]
  (not (contains? (:except ctx) id)))

(defn note!
  "Note the diagnostic `id` with the `details` in the reading context
  `ctx`, unless the context noted the same one already."
  ([ctx id]
   (note! ctx id {}))
  ([ctx id details]
   (let [diagnostic (into {:id id} details)]
     (when-not (contains? @(:noted ctx) diagnostic)
       (vswap! (:noted ctx) conj diagnostic)
       (vswap! (:report ctx) conj diagnostic)))))

(defn within-limit!
  "Count one more of what the limit `k` of the reading context `ctx`
  counts, and return whether the count is within the limit. Beyond it,
  the report says so, once."
  [ctx k]
  (let [limit (get-in ctx [:limits k])
        n     (get (vswap! (:counts ctx) update k (fnil inc 0)) k)]
    (or (nil? limit)
        (<= n limit)
        (do (note! ctx (limit-diagnostics k) {:limit limit})
            false))))

(defn noted
  "The diagnostics that the reading context `ctx` noted, in order."
  [ctx]
  @(:report ctx))

(defn unwritable
  "An exception for a tag or frame that can't be written, with the
  `message` saying why and the `details` in its data, whose :type is
  :dk.simongray.id3/unwritable."
  [message details]
  (ex-info message (assoc details :type :dk.simongray.id3/unwritable)))
