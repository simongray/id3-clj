id3-clj
=======

[![Clojars Project](https://img.shields.io/clojars/v/dk.simongray/id3-clj.svg)](https://clojars.org/dk.simongray/id3-clj)

This library reads and writes [ID3v2](https://id3.org/id3v2.4.0-structure)
tags, the metadata at the start of MP3 files, in Clojure and
ClojureScript. It reads ID3v2.2, 2.3 and 2.4, writes 2.3 and 2.4, and
covers the chapters of the
[Chapter Frame Addendum](https://id3.org/id3v2-chapters-1.0).

- **ID3 as the specs define it.** Each frame has the fields that the spec
  gives it, under the spec's names. The code cites the section of each
  rule it implements.
- **Plain data.** Tags and frames are maps, and pictures are bytes.
- **What it reads, it writes.** A tag that you read writes back with the
  same frames, which property tests check on every platform.
- **Strict, with quirks.** Each accommodation for a tag that breaks the
  spec is a quirk with an id. The report of a reading names the quirks
  that applied, and you can turn any of them off.
- **Room for more.** A frame of a kind that the library doesn't parse
  keeps its content as bytes, and you can plug in a reader of your own.

The same code runs on the JVM, in Node and in browsers.

> This library was spun out of
> [podcast-clj](https://github.com/simongray/podcast-clj), a library for
> making podcast software, which uses it to read the tags of episodes.
> Like podcast-clj, it was developed with assistance from frontier LLMs.

Getting started
---------------

It requires Clojure 1.11+ and Java 11+. In ClojureScript, it runs in Node
22+ and in browsers. Add it from
[Clojars](https://clojars.org/dk.simongray/id3-clj) to the `:deps` in your
`deps.edn`:

```clojure
dk.simongray/id3-clj {:mvn/version "0.1.0"}
```

For unreleased changes, depend on the latest commit on `master` instead.
In shadow-cljs, a Git dependency only works through `deps.edn`, so set
`:deps true` in your `shadow-cljs.edn` too.

Reading
-------

The library reads tags from bytes: a byte array on the JVM and a
Uint8Array in JavaScript. You can pass the whole file, but reading a tag
only needs its own bytes. Given the first ten, `id3/header` tells you how
many there are:

```clojure
(require '[clojure.java.io :as io]
         '[dk.simongray.id3 :as id3])

(defn read-tag
  "The ID3v2 tag at the start of the file at `path`."
  [path]
  (with-open [in (io/input-stream path)]
    (let [head (.readNBytes in 10)]
      (when-let [{:keys [end]} (id3/header head)]
        (id3/read (byte-array (concat head (.readNBytes in (- end 10)))))))))

(read-tag "episode.mp3")
;; => {:version  4
;;     :revision 0
;;     :flags    #{}
;;     :size     246
;;     :end      256
;;     :frames   [{:id "TIT2" :encoding :latin1 :text "Episode 12"}
;;                {:id "TPE1" :encoding :utf8 :text "Søren Kierkegaard"}
;;                {:id           "APIC"
;;                 :encoding     :latin1
;;                 :mime-type    "image/jpeg"
;;                 :picture-type 3
;;                 :description  "Cover"
;;                 :data         #object["[B" ...]}
;;                ...]}
```

The `:end` is where the tag ends, which is also where the audio or
another tag starts. Some writers put a new tag in front of the old one,
so `id3/read-all` reads all the tags in a row.

The [`frames`](src/dk/simongray/id3/frames.cljc) namespace lists the
fields of each kind of frame. A picture type is a number, and
`frames/picture-types` gives its name, e.g. "Cover (front)" for 3. A
frame of a kind that the library doesn't parse, such as PRIV, keeps its
content as bytes under `:data`.

Chapters
--------

A CHAP frame is a chapter, with its start and end in milliseconds and the
frames inside it, such as its title. A CTOC frame is a table of contents.
The [`chapters`](src/dk/simongray/id3/chapters.cljc) namespace puts
chapters in the order of the top-level table:

```clojure
(require '[dk.simongray.id3.chapters :as chapters])

(->> (chapters/ordered (:frames (read-tag "episode.mp3")))
     (map (juxt :element-id :start-time :end-time)))
;; => (["intro" 0 95000] ["talk" 95000 3540000])
```

Chapters that the table doesn't list come last, by start time. To tell
which ones it lists, use `chapters/listed`.

Writing
-------

To turn a tag back into bytes, use `id3/write`. Its frames have the
fields that reading gives them, so a frame that you read writes back as
it was:

```clojure
;; cover holds the bytes of a JPEG image
(def tag
  {:version 4
   :frames  [{:id "TIT2" :text "Episode 12"}
             {:id "TPE1" :text "Søren Kierkegaard"}
             {:id           "APIC"
              :mime-type    "image/jpeg"
              :picture-type 3
              :data         cover}]})

(id3/write tag {:padding 1024})
```

The `:version` is 3 for ID3v2.3, or 4 for 2.4, the default. The
`:padding` option adds zero bytes after the frames, which leave room to
change the tag later without moving the audio. A frame without an
`:encoding` gets ISO-8859-1 when that can hold its text, or else UTF-8,
or UTF-16 in ID3v2.3.

To put the tag in front of the audio of a file, in place of the tags that
are there, use `id3/with-tag`:

```clojure
(defn retag!
  "Replace the ID3v2 tags of the file at `path` with `tag`."
  [path tag]
  (let [b (with-open [in (io/input-stream path)]
            (.readAllBytes in))]
    (with-open [out (io/output-stream path)]
      (.write out (id3/with-tag b tag)))))
```

For chapters, `chapters/toc` makes the table of contents that lists them
in order:

```clojure
(def chaps
  [{:id         "CHAP"
    :element-id "intro"
    :start-time 0
    :end-time   95000
    :frames     [{:id "TIT2" :text "Introduction"}]}
   {:id         "CHAP"
    :element-id "talk"
    :start-time 95000
    :end-time   3540000
    :frames     [{:id "TIT2" :text "The interview"}]}])

(id3/write {:frames (into [(chapters/toc chaps)] chaps)})
```

When a tag can't be written, e.g. because a string has a null character,
`id3/write` throws an exception. Its data has the `:type`
`:dk.simongray.id3/unwritable`, and the id of the `:frame` when a frame
is to blame.

Quirks
------

Reading follows the spec. Each accommodation for a tag that breaks it is
a quirk, e.g. reading text as UTF-8 when the frame declares ISO-8859-1
but the bytes are UTF-8. The report of a reading names the quirks that
applied and the frames that couldn't be parsed:

```clojure
(id3/report (read-tag "old-episode.mp3"))
;; => [{:id      :id3/utf8-latin1
;;      :level   :info
;;      :message "A frame declared its text as ISO-8859-1, but the bytes
;;                are UTF-8, so the text was read as UTF-8."}]
```

To turn quirks off, name them in the `:except` option, e.g.
`(id3/read tag-bytes {:except #{:id3/utf8-latin1}})`. The `catalogue` in
the [`diagnostics`](src/dk/simongray/id3/diagnostics.cljc) namespace
lists every diagnostic, and an app can add it to a catalogue of its own.

Limits
------

A tag from a stranger can hold millions of tiny frames or strings, which
take far more memory to read than their bytes. So a reading keeps a
limited number of frames, counting those inside chapters, and of strings
beyond the first of each text frame, and `id3/read-all` reads a limited
number of tags in a row. The report says when a reading reaches a limit.
The limits are in `id3/default-limits`, and the `:max-frames`,
`:max-strings` and `:max-tags` options change them.

Frames of your own
------------------

A kind of frame is a map of two functions. Its `:read` takes a reading
context and the content of a frame, and returns the frame's fields. Its
`:write` takes a context and the frame, and returns the content. Pass
kinds by frame id in the `:kinds` option of `id3/read` and `id3/write` to
add to the library's kinds or replace them. The
[`text`](src/dk/simongray/id3/text.cljc) and
[`frames`](src/dk/simongray/id3/frames.cljc) namespaces have the pieces:

```clojure
(require '[dk.simongray.id3.frames :as frames]
         '[dk.simongray.id3.text :as text]
         '[dk.simongray.webcrypto.bytes :as bytes])

(defn read-popularimeter
  "The fields of a POPM frame whose content is `b`, read in the context
  `ctx`: an email, a rating from 1 to 255, and how often it was played."
  [ctx b]
  (let [n         (bytes/length b)
        [email p] (text/string-at ctx b 0 n :latin1)]
    {:email   email
     :rating  (frames/u8 b p)
     :counter (frames/number b (inc p) (- n p 1))}))

(defn write-popularimeter
  "The content of the POPM frame `frame`, written in the context `ctx`."
  [ctx {:keys [email rating counter] :as frame}]
  (bytes/concat (text/encode-terminated email :latin1)
                (frames/number-bytes rating 1)
                (frames/number-bytes counter 4)))

(def opts
  "Options that read and write POPM frames."
  {:kinds {"POPM" {:read read-popularimeter :write write-popularimeter}}})

(-> {:frames [{:id "POPM" :email "me@example.org" :rating 196 :counter 9}]}
    (id3/write opts)
    (id3/read opts)
    :frames)
;; => [{:id "POPM" :email "me@example.org" :rating 196 :counter 9}]
```

The `bytes` namespace comes from
[webcrypto-clj](https://github.com/simongray/webcrypto-clj), which this
library depends on.

Development
-----------

```bash
clojure -X:test                       # the tests on the JVM
npm install                           # once, for ClojureScript
clojure -M:cljs compile test          # the tests in Node
clojure -M:cljs compile browser-test  # the tests in a browser
node test/browser-run.mjs             # run them in a headless Chrome
```

License
-------

The id3-clj project is licensed under the [MIT licence](LICENSE).
