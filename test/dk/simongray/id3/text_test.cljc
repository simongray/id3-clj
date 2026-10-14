(ns dk.simongray.id3.text-test
  "The four encodings of ID3, and the strings that terminators end."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.id3.diagnostics :as diagnostics]
            [dk.simongray.id3.test-support
             :refer [ascii bs failure utf16be utf16le utf8]]
            [dk.simongray.id3.text :as text]
            [dk.simongray.webcrypto.bytes :as bytes]))

(deftest decoding
  (is (= "Blåbær" (text/decode (bs [0x42 0x6C 0xE5 0x62 0xE6 0x72]) :latin1)))
  (is (= "Blåbær" (text/decode (bs (utf8 "Blåbær")) :utf8)))
  (is (= "Blåbær" (text/decode (bs (utf16le "Blåbær")) :utf16le)))
  (is (= "Blåbær" (text/decode (bs (utf16be "Blåbær")) :utf16be)))
  (is (= "😀" (text/decode (bs [0x3D 0xD8 0x00 0xDE]) :utf16le))
      "a surrogate pair")
  (is (= "�" (text/decode (bs [0xC3]) :utf8)) "bytes that aren't valid")
  (is (= 70000 (count (text/decode (bs (repeat 70000 0x41)) :latin1)))
      "a long text"))

(deftest utf8-check
  (is (text/utf8? (bs (utf8 "Ærø")) 0 5))
  (is (not (text/utf8? (bs (ascii "plain")) 0 5)) "ASCII alone")
  (is (not (text/utf8? (bs [0x42 0xE5 0x62]) 0 3)) "ISO-8859-1")
  (is (not (text/utf8? (bs [0xC3]) 0 1)) "cut short")
  (is (not (text/utf8? (bs [0xC0 0x80]) 0 2)) "overlong"))

(deftest strings
  (let [ctx (diagnostics/context {})]
    (is (= ["A" "B"] (text/strings ctx (bs (concat (ascii "A") [0] (ascii "B")))
                                   0 3 :latin1)))
    (is (= ["A" "" "B"] (text/strings ctx (bs (concat (ascii "A") [0 0]
                                                      (ascii "B")))
                                      0 4 :latin1))
        "an empty string between two")
    (is (= ["A"] (text/strings ctx (bs (concat (ascii "A") [0 0 0]))
                               0 4 :latin1))
        "terminators at the end")
    (is (= [""] (text/strings ctx (bs []) 0 0 :latin1)) "none")
    (testing "UTF-16, whose terminator is two bytes on a boundary of two"
      (is (= ["Ā" "B"]
             (text/strings ctx (bs (concat [0xFF 0xFE 0x00 0x01 0 0 0xFF 0xFE]
                                           (utf16le "B")))
                           0 10 :utf16))))))

(deftest encodings
  (let [ctx (diagnostics/context {})]
    (is (= :utf8 (text/text-encoding ctx (bs [3]))))
    (is (thrown? #?(:clj Exception :cljs :default)
                 (text/text-encoding ctx (bs []))))
    (is (= [2 2 1 1] (map text/width [:utf16 :utf16be :utf8 :latin1]))
        "the width of a terminator")))

(deftest encoding
  (doseq [encoding [:latin1 :utf8 :utf16le :utf16be]]
    (is (= "Blåbær" (text/decode (text/encode "Blåbær" encoding) encoding))
        (name encoding)))
  (is (= "fffe4100" (bytes/hex (text/encode "A" :utf16)))
      "UTF-16 as little-endian after a byte order mark")
  (is (= "3dd800de" (bytes/hex (text/encode "😀" :utf16le)))
      "a surrogate pair")
  (doseq [units [[0xFFFE] [0xFEFF 0x41] [0xD800] [0xDC00 0x41]]]
    (let [s (apply str (map char units))]
      (is (= s (text/decode (text/encode s :utf16be) :utf16be))
          "a code unit that some decoders change, the same on every
           platform")))
  (is (= (str "A" (char 0xFFFD)) (text/decode (bs [0 0x41 0x42]) :utf16be))
      "an odd byte at the end")
  (is (text/latin1? "Blåbær"))
  (is (not (text/latin1? "Łódź")))
  (is (= :dk.simongray.id3/unwritable
         (:type (failure #(text/encode "Łódź" :latin1)))))
  (testing "strings"
    (is (= "4100" (bytes/hex (text/encode-terminated "A" :latin1))))
    (is (= "fffe41000000" (bytes/hex (text/encode-terminated "A" :utf16))))
    (is (= "00410000" (bytes/hex (text/encode-terminated "A" :utf16be))))
    (is (= :dk.simongray.id3/unwritable
           (:type (failure #(text/encode-string (str "A" (char 0)) :utf8))))
        "a null character, which would end the string")
    (is (= :dk.simongray.id3/unwritable
           (:type (failure #(text/encode-string nil :utf8))))
        "no string")))

(deftest encodings-for-writing
  (is (= :latin1 (text/encoding-for 4 nil ["Blåbær" ""])))
  (is (= :utf8 (text/encoding-for 4 nil ["Blåbær" "Łódź"])))
  (is (= :utf16 (text/encoding-for 3 nil ["Łódź"])))
  (is (= :utf8 (text/encoding-for 4 nil ["Ã¦"]))
      "text that would read as UTF-8")
  (is (= :latin1 (text/encoding-for 4 :latin1 ["Ã¦"]))
      "unless the frame asks for ISO-8859-1")
  (is (= :utf16be (text/encoding-for 4 :utf16be ["A"])))
  (is (= :latin1 (text/encoding-for 3 :utf16be ["A"]))
      "an encoding that ID3v2.3 lacks"))
