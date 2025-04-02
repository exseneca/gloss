(ns gloss.stemmer
  (:require [clojure.string :as str]))

(def vowels #{\a \e \i \o \u \á \é \í \ó \ú \ü})

(defn get-first-non-vowel-following-vowel [vowels index rst prev]
  (let [next-letter (first rst)]
    (cond (and (vowels prev)
               next-letter
               (not (vowels next-letter)))
          (inc index)

          (empty? rst)
          index

          :else
          (get-first-non-vowel-following-vowel
           vowels
           (inc index)
           (rest rst)
           next-letter))))

(defn get-r1
  "R1 is the region after the first non-vowel followed
  by a vowel. And the null area after the word if there
  is no such non-vowel"
  [vowels word]
  (let [start (get-first-non-vowel-following-vowel
               vowels
               1
               (rest word)
               (first word))
        end (count word)]
    [start end]))

;; TODO: make these into unit tests
(assert (= [5 9] (get-r1 #{\a \e \i \o \u} "beautiful")))
(assert (= [5 6] (get-r1 #{\a \e \i \o \u} "beauty")))
(assert (= [4 4] (get-r1 #{\a \e \i \o \u} "beau")))
(assert (= [2 13] (get-r1 #{\a \e \i \o \u} "animadversion")))
(assert (= [5 9] (get-r1 #{\a \e \i \o \u} "sprinkled")))
(assert (= [3 9] (get-r1 #{\a \e \i \o \u} "eucharist")))

(defn get-r2 [vowels r1 word]
  (let [r1-start (first r1)
        r1-string (drop r1-start word)
        end (second r1)
        r2-start (if (empty? r1-string)
                   0
                   (get-first-non-vowel-following-vowel
                    vowels
                    1
                    (rest r1-string)
                    (first r1-string)))]
    [(+ r1-start r2-start) ;; need to add offset 
     end]))

(assert (= [7 9] (get-r2 #{\a \e \i \o \u} [5 9] "beautiful")))
(assert (= [6 6] (get-r2 #{\a \e \i \o \u} [5 6] "beauty")))
(assert (= [4 4] (get-r2 #{\a \e \i \o \u} [4 4] "beau")))
(assert (= [4 13] (get-r2 #{\a \e \i \o \u} [2 13] "animadversion")))
(assert (= [9 9] (get-r2 #{\a \e \i \o \u} [5 9] "sprinkled")))
(assert (= [6 9] (get-r2 #{\a \e \i \o \u} [3 9] "eucharist")))

(defn next-vowel [vowels index word]
  (cond (empty? word)
        index
        (vowels (first word))
        index
        :else
        (next-vowel
         vowels
         (inc index)
         (rest word))))

(defn next-non-vowel [vowels index word]
  (cond (empty? word)
        index
        (not (vowels (first word)))
        index
        :else
        (next-non-vowel
         vowels
         (inc index)
         (rest word))))

(defn get-rv
  "If the second letter is a consonant, RV is the region after the next
  following vowel, or if the first two letters are vowels, RV is the
  region after the next consonant, and otherwise (consonant-vowel case)
  RV is the region after the third letter. But RV is the end of the word
  if these positions cannot be found."
  [vowels word]
  (let [end (count word)
        second-letter (second word)
        start-rv
        (cond (not (vowels second-letter))
              (inc (next-vowel
                    vowels
                    2
                    (drop 2 word)))
              (and (vowels (first word))
                   (vowels (second word)))
              (inc (next-non-vowel
                    vowels
                    2
                    (drop 2 word)))
              :else 3)]
    [(if (> start-rv end)
       end
       start-rv) ;; Clip if off the end
     end]))

(assert (= [3 5] (get-rv vowels "macho")))
(assert (= [3 5] (get-rv vowels "oliva")))
(assert (= [3 7] (get-rv vowels "trabajo")))
(assert (= [3 5] (get-rv vowels "áureo")))

(def step-0-suffixes  #{"me" "se" "sela" "selo" "selas" "selos" "la" "le" "lo" "las" "les" "los" "nos"})
(def step-0-a-pattern #{"iéndo" "ándo" "ár" "ér" "ír"})
(def step-0-b-pattern #{"ando" "iendo" "ar" "er" "ir"})

(defn make-suffix-regex
  [suffixes]
  (->> (map (fn [s] (str s "$")) suffixes)
       (str/join "|")
       (re-pattern)))

(defn make-step-0-second-suffix-regex
  [s suffixes]
  (->> (map (fn [suffix] (str suffix s "$")) suffixes)
       (str/join "|")
       (re-pattern)))

(defn delete-suffix [word suffix]
  (when (not (re-find (re-pattern (str suffix "$")) word))
    (throw (ex-info "Word does not end in suffix" {:word word
                                                   :suffix suffix})))
  (->>
   (drop-last (count suffix) word)
   (apply str)))

(defn replace-accents-step-0-a [word]
  (let [mapping {"iéndo" "iendo"
                 "ándo" "ando"
                 "ár" "ar"
                 "ér" "er"
                 "ír" "ir"}
        ending
        (re-find (make-suffix-regex step-0-a-pattern) word)]
    (when (not ending)
      (throw (ex-info "Didn't find step 0 a ending" {:word word})))
    (let [with-removed (delete-suffix word ending)
          to-swap (get mapping ending)]
      (str with-removed to-swap))))

(defn step-0 [config
              {:keys [rv]}
              word]
  (let [re (make-suffix-regex step-0-suffixes) ;; TODO put in config
        suffix (re-find re word)]
    ;(println "suffix: " suffix)
    (if suffix
      (let [step-a-re (make-step-0-second-suffix-regex suffix step-0-a-pattern)
            sf-a (re-find step-a-re word)
            step-b-re (make-step-0-second-suffix-regex suffix step-0-b-pattern)
            sf-b (re-find step-b-re word)
            step-c-re #"uyendo"
            sf-c (re-find step-c-re word)]
        #_(println {:sf-a sf-a :sf-b sf-b :sf-c sf-c
                    :step-a-re step-a-re})
        (cond sf-a
              (let [pos (- (count word)
                           (count sf-a))]
                (if (>= pos (first rv))
                  (-> (delete-suffix word suffix)
                      (replace-accents-step-0-a))
                  word))

              sf-b
              (let [pos (- (count word)
                           (count sf-b))]
                (if (>= pos (first rv))
                  (delete-suffix word suffix)
                  word))
              sf-c
              (let [pos (inc (- (count word)
                                (count sf-b)))]
                (if (>= pos (first rv))
                  (delete-suffix word suffix)
                  word))
              :else
              word))
      word)))

(def step-1-a-pattern #{"anza" "anzas" "ico" "ica" "icos" "icas" "ismo" "ismos" "able" "ables" "ible" "ibles" "ista" "istas" "oso" "osa" "osos" "osas" "amiento" "amientos" "imiento" "imientos"})
(def step-1-b-pattern #{"adora" "ador" "ación" "adoras" "adores" "aciones" "ante" "antes" "ancia" "ancias"})

(defn delete-if-ends-in-ic [r2 pos word]
  (if (and (>= (- 2 pos) (first r2))
           (re-find #"ic$" word))
    (delete-suffix word "ic")
    word))

(defn delete-if-ends-in-at [r2 pos word]
  (if (and (>= (- 2 pos) (first r2))
           (re-find #"at$" word))
    (delete-suffix word "at")
    word))

(defn step-1-f-post-processing [r2 pos word]
  (let [first-step (if (and (>= (- 2 pos) (first r2))
                            (re-find #"iv$" word))
                     (delete-suffix word "iv")
                     word)]
    (cond (not= (count first-step) (count word))
          (if (and (>= (- 4 pos) (first r2))
                   (re-find #"at$" first-step))
            (delete-suffix first-step "at")
            first-step)
          :else
          (if (and (>= (- 2 pos) (first r2))
                   (re-find #"at$" word))
            (delete-suffix word "at")
            word))))

(defn step-1-g-post-processing [r2 pos word]
  (let [re #"ante$|able$|ible$"
        suffix (re-find re word)]
    (if (and suffix (>= (- pos (count suffix))
                        (first r2)))
      (delete-suffix word suffix)
      word)))

(defn step-1-h-post-processing [r2 pos word]
  (let [re #"abil$|ic$|iv$"
        suffix (re-find re word)]
    (if (and suffix (>= (- pos (count suffix))
                        (first r2)))
      (delete-suffix word suffix)
      word)))

(defn get-pos [word suffix]
  (- (count word)
     (count suffix)))

(defn step-1
  [config
   {:keys [r1 r2 rv]}
   word]
  (let [step-a-re (make-suffix-regex step-1-a-pattern)
        suffix-a (re-find step-a-re word)
        step-b-re (make-suffix-regex step-1-b-pattern)
        suffix-b (re-find step-b-re word)
        step-c-re #"logía$|logías$"
        suffix-c (re-find step-c-re word)
        step-d-re #"ución$|uciones$"
        suffix-d (re-find step-d-re word)
        step-e-re #"encia$|encias$"
        suffix-e (re-find step-e-re word)
        step-f-re #"amente$"
        suffix-f (re-find step-f-re word)
        step-g-re #"mente$"
        suffix-g (re-find step-g-re word)
        step-h-re #"idad$|idades$"
        suffix-h (re-find step-h-re word)
        step-i-re #"iva$|ivo$|ivas$|ivos$"
        suffix-i (re-find step-i-re word)]
    (cond suffix-a
          (let [pos (get-pos word suffix-a)]
            (if (>= pos (first r2))
              (delete-suffix word suffix-a)
              word))
          suffix-b
          (let [pos (get-pos word suffix-b)]
            (if (>= pos (first r2))
              (->> (delete-suffix word suffix-b)
                   (delete-if-ends-in-ic r2 pos))
              word))
          suffix-c
          (let [pos (get-pos word suffix-c)]
            (if (>= pos (first r2))
              (-> (delete-suffix word suffix-c)
                  (str "log"))
              word))
          suffix-d
          (let [pos (get-pos word suffix-d)]
            (if (>= pos (first r2))
              (-> (delete-suffix word suffix-d)
                  (str "u"))
              word))
          suffix-e
          (let [pos (get-pos word suffix-e)]
            (if (>= pos (first r2))
              (-> (delete-suffix word suffix-e)
                  (str "ente"))
              word))
          suffix-f
          (let [pos (get-pos word suffix-f)]
            (if (>= pos (first r1))
              (->> (delete-suffix word suffix-f)
                   (step-1-f-post-processing r2 pos))
              word))
          suffix-g
          (let [pos (get-pos word suffix-g)]
            (if (>= pos (first r2))
              (->> (delete-suffix word suffix-g)
                   (step-1-g-post-processing r2 pos))
              word))
          suffix-h
          (let [pos (get-pos word suffix-h)]
            (if (>= pos (first r2))
              (->> (delete-suffix word suffix-h)
                   (step-1-h-post-processing r2 pos))
              word))
          suffix-i
          (let [pos (get-pos word suffix-i)]
            (if (>= pos (first r2))
              (->> (delete-suffix word suffix-i)
                   (delete-if-ends-in-at r2 pos))
              word))
          :else
          word)))

(def step-2a-pattern #{"ya" "ye" "yan" "yen" "yeron" "yendo" "yo" "yó" "yas" "yes" "yais" "yamos"})

(defn delete-if-ends-in-u [word]
  (if (re-find #"u$" word)
    (delete-suffix word "u")
    word))

(defn step-2a [config {:keys [rv]} word]
  (let [re (make-suffix-regex step-2a-pattern)
        suffix (re-find re word)]
    (if suffix
      (let [pos (get-pos word suffix)]
        (if (>= pos (first rv))
          (->> (delete-suffix word suffix)
               (delete-if-ends-in-u))
          word))
      word)))

(def step-2b-pattern #{"arían" "arías" "arán" "arás" "aríais" "aría" "aréis" "aríamos" "aremos" "ará" "aré" "erían" "erías" "erán" "erás" "eríais" "ería" "eréis" "eríamos" "eremos" "erá" "eré" "irían" "irías" "irán" "irás" "iríais" "iría" "iréis" "iríamos" "iremos" "irá" "iré" "aba" "ada" "ida" "ía" "ara" "iera" "ad" "ed" "id" "ase" "iese" "aste" "iste" "an" "aban" "ían" "aran" "ieran" "asen" "iesen" "aron" "ieron" "ado" "ido" "ando" "iendo" "ió" "ar" "er" "ir" "as" "abas" "adas" "idas" "ías" "aras" "ieras" "ases" "ieses" "ís" "áis" "abais" "íais" "arais" "ierais" "aseis" "ieseis" "asteis" "isteis" "ados" "idos" "amos" "ábamos" "íamos" "imos" "áramos" "iéramos" "iésemos" "ásemos" "en" "es" "éis" "emos"})

(defn delete-u-from-gu-when-special [special? word]
  (if special?
    (if (re-find #"gu$" word)
      (delete-suffix word "u")
      word)
    word))

(defn delete-u-from-gu-when-special-and-in-rv [special? rv pos word]
  (if special?
    (if (and (re-find #"gu$" word)
             (>= (dec pos) (first rv)))
      (delete-suffix word "u")
      word)
    word))

(defn step-2b [config {:keys [rv]} word]
  (let [re (make-suffix-regex step-2b-pattern)
        suffix (re-find re word)
        special-set #{"en" "es" "éis" "emos"}
        special? (special-set suffix)]
    (if suffix
      (let [pos (get-pos word suffix)]
        (if (>= pos (first rv))
          (->> (delete-suffix word suffix)
               (delete-u-from-gu-when-special special?))
          word))
      word)))

(def step-3-pattern #{"os" "a" "o" "á" "í" "ó" "e" "é"})

(defn step-3 [config {:keys [rv]} word]
  (let [re (make-suffix-regex step-3-pattern)
        suffix (re-find re word)
        special-set #{"e" "é"}
        special? (special-set suffix)]
    (if suffix
      (let [pos (get-pos word suffix)]
        (if (>= pos (first rv))
          (->> (delete-suffix word suffix)
               (delete-u-from-gu-when-special-and-in-rv special? rv pos))
          word))
      word)))

(defn switch-if-mapping [mapping c]
  (if (mapping c)
    (mapping c)
    c))

(defn remove-acute [word]
  (let [mapping {\á \a
                 \é \e
                 \í \i
                 \ó \o
                 \ú \u}]
    (->> (map (partial switch-if-mapping mapping) word)
         (apply str))))

(defn stem
  ([{:keys [vowels] :as config}
    word]
   (let [r1 (get-r1 vowels word)
         r2 (get-r2 vowels r1 word)
         rv (get-rv vowels word)
         step-0-result (step-0 config {:rv rv} word)
           ;_ (print {:step-0 step-0-result})
         step-1-result (step-1 config {:r1 r1 :r2 r2 :rv rv} step-0-result)
         step-2a-result (if (= (count step-0-result)
                               (count step-1-result))
                          (step-2a config {:rv rv} step-1-result)
                          step-1-result)
         step-2b-result (if (= (count step-0-result)
                               (count step-1-result)
                               (count step-2a-result))
                          (step-2b config {:rv rv} step-2a-result)
                          step-2a-result)
         step-3-result (step-3 config {:rv rv} step-2b-result)]
     (remove-acute step-3-result)))
  ([word]
   (stem {:vowels vowels} word)))

(assert (= "mentocaliendo"
           (step-0 {} {:rv [3 15]} "mentocaliéndome")))
