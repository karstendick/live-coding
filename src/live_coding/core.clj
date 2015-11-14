(ns live-coding.core
  (:use [clojure.repl]
        [overtone.live]
        [overtone.inst.piano]
        [overtone.inst.sampled-piano]
        [overtone.inst.drum :only [quick-kick haziti-clap soft-hat open-hat snare]]))


(defn piano-chord [root chord-name]
  (doseq [note (chord root chord-name)]
    (piano note)))



;(twinkle)
;(stop)


;(def p sampled-piano)
(defn play-chord [root chord-name]
  (doseq [note (chord root chord-name)]
    (piano note)))

(defn play-chord [chord]
  (map piano chord))

;; play a chord progression on our piano
(let [time (now)]
  (at time (play-chord (chord :D3 :major7)))
  (at (+ 2000 time) (play-chord (chord :A3 :major)))
  (at (+ 3000 time) (play-chord (chord :A3 :major7)))
  (at (+ 4300 time) (play-chord (chord :F3 :major7))))

;; here's a different function that encapsulates the above
(defn play-progression [chords]
  (if (empty? chords) nil
    (doseq []
      (play-chord (first chords))
      (Thread/sleep 2000)
      (play-progression (rest chords)))))

;; define a simple instrument. in the video, sam uses :action :free,
;; but this is now :action FREE
(definst beep [note 60]
  (let [sound-src (sin-osc (midicps note))
        env       (env-gen (perc 0.01 1.0) :action FREE)] ; sam uses :free
    (* sound-src env)))

;; admire our beep :-)
(beep)

;; beep across a wide range of sounds
(for [i (range 110)] (at (+ (now) (* i 20)) (beep i)))

;; model a plucked string. this is really cool!
(definst plucked-string [note 60 amp 0.8 dur 2 decay 30 coef 0.3 gate 1]
  (let [freq   (midicps note)
        noize  (* 0.8 (white-noise))
        dly    (/ 1.0 freq)
        plk    (pluck noize gate dly dly decay coef)
        dist   (distort plk)
        filt   (rlpf dist (* 12 freq) 0.6)
        clp    (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur)) reverb)))

;; ___|)_______________|\________________|\______________|\_______________|\________
;;|___/___||___________|_________________|_______________|________________|_________||
;;|__/|___||.________,-.___( )___o-;___,-.___o-;__( )__,-.________o-; __,-.___o-;__.||
;;|_/(|,\_||.___(_)__`-'___|______/____`-'____/___|____`-'___(_)___/____`-'____/___.||
;;|_\_|_/_||____|__________|______________________|__________|______________________||
;;    |         |          |/                     |/         |
;;  (_|         |/                                           |/

;; note: the underscores are rests
(def reich-degrees [:vi :vii :i+ :_ :vii :_ :i+ :vii :vi :_ :vii :_])
(def pitches (degrees->pitches reich-degrees :diatonic :C4))

;; temporal recursion: create a function that takes:
;; 1) time to play a note
;; 2) list of notes to play
;; 3) seperation of notes
;;
;; Armed with these parameters, we check if the note is a rest,
;; if it isn't, schedule it to be played. then, we schedule
;; a recursive call to be made to our function again at the new time,
;; calculated by adding the current time plus the separation.
(defn play
  [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (piano note)))
    (let [next-time (+ time sep)]
      (apply-by next-time play [next-time (rest notes) sep]))))

(defn play-chords
  [time cs sep]
  (let [c (first cs)]
    (when c
      (at time (play-chord c)))
    (let [next-time (+ time sep)]
      (apply-by next-time play-chords [next-time (rest cs) sep]))))

(defn play-fns
  [time cs sep f]
  (let [c (first cs)]
    (when c
      (at time (f c)))
    (let [next-time (+ time sep)]
      (apply-by next-time play-fns [next-time (rest cs) sep f]))))

;; play some pitches
(play (now) pitches 200)

;; cycle through some pitches
;; this will loop indefinitely.
(let [t (+ 500 (now))]
  (play t (cycle pitches) 1000)
  (play t (cycle pitches) 1020))

(def chords [(chord :c3 :major)
             (chord :f3 :major)
             (chord :g3 :major)
             (chord :c3 :major)])

(defn play-chord [a-chord]
  (doseq [note a-chord]
    (piano note :amp 0)))

(def kicks [1 nil 1 nil])
(def play-kick (fn [&args]  (quick-kick :amp 0.5)))

(def claps [nil 1 nil 1])
(def play-clap (fn [&args] (haziti-clap :decay 0.5 :amp 1)))

(def snares [nil nil 1 1
             nil nil 1 nil
             nil 1 1 1
             nil nil 1 nil])
(def play-snare (fn [&args] (snare :decay 1 :amp 1)))

(play-progression chords)

(play-chords  (+ 500 (now)) chords 1000)

(let [t (+ 500 (now))]
  (play-chords t (cycle chords) 1000))

(quick-kick :amp 0.5)


(play-fns  (+ 500 (now)) chords 1000 play-chord)

(let [t (+ 500 (now))]
  (play-fns t (cycle chords) 1000 play-chord)
  (play-fns t (cycle kicks) 1000 play-kick)
  (play-fns t (cycle claps) 1000 play-clap)
  (play-fns t (cycle snares) 250 play-snare))

(def ring-hat (freesound 12912))
(def fs-snare (freesound 26903))
(def click (freesound 406))
(def wop (freesound 85291))
(def subby (freesound 25649))
(def dirty-kick quick-kick)

(defsynth wobble-bass [amp 1 note 52 wobble 1 detune 1.01 wob-lo 200 wob-hi 20000 pan 0]
  (let [saws          (mix (saw [note (* note detune)]))
        wob-freq      (lin-exp (lf-saw wobble) -1 1 wob-lo wob-hi)
        wob-freq      (lag wob-freq 0.05)
        filtered-saws (lpf saws wob-freq)
        normalized    (normalizer filtered-saws)
        amplified     (* amp normalized)]
    (out 0 (pan2 amplified pan))))

(wobble-bass :amp 0.3)

(def a {:rate 0.5})
(def b {:rate 3})
(def c {:rate 10})

(def p piano)

(def pats {subby [1 1 0 1 0 1 0 0]
           snare [1 0 0 1 0 0 1 0]
           wop   [1 0 0 0 0 0 0 1]})
(def live-pats (atom pats))

(defn flatten1
  "Takes a map and returns a seq of all the key val pairs:
      (flatten1 {:a 1 :b 2 :c 3}) ;=> (:b 2 :c 3 :a 1)"
  [m]
  (reduce (fn [r [arg val]] (cons arg (cons val r))) [] m))

(defn normalise-beat-info
  [beat]
  (cond
   (= 1 beat)         {}
   (= 0 beat)         nil
   (nil? beat)        nil
   (map? beat)        beat
   (sequential? beat) beat
   :else              {}))

(defn schedule-pattern
  [curr-t pat-dur sound pattern]
  {:pre [(sequential? pattern)]}
  (let [beat-sep-t (/ pat-dur (count pattern))]
    (doseq [[beat-info idx] (partition 2 (interleave pattern (range)))]
      (let [beat-t    (+ curr-t (* idx beat-sep-t))
            beat-info (normalise-beat-info beat-info)]
        (if (sequential? beat-info)
          (schedule-pattern beat-t beat-sep-t sound beat-info)
          (when beat-info
            (at beat-t (apply sound (flatten1 beat-info)))))))))

(defn live-sequencer
  [curr-t pat-dur live-patterns]
  (doseq [[sound pattern] @live-patterns]
    (schedule-pattern curr-t pat-dur sound pattern))
  (let [new-t (+ curr-t pat-dur)]
    (apply-by new-t #'live-sequencer [new-t pat-dur live-patterns])))

(defn n
  [note-kw]
  {:note (note note-kw)})



(live-sequencer (now) 4000 live-pats)
(swap! live-pats assoc subby [1 1 0 0 b 0 b [1 1 1 1 1 1 1 1]])
(swap! live-pats assoc fs-snare [1 1 c c
                                 1 a [1 a c 1] c])
(swap! live-pats assoc wop   [c a 0 0 a c c c])
(swap! live-pats assoc dirty-kick  [1 0 [1 1 1 1] 0])
(swap! live-pats assoc p [{:note (note :c4)}
                          (repeat 2 {:note (note :f4)})
                          (repeat 4 {:note (note :g4)})
                          {:note (note :c4)}])
(swap! live-pats assoc p [[(n :c4) (n :d4)]
                          [0 (n :f4)]
                          (repeat 4 (n :g4))
                          (n :c4)])




(swap! live-pats dissoc p)

(swap! live-pats dissoc dirty-kick)
(swap! live-pats dissoc subby)
(swap! live-pats dissoc fs-snare)

(swap! live-pats empty)

(def wb (wobble-bass :amp 0.5 :note 30 :wob-hi 2000))
(ctl wb :amp 0.1 :note 48 :wobble 2)



(stop)

;(println "foo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluate whole file by hitting
;;   ctrl + shift + enter, or
;;   cmd + shift + enter
;;
;; add some functions under these comment lines
;; evaluate each form (line or function) by hitting
;;   ctrl + enter, or
;;   cmd + enter
;;
;; for example
;; (piano 67)
;;
;; listen to the sound you have made
;;
