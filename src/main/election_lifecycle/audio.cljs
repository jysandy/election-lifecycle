(ns election-lifecycle.audio)

(defonce audio-context (atom nil))
(defonce source (atom nil))
(defonce audio-buffer (atom nil))
(defonce playing? (atom false))

(defn start-playing! []
  (reset! source (.createBufferSource @audio-context))
  (set! (.-buffer @source) @audio-buffer)
  (.connect @source (.-destination @audio-context))
  (set! (.-loop @source) true)
  (reset! playing? true)
  (.start @source 0))

(defn stop-playing! []
  (reset! playing? false)
  (.stop @source 0))

(defn init-background-track!
  []
  (reset! audio-context (js/AudioContext.))
  (let [request (js/XMLHttpRequest.)]
    (.open request "GET", "background-noise.wav", true)
    (set! (.-responseType request) "arraybuffer")
    (set! (.-onload request) (fn []
                               (.decodeAudioData @audio-context
                                                 (.-response request)
                                                 (fn [buffer]
                                                   (reset! audio-buffer buffer)))
                               (.addEventListener (js/document.getElementById "play-pause-button")
                                                  "click"
                                                  (fn []
                                                    (when (= (.-state @audio-context) "suspended")
                                                      (.resume @audio-context))
                                                    (if @playing?
                                                      (stop-playing!)
                                                      (start-playing!))))))
    (.send request)))



