(ns election-lifecycle.audio)

(defonce audio-element (atom nil))
(defonce playing? (atom false))

(defn start-playing! []
  (reset! playing? true)
  (.play @audio-element))

(defn stop-playing! []
  (reset! playing? false))

(defn init-background-track!
  []
  (reset! audio-element (js/document.querySelector "audio"))
  (.addEventListener @audio-element "ended" (fn []
                                              (when @playing?
                                                (.play @audio-element))))
  (.addEventListener (js/document.getElementById "play-pause-button")
                     "click"
                     (fn []
                       (if @playing?
                         (stop-playing!)
                         (start-playing!)))))



