(ns test-runner
  (:require [cljs.test :refer [run-tests]]
            [ol.rfc9839-cljs-test]))

(defn -main []
  (run-tests 'ol.rfc9839-cljs-test))

(set! *main-cli-fn* -main)