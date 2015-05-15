(ns hive.core
  (:require [hive.coord :as c]))

(enable-console-print!)

(def up2 (c/add c/up c/up))

(println "Hello there" up2)
