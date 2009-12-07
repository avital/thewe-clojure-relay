(ns we)

(def atom-base-dir "/home/avital/swank/db/2/")

(run-server {:port 31338}
  "/*" (servlet server))
