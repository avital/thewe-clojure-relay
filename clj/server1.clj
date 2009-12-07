(ns we)

(def atom-base-dir "/home/avital/swank/db/1/")

(run-server {:port 31337}
  "/*" (servlet server))
