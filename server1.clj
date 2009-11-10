(ns we)

(def hi 192)
(run-server {:port 31337}
  "/*" (servlet server))
