-(ns we
  (:use compojure))

(run-server {:port 31337}
  "/*" (servlet server))
