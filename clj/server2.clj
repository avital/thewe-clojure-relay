(ns we)

(run-server {:port 31338}
  "/*" (servlet server))
