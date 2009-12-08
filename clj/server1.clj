(ns we)

(run-server {:port 31337}
  "/*" (servlet server))
