{:user {:plugins [[cider/cider-nrepl "0.7.0-SNAPSHOT"]
                  [lein-kibit "0.0.8"]]
        :dependencies [[alembic "0.2.1"]
                       [clj-stacktrace "0.2.7"]]
        :injections [(let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                            'print-cause-trace)
                           new (ns-resolve (doto 'clj-stacktrace.repl require)
                                           'pst)]
                       (alter-var-root orig (constantly (deref new))))]}}
