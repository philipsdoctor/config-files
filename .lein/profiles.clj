{:user {:plugins [[cider/cider-nrepl "0.7.0"]
                  [lein-kibit "0.0.8"]
                  [com.jakemccrary/lein-test-refresh "0.5.2"]
                  [com.palletops/lein-shorthand "0.4.0"]]
        :dependencies [[alembic "0.3.2"]
                       [clj-stacktrace "0.2.8"]]
        :shorthand {. [alembic.still/distill alembic.still/lein]}}
        :injections [(let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                            'print-cause-trace)
                           new (ns-resolve (doto 'clj-stacktrace.repl require)
                                           'pst)]
                       (alter-var-root orig (constantly (deref new))))]}
