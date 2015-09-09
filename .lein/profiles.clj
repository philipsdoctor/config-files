{:user {:plugins [[cider/cider-nrepl "0.9.1"]
                  [lein-pprint "1.1.1"]
                  [lein-kibit "0.0.8"]
                  [com.jakemccrary/lein-test-refresh "0.5.2"]
                  [com.palletops/lein-shorthand "0.4.0"]
                  [refactor-nrepl "1.1.0"]
                  ]
        :signing {:gpg-key "2ED94C0F"}
        :dependencies [[alembic "0.3.2"]
                       [slamhound "1.5.5"]
                       [org.clojure/tools.nrepl "0.2.7"]
                       [clj-stacktrace "0.2.8"]]
        :shorthand {. [alembic.still/distill alembic.still/lein]}}
 :injections [(let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                     'print-cause-trace)
                    new (ns-resolve (doto 'clj-stacktrace.repl require)
                                    'pst)]
                (alter-var-root orig (constantly (deref new))))]}
