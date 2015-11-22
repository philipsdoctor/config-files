{:user {:plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [lein-pprint "1.1.1"]
                  [lein-kibit "0.0.8"]
                  [com.palletops/lein-shorthand "0.4.0"]
                  [refactor-nrepl "2.0.0-SNAPSHOT"]]
        :signing {:gpg-key "43FA2B12"}
        :dependencies [[alembic "0.3.2"]
                       [slamhound "1.5.5"]
                       [clj-stacktrace "0.2.8"]]
        :shorthand {. [alembic.still/distill alembic.still/lein]}}
 :injections [(let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                     'print-cause-trace)
                    new (ns-resolve (doto 'clj-stacktrace.repl require)
                                    'pst)]
                (alter-var-root orig (constantly (deref new))))]}
