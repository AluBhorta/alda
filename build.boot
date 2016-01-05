(set-env!
  :source-paths #{"client/src"}
  :resource-paths #{"server/src" "server/test"
                    "server/grammar" "examples"}
  :dependencies '[
                  ; dev
                  [adzerk/bootlaces      "0.1.12" :scope "test"]
                  [adzerk/boot-jar2bin   "1.0.0"  :scope "test"]
                  [adzerk/boot-test      "1.0.4"  :scope "test"]

                  ; server
                  [org.clojure/clojure    "1.7.0"]
                  [instaparse             "1.4.1"]
                  [io.aviso/pretty        "0.1.20"]
                  [com.taoensso/timbre    "4.1.1"]
                  [clj-http               "2.0.0"]
                  [ring                   "1.4.0"]
                  [ring/ring-defaults     "0.1.5"]
                  [compojure              "1.4.0"]
                  [djy                    "0.1.4"]
                  [str-to-argv            "0.1.0"]
                  [jline                  "2.12.1"]
                  [org.clojars.sidec/jsyn "16.7.3"]

                  ; client
                  [org.apache.commons/commons-lang3     "3.4"]
                  [org.apache.httpcomponents/httpclient "4.5.1"]
                  [com.beust/jcommander                 "1.48"]
                  [org.fusesource.jansi/jansi           "1.11"]
                  [net.jodah/recurrent                  "0.4.0"]
                  [us.bpsm/edn-java                     "0.4.6"]
                  ])

(require '[adzerk.bootlaces    :refer :all]
         '[adzerk.boot-jar2bin :refer :all]
         '[adzerk.boot-test    :refer :all]
         '[alda.util]
         '[alda.version])

; sets log level to TIMBRE_LEVEL (if set) or :warn
(alda.util/set-timbre-level!)

; version number is stored in alda.version
(bootlaces! alda.version/-version-)

(defn- exe-version
  "Convert non-exe-friendly version numbers like 1.0.0-rc1 to four-number
   version numbers like 1.0.0.1 that launch4j can use to make exe files."
  [version]
  (if-let [[_ n rc] (re-matches #"(.*)-rc(\d+)" version)]
    (format "%s.%s" n rc)
    (if-let [[_ n _] (re-matches #"(.*)-SNAPSHOT" version)]
      (format "%s.999" n)
      version)))

(task-options!
  pom     {:project 'alda
           :version alda.version/-version-
           :description "A music programming language for musicians"
           :url "https://github.com/alda-lang/alda"
           :scm {:url "https://github.com/alda-lang/alda"}
           :license {"name" "Eclipse Public License"
                     "url" "http://www.eclipse.org/legal/epl-v10.html"}}

  install {:pom "alda/alda"}

  jar     {:file "alda.jar"
           :main 'alda.Client}

  exe     {:name      'alda
           :main      'alda.Client
           :version   (exe-version alda.version/-version-)
           :desc      "A music programming language for musicians"
           :copyright "2016 Dave Yarwood et al"}

  target  {:dir #{"target"}}

  test    {:namespaces '#{
                          ; ; general tests
                          ; alda.parser.barlines-test
                          ; alda.parser.clj-exprs-test
                          ; alda.parser.event-sequences-test
                          ; alda.parser.comments-test
                          ; alda.parser.duration-test
                          ; alda.parser.events-test
                          ; alda.parser.octaves-test
                          ; alda.parser.repeats-test
                          ; alda.parser.score-test
                          alda.parser.variables-test
                          ; alda.lisp.attributes-test
                          ; alda.lisp.cram-test
                          ; alda.lisp.chords-test
                          ; alda.lisp.duration-test
                          ; alda.lisp.global-attributes-test
                          ; alda.lisp.markers-test
                          ; alda.lisp.notes-test
                          ; alda.lisp.parts-test
                          ; alda.lisp.pitch-test
                          ; alda.lisp.score-test
                          ; alda.lisp.voices-test
                          ; alda.util-test

                          ; ; benchmarks / smoke tests
                          ; alda.parser.examples-test
                          }})

(deftask dev
  "Runs the Alda server for development.

   There is a middleware that reloads all the server namespaces before each
   request, so that the server does not need to be restarted after making
   changes.

   The -F/--alda-fingerprint option technically does nothing, but including it
   as a long-style option when running this task from the command line* allows
   the Alda client to identify the dev server process as an Alda server and
   include it in the list of running servers.

   * e.g.: boot dev --port 27713 --alda-fingerprint

   Take care to include the --port long option as well, so the client knows
   the port on which the dev server is running."
  [p port             PORT int  "The port on which to start the server."
   F alda-fingerprint      bool "Allow the Alda client to identify this as an Alda server."]
  (comp
    (with-pre-wrap fs
      (require 'alda.server)
      (require 'alda.util)
      ((resolve 'alda.util/set-timbre-level!) :debug)
      ((resolve 'alda.server/start-server!) (or port 27713))
      fs)
    (wait)))

(deftask package
  "Builds an uberjar."
  []
  (comp (javac) (pom) (uber) (jar)))

(deftask build
  "Builds an uberjar and executable binaries for Unix/Linux and Windows."
  [f file       PATH file "The path to an already-built uberjar."
   o output-dir PATH str  "The directory in which to places the binaries."]
  (comp
    (if-not file (package) identity)
    (bin :file file :output-dir output-dir)
    (exe :file file :output-dir output-dir)))

(deftask deploy
  "Builds uberjar, installs it to local Maven repo, and deploys it to Clojars."
  []
  (comp (build-jar) (push-release)))

