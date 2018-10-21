(ns ring-undertow-simple.core
  (:import (io.undertow Undertow)
           (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.util Headers HeaderValues HeaderMap)))

(defn- get-headers
  [^HeaderMap header-map]
  (persistent!
    (reduce
      (fn [headers ^HeaderValues entry]
        (let [k (.. entry getHeaderName toString toLowerCase)
              val (if (> (.size entry) 1)
                    (clojure.string/join "," (iterator-seq (.iterator entry)))
                    (.get entry 0))]
          (assoc! headers k val)))
      (transient {})
      header-map)))

(defn- build-exchange-map
  [^HttpServerExchange exchange]
  (let [headers (.getRequestHeaders exchange)
        ctype (.getFirst headers Headers/CONTENT_TYPE)]
    {:server-port        (-> exchange .getDestinationAddress .getPort)
     :server-name        (-> exchange .getHostName)
     :remote-addr        (-> exchange .getSourceAddress .getAddress .getHostAddress)
     :uri                (-> exchange .getRequestURI)
     :query-string       (-> exchange .getQueryString)
     :scheme             (-> exchange .getRequestScheme .toString .toLowerCase keyword)
     :request-method     (-> exchange .getRequestMethod .toString .toLowerCase keyword)
     :headers            (-> exchange .getRequestHeaders get-headers)
     :content-type       ctype
     :content-length     (-> exchange .getRequestContentLength)
     :character-encoding (or (when ctype (Headers/extractTokenFromHeader ctype "charset"))
                             "ISO-8859-1")
     :body               (.getInputStream exchange)}))

(defn- set-exchange-response
  [^HttpServerExchange exchange {:keys [status headers body]}]
  (when-not exchange
    (throw (Exception. "Null exchange given.")))
  (when status
    (.setResponseCode exchange status))
  (set-headers (.getResponseHeaders exchange) headers)
  (respond body exchange))

(defn- undertow-handler
  "Returns an Undertow HttpHandler implementation for the given Ring handler."
  [handler non-blocking]
  (reify
    HttpHandler
    (handleRequest [_ exchange]
      (when-not non-blocking
        (.startBlocking exchange))
      (let [request-map (build-exchange-map exchange)
            response-map (handler request-map)]
        #_(set-exchange-response exchange response-map)))))

(defn handler [req]
  {:status 200})

(def s (Undertow/builder))
(.addHttpListener s 8090 "localhost")
(.setHandler s (undertow-handler handler false))
(.start (.build s))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main []
  (println "SHIT"))
