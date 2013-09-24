(use posix setup-api)
(use http-client test intarweb uri-common awful)

(define server-uri "http://localhost:8080")

(define (get path/vars)
  (let ((val (with-input-from-request
              (make-pathname server-uri path/vars)
              #f
              read-string)))
    (close-all-connections!)
    val))

(define (expect/text title content)
  (sprintf "\n<div class=\"page-header\">\n<h1>~a</h1></div>~a\n<br />\n<br />"
           title content))

(define (expect/markdown content)
  (sprintf "\n<p>~a</p>" content))

(test-begin "awful-blog")

(pp (get "/blog/"))

(test (expect/text "foo" "this is foo")
      (get "/blog/foo"))

(test (expect/markdown "this is bar")
      (get "/blog/bar"))

(test-end "awful-blog")
