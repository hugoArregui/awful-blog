(use awful awful-blog traversal html-tags)
(use html-tags html-utils)

(enable-sxml #t)
(generate-sxml? #t)

(entries-dir "notes/")

(define (wrap-content content)
  content)

(entry->sxml (lambda (entry)
               (wrap-content (entry->sxml/default entry))))

(define (define-index-page url entries tags)
  (define-page url
    (lambda ()
      `(ul
         ,(map (lambda (entry)
                 (let ((url (index-url url entry)))
                   `(li (a (@ (href ,url))
                           ,(entry-title entry))
                        " (" ,(string-join (map symbol->string (entry-tags entry)) ", " ) ")" )))
               entries)))
    title: "Index" ))

(define (define-blog-pages url)
  (let* ((entries (collect-entries))
         (tags    (remove-duplicatesq (append-map entry-tags entries))))
    (define-index-page url entries tags)
    (for-each (cut define-entry-page url <>) entries)))

(define-blog-pages "/blog")

(define-page "/test"
  (lambda () 
    `(p "hello"))
  title: "Un test"
  headers: (<meta> http-equiv:"Content-Type" content:"text/html; charset=utf-8"))

