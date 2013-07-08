(use awful awful-blog traversal html-tags)
(use html-tags html-utils)

(enable-sxml #t)
(generate-sxml? #t)
(page-charset "utf-8")


(when (development-mode?)
  (add-request-handler-hook!
    'reload-on-request (lambda (path handler)
                         (reload-apps (awful-apps))
                         (handler))))


(entries-dir "notes/")

(define (wrap-content content)
  content)

(entry->sxml (lambda (entry)
               (wrap-content (entry->sxml/default entry))))

(define (filter-entries-by-tag entries tags)
  (if (null? tags)
    entries
    (filter (lambda (entry)
              (any (lambda (tag)
                     (member tag tags)) (entry-tags entry))) entries)))

(define (define-index-page url entries)
  (define-page url
    (lambda ()
      (let* (($tags   ($ 'tags ""))
             (tags    (map string->symbol (string-split $tags ",")))
             (entries (filter-entries-by-tag entries tags)))
        (wrap-content 
          (<ul>
            (map (lambda (entry)
                   (let ((url  (index-url url entry)))
                     (<li> (link url (entry-title entry)) 
                           " ( " 
                           (map (lambda (tag)
                                          (list (link (string-append "?tags=" $tags "," (symbol->string tag)) tag) " "))
                                        (entry-tags entry))
                           ")")))
                 entries)))))
    title: "Index" ))

(define (define-blog-pages url)
  (let* ((entries (collect-entries)))
    (define-index-page url entries)
    (for-each (cut define-entry-page url <>) entries)))

(define-blog-pages "/blog")
