(use awful awful-blog traversal html-tags)
(use html-tags html-utils)

(enable-sxml #t)
(generate-sxml? #t)
(page-charset "utf-8")
(page-template (lambda (content . rest) content))

(define (wrap-content content)
  content)

(entry->sxml (lambda (entry)
               (wrap-content (entry->sxml/default entry))))

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
  (let* ((entries (branch
                    ((entry title:    "bar"
                            url:      "bar"
                            resource: "bar"
                            type:     'markdown
                            tags:     ('bar))
                     (entry title:    "foo"
                            url:      "foo"
                            resource: "foo"
                            tags:     ('foo))
                     (entry title:    "hh"
                            url:      "hh"
                            resource: "hh.html"
                            tags:     ('hh)))
                   base-dir: "notes/")))
    (define-index-page url entries)
    (for-each (cut define-entry-page url <>) entries)))

(define-blog-pages "/blog")
