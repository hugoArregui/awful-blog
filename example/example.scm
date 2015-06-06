(use awful awful-blog traversal html-tags srfi-13)
(use html-tags html-utils)

(enable-sxml #t)
(generate-sxml? #t)
(page-charset "utf-8")


(when (development-mode?)
  (add-request-handler-hook!
    'reload-on-request (lambda (path handler)
                         (reload-apps (awful-apps))
                         (handler))))


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
  (let ((entries (branch
                   ((entry title:    "Something in markdown format!!" 
                           url:      "/some" 
                           resource: "some.md")
                    (entry title:    "pacman"
                           url:      "/pacman" 
                           resource: "pacman"
                           tags: ('arch))
                    (entry title:    "Mitopoeia"
                           url:      "/mitopoeia" 
                           resource: "mitopoeia.html"
                           tags: ('literature))
                    (entry title:    "Read the docs!"
                           resource: "http://api.call-cc.org/doc/"
                           tags: ('chicken))
                    (entry title:    "List"
                           url:      "/list"
                           resource: "list.tsv")
                    (entry title:    "Literature" 
                           url:      "/literature" 
                           resource: "literature" 
                           tags: ('literature 'wikipedia)))
                   base-dir: "notes/")))
    (define-index-page url entries)
    (for-each (cut define-entry-page url <>) entries)))

(define-blog-pages "/blog")
