(module awful-blog
        (collect-entries define-entry-page index-url 
         entry->string entry-title entry-tags entry-type entry-url entry-resource entry-extra ; entry related procedures
         text-entry->sxml markdown-entry->sxml entry->sxml/default ; entry conversion procedures
         entry->sxml entries-dir entries-info-extension default-text-file-extension default-markdown-file-extension) ; parameters

        (import chicken scheme data-structures files)
        (use awful posix matchable srfi-13 utils srfi-1 traversal lowdown html-tags html-utils)

        (enable-sxml #t)
        (generate-sxml? #t)

        (define entries-dir                     (make-parameter "articles/"))
        (define entries-info-extension          (make-parameter "info"))
        (define default-text-file-extension     (make-parameter ""))
        (define default-markdown-file-extension (make-parameter "md"))

        (define-record entry title tags url type resource extra)

        (define (redirect-entry? entry)
          (eq? (entry-type entry) 'redirect))

        (define (entry->string entry)
          (format "<#entry title: ~a, tags: ~a, type: ~a, url: ~a, resource: ~a, extra: ~a>" 
                  (entry-title entry) 
                  (entry-tags  entry) 
                  (entry-type  entry) 
                  (entry-url   entry)
                  (entry-resource  entry)
                  (entry-extra entry)))

        (define (invalid-entry-error entry msg)
          (error (format "invalid entry: ~a (~a)" msg (entry->string entry))))

        (define (validate-entry entry)
          (cond ((not (entry-type entry))
                 (invalid-entry-error entry "type is required"))
                ((not (entry-title entry))
                 (invalid-entry-error entry "title is required"))
                ((and (not (redirect-entry? entry))
                      (not (entry-url entry)))
                 (invalid-entry-error entry "url is required"))
                (#t
                 entry)))

        (define (append-file-extension file extension)
          (if (string=? "" extension)
            file
            (string-append file "." extension)))

        (define (make-entry-from-info-file info-file)
          (let* ((data          (with-input-from-file info-file (lambda () (read))))
                 (info-filename (pathname-file info-file))
                 (entry         (make-entry #f '() #f #f #f '())))
            (for-each (match-lambda 
                        (('title title)
                         (entry-title-set! entry title))
                        (('tags tags ...)
                         (entry-tags-set! entry tags))
                        (('url url)
                         (entry-url-set! entry url))
                        (('type type)
                         (entry-type-set! entry type))
                        (('resource resource)
                         (entry-resource-set! entry resource))
                        (extra
                          (entry-extra-set! entry extra)))
                      data)
            (unless (entry-resource entry)
              (entry-resource-set! entry 
                                   (case (entry-type entry)
                                     ((text)
                                      (append-file-extension info-filename (default-text-file-extension)))
                                     ((markdown)
                                      (append-file-extension info-filename (default-markdown-file-extension)))
                                     (else
                                       (invalid-entry-error entry "resource is required")))))
            (validate-entry entry)))

        (define (collect-entries)
          (find-files (entries-dir)
                      #:test   (string-append ".*\\." (entries-info-extension))
                      #:seed   '()
                      #:action (lambda (info-file files)
                                 (cons (make-entry-from-info-file info-file) files))))

        (define (text-entry->sxml entry)
          (let* ((file    (make-pathname (entries-dir) (entry-resource entry)))
                 (content (with-input-from-file file (lambda () (read-all))))
                 (title   (entry-title entry)))
            `((div (@ (class "page-header"))
                   (h1 ,title))
              ,(map (lambda (line)
                      (list line '(br)))
                    (string-split content "\n" #t)))))

        (define (markdown-entry->sxml entry)
          (let* ((file    (make-pathname (entries-dir) (entry-resource entry)))
                 (content (with-input-from-file file (lambda () (markdown->sxml (current-input-port)))))
                 (title   (entry-title entry)))
            content))

        (define (entry->sxml/default entry)
          (case (entry-type entry)
            ((text)
             (text-entry->sxml entry))
            ((markdown)
             (markdown-entry->sxml entry))
            (else
              (error 'entry->sxml (format "entry type ~a not supported" (entry-type entry))))))

        (define entry->sxml (make-parameter entry->sxml/default))

        (define (define-entry-page mount-url entry . args)
          (if (not (redirect-entry? entry))
            (let* ((path       (make-pathname mount-url (entry-url entry)))
                   (extra-args (if (not (member 'title: args))
                                 (cons* 'title: (entry-title entry) args)
                                 args)))
              (apply define-page 
                     (cons* path
                            (lambda () 
                              ((entry->sxml) entry)) args)))))

        (define (index-url mount-url entry)
          (if (redirect-entry? entry)
            (entry-resource entry)
            (make-pathname mount-url (entry-url entry))))

); end module
