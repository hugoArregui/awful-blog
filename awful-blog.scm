(module awful-blog
	*
        ; (define-entry-page index-url 
        ;  make-entry entry->string entry-title entry-tags entry-type entry-url entry-resource entry-extra 
        ;  entry-extra-get ; entry related procedures
        ;  text-entry->sxml markdown-entry->sxml entry->sxml/default ; entry conversion procedures
        ;  entry->sxml entries-dir entries-info-extension default-file-extensions) ; parameters

        (import chicken scheme data-structures files)
        (use awful posix matchable srfi-13 utils srfi-1 traversal lowdown html-tags html-utils html-parser)
        (use (only sequences all?))
        (use-for-syntax files srfi-13)

        (enable-sxml #t)

        (define default-file-extensions    (make-parameter `((""     . text)
                                                             ("md"   . markdown)
                                                             ("html" . html)
                                                             ("scm"  . shtml)
                                                             ("tsv"  . tsv))))

        (define-record entry title tags url type resource extra)

	(define (entry-extra-get entry key)
	  (let* ((extra (entry-extra entry))
	  	 (value (assq key extra)))
	    (and value
	    	 (cadr value))))

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

        (define (text-entry->sxml entry)
          (let* ((file    (entry-resource entry))
                 (content (with-input-from-file file read-all))
                 (title   (entry-title entry)))
            `((div (@ (class "page-header"))
                   (h1 ,title))
              ,(map (lambda (line)
                      (list line '(br)))
                    (string-split content "\n" #t)))))

        (define (markdown-entry->sxml entry)
          (let* ((file    (entry-resource entry))
                 (content (with-input-from-file file (cut markdown->sxml (current-input-port))))
                 (title   (entry-title entry)))
            content))

        (define (html-entry->sxml entry)
          (let* ((file    (entry-resource entry))
                 (content (with-input-from-file file html->sxml))
                 (title   (entry-title entry)))
            (cdr content)))

        (define (shtml-entry->sxml entry)
          (let* ((file    (entry-resource entry))
                 (content (with-input-from-file file read))
                 (title   (entry-title entry)))
            (eval content)))

	(define (tsv->sxml entry)
	  (let ((split-by-tab (cut string-split <> "\t" #t))
		(file         (entry-resource entry)))
	    (call-with-values (lambda ()
				(let loop ((lines    (string-split (read-all file) "\n"))
					   (headers  #f)
					   (rows     '()))
				  (cond ((null? lines)
					 (values headers rows))
					((string-prefix? "#" (car lines))
					 (loop (cdr lines) headers rows))
					((not headers)
					 (loop (cdr lines) (split-by-tab (car lines)) rows))
					(#t
					 (let* ((row      (split-by-tab (car lines)))
						(lrow     (length row))
						(lheaders (length headers)))
					   (if (< lrow lheaders)
					     (append! row (make-list (- lheaders lrow))))
					   (loop (cdr lines) headers (cons row rows)))))))
			      (lambda (headers rows)
				`((table
				    (thead
				      (tr
					,(map (lambda (header)
						`(th ,header))
					      headers)))
				    (tbody
				      ,(map (lambda (row)
					      `(tr 
						 ,(map (lambda (cell)
							 `(td ,cell))
						       row)))
					    rows))))))))

        (define (entry->sxml/default entry)
          (case (entry-type entry)
            ((text)
             (text-entry->sxml entry))
            ((markdown)
             (markdown-entry->sxml entry))
            ((html)
             (html-entry->sxml entry))
            ((shtml)
             (shtml-entry->sxml entry))
            ((tsv)
             (tsv->sxml entry))
            (else
              (error 'entry->sxml (format "entry type ~a not supported" (entry-type entry))))))

        (define entry->sxml (make-parameter entry->sxml/default))

        (define (define-entry-page mount-url entry . args)
          (if (not (redirect-entry? entry))
            (let* ((path       (make-pathname mount-url (entry-url entry)))
                   (extra-args (if (not (member 'title: args)) (cons* 'title: (entry-title entry) args)
                                 args)))
              (apply define-page 
                     (cons* path
                            (lambda () 
                              ((entry->sxml) entry)) extra-args)))))

        (define (index-url mount-url entry)
          (if (redirect-entry? entry)
            (entry-resource entry)
            (make-pathname mount-url (entry-url entry))))

	(define (filter-entries-by-tag entries tags)
	  (if (null? tags)
	    entries
	    (filter (lambda (entry)
		      (all? (cut member <> (entry-tags entry)) tags)) entries)))

	(define make-entry 
	  (let ((original    make-entry))
	    (lambda (title url resource #!key (tags '()) (type #f) (extra '()))
	      (let ((type 
		      (cond (type type)
			    ((string-prefix? "http" resource)
			     'redirect)
			    ((assoc (or (pathname-extension resource) "") 
				    (default-file-extensions)) => cdr)
			    (#t
			     (error (string-append "cannot found type for: " resource))))))
		(original title tags url type resource extra)))))


	(define-for-syntax (lookup-property properties property #!key default)
	  (let ((r (member property properties)))
	    (if r
	      (cadr r)
	      default)))

	(define-for-syntax (url-join base relative)
	  (let ((separator (if (or (string-prefix? "/" relative)
				   (string-suffix? "/" base))
			     ""
			     "/")))
	    (string-append base separator relative)))

	(define-syntax branch
	  (er-macro-transformer
	    (lambda (x r c)
	      (let* ((childs       (cadr x))
		     (props        (cddr x))
		     (%make-entry  (r 'make-entry))
		     (%list        (r 'list))
		     (%append      (r 'append))
		     (base-dir     (lookup-property props 'base-dir: default: ""))
		     (base-url     (lookup-property props 'base-url: default: ""))
		     (tags         (lookup-property props 'tags: default: '())))
		(let loop ((childs   childs)
			   (entries  '())
			   (branches '()))
		  (if (null? childs)
		    (if (null? branches)
		      `(,%list ,@entries)
		      `(,%append (,%list ,@entries) ,@branches))
		    (let* ((child  (car childs))
			   (lookup (lambda (property #!key default)
				     (lookup-property child property default: default))))
		      (if (eq? (car child) 'entry)
			(loop (cdr childs) 
			      (cons `(,%make-entry ,(lookup 'title:)
						   ,(url-join base-url (lookup 'url:))
						   ,(make-pathname base-dir (lookup 'resource:))
						   tags: ,(cons %list (append tags (lookup 'tags: default: '())))
						   extra: ,(cons %list (lookup 'extra: default: '()))
						   type: ,(lookup 'type: default: #f))
				    entries)
			      branches)
			(loop (cdr childs) 
			      entries 
			      (cons `(branch
				       ,(cadr child)
				       base-dir: ,(make-pathname base-dir (lookup 'base-dir: default: ""))
				       base-url: ,(url-join base-url (lookup 'base-url: default: ""))
				       tags: ,(append tags (lookup 'tags: default: '())))
				    branches))))))))))
); end module
