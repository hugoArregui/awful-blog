# awful-blog 

A blog egg for Awful. A blog is a just a collection of entries. A new entry can be created using the make-entry procedure:

	[procedure] (make-entry (title resource #!key url tags type extra))

-  title: entry title
-  resource: if type == redirect, resource is the redirect url,
   otherwise it should point to a file.
-  url: url in which the entry will be mounted
-  tags: entry tags (usually, a list of symbols)
-  type: types supported by default are: text, markdown, html, redirect, shtml
-  extra: a list extra data


## Index

A very common use case (for me at least) is to define a scm file with an
"index", that is: a scheme file where the entries are statically defined.

There is a syntax sugar to do that:

    [macro] (branch ((entry title: <title> url: <url> resource: <resource> | branch ) ...)
                    [base-dir: <dir>] [base-url: <url>] [tags: <tags])

The branch macro defines a list of entries and/or sub-branches. The branch
properties (base-dir, base-url and tags) are inherited from their childs.

Example:

```scheme
(let ((entries (branch
                 ((entry title:    "Something in markdown format!!" 
                         url:      "/some" 
                         resource: "some.md")
                  (entry title:    "pacman"
                         url:      "/pacman" 
                         resource: "pacman"
                         tags: ('arch))
                  (entry title:    "Read the docs!"
                         resource: "http://api.call-cc.org/doc/"
                         tags: ('chicken))
                  (entry title:    "List"
                         url:      "/list"
                         resource: "list.tsv")
                  (branch
                    ((entry title:    "Mitopoeia"
                            url:      "/mitopoeia" 
                            resource: "mitopoeia.html")
                     (entry title:    "Literature" 
                            url:      "/literature" 
                            resource: "literature" 
                            tags: ('wikipedia)))
                    tags: ('literature)))
                 base-dir: "notes/")))
```

Here a parent branch defines the base url for the entries "notes/", but also
a second one is defined with the tag "literature", to avoid repeating the tag in
each entry.

For more info, check the example into "example" dir.

## API specification 

### Records 

    [record] (entry title tags url type resource extra)

### Parameters

#### default-file-extension

    [parameter] default-file-extension 

Alist containing mappings file-extension -> type.

####  entry->sxml 

    [parameter] entry->sxml

entry->sxml conversion procedure, default: entry->sxml/default 

### Procedures

#### make-entry

	[procedure] (make-entry (title resource #!key url tags type extra))

Creates a new entry

#### define-entry-page 

    [procedure] (define-entry-page mount-url entry)

Defines an awful page from an entry. 

#### filter-entries-by-tag

    [procedure] (filter-entries-by-tag entries tags)

#### index-url 

    [procedure] (index-url mount-url entry)

Returns the entry url at which the index should point to.

#### text-entry->sxml 

    [procedure] (text-entry->sxml entry)

#### markdown-entry->sxml 

    [procedure] (markdown-entry->sxml entry)

#### entry->sxml/default 

    [procedure] (entry->sxml/default entry)
    
#### html-entry->sxml

    [procedure] (html-entry->sxml entry)

#### shtml-entry->sxml

    [procedure] (shtml-entry->sxml entry)

#### tsv->sxml

    [procedure] (tsv->sxml entry)

### Macros

#### branch

    [macro] (branch ((entry title: <title> url: <url> resource: <resource> | branch ) ...)
                    [base-dir: <dir>] [base-url: <url>] [tags: <tags])
