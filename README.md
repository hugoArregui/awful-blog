# awful-blog 

A blog egg for Awful. The idea is very simple: each blog entry has info file in the entries directory (see entries-dir), 
the info file is parsed and loaded into the app. 

The info file:

    ((title <title>)
     (type <text|markdown|redirect>)
     (resource <filename|url>)   ;optional for type != redirect
     (tags <tag ...>)
     (url  <url>))

-  title: entry title
-  type: currently supported by the tool are: text, markdown, redirect
-  resource: if type == redirect, resource is the redirect url,
   otherwise point to a non-default file location. If omitted, awful-blog will
   search for a file with the same name as the info file and the default
   extension (see default-text-file-extension, default-markdown-file-extension)
-  tags: entry tags
-  url: url in which the entry will mounted (normally, after a default mount point)
-  any other data will be stored in the "extra" slot of the entry.

For more info, you could check the example into "example" dir.

## API specification 

### Records 

    [record] (entry title tags url type resource extra)

### Parameters

#### entries-dir 

    [parameter] entries-dir 

Directory for seach entries.

#### entries-info-extension 

    [parameter] entries-info-extension 

File extension for entry files: info.

#### default-text-file-extension 

    [parameter] default-text-file-extension 

Default file extension for text files: "".

#### default-markdown-file-extension 

    [parameter] default-markdown-file-extension

Default file extension for markdown files: "md".
    
####  entry->sxml 

    [parameter] entry->sxml

entry->sxml conversion procedure, default: entry->sxml/default 

### Procedures

#### collect-entries 

    [procedure] (collect-entries)

Returns a list of collected entries from (entries-dir).

#### define-entry-page 

    [procedure] (define-entry-page mount-url entry)

Defines a awful page from an entry. 

#### index-url 

    [procedure] (index-url mount-url entry)

Returns the entry url at which the index should point to.

#### text-entry->sxml 

    [procedure] (text-entry->sxml entry)

#### markdown-entry->sxml 

    [procedure] (markdown-entry->sxml entry)

#### entry->sxml/default 

    [procedure] (entry->sxml/default entry)
    

