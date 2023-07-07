(library (borsch)
   (export
      define*

      any->string
      string-empty?
      string-split
      string-remove-nl
      string-index
      string-contains?
      string-trim-left
      string-trim-right
      string-trim
      string-pad-right
      string-join

      first
      second
      plist-get
      plist-put
      plist-for-each
      list-empty?
      make-stack
      stack-empty?
      stack-push!
      stack-top
      stack-list
      stack-remove!
      stack-pop!
      add-to-list

      bit
      count-digits-num
      try
      error->string
      call-foreign
      while
      run-hooks
      add-hook
      remove-hook

      file-is-directory?
      file-is-regular?
      file-is-link?
      file-is-regular/link?
      file-delete
      file-copy 
      file-mkdir 
      file-list
      file-find
      file-delete-recursive 
      file>
      file>>
      file-is-executable?

      timer-set-time
      timer-set-interval
      make-timer
      timer-delete)
   (import
      (borsch keyword)
      (borsch strings)
      (borsch lists)
      (borsch base)
      (borsch file)
      (borsch timer)))
