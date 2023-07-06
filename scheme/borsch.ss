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
      remove-hook)
   (import
      (borsch keyword)
      (borsch strings)
      (borsch lists)
      (borsch base)))
