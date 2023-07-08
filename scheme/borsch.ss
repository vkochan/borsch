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
      current-cwd
      with-current-cwd
      current-cwd-handler

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
      timer-delete

      current-buffer
      with-current-buffer
      buffer-line-num
      buffer-set-mode-name
      buffer-mode-name
      buffer-set-state-name
      buffer-state-name
      buffer-snapshot
      buffer-undo
      buffer-redo
      buffer-filename
      buffer-set-filename
      buffer-env
      define-local
      buffer-name
      buffer-set-name
      buffer-set-readonly
      buffer-is-readonly?
      buffer-is-modified?
      buffer-is-dirty?
      set-text-style
      add-text-property
      remove-text-property
      get-text-property
      set-text-property
      highlight-range
      highlight-clear

      style-add
      style-modify
      define-style
      style-set
      color-name->number
      color-number->name
      style-name->bit
      style-name->number
      style->list

      syntax-set-lang
      syntax-add-style
      syntax-delete-style
      syntax-set-style
      syntax-delete-all-styles
      syntax-highlight)
   (import
      (borsch keyword)
      (borsch strings)
      (borsch lists)
      (borsch base)
      (borsch file)
      (borsch timer)
      (borsch buffer)
      (borsch style)
      (borsch syntax)))
