(library (borsch)
   (export
      bind-key
      unbind-key
      global-keymap
      make-empty-keymap
      make-keymap
      keymap-set-parent

      do-quit
      message

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
      buffer-save
      buffer-filename
      buffer-set-filename
      buffer-env
      local-symbol-bound?
      get-local-symbol
      set-local-symbol!
      get-local
      set-local!
      local-bound?
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
      buffer-get

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
      syntax-highlight
      
      cursor
      cursor-set
      with-saved-cursor
      cursor-to-next-char
      cursor-to-prev-char
      cursor-to-next-word
      cursor-to-prev-word
      cursor-to-word-end
      cursor-to-next-longword
      cursor-to-prev-longword
      cursor-to-longword-end
      cursor-to-line
      cursor-to-line-up
      cursor-to-line-down
      cursor-to-next-line
      cursor-to-prev-line-end
      cursor-to-line-start
      cursor-to-line-finish
      cursor-to-line-begin
      cursor-to-line-end
      cursor-to-begin
      cursor-to-end
      is-last-line?
      cursor-to-each-line
      text-modify
      text-insert
      text-append
      text-insert-char
      text-insert-nl
      text-insert-empty-line-up
      text-insert-empty-line
      text-insert-file
      text-end-pos
      text-begin-pos
      text-line-end-pos
      text-line-begin-pos
      text-line-finish-pos
      text-line-start-pos
      text-prev-line-end-pos
      text-next-line-begin-pos
      text-next-line-pos
      text-prev-line-pos
      text-longword-end-pos
      text-prev-longword-pos
      text-next-longword-pos
      text-word-end-pos
      text-prev-word-pos
      text-next-word-pos
      text-prev-char-pos
      text-next-char-pos)
   (import
      (borsch keymap)
      (borsch runtime)
      (borsch keyword)
      (borsch strings)
      (borsch lists)
      (borsch base)
      (borsch file)
      (borsch timer)
      (borsch buffer)
      (borsch style)
      (borsch syntax)
      (borsch text)))
