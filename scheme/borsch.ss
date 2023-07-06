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
      string-join)
   (import
      (borsch keyword)
      (borsch strings)))
