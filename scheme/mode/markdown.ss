(define-mode markdown-mode "Markdown" text-mode
   (syntax-set-lang 'markdown)
)

(file-match-mode-add '(".*\\.md$" . markdown-mode))


(syntax-highlight 'markdown
"
(atx_heading (inline) @title)
(setext_heading (paragraph) @title)

[
  (atx_h1_marker)
  (atx_h2_marker)
  (atx_h3_marker)
  (atx_h4_marker)
  (atx_h5_marker)
  (atx_h6_marker)
  (setext_h1_underline)
  (setext_h2_underline)
] @symbol

[
  (link_title)
  (indented_code_block)
  (fenced_code_block)
] @string

[
  (fenced_code_block_delimiter)
] @delimiter

(code_fence_content) @comment

[
  (link_destination)
] @uri

[
  (link_label)
] @reference

[
  (list_marker_plus)
  (list_marker_minus)
  (list_marker_star)
  (list_marker_dot)
  (list_marker_parenthesis)
  (thematic_break)
] @symbol

[
  (block_continuation)
  (block_quote_marker)
] @comment

[
  (backslash_escape)
] @escape
"
)
