(define-mode gnumake-mode "GNU Make" text-mode
   (syntax-set-lang 'gnumake)
)

(add-to-list 'file-match-mode '("GNUMakefile|Makefile|.*\\.mk$|.*\\.make$" . gnumake-mode))

(define gnumake-syntax-bracket-match
   "[
    \"(\"
    \")\"
    \"{\"
    \"}\"
   ] @punctuation.bracket"
)

(define gnumake-syntax-delimiter-match
   "[
    \":\"
    \"&:\"
    \"::\"
    \"|\"
    \";\"
    \"\"\"
    \"'\"
    \",\"
   ] @punctuation.delimiter"
)

(define gnumake-syntax-special-match
   "[
    \"$\"
    \"$$\"
   ] @punctuation.special"
)

(define gnumake-syntax-special-match2
   "(automatic_variable
       [ \"@\" \"%\" \"<\" \"?\" \"^\" \"+\" \"/\" \"*\" \"D\" \"F\"]
       @punctuation.special)"
)

(define gnumake-syntax-error-match
   "(automatic_variable \"/\" @error . [\"D\" \"F\"])"
)

(define gnumake-syntax-operator-match
   "[
    \"=\"
    \":=\"
    \"::=\"
    \"?=\"
    \"+=\"
    \"!=\"
    \"@\"
    \"-\"
    \"+\"
   ] @operator"
)

(define gnumake-syntax-string-match
   "[
    (text)
    (string)
    (raw_text)
   ] @string"
)

(define gnumake-syntax-string-match2 "(variable_assignment (word) @string)")

(define gnumake-syntax-conditional-match
   "[
    \"ifeq\"
    \"ifneq\"
    \"ifdef\"
    \"ifndef\"
    \"else\"
    \"endif\"
    \"if\"
    \"or\"
    \"and\"
   ] @conditional"
)

(define gnumake-syntax-loop-match "\"foreach\" @loop")

(define gnumake-syntax-keyword-match
   "[
    \"define\"
    \"endef\"
    \"vpath\"
    \"undefine\"
    \"export\"
    \"unexport\"
    \"override\"
    \"private\"
   ] @keyword"
)

(define gnumake-syntax-include-match
   "[
    \"include\"
    \"sinclude\"
    \"-include\"
   ] @include"
)

(define gnumake-syntax-builtin-function-match
   "[
    \"subst\"
    \"patsubst\"
    \"strip\"
    \"findstring\"
    \"filter\"
    \"filter-out\"
    \"sort\"
    \"word\"
    \"words\"
    \"wordlist\"
    \"firstword\"
    \"lastword\"
    \"dir\"
    \"notdir\"
    \"suffix\"
    \"basename\"
    \"addsuffix\"
    \"addprefix\"
    \"join\"
    \"wildcard\"
    \"realpath\"
    \"abspath\"
    \"call\"
    \"eval\"
    \"file\"
    \"value\"
    \"shell\"
   ] @keyword.function"
)

(define gnumake-syntax-exception-match
   "[
    \"error\"
    \"warning\"
    \"info\"
   ] @exception"
)

;; Variable
(define gnumake-syntax-variable-assign-match 
   "(variable_assignment
     name: (word) @constant)"
)

(define gnumake-syntax-variable-ref-match
   "(variable_reference
     (word) @constant)"
)

(define gnumake-syntax-comment-match "(comment) @comment")

(define gnumake-syntax-regex-match
   "((word) @clean @string.regex
    (#match? @clean \"[%\\*\\?]\"))"
)

(define gnumake-syntax-text-danger-match
   "(function_call
     function: \"error\"
     (arguments (text) @text.danger))"
)

(define gnumake-syntax-text-warning-match
   "(function_call
     function: \"warning\"
     (arguments (text) @text.warning))"
)

(define gnumake-syntax-text-node-match
   "(function_call
     function: \"info\"
     (arguments (text) @text.note))"
)

;; Install Command Categories
;; Others special variables
;; Variables Used by Implicit Rules
(define gnumake-syntax-builtin-variable-match
   "[
    \"VPATH\"
    \".RECIPEPREFIX\"
   ] @constant.builtin"
)

(define gnumake-syntax-builtin-variable-assign-match
   "(variable_assignment
     name: (word) @clean @constant.builtin
        (#match? @clean \"^(AR|AS|CC|CXX|CPP|FC|M2C|PC|CO|GET|LEX|YACC|LINT|MAKEINFO|TEX|TEXI2DVI|WEAVE|CWEAVE|TANGLE|CTANGLE|RM|ARFLAGS|ASFLAGS|CFLAGS|CXXFLAGS|COFLAGS|CPPFLAGS|FFLAGS|GFLAGS|LDFLAGS|LDLIBS|LFLAGS|YFLAGS|PFLAGS|RFLAGS|LINTFLAGS|PRE_INSTALL|POST_INSTALL|NORMAL_INSTALL|PRE_UNINSTALL|POST_UNINSTALL|NORMAL_UNINSTALL|MAKEFILE_LIST|MAKE_RESTARTS|MAKE_TERMOUT|MAKE_TERMERR|\\.DEFAULT_GOAL|\\.RECIPEPREFIX|\\.EXTRA_PREREQS)$\"))"
)

(define gnumake-syntax-builtin-variable-ref-match
   "(variable_reference
     (word) @clean @constant.builtin
     (#match? @clean \"^(AR|AS|CC|CXX|CPP|FC|M2C|PC|CO|GET|LEX|YACC|LINT|MAKEINFO|TEX|TEXI2DVI|WEAVE|CWEAVE|TANGLE|CTANGLE|RM|ARFLAGS|ASFLAGS|CFLAGS|CXXFLAGS|COFLAGS|CPPFLAGS|FFLAGS|GFLAGS|LDFLAGS|LDLIBS|LFLAGS|YFLAGS|PFLAGS|RFLAGS|LINTFLAGS|PRE_INSTALL|POST_INSTALL|NORMAL_INSTALL|PRE_UNINSTALL|POST_UNINSTALL|NORMAL_UNINSTALL|MAKEFILE_LIST|MAKE_RESTARTS|MAKE_TERMOUT|MAKE_TERMERR|\\.DEFAULT_GOAL|\\.RECIPEPREFIX|\\.EXTRA_PREREQS\\.VARIABLES|\\.FEATURES|\\.INCLUDE_DIRS|\\.LOADED)$\"))"
)

;; Standart targets
(define gnumake-syntax-target-standart-match
   "(targets
     (word) @constant.macro
     (#match? @constant.macro \"^(all|install|install-html|install-dvi|install-pdf|install-ps|uninstall|install-strip|clean|distclean|mostlyclean|maintainer-clean|TAGS|info|dvi|html|pdf|ps|dist|check|installcheck|installdirs)$\"))"
)

;; Builtin targets
(define gnumake-syntax-target-builtin-match
   "(targets
     (word) @constant.macro
     (#match? @constant.macro \"^\\.(PHONY|SUFFIXES|DEFAULT|PRECIOUS|INTERMEDIATE|SECONDARY|SECONDEXPANSION|DELETE_ON_ERROR|IGNORE|LOW_RESOLUTION_TIME|SILENT|EXPORT_ALL_VARIABLES|NOTPARALLEL|ONESHELL|POSIX)$\"))"
)

(define gnumake-syntax-bracket-style          '(fg: "yellow"))
(define gnumake-syntax-special-style          '(fg: "bright-yellow" attr: "bold"))
(define gnumake-syntax-operator-style         '(fg: "green"))
(define gnumake-syntax-string-style           '(fg: "bright-yellow"))
(define gnumake-syntax-conditional-style      '(fg: "green"))
(define gnumake-syntax-loop-style             '(fg: "green"))
(define gnumake-syntax-keyword-style          '(fg: "green"))
(define gnumake-syntax-include-style          '(fg: "green"))
(define gnumake-syntax-builtin-function-style '(fg: "magenta"))
(define gnumake-syntax-comment-style          '(fg: "bright-black"))
(define gnumake-syntax-target-style           '(fg: "blue"))
(define gnumake-syntax-variable-style         '(fg: "cyan"))
(define gnumake-syntax-exception-style        '(fg: "magenta"))

(syntax-set-style 'gnumake gnumake-syntax-bracket-match gnumake-syntax-bracket-style)
(syntax-set-style 'gnumake gnumake-syntax-special-match gnumake-syntax-special-style)
(syntax-set-style 'gnumake gnumake-syntax-special-match2 gnumake-syntax-special-style)
(syntax-set-style 'gnumake gnumake-syntax-operator-match gnumake-syntax-operator-style)
(syntax-set-style 'gnumake gnumake-syntax-string-match gnumake-syntax-string-style)
;;(syntax-set-style 'gnumake gnumake-syntax-string-match2 gnumake-syntax-string-style)
(syntax-set-style 'gnumake gnumake-syntax-conditional-match gnumake-syntax-conditional-style)
(syntax-set-style 'gnumake gnumake-syntax-loop-match gnumake-syntax-loop-style)
(syntax-set-style 'gnumake gnumake-syntax-keyword-match gnumake-syntax-keyword-style)
(syntax-set-style 'gnumake gnumake-syntax-include-match gnumake-syntax-include-style)
(syntax-set-style 'gnumake gnumake-syntax-builtin-function-match gnumake-syntax-builtin-function-style)
(syntax-set-style 'gnumake gnumake-syntax-comment-match gnumake-syntax-comment-style)
(syntax-set-style 'gnumake gnumake-syntax-target-standart-match gnumake-syntax-target-style)
(syntax-set-style 'gnumake gnumake-syntax-target-builtin-match gnumake-syntax-target-style)
(syntax-set-style 'gnumake gnumake-syntax-variable-assign-match gnumake-syntax-variable-style)
(syntax-set-style 'gnumake gnumake-syntax-variable-ref-match gnumake-syntax-variable-style)
(syntax-set-style 'gnumake gnumake-syntax-exception-match gnumake-syntax-exception-style)
