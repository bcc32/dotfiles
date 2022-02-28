(require 's)

;; TODO: Upstream changes.

(defun bcc32-ocaml-abbrevs ()
  "Define a bunch of useful OCaml abbrevs,
mostly for Jane Street preprocessor stuff, but also for firm conventions."
  (interactive)
  (with-eval-after-load 'tuareg
    (add-hook 'tuareg-mode-hook #'abbrev-mode))
  (bcc32--define-deriving-abbrevs)
  (define-abbrev global-abbrev-table "qdep" ""
    (lambda ()
      (let ((yyyy-mm (format-time-string "%Y-%m")))
        (skeleton-insert (cons nil `("[@@deprecated \"[since ",yyyy-mm"]"_"\"]")))))
    :case-fixed t)
  (define-abbrev global-abbrev-table "qimpl" ""
    (lambda ()
      "If in an ml file, insert a CR and a [raise_s] with the source code position \
to implement something, if not insert 'qimpl'"
      (let ((basename (file-name-nondirectory (buffer-file-name))))
        (insert
         (if (not (string-suffix-p ".ml" basename))
             "qimpl"
           (concat "(" "* CR " (user-login-name) " for " (user-login-name) ": implement *)\nraise_s [%message \"unimplemented\" [%here]]")
           ))))
    :case-fixed t)
  (define-abbrev global-abbrev-table "qmli" ""
    (lambda ()
      "If in [foo.mli], insert [include Foo_intf.Foo (** @inline *)].  \
If not in an [.mli] file, insert [qmli]."
      (let ((basename (file-name-nondirectory (buffer-file-name))))
        (insert
         (if (not (string-suffix-p ".mli" basename))
             "qmli"
           (let ((module-name (s-capitalize (file-name-sans-extension basename))))
             (concat "include "module-name"_intf."module-name" (** @inline *)"))))))
    :case-fixed t)
  (let ((user (user-login-name)))
    (dolist (z `(
                 ("qalc"   "[@alert \"" _ "\"]"                                          )
                 ("qald"   "[@@alert " _ " \"\"]"                                        )
                 ("qb"     "[%bind" _ "]"                                                )
                 ("qbd"    "[%bin_digest:" _ "]"                                         )
                 ("qbo"    "[%bind_open" _ "]"                                           )
                 ("qc"     "[%compare:" _ "]"                                            )
                 ("qce"    "[%compare.equal:" _ "]"                                      )
                 ("qcr"    comment-start "CR " ,user ":" _                    comment-end)
                 ("qcrd"   comment-start "CR-someday " ,user ":" _            comment-end)
                 ("qcrf"   comment-start "CR " ,user " for " ,user ":" _      comment-end)
                 ("qcrs"   comment-start "CR-soon " ,user ":" _               comment-end)
                 ("qcrsf"  comment-start "CR-soon " ,user " for " ,user ":" _ comment-end)
                 ("qd"     "[@default" _ "]"                                             )
                 ("qdd"    "[@default" _ "] [@sexp_drop_default]"                        )
                 ("qdua"   "Delete upon addressing."                                     )
                 ("qdur"   "Delete upon reading."                                        )
                 ("qduw"
                  comment-start "CR " ,user " for " ,user ": re-enable \"unused\" warnings" comment-end "\n"
                  "[@@@disable_unused_warnings]")
                 ("qe"     "[%expect {| |}];"                                            )
                 ("qh"     "[%here]"                                                     )
                 ("qhh"    "~here:[%here]"                                               )
                 ("qim"    "[@@immediate]"                                               )
                 ("qlb"    "let%bind"                                                    )
                 ("qlbe"   "let%bind () = [%expect {| |}] in"                            )
                 ("qlbu"   "let%bind () =" _ " in"                                       )
                 ("qlec"   "let equal = [%compare.equal: t]"                             )
                 ("qlet"   "let%expect_test \"" _ "\" ="                                 )
                 ("qletbd" "\
let%expect_test \"bin digest\" =
  print_endline [%bin_digest: t];
  [%expect {|  |}]
;;")
                 ("qlhh"  "let hash = [%hash: t]"                                        )
                 ("qlm"   "let%map"                                                      )
                 ("qlmo"  "let%map_open"                                                 )
                 ("qloc"  "let open Command.Let_syntax in"                               )
                 ("qlod"  "let open Deferred.Let_syntax in"                              )
                 ("qlol"  "let open List.Let_syntax in"                                  )
                 ("qlomo" "\
let open ?.Option.Optional_syntax in
match%optional " _ " with
| None -> ?
| Some ? -> ?")
                 ("qlop"  "let open Param.Let_syntax in"                                 )
                 ("qlt"   "let%test \"" _ "\" ="                                         )
                 ("qltm"  "let%test_module \"" _ "\" =\n  (module struct\n    \n  end)"  )
                 ("qltu"  "let%test_unit \"" _ "\" ="                                    )
                 ("qma"   "[%map" _ "]"                                                  )
                 ("qmb"   "match%bind" _ "with"                                          )
                 ("qme"   "[%message" _ "]"                                              )
                 ("qmm"   "match%map" _ "with"                                           )
                 ("qmo"   "[%map_open" _ "]"                                             )
                 ("qmon"  "[%message.omit_nil" _ "]"                                     )
                 ("qmopt" "\
match%optional " _ " with
| None -> ?
| Some ? -> ?")
                 ("qobi"   "open! Base\nopen! Import\n"                                  )
                 ("qocai"  "open! Core\nopen! Async\nopen! Import\n"                     )
                 ("qoci"   "open! Core\nopen! Import\n"                                  )
                 ("qocki"  "open! Core_kernel\nopen! Import\n"                           )
                 ("qoi"    "open! Import\n"                                              )
                 ("qon"    "[@omit_nil]"                                                 )
                 ("qos"    "[%of_sexp:" _ "]"                                            )
                 ("qrm"    "raise_s [%message \"" _ "\"]"                                )
                 ("qs"     "[%sexp" _ "]"                                                )
                 ("qsdd"   "[@sexp_drop_default]"                                        )
                 ("qside"  "(*_ This signature is deliberately empty. *)"                )
                 ("qso"    "[%sexp_of:" _ "]"                                            )
                 ("qp"    "|> [%sexp_of:" _ "] |> print_s;"                              )
                 ("qte"    "[%test_eq:" _ "] ? ?"                                        )
                 ("qto"    "[%typerep_of:" _ "]"                                         )
                 ("qtp"    "[%test_pred:" _ "] ? ?"                                      )
                 ("qtr"    "[%test_result:" _ "] ? ~expect:?"                            )
                 ("qwt"    "with type t := t"                                            )
                 ("qwt1"   "with type 'a t := 'a t"                                      )
                 ("qwt2"   "with type ('a, 'b) t := ('a, 'b) t"                          )
                 ("qwt3"   "with type ('a, 'b, 'c) t := ('a, 'b, 'c) t"                  )
                 ;; ppx_log abbrevs
                 ("qlogd"  "[%log.debug log" _ "]"                                       )
                 ("qlogi"  "[%log.info log" _ "]"                                        )
                 ("qloge"  "[%log.error log" _ "]"                                       )
                 ("qloggd" "[%log.global.debug" _ "]"                                    )
                 ("qloggi" "[%log.global.info" _ "]"                                     )
                 ("qlogge" "[%log.global.error" _ "]"                                    )
                 ))
      (if (= 2 (length z))
          (let ((abbrev    (elt z 0))
                (expansion (elt z 1)))
            (define-abbrev global-abbrev-table abbrev expansion nil :case-fixed t))
        (let* ((abbrev    (first z))
               (skeleton  (rest z))
               (expansion "") ;; because we want to insert a skeleton
               (hook      `(lambda () (skeleton-insert (cons nil ',skeleton)))))
          (define-abbrev global-abbrev-table abbrev expansion hook :case-fixed t))))))

(defun bcc32--define-deriving-abbrevs ()
  (bcc32--define-deriving-abbrevs-helper
   nil
   '((("b" "bin_io"))
     (("c" "compare"))
     (("e" "enumerate"))
     (("f" "fields"))
     (("h" "hash"))
     (("p" "python"))
     (("q" "equal"))
     (("s" "sexp") ("so" "sexp_of") ("os" "of_sexp"))
     (("t" "typerep"))
     (("v" "variants"))))
  ;; TODO: Figure out why this line is here.
  (dolist (abbrev '("qdv"))
    (define-abbrev global-abbrev-table abbrev nil nil :case-fixed t)))

(defun bcc32--define-deriving-abbrevs-helper (include consider)
  (pcase consider
    (`(,choices . ,consider)
     (bcc32--define-deriving-abbrevs-helper include consider)
     (dolist (choice choices)
       (bcc32--define-deriving-abbrevs-helper (cons choice include) consider)))
    ('()
     (when include
       (let* ((include (reverse include))
              (abbrev (apply 'concat (cons "qd" (mapcar #'first include))))
              (expansion
               (concat "[@@deriving "
                       (mapconcat #'second include ", ")
                       "]")))
         (define-abbrev global-abbrev-table abbrev expansion nil :case-fixed t))))))
