(lang dune 3.19)

(name unto)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name unto)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   (ocaml :>=5.2)
   (core :=v0.16.2)
   (core_unix :=v0.16.0)
   (dotenv :=0.0.3)
   (dream :=1.0.0~alpha8)
   (dream-html :=3.10.1)
   (ezcurl :=0.2.4)
   (lz4 :=1.3.0)
   (safepass :=3.1)
   (sqlite3 :=5.3.1)
   (sqlgg :=20231201)
   (uri :=4.4.0)
   (yojson :=2.2.2)
   (ppx_jane :=v0.16.0)
   (ppx_inline_test :=v0.16.1)
   (ppx_expect :=v0.16.1)
   (ppx_deriving :=6.0.3)
   (ppx_sexp_message :=v0.16.0)
   (ppx_yojson_conv :=v0.16.0)
   (ppx_fields_conv :=v0.16.0)
   (ppx_let :>=v0.16.0)
   )
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
