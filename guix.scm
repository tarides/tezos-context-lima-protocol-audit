(use-modules (guix packages)
	     (guix git)
	     (guix git-download)
	     (guix gexp)
	     (guix build-system dune)
	     ((guix licenses) #:prefix license:)
	     (gnu packages certs)
	     (gnu packages ocaml)
	     (tarides packages ocaml)
	     (tarides packages tezos)
	     (tarides packages irmin))

;; Define the Git URL, version and commit of Irmin to use

(define irmin-url "https://github.com/adatario/irmin")
(define irmin-version "3.7.0-io-stats") ; this is only used for human readability
(define irmin-commit "2df6ebfcb4194d0548eae9e32d2dea16fc2c0c92")
(define irmin-sha256 "1qkja1wn7bajyfrzwyhx1h0z3676j51289x9yjfv89r24p53f5bf")

(define ocaml-tezos-context-trace
  (let ((commit "856c361797cd2dea90f1af8925fe97e8010624df")
	(revision "0"))
    (package
      (name "ocaml-tezos-context-trace")
      (version (git-version "git" revision commit))
      (home-page "https://github.com/adatario/tezos-context-trace")
      (source
       (origin
	 (method git-fetch)
	 (uri (git-reference
	       (url home-page)
	       (commit commit)))
	 (sha256
	  (base32
	   "02mxvgs0x8x1l9h8lqcdaj58b4svv7b3xcvi476ny1lhjwhw0x53"))))
      (build-system dune-build-system)
      ;; Only build the tezos-context-trace package. We don't need the
      ;; recorder shim and tools. They also won't build with the patches we
      ;; apply on Tezos.
      (arguments `(#:package "tezos-context-trace"))
      (propagated-inputs
       (list

	;; tezos-contest (aka lib_context)
	ocaml-tezos-context

	;; Extra dependencies required by the replay patches
	ocaml-ppx-deriving
	ocaml-ppx-deriving-yojson
	ocaml-printbox
	ocaml-bentov

	;; The manage_actions tool makes calls to tzstats.com and
	;; requires SSL certs
	nss-certs))
      (synopsis "Tezos Context Trace tools.")
      (description "Tools that allow replaying of Tezos Context action
traces.  This is used to benchmark performance of changes to Irmin.")
      (license license:isc))))

(package-with-irmin-3.7
 (package-with-tezos-16
  (package
    (name "ocaml-tezos-lima-performance-audit")
    (version "2023.05.01-dev")
    (home-page "https://github.com/adatario/tezos-context-trace")
    (source (git-checkout (url (dirname (current-filename)))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-tezos-context
	   ocaml-tezos-context-trace))
    (synopsis #f)
    (description #f)
    (license license:isc))

  ;; Apply patch that exposes Irmin stats to Tezos Context
  #:patches (list
	     (local-file "./patches/tezos-context-add-irmin-stats.patch")
	     (local-file
	      "./patches/tezos-context-expose-irmin-pack-unix-stats.patch")))
 
 #:origin (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url irmin-url)
		  (commit irmin-commit)))
	    (sha256 (base32 irmin-sha256)))
 #:version irmin-version)
