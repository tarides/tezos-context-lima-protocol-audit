(use-modules (guix packages)
	     (guix git)
	     (guix git-download)
	     (guix gexp)
	     (guix build-system dune)
	     ((guix licenses) #:prefix license:)
	     (gnu packages certs)
	     (gnu packages ocaml)
	     (gnu packages maths)
	     (gnu packages benchmark)
	     (tarides packages ocaml)
	     (tarides packages tezos)
	     (tarides packages irmin))

;; Define the Git URL, version and commit of Irmin to use

(define irmin-url "https://github.com/adatario/irmin")
(define irmin-version "3.7.0-tclpa") ; this is only used for human readability
(define irmin-commit "789761f7ce2e8d28a64d41eed8e533f292a1476b")
(define irmin-sha256 "06081k28nbxxcprxh90p0n1lkjqig8ria4bcrrygxbx0z9lgp6xi")

;; Use a local checkout for applying hacks to Irmin faster
(define irmin-use-local-checkout #f)

(define add-landmarks-to-irmin
  (package-input-rewriting/spec
   `(("ocaml-irmin"
      . ,(lambda (p) (package
		      (inherit p)
		      (propagated-inputs
		       (modify-inputs (package-propagated-inputs p)
				      (append ocaml-landmarks)))))))))

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

(add-landmarks-to-irmin
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
	   ocaml-tezos-context-trace

	   ocaml-landmarks))
    (native-inputs (list gnuplot fio))
    (synopsis #f)
    (description #f)
    (license license:isc))

   ;; Apply patch that exposes Irmin stats to Tezos Context
   #:patches (list
	      (local-file "./patches/tezos-context-add-irmin-stats.patch")
	      (local-file
	       "./patches/tezos-context-expose-irmin-pack-unix-stats.patch")))
 
  #:origin (if irmin-use-local-checkout
	       (git-checkout (url "/home/adatario/dev/irmin/worktrees/tclpa"))
	       (origin
		(method git-fetch)
		(uri (git-reference
		      (url irmin-url)
		      (commit irmin-commit)))
		(sha256 (base32 irmin-sha256))))
  #:version irmin-version))
