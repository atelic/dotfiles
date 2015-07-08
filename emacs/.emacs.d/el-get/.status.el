((base16 status "installed" recipe
		 (:name base16 :description "Base16 color theme for GNU Emacs" :website "http://chriskempson.github.io/base16/" :type github :pkgname "neil477/base16-emacs" :minimum-emacs-version 24 :prepare
				(add-to-list 'custom-theme-load-path default-directory)))
 (dash status "installed" recipe
	   (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (el-get status "installed" recipe
		 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :compile
				("el-get.*\\.el$" "methods/")
				:features el-get :post-init
				(when
					(memq 'el-get
						  (bound-and-true-p package-activated-list))
				  (message "Deleting melpa bootstrap el-get")
				  (unless package--initialized
					(package-initialize t))
				  (when
					  (package-installed-p 'el-get)
					(let
						((feats
						  (delete-dups
						   (el-get-package-features
							(el-get-elpa-package-directory 'el-get)))))
					  (el-get-elpa-delete-package 'el-get)
					  (dolist
						  (feat feats)
						(unload-feature feat t))))
				  (require 'el-get))))
 (epl status "installed" recipe
	  (:name epl :description "EPL provides a convenient high-level API for various package.el versions, and aims to overcome its most striking idiocies." :type github :pkgname "cask/epl"))
 (f status "installed" recipe
	(:name f :website "https://github.com/rejeep/f.el" :description "Modern API for working with files and directories in Emacs" :depends
		   (s dash)
		   :type github :pkgname "rejeep/f.el"))
 (let-alist status "required" recipe nil)
 (multiple-cursors status "installed" recipe
				   (:name multiple-cursors :description "An experiment in adding multiple cursors to emacs" :type github :pkgname "magnars/multiple-cursors.el"))
 (pkg-info status "installed" recipe
		   (:name pkg-info :description "Provide information about Emacs packages." :type github :pkgname "lunaryorn/pkg-info.el" :depends
				  (dash epl)))
 (projectile status "installed" recipe
			 (:name projectile :description "Project navigation and management library for Emacs." :type github :pkgname "bbatsov/projectile" :depends
					(dash s f pkg-info)))
 (pymacs status "installed" recipe
		 (:name pymacs :description "Interface between Emacs Lisp and Python" :type github :pkgname "pinard/Pymacs" :prepare
				(progn
				  (el-get-envpath-prepend "PYTHONPATH" default-directory)
				  (autoload 'pymacs-load "pymacs" nil t)
				  (autoload 'pymacs-eval "pymacs" nil t)
				  (autoload 'pymacs-exec "pymacs" nil t)
				  (autoload 'pymacs-call "pymacs")
				  (autoload 'pymacs-apply "pymacs"))
				:build
				("make")))
 (rope status "installed" recipe
	   (:name rope :description "A python refactoring library" :post-init
			  (el-get-envpath-prepend "PYTHONPATH" default-directory)
			  :type git :url "https://github.com/python-rope/rope.git"))
 (ropemacs status "installed" recipe
		   (:name ropemacs :description "An Emacs minor mode for using rope python refactoring library in emacs." :post-init
				  (progn
					(unless
						(boundp 'pymacs-load-path)
					  (setq pymacs-load-path nil))
					(add-to-list 'pymacs-load-path default-directory))
				  :depends
				  (rope ropemode pymacs)
				  :type git :url "https://github.com/python-rope/ropemacs"))
 (ropemode status "installed" recipe
		   (:name ropemode :description "Common parts of ropemacs and ropevim." :post-init
				  (progn
					(unless
						(boundp 'pymacs-load-path)
					  (setq pymacs-load-path nil))
					(add-to-list 'pymacs-load-path default-directory))
				  :type git :url "https://github.com/python-rope/ropemode"))
 (s status "installed" recipe
	(:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el")))
