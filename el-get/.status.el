((anything status "installed" recipe
	   (:name anything :website "http://www.emacswiki.org/emacs/Anything" :description "Open anything / QuickSilver-like candidate-selection framework" :type git :url "http://repo.or.cz/r/anything-config.git" :shallow nil :load-path
		  ("." "extensions" "contrib")
		  :features anything))
 (auto-complete status "installed" recipe
		(:name auto-complete :website "http://cx4a.org/software/auto-complete/" :description "The most intelligent auto-completion extension." :type github :pkgname "m2ym/auto-complete" :depends
		       (popup fuzzy)))
 (coffee-mode status "installed" recipe
	      (:name coffee-mode :website "http://ozmm.org/posts/coffee_mode.html" :description "Emacs Major Mode for CoffeeScript" :type github :pkgname "defunkt/coffee-mode" :features coffee-mode :post-init
		     (progn
		       (add-to-list 'auto-mode-alist
				    '("\\.coffee$" . coffee-mode))
		       (add-to-list 'auto-mode-alist
				    '("Cakefile" . coffee-mode))
		       (setq coffee-js-mode 'javascript-mode))))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :features el-get :info "." :load "el-get.el"))
 (expand-region status "installed" recipe
		(:name expand-region :type git :url "https://github.com/magnars/expand-region.el.git"))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/m2ym/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "m2ym/fuzzy-el"))
 (grep-edit status "installed" recipe
	    (:name grep-edit :type emacswiki :features
		   (grep-edit)))
 (less-css-mode status "installed" recipe
		(:name less-css-mode :type git :url "https://github.com/purcell/less-css-mode.git"))
 (magit status "required" recipe nil)
 (markdown-mode status "installed" recipe
		(:name markdown-mode :description "Major mode to edit Markdown files in Emacs" :type git :url "git://jblevins.org/git/markdown-mode.git" :post-init
		       (add-to-list 'auto-mode-alist
				    '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
 (milkode status "installed" recipe
	  (:name milkode :type git :url "https://github.com/ongaeshi/emacs-milkode.git" :features
		 (milkode)))
 (mooz-js2-mode status "installed" recipe
		(:name mooz-js2-mode :type git :url "https://github.com/mooz/js2-mode.git" :post-init
		       (progn
			 (autoload 'js2-mode "js2-mode" nil t))))
 (multiple-cursors status "installed" recipe
		   (:name multiple-cursors :type git :url "https://github.com/magnars/multiple-cursors.el.git"))
 (popup status "installed" recipe
	(:name popup :type git :url "https://github.com/auto-complete/popup-el.git" :features
	       (popup)))
 (rabbit-mode status "installed" recipe
	      (:name rabbit-mode :type git :url "https://github.com/rabbit-shocker/rabbit.git" :load-path
		     ("misc/emacs")
		     :after
		     (lambda nil
		       (autoload 'rabbit-mode "rabbit-mode" "major mode for Rabbit" t)
		       (add-to-list 'auto-mode-alist
				    '("\\.\\(rbt\\|rab\\)$" . rabbit-mode)))))
 (rst-mode status "installed" recipe
	   (:name rst-mode :description "Mode for viewing and editing reStructuredText-documents." :type http :url "http://docutils.sourceforge.net/tools/editors/emacs/rst.el" :features rst))
 (ruby-mode-trunk status "installed" recipe
		  (:name ruby-mode-trunk :type svn :url "http://svn.ruby-lang.org/repos/ruby/trunk" :load-path
			 ("misc")
			 :after
			 (lambda nil
			   (add-to-list 'auto-mode-alist
					'("\\.rb$" . ruby-mode))
			   (add-to-list 'auto-mode-alist
					'("\\.rake$" . ruby-mode))
			   (add-to-list 'auto-mode-alist
					'("\\.gemspec$" . ruby-mode))
			   (add-to-list 'auto-mode-alist
					'("Rakefile$" . ruby-mode))
			   (add-to-list 'auto-mode-alist
					'("Gemfile$" . ruby-mode))
			   (add-to-list 'interpreter-mode-alist
					'("ruby" . ruby-mode))
			   (add-hook 'c-mode-hook 'ruby-style-c-mode)
			   (add-hook 'c++-mode-hook 'ruby-style-c-mode))
			 :features
			 (ruby-mode ruby-style)))
 (run-test status "installed" recipe
	   (:name run-test :type git :url "https://github.com/kou/run-test.git" :load-path
		  ("lib")
		  :features
		  (run-test-setting)))
 (textile-mode status "installed" recipe
	       (:name textile-mode :description "Textile editing mode" :type http :url "http://dev.nozav.org/scripts/textile-mode.el" :prepare
		      (progn
			(autoload 'textile-mode "textile-mode" "Textile editing mode." t)
			(add-to-list 'auto-mode-alist
				     '("\\.textile\\'" . textile-mode))))))
