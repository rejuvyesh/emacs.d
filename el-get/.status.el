((Enhanced-Ruby-Mode status "installed" recipe
                     (:name Enhanced-Ruby-Mode :description "Major mode for editing Ruby files" :type github :pkgname "zenspider/enhanced-ruby-mode"))
 (completing-help status "installed" recipe
                  (:name completing-help :type http :website "http://homepage1.nifty.com/bmonkey/emacs/index-en.html" :description "Display help on completions" :url "http://homepage1.nifty.com/bmonkey/emacs/elisp/completing-help.el"))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (rope status "installed" recipe
       (:name rope :description "A python refactoring library" :post-init
              (el-get-envpath-prepend "PYTHONPATH" default-directory)
              :type hg :url "http://bitbucket.org/agr/rope"))
 (vcomp status "removed" recipe nil)
 (virtualenv status "installed" recipe
             (:name virtualenv :description "Virtualenv for Python" :type github :pkgname "aculich/virtualenv.el"))
 (webjump++ status "installed" recipe
            (:name webjump++ :type github :description "Easy search web on engines." :pkgname "renard/webjump-plus-plus")))
