require "muflax"

BaseDir = File.expand_path "~/.emacs.d"

def compile_file path
  if File.extname(path) == '.el'
    # only recompile out-dated files
    elc = "#{File.dirname(path)}/#{File.basename(path, ".el")}.elc"
    unless File.exists? elc and File.mtime(elc) >= File.mtime(path)
      ap "compiling #{path}..."
      sh "emacs --batch --eval '(package-initialize)' -f batch-byte-compile #{path}"
    end
  end
end

desc "compile init file"
task :init do
  compile_file "#{BaseDir}/init.el"
end

desc "compile site-lisp (except for major packages)"
task :site_lisp => [:init] do
  compile_dir "#{BaseDir}/site-lisp"
end

desc "push to github"
task :push do
  sh "git push origin master"
end

task :default => :site_lisp
