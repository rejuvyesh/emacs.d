require "muflax"

BaseDir = File.expand_path "~/.emacs.d"

def compile_dir dir, compile_everything: false, recurse: true
  Dir["#{dir}/*"].each do |path|
    if File.directory? path
      # use makefiles if they exist, otherwise recurse
      if File.exists? "#{path}/Makefile"
        return unless compile_everything
        ap "compiling #{path}..."
        sh "cd #{path}; make"
      elsif File.exists? "#{path}/Rakefile"
        return unless compile_everything
        ap "compiling #{path}..."
        sh "cd #{path}; rake"
      else
        compile_dir path if recurse
      end
    else
      compile_file path
    end
  end
end

def compile_file path
  if File.extname(path) == ".el"
    # only recompile out-dated files
    elc = "#{File.dirname(path)}/#{File.basename(path, ".el")}.elc"
    unless File.exists? elc and File.mtime(elc) >= File.mtime(path)
      ap "compiling #{path}..."
      sh "emacs --batch --eval '(load \"#{BaseDir}/load-path.el\")' -f batch-byte-compile #{path} > /dev/null"
    end
  end
end

desc "compile ALL the elisp"
task :everything => [:init] do
  compile_dir "#{BaseDir}/local", compile_everything: true
end

desc "compile init file"
task :init do
  compile_dir BaseDir, recurse: false
end

desc "compile local packages (except for major ones)"
task :local do
  compile_dir "#{BaseDir}/local"
end

desc "compile local config"
task :config => [:init, :local]

task :default => :local
