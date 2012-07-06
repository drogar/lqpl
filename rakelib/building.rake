require 'ant'
require 'GUI/src/version'
require 'config/build_config'
require 'rspec/core/rake_task'


$LOAD_PATH << "GUI"


directory "out/bin"
directory "out/lqpl_gui"
directory "out/lib/java"

abs_out = File.absolute_path('out')
redist_jars = FileList.new('GUI/lib/java/*')

build = namespace :build do
  task :server_clean do
    sh "runghc Setup.hs clean"
  end
  task :server_config do
    sh "runghc Setup.hs configure --prefix=#{abs_out}"
  end

  task :server_config_with_tests do
    sh "runghc Setup.hs configure --prefix=#{abs_out} --enable-tests"
  end
  desc 'Build the Haskell Compiler and Emulator'
  task :server => ["out/bin",:server_config] do
    sh "runghc Setup.hs build && runghc Setup.hs install"
    #sh "strip out/bin/#{sp}"
  end

  task :server_with_tests => ["out/bin",:server_config_with_tests] do
    sh "runghc Setup.hs build && runghc Setup.hs install"
    #sh "strip out/bin/#{sp}"
  end

  desc 'Copy JRuby files in preparation for JAR'
  task :copy_jruby => "out/lqpl_gui" do
    ant.copy :todir => "out/lqpl_gui" do
      fileset :dir => "GUI/src", :excludes => "**/*.java"
    end
  end
  desc 'Compile java files in preparation for JAR'
  task :compile => "out/lqpl_gui" do
    ant.javac :srcdir => "GUI/src", :destdir => "out/lqpl_gui", :includeAntRuntime => "no",
      :classpath => "GUI/lib/java/forms_rt.jar:GUI/lib/java/jruby-complete.jar:GUI/lib/java/monkeybars-1.1.1.jar"
  end
  desc 'Make a jar'
  task :jar => ["out", "GUI/Manifest", :compile, :copy_jruby] do
    ant.jar :destfile => "out/lqpl_gui.jar", :basedir => "out/lqpl_gui", :manifest => "GUI/Manifest"
  end
  desc 'Build the lqpl system'
  task :all => [:jar, :server, "out/lib/java"] do
    redist_jars.each {|jar| cp jar, "out/lib/java/",:preserve => true}
  end

end
mac=false
case Config::CONFIG["host_os"]
when /darwin/i # OSX specific code
  tech="x86_64-apple-darwin"
  tar_options = "--disable-copyfile"
  mac=true
when /^win|mswin/i # Windows specific code
  tech="x86_32-mswin"
  tar_options = ""
when /linux/i # Linux specific code
  tech="x86_64-linux"
  tar_options = ""
end

directory "out"
directory "out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/bin"
directory "out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/lib"
directory "out/lqpl-#{LQPL_GUI_VERSION}-source"

directory "out/LQPLEmulator.app/Contents/MacOS"
directory "out/LQPLEmulator.app/Contents/PkgInfo"
directory "out/LQPLEmulator.app/Contents/Resources/Java/bin"
directory "out/LQPLEmulator.app/Contents/Resources/Java/lib"



bin_dist_includes = FileList.new

DIST_INCLUDES.each {|incf| bin_dist_includes.include(incf)}

source_dist_files = FileList.new('./*')

EXCLUDE_FROM_SOURCE_DIST.each {|exf| source_dist_files.exclude(exf)}

dist = namespace :dist do
  desc "Make a #{tech} binary distribution"
  task :binary => ["out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/bin", "out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/lib/java",build[:jar]] do
    cp "out/lqpl_gui.jar","out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/",:preserve => true

    redist_jars.each {|jar| cp jar,  "out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/lib/java",:preserve => true}
    bin_dist_includes.each do |f|
      cp "#{f}", "out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/",:preserve => true
    end
    copy_server_bin "out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/bin/"
    $stdout << "Creating tar file: lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}.tgz\n"
    sh "tar #{tar_options} -C out -czf lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}.tgz lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}"
  end
  desc "Make a source distribution"
  task :source =>["out/lqpl-#{LQPL_GUI_VERSION}-source"] do
    source_dist_files do |gsource|
      cp("#{gsource}","out/lqpl-#{LQPL_GUI_VERSION}-source",:preserve => true)
    end
    $stdout << "Creating tar file: out/lqpl-#{LQPL_GUI_VERSION}-source.tgz\n"
    sh "tar #{tar_options} -C out  -czf out/lqpl-#{LQPL_GUI_VERSION}-source.tar_optionsMgz out/lqpl-#{LQPL_GUI_VERSION}-source"
  end
  if mac
    task :mac_dirs => ["out/LQPLEmulator.app/Contents/MacOS", "out/LQPLEmulator.app/Contents/PkgInfo","out/LQPLEmulator.app/Contents/Resources/Java/bin","out/LQPLEmulator.app/Contents/Resources/Java/lib"]
    desc "make a mac app"
    task :mac_app => [:mac_dirs,build[:jar]] do
      cp "/System/Library/Frameworks/JavaVM.framework/Versions/A/Resources/MacOS/JavaApplicationStub","out/LQPLEmulator.app/Contents/MacOS/"
      cp "config/Info.plist","out/LQPLEmulator.app/Contents/"
      cp "GUI/icons/lqpl.icns","out/LQPLEmulator.app/Contents/Resources"
      cp "out/production/lqpl_gui.jar","out/LQPLEmulator.app/Contents/Resources/Java"
      cp "GUI/lib/java","out/LQPLEmulator.app/Contents/Resources/Java/lib/"
      copy_server_bin "out/LQPLEmulator.app/Contents/Resources/Java/bin/"
    end
  end
end

tests = namespace :test do
  RSpec::Core::RakeTask.new(:spec) do |t|
    t.pattern="GUI/spec/**/*_spec.rb"
    t.rspec_opts = "--color"
    t.ruby_opts = "--1.9 -IGUI"
  end

  desc "Run gui specs after ensuring jar is built"
  task :spec => [build[:jar]]

  desc "Run lqpl Compiler and Emulator tests"
  task :server_tests =>[build[:server_with_tests]] do
     sh "runghc Setup.hs test --show-details=failures"
   end
   desc "Run all tests"
   task :all => [:spec,:server_tests]
end

task :clean => [build[:server_clean]]

namespace :run do
  desc "Run lqpl, ensuring all built"
  task :lqpl => [build[:all]] do
    sh "(cd out; java -jar lqpl_gui.jar -cp #{redist_jars.to_s.gsub!(' ',':')})"
  end
end

def copy_server_bin(to_dir)
  bin_copied = false
  HASKELL_BIN_DIRS.each do |d|
    if File.file?("#{d}/lqpl") and not bin_copied
      cp "#{d}/lqpl*","#{to_dir}"
      bin_copied=true
    end
  end
  abort "No lqpl executables found!!!" if not bin_copied
end