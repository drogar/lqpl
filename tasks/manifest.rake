file 'Manifest' do
  File.open 'Manifest' , 'w' do |f|
    f.puts 'Class-Path: . lib/java/jruby-complete.jar lib/java/monkeybars-1.1.1.jar lib/java/forms_rt.jar'
    f.puts 'Main-Class: com.drogar.lqpl.Main'
  end
end