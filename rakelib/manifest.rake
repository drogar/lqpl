file 'GUI/Manifest' do
  File.open 'GUI/Manifest', 'w' do |f|
    f.puts 'Class-Path: . lib/java/jruby-complete.jar lib/java/monkeybars-1.1.1.jar'
    f.puts 'Main-Class: com.drogar.lqpl.Main'
  end
end
