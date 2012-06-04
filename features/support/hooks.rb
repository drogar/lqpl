
Before do
  begin
    com.drogar.qface.Main.main([])
  rescue Exception => e
    puts "Exception from main: #{e}"
  end

  java_import org.netbeans.jemmy.operators.JFrameOperator
  $qe_frame = JFrameOperator.new "Quantum Emulator"
end

After do
  $qe_frame.close
end