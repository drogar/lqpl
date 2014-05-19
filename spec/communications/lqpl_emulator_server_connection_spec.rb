# Encoding: UTF-8
require 'spec/spec_helper'

describe LqplEmulatorServerConnection do
  context 'creation' do
    context 'enforce singleton' do
      before :each do
        @sc = nil
      end
      after :each do
        @sc.close_down if @sc
      end
      it 'gives an error when trying to create with new' do
        expect { @sc = LqplEmulatorServerConnection.new }.to raise_error NoMethodError
      end
      it 'allows default creation with a port of 9502' do
        @sc = LqplEmulatorServerConnection.instance
        expect(@sc.port).to eq(9502)
      end
    end
    context 'commander' do
      before :each do
        @sc = LqplEmulatorServerConnection.instance
      end
      specify { expect(@sc.methods).to include(:send_load_from_file) }
      specify { expect(@sc.methods).to include(:get_qstack) }
      specify { expect(@sc.methods).to include(:get_stack_translation) }
      specify { expect(@sc.methods).to include(:get_classical_stack) }
      specify { expect(@sc.methods).to include(:get_dump) }
      specify { expect(@sc.methods).to include(:get_code) }
      specify { expect(@sc.methods).to include(:get_codepointer) }
      specify { expect(@sc.methods).to include(:do_run) }
      specify { expect(@sc.methods).to include(:do_trim) }
      specify { expect(@sc.methods).to include(:do_simulate) }
      specify { expect(@sc.methods).to include(:do_depth_multiple) }
    end
    context 'connection' do
      before :each do
        @sc = LqplEmulatorServerConnection.get_instance
      end
      after :all do
        @sc.close_down if @sc
      end
      it 'connects to the lqpl-serv process when created' do
        @sc.connect
        expect(@sc).to be_connected
      end
    end
  end
end
