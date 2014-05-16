# Encoding: UTF-8
require 'spec/spec_helper'

describe CompilerServerConnection do
  context 'creation' do
    context 'enforce singleton' do
      before(:each) do
        @cmp = nil
      end
      after(:each) do
        @cmp.close_down if @cmp
      end
      it 'gives an error when trying to create with "new"' do
        expect { @cmp = CompilerServerConnection.new }.to raise_error NoMethodError
      end
      it 'allows passing a port number > 1000 on creation' do
        @cmp = CompilerServerConnection.get_instance 3456
        expect(@cmp.port).to eq 3456
      end
      it 'allows default creation with a port of 7683' do
        @cmp = CompilerServerConnection.get_instance
        expect(@cmp.port).to eq(7683)
      end
    end
    context 'commander' do
      before :each do
        @sc = CompilerServerConnection.instance
      end
      specify { expect(@sc.methods).to include(:compile) }
      specify { expect(@sc.methods).to include(:success_or_fail_message) }
    end

    context 'connection' do
      before :each do
        @cmp = CompilerServerConnection.get_instance
      end
      after :all  do
        @cmp.close_down if @cmp
      end
      it 'connects to the lqpl-compiler process when created' do
        @cmp.connect
        expect(@cmp).to be_connected
      end
    end
  end

end
