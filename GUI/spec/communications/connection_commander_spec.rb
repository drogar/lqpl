# encoding: utf-8
require 'spec/spec_helper'

describe ConnectionCommander do
  let(:conn) {double}
  subject {ConnectionCommander.new(conn)}
  describe "bypass message" do
    it "ignores input that matches the input" do
      allow(conn).to receive(:receive_command_data).and_return('match', 'not')
      expect(subject.bypass_messages(/match/)).to eq('not')
    end
  end
  describe :send_list_of_lines do
    it "sends each line and receives the command data" do
      expect(conn).to receive(:send_and_receive_command).exactly(3).times
      subject.send_list_of_lines(["one", "two", "three"])
    end
  end
  describe :send_file do
    it "reads the file, sends the start, the file and the finish" do
      expect(File).to receive(:read).with("fname").and_return("fdata")
      expect(conn).to receive(:send_command).with('start')
      expect(conn).to receive(:send_command).with('fdata')
      expect(conn).to receive(:send_command).with('finish')
      subject.send_file('start', 'fname', 'finish')
    end
  end
end