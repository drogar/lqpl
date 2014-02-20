# encoding: utf-8
require 'spec/spec_helper'

describe CompilerCommandInterpretor do
  let(:conn) {double('connection')}
  subject {CompilerCommandInterpretor.new(conn)}
  specify {expect(CompilerCommandInterpretor::COMMAND_START).to eq(/(CS_)|(<qpo)|(<compilefail)|(<getFirst)/)}
end