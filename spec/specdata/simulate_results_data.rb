def make_triple(a,b,c)
  "<triple><string>#{a}</string><string>#{b}</string><string>#{c}</string></triple>"
end

ONEELT = "<Simulated><double>0.27</double><results>"+make_triple("a","Coin","Heads")+"</results></Simulated>"
TWOELTS = "<Simulated><double>0.73</double><results>"+make_triple("a","Coin","Heads")+make_triple("q","qubit","0")+"</results></Simulated>"