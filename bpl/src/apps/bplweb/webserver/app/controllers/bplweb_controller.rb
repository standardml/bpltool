require "xmlrpc/client"

$xmlrpcserverport = 3197

class BplwebController < ApplicationController
  def test
  end

  def index
    #@example_pages, @examples = paginate :examples, {:per_page => 20, :order => "filename"}
    @examples = Example.all[:order => "filename"]
    params = params()
    @filename = ''
    @title = ''
    @signature = "(* Features Demo *)\n" +
                 "active0 (K), active (K1 -: 1), active(K21 =: 2 --> 1),\n" +
                 "passive0(L), passive(L1 -: 1), passive(L2 -: 2), passive(L20 =: 2 --> 0),\n" +
                 "atomic0 (M), atomic (M1 -: 1), atomic (M2 -: 2)"
    @agent = "-//[e1,e2] o (\n" +
             " K21[f][[],[b1,b2]] o (<[b1,b2]> L2[b1,b2] o M1[e1]) `|`\n" +
             " L1[e2] o M2[e1,e2]                                  `|`\n" +
             " K1[e2] o M                                          `|`\n" +
             " L20[][[b1],[b2]] o (<[b1,b2]> L2[b1,b2] o <->)\n" +
             ")"
    @simplifymatches = params[:simplifymatches] == "on"
    params[:syntacticsugar] = true if !params.has_key? :syntacticsugar
    @syntacticsugar = params[:syntacticsugar]
    @rules = [Rule.new(:redex => "(<[y1,y2]> L2[y1,y2] o `[x1]`) ||\n" +
                                 "L1[x2] ||\n" +
                                 "K1[x2] o merge(2)",
                       :react => "((<[y1,y2]> y1//[] * y2//[] * K) * x1//[] * x2//[] *\n" +
                                 " (-/x1 o (L1[x1] o (`[]` `|` `[x1]`)) `|` `[]`) *\n" +
                                 " <->) o\n" +
                                 "@@[2&[x1],0&[],1&[],3&[]]",
                       :inst => '')]
    if params[:id]
      begin
        example = Example.find(params[:id])
        @filename  = example.filename
        @title     = example.title
        @signature = example.signature
        @agent     = example.agent
        @rules     = Rule.find(:all, :conditions => ['eid = ?', example.id])
      rescue ActiveRecord::RecordNotFound
      end
    end
  end

  def bigraphsyntax
  end

  def instantiationsyntax
  end

  def matchrequest
    # Make an object to represent the XML-RPC server.
    server = XMLRPC::Client.new( "localhost", "/RPC2", $xmlrpcserverport)

    params = params()

    sessionid = params ['sessionid'].to_i
    sessionid = session[:id] if session[:id]

    id = {'sessionid' => sessionid,
      'matchingid' => params ['matchingid'].to_i}


    signature = params ['signature']
    agent     = params ['agent']
    simplifymatches = params ['simplifymatches'] == "on"
    syntacticsugar = params ['syntacticsugar'] == "on"
    redex     = params ['redex']
    react     = params ['react']
    inst     = params ['inst']
    rules = Array.new
    i = 0
    while true
      redexi = redex [i.to_s]
      reacti = react [i.to_s]
      insti  = inst  [i.to_s]
      break unless redexi && reacti && insti
      rules [i] = {'redex' => redexi, 
                   'react' => reacti,
                   'inst' => insti}
      i += 1
    end
    matchcount   = params ['matchcount'].to_i
    rulestomatch = params ['rulestomatch'].to_i
    # If fewer matches have been than the number of rules, result
    # listeners will hang if we don't request at least one match
    # from each rule, i.e., request a number of matches equal to
    # the number of rules:
    if rulestomatch < 0 && 0 <= matchcount && matchcount < rules.length
      matchcount = rules.length 
    end
    requestno    = params ['requestno'].to_i
print "matchrequest " + requestno.to_s + " calling server...\n"
    begin
      # Call the remote server and get our result
      @result = server.call("matchrequest", id, signature, agent,
                            simplifymatches, syntacticsugar, rules,
                            matchcount, rulestomatch, requestno)
      if @result['type'] == 'OK'
        session[:id] = @result ['id']['sessionid'].to_i 
      end
    rescue StandardError => e
      @result = {'type' => 'ERROR', 
                 'errtxt' => 'Exception in matchrequest from XMLRPC: ' + e.message + "\n" + e.backtrace.delete_if{|x| x.include? "/usr/lib/"}.join("\n")}
    end
print "Server call returned to matchrequest " + requestno.to_s + ".\n"
  end

  def resultrequest
    # Make an object to represent the XML-RPC server.
    server = XMLRPC::Client.new( "localhost", "/RPC2", $xmlrpcserverport)

    params = params()

    id = {'sessionid' => params ['sessionid'].to_i,
      'matchingid' => params ['matchingid'].to_i}
    rule = params ['rule']
    match = params ['match']
print "resultrequest " + id.to_s + " calling server...\n"
    begin
      # Call the remote server and get our result
      @result = server.call("resultrequest", id, rule, match)
      if @result['type'] == 'OK'
        session[:id] = @result ['id']['sessionid'].to_i 
      end
    rescue StandardError => e
      @result = {'type' => 'XMLRPCERROR',
        'errtxt' => 'Exception in resultrequest from XMLRPC: ' +
                     e.message + "\n" +
                     e.backtrace.delete_if{|x| x.include? "/usr/lib/"}.join("\n")}
    rescue Timeout::Error
      @result = {'type' => 'TIMEOUT'}
    end
print "Server call returned to resultrequest " + id.to_s + ".\n"
  end

  def reactrequest
    # Make an object to represent the XML-RPC server.
    server = XMLRPC::Client.new( "localhost", "/RPC2", $xmlrpcserverport)

    params = params()

    id = {'sessionid' => params ['sessionid'].to_i,
      'matchingid' => params ['matchingid'].to_i}
    rule = params ['rule'].to_i
    match = params ['match'].to_i
    requestno = params ['requestno'].to_i
print "reactrequest " + id.to_s + " calling server (requestno = " + requestno.to_s + ")...\n"
    begin
      # Call the remote server and get our result
      @result = server.call("reactrequest", id, rule, match, requestno)
      if @result['type'] == 'OK'
        session[:id] = @result ['id']['sessionid'].to_i 
      end
    rescue StandardError => e
      @result = {'type' => 'XMLRPCERROR',
        'errtxt' => 'Exception in reactrequest from XMLRPC: ' +
                     e.message + "\n" +
                     e.backtrace.delete_if{|x| x.include? "/usr/lib/"}.join("\n")}
    rescue Timeout::Error
      @result = {'type' => 'TIMEOUT'}
    end
print "Server call returned to reactrequest " + id.to_s + ".\n"
  end
  

  def simplifyrequest
    # Make an object to represent the XML-RPC server.
    server = XMLRPC::Client.new( "localhost", "/RPC2", $xmlrpcserverport)

    params = params()

    signature = "[" + params ['signature'] + "]"
    agent = params ['agent']
    syntacticsugar = params ['syntacticsugar'] == "on"


    begin
      # Call the remote server and get our result
      @result = server.call("simplifyrequest", signature, agent, syntacticsugar);
    rescue StandardError => e
      @result = {'type' => 'XMLRPCERROR',
        'errtxt' => 'Exception in reactrequest from XMLRPC: ' +
                     e.message + "\n" +
                     e.backtrace.delete_if{|x| x.include? "/usr/lib/"}.join("\n")}
    rescue Timeout::Error
      @result = {'type' => 'TIMEOUT'}
    end
print "Server call returned to simplifyrequest " + id.to_s + ".\n"
  end
  
  def svgrequest
    # Return SVG representation of a bigraph
    params = params()
    
    signature = "[" + params [:signature] + "]"
    bigraph = params [:bigraph]
    
    begin
      @result = ""
      # Call the prettyprinter
      if File.exist? "../../svg/bg2svg/bg2svg" then
        f = IO.popen("../../svg/bg2svg/bg2svg", "r+")
      else
        f = IO.popen("../../../svg/bg2svg/bg2svg", "r+")
      end
      f.puts "SIGNATURE"
      f.puts signature
      f.puts "ENDSIGNATURE"
      f.puts "BIGRAPH"
      f.puts bigraph
      f.puts "ENDBIGRAPH"
      f.flush
      @result += f.gets while !f.eof?
      f.close
    rescue StandardError => txt
      @result += "<p class='info'>[unable to generate image: " + txt.to_s + "]</p>"
    end
  end

end
