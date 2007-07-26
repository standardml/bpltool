require "xmlrpc/client"

$xmlrpcserverport = 3197

class BplwebController < ApplicationController
  def index
    @example_pages, @examples = paginate :examples, {:per_page => 20, :order => "filename"}
    params = params()
    @filename = ''
    @title = ''
    @signature = 'passive(Get =: 1 --> 1), passive(Send -: 2)'
    @agent = 'Get[y][[z]] o (&lt;[z]> z//[] * &lt;->) `|` Send[x,y] o &lt;->'
print "\n===>[", params[:simplifymatches], "]\n"
    @simplifymatches = params[:simplifymatches] == "on"
    @rules = [Rule.new(:redex => 'Get[y1][[z1]] `|` Send[x1,y1] o &lt;->',
                       :react => '(x1//[] * y1/z1 * idp(1)) o `[z1]`',
                       :inst => '[0 |-&gt; 0]')]
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
                            simplifymatches, rules,
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

    begin
      # Call the remote server and get our result
      @result = server.call("simplifyrequest", signature, agent);
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

end