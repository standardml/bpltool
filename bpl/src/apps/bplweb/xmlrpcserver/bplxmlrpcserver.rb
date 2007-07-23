# Copyright (c) 2007  The BPL Group at the IT University of Copenhagen

# This file is part of BPL.

# BPL is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.

# BPL is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with BPL; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
# USA


require "xmlrpc/server"


class Worker
  def initialize matching
print "running worker threads...\n"
    results      = matching.results
    react        = matching.react
    found   = matching.found
    foundall= matching.foundall
    errors       = matching.errors
    mutex        = matching.mutex
    worker       = matching.worker
    resultsflag  = matching.resultsflag
    matcherready = ConditionVariable.new
    matcher = Object.new
    Thread.new {
print "running matcher worker thread...\n"
      mutex.synchronize {
        while true
          print "matcher worker going to sleep...\n"
          $stdout.flush
          worker.wait(mutex)
          print "matcher worker thread waking up...\n"
          #print "IO === matcher = "
          #if IO === matcher
          #  print "yes"
          #else
          #  print "no"
          #end
          #print "\n"
          $stdout.flush
          unless IO === matcher && !matcher.closed?
            print "running bplwebback...\n"
            $stdout.flush
            matcher = IO.popen("../backend/mlton", "w+")
            matcher.fcntl(4, 0x40000) # Avoid buffering
            matcher.sync = false # Avoid buffering!
            line = matcher.gets
            if line.strip.upcase != "READY"
              print "Error: expected 'READY' from bplwebback, got '#{line}\n'"
            end
            matcherready.broadcast
            print "pid = " + matcher.pid.to_s + "\n"
          end
          print "IO === matcher = "
          if IO === matcher
            print "yes\n"
          else
            print "no\n"
          end
          print "sending signal INT to bplwebback...\n"
          $stdout.flush
          Process.kill("SIGINT", matcher.pid)
          print "signal INT sent to bplwebback.\n"
          $stdout.flush
          if react['rule'] && react['match']
            rule = react['rule']
            match = react['match']
            print("sending to bplwebback:\n" +
                  "REACT\nRULENO:#{rule}\nMATCHNO:#{match}\nENDREACT\n")
            $stdout.flush
            matcher.puts("REACT\nRULENO:#{rule}\nMATCHNO:#{match}\nENDREACT\n")
            matcher.flush
          elsif !(defined? agent) || !(defined? rules) ||
              signature != matching.signature ||
              agent != matching.agent || rules != matching.rules
            print "new agent or rules\n"
            $stdout.flush
            signature    = matching.signature
            agent        = matching.agent
            rules        = matching.rules
            rulestomatch = matching.rulestomatch
            matchcount   = matching.matchcount
            rules_formatted = "["
            first_rule = true
            rules.each {|rule|
              rules_formatted += ",\n" unless first_rule
              first_rule = false
              rules_formatted += "{"
              first_field = true
              ['redex', 'react', 'inst'].each {|key|
                value = rule [key]
                if value
                  rules_formatted += ",\n" unless first_field
                  first_field = false
                  rules_formatted += key + " = " + value
                end
              }
              rules_formatted += "}"
            }
            rules_formatted += "]"
            print "sending to bplwebback:\n"
            #print("MATCH\nSIGNATURE\n[#{signature}]\nENDSIGNATURE\n" +
            #  "AGENT\n#{agent}\nENDAGENT\n" +
            #  "RULES\n#{rules}\nENDRULES\nUSERULES:#{rulestomatch}\n" +
            #  "MATCHCOUNT:#{matchcount}\nENDMATCH\n")
            for line in ["MATCH",
                          "SIGNATURE",
                          "[#{signature}]",
                          "ENDSIGNATURE",
                          "AGENT",
                          "#{agent}",
                          "ENDAGENT",
                          "RULES",
                          "#{rules_formatted}",
                          "ENDRULES",
                          "USERULES:#{rulestomatch}",
                          "MATCHCOUNT:#{matchcount}",
                          "ENDMATCH"]
              print(line + "\n")
              matcher.puts(line + "\n")
              matcher.flush()
            end
            $stdout.flush
          elsif rulestomatch != matching.rulestomatch
            print "new rules to match: #{rulestomatch}\n"
            $stdout.flush
            rulestomatch = matching.rulestomatch
            matchcount   = matching.matchcount
            print "sending to bplwebback: USERULES:#{rulestomatch}\nMATCHCOUNT:#{matchcount}\n"
            matcher.puts
            ("REMATCH\nUSERULES:#{rulestomatch}\n" +
             "MATCHCOUNT:#{matchcount}\nENDREMATCH\n")
            matcher.flush
          elsif matchcount != matching.matchcount
            print "new match count: #{matchcount}\n"
            $stdout.flush
            matchcount   = matching.matchcount
            print "sending to bplwebback: MATCHCOUNT:#{matchcount}\n"
            matcher.puts
            ("REMATCH\nMATCHCOUNT:#{matchcount}\nENDREMATCH\n")
            matcher.flush
          else
            print "Do nothing.\n"
            $stdout.flush
          end
        end
      }
    }
    Thread.new {
print "running result worker thread...\n"
      mutex.synchronize {
        until IO === matcher
          print "result worker checking for matcher readiness...\n"
          matcherready.wait(mutex)
        end
      }
      while true
        print "result worker thread waking up...\n"
        break if matcher.eof?
        line = matcher.gets
        print "result worker thread reading from bplwebback: #{line}"
        case line.strip.upcase
          when "RESULT"
            result = {}
            rule = ""
            match = ""
            result['context'] = ""
            result['parameter'] = ""
            result['tree'] = ""
            if matcher.eof?
              print "result worker thread: unexpected EOF from bplwebback\n"
              errors.push "Matcher error: unexpected EOF"
              resultsflag.broadcast
              break
            end
            line = matcher.gets
        print "result worker thread reading from bplwebback: #{line}"
            $stdout.flush
            curr = ""
            while line.strip.upcase != "END"
              case line
                when /^ *rule = *(.*)/      then curr = rule
                when /^ *match = *(.*)/     then curr = match
                when /^ *context = *(.*)/   then curr = result['context']
                when /^ *parameter = *(.*)/ then curr = result['parameter']
                when /^ *tree = *(.*)/      then curr = result['tree']
                when /^(.*)/              then 0
              end
              curr << "#{$1}\n"
              if matcher.eof?
                print "result worker thread: unexpected EOF from bplwebback\n"
                errors.push "Matcher error: unexpected EOF"
                resultsflag.broadcast
                break
              end
              line = matcher.gets
              print "result worker thread reading from bplwebback: #{line}"
            end
            rulenum = rule.to_i
            results[rulenum] = {} unless results[rulenum]
            print "result worker storing result (#{rulenum},#{match}): #{result}\n"
            $stdout.flush
            results[rulenum][match.to_i] = result
          when "NOMOREMATCHES"
            rule = ""
            matches = ""
            if matcher.eof?
              print "result worker thread: unexpected EOF from bplwebback when reading NOMOREMATCHES\n"
              errors.push "Matcher error: unexpected EOF"
              resultsflag.broadcast
              break
            end
            line = matcher.gets
            print "result worker thread reading from bplwebback: #{line}"
            curr = ""
            while line.strip.upcase != "END"
              case line.strip
                when /^rule = *(.*)/      then curr = rule
                when /^matches = *(.*)/   then curr = matches
                else 
                  print "result worker thread: unexpected line from bplwebback when reading NOMOREMATCHES: #{line}"
              end
              curr << $1
              if matcher.eof?
                print "result worker thread: unexpected EOF from bplwebback when reading NOMOREMATCHES\n"
                errors.push "Matcher error: unexpected EOF"
                resultsflag.broadcast
                break
              end
              line = matcher.gets
              print "result worker thread reading from bplwebback: #{line}"
            end
            $stdout.flush
            rulenum = rule.to_i
            if rulenum < 0
              foundall[0] = matches.to_i
            else
              found[rulenum] = matches.to_i
            end
          when "NEWAGENT"
            if matcher.eof?
              print "result worker thread: unexpected EOF from bplwebback when reading NEWAGENT\n"
              errors.push "Matcher error: unexpected EOF"
              resultsflag.broadcast
              break
            end
            newagent = ""
            line = matcher.gets
            while line.strip.upcase != "END"
              newagent += line
              if matcher.eof?
                print "result worker thread: unexpected EOF from bplwebback when reading new agent\n"
                errors.push "Matcher error: unexpected EOF when reading new agent"
                resultsflag.broadcast
                break
              end
              line = matcher.gets
            end
            newagent = newagent.strip
            print "result worker storing new agent '#{newagent}'"
            $stdout.flush
            react['newagent'] = newagent
          when "ERROR"
            errtxt = ""
            if matcher.eof?
              print "result worker thread: unexpected EOF from bplwebback when reading ERROR\n"
              errors.push "Matcher error: unexpected EOF"
              resultsflag.broadcast
              break
            end
            line = matcher.gets
            while line.strip.upcase != "END"
              errtxt += line
              if matcher.eof?
                print "result worker thread: unexpected EOF from bplwebback\n"
                errors.push errtxt
                errors.push "Matcher error: unexpected EOF"
                resultsflag.broadcast
                break
              end
              line = matcher.gets
            end
            print "result worker storing error #{errtxt}"
            $stdout.flush
            errors.push errtxt
          else
            errors.push line
            print "result worker storing error #{line}"
            $stdout.flush
        end
        resultsflag.broadcast
      end
    }
    print "worker threads end.\n"
    $stdout.flush
  end
end

noid = {'sessionid' => -1, 'matchingid' => -1}

emptymatch = {'context' => "", 'parameter' => "", 'tree' => ""}

noresult = { 'ruleno' => -1, 'matchno' => -1, 'match' => emptymatch }  

# A Matching object handles a single user session.
# A matching is identified within a session by a matchingid.
# If the agent or rules change in some matchingrequest, the
# matchingid is incremented.
# The matching has one worker thread controlling the external
# matching process, collecting its results and adding them to
# the results table (and then signalling the resultsflag).
# The worker must be signaled when a change in the job has
# occurred.
class Matching
  attr_reader :id, :signature, :agent, :rules, :matchcount, :rulestomatch,
  :react, :requestno, :results, :found, :foundall, :errors,
  :mutex, :worker, :resultsflag
  def initialize (id)
    @id           = id        # Matching ID (determines agent & rules)
    @signature    = ""        # Signature for use in agent
    @agent        = ""        # Agent in which to match...
    @rules        = ""        # ...one or more rules
    @matchcount   = -1        # Requested matches, -1 means 'all'
    @rulestomatch = -2        # Rules to match, -1 means 'all'
    @requestno    = -1        # GUI's request ID
    @react        = Hash.new  # signal via ['rule'], ['match'], ['newagent']
    @results      = Array.new # [r][m] contains match m of rule r
    @found        = Array.new # [r] contains total# of matches for rule r
    @foundall     = Array.new # contains total# of matches for all rules
    @errors       = Array.new # list of errors
    @mutex        = Mutex.new # Syncing Matching method calls
    @worker       = ConditionVariable.new # Communic. with matcher worker
    @resultsflag  = ConditionVariable.new # Communic. with results worker
    Worker.new self # Start the worker threads
  end

  def matchrequest1 (id, signature, agent, rules,
                    matchcount, rulestomatch, requestno)
print "matchrequest1 ([" + id['sessionid'].to_s + ", " +
      id['matchingid'].to_s + "], " + matchcount.to_s + ", " +
      rulestomatch.to_s + ", " + requestno.to_s + ")\n"
    @mutex.synchronize {
      if id['matchingid'] >= 0 && id['matchingid'] < @id['matchingid'] ||
          requestno < @requestno # out-of-order (i.e., late) request
print "matchrequest1 received request #{requestno} AFTER request #{@requestno}, so TIMEOUT is returned.\n"
$stdout.flush
        return {'type' => "TIMEOUT", 'id' => @id} 
      end
      if requestno == @requestno # repeated request
        return {'type' => "OK", 'id' => @id}
      end
      @requestno = requestno

print "signature = '" + signature.to_s + "',  @signature = '" + @signature.to_s + "'\n"
print "agent = '" + agent.to_s + "',  @agent = '" + @agent.to_s + "'\n"

      if signature == @signature && agent == @agent && rules == @rules
        # similar request
        if @matchcount < matchcount || (@matchcount > 0 && matchcount < 0)
          @matchcount = matchcount
          @worker.broadcast
        else
          @matchcount = matchcount
        end
      else
        print "matchrequest starting new matching...\n"
        # New matching because agent or rules differ
        @id['matchingid'] += 1
        @signature = signature
        @agent = agent
        @rules = rules
        @matchcount = matchcount
        @rulestomatch = rulestomatch
        @results.clear
        @found.clear
        @foundall.clear
        @errors.clear
        @worker.broadcast
      end
    }
    return {'type' => "OK", 'id' => @id}
  end

  def resultrequest (id, ruleno, matchno)
print "Matching.resultrequest (["
print id['sessionid']
print ", "
print id ['matchingid']
print "], "
print ruleno
print ", "
print matchno
print ") called.\n"
    @mutex.synchronize {
      ruleno = ruleno.to_i
      matchno = matchno.to_i
      if id ['matchingid'] < @id ['matchingid']
        return {'type' => "TIMEOUT", 'id' => @id}
      end
      resultsready = false
      print "Matching.resultrequest entering loop...\n"
      until resultsready || id != @id
        print "Matching.resultrequest: @foundall[0] = " + @foundall[0].to_s + "\n"
        print "Matching.resultrequest: @found[#{ruleno}] = " + @found[ruleno].to_s + "\n"
        resultsready = @results[ruleno] && @results[ruleno][matchno] ||
          @foundall[0] || @found[ruleno] || !@errors.empty?
        @resultsflag.wait(@mutex) unless resultsready
        print "Matching.resultrequest continuing loop...\n"
      end
      print "Matching.resultrequest exiting loop.\n"
      if id == @id
        if @errors.empty?
          if @results[ruleno] && @results[ruleno][matchno]
            return {
              'type' => "OK", 
              'id' => @id, 
              'result' => @results[ruleno][matchno]
            }
          elsif @foundall[0]
            return {
              'type' => "NOMOREMATCHES",
              'id' => @id,
              'rule' => -1,
              'matches' => @foundall[0]
            }
          elsif @found[ruleno]
            return {
              'type' => "NOMOREMATCHES",
              'id' => @id,
              'rule' => ruleno,
              'matches' => @found[ruleno]
            }
          end
        else
          errtxt = @errors.first
          # This makes subsequent identical requests hang: @errors.delete_at 0
          print "Matching.resultrequest1 returning error '" + errtxt + "'\n"
          return {
            'type' => "ERROR",
            'subtype' => "MATCHER",
            'errtxt' => errtxt
          }
        end
      else
        return {'type' => "TIMEOUT", 'id' => @id, 'result' => noresult}
      end
    }
  end

  def reactrequest1 (id, rule, match, requestno)
      rule = rule.to_i
      match = match.to_i
      requestno = requestno.to_i
print "reactrequest1 ([" + id['sessionid'].to_s + ", " +
      id['matchingid'].to_s + "], " + rule.to_s + ", " +
      match.to_s + ", " + requestno.to_s + ")\n"
    @mutex.synchronize {
      print "reactrequest1 entered synchronize\n"
      if id['matchingid'] >= 0 && id['matchingid'] < @id['matchingid'] ||
          requestno < @requestno # out-of-order (i.e., late) request
        return {'type' => "TIMEOUT", 'id' => @id} 
      end
      print "reactrequest1 didn't return TIMEOUT\n"
      if requestno == @requestno # repeated request
        return {'type' => "OK", 'id' => @id}
      end
      @requestno = requestno
      print "reactrequest1 registered requestno\n"

      @react['rule'] = rule
      @react['match'] = match
      print "reactrequest1 broadcasting to worker\n"
      @worker.broadcast

      @resultsflag.wait(@mutex) until @react['newagent'] || !@errors.empty?
      if @errors.empty?
        agent = @react['newagent']
        @react.clear
        print "reactrequest1 returning agent '" + agent + "'\n"
        return {'type' => "OK", 'id' => @id, 'agent' => agent}
      else
        errtxt = @errors.first
        # This makes subsequent identical requests hang: @errors.delete_at 0
        print "reactrequest1 returning error '" + errtxt + "'\n"
        return {
          'type' => "ERROR",
          'subtype' => "MATCHER",
          'errtxt' => errtxt
        }
      end    
    }
  end

end

# The Serverobj keeps track of the various user sessions.
# It receives a match request or a result request.
# The request is checked to see which session it refers
# to, and the request is forwarded to the corresponding matching.
# If no session for the request exists yet, one is created.
# !!!NOTE: We need to add some cleanup-when-timeout stuff.
class Serverobj
  def initialize
    @nextsessionid = 1
    print "new: nextsessionid = "
    print @nextsessionid
    print "\n"
    @mutex = Mutex.new
    @matchingsflag = ConditionVariable.new
    @matchingssessionidflag = ConditionVariable.new
  end

  def matchrequest (id, signature, agent, rules,
                    matchcount, rulestomatch, requestno)
    print "matchrequest ([" + id['sessionid'].to_s + ", " +
      id['matchingid'].to_s + "], " + requestno.to_s + ")\n"
    print "args: "
    print  [id, signature, agent, rules,
                    matchcount, rulestomatch, requestno].join(",")
    print "\n"
    sessionid = id['sessionid'].to_i
    matching = nil
    @mutex.synchronize {
      if sessionid < 0
        sessionid = @nextsessionid
        @nextsessionid += 1
        print "sessionid = #{sessionid}\n"
      end
      if @matchings
        matching = @matchings[sessionid]
      else
        @matchings = []
        @matchingsflag.broadcast
      end
    }
    id = {'sessionid' => sessionid, 'matchingid' => id['matchingid']}
    if matching
      return matching.matchrequest1(id, signature, agent, rules, matchcount,
                                   rulestomatch, requestno)
    end
    print "matchrequest making new Matching object for session "
    print sessionid
    print "\n"
    id['matchingid'] = 1
    matching = Matching.new(id)
    @matchings[sessionid] = matching
    @matchingssessionidflag.broadcast
    return matching.matchrequest1(id, signature, agent, rules, matchcount, 
                                  rulestomatch, requestno)
  end

  def resultrequest (id1, ruleno, matchno)
    @mutex.synchronize {
    matching = nil
    aid = Hash.new
    aid.replace id1
    print "resultrequest ([" + aid['sessionid'].to_s + ", " +
      aid['matchingid'].to_s + "], " + ruleno.to_s + ", " +
      matchno.to_s + ")\n"
    sessionid = aid['sessionid']
      if sessionid < 0
        return {'type' => 'ERROR', 'subtype' => 'MATCHSERVER',
                'errtxt' => "Matching server: Wrong id: " + aid.to_s}
      end
      @matchingsflag.wait(@mutex) unless @matchings
      until @matchings[sessionid]
        @matchingssessionidflag.wait(@mutex)
      end
      matching = @matchings[sessionid]
    if defined? matching && matching
      return matching.resultrequest(aid, ruleno, matchno)
    else
      return {'type' => "ERROR", 'subtype' => 'MATCHSERVER',
              'errtxt' => 'Matching server: matching lost'}
    end
    }
  end

  def reactrequest (id1, ruleno, matchno, requestno)
    @mutex.synchronize {
    matching = nil
    aid = Hash.new
    aid.replace id1
    print "reactrequest ([" + aid['sessionid'].to_s + ", " +
      aid['matchingid'].to_s + "], " + ruleno.to_s + ", " +
      matchno.to_s + ", " + requestno.to_s + ")\n"
    sessionid = aid['sessionid']
      if sessionid < 0
        return {'type' => 'ERROR', 'subtype' => 'MATCHSERVER',
                'errtxt' => "Matching server: Wrong id: " + aid.to_s}
      end
      @matchingsflag.wait(@mutex) unless @matchings
      until @matchings[sessionid]
        @matchingssessionidflag.wait(@mutex)
      end
      matching = @matchings[sessionid]
      if defined? matching && matching
        return matching.reactrequest1(aid, ruleno, matchno, requestno)
      else
        return {'type' => "ERROR", 'subtype' => 'MATCHSERVER',
                'errtxt' => 'Matching server: matching lost'}
    end
    }
  end

  def simplifyrequest (agent)
    print "simplifyrequest (" + agent + ")\n"
    matcher = IO.popen("../backend/mlton", "w+")
    matcher.fcntl(4, 0x40000) # Avoid buffering
    matcher.sync = false # Avoid buffering!
    line = matcher.gets
    if line.strip.upcase != "READY"
      print "Error: expected 'READY' from bplwebback, got '#{line}\n'"
    end
    
    matcher.puts("SIMPLIFY\nAGENT\n#{agent}\nENDAGENT\nENDSIMPLIFY\n")
    matcher.flush
    line = matcher.gets
    
    print "reading from bplwebback: #{line}"
    case line.strip.upcase
      when "SIMPLIFIEDAGENT"
        if matcher.eof?
          print "result worker thread: unexpected EOF from bplwebback when reading SIMPLIFIEDAGENT\n"
          matcher.close
          break
        end
        simplifiedagent = ""
        line = matcher.gets
        while line.strip.upcase != "END"
          simplifiedagent += line
          if matcher.eof?
            print "result worker thread: unexpected EOF from bplwebback when reading new agent\n"
            break
          end
          line = matcher.gets
        end
        matcher.close
        simplifiedagent = simplifiedagent.strip
        print "returning simplified agent '#{simplifiedagent}'"
        return {'type' => "OK", 'simplifiedagent' => simplifiedagent}
      when "ERROR"
        errtxt = ""
        if matcher.eof?
          print "unexpected EOF from bplwebback when reading ERROR\n"
          matcher.close
          break
        end
        line = matcher.gets
        while line.strip.upcase != "END"
          errtxt += line
          if matcher.eof?
            print "result worker thread: unexpected EOF from bplwebback\n"
            break
          end
          line = matcher.gets
        end
        matcher.close
        print "returning error #{errtxt}"
        return {
          'type' => "ERROR",
          'subtype' => "MATCHER",
          'errtxt' => errtxt
        }
      else
        print "returning error #{line}"
        return {
          'type' => "ERROR",
          'subtype' => "MATCHER",
          'errtxt' => line
        }
    end
    matcher.close
  end  

end
        

server = XMLRPC::Server.new(3197)

interface =
  # no XMLRPC prefix
  XMLRPC::interface("") {
  add_method("struct matchrequest (struct, string, string, array, int, int, int)",
             # return: {type, {sessionid, matchingid}}
             # {sessionid, matchingid}
             # signature
             # agent
             # rules
             # matchcount
             # rulestomatch
             # requestno
             "matchrequest({sessionid, matchingid}, signature, agent, rules, matchcount, rulestomatch, requestno)
  returns a {type, {sessionid, matchingid}}
  where type is 'OK' or 'TimeOut'",
             "matchrequest")
  add_method("struct resultrequest (struct, int, int)",
             # return: {type, {context, parameter, tree}}
             # {sessionid, matchingid}
             # ruleno
             # matchno
             # requestno
             "resultrequest({sessionid, matchingid}, ruleno, matchno)
  returns {type, {context, parameter, tree}}
  where type is 'TimeOut' or 'NoMoreMatches' or errtxt",
             "resultrequest")
}

serverobj = Serverobj.new

server.add_handler("resultrequest", ["struct", "struct", "int", "int"],
"resultrequest({sessionid, matchingid}, ruleno, matchno)
  returns {type, {context, parameter, tree}}
  where type is 'TimeOut' or 'NoMoreMatches' or errtxt") {
  |id, ruleno, matchno|
  serverobj.resultrequest(id, ruleno, matchno)
}

server.add_handler("matchrequest", 
                   ['struct', 'struct', 'string', 'string', 'array', 'int', 'int', 'int'],
                   "matchrequest({sessionid, matchingid}, signature, agent, rules, matchcount, rulestomatch, requestno)
  returns a {type, {sessionid, matchingid}}
  where type is 'OK' or 'TimeOut'") {
  |id, signature, agent, rules, matchcount, rulestomatch, requestno|
  serverobj.matchrequest(id, signature, agent, rules,
                                matchcount, rulestomatch, requestno)
}

server.add_handler("reactrequest", 
                   ['struct', 'struct', 'int', 'int', 'int'],
                   "reactrequest({sessionid, matchingid}, rule, match, requestno)
  returns a {type, {agent}}
  where type is 'OK' or 'TimeOut'") {
  |id, rule, match, requestno|
  serverobj.reactrequest(id, rule, match, requestno)
}

server.add_handler("simplifyrequest", 
                   ['string'],
                   "simplifyrequest(agent)
  returns a {type, {agent}}
  where type is 'OK' or 'TimeOut'") {
  |agent|
  serverobj.simplifyrequest(agent)
}

server.serve
