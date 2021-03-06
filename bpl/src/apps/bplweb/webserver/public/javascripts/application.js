// Place your application-specific JavaScript functions and classes here
// This file is automatically included by javascript_include_tag :defaults
/* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
 *
 * This file is part of BPL.
 *
 * BPL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * BPL is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BPL; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 */
/** set element style
 * objId    = element id.
 * style    = the style to be changed.
 * value    = the value assigned to the style.
 */
function setStyle(objId, style, value){
   document.getElementById(objId).style[style]= value;
}

function max (a, b) { return (a > b ? a : b); }

function min (a, b) { return (a < b ? a : b); }

function gettextsize (text) {
  var l = text.length;
  var r = 0;
  var c = 0;
  var maxcol = 0;
  for (i = 0; i < l; i++)
    switch (text.charAt (i)) {
    case '\n':
      if (c > maxcol) maxcol = c;
      c = 0; r++;
      break;
    default:
      c++;
    }
  if (c > maxcol) maxcol = c;
  return {rows: r + 1, cols: maxcol};
}

function escapeHTML (s) {
  return s.replace (/&/g, "&amp;").replace (/</g, "&lt;").replace (/>/g, "&gt;");
}

function resizenode (textareanode) {
  var nodetext = textareanode.value;
  var textsize = gettextsize (nodetext);
  var rows = min (max (textsize.rows, 1), 20);
  var cols = min (max (textsize.cols + 2, 40), 100);
  textareanode.rows = rows;
  textareanode.cols = cols;
}

function initialresizing () {
  resizenode($('signature'));
  resizenode($('agent'));
  var redex_body;
  for (var i = 0; redex_body = $("redex[" + i + "]"); ++i)
    resizenode(redex_body);
  var react_body;
  for (var i = 0; react_body = $("react[" + i + "]"); ++i)
    resizenode(react_body);
}

function initialredraw () {
  if ($('showimgs').checked) {
    drawsvgrequest ($('agent'), "agent-image");
  var image;
  for (var i = 0; image = $("rule[" + i + "]-image"); ++i)
    $(image).setStyle ({display: 'block'});
  var redex_body;
  for (var i = 0; redex_body = $("redex[" + i + "]"); ++i)
    drawsvgrequest (redex_body, "rule[" + i + "]-redex-image");
  var react_body;
  for (var i = 0; react_body = $("react[" + i + "]"); ++i)
    drawsvgrequest (react_body, "rule[" + i + "]-react-image");
  }
}

function editnode (textnode) {
  var textnode = $(textnode);
  var nodetext = textnode.textContent;
  var editnode = document.createElement ("textarea");
  editnode.setAttribute ("type", "textfield");
  editnode.setAttribute ("class", "editclicked");
  editnode.innerHTML = nodetext;
  var nodetextsize = gettextsize (nodetext);
  var rows = min (max (nodetextsize.rows, 1), 20);
  var cols = min (max (nodetextsize.cols + 2, 40), 100);
  editnode.setAttribute ("rows", rows);
  editnode.setAttribute ("cols", cols);
  var oknode = document.createElement ("input");
  oknode.setAttribute ("type", "button");
  oknode.setAttribute ("value", "OK");
  oknode.setAttribute ("onclick", "displaynode(this.parentNode);");
  var container = document.createElement ("div");
  container.appendChild (editnode);
  container.appendChild (oknode);
  textnode.parentNode.insertBefore(container, textnode);
  textnode.setStyle({display: 'none'});
}


function displaynode (container) {
  var editnode = container.firstChild;
  var nodetext = editnode.value;
  var textnode = $(container.nextSibling);
  textnode.innerHTML = nodetext;
  container.parentNode.removeChild (container);
  textnode.setStyle({display: 'block'});
}


function visibilitytoggler (id, hide) {
  var plusstyle = "";
  var minusstyle = "";
  if (hide)
    minusstyle = " style='display: none;'";
  else
    plusstyle = " style='display: none;'";
  return "<a href='#' class='visibilitytoggle' onclick='togglevisibility (\"" + id + "\"); return false;'><span id='" + id + "-p'" + plusstyle + ">+</span><span id='" + id + "-m'" + minusstyle + ">&#8722;</span></a>\n";
}


function togglevisibility (id) {
  Effect.toggle (id + "-body", 'appear', {duration: 0.25});
  Effect.toggle (id + "-p", 'appear', {duration: 0});
  Effect.toggle (id + "-m", 'appear', {duration: 0});
}


function rulechild (title, helplink, id, areaid, term, insertbody, drawonchange) {
  var titleattr = "";
  if (term) titleattr = "title='Enter " + term + " here' ";
  return ("	    <div id='" + id + "'>\n" +
"	      <p class='head'>\n" +
"	        <span class='toggler' target='" + id + "'>" +
                  visibilitytoggler (id) + "</span>\n" +
"                " + title + ": \n" + helplink +
"              </p>\n" +
"	       <div id='" + id + "-body' class='body'>\n" +
(insertbody ? 
"                <textarea name='" + areaid + "' id='" + areaid + "' class='editablecode' " + titleattr + "rows='1' cols='60'\n" +
(drawonchange ?
"                 onchange='redraw (this, \"" + id + "-image\");'\n" : "") +
"                 onkeyup='resizenode (this);'></textarea>\n" : "") +
"              </div>\n" +
"            </div>\n");
}


function getruleno (inputnode) {
  var ruleidstr = inputnode.up ("div").getAttribute ("id");
  var rulenostr = ruleidstr.match (/rule\[([0-9]*)\]/);
  return rulenostr [1].valueOf ();
}

var bigraphhelp = "<a\n" +
"	      href='bigraphsyntax'\n" +
"	      class='helpicon'\n" +
"             target='bigraph Syntax'><img\n" +
"	      class='helpicon'\n" +
"	      src='/images/Icon_help.gif'\n" + 
"              alt=' ? '\n" +
"              title='Help for bigraph syntax' /></a>";

var instantiationhelp = "<a\n" +
"	      href='instantiationsyntax'\n" +
"	      class='helpicon'\n" +
"             target='Instantiation Syntax'><img\n" +
"	      class='helpicon'\n" +
"	      src='/images/Icon_help.gif'\n" + 
"              alt=' ? '\n" +
"              title='Help for instantiation syntax' /></a>";

function addrule () {
  var rulesnode = $("rules-body");
  var nodeno = rulesnode.immediateDescendants ().length;
  var rulenode = document.createElement ("div");
  rulenode.setAttribute ("id", "rule[" + nodeno + "]");
  rulenode.innerHTML =
"	  <p class='head'>\n" +
"	    <table width='70%'>\n" +
"	      <tr>\n" +
"	        <td align='left'>\n" +
"	        <span class='toggler' target='rule[" + nodeno + "]'>" +
                  visibilitytoggler ("rule[" + nodeno + "]") + "</span>\n" +
"	          Rule <span class='ruleno'>" + nodeno + "</span>:</td>\n" +
"		<td align='right'>\n" +
"		  <input type='button' value='Delete'\n" +
"                   title='Delete this rule'\n" +
"                   onclick='deleterule (getruleno(this));' />\n" +
"		  <input type='button' value='1 Match'\n" +
"                   title='Find one match of this rule'\n" +
"                   onclick='matchrequest (getruleno(this), 1);' />\n" +
"		  <input type='button' value='All Matches'\n" +
"                   title='Find all matches of this rule'\n" +
"                   onclick='matchrequest (getruleno(this), -1);' />\n" +
"		  <input type='button' value='Stop'\n" +
"		disabled='disabled' />\n" +
"		</td>\n" +
"	      </tr>\n" +
"	    </table>\n" +
"	  </p>\n" +
"	  <div id='rule[" + nodeno + "]-body' class='body'>\n" +
rulechild ('Redex', bigraphhelp, "rule[" + nodeno + "]-redex", "redex[" + nodeno + "]", 'a redex bigraph', true, true) +
rulechild ('React', bigraphhelp, "rule[" + nodeno + "]-react", "react[" + nodeno + "]", 'a reactum bigraph', true, true) +
rulechild ('Instantiation', instantiationhelp, "rule[" + nodeno + "]-inst", "inst[" + nodeno + "]", 'an instantiation', true, false) +
"	    <div id='rule[" + nodeno + "]-image'" +
(($("showimgs").checked) ? "" : " style='display: none;'") + ">\n" +
"              <span id='rule[" + nodeno + "]-redex-image' onclick='showsource(this);'>\n" +
"              </span>\n" +
"              <span style='vertical-align: 150%; font-size: larger;'>\n" +
"                &#160;&#8594;&#160;\n" +
"              </span>\n" +
"              <span id='rule[" + nodeno + "]-react-image' onclick='showsource(this);'>\n" +
"              </span>\n" +
"	    </div>\n" +
rulechild ("<span id='rule[" + nodeno + "]-count" + nodeno +"'></span> Matches", "", "rule[" + nodeno + "]-matches", "rule[" + nodeno + "]-matches", false, false, false) +
"         </div>";

  rulesnode.appendChild (rulenode);

}


function deleterule (ruleno) {
  var thisrule = $("rule[" + ruleno + "]");
  // Renumber following nodes
  var rule = thisrule.next ()
  thisrule.parentNode.removeChild (thisrule);
  for (;
       !(rule === undefined);
       rule = rule.next (), ruleno++) {
    var rulenodeid = rule.getAttribute ("id");
    var oldnum = rulenodeid.replace ("rule", "");
    var newnum = "[" + ruleno + "]";
    var namednodes = rule.getElementsBySelector ('[id|="'+ rulenodeid + '"]')
      .concat (rule.getElementsBySelector ('[id|="redex' + oldnum + '"]'))
      .concat (rule.getElementsBySelector ('[id|="react' + oldnum + '"]'))
      .concat (rule.getElementsBySelector ('[id|="inst' + oldnum + '"]'));
    var newrulenodeid = "rule[" + ruleno + "]";
    for (var i = 0; i < namednodes.length; i++) {
      var node = namednodes [i];
      var idstr = node.getAttribute ("id");
      idstr = idstr.replace (/\[[0-9]*\]/, newnum);
      node.setAttribute ("id", idstr);
    }
    namednodes = rule.getElementsByClassName ('editablecode');
    for (var i = 0; i < namednodes.length; i++) {
      var node = namednodes [i];
      var namestr = node.getAttribute ("name");
      namestr = namestr.replace (/\[[0-9]*\]/, newnum);
      node.setAttribute ("name", namestr);
    }
    var togglers = rule.getElementsByClassName ("toggler");
    for (var i = 0; i < togglers.length; i++) {
      var toggler = togglers [i];
      var target = toggler.getAttribute ("target");
      target = target.replace (oldnum, newnum);
      var hide = $(target + "-p").visible ();
      toggler.setAttribute ("target", target);
      togglers [i].innerHTML = visibilitytoggler (target, hide);
    }
    rule.setAttribute ("id", newrulenodeid);
    rule.down ('[class=ruleno]').innerHTML = ruleno;
  }
}

function addresult (rule, match, result) {
    //  alert ("adding result " + rule + "-" + match);
  var rstr = 'rule[' + rule + ']';
  var rmstr = rstr + '-match[' + match + ']';
  var matchnode = $(rmstr)
  if (!matchnode) {
    var parent = $(rstr + '-matches-body');
    matchnode = document.createElement ("div");
    matchnode.setAttribute ("id", rmstr);
    matchnode.setAttribute ("class", "match"); 
    matchnode.innerHTML =
"   <p class='head'>\n" +
"     <table width='100%'>\n" +
"       <tr>\n" +
"	 <td align='left'>\n" +
"	   " + visibilitytoggler (rmstr) + "\n" +
"	   Match " + match + ":\n" +
"	 </td>\n" +
"	 <td align='right'>\n" +
"	   <input type='button' value='React' \n" +
"             onclick='reactrequest (" + rule + ", " + match + ");' />\n" +
"	 </td>\n" +
"       </tr>\n" +
"     </table>\n" +
"   </p>\n" +
"   <div id='" + rmstr + "-body' class='body'>\n" +
"     <div id='" + rmstr + "-ctx'>\n" +
"       <p class='head'>\n" +
"	 " + visibilitytoggler (rmstr + '-ctx') + "\n" +
"	 Context:\n" +
"       </p>\n" +
"       <div id='" + rmstr + "-ctx-body' class='body'>\n" +
"	 <pre class='code'>" + escapeHTML (result ['context']) + "</pre>\n" +
"       </div>\n" +
"       <div id='" + rmstr + "-ctx-image' class='image' onclick='showsource(this);'>\n" +
"       </div>\n" +
"     </div>\n" +
"     <div id='" + rmstr + "-par'>\n" +
"       <p class='head'>\n" +
"	 " + visibilitytoggler (rmstr + '-par') + "\n" +
"	 Parameter:\n" +
"       </p>\n" +
"       <div id='" + rmstr + "-par-body' class='body'>\n" +
"	 <pre class='code'>" + escapeHTML (result ['parameter']) + "</pre>\n" +
"       </div>\n" +
"       <div id='" + rmstr + "-par-image' class='image' onclick='showsource(this);'>\n" +
"       </div>\n" +
"     </div>\n" +
"     <div id='" + rmstr + "-tree'>\n" +
"       <p class='head'>\n" +
"	 " + visibilitytoggler (rmstr + '-tree', true) + "\n" +
"	 Inference tree:\n" +
"       </p>\n" +
"       <div id='" + rmstr + "-tree-body' class='body' style='display: none;'>\n" +
"	 <pre class='code'>" + result ['tree'] + "</pre>\n" +
"       </div>\n" +
"     </div>\n" +
"   </div>";
    parent.appendChild (matchnode);
  } else {
    $(rmstr + '-ctx-body').innerHTML
      = '<pre class="code">' + result ['context'] + '</pre>';
    $(rmstr + '-par-body').innerHTML
      = '<pre class="code">' + result ['parameter'] + '</pre>';
    $(rmstr + '-tree-body').innerHTML
      = '<pre class="code">' + result ['tree'] + '</pre>';
  }
  if ($('showimgs').checked) {
    drawsvgrequest
      ($(rmstr + "-ctx-body"), rmstr + "-ctx-image");
    drawsvgrequest
      ($(rmstr + "-par-body"), rmstr + "-par-image");
  }
}

function getPos (pos) {
  var s = "";
  var t = pos ['line'];
  if (t >= 0) {
    s = String (t);
    t = pos ['col']; if (t >= 0) s += "." + String (t);
  }
  return s;
}

function getInterval (errTree) {
  var s = "";
  var t = errTree ['field']; if (t) s += t + ":";
  var tfrom = errTree ['from'];
  if (tfrom) {
    var interval = getPos (tfrom);
    var tto = errTree ['to'];
    if (tto && (tto ['line'] != tfrom ['line']
                || tto ['col'] != tfrom ['col']))
      interval += "-" + getPos (tto);
    if (interval != "") s += interval + ":";
  }
  return s;
}

function errTreeToString (errTree, indent) {
  var s = "";
  for (var i = 0; i < indent; i++) s += " ";
  var interval = getInterval (errTree, indent);
  if (interval != "") s += interval + " ";
  var t = errTree ['txt']; if (t != "") s += t;
  t = errTree ['suberrors']; if (t) s += "\n" + errorToString (t, indent + 1);
  return s;
}

function errorToString (errTrees, indent) {
  var s = "";
  var tab = "";
  for (var i = 0; i < indent; i++) tab += " ";
  var notfirst = false;
  for (var i = 0; i < errTrees.length; i++) {
    if (notfirst) s += "\n" + tab; notfirst = true;
    s += errTreeToString (errTrees [i]);
  }
  return s;
}

var id = {'sessionid': -1, 'matchingid': -1};

function resultrequest (rule, match, rulestomatch, matchcount) {
    // alert ('Sending for result (' + rule + ", " + match + ")");
  new Ajax.Request
    ('/bplweb/resultrequest',
     {'parameters': 'sessionid=' + id['sessionid'] + '&matchingid=' + id['matchingid']
      + '&rule=' + rule + '&match=' + match,
      onSuccess: function (transport, json) {
	 //    alert("got result");
          result = eval ("(" + transport.responseText + ")");
          switch (result ['type'].toUpperCase ()) {
          case 'OK':
            id = result.id;
            addresult (rule, match, result ['result']);
            if (matchcount > 1)
              resultrequest (rule, match + 1, rulestomatch, matchcount - 1);
            else if (matchcount < 0)
              resultrequest (rule, match + 1, rulestomatch, matchcount);
            break;
	  case 'NOMOREMATCHES':
	    id = result.id;
	    if (result.rule < 0)
              $('totalmatches-count').innerHTML = "(" + result.matches +
	        (result.matches == 1 ? " match)" : " matches)");
	    else {
	      var matchcounter = $("rule[" + result.rule + "]-count");
	      if (matchcounter)
	        matchcounter.innerHTML = result.matches.toString ();
	    }
	    //alert ("Found a total of " + result.matches + " matches for "
	    //        + (result.rule < 0 ? "all rules" : "rule " + result.rule));
	    break;
          case 'TIMEOUT':
            if (confirm 
                  ("The request for match #" + match + " of rule #" +
                   rule + " timed out.  Request again?"))
              resultrequest (rule, match, rulestomatch, matchcount);
            break;
          case 'ERROR':
            if (result ['subtype'].toUpperCase () == 'MATCHER')
              alert ("Error: " + errorToString (eval ("(" + result.errtxt + ")")));
            else
              alert ("Error: " + result.errtxt);
            break;
          default:
            alert ("Unrecognised server result response: " + transport.responseText);
          }
        }
     });
}

var requestno = 1;

function deletematches (rulestomatch) {
  var i;
  if (rulestomatch < 0) i = 0; else i = rulestomatch;
  var ruletoclear;
  for (; ruletoclear = $('rule[' + i + ']-matches-body'); i++) {
    var c;
    while (c = ruletoclear.firstChild) ruletoclear.removeChild (c);
    var counter = $('rule[' + i + ']-count');
    if (counter) counter.innerHTML = "";
    if (rulestomatch >= 0) break;
  }
  $('totalmatches-count').innerHTML = "";
}

function drawsvgrequest (bigraphnode, imgnodeid) {
  var bigraph = bigraphnode.value;
  if (!bigraph)
    bigraph = bigraphnode.textContent;
  if (!bigraph)
    bigraph = bigraphnode.innerText;
  new Ajax.Request
    ('/bplweb/svgrequest',
     {'parameters': {'sessionid': id['sessionid'], 'matchingid': id['matchingid'],
      'signature': $('signature').value, 'bigraph': bigraph},
      onSuccess: function (transport) {
      	$(imgnodeid).innerHTML = transport.responseText;
      }})
}

function redraw (bigraphnode, imgnodeid) {
  if ($('showimgs').checked) drawsvgrequest (bigraphnode, imgnodeid);
}

function BPLcheckSVG () {
  if (!document.implementation.hasFeature("org.w3c.dom.svg", "1.0"))
    checkAndGetSVGViewer();
}

function toggleshowimgs (checkbox) {
  var bigraph;
  var istr;
  var i;
  if (checkbox.checked) {
    BPLcheckSVG ();
    drawsvgrequest ($("agent"), "agent-image");
    for (i = 0; istr = '[' + i + ']', bigraph = $('redex' + istr); i++) {
      $('rule' + istr + '-image').setStyle ({display: 'block'});
      drawsvgrequest (bigraph, 'rule' + istr + '-redex-image');
      drawsvgrequest ($('react' + istr), 'rule' + istr + '-react-image');
      var rulematchstr = 'rule' + istr + '-match';
      var jstr;
      for (
        var j = 0;
	jstr = rulematchstr + '[' + j + ']',
        bigraph = $(jstr + '-ctx-body');
	j++) {
	drawsvgrequest (bigraph, jstr + '-ctx-image');
	drawsvgrequest ($(jstr + '-par-body'), jstr + '-par-image');
      }
    }
  } else {
    $("agent-image").innerHTML = "";
    for (i = 0; istr = '[' + i + ']', bigraph = $('redex' + istr); i++) {
      $('rule' + istr + '-image').setStyle ({display: 'none'});
      $('rule' + istr + '-redex-image').innerHTML = "";
      $('rule' + istr + '-react-image').innerHTML = "";
      var rulematchstr = 'rule' + istr + '-match';
      var jstr;
      for (
        var j = 0;
	jstr = rulematchstr + '[' + j + ']',
        bigraph = $(jstr + '-ctx-body');
	j++) {
	$(jstr + '-ctx-image').innerHTML = "";
	$(jstr + '-par-image').innerHTML = "";
      }
    }
  }
}

function matchrequest (rulestomatch, matchcount) {
    //  alert("sending match request");
  deletematches (rulestomatch);
  new Ajax.Request
    ('/bplweb/matchrequest',
     {'parameters': 'sessionid=' + id['sessionid'] + '&matchingid=' + id['matchingid']
      + '&matchcount=' + matchcount
      + '&rulestomatch=' + rulestomatch + '&requestno=' + new Date ().getTime () % 100000000
      + '&' + $('agent-rule-form').serialize (),
      onSuccess: function (transport, json) {
          result = eval ("(" + transport.responseText + ")");
          switch (result ['type'].toUpperCase ()) {
          case 'OK':
            id = result.id;
//            alert ("Matching initiated OK.");
            if (rulestomatch < 0) {
              var l = $('rules-body').immediateDescendants ().length;
              for (var rule = 0; rule < l; rule++)
                resultrequest (rule, 0, rulestomatch, matchcount);
            } else
              resultrequest (rulestomatch, 0, rulestomatch, matchcount);
            break;
          case 'TIMEOUT':
            alert ("The match request timed out.  Please try again.");
            break;
          case 'ERROR':
            alert ("Error: " + result.errtxt);
            break;
          default:
            alert ("Unrecognised server match response: " + transport.responseText);
          }
        }
     });
}


function reactrequest (rule, match) {
  new Ajax.Request
    ('/bplweb/reactrequest',
     {'parameters': 'sessionid=' + id['sessionid'] + '&matchingid=' + id['matchingid']
      + '&rule=' + rule + '&match=' + match + '&requestno=' + new Date ().getTime () % 100000000,
      onSuccess: function (transport, json) {
          result = eval ("(" + transport.responseText + ")");
          switch (result ['type'].toUpperCase ()) {
          case 'OK':
            id = result.id;
            deletematches (-1);
            var agentnode = $('agent');
            agentnode.value = result.agent;
            resizenode (agentnode);
            if ($('showimgs').checked) drawsvgrequest (agentnode, 'agent-image');
            break;
          case 'TIMEOUT':
            alert ("The react request timed out.  Please try again.");
            break;
          case 'ERROR':
            if (result ['subtype'].toUpperCase () == 'MATCHER')
              alert ("Error: " + errorToString (eval ("(" + result.errtxt + ")")));
            else
              alert ("Error: " + result.errtxt);
            break;
          default:
            alert ("Unrecognised server react response: " + transport.responseText);
          }
        }
     });
}


function simplifyrequest () {
  new Ajax.Request
    ('/bplweb/simplifyrequest',
     {'parameters': $('agent-rule-form').serialize (),
      onSuccess: function (transport, json) {
          result = eval ("(" + transport.responseText + ")");
          switch (result ['type'].toUpperCase ()) {
          case 'OK':
            $('agent').value = result.simplifiedagent;
            resizenode($('agent'));
            break;
          case 'TIMEOUT':
            alert ("The simplification request timed out.  Please try again.");
            break;
          case 'ERROR':
            alert ("Error: " + result.errtxt);
            break;
          default:
            alert ("Unrecognised server simplification response: " + transport.responseText);
          }
        }
     });
}


function openhelpwindow (url, title, width, height) {
  window.open
    ('/bplweb/' + url, 'Help: ' + title,
     'menubar=no,toolbar=no,location=no,directories=no,scrollbars=yes,resizeable=yes,dependent=yes,width=' + width + ',height=' + height);
}

function showsource (bigraph) {
  var srcwin = window.open
    ('', 'Illustration source code',
     'menubar=no,toolbar=no,location=no,directories=no,scrollbars=yes,resizeable=yes,dependent=yes');
  var srcdoc = srcwin.document;
  var pre = srcdoc.createElement("pre");
  var text = $(bigraph).getElementsByClassName("source")[0].textContent;
  if (text) {
    pre.textContent = text;
  } else {
    pre.innerText = $(bigraph).getElementsByClassName("source")[0].innerText;
  }
  var body = srcdoc.body;
  var child = body.firstChild;
  if (child) { body.removeChild (child); } 
  body.appendChild (pre);
}
