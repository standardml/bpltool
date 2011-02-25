init();

function init(){
		var p = document.getElementById("p_add_link");
		
		var text = document.createTextNode("Add another file (wsdl or xsd)…");
			
        if(document.all) p.attachEvent("onclick",add);
        else p.addEventListener("click",add,true);
        
        p.replaceChild(text, document.getElementById("a_add_link"));
        

}


function add(){
        //On compte le nombre de label et on sélectionne le premier fieldset
        var count = document.getElementById("opt_field_zone").getElementsByTagName("label").length;
        var field_zone = document.getElementById("opt_field_zone");
        var p_add_link = document.getElementById("p_add_link");
        count ++;
        
        //label creation
        var p_label = document.createElement("p");
        p_label.setAttribute("class","file_input_label");        
        var label_text1 = document.createTextNode("Pick an ");
        var label_text2 = document.createTextNode("optional");
        var label_text3 = document.createTextNode(" wsdl or xsd file :");
        var em = document.createElement("em");
        em.appendChild(label_text2);
        var label = document.createElement("label");
        label.appendChild(label_text1);
        label.appendChild(em);
        label.appendChild(label_text3);
        p_label.appendChild(label);
        
        
        //file input creation
        var p_input = document.createElement("p");
        p_input.setAttribute("class","file_input");   
        var input = document.createElement("input");
        input.setAttribute("type","file");
        input.setAttribute("name","opt_file"+count);
        p_input.appendChild(input);
        
        
        //On raccroche ici tous nos éléments virtuels à une balise de notre fichier XHTML. Ils sont alors affichés   
        field_zone.insertBefore(p_label, p_add_link);
        field_zone.insertBefore(p_input, p_add_link);
}
