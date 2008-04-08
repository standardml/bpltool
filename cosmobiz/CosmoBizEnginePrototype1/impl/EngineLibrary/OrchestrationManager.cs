using System;
using System.Collections.Generic;
using System.Text;
using System.Xml.Serialization;
using System.IO;
using System.Diagnostics;

namespace CosmoBiz.EngineLibrary
{
  // Only handles the logic of orchestrations:
  // * Interprets the workflow of an orchestration
  // * Tells the taskletmanager what tasklets to run
  // * Performs transformations on the orchestration
  // * Is able of writing transformed orchestrations as a new XML file.
  class OrchestrationManager
  {
    // Boolean that represents if the orchestration is at end or not:
    private Boolean atEnd;
    // The current orchestration:
    private orchestration currentOrchestration;
    // The current tasklet:
    private orchestrationTasklet currentTasklet; 
    // An enumerator that goes over the tasklets in the orchestration.
    private System.Collections.IEnumerator taskletEnum;    
    // Dictionary containing "Global" variables.
    private Dictionary<string, object> globals; 

    // Public property to get the value of atEnd:
    public Boolean AtEnd
    {
      get { return (atEnd); }
    }

    /*
     * Constructor:
     * Initializes fields and loads an orchestration.
     */
    public OrchestrationManager()
    {
      globals = new Dictionary<string, object>();
      LoadOrchestration();
      atEnd = false;
    }

    /*
     * Function for loading an orchestration
     * (currently hardcoded, needs to be extended so the orchestration's location
     * is passed as a variable)
     * - Creates a XMLSerializer for Type orchestration 
     *   (which is auto-generated from OrchestrationExample.xsd)
     * - Reads and deserializes an orchestration xml file.
     * - Starts an enumerator on the tasklets in the orchestration
     *   (as we start adding workflow properties to the orchestration, this will
     *    likely have to be changed)
     * - Moves the enumerator to the first element.     
     */
    private void LoadOrchestration()
    {
      // Read an orchestration using the xmlSerializer and a generated class "orchestration"
      XmlSerializer s = new XmlSerializer(typeof(orchestration));
      TextReader r = new StreamReader("\\Program Files\\engineapplication\\OrchestrationExample.xml");

      currentOrchestration = (orchestration)s.Deserialize(r);
      r.Close();

      // Starting an enumerator to go through the tasklets
      // in the order that they are listed in the XML file
      taskletEnum = currentOrchestration.tasklet.GetEnumerator();
      taskletEnum.MoveNext();
    }

    /*
     * Function for saving the current orchestration under a specific name.
     * (currently hardcoded to store the file on the storage card location of the
     *  mobile device)
     */
    public void SaveOrchestration(String name)
    {      
      XmlSerializer s = new XmlSerializer(typeof(orchestration));
      TextWriter w = new StreamWriter("\\Storage Card\\" + name + ".xml");
      s.Serialize(w, currentOrchestration);
      w.Close();
    }

    /*
     * Function for retreiving the next task (a simplified representation of a tasklet)
     */
    public Task NextTask()
    {
      // To be extended: make a specialized Exception type.
      if (atEnd) throw new Exception("Orchestration is at end.");
      currentTasklet = (orchestrationTasklet)taskletEnum.Current;
      
      // Get the location details from the tasklet 
      // (to find out how it should be loaded)
      Task t = new Task(
        ((orchestrationTasklet)taskletEnum.Current).assembly,
        ((orchestrationTasklet)taskletEnum.Current).name,
        ((orchestrationTasklet)taskletEnum.Current).type); // when using load
    
      // Load the inputs of the tasklet.
      if (((orchestrationTasklet)taskletEnum.Current).input != null)
        foreach (orchestrationTaskletInput i in ((orchestrationTasklet)taskletEnum.Current).input)
        {
          // Code for debugging:
          if ((i.type == "global"))
          {
            Debug.WriteLine("Contents of globals:");
            foreach (KeyValuePair<String, Object> kvp in globals)
              Debug.WriteLine(kvp.Key + ":" + kvp.Value.ToString());
            Debug.WriteLine("-----");
          }

          if (i.type == "constant") t.AddInput(i.name, i.value);
          else if ((i.type == "global") && globals.ContainsKey(i.value)) t.AddInput(i.name, globals[i.value]);
          else t.AddInput(i.name, i.value);      
        }

      atEnd = !taskletEnum.MoveNext();

      return t;

    }

    /*
     * Function that insert the output of a tasklet into the orchestration. 
     * The argument is a dictionairy from string to object, where the key is the
     * name of an output, and the value is the value of an output.
     */
    public void InsertOutput(Dictionary<string, object> taskletState)
    {
      orchestrationTaskletOutput[] op = new orchestrationTaskletOutput[taskletState.Count];
      int i = 0;

      foreach (KeyValuePair<string, object> o in taskletState)
      {
        orchestrationTaskletOutput oto = new orchestrationTaskletOutput();
        oto.name = o.Key;
        oto.Value = o.Value.ToString();
        oto.type = o.Value.GetType().Name;
        op[i] = oto;
        i++;
      }

      currentTasklet.outputs = op;
    }

    /*
     * An function for updating the "global variables" of the orchestration. These
     * are variables that can be accessed by any tasklet.
     */
    public void UpdateGlobals(Dictionary<string, object> taskletState)
    {      
      foreach (KeyValuePair<string, object> o in taskletState)
      {
        if (globals.ContainsKey(o.Key))
        {
          globals.Remove(o.Key);
        }
        globals.Add(o.Key, o.Value);
      }
    }

  }
}

