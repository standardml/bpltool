using System;
using System.Collections.Generic;
using System.Text;

namespace CosmoBiz.EngineLibrary
{
  /*
   * A simplified representation of tasklets, containing:
   * - Their physical location
   * - Input values
   * - Actions
   */
  class Task
  {
    // The name of the assembly that the tasklet is a part of.
    private String assembly;
    // The name of the tasklet.
    private String tasklet;
    // Inputs to the tasklet, represented by a dictionary from string to object.
    // Key(String): The name of the inpit
    // Value(Object): The value of the inpit
    private Dictionary<String, Object> input;
    // The type of the tasklet, currently:
    // MD: MicroSoft Dynamics - MicroSoft's Format for tasklets
    // CB: CosmoBiz           - Our own format for tasklets, currently outdated (was initially used for testing)
    private String type;
    public List<actionType> Actions;

    // public properties to access these values.
    public String Assembly { get { return assembly; } }
    public String Tasklet { get { return tasklet; } }
    public Dictionary<String, Object> Input { get { return input; } }
    public String Type { get { return type; } }

    // constructor
    public Task(String assembly, String tasklet, String type)
    {
      input = new Dictionary<String, Object>();
      Actions = new List<actionType>();

      this.assembly = assembly;
      this.tasklet = tasklet;
      this.type = type;
    }

    // function for adding inputs to the task.
    public void AddInput(String name, Object value)
    {
      input.Add(name, value);
    }
  }
}