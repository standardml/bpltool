using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Windows.Forms;
using CosmoBiz.TaskletLibrary;
using System.Diagnostics;
using Microsoft.Dynamics.Mobile.Framework.Controls;
using Microsoft.Dynamics.Mobile.Framework;
using System.Drawing;
using Microsoft.Dynamics.Mobile.Framework.Entities;

namespace CosmoBiz.EngineLibrary
{
  /*
   * The TaskletManager:
   * A class that handles the loading and running of tasklets.
   */
  public class TaskletManager
  {
    // The orchestration manager that the taskletmanager uses.
    private OrchestrationManager om;
    // the UI Form that the taskletmanager uses.
    private UIForm uif;

    /*
     * Constructor
     * Initializes the orchestration manager and UIForm.
     */
    public TaskletManager()
    {
      om = new OrchestrationManager();
      uif = new UIForm();
    }

    /*
     * Starts the Takslet manager. 
     * The tasklet manager will run the tasklets in the orchestration loaded by the
     * orchestration manager, and stop when the orchestration manager reports it is
     * at end.
     */
    public void Run()
    {
      while (!(om.AtEnd))
      {
        RunTask(om.NextTask());
      }
    }

    /*
     * Runs a Task:
     * Determines the type of the task and then calls specilaized methods.
     */
    private void RunTask(Task t)
    {
      if (t.Type == "CB")
        RunTaskCB(t);
      else
        RunTaskMD(t);
    }

    /*
     * Runs a CosmoBiz Tasklet.
     */
    private void RunTaskCB(Task t)
    {
      // Load the Assembly that contains the tasklet.
      Assembly assembly = Assembly.Load(t.Assembly);

      // Note: This can be changed to be more effective by using GetType!
      // Get an instance of the tasklet type:
      TaskletForm tf = null;
      foreach (Type type in assembly.GetTypes())
      {
        if (type.Name == t.Tasklet)
          tf = (TaskletForm)System.Activator.CreateInstance(type);     
      }

      // If there are inputs, add them to the tasklet.
      if (t.Input.Count > 0)
      {
        Dictionary<String, Object>.ValueCollection.Enumerator e;
        e = t.Input.Values.GetEnumerator();
        e.MoveNext();
        tf.SetInput(e.Current.ToString());
      }

      // Show the tasklet and wait for it to be closed.
      tf.ShowDialog();

      Debug.WriteLine(tf.GetOutput());
    }


    /*
     * Runs a Microsoft Dynamics Tasklet.
     */
    private void RunTaskMD(Task t)
    {
      Debug.WriteLine("Running a Microsoft Dynamics Tasklet");

      // Load the Assembly that contains the tasklet.
      Debug.WriteLine("Loading assembly");
      Assembly assembly = Assembly.Load(t.Assembly);

      Tasklet tasklet = null;

      // Note: This can be changed to be more effective by using GetType!
      // Get an instance of the tasklet type:
      Debug.WriteLine("Loading type");
      foreach (Type type in assembly.GetTypes())
      {
        if (type.Name == t.Tasklet)
          tasklet = (Tasklet)System.Activator.CreateInstance(type);
        //Debug.WriteLine(type.FullName); //Test_Tasklet.TestForm -> if we want to use GetType... probably also to use direct instantiation.
      }

      // set the tasklets container to be the UIForm.
      Debug.WriteLine("|Adding to the container form");
      tasklet.Container = uif;

      // Try injecting input into the tasklet
      Debug.WriteLine("|Inputting into the tasklet");
      try
      {
        tasklet.InjectInput(t.Input);
      }
      catch (Microsoft.Dynamics.Mobile.Framework.Entities.InvalidInputParameterException e)
      {
        // write some nice exception handling code - for now, just ignore.
      }

      // Add listeners to the tasklet
      tasklet.OutputChanged += new EventHandler(TaskletOutputChanged);
      tasklet.Closing += new EventHandler<Microsoft.Dynamics.Mobile.Framework.Entities.ExitResultEventArgs>(this.TaskletClosing);

      // Activate the tasklet
      Debug.WriteLine("|trying to activate the tasklet");
      tasklet.Activate();

      // Start the tasklet
      Debug.WriteLine("|trying to start the tasklet");
      tasklet.Start();

      // Show the UIForm and wait until it is closed.
      Debug.WriteLine("|Giving control to the UIForm");
      uif.ShowDialog();

      Debug.WriteLine("|UIForm has exited");

      // Remove the taslet from the UIForm
      // is this nessecairy, shouldn't this happen through tasklet.Close()?
      Debug.WriteLine("|Closing the active control");
      uif.Close(uif.ActiveControl);

      Debug.WriteLine("|done running tasklet");

      // Save the orchestration (for testing - rather slow right now!)
      Debug.WriteLine("|Saving the orchestration");
      om.SaveOrchestration("output_" +t.Assembly + "_" + t.Tasklet);

      // Close the tasklet.
      tasklet.Close();      
    }

    /*
     * Handler for when the tasklets output is changed:
     * updates the outputs in the orchestration
     */
    internal void TaskletOutputChanged(object sender, EventArgs e)
    {
      ITasklet tasklet = (ITasklet)sender;
      this.UpdateOutput(tasklet);
    }


    /*
     * Handler for when the tasklets output is closing:
     * updates the orchestration managers global variables.
     */
    internal void TaskletClosing(object sender, ExitResultEventArgs e)
    {
      Debug.WriteLine("Closing is being called.");
      ITasklet tasklet = (ITasklet)sender;

      if (tasklet != null)
      {
        Dictionary<string, object> taskletState = tasklet.ExtractOutput();

        om.UpdateGlobals(taskletState);
        om.MoveNext();
      }
    }

    /*
     * Method that handles inserting the tasklets updated outputs into 
     * the orchestration manager
     */
    private void UpdateOutput(ITasklet tasklet)
    {
      if (tasklet != null)
      {
        Dictionary<string, object> taskletState = tasklet.ExtractOutput();       

        foreach (Object o in taskletState.Values)
        {
          Debug.WriteLine(o.ToString());
        }
        om.InsertOutput(taskletState);
      }
    }

  }
}
