using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Reflection;
using System.Windows.Forms;
using System.Diagnostics;
using System.Drawing;
using Microsoft.Dynamics.Mobile.Framework;
using Microsoft.Dynamics.Mobile.Framework.Configuration;
using Microsoft.Dynamics.Mobile.Framework.CompositeUI;
using Microsoft.Dynamics.Mobile.Framework.Controls;
using Microsoft.Dynamics.Mobile.Framework.Entities;
using Microsoft.Dynamics.Mobile.Framework.Runtime;
using Microsoft.Dynamics.Mobile.Framework.Services;
using CosmoBiz.TaskletLibrary;
using Services.StyleService;
using System.Threading;

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
    private LoggingService ls;
    private DatabaseCatalog dbc;
    private CosmoBizContextMenuManager cmm;
    private StyleService ss;
    private MainMenuManager mm;
    private Boolean exitOrchestration = false;
    private Tasklet currentTasklet = null;
    private Queue<Tasklet> taskletQueue;

    /*
     * Constructor
     * Initializes the orchestration manager and UIForm.
     */
    public TaskletManager()
    {
      om = new OrchestrationManager();
      uif = new UIForm();
      ls = new LoggingService("\\Storage Card\\logs\\");
      dbc = new DatabaseCatalog("databaseCatalog");
      dbc.Initialize();
      cmm = uif.cmm;

      //cmm = new CosmoBizContextMenuManager();
      //cmm.mainMenu = uif.MainMenu;

      //mm = new MainMenuManager(this, uif.MainMenu);
      mm = uif.mm;
      mm.SetOwner(this);

      ss = new StyleService();

      uif.Closing += this.UIForm_Closing;
    }

    public TaskletManager Clone()
    {
      TaskletManager tm = new TaskletManager();
      //tm.om = this.om;
      tm.om.SetGlobals(this.om.CopyGlobals());
      //tm.uif = this.uif;
      tm.ls = this.ls;
      tm.dbc = this.dbc;

      //tm.cmm = this.cmm;

      //tm.mm = this.mm;
      tm.ss = this.ss;

      //uif.Closing += this.UIForm_Closing;

      return tm;
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
        if (exitOrchestration) break;
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
      Type myType = null;
      foreach (Type type in assembly.GetTypes())
      {
        if (type.Name == t.Tasklet)
        {
          tf = (TaskletForm)System.Activator.CreateInstance(type);
          myType = type;
        }
      }

      Debug.WriteLine(myType.Name);
      Debug.WriteLine("--");
      foreach (FieldInfo f in myType.GetFields())
      {
        Debug.WriteLine(f.Name.ToString());
        Debug.WriteLine(f.Attributes.ToString());
        Debug.WriteLine(f.GetCustomAttributes(false).ToString());
      }
      Debug.WriteLine("--");

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
        {
          tasklet = (Tasklet)System.Activator.CreateInstance(type);                   
        }
        //Debug.WriteLine(type.FullName); //Test_Tasklet.TestForm -> if we want to use GetType... probably also to use direct instantiation.
      }
      currentTasklet = tasklet;

      IServiceFactory serviceFactory;
      TaskletWorkItem item = new TaskletWorkItem();

      serviceFactory = new ServiceFactory(item, ServiceScope.Tasklet);

      foreach (PropertyInfo info in tasklet.GetType().GetProperties())
      {
        if (info.IsDefined(typeof(RolePadServiceAttribute), true))
        {
          if (info.PropertyType.Name == "ILoggingService")
            info.SetValue(tasklet, ls, null);

          if (info.PropertyType.Name == "DatabaseCatalog")
            info.SetValue(tasklet, dbc, null);

          if (info.PropertyType.Name == "StyleService")
            info.SetValue(tasklet, ss, null);                    
        }
      }


      //tasklet.Configuration = new EmbeddedConfiguration("<appSettings><add key = \"TaskletMode\" value = \"View\"/></appSettings>");

      tasklet.Configuration = new EmbeddedConfiguration();
      //tasklet.Configuration.AppSettings.Add("TaskletMode", "View");

      foreach (KeyValuePair<String, Object> p in t.Settings)
        tasklet.Configuration.AppSettings.Add(p.Key, p.Value.ToString());


      // is this still nessecairy?
      tasklet.Definition = new TaskletDefinition("a", "b");

      mm.AddTasklet(t);

      cmm.Actions = tasklet.Actions;
      tasklet.ContextMenuManager = cmm;

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
      //uif.Show();
      Debug.WriteLine("|UIForm has exited");

      // Remove the taslet from the UIForm
      // is this nessecairy, shouldn't this happen through tasklet.Close()?
      Debug.WriteLine("|Closing the active control");
      uif.Close(uif.ActiveControl);

      Debug.WriteLine("|done running tasklet");

      // Save the orchestration (for testing - rather slow right now!)
      Debug.WriteLine("|Saving the orchestration");
      //om.SaveOrchestration("output_" +t.Assembly + "_" + t.Tasklet);

      // Close the tasklet.
      tasklet.Close();
      currentTasklet = null;
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
          if (o != null) Debug.WriteLine(o.ToString());
        }
        om.InsertOutput(taskletState);
      }
    }


    internal void ExitOrchestration()
    {
      exitOrchestration = true;
      if (currentTasklet != null)
      {
        uif.Close();
        uif.Close(uif.ActiveControl);
        currentTasklet.Close();
      }
    }

    internal void ExitTasklet(ExitResult Result)
    {
      if (currentTasklet != null)
      {
        uif.Close();
        uif.Close(uif.ActiveControl);
        currentTasklet.Close(Result);
      }
    }

    internal void RealLoadOrchestration(string name)
    {
      om.LoadOrchestration(name);
    }

    internal void LoadOrchestration(string name)
    {
      //throw new Exception("The method or operation is not implemented.");
      // foreach processType in 

      //om.LoadOrchestration(name);

      //TaskletManager tm = new TaskletManager();
      //tm.Clone(this);

      // make sure that globals is updated.
      Dictionary<string, object> taskletState = currentTasklet.ExtractOutput();
      om.UpdateGlobals(taskletState);

      TaskletManager tm = this.Clone();
      tm.RealLoadOrchestration(name);
      tm.Run();
      //tm.Close();
      //Thread MyThread = new Thread(new ThreadStart(tm.Run));
      //MyThread.Join();

      /*
      if (currentTasklet != null)
      {
        uif.Close();
        uif.Close(uif.ActiveControl);
        currentTasklet.Close();
      }
       * */
      //this.Run();
      // uif.Close();
    }

    /*
    private void Close()
    {
      uif.Close();
      uif.Dispose();
    }
    */

    private void UIForm_Closing(object sender, CancelEventArgs e)
    {
      // this messes up the show dialog...
      UIForm f = (UIForm)sender;
      if (f.ActiveControl != null)
      {
        f.Close(f.ActiveControl);
        if (f.Controls.Count > 0) e.Cancel = true;
      }
      //f.
        
    }
  }
}
