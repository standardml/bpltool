using System;
using System.Collections.Generic;
using System.Windows.Forms;
using CosmoBiz.EngineLibrary;
using System.Threading;

/*
 * Main aplication - all the actual functionality is stored in the EngineLibrary
 */

namespace CosmoBiz.EngineApplication
{
  static class Program
  {
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [MTAThread]
    static void Main()
    {
      //Start a new Tasklet Manager.
      TaskletManager tm = new TaskletManager();
      //Thread MyThread = new Thread(new ThreadStart(tm.Run));
      //MyThread.Start();
      //MyThread.Join();
      tm.Run();        
    }
  }
}