using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace CosmoBiz.EngineLibrary
{
  class MainMenuManager
  {
    private MainMenu mainMenu;
    private MenuItem mainItem;
    private TaskletManager owner;

    public MainMenuManager(TaskletManager o, MainMenu mm)
    {
      owner = o;

      mainMenu = mm;      
      mainItem = new MenuItem();
      mainItem.Text = "Menu";
      mainMenu.MenuItems.Add(mainItem);
    }

    public void AddTasklet(Task t)
    {

      // Still have to do sorting here:
      //t.openActions.Sort(      

      foreach (actionType a in t.Actions)
      {
        foreach (MenuItem i in AddAction(a))
          mainItem.MenuItems.Add(i);
      }
    }

    private List<MenuItem> AddAction(actionType a)
    {
      if (a.GetType() == typeof(openType))      
        return AddOpenAction((openType)a);
      else if (a.GetType() == typeof(groupType))
        return AddGroupAction((groupType)a);
      else if (a.GetType() == typeof(exitOrchestrationType))
        return AddExitOrchestrationAction((exitOrchestrationType)a);
      return null;
    }

    private List<MenuItem> AddOpenAction(openType o)
    {
      MenuItem m = new MenuItem();
      m.Text = o.text;
      m.Click += new EventHandler(MenuClicked);

      List<MenuItem> l = new List<MenuItem>();
      l.Add(m);
      return l;
    }

    private List<MenuItem> AddGroupAction(groupType g)
    {
      List<MenuItem> l = new List<MenuItem>();

      if (g.type == "Node")
      {
        MenuItem m = new MenuItem();
        m.Text = g.text;
        if (g.Items != null)
          foreach (actionType a in g.Items)
          {
            foreach (MenuItem i in AddAction(a))
              m.MenuItems.Add(i);
          }
        l.Add(m);
      }
      else
      {
        l.Add(CreateLine());
        if (g.Items != null)
          foreach (actionType a in g.Items)
          {
            l.AddRange(AddAction(a));
          }
        l.Add(CreateLine());
      }
      return l;
    }

    private List<MenuItem> AddExitOrchestrationAction(exitOrchestrationType e)
    {
      MenuItem m = new MenuItem();
      m.Text = e.text;
      m.Click += new EventHandler(ExitOrchestrationClicked);

      List<MenuItem> l = new List<MenuItem>();
      l.Add(m);
      return l;
    }

    void MenuClicked(object sender, EventArgs e)
    {
      // To be imlemented
    }

    void ExitOrchestrationClicked(object sender, EventArgs e)
    {
      owner.ExitOrchestration();     
    }

    private static MenuItem CreateLine()
    {
      MenuItem item = new MenuItem();
      item.Text = "-";
      return item;
    }


  }
}
