using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace CosmoBiz.EngineLibrary
{
  public class MainMenuManager
  {
    private MainMenu mainMenu;
    private MenuItem mainItem;
    private TaskletManager owner;
    private ActionSorter sort;

    public MainMenuManager(MainMenu mm) : this(null, mm)
    {     
    }

    public void SetOwner (TaskletManager o)
    {
      owner = o;
    }

    public MainMenuManager(TaskletManager o, MainMenu mm)
    {
      owner = o;

      sort = new ActionSorter();

      mainMenu = mm;      
      mainItem = new MenuItem();
      mainItem.Text = "Menu";
      mainMenu.MenuItems.Add(mainItem);
    }

    public void AddTasklet(Task t)
    {
      mainItem.MenuItems.Clear();
      // Still have to do sorting here:
      //t.openActions.Sort(      
      //t.Actions.Sor
      //t.Actions.Sort(
      sort.Sort(t.Actions);
      //t.Actions.Sort(sort);
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
      else if (a.GetType() == typeof(exitProcessType))
        return AddExitOrchestrationAction((exitProcessType)a);
      return null;
    }

    private List<MenuItem> AddOpenAction(openType o)
    {
      OpenActionMenuItem m = new OpenActionMenuItem();
      m.Action = o;
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
        {
          List<actionType> actionList = new List<actionType>(g.Items);          
          sort.Sort(actionList);
          foreach (actionType a in actionList)
          {
            foreach (MenuItem i in AddAction(a))
              m.MenuItems.Add(i);
          }
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

    private List<MenuItem> AddExitOrchestrationAction(exitProcessType e)
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
      OpenActionMenuItem m = (OpenActionMenuItem)sender;
      if (m.Action.process != null && m.Action.process != "")
      {
        owner.LoadOrchestration(m.Action.process);     
      }
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
