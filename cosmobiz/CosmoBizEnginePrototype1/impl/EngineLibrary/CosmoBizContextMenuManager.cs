using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Dynamics.Mobile.Framework;
using System.Collections.ObjectModel;
using Microsoft.Dynamics.Mobile.Framework.Entities;
using Microsoft.Dynamics.Mobile.Framework.Runtime;
using System.Windows.Forms;

namespace CosmoBiz.EngineLibrary
{
  public class CosmoBizContextMenuManager : IContextMenuManager
  {
    // Fields
    private ActionCollection actions;
    private IMainMenuManager mainMenuManager;
    private Collection<RolePadMenuItem> rolePadMenuItems = new Collection<RolePadMenuItem>();
    public MainMenu mainMenu;

    public CosmoBizContextMenuManager()
    {
      this.rolePadMenuItems = new Collection<RolePadMenuItem>();
    }


    // Methods
    //[SuppressMessage("Microsoft.Globalization", "CA1303:DoNotPassLiteralsAsLocalizedParameters", MessageId="System.Windows.Forms.MenuItem.set_Text(System.String)", Justification="The '-' special char is represented as a line by the MainMenu")]
    private static MenuItem CreateLine()
    {
      MenuItem item = new MenuItem();
      item.Text = "-";
      return item;
    }

    public void LoadMenu(ContextMenu contextMenu, params string[] parameterNames)
    {
      this.LoadMenu(contextMenu, false, parameterNames);
    }

    public void LoadMenu(ContextMenu contextMenu, bool removeFromMainMenu, params string[] parameterNames)
    {
      // Fill up the main menu.
      /*
      foreach (Action action in this.actions)
      {
        MenuItem menuItem = new MenuItem();
        menuItem.Text = action.Text;
        mainMenu.MenuItems.Add(menuItem);        
      }
      */
      /*
      SortedCollection<RolePadMenuItem> sorteds = new SortedCollection<RolePadMenuItem>();
      if (this.actions.Count != 0)
      foreach (Action action in this.actions.Select(parameterNames))
      {
        if (removeFromMainMenu)
        {
          this.mainMenuManager.Remove(action);
        }
        RolePadMenuItem item = new RolePadMenuItem(action);
        sorteds.Add(item);
        this.rolePadMenuItems.Add(item);
      }
      Group group = null;
      MenuItem menuItem = null;
      foreach (RolePadMenuItem item3 in sorteds)
      {
        Group group2 = item3.Group;
        if (group != group2)
        {
          bool flag = (group != null) && (group.GroupType == GroupType.Flat);
          bool flag2 = (group != null) && (group.GroupType == GroupType.Node);
          bool flag3 = (group2 != null) && (group2.GroupType == GroupType.Flat);
          bool flag4 = (group2 != null) && (group2.GroupType == GroupType.Node);
          if ((this.rolePadMenuItems.IndexOf(item3) > 0) && (flag || flag3))
          {
            contextMenu.MenuItems.Add(CreateLine());
          }
          if (flag4)
          {
            menuItem = new MenuItem();
            menuItem.Text = group2.Text;
            contextMenu.MenuItems.Add(menuItem);
          }
          if (flag2)
          {
            menuItem = null;
          }
          group = group2;
        }
        if (menuItem != null)
        {
          menuItem.MenuItems.Add(item3.MenuItem);
        }
        else
        {
          contextMenu.MenuItems.Add(item3.MenuItem);
        }
      }
      */
    }

    public void Refresh()
    {
      foreach (RolePadMenuItem item in this.rolePadMenuItems)
      {
        item.UpdateMenuItemStatus();
      }
    }

    // Properties
    public ActionCollection Actions
    {
      get
      {
        return this.actions;
      }
      set
      {
        this.actions = value;
      }
    }

    public IMainMenuManager MainMenuManager
    {
      get
      {
        return this.mainMenuManager;
      }
      set
      {
        this.mainMenuManager = value;
      }
    }
  }
}