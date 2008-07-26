using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Microsoft.Dynamics.Mobile.Framework;

namespace CosmoBiz.EngineLibrary
{
  /*
   * The UI Form:
   * Basicly an empty form that acts as a container for Microsoft Dynamics tasklets.  
   */
  public partial class UIForm : Form, ITaskletContainer
  {
    // The active Control.
    private Control ac;
    public MainMenuManager mm;
    public CosmoBizContextMenuManager cmm;

    public MainMenu MainMenu
    {
      get { return this.mainMenu; }
    }

    /*
     * Constructor
     */
    public UIForm()
    {
      InitializeComponent();

      cmm = new CosmoBizContextMenuManager();
      cmm.mainMenu = this.MainMenu;

      mm = new MainMenuManager(this.MainMenu);

    }

    // Public property to get the active control.
    public Control ActiveControl { get { return ac; } }

    /*
     * Function for adding a tasklet to the form,
     * the tasklet will become the active control.
     */
    public void Show(Control c)
    {
      c.Location = new Point(0, 0);
      this.Controls.Add(c);
      ac = c;
    }

    /*
     * Method that removes a tasklet from the UIForm (and effectivly closes it)
     * Note: this can probably be done better, I need to have a look at Microsoft
     *       Dynamics container.
     */
    public void Close(Control c)
    {
      this.Controls.Remove(c);
      if (this.Controls.Count > 0)
      {
        ac = this.Controls[this.Controls.Count - 1];
        ac.BringToFront();
      }
      else
        ac = null;
    }

    /*
     * Method for closing all open tasklets.
     * To be implemented.
     */
    public void CloseAll()
    {
      // to be implemented
    }

    /*
     * Method for showing a confirmation box.
     * To be implemented
     */
    public bool Confirm(String s)
    {
      return true;
    }

    /*
     * Method for showing an alert box.
     * To be implemented
     */
    public void Alert(String s)
    {
    }

  }
}