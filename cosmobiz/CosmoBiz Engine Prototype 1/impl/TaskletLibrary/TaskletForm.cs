using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace CosmoBiz.TaskletLibrary
{
  public class TaskletForm : Form
  {
    private String input;
    private String output;

    protected virtual void UseInput()
    { ;}

    public void SetInput(String i)
    {
      input = i;
      UseInput(); 
    }

    public String GetOutput()
    {
      return output;
    }

    protected String GetInput()
    {
      return input;
    }

    protected void SetOutput(String s)
    {
      output = s;
    }

    public TaskletForm()
    {
    }
  }
}
